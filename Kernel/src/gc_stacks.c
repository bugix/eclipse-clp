/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1988-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * SEPIA SOURCE FILE
 *
 * $Id: gc_stacks.c,v 1.14 2017/02/02 19:21:03 jschimpf Exp $
 *
 * IDENTIFICATION	gc_stacks.c
 *
 * DESCRIPTION		SEPIA stack garbage collector
 *			Please refer to report IR-LP-13-26
 *
 * CHANGE NOTE:	Due to the general design, it is not allowed to mark twice
 *		from the same root pword. Normally this is ok, since the
 *		traversal algorithm guarantees that every root is visited
 *		only once during marking (e.g. choicepoints). Where single
 *		traversal cannot be guaranteed (e.g. marking from "old"
 *		locations, as done in mark_from_trail(), or marking
 *		environments multiple times in different states of activity),
 *		we use ALREADY_MARKED_FROM bits to remember that a root
 *		was already used for marking.
 *		Note that this requires that all trailed items (except those
 *		that are only trailed via simple TRAILED_WORD32 value trails)
 *		must have tags! This is the reason that abstact machine
 *		"registers" like WL, POSTED, etc have tags.
 *
 * CONTENTS:	Stack garbage collector
 *
 *			collect_stacks()
 *
 *		Stack overflow handling routines
 *
 *			trail_ov()
 *			global_ov()
 *			final_overflow()
 *			local_ov()
 *			control_ov()
 *
 *		Traversal functions for dictionary collector
 *
 *			mark_dids_from_pwords()
 *			mark_dids_from_stacks()
 *
 *
 * AUTHOR       VERSION  DATE   REASON
 * Joachim Schimpf	880706	Created file.
 *
 */

#define DEBUG_GC
#define INCR_GC_LIMIT	16

/*
 * INCLUDES:
 */

#include "config.h"
#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "error.h"
#include "mem.h"
#include "dict.h"
#include "ec_io.h"
#include "opcode.h"
#include "emu_export.h"
#include "os_support.h"

/*
 * extern declarations
 */

/*
 * global variables
 */

uword
#ifdef DEBUG_GC
	stop_at_ = MAX_U_WORD,
#endif
	incremental_= 0,	/* number of consecutive incremental GCs */
	collections_ = 0,	/* statistics	*/
	average_area_ = 0,
	collection_time_ = 0;

double	average_ratio_ = 1.0,
	total_garbage_ = 0;


/*
 * static functions
 */

static void
	make_choicepoint(ec_eng_t*,word ar),
	pop_choicepoint(ec_eng_t*),
	non_marking_reference(ec_eng_t*,pword **ref),
	mark_from_trail(ec_eng_t*,control_ptr GCB),
	mark_from_references(ec_eng_t*),
	mark_from(ec_eng_t*,word tag, pword *ref, int ref_in_segment),
	compact_and_update(ec_eng_t*),
	compact_trail(ec_eng_t*,register pword **garbage_list),
	reset_env_marks(ec_eng_t*,control_ptr GCB),
	update_trail_ptrs(ec_eng_t*,control_ptr GCB),
	ov_reset(ec_eng_t*);

static pword
	** early_untrail(ec_eng_t*,control_ptr GCB, register pword **tr, control_ptr fp, pword **garbage_list, word *trail_garbage),
	** mark_from_control_frames(ec_eng_t*,control_ptr GCB, word *trail_garb_count);


/*
 * macros
 */

#define Chp_Tg(b)	(((b).top - 1)->frame.chp->tg)
#define Chp_Tt(b)	(((b).top - 1)->frame.chp->tt)
#define Chp_Sp(b)	(((b).top - 1)->frame.chp->sp)
#define Chp_E(b)	(((b).top - 1)->frame.chp->e)

#define PrevEnv(e)	(*(pword **)(e))

/* this macro assumes that GCTG = Chp_Tg(GCB) */
#define InCurrentSegment(ptr) \
	((ptr) >= GCTG && (ptr) < TG)

#define Set_Bit(mask,pw)	(pw)->tag.kernel |= (mask);
#define Clr_Bit(mask,pw)	(pw)->tag.kernel &= ~(mask);

#define Marked(tag)		((tag) & MARK)
#define IsLink(tag)		((tag) & LINK)

#define TMIN		TUNIV
#define TMAX		TBUFFER

/*
 * Caution: MARK and LINK bits are sometimes used for other purposes.
 * This should not lead to conflicts, but be careful when changing things!
 * ALREADY_MARKED_FROM is the same as MARK, but only used on tags of pwords
 * outside the collection segment, which are never MARKed, so this is safe.
 * MARK_FULL_DE is the same as LINK, but only used in the combination
 * MARK_FULL_DE|TSUSP in the tag-argument of mark_from().  This is ok
 * since valid tags never have the LINK bit set.
 */
#define MARK_FULL_DE		LINK
#define ALREADY_MARKED_FROM	MARK

#define AlreadyMarkedFrom(tag)	((tag) & ALREADY_MARKED_FROM)

/*
 * this macro is supposed to be applied to a pword that is known
 * to be unmarked (yet)
 */

#define Mark_from(tag, ref, in_seg) \
{\
    if (ISPointer(tag))\
	mark_from(ec_eng,tag,ref,in_seg);\
}

#define Mark_from_pointer(tag, ref, in_seg) \
{\
    mark_from(ec_eng,(word) (tag),(pword *)(ref),in_seg);\
}


#define PointerToLink(oldtag,ptr) \
    ((oldtag) & MARK | (word)ptr >> 2 | LINK)

#define PointerToMarkedLink(ptr) \
    ((word)ptr >> 2 | (MARK|LINK))


#define LinkToPointer(link) \
    (pword *)((link) & SIGN_BIT | ((link) << 2 & ~SIGN_BIT))


#define Into_Reloc_Chain_Nonmarking(target, ref) \
{\
    (ref)->val.all = (target)->tag.all;\
    (target)->tag.all = PointerToLink((target)->tag.all,ref);\
}

#define Into_Reloc_Chain(target, ref) \
{\
    (ref)->val.all = (target)->tag.all;\
    (target)->tag.all = PointerToMarkedLink(ref);\
}


/* Environment descriptors and corresponding access macros.
 * Environment descriptors occur in call and retry/trust_inline
 * instructions. They indicate which parts of an environment are active,
 * and consist of an environment size or an activity bitmap (EAM).  */

/* access environment descriptor, given code pointer */
#define EnvDescPP(pp)	(*((word*)(pp)))
/* access environment descriptor, given stack pointer to return address */
#define EnvDesc(sp)	EnvDescPP(*(vmcode**)(sp) - 1)

/*------------------------------------------------------------------
 * Debugging the GC
 *------------------------------------------------------------------*/

#ifdef DEBUG_GC

#define NO	0
#define YES	1

#define Check_Pointer(ptr) \
    if ((ptr) > TG && (ptr) < TG_LIM)\
	_gc_error("invalid pointer encountered\n");

#define Check_Tag_Range(target_tag) \
    if (TagTypeC(target_tag) < TMIN || TagTypeC(target_tag) > TMAX)\
	_gc_error1("invalid tag (0x%x)\n", target_tag);

#define Check_Tag(target_tag) \
    if (IsLink(target_tag))\
	_gc_error1("unexpected unmarked link (0x%x)\n", target_tag);

#define Check_Functor(target_tag) \
    if (TagTypeC(target_tag) != TDICT)\
	_gc_error("invalid structure reference\n");

#define Check_Susp(target_tag) \
    if (TagTypeC(target_tag) != TDE)\
	_gc_error("invalid suspension pointer\n");

#define Check_Size(esize) \
    if ((uword)esize > 1000000) {\
	p_fprintf(current_err_,\
		"GC warning: unlikely environment size (%" W_MOD "x %" W_MOD "x)\n",\
			edesc,esize);\
	ec_flush(current_err_);\
    }

#else /* DEBUG_GC */

#define Check_Pointer(ptr)
#define Check_Tag_Range(target_tag)
#define Check_Tag(target_tag)
#define Check_Functor(target_tag)
#define Check_Susp(target_tag)
#define Check_Size(esize)

#endif /* DEBUG_GC */

#define Print_Err(msg)		_gc_error(msg);
#define Print_Err1(msg, arg)	_gc_error1(msg, arg);

static void
_gc_error(char *msg)
{
    (void) ec_outfs(current_err_,"GC internal error: ");
    (void) ec_outfs(current_err_,msg);
    ec_flush(current_err_);
}

static void
_gc_error1(char *msg, word arg)
{
    (void) ec_outfs(current_err_,"GC internal error: ");
    p_fprintf(current_err_, msg, arg);
    ec_flush(current_err_);
}


/*------------------------------------------------------------------
 * GC builtins
 *------------------------------------------------------------------*/

/*
 * set or query the GC interval (in bytes!)
 */

static int
p_gc_interval(value val, type tag, ec_eng_t *ec_eng)
{
    if (IsRef(tag))
    {
	Return_Unify_Integer(val, tag, TG_SEG * sizeof(pword));
    }
    else
    {   
	pword *tg_gc;
	Check_Integer(tag);
	    /*
	     * update TG_SL: if the new value is below TG,
	     * the next overflow check invokes the GC
	     */
	if (val.nint < sizeof(pword))
		{ Bip_Error(RANGE_ERROR); }
	TG_SEG = val.nint / sizeof(pword);
	if (TG_SEG > (pword *) ec_eng->global_trail[1].start - (pword *) ec_eng->global_trail[0].start)
	    TG_SEG = (pword *) ec_eng->global_trail[1].start - (pword *) ec_eng->global_trail[0].start;
	Succeed_;
    }
}


/*ARGSUSED*/
static int
p_gc_stat(value vwhat, type twhat, value vval, type tval, ec_eng_t *ec_eng)
{
    pword result;

    result.tag.kernel = TINT;
    switch(vwhat.nint)
    {
    case 0:	/* gc_number */
	result.val.nint = collections_;
	break;
    case 1:	/* gc_collected */
	Make_Float(&result, total_garbage_ * sizeof(pword));
	break;
    case 2:	/* gc_area */
	result.val.nint = average_area_ * sizeof(pword);
	break;
    case 3:	/* gc_ratio */
	Make_Float(&result, average_ratio_ * 100.0);
	break;
    case 4:	/* gc_time */
	Make_Float(&result, (double) collection_time_ / clock_hz);
	break;

    case 5:	/* wake_count */
	result.val.nint = ec_eng->wake_count;
	break;


    case 8:	/* global stack used */
	result.val.nint = (char *) TG -
		(char *) ec_eng->global_trail[0].start;
	break;
    case 9:	/* global stack allocated */
	result.val.nint = (char *) ec_eng->global_trail[0].end -
		 (char *) ec_eng->global_trail[0].start;
	break;
    case 10:	/* global stack peak */
	result.val.nint = (char *) ec_eng->global_trail[0].peak -
		 (char *) ec_eng->global_trail[0].start;
	break;
    case 11:	/* trail/global stack size */
	result.val.nint = (char *) ec_eng->global_trail[1].start -
		 (char *) ec_eng->global_trail[0].start;
	break;
    case 12:	/* trail stack used */
	result.val.nint = (char *) ec_eng->global_trail[1].start -
		(char *) TT;
	break;
    case 13:	/* trail stack allocated */
	result.val.nint = (char *) ec_eng->global_trail[1].start -
		 (char *) ec_eng->global_trail[1].end;
	break;
    case 14:	/* trail stack peak */
	result.val.nint = (char *) ec_eng->global_trail[1].start -
		 (char *) ec_eng->global_trail[1].peak;
	break;
    case 15:	/* trail/global stack size */
	result.val.nint = (char *) ec_eng->global_trail[1].start -
		 (char *) ec_eng->global_trail[0].start;
	break;

    case 16:	/* control stack used */
	result.val.nint = (char *) B.args -
		(char *) ec_eng->control_local[0].start;
	break;
    case 17:	/* control stack allocated */
	result.val.nint = (char *) ec_eng->control_local[0].end -
		 (char *) ec_eng->control_local[0].start;
	break;
    case 18:	/* control stack peak */
	result.val.nint = (char *) ec_eng->control_local[0].peak -
		 (char *) ec_eng->control_local[0].start;
	break;
    case 19:	/* local/control stack size */
	result.val.nint = (char *) ec_eng->control_local[1].start -
		 (char *) ec_eng->control_local[0].start;
	break;
    case 20:	/* local stack used */
	result.val.nint = (char *) ec_eng->control_local[1].start -
		(char *) SP;
	break;
    case 21:	/* local stack allocated */
	result.val.nint = (char *) ec_eng->control_local[1].start -
		 (char *) ec_eng->control_local[1].end;
	break;
    case 22:	/* local stack peak */
	result.val.nint = (char *) ec_eng->control_local[1].start -
		 (char *) ec_eng->control_local[1].peak;
	break;
    case 23:	/* local/control stack size */
	result.val.nint = (char *) ec_eng->control_local[1].start -
		 (char *) ec_eng->control_local[0].start;
	break;

    default:
	result.val.nint = 0;
	break;
    }
    Return_Unify_Pw(vval, tval, result.val, result.tag);
}

static int
p_stat_reset(ec_eng_t *ec_eng)
{
    collections_ = 0;
    total_garbage_ = 0.0;
    average_area_ = 0;
    collection_time_ = 0;
    average_ratio_ = 1.0;
    ec_eng->global_trail[0].peak = ec_eng->global_trail[0].end;
    ec_eng->global_trail[1].peak = ec_eng->global_trail[1].end;
    ec_eng->control_local[0].peak = ec_eng->control_local[0].end;
    ec_eng->control_local[1].peak = ec_eng->control_local[1].end;
    ec_eng->wake_count = 0;
    Succeed_
}



/*------------------------------------------------------------------
 * The toplevel function for collecting the global stack:
 *
 * collect_stacks(engine, arity, forced)
 *	arity gives the number of active argument registers.
 *	All VM registers have to be exported.
 *	TG, TT and GB must be imported after the collection.
 *	We assume that on top of the local stack there is a return
 *	address pointing behind the environment size of the current
 *	environment.
 *------------------------------------------------------------------*/

int
collect_stacks(ec_eng_t *ec_eng, word arity, word gc_forced)
{
    word total, garbage, trail_garb_count, gc_time;
    pword **trail_garb_list;
    pword *ideal_gc_trigger, *min_gc_trigger, *max_gc_trigger;
    control_ptr GCB;
    int leave_choicepoint = 0;

    /*
     * Find GCB from GCTG
     * GCB is a conceptual register, pointing to the newest choice point
     * that already existed at the time of the last garbage collection.
     */
    Compute_Gcb(GCB.args);


    /*
     * Now decide whether to garbage collect or to just expand the stack
     * 
     * min_gc_trigger makes sure we collect at least gc_interval bytes
     *    (except when we can't grow the stack to achieve that).
     * ideal_gc_trigger is the point we should ideally collect beyond
     *    in order to avoid quadratic collection time behaviour.
     * max_gc_trigger has been introduced to reduce intervals again when
     *    we approach the final stack limit (i.e. TT). Otherwise big atomic
     *    allocations can cause overflow when we haven't collected for
     *    a long time.
     */
    Safe_Add_To_Pointer(GCTG, GCTG - BChp(GCB.args)->tg, (pword *) TT, ideal_gc_trigger);
    Safe_Add_To_Pointer(GCTG, TG_SEG, (pword *) TT, min_gc_trigger);
    max_gc_trigger = GCTG + ((pword *) TT - GCTG) / 2;

#if 0
    p_fprintf(log_output_, "Remaining space              %12d\n", (char*)TT - (char*)TG);
    p_fprintf(log_output_, "Distance to min_gc_trigger   %12d\n", (char*)min_gc_trigger - (char*)TG);
    p_fprintf(log_output_, "Distance to ideal_gc_trigger %12d\n", (char*)ideal_gc_trigger - (char*)TG);
    p_fprintf(log_output_, "Distance to max_gc_trigger   %12d\n", (char*)max_gc_trigger - (char*)TG);
    if (!(TG < max_gc_trigger))
	p_fprintf(log_output_, "gc because beyond max_gc_trigger\n");
#endif

    if (!gc_forced &&           /* not triggered by garbage_collect/0 */
        (NbStreamsFree > 0) &&  /* not triggered by running out of streams */
        ( ( EclGblFlags & GC_ADAPTIVE
            && TG < ideal_gc_trigger  &&  TG < max_gc_trigger )
        || TG < min_gc_trigger
        ))
    {
	/*
	 * Try to expand the stack rather than doing gc
	 */
	trim_global_trail(ec_eng, TG_SEG);

	/*
	 * trim_global_trail() may expand the stack less than desired,
	 * because of lack of memory, but this doesn't matter much.
	 * As long as the new TG_LIM is larger than the current trigger
	 * point, we delay the collection until TG_LIM is reached.
	 */
	if (TG_LIM > TG_SLS)
	{
	    Set_Tg_Soft_Lim(TG_LIM);
	    return 0;
	}
	if (EclGblFlags & GC_VERBOSE)
	{
	    (void) ec_outfs(log_output_,"GC: couldn't grow global stack as requested, forcing gc\n");
	    ec_flush(log_output_);
	}
    }


    /*
     * Do the garbage collection, if enabled
     */
    if (EclGblFlags & GC_ENABLED)
    {
	gc_time = user_time();

	if (EclGblFlags & GC_VERBOSE) {
	    (void) ec_outfs(log_output_,"GC ."); ec_flush(log_output_);
	}
#ifdef DEBUG_GC
	if (collections_ == stop_at_)
	    total = 0;
	if (SV)
	    Print_Err("SV (suspending variables list) not empty\n");
#endif
		/*
		 * If an incremental choicepoint has been buried under a
		 * regular one, invalidate it to avoid loss of garbage.
		 * This is done by copying the fields from the chp below.
		 */
	if (GCB.top < B.top && IsGcFrame(GCB.top - 1))
	{
	    control_ptr chp;
	    GCB.chp = (GCB.top - 1)->frame.chp;	/* set GCB one deeper */
	    incremental_ = 0;
	    chp.top = GCB.top - 1;
	    chp.chp = chp.top->frame.chp;
	    GCB.chp->tg = chp.chp->tg;
	    GCB.chp->tt = chp.chp->tt;
	    GCB.chp->ld = chp.chp->ld;
	}

		/*
		 * For the duration of the GC, we use GCTG to cache Chp_Tg(GCB)
		 */
	GCTG = Chp_Tg(GCB);
	total = TG - Chp_Tg(GCB);

	make_choicepoint(ec_eng, arity);
		/*
		 * Mark GCB's witness pword first (This should normally be
		 * Mark_from_pointer(TREF, (pword *) &Chp_Tg(GCB), NO);
		 * but eg. InCurrentSegment() keeps using Chp_Tg(GCB)).
		 */
	Set_Bit(MARK, Chp_Tg(GCB));
		/*
		 * mark what is reachable from variables older than GCB
		 */
	mark_from_trail(ec_eng, GCB);
		/*
		 * Take care of the coroutining registers.
		 * The LD list is handled separately.
		 */
	Mark_from_pointer(TSUSP, &DE, NO);
	Mark_from_pointer(TLIST, (pword *) &MU, NO);
	Mark_from(TAGGED_WL.tag.kernel, &TAGGED_WL, NO);
	Mark_from(POSTED.tag.kernel, &POSTED, NO);
	Mark_from_pointer(WP_STAMP.tag.kernel, &WP_STAMP, NO);
	Mark_from_pointer(PostponedList.tag.kernel, &PostponedList, NO);
		/*
		 * Mark the list of cut actions
		 */
	Mark_from_pointer(TCOMP, (pword *) &LCA, NO);
#ifdef NEW_ORACLE
		/*
		 * Mark the oracle registers
		 */
	if (TO) Mark_from_pointer(TCOMP, (pword *) &TO, NO);
#endif
		/*
		 * Mark from global and external references
		 */
	mark_from_references(ec_eng);
		/*
		 * process control frames and the related environments,
		 * do virtual backtracking and trail garbage detection
		 */
	trail_garb_list = mark_from_control_frames(ec_eng, GCB, &trail_garb_count);
	reset_env_marks(ec_eng, GCB);
		/*
		 * end of the marking phase
		 */
	if (EclGblFlags & GC_VERBOSE) {
	    (void) ec_outfs(log_output_,"."); ec_flush(log_output_);
	}
		/*
		 * compact global stack and trail
		 */
	compact_and_update(ec_eng);
	if (trail_garb_count) compact_trail(ec_eng, trail_garb_list);
		/*
		 * scan the choicepoints and update the tt entries
		 */
	update_trail_ptrs(ec_eng, GCB);
		/*
		 * restore the (updated) machine state
		 */
	pop_choicepoint(ec_eng);
		/*
		 * statistics
		 */
	garbage = total - (TG - Chp_Tg(GCB));
	average_area_ =
	    ((average_area_ * collections_) + total) / (collections_ + 1);
	if (garbage || total_garbage_ > 0.0)
	    average_ratio_ *=
		(total_garbage_ + garbage)
		/ (total_garbage_ + average_ratio_ * total);
	total_garbage_ += garbage;
	collections_++;
	gc_time = user_time() - gc_time;
	collection_time_ += gc_time;

	if (EclGblFlags & GC_VERBOSE)
	{
	    word trail_total = Chp_Tt(GCB) - TT + trail_garb_count;

	    p_fprintf(log_output_,
		". global: %d - %d (%.1f %%), trail: %d - %d (%.1f %%), time: %.3f\n",
		sizeof(pword) * total,
		sizeof(pword) * garbage,
		(100.0*garbage)/total,
		4 * trail_total,
		4 * trail_garb_count,
		trail_total ? (100.0*trail_garb_count)/trail_total : 0.0,
		(double)gc_time/clock_hz
	    );
	    ec_flush(log_output_);
	}

	    /*
	     * Remember the stack pointer's value after the collection
	     */
	GCTG = TG;

	    /* We may trim the local stack only when we are sure that there are
	     * no garbage trail entries pointing above the top of SP !
	     * This is the case after a gc.
	     */
	(void) trim_control_local(ec_eng);

	/* Shrink the dynamic event queue to at least
	 *  MIN_DYNAMIC_EVENT_SLOTS free
	 */
	trim_dynamic_event_queue(ec_eng);
    }


    /*
     * re-adjust the stacks
     */
    trim_global_trail(ec_eng, TG_SEG);
    if (TG_LIM - TG < TG_MIN_SEG)
    {
	ov_reset(ec_eng);		/* overflow even after collection */
    }
    Set_Tg_Soft_Lim(TG_LIM);

    return leave_choicepoint;
}


/*
 * save the VM registers in a new choicepoint
 * This is to simplify the algorithm
 */

static void
make_choicepoint(ec_eng_t *ec_eng, word ar)
{
    chp_ptr chp;
    top_ptr top;
    pword *pw;

    if (GB != Chp_Tg(B))
    {
	Print_Err("GB != B->tg");
    }

    chp = (B.chp)++;
    chp->sp = SP;
    chp->tg = TG;
    chp->tt = TT;
    chp->e = E;
    chp->ld = LD;
    pw = &A[1];
    for(; ar > 0; ar--) {
	*((B.args)++) = *(pw++);
    }
    top = (B.top)++;
    top->frame.chp = chp;
    top->backtrack = gc_fail_code_;

    pw = TG++;				/* push a dummy word (needed	*/
    pw->tag.kernel = TNIL;		/* for updating chp->tg)	*/
}
 
/*
 * restore from the choicepoint the VM registers that may have changed
 * during garbage collection
 */

static void
pop_choicepoint(ec_eng_t *ec_eng)
{
    control_ptr chp;
    top_ptr top;
    pword *pw;

    top = B.top - 1;
    chp.chp = top->frame.chp;
    TT = chp.chp->tt;
    TG = chp.chp->tg;
    LD = chp.chp->ld;
    chp.chp++;
    pw = &A[1];			/* reload arguments	*/
    while(chp.top < top)
	*pw++ = *(chp.args)++;
    B.any_frame = top->frame;	/* pop the choicepoint	*/

    GB = Chp_Tg(B);

    /* Now mark the other arguments invalid (for recursive emulators).
     * Caution: There may be a module argument which must be skipped first.
     */
    while(++pw < &A[NARGREGS] && pw->tag.kernel != TEND)
    {
	pw->tag.kernel = TEND;
	pw->val.nint = 0x11111111;
    }
}

/* pop a dummy choicepoint, don't restore anything */

static void
drop_choicepoint(ec_eng_t *ec_eng)
{
    TG = Chp_Tg(B);             /* pop dummy pword */
    B.args = BPrev(B.args);     /* pop the choicepoint	*/
}


/*-------------------------------------------------------------------
 * marking phase
 *-------------------------------------------------------------------*/


/*
 * process the trail entries younger than the control frame fp:
 * - remove unnecessary trails of locations newer than fp
 * - early untrail and remove trails of (so far) unreachable locations
 * - link other entries into relocation chains
 */
static pword **
early_untrail(ec_eng_t *ec_eng, control_ptr GCB, register pword **tr, control_ptr fp, pword **garbage_list, word *trail_garbage)
{
    register pword *trailed_item;
    register word i, what, trailed_tag;
    register pword **prev_tt = fp.chp->tt;
    register pword *prev_tg = fp.chp->tg;
    pword *prev_sp = fp.chp->sp;
    pword *gcb_tg = Chp_Tg(GCB);
    pword *gcb_sp = Chp_Sp(GCB);

    while (tr < prev_tt)		/* partial untrailing */
    {
	switch ((word) *tr & 3)
	{
	case TRAIL_ADDRESS:
	    trailed_item = *tr;
	    if (trailed_item < prev_tg)
	    {
		if (trailed_item >= gcb_tg)
		{
		    if (!Marked(trailed_item->tag.kernel))
		    {
			/* early reset, since this variable is
			 * only reachable after backtracking
			 */
#ifdef DEBUG_GC
			if (IsLink(trailed_item->tag.kernel))
			    Print_Err("unmarked link in early_reset\n");
#endif
			trailed_item->val.ptr = trailed_item;
			trailed_item->tag.kernel = TREF;
			(*trail_garbage)++;
			*tr = (pword *)garbage_list;
			garbage_list = tr;
		    }
		    else
		    {
			Into_Reloc_Chain(trailed_item,(pword*)tr)
		    }
		}
		else
		{
		    /* reset ALREADY_MARKED_FROM, it was set in mark_from_trail */
		    Clr_Bit(ALREADY_MARKED_FROM, trailed_item);
		}
	    }
	    else if (trailed_item < prev_sp
#ifdef AS_EMU
		&& (trailed_item < TG_LIM || trailed_item >= spmax_)
#endif
		)
	    {
		/* such trail entries can only occur after
		 * a cut and before a fail through this cut.
		 */
		(*trail_garbage)++;
		*tr = (pword *)garbage_list;
		garbage_list = tr;
	    }
	    else if (trailed_item >= gcb_sp)
	    {
		/* reset ALREADY_MARKED_FROM, it was set in mark_from_trail */
		Clr_Bit(ALREADY_MARKED_FROM, trailed_item);
	    }
	    tr++;
	    break;

	case TRAIL_TAG:
	    trailed_item = *(tr+1);
	    if (trailed_item < prev_tg)
	    {
		if (trailed_item >= gcb_tg)
		{
		    if (!Marked(trailed_item->tag.kernel))
		    {
			/* early reset, since this variable is
			 * only reachable after backtracking
			 */
#ifdef DEBUG_GC
			 if (IsLink(trailed_item->tag.kernel))
			    Print_Err( "unmarked link in early_reset\n");
#endif
			trailed_item->val.ptr = trailed_item;
			trailed_item->tag.kernel = TrailedTag(*tr);
			*trail_garbage += 2;
			*(tr+1) = (pword *)garbage_list;
			garbage_list = tr;
		    }
		    else
		    {
			trailed_tag = TrailedTag(*tr);
			/*
			 * CAUTION: we mark here with a non-standard tag which
			 * has the TREFBIT removed. The reason is that this
			 * should be treated as a self-reference although it
			 * doesn't look like one.
			 */
			Mark_from_pointer(trailed_tag & ~TREFBIT, (pword *) (tr + 1), NO);
		    }
		}
		else
		{
		    /* reset ALREADY_MARKED_FROM, it was set in mark_from_trail */
		    Clr_Bit(ALREADY_MARKED_FROM, trailed_item);
		}
	    }
	    else if (trailed_item < prev_sp
#ifdef AS_EMU
		&& (trailed_item < TG_LIM || trailed_item >= spmax_)
#endif
		)
	    {
		/* cut garbage, remove the trail entry */
		*trail_garbage += 2;
		*(tr+1) = (pword *)garbage_list;
		garbage_list = tr;
	    }
	    else if (trailed_item >= gcb_sp)
	    {
		/* reset ALREADY_MARKED_FROM, it was set in mark_from_trail */
		Clr_Bit(ALREADY_MARKED_FROM, trailed_item);
	    }
	    tr += 2;
	    break;

	case TRAIL_MULT:
	    i = (word) *tr;
	    what = TrailedType(i);
	    trailed_item = *(tr+1);
	    if (trailed_item >= prev_tg && trailed_item < prev_sp
#ifdef AS_EMU
		&& (trailed_item < TG_LIM || trailed_item >= spmax_)
#endif
		)
	    {
		/* cut garbage, remove the trail entry */
		i = TrailedNumber(i) + 3;
		*trail_garbage += i;
		*(tr+1) = (pword *)garbage_list;
		garbage_list = tr;
		tr += i;
		break;
	    }
	    if (trailed_item >= gcb_tg && trailed_item < gcb_sp
#ifdef AS_EMU
		&& (trailed_item < TG_LIM || trailed_item >= spmax_)
#endif
		)
	    {
		/*
		 * Special case of the trailed WAKE bit in a TDE tag:
		 * We have to disable early untrail, otherwise some
		 * woken goals would show up as unwoken in the LD list.
		 */
		if (what == TRAILED_WORD32 &&
		    TrailedOffset(i) == 1 &&
		    TagTypeC((word) *(tr+2)) == TDE)
		{
		    /* The flag MARK_FULL_DE is used to tell the
		     * marking routine to ignore the WAKE bit and to
		     * mark the full suspension as if it were unwoken.
		     */
		    Mark_from_pointer(MARK_FULL_DE|TSUSP, tr+1, NO);
		    tr += TrailedNumber(i) + 3;
		}
		else if (!Marked(trailed_item->tag.kernel))
		{
		    /* early untrail, since this item is
		     * only reachable after backtracking
		     */
#ifdef DEBUG_GC
		    if (IsLink(trailed_item->tag.kernel))
			Print_Err( "unmarked link in early_reset\n");
#endif
		    trailed_item = (pword *) ((uword *) trailed_item
			+ TrailedOffset(i));
		    i = TrailedNumber(i);
		    *trail_garbage += i + 3;
		    *(tr+1) = (pword *)garbage_list;
		    garbage_list = tr;
		    tr += 2;
		    do {
#if 0
			/*
			 * This actually occurs, but shouldn't - needs investigation
			 */
			if (IsLink(trailed_item->tag.kernel) || Marked(trailed_item->tag.kernel))
			{
			    Print_Err("unexpected mark/link during early_reset\n");
			}
#endif
			trailed_item->val.ptr = *tr++;
			trailed_item = (pword *)
			    ((uword *) trailed_item + 1);
		    } while (i--);
		}
		else /* the whole item is already marked */
		{
		    Into_Reloc_Chain(trailed_item,(pword*)(tr+1))
		    trailed_item = (pword *) ((uword *) trailed_item
			+ TrailedOffset(i));

		    i = TrailedNumber(i);
		    tr += 2;
		    if (what == TRAILED_PWORD)
		    {
			i /= 2;
			do
			{
			    /*
			     * CAUTION: for trailed self-references, we mark
			     * here with a non-standard tag which has the
			     * TREFBIT removed. The reason is that for marking
			     * purposes this should be treated as a self-
			     * reference although it doesn't look like one.
			     */
			    trailed_tag = ((pword*)tr)->tag.kernel;
			    if (((pword*)tr)->val.ptr == trailed_item)
				trailed_tag &= ~TREFBIT;
			    Mark_from(trailed_tag, ((pword*)tr), NO);
			    tr = (pword **)((pword*)tr + 1);
			    ++trailed_item;
			} while (i--);
		    }
		    else if (what == TRAILED_REF)
			do
			{
			    trailed_tag = TREF;
			    if (*tr == trailed_item)	/* CAUTION: see above */
				trailed_tag &= ~TREFBIT;
			    Mark_from_pointer(trailed_tag, ((pword*)tr), NO);
			    tr++;
			    trailed_item = (pword*) ((uword*)trailed_item + 1);
			} while (i--);
		    else if (what == TRAILED_COMP)
			do
			{
			    Mark_from_pointer(TCOMP, ((pword*)tr), NO);
			    tr++;
			} while (i--);
		    else if (what == TRAILED_WORD32)
			tr += i + 1 ;
		    else
		    {
			Print_Err(
			"bad extension trail entry in early_reset\n");
			tr += 2;
		    }
		}
		break;
	    }
	    /*
	     * The following code is to detect unnecessary pointer trails.
	     * Applies to trailed locations (trailed_item) in the heap
	     * and old parts of local and global stack, e.g. suspending
	     * list pointers and setarg'd structure arguments.
	     * We assume: (trailed_item < gcb_tg || trailed_item >= gcb_sp)
	     *
	     * ??? shouldn't that (the cut garbage bit) more generally apply to 
	     * (trailed_item < prev_tg || trailed_item >= prev_sp) ???
	     */
	    if ((what == TRAILED_REF || what == TRAILED_COMP) &&
		TrailedNumber(i) == 0)
	    {
		pword *trailed_ptr = *(tr+2);
		if (trailed_ptr >= prev_tg && trailed_ptr < prev_sp
#ifdef AS_EMU
		    && (trailed_ptr < TG_LIM || trailed_ptr >= spmax_)
#endif
		    )
		{
		    /* cut garbage, remove the trail entry */
		    *trail_garbage += 3;
		    *(tr+1) = (pword *)garbage_list;
		    garbage_list = tr;
		}
		else	/* mark from the old value */
		{
		    /* CAUTION: see above */
		    trailed_tag = (what == TRAILED_COMP) ? (word) TCOMP
		    	: (*(tr+2) == trailed_item) ? (word) (TREF & ~TREFBIT)
			: (word) TREF;
		    Mark_from(trailed_tag, (pword *)(tr+2), NO);
		}
		/* reset ALREADY_MARKED_FROM, it was set in mark_from_trail */
		Clr_Bit(ALREADY_MARKED_FROM, trailed_item);
	    }
	    else if (what == TRAILED_PWORD && TrailedNumber(i) == 1)
	    {
		pword *trailed_ptr = ((pword *)(tr+2))->val.ptr;
		trailed_tag = ((pword *)(tr+2))->tag.kernel;
		if (ISPointer(trailed_tag) &&
		    trailed_ptr >= prev_tg && trailed_ptr < prev_sp
#ifdef AS_EMU
		    && (trailed_ptr < TG_LIM || trailed_ptr >= spmax_)
#endif
		    )
		{
		    /* cut garbage, remove the trail entry */
		    *trail_garbage += 4;
		    *(tr+1) = (pword *)garbage_list;
		    garbage_list = tr;
		}
		else	/* mark from the old value */
		{
		    /* CAUTION: see above */
		    if (((pword *)(tr+2))->val.ptr == trailed_item)
		    	trailed_tag &= ~TREFBIT;
		    Mark_from(trailed_tag, (pword *)(tr+2), NO);
		}
		/* reset ALREADY_MARKED_FROM, it was set in mark_from_trail */
		Clr_Bit(ALREADY_MARKED_FROM, trailed_item);
	    }
	    tr += TrailedNumber(i) + 3;
	    break;

	case TRAIL_EXT:
	    i = (word) tr[TRAIL_UNDO_FLAGS];
	    trailed_item = tr[TRAIL_UNDO_ADDRESS];
	    switch(TrailedEtype(i))
	    {

	    case TRAIL_UNDO:
		if (InCurrentSegment(trailed_item))
		{
		    if (!Marked(trailed_item->tag.kernel))
		    {
			untrail_ext(ec_eng, tr, UNDO_GC);	/* early untrail */
			*trail_garbage += TrailedEsize(i);
			*(tr+1) = (pword *)garbage_list;
			garbage_list = tr;
			break;
		    }
		    else	/* enter in relocation chains	*/
		    {
			Into_Reloc_Chain(trailed_item,(pword*)(tr+TRAIL_UNDO_ADDRESS))
		    }
		}
		/* Mark the data if it contains pwords.  This is
		 * simpler than marking the untrail data in a value
		 * trail, because is will just be used, not restored.
		 */
		if (TrailedType(i) == TRAILED_PWORD)
		{
		    word n_pwords = (TrailedEsize(i) - TRAIL_UNDO_SIMPLE_HEADER_SIZE)/2;
		    pword *pdata = (pword *) (tr + TRAIL_UNDO_SIMPLE_HEADER_SIZE);
		    for(; n_pwords > 0; --n_pwords, ++pdata)
		    {
			Mark_from(pdata->tag.kernel, pdata, NO);
		    }
		}
		break;

	    case TRAIL_UNDO_STAMPED:
	    {
		pword *stamp = tr[TRAIL_UNDO_STAMP_ADDRESS];
		/* first reset ALREADY_MARKED_FROM, if it was set in mark_from_trail */
		if (!InCurrentSegment(stamp))
		{
		    Clr_Bit(ALREADY_MARKED_FROM, stamp);
		}
		/*
		 * Three cases now:
		 * - timestamp too new: frame is cut garbage, just delete it
		 * - item unreachable: early untrail and delete frame
		 * - otherwise: keep the frame
		 */
		if (tr[TRAIL_UNDO_OLDSTAMP] >= prev_tg)
		{
		    /* Timestamp's old value indicates the frame is cut garbage.
		     * Caution: The timestamp could be reset here, but if
		     * !InCurrentSegment(stamp), the timestamp has been
		     * marked_from and its value may be overwritten.
		     * If Marked(), tag and possibly value are overwritten.
		     * The remaining case (InCurrentSegment(stamp) && !Marked(stamp))
		     * is unlikely. We therefore never reset the stamp.
		     * The only consequence of this is that the stamp may keep
		     * an extra witness pword alive.
		     */
		    *trail_garbage += TrailedEsize(i);
		    *(tr+1) = (pword *)garbage_list;
		    garbage_list = tr;
		    break;
		}
		else if (InCurrentSegment(trailed_item) && !Marked(trailed_item->tag.kernel))
		{
		    /* early untrail: item not reachable until after failure */
		    /* Above comment on timestamp applies here as well */
		    untrail_ext(ec_eng, tr, UNDO_GC);
		    *trail_garbage += TrailedEsize(i);
		    *(tr+1) = (pword *)garbage_list;
		    garbage_list = tr;
		    break;
		}
		else			/* useful trail, mark */
		{
		    /* Enter (weak) item pointer into relocation chain */
		    if (InCurrentSegment(trailed_item)) /* && Marked(trailed_item->tag.kernel) */
		    {
			Into_Reloc_Chain(trailed_item,(pword*)(tr+TRAIL_UNDO_ADDRESS))
		    }
		    /* current stamp: mark or just enter into relocation chain.
		     * Note that the stamp pointer is a strong pointer.  */
		    if (InCurrentSegment(stamp))
		    {
			Mark_from_pointer(TREF, ((pword*)(tr+TRAIL_UNDO_STAMP_ADDRESS)), NO);
		    }
		    /* mark the old stamp */
		    Mark_from_pointer(TREF, ((pword*)(tr+TRAIL_UNDO_OLDSTAMP)), NO);
		    /* Mark the data if it contains pwords.  This is
		     * simpler than marking the untrail data in a value
		     * trail, because is will just be used, not restored.
		     */
		    if (TrailedType(i) == TRAILED_PWORD)
		    {
			word n_pwords = (TrailedEsize(i) - TRAIL_UNDO_STAMPED_HEADER_SIZE)/2;
			pword *pdata = (pword *) (tr + TRAIL_UNDO_STAMPED_HEADER_SIZE);
			for(; n_pwords > 0; --n_pwords, ++pdata)
			{
			    Mark_from(pdata->tag.kernel, pdata, NO);
			}
		    }
		}
	    }
		break;

/**** BEGIN EXTENSION SLOT ****

Name:	GC_EARLY_UNTRAIL

Parameters:
pword **tr      points to extension trail frame, which is already in a
		relocation chain, so the address field is overwritten

Code Template:
	    case TRAIL_EXTENSION:
		if the trail frame contains pointers or pwords,
		use them for marking

****** END EXTENSION SLOT *****/
	    }
	    tr += TrailedEsize(*tr);
	    break;

	}
    }
    return garbage_list;
}



/*
 * Go through the environment chain of control frame fp, and
 * execute SlotAction(_pslot) for each active environment slot.
 * Stop if the chain merges with a previously processed chain
 * (mergepoint).  Then compute the mergepoint for the chain
 * that will be processed next.  The activity descriptor for
 * the first environment in the chain must be given as edesc.
 * Note that in the waking routines we have environments of
 * statically unknown size.  They are marked in the code with
 * a size of -1; the real size is computed from the tag of Y1.
 */

/* Walk_Env_Chain(+fp,?mergepoint,?edesc) */
#define Walk_Env_Chain(_pslot, SlotAction) { \
        pword *_pslot;\
	pword *env = fp.chp->e; \
 \
	/* start of next environment chain */ \
	pword *next_chain = (fp.top-1)->frame.chp->e; \
	pword *next_mergepoint = (env >= next_chain) ? env : (pword *)0; \
 \
	/* process environments up to and including the shared one */ \
	/* while (env <= mergepoint) */ \
	for(;;) \
	{ \
	    if (EdescIsSize(edesc)) { \
		/* we have only an environment size, all slots active */ \
		word sz = EdescSize(edesc,env); \
		Check_Size(sz) \
		for (_pslot = env - sz; _pslot < env; _pslot++) \
		{ \
		    SlotAction /*(_pslot)*/ \
		} \
	    } else { \
		/* we have an environment activity bitmap */ \
		uword *eam_ptr = EdescEamPtr(edesc); \
		_pslot = env; \
		do { \
		    int i=EAM_CHUNK_SZ; \
		    uword eam = EamPtrEam(eam_ptr); \
		    for(;eam;--i) { \
			--(_pslot); \
			if (eam & 1) { \
			    SlotAction /*(_pslot)*/ \
			} \
			eam >>= 1; \
		    } \
		    _pslot -= i; \
		} while (EamPtrNext(eam_ptr)); \
	    } \
	    if (env >= mergepoint) \
		break; \
 \
	    edesc = EnvDesc((pword**)env + 1); \
	    env = PrevEnv(env); \
 \
	    if (!next_mergepoint && env >= next_chain) \
		next_mergepoint = env; \
	} \
 \
	/* compute the next mergepoint	*/ \
	if (next_mergepoint) \
	    mergepoint = next_mergepoint; \
	else \
	{ \
	    do \
	    { \
		env = PrevEnv(env); \
	    } \
	    while(env < next_chain); \
	    mergepoint = env; \
	} \
}


/*
 * Go down control frames and environments, marking their contents,
 * and interleaving an early-reset step between control frames.
 * The collection choicepoint must be on top of control stack.
 * GCB must point to a frame that has tg,sp,tt and e fields!
 */

static pword **
mark_from_control_frames(ec_eng_t *ec_eng, control_ptr GCB, word *trail_garb_count)
{
    control_ptr		fp, top, pfp;
    register pword	*pw, *prev_de;
    pword		*next_de, *mergepoint;
    pword		**tr, **trail_garb_list;
    word		edesc;

    tr = TT;
    mergepoint = Chp_E(GCB);
    trail_garb_list = (pword **) 0;
    *trail_garb_count = 0;
    prev_de = (pword *) 0;
    next_de = LD;

    pfp.args = B.args;
    top.top = pfp.top - 1;
    fp.any_frame = top.top->frame;

    do	/* loop through control frames until we reach GCB */
    {
#ifdef DEBUG_GC
	if (IsRecursionFrame(top.top) || IsExceptionFrame(top.top))
	{
	    Print_Err("bad frame in mark_from_choicepoints\n");
	}
#endif

/**** BEGIN EXTENSION SLOT ****

Name:	GC_MARK_CONTROL_FRAME

Parameters:
	control_ptr top		points to the top frame of a control frame
	control_ptr fp		points to the bottom of this frame

Code Template:
	else if ( this_is_an_extension_frame(top) )
	{
	    Go through the frame and call Mark_from(pw->tag.kernel, pw, NO)
	    for every pword pw stored in the frame.
	    The 4 standard frame entries Sp,Tg,Tt and E are handled by
	    the subsequent code.
	}

****** END EXTENSION SLOT *****/

	if (IsRetryMeInlineFrame(top.top))
	{
	    edesc = EnvDescPP(top.top->backtrack + RETRY_ME_INLINE_SIZE - 1);
	    pw = (pword *)(fp.chp + 1);
	}
	else if (IsTrustMeInlineFrame(top.top))
	{
	    edesc = EnvDescPP(top.top->backtrack + TRUST_ME_INLINE_SIZE - 1);
	    pw = (pword *)(fp.chp + 1);
	}
	else if (IsRetryInlineFrame(top.top))
	{
	    edesc = EnvDescPP(top.top->backtrack + RETRY_INLINE_SIZE - 1);
	    pw = (pword *)(fp.chp + 1);
	}
	else if (IsTrustInlineFrame(top.top))
	{
	    edesc = EnvDescPP(top.top->backtrack + TRUST_INLINE_SIZE - 1);
	    pw = (pword *)(fp.chp + 1);
	}
	else if (IsParallelFrame(top.top))
	{
	    edesc = EnvDesc(fp.chp->sp);
	    pw = (pword *)(fp.chp_par + 1);
	}
	else /* if (IsChoicePoint(top.top)) */
	{
	    edesc = EnvDesc(fp.chp->sp);
	    pw = (pword *)(fp.chp + 1);
	}

	for (; pw < top.args; pw++)	/* mark from arguments	*/
	{
	    Mark_from(pw->tag.kernel, pw, NO)
	}

	top.top = fp.top - 1;		/* find next full frame	*/

        /* Go through the environment chain of frame fp, marking from
         * the permanent variables.  Stop if the chain merges with a
         * previously processed chain (mergepoint).  edesc is the
         * activity descriptor for the first environment in the chain.
         */
	Walk_Env_Chain(pslot, /* (fp,mergepoint,edesc) */
	    if (!AlreadyMarkedFrom(pslot->tag.kernel))
	    {
		Mark_from(pslot->tag.kernel, pslot, NO)
		Set_Bit(ALREADY_MARKED_FROM, pslot)
	    }
	)

	/*
	 * Process the LD list in this stack segment. Deterministically
	 * woken goals are removed from the list. Nondeterministically
	 * woken ones are already marked from the trail at this time
	 * (recognisable e.g. from the marked module field).
	 * Some unmarked woken goals may be marked later from a second
	 * suspending variable, but since they are already woken it's
	 * no problem that they are missing from the LD list.
	 */
	fp.chp->ld = prev_de;		/* add ld field to backpatch chain */
	prev_de = (pword *) &fp.chp->ld;
	while (next_de >= top.top->frame.chp->tg)
	{
	    if (Marked(next_de->tag.kernel) &&
		Marked(next_de[SUSP_MODULE].tag.kernel)
	    || !Marked(next_de->tag.kernel) && !SuspDead(next_de))
	    {
		/*
		 * Found a non-garbage suspension next_de.
		 * Update all fields in the prev_de chain to point to it.
		 */
		do {
		    pw = prev_de->val.ptr;
		    prev_de->val.ptr = next_de;
		    Mark_from_pointer(TSUSP, prev_de, NO); /* the NO is ok! */
		    prev_de = pw;
		} while(prev_de);
		prev_de = &next_de[SUSP_LD];	/* start a new chain */
		next_de = next_de[SUSP_LD].val.ptr;
		prev_de->val.ptr = (pword *) 0;
	    }
	    else /* deterministically woken, skip it */
	    {
		pw = next_de[SUSP_LD].val.ptr;
		next_de[SUSP_LD].val.ptr = (pword *) 0;	/* not necessary */
		next_de = pw;
	    }
	}

	/*
	 * Enter the frame's (and the previous small frame's) TG fields
	 * into relocation chains so that they are updated in the
	 * compaction phase.
	 * These used to be non-marking references. Now we have a
	 * "witness" TNIL pword pushed with every choicepoint which must
	 * be preserved, so we mark it here.
	 */
	do {
	    pfp.any_frame = (pfp.top - 1)->frame;
	    Mark_from_pointer(TREF, ((pword*)&pfp.chp->tg), NO);
	} while (pfp.args > fp.args);

	/*
	 * replace the TT field by the (future) offset from TT
	 */
	tr = fp.chp->tt;		/* remember its original value */
	fp.chp->tt = (pword **)(fp.chp->tt - TT - *trail_garb_count);

	fp.any_frame = top.top->frame;

	/*
	 * Do virtual backtracking and trail garbage detection
	 * for the trail segment newer than fp->tt.
	 * Note that the last invocation of early_untrail does
	 * not do any further untrails.
	 * It is only necessary to collect trail cut garbage!
	 */
	trail_garb_list =
	    early_untrail(ec_eng, GCB, tr, fp, trail_garb_list, trail_garb_count);

    } while (fp.top >= GCB.top);

#ifdef DEBUG_GC
    if (InCurrentSegment(next_de))
	_gc_error("next_de in current segement");
#endif
    do {
	pw = prev_de->val.ptr;
	prev_de->val.ptr = next_de;
	prev_de = pw;
    } while(prev_de);

    return trail_garb_list;
}


static void
reset_env_marks(ec_eng_t *ec_eng, control_ptr GCB)
{
    control_ptr		fp, top;
    pword		*mergepoint;
    word		edesc;

    mergepoint = Chp_E(GCB);

    top.top = B.top - 1;
    fp.any_frame = top.top->frame;

    do	/* loop through control frames until we reach GCB */
    {
#ifdef DEBUG_GC
	if (IsRecursionFrame(top.top) || IsExceptionFrame(top.top))
	{
	    Print_Err("bad frame in mark_from_choicepoints\n");
	    edesc = EnvDesc(fp.chp->sp);
	}
#endif

/**** BEGIN EXTENSION SLOT ****

Name:	GC_MARK_CONTROL_FRAME

Parameters:
	control_ptr top		points to the top frame of a control frame
	control_ptr fp		points to the bottom of this frame

Code Template:
	else if ( this_is_an_extension_frame(top) )
	{
	    Find environment descriptor from execution context
	}

****** END EXTENSION SLOT *****/

	else if (IsRetryMeInlineFrame(top.top))
	{
	    edesc = EnvDescPP(top.top->backtrack + RETRY_ME_INLINE_SIZE - 1);
	}
	else if (IsTrustMeInlineFrame(top.top))
	{
	    edesc = EnvDescPP(top.top->backtrack + TRUST_ME_INLINE_SIZE - 1);
	}
	else if (IsRetryInlineFrame(top.top))
	{
	    edesc = EnvDescPP(top.top->backtrack + RETRY_INLINE_SIZE - 1);
	}
	else if (IsTrustInlineFrame(top.top))
	{
	    edesc = EnvDescPP(top.top->backtrack + TRUST_INLINE_SIZE - 1);
	}
	else /* if (IsChoicePoint(top.top)) */
	{
	    edesc = EnvDesc(fp.chp->sp);
	}

	top.top = fp.top - 1;		/* find next full frame	*/

	Walk_Env_Chain(pslot, /* (fp,mergepoint,edesc) */
	    if (AlreadyMarkedFrom(pslot->tag.kernel))
	    {
		Clr_Bit(ALREADY_MARKED_FROM, pslot)
	    }
	)

	fp.any_frame = top.top->frame;

    } while (fp.top >= GCB.top);
}


static void
non_marking_reference(ec_eng_t *ec_eng, pword **ref)
{
    pword *pw = *ref;

    if (InCurrentSegment(pw))
    {
	Into_Reloc_Chain_Nonmarking(pw, (pword *)ref);
    }
}


/*
 * Scan the trail for locations that have been bound since the creation
 * of the GCB choicepoint, and use these locations as marking roots.
 *
 * Because of value trailing, it it possible to encounter multiple
 * trail entries for the same location.  These may be several
 * value-trails, or one address-trail plus one or more value-trails. 
 * Since our marking process is destructive, we cannot mark twice from
 * the same location.  To avoid this, we set the ALREADY_MARKED_FROM
 * bit in the tag of the trailed (and marked-from) location on the
 * first encounter, and suppress all subsequent marking attempts (the
 * corresponding check is in mark_from()).  These subsequent marking
 * attempts may occur either in mark_from_trail() itself or during
 * explicit marking of certain global locations in collect_stack(). 
 * The bits are reset during the second trail traversal, in
 * early_untrail().  Great care must be taken to ensure that for every
 * bit-setting in mark_from_trail() there is corresponding code in
 * early_untrail() to reset it.
 * Caution: the ALREADY_MARKED_FROM is the same physical bit as the
 * MARK bit, but there is no conflict because MARK bits are only set
 * within the current collection segment, while ALREADY_MARKED_FROM
 * bits are set only outside of it.
 */

static void
mark_from_trail(ec_eng_t *ec_eng, control_ptr GCB)
{
    register pword *gc_tg = Chp_Tg(GCB);
    register pword **limit_tt = Chp_Tt(GCB);
    pword *gc_sp = Chp_Sp(GCB);
    register pword **tr = TT;
    register pword *trailed_item;
    word i, what;

    while (tr < limit_tt)
	switch ((word) *tr & 3)
	{
	case TRAIL_ADDRESS:
	    trailed_item = *tr++;
	    if (trailed_item < gc_tg || trailed_item > gc_sp
#ifdef AS_EMU
		|| (trailed_item > TG_LIM && trailed_item < spmax_)
#endif
	    )
	    {
		Mark_from(trailed_item->tag.kernel, trailed_item, NO)
		Set_Bit(ALREADY_MARKED_FROM, trailed_item);
	    }
	    break;
	case TRAIL_TAG:
	    trailed_item = *(tr+1);
	    tr += 2;
	    if (trailed_item < gc_tg || trailed_item > TG_LIM)
	    {
		Mark_from(trailed_item->tag.kernel, trailed_item, NO)
		Set_Bit(ALREADY_MARKED_FROM, trailed_item);
	    }
	    break;
	case TRAIL_MULT:
	    i = (word) *tr++;
	    trailed_item = (pword *)((uword *)(*tr++) + TrailedOffset(i));
	    what = TrailedType(i);
	    i = TrailedNumber(i);
	    if (trailed_item < gc_tg || trailed_item > TG_LIM)
	    {
		if (what == TRAILED_PWORD)
		{
		    i /= 2;
		    if (i > 0)
		    {
			do
			{
			    Mark_from(((pword*)tr)->tag.kernel,
							((pword*)tr), NO);
			    if (trailed_item < gc_tg || trailed_item > gc_sp)
				Mark_from(trailed_item->tag.kernel,
							trailed_item, NO);
			    trailed_item++;
			    tr = (pword **)((pword*)tr + 1);
			} while (i--);
		    }
		    else
		    {
			/* Mark only from the current value, the old
			 * value is handled later in early_untrail()
			 */
			if (trailed_item < gc_tg || trailed_item > gc_sp)
			{
			    Mark_from(trailed_item->tag.kernel,
						    trailed_item, NO);
			    Set_Bit(ALREADY_MARKED_FROM, trailed_item);
			}
			tr = (pword **)((pword*)tr + 1);
		    }
		}
		else if (what == TRAILED_REF || what == TRAILED_COMP)
		{
		    word trailed_tag = trailed_item->tag.kernel;
#ifdef DEBUG_GC
		    if ((what == TRAILED_REF && !IsTag(trailed_tag,TVAR_TAG))
			|| (what == TRAILED_COMP && !IsTag(trailed_tag,TCOMP)))
		    {
			_gc_error("Illegal TRAILED_REF or TRAILED_COMP");
		    }
#endif
		    if (i > 0)
			do
			{
			    Mark_from_pointer(trailed_tag, ((pword*)tr), NO); /* old */
			    if (trailed_item < gc_tg || trailed_item > gc_sp)
				Mark_from_pointer(trailed_tag, trailed_item, NO);
			    trailed_item++;
			    tr++;
			} while (i--);
		    else
		    {
			/* Mark only from the current value, the old
			 * value is handled later in early_untrail()
			 */
			if (trailed_item < gc_tg || trailed_item > gc_sp)
			{
			    Mark_from_pointer(trailed_tag, trailed_item, NO);
			    Set_Bit(ALREADY_MARKED_FROM, trailed_item);
			}
			tr++;
		    }
		}
		else if (what == TRAILED_WORD32)
		    tr += i + 1;
		else
		    Print_Err1(
			"bad extension trail entry in mark_from_trail: %x\n",
			(word) *tr);
	    }
	    else	/* skip the trail entry */
		tr += i + 1;
	    break;

	case TRAIL_EXT:
	    switch (TrailedEtype(*tr))
	    {
	    case TRAIL_UNDO:
		break;

	    case TRAIL_UNDO_STAMPED:
		{
		    pword *stamp = tr[TRAIL_UNDO_STAMP_ADDRESS];
		    if (!InCurrentSegment(stamp))
		    {
			/* Mark only from the current value, the old
			 * value is handled later in early_untrail()
			 */
			Mark_from(stamp->tag.kernel, stamp, NO);
			Set_Bit(ALREADY_MARKED_FROM, stamp);
		    }
		}
		break;

/**** BEGIN EXTENSION SLOT ****

Name:	GC_MARK_TRAIL

Parameters:
	pword **tr      points to extension trail frame

Code Template:
	    case TRAIL_EXTENSION:
		if the trailed object is older than GCB then mark from the
		new value of the trailed object. For value trails the old
		value must be used for marking as well!
		break;

****** END EXTENSION SLOT *****/

	    default:
		Print_Err("unknown extension trail frame type in mark_from_trail\n");
		break;
	    }
	    tr += TrailedEsize(*tr);
	    break;
	}
}

static void
mark_from_references(ec_eng_t *ec_eng)
{
    {
	ec_ref ref = ec_eng->allrefs.next;
	while(ref != &ec_eng->allrefs)
	{
	    Mark_from(ref->var.tag.kernel, &ref->var, NO)
	    ref = ref->next;
	}
    }
    {
	globalref *ref = ec_eng->references;
	for(; ref; ref=ref->next)
	{
	    Mark_from_pointer(TREF, &ref->ptr, NO)
	}
    }
}


/*
 * The basic marking procedure. It should not be called directly, 
 * but the macro Mark_from() should always be used.
 *
 * ref	points to the word that has the reference.
 *	It is NOT always the value part of a pword !
 * tag	is the type of this reference (Ref or Compound tag)
 * ref_in_segment is YES, if the reference is within the
 *	collection segment, NO otherwise.
 *
 * NOTE: ref->tag may be already overwritten and hence different from tag
 *	or it may not even exist (eg. references from the trail)
 *
 *	Recursion has been removed using an explicit stack on the local.
 */

#define Pdl_Init()	pword *pdl_bottom = SP
#define Pdl_Empty()	(SP == pdl_bottom)
#define Pdl_Arity()	SP->tag.kernel
#define Pdl_Target()	SP->val.ptr
#define Pdl_Pop()	++SP
#define Pdl_Push(i,t) { \
	if (--SP <= ec_eng->sp_limit && local_ov(ec_eng)) \
	    ec_panic("Out of local stack space","garbage collection"); \
	SP->tag.kernel = (i); \
	SP->val.ptr = (t); \
    }


static void
mark_from(
	ec_eng_t *ec_eng,	/* engine */
	word tag,		/* type of the reference */
	pword *ref,		/* location of the reference */
	int ref_in_segment)	/* true if ref is in the current segment */
{
    register pword *target;
    register word target_tag;
    register int i;

    Pdl_Init();

    /*
     * If the reference is from outside the collection segment, we may
     * already have used it for marking. In this case, ignore it now.
     */
    if (!ref_in_segment && AlreadyMarkedFrom(tag))
	return;
    	
    for(;;)		/* tail recursion loop */
    {
	target = ref->val.ptr;
	if (!InCurrentSegment(target))
	    goto _return_;

	target_tag = target->tag.kernel;	/* save the original tag */

	if (ref_in_segment && ref < target)
	{
	    Set_Bit(MARK, target)
	}
	else	/* a reference from outside into the current segment	*/
		/* or a down-pointer within the current segment		*/
	{
	    Into_Reloc_Chain(target, ref)
	}

	/*
	 * CAUTION: the tag of the target is now destroyed !
	 * It is still available in target_tag.
	 */

	if (ISRef(tag) && ref != target)	/* handling of untyped references	*/
	{
	    if (Marked(target_tag))
		goto _return_;
	    Check_Tag(target_tag)
	    Check_Tag_Range(target_tag)
	    /* Mark_from(target_tag, target, YES) */
	}
	else switch(TagTypeC(tag))	/* handling of typed pointers	*/
	{

	case TLIST:
	case TRAT:
	case TMETA:			/* self reference or from trail */
	case THANDLE:
	    if (!Marked(target_tag))
	    {
		Check_Tag(target_tag)
		/* Mark_from(target_tag, target, YES) */
		if (ISPointer(target_tag))
		{
		    Pdl_Push(1,target+1);
		    goto _mark_from_pointer_;
		}
	    }
	    target_tag = (++target)->tag.kernel;
	    if (Marked(target_tag))
		goto _return_;
	    Check_Tag(target_tag)
	    Set_Bit(MARK, target)
	    /* Mark_from(target_tag, target, YES) */
	    break;

	case TCOMP:
	    if (Marked(target_tag))
		goto _return_;		/* the structure is already marked as a whole */
	    Check_Tag(target_tag)
	    Check_Functor(target_tag)
	    i = DidArity(target->val.did);
	    ++target;
	    goto _mark_pwords_;		/* (i,target) */

	case TVAR_TAG:
	case TNAME:
	case TUNIV:
	    if (Marked(target_tag))
		goto _return_;
	    Check_Tag(target_tag)
	    /* Mark_from(target_tag, target, YES) */
	    break;

	case TSUSP:
	    if (!(tag & MARK_FULL_DE))
	    {
		if (Marked(target_tag))
		    goto _return_;
		Check_Tag(target_tag)
		Check_Susp(target_tag)
		/*
		 * mark suspensions according to their woken bit,
		 * either completely or only the header
		 */
		if (SuspTagDead(target_tag))
		    goto _return_;
	    }
	    else if (!Marked(target_tag))
	    {
		Check_Susp(target_tag)
	    }
	    /* mark the subsequent pwords: state, goal, module */
	    i = SUSP_SIZE - SUSP_HEADER_SIZE;
	    target += SUSP_HEADER_SIZE;
	    goto _mark_pwords_;		/* (i,target) */

	case TDBL:
	case TBIG:
	case TIVL:
	case TSTRG:
	case TEXTERN:
	case TPTR:
	    goto _return_;		/* nothing to mark recursively	*/

/**** BEGIN EXTENSION SLOT ****

Name:	GC_MARK_TYPED_POINTER

Desc:	The target item is referenced by a TEXTENSION_POINTER pointer.
	The target tag is already overwritten, but still available in
	target_tag. The code here should recursively mark what is
	referenced by the pointed-to item.

Parameters:
	word target_tag		Tag and address of the first pword
	pword *target			referenced by the typed pointer

Code Template:
	case TEXTENSION_POINTER:
	    Set the MARK bit and call Mark_from() for all pwords
	    contained in the referenced item and Mark_from_pointer()
	    for all potential references into the global stack.
	    The tail recursive call should be replaced by break;
	    If there is nothing to mark recursively: goto _return_;

****** END EXTENSION SLOT *****/

	default:
	    Print_Err1("bad pointer tag (%x) in mark_from\n", tag);
	    ec_flush(current_err_);
	    break;
	}

/* _mark_from_: */		/* Mark_from(target_tag, target, YES) */
	if (!ISPointer(target_tag))
	    goto _return_;

_mark_from_pointer_:		/* mark_from(target_tag, target, YES) */
	tag = target_tag;	/* setup parameters for tail recursion	*/
	ref = target;
	ref_in_segment = YES;
	continue;

_return_:
	if (Pdl_Empty())
	    return;
	i = Pdl_Arity();
	target = Pdl_Target();
	Pdl_Pop();

_mark_pwords_:			/* (i, target) */
	while(i-- > 0)
	{
	    target_tag = target->tag.kernel;
	    if (!Marked(target_tag))
	    {
		Check_Tag(target_tag)
		Set_Bit(MARK, target)
		/* Mark_from(target_tag, target, YES) */
		if (ISPointer(target_tag))
		{
		    if (i>0) { Pdl_Push(i,target+1); }
		    goto _mark_from_pointer_;
		}
	    }
	    ++target;
	}
	goto _return_;

    } /* end for */
}


/*-------------------------------------------------------------------
 * compaction phase
 *-------------------------------------------------------------------*/

/*
 * Compact the global stack in one bottom-up pass, updating the relocation
 * chains on-the-fly.
 * Note that, if there was no garbage, the items are copied onto themselves.
 * Otherwise, the destination is at least 1 pword below.
 */

static void
compact_and_update(ec_eng_t *ec_eng)
{
    register pword *current, *compact, *ref;
    register word link_or_tag, current_tag;

    current = compact = GCTG;
    while (current < TG)
    {
	link_or_tag = current_tag = current->tag.kernel;
	/* first update the relocation chain, if any	*/
	while (IsLink(link_or_tag))
	{
	    ref = LinkToPointer(link_or_tag);
	    link_or_tag = ref->val.all;
	    ref->val.ptr = compact;
	}

	if (ISPointer(link_or_tag))
	{
	    if (Marked(current_tag))
	    {
		compact->tag.kernel = link_or_tag & ~MARK;
		if ((ref = current->val.ptr) > current && ref < TG)
		{
		    Into_Reloc_Chain(ref,compact)
		}
		else
		    compact->val.all = current->val.all;
		compact++;
	    }
	    current++;
	}
	else if (!ISSpecial(link_or_tag))	/* simple types */
	{
	    if (Marked(current_tag))
	    {
		compact->tag.kernel = link_or_tag & ~MARK;
		(compact++)->val.all = current->val.all;
	    }
	    current++;
	}
	else
	    switch (TagTypeC(link_or_tag))
	    {
	    case TDE:	/* treat suspension, except goal and module field */
		if (Marked(current_tag)) {
		    compact->tag.kernel = link_or_tag & ~MARK;
		    if ((ref = current->val.ptr) > current)	/* LD link */
		    {
#ifdef DEBUG_GC
			/* this case should never occur: LD goes down */
			_gc_error("LD list corrupted (5)\n");
#endif
			Into_Reloc_Chain(ref,compact)
		    }
		    else
			compact->val.all = current->val.all;
		    compact[SUSP_PRI] = current[SUSP_PRI];
		    compact[SUSP_INVOC] = current[SUSP_INVOC];
		    compact += SUSP_HEADER_SIZE;
		}
		current += SUSP_HEADER_SIZE;
		break;

	    case TEXTERN:
		if (Marked(current_tag))
		{
		    compact->tag.kernel = link_or_tag & ~MARK;
		    (compact++)->val.all = current->val.all;
		    *compact++ = current[1];
		}
		current += 2;
		break;

	    case TBUFFER:
		if (Marked(current_tag))
		{
		    int i = BufferPwords(current);
		    compact->tag.kernel = link_or_tag & ~MARK;
		    (compact++)->val.all = (current++)->val.all;
		    do
			*compact++ = *current++;
		    while (--i > 1);
		}
		else
		    current += BufferPwords(current);
		break;

/**** BEGIN EXTENSION SLOT ****

Name:	GC_COMPACT

Parameters:
	current	 	old address of the object
	compact	 	new address of the object

Code Template:
	    case TEXTENSION:
		if (Marked(current_tag))
		{
		    copy the object down from current to compact;
		    if it contains pointers UP the global stack,
		    these must be entered into a relocation chain
		    rather than copied
		}
		else
		{
		    skip the object by incrementing current
		}
		break;

****** END EXTENSION SLOT *****/

	    default:
		Print_Err1("illegal tag (%d) in compact_and_update\n",
		    (word) TagTypeC(link_or_tag));
		ec_flush(current_err_);
		current++;
		break;
	    }
    }
#ifdef WIPE_FREE_GLOBAL
    while (compact < current)
    {
    	compact->val.ptr = 0;
	(compact++)->tag.kernel = TEND;
    }
#endif
}


/*
 * Compact the trail by copying down all the space between
 * the elements of the garbage list.
 */
static void
compact_trail(ec_eng_t *ec_eng,  pword **garbage_list)
{
    register pword **compact, **from, **to;

    End_Of_Frame(garbage_list, compact);
    from = garbage_list;
    garbage_list = (pword **)TrailedLocation(garbage_list);
    while (garbage_list) {
	End_Of_Frame(garbage_list, to);
	while (from > to)
	    *--compact = *--from;
	from = garbage_list;
	garbage_list = (pword **)TrailedLocation(garbage_list);
    }
    to = TT;
    while (from > to)
	*--compact = *--from;
    TT = compact;
}


/*
 * Set the tt fields of the control frames to their new values
 */
static void
update_trail_ptrs(ec_eng_t *ec_eng, control_ptr GCB)
{
    register control_ptr fp, top;

    fp.top = B.top;
    do {
	top.top = (fp.top - 1);
	fp.any_frame.chp = top.top->frame.chp;
	fp.chp->tt = TT + (word)(fp.chp->tt);
    } while (fp.top > GCB.top);
}


/*-------------------------------------------------------------------
 * overflow in spite of GC or in a position where no GC can be done
 *-------------------------------------------------------------------*/

/*
 * TT has grown below TT_LIM
 *
 * We first trigger a gc and reduce the gap from TRAIL_GAP to GLOBAL_TRAIL_GAP.
 * The gc will hopefully reduce the trail. If not, we get a second overflow,
 * then we allocate a new page.
 */

#define	TRAIL_GAP	(GLOBAL_TRAIL_GAP + 128)

void
trail_ov(ec_eng_t *ec_eng)
{
    TT_LIM = (pword **)
	    ((pword *) ec_eng->global_trail[1].end + GLOBAL_TRAIL_GAP);
    if (TT > TT_LIM)
    {
	/* There is still some space, schedule a global stack collection only
	 */
	if (TG_SLS > TG)
	{
	    Restore_Tg_Soft_Lim(TG)
	}
	return;
    }

    /* grow the trail */
    if (!adjust_stacks(ec_eng->global_trail,
	    ec_eng->global_trail[0].end,
	    (uword *) ((pword *) TT - TRAIL_GAP), 0))
    {
	/* stacks collide, make a last try with shrinking the global */
	if (!adjust_stacks(ec_eng->global_trail,
		(uword *) (TG + GLOBAL_TRAIL_GAP),
		(uword *) ((pword *) TT - TRAIL_GAP), 0))
	{
	    ov_reset(ec_eng);		/* give up */
	}
	Set_Tg_Lim((pword *) ec_eng->global_trail[0].end - GLOBAL_TRAIL_GAP)
    }
    TT_LIM = (pword **)
	    ((pword *) ec_eng->global_trail[1].end + TRAIL_GAP);
    return;
}

/*
 * TG has grown above TG_LIM (and above TG_SL)
 * Should happen only outside the emulator (when no GC can be done)
 * or due to some erroneous big allocation inside the emulator.
 * We increase TG_LIM as much as necessary. This is first tried
 * without, and if that fails, with shrinking the trail.
 */
void
global_ov(ec_eng_t *ec_eng)
{
    if (final_overflow(ec_eng))
	ov_reset(ec_eng);
}


/*
 * The same as global_ov(), but returns true or false
 */

int
final_overflow(ec_eng_t *ec_eng)
{
    if (!adjust_stacks(ec_eng->global_trail,
	    (uword *) (TG + GLOBAL_TRAIL_GAP + 1), /* +1 to avoid looping */
	    ec_eng->global_trail[1].end, 0))
    {
	/* stacks collide, make a last try with shrinking the trail */
	if (!adjust_stacks(ec_eng->global_trail,
		(uword *) (TG + GLOBAL_TRAIL_GAP + 1),
		(uword *) ((pword *) TT - TRAIL_GAP), 0))
	{
	    return 1;
	}
	TT_LIM = (pword **)
	    ((pword *) ec_eng->global_trail[1].end + TRAIL_GAP);
    }
    Set_Tg_Lim((pword *) ec_eng->global_trail[0].end - GLOBAL_TRAIL_GAP)
    return 0;
}


/*
 * SP has grown below sp_limit
 */

int
local_ov(ec_eng_t *ec_eng)
{
    if (!adjust_stacks(ec_eng->control_local,
	    ec_eng->control_local[0].end,
	    (uword *) (SP - LOCAL_CONTROL_GAP), 0))
    {
	if (!adjust_stacks(ec_eng->control_local,
		(uword *) (B.args + LOCAL_CONTROL_GAP),
		(uword *) (SP - LOCAL_CONTROL_GAP), 0))
	{
	    return 1;
	}
	ec_eng->b_limit =
	    (pword *) ec_eng->control_local[0].end - LOCAL_CONTROL_GAP;
    }
    ec_eng->sp_limit = (pword *) ec_eng->control_local[1].end + LOCAL_CONTROL_GAP;
    return 0;
}

int
control_ov(ec_eng_t *ec_eng)
{
    if (!adjust_stacks(ec_eng->control_local,
	    (uword *) (B.args + LOCAL_CONTROL_GAP),
	    ec_eng->control_local[1].end, 0))
    {
	if (!adjust_stacks(ec_eng->control_local,
		(uword *) (B.args + LOCAL_CONTROL_GAP),
		(uword *) (SP - LOCAL_CONTROL_GAP), 0))
	{
	    return 1;
	}
	ec_eng->sp_limit =
	    (pword *) ec_eng->control_local[1].end + LOCAL_CONTROL_GAP;
    }
    ec_eng->b_limit = (pword *) ec_eng->control_local[0].end - LOCAL_CONTROL_GAP;
    return 0;
}


/*
 * Adjust the stacks such that the global stack has space for margin pwords.
 * Return 0 if that was not possible.
 * Set TG_LIM and TT_LIM according to new stack sizes, leaving proper gaps.
 * @return	1 if ok
 *		0 if no adjustment, or only partial adjustment
 */

int
trim_global_trail(ec_eng_t *ec_eng, uword margin)
{
    pword *tg_new, *tt_new, *split_at;
    uword ratio;
    int res = 1;

    /* compute the current global/trail ratio (careful with boundary conditions) */
    /* for small stacks this approaches ratio 32 = 32000/1000 */
    ratio = ((uword*)TG - (uword*)TG_ORIG + 32000) / ((uword*)TT_ORIG - (uword*)TT + 1000);
    if (ratio == 0) ratio = 1;

    Safe_Add_To_Pointer(TG, margin + GLOBAL_TRAIL_GAP, (pword *) TT, tg_new);
    Safe_Sub_From_Pointer((pword *) TT, margin/ratio + TRAIL_GAP, (pword *) TG, tt_new);
    /* first try to grow global and trail proportionally */
    if (!adjust_stacks(ec_eng->global_trail, (uword*) tg_new, (uword *) tt_new, 0))
    {
	Store_Eng_OSError();
	/* try without accommodating margin, just partition the remaining
	 * space, roughly preserving the current trail/global ratio
	 */
	res = 0;
	split_at = (pword *) TT - ((pword *) TT - TG)/(ratio + 1);
	tg_new = TG + GLOBAL_TRAIL_GAP;
	tt_new = (pword *) TT - TRAIL_GAP;

	if (!adjust_stacks(ec_eng->global_trail, (uword*) tg_new, (uword*) tt_new, (uword *) split_at))
	{
	    Store_Eng_OSError();
	    return res;
	}
	/* else partial adjustment, still return error code */
    }
    /* the following will also adjust TG_SL if necessary */
    Set_Tg_Lim((pword *) ec_eng->global_trail[0].end - GLOBAL_TRAIL_GAP)
    TT_LIM = (pword **) ((pword *) ec_eng->global_trail[1].end + TRAIL_GAP);
    return res;
}


/*
 * Adjust local control to have some default space above the stack tops
 */
#define LOCAL_CONTROL_DEFAULT	LOCAL_CONTROL_GAP
int
trim_control_local(ec_eng_t *ec_eng)
{
    if (!adjust_stacks(ec_eng->control_local,
	    (uword *) (B.args + LOCAL_CONTROL_DEFAULT),
	    (uword *) (SP - LOCAL_CONTROL_DEFAULT), 0))
    {
	return 0;
    }
    ec_eng->b_limit = (pword *) ec_eng->control_local[0].end - LOCAL_CONTROL_GAP;
    ec_eng->sp_limit = (pword *) ec_eng->control_local[1].end + LOCAL_CONTROL_GAP;
    return 1;
}


static void
ov_reset(ec_eng_t *ec_eng)
{
    pword exit_tag;
    Make_Atom(&exit_tag, d_.global_trail_overflow);
    Exit_Block(exit_tag.val, exit_tag.tag);
}


/*-------------------------------------------------------------------
 * Marking routines for dictionary GC
 *-------------------------------------------------------------------*/

/*
 * Mark the DIDs in a consecutive block of pwords. This block may be in
 * the Prolog stacks or on the heap. Note that we do not follow references
 * and the like, we just scan the block once, looking for atoms, functors
 * (TDICT tags) and variable names.
 */

void
mark_dids_from_pwords(/*noengine*/ pword *from, register pword *to)
{
    register pword *pw = from;
    dident a;

    while (pw < to)
    {
	switch (TagType(pw->tag))
	{
	case TDICT:			/* mark atoms and functors */
	    if ((a = pw->val.did) != D_UNKNOWN)
	    {
		Mark_Did(a);
	    }
	    else
	    {
		Print_Err("Undefined atom or functor");
	    }
	    pw++;
	    break;

	case TSTRG:
	    /* handle persistent strings by marking the corresponding atom */
	    if (StringInDictionary(pw->val))
	    {
		a = check_did_n(StringStart(pw->val), StringLength(pw->val), 0);
		if (a != D_UNKNOWN)
		{
		    Mark_Did(a);
		}
		else
		{
		    Print_Err("No atom corresponding to persistent string");
		}
	    }
	    pw++;
	    break;

	case TNAME:			/* mark variable names */
	case TMETA:
	case TUNIV:
	    if (IsNamed(pw->tag.kernel))
	    {
		Mark_VarName(pw->tag.kernel);
	    }
	    pw++;
	    break;

	case TDE:
	    pw += SUSP_HEADER_SIZE;
	    break;

	case TBUFFER:
	    pw += BufferPwords(pw);
	    break;

	case TEXTERN:
	    if (IsTag(pw[1].tag.kernel, TPTR))
	    {
		if (ExternalClass(pw)->mark_dids && ExternalData(pw))
		{
		    ExternalClass(pw)->mark_dids(ExternalData(pw));
		}
		pw += 2;
	    }
	    else
	    {
		Print_Err("TEXTERN not followed by TPTR");
		pw += 1;
	    }
	    break;

/**** BEGIN EXTENSION SLOT ****

Name:	GC_MARK_DIDS_FROM_PWORDS

Parameters:
	pw	 	pword to mark from

Code Template:
	case TEXTENSION:
	    If object contains dictionary references, call Mark_Did()
	    or Mark_VarName() and increment pw as needed.
	    If no dictionary references, only increment pw.

****** END EXTENSION SLOT *****/

	default:			/* skip other pword-sized stuff */
	    pw++;
	    break;
	}
    }
}


void
mark_local_conservative(/*noengine*/ pword *from, pword *to)
{
    word *p;

    for(p = (word*) from; p <= (word*)(to-1); ++p)
    {
	switch (TagType(((pword*)p)->tag))
	{
	case TDICT:			/* mark atoms */
	    ec_mark_did_conservative(((pword*)p)->val.did);
	    break;

	case TSTRG:
	    ec_mark_string_conservative(((pword*)p)->val.ptr);
	    break;

	/* we assume no TNAME (or other named variable) tags in environments */
	}
    }
}


/**
 * Mark dictionary items that are referenced from the given engine,
 * i.e. stacks and other engine-related list and fields.
 * There is NO need to mark:
 * - source_pos fields in the debug information (these are DICT_CODE_REF)
 * 
 */
void
mark_dids_from_stacks(ec_eng_t *ec_eng, word arity)
{

    globalref *gref;

    /* Various engine fields and lists */
    Mark_Did(ec_eng->default_module);
    mark_dids_dynamic_event_queue(ec_eng);
    for (gref=ec_eng->references; gref; gref=gref->next) {
	Mark_Did(gref->name);
	Mark_Did(gref->module);
	/* ignore gref->ptr, as it always points into the global stack */
    }
    if (ec_eng->storage)
	heap_htable_tid.mark_dids(ec_eng->storage);
    if (ec_eng->report_to)
	heap_rec_header_tid.mark_dids(ec_eng->report_to);
    
    if (B.args == B_ORIG) {
	/* This is the case where DGC is called from ec_emu_fini():
	 * The control stack is already gone. Just mark arguments and global.
	 */
	mark_dids_from_pwords(&A[1], &A[arity+1]);
	mark_dids_from_pwords(TG_ORIG, TG);
	return;
    }

    /* auxiliary choicepoint to take care of the active arguments */

    make_choicepoint(ec_eng, arity);

    /* global */

    mark_dids_from_pwords(TG_ORIG, TG);


    /* trail */

    {
	register pword **tt = TT;
	word	i;

	while(tt < TT_ORIG)
	{
	    switch((((word) *tt) & 3))
	    {
	    case TRAIL_ADDRESS:
		break;
	    case TRAIL_TAG:
		if (IsNamed(TrailedTag(*tt)))
		{
		    Mark_VarName(TrailedTag(*tt));
		}
		break;
	    case TRAIL_MULT:
		i = (word) *tt;
		switch (TrailedType(i))
		{
		case TRAILED_PWORD:
		    mark_dids_from_pwords((pword *) (tt+2),
					(pword *) (tt+3+TrailedNumber(i)));
		    break;
		}
		break;
	    case TRAIL_EXT:
		i = (word) *tt;
		switch (TrailedEtype(i))
		{
		case TRAIL_UNDO:
		    switch (TrailedType(i))
		    {
		    case TRAILED_PWORD:
			mark_dids_from_pwords(
			    (pword *) (tt+TRAIL_UNDO_SIMPLE_HEADER_SIZE),
			    (pword *) (tt+TrailedEsize(i)));
		    break;
		    }
		    break;
		case TRAIL_UNDO_STAMPED:
		    /* TRAIL_UNDO_STAMP_ADDRESS and TRAIL_UNDO_OLDSTAMP
		     * don't contain dids and don't need to be marked */
		    switch (TrailedType(i))
		    {
		    case TRAILED_PWORD:
			mark_dids_from_pwords(
			    (pword *) (tt+TRAIL_UNDO_STAMPED_HEADER_SIZE),
			    (pword *) (tt+TrailedEsize(*tt)));
		    break;
		    }
		    break;
		default:
		    break;
		}
		break;
	    }
	    End_Of_Frame(tt, tt);
	}
    }
    

    /* control & local */

    {
	control_ptr	fp, top;
	pword		*mergepoint;
	word		edesc;

	mergepoint = ((invoc_ptr) (B_ORIG + SAFE_B_AREA))->e;
	top.top = B.top - 1;		/* find first full frame	*/
	fp.any_frame = top.top->frame;

	for (;;)	/* loop through all control frames, except the bottom one */
	{
	    if (IsRetryMeInlineFrame(top.top))
	    {
		edesc = EnvDescPP(top.top->backtrack + RETRY_ME_INLINE_SIZE - 1);
	    }
	    else if (IsTrustMeInlineFrame(top.top))
	    {
		edesc = EnvDescPP(top.top->backtrack + TRUST_ME_INLINE_SIZE - 1);
	    }
	    else if (IsRetryInlineFrame(top.top))
	    {
		edesc = EnvDescPP(top.top->backtrack + RETRY_INLINE_SIZE - 1);
	    }
	    else if (IsTrustInlineFrame(top.top))
	    {
		edesc = EnvDescPP(top.top->backtrack + TRUST_INLINE_SIZE - 1);
	    }
	    else if (IsNestingFrame(top.top))
	    {
		/* a Prolog call from within a C external */
		mark_dids_from_pwords(&fp.invoc->arg_0, top.args);
		edesc = EnvDesc(fp.chp->sp);
	    }
	    else if (IsRecursionFrame(top.top))
	    {
		break;
	    }
	    else if (IsExceptionFrame(top.top))
	    {
		/* This is different because we don't know the size and activity
		 * of the first environment.  We therefore employ "conservative"
		 * marking over the whole space fp->sp..fp->e, i.e. mark anything
		 * that "looks like" a valid dictionary reference.
		 */
		mark_dids_from_pwords((pword *)(fp.exception + 1), top.args);
		mark_local_conservative(fp.exception->sp, fp.exception->e);
		edesc = Esize(0);	/* taken care of above */
	    }
	    else if (IsParallelFrame(top.top))
	    {
		mark_dids_from_pwords((pword *)(fp.chp_par + 1), top.args);
		edesc = EnvDesc(fp.chp_par->sp);
	    }
	    else /* if (IsChoicePoint(top.top)) */
	    {
		mark_dids_from_pwords((pword *)(fp.chp + 1), top.args);
		edesc = EnvDesc(fp.chp->sp);
	    }

	    top.top = fp.top - 1;		/* find next full frame	*/

	    Walk_Env_Chain(pslot, /* (fp,mergepoint,edesc) */
		mark_dids_from_pwords(pslot, pslot+1);
	    )

	    fp.any_frame = top.top->frame;
	}

	if (fp.args == B_ORIG + SAFE_B_AREA)
	{
	    mark_dids_from_pwords(&fp.invoc->arg_0, top.args);
	}
	else
	{
	    Print_Err("bad bottom frame in mark_dids_from_stacks()\n");
	}
    }

    drop_choicepoint(ec_eng);
}


/*-------------------------------------------------------------------
 * Copying/Shifting stacks
 *-------------------------------------------------------------------*/

/* Add a word-offset to a pointer */
#define WOff(p,off) ((uword*)(p)+(off))

/* The following macros refer implicitly to ec_eng and xxx_off! */

/* Copy source engine's pointer ps to destination engine's pointer pd.
 * (handles pointers to global, local, heap, or NULL)
 */
#define Clone_Pointer(ps, pd) {\
    if ((ps) >= TG_ORIG) {\
	if ((ps) < TG)              (pd) = (pword*)WOff(ps,tg_off);\
	else if ((ps) >= SP_ORIG)   (pd) = (ps);\
	else if ((ps) >= SP)	    (pd) = (pword*)WOff(ps,sp_off);\
	else assert(0);\
    } else (pd) = (ps);\
}

/* Copy source engine's pword *s to destination engine's pword *d */
#define Clone_Pw(s, d) {\
    (d)->tag = (s)->tag;\
    if (ISPointer((s)->tag.kernel)) Clone_Pointer((s)->val.ptr, (d)->val.ptr)\
    else if (IsTag((s)->tag.kernel,TCUT) && B_ORIG <= (s)->val.ptr && (s)->val.ptr < B.args)\
        (d)->val.ptr = (pword*) WOff((s)->val.ptr, b_off);\
    else (d)->val = (s)->val;\
}

/* If pointer pd still points to a source engine location, adjust it
 * to point to the destintation engine location instead (sp_off/tg_off).
 * CAUTION: these operations must be idempotent, i.e. when
 * the pointer is already adjusted, don't touch it again!
 */
#define Adjust_Pointer(pd) {\
    if ((pd) >= TG_ORIG) {\
        if ((pd) < TG)	            (pd) = (pword*)WOff(pd,tg_off);\
	else if ((pd) >= SP_ORIG)   ;\
	else if ((pd) >= SP)	    (pd) = (pword*)WOff(pd,sp_off);\
	else assert(0);\
    }\
}

/* Adjust destination engine's pword *d */
#define Adjust_Pw(d) {\
    if (ISPointer((d)->tag.kernel)) Adjust_Pointer((d)->val.ptr)\
    else if (IsTag((d)->tag.kernel,TCUT) && B_ORIG <= (d)->val.ptr && (d)->val.ptr < B.args)\
        (d)->val.ptr = (pword*) WOff((d)->val.ptr, b_off);\
}


void
shift_stacks(ec_eng_t *ec_eng, int copy, word b_off, word sp_off, word tg_off, word tt_off)
{

    /* Global stack */
    {
        pword *src = TG_ORIG;
        pword *dest = (pword*) WOff(src, tg_off);

        while(src < ec_eng->tg)
        {
            switch(TagType(src->tag))
            {
            case TDE:	/* treat suspension header (rest is pwords) */
                Clone_Pointer(src[SUSP_LD].val.ptr, dest[SUSP_LD].val.ptr);
                dest[SUSP_FLAGS].tag = src[SUSP_FLAGS].tag;
                dest[SUSP_PRI] = src[SUSP_PRI];
                dest[SUSP_INVOC] = src[SUSP_INVOC];
                src += SUSP_HEADER_SIZE;
                dest += SUSP_HEADER_SIZE;
                break;

            case TEXTERN:
                dest[0] = src[0];
                dest[1] = src[1];
                /* copy, but don't trail, assuming trail will also be copied! */
                if (copy && ExternalClass(src)->copy && ExternalData(src))
                    dest[1].val.ptr = (pword *) ExternalClass(src)->copy(ExternalData(src));
                src += HANDLE_ANCHOR_SIZE;
                dest += HANDLE_ANCHOR_SIZE;
                break;

            case TBUFFER:
            {
                int i = BufferPwords(src);
                do
                    *dest++ = *src++;
                while (--i > 0);
                break;
            }

            default:
                Clone_Pw(src, dest);
                ++src; ++dest;
                break;
            }
        }
    }

    /* Trail stack */
    {
        int trailed_type, trailed_etype;
        pword **tt = TT;
        pword **dest_tt = tt + tt_off;
        pword **end_of_frame;

        while(tt < TT_ORIG)
        {
            switch((((word) *tt) & 3))
            {
            case TRAIL_ADDRESS:
                Clone_Pointer(*tt, *dest_tt);
                tt++; dest_tt++;
                break;
            case TRAIL_TAG:
                *dest_tt++ = *tt++;
                Clone_Pointer(*tt, *dest_tt);
                tt++; dest_tt++;
                break;
            case TRAIL_MULT:
                End_Of_Frame(tt, end_of_frame);
                trailed_type = TrailedType(*tt);
                *dest_tt++ = *tt++;
                Clone_Pointer(*tt, *dest_tt);
                tt++; dest_tt++;
    _clone_trailed_data_:
                switch (trailed_type)
                {
                case TRAILED_PWORD:
                    while (tt < end_of_frame) {
                        Clone_Pw((pword*)tt, (pword*)dest_tt);
                        tt += 2; dest_tt += 2;
                    }
                    break;
                case TRAILED_WORD32:
                    while (tt < end_of_frame) {
                        *dest_tt++ = *tt++;
                    }
                    break;
                case TRAILED_REF:
                case TRAILED_COMP:
                    while (tt < end_of_frame) {
                        Clone_Pointer(*tt, *dest_tt);
                        tt++; dest_tt++;
                    }
                    break;
                }
                break;
            case TRAIL_EXT:
                End_Of_Frame(tt, end_of_frame);
                trailed_type = TrailedType(*tt);
                trailed_etype = TrailedEtype(*tt);
                *dest_tt++ = *tt++;                 /*TRAIL_UNDO_FLAGS*/
                Clone_Pointer(*tt, *dest_tt);       /*TRAIL_UNDO_ADDRESS*/
                tt++; dest_tt++;
                *dest_tt++ = *tt++;                 /*TRAIL_UNDO_FUNCT*/
                switch (trailed_etype)
                {
                case TRAIL_UNDO:
                    goto _clone_trailed_data_;
                case TRAIL_UNDO_STAMPED:
                    Clone_Pointer(*tt, *dest_tt);   /*TRAIL_UNDO_STAMP_ADDRESS*/
                    tt++; dest_tt++;
                    Clone_Pointer(*tt, *dest_tt);   /*TRAIL_UNDO_OLDSTAMP*/
                    tt++; dest_tt++;
                    goto _clone_trailed_data_;
                }
                assert(0);
                break;
            }
        }
    }

    /* The local stack cannot be traversed easily. Instead,
     * we first copy all of it verbatim, then fix pointers later.
     */
    memcpy((void*)WOff(SP,sp_off), (void*)SP, (char*)SP_ORIG - (char*)SP);

    /* copy control stack and fix pointers in environment slots */
    {
	control_ptr top;
	pword *mergepoint = ((invoc_ptr) (B_ORIG + SAFE_B_AREA))->e;
	top.top = B.top - 1;

	for (;;)	/* loop through all control frames */
	{
            control_ptr	fp, dest_fp, dest_top;
            pword *pw, *ce;
            word edesc;
            int i;

            dest_top.args = (pword*) WOff(top.args, b_off);
            dest_top.top->backtrack = top.top->backtrack;
            dest_top.top->frame.args = (pword*) WOff(top.top->frame.args,b_off);

	    fp.any_frame = top.top->frame;
            dest_fp.args = (pword*) WOff(fp.args, b_off);

	    if (IsRetryMeInlineFrame(top.top))
	    {
                pw = (pword*) (fp.chp + 1);
		edesc = EnvDescPP(top.top->backtrack + RETRY_ME_INLINE_SIZE - 1);
	    }
	    else if (IsTrustMeInlineFrame(top.top))
	    {
                pw = (pword*) (fp.chp + 1);
		edesc = EnvDescPP(top.top->backtrack + TRUST_ME_INLINE_SIZE - 1);
	    }
	    else if (IsRetryInlineFrame(top.top))
	    {
                pw = (pword*) (fp.chp + 1);
		edesc = EnvDescPP(top.top->backtrack + RETRY_INLINE_SIZE - 1);
	    }
	    else if (IsTrustInlineFrame(top.top))
	    {
                pw = (pword*) (fp.chp + 1);
		edesc = EnvDescPP(top.top->backtrack + TRUST_INLINE_SIZE - 1);
	    }
	    else if (IsRecursionFrame(top.top))
	    {
                /* No nesting allowed (nesting_level==0 already checked) */
                assert(fp.args == B_ORIG + SAFE_B_AREA);
                /* Initial frame should not have saved arguments! */
                assert(fp.invoc == top.invoc-1);

                dest_fp.invoc = fp.invoc;       /* copy wholesale, then adjust pointers */
                dest_fp.invoc->ppb = (pword*) WOff(fp.invoc->ppb, b_off);
                dest_fp.invoc->pb = (pword*) WOff(fp.invoc->gb, b_off);
                Adjust_Pointer(dest_fp.invoc->eb)
                Adjust_Pointer(dest_fp.invoc->gb)
                Adjust_Pointer(dest_fp.invoc->de)
                Adjust_Pointer(dest_fp.invoc->mu)
                Adjust_Pointer(dest_fp.invoc->sv)
                Adjust_Pw(&dest_fp.invoc->wp_stamp)
                Adjust_Pw(&dest_fp.invoc->postponed_list)
                Adjust_Pw(&dest_fp.invoc->wl)
                Adjust_Pointer(dest_fp.invoc->oracle)
                Adjust_Pointer(dest_fp.invoc->gctg)
                Adjust_Pointer(dest_fp.invoc->tg_soft_lim)
                Adjust_Pointer(dest_fp.invoc->tg_before)
                Adjust_Pw(&dest_fp.invoc->arg_0)
		break;
	    }
	    else if (IsExceptionFrame(top.top))
	    {
                assert(0);
	    }
	    else if (IsParallelFrame(top.top))
	    {
                dest_fp.chp_par->alt = fp.chp_par->alt;
                dest_fp.chp_par->ppb = (pword*) WOff(fp.chp_par->ppb, b_off);
                dest_fp.chp_par->node = fp.chp_par->node;
                pw = (pword*) (fp.chp_par + 1);
		edesc = EnvDesc(fp.chp_par->sp);
	    }
	    else /* if (IsChoicePoint(top.top)) */
	    {
                pw = (pword*) (fp.chp + 1);
		edesc = EnvDesc(fp.chp->sp);
	    }

            /* copy fields common to all frames */
            dest_fp.chp->sp = (pword*) WOff(fp.chp->sp, sp_off);
            dest_fp.chp->tg = (pword*) WOff(fp.chp->tg, tg_off);
            dest_fp.chp->tt = (pword**)WOff(fp.chp->tt, tt_off);
            dest_fp.chp->e  = (pword*) WOff(fp.chp->e , sp_off);
            dest_fp.chp->ld = fp.chp->ld ? (pword*) WOff(fp.chp->ld, tg_off) : 0;

            /* copy the arguments */
            for(; pw < top.args; ++pw) {
                Clone_Pw(pw, ((pword*) WOff(pw,b_off)))
            }

            /* fix pointers in destination environment slots */
	    Walk_Env_Chain(pslot, /* (+fp,?mergepoint,?edesc) */
                pword *dest_pslot = (pword*) WOff(pslot, sp_off);
                Adjust_Pw(dest_pslot)
	    )

            /* Adjust the CE fields that link environments */
            pw = dest_fp.chp->e;
            while(pw < (pword*) WOff(SP_ORIG,sp_off)) {
                ce = PrevEnv(pw);
                if (!(SP <= ce && ce <= SP_ORIG))
                    break;      /* rest of chain already adjusted */
                ce = (pword*) WOff(ce, sp_off);
                PrevEnv(pw) = ce;
                pw = ce;
            }

	    top.top = fp.top - 1;
	}
    }
}


/*
 * Copy the state of ec_eng to the clean engine to_eng.
 *
 * @return
 *      PSUCCEED        ok
 *      ENGINE_BUSY     either engine is not in the required state
 *      SYS_ERROR       to_eng's stacks are too small
 */

int
ecl_engine_clone(ec_eng_t* ec_eng, ec_eng_t* to_eng, int arity)
{
    uword control_local_avail, global_trail_avail;      /* in words */
    uword b_need, sp_need, tg_need, tt_need;            /* in words */
    word  b_off, sp_off, tg_off, tt_off;                /* in words */
    int i;

    /* CAUTION: normal engine macros implicitly refer to ec_eng! */

    /* Check the source engine */
    if (EngIsDead(ec_eng)) return ENGINE_DEAD;
    if (!EngIsOurs(ec_eng)) return ENGINE_NOT_OWNED;
    if (!NoCleanup(ec_eng) || ec_eng->nesting_level > 1) return ENGINE_BUSY;

    /*
     * Check if the target engine can be used.
     * Engine state should be 'failed' or 'exception'.
     */
    if (EngIsDead(to_eng)) return ENGINE_DEAD;
    if (!EngIsOurs(to_eng)) return ENGINE_NOT_OWNED;
    if (!NoCleanup(to_eng) || to_eng->nesting_level > 1
      || to_eng->paused || (to_eng->event_flags & ~DICT_GC_REQUEST)
      || to_eng->followed_oracle || to_eng->pending_oracle
      || to_eng->ntry > 0 || to_eng->leaf
      || to_eng->trace_data.debug_top.val.ptr
      || to_eng->allrefs.next != &to_eng->allrefs
      || to_eng->allrefs.prev != &to_eng->allrefs
      || !IsInteger(to_eng->a[1].tag)
      || !(to_eng->a[1].val.nint == PFAIL || to_eng->a[1].val.nint == PTHROW))
    {
        return ENGINE_BUSY;
    }

    /* Prepare/resize the target engine's stacks (already allocated).
     * We look only at the source engine's xx_limit registers, even
     * though that may indicate more space than is actually needed.
     * We could instead set the stack pointers and then use the
     * trim_xxx() functions.
     */
    b_need  = (uword*)(ec_eng->b_limit  + LOCAL_CONTROL_GAP) - (uword*)B_ORIG;
    sp_need = (uword*)SP_ORIG - (uword*)(ec_eng->sp_limit - LOCAL_CONTROL_GAP);
    tg_need = (uword*)(ec_eng->tg_limit + GLOBAL_TRAIL_GAP)  - (uword*)TG_ORIG;
    tt_need =  (uword*)TT_ORIG - (uword*)((pword*)ec_eng->tt_limit - TRAIL_GAP);
    control_local_avail = to_eng->control_local[1].start - to_eng->control_local[0].start;
    global_trail_avail = to_eng->global_trail[1].start - to_eng->global_trail[0].start;
    if ( b_need + sp_need > control_local_avail
     || tg_need + tt_need > global_trail_avail
     || !adjust_stacks(to_eng->control_local, to_eng->control_local[0].start + b_need, to_eng->control_local[1].start - sp_need, 0)
     || !adjust_stacks(to_eng->global_trail,  to_eng->global_trail[0].start + tg_need, to_eng->global_trail[1].start - tt_need, 0))
    {
        Store_Eng_OSError();       /* store error code in ec_eng! */
        return SYS_ERROR;
    }

    /* Compute relocation offsets for the different stacks (in words!) */
    b_off  = to_eng->control_local[0].start - ec_eng->control_local[0].start;
    sp_off = to_eng->control_local[1].start - ec_eng->control_local[1].start;
    tg_off = to_eng->global_trail[0].start  - ec_eng->global_trail[0].start;
    tt_off = to_eng->global_trail[1].start  - ec_eng->global_trail[1].start;

    /* Copy and relocate registers */
    /* Local stack pointers */
    to_eng->sp_limit = (pword*) WOff(ec_eng->sp_limit, sp_off);
    to_eng->sp = (pword*) WOff(ec_eng->sp, sp_off);
    to_eng->e  = (pword*) WOff(ec_eng->e, sp_off);
    to_eng->eb = (pword*) WOff(ec_eng->eb, sp_off);

    /* Control stack pointers */
    to_eng->b_limit = (pword*) WOff(ec_eng->b_limit, b_off);
    to_eng->b.args = (pword*) WOff(ec_eng->b.args, b_off);
    to_eng->pb = (pword*) WOff(ec_eng->pb, b_off);
    to_eng->ppb = (pword*) WOff(ec_eng->ppb, b_off);

    /* Trail stack pointers */
    to_eng->tt_limit = (pword**) WOff(ec_eng->tt_limit, tt_off);
    to_eng->tt = (pword**) WOff(ec_eng->tt, tt_off);

    /* Global stack pointers */
    to_eng->tg_limit = (pword*) WOff(ec_eng->tg_limit, tg_off);
    to_eng->tg = (pword*) WOff(ec_eng->tg, tg_off);
    to_eng->gb = (pword*) WOff(ec_eng->gb, tg_off);
    to_eng->gctg = (pword*) WOff(ec_eng->gctg, tg_off);

    /* Global stack pointers that can be NULL */
    to_eng->lca = ec_eng->lca ? (pword*) WOff(ec_eng->lca, tg_off) : 0;
    to_eng->de = ec_eng->de ? (pword*) WOff(ec_eng->de, tg_off) : 0;
    to_eng->ld = ec_eng->ld  ? (pword*) WOff(ec_eng->ld, tg_off) : 0;
    to_eng->mu = ec_eng->mu ? (pword*) WOff(ec_eng->mu, tg_off) : 0;
    to_eng->sv = ec_eng->sv ? (pword*) WOff(ec_eng->sv, tg_off) : 0;
    to_eng->occur_check_boundary = ec_eng->occur_check_boundary ? (pword*) WOff(ec_eng->occur_check_boundary, tg_off) : 0;
    to_eng->top_constructed_structure = ec_eng->top_constructed_structure ? (pword*) WOff(ec_eng->top_constructed_structure, tg_off) : 0;
    to_eng->oracle = ec_eng->oracle ? (pword*) WOff(ec_eng->oracle, tg_off) : 0;

    assert(ec_eng->tg_soft_lim_shadow);
    to_eng->tg_soft_lim =
    to_eng->tg_soft_lim_shadow = (pword*) WOff(ec_eng->tg_soft_lim_shadow, tg_off);

    /* Tagged registers */
    assert(ec_eng->wl.val.ptr);
    Clone_Pw(&ec_eng->wl, &to_eng->wl);
    Clone_Pw(&ec_eng->wp_stamp, &to_eng->wp_stamp);
    Clone_Pw(&ec_eng->postponed_list, &to_eng->postponed_list);
    Clone_Pw(&ec_eng->posted, &to_eng->posted);

    /* rebuild the global references */
    {
	globalref **tail = &to_eng->references;
	globalref *from = ec_eng->references;
        assert(!to_eng->references);  /* according to engine status */
	for (; from; from=from->next) {
	    globalref *to = (globalref*) hp_alloc_size(sizeof(globalref));
	    to->ptr = (pword*) WOff(from->ptr, tg_off);
	    to->name = from->name;
	    to->module = from->module;
	    *tail = to;
	    tail = &to->next;
	}
	*tail = NULL;
    }

    /* Inherit literally */
    to_eng->wp = ec_eng->wp;

    to_eng->last_os_error = ec_eng->last_os_error;
    to_eng->last_os_errgrp = ec_eng->last_os_errgrp;

    to_eng->segment_size = ec_eng->segment_size;
    to_eng->frand_state = ec_eng->frand_state;	/* important */
    to_eng->default_module = ec_eng->default_module;

    /* Don't touch fields that belong to the engine, rather than to what it computes!
    to_eng->pp
    to_eng->options	?
    to_eng->parse_env
    to_eng->storage
    to_eng->report_to
    to_eng->vm_flags
    to_eng->dyn_event_q         
    to_eng->it_buf
    to_eng->own_thread
    to_eng->run_thread
    to_eng->next,prev
    to_eng->lock,ref_ctr
    to_eng->cond
    to_eng->owner_thread
    to_eng->requested_exit_code
    to_eng->parent_engine
    to_eng->wake_count
    to_eng->stop_address,bt_index,backtrace
    */

    /* copy/relocate the arguments and stacks */
    for (i=0; i<= arity; ++i) {
        Clone_Pw(&ec_eng->a[i], &to_eng->a[i]);
    }
    make_choicepoint(ec_eng, 0);        /* only used for Walk_Env_Chain */
    shift_stacks(ec_eng, 1, b_off, sp_off, tg_off, tt_off);
    drop_choicepoint(ec_eng);

    /* If the source engine has a dictionary marking request pending,
     * perform the marking now. This ensures that everything that was
     * cloned is surely marked, even in the case that the destination
     * engine does not have a corresponding marking request.
     */
    if (ec_atomic_load(&EVENT_FLAGS) & DICT_GC_REQUEST) {
        ec_atomic_and(&EVENT_FLAGS, ~DICT_GC_REQUEST);
        ecl_mark_engine(ec_eng, arity);
    }

    EngLogMsg(ec_eng, "was cloned", 0);
    EngLogMsg(to_eng, "is a clone", 0);
    return PSUCCEED;
}


/*-------------------------------------------------------------------
 * Initialisation
 *-------------------------------------------------------------------*/

void
bip_gc_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	(void) exported_built_in(in_dict("statistics_reset",0),
				p_stat_reset,	B_SAFE);
	(void) local_built_in(in_dict("gc_stat", 2),
				p_gc_stat,	B_UNSAFE|U_SIMPLE);
	(void) local_built_in(in_dict("gc_interval", 1),
				p_gc_interval,	B_UNSAFE|U_SIMPLE);
    }
}
