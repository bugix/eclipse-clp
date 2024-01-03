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
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * SEPIA C SOURCE MODULE
 *
 * VERSION	$Id: emu_c_env.c,v 1.22 2017/09/01 16:08:57 jschimpf Exp $
 */

/*
 * IDENTIFICATION		emu_c_env.c
 *
 * DESCRIPTION			This file contains auxiliary C functions for
 *				the emulator.
 */

#include "config.h"
#include "sepia.h"
#include "types.h"
#include "error.h"
#include "embed.h"
#include "mem.h"
#include "ec_io.h"
#include "dict.h"
#include "emu_export.h"
#include "module.h"
#include "debug.h"
#include "opcode.h"
#include "os_support.h"

#include <setjmp.h>


extern int		*interrupt_handler_flags_;
extern pri		**interrupt_handler_;
extern dident		*interrupt_name_;
extern void		msg_nopoll();
extern void		ec_init_globvars(ec_eng_t*);
extern int		ec_init_postponed(ec_eng_t*);


#define Bind_Named(pwn, pw)\
                         Trail_Tag_If_Needed_Gb(pwn); \
			 (pwn)->val.ptr = (pw);\
			 (pwn)->tag.kernel = TREF;

#define DELAY_SLOT		1	/* 'suspend' attribute slot */
#define CONSTRAINED_OFF		2	/* 'constrained' list */
/* If you change the above, update ic.c as well. */



/*------------------------------------------
 * the various entry points to the emulator
 *------------------------------------------*/

/*
 * What are the setjmp/longjmps good for?
 * In order to allow exit_block/1 from inside an interrupt emulator, we
 * map it onto the longjmp feature of C. Every emulator call is preceded
 * by a setjmp(), which catches longjmps that are executed while this
 * emulator invocation is active. When a longjmp is caught, we call the
 * emulator again and let it execute an exit_block/1. If the exit_block/1
 * is not caught inside this recursion level, the emulator exits with PTHROW
 * and we have to continue exiting in older emulator invocations by doing
 * another longjmp.
 */

extern vmcode	eval_code_[],
		recurs_code_[],
		slave_code_[],
		boot_code_[],
		query_code_[],
		it_code_[],
		it_block_code_[],
		*do_exit_block_code_;

extern vmcode	stop_fail_code_[],
		recurs_fail_code_[],
		slave_fail_code_[];

extern vmcode	*fail_code_,
		*bip_error_code_;

extern st_handle_t eng_root_branch;

/*
 * If we have reinitialised or restored the machine state,
 * we must make sure that the FakedOverflow condition is
 * in the corresponding state.
 */
void
re_fake_overflow(ec_eng_t *ec_eng)
{
    /* Note the order: reset unconditionally, then test and set faked overflow.
     * Other threads may raise events and fake overflow asynchronously.
     */
    Reset_Faked_Overflow;
    if (MU || (ec_atomic_load(&EVENT_FLAGS) && ec_eng->nesting_level == 1 && !PO))
    {
	Fake_Overflow;
    }
}

#define EMU_INIT_LD	1
#define EMU_INIT_WL	2
#define EMU_INIT_GV	4


static void
save_vm_status(ec_eng_t *ec_eng, vmcode *fail_code, int options)
{
    register pword *pw1;
    register control_ptr b_aux;
    register uint32_t i;
    extern vmcode fail_return_env_0_[];

    /*
     * Build the invocation frame
     *
     * We leave space for one inline frame (the biggest frame with constant size)
     * on top of the control stack to prevent overwriting useful information
     * in interrupt emulators. Thus we don't have to mask interrupts when
     * building small control frames.
     * We push a dummy return address onto the local stack because
     * the GC relies on the sp-entries in control frames pointing to
     * valid return addresses.
     */

    /* push a dummy return address (needed in the GC) */
    SP = (pword *) (((vmcode **) SP) -1);
    *((vmcode **) SP) = &fail_return_env_0_[1];

    i = VM_FLAGS;
    if (fail_code != &recurs_fail_code_[0])
	B.args += SAFE_B_AREA;		/* leave some free space */
    b_aux.args = B.args;

    b_aux.invoc->tg_before = TG;	/* for restoring TG after exiting */

    b_aux.invoc->wl = TAGGED_WL;
    b_aux.invoc->wp = WP;
    b_aux.invoc->wp_stamp = WP_STAMP;
    if (options & EMU_INIT_WL)
    {
	/* wl_init() must be done between saving tg_before and tg */
	/* it saves WL, LD, WP */
	Make_Struct(&TAGGED_WL, wl_init(ec_eng));
	/* don't update timestamp, WP must look "old" */
	WP = PRIORITY_MAIN;
    }

#ifdef NEW_ORACLE
    b_aux.invoc->oracle = TO;
    b_aux.invoc->followed_oracle = FO;
    b_aux.invoc->pending_oracle = PO;
    FO = PO = (char *) 0;
    TO = (pword *) 0;
    /* no oracles in recursive emulators! */
    if (ec_eng->nesting_level == 0  &&  VM_FLAGS & ORACLES_ENABLED)
    {
	O_Push(1, O_PAR_ORACLE);	/* also inits TO */
    }
#endif

    b_aux.invoc->postponed_list = PostponedList;
    if (options & EMU_INIT_GV)
    {
	ec_init_globvars(ec_eng);

	ec_init_postponed(ec_eng);

	/* no need to save/restore POSTED: ignored in nested engines */

    	b_aux.invoc->trace_data = ec_eng->trace_data;
	Make_Integer(&TAGGED_TD, 0);
	FCULPRIT = -1;
	/* FTRACE = NULL; */
    }

    b_aux.invoc->eb = EB;
    b_aux.invoc->sp = EB = SP;
    b_aux.invoc->gb = GB;
    b_aux.invoc->tg = GB = TG;		/* for retry from this frame */
    Push_Witness;			/* must be first new thing on global */
    b_aux.invoc->tt = TT;
    b_aux.invoc->e = E;
    b_aux.invoc->flags = i;
    b_aux.invoc->nesting_level = ec_eng->nesting_level;
    b_aux.invoc->pp = PP;
    b_aux.invoc->mu = MU;
    b_aux.invoc->sv = SV;
    b_aux.invoc->ld = LD;
    b_aux.invoc->de = DE;
    b_aux.invoc->ppb = PPB;
#ifdef PB_MAINTAINED
    b_aux.invoc->pb = PB;
#endif
    b_aux.invoc->node = eng_root_branch;
    b_aux.invoc->global_bip_error = ec_eng->global_bip_error;
	ec_eng->global_bip_error = 0;
    b_aux.invoc->last_os_error = ec_eng->last_os_error;
    b_aux.invoc->last_os_errgrp = ec_eng->last_os_errgrp;
    b_aux.invoc->gctg = GCTG;
    GCTG = TG;
    Save_Tg_Soft_Lim(b_aux.invoc->tg_soft_lim);
    b_aux.invoc->parser_env = PARSENV;

    pw1 = &A[0];
    b_aux.invoc->arg_0 = *pw1++;
    b_aux.invoc += 1;
    /* don't save any arguments for the initial frame to make invocation
     * frames identical size for all parallel engines */
    if (ec_eng->nesting_level > 0)
    {
	for(i = 1; i < NARGREGS; i++) {
	    if(pw1->tag.kernel != TEND) {
		*(b_aux.args)++ = *pw1++;
	    } else break;
	}
    }

    b_aux.top->backtrack = fail_code;
    b_aux.top->frame.invoc = B.invoc;
    B.top = b_aux.top + 1;
#ifdef PB_MAINTAINED
    PB =
#endif
    PPB = B.args;

    /*
     * Do some initialisation common to all recursive emulator invocations
     */

    ec_eng->nesting_level++;

    DE = MU = SV = (pword *) 0;

#ifdef OC
    OCB = (pword *) 0;
#endif

    re_fake_overflow(ec_eng);

    Restore_Tg_Soft_Lim(TG + TG_SEG)
}


/*
 * Idea for event handling: Have the EventPending check here in the loop
 * and dispatch to next predicate, handler or pred continuation, which
 * all correspond to a C function entry point.
 * This returns PSUCCESS or PFAIL or PTHROW (throw argument is in A1)
 */
static int
_emul_trampoline(ec_eng_t *ec_eng)
{
    continuation_t continuation = ec_.emulator;
    do
    {
	continuation = (continuation_t) (*continuation)(ec_eng);
    } while (continuation);
    return A[0].val.nint;
}

static void
_start_goal(ec_eng_t *ec_eng, value v_goal, type t_goal, value v_mod, type t_mod)
{
    A[1].val.all = v_goal.all;
    A[1].tag.all = t_goal.all;
    A[2].val.all = v_mod.all;
    A[2].tag.all = t_mod.all;
}


/*
 * This is a wrapper round the emulator _emul_trampoline()
 * which catches the longjumps.
 */
static int
emulc(ec_eng_t *ec_eng)
{
    action_list_t *saved_cleanup_bot;
    sigjmp_buf longjmp_dest;
    void *saved_longjmp_dest;
    void *saved_thread;
    int jump, res;

    /*
     * Arrange for catching longjmps during emulation.
     * Handle cleanup of held locks and objects.
     */
    saved_cleanup_bot = ec_eng->cleanup_bot;
    ec_eng->cleanup_bot = ec_eng->cleanup;

    jump = sigsetjmp(longjmp_dest, 1);
    if (jump == 0) {
	/* We are in the first call */
	saved_longjmp_dest = ec_eng->it_buf;
	ec_eng->it_buf = (sigjmp_buf *) longjmp_dest;
	/* run_thread used to longjmp from signal handlers */
	saved_thread = ec_eng->run_thread;
	ec_eng->run_thread = ec_thread_self();

    } else {

	/* Do the cleanup we missed by not returning from the external */
	Do_Cleanup()

	switch(jump)
	{
	case PFAIL:
	    /* We get here when a C++ external wants to fail */
	    PP = fail_code_;
	    break;
	case PTHROW: 
	    /* we get here when a recursive emulator throws or
	     * an external called Exit_Block() (eg. on stack overflow)
	     */
	    PP = do_exit_block_code_;
	    /* in case we aborted in polling mode */
	    msg_nopoll();
	    break;
	case PEXITED: 
	    PP = engine_exit_code_;
	    break;
	default:
	    /* We get here when a C++ external wants to raise an error */
	    PP = bip_error_code_;
	    break;

	}
    }
    res = _emul_trampoline(ec_eng);
    ec_eng->cleanup_bot = saved_cleanup_bot;
    ec_eng->it_buf = saved_longjmp_dest;
    ec_eng->run_thread = saved_thread;
    return res;
}


/**
 * Run a main loop (sepia_kernel:main/1) in a completely empty engine.
 * This should return PYIELD(PFAIL), indicating the engine is ready
 * to be resumed with a goal.
 */
int
eclipse_main(ec_eng_t *ec_eng, int startup)
{
    assert(EngIsOurs(ec_eng));
    save_vm_status(ec_eng, &stop_fail_code_[0], EMU_INIT_LD|EMU_INIT_WL);
    A[1] = ecl_term(ec_eng, ec_did("main",1), ec_long(startup));
    A[2].val.did = ec_.d.kernel_sepia;
    A[2].tag.kernel = ModuleTag(ec_.d.kernel_sepia);
    PP = &eval_code_[0];
    return emulc(ec_eng);
}


/**
 * Call a subgoal (the engine must be in a call-state, e.g inside an external)
 *
 * The goal is made deterministic (either GOAL_CUT or GOAL_NOTNOT).
 * When also setting GOAL_CATCH, exceptions are returned as PTHROW
 * instead of leading to a longjmp() by default.  CAUTION: in this case,
 * A[1] holds the Ball, and A[1]'s previous content is lost.
 *
 * During these subgoal executions, dictionary GCs may occur, which may
 * miss didents that are stored in local C variables.
 * However, stack GCs will only collect stack produced during the call,
 * so TG pointers in the C environment are safe.
 */
int
sub_emulc_opt(ec_eng_t *ec_eng, value vgoal, type tgoal, value vmod, type tmod, int options)
{
    int		result, init_options;
    vmcode	*pp;

    switch(options & ~GOAL_CATCH) {
	case GOAL_CUT:
	    pp = &recurs_code_[0];
	    init_options = 0;
	    break;
	case GOAL_NOTNOT:
	    pp = &eval_code_[0];
	    init_options = EMU_INIT_LD|EMU_INIT_WL;
	    break;
	default:
	    return RANGE_ERROR;
    }
    assert(EngIsOurs(ec_eng));
    save_vm_status(ec_eng, &recurs_fail_code_[0], init_options);
    PP = pp;
    A[1].val.all = vgoal.all;
    A[1].tag.all = tgoal.all;
    A[2].val.all = vmod.all;
    A[2].tag.all = tmod.all;
    result = emulc(ec_eng);
    while (result == PYIELD)
    {
	Make_Atom(&A[1], in_dict("Nested emulator yielded",0));
	Make_Integer(&A[2], RESUME_CONT);
    	result = emulc(ec_eng);
    }
    if (result == PTHROW  &&  !(options & GOAL_CATCH))
	siglongjmp(*(sigjmp_buf*)ec_eng->it_buf, PTHROW);
    return result;
}

int
ecl_subgoal(ec_eng_t *ec_eng, pword goal, pword mod, int options)
{
    return sub_emulc_opt(ec_eng, goal.val, goal.tag, mod.val, mod.tag, options);
}


int
slave_emulc(ec_eng_t *ec_eng)
{
    int		result;

    save_vm_status(ec_eng, &slave_fail_code_[0], EMU_INIT_LD|EMU_INIT_WL);
    PP = &slave_code_[0];

    result = emulc(ec_eng);
    while (result == PYIELD)
    {
	Make_Atom(&A[1], in_dict("Nested emulator yielded",0));
	Make_Integer(&A[2], RESUME_CONT);
    	result = emulc(ec_eng);
    }

    if (result == PTHROW)
	siglongjmp(*(sigjmp_buf*)ec_eng->it_buf, PTHROW);
    return result;

}


/*
 * Resume an engine that runs the main/0 loop.  Returns with (see yield/4):
 *	A[0]	PYIELD
 *	A[1]	Status PSUCCEED,...,PFLUSHIO [also this function's return code]
 *	A[2]	ToParent (for PTHROW,PEXITED,PYIELD,PWAITIO,PFLUSHIO)
 *	A[3]	-FromParent (for next resume)
 *	A[4]	-ResumeType (for next resume)
 * On return, A[1] is a dereferenced integer, identical to return value.
 * No error code returned here.
 */

int
resume_emulc(ec_eng_t *ec_eng)
{
    int res, status;
    res = emulc(ec_eng);
    assert(IsInteger(A[1].tag));	/* expect status or exit code */
    if (res == PYIELD) {
	res = A[1].val.nint;
	assert(PSUCCEED <= res  && res <= PFLUSHIO);
	return res;
    }

    assert(res == PEXITED);
    /* simulate yield(PEXITED,ExitCode,_,_) calling convention */
    A[2] = A[1];
    Make_Integer(&A[1], PEXITED);
    A[0].val.nint = PYIELD;
    ec_emu_fini(ec_eng);
    return PEXITED;
}


/*
 * Get an engine to the PEXITED state (from any other state),
 * which essentially means cutting and untrailing everything.
 * The previous engine state becomes the (numeric) exit code.
 * The engine is finalized (stacks deallocated).
 */

void
ecl_engine_exit(ec_eng_t *ec_eng, int exit_code)
{
    int res;
    assert(!EngIsDead(ec_eng));
    assert(EngIsOurs(ec_eng));
    PP = &engine_exit_code_[0];
    Make_Integer(&A[1], exit_code);
    res = emulc(ec_eng);
    assert(res == PEXITED);

    /* exit as if with yield(PEXITED,ExitCode,_,_) */
    A[2] = A[1];
    Make_Integer(&A[1], PEXITED);
    A[0].val.nint = PYIELD;
    ec_emu_fini(ec_eng);
}



/*
 * For booting: the 1st argument is the bootfile name
 */ 

int
boot_emulc(ec_eng_t *ec_eng, value v_file, type t_file, value v_mod, type t_mod)
{
    int		result;
    save_vm_status(ec_eng, &stop_fail_code_[0], EMU_INIT_LD);
    PP = &boot_code_[0];
    _start_goal(ec_eng, v_file, t_file, v_mod, t_mod);
    result = emulc(ec_eng);
    while (result == PYIELD)
    {
	Make_Atom(&A[1], in_dict("Nested emulator yielded",0));
	Make_Integer(&A[2], RESUME_CONT);
    	result = emulc(ec_eng);
    }
    return result;
}


/*
 * make an exit_block with the given exit tag
 */

int
ecl_return_throw(ec_eng_t *ec_eng, value v_tag, type t_tag)
{
    A[1].val.all = v_tag.all;
    A[1].tag.all = t_tag.all;
    return PTHROW;
}

void
ecl_longjmp_throw(ec_eng_t *ec_eng, value v_tag, type t_tag)
{
    A[1].val.all = v_tag.all;
    A[1].tag.all = t_tag.all;
    siglongjmp(*(sigjmp_buf*)ec_eng->it_buf, PTHROW);
}


/*
 * Perform (one of) the actions encoded in event_flags:
 *  - URGENT_EVENT_POSTED
 *  - EXIT_REQUEST
 * either via return code or via siglongjmp.
 */
int
ecl_do_requested_action(ec_eng_t *ec_eng, int event_flags, int jump)
{
    int action;
    if (event_flags & URGENT_EVENT_POSTED) {
	pword ball;
	next_posted_item(ec_eng, &ball, 1);
	get_heapterm(ec_eng, &ball, &A[1]);
	free_heapterm(&ball);
	action = PTHROW;

    } else if (event_flags & EXIT_REQUEST) {
	Make_Integer(&A[1], ec_eng->requested_exit_code);
	ec_atomic_and(&EVENT_FLAGS, ~EXIT_REQUEST);
	/* do not clear FakeOverflow, too complex to get right here */
	action = PEXITED;
    } else {
	assert(0);
    }
    if (jump)
	siglongjmp(*(sigjmp_buf*)ec_eng->it_buf, action);
	/*NOTREACHED*/
    else
	return action;
}



/*------------------------------------------
 * Shared resource management
 *------------------------------------------*/

void
ec_cleanup_unlock(void *plock)
{
    mt_mutex_unlock((ec_mutex_t*)plock);
}



/*------------------------------------------
 * Synchronous event handling
 *------------------------------------------*/

#ifdef DEBUG_EVENT_Q
#define event_q_assert(ex) {						\
    if (!(ex)) {							\
	(void) p_fprintf(current_err_, "Assertion Failed at ");		\
	(void) p_fprintf(current_err_, "file \"%s\"", __FILE__);	\
	(void) p_fprintf(current_err_, " line %d\n", __LINE__);		\
	(void) ec_panic("Assertion Failed", "Event queue");		\
    }									\
}
#else
#define event_q_assert(ex)
#endif

#define IsEmptyDynamicEventQueue()			\
	(ec_eng->dyn_event_q.free_event_slots == 	\
	 ec_eng->dyn_event_q.total_event_slots)


#ifdef PRINTAM
void
print_dynamic_queued_events(ec_eng_t *ec_eng)
{
    dyn_event_q_slot_t *slot;
    uword cnt = 0, total;

    slot = ec_eng->dyn_event_q.prehead->next; /* get */
    total = ec_eng->dyn_event_q.total_event_slots - ec_eng->dyn_event_q.free_event_slots;
    p_fprintf(current_err_, "Dynamic event queue: Total: %" W_MOD "d Free: %" W_MOD "d:", 
	ec_eng->dyn_event_q.total_event_slots, ec_eng->dyn_event_q.free_event_slots);
    for( cnt = 0; cnt < total; cnt++, slot = slot->next )
    {
	p_fprintf(current_err_, " %d:%x", slot->event_data.tag.kernel, slot->event_data.val.ptr);
    }
    ec_newline(current_err_);
}
#endif


/**
 * Post a pword item (whose content is not important here) as
 * either urgent (front of queue) or normal (tail of queue).
 *
 * @param event		either event (not urgent) or heap_term (urgent)
 * @param no_duplicates	do nothing if already in queue (with same urgency)
 * @param urgent	0 (insert at tail) or 1 (insert at front)
 *
 * @return
 *	PSUCCEED	item was posted
 *	PFAIL		duplicate, not posted
 *	ENGINE_DEAD	engine dead, not posted
 */

static int
_post_item(ec_eng_t *ec_eng, pword item, int no_duplicates, int urgent)
{
    int res = PSUCCEED;

    mt_mutex_lock(&ec_eng->lock);

    if (EngIsDead(ec_eng)) {
	res = ENGINE_DEAD;
	goto _unlock_return_;
    }

    if (no_duplicates)
    {
	uword cnt, total;
	/* if this exact item is already posted, don't do it again */
	dyn_event_q_slot_t *slot = ec_eng->dyn_event_q.prehead->next; /* get */
	
	total = ec_eng->dyn_event_q.total_event_slots - ec_eng->dyn_event_q.free_event_slots;
	for( cnt = 0; cnt < total; cnt++, slot = slot->next )
	{
	    if (slot->urgent == urgent
	     && slot->event_data.tag.all == item.tag.all
	     && slot->event_data.val.all == item.val.all)
	    {
		res = PFAIL;
		goto _unlock_return_;
	    }
	}
    }

    /* Is the queue full? */
    if (ec_eng->dyn_event_q.free_event_slots != 0) 
    {
	/* No! */
	ec_eng->dyn_event_q.free_event_slots--;
    }
    else
    {
	/* Yes! */
	dyn_event_q_slot_t *slot;

	event_q_assert(ec_eng->dyn_event_q.prehead == 
		       ec_eng->dyn_event_q.tail); /* put == get */

	slot = hp_alloc_size(sizeof(*slot));

	/* insert after tail(==prehead), make it the new prehead */
	slot->prev = ec_eng->dyn_event_q.tail;
	slot->next = ec_eng->dyn_event_q.tail->next;
	slot->next->prev = slot;
	ec_eng->dyn_event_q.tail->next = slot;
	ec_eng->dyn_event_q.prehead = slot;
	ec_eng->dyn_event_q.total_event_slots++;
    }

    if (urgent) {
	ec_eng->dyn_event_q.prehead->urgent = urgent;
	ec_eng->dyn_event_q.prehead->event_data = item;
	ec_eng->dyn_event_q.prehead = ec_eng->dyn_event_q.prehead->prev;
    } else {
	ec_eng->dyn_event_q.tail = ec_eng->dyn_event_q.tail->next;
	ec_eng->dyn_event_q.tail->urgent = urgent;
	ec_eng->dyn_event_q.tail->event_data = item;
    }
    ec_atomic_or(&EVENT_FLAGS, EVENT_POSTED|(urgent?URGENT_EVENT_POSTED:0));
    Fake_Overflow;

    /* attempt to expediate urgent request handling */
    if (urgent && EngIsPaused(ec_eng))
	ecl_interrupt_pause(ec_eng);

_unlock_return_:
    mt_mutex_unlock(&ec_eng->lock);
    return res;
}


/**
 * Post event goal to engine.
 *
 * @param event
 *	atom
 *	event-THANDLE
 *	event-TPTR
 * @param no_duplicates
 *	do nothing if event already in queue
 *
 * @return
 *	PSUCCEED		was posted
 *	ENGINE_DEAD		engine dead, not posted
 *	TYPE_ERROR		not an event handle or atom
 *	STALE_HANDLE		stale event handle
 *	INSTANTIATION_FAULT	event uninstantiated
 */

static int
_post_event_dynamic(ec_eng_t *ec_eng, pword event, int no_duplicates)
{
    int res;

    if (IsHandle(event.tag))
    {
	Check_Type(event.val.ptr->tag, TEXTERN);
	if (ExternalClass(event.val.ptr) != &heap_event_tid) {
	    return TYPE_ERROR;
	}
        if (!(ExternalData(event.val.ptr))) {
	    return STALE_HANDLE;
	}

	/* If the event is disabled, don't post it to the queue */
	if (!((t_heap_event *)ExternalData(event.val.ptr))->enabled) {
	    return PSUCCEED;
	}
	
	/* Don't put the handle in the queue! */
	event.tag.kernel = TPTR;
	event.val.wptr = heap_event_tid.copy(ExternalData(event.val.ptr));
    }
    else if (IsTag(event.tag.kernel, TPTR))
    {
	/* Assume it's a TPTR to a t_heap_event (we use this when posting
	 * an event that was stored in a stream descriptor).
	 * As above, if the event is disabled, don't post it to the queue.
	 */
	if (!((t_heap_event *)event.val.ptr)->enabled) {
	    return PSUCCEED;
	}
	event.val.wptr = heap_event_tid.copy(event.val.wptr);
    }
    else if (!IsAtom(event.tag))
    {
	return IsRef(event.tag) ? INSTANTIATION_FAULT : TYPE_ERROR;
    }

    res = _post_item(ec_eng, event, no_duplicates, 0);

    if (res != PSUCCEED) {
	/* If the anonymous event handle reference count was bumped
	 * (via the copy ready for queue insertion) decrement it again!
	 */
	if (IsTag(event.tag.kernel, TPTR))
	    heap_event_tid.free(event.val.wptr);
	res = (res==PFAIL) ? PSUCCEED : res;	/* suppressed duplicate is ok */
    }
    return res;
}


/**
 * Shorthands for event posting.
 */
int Winapi
ecl_post_event_unique(ec_eng_t *ec_eng, pword event)
{
    return _post_event_dynamic(ec_eng, event, 1);
}

int Winapi
ecl_post_event(ec_eng_t *ec_eng, pword event)
{
    return _post_event_dynamic(ec_eng, event, 0);
}

int Winapi
ecl_post_event_string(ec_eng_t *ec_eng, const char *event)
{
    pword pw;
    Make_Atom(&pw, in_dict((char *) event,0));
    return _post_event_dynamic(ec_eng, pw, 0);
}


/**
 * Request that the engine perform a throw(Ball) at the next occasion.
 * Do this by posting a heap copy of ball as an 'urgent' item.
 * The target engine does not have to be owned by the calling thread.
 *
 * @param from_eng	the engine where the ball term resides (owned)
 *			(can be NULL if ball is simple)
 * @param ec_eng	the engine to post to (not owned)
 * @param ball		the exception term to throw
 *
 * @return
 *	PSUCCEED	request was raised (or already there)
 *	ENGINE_DEAD	engine is already dead, request ignored
 *	TYPE_ERROR	non-simple ball and no from_eng
 *
 */
int Winapi
ecl_post_throw(ec_eng_t *from_eng, ec_eng_t *ec_eng, pword ball)
{
    int res;
    pword heap_ball;
    if (IsSimple(ball.tag))
	create_heapterm_simple(&heap_ball, ball);
    else if (from_eng)
	create_heapterm(from_eng, &heap_ball, ball.val, ball.tag);
    else
    	return TYPE_ERROR;
    res = _post_item(ec_eng, heap_ball, 1/*dup?*/, 1);
    if (res != PSUCCEED)
        free_heapterm(&heap_ball);
    return (res==PFAIL) ? PSUCCEED : res; /* suppressed duplicate is ok */
}


/*
 * This is expected to be called according to EVENT_FLAGS
 * settings EVENT_POSTED and URGENT_EVENT_POSTED.
 * Any associated Fake_Overflow has already been reset.
 * On return, Fake_Overflow and EVENT_FLAGS are adjusted.
 *
 * @param expect_urgent	0 (any) or 1 (urgent) events
 * @return
 *	0: normal event (returned value is an event)
 *	1: urgent event (returned value is a heap_term)
 *
 * Example usage:
 *
 * if (FakedOverflow() && (EVENT_FLAGS & EVENT_POSTED)) {
 *	pword item;
 *	if (next_posted_item(ec_eng, &item, 0)) {
 *	   item is a a heap_ball
 *	} else {
 *	   item is an event
 *	}
 *
 * if (FakedOverflow() && (EVENT_FLAGS & URGENT_EVENT_POSTED)) {
 *	pword heap_ball;
 *	next_posted_item(ec_eng, &heap_ball, 1);
 *	...
 * }
 */

int
next_posted_item(ec_eng_t *ec_eng, pword *out, int expect_urgent)
{
    int res = 0;
    dyn_event_q_slot_t *first;

    mt_mutex_lock(&ec_eng->lock);

    assert(!IsEmptyDynamicEventQueue());

    first = ec_eng->dyn_event_q.prehead->next;
    assert(expect_urgent ? first->urgent : 1);

    *out = first->event_data;
    res = first->urgent ? 1 : 0;
    ec_eng->dyn_event_q.prehead = first;	/* pop */
    ec_eng->dyn_event_q.free_event_slots++;

    /* If the queue contains events, fake overflow to handle next */
    if (IsEmptyDynamicEventQueue()) 
    {
	/* queue empty, reset EVENT_FLAGS */
	assert(ec_eng->dyn_event_q.prehead == 
		       ec_eng->dyn_event_q.tail); /* put == get */
	ec_atomic_and(&EVENT_FLAGS, ~(EVENT_POSTED|URGENT_EVENT_POSTED));
    }
    else if (first->next->urgent)
    {
	/* both flags must still be set */
	assert(res == 2);
	assert((ec_atomic_load(&EVENT_FLAGS) & (EVENT_POSTED|URGENT_EVENT_POSTED)) == (EVENT_POSTED|URGENT_EVENT_POSTED));
	Fake_Overflow;
    }
    else if (res==2)
    {
	/* we consumed the last urgent */
	ec_atomic_and(&EVENT_FLAGS, ~URGENT_EVENT_POSTED);
	assert(ec_atomic_load(&EVENT_FLAGS) & EVENT_POSTED);
	Fake_Overflow;
    }
    else
    {
	/* we consumed a non-urgent, no change */
	assert((ec_atomic_load(&EVENT_FLAGS) & (EVENT_POSTED|URGENT_EVENT_POSTED)) == EVENT_POSTED);
	Fake_Overflow;
    }

    mt_mutex_unlock(&ec_eng->lock);
    return res;
}


/*
 * Remove a disabled event from the dynamic event queue
 */

void 
purge_disabled_dynamic_events(ec_eng_t *ec_eng, t_heap_event *event)
{
    uword cnt = 0, total;
    pword *pevent;

    mt_mutex_lock(&ec_eng->lock);

    total = ec_eng->dyn_event_q.total_event_slots - ec_eng->dyn_event_q.free_event_slots;

    if (total > 0 )
    {
	dyn_event_q_slot_t *slot = ec_eng->dyn_event_q.prehead->next;

	/* Process all slots but the tail */
	for( cnt = 1; cnt < total; cnt++ )
	{
	    pevent = &slot->event_data;

	    if (IsTag(pevent->tag.kernel, TPTR) && pevent->val.wptr == (uword*)event)
	    {
		dyn_event_q_slot_t *next = slot->next;
		ec_eng->dyn_event_q.free_event_slots++;
		slot->prev->next = slot->next;	/* unlink */
		slot->next->prev = slot->prev;
		slot->next = ec_eng->dyn_event_q.tail->next; /* insert after tail */
		ec_eng->dyn_event_q.tail->next = slot;
		slot->next->prev = slot;
		slot->prev = ec_eng->dyn_event_q.tail;
		ExternalClass(pevent->val.ptr)->free(ExternalData(pevent->val.ptr));
		slot = next;
		continue;
	    }
	    slot = slot->next;
	}

	/* Special case tail element removal. This also handles the case 
	 * where the circular list is full - in either case simply rewind 
	 * the tail pointer.
	 */
	event_q_assert(slot == ec_eng->dyn_event_q.tail);
	pevent = &slot->event_data;
	if (IsTag(pevent->tag.kernel, TPTR) && pevent->val.wptr == (uword*)event)
	{
	    ec_eng->dyn_event_q.free_event_slots++;
	    ec_eng->dyn_event_q.tail = slot->prev;
	    ExternalClass(pevent->val.ptr)->free(ExternalData(pevent->val.ptr));
	}

	/* If queue is now empty, clear the flags 
	 */
	if (IsEmptyDynamicEventQueue())
	{
	    ec_atomic_and(&EVENT_FLAGS, ~EVENT_POSTED);
	}
    }
    mt_mutex_unlock(&ec_eng->lock);
}


/*
 * Initialise dynamic event queue
 */

void
ec_init_dynamic_event_queue(ec_eng_t *ec_eng)
{
    int cnt;

    if ((ec_eng->dyn_event_q.prehead = 
	(dyn_event_q_slot_t *)hp_alloc_size(sizeof(dyn_event_q_slot_t))) == NULL) 
    {
	ec_panic(MEMORY_P, "emu_init()");
    }

    ec_eng->dyn_event_q.tail = ec_eng->dyn_event_q.prehead;

    for(cnt = 0; cnt < MIN_DYNAMIC_EVENT_SLOTS - 1; cnt++) 
    {
	dyn_event_q_slot_t *slot = hp_alloc_size(sizeof(*slot));
	if (!slot) 
	{
	    ec_panic(MEMORY_P, "emu_init()");
	}
	slot->prev = ec_eng->dyn_event_q.tail;
	ec_eng->dyn_event_q.tail->next = slot;
	ec_eng->dyn_event_q.tail = slot;
    }

    /* Link tail to head to complete circular list creation */
    ec_eng->dyn_event_q.tail->next = ec_eng->dyn_event_q.prehead;
    ec_eng->dyn_event_q.prehead->prev = ec_eng->dyn_event_q.tail;

    /* Set tail insertion point */
    /* Empty queue condition: 
     * IsEmptyDynamicEventQueue(). In addition, when queue is empty
     * or full: tail->next (put) == prehead->next (get)
     */
    ec_eng->dyn_event_q.tail = ec_eng->dyn_event_q.prehead;

    /* Dynamic queue is initially empty */
    ec_eng->dyn_event_q.total_event_slots = 
		ec_eng->dyn_event_q.free_event_slots = MIN_DYNAMIC_EVENT_SLOTS;
}


/* Shrink the dynamic event queue to at least
 * MIN_DYNAMIC_EVENT_SLOTS free.
 * Used during GC.
 */

void
trim_dynamic_event_queue(ec_eng_t *ec_eng)
{
    if (ec_eng->dyn_event_q.free_event_slots > MIN_DYNAMIC_EVENT_SLOTS)
    {
	dyn_event_q_slot_t *slot = ec_eng->dyn_event_q.tail->next; /* put */
	uword new_free_slots =	ec_eng->dyn_event_q.free_event_slots / 
					DYNAMIC_EVENT_Q_SHRINK_FACTOR;
	if (new_free_slots < MIN_DYNAMIC_EVENT_SLOTS) {
	    new_free_slots = MIN_DYNAMIC_EVENT_SLOTS;
	}

	if (EclGblFlags & GC_VERBOSE) {
	    p_fprintf(log_output_,	
		      "shrink dynamic event queue from Total: %" W_MOD "u"
		      " Free: %" W_MOD "u to Total: %" W_MOD "u Free: %" W_MOD "u (elements)\n", 
		      ec_eng->dyn_event_q.total_event_slots, 
		      ec_eng->dyn_event_q.free_event_slots, 
		      ec_eng->dyn_event_q.total_event_slots - 
		      (ec_eng->dyn_event_q.free_event_slots - new_free_slots), new_free_slots);
		      ec_flush(log_output_);
	}

	for ( ; ec_eng->dyn_event_q.free_event_slots > new_free_slots 
	      ; ec_eng->dyn_event_q.free_event_slots--, 
		ec_eng->dyn_event_q.total_event_slots-- )
	{
	    ec_eng->dyn_event_q.tail->next = slot->next;
	    slot->next->prev = ec_eng->dyn_event_q.tail;
	    hp_free_size(slot, sizeof(dyn_event_q_slot_t));
	    slot = ec_eng->dyn_event_q.tail->next;
	}
    }
}

/**
 * Mark dictionary items in the queue.
 * CAUTION: this uses knowledge about the conventions for queue entries.
 */
void
mark_dids_dynamic_event_queue(ec_eng_t *ec_eng)
{
    mt_mutex_lock(&ec_eng->lock);
    if (!IsEmptyDynamicEventQueue()) 
    {
	dyn_event_q_slot_t *slot = ec_eng->dyn_event_q.prehead;
	do {
	    slot = slot->next;
	    if (slot->urgent) {
		mark_dids_from_heapterm(&slot->event_data);
	    } else if (IsTag(slot->event_data.tag.kernel, TPTR)) {
		heap_event_tid.mark_dids((t_ext_ptr)slot->event_data.val.wptr);
	    } else {
		mark_dids_from_pwords(&slot->event_data, &slot->event_data+1);
	    }
	} while(slot != ec_eng->dyn_event_q.tail);
    }
    mt_mutex_unlock(&ec_eng->lock);
}

void
ec_fini_dynamic_event_queue(ec_eng_t *ec_eng)
{
    dyn_event_q_slot_t *slot = ec_eng->dyn_event_q.prehead;
    do {
	dyn_event_q_slot_t *next = slot->next;
	hp_free_size(slot, sizeof(dyn_event_q_slot_t));
	slot = next;
    } while (slot != ec_eng->dyn_event_q.prehead);
    ec_eng->dyn_event_q.prehead = ec_eng->dyn_event_q.tail = NULL;
    ec_eng->dyn_event_q.free_event_slots = ec_eng->dyn_event_q.total_event_slots = 0;
}



/*----------------------------------------------
 * Auxiliary functions for the emulator
 *----------------------------------------------*/

/*
 * UNIFY		var		nonvar		any
 * 
 * with var:		ec_unify()
 * 
 * with nonvar:		Bind_Var()	ec_unify()
 * 
 * with any:		ec_unify()	ec_unify()	ec_unify()
 */

/*
 * ec_unify() -- copy of the general unifier, callable from C code
 *
 * Note that Occur_Check_Boundary(0) is done after return from the builtin.
 */

int
ec_unify_(ec_eng_t *ec_eng,
	value v1, type t1,
	value v2, type t2,
	pword **list)		/* list of unified metaterms */
{
    register long arity;
    register pword *pw1, *pw2;

    /* In Request_Unify it may happen that the tag is REF/NAME but
       it has been already bound by a previous Request */
    if (IsRef(t1))
    {
	pw1 = v1.ptr;
	Dereference_(pw1);
	t1.all = pw1->tag.all;
	v1.all = pw1->val.all;
    }
    if (IsRef(t2))
    {
	pw2 = v2.ptr;
	Dereference_(pw2);
	t2.all = pw2->tag.all;
	v2.all = pw2->val.all;
    }
	
    for (;;)
    {
	if(IsVar(t1))
	{
	    if(IsVar(t2)) 		/* both are free:	*/
	    {
		if (v1.ptr < v2.ptr)
		    if (v1.ptr < TG)
		    {
			Trail_If_Needed(v2.ptr);
			v2.ptr->val.ptr = v1.ptr;
		    }
		    else
		    {
			Trail_If_Needed_Eb(v1.ptr);
			v1.ptr->val.ptr = v2.ptr;
		    }
		else if (v1.ptr > v2.ptr)
		    if (v2.ptr < TG)
		    {
			Trail_If_Needed(v1.ptr);
			v1.ptr->val.ptr = v2.ptr;
		    }
		    else
		    {
			Trail_If_Needed_Eb(v2.ptr);
			v2.ptr->val.ptr = v1.ptr;
		    }
		else
		    ;		/* succeed */
	    }
	    else 			/* only t1 is free */
	    {
		Occur_Check_Read(v1.ptr, v2, t2, return PFAIL)
		if (IsRef(t2)) {
		    Trail_If_Needed(v1.ptr);
		    v1.ptr->val.ptr = v2.ptr->val.ptr;
		} else {
		    Bind_(v1.ptr, v2.all, t2.all)
		}
	    }
	    return PSUCCEED;
	}
	else if (IsVar(t2))		/* only t2 is free */
	{
	    Occur_Check_Read(v2.ptr, v1, t1, return PFAIL)
	    if (IsRef(t1)) {
		Trail_If_Needed(v2.ptr);
		v2.ptr->val.ptr = v1.ptr->val.ptr;
	    } else {
		Bind_(v2.ptr, v1.all, t1.all)
	    }
	    return PSUCCEED;
	}
	else if (IsRef(t1))		/* t1 is a nonstandard variable */
	{
	    pword aux_pw;
	    Occur_Check_Read(v1.ptr, v2, t2, return PFAIL)
	    aux_pw.val.all = v2.all;
	    aux_pw.tag.all = t2.all;
	    return bind_c(ec_eng, v1.ptr, &aux_pw, list);
	}
	else if (IsRef(t2))		/* t2 is a nonstandard variable */
	{
	    pword aux_pw;
	    Occur_Check_Read(v2.ptr, v1, t1, return PFAIL)
	    aux_pw.val.all = v1.all;
	    aux_pw.tag.all = t1.all;
	    return bind_c(ec_eng, v2.ptr, &aux_pw, list);
	}
	/* two non-variables */
	else if (TagType(t1) != TagType(t2))
	{
	    return PFAIL;
	}
	else if (IsSimple(t1))
	{
	    if (SimpleEq(t1.kernel, v1, v2))
		return PSUCCEED;
	    else
		return PFAIL;
	}
	else if (IsList(t1))
	{
	    arity = 2;
	}
	else if (IsStructure(t1))
	{
	    if (v1.ptr->val.did != v2.ptr->val.did)
		return PFAIL;
	    if ((arity = DidArity(v1.ptr->val.did)) == 0)
		return PSUCCEED;
	    v1.ptr++;
	    v2.ptr++;
	}
	else if (IsString(t1))
	{
	    Compare_Strings(v1, v2, arity)
	    if (arity >= 0)
		return PFAIL;
	    else
		return PSUCCEED;
	}
	else
	{
#ifdef PRINTAM
	    if (!(TagType(t1) >= 0 && TagType(t1) <= NTYPES))
	    {
	    p_fprintf(current_err_, "ec_unify(): unknown tag (%x) encountered\n",
			t1.kernel);
	    return PFAIL;
	    }
#endif
	    return tag_desc[TagType(t1)].equal(v1.ptr, v2.ptr) ? PSUCCEED : PFAIL;
	}

	Return_On_Request();	/* because we might be looping */
	
	/* arity > 0 */
	for (;;)
	{
	    int res;
	    pw1 = v1.ptr++;
	    pw2 = v2.ptr++;
	    Dereference_(pw1);
	    Dereference_(pw2);
	    if (--arity == 0)
		break;
	    res = ec_unify_(ec_eng, pw1->val, pw1->tag, pw2->val, pw2->tag, list);
	    if (res != PSUCCEED)
		return res;	/* PFAIL|PTHROW|PEXITED */
	}
	v1.all = pw1->val.all;
	t1.all = pw1->tag.all;
	v2.all = pw2->val.all;
	t2.all = pw2->tag.all;
    }
}


int
deep_suspend(ec_eng_t *ec_eng,
	value val, type tag,
	int position,		/* must be > 0 */
	pword *susp,		/* must be dereferenced */
	int slot)
{
    register int arity;
    register pword *arg_i;
    int		res;

    for (;;)
    {
	if (IsRef(tag))
	{
	    return insert_suspension(ec_eng, val.ptr, position, susp, slot);
	}
	else if (IsList(tag))
	    arity = 2;
	else if (IsStructure(tag))
	{
	    arity = DidArity(val.ptr->val.did);
	    val.ptr++;
	}
	else
	    return PSUCCEED;
 
	for(;arity > 1; arity--)
	{
	    arg_i = val.ptr++;
	    Dereference_(arg_i);
	    if (IsRef(arg_i->tag))
		res = insert_suspension(ec_eng, arg_i, position, susp, slot);
	    else
		res = deep_suspend(ec_eng, arg_i->val, arg_i->tag, position, 
				susp, slot);
	    if (res != PSUCCEED)
		return res;
	}
	arg_i = val.ptr;		/* tail recursion */
	Dereference_(arg_i);
	val.all = arg_i->val.all;
	tag.all = arg_i->tag.all;
    }
}


pword *
add_attribute(ec_eng_t *ec_eng, word tv, pword *va, word ta, int slot)
{
    register pword *s, *t;

    s = TG;
    TG += 2 + MetaArity + 1;
    s[0].val.ptr = s;		/* metaterm */
    s[0].tag.kernel = TagNameField(tv) | RefTag(TMETA);
    s[1].val.ptr = s + 2;
    s[1].tag.kernel = TCOMP;
    s[2].val.did = in_dict("meta", MetaArity);
    s[2].tag.kernel = TDICT;
    for (t = &s[3]; t < TG; t++)
    {
	t->val.ptr = t;
	t->tag.kernel = TREF;
    }
    s[slot+2].val.ptr = va;
    s[slot+2].tag.kernel = ta;
    Check_Gc
    return s;
}

/*
 * Create the attribute for the suspend extension.
 * The first a difference list, the others are normal lists.
 */
static pword *
_suspension_attribute(ec_eng_t *ec_eng, pword *susp, int position)
{
    register pword	*t, *d, *s;
    register int	i;
    register int	arity = DidArity(d_.suspend_attr);

    if (position > arity) {
	position = 1;
    }

    t = TG;
    Push_Struct_Frame(d_.suspend_attr);
    d = TG;
    Push_Struct_Frame(d_.minus);
    s = TG;
    Push_List_Frame();

    s->val.ptr = susp;		/* list element */
    s->tag.kernel = TSUSP;
    Make_Struct(t+1, d);
    if (position == 1)
    {
	Make_List(d+1,s);	/* singleton dlist */
	Make_Ref(d+2,s+1);
	Make_Var(s+1);

	for(i=2; i<=arity; i++)
	{
	    Make_Nil(t+i);
	}
    }
    else
    {
	Make_Var(d+1);		/* empty dlist */
	Make_Ref(d+2,d+1);

	for(i=2; i<=arity; i++)
	{
	    if (i == position) {
		Make_List(t+i,s);
		Make_Nil(s+1);
	    } else {
		Make_Nil(t+i);
	    }
	}
    }
    return t;
}

int
insert_suspension(ec_eng_t *ec_eng,
	pword *var,
	int position,		/* must be > 0 */
	pword *susp,		/* must be dereferenced */
	int slot)
{
    register pword *s, *t;
    int			i;

    if (IsMeta(var->tag)) {		/* already a metaterm */

	t = MetaTerm(var)->val.ptr + slot;	/* find the dlist to insert */
	Dereference_(t);
	if (IsRef(t->tag)) {
	    if (slot != DELAY_SLOT)
		return ATTR_FORMAT;
	    s = _suspension_attribute(ec_eng, susp, position);
	    if (!s)
		return RANGE_ERROR;
	    Bind_Var(t->val, t->tag, s, TCOMP);
	    return PSUCCEED;
	} else if (!IsStructure(t->tag))
	    return ATTR_FORMAT;
	t = t->val.ptr;
	if ((DidArity(t->val.did)) < position) {
	    if (slot != DELAY_SLOT)
		return RANGE_ERROR;
	    position = 1;		/* force to the 1st list */
	}
	
	return ecl_enter_suspension(ec_eng, t+position, susp);
    }
    else if (IsRef(var->tag)) {
	if (slot != DELAY_SLOT)
	    return ATTR_FORMAT;
	t = _suspension_attribute(ec_eng, susp, position);
	if (!t)
	    return RANGE_ERROR;
	s = add_attribute(ec_eng, var->tag.kernel, t, (word) TCOMP, slot);
	Bind_Var(var->val, var->tag, s, TREF);
    }
    Check_Gc;
    return PSUCCEED;
}

int
ecl_enter_suspension(ec_eng_t *ec_eng, pword *t, pword *susp)
{
    register pword *s, *head;
    pword	   *dlp;

    dlp = t;
    Dereference_(t);
    s = TG;
    TG += 2;			/* make a list cell */
    s[0].val.ptr = susp;
    s[0].tag.kernel = TSUSP;
    if IsRef(t->tag) {		/* first insert */
	s[1].tag.kernel = TNIL;
	Bind_Var(t->val, t->tag, &s[0], TLIST);
    } else {
	if (IsStructure(t->tag)) {		/* it already exists */
	    t = t->val.ptr;
	    if (t->val.did != d_.minus)		/* check the functor */
		return ATTR_FORMAT;
	    head = ++t;
	    Dereference_(head);
	} else if (IsList(t->tag) || IsNil(t->tag)) {
	    /* not a difference list */
	    head = t;
	    t = dlp;
	} else
	    return ATTR_FORMAT;

	/*
	 * dlp is the (undereferenced) difference list pointer (if any)
	 * t is the (undereferenced) list pointer
	 * head is the (dereferenced) list pointer
	 */

	/*
	 * Incomplete garbage collection: Get rid of woken
	 * suspensions at the beginning of the list.
	 */
	while (IsList(head->tag))
	{
	    register pword *psusp = head->val.ptr;
	    Dereference_(psusp);
	    if (!IsTag(psusp->tag.kernel, TSUSP))
		return ATTR_FORMAT;
	    if (!SuspDead(psusp->val.ptr))
		break;
	    head = head->val.ptr + 1;
	    Dereference_(head);
	}

	/* head now points to the rest of the old suspension list */
 
	if (IsList(head->tag) || IsNil(head->tag)) {
	    s[1] = *head;
	    /* t may be TREF, TLIST or TNIL */
	    if (t < GB || !ISPointer(t->tag.kernel) || t->val.ptr < GB)
	    {
		Trail_Pword(t);
	    }
	    t->tag.kernel = TLIST;
	    t->val.ptr = s;
	} else if (!IsRef(head->tag))
	    return ATTR_FORMAT;
	else {				/* empty dlist, replace it */
	    value v;
	    s[1].val.ptr = &s[1];
	    s[1].tag.kernel = TREF;
	    TG += 3;
	    s[2].val.did = d_.minus;	/* new difference list header */
	    s[2].tag.kernel = TDICT;
	    s[3].val.ptr = s;
	    s[3].tag.kernel = TLIST;
	    s[4].val.ptr = &s[1];
	    s[4].tag.kernel = TREF;
	    v.ptr = &s[2];
	    (void) ecl_assign(ec_eng, dlp, v, tcomp);
	}
    }
    Check_Gc;
    return PSUCCEED;
}

int
ecl_notify_constrained(ec_eng_t *ec_eng, pword *pvar)
{
    pword	*p;

    if (!IsMeta(pvar->tag)) {
	Succeed_
    }
    p = MetaTerm(pvar->val.ptr);
    p = p->val.ptr + DELAY_SLOT;
    Dereference_(p);
    if (!IsStructure(p->tag)) {
	Succeed_
    }
    return ecl_schedule_susps(ec_eng, p->val.ptr + CONSTRAINED_OFF);
}

/*
 * Pick up the first woken goal with priority higher than prio,
 * remove it from its list and set WP to the priority
 */
pword *
first_woken(ec_eng_t *ec_eng, int prio)
{
    register int	i;
    register pword	*p = WL;
    register pword	*s;
    register pword	*t;
    register pword	*u;

    if (p == (pword *) 0)
	return 0;
    if (prio > WLMaxPrio(p))
	prio = WLMaxPrio(p) + 1;
    p = WLFirst(p) - 1;
    for (i = 1; i < prio; i++) {
	t = ++p;		/* no references allowed */
	if (IsList(t->tag)) {
	    for (;;) {
		t = t->val.ptr;
		s = t++;
		Dereference_(s);
		Dereference_(t);
		if (IsSusp(s->tag)) {
		    u = s->val.ptr;
		    if (!SuspDead(u))
			break;
		} else
		    p_fprintf(current_err_, "*** woken list %d is corrupted\n", i);
		if (IsNil(t->tag)) {
		    s = 0;
		    break;
		}
	    }
	    /* replace the list head */
	    if (p->val.ptr < GB) {
		Trail_Pword(p);
	    }
	    if (IsList(t->tag))
		p->val.ptr = t->val.ptr;
	    else
	    {
		/* Use a timestamp (which happens to look like a [])
		 * to terminate the list */
		Make_Stamp(p);
	    }
	    if (s) {
		Set_WP(SuspRunPrio(s))
		return s;
	    }
	}
    }
    return 0;
}

/*
 * Initialize the WL structure
 */
pword *
wl_init(ec_eng_t *ec_eng)
{
    pword	*p = TG;
    int	i;

    Push_Struct_Frame(d_.woken);
    *WLPrevious(p) = TAGGED_WL;
    Make_Integer(WLPreviousWP(p), WP);
    Make_Susp(WLPreviousLD(p), LD);
    for (i=WL_FIRST; i <= WL_ARITY; i++)
	p[i].tag.kernel = TNIL;
    return p;
}

/*
 * binding routine for non-standard variables
 *
 * receives:
 * 	pw1	a non-standard variable
 *		(ie. IsRef(pw1) && !IsVar(pw1))
 *	pw2	a general term, but not a (standard) free variable
 *		(ie. !IsVar(pw2))
 *
 * binds the non-standard variable pw1 to the term referenced by pw2
 */

int
bind_c(ec_eng_t *ec_eng, register pword *pw1, register pword *pw2, register pword **list)
{
    switch(TagType(pw1 -> tag))
    {
    case TNAME:			/* a named variable */
	pw1 = pw1->val.ptr;
	switch(TagType(pw2->tag))
	{
	case TNAME:
	    pw2 = pw2->val.ptr;
	    if (pw1 < pw2)
	    {
		Bind_Named(pw2, pw1);
	    }
	    else if (pw1 > pw2)
	    {
		Bind_Named(pw1, pw2);
	    }
	    break;

	case TMETA:
	    pw2 = pw2->val.ptr;
	    if (pw2 > pw1) /* we bind the "wrong" direction, copy the name */
	    {
		Trail_Tag_If_Needed_Gb(pw2)
		pw2->tag.kernel = TagNameField(pw1->tag.kernel) | RefTag(TMETA);
	    }
	    Bind_Named(pw1, pw2);
	    break;

	case TUNIV:
	    pw2 = pw2->val.ptr;
	    Bind_Named(pw1, pw2);
	    break;

	default:
	    Trail_Tag_If_Needed_Gb(pw1);
	    *pw1 = *pw2;
	}
	return PSUCCEED;

    case TMETA:
    {
	pw1 = pw1->val.ptr;
	switch(TagType(pw2->tag))
	{
	case TNAME:
	    pw2 = pw2->val.ptr;
	    if (pw1 > pw2) /* we bind the "wrong" direction, copy the name */
	    {
		Trail_Tag_If_Needed_Gb(pw1)
		pw1->tag.kernel = TagNameField(pw2->tag.kernel) | RefTag(TMETA);
	    }
	    Bind_Named(pw2, pw1);
	    return PSUCCEED;

	case TUNIV:
	    return PFAIL;

	case TMETA:
	    pw2 = pw2->val.ptr;
	    if (pw1 > pw2)
	    {
		Trail_Tag_If_Needed_Gb(pw1)
		pw1->tag.kernel = TREF;
		pw1->val.all = pw2->val.all;
	    }
	    else if (pw1 < pw2)
	    {
		Trail_Tag_If_Needed_Gb(pw2)
		pw2->tag.kernel = TREF;
		pw2->val.all = pw1->val.all;
		pw1 = pw2;
	    }
	    else
		return PSUCCEED;
	    break;

	default:
	    Trail_Tag_If_Needed_Gb(pw1)
	    *pw1 = *pw2;
	}

	pw2 = TG;
	TG += 2;
	Check_Gc;
	pw2[0].val.ptr = pw1;
	pw2[0].tag.kernel = TLIST;
	if (*list) {
	    pw2[1].val.ptr = *list;
	    pw2[1].tag.kernel = TLIST;
	} else {
	    pw2[1].tag.kernel = TNIL;
	    if (list == &MU) {
		Fake_Overflow;
	    }
	}
	*list = pw2;
	return PSUCCEED;
    }

    case TUNIV:
	/* TUNIV variables are all-quantified variables,
	 * so any attempt to constrain them must fail! */
	switch(TagType(pw2->tag))
	{
	case TNAME:
	    pw1 = pw1->val.ptr;
	    pw2 = pw2->val.ptr;
	    Bind_Named(pw2, pw1);
	    return PSUCCEED;
	case TUNIV:
	    if (pw1->val.ptr == pw2->val.ptr)
		return PSUCCEED;
	    /* else */
	default:
	    return PFAIL;
	}

/*
 * EXTENSION SLOT HERE
 */

    default: 
	p_fprintf(current_err_, "bind_c(): unknown tag (%x) encountered\n", 
		pw1->tag.kernel);
	return (PFAIL);
    }
}


/*
 * Instantiate a metaterm without triggering meta_unification events
 */

int
meta_bind(ec_eng_t *ec_eng, pword *pvar, value v, type t)
{
    if (IsVar(t) && v.ptr >= TG)	/* local -> meta */
    {
	Trail_If_Needed_Eb(v.ptr)
	v.ptr->val.ptr = pvar;
    }
    else				/* bind the metaterm pvar */
    {
	Trail_Tag_If_Needed_Gb(pvar)
	pvar->tag.all = t.all;
	pvar->val.all = v.all;
    }
    Succeed_;
}


/*
 * ecl_assign() - destructive assignment to a pword in the global stack
 *
 * Used to implement setarg/3 and the like.
 * It is not allowed to assign to a variable, in order to reduce the
 * confusing side effects caused by this facility [check has been removed].
 * Originally, we had the additional restriction that also the new value
 * of the pword should not be a variable to avoid multiple references
 * to the modified location. However, this proved to be too restrictive
 * for the applications, e.g. in difference lists.
 *
 * This solution should be optimal. Some thoughts about this problem:
 * To optimize space reuse and trailing, we need to know the age of
 * a binding. A binding is always younger than the bound location and
 * also younger than the binding value.
 * If the old binding was already done in the current choicepoint
 * segment (NewValue), we do not have to trail the update.
 * When the value we bind to is in the current choicepoint segment, we
 * can use it as the indicator of the binding age. If it is older, or
 * if we bind to a constant (which has no age), we create an intermediate
 * cell on top of the stack, so that we can later use its address to
 * determine the binding age. 
 */

int				/* returns PSUCCEED */
ecl_assign(
	ec_eng_t *ec_eng,
    	pword *argpw,	/* location to be modified */
	value v, type t)	/* the new value and tag */
{
#if 0
#ifdef PRINTAM
    if (!(TG_ORIG <= argpw && argpw < TG) &&
    	!((void *)ec_eng <= (void *)argpw &&
			   (void *)argpw < (void *)(ec_eng+1)))
    {
	pword *argpw1 = argpw;
	p_fprintf(current_output_,"INTERNAL ERROR: ecl_assign of heap term: ");
	Dereference_(argpw1)
	writeq_term(argpw1->val.all, argpw1->tag.all);
	ec_newline(current_output_);
    }
#endif
#endif
    if (IsVar(t) && v.ptr > TG)	/* globalize local variables */
    {
	register pword *new = TG++;
	Check_Gc;
	new->val.ptr = new;
	new->tag.kernel = TREF;
	Trail_If_Needed(v.ptr)
	v.ptr = v.ptr->val.ptr = new;
    }

    if (!NewLocation(argpw))		/* not completely deterministic */
    {
	if (!NewValue(v, t))		/* binding age will not be implicit */
	{
	    register pword *new = TG++; /* create an intermediate cell */
	    Check_Gc;
	    new->val.all = v.all;
	    new->tag.all = t.all;
	    v.ptr = new;
	    t.kernel = TREF;
	}
	if (!NewValue(argpw->val, argpw->tag))
	{
					/* old binding wasn't in this sgmt */
	    Trail_Pword(argpw);		/* don't "optimize" this (bug #609) */
	}
    }
    argpw->tag.all = t.all;
    argpw->val.all = v.all;
    Succeed_;
}


/*
 * pword *ec_nonground(val,tag)
 *
 * Check if a term is nonground. Returns a pointer to the first
 * variable encountered, otherwise NULL.
 */

pword *
ec_nonground(value val, type tag)	/* expects a dereferenced argument */
{
    register int arity;
    register pword *arg_i;

    for (;;)
    {
	if (IsRef(tag))
	    return val.ptr;
	else if (IsList(tag))
	    arity = 2;
	else if (IsStructure(tag))
	{
	    arity = DidArity(val.ptr->val.did);
	    val.ptr++;
	}
	else
	    return (pword *) 0;
 
	for(;arity > 1; arity--)
	{
	    register pword *pvar;
	    arg_i = val.ptr++;
	    Dereference_(arg_i);
	    if ((pvar = ec_nonground(arg_i->val,arg_i->tag)))
		return pvar;
	}
	arg_i = val.ptr;		/* tail recursion */
	Dereference_(arg_i);
	val.all = arg_i->val.all;
	tag.all = arg_i->tag.all;
    }
}

/*---------------------------------------------
 * Cut across PB
 *---------------------------------------------*/

#ifdef PB_MAINTAINED

int
cut_across_pb(ec_eng_t *ec_eng, old_b)
pword *old_b;		/* old_b < PB */
{
    do
    {
	PB = BPar(PB)->ppb;
    } while (old_b < PB);
    if (old_b < PPB) {
	PPB = PB;
	do
	    PPB = BPar(PPB)->ppb;
	while (old_b < PPB);
	return cut_public();
    }
    return 1;
}

#endif

/*---------------------------------------------
 * Trailing/Untrailing
 *---------------------------------------------*/

/*
 * This function extends the Untrail_Variables() macro.
 * It is called when the trail is neither address nor tag nor value trail.
 *
 * Untrail the extended trail frame that trail_ptr points to.
 * The frame must be popped by the caller !
 *
 * This function (when called during failure) relies on TG/GB having
 * their pre-failure values!
 */

void
untrail_ext(ec_eng_t *ec_eng, pword **trail_ptr, int undo_context)
{
    switch(TrailedEtype(*trail_ptr))
    {

    case TRAIL_UNDO:
	/* call undo function */
	(* (void(*)(pword*,word*,int,int,ec_eng_t*)) (trail_ptr[TRAIL_UNDO_FUNCT])) (
		trail_ptr[TRAIL_UNDO_ADDRESS],
		(word*) (trail_ptr + TRAIL_UNDO_SIMPLE_HEADER_SIZE),
		TrailedEsize(trail_ptr[TRAIL_UNDO_FLAGS]) - TRAIL_UNDO_SIMPLE_HEADER_SIZE,
		undo_context,
		ec_eng
	    );
	break;

    case TRAIL_UNDO_STAMPED:
	/*
	 * first reset timestamp
	 * this is not done in gc because the stamp location may already be
	 * marked. The only consequence of this is that the stamp keeps
	 * an extra witness alive.
	 */
	if (undo_context == UNDO_FAIL)
	{
	    trail_ptr[TRAIL_UNDO_STAMP_ADDRESS]->val.ptr = trail_ptr[TRAIL_UNDO_OLDSTAMP];

	    /* do nothing if the trail is redundant according to timestamp */
	    if (!OldStamp(trail_ptr[TRAIL_UNDO_STAMP_ADDRESS]))
		return;
	}
	/* then call undo function */
	(* (void(*)(pword*,word*,int,int,ec_eng_t*)) (trail_ptr[TRAIL_UNDO_FUNCT])) (
		trail_ptr[TRAIL_UNDO_ADDRESS],
		(word*) (trail_ptr + TRAIL_UNDO_STAMPED_HEADER_SIZE),
		TrailedEsize(trail_ptr[TRAIL_UNDO_FLAGS]) - TRAIL_UNDO_STAMPED_HEADER_SIZE,
		undo_context,
		ec_eng
	    );
	break;

/* EXTENSION SLOT HERE */

    }
}


/*
 * _untrail_cut_action()
 * called only by untrail_ext() during untrailing
 */
static void
_untrail_cut_action(pword *action_frame, word *pdata, int data_size, int undo_context, ec_eng_t *ec_eng)
{
    if (action_frame == LCA)
    {
	do_cut_action(ec_eng);
    }
    /* else the action has already been executed by a cut */
}
    

/*
 * do_cut_action() is called at cut time or during untrailing
 * The LCA register is a pointer to a cut action frame with the format:
 *
 *	TDICT	arg/3				don't care functor
 *	TCOMP	<ptr to next (older) action>	chain of cut actions
 *	TINT	<address of C action function>
 *	TAG	VAL				argument for the action
 */
void
do_cut_action(ec_eng_t *ec_eng)
{
    /* call the action function */
    (* (void(*)(value,type,ec_eng_t*)) (LCA[2].val.ptr)) (LCA[3].val, LCA[3].tag, ec_eng);

    /* advance the LCA register */
    if (IsStructure(LCA[1].tag))
	LCA = LCA[1].val.ptr;
    else
	LCA = (pword *) 0;
}


/*
 * ecl_schedule_cut_fail_action(ec_eng, function, v, t)
 *
 * create a cut-action frame on the global stack and a corresponding
 * undo-frame on the trail.
 * The cut-action frame is linked into the global list of cut-action frames,
 * starting with the LCA register.
 */
void
ecl_schedule_cut_fail_action(
	ec_eng_t *ec_eng,
	void	(*function)(value, type, ec_eng_t*),
	value	v,
	type	t)
{
    pword *action_frame = TG;

    TG += 4;
    Check_Gc;
    action_frame[0].val.did = d_.arg;	/* just any arity 3 functor ... */
    action_frame[0].tag.kernel = TDICT;
    action_frame[1].val.ptr = LCA;
    if (LCA)
	action_frame[1].tag.kernel = TCOMP;
    else
	action_frame[1].tag.kernel = TNIL;
    action_frame[2].val.ptr = (pword *) function;
    action_frame[2].tag.kernel = TINT;
    action_frame[3].val.all = v.all;
    action_frame[3].tag.all = t.all;

    Trail_Undo(action_frame, _untrail_cut_action);
    LCA = action_frame;
}


/*
 * C function interfaces for use in extensions
 */

/*
 * The function to create an (optionally time-stamped) undo trail:
 * 
 * void ec_trail_undo(
 *	ec_eng,		engine
 *	function,	address of untrail function
 *	pitem,		address of related item, or NULL
 *			(pointer to pword on heap, or anything elsewhere)
 *	pstamp,		address of time stamp (we only trail if it is old)
 *			or NULL for non-timestamped trail
 *	pdata,		pointer to untrail data or NULL
 *	data_size,	size of untrail data in words (0..2^23)
 *	data_type	TRAILED_PWORD or TRAILED_WORD32
 *   )
 *
 * The untrail function will later be called as follows:
 *
 * void undo(
 *	pitem,		address of related item
 *	pdata,		pointer to untrail data
 *	data_size,	size of untrail data in words
 *	undo_context,	UNDO_FAIL or UNDO_GC
 *	ec_eng_t*
 * )
 */

void
ecl_trail_undo(
	ec_eng_t *ec_eng,
	void	(*function)(pword*,word*,int,int,ec_eng_t*),
	pword	*pitem,
	pword	*pstamp,
	word	*pdata,
	int	data_size,
	int	data_type)
{
    int i;
    uword *traildata = (uword *)TT - data_size;

    if (pstamp)
    {
	if (!OldStamp(pstamp))	/* trail redundant? */
	    return;

	TT = (pword **) (traildata - TRAIL_UNDO_STAMPED_HEADER_SIZE);
	Check_Trail_Ov
	TT[TRAIL_UNDO_FLAGS] = (pword *)
    		( TrailedEsizeField(TRAIL_UNDO_STAMPED_HEADER_SIZE + data_size)
		| TrailedEtypeField(TRAIL_UNDO_STAMPED)
		| TRAIL_EXT | (data_type & TRAILED_TYPE_MASK));
	TT[TRAIL_UNDO_STAMP_ADDRESS] = pstamp;
	TT[TRAIL_UNDO_OLDSTAMP] = ISPointer(pstamp->tag.kernel) ? pstamp->val.ptr : 0;
	Make_Stamp(pstamp);
    }
    else
    {
	TT = (pword **) (traildata - TRAIL_UNDO_SIMPLE_HEADER_SIZE);
	Check_Trail_Ov
	TT[TRAIL_UNDO_FLAGS] = (pword *)
    		( TrailedEsizeField(TRAIL_UNDO_SIMPLE_HEADER_SIZE + data_size)
		| TrailedEtypeField(TRAIL_UNDO)
		| TRAIL_EXT | (data_type & TRAILED_TYPE_MASK));
    }

    TT[TRAIL_UNDO_ADDRESS] = pitem;
    *((void (**)(pword*,word*,int,int,ec_eng_t*)) (TT+TRAIL_UNDO_FUNCT)) = function;

    for(i=0; i<data_size; ++i)
    {
	traildata[i] = ((uword *) pdata)[i];
    }
}



#define GlobalRef(ref)		((ref) < TG && (ref) >= TG_ORIG)
#define LocalRef(ref)		((ref) < SP_ORIG && (ref) >= SP)
#define TrailRef(ref)		((pword**)(ref) < TT_ORIG && (pword**)(ref) >= TT)
#define MachineRef(ref)		((word*)(&ec_) <= (word*)(ref) && (word*)(ref) < (word*)(&ec_ + 1))

/*
 * This function checks very thoroughly that the pointer is a valid local
 * or global reference.
 */
int
check_pword(ec_eng_t *ec_eng, pword *ref)
{
    int		arity;

    if (!(GlobalRef(ref) || LocalRef(ref)
    	|| TrailRef(ref) || address_in_heap(&global_heap, ref)
	|| MachineRef(ref)))
	return 0;
    /* Now we can test the contents */
    switch (TagType(ref->tag))
    {
    case TLIST:
	if (!(GlobalRef(ref->val.ptr) || address_in_heap(&global_heap, ref->val.ptr)))
	    return 0;
	return check_pword(ec_eng, ref->val.ptr) && check_pword(ec_eng, ref->val.ptr+1);

    case TCOMP:
	ref = ref->val.ptr;
	if (!ref)
	    return 1;	/* this is for the TCOMP|0 in woken/15 */
	if (!(GlobalRef(ref) || address_in_heap(&global_heap, ref->val.ptr)))
	    return 0;
	if (bitfield_did((word) DidBitField(ref->val.did)) != ref->val.did)
	    return 0;
	arity = DidArity(ref->val.did);
	for (ref++; arity; arity--, ref++)
	    if (!check_pword(ec_eng, ref))
		return 0;
	return 1;

    case TSTRG:
    case TBIG:
#ifndef UNBOXED_DOUBLES
    case TDBL:
#endif
    case TIVL:
	if (!(GlobalRef(ref->val.ptr) || address_in_heap(&global_heap, ref->val.ptr)))
	    return 0;
	return TagType(ref->val.ptr->tag) == TBUFFER;

    case TRAT:
	if (!(GlobalRef(ref->val.ptr) || address_in_heap(&global_heap, ref->val.ptr)))
	    return 0;
	return TagType(ref->val.ptr->tag) == TBIG;

    case TSUSP:
	ref = ref->val.ptr;
	if (!GlobalRef(ref))
	    return 0;
	return TagType(ref->tag) == TDE &&
		(ref->val.ptr == 0 || GlobalRef(ref->val.ptr));

    case TNIL:
    case TINT:
#ifdef UNBOXED_DOUBLES
    case TDBL:
#endif
	return 1;

    case TDICT:
	return bitfield_did((word) DidBitField(ref->val.did)) == ref->val.did;

    case TVAR_TAG:
	if (ref->val.ptr != ref)
	    return check_pword(ec_eng, ref->val.ptr);
	return 1;

    case TNAME:
	if (ref->val.ptr != ref)
	    return check_pword(ec_eng, ref->val.ptr);
	return (IsNamed(ref->tag.kernel) &&
	    address_in_heap(&global_heap, (pword *) TagDid(ref->tag.kernel)));

    case TMETA:
	if (ref->val.ptr != ref)
	    return check_pword(ec_eng, ref->val.ptr);
	return check_pword(ec_eng, ref->val.ptr + 1);

    default:
	return 0;
    }
}

#ifdef PRINTAM
/*---------------------------------------
 * Debugging support
 *---------------------------------------*/

void
check_arg(ec_eng_t *ec_eng, pword *pw)
{
    switch (TagType(pw->tag))
    {
    case TCOMP:
	if (SameTypeC(pw->val.ptr->tag, TDICT))
	    return;
	break;
    case TLIST:
	return;
    case TSUSP:
	if (pw->val.ptr < TG && pw->val.ptr >= TG_ORIG)
	    return;
	break;
    case THANDLE:
	if (pw->val.ptr < TG && pw->val.ptr >= TG_ORIG
	 && SameTypeC(pw->val.ptr[0].tag, TEXTERN)
	 && SameTypeC(pw->val.ptr[1].tag, TPTR))
	    return;
	break;
    case TIVL:
    case TBIG:
    case TSTRG:
#ifndef UNBOXED_DOUBLES
    case TDBL:
#endif
	if (SameTypeC(pw->val.ptr->tag, TBUFFER))
	    return;
	break;
    case TRAT:
	if (SameTypeC(pw->val.ptr[0].tag, TBIG) &&
	    SameTypeC(pw->val.ptr[1].tag, TBIG))
	    return;
	break;
    case TNIL:
    case TINT:
    case TDICT:
#ifdef UNBOXED_DOUBLES
    case TDBL:
#endif
	return;
    case TVAR_TAG:
	return;
    case TNAME:
    case TMETA:
    case TUNIV:
	if (pw->val.ptr < TG && pw->val.ptr >= TG_ORIG)
	    return;
	break;
    }
    p_fprintf(current_err_,
	"INTERNAL ERROR: illegal pword encountered: val=%x tag=%x\n",
	pw->val.all, pw->tag.all);
    ec_flush(current_err_);
}


#define InGlobal(p)  ((p) >= min && (p) < max)
#define InHeap(p)  (address_in_heap(&global_heap, p))

void
check_global1(ec_eng_t *ec_eng, register pword *min, register pword *max)
{
    register pword *pw = min;
    extern pword    woken_susp_;

    if (ec_eng->nesting_level > 1)
	return;

    while (pw < max)
    {
	switch (TagType(pw->tag))
	{
	case TVAR_TAG:
	case TNAME:
	case TMETA:
	case TUNIV:
	    if (!IsRef(pw->tag))
		goto _problem_;
	    if (!InGlobal(pw->val.ptr))
		goto _problem_;
	    pw++;
	    break;

	case TCOMP:
	    /*
	    if (pw->val.ptr && !InGlobal(pw->val.ptr) && !IsPersistent(pw->tag))
		goto _problem_;
	    */
	    if (pw->val.ptr &&
	    	(!IsAtom(pw->val.ptr->tag) || DidArity(pw->val.ptr->val.did) == 0))
		goto _problem_;
	    pw++;
	    break;

	case TSTRG:
	case TBIG:
#ifndef UNBOXED_DOUBLES
	case TDBL:
#endif
	case TIVL:
	    /*
	    if (!InGlobal(pw->val.ptr) && !IsPersistent(pw->tag)) goto _problem_;
	    */
	    if (DifferTypeC(pw->val.ptr->tag,TBUFFER)) goto _problem_;
	    pw++;
	    break;

	case TRAT:
	    if (!InGlobal(pw->val.ptr) && !IsPersistent(pw->tag)) goto _problem_;
	    if (DifferTypeC(pw->val.ptr[0].tag, TBIG) ||
		DifferTypeC(pw->val.ptr[1].tag, TBIG)) goto _problem_;
	    pw++;
	    break;

	case TSUSP:
	    if (pw->val.ptr) {
		if (!InGlobal(pw->val.ptr) && pw->val.ptr != &woken_susp_) goto _problem_;
		if (DifferTypeC(pw->val.ptr->tag,TDE)) goto _problem_;
	    }
	    pw++;
	    break;

	case TLIST:
	    if (!InGlobal(pw->val.ptr) && !IsPersistent(pw->tag)) goto _problem_;
	    pw++;
	    break;

	case THANDLE:
	    if (!InGlobal(pw->val.ptr)) goto _problem_;
	    if (DifferTypeC(pw->val.ptr[0].tag, TEXTERN) ||
		DifferTypeC(pw->val.ptr[1].tag, TPTR)) goto _problem_;
	    pw++;
	    break;

	case TNIL:
	case TINT:
	case TDICT:
#ifdef UNBOXED_DOUBLES
	case TDBL:
#endif
	    pw++;
	    break;

	case TBUFFER:
	    pw += BufferPwords(pw);
	    break;

	case TEXTERN:
	    pw += 2;
	    break;

	case TDE:
	    pw += SUSP_SIZE - 2;
	    break;

	default:
	    goto _problem_;
	}
    }
    return;
_problem_:
    p_fprintf(current_err_,
	"INTERNAL ERROR: illegal pword encountered at 0x%x: val=0x%x tag=0x%x\nwhile scanning 0x%x .. 0x%x\n",
	pw, pw->val.all, pw->tag.all, min, max);
    ec_flush(current_err_);
    return;
}

void
check_global(ec_eng_t *ec_eng)
{
    check_global1(ec_eng, TG_ORIG, TG);
}

void
check_global2(ec_eng_t *ec_eng, pword *max)
{
    check_global1(ec_eng, TG_ORIG, max);
}

void
find_in_trail(ec_eng_t *ec_eng, pword *addr)
{
    pword **tr = TT;
    pword *trailed_item;
    long	i;

    while(tr < TT_ORIG)
    {
	switch((((word) *tr) & 3))
	{
	case TRAIL_ADDRESS:
	    trailed_item = *tr++;
	    break;
	case TRAIL_TAG:
	    trailed_item = *(tr+1);
	    break;
	case TRAIL_MULT:
	    i = (word) *tr;
	    trailed_item = (pword *)((uword *)(*(tr+1)) + TrailedOffset(i));
	    break;
	case TRAIL_EXT:
	    break;
	}
	if (trailed_item == addr)
	{
	    p_fprintf(current_err_,
		"Trail entry found for 0x%x at 0x%x\n", trailed_item, tr);
	    ec_flush(current_err_);
	}
	End_Of_Frame(tr, tr);
    }
}

void check_trail2(int print, ec_eng_t *ec_eng, pword **ttptr, pword **ttend, pword *min_tg_when_failing);

void
check_trail(ec_eng_t *ec_eng)
{
    extern vmcode par_fail_code_[];
    control_ptr fp;
    pword **tt = TT;
    pword *tg = TG;
    int print = 0;

    for(fp.args = B.args;;fp.args = BPrev(fp.args))
    {
	if (BPrev(fp.args) == (pword *) (fp.top - 1))
	{
	    /* small if-then-else choicepoint */
	}
	else
	{
	    check_trail2(print, ec_eng, tt, BChp(fp.args)->tt, tg);
	    tt = BChp(fp.args)->tt;
	    tg = BChp(fp.args)->tg;
	    break;
	}

	if (IsRecursionFrame(BTop(fp.args)))
	    break;
    }
    if (print) p_fprintf(current_err_, "BOTTOM\n");
    if (print) ec_flush(current_err_);
}

void
check_trail1(int print, ec_eng_t *ec_eng)
{
    check_trail2(print, ec_eng, TT, TT_ORIG, TG);
}

void
check_trail2(int print, ec_eng_t *ec_eng, pword **ttptr, pword **ttend, pword *min_tg_when_failing)
{
    word ctr, trtype;
    pword *pw;
    while(ttptr < ttend) {
	if (print) p_fprintf(current_err_, "TT=0x%08x: ", ttptr);
	switch((((word) *ttptr) & 3)) {
	case TRAIL_ADDRESS:
	    pw = *ttptr++;
	    if (print) p_fprintf(current_err_, "ADDRESS 0x%08x\n", pw);
	    if (min_tg_when_failing <= pw && pw < (pword*)TT)
	    {
		p_fprintf(current_err_, "Trailed address = 0x%08x\n", pw);
		emu_break();
	    }
	    break;
	case TRAIL_TAG:
	    pw = *(ttptr+1);
	    if (print) p_fprintf(current_err_, "TAG     0x%08x 0x%08x\n", pw, TrailedTag(*ttptr));
	    if (min_tg_when_failing <= pw && pw < (pword*)TT)
	    {
		p_fprintf(current_err_, "Trailed address = 0x%08x\n", pw);
		emu_break();
	    }
	    ttptr += 2;
	    break;
	case TRAIL_MULT:
	    ctr = (word) *ttptr++;
	    pw = *ttptr++;
	    trtype = TrailedType(ctr);
	    ctr = TrailedNumber(ctr);
	    if (print) {
		p_fprintf(current_err_, "MULT    0x%08x n=%d (%s)\n", pw, ctr+1,
	    	trtype==TRAILED_PWORD ? "pword" :
	    	trtype==TRAILED_REF ? "ref" :
	    	trtype==TRAILED_WORD32 ? "word" :
	    	trtype==TRAILED_COMP ? "comp" : "?");
		ec_flush(current_err_);
	    }
	    if (min_tg_when_failing <= pw && pw < (pword*)TT)
	    {
		p_fprintf(current_err_, "Trailed address = 0x%08x\n", pw);
		emu_break();
	    }
#if 1
	    if (!check_pword(ec_eng, pw) && !(
		    pw == &POSTED
		||
		    IsTag(pw->tag.kernel, TDE)
	    	))
	    {
		emu_break();
	    }
#endif
	    do {
		ttptr++;
	    } while (ctr--);
	    break;
	case TRAIL_EXT:
	    switch(TrailedEtype(*ttptr)) {
	    case TRAIL_UNDO:
		if (print) p_fprintf(current_err_, "UNDO    0x%08x\n", ttptr[TRAIL_UNDO_ADDRESS]);
		break;
	    case TRAIL_UNDO_STAMPED:
		if (print) p_fprintf(current_err_, "UNDO_ST 0x%08x\n", ttptr[TRAIL_UNDO_ADDRESS]);
#if 0
		if (ttptr[TRAIL_UNDO_OLDSTAMP] >= min_tg_when_failing)
		{
		    p_fprintf(current_err_, "UNDO_ST redundant 0x%08x\n", ttptr[TRAIL_UNDO_OLDSTAMP]);
		    ec_flush(current_err_);
		}
#endif
		if (TrailedType(*ttptr) == TRAILED_PWORD)
		{
		    word n_pwords = (TrailedEsize(*ttptr) - TRAIL_UNDO_STAMPED_HEADER_SIZE)/2;
		    pw = (pword *) (ttptr + TRAIL_UNDO_STAMPED_HEADER_SIZE);
		    for(; n_pwords > 0; --n_pwords, ++pw)
		    {
			if (ISPointer(pw->tag.kernel))
			{
			    if (min_tg_when_failing <= pw->val.ptr && pw->val.ptr < (pword*)TT)
				emu_break();
			    if (IsString(pw->tag) && !IsTag(pw->val.ptr->tag.kernel, TBUFFER))
				emu_break();
			}
		    }
		}
		break;
	    }
	    ttptr += TrailedEsize(*ttptr);
	    break;
	}
    }
    if (print) p_fprintf(current_err_, "TT=0x%08x: STOP\n", ttptr);
    if (print) ec_flush(current_err_);
}

#endif /* PRINTAM */
