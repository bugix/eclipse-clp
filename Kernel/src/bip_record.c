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
 * Copyright (C) 1989-2007 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: bip_record.c,v 1.10 2017/09/04 01:44:29 jschimpf Exp $
 */

/* ********************************************************************
 *
 *	ECLiPSe built-ins for the indexed database
 *
 ******************************************************************** */

#include	"config.h"
#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "error.h"
#include	"dict.h"
#include	"emu_export.h"
#include	"property.h"
#include	"module.h"
#include	"os_support.h"


#include <stdio.h>	/* for sprintf() */


/*----------------------------------------------------------------------
 * Recorded database primitives
 *
 * Data structure is a circular doubly linked list with one dummy
 * element as header. The header is referred to by the IDB_PROP property,
 * or passed around as a handle of type heap_rec_header_tid.
 * 
 * Individual recorded items are identified by their list element
 * and handles of type heap_rec_tid are used as "db references".
 * They are always created as part of a record-list, but can continue
 * to exist independently when their db-reference was obtained and
 * they were subsequently erased from the list.
 *
 * Locking policy:
 *  - all list elements (incl header) are individually reference-counted
 *  - read/write of the circular chain uses the header's lock
 *  - reading the .term can be done without lock (by any reference owner)
 *    because it is not mutable
 *----------------------------------------------------------------------*/


/* INSTANCE TYPE DECLARATION */

typedef struct record_elem {
    uword		ref_ctr;	/* one count for list membership */
    struct record_elem	*next, *prev;	/* NULL if not in list */

    struct record_header *header;	/* const */
    pword		term;		/* const */
} t_heap_rec;

/* first 3 fields must match those in struct record_elem! */
typedef struct record_header {
    uword		ref_ctr;	/* one count for list membership */
    struct record_elem	*next, *prev;	/* never NULL (self ref if empty) */

    word		max;		/* signal when count below max */
    word		count;		/* number of list elements */
    ec_mutex_t		lock;		/* protect next/prev list access */
    ec_cond_t		*cond;
} t_heap_rec_hdr;


/* METHODS */


/* Allocation of record elements */

static t_heap_rec *
_rec_create_elem(t_heap_rec_hdr *header)
{
    t_heap_rec *obj = (t_heap_rec *) hg_alloc_size(sizeof(t_heap_rec));
    obj->ref_ctr = 1;
    obj->next = obj->prev = obj;
    obj->header = header;
    obj->term.val.nint = 0;
    obj->term.tag.kernel = TEND;
    return obj;
}

/* Allocation of record header */

static t_heap_rec_hdr *
_rec_create(void)
{
    t_heap_rec_hdr *obj = (t_heap_rec_hdr *) hg_alloc_size(sizeof(t_heap_rec_hdr));
    obj->ref_ctr = 1;
    obj->next = obj->prev = (t_heap_rec*) obj;
    obj->max = 0;
    obj->count = 0;
    obj->cond = NULL;
    mt_mutex_init_recursive(&obj->lock);
    return obj;
}


t_ext_ptr
ec_record_create(void)
{
    return (t_ext_ptr) _rec_create();
}


/* Lose a reference to an element */

static void
_rec_free_elem(t_heap_rec *this)
{
    int rem = ec_atomic_add(&this->ref_ctr, -1);
    if (rem <= 0)
    {
	assert(rem==0);
#ifdef DEBUG_RECORDS
	ec_printff(current_err_, "\n_rec_free_elem(0x%x)", this);
#endif
	free_heapterm(&this->term);
	hg_free_size(this, sizeof(t_heap_rec));
    }
}


/* Remove and lose all elements from header's list (but note that the
 * elements may survive if db-references to them still exist) */

static void
_rec_free_elems(t_heap_rec_hdr *header)
{
    t_heap_rec *this = header->next;
    while (this != (t_heap_rec *)header)
    {
	t_heap_rec *next = this->next;
	this->prev = this->next = 0;
	_rec_free_elem(this);
	this = next;
    }
    header->count = 0;
    header->next = header->prev = (t_heap_rec *)header;
}


/* Lose a reference to the whole list identified by header */

static void
_rec_free_all(t_heap_rec_hdr *header)
{
    int rem = ec_atomic_add(&header->ref_ctr, -1);
    if (rem <= 0)
    {
#ifdef DEBUG_RECORDS
	ec_printff(current_err_, "\n_rec_free_all(0x%x)", header);
#endif
	_rec_free_elems(header); /* last ref, no lock needed */
	mt_mutex_destroy(&header->lock);
	if (header->cond) {
	    ec_cond_destroy(header->cond);
	    hg_free_size(header->cond, sizeof(ec_cond_t));
	}
	hg_free_size(header, sizeof(t_heap_rec_hdr));
    }
}


static t_heap_rec *
_rec_copy_elem(t_heap_rec *this)	/* this != NULL */
{
    ec_atomic_add(&this->ref_ctr, 1);
    return this;
}


static void
_rec_mark_elem(t_heap_rec *this)	/* this != NULL */
{
    mark_dids_from_heapterm(&this->term);
}


static void
_rec_mark_all(t_heap_rec_hdr *header)	/* header != NULL */
{
    t_heap_rec *this = header->next;
    while (this != (t_heap_rec*)header)
    {
	_rec_mark_elem(this);
	this = this->next;
    }
}

static dident
_kind_record()
{
    return d_.record;
}

static dident
_kind_dbref()
{
    return d_.dbref;
}

static int
_rec_lock(t_heap_rec_hdr *obj)
{
    return mt_mutex_lock(&obj->lock);
}

static int
_rec_trylock(t_heap_rec_hdr *obj)
{
    return mt_mutex_trylock(&obj->lock);
}

static int
_rec_unlock(t_heap_rec_hdr *obj)
{
    return mt_mutex_unlock(&obj->lock);
}

static int
_rec_signal(t_heap_rec_hdr *obj, int all)
{
    ec_cond_t *cv = obj->cond;
    /* no need to signal when no waiters */
    return cv? ec_cond_signal(cv, all) : 0;
}

static int
_rec_wait(t_heap_rec_hdr *obj, int timeout_ms)
{
    ec_cond_t *cv = obj->cond;
    /* create condition variable lazily (we expect obj to be locked here) */
    if (!cv) {
	cv = (ec_cond_t*) hg_alloc_size(sizeof(ec_cond_t));
	ec_cond_init(cv);
	obj->cond = cv;
    }
    return ec_cond_wait(cv, &obj->lock, timeout_ms);
}


/* CLASS DESCRIPTOR (method table) */
t_ext_type heap_rec_tid = {
    (void (*)(t_ext_ptr)) _rec_free_elem,
    (t_ext_ptr (*)(t_ext_ptr)) _rec_copy_elem,
    (void (*)(t_ext_ptr)) _rec_mark_elem,
    0, /* string_size */
    0, /* to_string */
    0,	/* equal */
    (t_ext_ptr (*)(t_ext_ptr)) _rec_copy_elem,
    0,	/* get */
    0,	/* set */
    _kind_dbref, /* kind */
    0,	/* lock */
    0,	/* trylock */
    0	/* unlock */
};

t_ext_type heap_rec_header_tid = {
    (void (*)(t_ext_ptr)) _rec_free_all,
    (t_ext_ptr (*)(t_ext_ptr)) _rec_copy_elem,
    (void (*)(t_ext_ptr)) _rec_mark_all,
    0, /* string_size */
    0, /* to_string */
    0,	/* equal */
    (t_ext_ptr (*)(t_ext_ptr)) _rec_copy_elem,
    0,	/* get */
    0,	/* set */
    _kind_record,
    (int (*)(t_ext_ptr)) _rec_lock,
    (int (*)(t_ext_ptr)) _rec_trylock,
    (int (*)(t_ext_ptr)) _rec_unlock,
    (int (*)(t_ext_ptr,int)) _rec_signal,
    (int (*)(t_ext_ptr,int)) _rec_wait
};


/*----------------------------------------------------------------------
 * PROLOG INTERFACE
 *----------------------------------------------------------------------*/


/* Get the record header from either the functor key or a handle.
 * "Hold" the object for later release.
 */

static int
_get_rec_list(value vrec, type trec, value vmod, type tmod, t_heap_rec_hdr **pheader, ec_eng_t *ec_eng)
{
    if (SameTypeC(trec, THANDLE))
    {
	Get_Typed_Object(vrec, trec, &heap_rec_header_tid, *pheader); /* may return STALE_HANDLE */
    }
    else
    {
	t_ext_ptr header;
	dident key_did;
	int err;
	Get_Key_Did(key_did,vrec,trec)
        Check_Module(tmod, vmod)
	err = get_visible_property_handle(key_did, IDB_PROP, vmod.did, tmod, &heap_rec_header_tid, &header);
	if (err < 0)
	    return err == PERROR ? NO_LOCAL_REC : err;
	Hold_Object_Until_Done(&heap_rec_header_tid,header);
	*pheader = (t_heap_rec_hdr*) header;
    }
    return PSUCCEED;
}


/*
 * is_record(Key)@Module checks whether Key is a record key (or handle)
 * on which recorded terms have been (and still are) stored.
 */

static int
p_is_record_body(value vrec, type trec, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_rec_hdr *header;
    int		err;

    err = _get_rec_list(vrec, trec, vmod, tmod, &header, ec_eng);
    if (err == NO_LOCAL_REC || err == STALE_HANDLE)
	err = PFAIL;
    else if (err == PSUCCEED && header->next == (t_heap_rec*) header)
	err = PFAIL;
    return err;
}

  
/**
 * recorded_count(+Key, -Count) is det
 * Return 0 if a named key does not exist.
 */
static int
p_recorded_count(value vrec, type trec, value vc, type tc, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_rec_hdr *header;
    word count;
    int err;

    err = _get_rec_list(vrec, trec, vmod, tmod, &header, ec_eng);
    if (err == PSUCCEED)
    	count = ec_atomic_load(&header->count);
    else if (err == NO_LOCAL_REC)
    	count = 0;
    else
	{ Bip_Error(err); }
    Return_Unify_Integer(vc, tc, count);
}


/**
 * record_set_max(+RecordHandle, +Max) is det
 * Set Key's maximum queue size to Max (unless the old max is greater).
 * This setting applies only to record_wait_append/4.
 * Helper predicate, only to be used while handle is locked.
 */
static int
p_record_set_max(value vrec, type trec, value vc, type tc, ec_eng_t *ec_eng)
{
    t_heap_rec_hdr *header;

    Check_Integer(tc);
    if (vc.nint < 0) {
	Bip_Error(RANGE_ERROR)
    }
    Get_Typed_Object(vrec, trec, &heap_rec_header_tid, header);
    if (header->max < vc.nint)
	header->max = vc.nint;
    Succeed_;
}


/**
 * record_below_max(+RecordHandle) is semidet
 * Fail if there are max or more record entries, otherwise reset max
 * to zero, assuming that all waiters are going to be signaled.
 * Helper predicate, only to be used while handle is locked!
 */
static int
p_record_below_max(value vrec, type trec, ec_eng_t *ec_eng)
{
    t_heap_rec_hdr *header;

    Get_Typed_Object(vrec, trec, &heap_rec_header_tid, header);
    if (header->count >= header->max) {
	Fail_;
    }
    /* don't signal until new waiters appear - this assumes they
     * all get signaled! */
    header->max = 0;
    Succeed_;
}


/* record_create(-Handle) creates an anonymous record */

static int
p_record_create(value vrec, type trec, ec_eng_t *ec_eng)
{
    pword rec;
    Check_Ref(trec);
    rec = ecl_handle(ec_eng, &heap_rec_header_tid, (t_ext_ptr) _rec_create());
    Return_Unify_Pw(vrec, trec, rec.val, rec.tag);
}


static int
p_local_record_body(value vkey, type tkey, value vmod, type tmod, ec_eng_t *ec_eng)
{
    pword	*prop, *p;
    dident	key_did;
    pword	old, new;
    int		err;

    Get_Functor_Did(vkey, tkey, key_did);
    Check_Module(tmod, vmod);

    mt_mutex_lock(&PropertyLock);
    err = get_property_ref(key_did, IDB_PROP, vmod.did, tmod,
				LOCAL_PROP, &prop);
    if (err < 0) {
	mt_mutex_unlock(&PropertyLock);
	Bip_Error(err);
    }
    if (!(err & NEW_PROP)) {
	assert(prop->tag.kernel == TPTR);
	_rec_free_all((t_heap_rec_hdr*) prop->val.wptr);
    }
    prop->tag.kernel = TPTR;
    prop->val.wptr = (uword *) _rec_create();
    mt_mutex_unlock(&PropertyLock);
    Succeed_;
}


static int
p_global_record_body(value vkey, type tkey, value vmod, type tmod, ec_eng_t *ec_eng)
{
    pword	*prop, *p;
    dident	key_did;
    int		err;

    Get_Functor_Did(vkey, tkey, key_did);
    Check_Module(tmod, vmod);

    mt_mutex_lock(&PropertyLock);
    err = get_property_ref(key_did, IDB_PROP, vmod.did, tmod,
				GLOBAL_PROP, &prop);
    if (err < 0) {
	mt_mutex_unlock(&PropertyLock);
	Bip_Error(err);
    }
    if (!(err & NEW_PROP)) {
	assert(prop->tag.kernel == TPTR);
	_rec_free_all((t_heap_rec_hdr*) prop->val.wptr);
    }
    prop->tag.kernel = TPTR;
    prop->val.wptr = (uword *) _rec_create();
    mt_mutex_unlock(&PropertyLock);
    Succeed_;
}


static int
p_abolish_record_body(value vkey, type tkey, value vmod, type tmod, ec_eng_t *ec_eng)
{
    dident	key_did;
    int		err;
    
    if (IsHandle(tkey))
    {
	return p_handle_free(vkey, tkey, ec_eng);
    }
    else
    {
	Get_Functor_Did(vkey, tkey, key_did);
        Check_Module(tmod, vmod);

	err = erase_property(key_did, IDB_PROP, vmod.did,tmod, LOCAL_PROP);

	if (err < 0)
	{
	    Bip_Error((err == PERROR) ? NO_LOCAL_REC : err);
	}
	else
	    Succeed_;
    }
}


/* record[az](+Key, ?Term)@Module */

static int
p_recorda_body(value vrec, type trec, value vterm, type tterm, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_rec_hdr *header;
    t_heap_rec *obj;
    pword copy_pw;
    int err;

    err = _get_rec_list(vrec, trec, vmod, tmod, &header, ec_eng);
    Return_If_Error(err);
    err = create_heapterm(ec_eng, &copy_pw, vterm, tterm);
    Return_If_Error(err);
    obj = _rec_create_elem(header);
    move_heapterm(&copy_pw, &obj->term);

    mt_mutex_lock(&header->lock);
    obj->next = header->next;
    obj->prev = (t_heap_rec*) header;
    header->next->prev = obj;
    header->next = obj;
    header->count++;
    mt_mutex_unlock(&header->lock);
    Succeed_;
}

static int
p_recordz_body(value vrec, type trec, value vterm, type tterm, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_rec_hdr *header;
    t_heap_rec *obj;
    pword copy_pw;
    int err;

    err = _get_rec_list(vrec, trec, vmod, tmod, &header, ec_eng);
    Return_If_Error(err);
    err = create_heapterm(ec_eng, &copy_pw, vterm, tterm);
    Return_If_Error(err);
    obj = _rec_create_elem(header);
    move_heapterm(&copy_pw, &obj->term);

    mt_mutex_lock(&header->lock);
    obj->next = (t_heap_rec*) header;
    obj->prev = header->prev;
    header->prev->next = obj;
    header->prev = obj;
    header->count++;
    mt_mutex_unlock(&header->lock);
    Succeed_;
}


/**
 * Specialized version of record_wait_append/4:  append a handle
 * (represented by class+data) to the record, and signal waiters.
 * CAUTION: data is not copied here, the reference is moved to the heap.
 */
int
ec_record_append(t_ext_ptr rec, t_ext_type *class, t_ext_ptr data)
{
    t_heap_rec_hdr *header = (t_heap_rec_hdr *) rec;
    t_heap_rec *obj = _rec_create_elem(header);
    create_heapterm_for_handle(&obj->term, class, data);
    mt_mutex_lock(&header->lock);
    obj->next = (t_heap_rec*) header;
    obj->prev = header->prev;
    header->prev->next = obj;
    header->prev = obj;
    header->count++;
    _rec_signal(header, 1);
    mt_mutex_unlock(&header->lock);
    Succeed_;
}


/* record[az](+Key, ?Term, -DbRef)@Module */

static int
p_recorda3_body(value vrec, type trec, value vterm, type tterm, value vdref, type tdref, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_rec_hdr *header;
    t_heap_rec *obj;
    pword copy_pw, ref_pw;
    int err;

    err = _get_rec_list(vrec, trec, vmod, tmod, &header, ec_eng);
    Return_If_Error(err);
    err = create_heapterm(ec_eng, &copy_pw, vterm, tterm);
    Return_If_Error(err);
    obj = _rec_create_elem(header);
    move_heapterm(&copy_pw, &obj->term);

    mt_mutex_lock(&header->lock);
    obj->next = header->next;
    obj->prev = (t_heap_rec*) header;
    header->next->prev = obj;
    header->next = obj;
    header->count++;
    mt_mutex_unlock(&header->lock);

    ref_pw = ecl_handle(ec_eng, &heap_rec_tid, (t_ext_ptr) _rec_copy_elem(obj));
    Return_Unify_Pw(vdref, tdref, ref_pw.val, ref_pw.tag);
}

static int
p_recordz3_body(value vrec, type trec, value vterm, type tterm, value vdref, type tdref, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_rec_hdr *header;
    t_heap_rec *obj;
    pword copy_pw, ref_pw;
    int err;

    err = _get_rec_list(vrec, trec, vmod, tmod, &header, ec_eng);
    Return_If_Error(err);
    err = create_heapterm(ec_eng, &copy_pw, vterm, tterm);
    Return_If_Error(err);
    obj = _rec_create_elem(header);
    move_heapterm(&copy_pw, &obj->term);

    mt_mutex_lock(&header->lock);
    obj->next = (t_heap_rec*) header;
    obj->prev = header->prev;
    header->prev->next = obj;
    header->prev = obj;
    header->count++;
    mt_mutex_unlock(&header->lock);

    ref_pw = ecl_handle(ec_eng, &heap_rec_tid, (t_ext_ptr) _rec_copy_elem(obj));
    Return_Unify_Pw(vdref, tdref, ref_pw.val, ref_pw.tag);
}


/* filter for recorded terms: simple tests to reduce the recorded terms
   returned to the ECLiPSe level, that need to be unified with the filter
   term. The filter test performs simple comparison on the arguments of the
   first argument of a recorded term against the filter, This is designed to
   speed up the matching of dynamic predicates, which are recorded as (H :- B),
   so that `filtering' is performed on the head H and its arguments.
*/
static int
_may_match_filter(value vfilter, uword tfilter, value vterm, type tterm)
{
    pword *farg;	/* pointer into filter term */
    pword *targ;	/* pointer into recorded term */
    int i;

    /* toplevel term */
    if (ISRef(tfilter) || IsRef(tterm))
        return 1;
    if (DifferTypeC(tterm,tfilter))
        return 0;
    if (ISSimple(tfilter))
        return SimpleEq(tfilter,vfilter,vterm);
    if (IsTag(tfilter,TCOMP))
    {
        farg = vfilter.ptr;
        targ = vterm.ptr;
	if (farg->val.did != targ->val.did)
	    return 0;
        ++farg;
        ++targ;
    }
    else if (IsTag(tfilter,TLIST))
    {
        farg = vfilter.ptr;
        targ = vterm.ptr;
    }
    else
	return 1;       /* in case of doubt, succeed (don't filter) */

    /* first argument (the head in case of a head:-body term */
    if (IsRef(farg->tag) || IsRef(targ->tag))
        return 1;
    if (DifferType(farg->tag, targ->tag))
        return 0;
    if (IsSimple(farg->tag))
        return SimpleEq(farg->tag.kernel,farg->val,targ->val);
    switch (TagType(farg->tag))
    {
    case TCOMP:
	targ = targ->val.ptr;
	farg = farg->val.ptr;
	i = DidArity(farg->val.did);
	if (farg->val.did != targ->val.did) return 0;
    _check_rec_args:
	do
	{
	    pword *f = ++farg;
	    ++targ;
	    Dereference_(f);
	    if (IsRef(f->tag) || IsRef(targ->tag))
	    	continue;
	    if (DifferType(f->tag, targ->tag))
	    	return 0;
            if (IsSimple(f->tag))
            {
                if (!SimpleEq(f->tag.kernel,f->val,targ->val))
		    return 0;
            }
            else if (IsTag(f->tag.kernel,TCOMP))
            {
		if (f->val.ptr->val.did != targ->val.ptr->val.did)
		    return 0;
            }
	} while (--i > 0);
	break;
    case TLIST:
	targ = targ->val.ptr-1;
	farg = farg->val.ptr-1;
	i = 2;
	goto _check_rec_args;
    }
    return 1;
}


/* recorded_list(+Key, -Terms)@Module */

static int
p_recorded_list_body(value vrec, type trec, value vl, type tl, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_rec_hdr *header;
    t_heap_rec *obj;
    int err;

    Check_Output_List(tl);
    err = _get_rec_list(vrec, trec, vmod, tmod, &header, ec_eng);
    if (err == PSUCCEED)
    {
	pword list;
	pword *tail = &list;
	mt_mutex_lock(&header->lock);
	for (obj = header->next; obj != (t_heap_rec*) header; obj = obj->next)
	{
	    pword *car = TG;
	    Make_List(tail, car);
	    Push_List_Frame();
	    get_heapterm(ec_eng, &obj->term, car);
	    tail = car+1;
	}
	Make_Nil(tail);
	mt_mutex_unlock(&header->lock);
	Return_Unify_Pw(vl, tl, list.val, list.tag);
    }
    else if (err == NO_LOCAL_REC)
    {
	Return_Unify_Nil(vl, tl);
    }
    else
    {
	Bip_Error(err);
    }
}


/* recorded_refs(+Key, ?Filter, -Refs)@Module */

static int
p_recorded_refs_body(value vrec, type trec, value vfilter, type tfilter, value vl, type tl, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_rec_hdr *header;
    t_heap_rec *obj;
    int err;

    Check_Output_List(tl);
    err = _get_rec_list(vrec, trec, vmod, tmod, &header, ec_eng);
    if (err == PSUCCEED)
    {
	pword list;
	pword *tail = &list;
	mt_mutex_lock(&header->lock);
	for (obj = header->next; obj != (t_heap_rec*) header; obj = obj->next)
	{
	    if (ISRef(tfilter.kernel) ||
		_may_match_filter(vfilter, tfilter.kernel, obj->term.val, obj->term.tag))
	    {
		pword *car = TG;
		Make_List(tail, car);
		Push_List_Frame();
		*car = ecl_handle(ec_eng, &heap_rec_tid, (t_ext_ptr) _rec_copy_elem(obj));
		tail = car+1;
	    }
	}
	Make_Nil(tail);
	mt_mutex_unlock(&header->lock);
	Return_Unify_Pw(vl, tl, list.val, list.tag);
    }
    else if (err == NO_LOCAL_REC)
    {
	Return_Unify_Nil(vl, tl);
    }
    else
    {
	Bip_Error(err);
    }
}


/* erase_all(+Key)@Module */

static int
p_erase_all_body(value vrec, type trec, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_rec_hdr *header;
    int err;

    err = _get_rec_list(vrec, trec, vmod, tmod, &header, ec_eng);
    if (err == PSUCCEED)
    {
	mt_mutex_lock(&header->lock);
	_rec_free_elems(header);
	mt_mutex_unlock(&header->lock);
    }
    else if (err == NO_LOCAL_REC)
    {
	err = PSUCCEED;
    }
    Bip_Error(err);
}


/* referenced_record(+DbRef, -Term) */

static int
p_referenced_record(value vrec, type trec, value vl, type tl, ec_eng_t *ec_eng)
{
    t_heap_rec *obj;
    pword result;

    Get_Typed_Object(vrec, trec, &heap_rec_tid, obj);
    get_heapterm(ec_eng, &obj->term, &result);
    if (IsRef(result.tag) && result.val.ptr == &result)
    {
	Succeed_;
    }
    Return_Unify_Pw(vl, tl, result.val, result.tag);
}


/* erase(+DbRef) */

static int
p_erase(value vrec, type trec, ec_eng_t *ec_eng)
{
    t_heap_rec *obj;
    pword result;

    Get_Typed_Object(vrec, trec, &heap_rec_tid, obj);
    if (obj->next)
    {
	mt_mutex_lock(&obj->header->lock);
	obj->next->prev = obj->prev;
	obj->prev->next = obj->next;
	obj->prev = obj->next = 0;
	obj->header->count--;
	mt_mutex_unlock(&obj->header->lock);
	_rec_free_elem(obj);
	Succeed_;
    }
    else /* was already removed from record-list */
    {
	Fail_;
    }
}


/*
 * Two internal predicates for stepping through the recorded list:
 * first_recorded(+Key, ?Filter, -Ref)@Module is semidet
 * next_recorded(+Ref, ?Filter, -Ref) is semidet
 * These cannot be used for logical update semantics!
 */

static int
p_first_recorded(value vrec, type trec, value vfilter, type tfilter, value vdref, type tdref, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_rec_hdr *header;
    t_heap_rec *obj;
    pword ref_pw;
    int err;

    err = _get_rec_list(vrec, trec, vmod, tmod, &header, ec_eng);
    if (err == PSUCCEED)
    {
	mt_mutex_lock(&header->lock);
	for(obj=header->next; obj != (t_heap_rec*) header; obj=obj->next)
	{
	    if (IsRef(tfilter) ||
		_may_match_filter(vfilter, tfilter.kernel, obj->term.val, obj->term.tag))
	    {
		obj = _rec_copy_elem(obj);
		mt_mutex_unlock(&header->lock);
		ref_pw = ecl_handle(ec_eng, &heap_rec_tid, (t_ext_ptr) obj);
		Return_Unify_Pw(vdref, tdref, ref_pw.val, ref_pw.tag);
	    }
	}
	mt_mutex_unlock(&header->lock);
    }
    else if (err != NO_LOCAL_REC)
    {
	Bip_Error(err);
    }
    Fail_;
}


static int
p_next_recorded(value vref1, type tref1, value vfilter, type tfilter, value vref2, type tref2, ec_eng_t *ec_eng)
{
    t_heap_rec_hdr *header;
    t_heap_rec *obj;
    pword ref_pw;

    Get_Typed_Object(vref1, tref1, &heap_rec_tid, obj);
    header = obj->header;
    mt_mutex_lock(&header->lock);
    for(;;)
    {
        obj = obj->next;
        if (!obj || obj == (t_heap_rec*) header)
        {
	    mt_mutex_unlock(&header->lock);
            Fail_;
        }
	if (IsRef(tfilter) ||
	    _may_match_filter(vfilter, tfilter.kernel, obj->term.val, obj->term.tag))
        {
            break;
        }
    }
    obj = _rec_copy_elem(obj);
    mt_mutex_unlock(&header->lock);
    ref_pw = ecl_handle(ec_eng, &heap_rec_tid, (t_ext_ptr) obj);
    Return_Unify_Pw(vref2, tref2, ref_pw.val, ref_pw.tag);
}


/*----------------------------------------------------------------------
 * the subsequent BIPs fail on error and set the global variable
 *----------------------------------------------------------------------*/

#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

/*
 * Check if key is a valid key for records
 */

/* ARGSUSED */
static int
p_valid_key(value v, type t, ec_eng_t *ec_eng)
{
    Error_If_Ref(t);
    if (IsAtom(t) || IsStructure(t) || IsNil(t) || IsList(t))
	{ Succeed_; }
    Check_Typed_Object_Handle(v, t, &heap_rec_header_tid);
    Succeed_;
}

#undef Bip_Error
#define Bip_Error(N) return(N);

/*----------------------------------------------------------------------
 * End of fail on error BIPs
 *----------------------------------------------------------------------*/

void
bip_record_init(int flags)
{
    pri		*pd;
    type	t;
    value	v1, v2;
    int		res;

    if (flags & INIT_SHARED)
    {
	(void) local_built_in(in_dict("valid_key", 1),
				 p_valid_key, B_SAFE|U_SIMPLE);
	(void) exported_built_in(in_dict("erase_all_body", 2),
				 p_erase_all_body, B_UNSAFE);
	(void) exported_built_in(in_dict("is_record_body", 2),
				 p_is_record_body, B_UNSAFE);
	(void) exported_built_in(in_dict("recorda_body", 3),
				 p_recorda_body, B_UNSAFE);
	(void) exported_built_in(in_dict("recordz_body", 3),
			         p_recordz_body, B_UNSAFE);
	(void) exported_built_in(in_dict("recorda_body", 4),
				 p_recorda3_body, B_UNSAFE);
	(void) exported_built_in(in_dict("recordz_body", 4),
				 p_recordz3_body, B_UNSAFE);
	exported_built_in(in_dict("recorded_list_body", 3),
			p_recorded_list_body, B_UNSAFE|U_FRESH)
	    -> mode = BoundArg(2, NONVAR);
	exported_built_in(in_dict("recorded_refs_body", 4),
			p_recorded_refs_body, B_UNSAFE|U_FRESH)
	    -> mode = BoundArg(2, NONVAR);
	exported_built_in(in_dict("referenced_record", 2),
				 p_referenced_record, B_UNSAFE|U_FRESH)
	    -> mode = BoundArg(2, NONVAR);
	(void) exported_built_in(in_dict("recorded_count_", 3),
				 p_recorded_count, B_UNSAFE);
	(void) exported_built_in(in_dict("erase", 1), p_erase, B_UNSAFE);
	(void) exported_built_in(in_dict("record_create", 1),
				 p_record_create, B_UNSAFE);
	(void) exported_built_in(in_dict("local_record_body", 2),
				 p_local_record_body, B_UNSAFE);
	(void) local_built_in(in_dict("global_record_body", 2),
				 p_global_record_body, B_UNSAFE);
	(void) exported_built_in(in_dict("abolish_record_body", 2),
				 p_abolish_record_body, B_UNSAFE);
	(void) local_built_in(in_dict("first_recorded_", 4),
				 p_first_recorded, B_UNSAFE);
	(void) local_built_in(in_dict("next_recorded", 3),
				 p_next_recorded, B_UNSAFE);
	(void) local_built_in(in_dict("record_set_max", 2),
				 p_record_set_max, B_SAFE);
	(void) local_built_in(in_dict("record_below_max", 1),
				 p_record_below_max, B_SAFE);
    }
}

