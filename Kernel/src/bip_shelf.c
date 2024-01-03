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
 * Copyright (C) 1996-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*----------------------------------------------------------------------
 * System:	ECLiPSe Constraint Logic Programming System
 * Version:	$Id: bip_shelf.c,v 1.8 2017/09/04 01:44:29 jschimpf Exp $
 *
 * Contents:	Built-ins for the shelf-primitives
 *
 *		This file has been factored out of bip_record.c in 05/2006
 *----------------------------------------------------------------------*/

#include	"config.h"
#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "error.h"
#include	"dict.h"
#include	"emu_export.h"
#include	"property.h"
#include	"os_support.h"

#include        <stdio.h>	/* for sprintf() */

/*----------------------------------------------------------------------
 * Prolog heap arrays
 *----------------------------------------------------------------------*/

/* INSTANCE TYPE DECLARATION */

typedef struct {
    word		ref_ctr;
    ec_mutex_t		lock;
    ec_cond_t		*cond;
    pword		array[ 1 /* + arity */ ];
} t_heap_array;


/* METHODS */

static int _heap_arr_set(t_ext_ptr h, int i, pword pw, ec_eng_t*);
static int _heap_arr_set_unlocked(t_ext_ptr h, int i, pword pw, ec_eng_t*);
static pword _heap_arr_get(t_ext_ptr h, int i, ec_eng_t*);
static int _heap_arr_get1(t_ext_ptr h, int i, ec_eng_t*, pword* pw);

static void
_free_heap_array(t_heap_array *obj)	/* obj != NULL */
{
    int rem = ec_atomic_add(&obj->ref_ctr, -1);
    if (rem <= 0)
    {
	pword *p = obj->array;
	int arity = DidArity(p[0].val.did);
	int i;
	assert(rem==0);
	mt_mutex_destroy(&obj->lock);
	if (obj->cond) {
	    ec_cond_destroy(obj->cond);
	    hg_free_size(obj->cond, sizeof(ec_cond_t));
	}
	for (i = arity; i > 0; --i)
	{
	    free_heapterm(&p[i]);
	}
	hg_free_size(obj, sizeof(t_heap_array) + arity*sizeof(pword));
#ifdef DEBUG_RECORD
	p_fprintf(current_err_, "\n_free_heap_array(0x%x)", obj);
	ec_flush(current_err_);
#endif
    }
}

static t_heap_array *
_copy_heap_array(t_heap_array *obj)	/* obj != NULL */
{
    ec_atomic_add(&obj->ref_ctr, 1);
    return obj;
}

static void
_mark_heap_array(t_heap_array *obj)	/* obj != NULL */
{
    pword *p = obj->array;
    int i = DidArity(p[0].val.did);
    mark_dids_from_pwords(p, p + 1);
    for (; i > 0; --i)
    {
	mark_dids_from_heapterm(&p[i]);
    }
}

static dident
_kind_heap_array()
{
    return d_.shelf;
}

static int
_lock_heap_array(t_heap_array *obj)
{
    return mt_mutex_lock(&obj->lock);
}

static int
_trylock_heap_array(t_heap_array *obj)
{
    return mt_mutex_trylock(&obj->lock);
}

static int
_unlock_heap_array(t_heap_array *obj)
{
    return mt_mutex_unlock(&obj->lock);
}

static int
_signal_heap_array(t_heap_array *obj, int all)
{
    ec_cond_t *cv = obj->cond;
    /* no need to signal when no waiters */
    return cv? ec_cond_signal(cv, all) : 0;
}

static int
_wait_heap_array(t_heap_array *obj, int timeout_ms)
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

t_ext_type heap_array_tid = {
    (void (*)(t_ext_ptr)) _free_heap_array,
    (t_ext_ptr (*)(t_ext_ptr)) _copy_heap_array,
    (void (*)(t_ext_ptr)) _mark_heap_array,
    0, /* string_size */
    0, /* to_string */
    0,	/* equal */
    (t_ext_ptr (*)(t_ext_ptr)) _copy_heap_array,
    _heap_arr_get,	/* get */
    _heap_arr_set,	/* set */
    _kind_heap_array,	/* kind */
    (int (*)(t_ext_ptr)) _lock_heap_array,	/* lock */
    (int (*)(t_ext_ptr)) _trylock_heap_array,	/* trylock */
    (int (*)(t_ext_ptr)) _unlock_heap_array,	/* unlock */
    (int (*)(t_ext_ptr,int)) _signal_heap_array,
    (int (*)(t_ext_ptr,int)) _wait_heap_array
};


/* PROLOG INTERFACE */

/*
 * shelf_create(+Name/Arity, +FieldInit, -Handle)
 * shelf_create(+InitTerm, -Handle)
 * shelf_get/xget(+Handle, +Index, -Field)	Index=0: get whole term
 * shelf_set/xset(+Handle, +Index, +Field)	Index=0: set whole term
 * shelf_abolish(+Handle)
 *
 * shelf_name(+NameTerm, +Handle, +Module)	give the shelf a name
 */


/*
 * Get a pointer to the shelf either from a handle
 * or from the SHELF_PROP property of a functor.
 * Must be called from within an external, because it calls Hold_Object_Until_Done!
 */
#define Get_Shelf(vhandle, thandle, vmod, tmod, obj)			\
	if (IsTag(thandle.kernel, THANDLE)) {				\
	    Get_Typed_Object(vhandle, thandle, &heap_array_tid, obj);	\
	} else {							\
	    dident name_did;						\
	    int err;							\
	    pword *prop;						\
	    Get_Key_Did(name_did, vhandle, thandle);			\
	    err = get_visible_property_handle(name_did, SHELF_PROP, vmod.did, tmod, &heap_array_tid, (t_ext_ptr*)&obj); \
	    if (err < 0) {						\
		Bip_Error(err == PERROR ? NO_LOCAL_REC : err);		\
	    }								\
	    Hold_Object_Until_Done(&heap_array_tid,obj)			\
	}


static int
p_shelf_create3(value vkey, type tkey, value vinit, type tinit, value vbag, type tbag, ec_eng_t *ec_eng)
{
    dident key_did;
    pword *p, bag;
    t_heap_array *obj;
    int i;
    Check_Ref(tbag);
    Get_Functor_Did(vkey, tkey, key_did);
    i = DidArity(key_did);
    if (i < 1)
	{ Bip_Error(RANGE_ERROR); }

    /* INSTANCE INITIALISATION */
    obj = (t_heap_array *) hg_alloc_size(
			    sizeof(t_heap_array) + i*sizeof(pword));
    obj->ref_ctr = 1;
    mt_mutex_init_recursive(&obj->lock);
    obj->cond = NULL;
    p = obj->array;
    for (; i > 0; --i)
    {
	int err = create_heapterm(ec_eng, &p[i], vinit, tinit);
	Return_If_Not_Success(err);
    }
    p[0].val.did = key_did;
    p[0].tag.kernel = TDICT;

    bag = ecl_handle(ec_eng, &heap_array_tid, (t_ext_ptr) obj);
    Return_Unify_Pw(vbag, tbag, bag.val, bag.tag);
}


static int
p_shelf_create2(value vinit, type tinit, value vbag, type tbag, ec_eng_t *ec_eng)
{
    pword bag;
    pword *pheap, *pglobal;
    t_heap_array *obj;
    int i, err;

    Check_Ref(tbag);
    Check_Structure(tinit);
    pglobal = vinit.ptr;
    i = DidArity(pglobal->val.did);

    /* INSTANCE INITIALISATION */
    obj = (t_heap_array *) hg_alloc_size(
			    sizeof(t_heap_array) + i*sizeof(pword));
    obj->ref_ctr = 1;
    mt_mutex_init_recursive(&obj->lock);
    obj->cond = NULL;
    pheap = obj->array;
    pheap[0] = pglobal[0];
    for (; i > 0; --i)
    {
	pword *parg = &pglobal[i];
	Dereference_(parg);
	err = create_heapterm(ec_eng, &pheap[i], parg->val, parg->tag);
	Return_If_Not_Success(err);
    }
    bag = ecl_handle(ec_eng, &heap_array_tid, (t_ext_ptr) obj);
    Return_Unify_Pw(vbag, tbag, bag.val, bag.tag);
}


static int
p_shelf_name(value vname, type tname, value vhandle, type thandle, value vmod, type tmod, ec_eng_t *ec_eng)
{
    pword *prop;
    dident name_did;
    int err;
    t_heap_array *obj;

    Get_Functor_Did(vname, tname, name_did);
    Get_Typed_Object(vhandle, thandle, &heap_array_tid, obj);

    mt_mutex_lock(&PropertyLock);
    err = get_property_ref(name_did, SHELF_PROP, vmod.did, tmod,
				LOCAL_PROP, &prop);
    if (err < 0) {
	mt_mutex_unlock(&PropertyLock);
	Bip_Error(err);
    }
    if (!(err & NEW_PROP)) {
	assert(prop->tag.kernel == TPTR);
	heap_array_tid.free((t_heap_array*) prop->val.wptr);
    }
    prop->tag.kernel = TPTR;
    prop->val.wptr = (uword *) heap_array_tid.copy(obj);
    mt_mutex_unlock(&PropertyLock);
    Succeed_;
}


static int
p_shelf_size(value vhandle, type thandle, value vval, type tval, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_array *obj;
    Get_Shelf(vhandle, thandle, vmod, tmod, obj);
    Return_Unify_Integer(vval, tval, DidArity(obj->array[0].val.did));
}


static int
p_shelf_set(value vhandle, type thandle, value vi, type ti, value vval, type tval, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_array *obj;
    pword pw = {vval,tval};
    Get_Shelf(vhandle, thandle, vmod, tmod, obj);
    Check_Integer(ti);
    return _heap_arr_set(obj, vi.nint, pw, ec_eng);
}

static int
p_shelf_get(value vhandle, type thandle, value vi, type ti, value vval, type tval, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_array *obj;
    pword pw = {vval,tval};
    Get_Shelf(vhandle, thandle, vmod, tmod, obj);
    Check_Integer(ti);
    if (vi.nint < 0 || vi.nint > DidArity(obj->array[0].val.did))
	{ Bip_Error(RANGE_ERROR); }
    _heap_arr_get1(obj, vi.nint, ec_eng, &pw);
    if (IsRef(pw.tag) && IsSelfRef(&pw))
    {
	Succeed_;	/* nothing to unify */
    }
    Return_Unify_Pw(vval, tval, pw.val, pw.tag);
}


static int
p_shelf_inc(value vhandle, type thandle, value vi, type ti, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_array *obj;
    pword *pw;
    Get_Shelf(vhandle, thandle, vmod, tmod, obj);
    Check_Integer(ti);
    if (vi.nint < 1 || vi.nint > DidArity(obj->array[0].val.did))
	{ Bip_Error(RANGE_ERROR); }
    Acquire_Lock_Until_Done(ec_eng, &obj->lock);
    pw = &obj->array[vi.nint];
    Check_Integer(pw->tag);
    if (pw->val.nint == MAX_S_WORD)
    {
	Bip_Error(RANGE_ERROR);
    }
    ++pw->val.nint;
    Succeed_;
}


static int
p_shelf_dec(value vhandle, type thandle, value vi, type ti, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_array *obj;
    pword *pw;
    Get_Shelf(vhandle, thandle, vmod, tmod, obj);
    Check_Integer(ti);
    if (vi.nint < 1 || vi.nint > DidArity(obj->array[0].val.did))
	{ Bip_Error(RANGE_ERROR); }
    Acquire_Lock_Until_Done(ec_eng, &obj->lock);
    pw = &obj->array[vi.nint];
    Check_Integer(pw->tag);
    if (pw->val.nint <= 0)
    {
	Fail_;
    }
    --pw->val.nint;
    Succeed_;
}


/*
 * Additional primitives for concurrency
 */

static int
p_shelf_inc_and_get(value vhandle, type thandle, value vi, type ti, value vval, type tval, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_array *obj;
    Get_Shelf(vhandle, thandle, vmod, tmod, obj);
    Check_Integer(ti);
    if (vi.nint < 0 || vi.nint > DidArity(obj->array[0].val.did))
	{ Bip_Error(RANGE_ERROR); }
    Acquire_Lock_Until_Done(ec_eng, &obj->lock);
    pword *pw = &obj->array[vi.nint];
    Check_Integer(pw->tag);
    if (pw->val.nint == MAX_S_WORD)
    {
	Bip_Error(RANGE_ERROR);
    }
    word count = ++pw->val.nint;
    Return_Unify_Integer(vval, tval, count);
}

static int
p_shelf_get_and_dec(value vhandle, type thandle, value vi, type ti, value vval, type tval, value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_array *obj;
    Get_Shelf(vhandle, thandle, vmod, tmod, obj);
    Check_Integer(ti);
    if (vi.nint < 1 || vi.nint > DidArity(obj->array[0].val.did))
	{ Bip_Error(RANGE_ERROR); }
    Acquire_Lock_Until_Done(ec_eng, &obj->lock);
    pword *pw = &obj->array[vi.nint];
    Check_Integer(pw->tag);
    if (pw->val.nint <= 0)
    {
	Fail_;
    }
    word count = pw->val.nint--;
    Return_Unify_Integer(vval, tval, count);
}


/*
 * Should have same semantics as test_and_setval/3, i.e.
 *
 * shelf_test_and_set(Shelf, Idx, Val, New) :-
 *	shelf_get(Shelf, Idx, Old), 
 *	Old == Val,
 *	shelf_set(Shelf, Idx, New).
 */

static int
p_shelf_test_and_set(value vhandle, type thandle, value vi, type ti,
	value vold, type told, value vval, type tval,
	value vmod, type tmod, ec_eng_t *ec_eng)
{
    t_heap_array *obj;
    int res;
    Get_Shelf(vhandle, thandle, vmod, tmod, obj);
    Check_Integer(ti);
    /* i==0 not supported here */
    if (vi.nint < 1 || vi.nint > DidArity(obj->array[0].val.did))
	{ Bip_Error(RANGE_ERROR); }

    mt_mutex_lock(&obj->lock);
    if (ec_compare_terms(vold, told, obj->array[vi.nint].val, obj->array[vi.nint].tag) == 0)
    {
	res = _heap_arr_set_unlocked(obj, vi.nint, (pword) {vval,tval}, ec_eng);
    }
    else res = PFAIL;
    mt_mutex_unlock(&obj->lock);
    return res;
}


/*
 * Analogy to hash_update would require:
 *
 * shelf_update(Shelf, Idx, Val, New) :-
 *	shelf_get(Shelf, Idx, Old), 
 *	Old = Val,	% unify before setting
 *	shelf_set(Shelf, Idx, New).
 *
 * but full unification is awkward to do here.  Could just have
 *
 * shelf_swap(Shelf, Idx, New, Out) :-
 *	shelf_get(Shelf, Idx, Old), 
 *	shelf_set(Shelf, Idx, New),
 *	Out = Old.
 *

static int
p_shelf_update(value vhandle, type thandle, value vi, type ti, value vold, type told, value vval, type tval, value vmod, type tmod, ec_eng_t *ec_eng)
{
    ...
}
*/


static int
_heap_arr_set_unlocked(t_ext_ptr h,
	int i,
	pword pw,	/* expected to be dereferenced */
	ec_eng_t *ec_eng)
{
    pword copy_pw;
    pword *pheap;
    int arity;
    int res = PSUCCEED;

    pheap = ((t_heap_array*)h)->array;
    arity = DidArity(pheap[0].val.did);
    if (i >= 1 && i <= arity)
    {
	res = create_heapterm(ec_eng, &copy_pw, pw.val, pw.tag);
	if (res != PSUCCEED) return res;
	free_heapterm(&pheap[i]);
	move_heapterm(&copy_pw, &pheap[i]);
    }
    else if (i == 0)
    {
	if (IsStructure(pw.tag) && pw.val.ptr->val.did == pheap[0].val.did)
	{
	    pword *aux = TG;
	    Push_Struct_Frame(pheap[0].val.did);
	    for (i=1; i<=arity; ++i)
	    {
		pword *parg = &pw.val.ptr[i];
		Dereference_(parg);
		res = create_heapterm(ec_eng, aux+i, parg->val, parg->tag);
		if (res != PSUCCEED) {
		    TG = aux;
		    return res;
		}
	    }
	    for (i=1; i<=arity; ++i)
	    {
		free_heapterm(&pheap[i]);
		move_heapterm(aux+i, &pheap[i]);
	    }
	    TG = aux;
	}
	else return RANGE_ERROR;
    }
    else return RANGE_ERROR;
    return res;
}

static int
_heap_arr_set(t_ext_ptr h,
	int i,
	pword pw,	/* expected to be dereferenced */
	ec_eng_t *ec_eng)
{
    mt_mutex_lock(&((t_heap_array*)h)->lock);
    int res = _heap_arr_set_unlocked(h, i, pw, ec_eng);
    mt_mutex_unlock(&((t_heap_array*)h)->lock);
    return res;
}


/* This should be phased out because of the problem of returning a self-ref */
static pword
_heap_arr_get(t_ext_ptr h, int i, ec_eng_t *ec_eng)	/* assumed to return dereferenced result */
{
    pword result;
    if (_heap_arr_get1(h, i, ec_eng, &result) != PSUCCEED) {
	result.tag.kernel = TNIL;
    	return result;
    }
    if (IsRef(result.tag) && IsSelfRef(&result))
    {
	result.val.ptr = TG;
	Push_Var();
    }
    return result;
}

static int
_heap_arr_get1(t_ext_ptr h, int i, ec_eng_t *ec_eng, pword *result)	/* assumed to return dereferenced result */
{
    pword *pheap;
    int arity;

    pheap = ((t_heap_array*)h)->array;
    arity = DidArity(pheap[0].val.did);
    if (i < 0 || i > arity)
    	return RANGE_ERROR;
    mt_mutex_lock(&((t_heap_array*)h)->lock);
    if (i > 0)
    {
	get_heapterm(ec_eng, &pheap[i], result);
    }
    else				/* get the whole array-term */
    {
	Make_Struct(result,TG);
	Push_Struct_Frame(pheap[0].val.did);
	for (i=1; i<=arity; ++i)
	{
	    get_heapterm(ec_eng, &pheap[i], &result->val.ptr[i]);
	}
    }
    mt_mutex_unlock(&((t_heap_array*)h)->lock);
    return PSUCCEED;
}


/*----------------------------------------------------------------------
 * Initialisation
 *----------------------------------------------------------------------*/

void
bip_shelf_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("shelf_create", 3), p_shelf_create3, B_SAFE|U_SIMPLE);
	(void) built_in(in_dict("shelf_create", 2), p_shelf_create2, B_SAFE|U_SIMPLE);
	(void) built_in(in_dict("shelf_get_",4), p_shelf_get, B_UNSAFE|U_FRESH);
	(void) built_in(in_dict("shelf_set_",4), p_shelf_set, B_SAFE);
	(void) built_in(in_dict("shelf_inc_",3), p_shelf_inc, B_SAFE);
	(void) built_in(in_dict("shelf_dec_",3), p_shelf_dec, B_SAFE);
	(void) built_in(in_dict("shelf_size_",3), p_shelf_size, B_SAFE);
	(void) built_in(in_dict("shelf_inc_and_get_",4), p_shelf_inc_and_get, B_SAFE);
	(void) built_in(in_dict("shelf_get_and_dec_",4), p_shelf_get_and_dec, B_SAFE);
	(void) built_in(in_dict("shelf_test_and_set_",5), p_shelf_test_and_set, B_SAFE);
	(void) built_in(in_dict("shelf_abolish", 1), p_handle_free, B_SAFE|U_NONE);
	(void) local_built_in(in_dict("shelf_name",3), p_shelf_name, B_SAFE);
    }
}
