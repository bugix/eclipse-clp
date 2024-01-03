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
 * Version:	$Id: bip_bag.c,v 1.6 2017/09/04 01:44:29 jschimpf Exp $
 *
 * Contents:	Built-ins for the bag-primitives
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
 * Bag primitives:
 *
 * bag_create(-Bag)
 * bag_enter(+Bag, +Term)
 * bag_dissolve(+Bag, -List)
 * CAUTION: The bag can no longer be accessed after bag_dissolve/2 !
 *
 * empty bag:	pword *pw1 = {TLIST|pw1, TNIL}
 *
 * 3-elem bag:	pword *pw1 = {TLIST|pw4, TLIST|pw2}
 *		pword *pw2 = {Term1,     TLIST|pw3}
 *		pword *pw3 = {Term2,     TLIST|pw4}
 *		pword *pw4 = {Term3,     TNIL}
 *
 *----------------------------------------------------------------------*/

/* INSTANCE TYPE DECLARATION */

typedef struct {
    pword		list[2];
    uword		size;
    uword		ref_ctr;
    ec_mutex_t		lock;
    ec_cond_t		*cond;
} t_heap_bag;


/* METHODS */
static void _free_heap_bag(t_heap_bag *obj);
static t_heap_bag * _copy_heap_bag(t_heap_bag *obj);
static void _mark_heap_bag(t_heap_bag *obj);

static void
_erase_heap_bag(t_heap_bag *obj)	/* obj != NULL */
{
    pword cdr;
    cdr = obj->list[1];
    while (IsList(cdr.tag))
    {
	pword *pw = cdr.val.ptr;
	free_heapterm(pw);
	cdr = pw[1];
	hg_free_size(pw, 2*sizeof(pword));
    }
    Make_List(obj->list, obj->list);	/* reinitialize */
    Make_Nil(&obj->list[1]);
    obj->size = 0;
}

static void
_free_heap_bag(t_heap_bag *obj)		/* obj != NULL */
{
    int rem = ec_atomic_add(&obj->ref_ctr, -1);
    if (rem <= 0)
    {
	assert(rem==0);
	_erase_heap_bag(obj);
	mt_mutex_destroy(&obj->lock);
	if (obj->cond) {
	    ec_cond_destroy(obj->cond);
	    hg_free_size(obj->cond, sizeof(ec_cond_t));
	}
	hg_free_size(obj, sizeof(t_heap_bag));
#ifdef DEBUG_RECORD
	p_fprintf(current_err_, "\n_free_heap_bag(0x%x)", obj);
	ec_flush(current_err_);
#endif
    }
}

static t_heap_bag *
_copy_heap_bag(t_heap_bag *obj)		/* obj != NULL */
{
    ec_atomic_add(&obj->ref_ctr,1);
    return obj;
}

static void
_mark_heap_bag(t_heap_bag *obj)		/* obj != NULL */
{
    pword *pw = obj->list;
    while (IsList(pw[1].tag))		/* for all bag elements */
    {
	pw = pw[1].val.ptr;
	mark_dids_from_heapterm(pw);
    }
}

static dident
_kind_bag()
{
    return d_.bag;
}

static int
_lock_bag(t_heap_bag *obj)
{
    return mt_mutex_lock(&obj->lock);
}

static int
_trylock_bag(t_heap_bag *obj)
{
    return mt_mutex_trylock(&obj->lock);
}

static int
_unlock_bag(t_heap_bag *obj)
{
    return mt_mutex_unlock(&obj->lock);
}

static int
_signal_bag(t_heap_bag *obj, int all)
{
    ec_cond_t *cv = obj->cond;
    /* no need to signal when no waiters */
    return cv? ec_cond_signal(cv, all) : 0;
}

static int
_wait_bag(t_heap_bag *obj, int timeout_ms)
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

const t_ext_type heap_bag_tid = {
    (void (*)(t_ext_ptr)) _free_heap_bag,
    (t_ext_ptr (*)(t_ext_ptr)) _copy_heap_bag,
    (void (*)(t_ext_ptr)) _mark_heap_bag,
    0, /* string_size */
    0, /* to_string */
    0,	/* equal */
    (t_ext_ptr (*)(t_ext_ptr)) _copy_heap_bag,
    0,	/* get */
    0,	/* set */
    _kind_bag,				/* kind */
    (int (*)(t_ext_ptr)) _lock_bag,	/* lock */
    (int (*)(t_ext_ptr)) _trylock_bag,	/* trylock */
    (int (*)(t_ext_ptr)) _unlock_bag,	/* unlock */
    (int (*)(t_ext_ptr,int)) _signal_bag,
    (int (*)(t_ext_ptr,int)) _wait_bag
};


/* PROLOG INTERFACE */

static int
p_bag_create(value vbag, type tbag, ec_eng_t *ec_eng)
{
    t_heap_bag *obj;
    pword bag;

    Check_Ref(tbag);

    /* INSTANCE INITIALISATION */
    obj = (t_heap_bag *) hg_alloc_size(sizeof(t_heap_bag));
    obj->size = 0;
    obj->ref_ctr = 1;
    Make_List(obj->list, obj->list);	/* pointer to last element (self) */
    Make_Nil(&obj->list[1]);
    mt_mutex_init_recursive(&obj->lock);
    obj->cond = NULL;
    bag = ecl_handle(ec_eng, &heap_bag_tid, (t_ext_ptr) obj);
    Return_Unify_Pw(vbag, tbag, bag.val, bag.tag);
}

static int
p_bag_enter(value vbag, type tbag, value vterm, type tterm, ec_eng_t *ec_eng)
{
    t_heap_bag *obj;
    pword copy_pw, *pw;
    int err;

    Get_Typed_Object(vbag, tbag, &heap_bag_tid, obj);

    if ((err = create_heapterm(ec_eng, &copy_pw, vterm, tterm)) != PSUCCEED)
	{ Bip_Error(err); }
    Acquire_Lock_Until_Done(ec_eng, &obj->lock);
    pw = (pword *) hg_alloc_size(2*sizeof(pword));
    move_heapterm(&copy_pw, pw);
    Make_Nil(pw + 1);
    Make_List(obj->list[0].val.ptr + 1, pw);
    obj->list[0].val.ptr = pw;
    ++obj->size;
    Succeed_;
}

static int
p_bag_retrieve(value vbag, type tbag, value vl, type tl, ec_eng_t *ec_eng)
{
    t_heap_bag *obj;
    pword list;
    register pword *car, *cdr, *pw;

    Get_Typed_Object(vbag, tbag, &heap_bag_tid, obj);
    Check_Output_List(tl);
    Acquire_Lock_Until_Done(ec_eng, &obj->lock);
    pw = &obj->list[1];
    cdr = &list;
    while (IsList(pw->tag))
    {
	pw = pw->val.ptr;
        car = TG;
        Push_List_Frame();
        Make_List(cdr, car);
	get_heapterm(ec_eng, pw, car);
        cdr = car + 1;
	pw += 1;
    }
    Make_Nil(cdr);
    Return_Unify_Pw(vl, tl, list.val, list.tag);
}

static int
p_bag_erase(value vbag, type tbag, ec_eng_t *ec_eng)
{
    t_heap_bag *obj;
    Get_Typed_Object(vbag, tbag, &heap_bag_tid, obj);
    Acquire_Lock_Until_Done(ec_eng, &obj->lock);
    _erase_heap_bag(obj);
    Succeed_;
}

static int
p_bag_count(value vbag, type tbag, value vc, type tc, ec_eng_t *ec_eng)
{
    t_heap_bag *obj;
    Check_Output_Integer(tc);
    Get_Typed_Object(vbag, tbag, &heap_bag_tid, obj);
    Return_Unify_Integer(vc, tc, obj->size);
}

static int
p_bag_dissolve(value vbag, type tbag, value vl, type tl, ec_eng_t *ec_eng)
{
    int res = p_bag_retrieve(vbag, tbag, vl, tl, ec_eng);
    p_handle_free(vbag, tbag, ec_eng);
    return res;
}


/*----------------------------------------------------------------------
 * Initialisation
 *----------------------------------------------------------------------*/

void
bip_bag_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("bag_create", 1), p_bag_create, B_SAFE|U_SIMPLE);
	(void) built_in(in_dict("bag_enter", 2), p_bag_enter, B_SAFE|U_NONE);
	(void) built_in(in_dict("bag_count", 2), p_bag_count, B_SAFE|U_NONE);
	(void) built_in(in_dict("bag_erase", 1), p_bag_erase, B_SAFE|U_NONE);
	(void) built_in(in_dict("bag_retrieve", 2), p_bag_retrieve, B_UNSAFE|U_FRESH);
	(void) built_in(in_dict("bag_dissolve", 2), p_bag_dissolve, B_UNSAFE|U_FRESH);
	(void) built_in(in_dict("bag_abolish", 1), p_handle_free, B_SAFE|U_NONE);
    }
}

