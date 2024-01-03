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
 * Copyright (C) 1997-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe Kernel Module
 *
 * $Id: handle.c,v 1.8 2017/01/16 19:04:18 jschimpf Exp $
 *
 * Author:	Stefano Novello, IC-Parc
 *		Joachim Schimpf, IC-Parc
 *
 * Contents:	Module to deal with generic handles
 *		from Prolog to external objects
 *
 * Description:
 *
 * |------------|
 * | THANDLE    |                         |-----------|
 * |	   --------+                      |           |
 * |------------|  |   |-----------|      |           |
 *      ...        |   | TPTR      |      |  <data>   |      |----------|
 *                 |   |       ---------> |           |      |  ....    |
 * |------------|  |   | - - - - - |      |-----------|      |  mark()  |
 * | THANDLE    |  +-> | TEXTERN   |                         |  copy()  |
 * |	   ----------> |       ----------------------------> |  free()  |
 * |------------|      |-----------|                         |----------|
 *                     unique anchor         data             type desc
 *                      on global
 *
 * A module that uses handles will associate a data structure with the
 * handle, as well as a type descriptor (method table).
 *
 * Handles to external data are implemented using a unique "anchor"
 * on the global stack. The Prolog code references the anchor via
 * THANDLE pointers.
 *
 * The anchor has two components:
 *
 *	t_ext_type *ExternalClass(p)	points to a method table (class)
 *	t_ext_ptr   ExternalData(p)	points to the external data
 *
 * The ExternalData() field can point to arbitrary external data.
 * The ExternalClass() field points to a user-supplied descriptor
 * which is a table of methods for standard operations on the data.
 * 
 * When the last THANDLE referring to an anchor disappears it will
 * eventually be garbage collected.  When the anchor disappears,
 * the external object's free() method gets invoked.
 *
 * If the object is manually freed before the anchor disappears, the
 * anchor becomes stale. This is marked by overwriting ExternalData()
 * with NULL. Since the anchor is never copied, all accesses via
 * any THANDLE will see that this handle is stale.
 *
 * If an anchor gets physically copied, e.g. by setval, record, etc,
 * the copy() or remote_copy() method is used to inform the external object.
 *
 * Other methods for comparing, printing and gc can be specified.
 */

#include "config.h"
#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "mem.h"
#include "error.h"
#include "ec_io.h"
#include "dict.h"
#include "database.h"
#include "emu_export.h"
#include "property.h"
#include "os_support.h"

#if 0
#define DbgPrintf(s,...) p_fprintf(current_err_,s, __VA_ARGS__);ec_flush(current_err_);
#else
#define DbgPrintf(s,...)
#endif



/*
 * Call cleanup method (if any) and mark handle as stale (of not already)
 * (pw)[1].val.ptr is ExternalData(pw) expansion to satisfy gcc-4.x
 */
#define AnchorFree(pw) { \
	if (ExternalData(pw)) { \
	    if (ExternalClass(pw)->free) \
		ExternalClass(pw)->free(ExternalData(pw)); \
	    (pw)[1].val.ptr = (pword *) NULL; \
	} \
}


/*
 * Function to free the handle on untrailing
 */
/*ARGSUSED*/
static void
_handle_cleanup(pword *pw,
	/* following arguments unused (untrail calling convention) */
	word *pdata, int size, int flags, ec_eng_t *ec_eng)
{
	if (!pw || DifferTypeC(pw->tag, TEXTERN))
	{
	    p_fprintf(current_err_, "ECLiPSe: handle_cleanup: invalid handle\n");
	    return;
	}
	AnchorFree(pw);
}


/*
 * Construct a new handle
 */
pword Winapi
ecl_handle(ec_eng_t *ec_eng, const t_ext_type *class, const t_ext_ptr data)
{
	pword handle;
	pword *pw;

	/* push global stack anchor */
	pw = TG;
	TG += HANDLE_ANCHOR_SIZE;
	Check_Gc;
	pw[0].tag.kernel = TEXTERN;
	pw[0].val.ptr = (pword *) class;
	pw[1].tag.kernel = TPTR;
	pw[1].val.ptr = (pword *) data;

	/* Make handle */
	handle.tag.kernel = THANDLE;
	handle.val.ptr = pw;

	/* Trail cleanup */
	ecl_trail_undo(ec_eng, _handle_cleanup, pw, NULL, NULL, 0, 0);

	return handle;
}


/*
 * Get the data pointer from a handle (expect the given type)
 */
int Winapi
ec_get_handle(const pword handle, const t_ext_type *cl, t_ext_ptr *data)
{
	const pword * pw = &handle;
	Dereference_(pw);
	Get_Typed_Object(pw->val, pw->tag, cl, *data);
	Succeed_;
}


/*
 * Free the handle eagerly (expect the given type)
 */
int Winapi
ec_free_handle(const pword handle, const t_ext_type *cl)
{
	const pword * pw = &handle;
	Dereference_(pw);
	Check_Typed_Object_Handle(pw->val,pw->tag,(t_ext_type *) cl);
	AnchorFree(pw->val.ptr);
	Succeed_;
}


/*
 * Free the handle eagerly (generic)
 */
int
p_handle_free(value v_handle, type t_handle, ec_eng_t *ec_eng)
{
	Check_Type(t_handle, THANDLE);
	Check_Type(v_handle.ptr->tag, TEXTERN);
	AnchorFree(v_handle.ptr);
	Succeed_;
}


/*
 * Arrange for the handle to get freed on cut
 */
int
p_handle_free_on_cut(value v_handle, type t_handle, ec_eng_t *ec_eng)
{
	Check_Type(t_handle, THANDLE);
	Check_Type(v_handle.ptr->tag, TEXTERN);

	ecl_schedule_cut_fail_action(ec_eng, (void (*)(value,type,ec_eng_t*)) p_handle_free,v_handle,t_handle);
	Succeed_;
}


/*
 * Copy an anchor
 */
void
handle_copy_anchor(
	pword *from,	/* a heap or global stack location */
	pword *to,	/* a heap or global stack location */
	ec_eng_t *ec_eng)/* destination engine (NULL for heap) */
{
	to[0] = from[0];
	if (ExternalClass(from)->copy && ExternalData(from))
	    to[1].val.ptr = (pword *) ExternalClass(from)->copy(ExternalData(from));
	else
	    to[1].val.ptr = (pword *) ExternalData(from);
	to[1].tag.kernel = from[1].tag.kernel;

	/* Trail cleanup */
	if (ec_eng)
	    ecl_trail_undo(ec_eng, _handle_cleanup, to, NULL, NULL, 0, 0);
}


/*----------------------------------------------------------------------
 * Prolog-level locking
 *----------------------------------------------------------------------*/

/*
 * ecl_trail_undo(ec_eng, _handle_unlock, v.ptr, NULL, NULL, 0, 0);
 * The trailed item is the duplicated stack anchor of the locked object.
 * If it has already been untrailed, it is stale, and we do nothing here.
 */

static void
_handle_unlock(pword *panchor, word *pdata, int size, int undo_context, ec_eng_t *ec_eng)
{
    assert(panchor != NULL);
    assert(IsTag(panchor[0].tag.kernel, TEXTERN));
    assert(IsTag(panchor[1].tag.kernel, TPTR));
    if (ExternalData(panchor)) {
	DbgPrintf("Untrail, unlocking\n", ExternalData(panchor));
	if (ExternalClass(panchor)->unlock(ExternalData(panchor))) {
	    assert(0);
	}
    } else {
	DbgPrintf("Untrail, stale, no unlocking\n", 0);
    }
}


static int
p_handle_lock_trailed(value v, type t, value vflag, type tflag, ec_eng_t *ec_eng)
{
    int res;
    pword result;

    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->lock) {
	Bip_Error(UNIMPLEMENTED);
    }
    DbgPrintf("Locking 0x%x\n", ExternalData(v.ptr));
    res = ExternalClass(v.ptr)->lock(ExternalData(v.ptr));
    if (res) {
	Store_Eng_OSError_And_Group(res,SYS_ERROR_OS);
	Bip_Error(SYS_ERROR);
    }
    result = ecl_handle(ec_eng, ExternalClass(v.ptr),
		    ExternalClass(v.ptr)->copy(ExternalData(v.ptr)));
    ecl_trail_undo(ec_eng, _handle_unlock, result.val.ptr, NULL, NULL, 0, 0);
    Return_Unify_Pw(vflag, tflag, result.val, result.tag);
}


static int
p_handle_unlock_free(value v, type t, ec_eng_t *ec_eng)
{
    int res;

    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->unlock) {
	Bip_Error(UNIMPLEMENTED);
    }
    if (ExternalData((v).ptr)) {
	/* handle still valid: unlock */
	DbgPrintf("Unlocking 0x%x\n", ExternalData(v.ptr));
	res = ExternalClass(v.ptr)->unlock(ExternalData(v.ptr));
	if (res) {
	    Store_Eng_OSError_And_Group(res,SYS_ERROR_OS);
	    Bip_Error(SYS_ERROR);
	}
	/* free the handle, indicating that we must not unlock again */
	return p_handle_free(v, t, ec_eng);
    } else {
	DbgPrintf("Not unlocking, stale\n", 0);
	Succeed_;
    }
}


#if 0
static int
p_handle_lock(value v, type t, ec_eng_t *ec_eng)
{
    int res;
    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->lock) {
	Bip_Error(UNIMPLEMENTED);
    }
    res = ExternalClass(v.ptr)->lock(ExternalData(v.ptr));
    if (res) {
	Store_Eng_OSError_And_Group(res,SYS_ERROR_OS);
	Bip_Error(SYS_ERROR);
    }
    Succeed_;
}

static int
p_handle_trylock(value v, type t, ec_eng_t *ec_eng)
{
    int res;
    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->trylock) {
	Bip_Error(UNIMPLEMENTED);
    }
    res = ExternalClass(v.ptr)->trylock(ExternalData(v.ptr));
    if (res>0) {
	Store_Eng_OSError_And_Group(res,SYS_ERROR_OS);
	Bip_Error(SYS_ERROR);
    }
    Succeed_If(res==0);
}

static int
p_handle_unlock(value v, type t, ec_eng_t *ec_eng)
{
    int res;
    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->unlock) {
	Bip_Error(UNIMPLEMENTED);
    }
    res = ExternalClass(v.ptr)->unlock(ExternalData(v.ptr));
    if (res) {
	Store_Eng_OSError_And_Group(res,SYS_ERROR_OS);
	Bip_Error(SYS_ERROR);
    }
    Succeed_;
}
#endif


static int
p_condition_signal(value v, type t, value vall, type tall, ec_eng_t *ec_eng)
{
    int err, all;

    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->signal) { Bip_Error(UNIMPLEMENTED); }
    Check_Atom(tall);
    if (vall.did==d_.all) all=1;
    else if (vall.did==d_.one) all=0;
    else { Bip_Error(RANGE_ERROR); }

    err = ExternalClass(v.ptr)->signal(ExternalData(v.ptr), all);
    if (err) {
	Store_Eng_OSError_And_Group(err,SYS_ERROR_OS);
	Bip_Error(SYS_ERROR);
    }
    Succeed_;
}

static int
p_condition_wait(value v, type t, value vtimeout, type ttimeout, ec_eng_t *ec_eng)
{
    int err, timeout_ms;

    Check_Type(t, THANDLE);
    if (!ExternalClass(v.ptr)->wait) { Bip_Error(UNIMPLEMENTED); }
    if (IsAtom(ttimeout) && vtimeout.did == d_.block) {
    	timeout_ms = -1;
    } else {
	Get_Milliseconds(vtimeout, ttimeout, timeout_ms);
	if (timeout_ms < 0) { Bip_Error(RANGE_ERROR); }
    }

    /* Allow the wait to be preempted via ecl_interrupt_pause() */
    if (!ecl_pause_engine(ec_eng, 2L, PAUSE_CONDITION_WAIT, ExternalClass(v.ptr), ExternalData(v.ptr))) {
	Succeed_;	/* urgent request detected, return */
    }
    err = ExternalClass(v.ptr)->wait(ExternalData(v.ptr), timeout_ms);
    ecl_unpause_engine(ec_eng);
    if (err > 0) {
	Store_Eng_OSError_And_Group(err,SYS_ERROR_OS);
	Bip_Error(SYS_ERROR);
    }
    Succeed_If(err==0);		/* fail if timeout */
}


/**
 * is_handle(?Thing, -HandleType)
 * This fails for variables (like is_handle/1), non-handles, and stale handles.
 */
static int
p_is_handle(value v, type t, value vk, type tk, ec_eng_t *ec_eng)
{
    dident kind;
    if (!IsTag(t.kernel, THANDLE)) {
	Fail_;	/* like is_handle/1 type test, fail even for variables */
    }
    if (!ExternalData(v.ptr))
    	kind = d_.nil;
    else if (!ExternalClass(v.ptr)->kind)
    	kind = d_.question;
    else
    	kind = ExternalClass(v.ptr)->kind();
    Return_Unify_Atom(vk, tk, kind);
}


static int
p_name_to_handle(value vt, type tt, value vn, type tn, value vh, type th, value vm, type tm, ec_eng_t *ec_eng)
{
    t_ext_type *what;
    int property;

    Check_Atom(tt);
    if (vt.did == d_.record)	    { property = IDB_PROP;	what = &heap_rec_header_tid; }
    else if (vt.did == d_.shelf)    { property = SHELF_PROP;	what = &heap_array_tid; }
    else if (vt.did == d_.store)    { property = HTABLE_PROP;	what = &heap_htable_tid; }
    else if (vt.did == d_.stream)   { property = STREAM_PROP;	what = &stream_tid; }
    else { Bip_Error(RANGE_ERROR); }

    if (IsHandle(tn)) {
	Check_Typed_Object_Handle(vn, tn, what);
	Return_Unify_Pw(vh, th, vn, tn);

    } else {
	dident key_did;
	t_ext_ptr obj;
	pword result;
	int err;

	Get_Key_Did(key_did, vn, tn);
	err = get_visible_property_handle(key_did, property, vm.did, tm, what, &obj);
	if (err < 0) {
	    if (err==PERROR) { Fail_; }
	    Bip_Error(err);
	}
	result = ecl_handle(ec_eng, what, obj);
	Return_Unify_Pw(vh, th, result.val, result.tag);
    }
}


void
bip_handles_init(int flags)
{
    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("is_handle", 2), p_is_handle, B_SAFE);
	(void) built_in(in_dict("name_to_handle_", 4), p_name_to_handle, B_SAFE);
	(void) built_in(in_dict("handle_close", 1), p_handle_free, B_SAFE);
#if 0
	(void) built_in(in_dict("handle_lock", 1), p_handle_lock, B_SAFE);
	(void) built_in(in_dict("handle_trylock", 1), p_handle_trylock, B_SAFE);
	(void) built_in(in_dict("handle_unlock", 1), p_handle_unlock, B_SAFE);
#endif
	(void) built_in(in_dict("handle_lock_trailed", 2), p_handle_lock_trailed, B_SAFE);
	(void) built_in(in_dict("handle_unlock_free", 1), p_handle_unlock_free, B_SAFE);
	(void) built_in(in_dict("condition_signal", 2), p_condition_signal, B_SAFE);
	(void) built_in(in_dict("condition_wait", 2), p_condition_wait, B_SAFE);
    }
}
