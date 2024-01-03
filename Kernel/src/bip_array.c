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
 * VERSION	$Id: bip_array.c,v 1.7 2017/09/01 03:05:09 jschimpf Exp $
 */

/****************************************************************************
 *
 *	SEPIA Built-in Predicates for arrays and global variables
 *
 *	name		C func		type		file
 *	----------------------------------------------------------------
 *	make_array_	p_make_array_	B_SAFE
 *	setval_body	p_setval_body	B_SAFE
 *	getval_body	p_getval_body	B_UNSAFE
 *	incval_body	p_incval_body	B_SAFE
 *	decval_body	p_decval_body	B_SAFE
 *	array_info	p_array_info    B_UNSAFE
 *
 *****************************************************************************/


/*
 * Arrays are implemented as values of the property ARRAY_PROP.
 * The tag part holds a type (using the general type of prolog objects) 
 * and the value part holds relevant information for this type:
 * If the arity of the dictionary entry is greater than 0:
 * - TINT: integer array. The second word is a pointer to the array.
 * - TDBL: double float array. The second word is a pointer to the array.
 * - TSTRG: byte array. The second word is a pointer to the array.
 * - TCOMP: prolog array. The second word is a pointer to the array.
 * the header of an array looks as follows (in dident):
 * did    (backpointer and arity can be deduced (questionable approach))
 * dim1
 * ...
 * dimn
 * contents ...
 *
 * Global variables are implemented as the property GLOBVAR_PROP.
 * The property value is the value of the global variable.
 * If the value is a heapterm, the access needs PropertyLock.
 *
 * Global Arrays are allocated separately and have a pointer in the
 * ARRAY_PROP property. Access to the actual array needs PropertyLock.
 */


#include "config.h"
#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "error.h"
#include "mem.h"
#include "dict.h"
#include "emu_export.h"
#include "property.h"
#include "module.h"
#include "os_support.h"



static dident	d_reference_;


/*
 * For aligning arrays
 */

#define RoundUp(n) ((n) - ((n)-1)%sizeof(maxelsize) - 1 + sizeof(maxelsize))

typedef union {
    uword	w;
    word	l;
    double	d;
} maxelsize;



/*
 *  erase_array_body(Array, Module)
 */
static int
p_erase_array_body(value val1, type tag1, value vmod, type tmod, ec_eng_t *ec_eng)
{
    dident	adid;
    int		prop;
    int		err;

    Get_Functor_Did(val1, tag1, adid);
    Check_Module(tmod, vmod);
    
    if (DidArity(adid) > 0)
	prop = ARRAY_PROP;
    else
	prop = GLOBVAR_PROP;
    err = erase_property(adid, prop, vmod.did, tmod, VISIBLE_PROP);
    if (err < PSUCCEED)
    {
	if (err == PERROR)
	{
#ifdef COMPAT_REFERENCES_AS_ARRAYS
	    err = erase_property(adid, GLOBREF_PROP, vmod.did, tmod, VISIBLE_PROP);
	    if (err >= PSUCCEED) {
		Succeed_;
	    }
	    if (err == PERROR)
#endif
		err = NOGLOBAL;
	}
        Bip_Error(err);
    }
    Succeed_;
}


/* get_elt_address must be called in an interrupt protected area */
uword *
get_elt_address(value v, type t, uword *kind, dident mod_did, type mod_tag, int *perr)
{
    pword	*pw, *q, *h, *p;
    int		ndim1, ndim2, i, n, err;
    dident	arraydid;
    uword	*w;

    if (IsList(t))
    	arraydid = d_.list;
    else
   	arraydid = v.ptr->val.did;
    ndim1 = DidArity(arraydid);
    if (IsList(t))
    	p = h = v.ptr - 1;
    else
    	p = h = v.ptr;
    for (i=0; i < ndim1; i++)
    {
	q = ++h;
	Dereference_(q)
	if(IsRef(q->tag))
	{
	    *perr = INSTANTIATION_FAULT;
	    return 0;
	}
	if(DifferTypeC(q->tag,TINT))
	{
	    *perr = TYPE_ERROR;
	    return 0;
	}
    }

    err = get_property_ref(arraydid, ARRAY_PROP, mod_did, mod_tag, VISIBLE_PROP, &pw);
    if (err < 0)
    {
	*perr = err==PERROR? NOGLOBAL: err;
	return 0;
    }
    *kind = pw->tag.kernel;
    pw = pw->val.ptr;
    ndim2 = DidArity((pw)->val.did);
    n = 0;
    w = ((uword *) pw) + 1;
    for(i = 0; i < ndim2; i++)
    {
	q = ++p;
	Dereference_(q)
	n *= *w;
	if(*w++ <= q->val.nint || q->val.nint < 0)
	{
	    *perr = RANGE_ERROR;
	    return 0;
	}
	n += q->val.nint;
    }
    w = (uword *)pw + RoundUp((ndim2+1)*sizeof(uword))/sizeof(uword);
    switch (*kind)
    {
    case TCOMP:		return (uword *) (((pword *) w) + n);
    case TSTRG:		return (uword *) (((unsigned char *)w) + n);
    case TINT:		return (uword *) (((word *)w) + n);
    case TDBL:		return (uword *) (((double *)w) + n);
    default:		return (uword *) 0;
    }
}

/* get_first_elt must be called in an interrupt protected area		*/
word
get_first_elt(pword *p, pword *q, uword *kind, uword *size, dident vmod_did, type mod_tag)
{
    dident mydid;
    uword *w;
    word i, n;
    int err;
    pword *pw;

    Dereference_(p)
    if (IsRef(p->tag))
	return(INSTANTIATION_FAULT);
    if (DifferTypeC(p->tag,TDICT))
	return(TYPE_ERROR);
    Dereference_(q)
    if (IsRef(q->tag))
	return(INSTANTIATION_FAULT);
    if (DifferTypeC(q->tag,TINT))
	return(TYPE_ERROR);
    n = q->val.nint;
    if (n<1)
	return(RANGE_ERROR);
    mydid = check_did(p->val.did, n);
    if (mydid == D_UNKNOWN)
	return(NOGLOBAL);
    err = get_property_ref(mydid, ARRAY_PROP, vmod_did, mod_tag, VISIBLE_PROP, &pw);
    if (err < 0)
	return err==PERROR? NOGLOBAL: err;
    *size = 4;
    *kind = pw->tag.kernel;
    switch (*kind)
    {
    case TCOMP:		*size = sizeof(pword); break;
    case TSTRG:		*size = sizeof(char); break;
    case TINT:		*size = sizeof(word); break;
    case TDBL:		*size = sizeof(double); break;
    }
    pw = pw->val.ptr;
    w = ((uword *) pw) + 1;
    for(i = 0; i < n ; i++)
	*size *= *w++;
    w = (uword *)pw + RoundUp((n+1)*sizeof(uword))/sizeof(uword);
    return((word) w);
}


static int 
p_test_and_setval_body(value a, type ta, value vc, type tc, value v, type t, value vmod, type tmod, ec_eng_t *ec_eng)
{
    int		err;

    Check_Module(tmod, vmod);
    Error_If_Ref(ta);
    if (IsAtom(ta) || IsNil(ta))
    {
    	pword *pw;
	pword copy_pw;
	
	mt_mutex_lock(&PropertyLock);
	err = get_property_ref(IsNil(ta) ? d_.nil : a.did, GLOBVAR_PROP,
				vmod.did, tmod, VISIBLE_PROP, &pw);
    	if (err < 0)
    	{
	    if (err == PERROR)
		err = NOGLOBAL;
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
        }
	if (IsGlobalPrologRefIndex(pw) || IsGlobalPrologRef(pw))
	{
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(TYPE_ERROR);
	}
	if (ec_compare_terms(vc, tc, pw->val, pw->tag))
	{
	    mt_mutex_unlock(&PropertyLock);
	    Fail_;
	}
        err = create_heapterm(ec_eng, &copy_pw, v, t);
	if (err != PSUCCEED)
	{
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
	}
	free_heapterm(pw);
        move_heapterm(&copy_pw, pw);
	mt_mutex_unlock(&PropertyLock);
	Succeed_;
    }
    else
    {
        Bip_Error(TYPE_ERROR);
    }
}


/*
 * Global References (multi-engine implementation)
 *
 * Declaration is recorded as a property, including the init value.
 * In addition, every engine has a table  module x name -> reference,
 * where an entry is created and initialized the first time the reference
 * is accessed (set or get).  The actual mutable cell is allocated on
 * the global stack (we could use a heap cell, but that would imply
 * trail pointers to the heap, difficult to relocate in engine copying).
 * The table entry is removed again on backtracking.
 * There are subtle semantic differences from the previous single-thread
 * implementation: when the reference is destroyed (currently only
 * via erase_module), any initialized engine-table entries continue
 * to exist and function.  The destruction will only be noticed after
 * failure across the initialization point in the particular engine,
 * i.e. no further initialization can be made.
 */

/* The following builtin uses the global error variable ! */
#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

static int
p_makeref(value vn, type tn, value vinit, type tinit, value vmod, type tmod, ec_eng_t *ec_eng)
{
    pword init_pw, *prop;
    int res;

    Check_Atom_Or_Nil(vn, tn);
    Check_Module_And_Access(vmod, tmod);

    /* If this is a re-declaration (after erase), some thread may still
     * have an active reference, but that is too hard to check for. */

    if (IsRef(tinit) && IsSelfRef(vinit.ptr)) {
        Make_Var(&init_pw);
    } else {
        /* make a reusable heap copy of the initalization value */
        res = ec_constant_table_enter(ec_eng, vinit, tinit, &init_pw);
        if (res != PSUCCEED)
        {
            Bip_Error(res==PFAIL ? UNIMPLEMENTED : res);
        }
    }

    mt_mutex_lock(&PropertyLock);
    res = get_property_ref(vn.did, GLOBREF_PROP, vmod.did, tmod, LOCAL_PROP, &prop);
    if (res < 0) {
	mt_mutex_unlock(&PropertyLock);
	Bip_Error(res);
    }
    move_heapterm(&init_pw, prop);
    mt_mutex_unlock(&PropertyLock);
    Succeed_;
}

#undef Bip_Error
#define Bip_Error(N) return(N);


/*
 * remove module:name entry from global references list
 */
static void
_initref_undo(pword *item, word *data, int size, int flags, ec_eng_t *ec_eng)
{
    dident name   = ((pword*)data)[0].val.did;
    dident module = ((pword*)data)[1].val.did;
    globalref **prev = &ec_eng->references;
    globalref *p;

    assert(flags == UNDO_FAIL);
    for(p=*prev; p; prev=&p->next,p=p->next) {
	if (p->module == module && p->name == name)
	{
	    *prev = p->next;
	    hp_free_size(p, sizeof(globalref));
	    return;
	}
    }
    assert(0);
}


/*
 * Create entry for module:name in global references list
 * PRE: no corresponding entry in ec_eng->references yet
 */
static int
_initref(ec_eng_t *ec_eng, dident name, dident module, type tmod, globalref **pp)
{
    int res; 
    pword init_pw;
    globalref *p;
    pword data[2];

    /* look up the initialization value (simple, because tabled constant) */
    res = get_visible_property(name, GLOBREF_PROP, module, tmod, &init_pw);
    if (res < 0) {
	Bip_Error(res==PERROR? NO_LOCAL_REC: res);
    }

    /* create entry */
    p = (globalref*) hp_alloc_size(sizeof(globalref));
    p->name = name;
    p->module = module;
    p->ptr = TG++;
    if (IsRef(init_pw.tag)) {
        Make_Var(p->ptr);
    } else {
        *p->ptr = init_pw;      /* simple or tabled constant */
    }
    Check_Gc;
    p->next = ec_eng->references;
    ec_eng->references = p;
    *pp = p;

    /* schedule removal on failure */
    Make_Atom(&data[0], name);
    Make_Atom(&data[1], module);
    ecl_trail_undo(ec_eng, _initref_undo, NULL, NULL,
	    (word*) data, 2*sizeof(pword)/sizeof(word), TRAILED_PWORD);
    Succeed_;
}


static int
p_setref(value vn, type tn, value v, type t, value vmod, type tmod, ec_eng_t *ec_eng)
{
    int res;
    globalref *p = ec_eng->references;

    Check_Atom_Or_Nil(vn, tn);
    Check_Module_And_Access(vmod, tmod);

    for(;; p=p->next) {
	if (!p) {
	    res = _initref(ec_eng, vn.did, vmod.did, tmod, &p);
	    Return_If_Error(res);
	    break;
	}
	if (p->module == vmod.did && p->name == vn.did)
	    break;
    }
    return ecl_assign(ec_eng, p->ptr, v, t);
}

static int
p_swapref(value vn, type tn, value vold, type told, value v, type t, value vmod, type tmod, ec_eng_t *ec_eng)
{
    int res;
    globalref *p = ec_eng->references;

    Check_Atom_Or_Nil(vn, tn);
    Check_Module_And_Access(vmod, tmod);

    for(;; p=p->next) {
	if (!p) {
	    res = _initref(ec_eng, vn.did, vmod.did, tmod, &p);
	    Return_If_Error(res);
	    break;
	}
	if (p->module == vmod.did && p->name == vn.did)
	    break;
    }
    pword old = *p->ptr;
    res = ecl_assign(ec_eng, p->ptr, v, t);
    Return_If_Not_Success(res);
    Return_Unify_Pw(vold, told, old.val, old.tag);
}


static int
p_getref(value vn, type tn, value v, type t, value vmod, type tmod, ec_eng_t *ec_eng)
{
    int res;
    globalref *p = ec_eng->references;

    Check_Atom_Or_Nil(vn, tn);
    Check_Module_And_Access(vmod, tmod);

    for(;; p=p->next) {
	if (!p) {
	    res = _initref(ec_eng, vn.did, vmod.did, tmod, &p);
	    Return_If_Error(res);
	    break;
	}
	if (p->module == vmod.did && p->name == vn.did)
	    break;
    }
    Return_Unify_Pw(v, t, p->ptr->val, p->ptr->tag);
}


int 
p_setval_body(value a, type ta, value v, type t, value vmod, type tmod, ec_eng_t *ec_eng)
{
    int		err;
    pword	copy_pw;

    Check_Module(tmod, vmod);
    if (IsAtom(ta) || IsNil(ta))
    {
    	pword *pw;

	mt_mutex_lock(&PropertyLock);
	err = get_property_ref(IsNil(ta) ? d_.nil : a.did, GLOBVAR_PROP,
				vmod.did, tmod, VISIBLE_PROP, &pw);
	if (err < 0)
    	{
	    mt_mutex_unlock(&PropertyLock);
	    if (err == PERROR) {
		/* backward compatibility: allow to set references */
		err = p_setref(a, ta, v, t, vmod, tmod, ec_eng);
		return err == NO_LOCAL_REC? NOGLOBAL: err;
	    }
	    Bip_Error(err);
        }
        err = create_heapterm(ec_eng, &copy_pw, v, t);
	if (err != PSUCCEED)
	{
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
	}
	free_heapterm(pw);
        move_heapterm(&copy_pw, pw);
	mt_mutex_unlock(&PropertyLock);
	Succeed_;
    }
    if (IsStructure(ta) || IsList(ta))
    {
    	uword	*adr;
    	uword	kind;
   
	mt_mutex_lock(&PropertyLock);
    	if (!(adr = get_elt_address(a, ta, &kind, vmod.did, tmod, &err)))
	{
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
    	}
	err = PSUCCEED;
	switch (kind)
	{
	case TCOMP:
	    free_heapterm((pword *)adr);
	    err = create_heapterm(ec_eng, (pword *)adr,v,t);
	    break;
	case TSTRG:
	    if (IsRef(t)) err = INSTANTIATION_FAULT;
	    else if (!IsInteger(t)) err = TYPE_ERROR;
	    else *((unsigned char *) adr) = (v.nint & 0XFF);
	    break;
	case TINT:
	    if (IsRef(t)) err = INSTANTIATION_FAULT;
	    else if (!IsInteger(t)) err = TYPE_ERROR;
	    else *((word *) adr) = v.nint;
	    break;
	case TDBL:
	    if (IsRef(t)) err = INSTANTIATION_FAULT;
	    else if (!IsDouble(t)) err = TYPE_ERROR;
	    else *((double *) adr) = Dbl(v);
	    break;
	}
	mt_mutex_unlock(&PropertyLock);
	return err;
    }
    Error_If_Ref(ta);
    Bip_Error(TYPE_ERROR);
}


static int
p_getval_body(value a, type ta, value v, type t, value vmod, type tmod, ec_eng_t *ec_eng)
{
    int		err;

    Check_Module(tmod, vmod);
    if (IsAtom(ta) || IsNil(ta))
    {
    	pword	*p;
	pword	result;
	
	mt_mutex_lock(&PropertyLock);
	err = get_property_ref(IsNil(ta) ? d_.nil : a.did, GLOBVAR_PROP,
				vmod.did, tmod, VISIBLE_PROP, &p);
	if (err < 0)
    	{
	    mt_mutex_unlock(&PropertyLock);
	    if (err == PERROR) {
		/* backward compatibility: allow to get references */
		err = p_getref(a, ta, v, t, vmod, tmod, ec_eng);
		return err == NO_LOCAL_REC? NOGLOBAL: err;
	    }
	    Bip_Error(err);
	}
    	get_heapterm(ec_eng, p, &result);
	mt_mutex_unlock(&PropertyLock);

	if (IsRef(result.tag) && result.val.ptr == &result)
	{
	    Succeed_;		/* a free variable	*/
	}
	Return_Unify_Pw(v,t,result.val,result.tag);
    }

    if (IsStructure(ta) || IsList(ta))
    {
	uword	*adr;
	uword	kind;
	pword	result;

	mt_mutex_lock(&PropertyLock);
	if (!(adr = get_elt_address(a, ta, &kind, vmod.did, tmod, &err)))
	{
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
	}
	switch (kind)
	{
	case TCOMP:
	    get_heapterm(ec_eng, (pword *)adr, &result);
	    if (IsRef(result.tag) && result.val.ptr == &result)
	    {
		mt_mutex_unlock(&PropertyLock);
		Succeed_;		/* a free variable	*/
	    }
	    break;
	case TSTRG:
	    result.val.nint = (word) *((unsigned char *) adr);
	    result.tag.kernel = TINT;
	    break;
	case TINT:
	    result.val.nint = (word) *((word *) adr);
	    result.tag.kernel = TINT;
	    break;
	case TDBL:
	    Make_Float(&result, *((double *) adr))
	    break;
	}
	mt_mutex_unlock(&PropertyLock);
	Return_Unify_Pw(v,t,result.val,result.tag);
    }
    Error_If_Ref(ta);
    Bip_Error(TYPE_ERROR);
}


static int
p_incval_body(value a, type ta, value vmod, type tmod, ec_eng_t *ec_eng)
{
    pword	*p;
    int		err;
    
    Check_Module(tmod, vmod);
    if (IsAtom(ta) || IsNil(ta))
    {
	mt_mutex_lock(&PropertyLock);
	err = get_property_ref(IsNil(ta) ? d_.nil : a.did, GLOBVAR_PROP,
				vmod.did, tmod, VISIBLE_PROP, &p);
	if (err < 0)
	{
	    if (err == PERROR)
		err = NOGLOBAL;
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
	}
	if((!IsInteger(p->tag)))
	{
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(TYPE_ERROR);
	}
	p->val.nint++;
	mt_mutex_unlock(&PropertyLock);
	Succeed_;
    }
    if (IsStructure(ta) || IsList(ta))
    {
    	uword	*adr;
    	uword	kind;
	pword	*pi;
	
	mt_mutex_lock(&PropertyLock);
    	if (!(adr = get_elt_address(a, ta, &kind, vmod.did, tmod, &err)))
	{
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
    	}
	if (kind == TINT)
	{
	    (*((int *) adr))++;
	}
	else if (kind == TCOMP)
	{
	    pi = (pword *) adr;
	    if (IsInteger(pi->tag))
	    {
		pi->val.nint++;
	    }
	    else
	    {
		mt_mutex_unlock(&PropertyLock);
		Bip_Error(TYPE_ERROR);
	    }
	}
	else
	{
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(TYPE_ERROR);
        }
	mt_mutex_unlock(&PropertyLock);
    	Succeed_;
    }
    Error_If_Ref(ta);
    Bip_Error(TYPE_ERROR);
}

static int
p_decval_body(value a, type ta, value vmod, type tmod, ec_eng_t *ec_eng)
{
    pword	*p;
    int		err;

    Check_Module(tmod, vmod);
    if (IsAtom(ta) || IsNil(ta))
    {
	mt_mutex_lock(&PropertyLock);
	err = get_property_ref(IsNil(ta) ? d_.nil : a.did, GLOBVAR_PROP,
				vmod.did, tmod, VISIBLE_PROP, &p);
	if (err < 0)
	{
	    if (err == PERROR)
		err = NOGLOBAL;
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
	}
	if((!IsInteger(p->tag)))
	{
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(TYPE_ERROR);
	}
	p->val.nint--;
	mt_mutex_unlock(&PropertyLock);
	Succeed_;
    }
    if (IsStructure(ta) || IsList(ta))
    {
    	uword	*adr;
    	uword	kind;
	pword	*pi;
   
	mt_mutex_lock(&PropertyLock);
    	if (!(adr = get_elt_address(a, ta, &kind, vmod.did, tmod, &err)))
	{
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
    	}
	if (kind == TINT)
	{
	    (*((int *) adr))--;
	}
	else if (kind == TCOMP)
	{
	    pi = (pword *) adr;
	    if (IsInteger(pi->tag))
	    {
		pi->val.nint--;
	    }
	    else
	    {
		mt_mutex_unlock(&PropertyLock);
		Bip_Error(TYPE_ERROR);
	    }
	}
	else
	{
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(TYPE_ERROR);
        }
	mt_mutex_unlock(&PropertyLock);
    	Succeed_;
    }
    Error_If_Ref(ta);
    Bip_Error(TYPE_ERROR);
}

/*
 * array_info(+Array, ?OptionList, +Module)
 *
 * The arguments of Array will be unified with the dimension sizes,
 * OptionList is unified with a two element list [<type>, <visibility>]
 */

static int
p_array_info(value varr, type tarr, value vopt, type topt, value vmod, type tmod, ec_eng_t *ec_eng)
{
    pword	prop;
    pword	*pw = (pword *) 0;
    int		i, arity, err;
    dident	wdid, vis;
    uword	*w;
    value	v;
    Prepare_Requests

    Check_Module(tmod, vmod);
    Check_Output_List(topt);
    switch (TagType(tarr))
    {
    case TLIST:
    	wdid = d_.list;
	pw = varr.ptr;
	break;
    case TCOMP:
    	wdid = varr.ptr->val.did;
	pw = varr.ptr + 1;
	break;
    case TDICT:
    	wdid = varr.did;
	break;
    case TNIL:
    	wdid = d_.nil;
	break;
    default:
	Bip_Error(IsRef(tarr) ? INSTANTIATION_FAULT : TYPE_ERROR);
    }

    arity = DidArity(wdid);
    err = get_visible_property(wdid, arity?ARRAY_PROP:GLOBVAR_PROP, vmod.did, tmod, &prop);
    if (err >= 0)
    {
	if (arity == 0)
	{
	    wdid = d_.prolog;
	}
	else
	{
	    switch(TagType(prop.tag))		/* get the type */
	    {
	    case TCOMP:
		wdid = d_.prolog;
		break;
	    case TSTRG:
		wdid = d_.byte;
		break;
	    case TINT:
		wdid = d_.integer0;
		break;
	    case TDBL:
		wdid = d_.float0;
		break;
	    default:
		p_fprintf(current_err_,
			"internal error: array structure corrupted\n");
		ec_flush(current_err_);
	    }

	    w = ((uword *)(prop.val.ptr) + 1);	/* unify the dimensions */
	    for(i = 0; i < arity; i++)
	    {
		v.nint = (word) *w++;
		Request_Unify_Pw(pw->val, pw->tag, v, tint);
		pw++;
	    }
	}
    }
    else
    {
#ifdef COMPAT_REFERENCES_AS_ARRAYS
	/* no variable/array, check for reference (backward compatibility) */
	err = get_visible_property(wdid, GLOBREF_PROP, vmod.did, tmod, &prop);
	if (err < 0) {
	    if (err == PERROR) {
		Fail_; /* no reference either */
	    } else {
		Bip_Error(err)
	    }
	}
	wdid = d_reference_;
#else
	if (err == PERROR) {
	    Fail_;
	} else {
	    Bip_Error(err)
	}
#endif
    }

    vis = (err == LOCAL_PROP) ? d_.local0 : d_.global0;

    pw = TG;				/* make options list */
    TG += 4;
    Check_Gc;
    pw[0].val.did = wdid;		/* [type, visibility] */
    pw[0].tag.kernel = TDICT;
    pw[1].val.ptr = &pw[2];
    pw[1].tag.kernel = TLIST;
    pw[2].val.did = vis;
    pw[2].tag.kernel = TDICT;
    pw[3].tag.kernel = TNIL;
    Request_Unify_List(vopt, topt, pw);
    Return_Unify
}


/*
 * free all the memory occupied by the array
 */

void
ec_free_array(pword *prop_value)
{
    uword *array_header = (uword *) prop_value->val.ptr;

    if (IsStructure(prop_value->tag))
    {
	int	dim = DidArity(array_header[0]);
	pword	*array_contents = (pword *)
	    (array_header + RoundUp((dim+1)*sizeof(uword))/sizeof(uword));
	uword	size;

	for (size = 1; dim > 0; --dim)	/* compute number of elements */
	    size *= array_header[dim];

	for (; size > 0; --size)
	    free_heapterm(array_contents++);
    }
    hg_free(array_header);
}


/*
 * Support function for the dictionary garbage collector:
 * Mark all DID's inside the array (applies only to 'prolog' arrays)
 */

void
mark_dids_from_array(pword *prop_value)
{
    extern void mark_dids_from_heapterm(pword *root);

    if (IsStructure(prop_value->tag))
    {
	uword	*array_header = (uword *) prop_value->val.ptr;
	int	dim = DidArity(array_header[0]);
	pword	*array_contents = (pword *)
	    (array_header + RoundUp((dim+1)*sizeof(uword))/sizeof(uword));
	register uword	size;

	for (size = 1; dim > 0; --dim)	/* compute number of elements */
	    size *= array_header[dim];

	for (; size > 0; --size)
	    mark_dids_from_heapterm(array_contents++);
    }
}

static int
p_xset(value vhandle, type thandle, value vi, type ti, value vval, type tval, ec_eng_t *ec_eng)
{
    pword pw;
    pw.val = vval;
    pw.tag = tval;
    Check_Type(thandle, THANDLE);
    Check_Type(vhandle.ptr->tag, TEXTERN);
    Check_Integer(ti);
    if (!(ExternalData(vhandle.ptr)))
	Bip_Error(STALE_HANDLE);
    if (!ExternalClass(vhandle.ptr)->set)
    	{ Bip_Error(UNIMPLEMENTED); }
    return ExternalClass(vhandle.ptr)->set(ExternalData(vhandle.ptr), vi.nint, pw, ec_eng);
}

static int
p_xget(value vhandle, type thandle, value vi, type ti, value vval, type tval, ec_eng_t *ec_eng)
{
    pword pw;
    Check_Type(thandle, THANDLE);
    Check_Type(vhandle.ptr->tag, TEXTERN);
    Check_Integer(ti);
    if (!(ExternalData(vhandle.ptr)))
	Bip_Error(STALE_HANDLE);
    if (!ExternalClass(vhandle.ptr)->get)
    	{ Bip_Error(UNIMPLEMENTED); }
    pw = ExternalClass(vhandle.ptr)->get(ExternalData(vhandle.ptr), vi.nint, ec_eng);
    Return_Unify_Pw(vval, tval, pw.val, pw.tag);
}


/* The following builtins use the global error variable ! */
#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

/*
  Create an array v of type vt in module vmod, vscope can
  be local or global.
*/
/*ARGSUSED*/
int
p_make_array_(value v, type t, value vt, type tt, value vscope, type tscope, value vmod, type tmod, ec_eng_t *ec_eng)
{
    int		ndim, size, i, nitem, err;
    pword	*p, *pp, *spw;
    type	tag;
    dident	arraydid;
    uword	*w;
    int		header_size;
    int		scope = (vscope.did == d_.local0 ? LOCAL_PROP : GLOBAL_PROP);

    Check_Module(tmod, vmod);
    Check_Module_Access(vmod, tmod);
    /* no need to check for tscope, system use only */

    if (IsAtom(t) || IsNil(t))	/* --------- variable --------- */
    {
	dident		wd = IsNil(t) ? d_.nil : v.did;
	pword		init_pw;

	Check_Atom(tt);
	Make_Integer(&init_pw, 0);
	if (vt.did != d_.prolog)
	{
	    Bip_Error(RANGE_ERROR);
	}

	mt_mutex_lock(&PropertyLock);
	err = get_property_ref(wd, GLOBVAR_PROP, vmod.did, tmod, scope, &p);
	if (err < 0) {
	    mt_mutex_unlock(&PropertyLock);
	    Bip_Error(err);
	}
	Make_Var(p);
	mt_mutex_unlock(&PropertyLock);
	Succeed_;
    }

    if (IsList(t))		/* -------------- Array ------------------*/
    {
    	arraydid = d_.list;
    }
    else
    {
	Check_Structure(t);
	arraydid = v.ptr->val.did;
    }

    Check_Atom(tt);
    if (vt.did == d_.prolog)
    {
	tag.kernel = TCOMP;
	size = sizeof(pword);
    }
    else if(vt.did == d_.byte)
    {
	tag.kernel = TSTRG;
	size = 1;
    }
    else if(vt.did == d_.integer0)
    {
	tag.kernel = TINT;
	size = sizeof(word);
    }
    else if(vt.did == d_.float0)
    {
	tag.kernel = TDBL;
	size = sizeof(double);
    }
    else
    {
	Bip_Error(RANGE_ERROR);
    }

    ndim = DidArity(arraydid);
    nitem = 1;

    /* compute the number of items which will be held by the array */
    if (IsList(t))
    	p = v.ptr - 1;
    else
     	p = v.ptr;
    for(i = 0; i < ndim; i++)
    {
	spw = ++p;
	Dereference_(spw);
	Check_Integer(spw->tag);
	if (spw->val.nint <= 0)
	{
	    Bip_Error(RANGE_ERROR);
	}
	nitem *= spw->val.nint;
    }

    /* We might need padding to properly align the array */
    header_size = RoundUp((ndim+1)*sizeof(uword));
    
    mt_mutex_lock(&PropertyLock);
    err = get_property_ref(arraydid, ARRAY_PROP, vmod.did, tmod, scope, &p);
    if (err < 0) {
	mt_mutex_unlock(&PropertyLock);
	Bip_Error(err);
    }
#if WARN_ARRAY_REDEFINITION
    if (!(err & NEW_PROP)) {
	/* trying to define a global when there is a global or
	   a local when there is a local here				*/
	mt_mutex_unlock(&PropertyLock);
	Bip_Error(ARRAY_EXISTS);
    }
#endif

    /* grab space for this array */
    p->tag.all = tag.all;			/* type of the array */
    p->val.ptr = (pword *)hg_alloc(size*nitem + header_size);
    p = p->val.ptr;
    /* initialize the header of the array */
    
    p->val.did = arraydid;	/* thus backward pointer and
				   the number of dimensions		*/
    w = ((uword *) p) + 1;     /* skip did information			*/
    
    if (IsList(t))
    	pp = v.ptr - 1;
    else
     	pp = v.ptr;
    for(i = 0; i < ndim; i++)
    {
	spw = ++pp;
	Dereference_(spw);
	*w++ = spw->val.nint;	/* size of each dimension */
    }

    /* initialize the elements */
    w = (uword *)p + header_size/sizeof(uword);
    switch (tag.kernel)
    {
    case TCOMP:
	p = (pword *) w;
	for(i = 0; i < nitem; i++)
	{
	    p->val.ptr = p;
	    (p++)->tag.kernel = TREF;
	}
	break;
    case TSTRG:
	{
	    unsigned char *s = (unsigned char *) w;
	    for(i = 0; i < nitem ; i++) *s++ = 0;
	}
	break;
    case TINT:
	{
	    word *s = (word *) w;
	    for(i = 0; i < nitem ; i++) *s++ = 0;
	}
	break;
    case TDBL:
	{
	    double *s = (double *) w;
	    for(i = 0; i < nitem ; i++) *s++ = 0.0;
	}
	break;
    }
    mt_mutex_unlock(&PropertyLock);
    Succeed_;
}



pword *
get_kernel_array(dident adid)
{
    int res;
    pword module;
    pword *pw = NULL;
    if (DidArity(adid) != 1)
	return 0;
    module.tag.kernel = ModuleTag(d_.kernel_sepia);
    module.val.did = d_.kernel_sepia;

    res = get_property_ref(adid, ARRAY_PROP,
		module.val.did, module.tag, VISIBLE_PROP, &pw);
    return pw;
}

int
make_kernel_array(ec_eng_t *ec_eng, dident adid, int length, dident atype, dident avisib)
{
    pword module;
    pword buf[5];

    if (DidArity(adid) != 1)
	return RANGE_ERROR;
    module.tag.kernel = ModuleTag(d_.kernel_sepia);
    module.val.did = d_.kernel_sepia;
    buf[0].val.ptr = &buf[3];
    buf[0].tag.kernel = TCOMP;
    buf[1].val.did = atype;
    buf[1].tag.kernel = TDICT;
    buf[2].val.did = avisib;
    buf[2].tag.kernel = TDICT;
    buf[3].val.did = adid;		/*  must be arity 1 !!! */
    buf[3].tag.kernel = TDICT;
    buf[4].val.nint = (word) length;
    buf[4].tag.kernel = TINT;
    return p_make_array_(buf[0].val, buf[0].tag, buf[1].val, buf[1].tag,
		    buf[2].val, buf[2].tag, module.val, module.tag, ec_eng);
}


void
bip_array_init(int flags)
{
    pri		*pd;

    if (flags & INIT_SHARED)
    {
	local_built_in(in_dict("array_info", 3), p_array_info, B_UNSAFE)
	    -> mode = BoundArg(1, GROUND) | BoundArg(2, GROUND);
	(void) local_built_in(in_dict("make_array_", 4),
			      p_make_array_, B_SAFE);
	(void) exported_built_in(in_dict("erase_array_body", 2),
				 p_erase_array_body, B_SAFE);
	pd = exported_built_in(in_dict("test_and_setval_body", 4), p_test_and_setval_body, B_SAFE);
	pd = exported_built_in(in_dict("setval_body", 3), p_setval_body, B_SAFE);
	pd = exported_built_in(in_dict("getval_body", 3), p_getval_body, B_UNSAFE|U_FRESH);
	pd -> mode = BoundArg(2, NONVAR);
	pd = exported_built_in(in_dict("incval_body",2), p_incval_body, B_UNSAFE);
	pd = exported_built_in(in_dict("decval_body",2), p_decval_body, B_UNSAFE);
	built_in(in_dict("xget",3), p_xget, B_UNSAFE)->mode = GROUND;
	built_in(in_dict("xset",3), p_xset, B_SAFE);
	local_built_in(in_dict("makeref_",3), p_makeref, B_SAFE);
	built_in(in_dict("setref_",3), p_setref, B_SAFE);
	built_in(in_dict("getref_",3), p_getref, B_UNSAFE);
	built_in(in_dict("swapref_",4), p_swapref, B_UNSAFE);
    }

    if (flags & INIT_PRIVATE)
    {
	value	vv, vm, vn, vt;

	d_reference_ = in_dict("reference", 0);

#ifdef DFID
	/* Initialization of predefined global Prolog variables */
	vv.did = d_.local0;
	vm.did = d_.kernel_sepia;

	/* temporary: use old style globvar-index for DFID variables */
	d_global_reference_index_ = in_dict("global_reference_index", 0);
	vt.did = d_global_reference_index_;

	/* global var 0 - unused (used to be postponed list) */
	GlobalVarIndex++;
	vm.did = in_dict("dfid", 0);
	/* global var 1 - DfidDepth */
	vn.did = in_dict("depth", 0);
	(void) p_make_array_(vn, tdict, vt, tdict, vv, tdict, vm, tdict, ec_eng);
	/* global var 2 - MaxDepth */
	vn.did = in_dict("max_depth", 0);
	(void) p_make_array_(vn, tdict, vt, tdict, vv, tdict, vm, tdict, ec_eng);
	/* global var 3 - DepthLimit */
	vn.did = in_dict("depth_limit", 0);
	(void) p_make_array_(vn, tdict, vt, tdict, vv, tdict, vm, tdict, ec_eng);
	/* global var 4 - DepthOV */
	vn.did = in_dict("depth_ov", 0);
	(void) p_make_array_(vn, tdict, vt, tdict, vv, tdict, vm, tdict, ec_eng);
#endif
    }

    /*
     * Initialize some global Prolog variables in sepia_kernel
     * that need to be accessed from C as well.
     */

}

/* Add all new code in front of the initialization function! */
