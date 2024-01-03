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
 * ECLiPSe LIBRARY MODULE
 *
 * $Id: embed.c,v 1.11 2017/09/01 03:05:09 jschimpf Exp $
 *
 *
 * IDENTIFICATION:	embed.c
 *
 * AUTHOR:		Joachim Schimpf
 * AUTHOR:		Stefano Novello
 *
 * CONTENTS:		name/arity
 *
 * DESCRIPTION:
 *	Call interface to embedded eclipse
 */


#include 	"config.h"
#include        "sepia.h"
#include 	"types.h"
#include 	"error.h"
#include 	"mem.h"
#include 	"dict.h"
#include	"module.h"
#include	"emu_export.h"
#include	"embed.h"
#include	"os_support.h"

#include <errno.h>

#ifdef STDC_HEADERS
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#else
#include <varargs.h>
extern char *	strcat();
extern char *	strcpy();
#endif


/*----------------------------------------------------------------------
 * External references:
 *
 * States of external references:
 *
 * EC_REF_C:	hp_allocated, simple value, not in global list
 *
 *	This is the state just after an ec_refs has been created by a
 *	call to ecl_refs_create(), or after backtracking to such a point.
 *	It is not "initialised" yet, i.e. no array (structure) for the
 *	n slots has been allocated on the global stack, and it is not
 *	yet known to the garbage collector. The var-field preliminarily
 *	holds the init-value instead of a pointer to a global stack array.
 *	
 * EC_REF_C_P:	hp_allocated, prolog value, in global list
 *
 *	This is the normal working state: the ec_refs is used from the
 *	C program, its var-field points to a global stack array of arity
 *	n, and it is known to the garbage collector via the global list.
 *	The transition from EC_REF_C to EC_REF_C_P happens on the first
 *	access to the ec_refs: a global stack array is allocated and its
 *	slots initialised with the requested init value.
 *
 * EC_REF_FREE:	deallocated, no value, not in global list
 *
 *	This state only exists temporarily just before deallocation.
 * 
 * Allowed transitions:
 * (none)	--create-->	EC_REF_C
 * EC_REF_C	--init-->	EC_REF_C_P
 * EC_REF_C	--destroy-->	EC_REF_FREE
 * EC_REF_C	--untrail-->	EC_REF_C
 * EC_REF_C_P	--destroy-->	EC_REF_FREE
 * EC_REF_C_P	--untrail-->	EC_REF_C
 *----------------------------------------------------------------------*/

void Winapi
ec_refs_destroy(ec_refs variable)
{
    if (!(variable->refstate & EC_REF_C) || variable->ref_ctr == 0)
	ec_panic("ec_ref already freed from C","ec_refs_destroy()");
    if (--variable->ref_ctr > 0)
        return;
    if (variable->refstate & EC_REF_P)
    {
	/* Unlink the ec_ref to make the global stack array become garbage */
	variable->next->prev = variable->prev;
	variable->prev->next = variable->next;
    }
    variable->refstate = EC_REF_FREE;
    hp_free_size(variable, sizeof(struct eclipse_ref_));
}

/*ARGSUSED*/
static void
_ec_refs_untrail(pword *parray, word *pdata, int size, int flags, ec_eng_t *ec_eng)
{
    ec_refs variable = ec_eng->allrefs.next;
    /* Find the ec_ref corresponding to parray in the global list. */
    /* If it's not in there, then it has already been destroyed! */
    while (variable != &ec_eng->allrefs)
    {
	if (variable->var.val.ptr == parray)
	{
	    if (!(variable->refstate == EC_REF_C_P))
		ec_panic("ec_ref already untrailed","_ec_refs_untrail()");
	    variable->refstate &= ~EC_REF_P;
	    variable->next->prev = variable->prev;	/* unlink */
	    variable->prev->next = variable->next;
	    variable->var = *((pword*) pdata);		/* reset value */
	    return;
	}
	variable = variable->next;
    }
}

int Winapi
ec_refs_size(const ec_refs variable)
{
    return variable->size;
}

ec_refs Winapi
ecl_refs_create_newvars(ec_eng_t *ec_eng, int n)
{
    ec_ref new;

    new = hp_alloc_size(sizeof(struct eclipse_ref_));
    new->var = ec_eng->allrefs.var;	/* a TREF|NULL (means: init as var) */
    new->refstate = EC_REF_C;
    new->size = n;
    new->next = new->prev = 0;
    new->eng = ec_eng;
    new->ref_ctr = 1;
    return new;
}

ec_refs Winapi
ecl_refs_create(ec_eng_t *ec_eng, int n, const pword initpw)
{
    ec_ref new;

    if (!(IsSimple(initpw.tag) || IsPersistent(initpw.tag)))
	    ec_panic("non-atomic initializer","ecl_refs_create()");
    new = hp_alloc_size(sizeof(struct eclipse_ref_));
    new->var = initpw;
    new->refstate = EC_REF_C;
    new->size = n;
    new->next = new->prev = 0;
    new->eng = ec_eng;
    new->ref_ctr = 1;
    return new;
}

ec_refs Winapi
ec_refs_copy(ec_refs variable)
{
    ++variable->ref_ctr;
    return variable;
}

static void
_ec_ref_init(ec_refs variable)
{
    pword * pw, initpw;
    int i;
    int n = variable->size;
    ec_eng_t *ec_eng = variable->eng;

    if (variable->refstate != EC_REF_C)
    	ec_panic("ec_refs already freed from C","_ec_ref_init()");

    initpw = variable->var;
    variable->refstate = EC_REF_C_P;

    /* Use the global stack array as trail item, so the trail entry */
    /* gets garbage collected together with it. */
    pw = TG;
    ecl_trail_undo(ec_eng, _ec_refs_untrail, pw, NULL,
	    (word *) &initpw, sizeof(pword)/sizeof(word), TRAILED_PWORD);

    Make_Struct(&(variable->var), pw);
    Push_Struct_Frame(ec_did("",n));
    if (IsRef(initpw.tag))
    {
	for (i=1; i<=n; i++)
	{ /* brackets important */
	    Make_Var(pw+i);
    	}
    }
    else
    {
	for (i=1; i<=n; i++)
	    pw[i] = initpw;
    }
    variable->next = ec_eng->allrefs.next;
    variable->prev = &ec_eng->allrefs;
    ec_eng->allrefs.next->prev = variable;
    ec_eng->allrefs.next = variable;
}

void Winapi
ec_refs_set(ec_refs variable, int i, const pword w)
{
    if (variable->refstate != EC_REF_C_P)
	_ec_ref_init(variable);
    if (i >= variable->size)
	ec_panic("out of bounds","ec_refs_set()");

    (void) ecl_assign(variable->eng, variable->var.val.ptr+i+1, w.val,w.tag);
}

pword Winapi
ec_refs_get(const ec_refs variable, int i)
{
    if (variable->refstate != EC_REF_C_P)
	_ec_ref_init(variable);
    if (i >= variable->size)
	ec_panic("out of bounds","ec_refs_get()");

    return variable->var.val.ptr[i+1];
}

ec_eng_t* Winapi
ec_refs_get_engine(const ec_refs variable)
{
    return variable->eng;
}


ec_ref Winapi
ecl_ref_create(ec_eng_t *ec_eng, pword initpw)
{
    return (ec_ref) ecl_refs_create(ec_eng, 1, initpw);
}

ec_ref Winapi
ecl_ref_create_newvar(ec_eng_t *ec_eng)
{
    return (ec_ref) ecl_refs_create_newvars(ec_eng, 1);
}

ec_ref Winapi
ec_ref_copy(ec_ref variable)
{
    return (ec_ref) ec_refs_copy((ec_refs) variable);
}

void Winapi
ec_ref_set(ec_ref variable, const pword w)
{
    ec_refs_set((ec_refs) variable, 0, w);
}

void
ec_ref_set_safe(ec_ref variable, const pword w)
{
    if (!EngIsDead(variable->eng)) {
	ec_refs_set((ec_refs) variable, 0, w);
	return;
    }
    /* Allow a simple value to be returned via an ec_ref even when the
     * engine is already dead. Used for returning an integer exit/1 value.
     */
    if (!(IsSimple(w.tag) || IsPersistent(w.tag)))
	return;
    variable->var = w;
}

pword Winapi
ec_ref_get(const ec_ref variable)
{
    return ec_refs_get((const ec_refs) variable, 0);
}

ec_eng_t* Winapi
ec_ref_get_engine(const ec_ref variable)
{
    return ec_refs_get_engine((const ec_refs) variable);
}

void Winapi
ec_ref_destroy(ec_ref variable)
{
    ec_refs_destroy((ec_refs) variable);
}


/*----------------------------------------------------------------------
 * Choicepoints and cuts
 *----------------------------------------------------------------------*/

void Winapi
ec_cut_to_chp(ec_ref chp)
{
    ecl_post_goal(chp->eng, ecl_term(chp->eng, ec_.d.colon,
			ec_atom(ec_.d.kernel_sepia),
    			ecl_term(chp->eng, ec_.d.cut_to, ec_ref_get(chp))));
}


/*----------------------------------------------------------------------
 * C->Prolog and Prolog->C type conversions
 *----------------------------------------------------------------------*/

pword Winapi
ec_atom(const dident a)
{
    pword w;
    if (a == ec_.d.nil)
    {
    	Make_Nil(&w);
    }
    else
    {
	Make_Atom(&w,a);
    }
    return w;
}

int Winapi
ec_get_atom(const pword w, dident *a)
{
    const pword * pw = &w;
    Dereference_(pw);
    if (IsAtom(pw->tag))
	*a = pw->val.did;
    else if (IsNil(pw->tag))
	*a = ec_.d.nil;
    else if (IsRef(pw->tag))
	return INSTANTIATION_FAULT;
    else
	return TYPE_ERROR;
    return PSUCCEED;
}

pword Winapi
ecl_string(ec_eng_t *ec_eng, const char *s)
{
	pword w;
	Make_String(&w, (char *) s);
	return w;
}

pword Winapi
ecl_length_string(ec_eng_t *ec_eng, int l, const char *s)
{
	pword w;
	char *s1;
	w.tag.kernel = TSTRG;
	w.val.ptr = TG;
	Push_Buffer(l+1);
	s1 = (char *) BufferStart(w.val.ptr);
	Copy_Bytes(s1, (char *) s, l);
	s1[l] = 0;
	return w;
}

int Winapi
ec_get_string(const pword w, char **s)
{
    const pword *pw = &w;
    Dereference_(pw);

    if (IsString(pw->tag)) 
	*s = StringStart(pw->val);
    else if (IsAtom(pw->tag)) 
	*s = DidName(pw->val.did);
    else if (IsNil(pw->tag)) 
	*s = DidName(ec_.d.nil);
    else if (IsRef(pw->tag))
	return INSTANTIATION_FAULT;
    else
	return TYPE_ERROR;
    return PSUCCEED;
}

int Winapi
ec_get_string_length(const pword w, char **s, long int *l)
{
    const pword *pw = &w;
    Dereference_(pw);

    if (IsString(pw->tag)) 
    {
	*s = StringStart(pw->val);
	*l = StringLength(pw->val);
    }
    else if (IsAtom(pw->tag)) 
    {
	*s = DidName(pw->val.did);
	*l = DidLength(pw->val.did);
    }
    else if (IsNil(pw->tag)) 
    {
	*s = DidName(ec_.d.nil);
	*l = 2;
    }
    else if (IsRef(pw->tag))
	return INSTANTIATION_FAULT;
    else
	return TYPE_ERROR;
    return PSUCCEED;
}

pword Winapi
ec_long(const long int l)
{
	pword w;
	Make_Integer(&w,(word)l);
	return w;
}

int Winapi
ec_get_long(const pword w, long int *l)
{
    const pword *pw = &w;
    Dereference_(pw);

    if (IsInteger(pw->tag)) 
    {
#if SIZEOF_WORD > SIZEOF_LONG
	/* range error if val.nint is too large for long */
	if (pw->val.nint > LONG_MAX || pw->val.nint < LONG_MIN)
	    return RANGE_ERROR;
#endif
	*l = pw->val.nint;
    } else if (IsBignum(pw->tag)) 
	return RANGE_ERROR;
    else if (IsRef(pw->tag))
	return INSTANTIATION_FAULT;
    else
	return TYPE_ERROR;
    return PSUCCEED;
}

pword Winapi
ecl_double(ec_eng_t *ec_eng, const double d)
{
    pword result;

    Make_Double(&result, d);
    return result;
}

int Winapi
ec_get_double(const pword w, double *d)
{
    const pword *pw = &w;
    Dereference_(pw);

    if (IsDouble(pw->tag)) 
	*d = Dbl(pw->val);
    else if (IsInteger(pw->tag)) 
	*d = (double) pw->val.nint;
    else if (IsRef(pw->tag))
	return INSTANTIATION_FAULT;
    else
	return TYPE_ERROR;
    return PSUCCEED;
}


#if 0
pword
ec_term(dident functor, ...)
{
    va_list ap;
    int arity = DidArity(functor);
    pword * pw;
    pword result;
    int i;

    va_start(ap, functor);

    pw = TG;
    Push_Struct_Frame(functor);
    for (i=1 ; i <= arity ; i++)
	pw[i] = va_arg(ap,pword);
    va_end(ap);

    Make_Struct(&result,pw);
    return result;
}
#endif

pword
ecl_term(ec_eng_t *ec_eng, dident functor, ...)
{
    va_list ap;
    int arity = DidArity(functor);
    pword * pw;
    pword result;
    int i;

    va_start(ap, functor);

    pw = TG;
    Push_Struct_Frame(functor);
    for (i=1 ; i <= arity ; i++)
	pw[i] = va_arg(ap,pword);
    va_end(ap);

    Make_Struct(&result,pw);
    return result;
}


pword Winapi
ecl_term_array(ec_eng_t *ec_eng, const dident functor, const pword *args)
{
    int arity;
    pword * pw;
    pword result;

    arity = DidArity(functor);

    pw = TG;
    Make_Struct(&result,pw);
    Push_Struct_Frame(functor);
    pw++;
    
    while(arity--)
	*pw++ = *args++;

    return result;
}


pword Winapi
ecl_matrixofdouble(ec_eng_t *ec_eng, int n, int m, const double *darr)
{
    dident row_functor = enter_dict("[]", n);
    dident col_functor = enter_dict("[]", m);
    pword *rows, *col;
    pword result;
    int i,j;

    rows = TG;
    Push_Struct_Frame(row_functor);
    for(i=1; i<=n; ++i)
    {
	col = TG;
	Make_Struct(&rows[i], col);
	Push_Struct_Frame(col_functor);
	for(j=1; j<=m; ++j)
	{
	    Make_Double(&col[j], *darr++);
	}
    }
    Make_Struct(&result,rows);
    return result;
}

pword Winapi
ecl_arrayofdouble(ec_eng_t *ec_eng, int n, const double *darr)
{
    dident functor = enter_dict("[]", n);
    pword result;
    pword *row;
    int i;

    row = TG;
    Push_Struct_Frame(functor);
    for(i=1; i<=n; ++i)
    {
    	Make_Double(&row[i], *darr++)
    }
    Make_Struct(&result,row);
    return result;
}


pword Winapi
ecl_list(ec_eng_t *ec_eng, const pword head, const pword tail)
{
    pword * pw;
    pword result;

    pw = TG;
    Push_List_Frame();
    pw[0] = head;
    pw[1] = tail;
    
    Make_List(&result,pw);
    return result;
}

pword Winapi
ecl_listofdouble(ec_eng_t *ec_eng, int length, const double *array)
{
    pword result;
    pword *pw = &result;
    while (length-- > 0)
    {
	Make_List(pw,TG);
	pw = TG;
	Push_List_Frame();
	*pw++ = ecl_double(ec_eng, *array++);
    }
    Make_Nil(pw);
    return result;
}

pword Winapi
ecl_listoflong(ec_eng_t *ec_eng, int length, const long int *array)
{
    pword result;
    pword *pw = &result;
    while (length-- > 0)
    {
	Make_List(pw,TG);
	pw = TG;
	Push_List_Frame();
	*pw++ = ec_long(*array++);
    }
    Make_Nil(pw);
    return result;
}

pword Winapi
ecl_listofchar(ec_eng_t *ec_eng, int length, const char *array)
{
    pword result;
    pword *pw = &result;
    while (length-- > 0)
    {
	Make_List(pw,TG);
	pw = TG;
	Push_List_Frame();
	*pw++ = ec_long(*array++);
    }
    Make_Nil(pw);
    return result;
}

pword Winapi
ecl_listofrefs(ec_eng_t *ec_eng, ec_refs refs)
{
    pword result;
    pword *pw = &result;
    int length = refs->size;
    int i;

    if (refs->refstate != EC_REF_C_P)
	_ec_ref_init(refs);

    for (i=1; i<=length; i++)
    {
	Make_List(pw,TG);
	pw = TG;
	Push_List_Frame();
	*pw++ = refs->var.val.ptr[i];
    }
    Make_Nil(pw);
    return result;
}

int Winapi
ec_get_nil(const pword list)
{
    const pword * pw = &list;
    Dereference_(pw);
    return IsNil(pw->tag)? PSUCCEED: PFAIL;
}

int Winapi
ec_is_var(const pword w)
{
    const pword * pw = &w;
    Dereference_(pw);
    return IsRef(pw->tag)? PSUCCEED: PFAIL;
}

int  Winapi
ec_get_list(const pword list, pword *car, pword *cdr)
{
    const pword * pw = &list;
    Dereference_(pw);

    if (IsList(pw->tag))
    {
	*car = pw->val.ptr[0];
	*cdr = pw->val.ptr[1];
	return PSUCCEED;
    }
    else if (IsNil(pw->tag))
	return PFAIL;
    else if (IsRef(pw->tag))
    	return INSTANTIATION_FAULT;
    else
    	return TYPE_ERROR;
}

int Winapi
ec_get_arg(const int n, pword term, pword *arg)
{
    pword * pw = &term;
    Dereference_(pw);

    if (IsStructure(pw->tag))
	if (n < 1  ||  n > DidArity(pw->val.ptr->val.did))
	    return RANGE_ERROR;
	else
	    *arg = pw->val.ptr[n];
    else if (IsList(pw->tag))
	if (n < 1  ||  n > 2)
	    return RANGE_ERROR;
	else
	    *arg = pw->val.ptr[n-1];
    else if (IsRef(pw->tag))
    	return INSTANTIATION_FAULT;
    else
    	return TYPE_ERROR;
    return PSUCCEED;
}

int Winapi
ec_get_functor(const pword term, dident *d)
{
    const pword * pw = &term;
    Dereference_(pw);

    if (IsStructure(pw->tag))
	*d = pw->val.ptr->val.did;
    else if (IsList(pw->tag))
    	*d = ec_.d.list;
    else if (IsRef(pw->tag))
    	return INSTANTIATION_FAULT;
    else
    	return TYPE_ERROR;
    return PSUCCEED;
}

int Winapi
ec_arity(const pword term)
{
    const pword * pw = &term;
    Dereference_(pw);
    if (IsList(pw->tag))
    	return 2;

    if (IsStructure(pw->tag))
    	return DidArity(pw->val.ptr->val.did);

    return 0;
}

pword Winapi
ecl_newvar(ec_eng_t *ec_eng)
{
    pword * pw;

    pw = TG++;
    Make_Ref(pw,pw);
    return *pw;
    
}

pword Winapi
ec_nil(void)
{
	pword p;

	Make_Nil(&p);
	return p;
}
	
static void
ec_deref(pword *ppw)	/* dereference in place */
{
    if (IsRef(ppw->tag))
    {
	pword *ppw1 = ppw;
	Dereference_(ppw);
	*ppw1 = *ppw;
    }
}


int Winapi
ec_var_lookup(ec_ref vars, char *name, pword *var)
{
	pword list;
	pword pair;
	pword varname;

	list = ec_ref_get(vars);
	while (ec_deref(&list),IsList(list.tag))
	{
	    if ( PSUCCEED == ec_get_arg(1,list,&pair) &&
		(ec_deref(&pair), IsList(pair.tag)) &&
		PSUCCEED ==  ec_get_arg(1,pair,&varname) &&
		(ec_deref(&varname), IsAtom(varname.tag)) &&
		0 == strcmp(DidName(varname.val.did),name) )
	    {
			ec_get_arg(2,pair,var);
			return PSUCCEED;
	    }
	    else
	    {
		    if (PSUCCEED != ec_get_arg(2,list,&list))
		    	return PFAIL;
	    }
	}
	return PFAIL;
}


/*----------------------------------------------------------------------
 * Support for external C predicates
 *----------------------------------------------------------------------*/

int Winapi
ecl_unify(ec_eng_t *ec_eng, pword pw1, pword pw2)
{
    return ec_unify_(ec_eng, pw1.val, pw1.tag, pw2.val, pw2.tag, &MU);
}


int Winapi
ecl_unify_arg(ec_eng_t *ec_eng, int n, pword term)
{
    return ec_unify_(ec_eng, A[n].val, A[n].tag, term.val, term.tag, &MU);
}

int Winapi
ec_compare(pword pw1, pword pw2)
{
    pword *ppw1 =  &pw1;
    pword *ppw2 =  &pw2;
    Dereference_(ppw1);
    Dereference_(ppw2);
    return ec_compare_terms(ppw1->val, ppw1->tag, ppw2->val, ppw2->tag);
}

pword Winapi
ecl_arg(ec_eng_t *ec_eng, int n)
{
    return A[n];
}

int Winapi
ecl_schedule_suspensions(ec_eng_t *ec_eng, pword attr, int pos)
{
    Check_Structure(attr.tag);
    if (pos < 1 || pos > DidArity(attr.val.ptr[0].val.did))
    	return RANGE_ERROR;
    return ecl_schedule_susps(ec_eng, &(attr.val.ptr[pos]));
}

int Winapi
ec_visible_procedure(dident proc_did, pword module, void **pproc)
{
    int res;
    pri *proc = visible_procedure(proc_did, module.val.did, module.tag, 0, &res);
    if (!proc)
    {
	return res;
    }
    *pproc = (void*) proc;
    return PSUCCEED;
}


/*----------------------------------------------------------------------
 * Some predefined external data types
 *----------------------------------------------------------------------*/

/*
 * double []
 */

static pword
_double_arr_get(t_ext_ptr h, int i, ec_eng_t *ec_eng)
{
    return ecl_double(ec_eng, ((double*)h)[i]);
}

static int
_double_arr_set(t_ext_ptr h, int i, pword pw, ec_eng_t *ec_eng)
{
    return ec_get_double(pw, &((double*)h)[i]);
}

const t_ext_type ec_xt_double_arr = {
    0, 0, 0, 0, 0, 0, 0,
    _double_arr_get,
    _double_arr_set
};


/*
 * long []
 */

static pword
_long_arr_get(t_ext_ptr h, int i, ec_eng_t *ec_eng)
{
    return ec_long(((long*)h)[i]);
}

static int
_long_arr_set(t_ext_ptr h, int i, pword pw, ec_eng_t *ec_eng)
{
    return ec_get_long(pw, &((long*)h)[i]);
}

const t_ext_type ec_xt_long_arr = {
    0, 0, 0, 0, 0, 0, 0,
    _long_arr_get,
    _long_arr_set
};


/*
 * char []
 */

static pword
_char_arr_get(t_ext_ptr h, int i, ec_eng_t *ec_eng)
{
    return ec_long((long) ((char*)h)[i]);
}

static int
_char_arr_set(t_ext_ptr h, int i, pword pw, ec_eng_t *ec_eng)
{
    long l;
    int err = ec_get_long(pw, &l);
    if (err == PSUCCEED)
    	((char*) h)[i] = (char) l;
    return err;
}

static int
_char_arr_ss(t_ext_ptr h, int quoted)
{
    return strlen((char*) h) + (quoted? 2: 0);
}

static int
_char_arr_tos(t_ext_ptr h, char *buf, int quoted)
{
    char *dest = buf;
    char *src = (char*) h;
    if (quoted)
    {
	*dest++ = '"';
	while ((*dest++ = *src++))
	    ;
	*(dest-1) = '"';
	*dest++ = 0;
    }
    else
    {
	while ((*dest++ = *src++))
	    ;
    }
    return dest-buf-1;
}

const t_ext_type ec_xt_char_arr = {
    0, 0, 0,
    _char_arr_ss,
    _char_arr_tos,
    0, 0,
    _char_arr_get,
    _char_arr_set
};

