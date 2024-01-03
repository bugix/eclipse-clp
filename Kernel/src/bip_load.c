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
 * VERSION	$Id: bip_load.c,v 1.7 2017/09/01 03:05:09 jschimpf Exp $
 */

/****************************************************************************
 *
 *		SEPIA Built-in Predicates for dynamic loading
 *
 *
 *****************************************************************************/

#include "config.h"

#ifdef _WIN32
#else
#include <sys/types.h>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#endif

#ifdef STDC_HEADERS
#include	<stdlib.h>
#else
extern char *getenv();
#endif

#ifdef HAVE_STRING_H
#include	<string.h>
#else
extern char	*strcpy();
#endif

#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "mem.h"
#include "dict.h"
#include "emu_export.h"
#include "error.h"
#include "opcode.h"
#include "ec_io.h"
#include "property.h"
#include "module.h"
#include "os_support.h"

#ifdef SBRK_UNDEF
extern char	*sbrk();
#endif

#if defined(HAVE_DLOPEN) || defined(_WIN32) || defined(HAVE_MACH_O_DYLD_H)
#define D_DEF
#endif

#if defined(HAVE_DLOPEN) || defined(_WIN32) || defined(D_LOAD) || defined(HAVE_MACH_O_DYLD_H)
#  ifndef D_LOAD
#    define D_LOAD
#  endif
#endif

#if (defined(HAVE_DLOPEN) && !defined(sun4_0)) || defined(HAVE_MACH_O_DYLD_H)
# define OS_SUPPORTS_DL
#endif

#if defined(HAVE_DLOPEN) && !defined(sun4_0)
#  include <dlfcn.h>
#elif defined(HAVE_MACH_O_DYLD_H)
#  include "dlfcn_simple.h"
#else
#ifndef _WIN32
# if defined(D_LOAD) || defined(D_DEF)
#  include <sys/file.h>
#  include <a.out.h>

#  ifdef hpux
#    define N_TXTOFF(f, hr)	hr.exec_tfile
#    define TD_SIZE(hr)		(hr.exec_tsize + hr.exec_dsize)
#    define BS_SIZE(hr)		(hr.exec_bsize)
#    define FileHeader		header
#    define AoutHeader		som_exec_auxhdr
#  else
#  if defined(mips) || defined(__alpha)
#    define TD_SIZE(hr)		(hr.tsize + hr.dsize)
#    define BS_SIZE(hr)		(hr.bsize)
#    define FileHeader		filehdr
#    define AoutHeader		aouthdr
#  else
#    define TD_SIZE(hr)		(hr.a_text + hr.a_data)
#    define BS_SIZE(hr)		(hr.a_bss)
#    define AoutHeader		exec
#  endif
#  endif
# endif	/* D_LOAD || D_DEF */
#endif /* _WIN32 */
#endif /* HAVE_DLOPEN */


#if defined(D_LOAD) && defined(D_DEF)

/****************************************************************
 * Dynamic loading and related
 ****************************************************************/


/*
 * 	p_load()	dload(file + options)
 *	dynamic loading of an object file.
 *	MUCH system dependent
 */

#ifdef _WIN32

struct dload_info {
  HINSTANCE handle;
  struct dload_info *next;
};

static struct dload_info *dload_list = 0;

static int 
p_load(value v, type t, ec_eng_t *ec_eng)
{
    char *name;
    char buf1[MAX_PATH_LEN];
    char winname[MAX_PATH_LEN];
    HINSTANCE dloaded;
    struct dload_info *dli;

    Get_Name(v,t,name)			/* get the name of the file */
    /* Make an absolute pathname, needed on Windows 95 */
    name = expand_filename(name, buf1, EXPAND_ABSOLUTE);
    dloaded = LoadLibrary(os_filename(name, winname));
    if (!dloaded)
    {
	Bip_Error(SYS_ERROR_WIN);
    }
    dli = (struct dload_info *) hp_alloc_size(sizeof(struct dload_info));
    dli->handle = dloaded;
    dli->next = dload_list;
    dload_list = dli;
    Succeed_;
}

void
bip_load_fini(void)
{
    while (dload_list)
    {
	struct dload_info *dli = dload_list;
	dload_list = dli->next;
	(void) FreeLibrary(dli->handle);
	hp_free_size(dli, sizeof(struct dload_info));
    }
}

#else
#ifdef OS_SUPPORTS_DL

#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL	0
#endif
/*
 * We have operating system support for dynamic loading, which
 * makes things simpler. The object to be loaded must be a
 * shared object. Compile it with
 *
 *	cc -I... -G -o <name>.so name.c
 */

/*
 * Remember the loaded objects in dload_list, which will be used
 * by external/2 and symbol_address/2.
 */

struct dload_info {
  void *handle;
  struct dload_info *next;
};

static struct dload_info *dload_list = 0;


static int 
p_load(value v, type t, ec_eng_t *ec_eng)
{
    char buf1[MAX_PATH_LEN];
    char *name;
    void *dloaded;
    struct dload_info *dli;

    Get_Name(v,t,name)			/* get the name of the file */
    /* Make an absolute pathname because dlopen sometimes
     * seems to have a wrong idea of the cwd.
     */
    name = expand_filename(name, buf1, EXPAND_ABSOLUTE);
    dloaded = dlopen(name, RTLD_NOW|RTLD_GLOBAL);
    if (!dloaded)
    {
	ec_outfs(current_err_, dlerror()); ec_newline(current_err_);
	Bip_Error(NO_SHARED_LIB);
    }
    dli = (struct dload_info *) hp_alloc_size(sizeof(struct dload_info));
    dli->handle = dloaded;
    dli->next = dload_list;
    dload_list = dli;
    Succeed_;
}

void
bip_load_fini(void)
{
    while (dload_list)
    {
	struct dload_info *dli = dload_list;
	dload_list = dli->next;
	(void) dlclose(dli->handle);
	hp_free_size(dli, sizeof(struct dload_info));
    }
}

#else /*!OS_SUPPORTS_DL */

static void *dload_list = 0;

static int 
p_load(value v, type t, ec_eng_t *ec_eng)
{
    Bip_Error(UNIMPLEMENTED);
}

void
bip_load_fini()
{
}

#endif /* OS_SUPPORTS_DL */
#endif /* _WIN32 */

#else /* D_LOAD && D_DEF */
Not_Available_Built_In(p_load)
#endif /* D_LOAD && D_DEF */



#ifdef D_DEF

/****************************************************************
 * Dynamic definitions and related
 ****************************************************************/

/*
 *	ec_getaddress(function_name)
 *	fetch the address of a symbol from the symbol table
 *	returns -1 if it was not possible.
 */

#ifdef _WIN32

word
ec_getaddress(char *s)
{
    struct dload_info *dli;

    for (dli = dload_list; dli; dli = dli->next)
    {
	word addr = (word) GetProcAddress(dli->handle, s);
	if (addr)
	    return addr;
    }
    return (word) 0;
}

#else
#ifdef OS_SUPPORTS_DL

static void *myself = (void *) 0;

word
ec_getaddress(char *s)
{
    word addr = 0;

    if (!myself)
    {
	if (!(myself = dlopen((char *) 0, RTLD_LAZY)))
	{
	    return 0;
	}
    }
    addr = (word) dlsym(myself, s);
    if (!addr)
    {
	struct dload_info *dli;
	for (dli = dload_list; dli; dli = dli->next)
	{
	    addr = (word) dlsym(dli->handle, s);
	    if (addr)
		return addr;
	}
    }
    return addr;
}

#else

word
ec_getaddress(char *s)
{
    return 0;
}

#endif
#endif

/*
 *	p_call_c()	call_c(foo(a1,...an),Value)
 *	calls the function whose system name is foo after
 *	translating the arguments, and 
 *	unifies Value with the value returned by the function, taken as
 *	an integer.
 */

#define MAX_CALL_C_ARITY	10
static int
p_call_c(value v, type t, value vr, type tr, ec_eng_t *ec_eng)
{
    word foo, aux;
    int arity;
    pword *p, *pw;
    pword prop;
    value arg[MAX_CALL_C_ARITY];
    dident mydid;
    double	f;
    int		res_type;
    value	resv;
    type	rest;

    Error_If_Ref(t)
    if (IsStructure(tr)) {
	mydid = vr.ptr->val.did;
	if (mydid == d_.float1)
	    res_type = TDBL;
	else if (mydid == d_.integer)
	    res_type = TINT;
	else if (mydid == d_.string)
	    res_type = TSTRG;
	else {
	    Bip_Error(RANGE_ERROR)
	}
	resv.all = vr.ptr[1].val.all;
	rest.all = vr.ptr[1].tag.all;
    }
    else if (IsRef(tr) || IsInteger(tr)) {
	res_type = TINT;
	resv.all = vr.all;
	rest.all = tr.all;
    }
    else {
	Bip_Error(TYPE_ERROR)
    }
    if(IsStructure(t))
	mydid = v.ptr->val.did;
    else if(IsAtom(t))
	mydid = v.did;
    else
    {
	Bip_Error(TYPE_ERROR);
    }
    arity = DidArity(mydid);
    mydid = add_dict(mydid, 0);
    if (get_global_property(mydid, SYSCALL_PROP, &prop) == PSUCCEED)
    {
	assert(IsInteger(prop.tag));
	foo = prop.val.nint;
    }
    else
    {
	foo = ec_getaddress(DidName(mydid));
	if(!foo)
	{
	    Bip_Error(NOCODE);
	}
	Make_Integer(&prop, foo);
	set_global_property(mydid, SYSCALL_PROP, &prop);
    }
    aux = 0;
    				/* arguments translation */
    while(arity-- > 0)
    {
	p = ++(v.ptr);
	Dereference_(p)
	if(IsRef(p->tag))
	{
	    Bip_Error(TYPE_ERROR);
	}
	else
	{
	    switch (TagType(p->tag))
	    {
	    case TINT:
		arg[aux++] = p->val;
		break;

	    case TDBL:
		arg[aux++].nint = ((long *) &Dbl(p->val))[0];
		arg[aux++].nint = ((long *) &Dbl(p->val))[1];
		break;

	    case TSTRG:
		arg[aux++].str = StringStart(p->val);
		break;
	    case TDICT:
		arg[aux++].str = DidName(p->val.did);
		break;
	    case TCOMP:
		{
		    uword	kind, size;
		    int		err;
		    word	res;
		    type	tm;

		    tm.kernel = ModuleTag(d_.kernel_sepia);

		    p = p->val.ptr;
		    if(p->val.did == d_.quotient)
		    {
			res = get_first_elt(p+1, p+2, &kind, &size,
					    d_.kernel_sepia, tm);
			if (res < 0)
			{
			    Bip_Error(res);
			}
		    }
		    else
		    {
			value	v1;

			v1.all = (word) p;
			res = (word) get_elt_address(v1, tcomp, &kind,
						    d_.kernel_sepia, tm, &err);
			if (!res)
			{
			    Bip_Error(err);
			}
		    }
		    arg[aux++].nint = res;
		}
		break;

		default:
		    Bip_Error(TYPE_ERROR)
	    }
	}
    }
    if (res_type == TDBL)
	switch(aux) {
	    case 0: f =  (* (double (*)()) foo)();
		    break;
	    case 1: f =  (* (double (*)()) foo)(arg[0].nint);
		    break;
	    case 2: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint);
		    break;
	    case 3: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint);
		    break;
	    case 4: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint);
		    break;
	    case 5: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint,arg[4].nint);
		    break;
	    case 6: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint,arg[4].nint, arg[5].nint);
		    break;
	    case 7: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint,arg[4].nint, arg[5].nint,
			    arg[6].nint);
		    break;
	    case 8:
	    case 9:
	    case 10: f =  (* (double (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint,arg[3].nint,arg[4].nint, arg[5].nint,
			    arg[6].nint,arg[7].nint, arg[8].nint,arg[9].nint);
		    break;
	    default:
		Bip_Error(ARITY_LIMIT)
	}
    else
	switch(aux) {
	    case 0: aux =  (* (int (*)()) foo)();
		    break;
	    case 1: aux =  (* (int (*)()) foo)(arg[0].nint);
		    break;
	    case 2: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint);
		    break;
	    case 3: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint);
		    break;
	    case 4: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint);
		    break;
	    case 5: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint,arg[4].nint);
		    break;
	    case 6: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint,arg[4].nint, arg[5].nint);
		    break;
	    case 7: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint, arg[3].nint,arg[4].nint, arg[5].nint,
			    arg[6].nint);
		    break;
	    case 8:
	    case 9:
	    case 10: aux =  (* (int (*)()) foo)(arg[0].nint,arg[1].nint,
			    arg[2].nint,arg[3].nint,arg[4].nint, arg[5].nint,
			    arg[6].nint,arg[7].nint, arg[8].nint,arg[9].nint);
		    break;
	    default:
		Bip_Error(ARITY_LIMIT)
	}
    if (res_type == TINT) {
	Return_Unify_Integer(resv, rest, aux);
    } else if (res_type == TDBL) {
	Return_Unify_Float(resv, rest, f);
    }
    else /* if (res_type == TSTRG) */
    {
	value	sv;
	Cstring_To_Prolog((char *) aux, sv);
	Return_Unify_String(resv, rest, sv.ptr);
    }
}

static int
p_symbol_address(value vals, type tags, value vala, type taga, ec_eng_t *ec_eng)
{
	char	*name;
	word	symbol;

	Get_Name(vals, tags, name);
	Check_Output_Integer(taga);
	symbol = ec_getaddress(name);
	if (!symbol)
	{
		Fail_;
	}
	Return_Unify_Integer(vala, taga, symbol);
}

#else	/* D_DEF */
Not_Available_Built_In(p_symbol_address)
Not_Available_Built_In(p_call_c)
#endif /* D_DEF */



/*
 * Licence checking
 *
 * If there is a pteclipse.so library, we load it dynamically.
 * It contains proper definitions of licence_checkout/6 etc.
 * If there is no pteclipse.so, we use the dummies defined here.
 */

/*ARGSUSED*/
static int
p_licence_checkout(value vfeature, type tfeature, value vpol, type tpol, value vversion, type tversion, value vlicloc, type tlicloc, value vmsg, type tmsg, value vstat, type tstat, ec_eng_t *ec_eng)
{
    pword pw;
    Prepare_Requests;
    Make_String(&pw, "ECLiPSe licence check failed\n");
    Request_Unify_Pw(vmsg, tmsg, pw.val, pw.tag);
    Request_Unify_Atom(vstat, tstat, d_.err);
    Return_Unify;
}

/*ARGSUSED*/
static int
p_licence_held(value vfeature, type tfeature, ec_eng_t *ec_eng)
{
    Fail_;
}

/*ARGSUSED*/
static int
p_licence_checkin(value vfeature, type tfeature, ec_eng_t *ec_eng)
{
    Succeed_;
}

/*ARGSUSED*/
static int
p_licence_heartbeat(value vfeature, type tfeature, value vminutes, type tminutes, value vrec, type trec, value vfrec, type tfrec, ec_eng_t *ec_eng)
{
    Fail_;
}


static void
_pt_init(int flags)
{
    char pteclipse[MAX_PATH_LEN];

    /* these are the dummies - they may be replaced by pteclipse */
    (void) exported_built_in(in_dict("licence_checkout", 6), p_licence_checkout, B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("licence_checkin", 1), p_licence_checkin, B_SAFE);
    (void) exported_built_in(in_dict("licence_heartbeat", 4), p_licence_heartbeat, B_SAFE);
    (void) exported_built_in(in_dict("licence_held", 1), p_licence_held, B_SAFE);

    strcpy(pteclipse, ec_eclipse_home);	/* check for pteclipse lib */
    strcat(pteclipse, "/lib/");
    strcat(pteclipse, HOSTARCH);
    strcat(pteclipse, "/pteclipse.");
    strcat(pteclipse, OBJECT_SUFFIX_STRING);
    if (ec_access(pteclipse, R_OK) == 0)
    {
	pword pw;
	int (*init_fct)();

	Make_Atom(&pw, in_dict(pteclipse,0));	/* load it */
	if (p_load(pw.val, pw.tag, NULL) != PSUCCEED)
	    ec_panic("Can't load library file", pteclipse);

	init_fct = (int(*)()) ec_getaddress("pteclipse_init");
	if (!init_fct)
	    ec_panic("Library file corrupted", pteclipse);

	switch ((*init_fct)(flags))		/* initialise */
	{
	case PSUCCEED:
	    return;
	case PFAIL:
	    ec_panic("Licensing problem", "initialisation");
	    break;
	case UNIMPLEMENTED:
	default:
	    break;	/* pteclipse not available, keep the dummies */
	}
    }
}


/****************************************************************
 * Common Initialization and Finalization
 ****************************************************************/

void
bip_load_init(int flags)
{
    value	dummy_v1;

    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("load",1), p_load, B_SAFE);
	(void) exported_built_in(in_dict("symbol_address", 2),
				p_symbol_address,	B_UNSAFE|U_SIMPLE);
	built_in(in_dict("call_c",2), p_call_c, B_UNSAFE|U_SIMPLE)
		-> mode = BoundArg(2, CONSTANT);

	_pt_init(flags);
    }

    dload_list = 0;
#ifndef _WIN32
#ifdef OS_SUPPORTS_DL
    myself = 0;
#endif
#endif
}


