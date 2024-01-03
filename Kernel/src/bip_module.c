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
 * VERSION	$Id: bip_module.c,v 1.19 2017/09/01 03:05:09 jschimpf Exp $
 */

/*
 *	File:	bip_module.c
 *	Author: dominic
 */

#include 	"config.h"
#include        "sepia.h"
#include        "types.h"
#include        "embed.h"
#include        "mem.h"
#include        "error.h"
#include        "ec_io.h"
#include	"dict.h"
#include	"database.h"
#include	"emu_export.h"
#include	"debug.h"
#include	"gencode.h"
#include	"module.h"
#include	"opcode.h"
#include	"property.h"
#include	"os_support.h"

extern syntax_desc
    *copy_syntax_desc(syntax_desc *sd);


/*
	tool_body_(Name1/Arity1, Name2/Arity2, BodyModule, SourceModule)
	returns the body procedure to a specified tool procedure
*/

static int
_tool_body(pri *proci, dident *pdid, int *parity, dident *pmodule, ec_eng_t *ec_eng)
{
    pri		*procb;
    int		flags;
    vmcode	*code;

    flags = proci->flags;
    code = proci->code.vmc;

    if (!(flags & CODE_DEFINED))
	return (flags & AUTOLOAD) ? NOT_LOADED : NOENTRY;

    if (!(flags & TOOL))
	return NO_TOOL;

    if (PriCodeType(proci) == VMCODE)
    {
	if (DebugProc(proci))
	    procb = (pri *) *(code + DEBUG_LENGTH + 1);
	else
	    procb = (pri *) *(code + 1);
	*pdid = procb->did;
	*parity = DidArity(procb->did);
	*pmodule = procb->module_def;
    }
    else /* don't know how to get the tool body */
    {
	return NO_TOOL;
    }
    return PSUCCEED;
}

static int
p_tool_body(value vi, type ti, value vb, type tb, value vmb, type tmb, value vm, type tm, ec_eng_t *ec_eng)
{
	dident	di;
	pri	*procb, *proci;
	int	flags, arity;
	dident	module;
	dident	pdid;
	pword	*ptr = TG;
	vmcode	*code;
	int	err;
	Prepare_Requests;

	Check_Module(tm, vm);
	Get_Proc_Did(vi, ti, di);
	if (!IsRef(tb)
	    && (!IsStructure(tb)
		|| vb.ptr->val.did != d_.quotient))
	{
	    Bip_Error(TYPE_ERROR);
	}
	Check_Output_Atom_Or_Nil(vmb, tmb);
	if (!(proci = visible_procedure(di, vm.did, tm, PRI_CREATE, &err)))
	{
	    Bip_Error(err);
	}

	err = _tool_body(proci, &pdid, &arity, &module, ec_eng);
	Return_If_Error(err);

	TG += 3;
	Check_Gc;
	ptr[0].tag.kernel = TDICT;
	ptr[0].val.did = d_.quotient;
	ptr[1].tag.kernel = TDICT;
	ptr[1].val.did = add_dict(pdid, 0);
	ptr[2].tag.kernel = TINT;
	ptr[2].val.nint = arity;

	Request_Unify_Atom(vmb, tmb, module);
	Request_Unify_Structure(vb, tb, ptr);
	Return_Unify;
}


/*******************************************************************
 *
 *	The functions to handle modules :
 *
 *	create_module/1
 *	erase_module/1
 *	lock/1			tool body of lock/0 + backward comp.
 *	lock/2			backward compatibility
 *	lock_pass_/2		tool body of lock_pass/1
 *	unlock/2
 *
 ******************************************************************* */

int
ec_create_module(dident module_did)
{
    pword		*prop;
    module_item		*m;
    int			res;

    mt_mutex_lock(&PropertyLock);
    res = get_property_ref(module_did, MODULE_PROP, d_.nil, tdict, GLOBAL_PROP, &prop);
    if (!(res & NEW_PROP)) {
	mt_mutex_unlock(&PropertyLock);
	Bip_Error(MODULE_EXISTS);
    }

    m = (module_item *) hg_alloc(sizeof(module_item));
    m->syntax = copy_syntax_desc(default_syntax);
    m->lock = NULL;
    m->procedures = NULL;
    m->properties = NULL;
    m->imports = NULL;

    prop->tag.kernel = TPTR;
    prop->val.ptr = (pword *) m;

    DidModule(module_did) = UNLOCK_MODULE;

    mt_mutex_unlock(&PropertyLock);
    Succeed_
}


static int
p_create_module(value v, type t, ec_eng_t *ec_eng)
{
    Check_Atom(t);	/* don't allow TNIL because of ModuleTag() problem */
    return ec_create_module(v.did);
}


static int
p_begin_module(value v, type t, ec_eng_t *ec_eng)
{
    Check_Module_And_Access(v, t);
    Succeed_;
}


/**
 * Set/Get the engine's context module for resumed goals
 */
static int
p_default_module(value v, type t, ec_eng_t *ec_eng)
{
#if 0
    if (IsRef(t)) {
#endif
	pword pw;
	pw.val.did = ec_eng->default_module;
	pw.tag.kernel = ModuleTag(d_.default_module);
        Return_Unify_Pw(v, t, pw.val, pw.tag);
#if 0
    }
    Check_Module_And_Access(v, t);
    ec_eng->default_module = v.did;
    Succeed_;
#endif
}


static int
p_lock1(value v, type t, ec_eng_t *ec_eng)
{
    Check_Module_And_Access(v, t);
    DidModule(v.did) = HARD_LOCK_MODULE;
    Succeed_;
}


static int
p_lock_pass_(value vl, type tl, value v, type t, ec_eng_t *ec_eng)
{
   module_item	*m;

   Check_Module_And_Access(v, t);
   Check_String(tl);

   DidModule(v.did) = SOFT_LOCK_MODULE;
   m = ModuleItem(v.did);
   /* the string should be stored crypted */
   m->lock = (char *) hg_alloc((int) StringLength(vl) + 1);
   Copy_Bytes(m->lock, StringStart(vl), StringLength(vl) + 1);

   Succeed_;
}


static int
p_lock2(value v, type t, value vl, type tl, ec_eng_t *ec_eng)
{
    return p_lock_pass_(vl, tl, v, t, ec_eng);
}


static int
p_unlock2(value v, type t, value vl, type tl, ec_eng_t *ec_eng)
{
   module_item	*m;

   Check_Atom_Or_Nil(v, t);
   Check_String(tl);

   if (!IsModule(v.did))
   {
       Bip_Error(MODULENAME);
   }
   if (!IsLocked(v.did))
   {
       Succeed_;
   }
   if (DidModule(v.did) == HARD_LOCK_MODULE)
   {
       Bip_Error(LOCKED);
   }
   m = ModuleItem(v.did);
   if (!strcmp(m->lock, StringStart(vl)))
   {
       hg_free(m->lock);
       DidModule(v.did) = UNLOCK_MODULE;
       m->lock = (char *) 0;
       Succeed_;
   }
   else
   {
       Bip_Error(WRONG_UNLOCK_STRING);
   }
}


static int
p_is_module(value v, type t, ec_eng_t *ec_eng)
{
    Check_Atom_Or_Nil(v, t);
    Succeed_If(IsModule(v.did));
}


static int
p_authorized_module(value v, type t, ec_eng_t *ec_eng)
{
    Check_Atom_Or_Nil(v, t);
    Succeed_If(IsModule(v.did) && (!IsLocked(v.did) || IsModuleTag(v.did, t)));
}


static int
p_is_locked(value v, type t, ec_eng_t *ec_eng)
{
    Check_Atom_Or_Nil(v, t);

    if (!IsModule(v.did))
    {
        Bip_Error(MODULENAME)
    }
    if (IsLocked(v.did))
    {
        Succeed_;
    }
    else
    {
        Fail_;
    }
}


/*******************************************************************
 *
 *			Properties functions
 *
 ******************************************************************* */


/*
	pr(Name/Arity)
	prints on the current_output the properties of a predicate
	in all modules.
*/
static int
p_pr(value v, type t, ec_eng_t *ec_eng)
{
    pri		*proc;
    dident	wdid;
    dident	module;
    int		flags;
    int		yes = 0;
    
    Get_Proc_Did(v, t, wdid);
    proc = wdid->procedure;
    
    while (proc)
    {
	module = proc->module_def;
	if (!module
#ifndef PRINTAM
	    || (IsLocked(module) && !PriExported(proc))
#endif
	    )
	{
	    proc = proc->nextproc;
	    continue;
	}
	
	yes = 1;
	p_fprintf(log_output_, "in %s: ", DidName(module));
	if (SystemProc(proc))
	    p_fprintf(log_output_, "system ");
	if (proc->flags & AUTOLOAD)
	    (void) ec_outfs(log_output_, "autoload ");
	if (proc->flags & PROC_DYNAMIC) {
	    (void) ec_outfs(log_output_, "dynamic ");
	} else {
	    (void) ec_outfs(log_output_, "static ");
	}
	switch(proc->flags & CODETYPE) {
	case VMCODE:
	    (void) ec_outfs(log_output_, "vmcode ");
	    break;
	case FUNPTR:
	    (void) ec_outfs(log_output_, "funptr ");
	    break;
	default:
	    (void) ec_outfs(log_output_, "code? ");
	    break;
	}
	switch(proc->flags & ARGPASSING) {
	case ARGFIXEDWAM:
	    (void) ec_outfs(log_output_, "argfixedwam ");
	    break;
	case ARGFLEXWAM:
	    (void) ec_outfs(log_output_, "argflexwam ");
	    break;
	default:
	    (void) ec_outfs(log_output_, "? ");
	    break;
	}
	if (proc->flags & EXTERN)
	{
	    (void) ec_outfs(log_output_, "external");
	    switch(proc->flags & UNIFTYPE) {
	    case U_NONE:
		(void) ec_outfs(log_output_, "_u_none ");
		break;
	    case U_SIMPLE:
		(void) ec_outfs(log_output_, "_u_simple ");
		break;
	    case U_GROUND:
		(void) ec_outfs(log_output_, "_u_ground ");
		break;
	    case U_UNIFY:	/* equal to fresh */
		(void) ec_outfs(log_output_, "_u_unify ");
		break;
	    case U_GLOBAL:
		(void) ec_outfs(log_output_, "_u_global ");
		break;
	    case U_DELAY:
		(void) ec_outfs(log_output_, "_u_delay ");
		break;
	    default:
		(void) ec_outfs(log_output_, "_u_? ");
		break;
	    }
	}
	else
	{
	    (void) ec_outfs(log_output_, "prolog ");
	}
	flags = proc->flags;
	if (flags & TOOL)
	    (void) ec_outfs(log_output_, "tool ");
	switch (PriScope(proc))
	{
	case EXPORT:
	    (void) ec_outfs(log_output_, "exported "); break;
	case LOCAL:
	    (void) ec_outfs(log_output_, "local "); break;
	case IMPORT:
	    (void) ec_outfs(log_output_, "imported "); break;
	case DEFAULT:
	    (void) ec_outfs(log_output_, "default "); break;
	case QUALI:
	    (void) ec_outfs(log_output_, "qualified "); break;
	}
	p_fprintf(log_output_, "%s ", DidName(proc->module_ref));
	
	if (flags & DEBUG_DB)
	    (void) ec_outfs(log_output_, "debugged ");
	if (flags & DEBUG_ST)
	    (void) ec_outfs(log_output_, "start_tracing ");
	if (flags & DEBUG_TR)
	    (void) ec_outfs(log_output_, "traceable ");
	else
	    (void) ec_outfs(log_output_, "untraceable ");
	if (flags & DEBUG_SP)
	    (void) ec_outfs(log_output_, "spied ");
	if (flags & DEBUG_SK)
	    (void) ec_outfs(log_output_, "skipped ");
	if (!PriReferenced(proc))
	    (void) ec_outfs(log_output_, "non_referenced ");
	
	if (flags & CODE_DEFINED)
	    (void) ec_outfs(log_output_, "code_defined ");
	proc = proc->nextproc;
	(void) ec_outfs(log_output_, "\n");
    }
    if (yes)
    {
	Succeed_;
    }
    else
    {
	Fail_;
    }
}


/* 	**************************************************************
 *		DECLARATIONS
 *	************************************************************** */

/*
	_tool_code(proc, debug)
	- makes the code for a tool interface
*/
static vmcode *
_tool_code(pri *procb, int debug)
{
    vmcode	*code;
    vmcode	*save;

    if (PriCodeType(procb) & VMCODE)
    {
	Allocate_Default_Procedure(3 + (debug?DEBUG_LENGTH:0), PriDid(procb));
	save = code;
	if (debug) {
	    Store_3(Debug_call, procb, CALL_PORT|FIRST_CALL|LAST_CALL);
	    Store_4d(d_.empty,0,0,0);
	}
	Store_i(JmpdP);
	Store_d(procb);
	Store_i(Code_end);
	return save;
    }
    else
    {
	return procb->code.vmc;		/* use the body's code */
    }
}


/*
	tool_(Name/Arity, SourceModule)
	set the tool flag of Name/Arity in SourceModule.
*/
static int
p_tool1(value vi, type ti, value vm, type tm, ec_eng_t *ec_eng)
{
#if 0
    dident	di;
    pri		*proci, *pd;
    int		err;

    Check_Module(tm, vm);
    Get_Proc_Did(vi, ti, di);

    proci = visible_procedure(di, vm.did, tm, PRI_CREATE, &err);
    if (!proci)
    {
	Bip_Error(err);
    }
    if (proci->flags & TOOL)
    {
	Succeed_;
    }
    err = pri_compatible_flags(proci, TOOL, TOOL);
    if (err != PSUCCEED)
    {
	Bip_Error(err);
    }
    pri_change_flags(proci, TOOL, TOOL);
    if (PriCodeType(proci) == VMCODE)
    {
	/* keep the old code, e.g. autoload_code... */
	/* update the code header, important for saving the arguments
	 * in the event mechanism */
	Incr_Code_Arity(PriCode(proci));
    }
    Succeed_;
#else
    Bip_Error(NOT_IMPLEMENTED);
#endif
}


#define TOOL_INHERIT_FLAGS (CODETYPE|ARGPASSING|EXTERN|UNIFTYPE)

static int
p_tool2(value vi, type ti, value vb, type tb, value vm, type tm, ec_eng_t *ec_eng)
{
    dident	di, db;
    pri		*procb, *proci;
    uint32_t	changed_flags, new_flags;
    pri_code_t	pricode;
    int		err;

    Check_Module(tm, vm);
    Get_Proc_Did(vi, ti, di);
    Get_Proc_Did(vb, tb, db);

    if (DidArity(di) + 1 != DidArity(db))
    {
        Bip_Error(RANGE_ERROR);
    }
    if (vm.did == d_.kernel_sepia)
	proci = export_procedure(di, vm.did, tm, &err);
    else
	proci = local_procedure(di, vm.did, tm, PRI_CREATE, &err);
    if (!proci)
    {
	Bip_Error(err);
    }
    procb = visible_procedure(db, vm.did, tm, PRI_CREATE, &err);
    if (!procb)
    {
	Bip_Error(err);
    }
    /* Incompatbilities of being a TOOL */
    if (DynamicProc(proci))
    {
	Bip_Error(INCONSISTENCY);
    }
    /* Incompatbilities of being a tool body */
    if (PriFlags(procb) & TOOL)
    {
	Bip_Error(INCONSISTENCY);
    }
    changed_flags = TOOL|TOOL_INHERIT_FLAGS|DEBUG_DB|SYSTEM;
    new_flags = TOOL
		|(TOOL_INHERIT_FLAGS & procb->flags)
		|(EclGblFlags & DBGCOMP ? DEBUG_DB : 0)
		|(vm.did == d_.kernel_sepia ? SYSTEM : 0);
    err = pri_compatible_flags(proci, changed_flags, new_flags);
    if (err != PSUCCEED)
    {
	Bip_Error(err);
    }
    pri_change_flags(proci, changed_flags & ~CODETYPE, new_flags & ~CODETYPE);
    Pri_Set_Reference(procb);
    proci->mode = procb->mode;
    pricode.vmc = _tool_code(procb, EclGblFlags & DBGCOMP);
    pri_define_code(proci, procb->flags & CODETYPE, pricode);
    /* make sure the tool body is exported or reexported, so it can
     * be invoked with a qualified call with lookup module vm */
    if (!PriAnyExp(procb) && !PriWillExport(procb))
    {
	if (PriScope(procb) == IMPORT)
	    procb = reexport_procedure(db, vm.did, tm, PriHomeModule(procb), &err);
	else
	    procb = export_procedure(db, vm.did, tm, &err);
	if (!procb)
	{
	    Bip_Error(err);
	}
    }
    Succeed_;
}


/*********************************************************************
			V I S I B I L I T Y   C H A N G E
**********************************************************************/

/* The following builtins use the global error variable ! */
#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)


/*
 * Implicit local declaration,
 * used by the compiler to prepare for the subsequent definition of a predicate
 */

static int
p_implicit_local(value v, type t, value vm, type tm, ec_eng_t *ec_eng)
{
    dident	d;
    int	err;

    Check_Module(tm, vm);
    Get_Proc_Did(v, t, d);

    if (!local_procedure(d, vm.did, tm, PRI_CREATE, &err))
    {
	Bip_Error(err);
    }
    Succeed_;
}


static int
p_local(value v, type t, value vm, type tm, ec_eng_t *ec_eng)
{
    dident	d;
    pri	*proc;
    int	err;

    Check_Module(tm, vm);
    Get_Proc_Did(v, t, d);

    proc = local_procedure(d, vm.did, tm, PRI_CREATE|PRI_DONTWARN, &err);
    if (!proc)
    {
	Bip_Error(err);
    }
    Succeed_;
}

static int
p_export(value v, type t, value vm, type tm, ec_eng_t *ec_eng)
{
    dident	d;
    pri	*proc;
    int	err;

    Check_Module(tm, vm);
    Get_Proc_Did(v, t, d);

    proc = export_procedure(d, vm.did, tm, &err);
    if (!proc)
    {
	Bip_Error(err);
    }
    Succeed_;
}


static int
p_import_from(value vim, type tim, value v, type t, value vm, type tm, ec_eng_t *ec_eng)
{
    dident	 d;
    pri	*proc, *export;
    int	 err;

    Check_Atom_Or_Nil(vim, tim);
    Check_Module(tm, vm);
    Get_Proc_Did(v, t, d);

    proc = import_procedure(d, vm.did, tm, vim.did, &err);
    if (!proc)
    {
	Bip_Error(err);
    }
    Succeed_;
}


static int
p_reexport_from(value vim, type tim, value v, type t, value vm, type tm, ec_eng_t *ec_eng)
{
    dident	 d;
    pri	*proc, *export;
    int	 err;

    Check_Atom_Or_Nil(vim, tim);
    Check_Module(tm, vm);
    Get_Proc_Did(v, t, d);

    proc = reexport_procedure(d, vm.did, tm, vim.did, &err);
    if (!proc)
    {
	Bip_Error(err);
    }
    Succeed_;
}


/*
  import_(+Lib, +Import_mod)
  Put Library in the 'imports' list of Import_mod
*/

/*ARGSUSED*/
static int
p_import(value library, type tlib, value import_mod, type tim, ec_eng_t *ec_eng)
{
    Check_Module_And_Access(import_mod, tim);
    Check_Module(tlib, library);
    return import_whole_module(library.did, import_mod.did);
}


static int
p_erase_module(value module, type module_tag, value from_mod, type tfrom_mod, ec_eng_t *ec_eng)
{
	module_item	*pm, *import_pm;
	int		 i;
	didlist		*lib_scan;
	pword		*prop;

	Check_Module(tfrom_mod, from_mod);

	Check_Atom_Or_Nil(module, module_tag);
	if (!IsModule(module.did))
	{
	    Succeed_;
	} else if (IsLocked(module.did)
		&& (from_mod.did != d_.kernel_sepia
			|| !IsModuleTag(from_mod.did, tfrom_mod)))
	{
	    Bip_Error(LOCKED);
	}

	pm = ModuleItem(module.did);

	/* first, clean the procedures, we can reclaim the space	*/
	erase_module_procs(pm);

	/* reclaim the properties					*/
	erase_module_props(pm);

	/* reclaim module descriptor */
	mt_mutex_lock(&PropertyLock);
	hg_free_size(pm->syntax, sizeof(syntax_desc));
	module.did->module = 0;
	(void) erase_global_property(module.did, MODULE_PROP);
	mt_mutex_unlock(&PropertyLock);

	Succeed_;
}

/*
 * Return a safe module for use in system predicates.
 */
/*ARGSUSED*/
static int
p_module_tag(value vm, type tm, value vs, type ts, ec_eng_t *ec_eng)
{
    type	t;

    t.kernel = ModuleTag(vm.did);
    Return_Unify_Pw(vs, ts, vm, t)
}


void
module_init(int flags)
{
   if (flags & INIT_SHARED)
   {
       ec_create_module(d_.kernel_sepia);	/* the kernel module */
       ec_create_module(d_.dummy_module);	/* a nameless module */

#ifdef DFID
       ec_create_module(in_dict("dfid", 0));	/* to initialize global vars */
#endif

       AbolishedProcedures = 0;
       AbolishedProcedures = 0;
   }
}


void
bip_module_init(int flags)
{
    if (!(flags & INIT_SHARED))
	return;
    (void) local_built_in(in_dict("erase_module_", 2), p_erase_module, B_SAFE);
    (void) local_built_in(in_dict("is_a_module", 1), p_is_module, B_SAFE);
    (void) local_built_in(in_dict("authorized_module", 1), p_authorized_module, B_SAFE);
    (void) built_in(in_dict("is_locked", 1), p_is_locked, B_SAFE);
    (void) built_in(in_dict("begin_module", 1), p_begin_module, B_SAFE);
    (void) local_built_in(in_dict("begin_module", 2), p_begin_module, B_SAFE);
    (void) local_built_in(in_dict("create_module_", 1), p_create_module, B_SAFE);
    (void) built_in(d_.lock, p_lock1, B_SAFE);
    (void) built_in(in_dict("lock", 2), p_lock2, B_SAFE);
    (void) built_in(in_dict("lock_pass_", 2), p_lock_pass_, B_SAFE);
    (void) built_in(in_dict("unlock", 2), p_unlock2, B_SAFE);
    (void) exported_built_in(in_dict("tool_", 2), p_tool1, B_UNSAFE);
    (void) exported_built_in(in_dict("tool_", 3), p_tool2, B_UNSAFE);
    exported_built_in(in_dict("tool_body_", 4), p_tool_body, B_UNSAFE|U_GROUND)
	-> mode = BoundArg(2, GROUND) | BoundArg(3, CONSTANT);
    (void) local_built_in(d_.localb, p_local, B_UNSAFE);
    (void) exported_built_in(in_dict("implicit_local",2), p_implicit_local, B_UNSAFE);
    (void) local_built_in(d_.exportb, p_export, B_UNSAFE);
    (void) local_built_in(in_dict("reexport_from_",3), p_reexport_from, B_UNSAFE);
    (void) local_built_in(d_.import_fromb, p_import_from, B_UNSAFE);
    (void) local_built_in(in_dict("import_", 2), p_import, B_UNSAFE);
    (void) local_built_in(in_dict("module_tag", 2), p_module_tag, B_UNSAFE);
    (void) exported_built_in(in_dict("default_module", 1), p_default_module,
    	B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("pr", 1), p_pr, B_SAFE);

}

/* Add all new code in front of the initialization function! */
