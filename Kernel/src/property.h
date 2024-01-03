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
 * SEPIA INCLUDE FILE
 *
 * VERSION	$Id: property.h,v 1.5 2016/10/28 22:44:33 jschimpf Exp $
 */
 
/*************************************************************
 *
 *                 property.h
 *
 *************************************************************/
 
/* scope of properties, options for access */
#define VISIBLE_PROP		0
#define LOCAL_PROP		1
#define GLOBAL_PROP		2

/* successful return codes */
/* #define LOCAL_PROP		1 */
/* #define GLOBAL_PROP		2 */
#define PROP_SCOPE		3
#define NEW_PROP		4	/* bit-significant */

#define HIDING_PROP		3
#define PropVisibility(res)	((res)&PROP_SCOPE)


/* types of properties */
#define EVENT_PROP		1
#define HTABLE_PROP		2
#define GLOBVAR_PROP		3
#define ARRAY_PROP		4
#define IDB_PROP		5
#define MODULE_PROP		6
#define SYSCALL_PROP		7
#define STREAM_PROP		8
#define PREFIX_PROP		9
#define INFIX_PROP		10
#define POSTFIX_PROP		11
#define TRANS_PROP		12  /* must be consecutive, write macros odd */
#define WRITE_TRANS_PROP	13  /* some values used in kernel.pl !!!     */
#define GOAL_TRANS_PROP		14	/* UNUSED (now inline) */
#define WRITE_GOAL_TRANS_PROP	15
#define CLAUSE_TRANS_PROP	16
#define WRITE_CLAUSE_TRANS_PROP	17
#define SHELF_PROP		18
#define GLOBREF_PROP		19


#define TransfTermIn(tr)	((tr) + 4)
#define TransfTermOut(tr)	((tr) + 5)

#define GlobalPrologRefIndexTag	TGRS
#define IsGlobalPrologRefIndex(p) SameTypeC((p)->tag, GlobalPrologRefIndexTag)

#define GlobalPrologRefTag   TGRL
#define IsGlobalPrologRef(p) SameTypeC((p)->tag, GlobalPrologRefTag)


Extern int	get_property_ref(dident, int, dident, type, int, pword**);
Extern int	swap_property(dident, int, dident, type, int, pword*, pword*);

Extern int	get_visible_property(dident functor, int property_name, dident module, type mod_tag, pword *result);
Extern int	get_visible_property_handle(dident functor, int property_name, dident module, type mod_tag, const t_ext_type *class, t_ext_ptr *result);
Extern pword	visible_property(dident functor, int property_name, dident module, type mod_tag, int *res);

Extern int	get_global_property(dident, int, pword*);
Extern int	set_global_property(dident, int, pword*);
Extern int	swap_global_property(dident, int, pword*, pword*);
Extern pword	global_property(dident functor, int property_name);

Extern int	erase_global_property(dident, int);
Extern int	erase_property(dident, int, dident, type, int);
Extern void	erase_module_props(module_item*);
Extern void	erase_records(pword*);
Extern void	mark_dids_from_properties(dident);

