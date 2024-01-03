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
 * VERSION	$Id: read.h,v 1.4 2017/02/05 03:00:33 jschimpf Exp $
 */

/*
 * IDENTIFICATION		read.h
 *
 * DESCRIPTION	
 *	
 *
 * CONTENTS:
 *
 */


/*
 * values for parser options
 * CAUTION: these numbers are used in Prolog io.pl
 */

#define VARNAMES_PLEASE	1	/* create named variables */
#define LAYOUT_PLEASE	2	/* create annotated_term{} structures */
#define VARLIST_PLEASE	4	/* create variable_names list */
#define READVAR_PAIRS	8	/* create ['X'|X] instead of 'X'=X in varlist */


Extern int ec_read_term(ec_eng_t *ec_eng, stream_id nst, int options, pword *result,
			pword *varlist, int *has_macro, value vm, type tm);

Extern pword *transf_meta_in(ec_eng_t*, pword *pw, dident mod, int *err);
Extern pword *transf_meta_out(ec_eng_t*, value val, type tag, pword *top,
			dident mod, pword *presult);
Extern pword *trafo_term(ec_eng_t*, dident tr_did, int flags, dident mv, type mt, int *tr_flags);
Extern int do_trafo(ec_eng_t*, pword *term);

