/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipseclp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe/Gurobi Interface
 * The Initial Developer of the Original Code is Joachim Schimpf
 * Portions created by the Initial Developer are
 * Copyright (C) 2012 Joachim Schimpf
 * 
 * Contributor(s): Joachim Schimpf, Coninfer Ltd
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe/Gurobi interface (for inclusion in eplex.c)
 */

#include "gurobi_c.h"

#define CPXENVptr			GRBenv*
#define CPXLPptr			GRBmodel*

typedef char* param_id_t;
typedef int sostype_t;
typedef int direction_t;

#include "eplex.h"	/* needs declarations above! */



/*
 * Capabilities
 */
#undef HAS_QUADRATIC
#undef HAS_MIQP
#define HAS_LIMITED_MIP_RESULTS
#define SOLVER_HAS_LOCAL_PARAMETERS
#define SOLVER_HAS_STR_PARAMS

/*
 * Constants
 */
#define SOLVER_SHORT_NAME	GRB
#define SOLVER_ATOMIC_NAME	"gurobi"
#define SOLVER_VERSION_INT	(100*GRB_VERSION_MAJOR+GRB_VERSION_MINOR)

#define CPXPUBLIC

#define CPX_INFBOUND		GRB_INFINITY

#define SOLVER_SENSE_LE		'<'
#define SOLVER_SENSE_GE		'>'
#define SOLVER_SENSE_EQ		'='
	
#define SOLVER_SOS_TYPE1	1
#define SOLVER_SOS_TYPE2	2

#define SOLVER_HAS_LOADORDER
#define SOLVER_DIR_DOWN	        0
#define SOLVER_DIR_UP           0
#define SOLVER_DIR_DEFAULT	0

#define CPX_COL_AT_LOWER	GRB_NONBASIC_LOWER
#define CPX_COL_AT_UPPER	GRB_NONBASIC_UPPER
#define CPX_COL_BASIC		GBB_BASIC
#define CPX_COL_FREE_SUPER	GRB_SUPERBASIC

#define STRBUFFERSIZE		GRB_MAX_STRLEN


#define Update_Model(LP)		GRBupdatemodel(LP)

/*
 * Other operations
 */
#define SetPreSolve(state)

#define Report_Error(Msg) \
	(void) ec_outfs(solver_streams[ErrType], "Gurobi error: "); \
	(void) ec_outfs(solver_streams[ErrType], Msg); \
	(void) ec_newline(solver_streams[ErrType]);

#define Report_Solver_Error(E)		Report_Error(GRBgeterrormsg(E))
