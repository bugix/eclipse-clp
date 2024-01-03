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
 * The Original Code is  The ECLiPSe/GLPK Interface
 * The Initial Developer of the Original Code is Joachim Schimpf
 * Portions created by the Initial Developer are
 * Copyright (C) 2019 Joachim Schimpf
 * 
 * Contributor(s): Joachim Schimpf, Coninfer Ltd
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe/Gurobi interface (for inclusion in eplex.c)
 */

#include "glpk.h"

#define CPXENVptr			glp_env*
#define CPXLPptr			glp_prob*

typedef int param_id_t;
typedef int sostype_t;

typedef struct {
    glp_smcp	sm;     /* simplex parameters */
    glp_iocp	io;     /* integer optimizer parameters */
    glp_iptcp	ip;     /* interior point parameters */
    int node_limit;     /* simulated parameters */
    int msg_lev;
    int out_frq;
    int out_dly;
} glp_env;


#include "eplex.h"	/* needs declarations above! */


/*
 * Capabilities
 */
#define HAS_LIMITED_MIP_RESULTS
#define SOLVER_HAS_LOCAL_PARAMETERS
#undef SOLVER_HAS_STR_PARAMS
#undef HAS_QUADRATIC
#undef HAS_MIQP
#undef HAS_CONCURRENT
#undef HAS_INDICATOR_CONSTRAINTS


/*
 * Constants
 */
#define SOLVER_SHORT_NAME	GLPK
#define SOLVER_ATOMIC_NAME	"glpk"
#define SOLVER_VERSION_INT	(100*GLP_MAJOR_VERSION+GLP_MINOR_VERSION)

#define CPXPUBLIC

#define CPX_INFBOUND		DBL_MAX /* finite */
#define INTS_NEED_INT_BOUNDS    /* integer variables must have integral bounds */

#undef IDX_OFFSET
#define IDX_OFFSET              1       /* solver's first row/col index */

#define SOLVER_SENSE_LE		GLP_UP
#define SOLVER_SENSE_GE		GLP_LO
#define SOLVER_SENSE_EQ		GLP_FX
	
#define SOLVER_SOS_TYPE1	1
#define SOLVER_SOS_TYPE2	2

#define CPX_COL_AT_LOWER	GLP_NL
#define CPX_COL_AT_UPPER	GLP_NU
#define CPX_COL_BASIC		GLP_BS
#define CPX_COL_FREE_SUPER	GLP_NF

#define STRBUFFERSIZE		256


#define Update_Model(LP)
#define SetPreSolve(state)

