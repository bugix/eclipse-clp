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
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1995-2012 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, Kish Shen and Andrew Eremin, IC-Parc
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe/COIN interface (for inclusion in eplex.c)
 */

/* C_TO_COIN is defined when compiling for COIN, mapping the C calls in 
   eplex.c to the procedures in C++ coinplex.cpp. Also defined if
   compiling the logged calls
*/

#ifdef C_TO_COIN
/*  these void are for void pointers, as the type is COIN solver specific
    and defined in coinplex.cpp only
*/
# define OsiXxxSolverInterface 		void
# define COINprob 			void
#endif

# define CPXENVptr			COINprob*
# define CPXLPptr			COINprob*

typedef int param_id_t;
typedef char sostype_t;

#include "eplex.h"	/* needs declarations above! */


# define SOLVER_SHORT_NAME OSI
# define SOLVER_ATOMIC_NAME "osi"

# define Update_Model(LP)

# define UsingBarrierNoCrossOver(d) 0

/* use only where solve_state is defined and set by call to 
   coin_get_result_state()
*/
# define SuccessState(d) (solve_state == state_success)
# define FailState(d) (solve_state == state_fail)
# define MIPSemiSuccessState(d) (solve_state == state_mipsemisucc)
# define MIPSemiFailState(d) (solve_state == state_mipsemifail)
# define LPAbortedState(d) (solve_state == state_lpaborted)
# define UnboundedState(d) (solve_state == state_unbounded)
# define MaybeFailState(d) (solve_state == state_unknown)

# define DualMethod(lpd,m,am) 0

# define SetPreSolve(state)

#define SOLVER_SENSE_LE	'L'
#define SOLVER_SENSE_GE	'G'
#define SOLVER_SENSE_EQ	'E'

#define SOLVER_SOS_TYPE1	'1'
#define SOLVER_SOS_TYPE2	'2'

# define CPX_COL_AT_LOWER                   3
# define CPX_COL_BASIC                      1
# define CPX_COL_AT_UPPER                   2
# define CPX_COL_FREE_SUPER                 0
# define CPX_COL_NONBASIC_ZERO_BOUNDED	CPX_COL_AT_LOWER
# define CPX_COL_NONBASIC_ZERO_UNBOUNDED CPX_COL_FREE_SUPER

# define CPX_INFBOUND coin_infinity(cpx_env) /* use the default cpx_env */

# define CPXPROB_MILP	PROBLEM_MIP
# define CPXPROB_QP	PROBLEM_QP
# define CPXPROB_MIQP	PROBLEM_MIQP 
# define CPXPROB_LP	PROBLEM_LP

# define CPX_MIN	SENSE_MIN
# define CPX_MAX	SENSE_MAX

# define SOLVER_HAS_STR_PARAMS /* has string parameters */
# define SOLVER_HAS_LOCAL_PARAMETERS
# define STRBUFFERSIZE 256 /* actual string param uses C++ string */

# ifdef COIN_USE_CLP

# define HAS_QUADRATIC /* CLP has quadratic */

# endif


/* solution states used in code to extract information from solver */ 
typedef enum
{
    state_success,
    state_fail,
    state_mipsemisucc,
    state_mipsemifail,
    state_lpaborted,
    state_unbounded,
    state_unknown
} state_t;

/* these are used to return useful information to the user (mainly reason for
   abort in the solve
*/
#define S_UNKNOWN	    0
#define S_SUCCESS	    1
#define S_FAIL		    2
#define S_UNBOUND	    3
#define S_UNBOUND_OR_FAIL   4
#define S_ABORT_UNKNOWN     5
#define S_ABORT_NUM	    6
#define S_ABORT_TIMELIM	    7
#define S_ABORT_NODELIM     8
#define S_ABORT_SOLLIM	    9
#define S_ABORT_LIM	   10
#define S_ABORT_PRIMOBJLIM 11
#define S_ABORT_DUALOBJLIM 12


#ifdef C_TO_COIN
#define EXTERN_C extern
#else
#define EXTERN_C extern "C"
#endif

/* The cpx_xxx() functions are compatible with other solvers.
 * The coin_xxx() ones are Coin-specific: use only in eplex_coin.c
 */
EXTERN_C int coin_get_objsen(COINprob * lp);
EXTERN_C int coin_get_numcols(COINprob* lp);
EXTERN_C int coin_get_numrows(COINprob* lp);
EXTERN_C int coin_get_probtype(COINprob* lp);
EXTERN_C int cpx_getrhs(COINprob * lp, double *rhs, int i);
EXTERN_C int cpx_getsense(COINprob * lp, char *rsense, int i);
EXTERN_C int cpx_getlb(COINprob * lp, double *lb, int j);
EXTERN_C int cpx_getub(COINprob * lp, double *ub, int j);
EXTERN_C int cpx_getbds(COINprob * lp, double *lb, double *ub, int j);
EXTERN_C int cpx_getctype(COINprob * lp, char *ctype, int j);
EXTERN_C int cpx_chgctype(COINprob * lp, int cnt, int *idxs, char *ctype);
EXTERN_C int cpx_setbds(COINprob * lp, int j, double lb, double ub);
EXTERN_C int cpx_setlb(COINprob * lp, int j, double lb);
EXTERN_C int cpx_setub(COINprob * lp, int j, double ub);
EXTERN_C int cpx_chgbds(COINprob * lp, int cnt, int * idxs, char * lu, double *bd);
EXTERN_C int cpx_loadbasis(COINprob * lp, int nc, int nr, const int *cbase, const int *rbase);
EXTERN_C int coin_getbasis(COINprob * lp, int *cbase, int *rbase);
EXTERN_C int coin_get_lpobjval(lp_desc* lp, double * objvalp);
EXTERN_C int coin_get_mipobjval(COINprob * lp, double * objvalp);
EXTERN_C int coin_get_bestmipbound(COINprob * lp, double * bound);
EXTERN_C int cpx_get_obj_coef(COINprob * lp, double *objc, int j);
EXTERN_C int cpx_chgobj(COINprob * lp, int cnt, int * idxs, double * values);
EXTERN_C int coin_get_order(COINprob * lp, int cnt, int * idxs, int * prio, int * direction);
EXTERN_C int coin_chgqobj(COINprob * lp, int i, int j, double value);
EXTERN_C int cpx_chgqpcoef(COINprob *glp, int i, int j, double val);
EXTERN_C int cpx_chgrhs(COINprob * lp, int cnt, int * idxs, double * values);
EXTERN_C int coin_loadprob(COINprob* lp, int mac, int mar, int objsen, double* objx, 
	double* rhsx, char* senx, 
	int * matbeg, int* matcnt, int* matind, double* matval, 
	double* lb, double* ub);
EXTERN_C int coin_setcoltype(COINprob* lp, char *ctype);
EXTERN_C int cpx_addcols(COINprob* lp, int coladded, int matnz, const double* objx, 
	 int* matbeg, const int* matind, const double* matval, 
	 const double* bdl, const double* bdu);
EXTERN_C int cpx_addrows(COINprob* lp, const int rowadded, int nzadded, 
	 const double* rhsx, const char* senx,
	 int* rmatbeg, int* rmatind, double* rmatval);
EXTERN_C int cpx_addsos(COINprob* lp, int nsos, int nsosnz, char* sostype, 
		  int* sosbeg, int* sosind, double* soswt);
EXTERN_C int cpx_chgobjsen(COINprob* lp, int objsen);
EXTERN_C int cpx_getrow(COINprob* lp, int* nnz, int* rmatind, double* rmatval, int nnz_sz, int idx);
EXTERN_C int coin_delrows(COINprob* lp, int ndr, int* idx);
EXTERN_C int coin_delcols(COINprob* lp, int ndr, int* idx);
EXTERN_C int cpx_delsos(COINprob* lp, int from, int to);
EXTERN_C int coin_free_prob(COINprob* lp);
EXTERN_C int coin_get_bar_primal_objval(COINprob* lp, double* objval);
EXTERN_C int coin_get_bar_dual_objval(COINprob* lp, double* objval);
EXTERN_C state_t coin_get_result_state(lp_desc* lpd);
EXTERN_C int coin_get_mipcutoff(COINprob* lp, double* bestbound);
EXTERN_C double coin_infinity(COINprob* lp);
EXTERN_C int coin_getdblparam(COINprob* lp, int key, double* value);
EXTERN_C int coin_getintparam(COINprob* lp, int key, int* value);
EXTERN_C int coin_getstrparam(COINprob* lp, int key, char* value);
EXTERN_C int coin_setdblparam(COINprob* lp, int key, double value);
EXTERN_C int coin_setintparam(COINprob* lp, int key, int value);
EXTERN_C int coin_setstrparam(COINprob* lp, int key, const char* value);
EXTERN_C int coin_set_qobj(COINprob* lp, int mac, int cb_cnt, int* cb_index, int*
		  cb_index2, double* cb_value); 
EXTERN_C int coin_get_solver_dblparam(COINprob* lp, int key, double* value);
EXTERN_C int coin_get_solver_intparam(COINprob* lp, int key, int* value);
EXTERN_C int coin_set_solver_dblparam(COINprob* lp, int key, double value);
EXTERN_C int coin_set_solver_intparam(COINprob* lp, int key, int value);
EXTERN_C int coin_get_eplex_strparam(COINprob* lp, int key, char* value);
EXTERN_C int coin_get_eplex_intparam(COINprob* lp, int key, int* value);
EXTERN_C int coin_set_eplex_strparam(COINprob* lp, int key, const char* value);
EXTERN_C int coin_set_eplex_intparam(COINprob* lp, int key, int value);
EXTERN_C int coin_solve_problem(lp_desc* lpd, 
	int meth, int auxmeth, int node_meth, int node_auxmeth);
EXTERN_C int cpx_get_soln_state(lp_desc* lpd);
EXTERN_C int coin_set_timeout(COINprob* lp, double timeout);
EXTERN_C int coin_create_prob(COINprob** lp, COINprob* def);
EXTERN_C int coin_reset_prob(lp_desc* lpd);
EXTERN_C int coin_writeprob(COINprob* lp, const char* file, char* otype);
EXTERN_C int coin_readprob(COINprob* lp, const char* file, char* otype);
EXTERN_C int cpx_getnumnz(COINprob* lp);
EXTERN_C int cpx_getnumint(COINprob* lp);
EXTERN_C int cpx_getnumbin(COINprob* lp);
EXTERN_C int cpx_getnumqpnz(COINprob* lp);
EXTERN_C int cpx_chgname(COINprob* lp, char ntype, int idx, const char* name, int length);
EXTERN_C int coin_get_dual_infeas(COINprob* lp, int* infeas);
EXTERN_C int coin_get_primal_infeas(COINprob* lp, int* infeas);
EXTERN_C int coin_bar_is_primal_feas(COINprob* lp);
EXTERN_C int coin_bar_is_dual_feas(COINprob* lp);
EXTERN_C void coin_get_solver_info(char* info);
EXTERN_C int solver_has_method(int m);
EXTERN_C int solver_has_node_method(int m);

