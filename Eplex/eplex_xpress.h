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
 * ECLiPSe/XPRESSMP interface (for inclusion in eplex.c)
 */

#include "xprs.h"

#define CPXLPptr		XPRSprob /* prob. pointer 13+ only! */
#define CPXENVptr		XPRSprob /* 'default' problem */

#define SOLVE_MIP_COPY

typedef int param_id_t;
typedef char sostype_t;
typedef char direction_t;       /* branching direction indicator type */

#include "eplex.h"		/* needs declarations above! */


# define SOLVER_SHORT_NAME XPRS
# define SOLVER_ATOMIC_NAME "xpress"
# define SOLVER_VERSION_INT XPRESS
# define XP_PROBNAME_MAX 200  /* maximum length of problem name */
# define HAS_QUADRATIC
# define HAS_MIQP
# define SOLVER_HAS_STR_PARAMS /* has string parameters */
# define STRBUFFERSIZE  256  /* string parameter buffer size (256 used in example)*/
/* copying a problem with zeroed quad. coeff. can lead to core dumps */
# define HAS_MIQP_FIXEDCOPYBUG 
# define HAS_INTLB_BUG /* LB lost when converting col to int type */
# if XPRESS <= 14
#  define HAS_MIQP_CALLBACKBUG /* callback to get MIQP solution core dumps */
#  define HAS_SAMEBOUNDSBUG /* sol. value = 0 if both bounds set to same */
# endif
# define SOLVER_HAS_LOCAL_PARAMETERS
# if XPRESS >= 14      /* need to constrain integer range...*/
#  define HAS_NARROW_INT_RANGE
#  ifndef XPRS_MAXINT  /* in case we are using an old xprs.h file */
#   define XPRS_MAXINT         2147483647
#  endif
# endif
# if XPRESS >= 15
#  define HAS_POSTSOLVE
/* XPRSpostsolve is not documented and not declared in xprs.h */
int XPRS_CC XPRSpostsolve(XPRSprob prob);
# endif
# if XPRESS < 20
#define HAS_NO_ADDSOS
# endif

# ifndef XPRESSMINOR
#  define XPRESSMINOR 0
# endif

# ifdef __STDC__
#  define __ANSIC_	/* used in xpresso.h */
# endif

# define XP_GLSTAT_OFFSET 0x10

#define SOLVER_SENSE_LE	'L'
#define SOLVER_SENSE_GE	'G'
#define SOLVER_SENSE_EQ	'E'

#define SOLVER_SOS_TYPE1	'1'
#define SOLVER_SOS_TYPE2	'2'

#define SOLVER_HAS_LOADORDER
#define SOLVER_DIR_DOWN     'D'
#define SOLVER_DIR_UP       'U'
#define SOLVER_DIR_DEFAULT  'N'

# define CPX_INFBOUND			XPRS_PLUSINFINITY
# define CPX_COL_AT_LOWER               0
# define CPX_COL_BASIC                  1
# define CPX_COL_AT_UPPER               2
# define CPX_COL_FREE_SUPER             3
# define CPX_COL_NONBASIC_ZERO_BOUNDED	CPX_COL_AT_LOWER
# define CPX_COL_NONBASIC_ZERO_UNBOUNDED CPX_COL_FREE_SUPER

# define Update_Model(LP)

# define SUPPORT_IIS
# if XPRESS <= 20
# define Find_Conflict(Err, L, NRows, NCols) { \
	Err = XPRSiis(L, ""); \
        if (!Err) Err = XPRSgetiis(L, &(NCols), &(NRows), NULL, NULL); \
}
# define Get_Conflict(L, Status, RowIdxs, RowStat, Nrows_p, ColIdxs, ColStat, Ncols_p)  \
	Status = XPRSgetiis(L, Ncols_p, Nrows_p,  ColIdxs, RowIdxs)
# else
# define Find_Conflict(Err, L, NRows, NCols) { \
	Err = XPRSiisfirst(L, 1, &Err); \
        if (!Err) Err = XPRSgetiisdata(L, 1, &(NRows), &(NCols), NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL); \
}
# define Get_Conflict(L, Status, RowIdxs, RowStat, Nrows_p, ColIdxs, ColStat, Ncols_p)  \
	Status = XPRSgetiisdata(L, 1, Nrows_p,  Ncols_p, RowIdxs, ColIdxs, NULL, NULL, NULL, NULL, NULL, NULL)
# endif

# define Get_LP_Objval(A1,A2)           XPRSgetdblattrib((A1)->lpcopy,XPRS_LPOBJVAL,A2)
# define Get_MIP_Objval(E,A1,A2)        XPRSgetdblattrib(A1,XPRS_MIPOBJVAL,A2)
# define Get_Best_Objbound(A1, A2)      XPRSgetdblattrib(A1,XPRS_BESTBOUND,A2)
# define Get_MIPCutOff(d, v) XPRSgetdblcontrol((d)->lpcopy, XPRS_MIPABSCUTOFF, v)
# define Get_Dual_Infeas(lp, v) XPRSgetintattrib(lp, XPRS_DUALINFEAS, v)
# define Get_Primal_Infeas(lp, v) XPRSgetintattrib(lp, XPRS_PRIMALINFEAS, v)

# define SetPreSolve(state) 


# define Get_Xp_Stat(lpd) \
	if (IsMIPProb(lpd->prob_type)) { \
	    (void) XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &lpd->sol_state); \
	    if (lpd->sol_state == XPRS_MIP_LP_NOT_OPTIMAL || \
               lpd->sol_state == XPRS_MIP_LP_OPTIMAL) \
		(void) XPRSgetintattrib(lpd->lpcopy, XPRS_LPSTATUS, &lpd->sol_state); \
	    else \
		lpd->sol_state += XP_GLSTAT_OFFSET; \
	} else if (lpd->prob_type == PROBLEM_FIXEDL || lpd->prob_type == PROBLEM_FIXEDQ) { \
        /* fixglobal only performed if MIP was optimal */ \
            (void) XPRSgetintattrib(lpd->lpcopy, XPRS_MIPSTATUS, &lpd->sol_state); \
            if (lpd->sol_state == XPRS_MIP_OPTIMAL) \
               (void) XPRSgetintattrib(lpd->lpcopy, XPRS_LPSTATUS, &lpd->sol_state); \
            else \
            { \
	       if (lpd->sol_state == XPRS_MIP_LP_NOT_OPTIMAL || \
                  lpd->sol_state == XPRS_MIP_LP_OPTIMAL) \
		  (void) XPRSgetintattrib(lpd->lpcopy, XPRS_LPSTATUS, &lpd->sol_state); \
	       else \
		  lpd->sol_state += XP_GLSTAT_OFFSET; \
            } \
        } else if (lpd->prob_type == PROBLEM_RELAXEDL || lpd->prob_type == PROBLEM_RELAXEDQ) \
        { \
	    (void) XPRSgetintattrib(lpd->lpcopy, XPRS_LPSTATUS, &lpd->sol_state); \
        } \
	else \
        { \
	    (void) XPRSgetintattrib(lpd->lp, XPRS_LPSTATUS, &lpd->sol_state); \
	}



# define SuccessState(d)	( \
	(d)->sol_state == XP_GLSTAT_OFFSET + XPRS_MIP_OPTIMAL || \
	(d)->sol_state == XPRS_LP_OPTIMAL )
/* the LP_CUTOFF* LPSTATUS happens only with MIP search, and as we access
   LPSTATUS for MIP search only if the MIP search is stopped at the root,
   we know that these states means that a cutoff occurred at the root node,
   and that the MIP optimal solution cannot be better than the cutoff, and
   this is considered to be a failure state
*/
# define FailState(d) ( \
	(d)->sol_state == XP_GLSTAT_OFFSET + XPRS_MIP_INFEAS || \
	(d)->sol_state == XPRS_LP_INFEAS || \
	(d)->sol_state == XPRS_LP_CUTOFF || \
	(d)->sol_state == XPRS_LP_CUTOFF_IN_DUAL)
/* catches the MIP cases only */
# define MIPSemiFailState(d) ( \
	(d)->sol_state == XP_GLSTAT_OFFSET + XPRS_MIP_NO_SOL_FOUND)
/* catches the MIP cases only */
# define MIPSemiSuccessState(d) ( \
	(d)->sol_state == XP_GLSTAT_OFFSET + XPRS_MIP_SOLUTION)
/* An aborted LP can be either semi-fail or semi-success */
# define LPAbortedState(d) ( \
        (d)->sol_state == XPRS_LP_UNFINISHED )
# define MaybeFailState(d) (0)
# define UnboundedState(d) ( \
	(d)->sol_state == XPRS_LP_UNBOUNDED )

# define DualMethod(lpd,m,am) 0

# define UsingBarrierNoCrossOver(d) (meth->meth == METHOD_BAR && meth->auxmeth == METHOD_NONE) 
# define Get_Bar_Primal_Obj(d, obj) XPRSgetdblattrib((d), XPRS_BARPRIMALOBJ, obj)
# define Get_Bar_Dual_Obj(d, obj) XPRSgetdblattrib((d), XPRS_BARDUALOBJ, obj)
# define Bar_Is_Primal_Feasible(lpd) \
	(XPRSgetdblattrib(lpd->lp, XPRS_BARPRIMALINF, &infeas), infeas < 1e-6)
# define Bar_Is_Dual_Feasible(lpd) \
	(XPRSgetdblattrib(lpd->lp, XPRS_BARDUALINF, &infeas), infeas < 1e-6)


