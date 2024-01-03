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


/* return 0 for success, -1 for licensing failure, 1 for error */
static int
cpx_init_env(CPXENVptr *penv, char *licloc, int serialnum, char *subdir)
{
    int err;
    Call(err, coin_create_prob(penv, NULL));
    return err ? -1 : 0;
}


static void
cpx_exit(CPXENVptr *penv)
{
    coin_free_prob(*penv);
    *penv = 0;
}


/* -------------------- Set/change problem data -------------------- */


static inline int
cpx_delrangeofrows(lp_desc *lpd, int from, int to)
{
    _grow_numbers_array(lpd, to);
    return coin_delrows(lpd->lp, to-from, &lpd->numbers[from]);
}


static inline int
cpx_delrangeofcols(lp_desc *lpd, int from, int to)
{
    _grow_numbers_array(lpd, to);
    return coin_delcols(lpd->lp, to-from, &lpd->numbers[from]);
}


static inline int
cpx_chgprobtype(COINprob *glp, int type)
{
    return 0;   /* not needed */
}


/* -------------------- Problem creation -------------------- */

static int
cpx_loadprob(lp_desc* lpd)
{
    int err;

    CallN(coin_create_prob(&(lpd->lp), cpx_env));
    CallN(lpd->lpcopy = lpd->lp);  /* no need for a copy in CPLEX */
    Call(err, coin_loadprob(lpd->lp, lpd->mac, lpd->mar,
    		lpd->sense, lpd->objx, lpd->rhsx, lpd->senx,
		lpd->matbeg, lpd->matcnt, lpd->matind, lpd->matval,
		lpd->bdl, lpd->bdu));
    if (err)
        return 1;

    if (IsMIPProb(lpd->prob_type))
    {
# if defined(LOG_CALLS) 
/* no need to log for XPRESS as ctype array not used directly */
	{ int i;
	    for (i=0; i<lpd->mac; ++i)
	    {
		Fprintf(log_output_, "\n\tlpd->ctype[%d] = '%c';", i, lpd->ctype[i]);
	    }
	}
# endif
	Call(err, coin_setcoltype(lpd->lp, lpd->ctype));
	if (err)
            return 1;
    }

    if (lpd->nsos)
    {
# if defined(LOG_CALLS)
      int i;
      Fprintf(log_output_, "\n\
          lpd->nsos = %d;\n\
          lpd->nsosnz = %d;\n\
          lpd->sostype = (sostype_t *) malloc(%d*sizeof(sostype_t));\n\
          lpd->sosbeg = (int *) malloc(%d*sizeof(int));\n\
          lpd->sosref = (double *) malloc(%d*sizeof(double));\n\
          lpd->sosind = (int *) malloc(%d*sizeof(int));",
	      lpd->nsos, lpd->nsosnz, lpd->nsos, lpd->nsos, 
	      lpd->nsosnz, lpd->nsosnz);
      for(i=0; i < lpd->nsos; ++i) {
	  Log2(lpd->sosbeg[%d] = %d, i, lpd->sosbeg[i]);
	  Log2(lpd->sostype[%d] = '%c', i, lpd->sostype[i]);
      }
      for(i=0; i < lpd->nsosnz; ++i) {
	  Log2(lpd->sosref[%d] = %f, i, lpd->sosref[i]);
	  Log2(lpd->sosind[%d] = %d, i, lpd->sosind[i]);
      }
# endif
	Call(err, cpx_addsos(lpd->lp, lpd->nsos, lpd->nsosnz, 
			    lpd->sostype, lpd->sosbeg, lpd->sosind, lpd->sosref));
	if (err)
            return 1;
	lpd->nsos_added = lpd->nsos;
    }
    if IsQPProb(lpd->prob_type)
    {
# ifdef HAS_QUADRATIC
	int i;
#  ifdef HAS_MIQP
	int ptype = (lpd->prob_type == PROBLEM_QP ? CPXPROB_QP : CPXPROB_MIQP);
#  else
	int ptype = CPXPROB_QP;
#  endif
	coin_set_qobj(lpd->lp, lpd->mac, lpd->cb_cnt, lpd->cb_index, lpd->cb_index2, lpd->cb_value);

# else /* !HAS_QUADRATIC */
	Fprintf(Current_Error, "Eplex error: Quadratic problems not supported for this solver!\n");
	ec_flush(Current_Error);
        return 1;
	Bip_Error(EC_EXTERNAL_ERROR);
# endif
    }
    return 0;    
}


static inline void
cpx_freeprob(lp_desc *lpd)
{
    CallN(coin_free_prob(lpd->lp));
    /* lpd->lp allocated with new, and freed with delete by coin_free_prob()
       so no need to free here
    */
}


/* -------------------- Solving -------------------- */

static int
cpx_prepare_solve(lp_desc* lpd, struct lp_meth *meth, double timeout)
{
    /* set timeout. If no timeout was set, vtimeout is integer 0 */
    if (timeout > 0.0)
    {
	Log1(coin_set_timeout(lpd->lp, %f), timeout);
	coin_set_timeout(lpd->lp, timeout);
    } 
/*    coin_set_solve_methods(lpd, meth, auxmeth, node_meth, node_auxmeth);*/
    return 0;
}


static int
cpx_solve(lp_desc* lpd, struct lp_meth *meth, double* bestbound, double* worstbound)
{
    int solve_state = S_UNKNOWN;  
    struct lp_sol *sol = &lpd->sol;

    Log2({lpd->prob_type = %d; lpd->presolve = %d;}, lpd->prob_type, lpd->presolve);
    Log4(coin_solve_problem(lpd, %d, %d, %d, %d), meth->meth, meth->auxmeth, meth->node_meth, meth->node_auxmeth);
    if (coin_solve_problem(lpd, meth->meth, meth->auxmeth, meth->node_meth, meth->node_auxmeth) == -1)
	return -1;

    /*********************************************************************
     *     Get State Information from External Solver                    *
     *********************************************************************/

    solve_state = coin_get_result_state(lpd);

/* Here we test for various states of the solution. The following macro tests
are defined for all the solvers:
  SuccessState: solution is `optimal' (may be optimal within tolerance)
  FailState: problem is proven infeasible or no MIP solution is better 
	     than cutoff.
  MIPSemiSuccessState: solution exist, but may be suboptimal (MIP only)
  MIPSemiFailState: no solution found yet, but problem not proven 
		    infeasible (MIP only)
  LPAbortedState: LP solve was aborted 
		  (for LP, or  root node LP solve for MIP (not CPLEX)) 
  UnboundedState: problem is unbounded
  MayBeFailState: problem is infeasible or unbounded, but solver cannot 
		  determine which
*/
  
    if (SuccessState(lpd)) {
	lpd->descr_state = DESCR_SOLVED_SOL;
	lpd->optimum_ctr++;
	if (IsMIPProb(lpd->prob_type))
	{
	    CallN(coin_get_mipobjval(lpd->lpcopy, worstbound));
	    /* bestbound may be different from objval because of tolerance */
	    lpd->objval = *worstbound;
	    CallN(coin_get_bestmipbound(lpd->lpcopy, bestbound));
	} else
	{
	    CallN(coin_get_lpobjval(lpd, &lpd->objval));
	    if (UsingBarrierNoCrossOver(lpd->lp))
	    { 
		CallN(coin_get_bar_primal_objval(lpd->lp, worstbound));
		CallN(coin_get_bar_dual_objval(lpd->lp, bestbound));
	    } else 
	    { 
		CallN(coin_get_lpobjval(lpd, bestbound));
		*worstbound = *bestbound;
	    }
	}

    } else if (FailState(lpd)) {
	/* for MIP problems, the MIP search may have nodes that were not
	   explored further because of cutoff -- i.e. they cannot lead to
	   solutions better than the cutoff. If no solution is found, the
	   problem is considered infeasible, but strictly it means there is
	   no solution better than cutoff. Unfortunately, it is not easy to
	   know if cutoff had occurred in a MIP search, so bestbound is set
	   to cutoff unless we know otherwise
	*/
	if (DualMethod(lpd, meth->meth, meth->auxmeth)) {
	    lpd->descr_state = DESCR_UNKNOWN_NOSOL;
	    /* dual infeasible ==> primal infeasible or unbounded
	       no information: full interval */
	    *worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
	    *bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	} else {
	    lpd->descr_state = DESCR_SOLVED_NOSOL;
	    *worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
		/* infeasible LP for Xpress, infeasible LP/MIP for COIN */
		*bestbound = (lpd->sense ==  SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);

	}
	lpd->objval = *worstbound;
	lpd->infeas_ctr++;
    } else if (MIPSemiSuccessState(lpd)) {
	lpd->descr_state = DESCR_ABORTED_SOL;
	lpd->abort_ctr++;

	CallN(coin_get_bestmipbound(lpd->lpcopy, bestbound));
	CallN(coin_get_mipobjval(lpd->lpcopy, worstbound));
	lpd->objval = *worstbound;

    } else if (MIPSemiFailState(lpd)) {
    /* For Xpress and COIN, the MIPSemiFailState does not include 
       aborting/stopping at the root node solve */
	CallN(coin_get_bestmipbound(lpd->lpcopy, bestbound)); 
	*worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);

	lpd->objval = *worstbound;
	lpd->descr_state = DESCR_ABORTED_NOSOL;
	lpd->abort_ctr++;

    } else if (UnboundedState(lpd)) {
	if (DualMethod(lpd, meth->meth, meth->auxmeth)) {
	    lpd->descr_state = DESCR_SOLVED_NOSOL;
	    lpd->infeas_ctr++;
	    *bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
	    *worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
	} else {
	    lpd->descr_state = DESCR_UNBOUNDED_NOSOL;
	    lpd->abort_ctr++;
	    *bestbound = *worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL : HUGE_VAL);
	}
	lpd->objval = *worstbound;

    } else if (MaybeFailState(lpd)) {
	lpd->descr_state = DESCR_UNKNOWN_NOSOL;
	lpd->infeas_ctr++;
	/* no information on bounds */
	*worstbound = (lpd->sense == SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
	*bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	lpd->objval = *worstbound;
    } else if (LPAbortedState(lpd)) {
	/* The exact status of an aborted LP case depends on the solving 
	   method used */
	int attr; /* variable for integer attribute */

	/* meth->meth is used for LP and root MIP LP */

	if (meth->meth == METHOD_DEFAULT) 
	    meth->meth = METHOD_DUAL;

	if (IsMIPProb(lpd->prob_type))
	{ /* MIP search aborted in root LP solve */
	    lpd->descr_state = DESCR_ABORTED_NOSOL;
	    lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
	    CallN(coin_get_bestmipbound(lpd->lpcopy, bestbound));
	    if (meth->meth == METHOD_DUAL)
	    {
		coin_get_dual_infeas(lpd->lpcopy, &attr);
		if (attr == 0)
		{
		    /* attr == 0 ==> we have a feasible dual `solution' for 
		       root node. This is superoptimal, and so can form the
		       best bound for the MIP problem
		    */
		    coin_get_lpobjval(lpd, bestbound);
		}
	    }
	} else
	{   /* !IsMIPProb */
	    switch (meth->meth)
	    {
	    case METHOD_DUAL:
		lpd->descr_state = DESCR_ABORTED_NOSOL;
		coin_get_dual_infeas(lpd->lp, &attr);
		/* attr == 0 ==> we have a feasible dual `solution' (i.e.
		   we are in phase II of the Simplex). This
		   is superoptimal for the original problem */
		if (attr == 0)
		    coin_get_lpobjval(lpd, bestbound);
		else /* no information on bestbound */
		    *bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
		lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
		break;
	    case METHOD_BAR:
		lpd->descr_state = DESCR_ABORTED_NOSOL;
		    if (coin_bar_is_primal_feas(lpd))
		    {/* primal feasible */
			coin_get_bar_primal_objval(lpd->lp, worstbound);
			lpd->objval = *worstbound;
			lpd->descr_state = DESCR_ABORTED_SOL;
		    }
		    if (coin_bar_is_dual_feas(lpd))
		    {/* dual feasible */
			coin_get_bar_dual_objval(lpd->lp, bestbound);
		    }
		    break;
	    case METHOD_PRIMAL:
		coin_get_primal_infeas(lpd->lp, &attr);
		/* attr == 0 ==> we have a feasible primal solution */
		if (attr == 0)
		{
		    coin_get_lpobjval(lpd, worstbound);
		    lpd->objval = *worstbound;
		    *bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
		    lpd->descr_state = DESCR_ABORTED_SOL;
		} else
		{
		    lpd->descr_state = DESCR_ABORTED_NOSOL;
		    *bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
		    lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
		}
		break;
	    default:
		/* this should not happen! */
		Fprintf(Current_Error, "Eplex error: Unexpected method case while classifying results. Aborting.\n");
		return -1;
	    }

	} /* !IsMIPProb() */

	lpd->abort_ctr++;


    } else { 
	/* fall back case where we don't have any information */
	lpd->descr_state = DESCR_ABORTED_NOSOL;
	*bestbound = (lpd->sense ==  SENSE_MIN ?  -HUGE_VAL : HUGE_VAL);
	lpd->objval = *worstbound = (lpd->sense == SENSE_MIN ? HUGE_VAL :  -HUGE_VAL);
    }

    return 0;
}


/* -------------------- Read/Write -------------------- */

static int
cpx_write(lp_desc *lpd, char *file, char *fmt)
{
    return coin_writeprob(lpd->lp, file, fmt);
}


static int
cpx_read(lp_desc *lpd, char *file, char *fmt)
{
    if (coin_create_prob(&(lpd->lp), cpx_env))
    	return -1;

    if (coin_readprob(lpd->lp, file, fmt))
    	return -1;

    /* initialize non-zero fields in lp_desc */
    CallN(lpd->lpcopy = lpd->lp);  /* no need for a copy */
    lpd->sense = coin_get_objsen(lpd->lp);
    lpd->mac = coin_get_numcols(lpd->lp);
    lpd->mar = coin_get_numrows(lpd->lp);
    lpd->prob_type = coin_get_probtype(lpd->lp);
    return 0;
}


/* -------------------- Parameter handling -------------------- */
/*
 * Parameter Table
 *
 * defines a table params[] which maps the symbolic names of the optimizer
 * flags to the internal numbers. The set of flags and the numbers differ
 * in each version, therefore this file has to be updated every time.
 * For a new version do the following:
 *
 * extract the CPX_PARAM_ lines out of cplex.h
 * ignore the aliases for old names in ifndef CPLEX_MODERN
 * ignore the boundary definitions CPX_PARAM_ALL_MIN/MAX
 * substitute as follows:
 *
 *   s/^#define[	 ]CPX_PARAM_\([^ ]*\).*$/{"\L\1\E", CPX_PARAM_\1, 0},/
 *
 * mark the int params with 0, the doubles with 1, the strings with 2
 * count the lines and define NUMPARAMS accordingly!
 * add the new section within the proper #if's
 */

#define NUMALIASES 2

# ifdef COIN_USE_CLP
#  define NUMPARAMS 22
#  include "coinplex_params.h"

# else
#  define NUMPARAMS 8
# endif

/* parameters for COIN OSI are more complicated, because there is
   no single uniform source for the parameters. The following types
   are currently defined:

   OSI Params -- common to all OSI based solvers
   =============================================
   0 - OSI integer params
   1 - OSI double  params
   2 - OSI string  params

   Solver(s) specific Params -- not defined by OSI
   ===============================================
   3 - Solver(s) integer params 
   4 - Solver(s) double  params 
   5 - Solver(s) string  params

   Eplex Params -- Params defined in eplex to control solver(s) behaviour
   ====================================================================== 
   6 - Eplex integer params 
   7 - Eplex double  params 
   8 - Eplex string  params

*/
/* these are taken from OsiSolverParameters.hpp */
#define OsiProbName 			0
#define OsiSolverName 			1
#define OsiDualObjectiveLimit 		0
#define OsiPrimallObjectiveLimit 	1
#define OsiDualTolerance 		2
#define OsiPrimalTolerance 		3
#define OsiMaxNumIteration 		0
#define OsiMaxNumIterationHotStart 	1

static struct param_desc params[NUMPARAMS+NUMALIASES] = {
/* OSI */
{"probname", OsiProbName, 2},
{"solvername", OsiSolverName, 2},
{"dualobjectivelimit", OsiDualObjectiveLimit, 1},
{"prinmalobjectivelimit", OsiPrimallObjectiveLimit, 1},
{"dualtolerance", OsiDualTolerance, 1},
{"primaltolerance", OsiPrimalTolerance, 1},
{"maxnumiteration", OsiMaxNumIteration, 0},
{"maxnumiterationhotstart", OsiMaxNumIterationHotStart, 0},

# ifdef COIN_USE_CLP
/* Solver */
{"node_limit", SolverMaxNumNode,  3},
{"solution_limit", SolverMaxNumSol, 3},
{"integrality", SolverIntegerTolerance, 4},
{"absmipgap", SolverAllowableGap, 4},
{"mipgap", SolverAllowableFractionGap, 4},
{"objdifference", SolverCutoffIncrement, 4},
{"absmipheuristicgap", SolverHeuristicGap, 4},
{"mipheuristicgap", SolverHeuristicFractionGap, 4},
{"lppresolve_tol", SolverLPPresolveTolerance, 4},

/* Eplex */
{"bar_ordering", EpxClpParam_bar_ordering, 8},
{"mip_print_freq", EpxClpParam_print_freq, 6},
{"loglevel", EpxClpParam_loglevel, 6},
{"mip_lploglevel", EpxClpParam_mip_lploglevel, 6},
{"bar_doKKT", EpxClpParam_doKKT, 6},
# endif


/*
 * Add some version-independent aliases to the table
 * This must remain at the end of the file!!!
 * If you add lines here, update NUMALIASES above!
 * NUMALIASES lines follow
 */

{"iteration_limit",OsiMaxNumIteration, 0},
{"feasibility_tol", OsiPrimalTolerance, 1},

};


static int
cpx_get_par_info(char* name, param_id_t* pparnum, int* ppartype)
{
    int i;
    for(i=0; i<NUMPARAMS+NUMALIASES; ++i)	/* lookup the parameter name */
    {
	if (strcmp(params[i].name, name) == 0)
	{
	    *pparnum = params[i].num;
	    *ppartype = params[i].type;
	    return 0;
	}
    }
    return 1;
}


static inline int
cpx_set_int_param(lp_desc *lpd, int parnum, int val)
{
    return coin_setintparam(lpd ? lpd->lp : cpx_env, parnum, val);
}

static inline int
cpx_set_dbl_param(lp_desc *lpd, int parnum, double val)
{
    return coin_setdblparam(lpd ? lpd->lp : cpx_env, parnum, val);
}

static inline int
cpx_set_str_param(lp_desc *lpd, int parnum, const char *val)
{
    return coin_setstrparam(lpd ? lpd->lp : cpx_env, parnum, val);
}

static inline int
cpx_get_int_param(lp_desc *lpd, int parnum, int *pval)
{
    return coin_getintparam(lpd ? lpd->lp : cpx_env, parnum, pval);
}

static inline int
cpx_get_dbl_param(lp_desc *lpd, int parnum, double *pval)
{
    return coin_getdblparam(lpd ? lpd->lp : cpx_env, parnum, pval);
}

static inline int
cpx_get_str_param(lp_desc *lpd, int parnum, char *pval)
{
    return coin_getstrparam(lpd ? lpd->lp : cpx_env, parnum, pval);
}

static inline double
cpx_feasibility_tol(lp_desc *lpd)
{
    double tol;
    coin_getdblparam(lpd->lp, OsiPrimalTolerance, &tol);
    return tol;
}

