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
 * ECLiPSe/GLPK interface (for inclusion in eplex.c)
 *
 * Notes on GLPK
 *
 * Parameters are grouped into parameter structures for the different
 * software components, and are passed along for certain operations.
 * There is an init function to fill each structure with default values:
 *
 *      void glp_init_smcp(glp_smcp *parm)     simplex
 *      void glp_init_iocp(glp_iocp *parm)     integer optimizer
 *      void glp_init_iptcp(glp_iptcp *parm)   interior point
 *
 * We keep one global set of default parameters (in cpx_env)
 * and make one copy per problem when created.
 * We simulate node_limit via MIP callback.
 *
 * GLPK is so far the only solver that numbers row/columns from 1..N.
 * Eplex's C level (and Prolog level) historically use 0..N-1.
 * The cpx_xxx() functions in this file expect row/column numbers 1..N
 * (so the caller has to add 1) both for single index arguments and
 * within matind arrays.  Such arguments are marked with *1* below.
 * 
 * In arrays passed to/from GLPK, slots 1..N are used, [0] is unused.
 * When handing Eplex arrays to a GLPK API function, we pass &array[-1]
 * to simulate the unused [0] element.
 *
 * GLPK requires integer variables to have integral bounds!
 *
 * Supported methods: AUTO, PRIMAL DUAL, BAR
 * Node_methods: not separately selectable
 *
 * No notion of different message kinds/streams (log,warning,error,result),
 * instead msg_lev parameter (OFF,ERR,ON,ALL,DBG).  We print everything
 * to the ECLiPSe 'output' stream...
 *
 * TODO:
 * Preprocess handling
 *
 * The functions here return 0 for success, 1 for error, -1 for unimplemented.
 */


/* -------------------- Initialization and setup -------------------- */

/* All GLPK output goes through this function.  As GLPK doesn't tell us
 * what type of message it is (error, warning, etc), we print everything
 * to Current_Output.  The CPLEX-style "channel" settings are ignored.
 * Verbosity must be adjusted using the msg_lev parameter.
 */
static int
_glp_output(void *info, const char *msg)
{
    (void) ec_outf(Current_Output, msg, strlen(msg));
    (void) ec_flush(Current_Output);
    return 0;
}


static void
_warn(char *msg)
{
    ec_outfs(solver_streams[WrnType], msg);
    ec_newline(solver_streams[WrnType]);
}


static int
cpx_init_env(CPXENVptr *penv, char *licloc, int serialnum, char *subdir)
{
    CPXENVptr env;
    int status = glp_init_env();
    if (status < 0 || status > 1)
        return 1;
    env = Malloc(sizeof(glp_env));
    env->node_limit = INT_MAX;
    glp_init_smcp(&env->sm);            /* init global parameters */
    glp_init_iocp(&env->io);
    glp_init_iptcp(&env->ip);
    env->msg_lev = GLP_MSG_ERR;
    *penv = env;
    glp_term_hook(_glp_output, NULL);
    return 0;
}


static void
cpx_exit(CPXENVptr *penv)
{
    glp_free_env();
    Free(*penv);
    *penv = 0;
}


/* -------------------- Set/change problem data -------------------- */

/* Set column bounds, inferring the appropriate "type".
 */
static inline int
cpx_setbds(glp_prob *glp, int j /*1*/, double lb, double ub)
{
    int coltype;
    if (ub < CPX_INFBOUND)
        if (lb > -CPX_INFBOUND)
            if (lb == ub) coltype = GLP_FX; else coltype = GLP_DB;
        else coltype = GLP_UP;
    else
        if (lb > -CPX_INFBOUND) coltype = GLP_LO;
        else coltype = GLP_FR;

    glp_set_col_bnds(glp, j, coltype, lb, ub);
    return 0;
}


static inline int
cpx_setlb(glp_prob *glp, int j /*1*/, double lb)
{
    return cpx_setbds(glp, j, lb, glp_get_col_ub(glp, j));
}


static inline int
cpx_setub(glp_prob *glp, int j /*1*/, double ub)
{
    return cpx_setbds(glp, j, glp_get_col_lb(glp, j), ub);
}


/*
 * CPLEX-style function to change multiple L/U/B bounds.
 * CAUTION: input arrays are 0-based, but column indexes still 1-based!
 */
static int
cpx_chgbds(glp_prob *glp, int sz, int *col /*1*/, char *lu, double *bd)
{
    int i;
    for (i=0; i<sz; ++i)
    {
        switch(lu[i])
        {
            case 'U': cpx_setub(glp, col[i], bd[i]); break;
            case 'L': cpx_setlb(glp, col[i], bd[i]); break;
            case 'B': glp_set_col_bnds(glp, col[i], GLP_FX, bd[i], bd[i]); break;
            default: return 1;
        }
    }
    return 0;
}


static int
cpx_chgrhs(glp_prob *glp, int sz, int *row /*1*/, double *rhs)
{
    int i;
    for (i=0; i<sz; ++i)
    {
        int rowtype = glp_get_row_type(glp, row[i]);
        glp_set_row_bnds(glp, row[i], rowtype, rhs[i], rhs[i]);
    }
    return 0;
}


static int
cpx_chgobj(glp_prob *glp, int sz, int *row /*1*/, double *obj)
{
    int i;
    for (i=0; i<sz; ++i)
    {
        glp_set_obj_coef(glp, row[i], obj[i]);
    }
    return 0;
}


static inline void
cpx_chgobjsen(glp_prob *glp, int sense)
{
    glp_set_obj_dir(glp, sense==SENSE_MIN ? GLP_MIN : GLP_MAX);
}


/*
 * CAUTION: input arrays are 0-based, but column indexes still 1-based!
 */
static int
cpx_chgctype(glp_prob *glp, int sz, int *col /*1*/, char *ctypes)
{
    int i;
    for (i=0; i<sz; ++i)
    {
        glp_set_col_kind(glp, col[i],
                ctypes[i]=='B' ? GLP_BV : ctypes[i] == 'I' ? GLP_IV : GLP_CV);
    }
    return 0;
}


/* all input arrays 0-based [0..nc-1] */
static int
cpx_addcols(glp_prob *glp,
        int nc, int nnz, double *obj,
        int *matbeg, int *matind /*1*/, double *matval,
        double *lb, double *ub)
{
    int i,j;
    j = glp_add_cols(glp, nc);
    for(i=0; i<nc; ++i,++j)
    {
        int k = matbeg[i];
        cpx_setbds(glp, j, lb[i], ub[i]);
        glp_set_mat_col(glp, j, matbeg[i+1]-k, &matind[k-1], &matval[k-1]);
        glp_set_obj_coef(glp, j, obj[i]);
    }
    if (matbeg[j] > nnz) return 1;
    return 0;
}


/* all input arrays 0-based [0..nc-1] */
static int
cpx_addrows(glp_prob *glp,
        int nr, int nnz, double *rhs, char *sense,
        int *matbeg, int *matind /*1*/, double *matval)
{
    int i,j;
    if (!nr) return 0;
    i = glp_add_rows(glp, nr);
    for(j=0; j<nr; ++i,++j)
    {
        int k = matbeg[j];
        glp_set_row_bnds(glp, i, sense[j], rhs[j], rhs[j]);
        glp_set_mat_row(glp, i, matbeg[j+1]-k, &matind[k-1], &matval[k-1]);
    }
    if (matbeg[j] > nnz) return 1;
    return 0;
}


static inline int
cpx_delrangeofrows(lp_desc *lpd, int from /*1*/, int to /*1*/)
{
    _grow_numbers_array(lpd, to);
    glp_del_rows(lpd->lp, to-from, &lpd->numbers[from-1]);
    return 0;
}


static inline int
cpx_delrangeofcols(lp_desc *lpd, int from /*1*/, int to /*1*/)
{
    _grow_numbers_array(lpd, to);
    glp_del_cols(lpd->lp, to-from, &lpd->numbers[from-1]);
    return 0;
}


static int
cpx_chgname(glp_prob *glp, int which, int i /*1*/, const char *name, int length)
{
    if (which == 'c')
        glp_set_col_name(glp, i, name);
    else if (which == 'r')
        glp_set_row_name(glp, i, name);
    else return 1;
    return 0;
}


static inline int
cpx_addsos(glp_prob *glp, int nsos, int nsosnz, sostype_t *sostype, int *sosbeg, int *sosind, double *sosref)
{
    return -1;
}


static int
cpx_delsos(lp_desc *lpd, int from, int to)
{
    return -1;
}


static int
cpx_loadbasis(glp_prob *glp, int nc, int nr, int* cbase, int* rbase)
{
    int i,j;
    for (j=0; j<nc; ++j)
        glp_set_row_stat(glp, j+1, cbase[j]);
    for (i=0; i<nr; ++i)
        glp_set_col_stat(glp, i+1, rbase[i]);
    return 0;
}


static inline int
cpx_chgprobtype(CPXLPptr glp, int type)
{
    return 0;   /* not needed */
}


static inline int
cpx_loadorder(CPXLPptr glp, int i, int *idx, int *prio, int *bdir)
{
    return -1;
}


static inline int
cpx_chgqpcoef(CPXLPptr glp, int i, int j, double val)
{
    return -1;
}


/* -------------------- Problem creation -------------------- */

static int cpx_getrhs(glp_prob *glp, double *rhs, int i /*1*/);

static void
_glp_get_solutions(lp_desc *lpd, int with_col_prim)
{
    int i,j;
    struct lp_sol *sol = &lpd->sol;

    /* columns/variables */
    if (with_col_prim && sol->sols)
        for (j=0; j<lpd->mac; ++j)
            sol->sols[j] = glp_get_col_prim(lpd->lp, j+1);
    if (sol->djs)
        for (j=0; j<lpd->mac; ++j)
            sol->djs[j] = glp_get_col_dual(lpd->lp, j+1);
    if (sol->cbase)
        for (j=0; j<lpd->mac; ++j)
            sol->cbase[j] = glp_get_col_stat(lpd->lp, j+1);

    /* rows/constraints */
    if (sol->slacks)
        for (i=0; i<lpd->mar; ++i)
        {
            double rhs;
            cpx_getrhs(lpd->lp, &rhs, i+1);
            sol->slacks[i] = rhs - glp_get_row_prim(lpd->lp, i+1);
        }
    if (sol->pis)
        for (i=0; i<lpd->mar; ++i)
            sol->pis[i] = glp_get_row_dual(lpd->lp, i+1);
    if (sol->rbase)
        for (i=0; i<lpd->mar; ++i)
            sol->rbase[i] = glp_get_row_stat(lpd->lp, i+1);
}


static void
_glp_callback(glp_tree *T, void *lpd)
{
    switch (glp_ios_reason(T))
    {
        case GLP_IBRANCH:       /* request for branching */
            {
                /* Implement termination at node_limit */
                int a_cnt, n_cnt, t_cnt;
                glp_ios_tree_size(T, &a_cnt, &n_cnt, &t_cnt);
                if (t_cnt > ((lp_desc*)lpd)->params.node_limit)
                    glp_ios_terminate(T);
            }
            break;

        case GLP_IBINGO:        /* better integer solution found */
            _glp_get_solutions((lp_desc*)lpd, 0);
            break;
    }
}


static int
cpx_loadprob(lp_desc *lpd)
{
    int i,j;

    lpd->lp = glp_create_prob();
    if (!lpd->lp)
        return 1;

    lpd->params = *cpx_env;             /* copy global parameters */
    lpd->params.sm.presolve = lpd->presolve ? GLP_ON : GLP_OFF; /*TODO*/
    lpd->params.io.presolve = lpd->presolve ? GLP_ON : GLP_OFF; /*TODO*/
    lpd->params.io.cb_func = _glp_callback;
    lpd->params.io.cb_info = lpd;

    glp_set_prob_name(lpd->lp, "eclipse");
    glp_set_obj_dir(lpd->lp, lpd->sense==SENSE_MIN ? GLP_MIN : GLP_MAX);
    glp_set_obj_coef(lpd->lp, 0, 0.0);  /* objective constant component */
    if (lpd->mar)
        glp_add_rows(lpd->lp, lpd->mar);
    if (lpd->mac)
        glp_add_cols(lpd->lp, lpd->mac);

    for(j=1; j<=lpd->mac; ++j)          /* columns */
    {
        cpx_setbds(lpd->lp, j, lpd->bdl[j-1], lpd->bdu[j-1]);
        glp_set_col_kind(lpd->lp, j, lpd->ctype[j-1]=='B' ? GLP_BV : lpd->ctype[j-1]=='I' ? GLP_IV : GLP_CV);
        glp_set_obj_coef(lpd->lp, j, lpd->objx[j-1]);
    }

    for(i=1; i<=lpd->mar; ++i)          /* rows */
    {
        glp_set_row_bnds(lpd->lp, i, lpd->senx[i-1], lpd->rhsx[i-1],  lpd->rhsx[i-1]);
    }

    for(j=0; j<lpd->mac; ++j)           /* coefficients */
    {
        i = lpd->matbeg[j]-1;           /* point at array[-1] */
        glp_set_mat_col(lpd->lp, j+1, lpd->matcnt[j], &lpd->matind[i], &lpd->matval[i]);
    }

    if (lpd->nsos)                      /* SOS not supported */
        return -1;

    return 0;
}


static inline void
cpx_freeprob(lp_desc *lpd)
{
    if (lpd->lp)
        glp_delete_prob(lpd->lp);
}


/* -------------------- Retrieve problem data -------------------- */

static int
cpx_getrhs(glp_prob *glp, double *rhs, int i /*1*/)
{
    int rowtype = glp_get_row_type(glp, i);
    if (rowtype == GLP_LO  ||  rowtype == GLP_FX)
        *rhs = glp_get_row_lb(glp, i);
    else if (rowtype == GLP_UP)
        *rhs = glp_get_row_ub(glp, i);
    else
        return 1;       /* GLP_FR, GLP_DB should not occur */
    return 0;
}


static inline int
cpx_getsense(glp_prob *glp, char *sense, int i /*1*/)
{
    /* should return SOLVER_SENSE_{LE,GE,EQ}, i.e. GLP_UP,GLP_LO,GLP_FX */
    *sense = glp_get_row_type(glp, i);
    return 0;
}


static inline int
cpx_getlb(glp_prob *glp, double *bd, int j /*1*/)
{
    *bd = glp_get_col_lb(glp, j);
    return 0;
}


static inline int
cpx_getub(glp_prob *glp, double *bd, int j /*1*/)
{
    *bd = glp_get_col_ub(glp, j);
    return 0;
}


static inline int
cpx_getbds(glp_prob *glp, double *lb, double *ub, int j /*1*/)
{
    *lb = glp_get_col_lb(glp, j);
    *ub = glp_get_col_ub(glp, j);
    return 0;
}


static inline int
cpx_getctype(glp_prob *glp, char *kind, int j /*1*/)
{
    switch(glp_get_col_kind(glp, j))
    {
        case GLP_CV: *kind = 'C'; return 0;
        case GLP_IV: *kind = 'I'; return 0;
        case GLP_BV: *kind = 'B'; return 0;
    }
    return 1;
}


static inline int
cpx_get_obj_coef(glp_prob *glp, double *bd, int j /*1*/)
{
    *bd = glp_get_obj_coef(glp, j);
    return 0;
}


/*
 * Get one row's coefficients.
 * Caution: this _returns_ 1-based column indices!
 */
static inline int
cpx_getrow(glp_prob *glp, int *nnz, int *matind /*1*/, double *matval /*1*/, int nnz_sz, int i /*1*/)
{
    *nnz = glp_get_mat_row(glp, i, matind-1, matval-1);
    return 0;
}


static inline int
cpx_getnumnz(glp_prob *glp)
{
    return glp_get_num_nz(glp);
}


static inline int
cpx_getnumint(glp_prob *glp)
{
    return glp_get_num_int(glp) - glp_get_num_bin(glp);
}


static inline int
cpx_getnumbin(glp_prob *glp)
{
    return glp_get_num_bin(glp);
}


static inline int
cpx_getnumqpnz(glp_prob *glp)
{
    return 0;
}


/* -------------------- Solving -------------------- */

static int
solver_has_method(int m) {
    switch (m) {
    case METHOD_AUTO:
    case METHOD_BAR:
    case METHOD_DEFAULT:
    case METHOD_PRIMAL:
    case METHOD_DUAL:
	return 1;
    default:
	return 0;
    }
}

static int
solver_has_node_method(int m) {
    switch (m) {
    case METHOD_DEFAULT:
	return 1;
    default:
	return 0;
    }
}


static int
cpx_prepare_solve(lp_desc* lpd, struct lp_meth *meth, double timeout)
{
    int timeout_ms = timeout>0.0 ? (int)(timeout*1000) : INT_MAX;

    /* catch unsupported problem type, and set correct time limit */
    switch(lpd->prob_type)
    {
        case PROBLEM_LP:
            if (meth->meth == METHOD_BAR) {
                lpd->params.ip.msg_lev = lpd->params.msg_lev;
            } else {
                lpd->params.sm.tm_lim = timeout_ms;
                lpd->params.sm.msg_lev = lpd->params.msg_lev;
                lpd->params.sm.out_frq = lpd->params.out_frq;
                lpd->params.sm.out_dly = lpd->params.out_dly;
            }
            break;

        case PROBLEM_RELAXEDL:
            lpd->params.sm.tm_lim = timeout_ms;
            lpd->params.sm.msg_lev = lpd->params.msg_lev;
            lpd->params.sm.out_frq = lpd->params.out_frq;
            lpd->params.sm.out_dly = lpd->params.out_dly;
            break;

        case PROBLEM_MIP:
            lpd->params.io.tm_lim = timeout_ms;
            lpd->params.io.msg_lev = lpd->params.msg_lev;
            lpd->params.io.out_frq = lpd->params.out_frq;
            lpd->params.io.out_dly = lpd->params.out_dly;

            lpd->params.sm.tm_lim = INT_MAX;    /* not sure about this */
            lpd->params.sm.msg_lev = lpd->params.msg_lev;
            lpd->params.sm.out_frq = lpd->params.out_frq;
            lpd->params.sm.out_dly = lpd->params.out_dly;
            break;

        default:
            return -1;
    }

    switch(meth->meth)          /* LP or root method */
    {
        case METHOD_AUTO:	lpd->params.sm.meth = GLP_DUALP; break;
        case METHOD_PRIMAL:	lpd->params.sm.meth = GLP_PRIMAL; break;
        case METHOD_DUAL:	lpd->params.sm.meth = GLP_DUAL; break;
        default:	        _warn("Unsupported solver method, using default.");
        case METHOD_DEFAULT:	break;
    }

    switch(meth->node_meth)     /* MIP node method */
    {
        default:	        _warn("Unsupported node_method, using default.");
        case METHOD_DEFAULT:	break;
    }
    return 0;
}


static int
cpx_solve(lp_desc* lpd, struct lp_meth *meth, double* bestbound, double* worstbound)
{
    int res, summary;
    int root_solved = 0;
    struct lp_sol *sol = &lpd->sol;

    switch (lpd->prob_type)
    {
        case PROBLEM_LP:
        case PROBLEM_RELAXEDL:
            res = glp_simplex(lpd->lp, &lpd->params.sm);
            lpd->sol_state = glp_get_status(lpd->lp);
            break;

        case PROBLEM_MIP:
            res = glp_simplex(lpd->lp, &lpd->params.sm);
            if (res==0)
            {
                root_solved = 1;
                res = glp_intopt(lpd->lp, &lpd->params.io);
                lpd->sol_state = glp_mip_status(lpd->lp);
            } else {
                lpd->sol_state = glp_get_status(lpd->lp);
            }
            break;

        default:
            return -1;
    }
    /*
    Fprintf(Current_Output, "Solver result= %d, status = %d", res, lpd->sol_state);
    ec_newline(Current_Output);
    */

    switch(res)
    {
        case 0:
        case GLP_EITLIM:
        case GLP_ETMLIM:
            break;
        case GLP_EOBJLL:
        case GLP_EOBJUL:
            assert(lpd->sol_state == GLP_UNBND);
            break;
        case GLP_ENODFS:       /* presolver detected failure */
        case GLP_ENOPFS:       /* presolver detected failure */
            assert(lpd->sol_state == GLP_UNDEF);
            lpd->sol_state = GLP_NOFEAS;  /* treat like infeasibility */
            break;
        default:
            lpd->sol_state == GLP_UNDEF;  /* if not already */
            Fprintf(solver_streams[WrnType], "GLPK solver error %d.", res);
            ec_flush(solver_streams[WrnType]);
            break;
    }

    switch(lpd->sol_state)
    {
        case GLP_OPT:   /* (smp,ipt,mip) */
	    lpd->optimum_ctr++;
	    lpd->descr_state = DESCR_SOLVED_SOL;
            if (lpd->prob_type == PROBLEM_MIP)
                lpd->objval = glp_mip_obj_val(lpd->lp);
            else if (meth->meth == METHOD_BAR)
                lpd->objval = glp_ipt_obj_val(lpd->lp);
            else
                lpd->objval = glp_get_obj_val(lpd->lp);
	    *worstbound = *bestbound = lpd->objval;
            break;

        case GLP_NOFEAS:        /* no solution exists (smp,ipt,mip) */
	    lpd->infeas_ctr++;
	    lpd->descr_state = DESCR_SOLVED_NOSOL;
	    lpd->objval = 0.0;
	    *worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	    *bestbound = (lpd->sense ==  SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
            break;

        case GLP_FEAS:  /* suboptimal (smp,mip) */
	    lpd->abort_ctr++;
	    lpd->descr_state = DESCR_ABORTED_SOL;
            if (lpd->prob_type == PROBLEM_MIP) {
                *worstbound = lpd->objval = glp_mip_obj_val(lpd->lp);
                *bestbound = glp_get_obj_val(lpd->lp);  /* use LP solution as bound */
            } else if (meth->meth == METHOD_BAR) {
                *worstbound = lpd->objval = glp_ipt_obj_val(lpd->lp);
                *bestbound = -HUGE_VAL;     /* ? */
                *bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL : HUGE_VAL);
            } else {
                *worstbound = lpd->objval = glp_get_obj_val(lpd->lp);
                *bestbound = (lpd->sense ==  SENSE_MIN ? -HUGE_VAL : HUGE_VAL);
            }
            break;

        case GLP_INFEAS:        /* no solution found (smp,ipt) */
	    lpd->infeas_ctr++;
	    lpd->descr_state = DESCR_ABORTED_NOSOL;
	    lpd->objval = 0.0;
	    *worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	    *bestbound = (lpd->sense ==  SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
            break;

        case GLP_UNBND:         /* smp */
	    lpd->abort_ctr++;
	    lpd->descr_state = DESCR_UNBOUNDED_NOSOL;
            *bestbound = *worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL : HUGE_VAL);
            break;

        case GLP_UNDEF:         /* smp,ipt,mip */
	    lpd->abort_ctr++;
	    lpd->descr_state = DESCR_UNKNOWN_NOSOL;
	    *worstbound = (lpd->sense == SENSE_MIN ? -HUGE_VAL :  HUGE_VAL);
	    *bestbound = (lpd->sense ==  SENSE_MIN ?  HUGE_VAL : -HUGE_VAL);
            break;

        default:
            return 1;
    }
    return 0;
}


static int
cpx_get_soln_state(lp_desc* lpd)
{
    if (lpd->prob_type == PROBLEM_MIP)
    {
        int j;
        if (lpd->sol.sols)
            for (j=0; j<lpd->mac; ++j)
                lpd->sol.sols[j] = glp_mip_col_val(lpd->lp, j+1);

        /* rest was already retrieved in callback! */
    }
    else
    {
        _glp_get_solutions(lpd, 1);
    }
    return 0;
}


/* -------------------- Read/Write -------------------- */

static int
cpx_read(lp_desc *lpd, char *file, char *fmt)
{
    glp_prob *glp;
    char buf[PATH_MAX];

    /* append .fmt suffix if plain file doesn't exist */
    if (access(file, R_OK))
    {
        int fmax = PATH_MAX-1-strlen(fmt)-1;
        if (fmax<0) fmax=0;
        buf[0] = 0;
        file = strcat(strcat(strncat(buf,file,fmax),"."),fmt);
    }

    if (!strcmp(fmt, "mps"))
    {
        glp = glp_create_prob();
        if (glp_read_mps(glp, GLP_MPS_DECK, NULL, file))
            return 1;
    }
    else if (!strcmp(fmt, "lp"))
    {
        glp = glp_create_prob();
        if (glp_read_lp(glp, NULL, file))
            return 1;
    }
    else
    {
        glp_printf("GLPK: Unknown format \"%s\"\n", fmt);
        return 1;
    }

    /* initialize non-zero fields in lp_desc */
    lpd->lpcopy = lpd->lp;  /* no need for a copy */
    lpd->sense = glp_get_obj_dir(glp) == GLP_MIN ? SENSE_MIN : SENSE_MAX;
    lpd->mac = glp_get_num_cols(glp);
    lpd->mar = glp_get_num_rows(glp);
    lpd->prob_type = glp_get_num_int(glp)>0 ? PROBLEM_MIP : PROBLEM_LP;
    lpd->params = *cpx_env;             /* init parameters */
    lpd->lp = glp;

    return 0;
}


static int
cpx_write(lp_desc *lpd, char *file, char *fmt)
{
    if (!strcmp(fmt, "mps"))
    {
        if (glp_write_mps(lpd->lp, GLP_MPS_DECK, NULL, file))
            return 1;
    }
    else if (!strcmp(fmt, "lp"))
    {
        if (glp_write_lp(lpd->lp, NULL, file))
            return 1;
    }
    else
    {
        glp_printf("GLPK: Unknown format \"%s\"\n", fmt);
        return -1;
    }
    return 0;
}


/* -------------------- Parameter handling -------------------- */

/*
 * Parameter Table
 *
 * defines a table params[] which maps the symbolic names of the optimizer
 * flags to our internal numbers.  The routines cpx_set_xxx_param()
 * map the numbers to fields in the parameter structures.
 *
 * For a new version, check the GLPK manual and possible glpk.h
 * for new parameters.
 *
 * Mark the int params with 0, the doubles with 1, the strings with 2
 * count the lines and define NUMPARAMS accordingly!
 * Use #if's for version differences.
 */

#define EXTRA_PAR_MSG_LEV       1001
#define EXTRA_PAR_NODE_LIMIT    1002
#define EXTRA_PAR_OUT_FRQ       1003
#define EXTRA_PAR_OUT_DLY       1004

#define NUMALIASES 10

#define NUMPARAMS 25
static struct param_desc params[NUMPARAMS+NUMALIASES] = {

{"meth",	2,	0},	/* the fields of glp_smcp{} */
{"pricing",	3,	0},
{"r_test",	4,	0},
{"tol_bnd",	5,	1},
{"tol_dj",	6,	1},
{"tol_piv",	7,	1},
{"obj_ll",	8,	1},
{"obj_ul",	9,	1},
{"it_lim",	10,	0},
{"ord_alg",	52,	0},	/* the fields of glp_iptcp{} */
{"br_tech",	102,	0},	/* the fields of glp_iocp{} */
{"bt_tech",	103,	0},
{"tol_int",	104,	1},
{"tol_obj",	105,	1},
{"pp_tech",	112,	0},
{"mip_gap",	113,	1},
{"mir_cuts",	114,	0},
{"gmi_cuts",	115,	0},
{"cov_cuts",	116,	0},
{"clq_cuts",	117,	0},
{"binarize",	119,	0},
{"fp_heur",	120,	0},
{"ps_heur",	121,	0},
{"ps_tm_lim",122,	0},
{"sr_heur",	123,	0},

/* ambiguous settings, which we replace with aliases
{"msg_lev",	1,	0},
{"tm_lim",	11,	0},
{"out_frq",	12,	0},
{"out_dly",	13,	0},
{"presolve",	14,	0},
{"msg_lev",	51,	0},
{"msg_lev",	101,	0},
{"tm_lim",	106,	0},
{"out_frq",	107,	0},
{"out_dly",	108,	0},
{"presolve",	118,	0},
*/

/*
 * Add some version-independent aliases to the table
 * This must remain at the end of the table!!!
 * If you add lines here, update NUMALIASES above!
 * NUMALIASES lines follow
 */

{"lowerobj_limit",	8,	1},
{"upperobj_limit",	9,	1},
{"feasibility_tol",	5,	1},     /* used in eplex.ecl! */
{"mipgap",		113,	1},
{"integrality",		104,	1},     /* used in eplex.ecl! */
{"iteration_limit",	10,	0},
{"msg_lev",	        EXTRA_PAR_MSG_LEV,	0},     /* all msg_lev */
{"out_frq",	        EXTRA_PAR_OUT_FRQ,	0},     /* all out_frq */
{"out_dly",	        EXTRA_PAR_OUT_DLY,	0},     /* all out_dly */
{"node_limit",	        EXTRA_PAR_NODE_LIMIT,	0},     /* simulated */

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


static int *
_int_param(glp_env *params, int parnum)
{
    switch(parnum)
    {
        case 1:	  return &params->sm.msg_lev;
        case 2:	  return &params->sm.meth;
        case 3:	  return &params->sm.pricing;
        case 4:	  return &params->sm.r_test;
        case 10:  return &params->sm.it_lim;
        case 11:  return &params->sm.tm_lim;
        case 12:  return &params->sm.out_frq;
        case 13:  return &params->sm.out_dly;
        case 14:  return &params->sm.presolve;

        case 51:  return &params->ip.msg_lev;
        case 52:  return &params->ip.ord_alg;

        case 101: return &params->io.msg_lev;
        case 102: return &params->io.br_tech;
        case 103: return &params->io.bt_tech;
        case 106: return &params->io.tm_lim;
        case 107: return &params->io.out_frq;
        case 108: return &params->io.out_dly;
        case 111: return &params->io.cb_size;
        case 112: return &params->io.pp_tech;
        case 114: return &params->io.mir_cuts;
        case 115: return &params->io.gmi_cuts;
        case 116: return &params->io.cov_cuts;
        case 117: return &params->io.clq_cuts;
        case 118: return &params->io.presolve;
        case 119: return &params->io.binarize;
        case 120: return &params->io.fp_heur;
        case 121: return &params->io.ps_heur;
        case 122: return &params->io.ps_tm_lim;
        case 123: return &params->io.sr_heur;
        default: return NULL;
    }
}

static double *
_dbl_param(glp_env *params, int parnum)
{
    switch(parnum)
    {
        case 5:	  return &params->sm.tol_bnd;
        case 6:	  return &params->sm.tol_dj;
        case 7:	  return &params->sm.tol_piv;
        case 8:	  return &params->sm.obj_ll;
        case 9:	  return &params->sm.obj_ul;
        case 104: return &params->io.tol_int;
        case 105: return &params->io.tol_obj;
        case 113: return &params->io.mip_gap;
        default: return NULL;
    }
}


static inline int
cpx_set_int_param(lp_desc *lpd, int parnum, int val)
{
    glp_env *env = lpd ? &lpd->params : cpx_env;
    int *ppar;
    switch(parnum) {
        case EXTRA_PAR_MSG_LEV: env->msg_lev = val; return 0;
        case EXTRA_PAR_OUT_FRQ: env->out_frq = val; return 0;
        case EXTRA_PAR_OUT_DLY: env->out_dly = val; return 0;
        case EXTRA_PAR_NODE_LIMIT: env->node_limit = val<0 ? 0 : val; return 0;
    }
    ppar = _int_param(env, parnum);
    if (!ppar) return 1;
    *ppar = val;
    return 0;
}

static inline int
cpx_set_dbl_param(lp_desc *lpd, int parnum, double val)
{
    double *ppar = _dbl_param(lpd ? &lpd->params : cpx_env, parnum);
    if (!ppar) return 1;
    *ppar = val;
    return 0;
}

static inline int
cpx_set_str_param(lp_desc *lpd, int parnum, const char *val)
{
    return 1;
}

static inline int
cpx_get_int_param(lp_desc *lpd, int parnum, int *pval)
{
    glp_env *env = lpd ? &lpd->params : cpx_env;
    int *ppar;
    switch(parnum) {
        case EXTRA_PAR_MSG_LEV: *pval = env->msg_lev; return 0;
        case EXTRA_PAR_OUT_FRQ: *pval = env->out_frq; return 0;
        case EXTRA_PAR_OUT_DLY: *pval = env->out_dly; return 0;
        case EXTRA_PAR_NODE_LIMIT: *pval = env->node_limit; return 0;
    }
    ppar = _int_param(env, parnum);
    if (!ppar) return 1;
    *pval = *ppar;
    return 0;
}

static inline int
cpx_get_dbl_param(lp_desc *lpd, int parnum, double *pval)
{
    double *ppar = _dbl_param(lpd ? &lpd->params : cpx_env, parnum);
    if (!ppar) return 1;
    *pval = *ppar;
    return 0;
}

static inline int
cpx_get_str_param(lp_desc *lpd, int parnum, char *pval)
{
    return 1;
}

static inline double
cpx_feasibility_tol(lp_desc *lpd)
{
    return lpd->params.sm.tol_bnd;
}
