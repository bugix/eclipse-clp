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
 * Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, Kish Shen and Andrew Eremin, IC-Parc
 * 
 * END LICENSE BLOCK */
/*
 * ECLiPSe / CPLEX interface
 *
 * System:	ECLiPSe Constraint Logic Programming System
 * Author/s:	Joachim Schimpf, IC-Parc
 *              Kish Shen,       IC-Parc
 * Version:	$Id: eplex.c,v 1.20 2016/11/09 13:16:21 jschimpf Exp $
 *
 */

/*#define LOG_CALLS*/
#undef LOG_CALLS
#ifdef LOG_CALLS
int log_ctr = 0;
#endif

#ifndef __STDC__
typedef unsigned size_t;
char *getenv();
#else
#include <stdlib.h>
#endif
#include <string.h>

#include <unistd.h>

# include <stdio.h>
# include <ctype.h>

#ifdef WIN32
# include <process.h>
# define unlink(File) _unlink(File)
# define getpid()   _getpid()
# define gethostid()  0
# define putenv(s) _putenv(s)
# define PATH_SEPARATOR ";"
#else
# define PATH_SEPARATOR ":"
#endif


/* Kish 2008-08-13:
   Cannot define __eprintf() here for PPC Mac OS X -- at least version 10.4
   we have access to. It is defined in stdc++, and Mac OS X does not allow 
   multiple definitions of symbols during linking (for flat_namespace). 
   versions, 

   Kish 2008-07-24: define __eprintf() for all cases -- Cisco lab Solaris
   has older libraries that does not have __eprintf() defined

   code modified from koders.com's definition of __eprintf(), which uses
   fiprintf(), which is also undefined for Intel MacOSX. Here eprintf()
   is redefined to just the abort. This should be OK, as it is used in 
   assert.h, which should only be used when debugging

-- Kish Shen 2007-11-22

   This is an implementation of the __eprintf function which is
   compatible with the assert.h which is distributed with gcc.

   This function is provided because in some cases libgcc.a will not
   provide __eprintf.  This will happen if inhibit_libc is defined,
   which is done because at the time that libgcc2.c is compiled, the
   correct <stdio.h> may not be available.  newlib provides its own
   copy of assert.h, which calls __assert, not __eprintf.  However, in
   some cases you may accidentally wind up compiling with the gcc
   assert.h.  In such a case, this __eprintf will be used if there
   does not happen to be one in libgcc2.c.  */

#if !(defined(__APPLE__) && defined(__ppc__))

void
__eprintf (format, file, line, expression)
     const char *format;
     const char *file;
     unsigned int line;
     const char *expression;
{
/* (void) fiprintf (stderr, format, file, line, expression);*/
  abort ();
 }

#endif


# include <limits.h>
# include <float.h>
# include <math.h>

#if defined(LOG_CALLS) 
# define USE_PROBLEM_ARRAY
#endif


/*
 * Macros to make code more readable 
 */

/* this extra step is needed to allow Call itself to be transformed */
#define Transform_Quoted(Item) Add_Quotes(Item)
#define Add_Quotes(Item) #Item

/* Call logging macros:
     Call(Ret, Call)   Log and call Call if LOG_CALLS, assign return value to Ret 
     CallN(Call)       Log and call Call if LOG_CALLS, return value is lost

Log only macros (these should be accompanied by an actual call to the
logged call!)

     Log1(Call, A1)...Log6(Call, A1,A2,A3,A4,A5,A6) 
                       Log Call if LOG_CALLS. Call should be in printf
                       form, with appropriate % arguments for the arguments
*/   
#ifdef LOG_CALLS 
# define Call(Err, C) { \
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";");\
    ec_flush(log_output_); \
    Err = C;\
}

# define CallN(C) { \
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";");\
    ec_flush(log_output_); \
    C;\
}

# define Log0(C) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";");\
    ec_flush(log_output_);\
}

# define Log1(C,A1) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";",A1);\
    ec_flush(log_output_);\
}

# define Log2(C,A1,A2) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";",A1,A2);\
    ec_flush(log_output_);\
}

# define Log3(C,A1,A2,A3) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";",A1,A2,A3);\
    ec_flush(log_output_);\
}

# define Log4(C,A1,A2,A3,A4) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";",A1,A2,A3,A4); \
    ec_flush(log_output_); \
}

# define Log5(C,A1,A2,A3,A4,A5) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";",A1,A2,A3,A4,A5); \
    ec_flush(log_output_); \
}

# define Log6(C,A1,A2,A3,A4,A5,A6) {\
    Fprintf(log_output_,  "\n\
	"Transform_Quoted(C)";",A1,A2,A3,A4,A5,A6); \
    ec_flush(log_output_); \
}

#else
# define Call(Err, C) {Err = C;}
# define CallN(C) C
# define Log0(C)
# define Log1(C,A1)
# define Log2(C,A1,A2)
# define Log3(C,A1,A2,A3)
# define Log4(C,A1,A2,A3,A4)
# define Log5(C,A1,A2,A3,A4,A5)
# define Log6(C,A1,A2,A3,A4,A5,A6)
#endif


/*
 * ECLiPSe declarations
 */

#include "external.h"


#if defined(WIN32) && defined(LOG_CALLS)
/* must be after include of external.h to avoid redefining log_output_ there 
   Windows workaround for log_output_ not exported in eclipse.def
*/
# define log_output_ ec_stream_id(ec_stream_nr("log_output"))
#endif

/* should be used only if v,t is a number */
#define DoubleVal(v, t) ( IsInteger(t) ? (double) (v).nint : \
			  IsDouble(t) ? Dbl(v) : coerce_to_double(ec_eng,v,t) )

#define Check_Constant_Range(x) \
	{if ((x) < -CPX_INFBOUND || (x) > CPX_INFBOUND) {Bip_Error(RANGE_ERROR);}}


/*
 * LpDescOnly can be used if we only want to access
 * fields within lp_desc, not in the external solver
 * this made a difference in old Xpress, but now there
 * is a difference only when logging calls
 */
#define LpDescOnly(vlp, tlp, lpd) Get_Typed_Object(vlp, tlp, &lp_handle_tid, lpd)

#ifdef LOG_CALLS
static int next_matno = 0, current_matno = -1;

# define LpDesc(vlp, tlp, lpd) {                        \
    Get_Typed_Object(vlp, tlp, &lp_handle_tid, (lpd));  \
    if ((lpd)->matno != current_matno)                  \
    {                                                   \
	Fprintf(log_output_, "\n\
	lpd = (lp_desc *) lpdmat[%d];", (lpd)->matno);  \
	ec_flush(log_output_);                          \
	current_matno = (lpd)->matno;                   \
    }                                                   \
}

#else
# define LpDesc(vlp, tlp, lpd) LpDescOnly(vlp, tlp, lpd)
#endif


#define IsIDArray(t) IsString(t)
#define Check_Array(t) Check_String(t)
#define IArrayStart(pw) ((int *)BufferStart(pw))
#define DArrayStart(pw) ((double *)BufferStart(pw))
#define CArrayStart(pw) ((char *)BufferStart(pw))
#define Return_Unify_Array(v,t,a) Return_Unify_String(v,t,a)
#define DArraySize(pbuf) ((BufferSize(pbuf) - 1) / sizeof(double))
#define IArraySize(pbuf) ((BufferSize(pbuf) - 1) / sizeof(int))

static pword * _create_carray(ec_eng_t*,int);
static pword * _create_darray(ec_eng_t*,int);
static pword * _create_iarray(ec_eng_t*,int);


/*
 * Solver-independent constants
 */

/* argument indices in Prolog-level prob-handle */
#define HANDLE_CPH		1	/* C-level handle */
#define HANDLE_STAMP		2	/* timestamp for prob-handle */
#define HANDLE_M_METH		0	/* Outputs: offsets from meth-field */
#define HANDLE_M_AUXMETH	1
#define HANDLE_M_NODEMETH	2
#define HANDLE_M_NODEAUXMETH	3
#define HANDLE_S_SOLS		0	/* Outputs: offsets from sols-field */
#define HANDLE_S_PIS		1
#define HANDLE_S_SLACKS		2
#define HANDLE_S_DJS		3
#define HANDLE_S_CBASE		4
#define HANDLE_S_RBASE		5
#define HANDLE_S_CPCM		6
#define HANDLE_S_IISR		7
#define HANDLE_S_IISC		8
#define HANDLE_S_IISCS		9

#define COL_STAMP       1       /* timestamp for a column (in attribute) */


#define DESCR_EMPTY		0	/* problem descriptor state */
#define DESCR_LOADED		1
#define DESCR_SOLVED_SOL	2
#define DESCR_SOLVED_NOSOL	3
#define DESCR_ABORTED_SOL	4
#define DESCR_ABORTED_NOSOL	5
#define DESCR_UNBOUNDED_NOSOL	6
#define DESCR_UNKNOWN_NOSOL	7

#define CSTR_TYPE_NORM          0 /* correspond to constraint_type_code/2 */
#define CSTR_TYPE_PERMCP        1
#define CSTR_TYPE_CONDCP        2

#define CSTR_STATE_NOTADDED    -1
#define CSTR_STATE_VIOLATED    -1
#define CSTR_STATE_SAT         -2
#define CSTR_STATE_BINDING     -3
#define CSTR_STATE_INVALID     -4
#define CSTR_STATE_INACTIVE    -5

#define MIPSTART_NONE		0
#define MIPSTART_ALL		1
#define MIPSTART_INT		2

#define CP_ACTIVE       1       /* correspond to cp_cond_code/2  */
#define CP_ADDINIT      2

#define NEWROW_INCR	60	/* default sizes for addrow arrays */
#define NEWNZ_INCR	510
#define NEWBD_INCR	510	/* arrays needed for changing bounds */
#define NEWCOL_INCR	1022	/* macsz arrays growth increment */
#define NEWSOS_INCR	32
#define CUTPOOL_INCR    10      /* number of cutpools increment */

#define RoundTo(n,unit) ((n) - ((n) - 1) % (unit) -1 + (unit))
#define Max(x,y)	((x)>(y)?(x):(y))

/* minimum number of words that Type would fit in */
#define NumberOfWords(Type) (1+(sizeof(Type)-1)/sizeof(word))


/*
 * Solver's starting row/column index, may be changed by subsequent includes
 */
#define IDX_OFFSET 0


/*
 * Include solver-specific declarations
 */
#ifdef CPLEX
#include "eplex_cplex.h"
#endif

#ifdef GUROBI
#include "eplex_gurobi.h"
#endif

#ifdef XPRESS
#include "eplex_xpress.h"
#endif

#ifdef COIN /* COIN based solvers */
#include "eplex_coin.h"
#endif

#ifdef GLPK
#include "eplex_glpk.h"
#endif


/* 
 * Problem handle
 */

/* Methods for lp_handle_tid */
static void _free_lp_handle(lp_desc *lpd);
static int _strsz_lp_handle(lp_desc *lpd, int quoted);
static int _tostr_lp_handle(lp_desc *lpd, char *buf, int quoted);

/* 2*sizeof(void *) for max. size for a printed address */
#define STRSZ_LP_HANDLE 2*sizeof(void *)+20
 
static int
_strsz_lp_handle(lp_desc *lpd, int quoted)
{
    return STRSZ_LP_HANDLE;
}

static int
_tostr_lp_handle(lp_desc *lpd, char *buf, int quoted)
{
    sprintf(buf, "'EPLEX_"Transform_Quoted(SOLVER_SHORT_NAME)"'(16'%" W_MOD "x)", (uword) lpd);
    return strlen(buf); /* size of actual string */
}

static dident d_eplex;

static dident
_kind_eplex()
{
    return d_eplex;
}

t_ext_type lp_handle_tid = {
    (void (*)(t_ext_ptr)) _free_lp_handle,  /* free */
    NULL,  /* copy */
    NULL,  /* mark_dids */
    (int (*)(t_ext_ptr,int)) _strsz_lp_handle,  /* string_size */
    (int (*)(t_ext_ptr,char *,int)) _tostr_lp_handle,  /* to_string */
    NULL,  /* equal */
    NULL,  /* remote_copy */
    NULL,  /* get */
    NULL,   /* set */
    _kind_eplex
};


typedef struct {
  int              oldmar, oldmac, oldsos, oldidc;
} untrail_data;
      
typedef struct {
  int              idx;
  char             ctype;
} untrail_ctype;

typedef struct {
    double bds[2];     /* bounds: lower, upper */
    int    idx;       /* index of column */
} untrail_bound;

typedef struct {
    int old_ptype;   /* old problem type */
} untrail_ptype;


/*
 * Global data
 */

/* ECLiPSe streams for 4 message types (log,result,warning,error) */
static stream_id solver_streams[4];

/* Atoms used to communicate with the Prolog level */
static dident d_le, d_ge, d_eq, d_optimizer, d_yes, d_no, d_colon2, d_empty, d_flag;

/* Global solver environment (!=0 when initialised) */
static CPXENVptr	cpx_env = (CPXENVptr) 0;


#ifdef CPLEX

static CPXCHANNELptr	cpxresults = (CPXCHANNELptr) 0;
static CPXCHANNELptr	cpxwarning = (CPXCHANNELptr) 0;
static CPXCHANNELptr	cpxerror = (CPXCHANNELptr) 0;
static CPXCHANNELptr	cpxlog = (CPXCHANNELptr) 0;

# if CPLEX >= 8
static void CPXPUBLIC eclipse_out(void *nst, const char*msg);
# else
static void CPXPUBLIC eclipse_out(void *nst, char*msg);
# endif
#endif /* CPLEX */


#ifdef XPRESS

/* Type of XPRESS library */
#define XP_OEM_UNKNOWN	-1
#define XP_OEM_NO	0
#define XP_OEM_YES	1
static int oem_xpress = XP_OEM_UNKNOWN;

static void XPRS_CC eclipse_out(XPRSprob prob, void *obj, const char *msg, int len, int msgtype);

#endif	/* XPRESS */


#if 0
void *
Malloc(size_t size)
{
    void *p;
    p = malloc(size);
    Fprintf(Current_Error, "%8x malloc(%d)\n", p, size);
    ec_flush(Current_Error);
    return p;
}

void *
Realloc(void *p, size_t size)
{
    Fprintf(Current_Error, "%8x realloc(%d)\n", p, size);
    ec_flush(Current_Error);
    return realloc(p, size);
}

void
Free(void *p)
{
    Fprintf(Current_Error, "%8x free\n", p);
    ec_flush(Current_Error);
    free(p);
}
#else
#ifdef USE_OWN_MALLOC

/* Eclipse's hp_alloc() can cause problems because of private heap limit */
#include "memman.h"
#define Malloc(size) hp_alloc(size)
#define Realloc(p, size) hp_resize(p, size)
#define Free(p) hp_free(p)

#else

#define Malloc(size) malloc(size)
#define Realloc(p, size) realloc(p, size)
#define Free(p) free(p)

#endif
#endif

/* free *p if it is pointing at something */
#define TryFree(p)  if (p) { CallN(Free(p)); p = NULL; } 


static void _grow_cb_arrays(lp_desc *, int);
static void _grow_numbers_array(lp_desc * lpd, int m);


/*
 * Include solver-specific code
 */
#ifdef CPLEX
#include "eplex_cplex.c"
#endif

#ifdef GUROBI
#include "eplex_gurobi.c"
#endif

#ifdef XPRESS
#include "eplex_xpress.c"
#endif

#ifdef COIN /* COIN based solvers */
#include "eplex_coin.c"
#endif

#ifdef GLPK
#include "eplex_glpk.c"
#endif


static double 
coerce_to_double(ec_eng_t *ec_eng, value vval, type tval)
{
    /* tval MUST be a number type */
    value buffer;

    tag_desc[TagType(tval)].coerce_to[TDBL](ec_eng, vval, &buffer);
    return Dbl(buffer);
}


int
p_cpx_cleanup(value vlp, type tlp, ec_eng_t *ec_eng)
{
    pword handle;
    handle.val.all = vlp.all;
    handle.tag.all = tlp.all;
    return ec_free_handle(handle, &lp_handle_tid);
}

static void
_free_lp_handle(lp_desc *lpd)
{
    if (lpd->descr_state != DESCR_EMPTY)
    {
        int i;

        cpx_freeprob(lpd);
	lpd->lp = NULL;

	TryFree(lpd->rhsx);
	TryFree(lpd->senx);
	TryFree(lpd->matbeg);
	TryFree(lpd->matcnt);
	TryFree(lpd->matind);
	TryFree(lpd->matval);
	TryFree(lpd->bdl);
	TryFree(lpd->bdu);
	TryFree(lpd->objx);
	TryFree(lpd->ctype);
	if (lpd->cb_sz)
	{
	    CallN(Free(lpd->cb_index));
	    TryFree(lpd->cb_index2);
	    CallN(Free(lpd->cb_value));
	}
	if (lpd->sossz)
	{
	    CallN(Free(lpd->sostype));
	    CallN(Free(lpd->sosbeg));
	    CallN(Free(lpd->sosind));
	    CallN(Free(lpd->sosref));
	}
	TryFree(lpd->rngval);
	TryFree(lpd->cname);
	TryFree(lpd->cstore);
	TryFree(lpd->rname);
	TryFree(lpd->rstore);
	TryFree(lpd->numbers);
	TryFree(lpd->zeroes);
	TryFree(lpd->dzeroes);
	if (lpd->nr_sz)
	{
	    CallN(Free(lpd->rmatbeg));
	    TryFree(lpd->rcompl);
	    TryFree(lpd->rindind);
	}
	if (lpd->nnz_sz)
	{
	    CallN(Free(lpd->rmatind));
	    CallN(Free(lpd->rmatval));
	}
/*
	if (lpd->cp_nr_sz)
	{
	    CallN(Free(lpd->cp_rmatbeg));
	    CallN(Free(lpd->cp_rhsx));
	    CallN(Free(lpd->cp_senx));
	}
	if (lpd->cp_nz_sz)
	{
	    CallN(Free(lpd->cp_rmatind));
	    CallN(Free(lpd->cp_rmatval));
	}
*/
	if (lpd->cp_nr_sz2)
	{
	    CallN(Free(lpd->cp_rmatbeg2));
	    CallN(Free(lpd->cp_rhsx2));
	    CallN(Free(lpd->cp_senx2));
	    CallN(Free(lpd->cp_active2));
	    CallN(Free(lpd->cp_initial_add2));
	    if (lpd->cp_nz_sz2)
	    {
		CallN(Free(lpd->cp_rmatind2));
		CallN(Free(lpd->cp_rmatval2));
	    }
	}

	for (i=0; i < lpd->cp_npools2; i++) { TryFree(lpd->cp_pools2[i]); }
	TryFree(lpd->cp_pools2);
	TryFree(lpd->cp_pools_max2);
	TryFree(lpd->cp_pools_sz2);
    }
#ifdef CPLEX
    CPXflushchannel (cpx_env, cpxresults);
    CPXflushchannel (cpx_env, cpxlog);
    CPXflushchannel (cpx_env, cpxerror);
    CPXflushchannel (cpx_env, cpxwarning);
#else
    (void) ec_flush(solver_streams[LogType]);
    (void) ec_flush(solver_streams[ResType]);
    (void) ec_flush(solver_streams[WrnType]);
    (void) ec_flush(solver_streams[ErrType]);
    
#endif
    CallN(Free(lpd));
}




/*
 * Get a licence if necessary, print message and fail if impossible.
 * If ok, do some low-level initialization
 *
 * As an indication that initialization was successful,
 * cpx_env is set not non-NULL, even for XPRESS.
 * All functions that do not use a problem handle must make sure that
 * the system is initialised by checking whether cpx_env is non-NULL!
 */

int
p_cpx_init(value vlicloc, type tlicloc, 
	   value vserialnum, type tserialnum, 
	   value vsubdir, type tsubdir, ec_eng_t *ec_eng)
{
    int i;

    /* Initialise global variables */
    for (i=0; i<4; i++)
	solver_streams[i] = Current_Null;
    d_le = ec_did("=<", 0);
    d_ge = ec_did(">=", 0);
    d_eq = ec_did("=:=", 0);
    d_optimizer = ec_did(SOLVER_ATOMIC_NAME, 0);
    d_yes = ec_did("yes", 0);
    d_no = ec_did("no", 0);
    d_eplex = ec_did("eplex", 0);
    d_colon2 = ec_did(":", 2);
    d_empty = ec_did("", 0);
    d_flag = ec_did("flag", 0);

    if (!cpx_env)
    {
        int err;
        char *licloc, *subdir;

        Check_Integer(tserialnum);
        Get_Name(vlicloc, tlicloc, licloc);
        if (*licloc == '\0') licloc = NULL;
	Get_Name(vsubdir, tsubdir, subdir);

        err = cpx_init_env(&cpx_env, licloc, vserialnum.nint, subdir);
        if (err)
        {
	    Fprintf(Current_Error, "Could not intialize %s\n", SOLVER_ATOMIC_NAME);
	    ec_flush(Current_Error);
            if (err<0) { Fail; }
	    Bip_Error(EC_EXTERNAL_ERROR);
        }
    }
# ifdef LOG_CALLS
    Fprintf(log_output_, "\nvoid step_%d() {\n", log_ctr++);
    ec_flush(log_output_);
# endif
    Succeed;
}


int
p_cpx_challenge(value v, type t, ec_eng_t *ec_eng)
{
# ifdef XPRESS
    int nvalue;
    char slicmsg[256];
    if (oem_xpress != XP_OEM_NO)
    {
#if XPRESS > 20
	XPRSbeginlicensing(NULL);
#endif
	if (XPRSlicense(&nvalue, slicmsg) != 8)
	{
	    oem_xpress = XP_OEM_YES;
	    Return_Unify_Integer(v, t, (long) nvalue);
	}
    }
    oem_xpress = XP_OEM_NO;
# endif
    Fail;
}


int
p_cpx_exit(ec_eng_t *ec_eng)
{
    if (cpx_env)
	cpx_exit(&cpx_env);
    Succeed;
}


int
p_cpx_prob_init(value vpre, type tpre, 
		value vcpy, type tcpy, 
		value vrow, type trow, 
		value vcol, type tcol, 
		value vnz, type tnz,
		value vdir, type tdir, 
		value vsense, type tsense, 
		value vhandle, type thandle, ec_eng_t *ec_eng)
{
    int	i;
    lp_desc *lpd;

    Check_Integer(tpre);
    Check_Integer(tcpy);
    Check_Integer(trow);
    Check_Integer(tcol);
    Check_Integer(tnz);
    Check_Structure(thandle);

    CallN(lpd = (lp_desc *) Malloc(sizeof(lp_desc)));
    /*CallN(_clr_lp_desc(lpd));*/
    CallN(memset(lpd, 0, sizeof(lp_desc)));


#ifdef USE_PROBLEM_ARRAY
    Log1(lpdmat[%d] = lpd, next_matno);
    current_matno = next_matno;
    lpd->matno = next_matno++;
#endif

#ifdef XPRESS
    lpd->copystatus = (vcpy.nint == 1 ? XP_COPYINVALID : XP_COPYOFF);

    {/* need unique name so that  file names created by XPRESS are unique
        dir is a directory path (but may need to be have the directory
        separator character (/ or \) added
     */
	int dirlen;

#ifdef WIN32
# define BASE_PROB_NAME \\eclipse
#else
# define BASE_PROB_NAME /eclipse
#endif 
	Check_String(tdir);
	dirlen = strlen(StringStart(vdir));
    
	lpd->probname = (char *) Malloc((50+dirlen) * sizeof(char));
	sprintf(lpd->probname, "%s"Transform_Quoted(BASE_PROB_NAME)"%u-%u", 
		StringStart(vdir), gethostid(), getpid()); 
	if (strlen(lpd->probname) > XP_PROBNAME_MAX)
	{
	    Fprintf(Current_Error, "Eplex error: the problem name for Xpress is too long.\n" 
            "Change tmp_dir to a directory with shorter path length.\n");
	    ec_flush(Current_Error);
	    Bip_Error(RANGE_ERROR);
	}
    }
#endif
    lpd->prob_type = PROBLEM_LP;
    lpd->presolve = vpre.nint;
    lpd->sense = vsense.nint;
    lpd->mac = vcol.nint;
    lpd->macsz = vcol.nint;
    lpd->macadded = 0;
    lpd->nidc = 0;
    lpd->marsz = vrow.nint;	       /* max number of rows */
    lpd->mar = vrow.nint;
    lpd->matnz = vnz.nint;	       /* number of nonzero coefficients */

    /* if vcol/vrow/vnz is 0, malloc arrays of at least size 1. This avoid
       calling malloc with size 0, and does create an array (Xpress crashes
       if NULL is given in place of an array address in some cases)
       This increment should only be done after assigning their original
       values to the lpd fields!

    */
    if (vcol.nint == 0) vcol.nint++; 
    if (vrow.nint == 0) vrow.nint++; 
    if (vnz.nint == 0) vnz.nint++;
    lpd->rhsx = (double *) Malloc(vrow.nint * sizeof(double));
    lpd->senx = (char *) Malloc(vrow.nint * sizeof(char));

    /* one extra element for matbeg, because some representations of the 
       matrix (e.g. COIN) needs the `matbeg' for vcol+1 to be specified,
       so that the end of the last column can be determined without matcnt
    */
    lpd->matbeg = (int *) Malloc((1+vcol.nint) * sizeof(int));
    lpd->matcnt = (int *) Malloc(vcol.nint * sizeof(int));
    lpd->matind = (int *) Malloc(vnz.nint * sizeof(int));
    lpd->matval = (double *) Malloc(vnz.nint * sizeof(double));

    lpd->bdl = (double *) Malloc(vcol.nint * sizeof(double));
    lpd->bdu = (double *) Malloc(vcol.nint * sizeof(double));
    lpd->dirtybdflag = 0;
    lpd->objx = (double *) Malloc(vcol.nint * sizeof(double));
    lpd->ctype = (char *) Malloc(vcol.nint * sizeof(char));
    for (i = 0; i < vcol.nint; i++)
    {
	lpd->bdl[i] = -CPX_INFBOUND;
	lpd->bdu[i] = CPX_INFBOUND;
	lpd->objx[i] = 0.0;
	lpd->ctype[i] = 'C';
    }

    /* the cutpools fields in lpd should all be zero/NULL at this point, so 
       they do not need to be initialised
    */

    lpd->descr_state = DESCR_LOADED;

    lpd->mipstart_dirty = 0;

    {/* Return the cplex descriptor in argument HANDLE_CPH of the handle structure. */
	vhandle.ptr[HANDLE_CPH] = ecl_handle(ec_eng, &lp_handle_tid, lpd);
	Make_Stamp(vhandle.ptr+HANDLE_STAMP); /* needed for other trail undos */
    }
    Succeed;
}


int
p_cpx_get_prob_param(value vlp, type tlp, value vp, type tp, value vval, type tval, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    int i;
    Check_Integer(tp);
    LpDesc(vlp, tlp, lpd);
    switch(vp.nint)
    {
    case 0:	i = lpd->mar; break;
    case 1:	i = lpd->mac; break;
    case 3:	i = lpd->sense; break;
    case 4:	i = lpd->prob_type; break;
    case 5:	i = lpd->optimum_ctr; break;
    case 6:	i = lpd->infeas_ctr; break;
    case 7:	i = lpd->abort_ctr; break;
    case 8:	i = lpd->sol_itcnt; break;
    case 9:	i = lpd->sol_nodnum; break;
    case 10:	i = lpd->descr_state; break;
    case 11:	i = lpd->sol_state; break;
    case 12:    Update_Model(lpd->lp);
		i = cpx_getnumnz(lpd->lp); break;
    case 13:    Update_Model(lpd->lp);
		i = IsMIPProb(lpd->prob_type) ? 
		    cpx_getnumint(lpd->lp) + cpx_getnumbin(lpd->lp) : 0;
		break;
    case 14:    Update_Model(lpd->lp);
		i = IsQPProb(lpd->prob_type) ? cpx_getnumqpnz(lpd->lp) : 0;
		break;
    case 15:	i = lpd->start_mac; break;
/*    case 16:	i = lpd->cp_nr; break;*/
    case 17:	i = lpd->cp_nr2; break;
    case 18:	i = lpd->cp_nact2; break;
    default:
	Bip_Error(RANGE_ERROR);
    }
    Return_Unify_Integer(vval, tval, i);
}


int
p_cpx_get_param(value vlp, type tlp, value vp, type tp, value vval, type tval, ec_eng_t *ec_eng)
{
    double dres;
    int ires;
    char sres[STRBUFFERSIZE];
    lp_desc *lpd; 

#ifndef COIN
    if (!cpx_env)
    {
	Bip_Error(EC_LICENSE_ERROR);
    }
#endif
    /* lpd is NULL for global/default param */
    if (IsHandle(tlp))  {
	LpDesc(vlp, tlp, lpd);
	if (!lpd->lp) lpd = NULL;
    } else lpd = NULL;

    if (IsAtom(tp))
    {
	param_id_t parnum;
	int partype;
	if (cpx_get_par_info(DidName(vp.did), &parnum, &partype))
	{
	    Fail;
	}

	switch(partype)
	{
	case 0:
	    if (!cpx_get_int_param(lpd, parnum, &ires))
	    {
		Return_Unify_Integer(vval, tval, ires);
	    }
	    break;

	case 1:
	    if (!cpx_get_dbl_param(lpd, parnum, &dres))
	    {
		Return_Unify_Float(vval, tval, dres);
	    }
	    break;

#ifdef SOLVER_HAS_STR_PARAMS
	case 2:
	    if (!cpx_get_str_param(lpd, parnum, sres))
	    {
	        value val;
	        Check_Output_String(tval);
	        Cstring_To_Prolog(sres, val);
	        Return_Unify_String(vval, tval, val.ptr);
	    }
	    break;
#endif

#ifdef COIN
	case 3:
	    if (!coin_get_solver_intparam((lpd==NULL ? cpx_env : lpd->lp), 
					      parnum, &ires))
	    {
		Return_Unify_Integer(vval, tval, ires);
	    }
	    break;

	case 4:
	    if (!coin_get_solver_dblparam((lpd==NULL ? cpx_env : lpd->lp),
					     parnum, &dres))
	    {
		Return_Unify_Float(vval, tval, dres);
	    }
	    break;

	case 6:
	    if (!coin_get_eplex_intparam((lpd==NULL ? cpx_env : lpd->lp), 
					      parnum, &ires))
	    {
		Return_Unify_Integer(vval, tval, ires);
	    } 
	    break;

	case 8:
	    if (!coin_get_eplex_strparam((lpd==NULL ? cpx_env : lpd->lp), 
					      parnum, sres))
	    {
	      value val;
	      Check_Output_String(tval);
	      Cstring_To_Prolog(sres, val);
	      Return_Unify_String(vval, tval, val.ptr);
	    }
	    break;
#endif
	}
	Bip_Error(TYPE_ERROR);	/* occurs only if params[i].type is wrong */
    }

    Check_Integer(tp);
    switch (vp.nint)
    {
    case -1:			/* get optimizer code */
	Return_Unify_Atom(vval, tval, d_optimizer);

    case -2:			/* get optimizer version */
#ifdef COIN
      {
          char ver[32];
	  pword pw;
	  coin_get_solver_info(ver);
	  Make_String(&pw, ver);
	  Return_Unify_Pw(vval, tval, pw.val, pw.tag);
      }
#else
	Return_Unify_Integer(vval, tval, SOLVER_VERSION_INT);
#endif

    case -3:			/* has_qp */
#ifdef HAS_QUADRATIC
	Return_Unify_Atom(vval, tval, d_yes);
#else
	Return_Unify_Atom(vval, tval, d_no);
#endif

    case -4:			/* has_miqp */
#ifdef HAS_MIQP
	Return_Unify_Atom(vval, tval, d_yes);
#else
	Return_Unify_Atom(vval, tval, d_no);
#endif

    case -5:			/* has_indicator_constraints */
#ifdef HAS_INDICATOR_CONSTRAINTS
	Return_Unify_Atom(vval, tval, d_yes);
#else
	Return_Unify_Atom(vval, tval, d_no);
#endif

    case -6:			/* has_sos */
#ifdef HAS_NO_ADDSOS
	Return_Unify_Atom(vval, tval, d_no);
#else
	Return_Unify_Atom(vval, tval, d_yes);
#endif

    default:
	Bip_Error(RANGE_ERROR);
    }
}


int
p_cpx_set_param(value vlp, type tlp, value vp, type tp, value vval, type tval, ec_eng_t *ec_eng)
/* fails if parameter unknown */
{
    param_id_t parnum;
    int partype;
    int err = 1;
    lp_desc *lpd; 

    Check_Atom(tp);
#ifndef COIN
    if (!cpx_env)
    {
	Bip_Error(EC_LICENSE_ERROR);
    }
#endif
    /* lpd is NULL for global/default param */
    if (IsHandle(tlp))  {
	LpDesc(vlp, tlp, lpd);
	if (!lpd->lp) lpd = NULL;
    } else lpd = NULL;

#ifndef SOLVER_HAS_LOCAL_PARAMETERS
    if (lpd != NULL) 
    {
        Fprintf(Current_Error, "Eplex error: per solver instance parameters are not supported for this solver. Use global parameters instead.\n");
	Bip_Error(UNIMPLEMENTED);
    }
#endif
    if (cpx_get_par_info(DidName(vp.did), &parnum, &partype))
    {
	Fail;
    }

    switch(partype)
    {
    case 0:
	Check_Integer(tval);
	err = cpx_set_int_param(lpd, parnum, vval.nint);
	break;

    case 1:
        Check_Float(tval);
        err = cpx_set_dbl_param(lpd, parnum, Dbl(vval));
	break;

#ifdef SOLVER_HAS_STR_PARAMS
    case 2:
    {
	char *s;
	Get_Name(vval, tval, s);
	if (strlen(s) >= STRBUFFERSIZE) Bip_Error(RANGE_ERROR);/*too large*/
        err = cpx_set_str_param(lpd, parnum, s);
	break;
    }
#endif

#if defined(XPRESS) && defined(WIN32)
    case 3:
    {
	char *s;
	Get_Name(vval, tval, s);
	err = XPRSsetlogfile(lpd->lp, s);
	break;
    }
#endif

#ifdef COIN
    /* Solver dependent parameters */
    case 3:
	Check_Integer(tval);
	err = coin_set_solver_intparam((lpd == NULL ? cpx_env : lpd->lp), parnum, vval.nint);
	break;

    case 4:
        Check_Float(tval);
        err = coin_set_solver_dblparam((lpd == NULL ? cpx_env : lpd->lp), parnum, Dbl(vval));
	break;

    case 6:
	Check_Integer(tval);
        err = coin_set_eplex_intparam((lpd == NULL ? cpx_env : lpd->lp), parnum, vval.nint);
	break;

    /* no double params defined yet
    case 7:
        Check_Float(tval);
        err = coin_set_eplex_dblparam((lpd == NULL ? cpx_env : lpd->lp), parnum, Dbl(vval));
	break;
    */

    case 8:
    {
	char *s;
	Get_Name(vval, tval, s);
	err = coin_set_eplex_strparam((lpd == NULL ? cpx_env : lpd->lp), parnum, s);
	break;
    }
#endif

    default:
	Bip_Error(RANGE_ERROR);
    }
    if (err) {
	Bip_Error(TYPE_ERROR);
    }
    Succeed;
}


/*----------------------------------------------------------------------*
 * Message and error output
 *----------------------------------------------------------------------*/

#ifdef CPLEX

static void CPXPUBLIC
# if CPLEX >= 8
eclipse_out(void * nst, const char * msg)
# else 
eclipse_out(void * nst, char * msg)
# endif
{
    (void) ec_print_msg((stream_id) nst, msg, 0);
}


int
p_cpx_output_stream(value vc, type tc, value vwhat, type twhat, value vs, type ts, ec_eng_t *ec_eng)
{
    stream_id nst;
    CPXCHANNELptr ch;
    pword pw;
    int err;

    if (!cpx_env)
    {
	Bip_Error(EC_LICENSE_ERROR);
    }
    Check_Integer(tc);
    switch (vc.nint) {
    case 0: ch = cpxresults; break;
    case 1: ch = cpxerror; break;
    case 2: ch = cpxwarning; break;
    case 3: ch = cpxlog; break;
    default: Bip_Error(RANGE_ERROR);
    }
    Check_Integer(twhat);
    pw.val = vs; pw.tag = ts;
    err = ec_get_stream(pw, &nst);	/* get a stream copy */
    if (err) { Bip_Error(err); }
    if (vwhat.nint == 0)
    {
	if (CPXdelfuncdest(cpx_env, ch, (void *) nst, eclipse_out) == 0)
            /* hope that CPXdelfuncdest() matches both function AND argument! */
            ec_release_stream(nst);	/* the copy made for CPXaddfuncdest */
	ec_release_stream(nst);	/* the copy made just now */
    } else {
	/* raise error only if adding a stream */
	if (CPXaddfuncdest(cpx_env, ch, (void *) nst, eclipse_out))
	    { Bip_Error(EC_EXTERNAL_ERROR); }
    }
    Succeed;
}

#else

# ifdef XPRESS
static void XPRS_CC
eclipse_out(XPRSprob lp, void * obj, const char * msg, int len, int msgtype)
{
    if (msgtype >= 1 && msgtype <= 4)
    {
	/* filter out and not print unwanted messages  */
	if (msgtype == 3)
	{
	    /* it seems that there are no other way of filtering out
               warning messages than to check the msg string
	    */
	    if (strncmp(msg, "?140 ", 5) == 0) return; /* basis lost */
	    if (strncmp(msg, "?359 ", 5) == 0) return; /* illegal int bounds */
	}
	/* Fprintf(solver_streams[msgtype-1], "*%d*", msgtype); */
        ec_print_msg(solver_streams[msgtype-1], (char*) msg, 1);
    }
}

#else

void
eclipse_out(int msgtype, const char* message)
{
    (void) ec_print_msg(solver_streams[msgtype], message, 1);
}

# endif

int
p_cpx_output_stream(value vc, type tc, value vwhat, type twhat, value vs, type ts, ec_eng_t *ec_eng)
{
    stream_id nst;
    stream_id *solver_stream;
    pword pw;
    int err;

# ifdef XPRESS
    if (!cpx_env)
    {
	Bip_Error(EC_LICENSE_ERROR);
    }
# endif
    Check_Integer(tc);
    Check_Integer(twhat);
    switch (vc.nint) {
	case 0: solver_stream = &solver_streams[ResType]; break;
	case 1: solver_stream = &solver_streams[ErrType]; break;
	case 2: solver_stream = &solver_streams[WrnType]; break;
	case 3: solver_stream = &solver_streams[LogType]; break;
	default: Bip_Error(RANGE_ERROR);
    }
    pw.val = vs; pw.tag = ts;
    err = ec_get_stream(pw, &nst);
    if (err) { Bip_Error(err); }
    if (vwhat.nint == 1)
	*solver_stream = nst;
    else if (*solver_stream == nst) {
	*solver_stream = Current_Null;
	ec_release_stream(nst);
    }
    Succeed;
}

#endif

/*----------------------------------------------------------------------*
 * Initial setup
 *----------------------------------------------------------------------*/

int
p_cpx_get_rhs(value vlp, type tlp, value vpool, type tpool, value vi, type ti, 
	      value vsense, type tsense, value vval, type tval, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    Prepare_Requests
    double rhs[1];
    char sen[1];
    dident sense;

    LpDesc(vlp, tlp, lpd);
    if (!(0 <= vi.nint && vi.nint < lpd->mar))
    {
	Bip_Error(RANGE_ERROR);
    }
    switch (vpool.nint)
    {
    case CSTR_TYPE_NORM:
	SetPreSolve(lpd->presolve);
	Update_Model(lpd->lp);	/* before cpx_get... */
	if (cpx_getrhs(lpd->lp, rhs, (int) vi.nint+IDX_OFFSET))
	   { Bip_Error(EC_EXTERNAL_ERROR); }
	if (cpx_getsense(lpd->lp, sen, (int) vi.nint+IDX_OFFSET))
	   { Bip_Error(EC_EXTERNAL_ERROR); }
	break;
/*
    case CSTR_TYPE_PERMCP:
	sen[0] = lpd->cp_senx[vi.nint];
	rhs[0] = lpd->cp_rhsx[vi.nint];
	break;
*/
    case CSTR_TYPE_CONDCP:
	sen[0] = lpd->cp_senx2[vi.nint];
	rhs[0] = lpd->cp_rhsx2[vi.nint];
	break;
    default:
	Bip_Error(RANGE_ERROR);
	break;
    }

    sense = sen[0] == SOLVER_SENSE_LE ? d_le :
	    sen[0] == SOLVER_SENSE_GE ? d_ge : d_eq;
    Request_Unify_Atom(vsense, tsense, sense);
    Request_Unify_Float(vval, tval, rhs[0]);

    Return_Unify;
}

int
p_cpx_set_rhs_coeff(value vlp, type tlp, value vi, type ti, value vsense, type tsense, value vval, type tval, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(ti);
    Check_Number(tval);
    if (vi.nint >= lpd->marsz) { Bip_Error(RANGE_ERROR); }
    Check_Atom(tsense);
    if (vsense.did == d_le) lpd->senx[vi.nint] = SOLVER_SENSE_LE;
    else if (vsense.did == d_ge) lpd->senx[vi.nint] = SOLVER_SENSE_GE;
    else if (vsense.did == d_eq) lpd->senx[vi.nint] = SOLVER_SENSE_EQ;
    else { Bip_Error(RANGE_ERROR); }
    lpd->rhsx[vi.nint] = DoubleVal(vval, tval);
    Check_Constant_Range(lpd->rhsx[vi.nint]);
    Succeed;
}

int
p_cpx_set_obj_coeff(value vlp, type tlp, value vj, type tj, value vval, type tval, ec_eng_t *ec_eng)
{
    lp_desc *lpd;  
    int j;

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tj);
    Check_Number(tval);
    j = vj.nint;

    if (j >= lpd->mac) { Bip_Error(RANGE_ERROR); }
    if (j >= lpd->macadded) j -= lpd->macadded; /* added col */

    lpd->objx[j] = DoubleVal(vval, tval);
    Check_Constant_Range(lpd->objx[j]);
    Succeed;
}


int
p_cpx_set_qobj_coeff(value vlp, type tlp, value vi, type ti, value vj, type tj, value vval, type tval, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    double coef;
    int i;
    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(ti);
    Check_Integer(tj);
    Check_Number(tval);
    if (IsInteger(tval)) {
	coef = (double) vval.nint;
    } else {
	Check_Float(tval);
	coef = DoubleVal(vval, tval);
	Check_Constant_Range(coef);
    }
    if (vj.nint >= lpd->mac || vi.nint >= lpd->mac) { Bip_Error(RANGE_ERROR); }

    if (lpd->cb_cnt == 0)		/* first quadratic coefficient */
    {
	/* change the problem type to quadratic if linear */
	switch (lpd->prob_type)
	{
	case PROBLEM_LP:
	    lpd->prob_type = PROBLEM_QP;
	    break;
	case PROBLEM_MIP:
	    lpd->prob_type = PROBLEM_MIQP;
	    break;
	case PROBLEM_QP:
	case PROBLEM_MIQP:
	    /* nothing to be done if already quadratic */
	    break;
	default:
	    Fprintf(Current_Error, 
	       "Eplex error: quadratic objective coefficients cannot be added to problem type %d\n.", 
	    lpd->prob_type);
	    Bip_Error(RANGE_ERROR);
	}
    }
    if (lpd->cb_cnt >= lpd->cb_sz)	/* grow arrays if necessary */
	_grow_cb_arrays(lpd, 1);
    i = lpd->cb_cnt++;
    lpd->cb_index[i] = vi.nint+IDX_OFFSET;
    lpd->cb_index2[i] = vj.nint+IDX_OFFSET;
    lpd->cb_value[i] = vi.nint==vj.nint ? 2*coef : coef;
    Succeed;
}


int
p_cpx_load_varname(value vlp, type tlp, value vj, type tj, value vname, type tname, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 

    Check_Integer(tj);
    Check_String(tname);
    LpDesc(vlp, tlp, lpd);
    if (vj.nint >= lpd->mac) { Bip_Error(RANGE_ERROR); }

    /* The ECLiPSe string is volatile, so we assume that the string gets copied! */
    if (cpx_chgname(lpd->lp, 'c', vj.nint+IDX_OFFSET, StringStart(vname), StringLength(vname)))
	{ Bip_Error(EC_EXTERNAL_ERROR); }
    Succeed;
}


/*----------------------------------------------------------------------*
 * Changing rhs 
 * cplex_change_rhs(++CPH, ++Size, ++RowIdxs, ++RhsCoeffs) 
 *   Changes the rhs coefficients. RowIdxs and RhsCoeffs are lists of length
 *   Size. There is no provision for backtracking: the changes should be
 *   undone with another call to cplex_change_rhs
 */
int
p_cpx_change_rhs(value vlp, type tlp, value vsize, type tsize, 
	      value vidxs, type tidxs, value vvals, type tvals, ec_eng_t *ec_eng)
{
    lp_desc *lpd;
    int i, err;

    LpDesc(vlp, tlp, lpd);
    Check_Integer(tsize);

    if (vsize.nint > 0)
    {
	double *rhs = (double *) Malloc(vsize.nint * sizeof(double));
	int *idxs = (int *) Malloc(vsize.nint * sizeof(int));   /* with IDX_OFFSET */

	for (i=0; i<vsize.nint; ++i)
	{
	    if (IsList(tidxs) && IsList(tvals))
	    {
		pword *ihead = vidxs.ptr;
		pword *itail = ihead + 1;
		pword *vhead = vvals.ptr;
		pword *vtail = vhead + 1;

		Dereference_(ihead);
		Dereference_(vhead);
		if (!IsInteger(ihead->tag) || !IsNumber(vhead->tag))
		{
		    Free(idxs);
		    Free(rhs);
		    Bip_Error(TYPE_ERROR); 
		}
		idxs[i] = ihead->val.nint;
		rhs[i] = DoubleVal(vhead->val, vhead->tag);
		/* check that the row index and rhs value are in range... */
		if (idxs[i] < 0 || idxs[i] >= lpd->mar || 
		    rhs[i] < -CPX_INFBOUND || rhs[i] > CPX_INFBOUND)
		{ 
		    Free(idxs);
		    Free(rhs);
		    Bip_Error(RANGE_ERROR); 
		}
		idxs[i] += IDX_OFFSET;

		Dereference_(itail);
		Dereference_(vtail);
		tidxs = itail->tag;
		vidxs = itail->val;
		tvals = vtail->tag;
		vvals = vtail->val;
	    } 
	    else 
	    { 
		Free(idxs);
		Free(rhs);
		Bip_Error(TYPE_ERROR); 
	    }
	}

        err = cpx_chgrhs(lpd->lp, vsize.nint, idxs, rhs);
	Free(idxs);
	Free(rhs);
	if (err) { Bip_Error(EC_EXTERNAL_ERROR); }
    }

    Succeed;
}

/*----------------------------------------------------------------------*
 * Changing column bounds 
 * cplex_change_cols_bounds(++CPH, ++Size, ++Idxs, ++Los, ++His) 
 *   Changes the lower and upper bounds of columns. Idxs, Los, His are
 *   lists of length Size. There is no provision for backtracking: the 
 *   changes should be  undone with another call to cplex_change_cols_bounds
 * Size argument no longer used.
 */
int
p_cpx_change_cols_bounds(value vlp, type tlp, value vsize, type tsize, 
      value vidxs, type tidxs, value vlos, type tlos, value vhis, type this, ec_eng_t *ec_eng)
{
    lp_desc *lpd;
    int i, err, size;

    LpDesc(vlp, tlp, lpd);
    while (IsList(tidxs) && IsList(tlos) && IsList(this))
    {
        pword *ihead = vidxs.ptr;
        pword *itail = ihead + 1;
        pword *lohead = vlos.ptr;
        pword *lotail = lohead + 1;
        pword *uphead = vhis.ptr;
        pword *uptail = uphead + 1;
        double lo, hi;

        Dereference_(ihead);
        Dereference_(lohead);
        Dereference_(uphead);
        if (!IsInteger(ihead->tag) || !IsNumber(lohead->tag) ||
            !IsNumber(uphead->tag))
        {
            Bip_Error(TYPE_ERROR); 
        }
        if (ihead->val.nint < 0 || ihead->val.nint >= lpd->mac) 
        { 
            Bip_Error(RANGE_ERROR); 
        }
        lo = DoubleVal(lohead->val, lohead->tag);
        if (lo < -CPX_INFBOUND) lo = -CPX_INFBOUND;
        hi = DoubleVal(uphead->val, uphead->tag);
        if (hi > CPX_INFBOUND) hi = CPX_INFBOUND;
        if (hi < lo)
        {
            /* Can't fail here, as we can't undo the changes aready made. */
            /* Bounds must be checked before calling this function! */
            Bip_Error(RANGE_ERROR);
        }
        if (cpx_setbds(lpd->lp, ihead->val.nint+IDX_OFFSET, lo, hi))
        {
            Bip_Error(EC_EXTERNAL_ERROR);
        }
        Dereference_(itail);
        Dereference_(lotail);
        Dereference_(uptail);
        tidxs = itail->tag;
        vidxs = itail->val;
        tlos = lotail->tag;
        vlos = lotail->val;
        this = uptail->tag;
        vhis = uptail->val;
    }
    if (!(IsNil(tidxs) && IsNil(tlos) && IsNil(this)))
    { 
        Bip_Error(TYPE_ERROR); 
    }
    Succeed;
}

int
p_cpx_lo_hi(value vlo, type tlo, value vhi, type thi, ec_eng_t *ec_eng)
{
    Prepare_Requests;
    Request_Unify_Float(vlo, tlo, -CPX_INFBOUND);
    Request_Unify_Float(vhi, thi, CPX_INFBOUND);
    Return_Unify;
}

/*----------------------------------------------------------------------*
 * Changing problem type
 */

static void
_cpx_reset_probtype(pword * pw, word * pdata, int size, int flags, ec_eng_t *ec_eng)
{
    int err;
    lp_desc *lpd = ExternalData(pw[HANDLE_CPH].val.ptr); 

    if (!lpd)
	return; /* stale handle */

    if (lpd->descr_state != DESCR_EMPTY)
    {
#ifdef CPLEX
	switch  (((untrail_ptype*) pdata)->old_ptype)
	{
	case PROBLEM_LP:
	    err = cpx_chgprobtype(lpd->lp, PROBLEM_LP);
	    break;
	case PROBLEM_QP:
	    err = cpx_chgprobtype(lpd->lp, PROBLEM_QP);
	    break;
	default:
	    Fprintf(Current_Error, "Eplex problem: Trying to reset an Eplex problem to a unsupported type: %i.\n", 
		    ((untrail_ptype*) pdata)->old_ptype);
	    ec_flush(Current_Error);
	    return;

	}
#endif
	lpd->prob_type = ((untrail_ptype*) pdata)->old_ptype;
#ifdef SOLVE_MIP_COPY
	if (lpd->copystatus != XP_COPYOFF && lpd->lp != lpd->lpcopy)
	{
	    CallN(XPRSdestroyprob(lpd->lpcopy));
	    CallN(lpd->lpcopy = lpd->lp);
	    Mark_Copy_As_Modified(lpd);
	}
#endif
    }
}

int
p_cpx_change_lp_to_mip(value vhandle, type thandle, ec_eng_t *ec_eng)
{
    lp_desc *lpd;
    int err;
    untrail_ptype pdata;

    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);
    pdata.old_ptype = lpd->prob_type;

    /* all columns are assumed to be type 'C' by changing to MIP. Any
       integer columns will be set explicitly later
    */
    switch (pdata.old_ptype)
    {
    case PROBLEM_LP:
	lpd->prob_type = PROBLEM_MIP;
	err = cpx_chgprobtype(lpd->lp, PROBLEM_MIP);
	break;
    case PROBLEM_QP:
#ifdef HAS_MIQP
	lpd->prob_type = PROBLEM_MIQP;
	err = cpx_chgprobtype(lpd->lp, PROBLEM_MIQP);
#else
	Fprintf(Current_Error, "Eplex error: this solver does not support solving of quadratic MIP problems.\n");
	ec_flush(Current_Error);
	Bip_Error(UNIMPLEMENTED);
#endif
	break;
    case PROBLEM_MIQP:
    case PROBLEM_MIP:
	return PSUCCEED;
    default:
	Fprintf(Current_Error, "Eplex error: trying to change problem to mixed integer from an unexpected state.\n");
	ec_flush(Current_Error);
	Bip_Error(RANGE_ERROR);
	break;
    }
	
    if (err) { Bip_Error(EC_EXTERNAL_ERROR); }

#ifdef SOLVE_MIP_COPY
    if (lpd->copystatus != XP_COPYOFF)  
    {
	Call(err, XPRScreateprob(&lpd->lpcopy));
	if (err) { Bip_Error(EC_EXTERNAL_ERROR); }
	Mark_Copy_As_Modified(lpd);
    }
#endif
    ecl_trail_undo(ec_eng, _cpx_reset_probtype, vhandle.ptr, NULL, (word*)&pdata, NumberOfWords(untrail_ptype), TRAILED_WORD32);
    return PSUCCEED;
}

/* cplex_set_problem_type(CPH, ProbType, SetSolverType)

   changes the problem type to ProbType. Used to set and reset the problem 
   type during probing, when the problem type might be changed temporarily.
   SetSolverType applies to external solvers which has its own problem type
   (currently CPLEX): it specifies if the external solver's problem type
   should be set as well (1 = yes, 0 = no). CPLEX's problem type cannot 
   always be changed when setting a problem type before a probe, e.g.
   CPXPROB_FIXEDMILP/MIQP can only be set if there is already a MILP/MIQP
   solution. Therefore, in these cases, the solver's problem type is only 
   changed during the solving of the problem in p_cpx_optimise().
*/
int 
p_cpx_set_problem_type(value vlp, type tlp, value vtype, type ttype, 
		       value vsetsolver, type tsetsolver, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    LpDescOnly(vlp, tlp, lpd);

    Check_Integer(ttype);
    Check_Integer(tsetsolver);
#ifdef CPLEX
    if (vsetsolver.nint == 1)
    {
	int err;
	switch (vtype.nint)
	{
	case PROBLEM_LP: 
	    err = cpx_chgprobtype(lpd->lp, PROBLEM_LP); 
	    break;
	case PROBLEM_MIP:
# ifndef HAS_RELAXEDLP
	    if (CPXgetprobtype(cpx_env, lpd->lp) == CPXPROB_LP &&
		lpd->ctype != NULL)
	    {
		/* this assumes that we have copied the ctype information
                   of original MIP problem to lpd->ctype 
		*/
		Call(err, CPXcopyctype(cpx_env, lpd->lp, lpd->ctype));
		TryFree(lpd->ctype);
	    }
	    else
# endif
	    {
		err = cpx_chgprobtype(lpd->lp, PROBLEM_MIP);
	    }
	    break;
	case PROBLEM_QP:
	    err = cpx_chgprobtype(lpd->lp, PROBLEM_QP);
	    break;
# ifdef HAS_MIQP
	case PROBLEM_MIQP:
	    if (CPXgetprobtype(cpx_env, lpd->lp) == CPXPROB_QP &&
		lpd->ctype != NULL)
	    {
		/* this assumes that we have copied the ctype information
                   of original MIQP problem to lpd->ctype 
		*/
		Call(err, CPXcopyctype(cpx_env, lpd->lp, lpd->ctype));
		TryFree(lpd->ctype);
	    }
	    else
	    {
		err = cpx_chgprobtype(lpd->lp, PROBLEM_MIP);
	    }
	    break;
	case PROBLEM_FIXEDQ:
	    err = cpx_chgprobtype(lpd->lp, PROBLEM_FIXEDQ);
	    break;
# endif
	case PROBLEM_FIXEDL:
	    err = cpx_chgprobtype(lpd->lp, PROBLEM_FIXEDL);
	    break;
	default:
	    Bip_Error(RANGE_ERROR);
	}
	if (err) Bip_Error(EC_EXTERNAL_ERROR);
    }
#endif

    lpd->prob_type = vtype.nint;
    Succeed;
}



/*----------------------------------------------------------------------*
 * Fails if method not supported by solver
 */
int 
p_cpx_valid_method(value vmethod, type tmethod, ec_eng_t *ec_eng)
{
    Check_Integer(tmethod);
    if (!solver_has_method(vmethod.nint)) { Fail }
    Succeed;
}

/*----------------------------------------------------------------------*
 * Fails if node method not supported by solver
 */
int 
p_cpx_valid_node_method(value vmethod, type tmethod, ec_eng_t *ec_eng)
{
    Check_Integer(tmethod);
    if (!solver_has_node_method(vmethod.nint)) { Fail }
    Succeed;
}

/*----------------------------------------------------------------------*
 * Changing column type 
 */

#define Get_Col_Bounds(j, lo0,hi0) {                    \
        if (cpx_getbds(lpd->lp, &lo0, &hi0, j+IDX_OFFSET))	\
	    { Bip_Error(EC_EXTERNAL_ERROR); }		\
}

#define Trail_Bounds(j, oldlo, oldhi, Stamp) {                         \
    untrail_bound udata;					       \
    udata.bds[0] = oldlo;                                              \
    udata.bds[1] = oldhi;                                              \
    udata.idx = j;                                                     \
    ecl_trail_undo(ec_eng, _cpx_restore_bounds, vhandle.ptr,                    \
                  Stamp, (word *) &udata,                              \
		  NumberOfWords(untrail_bound), TRAILED_WORD32);       \
}


static void _cpx_restore_bounds(pword*,word*,int,int,ec_eng_t*);

static void _cpx_reset_col_type(pword*,word*,int,int,ec_eng_t*);

int
p_cpx_change_col_type(value vhandle, type thandle, 
		      value vj, type tj, 
		      value vtype, type ttype, ec_eng_t *ec_eng)
{
    int j, res;
    int idx[1]; /* with IDX_OFFSET */
    char ctype[1];
    untrail_ctype udata;
    lp_desc *lpd;

    Check_Structure(thandle);
    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);
    Check_Integer(tj);

    j = (int) vj.nint;
    if (j >= lpd->mac || j < 0) { Bip_Error(RANGE_ERROR); }

    SetPreSolve(lpd->presolve);
    Mark_Copy_As_Modified(lpd);
    idx[0] = j+IDX_OFFSET;
    Update_Model(lpd->lp);	/* before CPXget... */
    res = cpx_getctype(lpd->lp, ctype, idx[0]);
    if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }

    if ((char) vtype.nint != ctype[0]) {
	/* only need to change if new column type different */
	udata.idx = idx[0];
	udata.ctype = ctype[0];

	/* if change is from B->I, just ignore it (user could have posted
           extra integer constraints)
	*/
	if (!((char) vtype.nint == 'I' && (char) ctype[0] == 'B'))
	{
#ifdef HAS_NARROW_INT_RANGE
            double lo0, hi0;
            Get_Col_Bounds(j, lo0, hi0);
            if (lo0 < -XPRS_MAXINT  ||  hi0 > XPRS_MAXINT)
            {
                /* no timestamp on these column bound changes: they will always
                   be untrailed. This is because the type changes may be 
                   applied to multiple merged columns, and we need to
                   ensure that the bound change is always undone
                */
                Trail_Bounds(j, lo0, hi0, NULL);
                if (lo0 < -XPRS_MAXINT) 
                    cpx_setlb(lpd->lp, j+IDX_OFFSET, -XPRS_MAXINT);
                if (hi0 > XPRS_MAXINT)
                    cpx_setub(lpd->lp, j+IDX_OFFSET, XPRS_MAXINT);
            }
#elif defined(INTS_NEED_INT_BOUNDS)
            double lo0, hi0, intlo, inthi;
            Get_Col_Bounds(j, lo0, hi0);
            intlo = ceil(lo0);
            inthi = floor(hi0);
            if (intlo > lo0  ||  inthi < hi0)
            {
                /* no timestamp on these column bound changes: they will always
                   be untrailed. This is because the type changes may be 
                   applied to multiple merged columns, and we need to
                   ensure that the bound change is always undone
                */
                Trail_Bounds(j, lo0, hi0, NULL);
                if (intlo > lo0)
                    cpx_setlb(lpd->lp, j+IDX_OFFSET, intlo);
                if (inthi < hi0)
                    cpx_setub(lpd->lp, j+IDX_OFFSET, inthi);
            }
#endif

	    ctype[0] = (char) vtype.nint;
	    res = cpx_chgctype(lpd->lp, 1, idx, ctype);
	    if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }	
	    ecl_trail_undo(ec_eng, _cpx_reset_col_type, vhandle.ptr, NULL, (word*)&udata, NumberOfWords(untrail_ctype), TRAILED_WORD32);
	}
    }
    Succeed;
}


static void _cpx_reset_col_type(pword * phandle, word * udata, int size, int flags, ec_eng_t *ec_eng)

{
    int idx[1];
    char octype[1];

    lp_desc *lpd = ExternalData(phandle[HANDLE_CPH].val.ptr); 

    if (!lpd)
	return; /* stale handle */

    idx[0] = ((untrail_ctype*) udata)->idx; 
        octype[0] = ((untrail_ctype*) udata)->ctype;

#if 0
    Fprintf(Current_Error, "Resetting col %d to %c, in gc:%d\n",
	idx[0], octype[0], ec_eng->vm_flags & NO_EXIT);
    ec_flush(Current_Error);
#endif

    if (lpd->descr_state != DESCR_EMPTY) 
    {
	if (cpx_chgctype(lpd->lp, 1, idx, octype))
	{
	    Fprintf(Current_Error, "Error in Changing column %d to type %c\n",
		    idx[0], octype[0]);
	    ec_flush(Current_Error);
	    return;
	}
      Mark_Copy_As_Modified(lpd);
    }
}


/*----------------------------------------------------------------------*
 * Adding constraints
 *----------------------------------------------------------------------*/
/*
 * We first collect the new row/col data in (growable) arrays using
 * p_cpx_set_matbeg(), p_cpx_set_matval() and p_cpx_set_obj_coeff() [cols]
 * p_cpx_new_row() and p_cpx_add_coeff() [rows].
 * Then, the information is transferred to the solver (and trailed)
 * by calling p_cpx_flush_new_rowcols().
 * On failure, the constraints get removed by _cpx_del_rowcols().
 *
 * added by AE 25/10/02
 * this is for adding rows whose index in the external
 * solver we want to know for sure - when we get duals
 * in colgen we really have to know we are getting the right
 * ones associated with the sp cost function vars
 * this requires all variables to have their index already in the attribute
 */

#define New_Row(nrs, nr_sz, senx, rhsx, rmatbeg, nnz, sense, vrhs, trhs, ExtraAlloc) {\
    if (nrs+1 >= nr_sz)	/* allocate/grow arrays */\
    {\
	CallN(nr_sz += NEWROW_INCR);\
	CallN(senx = (char *) Realloc(senx, nr_sz*sizeof(char)));\
	CallN(rhsx = (double *) Realloc(rhsx, nr_sz*sizeof(double)));\
	CallN(rmatbeg = (int *) Realloc(rmatbeg, nr_sz*sizeof(int)));\
        ExtraAlloc; \
    }\
    senx[nrs] = (char) sense;\
    rhsx[nrs] = DoubleVal(vrhs, trhs);\
    Check_Constant_Range(rhsx[nrs]);\
    rmatbeg[nrs] = nnz;\
    ++nrs;\
}

int
p_cpx_new_row(value vlp, type tlp, value vsense, type tsense, 
	      value vrhs, type trhs, value vgtype, type tgtype, 
	      value vidx, type tidx, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    int    idx, sense; 

    LpDescOnly(vlp, tlp, lpd);
    Check_Number(trhs);
    Check_Integer(tgtype);
    Check_Atom(tsense);
    if (vsense.did == d_le) sense = SOLVER_SENSE_LE;
    else if (vsense.did == d_ge) sense = SOLVER_SENSE_GE;
    else if (vsense.did == d_eq) sense = SOLVER_SENSE_EQ;
    else { Bip_Error(RANGE_ERROR); }
    
    switch (vgtype.nint)
    {
    case CSTR_TYPE_NORM:
	idx = lpd->mar+lpd->nr;
	New_Row(lpd->nr, lpd->nr_sz, lpd->senx, lpd->rhsx, lpd->rmatbeg, 
		lpd->nnz, sense, vrhs, trhs, {});
	break;
/*
    case CSTR_TYPE_PERMCP:
	idx = lpd->cp_nr;
	New_Row(lpd->cp_nr, lpd->cp_nr_sz, lpd->cp_senx, lpd->cp_rhsx, 
		lpd->cp_rmatbeg, lpd->cp_nnz, sense, vrhs, trhs, {});
	break;
*/
    case CSTR_TYPE_CONDCP:
	idx = lpd->cp_nr2;
	New_Row(lpd->cp_nr2, lpd->cp_nr_sz2, lpd->cp_senx2, lpd->cp_rhsx2, 
		lpd->cp_rmatbeg2, lpd->cp_nnz2, sense, vrhs, trhs, 
		{CallN(lpd->cp_active2 = (char *) Realloc(lpd->cp_active2, lpd->cp_nr_sz2*sizeof(char))); 
		 CallN(lpd->cp_initial_add2 = (char *) Realloc(lpd->cp_initial_add2, lpd->cp_nr_sz2*sizeof(char)));
		});
	break;
    default:
	Bip_Error(RANGE_ERROR);
	break;
    }
    Return_Unify_Integer(vidx, tidx, idx);
}


int
p_cpx_new_row_idc(value vlp, type tlp, value vsense, type tsense, 
	  value vrhs, type trhs, value vcompl, type tcompl,
	  value vindind, type tindind, ec_eng_t *ec_eng)
{
    int sense; 
    lp_desc *lpd;

    LpDescOnly(vlp, tlp, lpd);
    Check_Number(trhs);
    Check_Integer(tcompl);
    Check_Integer(tindind);
    Check_Atom(tsense);
    if (vsense.did == d_le) sense = SOLVER_SENSE_LE;
    else if (vsense.did == d_ge) sense = SOLVER_SENSE_GE;
    else if (vsense.did == d_eq) sense = SOLVER_SENSE_EQ;
    else { Bip_Error(RANGE_ERROR); }
    
    if (lpd->nr+1 >= lpd->nr_sz)	/* allocate/grow arrays */
    {
	CallN(lpd->nr_sz += NEWROW_INCR);
	CallN(lpd->senx = (char *) Realloc(lpd->senx, lpd->nr_sz*sizeof(char)));
	CallN(lpd->rhsx = (double *) Realloc(lpd->rhsx, lpd->nr_sz*sizeof(double)));
	CallN(lpd->rmatbeg = (int *) Realloc(lpd->rmatbeg, lpd->nr_sz*sizeof(int)));
	CallN(lpd->rcompl = (char *) Realloc(lpd->rcompl, lpd->nr_sz*sizeof(char)));
	CallN(lpd->rindind = (int *) Realloc(lpd->rindind, lpd->nr_sz*sizeof(int)));
    } else if (!lpd->rcompl) {
	/* Only used for IDC, may not be allocated yet */
	CallN(lpd->rcompl = (char *) Malloc(lpd->nr_sz*sizeof(char))); 
	CallN(lpd->rindind = (int *) Malloc(lpd->nr_sz*sizeof(int)));
    }
    lpd->senx[lpd->nr] = (char) sense;
    lpd->rhsx[lpd->nr] = DoubleVal(vrhs, trhs);
    Check_Constant_Range(lpd->rhsx[lpd->nr]);
    lpd->rmatbeg[lpd->nr] = lpd->nnz;
    lpd->rcompl[lpd->nr] = vcompl.nint;	/* complement flag */
    lpd->rindind[lpd->nr] = vindind.nint;	/* indicator variable index */
    ++lpd->nr;

    Succeed;
}



#define Add_Row_Coeff(nnz_sz, nnzs, rmatind, rmatval, idxj, val, tag) {\
    if (nnzs >= nnz_sz)	/* allocate/grow arrays */\
    {\
        CallN(nnz_sz += NEWNZ_INCR);\
	CallN(rmatind = (int *) Realloc(rmatind, nnz_sz*sizeof(int)));\
	CallN(rmatval = (double *) Realloc(rmatval, nnz_sz*sizeof(double)));\
    }\
    if (idxj >= lpd->mac) { Bip_Error(RANGE_ERROR); }\
    rmatind[nnzs] = idxj+IDX_OFFSET;\
    rmatval[nnzs] = DoubleVal(val, tag);\
    Check_Constant_Range(rmatval[nnzs]);\
    ++nnzs;\
}

int
p_cpx_add_coeff(value vlp, type tlp, value vj, type tj, value v, type t, value vpool, type tpool, ec_eng_t *ec_eng)
{
    lp_desc *lpd;

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tj);
    Check_Number(t);
    Check_Integer(tpool);

    switch (vpool.nint)
    {
    case CSTR_TYPE_NORM:
	Add_Row_Coeff(lpd->nnz_sz, lpd->nnz, lpd->rmatind, lpd->rmatval, vj.nint, v, t);
	break;
/*
    case CSTR_TYPE_PERMCP:
	Add_Row_Coeff(lpd->cp_nz_sz, lpd->cp_nnz, lpd->cp_rmatind, lpd->cp_rmatval, vj.nint, v, t);
	break;
*/
    case CSTR_TYPE_CONDCP:
	Add_Row_Coeff(lpd->cp_nz_sz2, lpd->cp_nnz2, lpd->cp_rmatind2, lpd->cp_rmatval2, vj.nint, v, t);
	break;
    default:
	Bip_Error(RANGE_ERROR);
	break;
    }
    Succeed;
}

static void
_grow_numbers_array(lp_desc * lpd, int m)   /* make sure array contains 0..m-1 */
{
    if (m > lpd->numsz)	/* grow auxiliary array if necessary */
    {
	int i = lpd->numsz;
	m = Max(m, i+NEWCOL_INCR);
	lpd->numsz = m;
# ifdef LOG_CALLS
	if (i == 0)
	    Fprintf(log_output_, "\n\
	    	lpd->numbers = (int *) malloc(%d*sizeof(int));\n\
	    	lpd->zeroes = (int *) malloc(%d*sizeof(int));\n\
	        lpd->dzeroes = (double *) malloc(%d*sizeof(double));", m, m, m);
	else
	    Fprintf(log_output_, "\n\
		lpd->numbers = (int *) realloc(lpd->numbers, %d*sizeof(int));\n\
		lpd->zeroes = (int *) realloc(lpd->zeroes, %d*sizeof(int));\n\
	        lpd->dzeroes = (int *) realloc(lpd->dzeroes, %d*sizeof(double));", m, m, m);
	Fprintf(log_output_, "\n\
	    { int i; for (i=%d; i<%d; i++)\n\
		{ lpd->numbers[i] = i; lpd->zeroes[i] = 0; lpd->dzeroes[i] = 0.0; } }", i, m);
# endif
	if (i == 0) {
	    lpd->numbers = (int *) Malloc(m*sizeof(int));
	    lpd->zeroes = (int *) Malloc(m*sizeof(int));
	    lpd->dzeroes = (double *) Malloc(m*sizeof(double));
	} else {
	    lpd->numbers = (int *) Realloc(lpd->numbers, m*sizeof(int));
	    lpd->zeroes = (int *) Realloc(lpd->zeroes, m*sizeof(int));
	    lpd->dzeroes = (double *) Realloc(lpd->dzeroes, m*sizeof(double));
	}
	for (; i < m; i++) {
	    lpd->numbers[i] = i;
	    lpd->zeroes[i] = 0;
	    lpd->dzeroes[i] = 0.0;
	}
    }
}


static void _cpx_restore_bounds(pword * phandle, word * udata, int size, int undo_context, ec_eng_t *ec_eng)
{
    lp_desc *lpd = ExternalData(phandle[HANDLE_CPH].val.ptr); 

    if (!lpd)
	return; /* stale handle */

    if (lpd->descr_state != DESCR_EMPTY)
    {
	/* lp has not been cleaned up */

	int res;
	untrail_bound adata; /* needed to ensure proper alignment */

	memcpy(&adata, udata, sizeof(untrail_bound));

	res = cpx_setbds(lpd->lp, adata.idx+IDX_OFFSET, adata.bds[0], adata.bds[1]);
	if (res != 0) 
	{
	    Fprintf(Current_Error, "Eplex external solver error while trying to restore bounds.to column %d\n", adata.idx);
	    ec_flush(Current_Error);

	}
    }
}


static void
reset_sos(lp_desc * lpd, int oldsos)
{
    if (lpd->nsos_added > oldsos)
    {
	if (cpx_delsos(lpd, oldsos, lpd->nsos_added))
	{
	    Fprintf(Current_Error, "Error in deleting SOSs %d..%d\n",
		oldsos, lpd->nsos_added);
	    ec_flush(Current_Error);
	}
	lpd->nsos = lpd->nsos_added = oldsos;
    }
}


static void
reset_idc(lp_desc * lpd, int oldidc)
{
#ifdef HAS_INDICATOR_CONSTRAINTS
    if (lpd->nidc > oldidc)
    {
	if (CPXdelindconstrs(cpx_env, lpd->lp, oldidc, lpd->nidc-1))
	{
	    Fprintf(Current_Error, "Error in deleting indicator constraints %d..%d\n",
		oldidc, lpd->nidc-1);
	    ec_flush(Current_Error);
	}
	lpd->nidc = oldidc;
    }
#endif
}


static void
reset_rowcols(lp_desc * lpd, int oldmar, int oldmac)
{
    if (lpd->mar > oldmar)
    {
        if (cpx_delrangeofrows(lpd, oldmar+IDX_OFFSET, lpd->mar+IDX_OFFSET))
        {
            Fprintf(Current_Error, "Error in deleting rows %d..%d\n",
                    oldmar, lpd->mar-1);
            ec_flush(Current_Error);
        }
        lpd->mar = oldmar;
    }
    if (lpd->macadded > oldmac)
    {
        if (cpx_delrangeofcols(lpd, oldmac+IDX_OFFSET, lpd->macadded+IDX_OFFSET))
        {
            Fprintf(Current_Error, "Error in deleting cols %d..%d\n",
                oldmac, lpd->macadded-1);
            ec_flush(Current_Error);
        }
        lpd->macadded = oldmac;
    }
    lpd->mac = lpd->macadded;
    Mark_Copy_As_Modified(lpd);
}


static void _cpx_del_rowcols(pword * phandle,word * udata, int size, int flags, ec_eng_t *ec_eng)
{
    lp_desc *lpd = ExternalData(phandle[HANDLE_CPH].val.ptr); 

    int oldmar = ((untrail_data*) udata)->oldmar, 
        oldmac = ((untrail_data*) udata)->oldmac,
        oldsos = ((untrail_data*) udata)->oldsos,
        oldidc = ((untrail_data*) udata)->oldidc;

    if (lpd  &&  lpd->descr_state != DESCR_EMPTY)
    {
#if 0
	Fprintf(Current_Error,
	    "Removing rows %d..%d, cols %d..%d, soss %d..%d, idcs %d..%d, in gc:%d\n",
	    oldmar, lpd->mar-1, oldmac, lpd->macadded-1,
	    oldsos, lpd->nsos_added, oldidc, lpd->nidc,
	    ec_eng->vm_flags & NO_EXIT);
	ec_flush(Current_Error);
#endif
	reset_idc(lpd, oldidc);
	reset_sos(lpd, oldsos);
	reset_rowcols(lpd, oldmar, oldmac);
    }
}


/*
 * flush_new_rowcols(+Handle, +HasObjCoeffs) expects the following input:
 * 
 *	lpd->mac	new column count (>= macadded, those already added)
 *	lpd->objx	objective coefficients for new vars only (if HasObjCoeffs)
 *	lpd->matnz	number of nonzero coefficients for new vars in old constraints
 *	lpd->matxxx	those nonzero coefficients
 *	lpd->ctype	types for new vars only
 *
 *	lpd->nr		number of rows to add
 *	lpd->senx	row senses
 *	lpd->rhsx	row RHSs
 *	lpd->nnz	number of nonzero coefficients to add
 *	lpd->rmatxxx	those nonzero coefficients
 *
 * We may add only columns or only rows.
 */
/* newcolobjs == 1  if non-zero objective coeffs are to be added */
int
p_cpx_flush_new_rowcols(value vhandle, type thandle, value vnewcolobjs, type tnewcolobjs, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    int res, coladded, rowadded, nzadded;

    Check_Structure(thandle);
    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);
    Check_Integer(tnewcolobjs);
    coladded = lpd->mac - lpd->macadded;
    rowadded = lpd->nr;
    nzadded = lpd->nnz;

    /***
    Fprintf(Current_Error, "Adding rows %d..%d, cols %d..%d\n",
    	lpd->mar, lpd->mar+rowadded-1,
    	lpd->mac, lpd->mac+coladded-1);
    ec_flush(Current_Error);
    ***/

    SetPreSolve(lpd->presolve);
    Mark_Copy_As_Modified(lpd);

#ifdef LOG_CALLS
    {
	int i;
	Fprintf(log_output_, "\n\
		lpd->nr = %d;", lpd->nr);
	Fprintf(log_output_, "\n\
                lpd->matnz = %d;", lpd->matnz);
	/* needed by OSI */
	Fprintf(log_output_, "\n\
                lpd->mac = %d;", lpd->mac);
	if (vnewcolobjs.nint)
	{
	    for (i=0; i<coladded; ++i)
	    {
		Fprintf(log_output_, "\n\
                lpd->objx[%d] = %.15e;", i, lpd->objx[i]);
	    }
	}
	for (i=0; i<coladded; ++i)
	{
	    Fprintf(log_output_, "\n\
                lpd->bdl[%d] = %.15e;\n\
                lpd->bdu[%d] = %.15e;", i, lpd->bdl[i], i, lpd->bdu[i]);
	}
	for (i=0; i<lpd->matnz; ++i)
	{
	    Fprintf(log_output_, "\n\
                lpd->matind[%d] = %d;\n\
                lpd->matval[%d] = %.15e;", i, lpd->matind[i], i, lpd->matval[i]);
	}
	if (lpd->matnz > 0)
	{
	    for (i=0; i<coladded; ++i)
	    {
		Fprintf(log_output_, "\n\
                lpd->matbeg[%d] = %d;", i, lpd->matbeg[i]);
	    }
	}

	for (i=0; i<lpd->nr; ++i)
	{
	    Fprintf(log_output_, "\n\
		lpd->senx[%d] = '%c';\n\
		lpd->rhsx[%d] = %.15e;\n\
		lpd->rmatbeg[%d] = %d;",
	    	i, lpd->senx[i], i, lpd->rhsx[i], i, lpd->rmatbeg[i]);
	}
	Fprintf(log_output_, "\n\
		lpd->nnz = %d;", lpd->nnz);
	for (i=0; i<lpd->nnz; ++i)
	{
	    Fprintf(log_output_, "\n\
		lpd->rmatind[%d] = %d;\n\
		lpd->rmatval[%d] = %.15e;",
	    	i, lpd->rmatind[i], i, lpd->rmatval[i]);
	}
    }
#endif

    if (coladded)
    {
	_grow_numbers_array(lpd, coladded+1);	/* for zeroes[] */

	res = cpx_addcols(lpd->lp, coladded, lpd->matnz, 
			 (vnewcolobjs.nint ? lpd->objx : lpd->dzeroes),
			 (lpd->matnz ? lpd->matbeg : lpd->zeroes),
			 lpd->matind, lpd->matval, lpd->bdl, lpd->bdu);
	TryFree(lpd->objx);
	TryFree(lpd->matbeg);
	TryFree(lpd->matind);
	TryFree(lpd->matval);
	lpd->matnz = 0;
	if (lpd->dirtybdflag & 3)
	{
	    if (lpd->dirtybdflag & 1) TryFree(lpd->bdl);
	    if (lpd->dirtybdflag & 2) TryFree(lpd->bdu);
	    lpd->dirtybdflag = 0;
	}
			     
	if (IsMIPProb(lpd->prob_type) && res == 0)
	{
	    int i, colidx;
	    if (lpd->qgtype) CallN(Free(lpd->qgtype));
	    lpd->qgtype = NULL;
	    if (lpd->mgcols) CallN(Free(lpd->mgcols));
	    lpd->mgcols = Malloc(coladded*sizeof(int));
	    Log1(lpd->mgcols = (int *) malloc(%d*sizeof(int)), coladded);
	    /* we reuse mgcols for the index of the added columns. We set the
               type for all columns, as Dash does not specify what types
               columns are set to in XPRSaddcols().
	    */
	    for ((i=0, colidx=lpd->macadded); i < coladded; (i++, colidx++))
	    {
		lpd->mgcols[i] = colidx+IDX_OFFSET;
		Log2(lpd->mgcols[%d] = %d, i, colidx+IDX_OFFSET);
		Log2(lpd->ctype[%d] = %d, i, lpd->ctype[i]);
	    }
	    Update_Model(lpd->lp);	/* columns must be added before type can be set */
	    res = cpx_chgctype(lpd->lp, coladded, lpd->mgcols, lpd->ctype);
	}

	if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }
    }
    Update_Model(lpd->lp);	/* columns must be added before rows can be created */
    if (rowadded)
    {
        lpd->rmatbeg[lpd->nr] = lpd->nnz;
	res = cpx_addrows(lpd->lp, rowadded, nzadded,
			 lpd->rhsx, lpd->senx, lpd->rmatbeg, lpd->rmatind, 
			 lpd->rmatval);
	if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }
    }

    if (coladded || rowadded)
    {
      untrail_data udata;
      udata.oldmac = lpd->macadded;
      udata.oldmar = lpd->mar;
      udata.oldsos = lpd->nsos_added;
      udata.oldidc = lpd->nidc;
      ecl_trail_undo(ec_eng, _cpx_del_rowcols, vhandle.ptr, vhandle.ptr+HANDLE_STAMP, (word*) &udata, NumberOfWords(untrail_data), TRAILED_WORD32); 
    }

    lpd->macadded = lpd->mac;	/* remember what we added */
    lpd->mar += rowadded;

    lpd->nr = lpd->nnz = 0;	/* maybe shrink arrays here */
    Succeed;
}


/*
 * Add Indicator Constraints from descriptor arrays to solver
 * Input:
 *	lpd->nr		number of indicator rows to add
 *	lpd->senx	row senses
 *	lpd->rhsx	row RHSs
 *	lpd->nnz	number of nonzero coefficients to add
 *	lpd->rmatxxx	those nonzero coefficients
 *	lpd->rcompl	the complement flags
 *	lpd->rindind	the indicator variable indexes
 */

int
p_cpx_flush_idcs(value vhandle, type thandle, ec_eng_t *ec_eng)
{
#ifdef HAS_INDICATOR_CONSTRAINTS
    int i;
    lp_desc *lpd; 
    untrail_data udata;

    Check_Structure(thandle);
    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);

    if (lpd->nr == 0)
    	Succeed;

    /* trail first, in case we abort during adding */
    udata.oldmac = lpd->macadded;
    udata.oldmar = lpd->mar;
    udata.oldsos = lpd->nsos_added;
    udata.oldidc = lpd->nidc;
    ecl_trail_undo(ec_eng, _cpx_del_rowcols, vhandle.ptr, vhandle.ptr+HANDLE_STAMP, (word*) &udata, NumberOfWords(untrail_data), TRAILED_WORD32); 

    lpd->rmatbeg[lpd->nr] = lpd->nnz;
    for(i=0; lpd->nr>0; --lpd->nr,++i)
    {
#if 0
	int k;
	Fprintf(Current_Error, "CPXaddindconstr(%d,%d,%d,%f,%d,...,...)\n",
		lpd->rindind[i], lpd->rcompl[i],
		 lpd->rmatbeg[i+1]-lpd->rmatbeg[i],	/* nzcnt */
		 lpd->rhsx[i], lpd->senx[i]);
	for(k=lpd->rmatbeg[i];k<lpd->rmatbeg[i+1];++k) {
	    Fprintf(Current_Error, "%d:%f, ", lpd->rmatind[k], lpd->rmatval[k]);
	}
	(void) ec_newline(Current_Error);
	(void) ec_flush(Current_Error);
#endif

	if (CPXaddindconstr(cpx_env, lpd->lp, lpd->rindind[i], lpd->rcompl[i],
		 lpd->rmatbeg[i+1]-lpd->rmatbeg[i],	/* nzcnt */
		 lpd->rhsx[i], lpd->senx[i],
		 lpd->rmatind+lpd->rmatbeg[i], lpd->rmatval+lpd->rmatbeg[i],
		 NULL))
	{
	    Bip_Error(EC_EXTERNAL_ERROR);
	}
	++lpd->nidc;
    }
    /* could free/resize arrays here */
    Succeed;
#else
    Bip_Error(UNIMPLEMENTED);
#endif
}


/*----------------------------------------------------------------------*
 * Updating variable bounds
 *----------------------------------------------------------------------*/

/* the *impose* procedures are for columns that have been added to the
   external solver already.
*/
int 
p_cpx_impose_col_lwb(value vhandle, type thandle, 
		     value vatt, type tatt, 
		     value vj, type tj, 
		     value vlo, type tlo, 
		     value vchanged, type tchanged, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    double lo0, hi0, newlo;
    int j, changed = 0;
    Check_Integer(tj);
    Check_Float(tlo);
    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);

    if (lpd->descr_state == DESCR_EMPTY) 
    {
	Fprintf(Current_Error, "Eplex error: empty handle\n");
	(void) ec_flush(Current_Error);
	Bip_Error(EC_EXTERNAL_ERROR);
    }

    j = (int) vj.nint;
    if (j >= lpd->macadded) { Bip_Error(RANGE_ERROR); } 
    if ((newlo = Dbl(vlo)) < -CPX_INFBOUND) newlo = -CPX_INFBOUND; 

    Update_Model(lpd->lp);	/* make sure bounds are up-to-date */
    Get_Col_Bounds(j, lo0, hi0);    
    if (newlo > hi0) 
    { 
	if (newlo <= hi0 + cpx_feasibility_tol(lpd)) newlo = hi0;
	else { Fail; }
    }

#ifdef INTS_NEED_INT_BOUNDS
    if (IsMIPProb(lpd->prob_type))
    {
        char ctype;
        if (cpx_getctype(lpd->lp, &ctype, j+IDX_OFFSET))
            { Bip_Error(EC_EXTERNAL_ERROR); }
        if (ctype != 'C')
            newlo = ceil(newlo);
    }
#endif
    if (lo0 < newlo) 
    {
	Trail_Bounds(j, lo0, hi0, vatt.ptr+COL_STAMP);
        cpx_setlb(lpd->lp, j+IDX_OFFSET, newlo);
        changed = 1;
    }

    Return_Unify_Integer(vchanged, tchanged, changed);
}

int 
p_cpx_impose_col_upb(value vhandle, type thandle, 
		     value vatt, type tatt, 
		     value vj, type tj, 
		     value vhi, type thi, 
		     value vchanged, type tchanged, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    double lo0, hi0, newhi;
    int j, changed = 0;

    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);
    Check_Integer(tj);
    Check_Float(thi);

    if (lpd->descr_state == DESCR_EMPTY) 
    {
	Fprintf(Current_Error, "Eplex error: empty handle\n");
	(void) ec_flush(Current_Error);
	Bip_Error(EC_EXTERNAL_ERROR);
    }

    j = (int) vj.nint;
    if (j >= lpd->macadded) { Bip_Error(RANGE_ERROR); } 
    if ((newhi = Dbl(vhi)) > CPX_INFBOUND) newhi = CPX_INFBOUND;

    Update_Model(lpd->lp);	/* make sure bounds are up-to-date */
    Get_Col_Bounds(j, lo0, hi0);
    if (newhi < lo0) 
    { 
	if (newhi >= lo0 - cpx_feasibility_tol(lpd)) newhi = lo0;
	else { Fail; }
    }

#ifdef INTS_NEED_INT_BOUNDS
    if (IsMIPProb(lpd->prob_type))
    {
        char ctype;
        if (cpx_getctype(lpd->lp, &ctype, j+IDX_OFFSET))
            { Bip_Error(EC_EXTERNAL_ERROR); }
        if (ctype != 'C')
            newhi = floor(newhi);
    }
#endif

    if (hi0 > newhi) 
    {
	Trail_Bounds(j, lo0, hi0, vatt.ptr+COL_STAMP);
        cpx_setub(lpd->lp, j+IDX_OFFSET, newhi);
        changed = 1;
    }

    Return_Unify_Integer(vchanged, tchanged, changed);
}

int
p_cpx_impose_col_bounds(value vhandle, type thandle, 
			value vatt, type tatt, 
			value vj, type tj, 
			value vflag, type tflag, 
			value vlo, type tlo, 
			value vhi, type thi, 
			value vchanged, type tchanged, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    double  lo0, hi0, newlo, newhi;
    int     res, j, tighten_only, changed = 0;
    char    ctype[1];

    Check_Integer(tj);
    Check_Integer(tflag);
    Check_Float(thi);
    Check_Float(tlo);
    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);

    if (lpd->descr_state == DESCR_EMPTY) 
    {
	Fprintf(Current_Error, "Eplex error: empty handle\n");
	(void) ec_flush(Current_Error);
	Bip_Error(EC_EXTERNAL_ERROR);
    }

    Update_Model(lpd->lp);	/* make sure types and bounds are up-to-date */

    j = (int) vj.nint;
    if (j >= lpd->macadded || j < 0) { Bip_Error(RANGE_ERROR); } 
    if ((newhi = Dbl(vhi)) > CPX_INFBOUND) newhi = CPX_INFBOUND;
    if ((newlo = Dbl(vlo)) < -CPX_INFBOUND) newlo = -CPX_INFBOUND;

    tighten_only = (int) vflag.nint;

    if (IsMIPProb(lpd->prob_type))
    {
        if (!tighten_only) {
            /* We can widen the bound. Check to make sure that the new 
               bounds are not too wide, i.e. invalid for the column type
            */
            res = cpx_getctype(lpd->lp, ctype, j+IDX_OFFSET);
            if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }
            if (ctype[0] == 'B' && (newhi > 1 || newhi < 0 || newlo > 1 || newlo < 0))
                { Bip_Error(RANGE_ERROR); }     /* we would have to change the type */
#ifdef HAS_NARROW_INT_RANGE
            if (ctype[0] == 'I')
            {
                /* restrict silently */
                if (newlo < -XPRS_MAXINT) newlo = -XPRS_MAXINT;
                if (newhi >  XPRS_MAXINT) newhi =  XPRS_MAXINT;
            }
#endif
        }
#ifdef INTS_NEED_INT_BOUNDS
        else
        {
            res = cpx_getctype(lpd->lp, ctype, j+IDX_OFFSET);
            if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }
        }
    }
    else
    {
        ctype[0] = 'C';
#endif
    }

    if (newlo > newhi) { Fail; } 
    Get_Col_Bounds(j, lo0, hi0);
    if (tighten_only)
    {
        /* Loosen new bounds to avoid failure due to near misses */
        if (newhi < lo0)
        {
            /* if new bound outside but within tolerance of old bound, ignore change */
            if (newhi >= lo0 - cpx_feasibility_tol(lpd)) newhi = lo0;
            else { Fail; }
        }
        else if (newlo > hi0)
        { 
            if (newlo <= hi0 + cpx_feasibility_tol(lpd)) newlo = hi0;
            else { Fail; }
        }
    }
    
#ifdef INTS_NEED_INT_BOUNDS
    if (ctype[0] != 'C')
    {
        newlo = ceil(newlo);
        newhi = floor(newhi);
    }
#endif
    if (newlo > lo0 || (!tighten_only && newlo < lo0))
    {
        changed = 1;
        Trail_Bounds(j, lo0, hi0, vatt.ptr+COL_STAMP);
        if (newhi < hi0 || (!tighten_only && newhi > hi0))
            cpx_setbds(lpd->lp, j+IDX_OFFSET, newlo, newhi);
        else
            cpx_setlb(lpd->lp, j+IDX_OFFSET, newlo);
    }
    else if (newhi < hi0 || (!tighten_only && newhi > hi0))
    {
        changed = 1;
        Trail_Bounds(j, lo0, hi0, vatt.ptr+COL_STAMP);
        cpx_setub(lpd->lp, j+IDX_OFFSET, newhi);
    }
    Return_Unify_Integer(vchanged, tchanged, changed);
}

int
p_cpx_get_col_bounds(value vlp, type tlp, 
		     value vj, type tj, 
		     value vlo, type tlo, 
		     value vhi, type thi, ec_eng_t *ec_eng)
{
    Prepare_Requests
    lp_desc *lpd; 
    double lo, hi;
    int j;

    LpDesc(vlp, tlp, lpd);
    Check_Integer(tj);
    j = vj.nint;

    if (lpd->descr_state == DESCR_EMPTY) 
    {
	Fprintf(Current_Error, "Eplex error: empty handle\n");
	(void) ec_flush(Current_Error);
	Bip_Error(EC_EXTERNAL_ERROR);
    }

    if (lpd->lp == NULL)
    {/* solver not created for problem yet, get bounds from arrays */
	Request_Unify_Float(vhi, thi, lpd->bdu[j]);
	Request_Unify_Float(vlo, tlo, lpd->bdl[j]);
	Return_Unify;
    }
    else if (j >= lpd->mac || j < 0) 
    {/* invalid column index */
	Bip_Error(RANGE_ERROR);
    }
    else if (j >= lpd->macadded)
    {/* column not yet added to solver, get bounds from arrays */
	j -= lpd->macadded; /* arrays are for new added columns only */
	Request_Unify_Float(vhi, thi, lpd->bdu[j]);
	Request_Unify_Float(vlo, tlo, lpd->bdl[j]);
	Return_Unify;
    }
    else
    {
	Update_Model(lpd->lp);	/* before CPXget... */

        Get_Col_Bounds((int) vj.nint, lo, hi)
	Request_Unify_Float(vhi, thi, hi);
	Request_Unify_Float(vlo, tlo, lo);
	Return_Unify;
    }
}


/*
 * cplex_set_new_cols(CPH, AddedCols, NewObjCoeffs, NewLos, NewHis, NonZeros)
 * 
 * Sets the following fields:
 *	lpd->mac	+AddedCols
 *	lpd->matnz	+NonZeros
 *	lpd->matxxx	get resized for AddedCols and NonZeros
 *	lpd->ctype	resized to AddedCols only and initialised to 'C'
 *	lpd->bdl/bdu	resized to AddedCols or maczs?? and filled
 *	lpd->objx	resized to AddedCols, if NewObjCoeffs given
 */

int
p_cpx_set_new_cols(value vlp, type tlp, value vadded, type tadded, value vobjs, type tobjs, value vlos, type tlos, value vhis, type this, value vnzs, type tnzs, ec_eng_t *ec_eng)
{
    /* the column `buffer arrays' are needed by CPLEX and Xpress's interface
       to pass information for the columns. Except for ctype
       and possibly bdl, bdu, we simpy pass
       default values to the solver on setup/adding columns
    */
    lp_desc *lpd; 
    int i;

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tadded);
    Check_Integer(tnzs);
    if (vadded.nint == 0) { Succeed; } /* no added columns, return now! */

    lpd->mac += vadded.nint;
    lpd->matnz = vnzs.nint;
    TryFree(lpd->matind);
    TryFree(lpd->matval);
    if (vnzs.nint > 0)
    {
	lpd->matind = (int *) Malloc((size_t) vnzs.nint*sizeof(int));    
	lpd->matval = (double *) Malloc((size_t) vnzs.nint*sizeof(double));
	Log1(lpd->matind = (int *) malloc((size_t) %d*sizeof(int)), vnzs.nint);    
	Log1(lpd->matval = (double *) Malloc((size_t) %d*sizeof(double)), vnzs.nint);
    }
    TryFree(lpd->ctype);
    TryFree(lpd->matbeg);
    TryFree(lpd->matcnt);
    lpd->ctype = (char *) Malloc((size_t) vadded.nint*sizeof(char));
    /* +1 for matbeg required by COIN */
    lpd->matbeg = (int *) Malloc((size_t) (vadded.nint+1)*sizeof(int));
    lpd->matcnt = (int *) Malloc((size_t) vadded.nint*sizeof(int));
    Log3({\n\
	lpd->ctype = (char *) Malloc((size_t) %d*sizeof(char));\n\
	lpd->matbeg = (int *) Malloc((size_t) %d*sizeof(int));\n\
	lpd->matcnt = (int *) Malloc((size_t) %d*sizeof(int));\n\
    }, vadded.nint, vadded.nint, vadded.nint);

    for (i=0; i<vadded.nint; i++) lpd->ctype[i] = 'C';

    /* treatment of bounds arrays lpd->bdl, lpd->bdu:

       if columns with non-default bounds were added immediately previously
       the bounds arrays will have been freed in p_cpx_flush_new_rowcols so
       any existing bounds arrays contain default values and are length lpd->macsz

       1) if vadded.nint > lpd->macsz any existing arrays are too small:
          a) if we have non-default bounds to apply we free any existing array
	     and Malloc a new one of correct size vadded.nint since Realloc may
	     have to copy the contents of the existing array and free it and we
	     need to overwrite the entries again anyway
	  b) if we have default bounds to apply we just expand the existing array
	     with Realloc and initialize the new positions
       2) otherwise  any existing arrays are big enough:
          a) if we have non-default bounds to apply we overwrite the necessary entries
	  b) if we have default bounds to apply we have nothing to do

       if we have non-default bounds we set the appropriate bit of the lpd->dirtybdflag
       so that the arrays can be freed in p_cpx_flush_new_rowcols
    */
    if (vadded.nint > lpd->macsz) /* any existing bound arrays are too small */
    {
        if (IsList(tlos)) /* non-default lower bounds */
	{
	    /* since Realloc may copy and free and we need to overwrite entries
	       anyway it is probably better to free and Malloc */
	    TryFree(lpd->bdl);
	    lpd->bdl = (double *) Malloc((size_t) vadded.nint*sizeof(double));
	    Log1(lpd->bdl = (double *) malloc(%d*sizeof(double)), vadded.nint);
	    if (IsList(this)) /* non-default upper bounds */
	    {
	        /* both bounds arrays are non-default and will be freed
		   immediately after flushing: no need to increase lpd->macsz since
		   we will need to Malloc new bounds arrays of correct size next
		   time around anyway */
	        TryFree(lpd->bdu);
		lpd->bdu = (double *) Malloc((size_t) vadded.nint*sizeof(double));
		Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), vadded.nint);
		/* fill the bounds arrays with explicit bounds */
		for (i = 0; (IsList(tlos) && IsList(this)); ++i)
		{
		    pword *lhead = vlos.ptr;
		    pword *ltail = lhead + 1;
		    pword *hhead = vhis.ptr;
		    pword *htail = hhead + 1;
		    double lo, hi;
		    
		    Dereference_(lhead);
		    if (IsInteger(lhead->tag)) {
		      lo = (double) lhead->val.nint;
		    } else {
		      Check_Float(lhead->tag);
		      lo = Dbl(lhead->val);
		    }
		    lpd->bdl[i] = (lo < -CPX_INFBOUND ? -CPX_INFBOUND : lo);
		    Dereference_(hhead);
		    if (IsInteger(hhead->tag)) {
		      hi = (double) hhead->val.nint;
		    } else {
		      Check_Float(hhead->tag);
		      hi = Dbl(hhead->val);
		    }
		    lpd->bdu[i] = (hi > CPX_INFBOUND ? CPX_INFBOUND : hi);
		    Dereference_(ltail);
		    tlos = ltail->tag;
		    vlos = ltail->val;
		    Dereference_(htail);
		    this = htail->tag;
		    vhis = htail->val;
		}
		/* check that there are the right number of bds */
		if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
		/* mark both arrays as "dirty" */
		lpd->dirtybdflag |= 3;
	    }
	    else /* default upper bounds */
	    {
	        int newcsz;

		newcsz = Max(vadded.nint, lpd->macsz+NEWCOL_INCR);
		if (lpd->bdu == NULL) /* upper bounds array freed */
		{
		    lpd->bdu = (double *) Malloc((size_t) newcsz*sizeof(double));
		    Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), newcsz);

		    for (i=0; i<newcsz; i++)
		    {
		      lpd->bdu[i]  = CPX_INFBOUND;
		    }
		}
		else
		{
		    lpd->bdu = (double *) Realloc(lpd->bdu, (size_t) newcsz*sizeof(double));
		    Log1(lpd->bdu = (double *) realloc(%d*sizeof(double)), newcsz);
		    for (i=lpd->macsz; i<newcsz; i++)
		    {
		      lpd->bdu[i]  = CPX_INFBOUND;
		    }
		}
		lpd->macsz = newcsz;
		/* fill the lower bounds array with explicit bounds */
		for (i = 0; (IsList(tlos)); ++i)
		{
		    pword *lhead = vlos.ptr;
		    pword *ltail = lhead + 1;
		    double lo;
		    
		    Dereference_(lhead);
		    if (IsInteger(lhead->tag)) {
		      lo = (double) lhead->val.nint;
		    } else {
		      Check_Float(lhead->tag);
		      lo = Dbl(lhead->val);
		    }
		    lpd->bdl[i] = (lo < -CPX_INFBOUND ? -CPX_INFBOUND : lo);
		    Dereference_(ltail);
		    tlos = ltail->tag;
		    vlos = ltail->val;
		}
		/* check that there are the right number of bds */
		if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
		/* mark lower bounds array as "dirty" */
		lpd->dirtybdflag |= 1;
	    }
	}
	else if (IsList(this)) /* default lower bounds, non-default upper bounds */
	{
	    int newcsz;
	    
	    newcsz = Max(vadded.nint, lpd->macsz+NEWCOL_INCR);
	    if (lpd->bdl == NULL) /* lower bounds array freed */
	    {
	        lpd->bdl = (double *) Malloc((size_t) newcsz*sizeof(double));
		Log1(lpd->bdl = (double *) malloc(%d*sizeof(double)), newcsz);
		for (i=0; i<newcsz; i++)
		{
		  lpd->bdl[i]  = -CPX_INFBOUND;
		}
	    }
	    else
	    {
	        lpd->bdl = (double *) Realloc(lpd->bdl, (size_t) newcsz*sizeof(double));
		Log1(lpd->bdl = (double *) realloc(%d*sizeof(double)), newcsz);
		for (i=lpd->macsz; i<newcsz; i++)
		{
		  lpd->bdl[i]  = -CPX_INFBOUND;
		}
	    }
	    lpd->macsz = newcsz;
	    TryFree(lpd->bdu);
	    lpd->bdu = (double *) Malloc((size_t) vadded.nint*sizeof(double));
	    Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), vadded.nint);
	    /* fill the upper bounds array with explicit bounds */
	    for (i = 0; (IsList(this)); ++i)
	    {
	        pword *hhead = vhis.ptr;
		pword *htail = hhead + 1;
		double hi;
		
		Dereference_(hhead);
		if (IsInteger(hhead->tag)) {
		  hi = (double) hhead->val.nint;
		} else {
		  Check_Float(hhead->tag);
		  hi = Dbl(hhead->val);
		}
		lpd->bdu[i] = (hi > CPX_INFBOUND ? CPX_INFBOUND : hi);
		Dereference_(htail);
		this = htail->tag;
		vhis = htail->val;
	    }
	    /* check that there are the right number of bds */
	    if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
	    /* mark upper bounds array as "dirty" */
	    lpd->dirtybdflag |= 2;
	}
	else /* default bounds */
	{
	    int newcsz;
	    
	    newcsz = Max(vadded.nint, lpd->macsz+NEWCOL_INCR);
	    if (lpd->bdl == NULL) /* lower bounds array freed */
	    {
	        lpd->bdl = (double *) Malloc((size_t) newcsz*sizeof(double));
		Log1(lpd->bdl = (double *) malloc(%d*sizeof(double)), newcsz);
		for (i=0; i<newcsz; i++)
		{
		  lpd->bdl[i]  = -CPX_INFBOUND;
		}
	    }
	    else
	    {
	        lpd->bdl = (double *) Realloc(lpd->bdl, (size_t) newcsz*sizeof(double));
		Log1(lpd->bdl = (double *) realloc(%d*sizeof(double)), newcsz);
		for (i=lpd->macsz; i<newcsz; i++)
		{
		  lpd->bdl[i]  = -CPX_INFBOUND;
		}
	    }
	    if (lpd->bdu == NULL) /* upper bounds array freed */
	    {
	        lpd->bdu = (double *) Malloc((size_t) newcsz*sizeof(double));
		Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), newcsz);
		for (i=0; i<newcsz; i++)
		{
		  lpd->bdu[i]  = CPX_INFBOUND;
		}
	    }
	    else
	    {
	        lpd->bdu = (double *) Realloc(lpd->bdu, (size_t) newcsz*sizeof(double));
		Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), newcsz);
		for (i=lpd->macsz; i<newcsz; i++)
		{
		  lpd->bdu[i]  = CPX_INFBOUND;
		}
	    }
	    lpd->macsz = newcsz;
	}
    }
    else /* any existing bound arrays are big enough */
    {
        if (IsList(tlos)) /* non-default lower bounds */
	{
	    if (lpd->bdl == NULL) 
	    {
		lpd->bdl = (double *) Malloc((size_t) vadded.nint*sizeof(double));
		Log1(lpd->bdl = (double *) malloc(%d*sizeof(double)), vadded.nint);
	    }
	    if (IsList(this)) /* non-default upper bounds */
	    {
	        if (lpd->bdu == NULL) 
                {
		    lpd->bdu = (double *) Malloc((size_t) vadded.nint*sizeof(double));
		    Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), vadded.nint);
		}
		/* fill the bounds arrays with explicit bounds */
		for (i = 0; (IsList(tlos) && IsList(this)); ++i)
		{
		    pword *lhead = vlos.ptr;
		    pword *ltail = lhead + 1;
		    pword *hhead = vhis.ptr;
		    pword *htail = hhead + 1;
		    double lo, hi;
		    
		    Dereference_(lhead);
		    if (IsInteger(lhead->tag)) {
		      lo = (double) lhead->val.nint;
		    } else {
		      Check_Float(lhead->tag);
		      lo = Dbl(lhead->val);
		    }
		    lpd->bdl[i] = (lo < -CPX_INFBOUND ? -CPX_INFBOUND : lo);
		    Dereference_(hhead);
		    if (IsInteger(hhead->tag)) {
		      hi = (double) hhead->val.nint;
		    } else {
		      Check_Float(hhead->tag);
		      hi = Dbl(hhead->val);
		    }
		    lpd->bdu[i] = (hi > CPX_INFBOUND ? CPX_INFBOUND : hi);
		    Dereference_(ltail);
		    tlos = ltail->tag;
		    vlos = ltail->val;
		    Dereference_(htail);
		    this = htail->tag;
		    vhis = htail->val;
		}
		/* check that there are the right number of bds */
		if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
		/* mark both arrays as "dirty" */
		lpd->dirtybdflag |= 3;
	    }
	    else /* default upper bounds */
	    {
	        if (lpd->bdu == NULL) /* upper bounds array freed */
		{
		    lpd->bdu = (double *) Malloc((size_t) lpd->macsz*sizeof(double));
		    Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), lpd->macsz);
		    for (i=0; i<lpd->macsz; i++)
		    {
		      lpd->bdu[i]  = CPX_INFBOUND;
		    }
		}
		/* fill the lower bounds array with explicit bounds */
		for (i = 0; (IsList(tlos)); ++i)
		{
		    pword *lhead = vlos.ptr;
		    pword *ltail = lhead + 1;
		    double lo;
		    
		    Dereference_(lhead);
		    if (IsInteger(lhead->tag)) {
		      lo = (double) lhead->val.nint;
		    } else {
		      Check_Float(lhead->tag);
		      lo = Dbl(lhead->val);
		    }
		    lpd->bdl[i] = (lo < -CPX_INFBOUND ? -CPX_INFBOUND : lo);
		    Dereference_(ltail);
		    tlos = ltail->tag;
		    vlos = ltail->val;
		}
		/* check that there are the right number of bds */
		if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
		/* mark lower bounds array as "dirty" */
		lpd->dirtybdflag |= 1;
	    }
	}
	else if (IsList(this)) /* default lower bounds, non-default upper bounds */
	{
	    if (lpd->bdl == NULL) /* lower bounds array freed */
	    {
	        lpd->bdl = (double *) Malloc((size_t) lpd->macsz*sizeof(double));
		Log1(lpd->bdl = (double *) malloc(%d*sizeof(double)), lpd->macsz);
		for (i=0; i<lpd->macsz; i++)
		{
		  lpd->bdl[i]  = -CPX_INFBOUND;
		}
	    }
	    /* fill the upper bounds array with explicit bounds */
	    for (i = 0; (IsList(this)); ++i)
	    {
	        pword *hhead = vhis.ptr;
		pword *htail = hhead + 1;
		double hi;
		
		Dereference_(hhead);
		if (IsInteger(hhead->tag)) {
		  hi = (double) hhead->val.nint;
		} else {
		  Check_Float(hhead->tag);
		  hi = Dbl(hhead->val);
		}
		lpd->bdu[i] = (hi > CPX_INFBOUND ? CPX_INFBOUND : hi);
		Dereference_(htail);
		this = htail->tag;
		vhis = htail->val;
	    }
	    /* check that there are the right number of bds */
	    if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
	    /* mark upper bounds array as "dirty" */
	    lpd->dirtybdflag |= 2;
	}
	else /* default bounds */
	{
	    if (lpd->bdl == NULL) /* lower bounds array freed */
	    {
	        lpd->bdl = (double *) Malloc((size_t) lpd->macsz*sizeof(double));
		Log1(lpd->bdl = (double *) malloc(%d*sizeof(double)), lpd->macsz);
		for (i=0; i<lpd->macsz; i++)
		{
		  lpd->bdl[i]  = -CPX_INFBOUND;
		}
	    }
	    if (lpd->bdu == NULL) /* upper bounds array freed */
	    {
	        lpd->bdu = (double *) Malloc((size_t) lpd->macsz*sizeof(double));
		Log1(lpd->bdu = (double *) malloc(%d*sizeof(double)), lpd->macsz);
		for (i=0; i<lpd->macsz; i++)
		{
		  lpd->bdu[i]  = CPX_INFBOUND;
		}
	    }
	}
    }

    /* fill the objective coefficients array if specified */
    if (IsList(tobjs)) 
    {

	if (IsList(tobjs)) /* only if there are objective coefficients */
	{
	    TryFree(lpd->objx);
	    Log1(lpd->objx = (double *) malloc((size_t) %d*sizeof(double)), vadded.nint);
	    lpd->objx = (double *) Malloc((size_t) vadded.nint*sizeof(double));
	}

	for (i = 0; IsList(tobjs); ++i)
	{
	    pword *head = vobjs.ptr;
	    pword *tail = head + 1;
	    double coeff;

	    Dereference_(head);
	    if (IsInteger(head->tag)) {
		coeff = (double) head->val.nint;
	    } else {
		Check_Float(head->tag);
		coeff = Dbl(head->val);
		Check_Constant_Range(coeff);
	    }
	    lpd->objx[i] = coeff;
	    Dereference_(tail);
	    tobjs = tail->tag;
	    vobjs = tail->val;
	}
	/* check that there are the right number of objs */
	if (i != vadded.nint) { Bip_Error(RANGE_ERROR) }
    }

    Succeed;
}


int
p_cpx_init_type(value vlp, type tlp, value vj, type tj, value vtype, type ttype, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    int j;

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tj);
    j = vj.nint;

    if (j >= lpd->mac) { Bip_Error(RANGE_ERROR); }
    if (j >= lpd->macadded) j -= lpd->macadded; /* added col */
    lpd->ctype[j] = (char) vtype.nint;
    if (vtype.nint != 'C')
    {
#ifdef INTS_NEED_INT_BOUNDS
        /* should we allow tolerance here? */
        lpd->bdl[j] = ceil(lpd->bdl[j]);
        lpd->bdu[j] = floor(lpd->bdu[j]);
#endif
	switch (lpd->prob_type)
	{
	case PROBLEM_LP:
	    lpd->prob_type = PROBLEM_MIP;
	    break;
#ifdef HAS_MIQP
	case PROBLEM_QP:
	    lpd->prob_type = PROBLEM_MIQP;
	    break;
	case PROBLEM_MIQP:
#endif
	case PROBLEM_MIP:
	    break;
	default:
	    Fprintf(Current_Error, "Eplex error: this solver does not support solving of quadratic MIP problems.\n");
	    ec_flush(Current_Error);
	    Bip_Error(EC_EXTERNAL_ERROR);
	    break;
	}

#ifdef XPRESS
    	++lpd->ngents;
#endif
    }
    Succeed;
}


/* Set bounds for new variables in buffer arrays.
 * Used for initial setup and for adding variables.
 */
int
p_cpx_init_bound(value vlp, type tlp, value vj, type tj, value vwhich, type twhich, value vval, type tval, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    int j;
    double bd;

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tj);
    Check_Atom(twhich);
    Check_Float(tval);
    bd = Dbl(vval);

    j = vj.nint;
    if (j >= lpd->mac) { Bip_Error(RANGE_ERROR); }
    if (j >= lpd->macadded) j -= lpd->macadded; /* added col */

    if (vwhich.did == d_le) {		/* upper bound */
	if (bd < lpd->bdl[j]) { Fail; }
	if (bd < lpd->bdu[j]) { lpd->bdu[j] = bd; lpd->dirtybdflag |= 2; }

    } else if (vwhich.did == d_ge) {	/* lower bound */
	if (bd > lpd->bdu[j]) { Fail; }
	if (bd > lpd->bdl[j]) { lpd->bdl[j] = bd; lpd->dirtybdflag |= 1; }

    } else if (vwhich.did == d_eq) {	/* both bounds */
	if (bd < lpd->bdl[j]  ||  lpd->bdu[j] < bd) { Fail; }
	lpd->bdl[j] = lpd->bdu[j] = bd;
	lpd->dirtybdflag |= 3;

    } else {
	Bip_Error(RANGE_ERROR);
    }
    Succeed;
}


/*----------------------------------------------------------------------*
 * Retrieving variable type and bounds
 *----------------------------------------------------------------------*/


int
p_cpx_get_col_type(value vlp, type tlp, value vj, type tj, value vtype, type ttype, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    char ctype[1];

    LpDesc(vlp, tlp, lpd);
    Check_Integer(tj);
    if (vj.nint >= lpd->mac || vj.nint < 0) { Bip_Error(RANGE_ERROR); }
    SetPreSolve(lpd->presolve);
    if (IsMIPProb(lpd->prob_type))
    {
	Update_Model(lpd->lp);	/* before CPXget... */
	if (cpx_getctype(lpd->lp, ctype, (int) vj.nint+IDX_OFFSET))
	{
	    Bip_Error(EC_EXTERNAL_ERROR);
	}
    }
    else
    {
	ctype[0] = 'C';
    }
    Return_Unify_Integer(vtype, ttype, (int) ctype[0]);
}

/*----------------------------------------------------------------------*
 * Updating objective
 *----------------------------------------------------------------------*/

static void
_grow_cb_arrays(lp_desc * lpd, int with_index2)
{
    if (lpd->cb_sz == 0)
    {
#ifdef LOG_CALLS
    Fprintf(log_output_, "\n\
	lpd->cb_sz = %d;\n\
	lpd->cb_index = (int *) malloc(lpd->cb_sz*sizeof(int));\n\
	lpd->cb_value = (double *) malloc(lpd->cb_sz*sizeof(double));",
	    NEWBD_INCR);
    if (with_index2)
	Fprintf(log_output_, "\n\
	    lpd->cb_index2 = (int *) malloc(lpd->cb_sz*sizeof(int));");
    ec_flush(log_output_);
#endif
	lpd->cb_sz = NEWBD_INCR;
	lpd->cb_index = (int *) Malloc(NEWBD_INCR*sizeof(int));
	if (with_index2)
	    lpd->cb_index2 = (int *) Malloc(NEWBD_INCR*sizeof(int));
	lpd->cb_value = (double *) Malloc(NEWBD_INCR*sizeof(double));
    }
    else
    {
#ifdef LOG_CALLS
    Fprintf(log_output_, "\n\
	lpd->cb_sz += %d;\n\
	lpd->cb_index = (int *) realloc(lpd->cb_index, lpd->cb_sz*sizeof(int));\n\
	lpd->cb_value = (double *) realloc(lpd->cb_value, lpd->cb_sz*sizeof(double));",
	    NEWBD_INCR);
    if (with_index2)
    {
	Fprintf(log_output_, "\n\
	    lpd->cb_index2 = lpd->cb_index2\n\
	    	? (int *) realloc(lpd->cb_index2, lpd->cb_sz*sizeof(int))\n\
		: (int *) malloc(lpd->cb_sz*sizeof(int));");
    }
    else if (lpd->cb_index2)
    {
	Fprintf(log_output_, "\n\
		free(lpd->cb_index2);\n\
		lpd->cb_index2 = 0;");
    }
    ec_flush(log_output_);
#endif
	lpd->cb_sz += NEWBD_INCR;
	lpd->cb_index = (int *) Realloc(lpd->cb_index, lpd->cb_sz*sizeof(int));
	lpd->cb_value = (double *) Realloc(lpd->cb_value, lpd->cb_sz*sizeof(double));
	if (with_index2)
	{
	    lpd->cb_index2 = lpd->cb_index2
	    	? (int *) Realloc(lpd->cb_index2, lpd->cb_sz*sizeof(int))
		: (int *) Malloc(lpd->cb_sz*sizeof(int));
	}
	else if (lpd->cb_index2)
	{
	    Free(lpd->cb_index2);
	    lpd->cb_index2 = 0;
	}
    }
}


int
p_cpx_new_obj_coeff(value vlp, type tlp, value vj, type tj, value vcoeff, type tcoeff, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    double coeff;
    int i;

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tj);
    if (vj.nint >= lpd->mac)
       { Bip_Error(RANGE_ERROR); }

    if (IsInteger(tcoeff)) {
	coeff = (double) vcoeff.nint;
    } else {
	Check_Float(tcoeff);
	coeff = Dbl(vcoeff);
	Check_Constant_Range(coeff);
    }
    if (lpd->cb_cnt >= lpd->cb_sz)	/* grow arrays if necessary */
    {
	_grow_cb_arrays(lpd, 0);
    }
    i = lpd->cb_cnt++;
    lpd->cb_index[i] = vj.nint+IDX_OFFSET;
    lpd->cb_value[i] = coeff;
    Succeed;
}

int
p_cpx_flush_obj(value vlp, type tlp, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    LpDesc(vlp, tlp, lpd);
    if (lpd->cb_cnt == 0)
    {
    	Succeed;
    }
    SetPreSolve(lpd->presolve);
    Mark_Copy_As_Modified(lpd);
    if (cpx_chgobj(lpd->lp, lpd->cb_cnt, lpd->cb_index, lpd->cb_value))
    {
	Bip_Error(EC_EXTERNAL_ERROR);
    }
    lpd->cb_cnt = 0;
    Succeed;
}

int
p_cpx_new_qobj_coeff(value vlp, type tlp, 
		     value vi, type ti, 
		     value vj, type tj, 
		     value vcoeff, type tcoeff, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    double coeff;

    Check_Integer(ti);
    Check_Integer(tj);
    LpDesc(vlp, tlp, lpd);
    if (vj.nint >= lpd->mac || vi.nint >= lpd->mac)
       { Bip_Error(RANGE_ERROR); }

    if (IsInteger(tcoeff)) {
	coeff = (double) vcoeff.nint;
    } else {
	Check_Float(tcoeff);
	coeff = Dbl(vcoeff);
	Check_Constant_Range(coeff);
    }
    if (vi.nint==vj.nint)
    	coeff *= 2;
    SetPreSolve(lpd->presolve);
    if (cpx_chgqpcoef(lpd->lp, vi.nint+IDX_OFFSET, vj.nint+IDX_OFFSET, coeff))
    {
	Bip_Error(EC_EXTERNAL_ERROR);
    }
    Succeed;
}


int
p_cpx_change_obj_sense(value vlp, type tlp, value vsense, type tsense, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 

    Check_Integer(tsense);
    LpDesc(vlp, tlp, lpd);

    SetPreSolve(lpd->presolve);
    lpd->sense = vsense.nint;
    cpx_chgobjsen(lpd->lp, vsense.nint);
#ifdef SOLVE_MIP_COPY
    if (lpd->copystatus != XP_COPYOFF) Mark_Copy_As_Modified(lpd);
#endif
    Succeed;
}

/*----------------------------------------------------------------------*
 * Initial matrix setup
 *----------------------------------------------------------------------*/

/* set_coeffs(+CPH, +Coeffs)
 * ColCoeffs is either a list or an array of lists of Row:Coeff structures.
 * Column index range is macadded..mac-1.
 */

static int
_store_column_coeffs(lp_desc *lpd, int colidx, pword *plist, int *pnz, ec_eng_t *ec_eng)
{
    int k = *pnz;
    int j = colidx - lpd->macadded;

    if (colidx >= lpd->mac)
        { Bip_Error(RANGE_ERROR); }

    lpd->matbeg[j] = k;
    Dereference_(plist);
    while (IsList(plist->tag)) {
        int rowidx;
        pword *prowidx, *prowcoef;
        pword *pelem = &plist->val.ptr[0];
        Dereference_(pelem);
        Check_Structure(pelem->tag);
        pelem = pelem->val.ptr;
        if (pelem->val.did != d_colon2)
            return TYPE_ERROR;

        prowidx = &pelem[1];
        Dereference_(prowidx);
        Check_Integer(prowidx->tag);
        rowidx = prowidx->val.nint;
        if (k >= lpd->matnz || rowidx >= lpd->mar || rowidx < 0)
            return RANGE_ERROR;
        lpd->matind[k] = rowidx+IDX_OFFSET;

        prowcoef = &pelem[2];
        Dereference_(prowcoef);
        Check_Number(prowcoef->tag);
        lpd->matval[k] = DoubleVal(prowcoef->val, prowcoef->tag);
        Check_Constant_Range(lpd->matval[k]);

        ++k;
        plist = &plist->val.ptr[1];
        Dereference_(plist);
    }
    if (!(IsNil(plist->tag) || IsRef(plist->tag)))
        return TYPE_ERROR;
    lpd->matcnt[j] = k - *pnz;
    lpd->matbeg[j+1] = k;       /* needed if matcnt is not used */

    *pnz = k;
    return PSUCCEED;
}

int
p_cpx_set_coeffs(value vlp, type tlp, value vcs, type tcs, ec_eng_t *ec_eng)
{
    lp_desc *lpd;
    int colidx;         /* column index */
    int nz = 0;         /* counts added nonzeros */

    LpDescOnly(vlp, tlp, lpd);
    colidx = lpd->macadded;
    if (IsNil(tcs) || IsAtom(tcs) && vcs.did == d_empty)
    {
        Succeed;
    }
    else if (IsList(tcs))
    {
        pword *pelem = vcs.ptr;
        pword *plist;
        do {
            int res = _store_column_coeffs(lpd, colidx, pelem, &nz, ec_eng);
            if (res != PSUCCEED) { Bip_Error(res) }
            plist = pelem+1;
            Dereference_(plist);
            pelem=plist->val.ptr;
            ++colidx;
        } while (IsList(plist->tag));
        Check_Nil(plist->tag);
    }
    else
    {
        int i;
	Check_Structure(tcs);
        for(i=1; i<=DidArity(vcs.ptr->val.did); ++i,++colidx)
        {
            int res = _store_column_coeffs(lpd, colidx, &vcs.ptr[i], &nz, ec_eng);
            if (res != PSUCCEED) { Bip_Error(res) }
        }
    }
    if (nz != lpd->matnz)
    {
        Fprintf(Current_Error, "Eplex error: nonzero count mismatch.\n");
	ec_flush(Current_Error);
	Bip_Error(EC_EXTERNAL_ERROR);
    }
    Succeed;
}


int
p_cpx_loadprob(value vlp, type tlp, ec_eng_t *ec_eng)
{
    int err;
    lp_desc *lpd; 
    LpDescOnly(vlp, tlp, lpd);

    SetPreSolve(lpd->presolve);
    lpd->start_mac = lpd->mac; 

    if (lpd->nsos) {
	if (lpd->prob_type == PROBLEM_QP) 
	    lpd->prob_type = PROBLEM_MIQP;
	else 
	    lpd->prob_type = PROBLEM_MIP;
    }

#ifndef HAS_MIQP
    if (lpd->prob_type == PROBLEM_MIQP)
    {
	Fprintf(Current_Error, "Eplex error: this solver does not support solving of quadratic MIP problems.\n");
	ec_flush(Current_Error);
	Bip_Error(UNIMPLEMENTED);
    }
#endif
#ifdef LOG_CALLS
    {
    int i;
# ifndef DUMPMAT
#  ifdef XPRESS
    Fprintf(log_output_, "\n\
	lpd->probname = (char *) malloc(16*sizeof(char));\n\
	strcpy(lpd->probname, \"eclipse\");"
    );
#  endif
#  ifdef CPLEX
    Log1(lpd->sense = %d, lpd->sense);
#  endif
    Fprintf(log_output_, "\n\
	lpd->sense = %d;\n\
    	lpd->macsz = %d;\n\
    	lpd->marsz = %d;\n\
    	lpd->matnz = %d;\n\
    	lpd->mac = %d;\n\
    	lpd->mar = %d;\n\
	lpd->rhsx = (double *) malloc(lpd->marsz * sizeof(double));\n\
	lpd->senx = (char *) malloc(lpd->marsz * sizeof(char));\n\
	lpd->matbeg = (int *) malloc((lpd->macsz+1) * sizeof(int));\n\
	lpd->matcnt = (int *) malloc(lpd->macsz * sizeof(int));\n\
	lpd->matind = (int *) malloc(lpd->matnz * sizeof(int));\n\
	lpd->matval = (double *) malloc(lpd->matnz * sizeof(double));\n\
	lpd->bdl = (double *) malloc(lpd->macsz * sizeof(double));\n\
	lpd->bdu = (double *) malloc(lpd->macsz * sizeof(double));\n\
	lpd->objx = (double *) malloc(lpd->macsz * sizeof(double));\n\
	lpd->ctype = (char *) malloc(lpd->macsz * sizeof(char));",
	lpd->sense,(lpd->macsz ? lpd->macsz : 1), (lpd->marsz ? lpd->marsz: 1), (lpd->matnz? lpd->matnz : 1),
	lpd->mac, lpd->mar);

    for (i=0; i<lpd->mac; ++i)
    {
	Fprintf(log_output_, "\n\tlpd->objx[%d] = %.15e;", i, lpd->objx[i]);
	Fprintf(log_output_, "\n\tlpd->bdl[%d] = %.15e;", i, lpd->bdl[i]);
	Fprintf(log_output_, "\n\tlpd->bdu[%d] = %.15e;", i, lpd->bdu[i]);
	Fprintf(log_output_, "\n\tlpd->matbeg[%d] = %d;", i, lpd->matbeg[i]);
	Fprintf(log_output_, "\n\tlpd->matcnt[%d] = %d;", i, lpd->matcnt[i]);
    }
    for (i=0; i<lpd->mar; ++i)
    {
	Fprintf(log_output_, "\n\tlpd->rhsx[%d] = %.15e;", i, lpd->rhsx[i]);
	Fprintf(log_output_, "\n\tlpd->senx[%d] = '%c';", i, lpd->senx[i]);
    }
    for (i=0; i<lpd->matnz; ++i)
    {
	Fprintf(log_output_, "\n\tlpd->matind[%d] = %d;", i, lpd->matind[i]);
	Fprintf(log_output_, "\n\tlpd->matval[%d] = %.15e;", i, lpd->matval[i]);
    }
# else  /* DUMPMAT */
    dump_problem(lpd);
# endif
    }
#endif /* LOG_CALLS */

    lpd->lp = NULL;
    if (cpx_loadprob(lpd))      /* do solver-specific setup */
    {
	Bip_Error(EC_EXTERNAL_ERROR);
    }

    /* free our copy of the problem */
    CallN(Free(lpd->rhsx)); lpd->rhsx = NULL;
    CallN(Free(lpd->senx)); lpd->senx = NULL;
    CallN(Free(lpd->matbeg)); lpd->matbeg = NULL;
    CallN(Free(lpd->matcnt)); lpd->matcnt = NULL;
    CallN(Free(lpd->matind)); lpd->matind = NULL;
    CallN(Free(lpd->matval)); lpd->matval = NULL;
    CallN(Free(lpd->bdl)); lpd->bdl = NULL;
    CallN(Free(lpd->bdu)); lpd->bdu = NULL;
    CallN(Free(lpd->ctype)); lpd->ctype = NULL;
    CallN(Free(lpd->objx)); lpd->objx = NULL;
    CallN(lpd->matnz = 0);
    lpd->macsz = 0;

    if (lpd->nsos)
    {
        CallN(lpd->nsos_added = lpd->nsos);
	CallN(lpd->nsosnz = 0);
	CallN(Free(lpd->sostype)); lpd->sostype = NULL;
	CallN(Free(lpd->sosbeg)); lpd->sosbeg = NULL;
	lpd->sossz = 0;
	CallN(Free(lpd->sosind)); lpd->sosind = NULL;
	CallN(Free(lpd->sosref)); lpd->sosref = NULL;
	lpd->sosnzsz = 0;
    }

    lpd->macadded = lpd->mac;
    Succeed;
}

/* add initial cutpool constraints to problem */
static int 
_setup_initial_cp_constraints(lp_desc * lpd, int add_all, int * unadded_cntp, 
			      int * cp_unadded, int * cp_map2)
{
    double * rhs, * rmatval;
    int * rmatbeg, * rmatind, i, offset, first = -1, 
	cp_rcnt2 = 0, rcnt = 0;
    char * senx;

                        
    rmatbeg = (int *)Malloc((lpd->cp_nr2+1)*sizeof(int));

    for (i=0; i < lpd->cp_nr2; i++) 
    {
	if (lpd->cp_active2[i] == 1) 
	{
	    if (lpd->cp_initial_add2[i] == 1 || add_all)
	    {/* active, added initially (or add all active constraints) */
		if (first == -1) 
		{
		    first = i;
		    offset = lpd->cp_rmatbeg2[first];
		}
		/* rmatbeg need to be offset from the start of array */
		rmatbeg[cp_rcnt2] = lpd->cp_rmatbeg2[i] - offset;
		cp_map2[i] = cp_rcnt2++;
		rcnt++;
		continue;
	    } else 
	    { /* active, but not added initially */
		cp_map2[i] = CSTR_STATE_NOTADDED; /* not added yet */
		cp_unadded[(*unadded_cntp)++] = i;
	    }
	} else
	{/* not active */
	    cp_map2[i] = CSTR_STATE_INACTIVE; /* not active */
	}

	if (rcnt > 0) 
	{/* there are some rows to add... */
	    rhs = &lpd->cp_rhsx2[first];
	    rmatind = &lpd->cp_rmatind2[offset];
	    rmatval = &lpd->cp_rmatval2[offset];
	    senx   =  &lpd->cp_senx2[first];
	    cpx_addrows(lpd->lp, rcnt, (lpd->cp_rmatbeg2[i] - offset), 
		       rhs, senx, rmatbeg, rmatind, rmatval);
	    rcnt = 0;
	    first = -1;
	}

    }

    if (rcnt > 0) 
    {/* there are some rows to add... */
	rhs = &lpd->cp_rhsx2[first];
	rmatind = &lpd->cp_rmatind2[offset];
	rmatval = &lpd->cp_rmatval2[offset];
	senx   =  &lpd->cp_senx2[first];
	cpx_addrows(lpd->lp, rcnt, (lpd->cp_nnz2 - offset), 
		   rhs, senx, rmatbeg, rmatind, rmatval);
	rcnt = 0;
	first = -1;
    }

    lpd->mar += cp_rcnt2;
    lpd->cp_nact2 = cp_rcnt2;
    if (cp_rcnt2 > 0) { Mark_Copy_As_Modified(lpd); }
    Free(rmatbeg);

    return 0;
}

/*----------------------------------------------------------------------*
 * Read/write
 *----------------------------------------------------------------------*/

int
p_cpx_lpwrite(value vfile, type tfile, value vformat, type tformat, 
	      value vlp, type tlp, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    char has_cp = 0, *file, *format;
    int oldmar, res;
    Get_Name(vformat, tformat, format);
    Get_Name(vfile, tfile, file);
    LpDesc(vlp, tlp, lpd);

    oldmar = lpd->mar;

    SetPreSolve(lpd->presolve);
    if (lpd->cp_nr2 > 0)
    {
	int unadded_cnt = 0, * cp_unadded, * cp_map2;

	cp_unadded = (int *)Malloc(lpd->cp_nr2*sizeof(int));
	cp_map2 = (int *)Malloc(lpd->cp_nr2*sizeof(int));
	if (_setup_initial_cp_constraints(lpd, 1, &unadded_cnt, cp_unadded, cp_map2) == -1) 
	{ 
	    reset_rowcols(lpd, oldmar, lpd->mac);
	    Bip_Error(RANGE_ERROR);
	}
    }
    res = cpx_write(lpd, file, format);
    reset_rowcols(lpd, oldmar, lpd->mac);
    if (res == 0) 
    {
	Succeed;
    } else 
    {
	Bip_Error(EC_EXTERNAL_ERROR);
    }
}


int
p_cpx_lpread(value vfile, type tfile, 
	     value vformat, type tformat, 
	     value vpresolve, type tpresolve,
	     value vhandle, type thandle, ec_eng_t *ec_eng)
{
    lp_desc *lpd;
    char *file, *format;
    int res;
#if defined(CPLEX) || defined(XPRESS)
    if (!cpx_env)
    {
	Bip_Error(EC_LICENSE_ERROR);
    }
#endif

    Get_Name(vfile, tfile, file);
    Get_Name(vformat, tformat, format);
    Check_Structure(thandle);
    Check_Integer(tpresolve);

    CallN((lpd =  (lp_desc *) Malloc(sizeof(lp_desc))));
    /*CallN(_clr_lp_desc(lpd));*/
    CallN(memset(lpd, 0, sizeof(lp_desc)));
    /* the logged code needs to be hand-adjusted to put file in scope */
    Log1({char *file = "%s";}, file); 
    lpd->presolve = vpresolve.nint;

#ifdef USE_PROBLEM_ARRAY
    Log1(lpdmat[%d] = lpd, next_matno);
    current_matno = next_matno;
    lpd->matno = next_matno++;
#endif

    if (cpx_read(lpd, file, format))
    {
	Bip_Error(EC_EXTERNAL_ERROR);
    }

    lpd->start_mac = lpd->macadded = lpd->mac;
    lpd->descr_state = DESCR_LOADED;

    {/* Return the cplex descriptor in argument HANDLE_CPH of the handle structure. */
	vhandle.ptr[HANDLE_CPH] = ecl_handle(ec_eng, &lp_handle_tid, lpd);
	Make_Stamp(vhandle.ptr+HANDLE_STAMP); /* needed for other trail undos */
    }
    Succeed;
}


void
_create_result_darray(ec_eng_t *ec_eng, value vhandle, int pos, int size, pword* pw, double** start)
{
    pword *argp = &vhandle.ptr[pos];

    Dereference_(argp);
    if (IsRef(argp->tag))
	*start = NULL;
    else
    {
	pw->tag.kernel = TSTRG;
	pw->val.ptr = _create_darray(ec_eng, size);
	*start = DArrayStart(pw->val.ptr);
    }
}

void
_create_result_iarray(ec_eng_t *ec_eng, value vhandle, int pos, int size, pword *pw, int** start)
{
    pword *argp = &vhandle.ptr[pos];

    Dereference_(argp);
    if (IsRef(argp->tag))
	*start = NULL;
    else
    {
	pw->tag.kernel = TSTRG;
	pw->val.ptr = _create_iarray(ec_eng, size);
	*start = IArrayStart(pw->val.ptr);
    }
}

/*----------------------------------------------------------------------*
 * Accessing Infeasible information
 *----------------------------------------------------------------------*/

#ifdef SUPPORT_IIS 
static int
_get_iis(lp_desc * lpd, int * nrowsp, int * ncolsp, int * rowidxs, int * colidxs, char * colstats)
{
    int status;
    int i;
# ifdef CPLEX
    int * rowstatbuf, * colstatbuf;

    rowstatbuf = Malloc(sizeof(int) * *nrowsp);
    colstatbuf = Malloc(sizeof(int) * *ncolsp);

# endif

    Get_Conflict(lpd->lp, status, rowidxs, rowstatbuf, nrowsp, colidxs, colstatbuf, ncolsp);

# ifdef CPLEX
    switch (status)
    {
    case CPX_STAT_CONFLICT_MINIMAL:
	for(i=0;i<*ncolsp;i++)
	{
	    switch (colstatbuf[i])
	    {
# ifdef HAS_GENERAL_CONFLICT_REFINER
	    case CPX_CONFLICT_MEMBER:
		colstats[i] = 'b';
		break;
#  endif
	    case CPX_CONFLICT_LB:
		colstats[i] = 'l';
		break;
	    case CPX_CONFLICT_UB:
		colstats[i] = 'u';
		break;
	    default:
		colstats[i] = 'x';
		break;
	    }
	}
	Free(rowstatbuf);
	Free(colstatbuf);
	break;
    default:
	Free(rowstatbuf);
	Free(colstatbuf);
	return -1;
	break;
#  ifdef HAS_GENERAL_CONFLICT_REFINER
    case CPX_STAT_CONFLICT_FEASIBLE:
	/* An infeaible problem can return CONFLICT_FEASIBLE, with no conflict set, probably because
	   problem is near feasible.
	*/
	*nrowsp = 0;
	*ncolsp = 0;
	Free(rowstatbuf);
	Free(colstatbuf);
	return 1;
	break;
#  endif
    }
# endif
# ifdef XPRESS
    if (!status) {
	for(i=0;i<*ncolsp;i++) colstats[i] = 'x';
    } else {
	return -1;
    }
# endif

    return 0;
}

#endif /* SUPPORT_IIS */


/*----------------------------------------------------------------------*
 * Solve
 *----------------------------------------------------------------------*/


static int 
_cstr_state(lp_desc * lpd, int row, char add_cp_cstr, double * sols, double tol)
{
    int lastarg, argpos;
    double lhs = 0.0, slack;

    /* add_cp_cstr == 2 if unbounded result -- simply add all constraints
       in this case by returning violated state
    */
    if (add_cp_cstr == 2) return CSTR_STATE_VIOLATED;
    lastarg = (row < lpd->cp_nr2 - 1 ? lpd->cp_rmatbeg2[row+1] : lpd->cp_nnz2);
    for (argpos = lpd->cp_rmatbeg2[row] ; argpos < lastarg ; argpos++)
    {
	lhs += sols[lpd->cp_rmatind2[argpos]-IDX_OFFSET] * lpd->cp_rmatval2[argpos];
    }
    /* definition of slack for all row types except ranged rows, which we
       don't use
    */
    slack = lpd->cp_rhsx2[row] - lhs;
    switch (lpd->cp_senx2[row])
    {
    case SOLVER_SENSE_LE:
	return (slack<-tol ? CSTR_STATE_VIOLATED 
		: (slack<=tol ? CSTR_STATE_BINDING : CSTR_STATE_SAT));
	break;
    case SOLVER_SENSE_GE:
	return  (slack > tol ? CSTR_STATE_VIOLATED
		 : (slack >= -tol ? CSTR_STATE_BINDING : CSTR_STATE_SAT));
	break;
    case SOLVER_SENSE_EQ:
	return (slack <= tol && slack >= -tol ? CSTR_STATE_BINDING
		: CSTR_STATE_VIOLATED);
	break;
    default:
	/* constraint type out of range */
	return CSTR_STATE_INVALID;
	break;
    }
}


/* cplex_optimise(Handle, SolveMethods, TimeOut, WriteBefore, MipStart,
		OutputPos, OptResult, OptStatus, WorstBound, BestBound)

   optimises problem in Handle. Handle is needed to access the result
   arrays located in Handle by the OutputPos arguments.
   OptResult is the resulting status after the optimisation, OptStatus is
   the optimiser-dependent status returned by the optimiser. Worst and
   Best bounds are the bounds on the optimal solution determined by the
   solver.

   Any solution state must be extracted from the optimiser in this procedure,
   as it modifies the problem by first adding the cutpool constraints before 
   calling the optimiser and then removing them before exiting.
*/

int
p_cpx_optimise(value vhandle, type thandle, value vmeths, type tmeths, 
         value vtimeout, type ttimeout, value vdump, type tdump, 
         value vmipstart, type tmipstart,
	 value vout, type tout, value vres, type tres, value vstat, type tstat,
	 value vworst, type tworst, value vbest, type tbest, ec_eng_t *ec_eng) 
{
    lp_desc *lpd; 
    int res, oldmar;
    int solspos, pispos, slackspos, djspos, cbasepos, rbasepos, cpcondmappos;
    int iis_rowspos, iis_colspos, iis_colstatspos;
    pword * pw, outsols, outpis, outslacks, outdjs, outcbase, outrbase;
    /* outdjs: when adding a solver, check to make sure that the reduced
       cost is of the same sign as what we defined (and what CPLEX and
       XPress uses). Reverse the signs before returning to ECLiPSe if required!
    */
    struct lp_meth meth;
    char has_cp = 0; /* has cutpool constraints added */
    char add_cp_cstr = 0;
    char *file = NULL;
    char *format = NULL;
    double bestbound, worstbound;
    int * cp_unadded, last_violated_idx, violated_cnt, unadded_cnt = 0;
    int * cp_map2;
    pword * old_tg;

    /*********************************************************************
     *     Type Checking and Initialisation                              *
     *********************************************************************/

    Prepare_Requests

    Check_Structure(thandle);
    Check_Structure(tmeths);
    Check_Integer(tout);
    Check_Integer(tmipstart);
    Check_Number(ttimeout);

    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);

    if (lpd->descr_state == DESCR_EMPTY)
    {
	Fprintf(Current_Error, "Eplex optimise: empty handle\n");
	(void) ec_flush(Current_Error);
	Bip_Error(EC_EXTERNAL_ERROR);
    }

    /* m(Method,AuxMeth,NodeMeth,NodeAuxMeth) */
    pw = &vmeths.ptr[1];
    Dereference_(pw);
    Check_Integer(pw->tag);
    meth.meth = pw->val.nint;
    pw = &vmeths.ptr[2];
    Dereference_(pw);
    Check_Integer(pw->tag);
    meth.auxmeth = pw->val.nint;
    pw = &vmeths.ptr[3];
    Dereference_(pw);
    Check_Integer(pw->tag);
    meth.node_meth = pw->val.nint;
    pw = &vmeths.ptr[4];
    Dereference_(pw);
    Check_Integer(pw->tag);
    meth.node_auxmeth = pw->val.nint;

    /* positions for output arrays in the Prolog handle */
    solspos = vout.nint + HANDLE_S_SOLS;
    pispos = vout.nint + HANDLE_S_PIS;
    slackspos = vout.nint + HANDLE_S_SLACKS;
    djspos = vout.nint + HANDLE_S_DJS;
    cbasepos = vout.nint + HANDLE_S_CBASE;
    rbasepos = vout.nint + HANDLE_S_RBASE;
    cpcondmappos = vout.nint + HANDLE_S_CPCM;
    iis_rowspos = vout.nint + HANDLE_S_IISR;
    iis_colspos = vout.nint + HANDLE_S_IISC;
    iis_colstatspos = vout.nint + HANDLE_S_IISCS;
    
    if (IsStructure(tdump)) 
    { /* write_before_solve(Format,File) */
	pw = &vdump.ptr[1];
	Dereference_(pw);
	Get_Name(pw->val, pw->tag, format);
	pw = &vdump.ptr[2];
	Dereference_(pw);
	Get_Name(pw->val, pw->tag, file);
    }

    SetPreSolve(lpd->presolve);

    oldmar = lpd->mar;
    if (lpd->cp_nr2 > 0)
    {

	pword map;

	_create_result_iarray(ec_eng, vhandle, cpcondmappos, lpd->cp_nr2, &map, &cp_map2);

	cp_unadded = (int *)Malloc(lpd->cp_nr2*sizeof(int));

	if (_setup_initial_cp_constraints(lpd, 0, &unadded_cnt, cp_unadded, cp_map2) == -1) 
	{ 
	    reset_rowcols(lpd, oldmar, lpd->mac);
	    Bip_Error(RANGE_ERROR);
	}
	if (cp_map2)
	    ecl_assign(ec_eng, vhandle.ptr+cpcondmappos, map.val, map.tag);
	has_cp = 1;
    }

    /* initialise the lp_sol structure
    */
    pw = &vhandle.ptr[solspos];
    Dereference_(pw);
    lpd->sol.oldmac = IsIDArray(pw->tag) ? DArraySize(pw->val.ptr) : 0;
    lpd->sol.oldsols = IsIDArray(pw->tag) ? DArrayStart(pw->val.ptr) : NULL;

    _create_result_darray(ec_eng, vhandle, solspos, lpd->mac, &outsols, &lpd->sol.sols);
#ifdef HAS_LIMITED_MIP_RESULTS
    if (IsMIPProb(lpd->prob_type)) {
	lpd->sol.djs = NULL;
	lpd->sol.cbase = NULL;
    } else
#endif
    {/* djs, basis, pis are available for non-MIP problems only for CPLEX;
        for XPRESS, the returned values are for the optimal LP node
     */
	_create_result_darray(ec_eng, vhandle,   djspos, lpd->mac, &outdjs, &lpd->sol.djs);
	_create_result_iarray(ec_eng, vhandle, cbasepos, lpd->mac, &outcbase, &lpd->sol.cbase);
    }

    /* allocate the row-wise arrays later as these may need to be expanded
       with the addition cutpool constraints
    */
    old_tg = TG; 
    _create_result_darray(ec_eng, vhandle, slackspos, lpd->mar, &outslacks, &lpd->sol.slacks);
#ifdef HAS_LIMITED_MIP_RESULTS
    if (IsMIPProb(lpd->prob_type)) {
	lpd->sol.pis = NULL;
	lpd->sol.rbase = NULL;
    } else
#endif
    {
	_create_result_iarray(ec_eng, vhandle, rbasepos, lpd->mar, &outrbase, &lpd->sol.rbase);
	_create_result_darray(ec_eng, vhandle,   pispos, lpd->mar, &outpis, &lpd->sol.pis);
    }
    lpd->sol.mac = lpd->mac;

    Log6({
	lpd->sol.sols   = (double *) malloc(sizeof(double) * %d);\n\
	lpd->sol.pis    = (double *) malloc(sizeof(double) * %d);\n\
	lpd->sol.slacks = (double *) malloc(sizeof(double) * %d);\n\
	lpd->sol.djs    = (double *) malloc(sizeof(double) * %d);\n\
	lpd->sol.cbase   = (int *) malloc(sizeof(int) * %d);
	lpd->sol.rbase   = (int *) malloc(sizeof(int) * %d);
    }, lpd->mac, lpd->mar, lpd->mar, lpd->mac, lpd->mac, lpd->mar);


    /* configure solver with timeout and solution methods
    */
    res = cpx_prepare_solve(lpd, &meth, DoubleVal(vtimeout,ttimeout));
    if (res)
    {
	reset_rowcols(lpd, oldmar, lpd->mac);
	Bip_Error(res<0 ? UNIMPLEMENTED : EC_EXTERNAL_ERROR);
    }
    meth.option_mipstart = vmipstart.nint;

    /* if solution values are unavailable, and there are unadded cutpool
       constraints, abort with RANGE_ERROR as we can't check for violations
    */
    if (unadded_cnt > 0 && lpd->sol.sols == NULL)
    {
	reset_rowcols(lpd, oldmar, lpd->mac);
	Bip_Error(RANGE_ERROR);
    }

    /*********************************************************************
     *     Solve Problem with the External Solver                        *
     *        depending on problem type, call the appropriate routine    *
     *        may solve multiple times with cutpool constraints          *
     *********************************************************************/

    do
    {
	int i;

	violated_cnt = 0;
	if (IsStructure(tdump))
	{
	    cpx_write(lpd, file, format); /* ignore any errors here */
	}

	/* Run the solver
	*/
        res = cpx_solve(lpd, &meth, &bestbound, &worstbound);
	if (res)
	{
	    if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);
	    Bip_Error(res<0 ? UNIMPLEMENTED : EC_EXTERNAL_ERROR);
	}

#ifdef LOG_CALLS
	Fprintf(log_output_, "\n}\nvoid step_%d() {\n", log_ctr++);
	ec_flush(log_output_);
	current_matno = -1; /* no current mat, exited from procedure */
#endif

	/*********************************************************************
	 *     Get Result from External Solver                               *
	 *       Get the result for the optimisation from the external       *
	 *       solver if there is one                                      *
	 *********************************************************************/

	switch (lpd->descr_state)
	{
	case DESCR_SOLVED_SOL:
	case DESCR_ABORTED_SOL:
	    add_cp_cstr = 1;
	    if (cpx_get_soln_state(lpd))
	    {
		if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);
		Bip_Error(EC_EXTERNAL_ERROR); 
	    }
	    break; 

	case DESCR_SOLVED_NOSOL:
	    add_cp_cstr = 0;
	    /* no solution; state always fail */
#ifdef SUPPORT_IIS
	    {
		pword *argp = &vhandle.ptr[iis_rowspos];

		Dereference_(argp);

		if (!IsRef(argp->tag))  
		{
		    int iis_nrows, iis_ncols;
		    int err;
		    pword * old_tg1;

		    pword iis_rowidxs, iis_colidxs, iis_colstats;

		    Find_Conflict(err, lpd->lp, iis_nrows, iis_ncols);
		    if (err) 
		    {/* we can't simply abort here if an error occurs, just create dummy arrays
                        and do not proceed to try to get the IIS */
			iis_nrows = 0;
			iis_ncols = 0;
		    }
		    old_tg1 = TG;
		    iis_rowidxs.val.ptr = _create_iarray(ec_eng, iis_nrows);
		    iis_rowidxs.tag.kernel = TSTRG;
		    iis_colidxs.val.ptr = _create_iarray(ec_eng, iis_ncols);
		    iis_colidxs.tag.kernel = TSTRG;
		    iis_colstats.val.ptr = _create_carray(ec_eng, iis_ncols);
		    iis_colstats.tag.kernel = TSTRG;


		    if (!err && (_get_iis(lpd, &iis_nrows, &iis_ncols, 
				     IArrayStart(iis_rowidxs.val.ptr), IArrayStart(iis_colidxs.val.ptr), 
					  CArrayStart(iis_colstats.val.ptr)) 
			    != 0)
			)
		    {
			/* something went wrong; reallocate iis arrays with 0 size */
			TG = old_tg1;
			iis_nrows = 0;
			iis_ncols = 0;
			iis_rowidxs.val.ptr = _create_iarray(ec_eng, 0);
			iis_colidxs.val.ptr = _create_iarray(ec_eng, 0);
			iis_colstats.val.ptr = _create_carray(ec_eng, 0);
		    }

		    ecl_assign(ec_eng, vhandle.ptr+iis_rowspos, iis_rowidxs.val, iis_rowidxs.tag);
		    ecl_assign(ec_eng, vhandle.ptr+iis_colspos, iis_colidxs.val, iis_colidxs.tag);
		    ecl_assign(ec_eng, vhandle.ptr+iis_colstatspos, iis_colstats.val, iis_colstats.tag);
			
		}
	    }
#endif
	    break;

	default:
	{
#ifdef HAS_POSTSOLVE
	    int presolve; /* postsolve prob. if it is left in a presolved state */

	    if (XPRSgetintattrib(lpd->lp, XPRS_PRESOLVESTATE, &presolve))
	    {
		if (presolve & 2) /* is in a presolve state */
		    CallN(XPRSpostsolve(lpd->lp)); /* post-solve problem if possible */
	    }
#endif
	    if (lpd->descr_state == DESCR_UNBOUNDED_NOSOL || 
		lpd->descr_state == DESCR_UNKNOWN_NOSOL)
	    {/* no result this time, but add all cutpool constraints 
                and resolve may give a solution
	     */
		add_cp_cstr = 2;
	    } else
	    {/* no results, and adding more constraints will not improve the 
		situation */
		add_cp_cstr = 0;
	    }
	}}
#ifdef COIN
	coin_reset_prob(lpd);
#endif

	if (add_cp_cstr)
	{
	    int zerobeg = 0, offset, nzcount, j;
	    double ftol = cpx_feasibility_tol(lpd);

	    i = 0;
	    last_violated_idx = -1;
	    while (i < unadded_cnt)
	    {
		if ((j = cp_unadded[i]) >= 0)
		{
		    switch (cp_map2[j] = _cstr_state(lpd,j,add_cp_cstr,lpd->sol.sols,ftol))
		    {
		    case CSTR_STATE_VIOLATED:
			violated_cnt++;
			offset = lpd->cp_rmatbeg2[j];
			nzcount = ( j <  lpd->cp_nr2-1 ? lpd->cp_rmatbeg2[j+1] - offset : lpd->cp_nnz2 - offset);

			cpx_addrows(lpd->lp, 1, nzcount,
				   &(lpd->cp_rhsx2[j]), &(lpd->cp_senx2[j]),
				   &zerobeg, /* only one row */ 
				   &(lpd->cp_rmatind2[offset]),
				   &(lpd->cp_rmatval2[offset]));
			lpd->mar++;
			cp_map2[j] = lpd->cp_nact2++;
			/* set last_violated_idx if it is not valid */
			if (last_violated_idx < 0) last_violated_idx = i;
			break;
		    case CSTR_STATE_SAT: /* satisfied, but not binding */
		    case CSTR_STATE_BINDING: /* satisfied and binding */
			if (last_violated_idx >= 0)
			{
			    cp_unadded[last_violated_idx] = last_violated_idx - i;
			    last_violated_idx = -1;
			}
			break;
		    case CSTR_STATE_INVALID: /* error */
			Bip_Error(RANGE_ERROR);
			break;
		    }
		    i++;
		} else
		{/* j < 0 : j is -displacement to unadded cstr */
		    i -= j;
		}
	    }
	    if (last_violated_idx >= 0) cp_unadded[last_violated_idx] = last_violated_idx- i;
	    if (violated_cnt > 0) 
	    {
		Mark_Copy_As_Modified(lpd);
		TG = old_tg; /* reallocate row-wise result arrays */
		if (lpd->sol.slacks != NULL)
		    _create_result_darray(ec_eng, vhandle, slackspos, lpd->mar, &outslacks, &lpd->sol.slacks);
		if (lpd->sol.rbase != NULL)
		    _create_result_iarray(ec_eng, vhandle, rbasepos, lpd->mar, &outrbase, &lpd->sol.rbase);
		if (lpd->sol.pis != NULL)
		    _create_result_darray(ec_eng, vhandle,   pispos, lpd->mar, &outpis, &lpd->sol.pis);
	    }
	} /* if (add_cp_cstr) */
	
    } while (violated_cnt > 0); /* do ... */

    Request_Unify_Integer(vres, tres, lpd->descr_state);
    Request_Unify_Integer(vstat, tstat, lpd->sol_state);

    /* (-)HUGE_VAL is used for the maximum best/worst bound instead of 
       (-)CPX_INFBOUND because:
          1) The objective value can exceed CPX_INFBOUND
          2) We use 1.0Inf at the ECLiPSe level for unbounded objective
             value
       Note worst and best bounds are unified even for failure case 
       (for use in any future failure handler)
    */
    Request_Unify_Float(vworst, tworst, worstbound);
    Request_Unify_Float(vbest, tbest, bestbound);

    if (add_cp_cstr == 1)
    {/* have results */
	if (lpd->sol.sols != NULL) 
	    ecl_assign(ec_eng, vhandle.ptr+solspos, outsols.val, outsols.tag);
	if (lpd->sol.pis != NULL) 
	    ecl_assign(ec_eng, vhandle.ptr+pispos,  outpis.val, outpis.tag);
	if (lpd->sol.slacks != NULL) 
	    ecl_assign(ec_eng, vhandle.ptr+slackspos, outslacks.val, outslacks.tag);
	if (lpd->sol.djs != NULL) 
	    ecl_assign(ec_eng, vhandle.ptr+djspos, outdjs.val, outdjs.tag);
	if (lpd->sol.cbase != NULL) 
	    ecl_assign(ec_eng, vhandle.ptr+cbasepos, outcbase.val, outcbase.tag);
	if (lpd->sol.rbase != NULL) 
	    ecl_assign(ec_eng, vhandle.ptr+rbasepos, outrbase.val, outrbase.tag);
    }
    else 
    {
	pword pw;
	/* no solution; reset arrays as these states might not fail */
	Make_Nil(&pw);
	if (lpd->sol.sols != NULL) 
	    ecl_assign(ec_eng, vhandle.ptr+solspos, pw.val, pw.tag);
	if (lpd->sol.pis != NULL) 
	    ecl_assign(ec_eng, vhandle.ptr+pispos, pw.val, pw.tag);
	if (lpd->sol.slacks != NULL) 
	    ecl_assign(ec_eng, vhandle.ptr+slackspos, pw.val, pw.tag);
	if (lpd->sol.djs != NULL) 
	    ecl_assign(ec_eng, vhandle.ptr+djspos, pw.val, pw.tag);
	if (lpd->sol.cbase != NULL) 
	    ecl_assign(ec_eng, vhandle.ptr+cbasepos, pw.val, pw.tag);
	if (lpd->sol.rbase != NULL) 
	    ecl_assign(ec_eng, vhandle.ptr+rbasepos, pw.val, pw.tag);
    }

    if (has_cp) reset_rowcols(lpd, oldmar, lpd->mac);

    memset(&lpd->sol, 0, sizeof(struct lp_sol));

    Return_Unify;
}


int
p_cpx_loadbase(value vlp, type tlp, value vcarr, type tcarr, value vrarr, type trarr, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    int res;
    Check_Array(tcarr);
    Check_Array(trarr);
    LpDesc(vlp, tlp, lpd);
    SetPreSolve(lpd->presolve);
    if (lpd->macadded == IArraySize(vcarr.ptr) && lpd->mar == IArraySize(vrarr.ptr)) {
	/* Fix b58: only load basis if current row/col == array sizes */
	res = cpx_loadbasis(lpd->lp, lpd->macadded, lpd->mar, IArrayStart(vcarr.ptr), IArrayStart(vrarr.ptr));
	if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }
    }
    Succeed;
}


int
p_cpx_loadorder(value vlp, type tlp, value vn, type tn, value vl, type tl, ec_eng_t *ec_eng)
{
#ifdef SOLVER_HAS_LOADORDER
    lp_desc *lpd; 
    int *idx, *prio;
    direction_t *bdir;  /* char or int */
    int i, res;
    pword *buf = TG;

    Check_Integer(tn);
    if (vn.nint <= 0) Succeed; /* no need to load anything */

    LpDesc(vlp, tlp, lpd);
    Push_Buffer(vn.nint*3*sizeof(int));
    idx = (int*) BufferStart(buf);
    prio = (int*) idx + vn.nint;
    bdir = (direction_t*) prio + vn.nint;
    i = 0;
    while (IsList(tl))
    {
	pword *car = vl.ptr;
	pword *cdr = car + 1;
	pword *pw;

	Dereference_(car);
	Check_Structure(car->tag);
	if (DidArity(car->val.ptr->val.did) != 3)
	    { Bip_Error(RANGE_ERROR); }

	pw = car->val.ptr + 1;		/* colindex */
	Dereference_(pw);
	idx[i] = pw->val.nint;
	pw = car->val.ptr + 2;		/* priority */
	Dereference_(pw);
	prio[i] = pw->val.nint;
	pw = car->val.ptr + 3;		/* direction */
	Dereference_(pw);
	bdir[i] = pw->val.nint<0 ? SOLVER_DIR_DOWN : pw->val.nint>0 ? SOLVER_DIR_UP : SOLVER_DIR_DEFAULT;
	++i;
	
	Dereference_(cdr);
	tl = cdr->tag;
	vl = cdr->val;
    }
    Check_List(tl);
    if (i != vn.nint)
	{ Bip_Error(RANGE_ERROR); }

    SetPreSolve(lpd->presolve);
    res = cpx_loadorder(lpd->lp, i, idx, prio, bdir);
    TG = buf;				/* pop aux arrays */
    if (res != 0) { Bip_Error(EC_EXTERNAL_ERROR); }
#endif
    Succeed;
}



/*
 * Add SOSs from descriptor arrays to solver
 */

int
p_cpx_flush_sos(value vhandle, type thandle, ec_eng_t *ec_eng)
{
#ifdef HAS_NO_ADDSOS
    Bip_Error(UNIMPLEMENTED);
#else
    lp_desc *lpd; 
    untrail_data udata;
    int err;

    Check_Structure(thandle);
    LpDesc(vhandle.ptr[HANDLE_CPH].val, vhandle.ptr[HANDLE_CPH].tag, lpd);

    if (lpd->nsos <= lpd->nsos_added)
    	Succeed;

#if defined(LOG_CALLS)
    int i;
    Fprintf(log_output_, "\n\
          lpd->nsos = %d;\n\
          lpd=>nsos_added = %d;\n\
          lpd->nsosnz = %d;\n\
          lpd->sostype = (sostype_t *) malloc(%d*sizeof(sostype_t));\n\
          lpd->sosbeg = (int *) malloc(%d*sizeof(int));\n\
          lpd->sosref = (double *) malloc(%d*sizeof(double));\n\
          lpd->sosind = (int *) malloc(%d*sizeof(int));\n",
	    lpd->nsos, lpd->nsos_added, lpd->nsosnz, lpd->nsos-lpd->nsos_added,
	      lpd->nsos-lpd->nsos_added, lpd->nsosnz, lpd->nsosnz);
    for(i=0; i < lpd->nsos-lpd->nsos_added; ++i) {
        Log2(lpd->sosbeg[%d] = %d, i, lpd->sosbeg[i]);
# ifndef GUROBI
	Log2(lpd->sostype[%d] = '%c', i, lpd->sostype[i]);
# else
	Log2(lpd->sostype[%d] = %d, i, lpd->sostype[i]);
# endif
    }
    for(i=0; i < lpd->nsosnz; ++i) {
        Log2(lpd->sosref[%d] = %f, i, lpd->sosref[i]);
	Log2(lpd->sosind[%d] = %d, i, lpd->sosind[i]);
    }
#endif

    err = cpx_addsos(lpd->lp, lpd->nsos-lpd->nsos_added, lpd->nsosnz,
                     lpd->sostype, lpd->sosbeg, lpd->sosind, lpd->sosref);
    if (err)
    {
	Bip_Error(err<0 ? UNIMPLEMENTED : EC_EXTERNAL_ERROR);
    }
    udata.oldmac = lpd->macadded;
    udata.oldmar = lpd->mar;
    udata.oldsos = lpd->nsos_added;
    udata.oldidc = lpd->nidc;
    ecl_trail_undo(ec_eng, _cpx_del_rowcols, vhandle.ptr, vhandle.ptr+HANDLE_STAMP, (word*) &udata, NumberOfWords(untrail_data), TRAILED_WORD32); 
    CallN(lpd->nsos_added = lpd->nsos);
    CallN(lpd->nsosnz = 0);
    /* could free/downsize arrays here */
    Succeed;
#endif
}


/*
 * Set up SOS arrays in descriptor.
 * This may be called multiple times before the SOSs are added to
 * the solver, either via cpx_loadprob or cpx_flush_sos.  Nothing is
 * trailed here, as we don't backtrack to any intermediate points.
 */

int
p_cpx_add_new_sos(value vlp, type tlp, 
	      value vsostype, type tsostype, 	/* 1 or 2 */
	      value vn, type tn,		/* member count */
	      value vl, type tl, ec_eng_t *ec_eng)		/* member list */
{
    lp_desc *lpd; 
    double weight;
    int i, nnewsos;

    Check_Integer(tsostype);
    Check_Integer(tn);

    LpDescOnly(vlp, tlp, lpd);
    if (vn.nint <= vsostype.nint)
    	Succeed; /* return immediately if sos set is trivial */

    /* the temporary array index of the new SOS */
    nnewsos = lpd->nsos - lpd->nsos_added;
    ++lpd->nsos;
    if (nnewsos+1 >= lpd->sossz)
    {
	/* allocate enough space for the new sos */
	/* CAUTION: in this array must have at least nnewsos+1 elements !!! */
	if (lpd->sossz == 0)
	{
	    lpd->sossz = NEWSOS_INCR;
	    lpd->sostype = (sostype_t *) Malloc(NEWSOS_INCR*sizeof(sostype_t));
	    lpd->sosbeg = (int *) Malloc(NEWSOS_INCR*sizeof(int));
	}
	else
	{
	    lpd->sossz += NEWSOS_INCR;
	    lpd->sostype = (sostype_t *) Realloc(lpd->sostype, lpd->sossz*sizeof(sostype_t));
	    lpd->sosbeg = (int *) Realloc(lpd->sosbeg, lpd->sossz*sizeof(int));
	}
    }
    lpd->sostype[nnewsos] = vsostype.nint==1 ? SOLVER_SOS_TYPE1 : SOLVER_SOS_TYPE2;
    lpd->sosbeg[nnewsos] = lpd->nsosnz;
    lpd->sosbeg[nnewsos+1] = lpd->nsosnz + vn.nint;

    /* allocate enough space for the sos members */
    i = lpd->nsosnz;
    lpd->nsosnz += vn.nint;
    if (lpd->nsosnz > lpd->sosnzsz)
    {
	if (lpd->sosnzsz == 0)
	{
	    lpd->sosnzsz = RoundTo(lpd->nsosnz, 512);
	    lpd->sosind = (int *) Malloc(lpd->sosnzsz*sizeof(int));
	    lpd->sosref = (double *) Malloc(lpd->sosnzsz*sizeof(double));
	}
	else
	{
	    lpd->sosnzsz = RoundTo(lpd->nsosnz, 512);
	    lpd->sosind = (int *) Realloc(lpd->sosind, lpd->sosnzsz*sizeof(int));
	    lpd->sosref = (double *) Realloc(lpd->sosref, lpd->sosnzsz*sizeof(double));
	}
    }

    for (weight = 1.0; IsList(tl); weight += 1.0, ++i)
    {
	pword *car = vl.ptr;
	pword *cdr = car + 1;

	Dereference_(car);
	if (!IsInteger(car->tag))
	    { Bip_Error(TYPE_ERROR); }

	lpd->sosind[i] = (int) car->val.nint+IDX_OFFSET;
	lpd->sosref[i] = weight;

	Dereference_(cdr);
	tl = cdr->tag;
	vl = cdr->val;
    }
    Check_List(tl);
    if (i != lpd->nsosnz)
	{ Bip_Error(RANGE_ERROR); }

    Succeed;
}


int
p_cpx_get_objval(value vlp, type tlp, value v, type t, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 

    LpDescOnly(vlp, tlp, lpd);
    Return_Unify_Float(v, t, lpd->objval);
}


/*
 * Retrieve the matrix coefficients:
 * - First call cplex_get_row(+CPH, +CType, +I, -Base) which 
 *   prepares for retrieval of row i of constraint type CType:
 *       normal constraints:
 *          retrieves the coefficients of row I into the rmatind/rmatval 
 *          arrays 
 *       cutpool constraints:
 *          setup lpd->nnz to the non-zero size for row i, and set Base
 *          to the offset to coefficients for row i in the appropriate
 *          rmatind/rmatval  arrays
 * - Then call cplex_get_col_coef(+CPH, +CType, +Base, -J, -C) which returns 
 *   one nonzero column J and the corresponding coefficient C. Successive
 *   calls return the other nonzero columns in decreasing order. When no
 *   nonzero column is left, cplex_get_col_coef/3 fails. The row number is
 *   the one given in the preceding cpx_get_row/2 call.  */

int
p_cpx_get_row(value vlp, type tlp, value vpool, type tpool,  
	      value vi, type ti, value vbase, type tbase, ec_eng_t *ec_eng)
{
    lp_desc *lpd;
    int base;
    LpDesc(vlp, tlp, lpd);

    Check_Integer(ti);

    Update_Model(lpd->lp);	/* before CPXget... */
    switch (vpool.nint)
    {
    case CSTR_TYPE_NORM:
    {
	int ncols; 
	base = 0; /* read one constraint only */
	ncols = lpd->mac;               /* max one coef per column */
	if (ncols > lpd->nnz_sz)	/* allocate/grow arrays */
	{
	    if (lpd->nnz_sz == 0)
	    {
		lpd->nnz_sz = ncols;
		lpd->rmatind = (int *) Malloc(ncols*sizeof(int));
		lpd->rmatval = (double *) Malloc(ncols*sizeof(double));
	    }
	    else
	    {
		lpd->nnz_sz = ncols;
		lpd->rmatind = (int *) Realloc(lpd->rmatind, ncols*sizeof(int));
		lpd->rmatval = (double *) Realloc(lpd->rmatval, ncols*sizeof(double));
	    }
	}
	SetPreSolve(lpd->presolve);
	if (cpx_getrow(lpd->lp, &lpd->nnz, lpd->rmatind, lpd->rmatval, lpd->nnz_sz, vi.nint+IDX_OFFSET))
            { Bip_Error(EC_EXTERNAL_ERROR); }
	break;
    }
/*
    case CSTR_TYPE_PERMCP:
	base = lpd->cp_rmatbeg[vi.nint];
	lpd->nnz = (vi.nint == lpd->cp_nr-1 ? lpd->cp_nnz-base : lpd->cp_rmatbeg[vi.nint+1]-base);
	break;
*/
    case CSTR_TYPE_CONDCP:
	base = lpd->cp_rmatbeg2[vi.nint];
	lpd->nnz = (vi.nint == lpd->cp_nr2-1 ? lpd->cp_nnz2-base : lpd->cp_rmatbeg2[vi.nint+1]-base);
	
	break;
    default:
	Bip_Error(RANGE_ERROR);
	break;
    }
    
    Return_Unify_Integer(vbase, tbase, base);
}


/* returns the coeff vc for vj'th argument of the Prolog variable array
   NOTE: assumes variable array created from a list in reverse column order
*/
int
p_cpx_get_col_coef(value vlp, type tlp, value vpool, type tpool, 
		   value vbase, type tbase, 
		   value vj, type tj, value vc, type tc, ec_eng_t *ec_eng)
{
    int i;
    lp_desc *lpd; 
    Prepare_Requests
    LpDescOnly(vlp, tlp, lpd);
    if (lpd->nnz == 0)
	{ Fail; }
    --lpd->nnz;
    i = vbase.nint + lpd->nnz;
    switch (vpool.nint)
    {
/*
    case CSTR_TYPE_PERMCP:
	Request_Unify_Integer(vj, tj, (lpd->mac - (lpd->cp_rmatind[i]-IDX_OFFSET)));
	Request_Unify_Float(vc, tc, lpd->cp_rmatval2[i]);
	break;
*/
    case CSTR_TYPE_CONDCP:
	Request_Unify_Integer(vj, tj, (lpd->mac - (lpd->cp_rmatind2[i]-IDX_OFFSET)));
	Request_Unify_Float(vc, tc, lpd->cp_rmatval2[i]);
	break;
    case CSTR_TYPE_NORM:
	Request_Unify_Integer(vj, tj, (lpd->mac - (lpd->rmatind[i]-IDX_OFFSET)));
	Request_Unify_Float(vc, tc, lpd->rmatval[i]);
	break;
    default:
	Bip_Error(RANGE_ERROR);
    }
    Return_Unify;
}

int
p_cpx_get_obj_coef(value vlp, type tlp, value vj, type tj, value vc, type tc, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    double d[1];
    Check_Integer(tj);
    LpDesc(vlp, tlp, lpd);
    if (vj.nint >= lpd->mac) { Bip_Error(RANGE_ERROR); }
    SetPreSolve(lpd->presolve);
    Update_Model(lpd->lp);	/* before cpx_get... */
    if (cpx_get_obj_coef(lpd->lp, d, (int) vj.nint+IDX_OFFSET))
	{ Bip_Error(EC_EXTERNAL_ERROR); }
    Return_Unify_Float(vc, tc, d[0]);
}


/*----------------------------------------------------------------------*
 * Shareable Stop-Flag
 *----------------------------------------------------------------------*/


typedef struct {
    word ref_ctr;
    int flag;
} t_flag;


static dident
_kind_flag(void)
{
    return d_flag;
}

static void
_free_flag(t_flag *obj)
{
    word rem = ec_atomic_add(&obj->ref_ctr, -1);
    if (rem <= 0)
        Free(obj);
}

static t_flag *
_copy_flag(t_flag *obj)
{
    ec_atomic_add(&obj->ref_ctr, 1);
    return obj;
}


static t_ext_type flag_tid = {
    (void (*)(t_ext_ptr)) _free_flag,
    (t_ext_ptr (*)(t_ext_ptr)) _copy_flag,
    0, /* mark */
    0, /* string_size */
    0, /* to_string */
    0,  /* equal */
    (t_ext_ptr (*)(t_ext_ptr)) _copy_flag,
    0,      /* get */
    0,      /* set */
    _kind_flag
};


static t_ext_ptr
_new_flag()
{
    t_flag *obj = (t_flag *) Malloc(sizeof(t_flag));
    obj->ref_ctr = 1;
    obj->flag = 0;
    return (t_ext_ptr) obj;
}


int
p_flag_create(value v, type t, ec_eng_t *ec_eng)
{
    pword pw = ecl_handle(ec_eng, &flag_tid, _new_flag());
    Return_Unify_Pw(v, t, pw.val, pw.tag);
}

int
p_flag_set(value v, type t, ec_eng_t *ec_eng)
{
    int err;
    t_flag *obj;
    pword pw = {v, t};
    if (err = ec_get_handle(pw, &flag_tid, (t_ext_ptr *) &obj)) {
    	Bip_Error(err);
    }
    obj->flag = 1;
    Succeed;
}

int
p_flag_reset(value v, type t, ec_eng_t *ec_eng)
{
    int err;
    t_flag *obj;
    pword pw = {v, t};
    if (err = ec_get_handle(pw, &flag_tid, (t_ext_ptr *) &obj)) {
    	Bip_Error(err);
    }
    obj->flag = 0;
    Succeed;
}

/*
 * link_flag(+CPH, +FlagHandle)
 * Associate FlagHandle (if instantiated) with problem CPH,
 * so it can be used to interrupt a solve of this problem.
 */
int
p_cpx_link_flag(value vlp, type tlp, value v, type t, ec_eng_t *ec_eng)
{
    if (!IsRef(t)) {
        int err;
        t_flag *obj;
        pword pw = {v, t};
        if (err = ec_get_handle(pw, &flag_tid, (t_ext_ptr *) &obj)) {
            Bip_Error(err);
        }
#ifdef CPLEX
        /* This should ideally be problem-specific, not global! */
        CPXsetterminate(cpx_env, &obj->flag);
#endif
    }
    Succeed;
}


/*----------------------------------------------------------------------*
 * Global stack arrays (used for answer vectors)
 *----------------------------------------------------------------------*/


int
p_create_darray(value vi, type ti, value varr, type tarr, ec_eng_t *ec_eng)
{
    pword *pbuf;
    Check_Integer(ti);
    pbuf = _create_darray(ec_eng, vi.nint);
    Return_Unify_String(varr, tarr, pbuf);
}

static pword *
_create_carray(ec_eng_t *ec_eng, int i)
{
    pword *pbuf = TG;
    Push_Buffer(i*sizeof(char) + 1);
    return pbuf;
}

static pword *
_create_darray(ec_eng_t *ec_eng, int i)
{
    pword *pbuf = TG;
    Push_Buffer(i*sizeof(double) + 1);
    return pbuf;
}

static pword *
_create_iarray(ec_eng_t *ec_eng, int i)
{
    pword *pbuf = TG;
    Push_Buffer(i*sizeof(int) + 1);
    return pbuf;
}

int
p_darray_size(value varr, type tarr, value vi, type ti, ec_eng_t *ec_eng)
{
    Check_Array(tarr);
    Return_Unify_Integer(vi, ti, DArraySize(varr.ptr));
}

int
p_get_darray_element(value varr, type tarr, value vi, type ti, value vel, type tel, ec_eng_t *ec_eng)
{
    double f;
    Check_Array(tarr);
    Check_Integer(ti);
    /* RANGE_ERROR if vi.nint is negative -- as it is a large unsigned */
    if ((unsigned) vi.nint >= DArraySize(varr.ptr))
	{ Bip_Error(RANGE_ERROR); }
    f = ((double *) BufferStart(varr.ptr))[vi.nint];
    Return_Unify_Float(vel, tel, f);
}

int
p_set_darray_element(value varr, type tarr, value vi, type ti, value vel, type tel, ec_eng_t *ec_eng)
{
    Check_Array(tarr);
    Check_Integer(ti);
    Check_Float(tel);
    Check_Number(tel);

    if ((unsigned) vi.nint >= DArraySize(varr.ptr))
	{ Bip_Error(RANGE_ERROR); }
    if (GB <= varr.ptr  &&  varr.ptr < TG)
    {
	((double *) BufferStart(varr.ptr))[vi.nint] = DoubleVal(vel, tel);
	Succeed;
    }
    else	/* nondeterministic */
    {
    	Bip_Error(UNIMPLEMENTED);
    }
}

int
p_darray_list(value varr, type tarr, value vmr, type tmr, value vlst, type tlst, ec_eng_t *ec_eng)
{
    pword	list;
    pword	*car;
    pword	*cdr = &list;
    unsigned	i;

    Check_Array(tarr);
    Check_Integer(tmr);
    if (vmr.nint > DArraySize(varr.ptr)) Bip_Error(RANGE_ERROR);
    for (i = 0; i < vmr.nint; ++i)
    {
	car = TG;
	Push_List_Frame();
	Make_List(cdr, car);
	Make_Float(car, ((double *) BufferStart(varr.ptr))[i]);
	cdr = car + 1;
    }
    Make_Nil(cdr);
    Return_Unify_Pw(vlst, tlst, list.val, list.tag);
}


/*----------------------------------------------------------------------*
 * CutPools
 *----------------------------------------------------------------------*/

/* cutpools implemented using the rowwise representation of a problem
   used by CPLEX and Xpress. These are then added to the problem before
   optimisation. There two types of cutpools: unconditional and
   conditional, each representing by its own data structures in the handle.
   Rows in the conditional cutpool have a `name' associated with them, which
   group the rows into different virtual pools. These virtual pools correspond
   conceptually to the different cutpools in a MP solver
*/

int
p_cpx_get_cutpool_size(value vlp, type tlp,  value vnr, type tnr, value vnnz, type tnnz, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    Prepare_Requests
    LpDescOnly(vlp, tlp, lpd);

    Request_Unify_Integer(vnr, tnr, lpd->cp_nr2);
    Request_Unify_Integer(vnnz, tnnz, lpd->cp_nnz2);
    Return_Unify;
}

int
p_cpx_reset_cutpool_size(value vlp, type tlp, 
     value vnr, type tnr, value vnnz, type tnnz, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    LpDescOnly(vlp, tlp, lpd);

    Check_Integer(tnr);
    Check_Integer(tnnz);

    if (vnr.nint > lpd->cp_nr2 || vnr.nint < 0) {Bip_Error(RANGE_ERROR);}
    lpd->cp_nr2 = vnr.nint;
    if (vnnz.nint > lpd->cp_nnz2 || vnnz.nint < 0) {Bip_Error(RANGE_ERROR);}
    lpd->cp_nnz2 = vnnz.nint;
    Succeed;
}

int
p_cpx_set_cpcstr_cond(value vlp, type tlp, value vidx, type tidx,
		      value vtype, type ttype, value vc, type tc, ec_eng_t *ec_eng)
{
    int i; 
    lp_desc *lpd;

    LpDescOnly(vlp, tlp, lpd);
    Check_Integer(tidx);
    Check_Integer(ttype);
    Check_Integer(tc);

    if (vidx.nint < 0 || vidx.nint >= lpd->cp_nr2) { Bip_Error(RANGE_ERROR); }
    switch (vtype.nint)
    {
    case CP_ACTIVE: /* active state */
	if (vc.nint != 0 && vc.nint != 1) { Bip_Error(RANGE_ERROR); } 
	lpd->cp_active2[vidx.nint] = (char) vc.nint;
	break;
    case CP_ADDINIT: /* add initially */
	if (vc.nint != 0 && vc.nint != 1) { Bip_Error(RANGE_ERROR); } 
	lpd->cp_initial_add2[vidx.nint] = (char) vc.nint;
	break;
    default:
	Bip_Error(RANGE_ERROR);
	break;
    }

    Succeed;
}

int
p_cpx_init_cpcstr(value vlp, type tlp, value vidx, type tidx, value vgrp, type tgrp,
		   value vact, type tact, value vinit_add, type tinit_add, ec_eng_t *ec_eng)
{
    lp_desc *lpd; 
    LpDescOnly(vlp, tlp, lpd);

    Check_Integer(tidx);
    Check_Integer(tgrp);
    Check_Integer(tact);
    Check_Integer(tinit_add);

    lpd->cp_initial_add2[vidx.nint] = (char) vinit_add.nint;
    lpd->cp_active2[vidx.nint] = (char) vact.nint;
    if (lpd->cp_pools_max2[vgrp.nint] >= lpd->cp_pools_sz2[vgrp.nint])
    {
	lpd->cp_pools_sz2[vgrp.nint] += NEWROW_INCR;
	lpd->cp_pools2[vgrp.nint] = (int *) Realloc(lpd->cp_pools2[vgrp.nint],lpd->cp_pools_sz2[vgrp.nint]*sizeof(int));
    }
    lpd->cp_pools2[vgrp.nint][lpd->cp_pools_max2[vgrp.nint]++] = vidx.nint;

    return PSUCCEED;
}

#define Expand_Named_CutPool_Arrays(lpd, n) { \
    int sz = lpd->cp_npools_sz2 += CUTPOOL_INCR; \
    /* expand the arrays */\
    lpd->cp_pools2 = (int **) Realloc(lpd->cp_pools2,sz*sizeof(int *));\
    lpd->cp_pools_max2 = (int *) Realloc(lpd->cp_pools_max2,sz*sizeof(int));\
    lpd->cp_pools_sz2 = (int *) Realloc(lpd->cp_pools_sz2,sz*sizeof(int));\
    lpd->cp_names2 = (char **) Realloc(lpd->cp_names2,sz*sizeof(char *));\
    for (i = n+1; i < sz; i++) \
    { /* initialise the new elements (except n - which will be filled \
         by Create_New_CutPool) */\
	lpd->cp_pools2[i] = NULL;\
        lpd->cp_names2[i] = NULL; \
	lpd->cp_pools_max2[i] = 0;\
	lpd->cp_pools_sz2[i] = 0;\
    }\
}

#define Create_New_CutPool(lpd, n, name) {                      \
    lpd->cp_pools_sz2[n] = NEWROW_INCR;                        \
    lpd->cp_pools_max2[n] = 0;                                  \
    lpd->cp_pools2[n] = Malloc(NEWROW_INCR*sizeof(int));        \
    lpd->cp_names2[n] = Malloc((strlen(name)+1)*sizeof(char));  \
    strcpy(lpd->cp_names2[n], name);                            \
}


int
p_cpx_get_named_cp_index(value vlp, type tlp, value vname, type tname,
		         value vnew, type tnew, value vidx, type tidx, ec_eng_t *ec_eng)
{
    int i, n;
    lp_desc *lpd;
    LpDescOnly(vlp, tlp, lpd);

    Check_Integer(tnew);

    if (lpd->cp_npools_sz2 == 0)
    {/* create the default group the first time we use the named groups */
	lpd->cp_npools2 = 1;
	Expand_Named_CutPool_Arrays(lpd, 0);
	Create_New_CutPool(lpd, 0, "[]");
    }
    if (IsNil(tname)) i = 0; /* default group */
    else
    {/* user defined named group */
	Check_Atom(tname);
	for(i=1; i < lpd->cp_npools2; ++i)  
	{
	    if (strcmp(lpd->cp_names2[i], DidName(vname.did)) == 0) break;
	}
	if (i == lpd->cp_npools2) 
	{/* name was not found */	    
	    if (vnew.nint == 0) { Fail; } /* don't create new group */
	    else if ((n = lpd->cp_npools2++) >= lpd->cp_npools_sz2)
	    { 
		Expand_Named_CutPool_Arrays(lpd, n);
	    }
	    Create_New_CutPool(lpd, n, DidName(vname.did));
	}
		
    }

    Return_Unify_Integer(vidx, tidx, i);
}


int
p_cpx_get_cpcstr_info(value vlp, type tlp, value vidx, type tidx, 
		      value vitype, type titype, value vval, type tval, ec_eng_t *ec_eng)
{
    int i, val;

    lp_desc *lpd;
    LpDescOnly(vlp, tlp, lpd);

    Check_Integer(titype);
    Check_Integer(tidx);

    if (vidx.nint < 0 || vidx.nint >= lpd->cp_nr2) { Bip_Error(RANGE_ERROR);}

    switch (vitype.nint)
    {
    case CP_ACTIVE: /* active state */
	val = (int) lpd->cp_active2[vidx.nint];
	break;
    case CP_ADDINIT:
	val = (int) lpd->cp_initial_add2[vidx.nint];
	break;
    default:
	Bip_Error(RANGE_ERROR);
	break;
     }

    Return_Unify_Integer(vval, tval, val);
}

int
p_cpx_get_named_cpcstr_indices(value vlp, type tlp, value vpidx, type tpidx,
				value vilst, type tilst, ec_eng_t *ec_eng)
{
    int i;
    pword list;
    pword * head, * next = &list;

    lp_desc *lpd;
    LpDescOnly(vlp, tlp, lpd);

    Check_Integer(tpidx);

    if (vpidx.nint < 0 || vpidx.nint >= lpd->cp_npools2) Bip_Error(RANGE_ERROR); 
    for (i=0; i < lpd->cp_pools_max2[vpidx.nint]; i++)
    {
	head = TG;
	Push_List_Frame();
	Make_List(next, head);
	Make_Integer(head, lpd->cp_pools2[vpidx.nint][i]);
	next = head + 1;
    }
    Make_Nil(next);
    Return_Unify_Pw(vilst, tilst, list.val, list.tag);
}


/*----------------------------------------------------------------------*
 * Extending basis matrices
 *----------------------------------------------------------------------*/


int
p_create_extended_iarray(value varr, type tarr, value vi, type ti, value vxarr, type txarr, ec_eng_t *ec_eng)
{
    pword *pbuf;

    Check_Integer(ti);
    Check_Array(tarr);
    pbuf = _create_iarray(ec_eng, vi.nint + IArraySize(varr.ptr));
    Return_Unify_String(vxarr, txarr, pbuf);
}

int
p_create_extended_darray(value varr, type tarr, value vi, type ti, value vxarr, type txarr, ec_eng_t *ec_eng)
{
    pword *pbuf;

    Check_Integer(ti);
    Check_Array(tarr);
    pbuf = _create_darray(ec_eng, vi.nint + DArraySize(varr.ptr));
    Return_Unify_String(vxarr, txarr, pbuf);
}

int
p_decode_basis(value varr, type tarr, value vout, type tout, ec_eng_t *ec_eng)
{
    int i,n;
    int *v;
    pword *pw = TG;
    Check_Array(tarr);
    v = IArrayStart(varr.ptr);
    n = IArraySize(varr.ptr);
    Push_Struct_Frame(Did("[]",n));
    for (i = 0; i < n; ++i)
    {
	Make_Integer(&pw[i+1], v[i]);
    }
    Return_Unify_Structure(vout, tout, pw);
}

int
p_copy_extended_column_basis(value varr, type tarr, value vlos, type tlos,
	value vhis, type this, value vxarr, type txarr, ec_eng_t *ec_eng)
{
    unsigned i;
    int      *v;
    int      *vx;

    Check_Array(tarr);
    Check_Array(txarr);
    Check_List(tlos);
    Check_List(this);

    v = IArrayStart(varr.ptr);
    vx = IArrayStart(vxarr.ptr);

    /*
     * Note that this assumes the basis status array contains
     * the status of the problem variables (columns) only
     * and not the status of the slack variables (rows).
     * We want to add status for the new columns so we must:
     * 1) copy the existing variables' status
     * 2) add the CPX_COL_FREE_SUPER status for the new columns
     */
    for (i = 0; i < IArraySize(varr.ptr); ++i)
    {
      vx[i] = v[i];
    }
    for (i = IArraySize(varr.ptr); i < IArraySize(vxarr.ptr); ++i)
    {
      if (IsList(tlos) && IsList(this))
      {
	double lo, hi;
	pword *car = vlos.ptr;
	pword *cdr = car + 1;
	Dereference_(car); Check_Double(car->tag);
	lo = Dbl(car->val);
	Dereference_(cdr); tlos = cdr->tag; vlos = cdr->val;
	car = vhis.ptr;
	cdr = car + 1;
	Dereference_(car); Check_Double(car->tag);
	hi = Dbl(car->val);
	Dereference_(cdr); this = cdr->tag; vhis = cdr->val;
	/* The following are the settings determined by experimentation */
	if (hi <= 0.0)
	    vx[i] = CPX_COL_AT_UPPER;
	else if (lo > -CPX_INFBOUND)
	    vx[i] = CPX_COL_AT_LOWER;
	else
	    vx[i] = CPX_COL_FREE_SUPER;
      }
      else { Bip_Error(TYPE_ERROR); }
    }
    Check_Nil(tlos);
    Succeed;
}

int
p_copy_extended_arrays(value vbarr, type tbarr, value vsarr, type tsarr, value vdarr, type tdarr, value vxbarr, type txbarr, value vxsarr, type txsarr, value vxdarr, type txdarr, ec_eng_t *ec_eng)
{
    int     i;
    int     *vb;
    int     *vxb;
    double  *vs;
    double  *vxs;
    double  *vd;
    double  *vxd;

    Check_Array(tbarr);
    Check_Array(tsarr);
    Check_Array(tdarr);
    Check_Array(txbarr);
    Check_Array(txsarr);
    Check_Array(txdarr);

    vb = IArrayStart(vbarr.ptr);
    vxb = IArrayStart(vxbarr.ptr);
    vs = DArrayStart(vsarr.ptr);
    vxs = DArrayStart(vxsarr.ptr);
    vd = DArrayStart(vdarr.ptr);
    vxd = DArrayStart(vxdarr.ptr);

    /*
     * Note that this assumes the basis status array contains
     * the status of the problem variables (columns) only
     * and not the status of the slack variables (rows).
     * We want to add status for the new columns so we must:
     * 1) copy the existing variables' status, solution value
     * 2) add the CPX_COL_FREE_SUPER status, 0.0 solution value
     *    and 0.0 reduced cost for the new columns
     */
    for (i = 0; i < IArraySize(vbarr.ptr); ++i)
    {
      vxb[i] = vb[i];
      vxs[i] = vs[i];
      vxd[i] = vd[i];
    }
    for (i = IArraySize(vbarr.ptr); i < IArraySize(vxbarr.ptr); ++i)
    {
      vxb[i] = CPX_COL_FREE_SUPER;
      vxs[i] = 0.0;
      vxd[i] = 0.0;
    }
    Succeed;
}


/*----------------------------------------------------------------------*
 * Accessing iarrays
 *----------------------------------------------------------------------*/


int
p_create_iarray(value vi, type ti, value varr, type tarr, ec_eng_t *ec_eng)
{
    pword *pbuf;
    Check_Integer(ti);
    pbuf = _create_iarray(ec_eng, vi.nint);
    Return_Unify_String(varr, tarr, pbuf);
}

int
p_iarray_size(value varr, type tarr, value vi, type ti, ec_eng_t *ec_eng)
{
    Check_Array(tarr);
    Return_Unify_Integer(vi, ti, IArraySize(varr.ptr));
}

int
p_get_iarray_element(value varr, type tarr, value vi, type ti, value vel, type tel, ec_eng_t *ec_eng)
{
    int i;
    Check_Array(tarr);
    Check_Integer(ti);
    if ((unsigned) vi.nint >= IArraySize(varr.ptr))
	{ Bip_Error(RANGE_ERROR); }
    i = ((int *) BufferStart(varr.ptr))[vi.nint];
    Return_Unify_Integer(vel, tel, i);
}

int
p_set_iarray_element(value varr, type tarr, value vi, type ti, value vel, type tel, ec_eng_t *ec_eng)
{
    Check_Array(tarr);
    Check_Integer(ti);
    Check_Integer(tel);
    if ((unsigned) vi.nint >= IArraySize(varr.ptr))
	{ Bip_Error(RANGE_ERROR); }
    if (GB <= varr.ptr  &&  varr.ptr < TG)
    {
	((int *) BufferStart(varr.ptr))[vi.nint] = vel.nint;
	Succeed;
    }
    else	/* nondeterministic */
    {
    	Bip_Error(UNIMPLEMENTED);
    }
}

int
p_iarray_list(value varr, type tarr, value vlst, type tlst, ec_eng_t *ec_eng)
{
    pword      list;
    pword      *car;
    pword      *cdr = &list;
    unsigned   i;

    Check_Array(tarr);
    for (i = 0; i < IArraySize(varr.ptr); ++i)
      {
	car = TG;
	Push_List_Frame();
	Make_List(cdr, car);
	Make_Integer(car, ((int *) BufferStart(varr.ptr))[i]);
	cdr = car + 1;
      }
    Make_Nil(cdr);
    Return_Unify_Pw(vlst, tlst, list.val, list.tag);
}

#ifdef DUMPMAT

int
dump_problem(lp_desc * lpd)
{
    int i;
    Fprintf(log_output_, "\n\
    	lpd->macsz = %d;\n\
    	lpd->marsz = %d;\n\
    	lpd->matnz = %d;\n\
    	lpd->mac = %d;\n\
    	lpd->mar = %d;\n\
	lpd->rhsx = a_rhsx;\n\
	lpd->senx = a_senx;\n\
	lpd->matbeg = a_matbeg;\n\
	lpd->matcnt = a_matcnt;\n\
	lpd->matind = a_matind;\n\
	lpd->matval = a_matval;\n\
	lpd->bdl = a_bdl;\n\
	lpd->bdu = a_bdu;\n\
	lpd->objx = a_objx;\n\
	lpd->ctype = a_ctype;",
      lpd->macsz, lpd->marsz, lpd->matnz,
      lpd->mac, lpd->mar);

    Fprintf(log_output_, "\n\
	/*\n\
	 * Problem data\n\
	 */\n\
	 ");
    Fprintf(log_output_, "double a_objx[%d] ={\n", lpd->mac);
    for (i=0; i<lpd->mac; ++i)
	Fprintf(log_output_, "%.15e,\n", lpd->objx[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "double a_bdl[%d] ={\n", lpd->mac);
    for (i=0; i<lpd->mac; ++i)
	Fprintf(log_output_, "%.15e,\n", lpd->bdl[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "double a_bdu[%d] ={\n", lpd->mac);
    for (i=0; i<lpd->mac; ++i)
	Fprintf(log_output_, "%.15e,\n", lpd->bdu[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "int a_matbeg[%d] ={\n", lpd->mac);
    for (i=0; i<lpd->mac; ++i)
	Fprintf(log_output_, "%d,\n", lpd->matbeg[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "int a_matcnt[%d] ={\n", lpd->mac);
    for (i=0; i<lpd->mac; ++i)
	Fprintf(log_output_, "%d,\n", lpd->matcnt[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "char a_ctype[%d] ={\n", lpd->mac);
    for (i=0; i<lpd->mac; ++i)
	Fprintf(log_output_, "'%c',\n", lpd->ctype[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "double a_rhsx[%d] ={\n", lpd->mar);
    for (i=0; i<lpd->mar; ++i)
	Fprintf(log_output_, "%.15e,\n", lpd->rhsx[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "char a_senx[%d] ={\n", lpd->mar);
    for (i=0; i<lpd->mar; ++i)
	Fprintf(log_output_, "'%c',\n", lpd->senx[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "int a_matind[%d] ={\n", lpd->matnz);
    for (i=0; i<lpd->matnz; ++i)
	Fprintf(log_output_, "%d,\n", lpd->matind[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "double a_matval[%d] ={\n", lpd->matnz);
    for (i=0; i<lpd->matnz; ++i)
	Fprintf(log_output_, "%.15e,\n", lpd->matval[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "\n\
	/*\n\
	 * End data\n\
	 */\n\
	 ");

# if 0
    Fprintf(log_output_, "\n\
    	lpd->cb_sz = %d;\n\
	lpd->cb_index = a_cb_index;\n\
	lpd->cb_index2 = a_cb_index2;\n\
	lpd->cb_value = a_cb_value;\n\
    	lpd->cb_cnt = %d;",
	lpd->cb_cnt, lpd->cb_cnt);
    Fprintf(log_output_, "int a_cb_index[%d] ={\n", lpd->cb_cnt);
    for (i=0; i<lpd->cb_cnt; ++i)
	Fprintf(log_output_, "%d,\n", lpd->cb_index[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "int a_cb_index2[%d] ={\n", lpd->cb_cnt);
    for (i=0; i<lpd->cb_cnt; ++i)
	Fprintf(log_output_, "%d,\n", lpd->cb_index2[i]);
    Fprintf(log_output_, "};\n\n");

    Fprintf(log_output_, "double a_cb_value[%d] ={\n", lpd->cb_cnt);
    for (i=0; i<lpd->cb_cnt; ++i)
	Fprintf(log_output_, "%.15e,\n", lpd->cb_value[i]);
    Fprintf(log_output_, "};\n\n");
# endif
}
#endif
