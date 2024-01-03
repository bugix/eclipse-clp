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
 * VERSION	$Id: dict.h,v 1.14 2017/09/01 03:05:09 jschimpf Exp $
 *
 * IDENTIFICATION:	dict.h
 *
 * DESCRIPTION:		Types and macro definitions related to
 *
 *				- dictionary
 *				- procedure table
 *				- properties
 *
 * AUTHORS:		Joachim Schimpf, Emmanuel van Rossum, Micha Meier
 *
 */


/*
 *	DICTIONARY PARAMETERS AND DATA TYPE DEFS
 */

/* Dictionary Related Definitions */
#define		D_UNKNOWN	0	/* unknown did */

/* values for the dict_flags field (bit-significant) */
#define		DICT_STABILITY	0x03	/* dict entry stability */
#define		DICT_VOLATILE	0	/* has only stack & property references	*/
#define		DICT_CODE_REF	1	/* may have code references		*/
#define		DICT_PERMANENT	3	/* do never remove from dictionary	*/

#define		DICT_HEAD	0x04	/* head of chain */

/* In unused (e.g. garbage collected) dict_items, we set the arity
** field to this value in order to catch bugs */
#define		UNUSED_DID_ARITY	(-3)



/* dictionary definitions */
#define		DidMacro(D)	(D)->macro
#define		DidModule(D)	(D)->module
#define		DidIsOp(D)	(D)->isop
#define		DidProc(D)	(D)->procedure
#define 	DidProperties(D) (D)->properties
#define 	DidBitField(D)	(D)->bitfield
#define 	DidNext(D)	(D)->next
#define 	DidStability(D)	((D)->dict_flags & DICT_STABILITY)
#define 	DidIsHead(D)	((D)->dict_flags & DICT_HEAD)

#define	Set_Did_Head(D) \
	ec_atomic_or(&(D)->dict_flags, DICT_HEAD)

#define	Clr_Did_Head(D) \
	ec_atomic_and(&(D)->dict_flags, ~DICT_HEAD)

#define Set_Did_Stability(D, NewStability) \
	ec_atomic_or(&(D)->dict_flags, NewStability)

/* marking for dictionary GC */
#define	Mark_Did(D)	ec_mark_did(D)
#define	Mark_VarName(t)	ec_mark_did(TagDid(t))

/* convert a given functor to another arity */
#define Add_Dict(d, new_arity) 	d = add_dict(d, new_arity)


/* ------------------------ PROCEDURE DESCRIPTOR ---------------------	*/

/*
 * A procedure descriptor (pri) describes a predicate visible in a module.
 * It stores all predicate properties, visibility information, and code.
 *
 * Every predicate has:
 * - a descriptor in the module where it is defined (LOCAL,EXPORT),
 *	this is called the "home" or "definition" descriptor.
 * - a descriptor in every module where it is visible (IMPORT,IMPEXP).
 * - a qualified access descriptor (QUALI) in every module where
 *     there is a compiled qualified access to it via :/2.
 * - a DEFAULT descriptor in every module where it is referenced but the
 *     source of the corresponding definition is not yet known.
 *
 * Procedure descriptors may be referenced from
 * - other procedure descriptors
 * - wam code (but only code in the same module as the descriptor!)
 * - certain stack data (suspensions, trace frames)
 * Procedure descriptors are only freed when their module is erased.
 *
 * Procedure descriptors are usually looked up via predicate functor and
 * module name. Related code and further description is in proc_desc.c.
 */


/*
 * Option values for calls to visible_procedure()
 */

#define PRI_CREATE	1	/* create pri if none exists */
#define PRI_REFER	2	/* mark pri as referenced */
#define PRI_DONTIMPORT	4	/* don't try to import if none exists */
#define PRI_DONTWARN	8	/* don't issue redefinition warnings */
#define PRI_EXPORTEDONLY 16	/* hide module internals */


/*
 * Generic pointer to the code that implements a procedure
 */

typedef union
{
    vmcode	*vmc;		/* pointer to virtual machine code	*/
    word	cint;		/* builtin number in emulator		*/
    int		(*func)();	/* address of C function		*/
} pri_code_t;


/*
 * The actual procedure descriptor (pri)
 */

#define PRIMODEBITS	24		/* bits used for storing mode	*/

typedef struct pri
{
    pri_code_t		code;		/* code (multiple types)	*/
    struct pri		*nextproc;	/* next pri with same functor	*/
    struct pri		*next_in_mod;	/* next pri in same module	*/
    dident		module_def;	/* module of this descriptor	*/
    dident		module_ref;	/* home module of the procedure	*/
    dident		did;		/* this procedure's functor	*/
    uint32_t		flags;		/* various flags, see below	*/
    unsigned		prio:4;		/* the schedule priority	*/
    unsigned		run_prio:4;	/* the run priority		*/
    unsigned		mode:PRIMODEBITS;	/* the mode declaration	*/
    dident		trans_function;	/* did of the transformation procedure*/
} pri;


/*
 * Access macros for procedure descriptor
 */

#define	PriCode(pd)		(pd)->code.vmc
#define	PriCint(pd)		(pd)->code.cint
#define	PriFunc(pd)		(pd)->code.func
#define	PriNext(pd)		(pd)->nextproc
#define	PriModule(pd)		(pd)->module_def
#define	PriHomeModule(pd)	(pd)->module_ref
#define	PriFlags(pd)		(pd)->flags
#define PriDid(pd)		(pd)->did
#define PriMode(pd)		(pd)->mode
#define PriPriority(pd)		(pd)->prio
#define PriRunPriority(pd)	(pd)->run_prio
#define PriScope(pd)		(PriFlags(pd) & PREDSCOPE)
#define	PriCodeType(pd)		(PriFlags(pd) & CODETYPE)
#define	PriArgPassing(pd)	(PriFlags(pd) & ARGPASSING)
#define UnifType(pd)		(PriFlags(pd) & UNIFTYPE)
#define StaticProc(pd)		(!DynamicProc(pd))
#define DynamicProc(pd)		(PriFlags(pd) & PROC_DYNAMIC)
#define SystemProc(pd)		(PriFlags(pd) & SYSTEM)
#define ToolProc(pd)		(PriFlags(pd) & TOOL)
#define DebugProc(pd)		(PriFlags(pd) & DEBUG_DB)
#define ParallelProc(pd)	(PriFlags(pd) & PROC_PARALLEL)
#define InvisibleProc(pd)	(PriFlags(pd) & DEBUG_INVISIBLE)

#define Pri_Set_Scope(pd,newscope) \
 	PriFlags(pd) = (PriFlags(pd) & ~PREDSCOPE) | (newscope);

#define PriReferenced(pd) \
 	(!(PriFlags(pd) & NOREFERENCE))
#define Pri_Set_Reference(pd) \
 	PriFlags(pd) &= ~NOREFERENCE;

#define IsVisibilityPri(pd)	((pd)  &&  PriScope(pd) != QUALI)
#define PriIsProxy(pd) \
 	(PriScope(pd) == IMPORT || PriScope(pd) == QUALI || PriScope(pd) == IMPEXP)
#define PriAnyExp(pd) \
 	(PriScope(pd) == EXPORT || PriScope(pd) == IMPEXP)
#define PriExported(pd) \
 	(PriScope(pd) == EXPORT)
#define PriExportedFrom(pd, module) \
 	(PriExported(pd)  &&  (module) == (pd)->module_ref)
#define PriWillExport(pd) \
 	(PriFlags(pd) & TO_EXPORT)


/* ---------------- GROUPS OF PROCEDURE FLAGS -----------------------	*/

/* Every flag must be either in COMMON_FLAGS or in DESCRIPTOR_FLAGS.
 * The COMMON_FLAGS belong to the procedure and have to be kept consistent
 * over all the linked descriptors for the same actual procedure.
 * The DESCRIPTOR_FLAGS belong to the descriptor only.
 */
#define COMMON_FLAGS	(~DESCRIPTOR_FLAGS)
#define DESCRIPTOR_FLAGS (NOREFERENCE|PREDSCOPE|TO_EXPORT|DONT_REEXPORT)


/* Flags that can't be changed once the procedure has been referenced,
 * i.e. all the information that might have been exploited in compiling
 * a call to the predicate (calling convention, etc).
 */
#define PF_DONT_CHANGE_WHEN_REFERENCED	\
	(TOOL|CODETYPE|ARGPASSING|UNIFTYPE)

/* Flags that can't be changed once the procedure has been defined.
 * This is basically the information that may be used when compiling
 * the code for the predicate.
 */
#define PF_DONT_CHANGE_WHEN_DEFINED	\
	(PF_DONT_CHANGE_WHEN_REFERENCED|PROC_PARALLEL|PROC_DEMON\
	|PROC_DYNAMIC|EXTERN)

/* Flags that can be changed even when not in the home module */
#define PF_CHANGE_FROM_ANYWHERE \
	(DEBUG_TR|DEBUG_SP|DEBUG_SK|DEBUG_ST)


/* The flags that must be stored along with the code in .eco files
 * (the ones that are set by compiler pragmas rather than proper queries)
 */
#define ECO_FLAGS	(SYSTEM|DEBUG_DB|DEBUG_SK|PROC_AUXILIARY)


/* ------------------------- PROCEDURE FLAGS ------------------------	*/

/* The descriptor was not yet referenced (from code, or equivalent).
 * This implies that it can still be changed rather freely.
 */
#define NOREFERENCE	0x20000000


/* Procedure visibility */
#define PREDSCOPE	0x07000000
#define QUALI		0x00000000
#define LOCAL		0x01000000
#define EXPORT		0x02000000
#define IMPORT		0x03000000
#define DEFAULT		0x04000000
#define IMPEXP		0x05000000

#define TO_EXPORT	0X10000000	/* always together with LOCAL */
#define DONT_REEXPORT	0X10000000	/* together with EXPORT/IMPEXP */


/* Various predicate properties (set via declarations or by the compiler) */
#define SYSTEM		0x40000000	/* was compiled with system-pragma */
#define PROC_PARALLEL	0x00400000	/* has been declared parallel	*/
#define PROC_DEMON	0x00200000	/* has been declared a demon	*/
#define PROC_DYNAMIC	0x80000000	/* has been declared dynamic	*/
#define EXTERN		0X00000080	/* has been declared external	*/
#define TOOL		0x00000040	/* it is a tool interface */
#define AUTOLOAD	0X00000400	/* autoload flag, causes autoload
					 * event rather than undefined
					 * procedure event */
#define PROC_AUXILIARY	0X00000800	/* it is a compiler auxiliary, ie. it
					   needs to be saved in .eco files */
#define PROC_DEPRECATED	0x00000020	/* deprecated, warn if used	*/


/* Debugger flags */
#define DEBUG_TYPES	0X081F0100
#define DEBUG_TR	0X00010000	/* traceable 		*/
#define DEBUG_SP	0X00020000	/* has a spy point	*/
#define DEBUG_SK	0X00040000	/* skipped		*/
#define DEBUG_DB	0x00080000	/* in debugging mode	*/
#define DEBUG_ST	0x00100000	/* start debugger	*/
#define DEBUG_INVISIBLE	0x08000000	/* not even a box	*/
#define DEBUG_TRMETA	0X00000100	/* always trace metacalled subgoals */
/* default flags for a traceable procedure */
#define DEBUG_DF	DEBUG_TR

/* the debugger flags occur also in the TRACE_MODE register
 * They are also defined in tracer.pl!
 */
#define TR_TRACING	DEBUG_TR	/* must match, see OfInterest() */
#define TR_LEAPING	DEBUG_SP	/* must match, see OfInterest() */
#define TR_STARTED	DEBUG_ST	/* arbitrary */

/*
#define UNUSED_BITS	0X0000E000
*/


/*
 * Different predicate implementations. Combinations currently used:
 *
 * CODETYPE	ARGPASSING	other
 *
 * VMCODE	ARGFIXEDWAM	-		static Prolog predicate
 * VMCODE	ARGFIXEDWAM	PROC_DYNAMIC	dynamic Prolog predicate
 * VMCODE	ARGFIXEDWAM	EXTERN		C external with WAM wrapper
 * VMCODE	ARGFLEXWAM	EXTERN		C external (in emulator)
 */

/* CODETYPE describes the contents of the .code field (a union) */
#define CODETYPE	0X00000200
#define	VMCODE		0X00000200	/* virtual machine code		*/
#define	FUNPTR		0X00000000	/* function pointer		*/

#define B_SAFE		0		/* obsolete built-in classification */
#define B_UNSAFE	0		/* obsolete built-in classification */

#define CODE_DEFINED	0x00800000	/* has non-default code		*/

/* ARGPASSING describes the calling convention of the predicate */
#define ARGPASSING	0X00000001
#define ARGFIXEDWAM	0X00000000	/* Args in A[1]..A[n]		*/
#define ARGFLEXWAM 	0X00000001	/* various regs and immediate args */
/* #define ARGSTACK	0X00000002	Args on local stack (obsolete)	*/
/* #define ARGSTRUCT	0X00000003	future extension */

#define PL_C_ARGS	0X00001000	/* arguments in C arguments (obsolescent) */


/* Unification types: used in the compiler (conditions),
   in the occur check etc. Currently only for builtins, but can hold also
   for Prolog ones.
   ***The order is important!
 */
#define UNIFTYPE	0X0000001c /* Unification type field */

#define U_NONE		0x00000000 /* no unification at all, must be 0!      */
#define U_SIMPLE	0x00000004 /* unify 1 arg with a simple term	      */
#define U_GROUND	0x00000008 /* unify args with ground terms+functor/3 */
#define U_FRESH		0x0000000c /* unify to a term with fresh variables   */
#define U_UNIFY		0x0000000c /* general unification		      */
#define U_GLOBAL	0x00000010 /* binds to a term with other variables   */
#define U_DELAY		0x00000014 /* a delay condition */



/*
 * The mode declarations are stored in a bitfield of PRIMODEBITS,
 * the mode specification for one argument consists of PMODEBITS bits.
 * This means that up to arity MAX_MODES the declaration
 * can be taken into account, higher arguments are ignored.
 */

#undef EXTENDED_MODES		/* set this to allow -+ and +- modes */

#define ANY			0		/* (?)	must be 0!	    */
#define OUTPUT                  1		/* (-)	actually uninit	    */
#define NONVAR                  2		/* (+)	nonground structure */
#define GROUND                  3		/* (++)			    */
#ifdef EXTENDED_MODES
#define NOALIAS                 5		/* (-+) term without aliases*/
#define NOALIAS_INST            7		/* (+-) no aliases, inst.   */
#endif

/* simulate an array of n-bit elements */
#ifdef EXTENDED_MODES
#define PMODEBITS		3
#define PMODEMASK		7
#else
#define PMODEBITS		2
#define PMODEMASK		3
#endif
#define MAX_MODES		(PRIMODEBITS/PMODEBITS)

/* Mode() and Next_Mode() must work even for arities bigger than MAX_MODES */
#define Mode(i, mode_decl)	\
        ((i) > MAX_MODES ? ANY :        \
	    (((mode_decl) >> (((i) - 1) * PMODEBITS)) & PMODEMASK))
#define Next_Mode(part_mode, next)	(next) = (part_mode) & PMODEMASK;    \
					(part_mode) >>= PMODEBITS;
#define Set_Mode(i, mode_decl, val)	\
	    {if ((i) <= MAX_MODES) (mode_decl) = \
		((mode_decl) & ~(PMODEMASK << (((i) - 1) * PMODEBITS))) | \
		    (val) << (((i) - 1) * PMODEBITS);\
	    }


/* Obsolete built-in binding information - being phased out */
#define CONSTANT                1
#define BoundArg(i, val)	0


/* ------------------------- PROPERTIES TABLE -----------------------	*/

typedef struct property			/* property descriptor */
{
    int			name;		/* the property name resp. number    */
    dident		module;		/* the definition module / D_UNKNOWN */
    pword 		property_value; /* prop value / TEND if undefined   */
    struct property	*next_prop,	/* next same module / next global    */
    			*next_mod;	/* same property in other module     */
} property;


#define EOI_SYMBOL	256		/* end of input pseudo character */

typedef struct syntax_desc
{
    unsigned char	char_class[EOI_SYMBOL+1]; /* 256 ASCII + 1 EOI symbol */
    int			options;
    unsigned char	current_sq_char;
    unsigned char	current_aq_char;
    unsigned char	current_ul_char;
    int			current_escape;
} syntax_desc;

extern syntax_desc	*default_syntax;

typedef struct didlist
{
    dident		 name;
    struct didlist	*next;
} didlist;

/* module descriptor	*/
typedef struct module_item	
{
    syntax_desc		*syntax;	/* module syntax descriptor	     */
    char		*lock;		/* the module password		     */
    pri			*procedures;	/* list of procedures in this module */
    					/* (PropertyLock) */
    property		*properties;	/* list of properties in this module */
    					/* (ProcListLock) */
    didlist		*imports;	/* list of imported modules (import/1)*/
    					/* (ModuleLock) */
} module_item;


/* macro transformation descriptor	*/
typedef struct macro_desc	
{
	int		flags;		/* transformation options	     */
	dident		trans_function;	/* did of the transformation procedure*/
	dident		module;		/* did of the transformation module  */
} macro_desc;

/* flags */
#define TR_FLAGS	0x00003f00
#define TR_TYPE		0x00000f00	/* mandatory flags in caller	*/
#define TR_TOP		0x00000100
#define TR_CLAUSE	0x00000200
#define TR_GOAL		0x00000400
#define TR_WRITE	0x00000800
#define TR_PROTECT	0x00001000
#define TR_GLOBAL	0x00002000


/* ---------------------------- OPERATORS -----------------------------	*/

/* an operator item is put in a pword: the tag part contains the prolog
   tag (8 bits), the associativity (4 bits), the precedence (11 bits)
   and bits for the garbage collector; the value part stores the did
   needed by the parser							*/
typedef pword opi;

/* access thru *opi							*/
#define		ASSOC_MASK	0X00000F00 /* mask the assoc bits	*/
#define		PRECED_MASK	0X07FF0000 /* mask the preced bits	*/
#define		ASSOC_SHIFT	8
#define		PRECED_SHIFT	16
#define		SHIFTED_ASSOC_MASK	(ASSOC_MASK >> ASSOC_SHIFT)
#define		SHIFTED_PRECED_MASK	(PRECED_MASK >> PRECED_SHIFT)

#define		OpiDid(O)	(O).val.did
#define		OpiAssoc(O) \
    (((O).tag.kernel >> ASSOC_SHIFT) & SHIFTED_ASSOC_MASK)
#define		OpiPreced(O)	\
    (((O).tag.kernel >> PRECED_SHIFT) & SHIFTED_PRECED_MASK)
#define		Set_Opi_Assoc(O, V) \
    (O).tag.kernel = ((O).tag.kernel & ~ASSOC_MASK) | ((V) << ASSOC_SHIFT)
#define		Set_Opi_Preced(O, V) \
    (O).tag.kernel = ((O).tag.kernel & ~PRECED_MASK) | ((V) << PRECED_SHIFT)
#define		Set_Opi_Invalid(O) \
    (O).tag.kernel = TEND

/* Operator Associativities						*/
#define		NIL_OP		0
#define		FX		1
#define		FY		2
#define		XF		3
#define		YF		4
#define		XFX		5
#define		XFY		6
#define		YFX		7
#define		FXX		8
#define		FXY		9
#define		MAX_ASSOC	9

/* Tests for Associativity */

#define IsPrefix2(op)		(OpiAssoc(op) >= FXX)
#define IsPostfixAss(ass)	((ass) == XF || (ass) == YF)
#define IsInfixOrPostfix(ass)	(XF <= (ass) && (ass) <= YFX)

/* Get precedences */

#define Get_Prefix_Prec(op_desc, oprec, rprec) \
	oprec = OpiPreced(op_desc); \
	rprec = OpiAssoc(op_desc) == FY ? oprec : oprec - 1;

#define Get_Postfix_Prec(op_desc, lprec, oprec) \
	oprec = OpiPreced(op_desc); \
	lprec = OpiAssoc(op_desc) == YF ? oprec : oprec - 1;

#define Get_Infix_Prec(op_desc, lprec, oprec, rprec) \
	oprec = OpiPreced(op_desc); \
	lprec = OpiAssoc(op_desc) == YFX ? oprec : oprec - 1; \
	rprec = OpiAssoc(op_desc) == XFY ? oprec : oprec - 1;

#define Get_Prefix2_Prec(op_desc, oprec, lprec, rprec) \
	oprec = OpiPreced(op_desc); \
	lprec = oprec - 1; \
	rprec = OpiAssoc(op_desc) == FXY ? oprec : oprec - 1;

#define InfixLeftPrecedence(op_desc) \
	( OpiAssoc(op_desc) == YFX ? OpiPreced(op_desc) : OpiPreced(op_desc) - 1 )

#define PostfixLeftPrecedence(op_desc) \
	( OpiAssoc(op_desc) == YF  ? OpiPreced(op_desc) : OpiPreced(op_desc) - 1 )



/*-------------------- function declarations --------------------*/

/* dictionary */
Extern dident	in_dict(char*,int);
Extern dident	enter_dict(char*,int);
Extern dident	enter_dict_n(char*,word,int);
Extern dident	add_dict(dident,int);
Extern dident	check_did(dident,int);
Extern dident	check_did_n(char*,word,int);
Extern dident	bitfield_did(word);
Extern void	ec_mark_did(dident);
Extern void	ec_mark_did_conservative(void*);
Extern void	ec_mark_string_conservative(void*);
Extern void	ecl_mark_engine(ec_eng_t*,word);

Extern int	next_functor(int *index, dident *did, int weak);
Extern int	p_gc_dictionary(ec_eng_t*);
Extern int	p_dict_param(value,type,value,type,ec_eng_t*);

Extern pword	*enter_string_n(char*,word,int);
Extern int	compare_strings(value v1, value v2);
Extern pword	*empty_string;


Extern int	ec_constant_table_enter(ec_eng_t*,value,type,pword*);

/* procedure descriptor handling */
Extern pri	*local_procedure(dident,dident,type,int,int*);
Extern pri	*export_procedure(dident,dident,type,int*);
Extern pri	*reexport_procedure(dident,dident,type,dident,int*);
Extern pri	*global_procedure(dident,dident,type,int*);
Extern pri	*import_procedure(dident,dident,type,dident,int*);
Extern pri	*visible_procedure(dident,dident,type,int,int*);
Extern pri	*qualified_procedure(dident,dident,dident,type,int*);
Extern int	import_whole_module(dident,dident);
Extern pri	*pri_home(pri*,int*);
Extern int	pri_compatible_flags(pri*,uint32_t,uint32_t);
Extern void	pri_change_flags(pri*,uint32_t,uint32_t);
Extern void	pri_init_code(pri*,int);
Extern void	pri_define_code(pri*,int,pri_code_t);
Extern int	pri_change_trans_function(pri*,dident);
Extern int	pri_change_mode(pri*,uint32_t);
Extern int	pri_change_prio(pri*,int);
Extern int	pri_change_run_prio(pri*,int);
Extern int	pri_abolish(pri *pd);
Extern void	erase_module_procs(module_item*);
Extern void	reclaim_procedure(vmcode *code);
Extern	void	ec_mark_dids_from_procs(pri *proc);
Extern	void	ec_free_dyn_code(vmcode *code);

Extern pri *	built_in(dident did1, int (*func) (/* ??? */), word flags);
Extern pri *	local_built_in(dident did1, int (*func) (/* ??? */), word flags);
Extern pri *	exported_built_in(dident did1, int (*func) (/* ??? */), word flags);
Extern pri *	b_built_in(dident did1, int (*func) (/* ??? */), dident module);
Extern int	b_built_code(pri *pd, word function, int nondet);
Extern int	ec_create_call_n(dident call_did);
Extern word	ec_getaddress(char*);
Extern vmcode * allocate_code_block(word size, uword btablepos, uword link, uword bid, uword fid, uword btype, uword cid);

/*
 * Pointers to some compiler-expanded builtin descriptors (in particular
 * those which have an emulator instruction and can delay or raise events)
 */
Extern pri
	*fail_proc_,
	*true_proc_,
	*softcut_proc_,
	*cut_to_proc_,
	*cut_to_stamp_proc_,
	*minus_proc_,
	*plus_proc_,
	*abs_proc_,
	*sgn_proc_,
	*add_proc_,
	*sub_proc_,
	*mul_proc_,
	*quot_proc_,
	*div_proc_,
	*rem_proc_,
	*fdiv_proc_,
	*min_proc_,
	*max_proc_,
	*mod_proc_,
	*and_proc_,
	*or_proc_,
	*xor_proc_,
	*bitnot_proc_,
	*lt_proc3_,
	*le_proc3_,
	*eq_proc3_,
	*ne_proc3_,
	*ge_proc3_,
	*gt_proc3_,
	*identical_proc_,
	*not_identical_proc_,
	*inequality_proc_,
	*not_ident_list_proc_,
	*arg_proc_,
	*arity_proc_,
	*make_suspension_proc_;

/* operator lookup */
Extern opi	visible_op(dident atom, dident module, type mod_tag, int *res);
Extern opi	visible_prefix_op(dident atom, dident module, type mod_tag, int *res);
Extern opi	visible_infix_op(dident atom, dident module, type mod_tag, int *res);
Extern opi	visible_postfix_op(dident atom, dident module, type mod_tag, int *res);
Extern int	is_visible_op(dident atom, dident module, type mod_tag);
Extern int	visible_operator(dident atom, dident module, type mod_tag);

/* ------------------------- STORE TABLE -----------------------*/

typedef struct htable_elem {
    struct htable_elem	*next;
    uword		hash;
    pword		key;
    pword		value;
} t_htable_elem;

typedef struct {
    uword		ref_ctr;
    uword		size;
    uword		nentries;
    uword		internal;
    t_htable_elem	**htable;
    ec_mutex_t		lock;
    ec_cond_t		*cond;
} t_heap_htable;

#define HTABLE_INTERNAL 1

/*-------------------- function declarations --------------------*/

Extern t_heap_htable	*htable_new(int); 
Extern void		htable_free(t_heap_htable *);
Extern int		store_set(t_heap_htable*,value,type,pword*);
Extern int		store_get(t_heap_htable*,value,type,pword*);
Extern int		store_get_else_set(t_heap_htable*,value,type,pword*);

