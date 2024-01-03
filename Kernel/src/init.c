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
 * VERSION	$Id: init.c,v 1.17 2017/09/01 03:05:09 jschimpf Exp $
 */

/****************************************************************************
 *
 *	init.c
 *	------
 *
 *	Initialisation routines for ECLiPSe
 *
 *
 ***************************************************************************/

#include 	"config.h"
#include        "sepia.h"
#include 	"types.h"
#include	"embed.h"
#include 	"error.h"
#include 	"mem.h"
#include 	"dict.h"
#include	"module.h"
#include	"os_support.h"
#include	"ec_io.h"
#include	"emu_export.h"

#include <errno.h>
#include <stdio.h>	/* for sprintf() */
#include <stdlib.h>	/* for exit() */

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_SYS_MMAN_H
#include	<sys/mman.h>
#endif



/*
 * EXTERN declarations
 */

extern int	io_init(int flags);


extern void	bip_arith_init(int flags),
		bip_array_init(int flags),
		bip_comp_init(int flags),
		bip_control_init(int flags),
		bip_engines_init(int flags),
		bip_db_init(int flags),
		bip_delay_init(int flags),
		bip_domain_init(int flags),
                bip_elipsys_fd_init(int flags),
		bip_emu_init(int flags),
		bip_gc_init(int flags),
		bip_handles_init(int flags),
		bip_io_init(int flags),
		bip_load_init(int flags),
		bip_misc_init(int flags, char *),
		bip_random_init(int flags),
		bip_module_init(int flags),
		bip_op_init(int flags),
		bip_parallel_init(),
		bip_copy_init(int flags),
		bip_serialize_init(int flags),
		bip_record_init(int flags),
		bip_store_init(int flags),
		bip_shelf_init(int flags),
		bip_bag_init(int flags),
		bip_heapevent_init(int flags),
		bip_strings_init(int flags),
		bip_tconv_init(int flags),
		code_init(int flags),
		compiler_init(int flags),
		dict_init(int flags),
		error_init(int flags),
		exit_mps(),
		handlers_init(int flags),
		handlers_fini(),
		lex_init(int flags),
		malloc_init(void),
		mem_init(int flags),
		mem_fini(void),
		module_init(int flags),
		opaddr_init(void),
		op_init(int flags),
		parallel_init(),
		msg_init(),
		read_init(int flags),
		setup_mps(),
		worker_init(),
		write_init(int flags);

extern void	user_init();

extern void	short_sleep();

extern void  default_panic(const char *what, const char *where);
extern char * eclipsehome(void);

/*
 * GLOBAL function declarations
 */

void		ec_worker_cleanup(void);

/*
 * LOCAL function declarations
 */

static void	wait_for_flag(volatile int *pflag, int mask);

static char * arg1 = "Embedded ECLiPSE";

/*
 * GLOBAL variable definitions
 */

/* added = {} initialisation (which does nothing) to work around MinGW bug
   with gcc 4.2 so that entry for ec_ will be generated for eclipse.def
   (Kish Shen 2010-09-24)
*/
t_eclipse_data	ec_ = {};


/* TODO: move the following into ec_ on main branch */
char *ec_eclipse_home;		/* canonical, hp_allocated */


/*
 * The ec_options structure is
 * - statically initialised
 * - can be overwritten by the embedding application before ec_init()
 * - is pure input, i.e. must not be changed by eclipse itself
 * - memory pointed to by members is owned by host application, not ECLiPSe
 */

t_eclipse_options ec_options =
{
	/* mapfile */
	(char *) NULL,

	/* parallel_worker */
	0,

	/* io_option */
	SHARED_IO,

	/* Argv,Argc */
	(char **) &arg1,
	1,

	/* rl */
#if defined(HAVE_READLINE)
	 1,
#else
	 0,
#endif

	/* localsize globalsize */
#if defined(HAVE_MMAP) || defined(_WIN32)
	VIRTUAL_LOCAL_STACK_DEFAULT*MB*2*SIZEOF_WORD,
	VIRTUAL_GLOBAL_STACK_DEFAULT*MB*2*SIZEOF_WORD,
#else
#define KB 1024
#define DEFAULT_LOCAL		200*KB
#define DEFAULT_GLOBAL		750*KB
	DEFAULT_LOCAL,DEFAULT_GLOBAL,
#endif
	/* privatesize,sharedsize */
	VIRTUAL_HEAP_DEFAULT*MB*2*SIZEOF_WORD,
	VIRTUAL_SHARED_DEFAULT*MB*2*SIZEOF_WORD,

	/* user_panic */
	default_panic,
	
	/* allocation */
#if defined(HAVE_MMAP) || defined(_WIN32)
#ifdef sun4_0
	ALLOC_FIXED,
#else
	ALLOC_VIRTUAL,
#endif
#else
	ALLOC_PRE,
#endif
	
	/* default_module */
	"eclipse",

	/* eclipse_home, input, non-canonical */
	(char *) 0,

	/* init_flags */
	(INIT_SHARED|INIT_PRIVATE|INIT_PROCESS),

	/* debug_level */
	0,

	/* default_language */
	(char *) 0,

	/* vm_options */
	0
};


/*----------------------------------------------------------------------
 * Setting the initialisation options
 *----------------------------------------------------------------------*/

/* backwards compatibility */
int Winapi
ec_set_option_int(int opt, int val)
{
    return ec_set_option_long(opt, (word) val);
}

int Winapi
ec_set_option_long(int opt, word val)
{
    return ecl_set_option_long(&ec_options, opt, val);
}

int Winapi
ecl_set_option_long(t_eclipse_options *poptions, int opt, word val)
{
    switch (opt) {
    case EC_OPTION_PARALLEL_WORKER:	poptions->parallel_worker = (int) val; break;
    case EC_OPTION_ARGC:	poptions->Argc = (int) val; break;
    case EC_OPTION_LOCALSIZE:	poptions->localsize = val; break;
    case EC_OPTION_GLOBALSIZE:	poptions->globalsize = val; break;
    case EC_OPTION_PRIVATESIZE:	poptions->privatesize = val; break;
    case EC_OPTION_SHAREDSIZE:	poptions->sharedsize = val; break;
    case EC_OPTION_ALLOCATION:	poptions->allocation = (int) val; break;
    case EC_OPTION_IO:		poptions->io_option = (int) val; break;
    case EC_OPTION_INIT:
	poptions->init_flags = (int) val | (poptions->init_flags & ~(INIT_SHARED|REINIT_SHARED|INIT_PRIVATE|INIT_PROCESS));
    	break;
    case EC_OPTION_DEBUG_LEVEL:	poptions->debug_level = val; break;
    case EC_OPTION_CWD_SEPARATE:ec_use_own_cwd = (int) val; break;
    case EC_OPTION_WITH_PROFILER:
	poptions->init_flags |= (val?INIT_WITH_PROFILER:0); break;
    default:			return RANGE_ERROR;
    }
    return PSUCCEED;
}

int Winapi
ec_set_option_ptr(int opt, void *val)
{
    return ecl_set_option_ptr(&ec_options, opt, val);
}

int Winapi
ecl_set_option_ptr(t_eclipse_options *poptions, int opt, void *val)
{
    switch (opt) {
    case EC_OPTION_MAPFILE:	poptions->mapfile = (char *) val; break;
    case EC_OPTION_ARGV:	poptions->Argv = (char **) val; break;
    case EC_OPTION_PANIC:	poptions->user_panic = (void(*)(const char*,const char *)) val; break;
    case EC_OPTION_DEFAULT_MODULE:	poptions->default_module = (char *) val; break;
    case EC_OPTION_DEFAULT_LANGUAGE:	poptions->default_language = (char *) val; break;
    case EC_OPTION_ECLIPSEDIR:	poptions->eclipse_home = (char *) val; break;
    default:			return RANGE_ERROR;
    }
    return PSUCCEED;
}


/*----------------------------------------------------------------
 * Initialisation
 *
 * init_flags indicates what parts of the system need to be
 * initialised (bit-significant flags):
 *
 *	INIT_SHARED	shared/saveable heap
 *	REINIT_SHARED	heap was restored, some info must be updated
 *	INIT_PRIVATE	C variables, private heap
 *	INIT_PROCESS	do initialisations that are needed once
 *
 * Initialisation is done in different situations:
 *
 * raw boot		INIT_SHARED|INIT_PRIVATE|INIT_PROCESS
 * after -r		REINIT_SHARED|INIT_PROCESS|INIT_PRIVATE
 * after -c		INIT_PROCESS|INIT_PRIVATE
 *----------------------------------------------------------------*/


int
eclipse_global_init(int init_flags)
{
    int err;

    ec_os_init();
    mem_init(init_flags);	/* depends on -c and -m options */

    /*
     * convert pathname to canonical representation
     */
    if (ec_options.eclipse_home)
    {
	char buf[MAX_PATH_LEN];
	(void) canonical_filename(ec_options.eclipse_home, buf);
	if (buf[0] != '/')
	{
	    /* This is mainly to enable the use of -D with relative path */
	    char buf2[MAX_PATH_LEN];
	    get_cwd(buf2, MAX_PATH_LEN);
	    strcat(buf2, buf);
	    ec_eclipse_home = strcpy((char*) hp_alloc(strlen(buf2)+1), buf2);
	}
	else
	{
	    ec_eclipse_home = strcpy((char*) hp_alloc(strlen(buf)+1), buf);
	}
    }
    else
    {
	ec_eclipse_home = strcpy((char*) hp_alloc(strlen(eclipsehome())+1), eclipsehome());
    }

    if (init_flags & INIT_WITH_PROFILER)
	ec_.emulator = ec_emulate_profile;
    else
	ec_.emulator = ec_emulate;

    dict_init(init_flags);
    opaddr_init();
    worker_init(init_flags);
    op_init(init_flags);
    module_init(init_flags);	/* creates modules */
    if ((err = io_init(init_flags)) != PSUCCEED)
    {
	char msg[1024];
	ec_make_error_message(err, "io_init", msg, 1024);
	ec_bad_exit(msg);
    }
    bip_emu_init(init_flags);
    bip_arith_init(init_flags);
    bip_array_init(init_flags);
    bip_comp_init(init_flags);
    bip_control_init(init_flags);
    bip_handles_init(init_flags);
    bip_engines_init(init_flags);
    bip_db_init(init_flags);
    bip_delay_init(init_flags);
    bip_domain_init(init_flags);
    bip_elipsys_fd_init(init_flags);
    bip_record_init(init_flags);
    bip_store_init(init_flags);
    bip_shelf_init(init_flags);
    bip_bag_init(init_flags);
    bip_heapevent_init(init_flags);
    bip_parallel_init(init_flags);
    bip_gc_init(init_flags);
    bip_io_init(init_flags);
    bip_op_init(init_flags);
    bip_copy_init(init_flags);
    bip_serialize_init(init_flags);
    compiler_init(init_flags);
    error_init(init_flags);
    lex_init(init_flags);
    read_init(init_flags);
    write_init(init_flags);
    bip_load_init(init_flags);
    bip_strings_init(init_flags);
    bip_tconv_init(init_flags);
    code_init(init_flags);
    bip_module_init(init_flags);
    user_init(init_flags);
    bip_misc_init(init_flags, ec_eclipse_home);
    bip_random_init(init_flags);
    handlers_init(init_flags);
    msg_init(init_flags);

    return 0;
}


int Winapi
ecl_init(t_eclipse_options *opts, ec_eng_t **eng)
{
    if (!opts)
	opts = &ec_options;	/* default to global options */

    /*
     * Init the global (shared) eclipse structures, dictionary, code...
     * Note that we don't have an engine yet!
     */
    eclipse_global_init(opts->init_flags);

    /* Initialize engines */
    return ecl_engines_init(opts, eng);
}


int Winapi
ec_init(void)
{
    ec_eng_t	*ec_eng;
    return ecl_init(NULL, &ec_eng);
}


int
eclipse_boot(ec_eng_t *ec_eng, char *initfile)
{
    value	v1, v2;
    type	t1, t2;
    v1.did = enter_dict(initfile, 0);
    t1.kernel = TDICT;
    v2.did = d_.kernel_sepia;
    t2.kernel = ModuleTag(d_.kernel_sepia);
    return boot_emulc(ec_eng, v1, t1, v2, t2);
}


/*----------------------------------------------------------------*/
/** Shutdown code (see also p_exit(), exit/1 and halt/0).
 *
 * Shutdown can be requested either from Prolog (exit/1,halt/0) or
 * from C (ec_cleanup()). In either case, we first do a cleanup at
 * the Prolog level (running finalization goals etc), then the low
 * level cleanup ec_cleanup1().
 * 
 * The cleanup is to be done such that all dynamic resources are
 * freed, and the system can either be reinitialised by ec_init(),
 * or, in the embedded case, the eclipse.[so,dll] can be unloaded,
 * freeing all ECLiPSe-related resources in the process.
 * In particular, we must take care of:
 * - closing all I/O
 * - destroying threads
 * - unloading shared libraries
 * - deallocating the engine stacks
 * - resetting signal handlers
 * - freeing all heap spaces
 * - resetting all static variables to their initial state
 * Because we destroy our shared and private heaps indiscriminately
 * at the end, we need not be too concerned about explicitly deallo-
 * cating all data structures that were previously allocated there.
 * However, we must then be sure that the embedding host does not
 * retain pointers to such (hg/hp_allocated) data. In case ECLiPSe
 * makes any allocations with the system malloc(), these must be
 * freed explicitly otherwise they will constitute a memory leak.
 */

int
ec_cleanup1(int exit_code)
{
    /*
     * Assume Prolog-level cleanup is already done,
     * either in ec_cleanup() or in exit/1
     */

    if (ec_options.parallel_worker)
    	halt_system(exit_code);

    ec_worker_cleanup();
    return PSUCCEED;
}

int Winapi
ec_cleanup(void)
{
    int res;
    pword goal;

    /* For backward compatibility with single-engine ECLiPSe: */
    if (EngIsOurs(default_eng))
	ecl_relinquish_engine_opt(default_eng, 1);
    if (default_eng->ref_ctr)
	ecl_free_engine(default_eng, 0);

    /* Do Prolog-level cleanup code: call cleanup_before_exit/0 */
    res = ecl_acquire_engine(aux_eng);
    if (res >= 0) {
	res = ecl_resume_goal(aux_eng, ec_atom(enter_dict("cleanup_before_exit", 0)),
    				ec_nil(), NULL, GOAL_NOTNOT);
    }
    if (res < 0) {
	char msg[] = "ECLiPSe: problem in cleanup_before_exit\n";
	if (write(2, msg, strlen(msg)))
	    /*ignore*/;
    }
    ecl_relinquish_engine_opt(aux_eng, 1);
    ecl_free_engine(aux_eng, 0);

    return ec_cleanup1(0);
}

void
ec_exit(int exit_code)
{
    ec_cleanup1(exit_code);
    exit(exit_code);
}

/*
 * Cleanup one worker
 */
void
ec_worker_cleanup(void)
{
    handlers_fini();		/* undo signal handler settings */
				/* (before shutting down emu and i/o) */

    ec_embed_fini();

    /* TODO: find handles stored in the heap, and free them */

    bip_load_fini();		/* unload any shared libraries */

    flush_and_close_io(1);	/* shut down I/O system */

    assert(ec_.m_aux.ref_ctr == 0);
    assert(ec_.m_sig.ref_ctr == 0);
    assert(ec_.m_timer.ref_ctr == 0);
    assert(ec_.m.ref_ctr == 0);
    
    ec_os_fini();		/* timers, threads, sockets */

    if (ec_options.parallel_worker)
	exit_mps();

    /* finally, release all heap memory */
    mem_fini();
}


static void
wait_for_flag(volatile int *pflag, int mask) /* volatile is important! */
{
    while (!(*pflag & mask))
	short_sleep(10000);
}

