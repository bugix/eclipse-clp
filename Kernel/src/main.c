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
 * END LICENSE BLOCK
 */


/*
 * ECLiPSe C SOURCE MODULE
 *
 * VERSION	$Id: main.c,v 1.11 2017/09/01 03:05:10 jschimpf Exp $
 *
 * Standalone main()
 *
 * Interprets the command line options, initialises the system,
 * and starts the main Prolog code for the standalone system.
 *
 */


#include "eclipse.h"
#include "os_support.h"

#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include <stdlib.h>
#include <string.h>



#define KB			1024
#define MIN_LOCAL		100*KB
#define MIN_GLOBAL		16*MB
#define MIN_PRIVATE		210*KB
#define MIN_SHARED		5*MB

#define MAX_MEMORY	(SIGN_BIT/8*15)	/* 15/16 of the address space */


/*
 * GLOBAL variable definitions
 */

/*
 * To avoid infinite loops when the memory is corrupted, this flag
 * is set when SEPIA did not manage it to print the prompt and start user query
 * after a reset. 0 means ok, 1 is after a reset and before a query.
 */
static int	memory_corrupted = 0;

sigjmp_buf         reset;

static void *main_thread_;


/*
 * Print the warning about wrong usage and bad_exit.
 * The argument is the bad option string and a NULL argument,
 * or a valid option string and a bad argument string.
 */
static void
usage(char *opt, char *arg)
{
    if (arg)
	fprintf(stderr,"Invalid argument for %s option: %s\n", opt, arg);
    else
	fprintf(stderr,"Invalid option: %s\n",opt);
    fprintf(stderr,"Usage:\n");
    fprintf(stderr,"  -e <goal>     goal to execute (in Prolog syntax)\n");
    fprintf(stderr,"  -f <file>     compile or load file on startup (.ecl or .eco)\n");
    fprintf(stderr,"  -g <size>     global+trail stack size\n");
    fprintf(stderr,"  -l <size>     local+control stack size\n");
    fprintf(stderr,"  -L <language> default language dialect\n");
    fprintf(stderr,"  -t <module>   name of initial toplevel module\n");
    fprintf(stderr,"  -d <seconds>  delayed startup\n");
    fprintf(stderr,"  -D <dir>      installation directory\n");
    fprintf(stderr,"  -P            enable profiling support\n");
    fprintf(stderr,"  --            end of ECLiPSe options\n");
    fprintf(stderr,"Deprecated:\n");
    fprintf(stderr,"  -b <file>     compile or load file on startup (same as -f)\n");
    fprintf(stderr,"  -h <size>     private heap size (ignored)\n");
    fprintf(stderr,"  -s <size>     shared heap size (ignored)\n");
    fprintf(stderr,"Specify <size> in kilobytes, megabytes (M suffix) or gigabytes (G suffix)\n");
#if 0
    fprintf(stderr,"Parallel system only:\n");
    fprintf(stderr,"-w <num>        number of parallel workers\n");
    fprintf(stderr,"-wmi            popup worker manager interface\n");
    fprintf(stderr,"-wv             verbose worker startup\n");
    fprintf(stderr,"-wx <exec>      use specified worker executable\n");
    fprintf(stderr,"Reserved:\n");
    fprintf(stderr,"-a <><><><>\n");
    fprintf(stderr,"-c <>\n");
    fprintf(stderr,"-m <>\n");
    fprintf(stderr,"-o \n");
    fprintf(stderr,"-r <>\n");
#endif
    exit(-1);
}


static void
main_panic(char *what, char *where)
{
    fprintf(stderr, "\n*** ECLiPSe fatal error: %s",what);

    if (where)
        fprintf(stderr, " in %s",where);

    if (ec_thread_self() != main_thread_) {
	fprintf(stderr, "\nExiting thread.\n");
	fflush(stderr);
	ec_thread_exit(NULL);	/* don't exit the whole process, if possible */
    }
    else
    {
	fprintf(stderr, "\nTrying to restart...\n");
	fflush(stderr);
	siglongjmp(reset, 1);
    }
}


static uword
sizearg(char *option, char *arg)
{
    char *arg_end;
    uword multiple = 0;
    long size = strtol(arg, &arg_end, 0);
    if (arg_end[0] == '\0')
	multiple = KB;
    else if (arg_end[1] == '\0')
	switch(arg_end[0])
	{
	case 'k': case 'K': multiple = KB; break;
	case 'm': case 'M': multiple = KB * KB; break;
	case 'g': case 'G': multiple = KB * KB * KB; break;
	}
    if (!multiple)
    	usage(option, arg);
    return (size > MAX_MEMORY/multiple) ? 0 : size * multiple;
}


static uword
posintarg(char *option, char *arg)
{
    char *arg_end;
    long i = strtol(arg, &arg_end, 0);
    if (arg == arg_end || *arg_end != 0  ||  i < 0 )
    	usage(option, arg);
    return (uword) i;
}


int
main(int argc, char **argv)
{
    char *	eclipsedir = (char *) 0;
    int		c, new_argc, res;
    int		with_profiler = 0;
    int		init_flags = INIT_SHARED|INIT_PRIVATE|INIT_PROCESS;
    char *	session, * nsrv_hostname;
    unsigned    nsrv_port_number;
    uword	size;
    ec_eng_t	*ec_eng;
    pword	goal;

#ifdef _WIN32
    /*
     * If stdio is not a tty, get rid of the console window. This is not ideal
     * since the window flashes up briefly, but no better solution yet.
     * (The correct way would be not to build eclipse.exe as a "console
     * application" and have a WinMain() instead of main(). But then we have
     * to do all the setup of stdin/out/err, argc/argv, environment etc
     * ourselves)
     */
    if (!isatty(_fileno(stdin))
     && !isatty(_fileno(stdout))
     && !isatty(_fileno(stderr)))
    {
	FreeConsole();
    }
#endif
	
    /*
     * collect information from the command line
     * remove some internally used arguments from the command line
     */
    for (c = new_argc = 1; c < argc; )
    {
	if (argv[c][0] == '-' && argv[c][2] == 0)	/* single char opt */
	{
	    switch (argv[c][1])
	    {
	    case 'g':				/* -g <size> */
		argv[new_argc++] = argv[c];		/* shift */
		if (++c + 1 > argc) usage(argv[c-1], NULL);
		argv[new_argc++] = argv[c];		/* shift */
		size = sizearg("-g", argv[c++]);
		ec_set_option_long(EC_OPTION_GLOBALSIZE, size);
		if (size < MIN_GLOBAL) {
		    ec_bad_exit("ECLiPSe: Global stack size too small.");
		}
		break;

	    case 'd':				/* -d <n> */
		/* delay worker startup by <n> seconds */
		if (++c + 1 > argc) usage(argv[c-1], NULL);
		ec_sleep((double) posintarg("-d", argv[c++]));
		break;

	    case 'D':				/* -D <eclipsedir> */
		if (++c + 1 > argc) usage(argv[c-1], NULL);
		eclipsedir = argv[c++];
		break;

	    case 'l':				/* -l <size> */
		argv[new_argc++] = argv[c];		/* shift */
		if (++c + 1 > argc) usage(argv[c-1], NULL);
		argv[new_argc++] = argv[c];		/* shift */
		size = sizearg("-l", argv[c++]);
		ec_set_option_long(EC_OPTION_LOCALSIZE, size);
		if (size < MIN_LOCAL) {
		    ec_bad_exit("ECLiPSe: local stack size too small.");
		}
		break;

#ifdef WITH_ALLOC_FIXED
	    case 'a':			/* -a <worker> <session> 
                                              <nsrv_hostname> <nsrv_port_no> */
		if (++c + 4 > argc) usage(argv[c-1], NULL);
		ec_set_option_int(EC_OPTION_PARALLEL_WORKER, atoi(argv[c++]));	
		session = argv[c++];
		nsrv_hostname = argv[c++];
		nsrv_port_number = atoi(argv[c++]);
		break;

	    case 'c':				/* -c <shared_map_file> */
		if (++c + 1 > argc) usage(argv[c-1], NULL);
		ec_set_option_ptr(EC_OPTION_MAPFILE, argv[c++]);
		ec_set_option_int(EC_OPTION_ALLOCATION, ALLOC_FIXED);
		init_flags &= ~INIT_SHARED;
		break;

	    case 'm':				/* -m <shared_map_file> */
		if (++c + 1 > argc) usage(argv[c-1], NULL);
		ec_set_option_ptr(EC_OPTION_MAPFILE, argv[c++]);
		ec_set_option_int(EC_OPTION_ALLOCATION, ALLOC_FIXED);
		break;

	    case 'h':				/* -h <size> */
		argv[new_argc++] = argv[c];		/* shift */
		if (++c + 1 > argc) usage(argv[c-1], NULL);
		argv[new_argc++] = argv[c];		/* shift */
		size = sizearg("-h", argv[c++]);
		ec_set_option_long(EC_OPTION_PRIVATESIZE, size);
		if (size < MIN_PRIVATE) {
		    ec_bad_exit("ECLiPSe: Private heap size too small.");
		}
		break;

	    case 's':				/* -s <size> */
		argv[new_argc++] = argv[c];		/* shift */
		if (++c + 1 > argc) usage(argv[c-1], NULL);
		argv[new_argc++] = argv[c];		/* shift */
		size = sizearg("-s", argv[c++]);
		ec_set_option_long(EC_OPTION_SHAREDSIZE, size);
		if (size < MIN_SHARED) {
		    ec_bad_exit("ECLiPSe: Shared heap size too small.");
		}
		break;
#else
	    case 'h':
	    case 's':
		fprintf(stderr,"Ignoring unsupported option %s\n", argv[c]);
		c += 2;
		break;
#endif

	    case 'o':				/* enable oracles */
		c += 1;
		/* vm_options = ORACLES_ENABLED; */
		break;

	    case 'P':				/* allow profiling */
		c += 1;
		with_profiler = 1;
		ec_set_option_int(EC_OPTION_WITH_PROFILER, 1);
		break;

                /* Options processed by Prolog-level code */
	    case 'b':				/* -b <bootfile> */
	    case 'f':				/* -f <file> */
	    case 'e':				/* -e <goal> */
	    case 'L':				/* -L <language> */
	    case 't':				/* -t <module> */
		argv[new_argc++] = argv[c];		/* shift */
		if (++c + 1 > argc) usage(argv[c-1], NULL);
		argv[new_argc++] = argv[c++];		/* shift */
		break;

	    case '-':				/* -- give the rest to Prolog */
		for (; c < argc; )
		    argv[new_argc++] = argv[c++];
		break;

	    default:				/* unknown: error */
		usage(argv[c], NULL);
		break;
	    }
	}
	else if (!strcmp(argv[c], "-debug_level"))
	{
	    if (++c + 1 > argc) usage(argv[c-1], NULL);
	    ec_set_option_int(EC_OPTION_DEBUG_LEVEL, atoi(argv[c++]));
	}
	else /* raise error unless preceeded by a -- option */
	{
	    usage(argv[c], NULL);
	}
    }

    /*----------------------------------------------------------------
     * Entry point after longjmp(reset)
     *----------------------------------------------------------------*/

    switch (sigsetjmp(reset,1))
    {
    case 0:		/* raw boot or -r from above */
	break;
    case 3:		/* restore program state */
    case 2:
	init_flags = REINIT_SHARED|INIT_PRIVATE;
	break;
    case 4:		/* restore execution state */
	init_flags = REINIT_SHARED|INIT_PRIVATE;
	break;
    case 1:		/* reset after fatal error */
    default:
	init_flags = 0;
	switch (memory_corrupted++)
	{
	    case 0:
		break;

	    case 1:
		/* try to print a message */
		memory_corrupted = 2;
		ec_bad_exit("ECLiPSe: Fatal error, memory corrupted.");
		/* fall to */
	    case 2:
		/* we couldn't even print the message */
		exit(-1);
	}
	break;
    }
    
    /*
     * set up our own panic function which longjumps back to reset
     */
    main_thread_ = ec_thread_self();
    ec_set_option_ptr(EC_OPTION_PANIC, main_panic);

    ec_set_option_int(EC_OPTION_INIT, init_flags);
    ec_set_option_int(EC_OPTION_ARGC, new_argc);
    ec_set_option_ptr(EC_OPTION_ARGV, argv);
    if (eclipsedir)
	ec_set_option_ptr(EC_OPTION_ECLIPSEDIR, eclipsedir);

    if (init_flags) {
	res = ecl_init(NULL, &ec_eng);
	if (res != PSUCCEED) {
	    char buf[1024];
	    ec_make_error_message(res, "initialization", buf, 1024);
	    ec_bad_exit(buf);
	}
    } else {
	/* re-init main engine after fatal error,
	 * hoping we can get a recovery prompt */
	extern int ecl_engine_init(ec_eng_t*, ec_eng_t*);

	ec_eng = &ec_.m;
	if (ecl_engine_init(NULL, ec_eng) != PSUCCEED) {
	    char buf[1024];
	    ec_make_error_message(res, "attempted restart", buf, 1024);
	    ec_bad_exit(buf);
	}
    }
    
    goal = ecl_term(ec_eng, ec_did(":",2),
		    ec_atom(ec_did("sepia_kernel",0)),
		    ec_atom(ec_did("standalone_toplevel",0)));

    for(;;) {
	long exit_code;

	res = ecl_resume2(ec_eng, goal, NULL);
	switch(res) {
	    case PSUCCEED:
	    case PFAIL:
	    case PTHROW:
		break;

	    case PEXITED:
		ec_get_long(ecl_arg(ec_eng,2), &exit_code);
		res = (int) exit_code;
		break;

	    case PWAITIO:
		fprintf(stderr,"Unexpected waitio request from main engine, exiting.\n");
		break;

	    default:
		fprintf(stderr,"Ignoring %s request from main engine, resuming.\n",
			res==PYIELD? "yield": res==PFLUSHIO? "flushio": "unknown");
		goal = ec_nil();
		continue;
	}
	break;
    }

    ec_cleanup();
    return res;
}

