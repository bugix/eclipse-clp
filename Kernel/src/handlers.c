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
 * VERSION	$Id: handlers.c,v 1.21 2017/09/09 18:52:05 jschimpf Exp $
 */

/** @file
 *
 * Signal handling, including builtins to set up and manipulate handler tables.
 *
 * Each signal is handled in one of the following ways:
 *
 *	- IH_UNCHANGED ECLiPSe makes no attempt at handling the signal
 *	- IH_SYSTEM_DFL	(default/0) handler was (re)set to SIG_DFL (OS default)
 *	- IH_IGNORE (true/0) signal is ignored (SIG_IGN on Unix)
 *	- IH_ECLIPSE_DFL (internal/0) caught by ECLiPSe to implement internals
 *	- IH_POST_EVENT (event/1) leads to posting a synchronous Prolog event
 *	- IH_THROW (throw/1) leads to throw(<signame>)
 *	- IH_ABORT (abort/0) calls abort/0
 *	- IH_HALT (halt/0) calls halt/0, terminates ECLiPSe subsystem
 *	- IH_HANDLE_ASYNC run Prolog handler in signal engine
 *
 * Fatal signals (SEGV etc) are handled directly in the problem
 * thread with catch_fatal().
 *
 * Other signals are handled by a dedicated signal_thread.
 * A signal handler writes the signal number into signal_pipe, and
 * the signal_thread reads this and handles the signals sequentially.
 *
 * If the handler is in Prolog, the signal_thread executes it in the
 * signal_engine (which is dedicated to signal handler execution).
 *
 * For IH_POST_EVENT/IH_THROW/IH_ABORT, the corresponding action
 * (posting a synchronous event, or executing throw/1) is engine-specific.
 * It is applied to the engine from where set_interrupt_handler/2 was
 * invoked.
 * 
 */

#include "config.h"

#include <errno.h>
#include <signal.h>
#ifdef HAVE_STRING_H
#include <string.h>
#else
extern char	*strcpy();
#endif

#include <stdio.h>	/* for sprintf() */
#include <stdlib.h>	/* for exit() */
#include <sys/time.h>	/* for setitimer() */

#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "mem.h"
#include "error.h"
#include "dict.h"
#include "emu_export.h"
#include "ec_io.h"
#include "module.h"
#include "property.h"
#include "os_support.h"


#define MAX_HANDLER_ARITY 4

/*
 * Define sig_action_t in a portable way
 */

#ifdef HAVE_SIGACTION
typedef struct sigaction sig_action_t;

#elif defined(HAVE_SIGVEC)
typedef struct sigvec sig_action_t;
#define sa_handler sv_handler
#define sa_mask sv_mask
#define sa_flags sv_flags

int sigaction(int sig, sig_action_t *action, sig_action_t *oldact)
{
    return sigvec(sig, action, oldact);
}
#else
#define MUST_RESET_HANDLER_ON_ENTRY
typedef struct {
	RETSIGTYPE (*sa_handler)(int);
	int sa_mask;
	int sa_flags;
} sig_action_t;

int sigaction(int sig, sig_action_t *action, sig_action_t *oldact)
{
    return signal(sig, action->sa_handler) == SIG_ERR ? -1 : 0;
}
#endif


/* SA_INTERRUPT is System V (pre-R4)
 * SVR4 has SA_RESTART instead, meaning the opposite
 */
# ifndef SA_INTERRUPT
#  define SA_INTERRUPT 0
# endif


/*
 * Signal blocking and unblocking
 */

#ifdef HAVE_PTHREAD_H
#include <pthread.h>

#define HAVE_SIG_MASKS
#define Empty_Sig_Mask(Mask) (void) sigemptyset(&(Mask));
#define Save_Sig_Mask(Mask) \
	(void) pthread_sigmask(SIG_SETMASK, NULL, &(Mask));
#define Restore_Sig_Mask(Mask) \
	(void) pthread_sigmask(SIG_SETMASK, &(Mask), NULL);
#define Restore_Signal(Mask,i) { \
	sigset_t mask; \
	sigemptyset(&mask); \
	sigaddset(&mask, i); \
	(void) pthread_sigmask(sigismember(&(Mask),i)?SIG_BLOCK:SIG_UNBLOCK, \
		&mask, NULL); }
#define Block_Signal(i) { \
	sigset_t mask; \
	sigemptyset(&mask); \
	sigaddset(&mask, i); \
	pthread_sigmask(SIG_BLOCK, &mask, NULL); }
#define Unblock_Signal(i) { \
	sigset_t mask; \
	sigemptyset(&mask); \
	sigaddset(&mask, i); \
	pthread_sigmask(SIG_UNBLOCK, &mask, (sigset_t *) 0); }


#elif HAVE_SIGPROCMASK

#define HAVE_SIG_MASKS
#define Empty_Sig_Mask(Mask) (void) sigemptyset(&(Mask));
#define Save_Sig_Mask(Mask) \
	(void) sigprocmask(SIG_SETMASK, (sigset_t *) 0, &(Mask));
#define Restore_Sig_Mask(Mask) \
	(void) sigprocmask(SIG_SETMASK, &(Mask), (sigset_t *) 0);
#define Restore_Signal(Mask,i) { \
	sigset_t mask; \
	sigemptyset(&mask); \
	sigaddset(&mask, i); \
	(void) sigprocmask(sigismember(&(Mask),i)?SIG_BLOCK:SIG_UNBLOCK, \
		&mask, NULL); }
#define Block_Signal(i) { \
	sigset_t mask; \
	sigemptyset(&mask); \
	sigaddset(&mask, i); \
	sigprocmask(SIG_BLOCK, &mask, (sigset_t *) 0); }
#define Unblock_Signal(i) { \
	sigset_t mask; \
	sigemptyset(&mask); \
	sigaddset(&mask, i); \
	sigprocmask(SIG_UNBLOCK, &mask, (sigset_t *) 0); }


#elif HAVE_SIGVEC

typedef int sigset_t;

#ifndef sigmask
# define sigmask(n)      (1 << ((n) - 1))
#endif

#define HAVE_SIG_MASKS
#define Empty_Sig_Mask(Mask) (Mask) = 0;
#define Save_Sig_Mask(Mask) (Mask) = sigblock(0);
#define Restore_Sig_Mask(Mask) (void) sigsetmask(Mask);
#define Restore_Signal(Mask,i) \
	(void) sigsetmask(sigblock(0) &~ sigmask(i) | (Mask) & sigmask(i));
#define Block_Signal(i) \
	(void) sigblock(sigmask(i));
#define Unblock_Signal(i) \
	(void) sigsetmask(sigblock(0) & ~sigmask(i));


#else

#undef HAVE_SIG_MASKS
#define Empty_Sig_Mask(Mask)
#define Save_Sig_Mask(Mask)
#define Restore_Sig_Mask(Mask)
#define Restore_Signal(Mask,i)
#define Block_Signal(i)
#define Unblock_Signal(i)

#endif


/* To remember the initial signal dispositions (for resetting) */
#ifdef HAVE_SIG_MASKS
static sigset_t initial_sig_mask_;
#endif


/*
 * The signal that flags C stack overflow must be executed on a different
 * stack, else the handler can't be called. This stack must be big enough
 * to call an emulator and execute the handler (which should call reset/0).
 */

#ifdef HAVE_SIGALTSTACK
static char		signal_stack[SIGSTKSZ];
static stack_t		sigstack_descr;
#else
# ifdef HAVE_SIGSTACK
# ifndef SV_ONSTACK
#  define SV_ONSTACK 1
# endif
# define SIGSTACK_SIZE	4096
static char		signal_stack[SIGSTACK_SIZE];
static struct sigstack	sigstack_descr;
# endif
#endif


#ifdef SIGBUS
#define IsSIGBUS(n) ((n) == SIGBUS)
#else
#define IsSIGBUS(n) 0
#endif
#ifdef SIGQUIT
#define IsSIGQUIT(n) ((n) == SIGQUIT)
#else
#define IsSIGQUIT(n) 0
#endif

#define FatalSignal(n) \
	((n)==SIGILL || (n)==SIGSEGV || IsSIGBUS(n) || IsSIGQUIT(n))

/* signals that are expected to get delivered to the culprit thread */
#define ThreadFatalSignal(n) \
	((n)==SIGILL || (n)==SIGSEGV || IsSIGBUS(n) || (n)==SIGFPE)

/* used only for piping message to signal thread */
#define PSEUDO_SIG_DICT_GC	(NSIG+1)


/*
 * GLOBAL variable definitions
 */

/* Error handlers */
pri	**error_handler_;
pri	**default_error_handler_;

/*
 * Interrupt handlers
 * There is a flag array and a handler array.
 * The latter is only valid if the flag is IH_HANDLE_ASYNC.
 */
int	*interrupt_handler_flags_ = 0;
pri	**interrupt_handler_ = 0;
ec_eng_t	**interrupt_posting_engine_ = 0;
dident	*interrupt_name_ = 0;
int	ec_sigalrm;	/* normally SIGALRM, but also defined on Windows */
int	ec_sigio;	/* normally SIGIO, but also defined on Windows */

static dident	d_event_, d_throw_, d_internal_, d_defers_, d_fatal_;
static type	kernel_tag_;
static int	user_error = USER_ERROR;


static int
	_set_error_array(pri **arr, word n, dident w, value vm, type tm, ec_eng_t*);


#define Check_Error_Number(v,t)				\
	Check_Integer(t)				\
	if ( (v).nint < 1				\
		|| (v).nint >= MAX_ERRORS		\
		|| !ErrorMessage[(v).nint] )		\
	    { Bip_Error(RANGE_ERROR) }

#define Check_Interrupt_Number(v,t)			\
    Check_Integer(t)					\
    if((v).nint <= 0 || (v).nint >= NSIG)		\
	{ Bip_Error(RANGE_ERROR) }


/*----------------------------------------------------------------------*
 * Signal Thread
 *----------------------------------------------------------------------*/

static void	*signal_thread = NULL;
static int	signal_pipe[2];


/**
 * Signal handler that writes the signal number to a pipe which is read
 * by the signal thread's main function.  Who executes this handler
 * depends on the signal masks, but usually it is the signal thread.
 */
static RETSIGTYPE
_write_to_pipe(int signr)
{
    char signr_byte = signr;
    if (!signal_thread)
    	return;				/* not initialised/already finalised */
#ifdef MUST_RESET_HANDLER_ON_ENTRY
    signal(signr, _write_to_pipe);	/* restore signal disposition */
#endif
    if(write(signal_pipe[1], &signr_byte, 1))
    	/*ignore*/;
}


/**
 * HALT signal handler
 */
static RETSIGTYPE
_halt_session(int signr)
{
    ec_cleanup();
    exit(0);
}


/**
 * Pretend that signal signr has occurred.
 * This is only used for ec_sigalrm and ec_sigio fake signals.
 */
void
ec_send_signal(int signr)
{
    if (0 < signr && signr <= NSIG) {
	switch(interrupt_handler_flags_[signr]) {
	    case IH_HANDLE_ASYNC:
	    case IH_THROW:
	    case IH_ABORT:
	    case IH_POST_EVENT:
		_write_to_pipe(signr);
		break;
	    case IH_HALT:
		_halt_session(signr);
		break;
	    default:
		/* ignore everything else for fake signals */
		break;
	}
    }
}


/**
 * Asynchronously initiate a dictionary garbage collection
 * by sending the PSEUDO_SIG_DICT_GC pseudo signal.
 */
void
ec_signal_dict_gc()
{
    _write_to_pipe(PSEUDO_SIG_DICT_GC);
}


/**
 * Signal handler for WAM-level profiling (runs in signal thread)
 */
static RETSIGTYPE
_sigprof_handler(int signr)
{ 
    ec_eng_t *ec_eng = ec_.profiled_engine;

    if (ec_eng)
	(void) ec_outfw(ec_.profile_stream, (word) ec_eng->pp);
}


/**
 * Set up signal handlers and signal masks for the signal_thread.
 * This must be executed in the signal handling thread!
 */

static int
_install_signal_thread_handler(int sig, int how)
{
    sig_action_t action;

    Empty_Sig_Mask(action.sa_mask);
    action.sa_flags = SA_INTERRUPT;

    switch(how)
    {
    case IH_UNCHANGED:
	/* We can't change back from something else to this one */
	break;

    case IH_IGNORE:
	action.sa_handler = SIG_IGN;
	if (sigaction(sig, &action, NULL)) {
	    errno = 0;
	    return 0;	/* something couldn't be ignored, silently accept */
	}
	break;

    case IH_SYSTEM_DFL:
	action.sa_handler = SIG_DFL;
	if (sigaction(sig, &action, NULL))
	    return -1;
	Block_Signal(sig);	/* do not handle in signal thread */
	break;

    case IH_HANDLE_ASYNC:
    case IH_THROW:
    case IH_ABORT:
    case IH_POST_EVENT:
	action.sa_handler = _write_to_pipe;
	if (sigaction(sig, &action, NULL))
	    return -1;
	Unblock_Signal(sig);	/* allow signal for this thread */
	break;

    case IH_HALT:
	action.sa_handler = _halt_session;
	if (sigaction(sig, &action, NULL))
	    return -1;
	Unblock_Signal(sig);	/* allow signal for this thread */
	break;

    case IH_ECLIPSE_DFL:
	/*
	 * This sets handlers that are needed to implement internal
	 * Eclipse functionality like timers, profiler etc
	 */
	if (FatalSignal(sig)) {
	    Block_Signal(sig);	/* handled in appropriate thread, not here */
	} else {
	    switch(sig)
	    {
#ifdef SIGPROF
	    /* The profiling timer is based on total process cpu time */
	    case SIGPROF:
		action.sa_handler = _sigprof_handler;
		if (sigaction(sig, &action, NULL))
		    return -1;
		Unblock_Signal(sig);	/* allow signal for this thread */
		break;
#endif
	    }
	}
	break;
    }
    return 0;
}


/**
 * Run the prolog handler for the given signal number.
 * This must be executed in the signal handling thread!
 */

static void
_run_prolog_handler(ec_eng_t *ec_eng, int sig)
{
    pri *proc = interrupt_handler_[sig];
    pword goal, module;

    if (EngIsDead(ec_eng)) {
	if (PSUCCEED != ecl_init_aux(NULL, ec_eng, 0)) {
	    fprintf(stderr, "Could not (re)init signal engine, ignoring signal %d\n", sig);
	    return;
	}
    } else if (!EngIsOurs(ec_eng)  &&  ecl_acquire_engine(ec_eng) != PSUCCEED) {
	fprintf(stderr, "Signal thread could not acquire signal engine - exiting\n");
	return;
    }

    /* construct goal <defmod>:<handler>(<sig>) */
    goal = ecl_term(ec_eng, d_.colon,
	    ec_atom(PriHomeModule(proc)),
	    DidArity(PriDid(proc)) == 0 ? ec_atom(PriDid(proc))
					: ecl_term(ec_eng, PriDid(proc), ec_long(sig)));
    Make_Module_Atom(&module, PriModule(proc));

    /* run the handler */
    for(;;)
    {
	int res = ecl_resume_goal(ec_eng, goal, module, NULL, GOAL_CUTFAIL);
	switch(res)
	{
	    case PFAIL:
	    case PTHROW:
	    case PEXITED:
		ecl_relinquish_engine_opt(ec_eng, 1);
		return;

	    case PSUCCEED:
		/* cannot happen */
	    default:
		fprintf(stderr, "Signal engine returned %d\n", res);
		Make_Atom(&goal, d_.abort);
		break;
	}
    }
}


/**
 * Main loop executed in signal_thread, reading from signal_fd.
 * Positive numbers are signal handling requests.
 * Negative numbers are signal handler change requests.
 * Function exits when signal_fd is at end-of-file.
 */

static int
_signal_thread_function(int signal_fd)
{
    for(;;)
    {
	char signr;

	/* Read signal number from the pipe, and handle it.
	 * Negative numbers indicate that the handler has changed. */
	int n = read(signal_fd, &signr, 1);
	if (n <= 0) {
	    if (n == 0) {		/* EOF: exit normally */
		signal_thread = NULL;
		close(signal_fd);
		return 0;
	    }
	    if (errno == EINTR) {
	        continue;
	    }
	    perror("read() in _signal_thread_function() - thread dying");
	    return -1;
	}
	if (0 < -signr && -signr <= NSIG) {
	    if (_install_signal_thread_handler(-signr, interrupt_handler_flags_[-signr]))
		perror("Installing signal handler");
	    continue;
	}
	if (signr == PSEUDO_SIG_DICT_GC) {
	    p_gc_dictionary(NULL);
	    continue;
	}
	if (!(0 < signr && signr <= NSIG)) {
	    fprintf(stderr, "Bad signal number on signal_pipe: %d - ignored\n", signr);
	    continue;
	}

	switch(interrupt_handler_flags_[signr])
	{
	    case IH_POST_EVENT:
		ecl_post_event_unique(interrupt_posting_engine_[signr],
					ec_atom(interrupt_name_[signr]));
		break;

	    case IH_THROW:
		ecl_post_throw(NULL, interrupt_posting_engine_[signr],
					ec_atom(interrupt_name_[signr]));
		break;

	    case IH_ABORT:
		ecl_post_throw(NULL, interrupt_posting_engine_[signr],
					ec_atom(d_.abort));
		break;

	    case IH_HANDLE_ASYNC:
		_run_prolog_handler(&ec_.m_sig, signr);
		break;

		/* This can happen when the global handler setting
		 * was changed while a signal was still in the queue.
		 */
	    default:
		fprintf(stderr, "Inconsistent handler setup for signal %d - signal ignored\n", signr);
		/*fall through*/
	    case IH_IGNORE:
		break;
	}
    }
}


/**
 * Create and start signal_thread, if not yet running.
 */

static int
_setup_signal_thread()
{
    if (!signal_thread)
    {
	if (pipe(signal_pipe))
	    return SYS_ERROR_ERRNO;
	if (ec_thread_create(&signal_thread, (void*(*)(void*))_signal_thread_function, (void*)(word)signal_pipe[0]))
	    return SYS_ERROR_OS;
    }
    return PSUCCEED;
}


/*----------------------------------------------------------------------*/

/**
 * Signal handler for fatal signals.  For most signals, this
 * will be executed on the thread that caused the problem.
 * It may execute on the sigaltstack signal_stack.
 */

static RETSIGTYPE
#ifdef SA_SIGINFO
_catch_fatal(int sig, siginfo_t *si, void *dummy)
#else
_catch_fatal(int sig)
#endif
{
    ec_eng_t *eng;
    char buf[128];
    char msg[] =
	"Possible reasons are:\n"
	"- a faulty external C function\n"
	"- certain operations on circular terms\n"
	"- machine stack overflow\n"
	"- an internal error in ECLiPSe\n";

#ifdef MUST_RESET_HANDLER_ON_ENTRY
    signal(sig, _catch_fatal);	/* restore signal disposition */
#endif
#ifdef SA_SIGINFO
    if (si)
	sprintf(buf, "Fatal signal (signal=%d, si_code=%d, si_addr=%08x)\n",
			sig, si->si_code, (int)(word)si->si_addr);
#else
    sprintf(buf, "Fatal signal (signal=%d)\n", sig);
#endif
    /* Can't use ECLiPSe I/O here, as it might need to PYIELD */
    if (write(2, buf, strlen(buf)))
    	/*ignore*/;

    /* Check if the problem happened inside an emulator, by looking
     * for an engine that is running in this thread. If so, perform a
     * throw(fatal_signal_caught).
     */
    eng = eng_chain_header;
    do {
	if (eng->run_thread == ec_thread_self()) {
	    pword ball;
	    Make_Atom(&ball, d_fatal_);
	    ecl_longjmp_throw(eng, ball.val, ball.tag);
	}
	eng = eng->next;
    } while(eng != eng_chain_header);

    /* If no engine found, print message here and panic */
    if (write(2, msg, strlen(msg)))
    	/*ignore*/;
    ec_panic("system now unstable, restart recommended.", NULL);
    exit(2);	/* in case ec_panic() accidentally returns */
}


/*----------------------------------------------------------------------*/

/**
 * Set up a signal handler for sig, according to how and proc.
 *
 * @param sig signal number
 * @param how one of IH_UNCHANGED, IH_SYSTEM_DFL, IH_IGNORE, IH_ECLIPSE_DFL,
 *	    IH_POST_EVENT, IH_THROW, IH_ABORT, IH_HALT, IH_HANDLE_ASYNC.
 * @param proc for IH_HANDLE_ASYNC the handler procedure indentifier,
 *		otherwise ignored (can be NULL)
 * @param ec_eng the engine to which IH_POST_EVENT/IH_THROW/IH_ABORT apply,
 *		otherwise ignored (can be NULL)
 */

static int
_install_int_handler(int sig, int how, pri *proc, ec_eng_t *ec_eng)
{
    sig_action_t action;

    Empty_Sig_Mask(action.sa_mask);
    action.sa_flags = SA_INTERRUPT;

    interrupt_handler_flags_[sig] = how;
    interrupt_handler_[sig] = proc;
    if (interrupt_posting_engine_[sig]) {
	engine_tid.free(interrupt_posting_engine_[sig]);
	interrupt_posting_engine_[sig] = NULL;
    }
    switch(how)
    {
	case IH_POST_EVENT:
	case IH_THROW:
	case IH_ABORT:
	    /* let signal thread post event to this engine */
	    interrupt_posting_engine_[sig] = engine_tid.copy(ec_eng);
	    break;
    }

    /* if this is a fake signal number, do nothing else */
    if (0
#ifndef SIGIO
	|| sig == ec_sigio
#endif
#ifndef SIGALRM
	|| sig == ec_sigalrm
#endif
	)
    {
	return PSUCCEED;	/* this is a fake signal number, do nothing */
    }

    /* Adjust the signal mask for the calling thread */
    switch(how)
    {
	case IH_UNCHANGED:
	    return PSUCCEED;

	case IH_IGNORE:
	    /* signal mask not important */
	    break;

	case IH_SYSTEM_DFL:
	    /* reset mask to how it was when ECLiPSe was initialized */
	    /* (assuming we are in the same thread...) */
	    Restore_Signal(initial_sig_mask_, sig);
	    break;

	case IH_POST_EVENT:
	case IH_THROW:
	case IH_ABORT:
	case IH_HANDLE_ASYNC:
	case IH_HALT:
	    /* block for this thread (and delegate to signal thread) */
	    Block_Signal(sig);
	    break;

	case IH_ECLIPSE_DFL:
	    if (ThreadFatalSignal(sig))
	    {
		/* These signals are handled by the culprit thread */
		if (sig == SIGSEGV)
		{
#ifdef HAVE_SIGALTSTACK
		    /* try to run the SIGSEGV handler on its own stack */
		    sigstack_descr.ss_sp = signal_stack;
		    sigstack_descr.ss_size = SIGSTKSZ;
		    sigstack_descr.ss_flags = 0;
		    (void) sigaltstack(&sigstack_descr, (stack_t *) 0);
		    /* We may need SA_SIGINFO for more sophisticated SEGV handling */
		    action.sa_flags = SA_ONSTACK | SA_INTERRUPT;
#endif
		}
#ifdef SA_SIGINFO
		action.sa_flags |= SA_SIGINFO;
#endif
#ifdef SA_SIGINFO
		action.sa_sigaction = _catch_fatal;
#else
		action.sa_handler = _catch_fatal;
#endif
		if (sigaction(sig, &action, NULL)) {
		    return SYS_ERROR_ERRNO;
		}
		/* The mask should apply to all threads, but this sets only
		 * the calling thread.  Repeat the setup in other threads if
		 * required.  Some of these signals can't be masked anyway */
		Unblock_Signal(sig);
	    }
	    break;

    }

    /* Notify the signal_thread of handler change.
     * Accesses the interrupt_handler_ arrays! */
    if (signal_thread)
	_write_to_pipe(-sig);

    return PSUCCEED;		/* can't check for errors */
}


/**
 * Initialize signal handling for a newly created thread.
 * Some signal handlers require a setup action in every thread.
 * This currently only works for threads that are created _after_
 * the handlers have been set up.
 */
int
ec_thread_reinstall_handlers(void *dummy)
{
#if 1
    /* More generic routine */
    int sig;
    for(sig=1; sig<NSIG; sig++)
    {
	if (interrupt_handler_flags_[sig] == IH_ECLIPSE_DFL)
	    _install_int_handler(sig, IH_ECLIPSE_DFL, NULL, NULL);
    }
#else
    /* Required per-thread re-setup for SIGSEGV (found by experiment) */
    sigaltstack(&sigstack_descr, (stack_t *) 0);
    Unblock_Signal(SIGSEGV);
#endif
    return 0;
}



/**
 * Given tagged integer or atom, return signal number (or negative error code)
 */
int ec_signalnum(value vsig, type tsig)
{
    if (IsInteger(tsig)) {
	if (vsig.nint > 0 && vsig.nint < NSIG && interrupt_name_[vsig.nint] != D_UNKNOWN)
	    return (int) vsig.nint;
	return RANGE_ERROR;

    } else if IsAtom(tsig) {
	int i;
	for (i = 1; i < NSIG; i++)
	    if (interrupt_name_[i] == vsig.did)
		return i;
	return RANGE_ERROR;

    } else if (IsBignum(tsig)) {
	return RANGE_ERROR;
    } else if IsRef(tsig) {
	return INSTANTIATION_FAULT;
    }
    return TYPE_ERROR;
}


/**
 * Implements interrupt_id_det(?Number, ?Name)
 */
static int
p_interrupt_id_det(value vnum, type tnum, value vname, type tname, ec_eng_t *ec_eng)
{
    if (IsInteger(tnum))
    {
	dident int_did;
	if (vnum.nint <= 0 || vnum.nint >= NSIG) {
	    Fail_;
	}
	if ((int_did = interrupt_name_[vnum.nint]) != D_UNKNOWN)
	{
	    Return_Unify_Atom(vname, tname, int_did);
	} else {
	    Return_Unify_Atom(vname, tname, d_.eocl);
	}
    }
    else if (IsAtom(tname))
    {
	int i;
	for (i = 1; i < NSIG; i++)
	{
	    if (interrupt_name_[i] == vname.did)
	    {
		Return_Unify_Integer(vnum, tnum, i);
	    }
	}
    }
    Fail_;
}


/**
 * Implements define_error(+Message, -ErrorNumber)
 */

static int
p_define_error(value valm, type tagm, value vale, type tage, ec_eng_t *ec_eng)
{
	int m, err;

	Check_String(tagm);
	Check_Ref(tage);

	m = user_error++;
	if(m >=  MAX_ERRORS)
	{
	    Bip_Error(RANGE_ERROR);
	}
	ErrorMessage[m] = (char *) hg_alloc((int)StringLength(valm)+1);
	(void) strcpy(ErrorMessage[m], StringStart(valm));
	error_handler_[m] = qualified_procedure(d_.error_handler,
		d_.kernel_sepia, d_.kernel_sepia, kernel_tag_, &err);
	Return_Unify_Integer(vale, tage, m);
}

/**
 * Get a procedure identifier for invoking handler mod:pdid.
 * The handler array entries are considered qualified references
 * from sepia_kernel. If no exported handler exists, we create one,
 * or export an existing local one.
 */
static pri *
_kernel_ref_export_proc(dident pdid, dident mod, type mod_tag)
{
    int err;
    pri *pd = visible_procedure(pdid, mod, mod_tag, 0, &err);
    if (!pd  ||  PriScope(pd) == LOCAL)
    {
	pd = export_procedure(pdid, mod, mod_tag, &err);
	if (!pd)
	    return 0;
    }
    return qualified_procedure(pdid, PriHomeModule(pd),
    				d_.kernel_sepia, kernel_tag_, &err);
}

/**
 * Implements set_error_handler(+EventId, +PredSpec)@Module.
 * Set a handler for an existing error code or an event name.
 */
static int
p_set_error_handler(value vn, type tn, value vp, type tp, value vm, type tm, ec_eng_t *ec_eng)
{
    dident	pdid;
    int		err, defers = 0;

    Error_If_Ref(tn);
    Check_Module(tm, vm);
    if (IsStructure(tp)  &&  vp.ptr->val.did == d_defers_)
    {
	++vp.ptr;
	Dereference_(vp.ptr);
	tp.all = vp.ptr->tag.all;
	vp.all = vp.ptr->val.all;
	defers = 1;
    }
    Get_Proc_Did(vp, tp, pdid);

    if (IsNumber(tn))
    {
	if (defers)
	    { Bip_Error(UNIMPLEMENTED); }
	Check_Error_Number(vn, tn)
	return _set_error_array(error_handler_, vn.nint, pdid, vm, tm, ec_eng);
    }
    else if (IsAtom(tn))
    {
	pri *proc;
	pword prop;

	if (DidArity(pdid) > MAX_HANDLER_ARITY)
	{
	    Bip_Error(RANGE_ERROR)
	}
	proc = _kernel_ref_export_proc(pdid, vm.did, tm);
	if (!proc)
	{
	    Get_Bip_Error(err);
	    Bip_Error(err);
	}

	prop.tag.kernel = TPROC | (defers? EVENT_DEFERS: 0);
	prop.val.ptr = (pword *) proc;
	set_global_property(vn.did, EVENT_PROP, &prop);
	Succeed_;
    }
    else
    {
	Bip_Error(TYPE_ERROR);
    }
}

/**
 * Implements post_events(+EventList).
 * Post events from a list into the event queue
 */
static int
p_post_events(value v, type t, ec_eng_t *ec_eng)
{
    if (IsList(t))
    {
	pword *cdr = v.ptr;
	for(;;)
	{
	    int res;
	    pword *car = cdr++;
	    Dereference_(car);
	    if (IsInteger(car->tag)) {
		Bip_Error(TYPE_ERROR);
	    }
	    /* Integers aren't allowed, let ecl_post_event type check rest */
	    res = ecl_post_event(ec_eng, *car);
	    if (res != PSUCCEED)
	    {
		Bip_Error(res);
	    }
	    Dereference_(cdr);
	    if (IsRef(cdr->tag))
	    {
		Bip_Error(INSTANTIATION_FAULT);
	    }
	    else if (IsNil(cdr->tag))
		break;
	    else if (IsList(cdr->tag))
		cdr = cdr->val.ptr;
	    else
	    {
		Bip_Error(TYPE_ERROR);
	    }
	}
	Succeed_;
    }
    Check_Nil(t);
    Succeed_;
}

/**
 * Implements set_default_error_handler(+ErrorNumber, +PredSpec)@Module.
 * Set the default handler for an existing error code.
 */
static int
p_set_default_error_handler(value vn, type tn, value vp, type tp, value vm, type tm, ec_eng_t *ec_eng)
{
    dident	pdid;
    Check_Error_Number(vn, tn)
    Check_Module(tm, vm);
    Get_Proc_Did(vp, tp, pdid);
    return _set_error_array(default_error_handler_, vn.nint, pdid, vm, tm, ec_eng);
}

/**
 * Helper function to set default_error_handler_[n] or error_handler_[n]
 * to an appropriate procedure identifier for the handler specified by m:w
 */
static int
_set_error_array(pri **arr, word n, dident w, value vm, type tm, ec_eng_t *ec_eng)
{
    pri		*proc;
    int		err;

    if(DidArity(w) > MAX_HANDLER_ARITY && (n < -(DEBUG_CALL_EVENT) || n > -(DEBUG_REDO_EVENT)))
    {
        Bip_Error(RANGE_ERROR)
    }
    if(w == d_.true0)
    {
	arr[n] = true_proc_;
	Succeed_;
    } else if(w == d_.fail) {
	arr[n] = fail_proc_;
	Succeed_;
    } /* else */
    proc = _kernel_ref_export_proc(w, vm.did, tm);
    if(!proc)
    {
	Get_Bip_Error(err);
	Bip_Error(err);
    }
    /* disallow tools here */
    arr[n] = proc;
    Succeed_;
}

/**
 * Implements reset_error_handler(+ErrorId).
 * For error numbers, reset handler to default setting.
 * For atomic error names, erase event property.
 */
static int
p_reset_error_handler(value vn, type tn, ec_eng_t *ec_eng)
{
    Error_If_Ref(tn);
    if (IsInteger(tn))
    {
	Check_Error_Number(vn,tn)
	error_handler_[vn.nint] = default_error_handler_[vn.nint];
	Succeed_;
    }
    else if IsAtom(tn)
    {
	int err = erase_global_property(vn.did, EVENT_PROP);
	if (err < 0 && err != PERROR)
	{
	    Bip_Error(err);
	}
	Succeed_;
    }
    else
    {
	Bip_Error(TYPE_ERROR);
    }
}


/**
 * Implements set_interrupt_handler(+SigNr, +PredSpec)@Module.
 */
static int
p_set_interrupt_handler(value vn, type tn, value vp, type tp, value vm, type tm, ec_eng_t *ec_eng)
{
    dident w;
    pri *proc = 0;
    int sig, err, how;

    Check_Module(tm, vm);
    sig = ec_signalnum(vn, tn);
    if (sig < 0) { Bip_Error(sig); }
    Get_Proc_Did(vp, tp, w);
    if(DidArity(w) > 1)
    {
        Bip_Error(RANGE_ERROR)
    }
    if (w == d_.default0)
	how = IH_SYSTEM_DFL;
    else if (w == d_internal_)
	how = IH_ECLIPSE_DFL;
    else if (w == d_.true0)
	how = IH_IGNORE;
    else if (w == d_event_)
	how = IH_POST_EVENT;
    else if (w == d_throw_)
	how = IH_THROW;
    else if (w == d_.abort)
	how = IH_ABORT;
    else if (w == d_.halt)
	how = IH_HALT;
    else
    {
	how = IH_HANDLE_ASYNC;
	proc = _kernel_ref_export_proc(w, vm.did, tm);
	if(!proc)
	{
	    Get_Bip_Error(err);
	    Bip_Error(err);
	}
    }
    err = _install_int_handler(sig, how, proc, ec_eng);
    Return_If_Error(err);
    Succeed_;
}


/**
 * Implements pause/0.
 */
static int
p_pause(ec_eng_t *ec_eng)
{
#ifdef SIGSTOP
    reset_ttys_and_buffers();
    (void) kill(0, SIGSTOP);
    Succeed_;
#else
#ifdef SIGSUSP
    reset_ttys_and_buffers();
    (void) kill(0, SIGSUSP);
    Succeed_;
#else
    Bip_Error(NOT_AVAILABLE);
#endif
#endif
}


/**
 * Implements get_interrupt_handler(+SigNr, -PredSpec, -Module).
 */
static int
p_get_interrupt_handler(value vn, type tn, value vh, type th, value vm, type tm, ec_eng_t *ec_eng)
{
    dident	wdid, module;
    pri		*proc;
    pword	*pw = TG;
    int		sig;
    Prepare_Requests;

    sig = ec_signalnum(vn, tn);
    if (sig < 0) { Bip_Error(sig); }
    if (!IsRef(th)) {
	Check_Structure(th);
	if (vh.ptr->val.did != d_.quotient) {Bip_Error(TYPE_ERROR); }
	Check_Output_Atom_Or_Nil(vh.ptr[1].val, vh.ptr[1].tag);
	Check_Output_Integer(vh.ptr[2].tag);
    }
    Check_Output_Atom_Or_Nil(vm, tm);

    switch(interrupt_handler_flags_[sig])
    {
    case IH_UNCHANGED:
	Fail_;
    case IH_SYSTEM_DFL:
	wdid = d_.default0;
	module = d_.kernel_sepia;
	break;
    case IH_ECLIPSE_DFL:
	wdid = d_internal_;
	module = d_.kernel_sepia;
	break;
    case IH_IGNORE:
	wdid = d_.true0;
	module = d_.kernel_sepia;
	break;
    case IH_POST_EVENT:
	wdid = d_event_;
	module = d_.kernel_sepia;
	break;
    case IH_THROW:
	wdid = d_throw_;
	module = d_.kernel_sepia;
	break;
    case IH_ABORT:
	wdid = d_.abort;
	module = d_.kernel_sepia;
	break;
    case IH_HALT:
	wdid = d_.halt;
	module = d_.kernel_sepia;
	break;
    case IH_HANDLE_ASYNC:
	proc = interrupt_handler_[sig];
	wdid = PriDid(proc);
	module = PriHomeModule(proc);
	break;
    default:
	Bip_Error(RANGE_ERROR);
    }
    pw = TG;
    Push_Struct_Frame(d_.quotient);
    Make_Atom(&pw[1], add_dict(wdid, 0));
    Make_Integer(&pw[2], DidArity(wdid));
    Request_Unify_Structure(vh, th, pw);
    Request_Unify_Atom(vm, tm, module);
    Return_Unify;
}


/**
 * Implements get_event_handler(+EventId, -PredSpec, -Module).
 */
static int
p_get_event_handler(value vn, type tn, value vh, type th, value vm, type tm, ec_eng_t *ec_eng)
{
    pri *proc;
    pword *pw;
    Prepare_Requests;

    Error_If_Ref(tn);
    if (!IsRef(th)) {
	Check_Structure(th);
	if (vh.ptr->val.did != d_.quotient) {Bip_Error(TYPE_ERROR); }
	Check_Output_Atom_Or_Nil(vh.ptr[1].val, vh.ptr[1].tag);
	Check_Output_Integer(vh.ptr[2].tag);
    }
    Check_Output_Atom_Or_Nil(vm, tm);
    if (IsAtom(tn))
    {
      pword prop;
      int res;
      res = get_global_property(vn.did, EVENT_PROP, &prop);
      Return_If_Not_Success(res);
      proc = (pri *) prop.val.ptr;
    } 
    else if (IsInteger(tn)) 
    {
      Check_Error_Number(vn,tn);
      proc = error_handler_[vn.nint];
      if(proc == (pri *) 0)
	proc = error_handler_[0];
    }
    else
    {
      Bip_Error(TYPE_ERROR)
    }
    pw = TG;
    Push_Struct_Frame(d_.quotient);
    Make_Atom(&pw[1], add_dict(PriDid(proc), 0));
    Make_Integer(&pw[2], DidArity(PriDid(proc)));
    Request_Unify_Structure(vh, th, pw);
    Request_Unify_Atom(vm, tm, PriHomeModule(proc));
    Return_Unify;
}


/* The following builtins use the global error variable ! */

#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

/**
 * Implements valid_error(+ErrorNumber).
 * Reports errors by failing with bip_error set.
 */
static int
p_valid_error(value vn, type tn, ec_eng_t *ec_eng)
{
    Check_Error_Number(vn,tn);
    Succeed_;
}

/* undo redefinition of Bip_Error() */
#undef Bip_Error
#define Bip_Error(N) return(N);


/**
 * Finalize ECLiPSe signal handling.
 * Reset all signal handlers that are set to Eclipse-specific
 * handling, because we are about to shut down Eclipse.
 * Additionally, ignore SIGPIPE, because it might be raised
 * during cleanup of the Eclipse streams.
 */

void
handlers_fini()
{
    int i;

    for(i = 1; i < NSIG; i++)
    {
	if (InterruptName[i] != D_UNKNOWN)
	{
	    switch (interrupt_handler_flags_[i])
	    {
	    case IH_ECLIPSE_DFL:
	    case IH_POST_EVENT:
	    case IH_THROW:
	    case IH_ABORT:
	    case IH_HANDLE_ASYNC:
	    case IH_HALT:
		(void) _install_int_handler(i, IH_SYSTEM_DFL, NULL, NULL);
		break;

	    case IH_UNCHANGED:
	    case IH_SYSTEM_DFL:
	    case IH_IGNORE:
	    default:
		break;
	    }
	}
    }
#ifdef SIGPIPE
    (void) _install_int_handler(SIGPIPE, IH_IGNORE, NULL, NULL);
#endif

    close(signal_pipe[1]);	/* will cause signal thread to exit */
    while (signal_thread)	/* wait for thread to die */
    	ec_sleep(0.001);

    ecl_free_engine(&ec_.m_sig, 0);
}


/*
 * prof(+SamplingRate, +Flags, +SampleStream)
 * Stop if SamplingRate==0.0
 */
static int
p_prof(value v, type t, value vf, type tf, value vs, type ts, ec_eng_t *ec_eng)
{
    int			err_or_copied;
    stream_id		nst;
    double		interv;

    Check_Double(t);
    interv = Dbl(v);

    /* TODO: make these operations atomic */
    if (interv > 0.0) {
	if (ec_.profiled_engine) {
	    Fail_;
	}
	nst = get_stream_id(vs, ts, SWRITE, 0, NULL, &err_or_copied);
	if (nst == NO_STREAM) {
	    Bip_Error(err_or_copied)
	}
	ec_.profile_stream = err_or_copied? nst : stream_tid.copy(nst);
	ec_.profiled_engine = engine_tid.copy(ec_eng);

    } else {
	if (ec_.profiled_engine == ec_eng) {
	    stream_tid.free(ec_.profile_stream);
	    ec_.profile_stream = NULL;
	    engine_tid.free(ec_.profiled_engine);
	    ec_.profiled_engine = NULL;
	} else if (ec_.profiled_engine) {
	    Fail_;	/* other engine is being profiled */
	} /* already off */

    }

#if defined(HAVE_SETITIMER) && defined(SIGPROF)
    {
	struct itimerval	desc;

	desc.it_value.tv_sec =
	desc.it_interval.tv_sec = (long) interv;
	desc.it_value.tv_usec =
	desc.it_interval.tv_usec = (long) ((interv-floor(interv))*1000000.0);
	if (desc.it_value.tv_sec==0 && desc.it_value.tv_usec==0 && interv>0.0)
	    desc.it_value.tv_usec = 1;

	if (setitimer(ITIMER_PROF, &desc, NULL) < 0) {
	    Bip_Error(SYS_ERROR_ERRNO);
	}
    }
#elif defined(USE_TIMER_THREAD)
    if (!ec_set_alarm(interv, interv, (void(*)(long))_sigprof_handler, 0L, NULL, NULL))
	{ Bip_Error(SYS_ERROR_OS); }
#endif

    Succeed_;
}


/**
 * Globally initialize ECLiPSe signal handling.
 */
void
handlers_init(int flags)
{
    register int i;

    d_event_ = in_dict("event",1);
    d_throw_ = in_dict("throw",1);
    d_defers_ = in_dict("defers",1);
    d_fatal_ = in_dict("fatal_signal_caught",0);
    d_internal_ = in_dict("internal",0);
    kernel_tag_.kernel = ModuleTag(d_.kernel_sepia);

    if (flags & INIT_SHARED)
    {
	ErrorHandler =
	    (pri **) hg_alloc(MAX_ERRORS * sizeof(pri *));
	DefaultErrorHandler =
	    (pri **) hg_alloc(MAX_ERRORS * sizeof(pri *));
	DefaultErrorHandler[0] = ErrorHandler[0] =
	    in_dict("boot_error", 2)->procedure;
	for(i = 1; i < MAX_ERRORS; i++)
	{
	    ErrorHandler[i] = (pri *) 0;
	    DefaultErrorHandler[i] = (pri *) 0;
	}

	InterruptHandler =
	    (pri **) hg_alloc(NSIG * sizeof(pri *));
	InterruptHandlerFlags =
	    (int *) hg_alloc(NSIG * sizeof(int));
	InterruptName =
	    (dident *) hg_alloc(NSIG * sizeof(dident));
	interrupt_posting_engine_ =
	    (ec_eng_t **) hg_alloc(NSIG * sizeof(ec_eng_t *));

	for(i = 0; i < NSIG; i++)
	{
	    InterruptHandler[i] = (pri *) 0;
	    InterruptHandlerFlags[i] = IH_UNCHANGED;
	    InterruptName[i] = D_UNKNOWN;
	    interrupt_posting_engine_[i] = NULL;
	}

	/*
	 * Assign the prolog names to the signals
	 */

	/* 0 is a pseudo-signal used for parallel abort */
	InterruptHandlerFlags[0] = IH_POST_EVENT;

#ifdef SIGHUP
	InterruptName[SIGHUP] = in_dict("hup", 0);
#endif
	InterruptName[SIGINT] = in_dict("int", 0);
#ifdef SIGQUIT
	InterruptName[SIGQUIT] = in_dict("quit", 0);
#endif
	InterruptName[SIGILL] = in_dict("ill", 0);
#ifdef SIGTRAP
	InterruptName[SIGTRAP] = in_dict("trap", 0);
#endif
	InterruptName[SIGABRT] = in_dict("abrt", 0);
#ifdef SIGEMT
	InterruptName[SIGEMT] = in_dict("emt", 0);
#endif
	InterruptName[SIGFPE] = in_dict("fpe", 0);
#ifdef SIGKILL
	InterruptName[SIGKILL] = in_dict("kill", 0);
#endif
#ifdef SIGBUS
	InterruptName[SIGBUS] = in_dict("bus", 0);
#endif
	InterruptName[SIGSEGV] = in_dict("segv", 0);
#ifdef SIGSYS
	InterruptName[SIGSYS] = in_dict("sys", 0);
#endif
#ifdef SIGPIPE
	InterruptName[SIGPIPE] = in_dict("pipe", 0);
#endif
#ifdef SIGALRM
	ec_sigalrm = SIGALRM;
	InterruptName[SIGALRM] = in_dict("alrm", 0);
#else
	ec_sigalrm = 0;	/* will be properly assigned below */
#endif
	InterruptName[SIGTERM] = in_dict("term", 0);
#ifdef SIGUSR1
	InterruptName[SIGUSR1] = in_dict("usr1", 0);
#endif
#ifdef SIGUSR2
	InterruptName[SIGUSR2] = in_dict("usr2", 0);
#endif
#ifdef SIGCHLD
	InterruptName[SIGCHLD] = in_dict("chld", 0);
#endif
#ifdef SIGCLD
	InterruptName[SIGCLD] = in_dict("chld", 0);	/* old name for CHLD */
#endif
#ifdef SIGWINCH
	InterruptName[SIGWINCH] = in_dict("winch", 0);
#endif
#ifdef SIGURG
	InterruptName[SIGURG] = in_dict("urg", 0);
#endif
#ifdef SIGSUSP
	InterruptName[SIGSUSP] = in_dict("susp", 0);
#endif
#ifdef SIGSTOP
	InterruptName[SIGSTOP] = in_dict("stop", 0);
#endif
#ifdef SIGTSTP
	InterruptName[SIGTSTP] = in_dict("tstp", 0);
#endif
#ifdef SIGCONT
	InterruptName[SIGCONT] = in_dict("cont", 0);
#endif
#ifdef SIGTTIN
	InterruptName[SIGTTIN] = in_dict("ttin", 0);
#endif
#ifdef SIGTTOU
	InterruptName[SIGTTOU] = in_dict("ttou", 0);
#endif
#ifdef SIGVTALRM
	InterruptName[SIGVTALRM] = in_dict("vtalrm", 0);
#endif
#ifdef SIGPROF
	InterruptName[SIGPROF] = in_dict("prof", 0);
#endif
#ifdef SIGXCPU
	InterruptName[SIGXCPU] = in_dict("xcpu", 0);
#endif
#ifdef SIGXFSZ
	InterruptName[SIGXFSZ] = in_dict("xfsz", 0);
#endif
#ifdef SIGPWR
	InterruptName[SIGPWR] = in_dict("pwr", 0);
#endif
#if defined(SIGIOT) && (SIGIOT != SIGABRT)
	InterruptName[SIGIOT] = in_dict("iot", 0);
#endif
#ifdef SIGWAITING
	InterruptName[SIGWAITING] = in_dict("waiting", 0);
#endif
#ifdef SIGLWP
	InterruptName[SIGLWP] = in_dict("lwp", 0);
#endif
#ifdef SIGPOLL
	InterruptName[SIGPOLL] = in_dict("poll", 0);
#endif
#ifdef SIGIO
	ec_sigio = SIGIO;
	InterruptName[SIGIO] = in_dict("io", 0);	/* after POLL */
#else
	ec_sigio = 0;		/* will be properly assigned below */
#endif
#ifdef SIGLOST
	InterruptName[SIGLOST] = in_dict("lost", 0);
#endif
#ifdef SIGQUIT
	InterruptName[SIGQUIT] = in_dict("quit", 0);
#endif
#ifdef SIGPHONE
	InterruptName[SIGPHONE] = in_dict("phone", 0);
#endif

	/*
	 * If we didn't have SIGALRM defined, find a free number and fake it
	 * (use 14 of possible). We use it for our timer implementation.
	 */
#ifndef SIGALRM
	for(i = 14; i < NSIG; i++)
	{
	    if (InterruptName[i] == D_UNKNOWN)
	    {
		ec_sigalrm = i;
		InterruptName[i] = in_dict("alrm", 0);
		break;
	    }
	}
	if (!ec_sigalrm)
	    ec_panic("Couldn't find a pseudo-signal number for SIGALRM", "handlers_init()");
#endif
#ifndef SIGIO
	for(i = 1; i < NSIG; i++)
	{
	    if (InterruptName[i] == D_UNKNOWN)
	    {
		ec_sigio = i;
		InterruptName[i] = in_dict("io", 0);
		break;
	    }
	}
	if (!ec_sigio)
	    ec_panic("Couldn't find a pseudo-signal number for SIGIO", "handlers_init()");
#endif
    }
    if (flags & INIT_PRIVATE)	/* handler arrays already exist */
    {
	/* get a private copy of the array pointers */
	error_handler_ = ErrorHandler;
	default_error_handler_ = DefaultErrorHandler;
	interrupt_handler_ = InterruptHandler;
	interrupt_handler_flags_ = InterruptHandlerFlags;
	interrupt_name_ = InterruptName;
    }
    /*
     * event builtins
     */
    if (flags & INIT_SHARED)
    {
	(void) exported_built_in(in_dict("prof", 3), p_prof, B_SAFE);
	(void) local_built_in(in_dict("post_events",1),
				p_post_events,			B_SAFE);
	(void) built_in(in_dict("pause",0),	p_pause,	B_SAFE);
	(void) built_in(in_dict("define_error", 2),
				p_define_error,		B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("reset_error_handler", 1),
				p_reset_error_handler,		B_SAFE);
	(void) built_in(in_dict("reset_event_handler", 1),
				p_reset_error_handler,		B_SAFE);

	(void) exported_built_in(in_dict("set_error_handler_", 3),
				p_set_error_handler,		B_SAFE);
	(void) exported_built_in(in_dict("set_default_error_handler_", 3),
				p_set_default_error_handler,	B_SAFE);
	(void) exported_built_in(in_dict("set_interrupt_handler_body", 3),
				p_set_interrupt_handler,	B_SAFE);
	local_built_in(in_dict("get_event_handler", 3),
				p_get_event_handler,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(2, NONVAR) | BoundArg(3, CONSTANT);
	built_in(in_dict("get_interrupt_handler", 3),
				p_get_interrupt_handler, B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(2, NONVAR) | BoundArg(3, CONSTANT);
	(void) local_built_in(in_dict("valid_error", 1),
				p_valid_error,		B_SAFE);
	local_built_in(in_dict("interrupt_id_det", 2),
				p_interrupt_id_det,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(1, NONVAR) | BoundArg(2, NONVAR);
    }
    
    if (flags & INIT_PROCESS)
    {
	Save_Sig_Mask(initial_sig_mask_);
	_setup_signal_thread();		/* could be lazy */
    }
    else		/* on reset signals may need to be unblocked  */
    {
	Restore_Sig_Mask(initial_sig_mask_);
    }

    errno = 0;		/*  we may have ignored some return values ... */

    user_error = USER_ERROR;
}


