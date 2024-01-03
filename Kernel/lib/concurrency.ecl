% ----------------------------------------------------------------------
% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipseclp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The concurrency library for ECLiPSe.
% The Initial Developer of the Original Code is  Coninfer Ltd.
% Portions created by the Initial Developer are
% Copyright (C) 2016 Coninfer Ltd
% 
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: concurrency.ecl,v 1.3 2017/09/03 00:50:40 jschimpf Exp $
% ----------------------------------------------------------------------


:- module(concurrency).

:- comment(categories, ["Programming Utilities","Techniques"]).
:- comment(summary, "Engine utilities and higher-level threading functionality").
:- comment(author, "Joachim Schimpf").
:- comment(copyright, "Coninfer Ltd").
:- comment(date, '$Date: 2017/09/03 00:50:40 $').
:- comment(desc, html("<P>
</P>
")).

:- lib(lists).
:- lib(error).


:- comment(engines/0, [
    summary:"Print the currently known engines and their properties (to log_output)",
    see_also:[engine_create/2,exit_all/1]
]).
:- export engines/0.
engines :-
	sepia_kernel:current_engines(Es),
	( foreach(E,Es), count(I,1,_) do
	    get_engine_property(E, all, Ps),
	    printf(log_output, "%4d: %w%n", [I,Ps])
	),
	fail ; true.	% free the handles


:- comment(exit_all/1, [
    amode:exit_all(+),
    args:["ExitCode":"A small integer"],
    summary:"Post exit(ExitCode) requests to all engines (except self)",
    see_also:[exit/1]
]).
:- export exit_all/1.
exit_all(Exit) :-
	engine_self(Self),
	sepia_kernel:current_engines(Es),
	( foreach(E,Es), count(_I,1,_), param(Exit,Self) do
	    ( E==Self -> true ; engine_post(E, exit(Exit)) )
	).


:- comment(concurrent/3, [
    amode:(concurrent(+,+,+) is semidet),
    args:["MaxThreads":"A positive integer",
    	"Goals":"A list of callable terms",
	"Options":"A list of option terms"
    ],
    summary:"Execute goals concurrently, using multiple threads",
    desc:html("<P>
    Execute the goals in Goals concurrently/in parallel.  Succeed once all
    goals have succeeded.  Only the first solution for each goal is computed.
    Fail (or throw) as soon as one of the goals is found to fail (or throw).
    </P><P>
    Operationally, a copy of each goal is executed in an engine.  Once all
    goals have succeeded, all goals are simultaneously instantiated to their
    first solutions.  The equivalent sequential operation could be written as:
    <PRE>
    concurrent(1, Goals, _) :-
	( foreach(Goal,Goals), foreach(Copy,Copies) do
	    copy_term(Goal, Copy),
	    once(Copy)
	),
	Goals = Copies.
    </PRE>
    A maximum of MaxThreads engines is used to work in parallel.  If there
    are more goals than engines, engines are reused as they become available.
    Use get_flag(cpu_count,N) to obtain the number of processors on the
    machine, which might be a good value for MaxThreads.  If some goals are
    expected to fail, a higher number of threads can be advantageous for
    finding failures earlier.
    </P><P>
    The Options argument can be used to pass extra options for the creation
    of the worker engines.  In particular, the stack size options local(KBytes)
    and global(KBytes) can be used to limit their memory consumption.
    </P><P>
    Due to the overheads of engine creation and destruction, this predicate
    is only useful for sufficiently long-running goals.
    </P>"),
    see_also:[concurrent_or/3,engine_create/2]
]).
:- export concurrent/3.
:- tool(concurrent/3, concurrent_/4).
concurrent_(MaxThreads, Goals, Options, Module) :-
	array_list(Goalz, Goals),
	arity(Goalz, NGoals),
	dim(Resultz, [NGoals]),
	NThreads is min(NGoals,MaxThreads),
	record_create(Ready),
	% Create and start the first NThreads engines
	( for(I,1,NThreads), foreach(E,Es), param(Options,Ready,Goalz,Module) do
	    engine_create(E, [thread,report_to(Ready)|Options]),
	    arg(I, Goalz, Goal),
	    engine_resume_thread(E, run(I,Goal,Module))
	),
	% Collect results of finished engines, and reuse them for remaining goals
	( for(I,NThreads+1,NGoals), param(Ready,Goalz,Resultz,Es,Module) do
	    process_result(Ready, Resultz, Es, E),
	    engine_resume(E, dummy, _false),	% forget engine result state
	    arg(I, Goalz, Goal),
	    engine_resume_thread(E, run(I,Goal,Module))
	),
	% No more goals. Collect results of finished engines and forget them.
	( for(_,1,NThreads), param(Ready,Resultz,Es) do
	    process_result(Ready, Resultz, Es, E),
	    handle_close(E)	% don't wait for GC
	),
	Goalz = Resultz,	% unify results
	handle_close(Ready).	% don't wait for GC

    run(I, Goal, Module) :-
	once(Goal)@Module,
	yield(I->Goal, _),	% deliver result
	fail.

    % Process one incoming result from the Ready-queue.  On success, return
    % the reusable engine, otherwise terminate all engines and fail/throw.
    process_result(Ready, Resultz, Es, E) :-
	record_wait_remove(Ready, E, block),	% E is ready
	engine_join(E, 1, Status),
	( Status = yielded(J->ResultJ) ->
	    arg(J, Resultz, ResultJ)		% store result
	; Status == false ->
	    kill_engines(Es),
	    fail
	; Status = exception(Ball) ->
	    kill_engines(Es),
	    throw(Ball)
	;
	    kill_engines(Es),
	    throw(unexpected_engine_status(Status))
	).

    % abort and destroy engines that may still be running
    kill_engines(Es) :-
    	( foreach(E,Es) do engine_post(E, exit(0)) ).

    handles_abolish(Es) :-
    	( foreach(E,Es) do handle_close(E) ).


% ----------------------------------------------------------------------

:- comment(concurrent_or/3, [
    amode:(concurrent_or(+,+,+) is semidet),
    args:["MaxThreads":"A positive integer",
    	"Goals":"A list of callable terms",
	"Options":"A list of option terms"
    ],
    summary:"Execute alternative goals concurrently, using multiple threads",
    desc:html("<P>
    Execute the goals in Goals concurrently/in parallel.  Succeed once the
    first goal succeeds, and use this goal's solution.  Only one solution
    is computed.  Fails if none of the goals has a solution.  Operationally,
    a copy of each goal is executed in an engine.  The first goal to succeed
    is chosen as the solution, and all others are aborted and ignored.
    The equivalent sequential operation could be written as:
    <PRE>
    concurrent_or(1, Goals, _) :-
	member(Goal, Goals),
	copy_term(Goal, Copy),
	call(Copy),
	!, Goal = Copy.
    </PRE>
    A maximum of MaxThreads engines is used to work in parallel.  If there
    are more goals than engines, engines are reused as they become available.
    </P><P>
    Use get_flag(cpu_count,N) to obtain the number of processors on the
    machine, which might be a good value for MaxThreads.  If some solutions
    are expected to be easier to find than others, a higher number of threads
    (up to one thread per goal) can be advantageous for finding these easy
    solutions earlier.  The latter is essentially a breadth-first-search.
    </P><P>
    The Options argument can be used to pass extra options for the creation
    of the worker engines.  In particular, the stack size options local(KBytes)
    and global(KBytes) can be used to limit their memory consumption.
    </P><P>
    Due to the overheads of engine creation and destruction, this predicate
    is only useful for sufficiently long-running goals.
    </P>"),
    see_also:[concurrent/3,engine_create/2]
]).
:- export concurrent_or/3.
:- tool(concurrent_or/3, concurrent_or_/4).
concurrent_or_(MaxThreads, Goals, Options, Module) :-
	array_list(Goalz, Goals),
	NThreads is min(arity(Goalz),MaxThreads),
	record_create(Ready),
	% Create and start the first NThreads engines
	( for(I,1,NThreads), foreach(E,Es), param(Options,Ready,Goalz,Module) do
	    engine_create(E, [thread,report_to(Ready)|Options]),
	    arg(I, Goalz, Goal),
	    engine_resume_thread(E, run(I,Goal,Module))
	),
	% loop until first solution found or all goals have failed
	(
	    fromto(NThreads,R,R1,0),		% remaining active engines
	    fromto(NThreads,I0,I,_),		% current goal index
	    param(Goalz,Es,Ready,Module)
	do
	    record_wait_remove(Ready, E, block),	% E is ready
	    engine_join(E, 1, Status),
	    ( Status==false ->
		I is I0+1,
	        ( I > arity(Goalz) ->
		    handle_close(E),
		    R1 is R-1
		;
		    arg(I, Goalz, Goal),
		    engine_resume_thread(E, run(I,Goal,Module)),
		    R1 = R
		)

	    ; Status=yielded(J->ResultJ) ->
		writeln(log_output, committing_to:J),
		kill_engines(Es),
		handles_abolish(Es),
		arg(J, Goalz, ResultJ),
		R1 = 0

	    ; Status = exception(Ball) ->
		kill_engines(Es),
		throw(Ball)
	    ;
		kill_engines(Es),
		throw(unexpected_engine_status(Status))
	    )
	),
	handle_close(Ready).	% don't wait for GC


