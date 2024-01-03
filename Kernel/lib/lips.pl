% ----------------------------------------------------------------------
% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1991-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: lips.pl,v 1.3 2009/07/16 09:11:24 jschimpf Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	lips.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		lips/0,1,2
%
% DESCRIPTION:		Measure the system's speed using naive reverse
%

:- module(lips).

:- comment(categories, ["Development Tools"]).
:- comment(summary, "Measure the system's speed using the naive reverse benchmark").
:- comment(author, "Joachim Schimpf, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2009/07/16 09:11:24 $").
:- comment(desc, html("
    Measure the system's speed in logical inferences per second, using
    the infamous naive reverse program. This test does not say very much
    about the quality of a system. All it gives is an indication about
    the speed of list processing.
    <P>
    The executed program is:
    <PRE>
    nreverse([], []).
    nreverse([X|L0],L) :-
	    nreverse(L0, L1),
	    concatenate(L1, [X], L).

    concatenate([], L, L).
    concatenate([X|L1], L2, [X|L3]) :-
	    concatenate(L1, L2, L3).
    </PRE>
    and the standard benchmark is to call nreverse/2 with a 30-element
    list as the first and a variable as the second argument.  This
    instance is assumed to have 496 logical inferences.
    ")).
:- comment(lips/0, [template:"lips",
    summary:"Run the benchmark a reasonable number of times and print the average speed"
    ]).
:- comment(lips/1, [template:"lips(+Count)",
    summary:"Run the benchmark Count times and print the average speed"
    ]).
:- comment(lips/2, [template:"lips(+Count,+Length)",
    summary:"Run the benchmark Count times with lists of length Length"
    ]).

:- export lips/0, lips/1, lips/2.

:- pragma(nodebug).
?- set_flag(gc, off).

lips :-
	lips(1000000, 30).

lips(Count) :-
	lips(Count, 30).

lips(Count, Length) :-
	conslist(Length, List),

	cputime(T4),
	compens_loop(Count, List),
	cputime(T5),
	Empty is T5-T4,

	cputime(T0),
	call_loop(Count, List),
	cputime(T1),
	T is T1-T0-Empty,

	printf("%d iterations of nrev(%d)\n", [Count,Length]),

	LI is Length*(Length+3)/2 + 1,
	LIPS is LI * Count / T / 1000,
	printf("%d * %.1f inferences / %.3f seconds / 1000 = %.1f KLIPS\n",
		[Count,LI,T,LIPS]).


compens_loop(N, List) :-
        between(1, N, 1, _),
        dummy(List, _),
        fail.
compens_loop(_, _).

call_loop(N, List) :-
        between(1, N, 1, _),
	nreverse(List, _),
	fail.
call_loop(_, _).

conslist(0, []) :- !.
conslist(N, [N|L]) :-
	N1 is N-1,
	conslist(N1, L).

dummy(_,_).


:- export par_lips/1.
par_lips(NReps) :-
        get_flag(cpu_count, MaxCpu),
        ( for(_,1,MaxCpu), foreach(E,Es) do
            engine_create(E, [thread])
        ),
        findall(NCpu-Time, (
                between(1, MaxCpu, 1, NCpu),
                length(NEs, NCpu),
                append(NEs, _, Es),
                par_lips(NEs, NReps, Time)
            ), Times),
        Times = [1-T1|_],
        ( foreach(NCpu-T,Times), param(T1) do
            Speedup is NCpu*T1/T,
            printf("%2d engines/threads: %5.1fs (%2.1f)%n", [NCpu,T,Speedup])
        ).


par_lips(Es, N, T) :-
	conslist(30, List),

	statistics(session_time, T0),
        ( foreach(E,Es), param(N,List) do
            engine_resume_thread(E, call_loop(N, List))
        ),
        ( foreach(E,Es) do
            engine_join(E, block, true)
        ),
	statistics(session_time, T1),
	T is T1-T0.


% This is the benchmark:

nreverse([], []).
nreverse([X|L0],L) :-
	nreverse(L0, L1),
	concatenate(L1, [X], L).

concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :-
	concatenate(L1, L2, L3).

?- writeln("Call lips/0 to run the standard naive reverse benchmark.").
