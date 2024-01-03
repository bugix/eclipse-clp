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
% Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: profile.pl,v 1.5 2017/08/19 12:53:36 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	profile.pl
 * DESCRIPTION: 	The profiling package for Prolog programs.
 * REVISION HISTORY:
 * AUTHOR	VERSION	 DATE
 * micha		12.11.92
 */

:- module(profile).

:- comment(categories, ["Development Tools"]).
:- comment(summary, "Profiling package for ECLiPSe programs").
:- comment(author, "Micha Meier and Stefano Novello, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2017/08/19 12:53:36 $").

:- comment(profile/1, [
    amode:(profile(+) is det),
    args:["Goal":"Callable term"],
    summary:"Execute Goal (once) and print the time spent in each predicate.",
    see_also:[get_flag / 2, profile / 2],
    desc:html("<P>\
   This predicate invokes the profiler with default options.  Equivalent to:
   <PRE>
       profile(Goal, []).
   </PRE>
   See profile/2 for details.
<P>
    ECLiPSe must have been started with the -P command line option
    (or equivalent) in order to support profiling.
</P>
"),
    eg:"
?- profile(length(_,100000000)).

		  PROFILING STATISTICS
		  --------------------
Goal:		  length(_80, 100000000)
Result:		  success
Sampling rate:	  every 0.01s process_cputime
Samples taken:	  182
Thread cputime:	  1.82s

Predicate	      Module	    %Time    Time   %Cum
---------------------------------------------------------
length            /2  sepia_kernel  79.7%    1.45s  79.7%
garbage_collect   /0  sepia_kernel  20.3%    0.37s 100.0%

Yes (1.86s cpu)
"
]).


:- comment(profile/2, [
    amode:(profile(+,++) is det),
    args:["Goal":"Callable term","Options":"List of options"],
    summary:"Execute Goal (once) and print the time spent in each predicate.",
    see_also:[get_flag / 2, profile / 1],
    desc:html("<P>
    Runs the given goal with the sampling profiler, which collects
    information about the currently executing predicate 100 times
    per second.  After the goal finishes, a result table is printed.
<P>
    ECLiPSe must have been started with the -P command line option
    (or equivalent) in order to support profiling.  It is, however, not
    necessary to compile the profiled code in a particular way; the profiler
    works independently of compiler optimizations and debug mode.
<P>
    Possible options:
    <DL>
    <DT><STRONG>keep_file</STRONG></DT><DD>
	don't destroy the samples file &lt;tmp_dir&gt;/eclipse.prof.&lt;pid&gt;
	that is used to collect profiling information during execution.
	Without this option, the file is automatically deleted.
    </DL>
<P>
    Notes:
    <UL>
    <LI>Garbage collection time is shown as predicate garbage_collect/0,
    even when garbage collection was automatically triggered.</LI>
    <LI>The time spent in simple predicates (i.e. those with call_type
    external, written in C) is counted towards their parent predicate.</LI>
    <LI>In cases where the predicate name is not available, e.g. local goals in
    a locked module, the information is grouped under a blank predicate
    name entry for the module.</LI>
    </UL>
    "),
    eg:"
?- profile(length(_,100000000), []).

		  PROFILING STATISTICS
		  --------------------
Goal:		  length(_80, 100000000)
Result:		  success
Sampling rate:	  every 0.01s process_cputime
Samples taken:	  182
Thread cputime:	  1.82s

Predicate	      Module	    %Time    Time   %Cum
---------------------------------------------------------
length            /2  sepia_kernel  79.7%    1.45s  79.7%
garbage_collect   /0  sepia_kernel  20.3%    0.37s 100.0%

Yes (1.86s cpu)
"]).


:- pragma(system).
:- export
	profile/1,
	profile/2.

:- import
	block/4,
	getw/2,
	prof_predicate_list/3,
	prof/3
    from sepia_kernel.

have_profiler(Out) :-
    ( get_flag(extension, profiler) -> true ;
	writeln(Out, "Profiler support not enabled, use -P command line option!"),
	fail
    ).

?- local initialization((
    % hack to suppress warning when it goes to a reference file in the tests
    ( get_stream_info(warning_output, device, file) -> W = null
    ; W = warning_output ),
    ( have_profiler(W) ->
	% Use SIGPROF timer, if supported
	( current_interrupt(_, prof) -> set_interrupt_handler(prof, internal/0)
	; true
	)
    ; true
    )
)).

:- tool(profile/1, profile_body/2).
:- tool(profile/2, profile_body/3).

profile_body(Goal, M) :-
    profile_body(Goal, [], M).

profile_body(Goal, Flags, M) :-
    ( have_profiler(error) -> true ; abort ),

    process_flags(Goal, Flags, 0, FlagBits),
    sprintf(GoalString, "%w", [Goal]),	% for later printing
    open_sample_file(File, Stream),
    Rate = 0.01,

    prof(Rate, FlagBits, Stream),	% start sampling
    cputime(T0),
    ( catch(Goal, Ball, true)@M ->
	( var(Ball) -> Result = success ; Result = throw(Ball) )
    ;
	Result = failure
    ),
    cputime(T1),
    prof(0.0, FlagBits, _),		% stop sampling
    Time is T1 - T0,
    close(Stream),

    build_preds(FlagBits, Preds),
    collect_times(Preds, Ticks, File),
    (FlagBits /\ 4 =:= 4 ->
	true
    ;
	delete(File)
    ),
    filter_used_preds(Preds, SortedPreds),
    prof_print(GoalString, SortedPreds, Ticks, Time, Rate, Result).


open_sample_file(File, Stream) :-
    get_flag(pid, Pid),
    get_flag(tmp_dir, Tmp),
    concat_atom([Tmp,'eclipse.prof.', Pid], File),
    open(File, write, Stream).


%
% Build the structure containing all compiled predicates
%
build_preds(Flags, PredsStructure) :-
    prof_predicate_list(Flags, Preds, Fixed),
    Fixed1 is Fixed+1,
    Preds1 = [pred(0, Fixed, ?, ?)|Preds],
    list_tree(Preds1, PredsStructure, Fixed1, N),
    (current_array(preds(_), _) ->
	erase_array(preds/1)
    ;
	true
    ),
    make_local_array(preds(N), integer).

%
% collect_times(+Preds, -Ticks, +File)
% Process the file with profiling output and record the ticks
% for each predicate
%
collect_times(Preds, Ticks, File) :-
    open(File, read, S),
    getw(S, Addr),
    loop_addr(Addr, S, Preds, 0, Ticks),
    close(S).

loop_addr(end_of_file, _, _, T, T).
loop_addr(Addr, S, Preds, T, Ticks) :-
    integer(Addr),
    T1 is T + 1,
    (search(Addr, Preds, pred(_, I, _, _)) ->
	true
    ;
	search(0, Preds, pred(_, I, _, _))	% add to bad samples
    ),
    incval(preds(I)),
    getw(S, NextAddr),
    loop_addr(NextAddr, S, Preds, T1, Ticks).

%
% Make a list with all executed predicates
%
filter_used_preds(Preds, Sorted) :-
    tree_list(Preds, Used),
    sort(1, >=, Used, Sorted).

prof_print(Goal, Preds, Ticks, Time, Rate, Result) :-
    ( current_interrupt(_, prof) -> Timer=process_cputime
    ; Timer=session_time
    ),
    printf('%n                  PROFILING STATISTICS%n', []),
    printf('                  %20c%n', [0'-]),
    printf('Goal:             %w%n', [Goal]),
    printf('Result:           %w%n', [Result]),
    printf('Sampling rate:    every %.2fs %s%n', [Rate,Timer]),
    printf('Samples taken:    %d%n', [Ticks]),
    printf('Thread cputime:   %.2fs%2n', [Time]),
    printf('Predicate             Module        %%Time    Time   %%Cum%n%57c%n', [0'-]),
    prof_print_preds(Preds, Ticks, Time, 0.0).

prof_print_preds([], _, _, _).
prof_print_preds([pred(Counter, P, M)|L], Ticks, Time, CumPerc0) :-
    Perc is Counter * 100.0/Ticks,
    STime is Counter * Time/Ticks,
    CumPerc is CumPerc0+Perc,
    (P = N/A ->
	printf('%-18.18s/%-2d %-12.12s%6.1f%%%8.2fs%6.1f%%%n',
		[N, A, M, Perc, STime, CumPerc])
    ;
	printf('%-18.18s    %-12.12s%6.1f%%%8.2fs%6.1f%%%n',
		[P, M, Perc, STime, CumPerc])
    ),
    prof_print_preds(L, Ticks, Time, CumPerc).

% Process the input flags
process_flags(_, [], F, F).
process_flags(G, [Flag|L], F, Flags) :-
    flag_value(G, Flag, V),
    F1 is F + V,
    process_flags(G, L, F1, Flags).

% These masks must correspond to those in emu_c_env.c
:- mode flag_value(?, ++, -).
flag_value(_, simple, 1) :- !.
flag_value(_, all, 2) :- !.
flag_value(_, keep_file, 4) :- !.
flag_value(G, F, _) :-
    error(6, profile(G, [F])).

%
% Tree manipulation predicates, taken from KCM
%
%%% author:     S. Novello
%%% date:       5/5/92
%%% modified by Micha, 11/11/92

% search(Key, Node, Contents)

:- mode search(++, ++, +).
search(Key,node(Ltree,EKey,_,EContents,Rtree),Contents) :-
	( Key < EKey ->
		search(Key,Ltree,Contents)
	;
		( tree_minkey(Rtree,MinKey),
		  Key >= MinKey ->
			search(Key,Rtree,Contents)
		;
			EContents = Contents
		)
	).

% tree_minkey(Node, MinKey)

:- mode tree_minkey(++, -).
tree_minkey(node(_,_,MinKey,_,_),MinKey).

%==============================================================================
% LIST/TREE
%==============================================================================
% list_tree(List, Tree)

list_tree(List,Tree, From, To) :- list_tree(List,null,Tree, From, To).

% list_tree(List, TreeIn, TreeOut, From, To)

list_tree([],T,T, N, N) :-!.
list_tree([Pred|T],TreeIn,TreeOut, From, To) :-
	Pred = pred(Key, I, _, _),
	ins(Key,Pred,TreeIn,Tree),
	(var(I) ->
	    I = From,
	    Next is From + 1,
	    list_tree(T,Tree,TreeOut, Next, To)
	;
	    list_tree(T,Tree,TreeOut, From, To)
	).

% ins(Key,Elem,OldTree,NewTree).
:- mode ins(++, ++, ++, -).

ins(Key,Elem,null,NewTree) :-
	!,
	NewTree = node(null,Key,Key,Elem,null).
ins(Key,Elem,node(Ltree,NKey,MinKey,NElem,Rtree), node(L,NKey,Min,NElem,R)) :-
	(Key >= NKey ->
		L=Ltree, Min=MinKey,
		ins(Key,Elem,Rtree,R)
	;
		Min is min(Key,MinKey),
		R=Rtree,
		ins(Key,Elem,Ltree,L)
	).

% member_tree(Key, Contents, Node)

member_tree(Key,Contents,node(L,K,_,C,R)):-member_tree(Key,Contents,L,K,C,R).

member_tree(Key,Contents,L,_,_,_) :- member_tree(Key,Contents,L).
member_tree(Key,Contents,_,Key,Contents,_).
member_tree(Key,Contents,_,_,_,R) :- member_tree(Key,Contents,R).

% tree_list(Tree, List)

tree_list(Tree,List):-tree_list(Tree,List,[]).

tree_list(null,X,Y):-!,X=Y.
tree_list(node(L,_,_,C,R),List,End) :-
	C = pred(_, I, P, M),
	getval(preds(I), Counter),
	(Counter > 0 ->
	    setval(preds(I), 0),
	    tree_list(L,List,[pred(Counter, P, M)|Rest])
	;
	    tree_list(L,List,Rest)
	),
	tree_list(R,Rest,End).

