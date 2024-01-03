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
% The Original Code is	The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is	 Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------

% System:	ECLiPSe Constraint Logic Programming System
%
% Component:	Implementation of do-loop construct
%		Part of module(sepia_kernel)
%
% Description:	This has been factored out of kernel.pl
%

%----------------------------------------------------------------
% Loop constructs
%----------------------------------------------------------------

:- export (do)/2.
:- export (do)/3.
:- export t_do/5.
:- export foreachelem_next/7.
:- export foreachelem_next/8.
:- export multifor_next/7.
:- export multifor_init/8.
:- tool((do)/2, (do)/3).
:- inline((do)/2, t_do/5).
:- set_flag(do/3, trace_meta, on).

%----------------------------------------------------------------------
% Definition for metacall
%----------------------------------------------------------------------

do(Specs, LoopBody, M) :-
	get_specs(Specs, Firsts, Lasts, PreGoals, RecHead, AuxGoals, RecCall, _Locals, _Name, M),
	!,
	( AuxGoals = true -> BodyGoals = LoopBody
	; BodyGoals = (AuxGoals,LoopBody) ),
	call(PreGoals)@M,
	copy_term(Lasts, LastsCopy, _),	% will only be used once!
	forallc(Firsts, body(RecHead,BodyGoals,RecCall), LastsCopy, M).
do(Specs, LoopBody, M) :-
	error(123, do(Specs, LoopBody), M).

    forallc(Args, _BodyTemplate, Args, _M) :- true, !.
    forallc(Args, BodyTemplate, LastsCopy, M) :-
	copy_term(BodyTemplate, Copy, _),
	body(Args, Goal, RecArgs) = Copy,
	call(Goal)@M,
	forallc(RecArgs, BodyTemplate, LastsCopy, M).


%----------------------------------------------------------------------
% Compilation
%----------------------------------------------------------------------

/**** REMEMBER TO UPDATE annotated_term used in raw form by expand_macros
 **** and friends when changing the definition here
 ****/
:- export struct(annotated_term(
	term,		% var, atomic or compound
	type,		% atom
        file,           % atom
        line,           % integer
        from,		% integer
	to		% integer
	% may be extended in future
    )).
	

t_do((Specs do LoopBody), NewGoal, AnnDoLoop, AnnNewGoal, M) :-
	annotated_arg(2, AnnDoLoop, AnnLoopBody),
        get_specs(Specs, Firsts, Lasts, PreGoals, RecHeadArgs, AuxGoals, RecCallArgs, LocalVars, Name, M),
	!,
	% expand body recursively
        tr_goals_annotated(LoopBody, AnnLoopBody, LoopBody1, AnnLoopBody1, M),
%	printf("Local vars: %w / %vw%n", [LocalVars, LocalVars]),
%	printf("Loop body: %Vw%n", [LoopBody1]),
        check_singletons(LoopBody1, LocalVars, AnnDoLoop),
	length(Lasts, Arity),
        aux_pred_name(M, Arity, Name),
	FirstCall =.. [Name|Firsts],		% make replacement goal
        term_to_annotated(FirstCall, AnnDoLoop, AnnFirstCall),
        term_to_annotated(PreGoals, AnnDoLoop, AnnPreGoals),
	flatten_and_clean(PreGoals, FirstCall, AnnPreGoals, AnnFirstCall, 
                          NewGoal, AnnNewGoal),
	BaseHead =.. [Name|Lasts],		% make auxiliary predicate
	RecHead =.. [Name|RecHeadArgs],
	RecCall =.. [Name|RecCallArgs],
        term_to_annotated(AuxGoals, AnnDoLoop, AnnAuxGoals),
        term_to_annotated(RecCall, AnnDoLoop, AnnRecCall),
        term_to_annotated(RecHead, AnnDoLoop, AnnRecHead),
        tr_goals_annotated(AuxGoals, AnnAuxGoals, AuxGoals1, AnnAuxGoals1, M),
        annotated_create((AnnAuxGoals1,AnnLoopBody1), AnnDoLoop, AnnRecCall0),
        flatten_and_clean((AuxGoals1,LoopBody1), RecCall, AnnRecCall0,
                          AnnRecCall, BodyGoals, AnnBodyGoals), 
        BHClause = (BaseHead :- true, !),
        RHClause = (RecHead :- BodyGoals),
        Directive = (?- set_flag(Name/Arity, auxiliary, on)),
	Code = [
	    BHClause,
	    RHClause,
            Directive
	],
        
        (nonvar(AnnDoLoop) ->
	    % Use anonymous variables in the base clause to avoid singleton warnings
            term_to_annotated(BHClause, AnnDoLoop, AnnBHClause, false),
            term_to_annotated(Directive, AnnDoLoop, AnnDirective),
            annotated_create((AnnRecHead :- AnnBodyGoals), AnnDoLoop, AnnRHClause),
            /* create a annotated list of Code  [
                AnnBHClause,
                AnnRHClause,
                AnnDirective
            ], */
            annotated_create([AnnBHClause|AnnCode1], AnnDoLoop, AnnCode),
            annotated_create([AnnRHClause|AnnCode2], AnnDoLoop, AnnCode1),
            annotated_create([AnnDirective|AnnCode3], AnnDoLoop, AnnCode2),
            annotated_create([], AnnDoLoop, AnnCode3)
        ;
            true
        ),
%	printf("Creating auxiliary predicate %w\n", Name/Arity),
%	write_clauses(Code),
%	writeclause(?- NewGoal),
	copy_term((Code,AnnCode), (CodeCopy,AnnCodeCopy), _),% strip attributes
        nested_compile_term_annotated(CodeCopy,AnnCodeCopy)@M.
t_do(Illformed, _, _, _, M) :-
	error(123, Illformed, M).

    aux_pred_name(_Module, _Arity, Name) :- nonvar(Name).
    aux_pred_name(Module, Arity, Name) :- var(Name),
	store_inc(name_ctr, Module),
	store_get(name_ctr, Module, I),
	concat_atom([do__,I], Name0),
	( nested_compile_load_flag(all), is_predicate(Name0/Arity)@Module ->
	    % Avoid name clashes (should only happen when a .eco file
	    % has been loaded into this module earlier)
	    aux_pred_name(Module, Arity, Name)
	;
	    % No name clash: ok.
	    % Name clash, but not loading: use same name to get reproducible
	    % .eco files when using compile(..., [output:eco,load:none])
	    Name = Name0
	).


    write_clauses([]).
    write_clauses([C|Cs]) :-
	writeclause(C),
	write_clauses(Cs).

    :- mode flatten_and_clean(?, ?, ?, ?, -, -).
    flatten_and_clean(G, Gs, AG, AGs, (G,Gs), AFG) :- var(G), !,
	annotated_create((AG,AGs), AG, AFG).
    flatten_and_clean(true, Gs, _AG, AGs, Gs, AGs) :- !.
    flatten_and_clean((G1,G2), Gs0, AG, AGs0, Gs, AGs) :-
        !,
	annotated_match(AG, (AG1,AG2)),
	flatten_and_clean(G1, Gs1, AG1, AGs1, Gs, AGs),
	flatten_and_clean(G2, Gs0, AG2, AGs0, Gs1, AGs1).
    flatten_and_clean(G, Gs, AG, AGs, (G,Gs), AFG) :-
	annotated_create((AG,AGs), AG, AFG).

reset_name_ctr(Module) :-
	store_set(name_ctr, Module, 0).

%----------------------------------------------------------------------
% get_spec defines the meaning of each specifier
%----------------------------------------------------------------------

:- mode get_specs(?,-,-,-,-,-,-,-,-,+).
get_specs(Specs, Firsts, Lasts, Pregoals, RecHead, AuxGoals, RecCall, Locals, Name, M) :-
	nonvar(Specs),
	get_specs(Specs, Firsts, [], Lasts, [], Pregoals, true, RecHead, [], AuxGoals, true, RecCall, [], Locals, [], Name, M).

:- mode get_specs(+,-,+,-,+,-,+,-,+,-,+,-,+,-,+,?,+).
get_specs((Specs1,Specs2), Firsts, Firsts0, Lasts, Lasts0, Pregoals, Pregoals0, RecHead, RecHead0, AuxGoals, AuxGoals0, RecCall, RecCall0, Locals, Locals0, Name, M) :- !,
	get_specs(Specs1, Firsts, Firsts1, Lasts, Lasts1, Pregoals, Pregoals1, RecHead, RecHead1, AuxGoals, AuxGoals1, RecCall, RecCall1, Locals, Locals1, Name, M),
	get_specs(Specs2, Firsts1, Firsts0, Lasts1, Lasts0, Pregoals1, Pregoals0, RecHead1, RecHead0, AuxGoals1, AuxGoals0, RecCall1, RecCall0, Locals1, Locals0, Name, M).
get_specs(Spec, Firsts, Firsts0, Lasts, Lasts0, Pregoals, Pregoals0, RecHead, RecHead0, AuxGoals, AuxGoals0, RecCall, RecCall0, Locals, Locals0, Name, M) :-
        get_spec(Spec, Firsts, Firsts0, Lasts, Lasts0, Pregoals, Pregoals0, RecHead, RecHead0, AuxGoals, AuxGoals0, RecCall, RecCall0, Locals, Locals0, Name, M).

:- mode get_spec(+,-,+,-,+,-,+,-,+,-,+,-,+,-,+,?,+).
get_spec(loop_name(Name),
	Firsts, Firsts,
	Lasts, Lasts,
	Pregoals, Pregoals,
	RecHeads, RecHeads,
	Goals, Goals,
	RecCalls, RecCalls,
	Locals, Locals,
	Name, _Module
    ) :- atom(Name), !.
get_spec(foreach(E,List),
	[List|Firsts], Firsts,
	[[]|Lasts], Lasts,
	Pregoals, Pregoals,
	[[E|T]|RecHeads], RecHeads,
	Goals, Goals,
	[T|RecCalls], RecCalls,
	[E|Locals], Locals,
	_Name, _Module
    ) :- !.
get_spec(foreacharg(A,Struct),
	[Struct,1,N1|Firsts], Firsts,
	[_,I0,I0|Lasts], Lasts,
	(arity(Struct,N),+(N,1,N1),Pregoals), Pregoals,
	[S,I0,I2|RecHeads], RecHeads,
	(+(I0,1,I1),arg(I0,S,A),Goals), Goals,
	[S,I1,I2|RecCalls], RecCalls,
	[A|Locals], Locals,
	_Name, _Module
    ) :- !.
get_spec(foreacharg(A,Struct,I),
	[Struct,1,N1|Firsts], Firsts,
	[_,I,I|Lasts], Lasts,
	(arity(Struct,N),+(N,1,N1),Pregoals), Pregoals,
	[S,I,I2|RecHeads], RecHeads,
	(+(I,1,I1),arg(I,S,A),Goals), Goals,
	[S,I1,I2|RecCalls], RecCalls,
	[A,I|Locals], Locals,
	_Name, _Module
    ) :- !.
get_spec(foreachelem(Elem,Array),
	[1,Array,[]|Firsts], Firsts,
	[_,[],_|Lasts], Lasts,
	(is_array(Array),Pregoals), Pregoals,
	[I,Arr,Stack|RecHeads], RecHeads,
	(sepia_kernel:foreachelem_next(I,Arr,Stack,I1,Arr1,Stack1,Elem),Goals), Goals,
	[I1,Arr1,Stack1|RecCalls], RecCalls,
	[Elem|Locals], Locals,
	_Name, _Module
    ) :- !.
get_spec(foreachelem(Elem,Array,Idx),
	[1,Array,[]|Firsts], Firsts,
	[_,[],_|Lasts], Lasts,
	(is_array(Array),Pregoals), Pregoals,
	[I,Arr,Stack|RecHeads], RecHeads,
	(sepia_kernel:foreachelem_next(I,Arr,Stack,I1,Arr1,Stack1,Elem,Idx),Goals), Goals,
	[I1,Arr1,Stack1|RecCalls], RecCalls,
	[Elem,Idx|Locals], Locals,
	_Name, _Module
    ) :- !.
get_spec(foreachindex(Idx,Array),
	[1,Array,[]|Firsts], Firsts,
	[_,[],_|Lasts], Lasts,
	(is_array(Array),Pregoals), Pregoals,
	[I,Arr,Stack|RecHeads], RecHeads,
	(sepia_kernel:foreachelem_next(I,Arr,Stack,I1,Arr1,Stack1,_,Idx),Goals), Goals,
	[I1,Arr1,Stack1|RecCalls], RecCalls,
	[Idx|Locals], Locals,
	_Name, _Module
    ) :- !.
get_spec(fromto(From,I0,I1,To),		% accumulator pair needed
	[From,To|Firsts], Firsts,
	[L0,L0|Lasts], Lasts,
	Pregoals, Pregoals,
	[I0,L1|RecHeads], RecHeads,
	Goals, Goals,
	[I1,L1|RecCalls], RecCalls,
	[I0,I1|Locals], Locals,
	_Name, _Module
    ) :- nonground(To), !.
get_spec(fromto(From,I0,I1,To),		% ground(To), only one arg
	[From|Firsts], Firsts,
	[To|Lasts], Lasts,
	Pregoals, Pregoals,
	[I0|RecHeads], RecHeads,
	Goals, Goals,
	[I1|RecCalls], RecCalls,
	[I0,I1|Locals], Locals,
	_Name, _Module
    ) :- !.
get_spec(count(I,FromExpr,To),		% accumulator pair needed
	[From,To|Firsts], Firsts,
	[L0,L0|Lasts], Lasts,
	Pregoals, Pregoals0,
	[I0,L1|RecHeads], RecHeads,
	(+(I0,1,I),Goals), Goals,
	[I,L1|RecCalls], RecCalls,
	[I|Locals], Locals,
	_Name, _Module
    ) :- var(I), nonground(To), !,
	( number(FromExpr) -> Pregoals = Pregoals0, From is FromExpr-1
	; Pregoals = (From is FromExpr-1, Pregoals0) ).
get_spec(count(I,FromExpr,To),
	[From|Firsts], Firsts,
	[To|Lasts], Lasts,
	Pregoals, Pregoals0,
	[I0|RecHeads], RecHeads,
	(+(I0,1,I),Goals), Goals,
	[I|RecCalls], RecCalls,
	[I|Locals], Locals,
	_Name, _Module
    ) :- var(I), integer(To), !,
	( number(FromExpr) -> Pregoals = Pregoals0, From is FromExpr-1
	; Pregoals = (From is FromExpr-1, Pregoals0) ).
get_spec(for(I,From,To),
	Firsts, Firsts0, Lasts, Lasts0, Pregoals, Pregoals0, RecHead, RecHead0,
	AuxGoals, AuxGoals0, RecCall, RecCall0, Locals, Locals0, Name, Module
    ) :- !,
	get_spec(for(I,From,To,1), Firsts, Firsts0, Lasts, Lasts0, Pregoals, Pregoals0,
	    RecHead, RecHead0, AuxGoals, AuxGoals0, RecCall, RecCall0, Locals, Locals0, Name, Module).
get_spec(for(I,FromExpr,To,Step),	% Special cases, only 1 arg needed
	[From|Firsts], Firsts,
	[Stop|Lasts], Lasts,
	Pregoals, Pregoals0,
	[I|RecHeads], RecHeads,
	(+(I,Step,I1),Goals), Goals,
	[I1|RecCalls], RecCalls,
	[I|Locals], Locals,
	_Name, _Module
    ) :- var(I),
	integer(Step),
	number(To),
	( number(FromExpr) ->
	    From = FromExpr,
	    Pregoals = Pregoals0,
	    compute_stop(From,To,Step,Stop)	% compute Stop at compile time
	; Step == 1 ->
	    Stop is To+1,
	    Pregoals = (From is min(FromExpr,Stop), Pregoals0)
	; Step == -1 ->
	    Stop is To-1,
	    Pregoals = (From is max(FromExpr,Stop), Pregoals0)
	;
	    fail			% general case
	),
	!.
get_spec(for(I,FromExpr,ToExpr,Step),	% Step constant: 2 args needed
	[From,Stop|Firsts], Firsts,
	[L0,L0|Lasts], Lasts,
	Pregoals, Pregoals0,
	[I,L1|RecHeads], RecHeads,
	(+(I,Step,I1),Goals), Goals,
	[I1,L1|RecCalls], RecCalls,
	[I|Locals], Locals,
	_Name, _Module
    ) :- var(I), integer(Step), !,
	% We require for FromExpr and ToExpr that they are only bound to
	% numbers at runtime. If not, use:  for(I,eval(F),eval(T)) do ...
	% We assume that ToExpr is always embedded in an expression
	% within StopGoal (otherwise explicit To is ToExpr needed!)
	compute_stop(From,ToExpr,Step,_,Stop,StopGoal),
	Pregoals1 = (StopGoal,Pregoals0),
	( number(FromExpr) -> Pregoals = Pregoals1, From = FromExpr
	; var(FromExpr) -> Pregoals = Pregoals1, From = FromExpr
	; Pregoals = (From is FromExpr, Pregoals1) ).
get_spec(for(I,FromExpr,ToExpr,StepExpr),	% Step variable: 3 args needed
	[From,Stop,Step|Firsts], Firsts,
	[L0,L0,_|Lasts], Lasts,
	Pregoals, Pregoals0,
	[I,L1,Step|RecHeads], RecHeads,
	(+(I,Step,I1),Goals), Goals,
	[I1,L1,Step|RecCalls], RecCalls,
	[I|Locals], Locals,
	_Name, _Module
    ) :- var(I),
	compute_stop(From,ToExpr,StepExpr,Step,Stop,StopGoal),
	!,
	Pregoals1 = (StopGoal,Pregoals0),
	( number(FromExpr) -> Pregoals = Pregoals1, From = FromExpr
	; var(FromExpr) -> Pregoals = Pregoals1, From = FromExpr
	; Pregoals = (From is FromExpr, Pregoals1) ).
get_spec(multifor(Idx,From,To),
	Firsts, Firsts0, Lasts, Lasts0, Pregoals, Pregoals0,
	RecHead, RecHead0, AuxGoals, AuxGoals0, RecCall, RecCall0, Locals, Locals0, Name, Module
    ) :- !,
	get_spec(multifor(Idx,From,To,1), Firsts, Firsts0, Lasts, Lasts0, Pregoals, Pregoals0,
	    RecHead, RecHead0, AuxGoals, AuxGoals0, RecCall, RecCall0, Locals, Locals0, Name, Module).
get_spec(multifor(Idx,From,To,Step),
	[RevFrom,RevTo,RevStep,RevStop|Firsts], Firsts,
	[RevStop,_,_,RevStop|Lasts], Lasts,
	Pregoals, Pregoals0,
	[RevIdx,RevTo,RevStep,RevStop|RecHeads], RecHeads,
	Goals, Goals0,
	[RevIdx1,RevTo,RevStep,RevStop|RecCalls], RecCalls,
	[Idx|Locals], Locals,
	_Name, _Module
    ) :-
	!,
	( var(Idx) ->
	    true
	;
	    list_length(Idx, N)
	),
	Pregoals = (
		% Check that the specifiers are valid.
		sepia_kernel:multifor_init(N, From, To, Step, RevFrom, RevTo, RevStep, RevStop),
		Pregoals0
	    ),
	Goals = (
		sepia_kernel:multifor_next(RevIdx, RevStop, RevTo, RevStep, RevIdx1, [], Idx),
		Goals0
	    ).
get_spec('*'(Specs1, Specs2),
	Firsts, FirstsTail,
	Lasts, LastsTail,
	Pregoals, PregoalsTail,
	RecHeads, RecHeadsTail,
	Goals, GoalsTail,
	RecCalls, RecCallsTail,
	Locals, LocalsTail,
	_Name, Module
    ) :-
	!,
	get_specs(Specs1,
		Firsts1, [],
		Lasts1, [],
		Pregoals, Pregoals2,
		RecHeads1, [],
		Goals1, Goals2,
		RecCalls1, [],
		Locals, Locals2,
		_Name1, Module),
	get_specs(Specs2,
		Firsts2, [],
		Lasts2, [],
		Pregoals2, PregoalsTail1,
		RecHeads2, RecHeadsTail,
		Goals2, GoalsTail2,
		RecCalls2, [],
		Locals2, LocalsTail,
		_Name2, Module),
	length(Firsts1, N1),
	length(Firsts2, N2),
	% Firsts: Firsts1 | Firsts2 | Firsts2
	length(DummyFirsts1, N1),
	append(Firsts2, FirstsTail, FirstsTail2),
	append(Firsts2, FirstsTail2, FirstsTail1),
	append(DummyFirsts1, FirstsTail1, Firsts),
	% Lasts: Lasts1 | _ | Firsts2
	length(DummyLasts, N2),
	append(Firsts2, LastsTail, LastsTail2),
	append(DummyLasts, LastsTail2, LastsTail1),
	append(Lasts1, LastsTail1, Lasts),
	% Pregoals: Pregoals1, Pregoals2, Spec2 short-circuit check
	PregoalsTail1 = (
		( Firsts2 = Lasts2 ->
		    DummyFirsts1 = Lasts1
		;
		    DummyFirsts1 = Firsts1
		),
		PregoalsTail
	    ),
	% RecHeads: RecHeads11 | Resets2 | RecHeads2
	length(Resets2, N2),
	length(RecHeads11, N1),
	append(Resets2, RecHeads2, RecHeadsTail1),
	append(RecHeads11, RecHeadsTail1, RecHeads),
	% Goals: ...
	length(RecCalls11, N1),
	length(RecCalls21, N2),
	% Lasts2 usually only in base head; need to rename...
	copy_term(Lasts2, Lasts21),
	Goals = ( RecHeads11 = RecHeads1, Goals1 ),
	GoalsTail2 = (
		( RecCalls2 = Lasts21 ->
		    RecCalls11 = RecCalls1,
		    RecCalls21 = Resets2
		;
		    RecCalls11 = RecHeads11,
		    RecCalls21 = RecCalls2
		),
		GoalsTail
	    ),
	% RecCalls: RecCalls11 | Resets2 | RecCalls21
	append(RecCalls21, RecCallsTail, RecCallsTail2),
	append(Resets2, RecCallsTail2, RecCallsTail1),
	append(RecCalls11, RecCallsTail1, RecCalls),
	% Locals: Locals1 | Locals2
	true.
get_spec('>>'(Specs1, Specs2),
	Firsts, FirstsTail,
	Lasts, LastsTail,
	Pregoals, PregoalsTail,
	RecHeads, RecHeadsTail,
	Goals, GoalsTail,
	RecCalls, RecCallsTail,
	Locals, LocalsTail,
	_Name, Module
    ) :-
	!,
	get_specs(Specs1,
		Firsts1, FirstsTail1,
		Lasts1, [],
		Pregoals, PregoalsTail1,
		RecHeads1, RecHeadsTail1,
		Goals1, true,
		RecCalls1, [],
		Locals1, [],
		_Name1, Module),
	get_specs(Specs2,
		Firsts2, [],
		Lasts2, [],
		Pregoals2, true,
		RecHeads2, RecHeadsTail,
		Goals, GoalsTail2,
		RecCalls2, [],
		Locals, LocalsTail,
		_Name2, Module),
	length(RecCalls1, N1),
	length(Firsts2, N2),
	Arity is 2*N1 + N2,

	% Set up the auxiliary predicate for iterating Spec1
	aux_pred_name(Module, Arity, NextPredName),
	append(Lasts1, Lasts2, LastsTail1),
	append(Lasts1, LastsTail1, Lasts11),
	NextBaseHead =.. [NextPredName | Lasts11],
	length(RecCalls11, N1),
	length(Firsts21, N2),
	append(RecCalls11, Firsts21, RecHeadsTail1),
	NextRecHead =.. [NextPredName | RecHeads1],
	append(RecCalls1, RecHeadsTail1, NextRecCalls1),
	NextRecCall =.. [NextPredName | NextRecCalls1],
	% Don't expand goals if goal_expansion is off
	global_flags(0,0,F),
	( F /\ 16'00000800 =:= 0 ->
	    Goals11 = Goals1,
	    Pregoals21 = Pregoals2
	;
	    tr_goals(Goals1, Goals11, Module),
	    tr_goals(Pregoals2, Pregoals21, Module)
	),
	check_singletons(Firsts2 - Pregoals2, Locals1, _),
	NextExtraGoal =
		( Firsts2 = Lasts2 ->
		    NextRecCall
		;
		    RecCalls11 = RecCalls1,
		    Firsts21 = Firsts2
		),
	flatten_and_clean((Goals11, Pregoals21), NextExtraGoal, _, _, NextGoals, _),
	NextCode = [
	    (NextBaseHead :- !, true),
	    (NextRecHead :- NextGoals),
	    (?- set_flag(NextPredName/Arity, auxiliary, on))
	],
	%printf("Creating auxiliary predicate %w\n", NextPredName/Arity),
	%write_clauses(NextCode),
	copy_term(NextCode, NextCodeCopy, _),	% strip attributes
	nested_compile_term(NextCodeCopy)@Module,

	% Use a different copy of Firsts2 in PreGoals and Firsts from what
	% is used in RecHead and AuxGoals (for when goal expansion not
	% used).
	copy_term(Firsts2, Firsts22),
	% Firsts: Firsts11 | Firsts22
	length(Firsts11, N1),
	append(Firsts22, FirstsTail, FirstsTail2),
	append(Firsts11, FirstsTail2, Firsts),
	% Lasts: _ | Lasts2
	length(DummyLasts1, N1),
	append(Lasts2, LastsTail, LastsTail2),
	append(DummyLasts1, LastsTail2, Lasts),
	% Pregoals: Pregoals1, set up first iteration
	append(Firsts11, Firsts22, FirstsTail1),
	NextPreCall =.. [NextPredName | Firsts1],
	PregoalsTail1 = (NextPreCall, PregoalsTail),
	% RecHeads: RecHeads11 | RecHeads2
	length(RecHeads11, N1),
	append(RecHeads11, RecHeads2, RecHeads),
	% Goals: ...
	length(RecCalls21, N2),
	append(RecCalls11, RecCalls21, RecHeadsTail2),
	append(RecHeads11, RecHeadsTail2, NextGoalCalls1),
	NextGoalCall =.. [NextPredName | NextGoalCalls1],
	% Lasts2 usually only in base head; need to rename
	copy_term(Lasts2, Lasts21),
	GoalsTail2 = (
		(
		    RecCalls2 = Lasts21
		->
		    NextGoalCall
		;
		    RecCalls11 = RecHeads11,
		    RecCalls21 = RecCalls2
		),
		GoalsTail
	    ),
	% RecCalls: RecCalls11 | RecCalls21
	append(RecCalls21, RecCallsTail, RecCallsTail1),
	append(RecCalls11, RecCallsTail1, RecCalls),
	% Locals: Locals2
	true.
get_spec(Param,
	GlobsFirsts, Firsts,
	GlobsLasts, Lasts,
	Pregoals, Pregoals,
	GlobsRecHeads, RecHeads,
	Goals, Goals,
	GlobsRecCalls, RecCalls,
	GlobsLocals, Locals,
	_Name, _Module
    ) :- Param =.. [param|Globs], Globs = [_|_], !,
	append(Globs, Firsts, GlobsFirsts),
	append(Globs, Lasts, GlobsLasts),
	append(Globs, Locals, GlobsLocals),
	append(Globs, RecHeads, GlobsRecHeads),
	append(Globs, RecCalls, GlobsRecCalls).

%:- mode compute_stop(?,?,?,-,-,-). % commented out because of compiler bug
compute_stop(From, To, Step, Step, Stop, Goal) :- var(Step), !,
	Goal = (Dist is max(sgn(Step)*(To-From+Step),0),
		Stop is From + sgn(Step)*(Dist - (Dist rem Step))).
compute_stop(From, To, 1, 1, Stop, Goal) :- !,
	Goal = (Stop is max(From, To+1)).
compute_stop(From, To, -1, -1, Stop, Goal) :- !,
	Goal = (Stop is min(From,To-1)).
compute_stop(From, To, Step, Step, Stop, Goal) :- integer(Step), Step > 1, !,
	Goal = (Dist is max(To-From+Step,0),
		Stop is From + Dist - (Dist rem Step)).
compute_stop(From, To, Step, Step, Stop, Goal) :- integer(Step), Step < 1, !,
	Goal = (Dist is max(From-To-Step,0),
		Stop is From - Dist + (Dist rem Step)).
compute_stop(From, To, StepExpr, Step, Stop, Goal) :-
	Goal = (Step is StepExpr,
		Dist is max(sgn(Step)*(To-From+Step),0),
		Stop is From + sgn(Step)*(Dist - (Dist rem Step))).


% Make a compute_stop/4 predicate, which computes the stop value on the
% spot in the general case, by using the code generated by compute_stop/6.

:- inline(compute_stop/4, tr_compute_stop/2).
tr_compute_stop(compute_stop(From, To, Step, Stop), Goal) :-
	compute_stop(From, To, Step, _, Stop, Goal0),
	expand_goal(Goal0, Goal).

:- pragma(expand).	% required for the following clause!
compute_stop(From, To, Step, Stop) :-
	compute_stop(From, To, Step, Stop).


%
% For the foreachelem specifiers, the iteration is controlled by three
% arguments:  The currently considered sub-array and its current index,
% and a stack of the pieces of the surrounding arrays (that are yet to
% be processed) in reverse order (i.e. outermost at the bottom).
%
% This scheme returns the elements in the correct order and gracefully
% handles "arrays" with "unorthodox" shape (e.g. different rows containing
% different numbers of columns, different parts of the "array" having
% different numbers of dimensions, etc.).
%
% The term [] is treated as an ordinary array element when encountered
% inside the arrays (consistent with dim/2), since empty dimensions are 
% pretty useless in multi-dimensional arrays.  Only a top-level [] is
% treated as the empty array.
%

% foreachelem_next(+I,+SubArr,+Stack, -I1,-SubArr,-Stack1, -Elem[,-Index])
% I and Arr refer to the current sub-array being traversed.
% ArrsIs is a stack of "continuations", i.e. array+index to go to
% once the current sub-array is exhausted.

foreachelem_next(I, Arr, Stack, I1, Arr1, Stack1, Elem) :-
	arg(I, Arr, ArrOrElem),
	( compound(ArrOrElem), functor(ArrOrElem, [], _) ->
	    % nested array
	    ( arity(Arr, I) ->
		foreachelem_next(1, ArrOrElem, Stack, I1, Arr1, Stack1, Elem)
	    ;
		I2 is I+1,
		foreachelem_next(1, ArrOrElem, [[I2|Arr]|Stack], I1, Arr1, Stack1, Elem)
	    )
	;
	    ( arity(Arr, I) ->
		( Stack = [[I1|Arr1]|Stack1]	% pop, one level up
		; Stack == [], Arr1 = []	% very last element
		)
	    ;
		I1 is I+1, Arr1 = Arr, Stack1 = Stack
	    ),
	    Elem = ArrOrElem
	).

% This variant returns the element index as well
% It doesn't do TRO on the stack in order to be able to construct the index.
foreachelem_next(I, Arr, Stack, I1, Arr1, Stack1, Elem, Index) :-
	arg(I, Arr, ArrOrElem),
	( compound(ArrOrElem), functor(ArrOrElem, [], _) ->	% nested array
	    I2 is I+1,
	    foreachelem_next(1, ArrOrElem, [[I2|Arr]|Stack], I1, Arr1, Stack1, Elem, Index)
	;
	    ( arity(Arr, I) ->			% last in this leaf array
		pop(Stack, Stack1, I1, Arr1)
	    ;
		I1 is I+1, Arr1 = Arr, Stack1 = Stack
	    ),
	    Elem = ArrOrElem,
	    this_index(Stack, Index, [I])
	).

    pop([], [], _, []).
    pop([[I0|Arr0]|Stack1], Stack, I, Arr) :-
    	( I0 > arity(Arr0) ->
	    pop(Stack1, Stack, I, Arr)
	;
	    I=I0, Arr=Arr0, Stack=Stack1
	).

    this_index([], Index, Index).
    this_index([[NextI|_]|Stack], Is, Is0) :-
	I is NextI-1,
	this_index(Stack, Is, [I|Is0]).


% 
% Auxiliaries for the multifor-specifier
% 

multifor_init(N, From, To, Step, RevFrom, RevTo, RevStep, RevStop) :-
	( validate_multifor_args(N, From, To, Step, From1, To1, Step1) ->
	    compute_multifor_stop_list(From1, To1, Step1, RevFrom, RevTo, RevStep, RevStop)
	;
	    length(Idx, N),
	    error(123, multifor(Idx, From, To, Step))
	).


    % Checks the iteration specifier arguments for multifor, and expands
    % any shorthand integer specifiers into corresponding lists of the
    % appropriate length.  Fails if anything is wrong.
validate_multifor_args(N, FromList0, ToList0, StepList0,
		FromList, ToList, StepList) :-
	% First check the inputs are valid, and try to determine the number
	% of iterators.
	( integer(FromList0) ->
	    FromList1 = FromList0
	; is_list(FromList0) ->
	    is_integer_expr_list_with_length(FromList0, FromList1, 0, N)
	;
	    nonvar(FromList0),
	    FromList1 is FromList0,
	    integer(FromList1)
	),
	( integer(ToList0) ->
	    ToList1 = ToList0
	; is_list(ToList0) ->
	    is_integer_expr_list_with_length(ToList0, ToList1, 0, N)
	;
	    nonvar(ToList0),
	    ToList1 is ToList0,
	    integer(ToList1)
	),
	( integer(StepList0) ->
	    StepList1 = StepList0,
	    StepList0 =\= 0
	; is_list(StepList0) ->
	    is_nonzero_integer_expr_list_with_length(StepList0, StepList1, 0, N)
	;
	    nonvar(StepList0),
	    StepList1 is StepList0,
	    integer(StepList1)
	),

	% Fail if we still don't know how many iterators we have.
	nonvar(N),

	% Must have at least one iterator.
	N > 0,

	( integer(FromList1) ->
	    dupl(FromList1, N, FromList)
	;
	    FromList = FromList1
	),
	( integer(ToList1) ->
	    dupl(ToList1, N, ToList)
	;
	    ToList = ToList1
	),
	( integer(StepList1) ->
	    dupl(StepList1, N, StepList)
	;
	    StepList = StepList1
	).

is_integer_expr_list_with_length([], Xs, N, Length) :- -?->
	Xs = [],
	Length = N.
is_integer_expr_list_with_length([X0 | Xs0], Xs, N, Length) :- -?->
	Xs = [X1 | Xs1],
	( integer(X0) ->
	    X1 = X0
	;
	    nonvar(X0),
	    X1 is X0,
	    integer(X1)
	),
	N1 is N + 1,
	is_integer_expr_list_with_length(Xs0, Xs1, N1, Length).

is_nonzero_integer_expr_list_with_length([], Xs, N, Length) :- -?->
	Xs = [],
	Length = N.
is_nonzero_integer_expr_list_with_length([X0 | Xs0], Xs, N, Length) :- -?->
	Xs = [X1 | Xs1],
	( integer(X0) ->
	    X1 = X0
	;
	    nonvar(X0),
	    X1 is X0,
	    integer(X1)
	),
	X1 =\= 0,
	N1 is N + 1,
	is_nonzero_integer_expr_list_with_length(Xs0, Xs1, N1, Length).

    % Version of the length/2 predicate which only measures the length of an
    % existing list: it will not construct anything, and will fail if the
    % list is not of fixed length.
list_length(Xs, N) :-
	list_length(Xs, 0, N).

list_length([], N0, N) :- -?->
	N = N0.
list_length([_ | Xs], N0, N) :- -?->
	N1 is N0 + 1,
	list_length(Xs, N1, N).

    % Create a list by duplicating the given element the given number of
    % times.
dupl(X, N, List) :-
	( N =< 0 ->
	    List = []
	;
	    List = [X | List1],
	    N1 is N - 1,
	    dupl(X, N1, List1)
	).


    % compute_multifor_stop_list(FromList, ToList, StepList,
    %		RevFromList, RevToList, RevStepList, RevStopList)
    %	Computes the Stop list for the multifor iterator.
    %	Given lists for From, To and Step, create reversed lists for From,
    %	To, Step and Stop.  Note that the To values in the reversed list are
    %	adjusted based on the corresponding From and Step values, a la
    %	compute_stop.  The Stop values for the list as a whole are the Stop
    %	value for the first element and the From values for the rest of the
    %	elements.  This corresponds to a value list one more than the
    %	largest value list we want, which will be reached if we allow the
    %	first value to be incremented beyond the corresponding To value.  We
    %	achieve this by dropping the first element of the To list (the last
    %	one when reversed), and multifor_next/7 will do what we
    %	want.  Note that this also means that multifor_next/7 will
    %	not look at the first value in the From list it is given, which
    %	means the Stop list will work just as well, which means we don't
    %	have to pass both the From and Stop list from one iteration of the
    %	do loop to the next.
    %	Note also that if compute_stop returns Stop the same as From for
    %	any element of the lists, then we don't want to execute any
    %	iterations of the do loop, so we return RevStopList the same as
    %	RevFromList.
    % Example:
    %	From = [1,1,1], To = [2,5,8]  ->  RevTo = [9,6], RevStop = [1,1,3]
compute_multifor_stop_list(FromList, ToList, StepList,
		RevFromList, RevToList, RevStepList, RevStopList) :-
	% Since the first element is treated specially, do that first.
	FromList = [From1 | FromTail],
	ToList = [To1 | ToTail],
	StepList = [Step1 | StepTail],
	compute_stop(From1, To1, Step1, Stop1),
	(
	    Stop1 \== From1,
/* No do loops in kernel.pl...
	    (
		foreach(From, FromTail),
		fromto([From1], RevFromTail, [From | RevFromTail], RevFromList),
		fromto([Stop1], RevStopTail, [From | RevStopTail], RevStopList),
		foreach(To, ToTail),
		fromto([], RevToTail, [Stop | RevToTail], RevToList),
		foreach(Step, StepTail),
		fromto([Step1], RevStepTail, [Step | RevStepTail], RevStepList)
	    do
		compute_stop(From, To, Step, Stop),
		Stop \== From
	    )
*/
	    compute_stop_tail(FromTail, ToTail, StepTail,
		    [From1], RevFromList, [Stop1], RevStopList,
		    [], RevToList, [Step1], RevStepList)
	->
	    true
	;
	    % Don't want any iteration to occur.
	    reverse(FromList, RevFromList),
	    RevStopList = RevFromList,
	    % Don't bother setting the rest?
	    reverse(ToList, RevToList),
	    reverse(StepList, RevStepList)
	).

compute_stop_tail([], [], [],
		RevFromList, RevFromList, RevStopList, RevStopList,
		RevToList, RevToList, RevStepList, RevStepList).
compute_stop_tail([From | FromTail], [To | ToTail], [Step | StepTail],
		RevFromList0, RevFromList, RevStopList0, RevStopList,
		RevToList0, RevToList, RevStepList0, RevStepList) :-
	compute_stop(From, To, Step, Stop),
	Stop \== From,
	compute_stop_tail(FromTail, ToTail, StepTail,
		[From | RevFromList0], RevFromList,
		[From | RevStopList0], RevStopList,
		[Stop | RevToList0], RevToList,
		[Step | RevStepList0], RevStepList).


    % Computes the next value to use for a multifor iterator.
    % Works with Step of either sign; assumes the "To" values have been
    % computed using compute_stop so that they match the "From" and "Step"
    % values properly.	Allows the "From" or "To" lists to be one shorter
    % than the "Idx" list, which means the most significant value will be
    % allowed to increment indefinitely.
    % Actually, we call it with RevStop instead of RevFrom, which is
    % identical up to the (ignored) most significant value...
    % The accumulator pair FwdIdx0, FwdIdx and the final call to reverse/3
    % is independent of all this and represents just a folded-in reverse/3.
multifor_next([Idx0 | RevIdx0], RevFrom, RevTo, [Step | RevStep], RevIdx,
		FwdIdx0, FwdIdx) :-
	Idx is Idx0 + Step,
	( RevTo = [Idx | RevTo1], RevFrom = [From | RevFrom1] ->
	    RevIdx = [From | RevIdx1],
	    multifor_next(RevIdx0, RevFrom1, RevTo1, RevStep, RevIdx1, [Idx0|FwdIdx0], FwdIdx)
	;
	    RevIdx = [Idx | RevIdx0],
	    reverse(RevIdx0, FwdIdx, [Idx0|FwdIdx0])
	).


%----------------------------------------------------------------------
% Singleton warnings
%----------------------------------------------------------------------

check_singletons(Term, QuantifiedVars, LoopLocation) :-
	get_flag(variable_names, check_singletons),
	collect_variables(QuantifiedVars^Term, [], Vars),
	sort(0, =<, Vars, SortedVars),
	SortedVars = [_X|Xs],
	check(_X, Xs, QuantifiedVars, LoopLocation),
	fail.
check_singletons(_, _, _).

:- mode collect_variables(?,?,-).
collect_variables(_X, Xs, [_X|Xs]) :-
	var(_X), !.
collect_variables(T, Xs, Xs) :-
	atomic(T), !.
collect_variables([T|Ts], Xs0, Xs) :- !,
	collect_variables(T, Xs0, Xs1),
	collect_variables(Ts, Xs1, Xs).
collect_variables(T, Xs0, Xs) :-
	T =.. [_|L],
	collect_variables(L, Xs0, Xs).

check(_X, [], QV, LL) :-
	warn(_X, QV, LL).
check(_X, [_Y|Ys], QV, LL) :-
	( _X == _Y ->
	     skip(_Y, Ys, QV, LL)
	;
	     warn(_X, QV, LL),
	     check(_Y,Ys, QV, LL)
	).

skip(_, [], _, _).
skip(_X, [_Y|Ys], QV, LL) :-
	( _X == _Y ->
	     skip(_Y, Ys, QV, LL)
	;
	     check(_Y,Ys, QV, LL)
	).

warn(_X, QuantifiedVars, LoopLocation) :-
	get_var_info(_X, name, Name),
	atom_string(Name, S),
	not substring(S, "_", 1),
	!,
	( occurs(_X, QuantifiedVars) ->
	    error(138, quantified(Name,LoopLocation))
	;
	    error(138, unquantified(Name,LoopLocation))
	).
warn(_, _, _).

