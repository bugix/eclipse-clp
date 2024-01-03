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
% The Initial Developer of the Original Code is  Joachim Schimpf
% Portions created by the Initial Developer are
% Copyright (C) 2016 Coninfer Ltd
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------

%
% System:	ECLiPSe Constraint Logic Programming System
% Component:	Definite Clause Grammar implementation
%		Part of module(sepia_kernel)
% Version:	$Id: kernel_dcg.pl,v 1.2 2016/10/06 13:53:41 jschimpf Exp $
%
% Description:	DCG transformation and phrase/2,3 builtins.  This is a
%		reimplementation replacing the code formerly in kernel.pl
%


%:- export dcg_rule/4.
dcg_rule((GrHeadTs --> GrMBody), Clause, AnnRule, AnnClause) :-
	valid_head(GrHeadTs),
	Clause = (Head :- MBody),
        same_annotation((AnnGrHeadTs-->AnnGrMBody), AnnRule, (AnnHead:-AnnMBody), AnnClause),
	% If present, unwrap matching-prefix -?->
	( nonvar(GrMBody), GrMBody = (-?->GrBody) ->
	    MBody = (-?->Body),
	    same_annotation((-?->AnnGrBody), AnnGrMBody, (-?->AnnBody), AnnMBody)
	;
	    GrBody=GrMBody, AnnGrBody=AnnGrMBody, Body=MBody, AnnBody=AnnMBody
	),
	( GrHeadTs = (GrHead,Ts) ->
	    valid_head(GrHead),
	    annotated_match(AnnGrHeadTs, (AnnGrHead,AnnTs)),
	    Body = (Body1, Eq),
	    annotated_create((AnnBody1,AnnEq), AnnTs, AnnBody),
	    append_args(GrHead, [S0,S], Head),
	    term_to_annotated(Head, AnnGrHead, AnnHead),	% TODO arguments
	    dcg_terminals(Ts, Eq, AnnTs, AnnEq, S, S1),
	    dcg_goal(GrBody, Body1, AnnGrBody, AnnBody1, S0, S1, _)
	;
	    append_args(GrHeadTs, [S0,S], Head),
	    term_to_annotated(Head, AnnGrHeadTs, AnnHead),	% TODO arguments
	    dcg_body(GrBody, Body, AnnGrBody, AnnBody, S0, S)
%	),
%	% Check whether annotated expansion is the same as unannotated
%	( nonvar(AnnClause) ->
%	    annotated_to_term(AnnClause, Clause1),
%	    ( Clause==Clause1 -> true ;
%		printf(error, "Transformation mismatch:%n%q%n%q%n", [Clause,Clause1]),
%		abort
%	    )
%	;
%	    true
	).


:- mode dcg_body(?,-,?,-,-,-).
dcg_body(GrBody, Goal, AnnGrBody, AnnGoal, S0, S) :-
	dcg_goal(GrBody, Goal0, AnnGrBody, AnnGoal0, S0, S1, Safety),
	terminate_body(Safety, Goal0, Goal, AnnGoal0, AnnGoal, S1, S).


:- mode dcg_goal(?,-,?,-,?,-,-).
dcg_goal(Var, Goal, AnnVar, AnnGoal, S0, S, safe) :- var(Var), !,
	Goal = phrase(Var,S0,S),
	term_to_annotated(Goal, AnnVar, AnnGoal).
dcg_goal(!, !, AnnCut, AnnCut, S, S, unsafe) :- !.
dcg_goal({}, true, AnnElem, AnnGoal, S, S, safe) :- !,
	term_to_annotated(true, AnnElem, AnnGoal).
dcg_goal({Goal}, Goal, AnnElem, AnnGoal, S, S, unsafe) :- !,
	annotated_arg(1, AnnElem, AnnGoal).
dcg_goal([], (S0=S), AnnElem, AnnGoal, S0, S, safe) :- !,
	term_to_annotated((S0=S), AnnElem, AnnGoal).
dcg_goal(Ts, Goal, AnnTs, AnnGoal, S0, S, safe) :- Ts=[_|_], !,
	dcg_terminals(Ts, Goal, AnnTs, AnnGoal, S0, S).
dcg_goal((L,R), Goal, AnnElem, AnnGoal, S0, S, Safety) :- !,
	annotated_match(AnnElem, (AnnL,AnnR)),
	dcg_goal(L, LGoal, AnnL, AnnLGoal, S0, S1, _),
	dcg_goal(R, RGoal, AnnR, AnnRGoal, S1, S, Safety),
	conjoin(LGoal, RGoal, Goal, AnnLGoal, AnnRGoal, AnnGoal, AnnElem).
dcg_goal((L;R), (LGoal;RGoal), AnnElem, AnnGoal, S0, S, safe) :- !,
	same_annotation((AnnL;AnnR), AnnElem, (AnnLGoal;AnnRGoal), AnnGoal),
	( nonvar(L), L=(C->T) ->
	    LGoal = (CGoal->TGoal),
	    same_annotation((AnnC->AnnT), AnnL, (AnnCGoal->AnnTGoal), AnnLGoal),
	    dcg_goal(C, CGoal, AnnC, AnnCGoal, S0, S1, _),
	    dcg_body(T, TGoal, AnnT, AnnTGoal, S1, S)
	;
	    dcg_body(L, LGoal, AnnL, AnnLGoal, S0, S)
	),
	dcg_body(R, RGoal, AnnR, AnnRGoal, S0, S).
dcg_goal((L->R), (LGoal->RGoal), AnnElem, AnnGoal, S0, S, Safety) :- !,
        same_annotation((AnnL->AnnR), AnnElem, (AnnLGoal->AnnRGoal), AnnGoal),
	dcg_goal(L, LGoal, AnnL, AnnLGoal, S0, S1, _),
	dcg_goal(R, RGoal, AnnR, AnnRGoal, S1, S, Safety).
dcg_goal((L|R), (LGoalT;RGoal), AnnElem, AnnGoal, S0, S, safe) :- !,
        same_annotation((AnnL|AnnR), AnnElem, (AnnLGoalT;AnnRGoal), AnnGoal),
	dcg_body(L, LGoal, AnnL, AnnLGoal, S0, S),
	dcg_body(R, RGoal, AnnR, AnnRGoal, S0, S),
	% prevent accidental introduction of (_->_;_)
	( LGoal=(_->_) ->
	    LGoalT = (LGoal,true),
	    annotated_create(true, AnnL, AnnTrue),
	    annotated_create((AnnLGoal,AnnTrue), AnnL, AnnLGoalT)
	;
	    LGoalT=LGoal, AnnLGoalT=AnnLGoal
	).
dcg_goal(\+L, \+LGoal, AnnElem, AnnGoal, S, S, unsafe) :- !,
        same_annotation(\+AnnL, AnnElem, \+AnnLGoal, AnnGoal),
	dcg_goal(L, LGoal, AnnL, AnnLGoal, S, _, _).
dcg_goal((Iter do Body), Goal, AnnElem, AnnGoal, S0, S, safe) :- !,
	Goal = (fromto(S0,S1,S2,S),Iter do NewBody),
	same_annotation((AnnIter do AnnBody), AnnElem, (AnnNewIter do AnnNewBody), AnnGoal),
	term_to_annotated(fromto(S0,S1,S2,S), AnnIter, AnnFromTo),
	annotated_create((AnnFromTo,AnnIter), AnnIter, AnnNewIter),
	dcg_body(Body, NewBody, AnnBody, AnnNewBody, S1, S2).
dcg_goal(NonTerminal, Goal, AnnElem, AnnGoal, S0, S, safe) :- callable(NonTerminal), !,
	append_args(NonTerminal, [S0,S], Goal),
	term_to_annotated(Goal, AnnElem, AnnGoal).	% TODO: do the arguments properly
dcg_goal(Elem, _Goal, _AnnElem, _AnnGoal, _S0, _S, _) :-
	throw(error(type_error(callable,Elem),_)).


/*
% code for plain lists
:- mode dcg_terminals(+,-,?,-,?,-).
dcg_terminals(Ts, (S0=S), AnnTs, AnnGoal, S0, S) :-
	term_to_annotated(S0, AnnTs, AnnS0),
	annotated_create((AnnS0=AnnTsS), AnnTs, AnnGoal),
	terminals(Ts, S, TsS, AnnTs, AnnTsS).

:- mode terminals(?,?,-,?,-).
terminals(Xs, _T, _XsT, _, _) :- var(Xs), !,
	throw(error(instantiation_error,_)).
terminals([], T, T, AnnNil, AnnT) :- !,
	annotated_create(T, AnnNil, AnnT).
terminals([X|Xs], T, [X|XsT], AnnXXs, AnnXXsT) :- !,
	same_annotation([AnnX|AnnXs], AnnXXs, [AnnX|AnnXsT], AnnXXsT),
	terminals(Xs, T, XsT, AnnXs, AnnXsT).
terminals(X, _, _, _, _) :-
	throw(error(type_error(terminal_sequence,X),_)).

*/	

% code for 'C'/3
:- mode dcg_terminals(+,-,?,-,?,-).
dcg_terminals(Xs, _Goal, _AnnXs, _AnnGoal, _S0, _S) :- var(Xs), !,
	throw(error(instantiation_error,_)).
dcg_terminals([], Goal, AnnXs, AnnGoal, S0, S) ?- !,
	Goal=true,  S0=S,
        annotated_create(Goal, AnnXs, AnnGoal).
dcg_terminals([X|Xs], Goal, AnnXXs, AnnGoal, S0, S) ?- !,
	annotated_match(AnnXXs, [AnnX|AnnXs]),
	C = 'C'(S0,X,S1),
	term_to_annotated(C, AnnX, AnnC),
	( Xs == [] ->
	    Goal = C, AnnGoal = AnnC, S1 = S
	;
	    Goal = (C,Goal1),
	    annotated_create((AnnC,AnnGoal1), AnnXXs, AnnGoal),
	    dcg_terminals(Xs, Goal1, AnnXs, AnnGoal1, S1, S)
	).
dcg_terminals(X, _Goal, _AnnXs, _AnnGoal, _S0, _S) :-
	throw(error(type_error(terminal_sequence,X),_)).

	
% 'unsafe' means the tail unification can not be propagated to the left
:- mode terminate_body(+,+,-,?,-,?,-).
terminate_body(safe, Goal, Goal, AnnGoal, AnnGoal, S, S).
terminate_body(unsafe, Goal, (Goal,S0=S), AnnGoal, AnnGoalEq, S0, S) :-
	term_to_annotated((S0=S), AnnGoal, AnnEq),
        annotated_create((AnnGoal,AnnEq), AnnGoal, AnnGoalEq).


:- mode conjoin(+,+,-,?,?,-,?).
conjoin(true, RGoal, RGoal, _AnnL, AnnR, AnnR, _AnnLocation) :- !.
conjoin(LGoal, true, LGoal, AnnL, _AnnR, AnnL, _AnnLocation) :- !.
conjoin(LGoal, RGoal, (LGoal,RGoal), AnnL, AnnR, AnnGoal, AnnLocation) :-
        annotated_create((AnnL,AnnR), AnnLocation, AnnGoal).


append_args(T0, Bs, T) :-
	T0 =.. FAs,
	append(FAs, Bs, FAsBs),
	T =.. FAsBs.


valid_head(X) :- var(X), !, throw(error(instantiation_error,_)).
valid_head(X) :- callable(X), !.
valid_head(X) :- throw(error(type_error(callable,X),_)).


trdcg(GrRule, Clause, AnnGrRule, AnnClause, Module) :-
	dcg_rule(GrRule, Clause0, AnnGrRule, AnnClause0),
	% clause-expand resulting clause
	expand_clause_annotated_(Clause0, AnnClause0, Clause, AnnClause, Module).

:- define_macro((-->)/2, trdcg/5, [clause,global]).


:- export 'C'/3.
:- inline('C'/3, tr_C/2).
tr_C('C'(XXs,X,Xs), XXs=[X|Xs]).

'C'([Token|Rest], Token, Rest).


:- export phrase/3.
:- tool(phrase/3, phrase_/4).
phrase_(GrBody, _S0, _S, _M) :- var(GrBody), !,
	throw(error(instantiation_error,_)).
phrase_(GrBody, S0, S, M) :-
	dcg_body(GrBody, Goals, _, _, S0, S),
	call(Goals)@M.

:- export phrase/2.
:- tool(phrase/2, phrase_/3).
phrase_(GrBody, S0, M) :-
	phrase_(GrBody, S0, [], M).

:- inline(phrase/2, tr_phrase/4).
:- inline(phrase/3, tr_phrase/4).
tr_phrase(phrase(GrBody,S0,S), Goals, AnnPhrase, AnnGoals) :-
	nonvar(GrBody),
	annotated_match(AnnPhrase, phrase(AnnGrBody,_,_)),
	dcg_body(GrBody, Goals, AnnGrBody, AnnGoals, S0, S).
tr_phrase(phrase(GrBody,S0), Goals, AnnPhrase, AnnGoals) :-
	nonvar(GrBody),
	annotated_match(AnnPhrase, phrase(AnnGrBody,_)),
	dcg_body(GrBody, Goals, AnnGrBody, AnnGoals, S0, []).


% Allow users to trace grammar rules through phrase/2,3
?- unskipped phrase_/3, phrase_/4.

