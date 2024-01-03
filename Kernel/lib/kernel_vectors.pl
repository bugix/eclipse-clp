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
% The Original Code is  The kernel_vectors support for ECLiPSe.
% The Initial Developer of the Original Code is  Coninfer Ltd.
% Portions created by the Initial Developer are
% Copyright (C) 2017.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------

:- pragma(expand).
:- pragma(noskip).


%----------------------------------------------------------------------
% Evaluate vector expressions
%----------------------------------------------------------------------

:- export
	eval_to_complete_list/2,
	eval_to_list/2,
	eval_to_array/2.

eval_to_complete_list(Expr, Zs) :-
	eval_to_list(Expr, Zs0),
	list_to_complete(Zs0, Zs, Zs0).

    list_to_complete(Xs, Xt) :-
	list_to_complete(Xs, Xt, Xs).

    list_to_complete(Xs, Xt, Xs0) :-
	sepia_kernel:list_end(Xs, Xs1),
	( var(Xs1), suspend(list_to_complete(Xs1, Xt, Xs0), 0, Xs1->inst)
	; Xs1==[], Xt=Xs0
	).


% Evaluate collections and their functions, return list.
eval_to_list(Xs, Zs) :- var(Xs), !,
	Zs=Xs.		% type check omitted
eval_to_list(eval(Expr), Zs) ?- !,
	( nonvar(Expr) -> eval_to_list(Expr, Zs)
	; suspend(eval_to_list(Expr, Zs), 0, Expr->inst)
	).
eval_to_list(Xe>>Ye, Zs) ?- !,
	eval_to_list(Xe, Xs),
	eval_to_list(Ye, Ys),
	pure_append(Xs, Ys, Zs).
eval_to_list(concat(Xe), Zs) ?- !,
	eval_to_list(Xe, Xs),
	pure_concat_any(Xs, Zs, []).
eval_to_list(subscript(A,I), Zs) ?- !,
	( var(A) ->
	    suspend(eval_to_list(subscript(A,I), Zs), 0, A->inst)
	; nonground(I,V) ->
	    suspend(eval_to_list(subscript(A,I), Zs), 0, V->inst)
	;
	    %%%TODO inst fault when A not instantiated deeply enough for I
	    subscript(A, I, X),
	    % coerce result if needed (Array element X may be a list)
	    coerce_to_list(X, Zs)
	).
% Expanded into three extra clauses below
%eval_to_list(Xs, Zs) :-
%	coerce_to_list(Xs, Zs).
eval_to_list([], Zs) :- !,
	Zs=[].
eval_to_list(Xs, Zs) :- Xs=[_|_], !,
	Zs=Xs.
eval_to_list(Xz, Zs) :- is_array(Xz),
	array_list(Xz, Zs).


% Coerce to list, no flattening
%delay coerce_to_list(Xs,_Ys) if var(Xs).
coerce_to_list(Xs, Ys) :- var(Xs), !,
	suspend(coerce_to_list(Xs, Ys), 0, Xs->inst).
coerce_to_list([], Ys) :- !,
	Ys=[].
coerce_to_list(Xs, Ys) :- Xs=[_|_], !,
	Ys=Xs.
coerce_to_list(Xz, Ys) :- is_array(Xz),
	array_list(Xz, Ys).


% Evaluate collections and their functions, return array.
eval_to_array(Xs, Zz) :- var(Xs), !,
	list_to_array(Xs, Zz, Xs).
eval_to_array(eval(Expr), Zz) :- !,
	( nonvar(Expr) -> eval_to_array(Expr, Zz)
	; suspend(eval_to_array(Expr, Zz), 0, Expr->inst)
	).
eval_to_array(Xe>>Ye, Zz) :- !,
	eval_to_array(Xe, Xz),
	eval_to_array(Ye, Yz),
	append_arrays(Xz, Yz, Zz).
eval_to_array(subscript(A,I), Zz) :- !,
	( var(A) ->
	    suspend(eval_to_array(subscript(A,I), Zz), 0, A->inst)
	; nonground(I,V) ->
	    suspend(eval_to_array(subscript(A,I), Zz), 0, V->inst)
	;
	    %%%TODO inst fault when A not instantiated deeply enough for I
	    subscript(A, I, X),
	    % coerce result if needed (Array element X may be a list)
	    coerce_to_array(X, Zz)
	).
eval_to_array(concat(Xe), Zz) :- !,
	eval_to_list(Xe, Xs),
	pure_concat_any(Xs, Zs, []),
	list_to_array(Zs, Zz, Zs).
% Expanded into two extra clauses below
%eval_to_array(Xs, Zz) :-
%	coerce_to_array(Xs, Zz).
eval_to_array(Xs, Yz) :- Xs=[_|Xs1], !,
	list_to_array(Xs1, Yz, Xs).
eval_to_array(Xz, Xz) :- is_array(Xz).


%delay coerce_to_array(Xs,_Ys) if var(Xs).
coerce_to_array(Xs, Ys) :- var(Xs), !,
	suspend(coerce_to_array(Xs, Ys), 0, Xs->inst).
coerce_to_array(Xs, Yz) :- Xs=[_|Xs1], !,
	list_to_array(Xs1, Yz, Xs).
coerce_to_array(Xz, Xz) :- is_array(Xz).


list_to_array(Xs, Xz, Xs0) :- var(Xz), !,
	sepia_kernel:list_end(Xs, Xs1),
	( var(Xs1), suspend(list_to_array(Xs1, Xz, Xs0), 0, Xz+Xs1->inst)
	; Xs1==[], array_list(Xz, Xs0)
	).
list_to_array(_Xs, Xz, Xs0) :-
	array_list(Xz, Xs0).


%delay append_arrays(Xz, Yz, Zz) if var(Xz);var(Yz).
append_arrays(Xz, Yz, Zz) :- var(Xz), !,
	suspend(append_arrays(Xz, Yz, Zz), 0, Xz->inst).
append_arrays(Xz, Yz, Zz) :- var(Yz), !,
	suspend(append_arrays(Xz, Yz, Zz), 0, Yz->inst).
append_arrays(Xz, Yz, Zz) :- array_concat(Xz, Yz, Zz).


%delay pure_append(Xs,_Ys,_Zs) if var(Xs).
pure_append(Xs, Ys, Zs) :- var(Xs), !,
	suspend(pure_append(Xs, Ys, Zs), 0, Xs->inst).
pure_append([], Ys, Ys).
pure_append([X|Xs], Ys, [X|Zs]) :- pure_append(Xs, Ys, Zs).


% Append to list to list/array, giving list.
% Delays for incomplete lists
%delay pure_append_any(Xs,_Ys,_Ys0) if var(Xs).
pure_append_any(Xs, Ys, Ys0) :- var(Xs), !,
	suspend(pure_append_any(Xs, Ys, Ys0), 0, Xs->inst).
pure_append_any([], Ys, Ys0) :- !, Ys=Ys0.
pure_append_any([Xs|Xss], Ys, Ys0) :-
	Ys = [Xs|Ys1],
	pure_append(Xss, Ys0, Ys1).
pure_append_any(Xz, Ys, Ys0) :- is_array(Xz),
	( foreacharg(X,Xz), fromto(Ys,[X|Ys1],Ys1,Ys0) do true).

% Concatenate all elements (list/array) of a list, giving list.
%delay pure_concat_any(Xs,_Ys,_Ys0) if var(Xs).
pure_concat_any(Xs, Ys, Ys0) :- var(Xs), !,
	suspend(pure_concat_any(Xs, Ys, Ys0), 0, Xs->inst).
pure_concat_any([], Ys, Ys0) :- !, Ys=Ys0.
pure_concat_any([Xc|Xcs], Ys, Ys0) :-
	pure_append_any(Xc, Ys, Ys1),
	pure_concat_any(Xcs, Ys1, Ys0).



%----------------------------------------------------------------------
% Arithmetic on vectors
%----------------------------------------------------------------------

:- inline(suspend_or_error/4).
suspend_or_error(ErrGoal, SuspGoal, Cond, M) :-
	( coroutining ->
	    suspend(SuspGoal, 0, Cond)@M
	;
	    error(4, ErrGoal, M)
	).


:- export sum/2.
:- tool(sum/2, sum_body/3).
sum_body(X, R, M) :- var(X), !,
	suspend_or_error(sum(X,R), sum(X,R), X->inst, M).
sum_body([], R, _M) :- !, R=0.
sum_body([X|Xs], R, M) :- !,
	eval(X, R1, M),		% backward compatibility: support expressions
	sum(Xs, R, R1, M).
sum_body(Xv*Yv, R, M) :- !,
    	eval_to_array(Xv, Xz),
	( var(Xz) ->
	    suspend_or_error(sum(Xv*Yv,R), sum(Xz*Yv,R), Xz->inst, M)
	;
	    eval_to_array(Yv, Yz),
	    ( var(Yz) ->
		suspend_or_error(sum(Xv*Yv,R), sum(Xz*Yz,R), Yz->inst, M)
	    ;
		N is min(arity(Xz),arity(Yz)),
		( for(I,1,N), param(Xz,Yz), fromto(0,R1,R2,R3) do
		    arg(I, Xz, X),
		    arg(I, Yz, Y),
		    R2 is R1 + X*Y
		),
                R = R3  % unify after loop (bug 824)
	    )
	).
sum_body(Xv, R, M) :-
    	eval_to_array(Xv, Xz),
	!,
	( var(Xz) ->
	    suspend_or_error(sum(Xv), sum(Xz,R), Xz->inst, M)
	;
	    ( foreacharg(X,Xz), fromto(0,R1,R2,R3) do
		R2 is R1 + X
	    ),
            R = R3  % unify after loop (bug 824)
	).
sum_body(X, R, M) :-
	error(5, sum(X, R), M).

    sum(X, R, R0, M) :- var(X), !,
	suspend_or_error(sum(X,R), sum([R0|X],R), X->inst, M).
    sum([], R, R0, _M) :- !, R=R0.
    sum([X|Xs], R, R0, M) :- !,
	eval(X, R1, M),		% backward compatibility: support expressions
	+(R0, R1, R2),
	sum(Xs, R, R2, M).
    sum(X, R, _R0, M) :-
	error(5, sum(X, R), M).


% min(+List, ?Min)
% max(+List, ?Max)
% The type of the result is the most general numeric type of the list elements.
% This is compatible with all arithmetic operations. It means that min/max
% should be seen as an arithmetic operation, not a list element selection
% predicate: the result may not be identical to any of the list elements!

/*
% simple version without delaying

min_body(X, R, M) :- var(X), !,
	error(4, min(X,R), M).
min_body(subscript(Array,Index), R, M) :- !,
	subscript(Array, Index, Elems, M),
	( number(Elems) -> R = Elems
	; var(Elems) -> error(4, min(Elems,R), M)
	; min_body(Elems, R, M)
	).
min_body([X1|Xs], R, M) :-
	eval(X1, R0, M),
	min1(Xs, R, R0, M).
min_body(X, R, M) :-
	error(5, min(X, R), M).

    min1(Xs, R, R0, M) :- var(Xs), !,
	error(4, min(Xs,R), M).
    min1([], R, R0, _M) :- !, R=R0.
    min1([X|Xs], R, R0, M) :- !,
	eval(X, R1, M),
	min(R0, R1, R2),
	min1(Xs, R, R2, M).
    min1(Xs, R, _R0, M) :-
	error(5, min(Xs, R), M).
*/

:- export min/2.
:- tool(min/2, min_body/3).
min_body(X, R, M) :- var(X), !,
	( coroutining ->
	    suspend(min(X,R), 0, X->inst)@M
	;
	    error(4, min(X,R), M)
	).
min_body([X1|Xs], R, M) :- !,
	( nonvar(X1) ->
	    eval(X1, R0, M),
	    min1(Xs, R, R0, M)
	; coroutining ->
	    suspend(min([X1|Xs],R), 0, X1->inst)@M
	;
	    error(4, min([X1|Xs],R), M)
	).
min_body(Xv, R, M) :-
    	eval_to_array(Xv, Xz),
	( var(Xz) ->
	    !,
	    suspend(min(Xz,R), 0, Xz->inst)@M
	;
	    arity(Xz, N), N>0,
	    !,
	    arg(N, Xz, R0),
	    ( for(I,N-1,1,-1), param(Xz), fromto(R0,R1,R2,R3) do
		arg(I, Xz, X),
		min(R1, X, R2)
	    ),
            R = R3
	).
min_body(X, R, M) :-
	error(5, min(X, R), M).

    min1(Xs, R, R0, M) :- var(Xs), !,
	( coroutining ->
	    suspend(min([R0|Xs],R), 0, Xs->inst)@M
	;
	    error(4, min(Xs,R), M)
	).
    min1([], R, R0, _M) :- !, R=R0.
    min1([X|Xs], R, R0, M) :- !,
	% nonvar(R0),
	( nonvar(X) ->
	    eval(X, R1, M),
	    min(R0, R1, R2),
	    min1(Xs, R, R2, M)
	; coroutining ->
	    suspend(min([R0,X|Xs],R), 0, X->inst)@M
	;
	    error(4, min([X|Xs],R), M)
	).
    min1(Xs, R, _R0, M) :-
	error(5, min(Xs, R), M).


:- export max/2.
:- tool(max/2, max_body/3).
max_body(X, R, M) :- var(X), !,
	( coroutining ->
	    suspend(max(X,R), 0, X->inst)@M
	;
	    error(4, max(X,R), M)
	).
max_body([X1|Xs], R, M) :- !,
	( nonvar(X1) ->
	    eval(X1, R0, M),
	    max1(Xs, R, R0, M)
	; coroutining ->
	    suspend(max([X1|Xs],R), 0, X1->inst)@M
	;
	    error(4, max([X1|Xs],R), M)
	).
max_body(Xv, R, M) :-
    	eval_to_array(Xv, Xz),
	( var(Xz) ->
	    !,
	    suspend(max(Xz,R), 0, Xz->inst)@M
	;
	    arity(Xz, N), N>0,
	    !,
	    arg(N, Xz, R0),
	    ( for(I,N-1,1,-1), param(Xz), fromto(R0,R1,R2,R3) do
		arg(I, Xz, X),
		max(R1, X, R2)
	    ),
            R = R3
	).
max_body(X, R, M) :-
	error(5, max(X, R), M).

    max1(Xs, R, R0, M) :- var(Xs), !,
	( coroutining ->
	    suspend(max([R0|Xs],R), 0, Xs->inst)@M
	;
	    error(4, max(Xs,R), M)
	).
    max1([], R, R0, _M) :- !, R=R0.
    max1([X|Xs], R, R0, M) :- !,
	% nonvar(R0),
	( nonvar(X) ->
	    eval(X, R1, M),
	    max(R0, R1, R2),
	    max1(Xs, R, R2, M)
	; coroutining ->
	    suspend(max([R0,X|Xs],R), 0, X->inst)@M
	;
	    error(4, max([X|Xs],R), M)
	).
    max1(Xs, R, _R0, M) :-
	error(5, max(Xs, R), M).


:- inline(sum/2, trans_list_op/2).
:- inline(min/2, trans_list_op/2).
:- inline(max/2, trans_list_op/2).
trans_list_op(Goal, Code) :-
	Goal =.. [Op, ExprList |Other],
	trans_expr_list(ExprList, EvalExprList, Code, Code0),
	Code \== Code0,		% prevent looping
	Code0 = sepia_kernel:NewGoal,
	NewGoal =.. [Op, EvalExprList |Other].

    trans_expr_list([E|Es], RRs, Code0, Code) ?- !,
	RRs = [R|Rs],
	trans_expr(E, R, Code0, Code1),
	trans_expr_list(Es, Rs, Code1, Code).
    trans_expr_list(VarNilJunk, VarNilJunk, Code, Code).


:- pragma(skip).
