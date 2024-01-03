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
% The Original Code is  the Sparse Set library for ECLiPSe
% The Initial Developer of the Original Code is  Joachim Schimpf
% Portions created by the Initial Developer are
% Copyright (C) 2009 - 2017 Joachim Schimpf
% 
% Contributor(s): Joachim Schimpf
% 
% END LICENSE BLOCK


:- module(sparse_set).

:- comment(summary, "Sparse integer sets").
:- comment(author, "Joachim Schimpf").
:- comment(copyright, "Joachim Schimpf, Coninfer Ltd").
:- comment(date, "$Date: 2017/09/28 22:43:43 $").
:- comment(desc, html("
    This module implements a data type to store sets of integers 1..Max.
    <P>
    The interesting characteristic is that the operations
    <UL>
    <LI>member insertion (ss_add/3)
    <LI>membership testing (ss_in/2)
    <LI>resetting to some earlier state (backtracking)
    </UL>
    are all constant-time operations.
    <P>
    Space requirement is linear in the maximum number of elements, which
    must be specified a priori.
    <P>
    Set creation is also linear in this implementation, but could in
    principle be constant (array elements don't need to be initialised).
")).


:- if(true).

% sparse[K] = M means the element K was inserted as the Mth member
% dense[M] = K  means the element K was inserted as the Mth member

% We could use a variant of this for a domain representation with
% - element removal
% - member testing
% - backtracking
% - domain creation
% - size querying
% being constant time.


:- local struct(sparse_set(
	members,	% current number of set members (integer >= 0)
	sparse,		% shelf[1..Max] of 1..Max+1
	dense		% shelf[1..Max] of 1..Max
    )).

:- export ss_create/2.
ss_create(Max, sparse_set{members:0, sparse:S, dense:D}) :-
	Invalid is Max+1,
	shelf_create(sparse/Max, Invalid, S),
	shelf_create(dense/Max, Invalid, D).

:- export ss_in/2.
ss_in(K, sparse_set{members:M, sparse:S, dense:D}) :-
	shelf_get(S, K, A),
	A =< M,		% and A>0, currently always true
	shelf_get(D, A, K).

:- export ss_add/2.	% destructive
ss_add(SS, K) :-
	SS = sparse_set{members:M, sparse:S, dense:D},
	shelf_get(S, K, A),
	( A =< M, shelf_get(D, A, K) ->
	    true
	;
	    M1 is M+1,
	    setarg(members of sparse_set, SS, M1),
	    shelf_set(D, M1, K),
	    shelf_set(S, K, M1)
	).

:- export ss_add_list/2.
ss_add_list(SS, Ks) :-
	SS = sparse_set{members:M0, sparse:S, dense:D},
	update_struct(sparse_set, [members:M3], SS0, SS),
	( foreach(K,Ks), fromto(M0,M1,M2,M3), param(S,D) do
	    shelf_get(S, K, A),
	    ( A =< M1, shelf_get(D, A, K) ->
		M2 = M1
	    ;
		M2 is M1+1,
		shelf_set(D, M2, K),
		shelf_set(S, K, M2)
	    )
	),
	( M3>M0 -> setarg(members of sparse_set, SS, M3) ; true ).

:- export ss_count/2.
ss_count(sparse_set{members:M}, M).

:- export ss_elements/2.
ss_elements(sparse_set{members:M, dense:D}, Es) :-
	( for(I,1,M), foreach(E,Es), param(D) do
	    shelf_get(D, I, E)
	).

:- export ss_print/1.
ss_print(sparse_set{members:M, sparse:S, dense:D}) :-
	shelf_get(S, 0, Sparse),
	shelf_get(D, 0, Dense),
	writeln(Sparse),
	writeln(Dense),
	writeln(members=M).


:- else.

% Same operations using a simple array

:- export ss_create/2.
ss_create(N, SS) :- functor(SS, [], N).

:- export ss_add/2.
ss_add(SS, K) :- arg(K, SS, 1).

:- export ss_add_list/2.
ss_add_list(SS, Ks) :-
	( foreach(K,Ks), param(SS) do
	    arg(K, SS, 1)
	).

:- export ss_in/2.
ss_in(K, SS) :- arg(K, SS, B), nonvar(B).

:- export ss_count/2.
ss_count(SS, M) :-
	( foreacharg(B,SS), fromto(0,M1,M2,M) do
	    ( var(B) -> M2=M1 ; M2 is M1+1 )
	).

:- export ss_elements/2.
ss_elements(SS, Es) :-
	( foreacharg(B,SS,K), fromto(Es,Es1,Es2,[]) do
	    ( var(B) -> Es1=Es2 ; Es1=[K|Es2]
	).

:- endif.
