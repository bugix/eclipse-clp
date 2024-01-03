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
% Version:	$Id: lists.pl,v 1.17 2017/09/03 14:08:55 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 * IDENTIFICATION:	lists.pl 
 *
 * DESCRIPTION: 	List manipulation library predicates
 *			Some of them are already needed in the kernel,
 *			defined there, and reexported here.
 *
 * CONTENTS:
 *	member/2,
 *	memberchk/2,
 *	append/3,
 *	nonmember/2,
 *	delete/3,
 *	intersection/3,
 *	subtract/3,
 *	union/3,
 *	length/2.
 *
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(lists).

:- comment(categories, ["Data Structures","Programming Utilities"]).
:- comment(summary, "Predicates for list manipulation").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2017/09/03 14:08:55 $").
:- comment(desc, html("<p>
    Library containing various simple list manipulation predicates which
    require no special form of lists. For ordered lists see library(ordset).
    A number of basic list processing predicates (is_list/1, append/3,
    member/2, length/2 etc) are available by default and do not require
    this library to be loaded.
</p><p>
    Note that in the predicate descriptions for this library, the '+' mode
    in the mode specification for list-valued arguments indicates that the
    list argument is required to be a proper list in the sense of is_list/1,
    i.e. all list tails must be recursively instantiated.
</p>
")).


% Make sure that the important operators are ok
:-	op(1100, xfy, ;),
	op(1050, xfy, ->).

:- pragma(system).

:- export
	checklist/2,
	flatten/2,
	flatten/3,
	halve/3,
	intersection/3,
	maplist/2,
	maplist/3,
	print_list/1,
	select/3,
	shuffle/2,
	splice/3,
	subset/2,
	union/3.

:- reexport
	append/3,
	delete/3,
	length/2,
	member/2,
	memberchk/2,
	nonmember/2,
	subtract/3,
	reverse/2
    from sepia_kernel.


:- tool(maplist/2, maplist_body/3).
:- meta_predicate(maplist(2,*)).
:- tool(maplist/3, maplist_body/4).
:- meta_predicate(maplist(2,*,*)).
:- tool(checklist/2, checklist_body/3).
:- meta_predicate(checklist(1,*)).
:- tool(print_list/1, print_list_/2).



% intersection(L1, L2, L3)
% L3 is the intersection of L1 and L2, with arguments ordered as in L1

intersection([], _, []).
intersection([Head|L1tail], L2, L3) :-
	memberchk(Head, L2),
	!,
	L3 = [Head|L3tail],
	intersection(L1tail, L2, L3tail).
intersection([_|L1tail], L2, L3) :-
	intersection(L1tail, L2, L3).


% union(L1, L2, L3)
% L3 is (L1-L2) + L2

union([], L, L).
union([Head|L1tail], L2, L3) :-
	memberchk(Head, L2),
	!,
	union(L1tail, L2, L3).
union([Head|L1tail], L2, [Head|L3tail]) :-
	union(L1tail, L2, L3tail).


% subset(Subset, S)

subset([],[]).
subset([X|L],[X|S]) :-
    subset(L,S).
subset(L, [_|S]) :-
    subset(L,S).


maplist_body(_, [], [], _).
maplist_body(Pred, [H1|T1], [H2|T2], M) :-
	call(Pred, H1, H2)@M,
	maplist_body(Pred, T1, T2, M).


maplist_body(_, [], _).
maplist_body(Pred, [Head|Tail], M) :-
	call(Pred, Head)@M,
	maplist_body(Pred, Tail, M).


checklist_body(_, [], _).
checklist_body(Pred, [Head|Tail], M) :-
	call(Pred, Head)@M,
	checklist_body(Pred, Tail, M).


flatten(List, Flat) :-
	flatten_aux(List, Flat, []).

flatten_aux([], Res, Cont) :- -?-> !, Res = Cont.
flatten_aux([Head|Tail], Res, Cont) :-
	-?->
	!,
	flatten_aux(Head, Res, Cont1),
	flatten_aux(Tail, Cont1, Cont).
flatten_aux(Array, Res, Cont) :- is_array(Array), !,
	( foreacharg(Arg,Array), fromto(Res,Res1,Res2,Cont) do
	    flatten_aux(Arg, Res1, Res2)
	).
flatten_aux(Term, [Term|Cont], Cont).


    % Depth-limited flatten.
flatten(Depth, List, Flat) :-
	flatten_aux(Depth, List, Flat, []).

flatten_aux(_Depth, [], Res, Cont) :- -?-> !, Res = Cont.
flatten_aux(Depth, [Head|Tail], Res, Cont) :-
	-?->
	!,
	( Depth > 0 ->
	    Depth1 is Depth - 1,
	    flatten_aux(Depth1, Head, Res, Cont1)
	;
	    Res = [Head|Cont1]
	),
	flatten_aux(Depth, Tail, Cont1, Cont).
flatten_aux(Depth, Array, Res, Cont) :- is_array(Array), !,
	( Depth > 0 ->
	    ( foreacharg(Arg,Array), fromto(Res,Res1,Res2,Cont) do
		flatten_aux(Arg, Res1, Res2)
	    )
	;
	    ( foreacharg(Arg,Array), fromto(Res,Res1,Res2,Cont) do
		Res1 = [Arg|Res2]
	    )
	).
flatten_aux(_, Term, [Term|Cont], Cont).


:- comment(append/2, [
    summary:"Concatenate a list of lists",
    amode:(append(+,-) is det),
    args:["ListOfLists":"A list of lists",
    	"LongList":"Concatenation of the individual lists"
    ],
    desc:html("Concatenates (appends) several lists into one long list.
    	The lists to be concatenated and their order is given as ListOfLists.

	If the number or length of the given lists are unknown, the predicate
	delays.
    "),
    eg:"\
	?- append([[a,b],[c],[],[d]], Zs).
	Zs = [a, b, c, d]
	Yes (0.00s cpu)

	?- append([[a,b],c,[d]], Zs).
	No (0.00s cpu)

	?- append([], Zs).
	Zs = []
	Yes (0.00s cpu)

	?- append([[a]|Bs], Zs).
	Bs = Bs
	Zs = [a|_177]
	Delayed goals:
		append(Bs, _177)
	Yes (0.00s cpu)

	?- append([[a],Bs,[c]], Zs).
	Zs = [a|_183]
	Delayed goals:
	    	lists : append3(Bs, [c], _183)
	Yes (0.00s cpu)
    ",
    see_also:[append/3, flatten/3]]).

:- export append/2.
delay append(Ls,_) if var(Ls).
append([], []).
append([L|Ls], Xs) :-
	append3(L, Xs1, Xs),
	append(Ls, Xs1).

    delay append3(Xs,_,_) if var(Xs).
    append3([], Ys, Ys).
    append3([X|Xs], Ys, [X|Zs]) :- append3(Xs, Ys, Zs).


/*
% Non-logical flattener, treats variables as objects, no depth limit.
% Accepts only data (lists and arrays)
:- export flatten_collection/2.
flatten_collection(Xs, Ys) :-
	flatten_collection(Xs, Ys, []).

    flatten_collection([], Ys, Ys0) ?- !,
	Ys=Ys0.
    flatten_collection([Xs|Xss], Ys, Ys0) ?- !,
	flatten_collection(Xs, Ys, Ys1),
	flatten_list_of_collections(Xss, Ys1, Ys0).
    flatten_collection(Xz, Ys, Ys0) :- is_array(Xz), !,
	( foreacharg(X,Xz), fromto(Ys,Ys1,Ys2,Ys0) do
	    flatten_collection(X, Ys1, Ys2)
	).
    flatten_collection(X, [X|Ys], Ys).

    flatten_list_of_collections([], Ys, Ys0) ?- !,
	Ys=Ys0.
    flatten_list_of_collections([Xs|Xss], Ys, Ys0) ?- !,
	flatten_collection(Xs, Ys, Ys1),
	flatten_list_of_collections(Xss, Ys1, Ys0).
    flatten_list_of_collections(X, _Ys, _Ys0) :- var(X), !,
	throw(error(instantiation_error, flatten_list_of_collections/3)).
    flatten_list_of_collections(X, _Ys, _Ys0) :-
	throw(error(type_error(list,X), flatten_list_of_collections/3)).
*/

:- comment(collection_to_list/2, [
    summary:"Convert a \"collection\" into a list",
    amode:(collection_to_list(+,-) is semidet),
    args:["Collection":"A term to be interpreted as a collection",
    	"List":"Output list"],
    exceptions:[4:"A (sub-)collection is insufficiently instantiated"],
    fail_if:"Collection is not a collection or collection expression",
    see_also:[eval_to_list/2, array_list/2, array_flat/3, is_list/1,
    		is_array/1, subscript/3, flatten/2, flatten/3],
    desc:html("\
   Converts various \"collection\" data structures (and expressions
   combining them) into a list.  Fails if it does not know how to do this.
   The supported collection types are:
<DL>
   <DT><STRONG>List</STRONG><DD>
	The list is returned unchanged. It must be terminated with [].
   <DT><STRONG>Array</STRONG><DD>
	The array is converted into a list, as with array_list/2.
</DL>
   In addition, the following collection-valued expressions are allowed:
<DL>
   <DT><STRONG>Array[...]</STRONG><DD>
	Subscript-reference: Extract an element or sub-array from Array.
	If a single array element is extracted, this element must itself
	be a collection (unless the result is being flattened).
   <DT><STRONG>Collection1&gt;&gt;Collection2</STRONG><DD>
	Concatenate the two collections into a single list.
   <DT><STRONG>concat(Collection)</STRONG><DD>
	If the collection is nested (at least 2-dimensional), the top
	level of the structure is removed and all its elements (which
	must be lists or arrays) are concatenated into a new list.
   <DT><STRONG>flatten(N,Collection) - nonlogical</STRONG><DD>
	If the collection is nested (multi-dimensional), the top N>=0
	nesting levels of the structure are converted into a flat list.
	In these top N levels, all subterms that look like list or array
	(including []) are interpreted as sub-collections and their
	elements added to the flattened result.
	If Collection is a single element (i.e. a non-collection term),
	the result is a single-element list containing it.
   <DT><STRONG>flatten(Collection) - nonlogical</STRONG><DD>
	If the collection is nested (multi-dimensional), all nesting
	structure is removed and a flat list is returned.
	If Collection is a single element (i.e. a non-collection term),
	the result is a single-element list containing it.
</DL>
    NOTE: The built-in predicates eval_to_list/2 and eval_to_complete_list/2
    are logical (constraint) versions of collection_to_list/2.  These
    versions do not support flatten/1,2 because there the interpretation of
    subterms depends on their instantiation rather than just their position.
"),
    eg:"\
   ?- List=[a,b,c,d], collection_to_list(List, Result).
   Result = [a, b, c, d]

   ?- Arr=[](a,b,c,d), collection_to_list(Arr, Result).
   Result = [a, b, c, d]

   ?- Arr=[](a,b,c,d), collection_to_list(Arr[2..3], Result).
   Result = [b, c]


   ?- Mat=[]([](a,b,c),[](d,e,f)), collection_to_list(Mat, Result).
   Result = [[](a, b, c), [](d, e, f)]

   ?- Mat=[]([](a,b,c),[](d,e,f)), collection_to_list(concat(Mat), Result).
   Result = [a, b, c, d, e, f]

   ?- Mat=[]([](a,b,c),[](d,e,f)), collection_to_list(flatten(1,Mat), Result).
   Result = [a, b, c, d, e, f]

   ?- Mat=[]([](a,b,c),[](d,e,f)), collection_to_list(Mat[1], Result).
   Result = [a, b, c]

   ?- Mat=[]([](a,b,c),[](d,e,f)), collection_to_list(Mat[1,*], Result).
   Result = [a, b, c]

   ?- Mat=[]([](a,b,c),[](d,e,f)), collection_to_list(Mat[*,2], Result).
   Result = [b, e]

   ?- Mat=[]([](a,b,c),[](d,e,f)), collection_to_list(Mat[1..2,2], Result).
   Result = [b, e]

   ?- Mat=[]([](a,b,c),[](d,e,f)), collection_to_list(Mat[1..2,2..3], Result).
   Result = [[](b, c), [](e, f)]

   ?- Mat=[]([](a,b,c),[](d,e,f)),
			collection_to_list(concat(Mat[1..2,2..3]), Result).
   Result = [b, c, e, f]


   ?- NL = [a,b,[c,d]], collection_to_list(NL, Result).
   Result = [a, b, [c, d]]

   ?- NL = [a,b,[c,d]], collection_to_list(flatten(1,NL), Result).
   Result = [a, b, c, d]

   ?- NL = [a,b,[](c,d)], collection_to_list(NL, Result).
   Result = [a, b, [](c, d)]

   ?- NL = [a,b,[](c,d)], collection_to_list(flatten(1,NL), Result).
   Result = [a, b, c, d]


   ?- NA = [](a,b,[c,d]), collection_to_list(NA, Result).
   Result = [a, b, [c, d]]

   ?- NA = [](a,b,[c,d]), collection_to_list(NA[3], Result).
   Result = [c, d]


   ?- Xs=[a,b], Yz=[](c,d), collection_to_list(Xs>>Yz, Result).
   Result = [a, b, c, d]


   % Error cases where collections expected
   ?- collection_to_list(no_collection, Result).
   No (0.00s cpu)

   ?- collection_to_list(99, Result).
   No (0.00s cpu)

   ?- collection_to_list(X, Result).
   instantiation fault in collection_to_list / 2

   ?- collection_to_list(concat([[1],2,[3]]), Result).
   No (0.00s cpu)


   % Special case: force treatment of a term as single-element collection
   ?- collection_to_list(flatten(99), Result).
   Result = [99]

   ?- collection_to_list(flatten(0,99), Result).
   Result = [99]

   ?- collection_to_list(flatten(X), Result).
   Result = [X]
"]).

% Changes wrt previous version:
%	Collection		6.1 result	7.0 result
%	X			fail		error
%	9			fail		fail
%	flatten(X)		fail		[X]
%	flatten(9)		fail		[9]
%	[](A,B)[1]		[A]		error
%	[](9,8)[1]		[9]		fail
%	flatten([](A,B)[1])	[A]		[A]
%	flatten([](9,8)[1])	[9]		[9]
%	[1|T]			[1|T]		error

:- export collection_to_list/2.
collection_to_list(Xe, Ys) :-
	eval_cexpr(Xe, Ys, [], false).

% Evaluate expressions and convert data structures
eval_cexpr(X, Ys, Ys0, AllowSingleton) :- var(X), !,
	( AllowSingleton==true -> Ys = [X|Ys0]
	; throw(error(instantiation_error, collection_to_list/2))
	).
eval_cexpr(Xs, Ys, Ys0, _AllowSingleton) :- Xs=[_|_], !,
	det_append(Xs, Ys0, Ys).
eval_cexpr(Xz, Ys, Ys0, _AllowSingleton) :- is_array(Xz), !,
	sepia_kernel:array_list(Xz, Ys, Ys0).
eval_cexpr(subscript(A,I), Ys, Ys0, AllowSingleton) :- !,
	subscript(A, I, X),
	collection_to_list_simple(X, Ys, Ys0, AllowSingleton).
eval_cexpr(Xe>>Ye, Zs, Zs0, AllowSingleton) :- !,
	eval_cexpr(Xe, Zs, Zs1, AllowSingleton),
	eval_cexpr(Ye, Zs1, Zs0, AllowSingleton).
eval_cexpr(concat(Xe), Ys, Ys0, _AllowSingleton) :- !,
	eval_cexpr(Xe, Xs, [], true),
	( foreach(Xi,Xs), fromto(Ys,Ys1,Ys2,Ys0) do
	    collection_to_list_simple(Xi, Ys1, Ys2, false)
	).
eval_cexpr(flatten(Xe), Ys, Ys0, _AllowSingleton) :- !,
	eval_cexpr(Xe, Xs, [], true),
	element_to_flat_list(Xs, Ys, Ys0, -1).
eval_cexpr(flatten(D,Xe), Ys, Ys0, _AllowSingleton) :- !,
	eval_cexpr(Xe, Xs, [], true),
	element_to_flat_list(Xs, Ys, Ys0, D).
eval_cexpr(X, [X|Ys0], Ys0, true).

    % Convert data structures only (no functions)
    collection_to_list_simple(X, Ys, Ys0, AllowSingleton) :- var(X), !,
	( AllowSingleton==true -> Ys = [X|Ys0]
	; throw(error(instantiation_error, collection_to_list/2))
	).
    collection_to_list_simple(Xs, Ys, Ys0, _AllowSingleton) :- Xs=[_|_], !,
	det_append(Xs, Ys0, Ys).
    collection_to_list_simple(Xz, Ys, Ys0, _AllowSingleton) :- is_array(Xz), !,
	sepia_kernel:array_list(Xz, Ys, Ys0).
    collection_to_list_simple(X, [X|Ys0], Ys0, true).

    % Like flatten/3, but works for arrays and lists, returning list.
    element_to_flat_list(X, Ys, Ys0, _D) :- var(X), !,
	Ys = [X|Ys0].
    element_to_flat_list(Xs, Ys, Ys0, D) :- Xs=[_|_], !,
	( D==0 ->
	    det_append(Xs, Ys0, Ys)
	;
	    D1 is D-1,
	    list_to_flat_list(Xs, Ys, Ys0, D1)
	).
    element_to_flat_list(Xz, Ys, Ys0, D) :- is_array(Xz), !,
	( D==0 ->
	    sepia_kernel:array_list(Xz, Ys, Ys0)
	;
	    D1 is D-1,
	    arity(Xz, N),
	    array_to_flat_list(Xz, 1, N, Ys, Ys0, D1)
	).
    element_to_flat_list(X, [X|Ys0], Ys0, _D).

    % Fails if Xs is improper list.
    list_to_flat_list(X, _Ys, _Ys0, _D) :- var(X), !,
	throw(error(instantiation_error, collection_to_list/2)).
    list_to_flat_list([], Ys, Ys, _D).
    list_to_flat_list([X|Xs], Ys, Ys0, D) :-
    	element_to_flat_list(X, Ys, Ys1, D),
	list_to_flat_list(Xs, Ys1, Ys0, D).

    % Returns nonvar(Cond) if insufficiently instantiated.
    % May partially instantiate Ys even if nonvar(Cond)!
    array_to_flat_list(Xz, I, N, Ys, Ys0, D) :-
	( I>N -> Ys=Ys0 ;
	    arg(I, Xz, X),
	    element_to_flat_list(X, Ys, Ys1, D),
	    I1 is I+1,
	    array_to_flat_list(Xz, I1, N, Ys1, Ys0, D)
	).

    % Append if Xs is a proper list.
    % If Xs ends in variable T, leave Zs untouched and return Cond=(T->inst)
    det_append(Xs, [], Zs) ?- !,
	sepia_kernel:list_end(Xs, Xt),
	( var(Xt), throw(error(instantiation_error, collection_to_list/2))
	; Xt==[], Zs = Xs		% avoid copying Xs
	).
    det_append(Xs, Ys, Zs) :-
	det_append1(Xs, Ys, Zs).

    det_append1(X, _Ys, _Zs) :- var(X), !,
	throw(error(instantiation_error, collection_to_list/2)).
    det_append1([], Ys, Ys).
    det_append1([X|Xs], Ys, Zs) :- Zs=[X|Zs1], det_append1(Xs, Ys, Zs1).




% Naive/nondet version:
:- export all_same_length/1.
all_same_length(Xss) :-
	( nils(Xss)
	; lists_tails(Xss, Tss), all_same_length(Tss)
	).


% Det version, returns variables to delay on.
% This is written to not make "trial" instantiations, or backtrack.
:- export all_same_length/2.
all_same_length(Xss, Vs) :-
	all_same_length(Xss, Vs, Xss).

    all_same_length(Xss, Vs, _Xss) :- var(Xss), !,
	Vs = [Xss].
    all_same_length([], Xss, Xss).  % they all have an open tail!
    all_same_length([Xs|Xss1], Vs, Xss) :-
	( var(Xs) ->
	    all_same_length(Xss1, Vs, Xss)
	; Xs=[_|_] ->
	    lists_tails(Xss, Tss),
	    all_same_length(Tss, Vs)
	; Xs=[] ->
	    Vs = [],
	    nils(Xss)
	;
	    throw(error(type_error(list_or_partial_list,Xs),all_same_length/2))
	).

    lists_tails([], []).
    lists_tails([[_|Ts]|Xss], [Ts|Tss]) :-
	lists_tails(Xss, Tss).

    nils([]).
    nils([[]|Xs]) :- nils(Xs).



:- comment(halve/3, [
    summary:"Split a list in the middle",
    amode:(halve(+,-,-) is det),
    template:"halve(+List, ?Front, ?Back)",
    desc:html("Returns two lists (Front and Back) which can be concatenated to give
	the original List. The length of the sub-lists is half the length of
	the original. If the original length is odd, Front is one longer"),
    eg:"\
	halve([a,b,c,d,e,f], [a,b,c], [d,e,f])
	halve([a,b,c,d,e,f,g], [a,b,c,d], [e,f,g])",
    see_also:[append/3]]).

halve(List, Front, Back) :-
	halve(List, List, Front, Back).

    halve([], Back0, Front, Back) :- !, Front=[], Back=Back0.
%   halve([_], Back0, Front, Back) :- !, Front=[], Back=Back0.	% Front=<Back
    halve([_], [X|Rs], Front, Back) :- !, Front=[X], Back=Rs.	% Front>=Back
    halve([_,_|Es], [X|Rs], [X|Fs], Back) :-
	halve(Es, Rs, Fs, Back).


:- comment(splice/3, [
    summary:"Merge two lists by interleaving the elements",
    args:["Odds":"List or variable",
    	"Evens":"List or variable",
	"List":"Variable or list"],
    amode:(splice(+,+,-) is det),
    amode:(splice(-,-,+) is multi),
    desc:html("Create a new list by alternating elements from two input lists,
    	starting with the first. When one input list is longer, its extra
	elements form the tail of the result list.
	<P>
	The reverse mode splice(-,-,+) is nondeterministic, and
	the most balanced solution(s) will be found first."),
    eg:"\
?- splice([1,2,3], [a,b,c], X).
X = [1, a, 2, b, 3, c]
Yes (0.00s cpu)

?- splice([1,2,3], [a,b,c,d,e], X).               
X = [1, a, 2, b, 3, c, d, e]
Yes (0.00s cpu)

?- splice(A, B, [1,a,2,b,3,c]).
A = [1, 2, 3]
B = [a, b, c]
More (0.00s cpu) ? ;
A = [1, 2, 3, c]
B = [a, b]
More (0.00s cpu) ? ;
A = [1, 2]
B = [a, b, 3, c]
More (0.00s cpu) ? ;
A = [1, 2, b, 3, c]
B = [a]
More (0.00s cpu) ? ;
A = [1]
B = [a, 2, b, 3, c]
More (0.00s cpu) ? ;
A = [1, a, 2, b, 3, c]
B = []
More (0.00s cpu) ? ;
A = []
B = [1, a, 2, b, 3, c]
Yes (0.00s cpu)
",
    see_also:[merge/3]]).

splice(Ls, Rs, LRs) :- Ls = [_|_],
	splice1(Ls, Rs, LRs).
splice([], Rs, Rs).

    splice1([L|Ls], [R|Rs], [L,R|LRs]) :-
	splice(Ls, Rs, LRs).
    splice1(Ls, [], Ls).
    	

% Shuffle a list
% This neat method seems to be from Lee Naish

:- comment(shuffle/2, [
	summary:"Shuffle a list, ie randomize the element order",
	amode:(shuffle(+,-) is det),
	template:"shuffle(+List, ?ShuffledList)",
	see_also:[msort/2]]).

shuffle(L, R) :-
        add_random_keys(L, KL),
        keysort(KL, KR),
        rm_keys(KR, R).

        % add random key to each list element
add_random_keys([], []).
add_random_keys([A|L], [K-A|KL]) :-
        random(K),
        add_random_keys(L, KL).

        % remove keys from association list
rm_keys([], []).
rm_keys([_K-A|KL], [A|L]) :-
        rm_keys(KL, L).


:- comment(print_list / 1, [
	summary:"Print the elements of a list, one per line",
	amode:(print_list(+) is det),
	template:"print_list(+List)"
    ]).

print_list_([], _).
print_list_([H|T], M) :-
	writeln(H)@M,
	print_list_(T, M).


:- comment(middle_out/2, [
	summary:"Reorder a list such that the middle elements come first",
	amode:(middle_out(+,-) is det),
	args:["List":"A list", "Reordered":"A variable or list"],
	eg:"
?- middle_out([1,2,3,4,5], Zs).
Zs = [3, 2, 4, 1, 5]
Yes (0.00s cpu)

?- middle_out([1,2,3,4,5,6], Zs).
Zs = [3, 4, 2, 5, 1, 6]
Yes (0.00s cpu)
	",
	see_also:[reverse/2]]).

:- export middle_out/2.
middle_out(XsYs, Zs) :-
	middle_out(XsYs, XsYs, [], Zs0),
	!, Zs=Zs0.

    middle_out([], Ys, Zs, Zs) :- evens(Zs, Ys).
    middle_out([_], [Y|Ys], Zs, [Y|Zs]) :- evens(Zs, Ys).
    middle_out([_,_|XsYs], [X|Xs], XYs, Zs) :-
	middle_out(XsYs, Xs, [X,_Y|XYs], Zs).

    evens([], []).
    evens([_,Y|XYs], [Y|Ys]) :- evens(XYs, Ys).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(select / 3, [
	summary:"Succeeds if List2 is List1 less an occurence of Element in List1.\n\n",
	amode:(select(+,+,-) is nondet),	% redundant, but common
	amode:(select(-,+,-) is nondet),	% redundant, but common
	amode:(select(-,-,-) is multi),
	template:"select(?Element, ?List1, ?List2)",
	desc:html("\
   Unifies the list List2 with the list List1 less an occurence of Element.
   Any alternative solutions are provided on backtracking.
<P>
   This predicate can be used to select an element from a list, delete an
   element or insert it.
<P>
   The definition of this Prolog library predicate is:
<PRE>
    select(A, [A|B], B).
    select(A, [B, C|D], [B|E]) :-
	    select(A, [C|D], E).
</PRE>
   This predicate does not perform any type testing functions.
   "),
	args:["?Element" : "Prolog term.", "?List1" : "List or variable.", "?List2" : "List or variable."],
	resat:"   Yes.",
	fail_if:"   Fails if List2 does not unify with List1 less an occurence of Element.\n\n",
	eg:"
Success:
    [eclipse]: select(X,[1,M,X],L), writeln((M,X,L)), fail.
    _g66 , 1 , [_g66, 1]
    _g66 , _g66 , [1, _g66]
    _g66 , _g72 , [1, _g66]
    no (more) solution.
    
    [eclipse]: select(3,[1,3,5,3],L).
    L = [1, 5, 3]    More? (;)
    L = [1, 3, 5]
    yes.
    
    [eclipse]: select(X,L,[a,b]), writeln((X,L)), fail.
    _g66 , [_g66, a, b]
    _g66 , [a, _g66, b]
    _g66 , [a, b, _g66]
    no (more) solution.
    
    select(X,[1,2],L).   (gives X=1 L=[2]; X=2 L=[1]).

Fail:
    select(1,[1,2,1,3],[2,3]).
	",
	see_also:[subtract / 3, member / 2]]).

select(Streams, Timeout, Ready) :- (number(Timeout);Timeout==block), !,
	% stream_select/3 used to be called select/3, catch such usage
	error(5, select(Streams, Timeout, Ready)).
select(X, XXs, Xs) :-
	select1(X, XXs, Xs).

select1(A, [A|C], C).
select1(A, [B|C], [B|D]) :-
	select1(A, C, D).


:- comment(delete / 3, [
	summary:"Succeeds if List2 is List1 less an occurence of Element in List1.\n\n",
	amode:(delete(+,+,-) is nondet),	% redundant, but common
	amode:(delete(-,+,-) is nondet),	% redundant, but common
	amode:(delete(-,-,-) is multi),
	template:"delete(?Element, ?List1, ?List2)",
	desc:html("\
   Unifies the list List2 with the list List1 less an occurence of Element.
   Any alternative solutions are provided on backtracking.
<P>
   This predicate can be used to select an element from a list, delete an
   element or insert it.
<P>
   The definition of this Prolog library predicate is:
<PRE>
    delete(A, [A|B], B).
    delete(A, [B, C|D], [B|E]) :-
	    delete(A, [C|D], E).
</PRE>
   This predicate does not perform any type testing functions.
   "),
	args:["?Element" : "Prolog term.", "?List1" : "List or variable.", "?List2" : "List or variable."],
	resat:"   Yes.",
	fail_if:"   Fails if List2 does not unify with List1 less an occurence of Element.\n\n",
	eg:"\nSuccess:\n   [eclipse]: delete(X,[1,M,X],L), writeln((M,X,L)), fail.\n   _g66 , 1 , [_g66, 1]\n   _g66 , _g66 , [1, _g66]\n   _g66 , _g72 , [1, _g66]\n   no (more) solution.\n\n   [eclipse]: delete(3,[1,3,5,3],L).\n   L = [1, 5, 3]    More? (;)\n   L = [1, 3, 5]\n   yes.\n\n   [eclipse]: delete(X,L,[a,b]), writeln((X,L)), fail.\n   _g66 , [_g66, a, b]\n   _g66 , [a, _g66, b]\n   _g66 , [a, b, _g66]\n   no (more) solution.\n\n   delete(X,[1,2],L).   (gives X=1 L=[2]; X=2 L=[1]).\nFail:\n   delete(1,[1,2,1,3],[2,3]).\n\n\n\n",
	see_also:[subtract / 3, member / 2]]).

:- comment(intersection / 3, [
	summary:"Succeeds if Common unifies with the list which contains the common elements\nof List1 and List2.\n\n",
	amode:(intersection(+,+,-) is det),
	template:"intersection(+List1, +List2, ?Common)",
	desc:html("\
   Common is unified with a list which contains the common elements of
   List1 and List2.
<P>
   The definition of this Prolog library predicate is:
<PRE>
intersection([], _, []).
intersection([Head|L1tail], L2, L3) :-
        memberchk(Head, L2),
        !,
        L3 = [Head|L3tail],
        intersection(L1tail, L2, L3tail).
intersection([_|L1tail], L2, L3) :-
        intersection(L1tail, L2, L3).
</PRE>
   This predicate does not perform any type testing functions.
<P>
   This predicate works properly for set operations only, so repeated
   elements and variable elements should not be used in the lists.
	"),
	args:["+List1" : "List.", "+List2" : "List.", "?Common" : "List or variable."],
	resat:"   Yes.",
	fail_if:"   Fails if Common does not unify with the list which contains the common\n   elements of List1 and List2.\n\n",
	eg:"\nSuccess:\n   intersection([1,2],[2,3],L).     (gives L=[2]).\n   intersection([a,d],[a,b,c],[a]).\n\nFail:\n   intersection([a,b],[a,b],[b]).\n\n\n",
	see_also:[subtract / 3, memberchk / 2, union / 3]]).

:- comment(length / 2, [
	summary:"Succeeds if the length of list List is N.\n\n",
	amode:(length(+,+) is semidet),
	amode:(length(+,-) is det),
	amode:(length(-,+) is det),
	amode:(length(-,-) is multi),
	template:"length(?List, ?N)",
	desc:html("\
   Unifies N with the length of list List.  length/2 can be used to create
   a list List of length N. The definition of this Prolog library predicate
   is:
<PRE>
length(List, Length) :-
        ( var(Length) ->
          length(List, 0, Length)
        ;
          Length >= 0,
          length1(List, Length) ).

length([], Length, Length).
length([_|L], N, Length) :-
        N1 is N+1,
        length(L, N1, Length).

length1([], 0) :- !.
length1([_|L], Length) :-
        N1 is Length-1,
        length1(L, N1).
</PRE>
   This predicate does not perform any type testing functions.
	"),
	args:["?List" : "List or variable.", "?N" : "Integer or variable."],
	resat:"   Yes.",
	fail_if:"   Fails if the length of list List does not unify with N.\n\n",
	eg:"\nSuccess:\n  length([1,2,3],N).   (gives N=3).\n  length([1,2,1,X],N). (gives X=_g84; N=4).\n  length(L,2).         (gives L=[_g62,_g72]). % creates list\nFail:\n  length([1,2,3],2).\n\n\n",
	see_also:[append / 3]]).

:- comment(member / 2, [
	summary:"Succeeds if Term unifies with a member of the list List.\n\n",
	amode:(member(-,+) is nondet),
	amode:(member(+,-) is nondet),
	amode:(member(-,-) is multi),
	template:"member(?Term, ?List)",
	desc:html("\
   Tries to unify Term with an element of the list List.
<P>
   If Term is a variable and List is a list, all the members of the list
   List are found on backtracking.
<P>
   If List is not instantiated, member/2 binds List to a new partial list
   containing the element Term.
<P>
   The definition of this Prolog library predicate is:
<PRE>
       member(X,[X|_]).
       member(X,[Y|T]) :- member(X,T).
</PRE>
   This predicate does not perform any type testing functions.
	"),
	args:["?Term" : "Prolog term.", "?List" : "List or variable."],
	resat:"   Yes.",
	fail_if:"   Fails if Term does not unify with a member of the list List.\n\n",
	eg:"\nSuccess:\n      member(q,[1,2,3,p,q,r]).\n      member(q,[1,2,F]).      (gives F=q).\n      member(X,[1,X]).        (gives X=1; X=_g118).\n      member(X,[2,I]).        (gives X=2 I=_g114; X=_g94 I=_g94).\n      member(1,L).            (gives L=[1|_g64];\n                                     L=[_g62,1|_g68] etc).\n\nFail:\n      member(4,[1,2,3]).\n\n\n\n",
	see_also:[memberchk / 2]]).

:- comment(memberchk / 2, [
	summary:"Succeeds if Term is a member of the list List.\n\n",
	amode:(memberchk(+,+) is semidet),
	amode:(memberchk(+,-) is det),
	template:"memberchk(+Term, ?List)",
	desc:html("\
   Unifies Term with the first matching element of the list List.
<P>
   If List is not instantiated, memberchk/2 binds List to a new partial
   list containing an element Term.
<P>
   The definition of this Prolog library predicate is:
<PRE>
memberchk(X,[X|_]) :- !.
memberchk(X,[_|T]):- memberchk(X,T).
</PRE>
   This predicate does not perform any type testing functions.
	"),
	args:["+Term" : "Prolog term.", "?List" : "List or a variable."],
	resat:"   No.",
	fail_if:"   Fails if Term is not a member of the list List.\n\n",
	eg:"\nSuccess:\n      memberchk(0,[1,B,2]). (gives B=0).\n      memberchk(1,[1,X]).   (gives X=_g76).\n      memberchk(1,X), memberchk(2,X).\n                            (gives X=[1,2|_g98]).\n\nFail:\n      memberchk(0,[1,2,3,4]).\n\n\n\n",
	see_also:[member / 2]]).

:- comment(nonmember / 2, [
	summary:"Succeeds if Element is not an element of the list List.\n\n",
	amode:(nonmember(+,+) is semidet),
	template:"nonmember(+Element, +List)",
	desc:html("\
   Used to check that Element is not a member of the list List.
<P>
   The definition of this Prolog library predicate is:
<PRE>
nonmember(Arg,[Arg|_]) :-
        !,
        fail.
nonmember(Arg,[_|Tail]) :-
        !,
        nonmember(Arg,Tail).
nonmember(_,[]).
</PRE>
   This predicate does not perform any type testing functions.
	"),
	args:["+Element" : "Prolog term.", "+List" : "List."],
	resat:"   No.",
	fail_if:"   Fails if Element is an element of the list List.\n\n",
	eg:"\nSuccess:\n  nonmember(q,[1,2,3,4,5,6,7]).\n\nFail:\n  nonmember(1,[1,2,3]).\n  nonmember(q,[1,2,2,X]). % X and q are unifiable\n\n\n\n",
	see_also:[member / 2, memberchk / 2]]).

:- comment(subtract / 3, [
	summary:"Succeeds if Remainder is the list which contains those elements of List1\nwhich are not in List2.\n\n",
	amode:(subtract(+,+,-) is det),
	template:"subtract(+List1, +List2, ?Remainder)",
	desc:html("\
   Unifies Remainder with a list containing those elements of List1 which
   are not in List2.
<P>
   The definition of this Prolog library Predicate is:
<PRE>
subtract([], _, []).
subtract([Head|Tail], L2, L3) :-
        memberchk(Head, L2),
        !,
        subtract(Tail, L2, L3).
subtract([Head|Tail1], L2, [Head|Tail3]) :-
        subtract(Tail1, L2, Tail3).
</PRE>
   This predicate does not perform any type testing functions.
<P>
   This predicate works properly for set operations only, so repeated
   elements and variable elements should not be used.
	"),
	args:["+List1" : "List.", "+List2" : "List.", "?Remainder" : "List or variable."],
	resat:"   No.",
	fail_if:"   Fails if if Remainder does not unify with the list which contains those\n   elements of List1 which are not in List2.\n\n",
	eg:"\nSuccess:\n   subtract([1,2,3,4],[1],R).     (gives R=[2,3,4]).\n   subtract([1,2,3],[3,4],R).     (gives R=[1,2]).\n   subtract([1,1,2,3],[2],[1,1,3]).\nFail:\n   subtract([1,1,2,3],[1],[1,2,3]). % Fails - List2 and\n                                    % Remainder share elements\n\n\n\n",
	see_also:[intersection / 3, union / 3]]).

:- comment(union / 3, [
	summary:"Succeeds if Union is the list which contains the union of elements in List1\nand those in List2.\n\n",
	amode:(union(+,+,-) is det),
	template:"union(+List1, +List2, ?Union)",
	desc:html("\
   Used to create the list of elements in List1 and not in List2, added to
   those in List2.
<P>
   The definition of this Prolog library predicate is:
<PRE>
union([], L, L).
union([Head|L1tail], L2, L3) :-
        memberchk(Head, L2),
        !,
        union(L1tail, L2, L3).
union([Head|L1tail], L2, [Head|L3tail]) :-
        union(L1tail, L2, L3tail).
</PRE>
   This predicate does not perform any type testing functions.
<P>
   This predicate works properly for set operations only, so repeated
   elements and variable elements should not be used.
	"),
	args:["+List1" : "List.", "+List2" : "List.", "?Union" : "List or variable."],
	resat:"   No.",
	fail_if:"   Fails if Union does not unify with the list which contains the union of\n   elements in List1 and those in List2.\n\n",
	eg:"\nSuccess:\n      union([1,2,3],[1,3],L).     (gives L=[2,1,3]).\n\nFail:\n      union([1,2,3,2],[1,3],[1,2,3]).  % repeated elements\n\n\n\n",
	see_also:[subtract / 3, intersection / 3]]).

:- comment(reverse / 2, [
	summary:"Succeeds if Reversed is the reversed list List.\n\n",
	amode:(reverse(+,-) is det),
	template:"reverse(+List, ?Reversed)",
	desc:html("\
   The List is reversed and the resulting list is unified with Reverse.
<P>
   The definition of this Prolog library predicate is:
<PRE>
reverse(List, Rev) :-
        reverse(List, Rev, []).

reverse([], L, L).
reverse([H|T], L, SoFar) :-
        reverse(T, L, [H|SoFar]).
</PRE>
   This predicate does not perform any type testing functions.
	"),
	args:["+List" : "A List.", "?Reversed" : "List or variable."],
	resat:"   No.",
	fail_if:"   Fails if Reverse does not unify with the reversed version of List.\n\n",
	eg:"\nSuccess:\n    [eclipse]: reverse([1,2,3,4,5], X).\n    X = [5, 4, 3, 2, 1]\n    yes.\n\n\n\n\n",
	see_also:[append / 3, member / 2]]).

:- comment(append / 3, [
	summary:"Succeeds if List3 is the result of appending List2 to List1.\n\n",
	amode:(append(+,+,-) is det),
	amode:(append(-,-,+) is multi),
	template:"append(?List1, ?List2, ?List3)",
	desc:html("\
   Unifies List3 to the result of appending List2 to List1.  On
   backtracking append/3 gives all possible solutions for List1 and List2,
   if both are uninstantiated.
<P>
   The definition of this Prolog library predicate is:
<PRE>
append([],X,X).
append([X|L1],L2,[X|L3]):-
        append(L1,L2,L3).
</PRE>
   This predicate does not perform any type testing functions.
	"),
	args:["?List1" : "List or variable.", "?List2" : "List or variable.", "?List3" : "List or variable."],
	resat:"   Yes.",
	fail_if:"   Fails if List3 does not unify with the result of appending List2 to\n   List1.\n\n",
	eg:"\nSuccess:\n  append([1,2],L2,[1,2,3,4]). (gives L2=[3,4]).\n  append([1,B],L2,[A,2,3,4]). (gives B=2 L2=[3,4] A=1).\n  append([1,2],L2,L3).        (gives L2=L2 L3=[1,2|L2]).\n  append([1],[2,3],L3).     (gives L3=[1,2,3]).\n\n  [eclipse]: append(L1,L2,[1,2]), writeln((L1,L2)), fail.\n  [] , [1, 2]\n  [1] , [2]\n  [1, 2] , []\n  no (more) solution.\nFail:\n  append(L1,[3],[1,2,3,4]).\n  append(1,L2,[1,2]).\n\n\n",
	see_also:[union / 3]]).

:- comment(checklist / 2, [
	summary:"Succeeds if Pred(Elem) succeeds for every element of List.\n\n",
	amode:(checklist(+,+)),
	template:"checklist(+Pred, +List)",
	desc:html("\
   This is a synonym for maplist/2.
	"),
	args:["+Pred" : "Atom or compound term.", "+List" : "List."],
	resat:"Resatisfiable if at least for one element of List the invocation of Pred with this additional argument is resatisfiable.",
	fail_if:"Fails if at least for one element of List the invocation of Pred with this additional argument fails.",
	eg:"\nSuccess:\n  checklist(integer, [1, 3, 5]).\n  checklist(spy, [var/1, functor/3]).\n\nFail:\n  checklist(current_op(_, _), [+, -, =]).\n  (fails because the precedence of = does not match that of +)\n\n\n\n",
	see_also:[maplist / 3]]).

:- comment(maplist / 2, [
	summary:"Succeeds if Pred(Elem) succeeds for every element of List.\n\n",
	amode:(maplist(+,+)),
	template:"maplist(+Pred, +List)",
	desc:html("\
   maplist/3 succeeds if for every element of List, the invocation of
   Pred with one aditional argument which is this element succeeds.
<P>
   The definition of this Prolog library predicate is:
<PRE>
:- tool(maplist/2, maplist_body/3).

maplist_body(_, [], _).
maplist_body(Pred, [Head|Tail], Module) :-
    call(Pred, Head)@Module,
    maplist_body(Pred, Tail, Module).
</PRE>
   This predicate does not perform any type testing functions.
	"),
	args:["+Pred" : "Atom or compound term.", "+List" : "List."],
	resat:"Resatisfiable if at least for one element of List the invocation of Pred with this additional argument is resatisfiable.",
	fail_if:"Fails if at least for one element of List the invocation of Pred with this additional argument fails.",
	eg:"\nSuccess:\n  maplist(integer, [1, 3, 5]).\n  maplist(spy, [var/1, functor/3]).\n\nFail:\n  maplist(current_op(_, _), [+, -, =]).\n  (fails because the precedence of = does not match that of +)\n\n\n\n",
	see_also:[maplist / 3]]).

:- comment(flatten / 2, [
	summary:"Succeeds if FlatList is the list of all elements in NestedList, as found in\na left-to-right, depth-first traversal of NestedList.\n\n",
	amode:(flatten(+,-) is det),
	template:"flatten(+NestedList, ?FlatList)",
	desc:html("\
   FlatList is the list built from all the non-list elements of NestedList
   and the flattened sublists.  The sequence of elements in FlatList is
   determined by a left-to-right, depth-first traversal of NestedList.
<P>
   The definition of this Prolog library predicate is:
<PRE>
flatten(List, Flat) :-
	flatten_aux(List, Flat, []).

flatten_aux([], Res, Cont) :- -?-> !, Res = Cont.
flatten_aux([Head|Tail], Res, Cont) :-
	-?->
	!,
	flatten_aux(Head, Res, Cont1),
	flatten_aux(Tail, Cont1, Cont).
flatten_aux(Term, [Term|Cont], Cont).
</PRE>
   This predicate does not perform any type testing functions.
<P>
   Since ECLiPSe 7.0, arrays are treated like lists, and also flattened.
	"),
	args:["+NestedList" : "A List.", "?FlatList" : "List or variable."],
	resat:"   No.",
	fail_if:"   Fails if FlatList does not unify with the flattened version of\n   NestedList.\n\n",
	eg:"
    Success:
        [eclipse]: flatten([[1,2,[3,4],5],6,[7]], L).
        L = [1, 2, 3, 4, 5, 6, 7]
        yes.

        [eclipse]: flatten([1,2,3], L).
        L = [1, 2, 3]
        yes.

      [eclipse]: flatten(a, L).
      L = [a]
      yes.

    Fail:
        [eclipse]: flatten([1,[3],2], [1,2,3]).
        no.",
	see_also:[flatten / 3, sort / 2, sort / 4, length / 2, member / 2]]).

:- comment(flatten / 3, [
	summary:"Depth-limited list flattening",
	amode:(flatten(++,+,-) is det),
	template:"flatten(++MaxDepth, +NestedList, ?FlatList)",
	args:[
	    "++MaxDepth" : "Maximum depth to flatten.",
	    "+NestedList" : "List.",
	    "?FlatList" : "List or variable."
	],
	desc:html("\
   Like flatten/2, but does not flatten beyond the specified depth MaxDepth.
   So flatten(0, List, Flat) just unifies Flat and List (no flattening),
   flatten(1, List, Flat) just flattens the top-level list of List, etc.
<P>
   This predicate does not perform any type testing functions.
<P>
   Since ECLiPSe 7.0, arrays are treated like lists, and also flattened.
	"),
	resat:"   No.",
	fail_if:"   Fails if FlatList does not unify with the flattened version of\n   NestedList.\n\n",
	eg:"
   Success:
      [eclipse]: flatten(0, [[1,2,[3,4],5],6,[7]], L).
      L = [[1, 2, [3, 4], 5], 6, [7]]
      yes.
      [eclipse]: flatten(1, [[1,2,[3,4],5],6,[7]], L).
      L = [1, 2, [3, 4], 5, 6, 7]
      yes.
      [eclipse]: flatten(2, [[1,2,[3,4],5],6,[7]], L).
      L = [1, 2, 3, 4, 5, 6, 7]
      yes.
      [eclipse]: flatten(3, [[1,2,[3,4],5],6,[7]], L).
      L = [1, 2, 3, 4, 5, 6, 7]
      yes.

      [eclipse]: flatten(1, a, L).
      L = [a]
      yes.
      
   Fail:
      [eclipse]: flatten(2, [1,[3],2], [1,2,3]).
      no.
",
	see_also:[flatten / 2, sort / 2, sort / 4, length / 2, member / 2]]).

:- comment(maplist / 3, [
	summary:"Succeeds if Pred(Old, New) succeeds for corresponding pairs of elements\nfrom OldList and NewList.\n\n",
	amode:(maplist(+,+,-)),
	amode:(maplist(+,-,+)),
	template:"maplist(+Pred, ?OldList, ?NewList)",
	desc:html("\
   Either OldList or NewList should be a proper list.  maplist/3 succeeds
   if for every corresponding pair of elements Old, New of the two lists
   OldList and NewList the invocation of Pred with two aditional arguments
   Old and New succeeds.
<P>
   The definition of this Prolog library predicate is:
<PRE>
:- tool(maplist/3, maplist_body/4).

maplist_body(_, [], [], _).
maplist_body(Pred, [H1|T1], [H2|T2], Module) :-
    call(Pred, H1, H2)@Module,
    maplist_body(Pred, T1, T2, Module).
</PRE>
   This predicate does not perform any type testing functions.
	"),
	args:["+Pred" : "Atom or compound term.", "?OldList" : "List or variable.", "?NewList" : "List or variable."],
	resat:"Resatisfiable if at least for one pair of corresponding elements of OldList and NewList the invocation of Pred with these two additional arguments is resatisfiable",
	fail_if:"Fails if at least for one pair of corresponding elements of OldList and NewList the invocation of Pred with these two additional arguments fails",
	eg:"\nSuccess:\n  maplist(integer_atom, [1, 2, 3], ['1', '2', '3']).\n  maplist(sin, [0, 1, 2], X).\n      (gives X = [0.0, 0.841471, 0.909297])\n  maplist(get_flag(var/1), [skip, type, spy], [off, built_in, off]).\nFail:\n  maplist(type_of, [1, a, \"a\"], [integer, atom, atom]).\n\n\n\n",
	see_also:[maplist / 2]]).

:- comment(subset / 2, [
	summary:"Succeeds if List is the list which contains all elements from SubList in\nthe same order as in SubList.\n\n",
	amode:(subset(-,+) is multi),
	template:"subset(?SubList, +List)",
	desc:html("\
   Used to test if a specified list contains all elements of another list,
   or to generate all sublists of a given list.
<P>
   The definition of this Prolog library predicate is:
<PRE>
        subset([],[]).
        subset([X|L],[X|S]) :-
            subset(L,S).
        subset(L, [_|S]) :-
            subset(L,S).
</PRE>
   This predicate does not perform any type testing functions.
<P>
   This predicate works properly for set operations only, so repeated
   elements, variable elements and unsorted lists should not be used.
	"),
	args:["?SubList" : "A term which unifies with a list.", "+List" : "A term which unifies with a list."],
	resat:"   Yes.",
	fail_if:"   Fails if SubList does not unify with a list whose elements are all\n   contained in List in the same order as in SubList.\n\n",
	eg:"\nSuccess:\n      subset([1,3], [1,2,3]).\n      subset(X, [1,3,4]).        % backtracks over all subsets\n\nFail:\n      subset([2,1], [1,2,3]).   % different order\n\n\n\n",
	see_also:[union / 3, subtract / 3, intersection / 3]]).
