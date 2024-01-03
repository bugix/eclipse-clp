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
% The Original Code is  The eclipse_6 library for ECLiPSe.
% The Initial Developer of the Original Code is  Coninfer Ltd.
% Portions created by the Initial Developer are
% Copyright (C) 2016.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------

:- module(eclipse_6).

:- comment(categories, ["Compatibility"]).
:- comment(summary, "Compatibility definitions for ECLiPSe 6.X").
:- comment(author, "Joachim Schimpf").
:- comment(copyright, "Joachim Schimpf, Coninfer Ltd").
:- comment(date, "$Date: 2017/08/01 13:34:01 $").
:- comment(desc, html("<P>
    This library is meant to help with problems that might occur when
    migrating an application from ECLiPSe 6.x to 7.x by reverting some
    changes that were introduced.  It provides:
<UL>
    <LI>a version of term_variables/2 with the old behaviour</LI>
    <LI>select/3 as an alias for stream_select/3</LI>
    <LI>the old parser behaviour for bignums in radix notation</LI>
    <LI>the right quote character as a symbol rather than a quote</LI>
</UL>
    The library can be loaded at the beginning of a source module using
    <PRE>
    :- lib(eclipse_6).
    </PRE>
    It should only be used as a temporary solution, please update your
    application code, or copy the specific workaround from the source
    of this library.
</P>")).

%----------------------------------------------------------------------

:- ensure_loaded(library(lists)).

% the default was changed in ECLiPSe 7.0
:- export syntax_option(not based_bignums).

% right quote is list_quote since ECLiPSe 7.0
:- export chtab(0'`, symbol).

% Returns variable list in reverse order
:- export term_variables/2.
term_variables(T, Vs) :-
	sepia_kernel:term_variables_reverse(T, Vs).


% Obsolete alias for stream_select/3
:- export select/3.
select(Streams, Timeout, Ready) :-
	stream_select(Streams, Timeout, Ready).


%
% subscript(+Matrix, +IndexList, ?Element)
% Differs from later version in that it returns nested lists instead
% of nested arrays for dim(M,[4,4]), subscript(M, [2..3,2..3], R)

:- export subscript/3.
:- tool(subscript/3, subscript/4).

subscript(Mat, Index, X, M) :-
	var(Index), !,
	( get_flag(coroutine,on) ->
	    suspend(subscript(Mat, Index, X, M), 2, Index->inst)
	;
	    error(4, subscript(Mat,Index,X), M)
	).
subscript(Mat, [], X, _M) :- !, X = Mat.
subscript(Mat, [IExpr|IExprs], X, M) :- !,
	subscript3(Mat, IExpr, X, M, IExprs).
subscript(Mat, Index, X, M) :-
	error(5, subscript(Mat,Index,X), M).

    subscript3(Mat, IExpr, X, M, IExprs) :-
	var(Mat), !,
	( get_flag(coroutine,on) ->
	    suspend(subscript(Mat,[IExpr|IExprs],X,M), 2, Mat->inst)
	;
	    error(4, subscript(Mat,[IExpr|IExprs],X), M)
	).
    subscript3(Mat, IExpr, X, M, IExprs) :-
	compound(Mat), !,
	subscript1(Mat, IExpr, X, M, IExprs).
    subscript3(Mat, IExpr, X, M, IExprs) :-
	is_handle(Mat), !,
	( IExprs = [] ->
	    eval(IExpr, I)@M,
	    xget(Mat, I, X)
	;
	    error(6, subscript(Mat,[IExpr|IExprs],X), M)
	).
    subscript3(Mat, IExpr, X, M, IExprs) :-
	string(Mat), !,
	( IExprs = [] ->
	    eval(IExpr, I)@M,
	    string_code(Mat, I, X)
	;
	    error(6, subscript(Mat,[IExpr|IExprs],X), M)
	).
    subscript3(Mat, IExpr, X, M, IExprs) :-
	error(5, subscript(Mat,[IExpr|IExprs],X), M).

    subscript1(Mat, IExpr, X, M, IExprs) :- integer(IExpr), !,
	arg(IExpr, Mat, Row),
	subscript(Row, IExprs, X, M).
    subscript1(Mat, Min..Max, Xs, M, IExprs) :- -?-> !,
	eval(Min, Imin)@M,
	eval(Max, Imax)@M,
	subscript2(Imin, Imax, Mat, IExprs, Xs, M).
    subscript1(Mat, IExpr, X, M, IExprs) :-
	eval(IExpr, I)@M,
	arg(I, Mat, Row),
	subscript(Row, IExprs, X, M).

    subscript2(Imin, Imax, Mat, IExprs, Xs, M) :-
	( Imin =< Imax ->
	    Xs = [X|Xs0],
	    +(Imin, 1, Imin1),
	    arg(Imin, Mat, Row),
	    subscript(Row, IExprs, X, M),
	    subscript2(Imin1, Imax, Mat, IExprs, Xs0, M)
	;
	    Xs = []
	).


% Original implementation from release 6.1 (current one in lib(lists)).
% This old version has some peculiarities that we don't want to replicate
% in the new version, that's why we supply it here in its original form.
% It also uses the above old version of subscript/3, returning
% nested lists for nested subranges.

:- export collection_to_list/2.

collection_to_list(Collection, _List) :-
	var(Collection),
	!,
	% Instantiation fault.
	fail.
collection_to_list([], List) :-
	!,
	List = [].
collection_to_list(Collection, List) :-
	Collection = [_|_],
	!,
	% It's already a list (or at least, looks that way...)
	List = Collection.
collection_to_list(subscript(Array, Indices), List) :-
	!,
	eclipse_6:subscript(Array, Indices, List0, lists),
	( nonvar(List0), List0 =[_|_] ->
	    List = List0
	;
	    % The subscript only returned a single item that wasn't a list.
	    % We need to return a list, and we need to support this case, so
	    % we make it a singleton list, despite this making the behaviour
	    % somewhat inconsistent (we're adjusting the depth of the result
	    % for this particular case only) - this is hard to avoid without
	    % writing an alternate implementation of subscript/3.
	    List = [List0]
	).
collection_to_list(flatten(Collection), List) :-
	!,
	collection_to_list(Collection, Lists),
	flatten(Lists, List).
collection_to_list(flatten(N, Collection), List) :-
	!,
	collection_to_list(Collection, Lists),
	lists:flatten(N, Lists, List).
collection_to_list(Collection, List) :-
	functor(Collection, [], _Arity),
%	!,
	% It's an array
	flatten_array(Collection, List).
%collection_to_list(Collection, List) :-
%	% Type error.
%	fail.

/*
:- comment(collection_to_list/2, [
    summary:"Convert a \"collection\" into a list (DEPRECATED)",
    amode:(collection_to_list(+,-) is det),
    template:"collection_to_list(+Collection, ?List)",
    desc:html("<P>
   Do not use this predicate, it has been superseded by collection_to_list/3.
<P>
   Converts various \"collection\" data structures into a list.  Fails if it
   does not know how to do this.  The supported collection types are:
<DL>
   <DT>List<DD>
	The list is returned unchanged.
   <DT>Array<DD>
	The array is flattened into a list using flatten_array/2.
   <DT>Subscript reference (Array[...])<DD>
	subscript/3 is called to evaluate the subscript reference.  Note
	that the result is not flattened, so if subscript/3 results in a
	nested list, the result is a nested list.  However if the result is
	a single element of the array that is not a list, this is converted
	into a list containing that element.
   <DT>flatten(Collection)<DD>
	Calls collection_to_list/2 on Collection and then flattens the
	result using flatten/2.
   <DT>flatten(MaxDepth, Collection)<DD>
	Calls collection_to_list/2 on Collection and then flattens the
	result up to MaxDepth levels of nesting using flatten/3.
</DL>
"),
    eg:"\
   ?- collection_to_list([a,b,[c,d]], List).
   List = [a, b, [c, d]]
   Yes
   ?- collection_to_list([](a,b,[c,d]), List).
   List = [a, b, [c, d]]
   Yes
   ?- collection_to_list([]([](a,b),[](c,d)), List).
   List = [a, b, c, d]
   Yes
   ?- A = []([](a,b),[](c,d)),
      collection_to_list(A[1..2,1], List).
   List = [a, c]
   Yes
   ?- A = []([](a,b,c),[](d,e,f)),
      collection_to_list(A[1..2,2..3], List).
   List = [[b, c], [e, f]]
   Yes
   ?- collection_to_list(flatten([a,b,[c,d]]), List).
   List = [a, b, c, d]
   Yes
   ?- collection_to_list(flatten([](a,b,[c,d])), List).
   List = [a, b, c, d]
   Yes
   ?- A = []([](a,b,c),[](d,e,f)),
      collection_to_list(flatten(A[1..2,2..3]), List).
   List = [b, c, e, f]
   Yes
   ?- L = [[a,b],[[c,d],[e,f]],g],
      collection_to_list(flatten(1, L), List).
   List = [a, b, [c, d], [e, f], g]
   Yes
",
    see_also:[collection_to_list/3]]).
*/
