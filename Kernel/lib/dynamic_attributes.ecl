% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright:	This file is in the public domain
% Version:	$Id: dynamic_attributes.ecl,v 1.1 2016/08/12 10:57:29 jschimpf Exp $
% Description:	SWI/hProlog-style variable attribute interface
%
% Implementation alternatives:
%  - either 1-to-1 mapping ECLiPSe attribute to SWI attribute
%  - or all SWI-attributes in one ECLiPSe attribute (done here)
% ----------------------------------------------------------------------

:- module(dynamic_attributes).

:- comment(categories, ["Compatibility"]).
:- comment(summary, "SWI/hProlog-style variable attribute interface").
:- comment(author, "Joachim Schimpf, Coninfer Ltd").
:- comment(copyright, "Public domain").
:- comment(date, "$Date: 2016/08/12 10:57:29 $").
:- comment(see_also, [meta_attribute/2,library(atts),library(swi)]).
:- comment(desc, html("<P>
    This library provides variable attributes in the style of Demoen
    (Bart Demoen: Dynamic attributes, their hProlog implementation,
    and a first evaluation. Report CW 350, Department of Computer Science,
    K.U.Leuven, Leuven, Belgium, Oct 2002), and aims specifically at
    compatibility with their implementation in SWI-Prolog.
</P><P>
    The designation 'dynamic attributes' indicates that these attribute
    names do not have to be declared, and attributes can be freely added
    and removed.  The implementation consists of a thin layer on top of
    ECLiPSe's native variable attributes: they are stored in a hash table
    attached to a variable as the ECLiPSe attribute 'dynamic_attributes'.
</P><P>
    Dynamic attributes can be freely mixed with ECLiPSe's native attributes.
</P>
")).

% TODO
% - copy_term handler could use attribute_goals interface

:- lib(hash).

:- meta_attribute(dynamic_attributes, [
	unify:unify_attrs/2,
	print:printed_attrs/2
   ]).


%:- export unify_attrs/2.
unify_attrs(_Term, Attrs) :-
	var(Attrs).
unify_attrs(Term, Attrs) :-
	nonvar(Attrs),
	hash_list(Attrs, Ms, Vs),
	( foreach(M,Ms), foreach(V,Vs), param(Term) do
	    ( current_module(M) ->
		M:attr_unify_hook(V, Term)
	    ;
		true
	    )
	).
	

%:- export printed_attrs/2.
printed_attrs(X, Printed) :-
	attributes_goals(X, Printed, []).


% Like meta/1, attvar/1 isn't really useful, since a variable may
% always have attributes that the caller doesn't know about.
:- export attvar/1.
:- comment(attvar/1, [
    summary:"Term is a variable with at least one dynamic attribute",
    amode:(attvar(?) is semidet),
    args:["Term":"term to be tested"]
    ]).
attvar(_{A}) ?- nonvar(A), hash_count(A, 0).
% alternatively
%attvar(X) :- meta(X).

:- comment(put_attr/3, [
    summary:"Add or replace a dynamic attribute",
    amode:(put_attr(-,+,?) is det),
    args:["Var":"variable",
    	"Name":"attribute name (atom)",
	"Value":"attribute value"]
    ]).
:- export put_attr/3.
put_attr(_{A}, Module, Value) ?- !,
	( var(A) -> hash_create(A) ; true ),
	hash_set(A, Module, Value).
put_attr(X, Module, Value) :- var(X), !,
	hash_create(A),
	hash_set(A, Module, Value),
	add_attribute(X, A).
put_attr(X, _Module, _Value) :-
	throw(error(uninstantiation_error(X), put_attr/3)).

:- comment(put_attrs/2, [
    summary:"Add or replace dynamic attributes",
    amode:(put_attrs(-,+) is det),
    args:["Var":"variable",
    	"Attrs":"Nested term att(Name,Value,MoreAttrs) or []"]
    ]).
:- export put_attrs/2.
put_attrs(_{A}, As) ?- !,
	( var(A) -> hash_create(A) ; true ),
	put_attrs_hash(A, As).
put_attrs(X, As) :- var(X), !,
	hash_create(A),
	add_attribute(X, A),
	put_attrs_hash(A, As).
put_attrs(X, _As) :-
	throw(error(uninstantiation_error(X), put_attrs/2)).

    put_attrs_hash(_A, []) :- !.
    put_attrs_hash(A, att(M,V,As)) :-
	hash_set(A, M, V),
	put_attrs_hash(A, As).


:- comment(get_attr/3, [
    summary:"Retrieve a dynamic attribute",
    amode:(get_attr(-,+,-) is semidet),
    args:["Var":"variable",
    	"Name":"attribute name (atom)",
	"Value":"attribute value"]
    ]).
:- export get_attr/3.
get_attr(_{A}, Module, Value) ?-
	nonvar(A),
	hash_get(A, Module, Value).
	
:- comment(get_attrs/2, [
    summary:"Get all dynamic attributes",
    amode:(get_attrs(-,-) is det),
    args:["Var":"variable",
    	"Attrs":"Nested term att(Name,Value,MoreAttrs) or []"]
    ]).
:- export get_attrs/2.
get_attrs(_{A}, As) ?-
	nonvar(A),
	hash_list(A, Ms, Vs),
	( foreach(M,Ms), foreach(V,Vs), fromto(As,att(M,V,As1),As1,[]) do
	    true
	).
	

:- comment(del_attr/2, [
    summary:"Delete a dynamic attribute",
    amode:(del_attr(?,+) is det),
    args:["Var":"variable or ",
    	"Name":"attribute name (atom)"]
    ]).
:- export del_attr/2.
del_attr(_{A}, Module) ?- !,
	( var(A) -> true ; hash_delete(A, Module) ).
del_attr(_X, _Module).

:- comment(del_attrs/1, [
    summary:"Delete all dynamic attributes",
    amode:(del_attrs(?) is det),
    args:["Var":"variable"]
    ]).
:- export del_attrs/1.
del_attrs(_{A}) ?- !,
	( var(A) -> true ; hash_erase(A) ).
del_attrs(_X).


:- export copy_term/3.	% replaces ECLiPSe's built-in...
:- comment(copy_term/3, [
    summary:"Compatibility: SWI/SICStus-Prolog term copy",
    amode:(copy_term(?,-,-) is det),
    args:["Term":"any term",
    	"Copy":"copy of term with plain variables",
	"Goals":"Goals to construct Copy's attributes"],
    see_also:[_:copy_term/3],
    desc:html("
    CAUTION: this predicate shares its name with ECLiPSe's copy_term/3
    built-in.  Use module-qualification or import-from to disambiguate.")
    ]).
copy_term(Term, Copy, Goals) :-
	eclipse_language:copy_term(Term, Copy, Metas),
	(
	    foreach([X,XC],Metas),
	    fromto(Gs,Gs2,Gs1,[]),
	    foreach(X,Xs),
	    foreach(XC,XCs)
	do
	    attributes_goals(X, Gs2, Gs1)
	),
	% this is what SWI implements:
	copy_term(Xs-Gs, XCs-Goals).
	% but it probably makes more sense not to copy free variables in Goals:
	%copy_term_vars(Term, Xs-Gs, XCs-Goals).

    attributes_goals(X, Gs, Gs0) ?-
    	( get_attrs(X, Attrs) ->
	    (
		fromto(Attrs, att(Module,Value,Attrs1), Attrs1, []),
		fromto(Gs,Gs2,Gs1,Gs0),
		param(X)
	    do
		(
		    current_module(Module),
		    current_predicate(attribute_goals/3)@Module,
		    Module:attribute_goals(X, Gs2, Gs1)
		->
		    true
		;
		    Gs2 = [put_attr(X,Module,Value)|Gs1]
		)
	    )
	;
	    Gs = Gs0
	).
    	

:- export copy_term_nat/2.
:- comment(copy_term_nat/2, [
    summary:"Copy term without variable attributes",
    amode:(copy_term_nat(?,-) is det),
    args:["Term":"any term",
    	"Copy":"copy of term with plain variables"],
    see_also:[_:copy_term/3],
    desc:html("
    This is equivant to
<PRE>
    copy_term_nat(Term, Copy) :-
        eclipse_language:copy_term(Term, Copy, _AttVars).
</PRE>
    ")
    ]).
copy_term_nat(Term, Copy) :-
	eclipse_language:copy_term(Term, Copy, _Metas).
	

:- export term_attvars/2.
term_attvars(Term, Vs) :-
	term_variables(Term, Xs),
	( foreach(X,Xs), fromto(Vs,Vs1,Vs2,[]) do
	    ( attvar(X) -> Vs1=[X|Vs2] ; Vs1=Vs2 )
	).

