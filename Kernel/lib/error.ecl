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
% The Original Code is  The error library for ECLiPSe.
% The Initial Developer of the Original Code is  Coninfer Ltd.
% Portions created by the Initial Developer are
% Copyright (C) 2016 Coninfer Ltd
% 
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: error.ecl,v 1.3 2017/09/03 00:50:40 jschimpf Exp $
% ----------------------------------------------------------------------


:- module(error).

:- comment(categories, ["Programming Utilities"]).
:- comment(summary, "Utilities for argument type testing and error generation").
:- comment(author, "Joachim Schimpf").
:- comment(copyright, "Coninfer Ltd").
:- comment(date, '$Date: 2017/09/03 00:50:40 $').
:- comment(desc, html("<P>
    This library is modelled after SWI-Prolog's library(error) and provides
    facilities to simplify the raising of ISO-Prolog conforming exceptions,
    and testing argument types and domains.
</P>")).


:- comment(instantiation_error/1, [
    args:["Culprit":"The term that caused the error (ignored)"],
    summary:"throw error(instantiation_error, _)"
]).
:- export instantiation_error/1.
instantiation_error(_Culprit) :-
	throw(error(instantiation_error, _)).	% ISO: no culprit


:- comment(uninstantiation_error/1, [
    args:["Culprit":"The term that caused the error"],
    summary:"throw error(uninstantiation_error(Culprit), _)"
]).
:- export uninstantiation_error/1.
uninstantiation_error(Culprit) :-
	throw(error(uninstantiation_error(Culprit), _)).


:- comment(type_error/2, [
    args:["Type":"Specification of the expected type",
	  "Culprit":"The term that caused the error"],
    summary:"throw error(type_error(Type,Culprit), _)",
    eg:"type_error(list, [foo|bar])"
]).
:- export type_error/2.
type_error(Type, Culprit) :-
	throw(error(type_error(Type, Culprit), _)).


:- comment(domain_error/2, [
    args:["Domain":"Specification of the expected domain",
	  "Culprit":"The term that caused the error"],
    summary:"throw error(domain_error(Domain,Culprit), _)",
    eg:"domain_error(non_empty_list, [])"
]).
%     domain_error(non_empty_list, []).
:- export domain_error/2.
domain_error(Type, Culprit) :-
	throw(error(domain_error(Type, Culprit), _)).


:- comment(existence_error/2, [
    args:["ObjectType":"Specification of the expected object type",
	  "Culprit":"The non-existing object"],
    summary:"throw error(existence_error(ObjectType,Culprit), _)",
    eg:"existence_error(procedure, ex_nihilo/0)",
    desc:html("<P>
    ISO-defined values for ObjectType: procedure, source_sink, stream.
    </P>")
]).
:- export existence_error/2.
existence_error(Type, Culprit) :-
	throw(error(existence_error(Type, Culprit), _)).


:- comment(permission_error/3, [
    args:["Operation":"Name of the faild operation",
	  "Permission":"Name of the violated permission",
	  "Culprit":"The term that caused the problem"],
    summary:"throw error(permission_error(Operation,Permission,Culprit), _)",
    desc:html("<P>
    ISO-defined values for Operation: access, create, input, modify,
    open, output, reposition.
    ISO-defined values for Permission: binary_stream, flag, operator,
    past_end_of_stream, private_procedure, static_procedure, source_sink,
    stream, text_stream, open, output, reposition.
    </P>"),
    eg:"permission_error(open, source_sink, '/etc/shadow')"
]).
:- export permission_error/3.
permission_error(Operation, PermissionType, Culprit) :-
	throw(error(permission_error(Operation, PermissionType, Culprit), _)).


:- comment(representation_error/1, [
    args:["Reason":"Name of the restriction that caused the problem"],
    summary:"throw error(representation_error(Reason), _)",
    desc:html("<P>
    ISO-defined values for Reason: character, character_code,
    in_character_code, max_arity, max_integer, min_integer.
    </P>")
]).
:- export representation_error/1.
representation_error(Flag) :-
	throw(error(representation_error(Flag), _)).


:- comment(evaluation_error/1, [
    args:["Reason":"Name of the exceptional result"],
    summary:"throw error(evaluation_error(Reason), _)",
    desc:html("<P>
    ISO-defined values for Reason: float_overflow, int_overflow, undefined,
    underflow, zero_divisor.
    </P>")
]).
:- export evaluation_error/1.
evaluation_error(Error) :-
	throw(error(evaluation_error(Error), _)).


:- comment(resource_error/1, [
    args:["Resource":"Name of the exhausted resource"],
    summary:"throw error(resource_error(Resource), _)"
]).
:- export resource_error/1.
resource_error(Resource) :-
	throw(error(resource_error(Resource), _)).


%:- comment(syntax_error/1, [
%    args:["Description":"Description of the error"],
%    summary:"throw error(syntax_error(Description), _)"
%]).
%:- export syntax_error/1.
%syntax_error(Description) :-
%	throw(error(syntax_error(Description), _)).


%:- export system_error/0.
%system_error :-
%	throw(error(system_error, _)).


%
% must_be(+Type, ?Term)	is det [or exception]
%

:- comment(must_be/2, [
    summary:"Test a term for an expected type and throw an error if it does not match",
    amode:(must_be(+,?) is det),
    args:["Type":"The name or descriptor of the expected type",
	"Term":"The term to be tested"],
    see_also:[type_of/2],
    desc:html("<P>
    This predicate performs a runtime type test and throws an error if the
    test fails.  The supported type descriptors include:
    <DL>
    <DT>acyclic</DT><DD>
    	an acyclic term, see acyclic/1</DD>
    <DT>array</DT><DD>
    	an array, i.e. a structure with functor []/N</DD>
    <DT>atom</DT><DD>
    	an atom, see atom/1</DD>
    <DT>atomic</DT><DD>
    	not a compound term, see atomic/1</DD>
    <DT>between(L,H)</DT><DD>
    	number between L and H, must be integer if L,H are integer</DD>
    <DT>boolean</DT><DD>
    	one of the atoms 'true' or 'false'</DD>
    <DT>breal</DT><DD>
    	a bounded real, see breal/1</DD>
    <DT>callable</DT><DD>
    	an atom or compound term, see callable/1</DD>
    <DT>char</DT><DD>
    	a single-character atom</DD>
    <DT>chars</DT><DD>
    	a list of single-character atoms</DD>
    <DT>code</DT><DD>
    	an integer character code</DD>
    <DT>codes</DT><DD>
    	a list of integer character codes</DD>
    <DT>compound</DT><DD>
    	a compound term (including list and array)</DD>
    <DT>cyclic</DT><DD>
    	a cyclic term, see cyclic/1</DD>
    <DT>encoding</DT><DD>
    	an atom specifying a character encoding, such as 'iso_latin_1'</DD>
    <DT>float</DT><DD>
    	a floating point number, see float/2</DD>
    <DT>ground</DT><DD>
    	a ground (i.e. fully instantiated) term, see ground/1</DD>
    <DT>handle</DT><DD>
    	a handle, see is_handle/1</DD>
    <DT>handle(Class)</DT><DD>
    	a handle of the given class, see is_handle/2</DD>
    <DT>list</DT><DD>
    	a proper list</DD>
    <DT>list(Type)</DT><DD>
    	a proper list with elements of Type</DD>
    <DT>list_or_partial_list</DT><DD>
    	a proper or partial (i.e. ending in a variable) list</DD>
    <DT>negative_integer</DT><DD>
    	integer less than zero</DD>
    <DT>nonneg</DT><DD>
    	integer greater or equal to zero</DD>
    <DT>nonvar</DT><DD>
	any non-variables, see nonvar/1</DD>
    <DT>number</DT><DD>
    	any number, see number/1</DD>
    <DT>oneof(Values)</DT><DD>
    	a member of the list Values</DD>
    <DT>positive_integer</DT><DD>
    	integer greater than zero</DD>
    <DT>rational</DT><DD>
    	a rational number, see rational/1</DD>
    <DT>real</DT><DD>
    	a float or breal number, see real/1</DD>
    <DT>string</DT><DD>
    	a string, see string/1</DD>
    <DT>text</DT><DD>
    	string, atom, chars or codes</DD>
    <DT>var</DT><DD>
    	an (uninstantiated) variable</DD>
    </DL>
    Many of these correspond to a simple type testing predicate. Note that
    tests that require traversal of compound terms (such as 'cyclic' or
    'list') can be expensive.
    </P><P>
    If Term does not satisfy the expected type, one of the following
    error terms is thrown, as appropriate:
    <DL>
    <DT>error(instantiation_error, _)</DT><DD>
    	an variable was detected where an instantiated term was expected</DD>
    <DT>error(uninstantiation_error(Term), _)</DT><DD>
    	an instantiated term was detected where a variable was expected</DD>
    <DT>error(type_error(Type,Term), _)</DT><DD>
    	Term is not of type Type</DD>
    <DT>error(domain_error(Type,Term), _)</DT><DD>
    	Term is of the right general type but has an unacceptable value</DD>
    </DL>
    A domain error is used instead of a type error when a number is
    outside an expected range, or an atom is not in the expected set.
    </P><P>
    If Type is not one of the recognized types, it is assumed that the type
    test failed, and one of the following error terms is thrown:
    <DL>
    <DT>error(instantiation_error, _)</DT><DD>
    	if Term was a variable</DD>
    <DT>error(type_error(Type,Term), _)</DT><DD>
    	otherwise</DD>
    </DL>
    </P>")
]).
:- export must_be/2.
must_be(Type, _) :- var(Type), !, instantiation_error(Type).
must_be(array, X) :- is_array(X), !.
must_be(atom, X) :- atom(X), !.
must_be(atomic, X) :- atomic(X), !.
must_be(boolean, X) :- (X==false;X==true), !.
must_be(breal, X) :- breal(X), !.
must_be(callable, X) :- callable(X), !.
must_be(compound, X) :- compound(X), !.
must_be(encoding, X) :- atom(X), encoding(X), !.
must_be(float, X) :- float(X), !.
must_be(ground, X) :- ground(X), !.
must_be(handle, X) :- is_handle(X), !.
must_be(handle(Class), X) :- atom(Class), is_handle(X, Class), !.
must_be(integer, X) :- integer(X), !.
must_be(nonvar, X) :- nonvar(X), !.
must_be(number, X) :- number(X), !.
must_be(rational, X) :- rational(X), !.
must_be(real, X) :- real(X), !.
must_be(string, X) :- string(X), !.
must_be(between(L,H), X) :- ( integer(L) -> integer(X) ; number(X) ), !,
	( L=<X, X=<H -> true ; domain_error(between(L,H), X) ).
must_be(nonneg, X) :- integer(X), !,
	( X>=0 -> true ; domain_error(nonneg, X) ).
must_be(negative_integer, X) :- integer(X), !,
	( X<0 -> true ; domain_error(negative_integer, X) ).
must_be(positive_integer, X) :- integer(X), !,
	( X>0 -> true ; domain_error(positive_integer, X) ).
must_be(acyclic, X) :- !,
	( acyclic_term(X) -> true ; domain_error(acyclic, X) ).
must_be(cyclic, X) :- !,
	( acyclic_term(X) -> domain_error(cyclic, X) ; true ).
must_be(oneof(Xs), X) :-
	oneof(Xs, X), !.
must_be(char, X) :- atom(X), !,
	( atom_length(X, 1) -> true ; domain_error(char, X) ).
must_be(code, X) :- integer(X), !,
	( 0=<X, X=<16'10ffff -> true ; domain_error(code, X) ).
must_be(chars, X) :- !,
	must_be_list(char, X).
must_be(codes, X) :- !,
	must_be_list(code, X).
must_be(list, X) :- !,
	is_list(X).
must_be(list(Type), X) :- !,
	must_be_list(Type, X).
must_be(list_or_partial_list, X) :-
	sepia_kernel:list_end(X,T), (var(T);T==[]), !.
must_be(text, X) :- !,
	must_be_text(X).
must_be(var, X) :- !,
	( var(X) -> true ; uninstantiation_error(X) ).
must_be(Type, X) :-
	% fail here for simple alternative instantiation/type error
	( var(X) -> instantiation_error(X) ; type_error(Type, X) ).

must_be_list(_Type, Xs) :- var(Xs), !, instantiation_error(Xs).
must_be_list(_Type, []) :- !.
must_be_list(Type, [X|Xs]) :- !, must_be(Type, X), must_be_list(Type, Xs).
must_be_list(Type, Xs) :- type_error(list(Type), Xs).
	
oneof([Y|Ys], X) ?- ( X==Y -> true ; oneof(Ys, X) ).

must_be_text(X) :- var(X), !, instantiation_error(X).
must_be_text(X) :- atom(X).	% includes []
must_be_text(X) :- string(X).
must_be_text(Cs) :- Cs = [C|_], !,
	( integer(C) -> must_be_list(code, Cs) ; must_be_list(char, Cs) ).
must_be_text(X) :- type_error(text, X).

encoding(octet).
encoding(ascii).
encoding(iso_latin_1).

