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
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: iso_light.ecl,v 1.12 2017/08/24 14:37:08 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% $Id: iso_light.ecl,v 1.12 2017/08/24 14:37:08 jschimpf Exp $
%
% IDENTIFICATION:	iso_light.ecl, based on obsolete iso.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		see export directives
%
% DESCRIPTION:		ISO Prolog compatibility package. It follows
%			* ISO/IEC 13211-1 (1995)
%			* ISO/IEC 13211-1 TC1 (2007)
%			* ISO/IEC 13211-1 TC2 (2012)
%			* ISO/IEC 13211-1 TC3 (2017)
%			This module contains ISO-only built-ins, and the
%			redefinitions necessary to get basic ISO semantics.
%			This is meant to be used as a language-module, so
%			do not export anything unwanted!
%

:- module(iso_light).

:- pragma(system).	% mark as built-ins

% Workaround: make tools (which don't obey the system directive) into built-ins
:- local initialization((
    current_module_predicate(exported,P), set_flag(P,type,built_in), fail ; true
)).

% suppress deprecation warnings for reexported builtins
:- pragma(deprecated_warnings(not_reexports)).

:- export
	op(0,   fy, abolish),			% likely to cause trouble
	op(0,   fy, dynamic).

:- export
	chtab(0'`, string_quote),
	chtab(0'", list_quote).

:- comment(categories, [`Compatibility`]).
:- comment(summary, `ISO Prolog compatibility library (light version)`).
:- comment(author, `Joachim Schimpf, Coninfer Ltd`).
:- comment(copyright, `Cisco Systems, Inc (2006), Coninfer Ltd (modifications 2007-2017)`).
:- comment(date, `$Date: 2017/08/24 14:37:08 $`).
:- comment(see_also, [library(multifile),library(iso_strict),library(iso)]).
:- comment(desc, html(`\
<h3>Overview</h3>
    This library provides the essential features of ISO-Prolog (ISO/IEC
    13211-1) without aiming for full conformance, in particular with respect
    to error handling.
    <P>
    This is one of three libraries providing different degrees of ISO
    compatibility:
    <DL>
    <DT><STRONG>iso_strict</STRONG></DT>
        <DD>Aims to be fully ISO compliant, and represents the
        'strict mode' required by the standard.</DD>
    <DT><STRONG>iso</STRONG></DT>
        <DD>Aims to be fully ISO compliant, but also includes
        ECLiPSe functionality that does not conflict with the standard.
    <DT><STRONG>iso_light</STRONG></DT>
        <DD>Provides the essence of ISO features without aiming for full
        conformance (in particular with respect to error handling), and
        may include ECLiPSe extensions that go beyond what the letter of
        the standard allows.  Also useful for importing individual ISO
        features into a program that is otherwise written in ECLiPSe dialect.
    </DL>
    <P>
<h3>Usage as a language-module</h3>
    The effect of this compatibility library is (with minor exceptions)
    local to the module where it is loaded.  An program written in this
    dialect should always be contained in a separate module, starting with
    a directive like
    <PRE>
    :- module(myisomodule, [], iso_light).
    </PRE>
    Here, the last argument of the module/3 directive indicates the language.
    It is not advisable to use ":-lib(iso_light)" or ":-ensure_loaded(library(iso_light))"
    within an eclipse_language module, because this would lead to import
    conflicts between the different versions of built-in predicates.
    <P>
    Alternatively, in order to use ISO-Prolog without having different
    modules, one can invoke eclipse with a "-L iso_light" command line option,
    or set the ECLIPSEDEFFAULTLANGUAGE environment variable to 'iso_light'.
    This will launch eclipse with a default module accepting 'iso_light' language
    instead of the usual 'eclipse_language'.
    <P>
<h3>Using individual features</h3>
    It is possible and meaningful to import only individual predicates
    from this library, e.g.
    <PRE>
    :- import sub_atom/5 from iso_light.
    </PRE>
    This will only make sub_atom/5 available, without affecting syntax
    settings in the importing module.
    <P>
<h3>Differences</h3>
    The main differences of the iso_light language compared to the
    default eclipse_language are the following:
    <UL>
    <LI>The syntax is more restricted, and ISO style is preferred, e.g.
        for writing hexadecimal numbers.</LI>
    <LI>Double quotes denote character lists, while in ECLiPSe they denote
        string constants.  ECLiPSe-strings can however be written using
        back-quotes, e.g. \`abc\`.</LI>
    <LI>Arithmetic functions like floor/1 return integer results.</LI>
    <LI>All ISO built-in predicates are available.</LI>
    </UL>
`)).

:- export
	syntax_option(dense_output),
	syntax_option(not(nl_in_quotes)),
	syntax_option(iso_escapes),
	syntax_option(iso_base_prefix),
	syntax_option(iso_restrictions),
	syntax_option(no_string_concatenation),
	syntax_option(eof_is_no_fullstop),
	syntax_option(not(syntax_errors_fail)),
	syntax_option(plus_is_no_sign),
	syntax_option(doubled_quote_is_quote),
%	syntax_option(no_array_subscripts),	% allow extension
	syntax_option(bar_is_no_atom),
%	syntax_option(no_attributes),		% allow extension
%	syntax_option(no_curly_arguments),	% allow extension
	syntax_option(blanks_after_sign),
	syntax_option(float_needs_point),
	syntax_option(limit_arg_precedence).

:- export
	op(200, xfx, (**)).

:- reexport eclipse_language except

	floor/2,			% these have different behaviour
	ceiling/2,
	current_op/3,
	round/2,
	truncate/2,
	(^)/2,				% for bagof semantics
	(^)/3,
	(is)/2,
	(>)/2,
	(<)/2,
	(>=)/2,
	(=<)/2,
	(=:=)/2,
	(=\=)/2,
	abolish/1,
	asserta/1,
	assertz/1,
	atom_length/2,
	clause/2,
	get_char/1,
	get_char/2,
	put_char/1,
	put_char/2,
	retract/1.

:- export
	abolish/1,
	asserta/1,
	assertz/1,
	at_end_of_stream/0,
	at_end_of_stream/1,
	atom_concat/3,
	atom_codes/2,
	atom_chars/2,
	atom_length/2,
	char_conversion/2,
	clause/2,
	current_char_conversion/2,
	current_input/1,
	current_output/1,
	current_prolog_flag/2,
	flush_output/0,
	flush_output/1,
	get_byte/1,
	get_byte/2,
	get_char/1,
	get_char/2,
	get_code/1,
	get_code/2,
	halt/1,
	initialization/1,
	number_chars/2,
	number_codes/2,
	peek_byte/1,
	peek_byte/2,
	peek_char/1,
	peek_char/2,
	peek_code/1,
	peek_code/2,
	put_byte/1,
	put_byte/2,
	put_char/1,
	put_char/2,
	put_code/1,
	put_code/2,
	retract/1,
	set_input/1,
	set_output/1,
	set_prolog_flag/2,
	set_stream_position/2,
	stream_property/2,
	sub_atom/5,
	subsumes_term/2,
	unify_with_occurs_check/2.

:- tool(initialization/1, initialization/2).

:- export (is)/2.
:- tool((is)/2, is_/3).

:- export (>=)/2, (>)/2, (=<)/2, (<)/2, (=:=)/2, (=\=)/2.
:- tool((<)/2, (<)/3),
   tool((>)/2, (>)/3),
   tool((=<)/2, (=<)/3),
   tool((>=)/2, (>=)/3),
   tool((=:=)/2, (=:=)/3),
   tool((=\=)/2, (=\=)/3).

:- use_module(iso_aux).

:- import
	check_callable/1,
	check_predspec/1,
	bip_error/1,
	set_bip_error/1
    from sepia_kernel.

%-----------------------------------------------------------------------
% 7.4 Directives
%-----------------------------------------------------------------------

:- reexport multifile.

initialization(Goal, Module) :-
	local(initialization(Goal))@Module.


%-----------------------------------------------------------------------
% 8.2 Term Unification
%-----------------------------------------------------------------------

unify_with_occurs_check(X, Y) :-		% 8.2.2
	( var(X) ->
            ( var(Y) ->
                X=Y
            ;
                \+occurs(X,Y),
                X=Y
            )
	; var(Y) ->
	    \+occurs(Y,X),
	    X=Y
	; compound(X) ->
	    functor(X, F, A),
	    functor(Y, F, A),
	    ( for(I,1,A), param(X,Y) do
		arg(I, X, AX),
		arg(I, Y, AY),
		unify_with_occurs_check(AX, AY)
	    )
	;
	    X=Y
	).


subsumes_term(General, Specific) :-
	\+ \+ (sepia_kernel:setuniv(Specific), General=Specific).


%-----------------------------------------------------------------------
% 8.9 Clause creation and destruction
% The redefinitions of assert are needed to raise the correct errors
% and to wrap variable subgoals into call/1.
%-----------------------------------------------------------------------

% don't retract all on a subsequent dynamic/1 declaration
:- set_event_handler(64, true/0).

:- tool(abolish/1, abolish_/2).			% 8.9.4
abolish_(Pred, Module) :-
	( check_predspec(Pred) ->
	    ( get_flag(Pred, stability, Stability)@Module,
	      get_flag(Pred, defined, on)@Module ->
		( Stability == (dynamic) ->
		    % Pred may still be imported and cause an error!
		    eclipse_language:abolish(Pred)@Module
		;
		    error(63, abolish(Pred))@Module
		)
	    ;
		true	% no such predicate (or no clauses)
	    )
	;
	    bip_error(abolish(Pred))@Module
	).


:- tool(asserta/1, asserta_/2).
asserta_(Clause, Module) :-
	( normalize_clause(Clause, NormClause) ->
	    eclipse_language:asserta(NormClause)@Module
	;
	    bip_error(asserta(Clause))@Module
	).

:- tool(assertz/1, assertz_/2).
assertz_(Clause, Module) :-
	( normalize_clause(Clause, NormClause) ->
	    eclipse_language:assertz(NormClause)@Module
	;
	    bip_error(assertz(Clause))@Module
	).

:- tool(retract/1, retract_/2).
retract_(Clause, Module) :-
	( Clause=(Head:-_) -> true ; Clause=Head ),
	( callable(Head) -> eclipse_language:retract(Clause)@Module
	; var(Head) -> error(4, retract(Clause))@Module
	; error(5, retract(Clause))@Module
	).

    :- mode normalize_clause(?,-).
    normalize_clause((Head:-Body), (Head:-NormBody)) :- !,
	check_callable(Head),
	( normalize_body(Body, NormBody) -> true ; set_bip_error(5) ).
    normalize_clause(Head, (Head:-true)) :-
	check_callable(Head).


:- tool(clause/2, clause_/3).
clause_(Head, Body, Module) :-
	( var(Body) -> eclipse_language:clause(Head, Body)@Module
	; callable(Body) -> eclipse_language:clause(Head, Body)@Module
	; error(5, clause(Head, Body))@Module
	).


%-----------------------------------------------------------------------
% 8.11 Stream selection and control
%-----------------------------------------------------------------------

current_input(Stream) :-			% 8.11.1
	( var(Stream) -> get_stream(input, Stream)
	; is_handle(Stream) -> get_stream(input, Stream)
	; error(6, current_input(Stream))
	).


current_output(Stream) :-			% 8.11.2
	( var(Stream) -> get_stream(output, Stream)
	; is_handle(Stream) -> get_stream(output, Stream)
	; error(6, current_output(Stream))
	).


set_input(Stream) :-				% 8.11.3
	check_stream_or_alias_io_type(Stream, input, _any),
	!,
	set_stream(input, Stream).
set_input(Stream) :-
	bip_error(set_input(Stream)).


set_output(Stream) :-				% 8.11.4
	check_stream_or_alias_io_type(Stream, output, _any),
	!,
	set_stream(output, Stream).
set_output(Stream) :-
	bip_error(set_output(Stream)).


flush_output :- flush(output).			% 8.11.7

flush_output(Stream) :-
	check_stream_or_alias_io_type(Stream, output, _any),
	!,
	flush(Stream).
flush_output(Stream) :-
	bip_error(flush_output(Stream)).


% Non-strict version
% Does not reject stream-aliases, and accepts all ECLiPSe properties
stream_property(Stream, Property) :-
	current_stream(Stream),
	( var(Property) ->
	    ( iso_only_stream_property(Stream, Property)
	    ; get_stream_info(Stream, Name, Value), Property =.. [Name,Value]
	    )
	; iso_only_stream_property(Property) ->
	    iso_only_stream_property(Stream, Property)
	; arity(Property,1) ->
	    Property =.. [Name, Value],
	    get_stream_info(Stream, Name, Value)
	;
	    error(6, stream_property(Stream, Property))
	).


at_end_of_stream :-
	get_stream_info(input, end_of_stream, Value),
	Value \== (not).

at_end_of_stream(Stream) :-
	get_stream_info(Stream, end_of_stream, Value),
	Value \== (not).

set_stream_position(Stream, P) :-		% 8.11.9
	check_stream_or_alias(Stream),
	( integer(P) -> true
	; var(P) -> set_bip_error(4)
	; set_bip_error(5)),
	( get_stream_info(Stream, reposition, true) -> true
	; set_bip_error(192) ),
	!,
	seek(Stream, P).
set_stream_position(Stream, P) :-
	bip_error(set_stream_position(Stream, P)).


%-----------------------------------------------------------------------
% 8.12 and 8.13 Character and byte input/output
% Implements the requirement that char&code predicates work
% only on text files, and byte predicates only on binary files.
%-----------------------------------------------------------------------

    check_type_out(_Type, X, _, _) :- var(X), !.
    check_type_out(Type, X, F, A) :- check_type_in(Type, X, F, A).

    check_type_in(Type, X, F, A) :-
	( is_type(Type, X) ->
	    true
    	; var(X) ->
	    throw(error(instantiation_error, F/A))
    	; supertype(Type, Super) ->
	    ( is_type(Super, X) -> throw(error(representation_error(Type), F/A))
	    ; throw(error(type_error(Super,X), F/A))
	    )
	;
	    throw(error(type_error(Type,X), F/A))
	).

    supertype(in_character_code, integer) :- !.
    supertype(character_code, integer) :- !.
    supertype(T, T).

    is_type(integer, X) :- integer(X).
    is_type(in_byte, X) :- integer(X), -1=<X, X=<255.
    is_type(byte, X) :- integer(X), 0=<X, X=<255.
    is_type(in_character_code, X) :- integer(X), -1=<X, X=<255.
    is_type(character_code, X) :- integer(X), 0=<X, X=<255.
    is_type(in_character, X) :- atom(X),
	( atom_length(X, 1) -> true ; X==end_of_file ).
    is_type(character, X) :- atom(X), atom_length(X, 1).

get_byte(Stream, Byte) :-
	check_stream_or_alias_io_type(Stream, input, binary),
	!,
	check_type_out(in_byte, Byte, get_byte, 2),
	get(Stream, Byte).
get_byte(Stream, Byte) :-
	bip_error(get_byte(Stream, Byte)).

get_code(Stream, Code) :-
	check_stream_or_alias_io_type(Stream, input, text),
	!,
	check_type_out(in_character_code, Code, get_code, 2),
	get(Stream, Code).
get_code(Stream, Code) :-
	bip_error(get_code(Stream, Code)).

get_char(Stream, Char) :-
	check_stream_or_alias_io_type(Stream, input, text),
	!,
	check_type_out(in_character, Char, get_char, 2),
	get(Stream, Code),
	( Code = -1 -> Char = end_of_file ; char_code(Char, Code) ).
get_char(Stream, Char) :-
	bip_error(get_char(Stream, Char)).


put_byte(Stream, Byte) :-
	check_stream_or_alias_io_type(Stream, output, binary),
	!,
	check_type_in(byte, Byte, put_byte, 2),
	put(Stream, Byte).
put_byte(Stream, Byte) :-
	bip_error(put_byte(Stream, Byte)).

put_code(Stream, Code) :-
	check_stream_or_alias_io_type(Stream, output, text),
	!,
	check_type_in(character_code, Code, put_code, 2),
	put(Stream, Code).
put_code(Stream, Code) :-
	bip_error(put_code(Stream, Code)).

put_char(Stream, Char) :-
	check_stream_or_alias_io_type(Stream, output, text),
	!,
	check_type_in(character, Char, put_char, 2),
	eclipse_language:put_char(Stream, Char).
put_char(Stream, Char) :-
	bip_error(put_char(Stream, Char)).


peek_byte(Stream, Byte) :-
	check_stream_or_alias_io_type(Stream, input, binary),
	!,
	check_type_out(in_byte, Byte, peek_byte, 2),
	get(Stream, Next), unget(Stream), Next=Byte.
peek_byte(Stream, Byte) :-
	bip_error(peek_byte(Stream, Byte)).

peek_code(Stream, Code) :-
	check_stream_or_alias_io_type(Stream, input, text),
	!,
	check_type_out(in_character_code, Code, peek_code, 2),
	get(Stream, Next), unget(Stream), Next=Code.
peek_code(Stream, Code) :-
	bip_error(peek_code(Stream, Code)).

peek_char(Stream, Char) :-
	check_stream_or_alias_io_type(Stream, input, text),
	!,
	check_type_out(in_character, Char, peek_char, 2),
	get_char(Stream, Next), unget(Stream), Next=Char.
peek_char(Stream, Char) :-
	bip_error(peek_char(Stream, Char)).


get_byte(Byte) :- get_stream(input, S), get_byte(S, Byte).
get_code(Code) :- get_stream(input, S), get_code(S, Code).
get_char(Char) :- get_stream(input, S), get_char(S, Char).
put_byte(Byte) :- get_stream(output, S), put_byte(S, Byte).
put_code(Code) :- get_stream(output, S), put_code(S, Code).
put_char(Char) :- get_stream(output, S), put_char(S, Char).
peek_byte(Byte) :- get_stream(input, S), peek_byte(S, Byte).
peek_code(Code) :- get_stream(input, S), peek_code(S, Code).
peek_char(Char) :- get_stream(input, S), peek_char(S, Char).


%-----------------------------------------------------------------------
% 8.14 Term input/output
%-----------------------------------------------------------------------

% char_conversion (dummy)

:- local record(cc).

char_conversion(C1, C2) :-			% 8.14.5
	check_one_char_atom_in(C1),
	check_one_char_atom_in(C2),
	( erase(cc, cc(C1,_)) -> true ; true ),
	( C1==C2 -> true ; record(cc, cc(C1,C2)) ).

    check_one_char_atom_in(C) :-
	( atom(C), atom_length(C, 1) -> true
	; var(C) -> throw(error(instantiation_error,char_conversion/2))
	; throw(error(representation_error(character),char_conversion/2))
	).

current_char_conversion(C1, C2) :-		% 8.14.6
	check_one_char_atom(C1),
	check_one_char_atom(C2),
	C1 \== C2,
	recorded(cc, cc(C1,C2)).		% logical update semantics

    check_one_char_atom(C) :-
	( atom(C), atom_length(C, 1) -> true
	; var(C) -> true
	; throw(error(type_error(character,C),current_char_conversion/2))
	).


% Logical update semantics explicitly required
:- export current_op/3.
:- tool(current_op/3, current_op_/4).
current_op_(A, P, Op, M) :-
	( var(Op) ->
	    findall(op(A,P,Op), eclipse_language:current_op(A,P,Op), Ops)@M,
	    member(op(A,P,Op), Ops)
	;
	    eclipse_language:current_op(A,P,Op)@M
	).


%-----------------------------------------------------------------------
% 8.16 Constant Processing
%-----------------------------------------------------------------------

% For producing domain error
atom_length(A, N) :-
	( integer(N), N<0 ->
	    error(6, atom_length(A, N))
	;
	    eclipse_language:atom_length(A, N)
	).


atom_concat(A, B, C) :-				% 8.16.2
	var(C), !,
	concat_atoms(A, B, C).
atom_concat(A, B, C) :-				% 8.16.2
	( var(A) -> true ; atom_string(A, SA) ),
	( var(B) -> true ; atom_string(B, SB) ),
	atom_string(C, SC),
	append_strings(SA, SB, SC),
	atom_string(A, SA),
	atom_string(B, SB).


sub_atom(Atom, Before, Length, After, SubAtom) :-	% 8.16.3
	var(SubAtom),
	atom_string(Atom, String),
	substring(String, Before, Length, After, SubString),
	atom_string(SubAtom, SubString).
sub_atom(Atom, Before, Length, After, SubAtom) :-
	nonvar(SubAtom),
	atom_string(Atom, String),
	atom_string(SubAtom, SubString),
	substring(String, Before, Length, After, SubString).


atom_chars(Atom, Chars) :-			% 8.16.4
	var(Atom),
	check_character_list_out(Chars, Chars, atom_chars, 2, _),
	concat_atom(Chars, Atom).
atom_chars(Atom, Chars) :-
	nonvar(Atom),
	check_character_list_out(Chars, Chars, atom_chars, 2, _),
	atom_codes(Atom, Codes),
	chars_codes(Chars, Codes).

    check_character_list_out(Cs, _, _, _, Nonground) :- var(Cs), !,
    	Nonground = true.
    check_character_list_out([], _, _, _, _) ?- !.
    check_character_list_out([C|Cs], All, F, A, Nonground) ?- !,
	( var(C) -> Nonground = true
	; atom(C), atom_length(C, 1) -> true
	; throw(error(type_error(character,C),F/A))
	),
	check_character_list_out(Cs, All, F, A, Nonground).
    check_character_list_out(_, All, F, A, _) :-
	throw(error(type_error(list,All),F/A)).


atom_codes(Atom, List) :-			% 8.16.5
	var(Atom),
	check_charcode_list_out(List, List, atom_codes, 2, _),
	string_list(String, List),
	atom_string(Atom, String).
atom_codes(Atom, List) :-
	nonvar(Atom),
	check_charcode_list_out(List, List, atom_codes, 2, _),
	atom_string(Atom, String),
	string_list(String, List).

    check_charcode_list_out(Cs, _, _, _, Nonground) :- var(Cs), !,
    	Nonground = true.
    check_charcode_list_out([], _, _, _, _) ?- !.
    check_charcode_list_out([C|Cs], All, F, A, Nonground) ?- !,
	( var(C) -> Nonground = true
	; integer(C), 0=<C, C=<255 -> true
	; integer(C) -> throw(error(representation_error(character_code),F/A))
	; throw(error(type_error(integer,C),F/A))
	),
	check_charcode_list_out(Cs, All, F, A, Nonground).
    check_charcode_list_out(_, All, F, A, _) :-
	throw(error(type_error(list,All),F/A)).


% number_chars/2 and number_codes/2 are a pain wrt exceptions...

number_chars(Number, Chars) :-			% 8.16.7
	check_character_list_out(Chars, Chars , number_chars, 2, Nonground),
	( var(Nonground) ->
	    concat_string(Chars, String),
	    ( string_to_number(String, Term) ->
		Number = Term
	    ;
	    	throw(error(syntax_error(number_expected),number_chars/2))
	    )
        ; number(Number) ->
            number_string(Number, String),      % write
            string_list(String, Codes),
	    chars_codes(Chars, Codes)
        ; var(Number) ->
            error(4, number_chars(Number, Chars))
        ;
            error(5, number_chars(Number, Chars))
        ).

number_codes(Number, Codes) :-			% 8.16.8
	check_charcode_list_out(Codes, Codes , number_codes, 2, Nonground),
	( var(Nonground) ->
	    string_list(String, Codes),
	    ( string_to_number(String, Term) ->
		Number = Term
	    ;
	    	throw(error(syntax_error(number_expected),number_codes/2))
	    )
        ; number(Number) ->
            number_string(Number, String),      % write
            string_list(String, Codes)
        ; var(Number) ->
            error(4, number_codes(Number, Codes))
        ;
            error(5, number_codes(Number, Codes))
        ).

    chars_codes([], []).
    chars_codes([Char|Chars], [Code|Codes]) :-
	char_code(Char, Code),
	chars_codes(Chars, Codes).

    string_to_number(String, Number) :-
	open(string(String), read, S),
	read_token(S, T1, _),
	( T1 == (-) ->
	    read_token(S, T2, _),
	    iso_number(T2),
	    Number is -T2
	;
	    iso_number(T1),
	    Number = T1
	),
	at_eof(S),
	close(S).

    iso_number(X) :- integer(X).
    iso_number(X) :- float(X).


%-----------------------------------------------------------------------
% 8.17 Implementation defined hooks
%-----------------------------------------------------------------------

:- export set_prolog_flag/2.
:- tool(set_prolog_flag/2, set_prolog_flag_/3).
set_prolog_flag_(Flag, Value, M) :-
	iso_aux:set_prolog_flag(Flag, Value, M, false).

:- export current_prolog_flag/2.
:- tool(current_prolog_flag/2, current_prolog_flag_/3).
current_prolog_flag_(Flag, Value, M) :-
	iso_aux:current_prolog_flag(Flag, Value, M, false).


halt(X) :- integer(X), !, exit(X).			% 8.17.4
halt(X) :- var(X), !, error(4, halt(X)).
halt(X) :- error(5, halt(X)).


%-----------------------------------------------------------------------
% 9. Evaluable functors
% The bulk of this is in iso_aux!
%-----------------------------------------------------------------------

is_(R, X, M) :- is_(R, X, M, light).

:- inline((is)/2, trans_is/2).
trans_is(Goal, Expanded) :- trans_is(Goal, Expanded, light).


% redefine the comparisons, using the visible is/2
<(X,Y,M)   :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1<Y1)@M.
>(X,Y,M)   :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1>Y1)@M.
=<(X,Y,M)  :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1=<Y1)@M.
>=(X,Y,M)  :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1>=Y1)@M.
=:=(X,Y,M) :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1=:=Y1)@M.
=\=(X,Y,M) :- (X1 is X)@M, (Y1 is Y)@M, eclipse_language:(X1=\=Y1)@M.

trans_compare(Goal, Expanded) :- trans_compare(Goal, Expanded, light).

:- inline((>=)/2,  trans_compare/2).
:- inline((>)/2,   trans_compare/2).
:- inline((=<)/2,  trans_compare/2).
:- inline((<)/2,   trans_compare/2).
:- inline((=:=)/2, trans_compare/2).
:- inline((=\=)/2, trans_compare/2).
