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
% The Original Code is  The ECLiPSe Library iso_heavy.
% The Initial Developer of the Original Code is  Coninfer Ltd.
% Portions created by the Initial Developer are
% Copyright (C) 2010-2013 Coninfer Ltd
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: iso_heavy.ecl,v 1.1 2017/08/19 20:00:10 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	iso_heavy.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		see export directive
%
% DESCRIPTION:		This implements ISO-conforming versions of predicates
%			that override ECLiPSe predicates.
%			Some of these are parameterised with strict/non-strict.
%			This is not meant to be used as a language-module.
%

:- module(iso_heavy).

:- pragma(system).

% Workaround: make tools (which don't obey the system directive) into built-ins
:- local initialization((
    current_module_predicate(exported,P), set_flag(P, type, built_in), fail ; true
)).

:- use_module(iso_aux).

:- import
	bip_error/1
    from sepia_kernel.

%-----------------------------------------------------------------------
% 7.8 Control constructs
%-----------------------------------------------------------------------

:- export call/1, call/2, call/3.

:- tool(call/1,call_/2).
call_(G, M) :-
	( normalize_call(G, G1) ->
	    eclipse_language:call(G1)@M
	;
	    throw(error(type_error(callable,G),call/1))
	).

    :- mode normalize_call(?,-).
    normalize_call(G, G) :- var(G), !.
    normalize_call(G, G1) :-
	normalize_body(G, G1).

:- tool(call/2,call_/3).
call_(P, A, M) :-
	( critical_goal(P, A, G) ->
	    ( normalize_body(G, G1) ->
		eclipse_language:call(G1)@M
	    ;
		throw(error(type_error(callable,G),call/1))
	    )
	;
	    eclipse_language:call(P, A)@M
	).

    critical_goal(','(G1), G2, ','(G1,G2)).
    critical_goal(';'(G1), G2, ';'(G1,G2)).
    critical_goal('->'(G1), G2, '->'(G1,G2)).

:- tool(call/3,call_/4).
call_(P, A1, A2, M) :-
	( critical_goal(P, A1, A2, G) ->
	    ( normalize_body(G, G1) ->
		eclipse_language:call(G1)@M
	    ;
		throw(error(type_error(callable,G),call/1))
	    )
	;
	    eclipse_language:call(P, A1, A2)@M
	).

    critical_goal(',', G1, G2, ','(G1,G2)).
    critical_goal(';', G1, G2, ';'(G1,G2)).
    critical_goal('->', G1, G2, '->'(G1,G2)).


%-----------------------------------------------------------------------
% 8.4 Term comparison
% compare/3:	extra type checks
%-----------------------------------------------------------------------

:- export compare/3.
compare(R, X, Y) :- var(R), !,
	eclipse_language:compare(R, X, Y).
compare(R, X, Y) :- atom(R), !,
	( (R==(=) ; R==(<) ; (R)==(>)) ->
	    eclipse_language:compare(R, X, Y)
	;
	    throw(error(domain_error(order,R),compare/3))
	).
compare(R, _, _) :-
	throw(error(type_error(atom,R),compare/3)).


:- export sort/2.
sort(Xs, Ss) :-
	( is_output_list(Ss) -> true
	; throw(error(type_error(list,Ss),sort/2)) ),
	eclipse_language:sort(Xs, Ss).


:- export keysort/2.
keysort(Xs, Ss) :-
	check_pair_list(Xs, Xs),
	check_pair_list_out(Ss, Ss),
	eclipse_language:keysort(Xs, Ss).

    check_pair_list(Ps, _) :- var(Ps), !,
	throw(error(instantiation_error,keysort/2)).
    check_pair_list([], _) :- !.
    check_pair_list([P|Ps], All) :- !,
	( var(P) -> throw(error(instantiation_error,keysort/2))
	; P = _-_ -> true
	; throw(error(type_error(pair,P),keysort/2))
	),
	check_pair_list(Ps, All).
    check_pair_list(_, All) :-
	throw(error(type_error(list,All),keysort/2)).


    check_pair_list_out(Ps, _) :- var(Ps), !.
    check_pair_list_out([], _) :- !.
    check_pair_list_out([P|Ps], All) :- !,
	( var(P) -> true
	; P = _-_ -> true
	; throw(error(type_error(pair,P),keysort/2))
	),
	check_pair_list_out(Ps, All).
    check_pair_list_out(_, All) :-
	throw(error(type_error(list,All),keysort/2)).


%-----------------------------------------------------------------------
% 8.5.5 Term decomposition
%-----------------------------------------------------------------------

:- export term_variables/2.
term_variables(Term, Vs) :-
	( is_output_list(Vs) -> true
	; throw(error(type_error(list,Vs),term_variables/2)) ),
	eclipse_language:term_variables(Term, Vs).
	

%-----------------------------------------------------------------------
% 8.10 All Solutions
% findall, bagof, setof: extra error checking
%-----------------------------------------------------------------------

:- export findall/3.
:- tool(findall/3,findall_/4).
findall_(Template, Goal, Instances, Module) :-
	( is_output_list(Instances) -> true
	; throw(error(type_error(list,Instances),findall/3)) ),
	( normalize_call(Goal, NormGoal) ->
	    eclipse_language:findall(Template, NormGoal, Instances)@Module
	;
	    throw(error(type_error(callable,Goal),findall/3))
	).

:- export bagof/3.
:- tool(bagof/3,bagof_/4).
bagof_(Template, QGoal, Instances, Module) :-
	( is_output_list(Instances) -> true
	; throw(error(type_error(list,Instances),bagof/3)) ),
	dequant(QGoal, Goal, QNormGoal, NormGoal),
	( normalize_call(Goal, NormGoal) ->
	    eclipse_language:bagof(Template, QNormGoal, Instances)@Module
	;
	    throw(error(type_error(callable,Goal),bagof/3))
	).

:- export setof/3.
:- tool(setof/3,setof_/4).
setof_(Template, QGoal, Instances, Module) :-
	( is_output_list(Instances) -> true
	; throw(error(type_error(list,Instances),setof/3)) ),
	dequant(QGoal, Goal, QNormGoal, NormGoal),
	( normalize_call(Goal, NormGoal) ->
	    eclipse_language:setof(Template, QNormGoal, Instances)@Module
	;
	    throw(error(type_error(callable,Goal),setof/3))
	).

    :- mode dequant(+,-,-,-).
    dequant(G, G, NG, NG) :- var(G), !.
    dequant(V^VG, G, V^VNG, NG) :- !,
	dequant(VG, G, VNG, NG).
    dequant(G, G, NG, NG).


%-----------------------------------------------------------------------
% 8.11 Stream selection and control
% open/3,4:	Restrictions on mode and options
%-----------------------------------------------------------------------

:- export open/5.
open(SourceSink, Mode, Stream, Options, Strict) :-
	( Mode\==update -> true
	; Strict==false -> true
	; throw(error(domain_error(io_mode,Mode),open/4))),
	( var(Stream) -> true
	; throw(error(uninstantiation_error(Stream),open/4))),
	check_stream_options(Options, Options, Strict),
	eclipse_language:open(SourceSink, Mode, Stream, Options).

    check_stream_options(Options, _, _) :- var(Options), !,
	throw(error(instantiation_error,open/4)).
    check_stream_options([], _, _) :- !.
    check_stream_options([Option|Options], All, Strict) :- !,
	( Strict==true -> strict_stream_option(Option) ; true ),
	check_stream_options(Options, All, Strict).
    check_stream_options(_Junk, All, _) :-
	throw(error(type_error(list,All),open/4)).

    % full checks are done in underlying open/4
    strict_stream_option(Option) :- var(Option), !.
    strict_stream_option(type(_)) :- !.
    strict_stream_option(reposition(_)) :- !.
    strict_stream_option(alias(_)) :- !.
    strict_stream_option(eof_action(_)) :- !.
    strict_stream_option(Option) :-
	throw(error(domain_error(stream_option,Option),open/4)).


:- export stream_property/3.
stream_property(Stream, Property, Strict) :-
	( var(Stream) -> true
	; is_handle(Stream) -> true
	; throw(error(domain_error(stream,Stream), stream_property/2))),
	current_stream(Stream),
	( var(Property) ->
	    (
		iso_only_stream_property(Stream, Property)
	    ;
	    	Strict==true,
		iso_ecl_stream_property(Property),
		Property =.. [Name,Value],
		get_stream_info(Stream, Name, Value)
	    ;
	    	Strict==false,
		get_stream_info(Stream, Name, Value),
		Property =.. [Name,Value]
	    )

	; iso_only_stream_property(Property) ->
	    iso_only_stream_property(Stream, Property)

	; Strict==true, iso_ecl_stream_property(Property) ->
	    Property =.. [Name, Value],
	    get_stream_info(Stream, Name, Value)

	; Strict==false, Property =.. [Name, Value],
	  get_stream_info(Stream, Name0, Value0), Name==Name0 ->
	    Value = Value0
	;
	    throw(error(domain_error(stream_property,Property), stream_property/2))
	).


%-----------------------------------------------------------------------
% 8.14 Term input/output
% read,read_term:	binary restriction and option error handling
% write_term etc:	binary restriction, options, option error handling
%-----------------------------------------------------------------------

:- export read/1.
:- tool(read/1, read_/2).
read_(Term, Module) :-
	read_term(input, Term, [], Module, false).

:- export read/2.
:- tool(read/2, read_/3).
read_(Stream, Term, Module) :-
	read_term(Stream, Term, [], Module, false).

% auxiliary for read_term/2,3
:- export read_term/5.
read_term(Stream, Term, Options, Module, Strict) :-	% 8.14.1
	check_stream_or_alias_io_type(Stream, input, text),
	!,
	check_read_options(Options, Options, Strict),
	eclipse_language:read_term(Stream, Term, Options)@Module.
read_term(Stream, Term, Options, Module, _) :-		% 8.14.1
	bip_error(read_term(Stream, Term, Options))@Module.

    check_read_options(Options, _, _) :- var(Options), !,
	throw(error(instantiation_error, read_term/3)).
    check_read_options([], _, _) :- !.
    check_read_options([O|Os], All, Strict) :- !,
	( Strict==true -> strict_read_option(O) ; true ),
	check_read_options(Os, All, Strict).
    check_read_options(_, All, _) :-
	throw(error(type_error(list,All), read_term/3)).

    strict_read_option(Option) :- var(Option), !,
	throw(error(instantiation_error, read_term/3)).
    strict_read_option(variables(_)) :- !.
    strict_read_option(variable_names(_)) :- !.
    strict_read_option(singletons(_)) :- !.
    strict_read_option(Option) :-
	throw(error(domain_error(read_option,Option), read_term/3)).


% auxiliary for write_term/2,3
:- export write_term/5.
write_term(Stream, Term, Options, Module, Strict) :-		% 8.14.2
	check_stream_or_alias_io_type(Stream, output, text),
	!,
	check_write_options(Options, Options, Strict),
	eclipse_language:write_term(Stream, Term, Options)@Module.
write_term(Stream, Term, Options, Module, _) :-		% 8.14.2
	bip_error(write_term(Stream, Term, Options))@Module.

    check_write_options(Options, _, _) :- var(Options), !,
	throw(error(instantiation_error, write_term/3)).
    check_write_options([], _, _) :- !.
    check_write_options([O|Os], All, Strict) :- !,
	( Strict==true -> strict_write_option(O) ; true ),
	check_write_options(Os, All, Strict).
    check_write_options(_, All, _) :-
	throw(error(type_error(list,All), write_term/3)).

    % full checks are done in underlying write_term/4
    strict_write_option(Option) :- var(Option), !.
    strict_write_option(quoted(_)) :- !.
    strict_write_option(ignore_ops(_)) :- !.
    strict_write_option(numbervars(_)) :- !.
    strict_write_option(variable_names(_)) :- !.
    strict_write_option(Option) :-
	throw(error(domain_error(write_option,Option), write_term/3)).

