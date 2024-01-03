% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright:	This file is in the public domain
% Version:	$Id: prolog_extras.ecl,v 1.4 2017/09/03 13:23:23 jschimpf Exp $
% Description:	Some predicates that are common in Prolog systems,
%		but that we do not want or need as ECLiPSe built-ins.
% ----------------------------------------------------------------------

:- module(prolog_extras).

:- comment(categories, ["Compatibility"]).
:- comment(summary, 'Some predicates commonly available in Prolog systems').
:- comment(author, 'Joachim Schimpf').
:- comment(copyright, 'This file is in the public domain').
:- comment(date, '$Date: 2017/09/03 13:23:23 $').

:- pragma(system).

:- lib(error).


%-----------------------------------------------------------------------
% Proposed ISO extension
% In our implementation, Cleanup is executed with the instantiations
% that are in place after Setup, any remaining variables are copies.
% We also ignore failure and throws from Cleanup.  This makes sense
% because it is unknown in which context Cleanup will be executed.
%-----------------------------------------------------------------------

:- export setup_call_cleanup/3.
:- tool(setup_call_cleanup/3, setup_call_cleanup_/4).

setup_call_cleanup_(Setup, Call, Cleanup, Module) :-
	once(Setup)@Module,
	must_be(callable, Cleanup),
	event_create(cleanup(Cleanup, Module), [defers], CleanupEvent),
	sepia_kernel:request_cut_fail_event(CleanupEvent),
	% choicepoint needed to stop local cuts in Call from triggering CleanupEvent
	(
	    sepia_kernel:get_cut(Before),
	    call(Call)@Module,
	    sepia_kernel:get_cut(After),
	    ( Before == After -> ! ; true )
	;
	    fail
	).

    cleanup(Cleanup, Module) :-
	(
	    once catch(Cleanup, _Ball, true)@Module,
	    fail
	;
	    events_nodefer
	).


%-----------------------------------------------------------------------
% Defined in ISO 13211-2 (module standard), and de-facto in many systems
%-----------------------------------------------------------------------

:- export predicate_property/2.
:- tool(predicate_property/2, predicate_property_/3).

predicate_property_(Head, Property, Module) :-
	Pred = N/A,
	( nonvar(Head) ->
	    must_be(callable, Head)
	;
	    current_predicate(Pred)@Module
	;
	    current_built_in(Pred)@Module
	),
	functor(Head, N, A),
	property(Pred, Property, Module).

    property(Pred, built_in, Module) :-		get_flag(Pred, type, built_in)@Module.
    property(Pred, dynamic, Module) :-		get_flag(Pred, stability, dynamic)@Module.
    property(Pred, static, Module) :-		get_flag(Pred, stability, static)@Module.
    property(Pred, exported, Module) :-		get_flag(Pred, visibility, V)@Module, (V == exported ; V == reexported).
    property(Pred, imported_from(M), Module) :- get_flag(Pred, visibility, imported)@Module, get_flag(Pred, definition_module, M)@Module.
    property(Pred, file(File), Module) :-	get_flag(Pred, source_file, File)@Module.
    property(Pred, line_count(Line), Module) :-	get_flag(Pred, source_line, Line)@Module.
    property(Pred, foreign, Module) :-		get_flag(Pred, call_type, external)@Module.
    property(Pred, nodebug, Module) :-		get_flag(Pred, debugged, off)@Module.
    property(Pred, notrace, Module) :-		get_flag(Pred, leash, notrace)@Module.
    property(Pred, undefined, Module) :-	get_flag(Pred, defined, off)@Module.
    property(Pred, Prop, Module) :-
    	( var(Prop) ->
	    get_flag(Pred, Name, Value)@Module, Prop =.. [Name,Value]
	;
	    Prop =.. [Name,Value], get_flag(Pred, Name, Value)@Module
	).


%-----------------------------------------------------------------------
% Basic delay features
%-----------------------------------------------------------------------

:- export dif/2.
:- inline(dif/2).
dif(X, Y) :- X~=Y.


:- export freeze/2.
:- tool(freeze/2, freeze_/3).
freeze_(X, G, M) :- var(X), suspend(G, 0, X->inst)@M.
freeze_(X, G, M) :- nonvar(X), call(G)@M.




