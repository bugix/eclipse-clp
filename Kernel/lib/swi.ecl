% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright:	This file is in the public domain
% Version:	$Id: swi.ecl,v 1.14 2017/09/03 13:23:23 jschimpf Exp $
% Description:	SWI Prolog compatibility package
% ----------------------------------------------------------------------

:- module(swi).

:- comment(categories, ["Compatibility"]).
:- comment(summary, 'SWI-Prolog compatibility package').
:- comment(author, 'J Chamois').
:- comment(copyright, 'This file is in the public domain').
:- comment(date, '$Date: 2017/09/03 13:23:23 $').
:- comment(desc, html('
    This library is incomplete, and intended to ease the task of
    porting SWI-Prolog programs to ECLiPSe Prolog, or to add modules
    written in SWI-Prolog to applications written in ECLiPSe. 
    <P>
    It reuses parts of the C-Prolog, Quintus and Iso compatibility
    libraries.
    <P>
    The effect of the compatibility library is local to the module where
    it is loaded. For maximum compatibility, an SWI program should
    be wrapped in a separate module starting with a directive like
    <PRE>
    :- module(mymodule, [], swi).
    </PRE>
    In this case, Eclipse-specific language constructs will not be available.
    <P>
    If the compatibility package is loaded into a normal Eclipse module, like
    <PRE>
    :- module(mymixedmdule).
    :- use_module(library(swi)).
    </PRE>
    then SWI and Eclipse language features can be used together. 
    However, ambiguities must be resolved explicitly and confusion may
    arise from the different meaning of quotes in Eclipse vs SWI-Prolog.
    ')).
:- comment(see_also, [library(cio),library(cprolog),library(quintus),library(sicstus)]).

% suppress deprecation warnings for reexported builtins
:- pragma(deprecated_warnings(not_reexports)).

:- reexport eclipse_language except
	dynamic/1,
        ensure_loaded/1,
	set_stream/2.

:- reexport cio.

:- reexport
	current_input/1,
	current_output/1,
	initialization/1,
	set_stream_position/2,
	set_input/1,
	set_output/1,
	set_stream_position/2,
	unify_with_occurs_check/2
   from iso_light.

:- eclipse_language:ensure_loaded(library(iso_error)).

:- reexport
	(multifile)/1, op(_,_,(multifile)),
	public/1, op(_,_,(public)),
	macro((:)/2,_,[clause]),
	dynamic/1,
        use_module/2,
        ensure_loaded/1		% caution: overrides
    from quintus.

:- reexport format.

:- reexport
	setup_call_cleanup/3
   from prolog_extras.

:- lib(lists).
:- reexport
	select/3
    from lists.

:- export			% temporary, while op/macros still global
	op(0,   xfx, (of)),
	op(0,   xfx, (with)),
	op(0,   xfy, (do)),
	op(0,   xfx, (@)),
	op(0,   fx, (-?->)),
	op(0,	fy, (not)),
	op(0,	fy, (spied)),
	op(0,	fx, (delay)),
	macro((with)/2, (=)/2, []),
	macro((of)/2, (=)/2, []).

:- local
	op(650, xfx, (@)).

:- export
	op(1150, fx, initialization),
	op(1150, fx, module_transparent),
	op(1150, fx, discontiguous),
	op(1150, fx, multifile),
	op(1150, fx, thread_local),
	op(1150, fx, volatile),
	op(954, xfy, \),
	op(600, xfy, :),
	op(500, yfx, xor),
	op(500, fx, ?),
	op(200, xfx, **),

	syntax_option(nested_comments),
	syntax_option(iso_escapes),
	syntax_option(iso_base_prefix),
	syntax_option(doubled_quote_is_quote),
	syntax_option(no_array_subscripts),
	syntax_option(bar_is_no_atom),
	syntax_option(no_attributes),
	syntax_option(no_curly_arguments),
	syntax_option(nl_in_quotes).


% Type tests

:- export rational/3.
rational(R, N, D) :-
	N is numerator(R),
	D is denominator(R).

:- export cyclic_term/1.
cyclic_term(T) :-
	\+ acyclic_term(T).


% Comparison

:- export
	op(700, xfx, [=@=, \=@=]),
	(=@=)/2,
	(\=@=)/2,
	(?=)/2,
	unifiable/3.

X =@= Y :-
	variant(X, Y).

X \=@= Y :-
	\+ variant(X, Y).

?=(X, Y) :- 
	( X == Y -> true ; X \= Y ).

unifiable(X, Y, Unifier) :-
	term_variables(X-Y, Vars),
	setof(Vars, X=Y, [Bindings]),
	do((foreach(V,Vars), foreach(B,Bindings), foreach(V=B, Unifier)), true ).


% Control

:- reexport apply/2 from apply.

:- export ignore/1.
:- inline(ignore/1, t_ignore/2).
t_ignore(ignore(Goal), (Goal->true;true)).
:- tool(ignore/1, ignore_/2).
ignore_(Goal, Module) :-
	( call(Goal)@Module -> true ; true ).

:- export freeze/2.
:- tool(freeze/2, freeze_/3).
:- inline(freeze/2, tr_freeze/2).
tr_freeze(freeze(X, Goal), (var(X) -> suspend(Goal, 0, (X->inst)) ; Goal)).

freeze_(X, Goal, Module) :- var(X), suspend(Goal, 0, (X->inst))@Module.
freeze_(X, Goal, Module) :- nonvar(X), call(Goal)@Module.


% Signals

:- export
	on_signal/3,
	current_signal/3.

:- tool(on_signal/3, on_signal_/4).
on_signal_(Name, Old, New, Module) :-
	once current_interrupt(Id, Name),
	get_interrupt_handler(Id, Old, _Module),
	@(set_interrupt_handler(Id, New),Module).

current_signal(Name, Id, Handler) :-
	current_interrupt(Id, Name),
	get_interrupt_handler(Id, Handler, _Module).


% Database

:- export flag/3, current_flag/1.
:- local store(flags).
flag(Key, Old, New) :-
	( store_get(flags, Key, Old) -> true ; Old = 0 ),
	store_set(flags, Key, New).

current_flag(Key) :-
	( ground(Key) ->
	    store_contains(flags, Key)
	;
	    stored_keys(flags, Keys),
	    member(Key, Keys)
	).


:- export hash_term/2.
hash_term(Term, Hash) :-
	term_hash(Term, -1, 1073741824 /*2^30*/, Hash).

:- export index/1.
index(_).

:- export hash/1.
hash(_).


% Examining the program

:- export
	current_functor/2,
	current_predicate/2,
	predicate_property/2.

current_functor(N, A) :-
	current_functor(N/A).

:- tool(current_predicate/2, current_predicate_/3).
current_predicate_(N, Term, M) :-
	( @(current_predicate(N/A),M) ; @(current_built_in(N/A),M) ),
	functor(Term, N, A).

:- tool(predicate_property/2, predicate_property_/3).
predicate_property_(Head, Property, Module) :-
	( var(Head) ->
	    current_predicate_(_, Head, Module)
	;
	    true
	),
	functor(Head, N, A),
	property(N/A, Property, Module).

    property(Pred, built_in, Module) :- get_flag(Pred, type, built_in)@Module.
    property(Pred, dynamic, Module) :- get_flag(Pred, stability, dynamic)@Module.
    property(Pred, exported, Module) :- get_flag(Pred, visibility, exported)@Module.
    property(Pred, imported_from(M), Module) :-
    	get_flag(Pred, visibility, imported)@Module,
	get_flag(Pred, definition_module, M)@Module.
    property(Pred, file(File), Module) :- get_flag(Pred, source_file, File)@Module.
    property(Pred, line_count(Line), Module) :- get_flag(Pred, source_line, Line)@Module.
    property(Pred, foreign, Module) :- get_flag(Pred, call_type, external)@Module.
    property(Pred, nodebug, Module) :- get_flag(Pred, debugged, off)@Module.
    property(Pred, notrace, Module) :- get_flag(Pred, leash, notrace)@Module.
    property(Pred, undefined, Module) :- get_flag(Pred, defined, off)@Module.


% I/O

:- export open_null_stream/1.
open_null_stream(S) :-
	var(S),
	S = null.
open_null_stream(S) :-
	atom(S),
	eclipse_language:set_stream(S, null).

:- export stream_property/2.
stream_property(Stream, Property) :-
	current_stream(Stream),
	stream_property1(Stream, Property).

    stream_property1(Stream, file_name(F)) :-
	get_stream_info(Stream, name, F).
    stream_property1(Stream, mode(M)) :-
	get_stream_info(Stream, mode, M).
    stream_property1(Stream, input) :-
	get_stream_info(Stream, mode, read).
    stream_property1(Stream, output) :-
	get_stream_info(Stream, mode, M),
	memberchk(M, [write, update]).
    stream_property1(Stream, alias(A)) :- atom(A),
	get_stream(A, Stream).
    stream_property1(Stream, position(P)) :-
	at(Stream, P).
    stream_property1(_Stream, type(binary)).
    stream_property1(Stream, end_of_stream(P)) :-
	(at_eof(Stream) -> P = at ; P = not).
    stream_property1(Stream, reposition(true)) :-
	get_stream_info(Stream, device, D),
	memberchk(D, [file,string]).
    stream_property1(Stream, tty(true)) :-
	get_stream_info(Stream, device, tty).


:- export seek/4.
seek(Stream, Offset, bof, NewLocation) ?-
	NewLocation = Offset,
	seek(Stream, NewLocation).
seek(Stream, Offset, current, NewLocation) ?-
	NewLocation is at(Stream) + Offset,
	seek(Stream, NewLocation).
seek(Stream, Offset, eof, NewLocation) ?-
	seek(Stream, end_of_file),
	NewLocation is at(Stream) + Offset,
	seek(Stream, NewLocation).

:- export set_stream/2.
set_stream(Stream, alias(Name)) :-
	eclipse_language:set_stream(Name, Stream).
set_stream(Stream, buffering(full)) :- !,
	set_stream_property(Stream, flush, flush).
set_stream(Stream, buffering(line)) :- !,
	set_stream_property(Stream, flush, end_of_line).
set_stream(Stream, buffering(false)) :-	% not implemented
	set_stream_property(Stream, flush, end_of_line).
%set_stream(Stream, eof_action(_)).
%set_stream(Stream, close_on_abort(_)).
%set_stream(Stream, encoding(_)).
%set_stream(Stream, timeout(_)).
%set_stream(Stream, record_position(_)).
%set_stream(Stream, file_name(_)).
%set_stream(Stream, tty(_)).

:- export wait_for_input/3.
wait_for_input(Streams, ReadyStreams, Timeout) :-
	stream_select(Streams, Timeout, ReadyStreams).

:- export byte_count/2.
byte_count(Stream, Count) :-
	at(Stream, Count).

:- export character_count/2.
character_count(Stream, Count) :-
	at(Stream, Count).

:- export line_count/2.
line_count(Stream, Count) :-
	get_stream_info(Stream, line, Count).


% Strings

:- export
	string_to_atom/2,
	string_to_list/2.

string_to_atom(S, A) :-
	atom_string(A, S).

string_to_list(S, L) :-
	string_list(S, L).



% Arithmetic

:- export between/3.
between(Low, High, Value) :-
	between(Low, High, 1, Value).

:- export op(400, xfx, rdiv).
:- export rdiv/3.	% for rdiv/2 function
rdiv(X, Y, Z) :- Z is rational(eval(X))/eval(Y).

:- export sum_list/2.
sum_list(Xs, S) :- S is sum(Xs).

:- export max_list/2.
max_list(Xs, S) :- S is max(Xs).

:- export min_list/2.
min_list(Xs, S) :- S is min(Xs).

:- export numlist/3.
numlist(L, H, Is) :- do((for(I,L,H), foreach(I,Is)), true).

:- export set_random/1.
set_random(seed(Seed)) :- seed(Seed).


% Lists

:- export merge_set/3.
merge_set(X, Y, XY) :-
	merge(0, <, X, Y, XY).

:- export predsort/3.
:- tool(predsort/3, predsort_/4).
predsort_(_, [], [], _) :- !.
predsort_(_, [X], [X], _) :- !.
predsort_(Pred, [X,Y|L], Sorted, Module) :-
	halve(L, [Y|L], Front, Back),
	predsort_(Pred, [X|Front], F, Module),
	predsort_(Pred, Back, B, Module),
	predmerge_(Pred, F, B, Sorted, Module).

    halve([_,_|Count], [H|T], [H|F], B) :- !,
	    halve(Count, T, F, B).
    halve(_, B, [], B).

    predmerge_(Pred, [H1|T1], [H2|T2], [Hm|Tm], Module) :- !,
	call(Pred, R, H1, H2)@Module,
	(   R = (<) -> Hm = H1, predmerge_(Pred, T1, [H2|T2], Tm, Module)
	;   R = (>) -> Hm = H2, predmerge_(Pred, [H1|T1], T2, Tm, Module)
	;              Hm = H1, predmerge_(Pred, T1, T2, Tm, Module)
	).
    predmerge_(_, [], L, L, _) :- !.
    predmerge_(_, L, [], L, _).


% Forall

:- export forall/2.
:- tool(forall/2, forall_/3).
forall_(Cond, Action, Module) :-
	\+ (call(Cond)@Module, \+ call(Action)@Module).


% Formatted write

:- export sformat/2.
sformat(String, Format) :-
	sformat(String, Format, []).

:- export sformat/3.
sformat(String, Format, Args) :-
	open(string(""), write, S),
	format(S, Format, Args),
	get_stream_info(S, name, String0),
	close(S),
	String = String0.


% Time

:- export get_time/1.
get_time(Time) :-
	Time is float(get_flag(unix_time)).

:- export get_time/1.
convert_time(Time, Year, Month, Day, Hour, Minute, Second, MilliSeconds) :-
	MilliSeconds is fix((Time - truncate(Time))*1000),
	Seconds is fix(Time),
	local_time(Year, Month, Day, Hour, Minute, Second, _DST, Seconds).

:- export convert_time/2.
convert_time(Time, String) :-
	Seconds is fix(Time),
	local_time_string(Seconds, "%c", String).


% Modules

:- export module_property/2.
module_property(M, file(F)) :-
	current_compiled_file(F, _, M).
module_property(M, exports(Ps)) :-
	get_module_info(M, interface, Interface),
	findall(P, (member((export P),Interface), P=_/_), Ps).
module_property(M, exported_operators(Ops)) :-
	get_module_info(M, interface, Interface),
	findall(Op, (member((export Op),Interface), Op=op(_,_,_)), Ops).
	

