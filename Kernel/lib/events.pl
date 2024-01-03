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
% Version:	$Id: events.pl,v 1.41 2017/08/23 12:01:39 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	events.pl, part of module(sepia_kernel)
 *
 * DESCRIPTION: 	
 *
 *
 * CONTENTS:     	Event-Related Prolog Procedures and Declarations
 *
 */

:- system.
:- pragma(nodebug).

%------------------------------------
% error/event handling builtins
%------------------------------------

get_error_handler(N, H, M) :- atom(N), !,
	error(5,get_error_handler(N, H, M)).
get_error_handler(N, H, M) :-
	get_event_handler(N, H, M).

current_error(N) :-
	(var(N) ->
		max_error(Max),
		gen_valid_errors(1, Max, N)
	;
	integer(N) ->
		error_id(N, _)
	;
		error(5, current_error(N))
	).

gen_valid_errors(Start, _Max, Start) :-
	error_id(Start, _).
gen_valid_errors(Start, Max, N) :-
	Start < Max,
	New is Start+1,
	gen_valid_errors(New, Max, N).


% The user-definable exit from a non-recoverable error.
error_exit :-
	throw(abort).

%-------------------------------------
% Here are the default error handlers
%
% Arguments of error handlers:
%   1	Error		integer or atom (identifies the error)
%   2	Culprit		usually a goal (but sometimes a clause, a N/A, etc)
%   3	ContextModule	context module (if not known, same as lookup module)
%   4	LookupModule	lookup module for the culprit (always a valid module,
%			except for error 86 NO_LOOKUP_MODULE)
%-------------------------------------

no_err_handler(X, Where) :-
	write(error, 'no error handler, module has been erased,'),
	nl(error),
	error_message(X, Where).

error_handler(X, Where) :- 
	error_message(X, Where),
	error(157, _).

:- tool(error_handler/3, error_handler/4).

error_handler(X, Where, CM, LM) :- 
	error_message(X, Where, CM, LM),
	error(157, _).


%-------------------------------------
% Undefined-call handler, may be redefined to fail, etc
%-------------------------------------

call_handler(X, Where, CM, LM) :- 
	atom(CM),		% The context module may not be checked yet,
	is_a_module(CM),	% since this is normally done by the callee!
	!,
	error_id(X, Msg), 
	% Avoid loops by recursive calls due to macros:
	% First remove 'm' or 'M' from the output flags so that we don't
	% hit undefined 'print attribute' predicates
	output_mode(Mode),
	string_list(Mode, ModeL),
	(member(0'm, ModeL) ->
	    delete(0'm, ModeL, NewModeL)
	;
	member(0'M, ModeL) ->
	    delete(0'M, ModeL, NewModeL)
	;
	    NewModeL = ModeL
	),
	string_list(NewMode, NewModeL),
	% And then disable write macros. This unfortunately also disables
	% goal macros which would not loop anyway...
	concat_string(['%w %', NewMode, 'Tw in module %w%n'], Format),
	( CM == LM -> QualWhere = Where ; QualWhere = LM:Where ),
	printf_body(error, Format, [Msg,QualWhere,CM], CM),
	error(157, _).
call_handler(_, Where, CM, _) :- 
	error(80, Where@CM).


%-------------------------------------
% Autoload and lazy predicate creation
%-------------------------------------

:- pragma(nodebug).
:- unskipped autoload_handler/4.
:- untraceable autoload_handler/4.
?- local variable(autoload_lock).
?- bag_create(Mutex), setval(autoload_lock,Mutex).
autoload_handler(_N, Goal, CM, LM) :-
	atom(CM),		% The context module is not checked yet,
	is_a_module(CM),	% since this is normally done by the callee!
	!,
	getval(autoload_lock,Mutex),
        ( with_mutex(Mutex,try_create_pred(Goal, LM)) ->
            ( LM==CM ->
                call(Goal)@CM
            ;
                :@(LM, Goal, CM)
            )
	;
	    error(68, Goal, CM)@LM
	).
autoload_handler(_, Goal, CM, _) :- 
	error(80, Goal@CM).


try_create_pred(Goal, LM) :-
        functor(Goal, Name, Arity),
	Pred = Name/Arity,
	( get_flag(Pred, defined, on)@LM ->
	    % already created since the error was raised (e.g. other thread)
	    get_flag(Pred, visibility, V)@LM,
	    (V==exported;V==reexported)

	; is_lazy_pred(LM, Name, Arity, Tool, Body, Proto) ->
	    % Create the body, unless it exists already
	    ( get_flag(Body, defined, on) ->
		true
	    ;
		Body = BName/N1,
		create_call_n(BName, N1)
	    ),
	    % Create the tool, unless it exists already
	    ( get_flag(Tool, tool, on) ->
		true
	    ;
		tool(Tool, Body),
		export(Tool)
	    ),
	    % Create same visibility as Proto
	    get_flag(Proto, visibility, Vis)@LM,
	    ( Vis==imported ->
		(import Pred from sepia_kernel)@LM
	    ; Vis==reexported ->
		(reexport Pred from sepia_kernel)@LM
	    ; Vis==exported, LM \== sepia_kernel ->
		% 'exported' condition covers case like iso_heavy,
		% where call/1-3 are redefined, but rest are reexported.
		(reexport Pred from sepia_kernel)@LM
	    ;
		true
	    )

	; % Autoloading
	    get_flag(Pred, autoload, on)@LM,	% may fail
	    get_flag(Pred, definition_module, HomeModule)@LM,
	    ensure_loaded_skip(library(HomeModule), HomeModule)
	).

is_lazy_pred(LM, Name, Arity, Tool, Body, Proto) :-
        multi_arity_pred(Name, Arity, Tool, Body, Proto),
        arity(Body) =< get_flag(max_predicate_arity),
        % is the visible prototype the standard one?
        get_flag(Proto, definition_module, DM)@LM,
	( DM==sepia_kernel -> true ; DM==iso_heavy ).

multi_arity_pred(call,  N,  call/N, call_/N1, call/1) :- N1 is N+1, N>1.
multi_arity_pred(call_, N1, call/N, call_/N1, call/1) :- N is N1-1, N>1.
%multi_arity_pred((:),   N,  (:)/N,  (:@)/N1,  (:)/2)  :- N1 is N+1, N>2.
%multi_arity_pred((:@),  N1, (:)/N,  (:@)/N1,  (:)/2)  :- N is N1-1, N>2.


% Some hacking here to suppress tracing of metacalls during ensure_loaded
:- pragma(debug).
ensure_loaded_skip(File, Module) :-
	% need the (untraceable) CALL port here for skipping
	ensure_loaded_silent(File, Module).
:- pragma(nodebug).

:- untraceable ensure_loaded_silent/2.
:- skipped ensure_loaded_silent/2.
ensure_loaded_silent(File, Module) :-
	ensure_loaded(File, Module).


%-------------------------------------
% Handler for error 86 - lookup module does not exist.
%-------------------------------------

% Culprit is an ok goal, but LM is an atom but not a module.
% If there is a library called LM, we try to load it.
:- unskipped no_lookup_module_handler/4.
:- untraceable no_lookup_module_handler/4.
no_lookup_module_handler(N, Goal, CM, LM) :- !,
	getval(prolog_suffix, ECLs),
	getval(eclipse_object_suffix, ECO),
	(
	    existing_file(library(LM), [ECO|ECLs], [readable], _),
	    error(133, LM)	% library loading notification
	->
	    ensure_loaded_skip(library(LM), CM),
	    ( is_a_module(LM) ->
		:@(LM, Goal, CM)
	    ;
		error_handler(N, Goal, CM, LM)
	    )
	;
	    error_handler(N, Goal, CM, LM)
	).


%-------------------------------------
% End-of-compilation warnings
%-------------------------------------

    % suppress these warnings until autoloading is done properly
declaration_warning_handler(_N, _Pred, lists) :- !.
declaration_warning_handler(_N, _Pred, profile) :- !.
declaration_warning_handler(75, Pred, Module) :- !,
	get_flag_body(Pred, definition_module, DM, Module),
	get_deprecation_advice(Pred, DM, Advice),
	!,
	warning_handler(75, Pred, Module),
	printf(warning_output, " Advice: %w%n", [Advice]).
    % suppress the warning if there is such a library
declaration_warning_handler(85, BadModule:_, _Module) :-
	known_library(BadModule),
	!.
    % suppress the warning if predicate will be created lazily
declaration_warning_handler(84, LM:N/A, _Module) ?-
	is_lazy_pred(LM, N, A, _, _, _),
	!.
declaration_warning_handler(N, Pred, Module) :-
	warning_handler(N, Pred, Module).

    % modules for which we raise no warning 85
    known_library(daVinci) :- !.	% because not in runtime system
    known_library(ic_gap_sbds) :- !.	% because not in runtime system
    known_library(ic_gap_sbdd) :- !.	% because not in runtime system
    known_library(Module) :-
	getval(prolog_suffix, ECLs),
	getval(eclipse_object_suffix, ECO),
	once existing_file(library(Module), [ECO|ECLs], [readable], _).


%-------------------------------------
% General warnings
%-------------------------------------

warning_handler(X, Where) :-
	write(warning_output, 'WARNING: '),
	warning_message(X, Where).

warning_handler(X, Where, Module) :-
	write(warning_output, 'WARNING: '),
	warning_message(X, Where, Module).


%-------------------------------------
% Undefined global entities
%-------------------------------------

undef_array_handler(N, setval_body(Name, Value, Module), _) :- !,
	undef_array_handler(N, setval(Name, Value), Module).
undef_array_handler(N, getval_body(Name, Value, Module), _) :- !,
	undef_array_handler(N, getval(Name, Value), Module).
undef_array_handler(_N, setval(Name, Value), Module) :-
	atom(Name),
	!,
    	( current_module(M), not is_locked(M), current_array(Name, _)@M ->
	    % there's one in another module, probably error
	    printf(warning_output,
	    	"WARNING: creating local variable(%w) in %w while there exists one in %w%n",
		[Name, Module, M])
	;
	    true	% create it silently
	),
	make_array_(Name, prolog, local, Module),
	setval_body(Name, Value, Module).
undef_array_handler(N, Goal, Module) :-
	error_handler(N, Goal, Module).


% repeated array declaration
make_array_handler(42, Culprit, Module, LM) :-
	make_array_args(Culprit, Array, Type, Visibility),
	!,
	( current_array(Array, [Type,Visibility])@Module ->
	    true	% it's the same
	;
	    warning_handler(42, Culprit),
	    functor(Array, N, A),
	    erase_array_body(N/A, Module),
	    :@(LM,Culprit,Module)
	).
make_array_handler(N, Culprit, Module, LM) :-
	error_handler(N, Culprit, Module, LM).

    make_array_args(make_array(Array, Type), Array, Type, global).
    make_array_args(make_local_array(Array, Type), Array, Type, local).
    make_array_args(local(variable(Array)), Array, prolog, local) :- !.
    make_array_args(local(variable(Array,_)), Array, prolog, local) :- !.
    make_array_args(global(variable(Array)), Array, prolog, global) :- !.
%    make_array_args(local(reference(Array)), Array, reference, local) :- !.
%    make_array_args(global(reference(Array)), Array, reference, global) :- !.
%    make_array_args(local(reference(Array,_)), Array, reference, local) :- !.
    make_array_args(local(array(Array, Type)), Array, Type, local) :- !.
    make_array_args(local(array(Array)), Array, prolog, local) :- !.
    make_array_args(global(array(Array, Type)), Array, Type, global) :- !.
    make_array_args(global(array(Array)), Array, prolog, global) :- !.


undef_record_handler(_N, Culprit) :-
	extract_record_key(Culprit, Key, Module),
	!,
	( current_module(M), not is_locked(M), current_record(Key)@M ->
	    printf(warning_output,
	    	"WARNING: creating local record(%w) in %w while there exists one in %w%n",
		[Key, Module, M])
	;
	    true	% create it silently
	),
	functor(Key, K, A),
	local_record_body(K/A, Module),
	call(Culprit).	% Culprit is a kernel tool body, so call/1 is ok
undef_record_handler(N, Culprit) :-
	error_handler(N, Culprit).

    extract_record_key(recorda_body(Key,_,M), Key, M).
    extract_record_key(recordz_body(Key,_,M), Key, M).
    extract_record_key(recorda_body(Key,_,_,M), Key, M).
    extract_record_key(recordz_body(Key,_,_,M), Key, M).


%-------------------------------------
% Syntax error handling
%-------------------------------------

parser_error_handler(N, Goal, M):- 
	( extract_module(Goal, CM) -> true ; CM = M ),
	error_id(N, Id), 
	( extract_stream(Goal, Stream) ->
	    get_context_and_skip_forward(Stream, Context),
	    ( extract_syntax_error_option(Goal, Option), Option\==default ->
		true
	    ; get_flag(syntax_option, syntax_errors_fail)@CM ->
		Option = fail
	    ;
		Option = error
	    ),
	    ( Option==quiet ->
		fail
	    ; Option==fail ->
		% old ECLiPSe style: print error directly, then fail
		print_syntax_error(Id, Context),
		fail
	    ; % Option==error and default
		% ISO style: throw error term
	        throw(error(syntax_error(Id), Context))
	    )
	;
	    error_message(N, Goal),
	    fail
	).


% Print syntax error, can be done from handler or after throw/catch
print_syntax_error(Id, context(_Stream, Device, Name, Line, String, From, Where)) ?- !,
	% Don't use Stream, it may be closed/stale.
	( Device==tty ->
	    true	% no need
	;
	    printf(error, "%s %w", [Device,Name]),
	    ( Line > 1 -> printf(error, ", line %d", [Line]) ; true ),
	    printf(error, ": ", [])
	),
	printf(error, "syntax error: %s%n", Id),
	( String == "" ->
	    true
	;
	    printf(error, "| %s%n", String),
	    Num is Where - From - 1,
	    string_print_length(String, 2, Num, Skip),
	    printf(error, "| %*c^ here%n", [Skip, 0' ])
	),
	flush(error).
print_syntax_error(Id, Context) :-
	printf(error, "syntax error: %s in %w%n%b", [Id,Context]).


get_context_and_skip_forward(Stream,
		context(Stream, DevName, Name, ErrLine, String, From, Where)) :-
	stream_info_(Stream, 13, Device),
	stream_info_(Stream, 6, Where),
	short_stream_name(Device, DevName, Stream, Name),
	stream_info_(Stream, 5, Line),
	get_context_strings(Device, Stream, Where, From, Left, Right, NewLine),
	concat_strings(Left, Right, String),
	ErrLine is Line-NewLine,
	set_stream_prop_(Stream, 5, Line).	% reset the line number


% Get some left and right context of the error. This is rather tricky,
% especially when we can't freely seek on the device.  Also, skip further
% input, how much depends on what device we are reading from.
% Make sure line numbers are repaired after seeking.
get_context_strings(Device, Stream, Pos, From, Left, Right, NewLine) :-
	( Device==file ; Device==string ),	% fully seekable devices
	!,
	seek_left_context(Stream, 70, 0, Pos, From, Left, NewLine),
	% skip forward to fullstop
	skip_to_eocl(Stream),
	% get limited amount of right context
	( NewLine > 0 ->
	    Right = ""
	;
	    at(Stream, EndPos),
	    stream_info_(Stream, 5, Line),	% save
	    MaxRight is 80-(From-Pos),
	    seek(Stream, Pos),
	    N is min(EndPos-Pos,MaxRight),
	    read_string(Stream, end_of_line, N, Right),
	    seek(Stream, EndPos),
	    set_stream_prop_(Stream, 5, Line)	% restore
	).
get_context_strings(Device, Stream, Pos, From, Left, Right, NewLine) :-
	( Device==pipe ; Device==socket ; Device==tty ), % buffer seekable
	!,
	stream_info_(Stream, 14, SeekLimit),	% buffer start
	seek_left_context(Stream, 70, SeekLimit, Pos, From, Left, NewLine),
	( Device==tty ->
	    % For tty, skip to end of line, unless already there
	    ( NewLine > 0 -> Skipped=""
	    ; read_string(Stream, end_of_line, _, Skipped)
	    )
	;
	    % Do a rough skip, as we can't seek back to get the context
	    skip_to_eocl_collect(Stream, Cs),
	    string_list(Skipped, Cs)
	),
	% get limited amount of right context
	( NewLine > 0 ->
	    Right = ""
	;
	    MaxRight is 80-(From-Pos),
	    split_string(Skipped, "\n", "", [RestLine|_]),
	    ( MaxRight < string_length(RestLine) ->
		first_substring(RestLine, 1, MaxRight, Right)
	    ;
		Right = RestLine
	    )
	).
get_context_strings(_Device, _Stream, _Pos, 0, "", "", 0).  % queue or null


    % Get context left of current position Pos, maximum Max bytes.
    % Return starting position From, string Left, and line end flag NewLine
    seek_left_context(Stream, Max, SeekLimit, Pos, From, Left, NewLine) :-
	stream_info_(Stream, 5, Line),		% save
	Back is min(Pos-SeekLimit,Max),
	BackPos is Pos-Back,
	seek(Stream, BackPos),
	read_string(Stream, "", Back, Left1),
	split_string(Left1, "\n", "", LeftParts),
	last_nonempty_string(LeftParts, Left, NewLine),
	From is Pos-string_length(Left)-NewLine,
	set_stream_prop_(Stream, 5, Line).	% restore

    last_nonempty_string([S|Ss], Last, NewLine) :-
	( Ss=[] -> Last=S, NewLine=0
	; Ss=[""] -> Last=S, NewLine=1
	; last_nonempty_string(Ss, Last, NewLine)
	).


% For seekable streams: skip token-wise to fullstop or end of stream
skip_to_eocl(Stream) :-
	( at_eof(Stream) ->
	    true
	;
	    read_token(Stream, _, Class),
	    ( Class==fullstop -> true 
	    ; Class==end_of_file -> true 
	    ; skip_to_eocl(Stream)
	    )
	).

% Skip to something that looks like fullstop, collecting the skipped text
skip_to_eocl_collect(Stream, Cs) :-
	( at_eof(Stream) -> Cs=[] ;
	    get(Stream, C),
	    ( C < 0 -> Cs=[]
	    ; C==0'. -> Cs=[C|Cs1], skip_to_eocl_collect1(Stream, Cs1)
	    ; get_chtab(C, terminator) -> Cs=[C]
	    ; Cs=[C|Cs1], skip_to_eocl_collect(Stream, Cs1)
	    )
	).

    skip_to_eocl_collect1(Stream, Cs) :-
	( at_eof(Stream) -> Cs=[] ;
	    get(Stream, C),
	    ( C < 0 -> Cs=[]
	    ; get_chtab(C, blank_space) -> Cs=[]
	    ; get_chtab(C, end_of_line) -> Cs=[]
	    ; C==0'. -> Cs=[C|Cs1], skip_to_eocl_collect1(Stream, Cs1)
	    ; Cs=[C|Cs1], skip_to_eocl_collect(Stream, Cs1)
	    )
	).

:- mode short_stream_name(+,-,+,-).
short_stream_name(file, file, Stream, File) :- !,
	stream_info_(Stream, 0, Name),
	local_file_name(Name, File).
short_stream_name(queue, 'queue stream', Stream, Stream) :- !.
short_stream_name(string, 'string stream', Stream, Stream) :- !.
short_stream_name(null, 'null stream', _Stream, null) :- !.
short_stream_name(Device, Device, Stream, Name) :-	% tty,socket,pipe,null
	stream_info_(Stream, 0, Name).


%-------------------------------------

singleton_in_loop(N, Occurrence) :-
	( Occurrence = quantified(Name, Location) ->
	    printf(warning_output,
		"*** Warning: Singleton local variable %a in do-loop (not used in loop body)%n",
		[Name])
	; Occurrence = unquantified(Name, Location) ->
	    printf(warning_output,
		"*** Warning: Singleton local variable %a in do-loop, maybe param(%a) missing?%n",
		[Name,Name])
	;
	    error_handler(N, Occurrence)
	),
	( nonvar(Location), annotated_term{line:Line,file:File} = Location ->
	    printf(warning_output, "in line %d in file %s%n", [Line, File])
	;
	    true
	),
	flush(warning_output).
	
% extract_stream(Goal, Stream)
:- mode extract_stream(+, -).
extract_stream(read(_), input).
extract_stream(read_(_, _), input).
extract_stream(readvar(S, _, _), S).
extract_stream(readvar(S, _, _, _), S).
extract_stream(read_term(S, _, _, _, _, _, _), S).
extract_stream(read_string(_, _, _), input).
extract_stream(read_string(S, _, _, _), S).
extract_stream(read_string(S, _, _, _, _), S).
extract_stream(read(S, _), S).
extract_stream(read_(S, _, _), S).
extract_stream(read_token(S, _, _), S).
extract_stream(read_token_(S, _, _, _), S).
extract_stream(read_exdr(S, _), S).
extract_stream(compile_stream(S), S).
extract_stream(compile_stream_(S, _), S).
extract_stream(get(_), input).
extract_stream(get(S, _), S).
extract_stream(get0(_), input).
extract_stream(get0(S, _), S).
extract_stream(get_char(_), input).
extract_stream(get_char(S, _), S).
extract_stream(getw(S, _), S).
extract_stream(tyi(_), input).
extract_stream(tyi(S, _), S).
extract_stream(tyo(_), output).
extract_stream(tyo(S, _), S).
extract_stream(flush(S), S).
extract_stream(format(_, _), output).
extract_stream(format(S, _, _), S).
extract_stream(format_body(_, _, _), output).
extract_stream(format_body(S, _, _, _), S).
extract_stream(printf(_, _), output).
extract_stream(printf(S, _, _), S).
extract_stream(printf_body(_, _, _), output).
extract_stream(printf_body(S, _, _, _), S).
extract_stream(write(_), output).
extract_stream(write(S, _), S).
extract_stream(write_(_, _), output).
extract_stream(write_(S, _, _), S).
extract_stream(write_term(S,_,_,_,_,_,_,_), S).
extract_stream(writeln_body(_,_), output).
extract_stream(writeln_body(S,_,_), S).
extract_stream(writeln(_), output).
extract_stream(writeln(S,_), S).
extract_stream(nl, output).
extract_stream(nl(S), S).
extract_stream(close(S), S).

% This should be replaced with a more generic way of getting
% the context module from a tool body goal
:- mode extract_module(+, -).
extract_module(read_(_, M), M).
extract_module(readvar(_, _, _, M), M).
extract_module(read_term(_, _, _, _, _, _, M), M).
extract_module(read_(_, _, M), M).
extract_module(read_token_(_, _, _, M), M).
extract_module(compile_stream_(_, M), M).
extract_module(format_body(_, _, M), M).
extract_module(format_body(_, _, _, M), M).
extract_module(printf_body(_, _, M), M).
extract_module(printf_body(_, _, _, M), M).
extract_module(write_(_, M), M).
extract_module(write_(_, _, M), M).
extract_module(writeln_body(_,M), M).
extract_module(writeln_body(_,_,M), M).

extract_syntax_error_option(read_term(_,_,_,Option,_,_,_), Option).


%-------------------------------------
% I/O event handling
%-------------------------------------

% eof_handler/4 - take the appropriate action for each culprit
% CARE: eof_handler/4 fails for other culprits

eof_handler(N, Goal, Module, LM) :-
	extract_stream(Goal, Stream),
	( stream_info_(Stream, 19, on) ->	% yield
	    stream_info_(Stream, 4, PhysicalStream),
	    (is_remote_sync_queue(PhysicalStream, _, ControlStream) ->
		remote_input(PhysicalStream, ControlStream)
	    ;
		yield(6, PhysicalStream, _)	% 6 == PWAITIO == EC_waitio
	    ),
	    :@(LM, Goal, Module)
	;
	    eof_handler(N, Goal)
	).


:- mode eof_handler(++, +).
eof_handler(_, read(end_of_file)).
eof_handler(_, read_(end_of_file, _)).
eof_handler(_, read(_, end_of_file)).
eof_handler(_, read_(_, end_of_file, _)).
eof_handler(_, read_exdr(_, _)) :- fail.
eof_handler(_, readvar(_, end_of_file, [])).
eof_handler(_, readvar(_, end_of_file, [], _)).
eof_handler(_, read_token(_, end_of_file, end_of_file)).
eof_handler(_, read_token_(_, end_of_file, end_of_file, _)).
eof_handler(_, read_string(_, Len, "")) :- ( integer(Len) -> true ; Len=0 ).
eof_handler(_, read_string(_, _, _, _)) :- fail.
eof_handler(_, read_string(_, _, _, -1, "")).
eof_handler(_, compile_stream(_)).
eof_handler(_, compile_stream_(_,_)).
eof_handler(_, get(-1)).
eof_handler(_, get(_, -1)).
eof_handler(_, get0(-1)).
eof_handler(_, get0(_, -1)).
eof_handler(_, tyi(-1)).
eof_handler(_, tyi(_, -1)).
eof_handler(_, get_char(_)) :- fail.
eof_handler(_, get_char(_, _)) :- fail.
eof_handler(_, getw(_, end_of_file)).
eof_handler(_, read_term(S,Term,Flags,_,[],0,_)) :-
	( getbit(Flags, 1, 1) ->	% LAYOUT_PLEASE?
	    Term = annotated_term(end_of_file,end_of_file,File,Line,End,End),
	    stream_info_(S, 0 /*name*/, File),
	    stream_info_(S, 5 /*line*/, Line),
	    at(S, End)
	;
	    Term = end_of_file
	).


past_eof_handler(N, Goal) :-
	extract_stream(Goal, Stream),
	stream_info_(Stream, 37, Action),	% eof_action
	( Action == error ->
	    close(Stream, [force(true)]),
	    error_handler(N, Goal)
	;
	    % Action == eof_code ->
	    % Action == reset ->	% should never happen!
	    eof_handler(N, Goal)
	).


%-------------------------------------
% Compilation related handlers
%-------------------------------------

compiler_warning_handler(N, Proc) :-
	( ( nonvar(Proc), Proc=Term@File:Line
	  ; compiled_file(File, Line), Term=Proc) ->
	    write(error, '\n*** '),
	    error_id(N, M), 
	    write(error, M), 
	    write(error, ': '),
	    printf_with_current_modes(error, Term), 
	    (Line > 0 ->
		    printf(error, "\n\tbefore line %d in the file %s",
			[Line, File])
	    ;
		    true
	    ),
	    nl(error),
	    flush(error)
	;
	    error_handler(N, Proc)
	).

compiler_error_handler(N, Proc) :-
	compiler_warning_handler(N, Proc),
	fail.

compiler_abort_handler(N, File, _Module) :-
	error_id(N, M), 
	printf(error, "\n*** %s", M),
	(compiled_file(File, Line) ->
	    (Line > 0 ->
		    printf(error, "\n\tbefore line %d in the file %s",
			[Line, File])
	    ;
		    true
	    )
	;
	    printf(error, " in the file %s\n", File)
	),
	nl(error),
	flush(error).

pragma_handler(148, pragma(Pragma), Module) :-
	record_pragma(Pragma, Module), !.
pragma_handler(N, Proc, _Module) :-
	compiler_error_handler(N, Proc).


compiled_file_handler(N, (File, Size, Time), Module) :- !,
	compiled_file_handler(N, File, Size, Time, Module).
compiled_file_handler(N, Goal, Module) :-
	error_handler(N, Goal, Module).

compiled_file_handler(_, term, _, _, _) :- !.
compiled_file_handler(_, File, Size, Time, _Module) :-
	( File = source(Source) ->
	    true
	;
	    local_file_name(File, Source)
	),
	( Size < 0 ->
	    printf(log_output, "%-10s loaded in %.2f seconds\n%b",
	    	[Source, Time])
	;
	    printf(log_output, "%-10s compiled %d bytes in %.2f seconds\n%b",
		[Source, Size, Time])
	).


% end of loading a code unit: do any finishing up work
unit_loaded_handler(_, Options, Module) :-
	run_stored_goals(initialization_goals, Module),
	( memberchk(check, Options) ->
	    record(compiled_modules, Module)
	;
	    true
	).


record_compiled_file_handler(_, File-Goal, Module) :-
	canonical_path_name(File, CanonicalFile0),
	( string(CanonicalFile0) ->
	    atom_string(CanonicalFile, CanonicalFile0)
	;
	    CanonicalFile = CanonicalFile0
	),
	record_compiled_file(CanonicalFile, Goal, Module).


local_file_name(File:Line, LocalF:Line) :- !,
	local_file_name(File, LocalF).
local_file_name(File, LocalF) :-
	getcwd(Cwd),
	atom_string(File, FileS),
	(substring(FileS, Cwd, 1) ->
	    Pos is string_length(Cwd) + 1,
	    Len is string_length(FileS) - Pos + 1,
	    first_substring(FileS, Pos, Len, LocalF)
	;
	    LocalF = File
	).

:- export redef_other_file_handler/2.
redef_other_file_handler(_, (Pred, OldFile0, NewFile0)) :-
	local_file_name(OldFile0, OldFile),
	local_file_name(NewFile0, NewFile),
	printf(warning_output, "WARNING: %w in file %w replaces previous definition in file %w%n",
		 [Pred,NewFile,OldFile]).


:- mode library_module(++, -).
library_module(library(File), File) :- !.
library_module(File, File).

error_message(X, Where):- 
	error_id(X, M), 
	write(error, M), 
	write(error, ' in '),
	printf_goal(error, Where), 
	nl(error),
	flush(error).


% What's all these different modules?
% 
% 				CM	LM	TrueLM	UsedLM
% :- module(lm).
% ?- lm1:p(X).			lm	lm	lm1	lm1
% prints "error in lm1:p(X)" using lm1's syntax
%
% :- module(lm).
% :- import p/1 from lm1.
% ?- lm1:p(X).			lm	lm	lm1	lm
% prints "error in p(X)" using lm's syntax
% ?- p(X).			lm	lm	lm	lm
% prints "error in p(X)" in lm's syntax
%
% :- module(lm).
% ?- lm1:p(X)@cm.		cm	lm	lm1	lm1
% prints "error in lm1:p(X) in module cm" using lm1's syntax
%
% :- module(lm).
% :- import p/1 from lm1.
% ?- lm1:p(X)@cm.		cm	lm	lm1	lm
% prints "error in p(X) in module cm" using lm's syntax
% ?- p(X)@cm.			cm	lm	lm	lm
% prints "error in p(X) in module cm" using lm's syntax


error_message(X, Goal, CM, LM):- 
	error_id(X, M),
	write(error, M),
	write(error, ' in '),

	% Strip off any module qualifier to find the true lookup module
	unqualify(Goal, LM, TrueLM, PlainGoal),

	% Add back a qualifier only if predicate not anyway visible in LM
	qualify_goal_if_needed(PlainGoal, LM, TrueLM, QualGoal, UsedLM),

	% Print the goal using the syntax from one of the lookup modules,
	% to make sure we have the relevant goal output transformations.
	% We prefer LM to TrueLM because that might have some user's trans-
	% formations in addition, which may be needed for goal's arguments.
	( is_a_module(UsedLM) ->
	    printf_goal_body(error, QualGoal, UsedLM)
	;
	    printf_goal(error, QualGoal)
	),

	% If we have an interesting context module, print it
	( atom(CM), is_a_module(CM), not is_locked(CM), CM \== LM ->
	    write(error, ' in module '),
	    write(error, CM)
	;
	    true
	),
	nl(error),
	flush(error).


warning_message(X, Where):- 
	error_id(X, M), 
	write(warning_output, M), 
	write(warning_output, ' in '),
	printf_goal(warning_output, Where), 
	nl(warning_output),
	flush(warning_output).

warning_message(X, Where, Module):- 
	error_id(X, M),
	write(warning_output, M),
	write(warning_output, ' in '),
	printf_goal_body(warning_output, Where, Module),
	write(warning_output, ' in module '),
	write(warning_output, Module),
	nl(warning_output),
	flush(warning_output).

/* Finally boot_error/2 can be properly redefined. It is used
 * as error handler when no error handler can be found
 */
boot_error(N, Goal) :-
	write(error, 'no error handler: '),
	( error_message(N, Goal) ->
	    compiled_file(File, Line),
	    (Line > 0 ->
		    printf(error, "\n\tbefore line %d in the file %s",
			[Line, File])
	    ;
		    true
	    ),
	    nl(error),
	    exit0(-1)	% to avoids loops in error 152 in exit/1
	;
	    writeln(error, N)
	).


% This handler is called when we were trying to close one of the predefined
% streams, whether explicitly or via their handle or another alias.

close_handler(E, close(Stream, Options)) ?- !,
	get_stream(Stream, Handle),
	( default_stream(_, Stream) ->
	    % Don't close stdin etc.
	    flush_if_output(Stream)

	; default_stream(_, FixedStream),
	  get_stream(FixedStream, Handle) ->
	    % Trying to close another alias or the handle of a fixed stream:
	    % don't close it!  Erase alias, unless a predefined one.
	    flush_if_output(Stream),
	    erase_alias(Stream)

	; default_stream(Stream, FixedStream) ->
	    % Allow closing default streams explicitly via the user_xxx alias.
	    % Close user_xxx's handle after setting alias back to stdxxx.
	    set_stream(Stream, FixedStream),
	    close(Handle, Options)

	; default_stream(DefaultStream, _),
	  get_stream(DefaultStream, Handle) ->
	    % Trying to close a stream that is in use as a default stream:
	    % don't close it!  Erase alias, unless a predefined one.
	    flush_if_output(Stream),
	    erase_alias(Stream)

	; current_stream(Stream, DefaultStream) ->
	    % close current stream handle after setting alias back to default
	    set_stream(Stream, DefaultStream),
	    close(Handle, Options)

	; current_stream(CurrentStream, DefaultStream),
	  get_stream(CurrentStream, Handle) ->
	    % reset current stream that was redirected to Handle, and try again
	    set_stream(CurrentStream, DefaultStream),
	    close(Stream, Options)
	;
	    % should not occur
	    error_handler(E, close(Stream, Options))
	).
close_handler(_, close(Stream)) ?- !,
	close_handler(_, close(Stream, [])).
close_handler(ErrorNumber, Goal) :-
        error_handler(ErrorNumber, Goal).

    % The 'current' streams, and the 'default' streams to reset them to
    :- mode current_stream(?,-).
    current_stream(input,	user_input).
    current_stream(output,	user_output).
    current_stream(warning_output, user_output).
    current_stream(log_output,	user_output).
    current_stream(error,	user_error).

    % The 'default' streams, and the 'fixed' streams to reset them to
    default_stream(user_input,	stdin).
    default_stream(user_output,	stdout).
    default_stream(user_error,	stderr).
    default_stream(null,	null).

    erase_alias(stdin) :- !.
    erase_alias(stdout) :- !.
    erase_alias(stderr) :- !.
    erase_alias(user_input) :- !.
    erase_alias(user_output) :- !.
    erase_alias(user_error) :- !.
    erase_alias(input) :- !.
    erase_alias(output) :- !.
    erase_alias(error) :- !.
    erase_alias(warning_output) :- !.
    erase_alias(log_output) :- !.
    erase_alias(null) :- !.
    erase_alias(Stream) :- atom(Stream), !, erase_stream_property(Stream).
    erase_alias(_).

    flush_if_output(Stream) :-
	( stream_info_(Stream, 35/*output*/, true) -> flush(Stream) ; true ).


% Currently only used for output goals
io_yield_handler(_, Goal) :-
	(
	    extract_stream(Goal, Stream),	% should not fail, but...
	    stream_info_(Stream, 4, PhysicalStream),
	    do_stream_yield(PhysicalStream),
	    fail		% recover memory used during yielding
	;
	    true
	).

do_stream_yield(PhysicalStream) :-
	(is_remote_sync_queue(PhysicalStream, RemoteStream, ControlStream) ->
	    remote_output(PhysicalStream, ControlStream, RemoteStream)
	;   yield(7, PhysicalStream, _)
	    % 7 == PFLUSHIO == EC_flushio
        ).


% This is the handler for all errors from the operating system.  It has
% special treatment for "Interrupted system call" and will restart certain
% builtins in that case.  The advantage of doing this through the handler
% rather than directly in C is that this gives the system a chance to
% run a synchronous interrupt handler before the goal gets restarted.

system_error_handler(E, Goal, CM, LM):-
	errno_id(Msg),
	( Msg = "Interrupted system call", restartable_builtin(Goal) ->
	    :@(LM, Goal, CM)
	;
	    error_id(E, EclMsg),
	    printf(error, "%w: %w in ", [EclMsg, Msg]),
	    printf_goal(error, Goal), 
	    nl(error),
	    flush(error),
	    error(157, _)
	).
	
    % Builtins that can raise EINTR and can be restarted after that
    restartable_builtin(accept(_,_,_)).
    restartable_builtin(cd(_)).
    restartable_builtin(open(_,_,_)).
    restartable_builtin(close(_)).
    restartable_builtin(close(_,_)).
    restartable_builtin(connect(_,_)).
    restartable_builtin(stream_select(_,_,_)).
    restartable_builtin(wait(_,_,_)).


dynamic_handler(_, dynamic(Name/Arity), Module) :-
	!,
	functor(F, Name, Arity),
	retract_all_body(F, Module).
dynamic_handler(N, Proc, Module) :-
	error_handler(N, Proc, Module).

macro_handler(N, define_macro(T, P, F), M) :- !,
	macro_handler(N, define_macro_(T, P, F, M), _).
macro_handler(N, define_macro_(T, QP, F, M), _) :-
	unqualify(QP, M, LMnew, P),
	(
	    current_macro_body(T, P, F1, LMold, M),
	    same_macro_flags(F, F1),
	    same_trans_pred(P, LMnew, LMold)
	->
	    true	% don't warn, definitions are the same
	;
	    warning_handler(N, define_macro(T, P, F), M),
	    erase_macro_(T, F, M),
	    define_macro_(T, P, F, M)
	).

    same_macro_flags(A, B) :-
	subtract(A, [local,read,term], A1), sort(A1, NormFlags),
	subtract(B, [local,read,term], B1), sort(B1, NormFlags).

    same_trans_pred(_P, M, M) :- !.
    same_trans_pred(P, M1, M2) :-
	get_flag_body(P, definition_module, DM, M1),
	get_flag_body(P, definition_module, DM, M2).


%-------------------------------------
% Arithmetic handlers
%-------------------------------------

integer_overflow_handler(E, Goal) :-
	Goal =.. [F,X|T],
	( bignum(X, BigX) ->		% convert one arg to bignum if possible
	    NewGoal =.. [F,BigX|T],
	    call(NewGoal)		% redo the operation with bignums
	;
	    error_handler(E, Goal)
	).

% This one is called when an argument of a comparison
% is neither a number nor a free variable.
% The arguments are evaluated and the goal is re-called.

compare_handler(_, Goal, CM, LM) :-
	functor(Goal, F, A),
	arg(1, Goal, X),
	arg(2, Goal, Y),
	( A > 2 ->
	    arg(3, Goal, M),		% for >= 6.0 culprit is tool body
	    functor(NewGoal, F, 2),
	    arg(1, NewGoal, X1),
	    arg(2, NewGoal, Y1)
	;
	    functor(NewGoal, F, A),	% up to 5.10 culprit is tool
	    arg(1, NewGoal, X1),
	    arg(2, NewGoal, Y1),
	    M = CM
	),
	call(X1 is X)@M,		% call the visible is/2 (e.g. for iso)
	call(Y1 is Y)@M,
	( number(X1), number(Y1) ->
	    :@(LM,NewGoal,M)
	; var(X1) ->
	    :@(LM,NewGoal,M)
	; var(Y1) ->
	    :@(LM,NewGoal,M)
	;
	    error(24, NewGoal, M)
	).


%-------------------------------------
% Module related handlers
%-------------------------------------

% allow certain things even if the module is locked

locked_access_handler(_, unskipped PredSpec) :-
	unskipping_allowed(PredSpec),
	!,
	unskipped PredSpec.
locked_access_handler(_, export PredSpec) :-
	exporting_allowed(PredSpec),
	!,
	export PredSpec.
locked_access_handler(E, Goal) :-
	error_handler(E, Goal).

% allow certain kernel predicates to be made unskipped

unskipping_allowed((is)/2).
unskipping_allowed((>)/2).
unskipping_allowed((<)/2).
unskipping_allowed((>=)/2).
unskipping_allowed((=<)/2).
unskipping_allowed((=:=)/2).
unskipping_allowed((=\=)/2).

% and certain not to be global any longer

exporting_allowed(wake/0).


ambiguous_import_resolve(_, Pred, Module) :-
	resolvable_predicate(Pred),
	% if there is a unique module exporting a 'built_in', use it
	findall(EM, importable_from(Module, Pred, built_in, EM), [M]),
	(import Pred from M) @ Module.

    importable_from(Module, Pred, Type, M) :-
	get_module_info(Module, imports, Imports),
	member(M, Imports),
	current_module_predicate(exported_reexported, Pred)@M,
	get_flag(Pred, type, Type)@M.

ambiguous_import_warn(_, Pred, Module) :-
	findall(EM, importable_from(Module, Pred, _any, EM), ExportingModules),
	printf(warning_output, "Ambiguous import of %w from %w in module %w%n",
	    [Pred, ExportingModules, Module]).

    % For these predicates, ambiguous imports will be resolved
    % in favour of definitions with the built_in/system property.
    resolvable_predicate((>)/2).
    resolvable_predicate((<)/2).
    resolvable_predicate((>=)/2).
    resolvable_predicate((=<)/2).
    resolvable_predicate((=:=)/2).
    resolvable_predicate((=\=)/2).


%-------------------------------------
% Optimization message handler
%-------------------------------------

cost_handler(_, (Cost, _)) :-
	printf("Found a solution with cost %w%n%b", Cost).
cost_handler(_, no(Cost, _)) :-
	printf("Found no solution with cost %w%n%b", Cost).


%-------------------------------------
% Symbolic waking triggers
%-------------------------------------

?- makeref_(trigger_suspensions, [], sepia_kernel).

% The postponed list is separate because it is also accessed from C
% Moreover, the postponed list is emptied on waking. This makes a difference
% for demons (which would otherwise stay on the list). This semantics
% seems more useful, because demon predicates are often not aware that
% they have been transferred to the postponed-list and therefore cause
% looping when they stay on the list. Conceptually, every postponed-list
% is woken exactly once, and a fresh postponed list is created at that time.

:- export
	attach_suspensions/2,
	attached_suspensions/2,
	schedule_suspensions/1,
        current_trigger/1,
	trigger/1.

trigger(postponed) :- !,
	trigger_postponed.
trigger(N) :-
	schedule_suspensions(N),
	wake.

trigger_postponed :-
	get_postponed_nonempty(WL),	% and reinitialise
	!,
	schedule_suspensions(2,WL),
	wake,
	trigger_postponed.
trigger_postponed.


attached_suspensions(N, Susps) :-
	atom(N), !,
	( find_trigger(N, WL) ->
	    arg(2, WL, Susps)
	;
	    Susps = []
	).
attached_suspensions(N, Susps) :-
	nonvar(N), !,
	error(5, attached_suspensions(N, Susps)).
attached_suspensions(N, Susps) :-
	error(4, attached_suspensions(N, Susps)).


schedule_suspensions(N) :-
	( find_trigger(N, WL) ->
	    schedule_suspensions(2,WL)
	;
	    true
	).


    find_trigger(postponed, ESusp) :- !,
	get_postponed_nonempty(ESusp).	% and reinitialise
    find_trigger(T, WL) :-
	getref(trigger_suspensions, List),
	find_trigger(List, T, WL).

    :- mode find_trigger(+,+,-).
    find_trigger([ESusp|ESusps], T, WL) :-
	( ESusp = es(T,_) ->
	    WL = ESusp
	;
	    find_trigger(ESusps, T, WL)
	).

    enter_trigger(postponed, ESusp) :- !,
	get_postponed(ESusp).
    enter_trigger(T, WL) :-
	getref(trigger_suspensions, List),
	( find_trigger(List, T, WL) ->	% and reinitialise
	    true
	;
	    WL = es(T,[]),
	    setref(trigger_suspensions,[WL|List])
	).


current_trigger(postponed).
current_trigger(Trigger) :-
        getref(trigger_suspensions, List),
        member(es(Trigger, _), List).


attach_suspensions(postponed, Susp) ?- !,
	postpone_suspensions(Susp).
attach_suspensions(Trigger, Susp) :-
	atom(Trigger), !,
	attach_suspensions1(Trigger, Susp).
attach_suspensions(Trigger, Susp) :-
	nonvar(Trigger), !, 
	error(5, attach_suspensions(Trigger, Susp)).
attach_suspensions(Trigger, Susp) :-
	error(4, attach_suspensions(Trigger, Susp)).

attach_suspensions1(Trigger, Susp) :-
	var(Susp), !,
	error(4, attach_suspensions(Trigger, Susp)).
attach_suspensions1(_Trigger, []) :- !.
attach_suspensions1(Trigger, Susps) :-
	Susps = [_|_], !,
	enter_trigger(Trigger, Entry),
	enter_suspensions_list(Trigger, Entry, Susps).
attach_suspensions1(Trigger, Susp) :-
	atomic(Susp), is_suspension(Susp), !,
	enter_trigger(Trigger, Entry),
	enter_suspension_list(2, Entry, Susp).
attach_suspensions1(Trigger, Susp) :-
	error(5, attach_suspensions(Trigger, Susp)).

    enter_suspensions_list(Trigger, _Entry, Susps) :-
    	var(Susps), !,
	error(4, attach_suspensions(Trigger, Susps)).
    enter_suspensions_list(_, _, []) :- !.
    enter_suspensions_list(Trigger, Entry, [Susp|Susps]) :- !,
	enter_suspension_list(2, Entry, Susp),
	enter_suspensions_list(Trigger, Entry, Susps).
    enter_suspensions_list(Trigger, _Entry, Susps) :-
	error(5, attach_suspensions(Trigger, Susps)).


% Specialised code for attach_suspensions(postponed, Susp):
% This is not strictly necessary, but we can clean up the postponed
% list slightly more eagerly than an arbitrary suspension list.
postpone_suspensions(Susp) :-
	var(Susp), !,
	error(4, attach_suspensions(postponed, Susp)).
postpone_suspensions([]) :- !.
postpone_suspensions(Susps) :-
	Susps = [_|_], !,
	postpone_suspensions(1, s(Susps)).
postpone_suspensions(Susp) :-
	atomic(Susp), is_suspension(Susp), !,
	postpone_suspensions(1, s([Susp])).
postpone_suspensions(Susp) :-
	error(5, attach_suspensions(postponed, Susp)).



%-------------------------------------
% default error handler definitions
%-------------------------------------

?- set_default_error_handler_(1, error_handler/2, sepia_kernel),
   set_default_error_handler_(2, error_handler/2, sepia_kernel),
   set_default_error_handler_(3, error_handler/2, sepia_kernel),
   set_default_error_handler_(4, error_handler/4, sepia_kernel),
   set_default_error_handler_(5, error_handler/4, sepia_kernel),
   set_default_error_handler_(6, error_handler/4, sepia_kernel),
   set_default_error_handler_(7, error_handler/2, sepia_kernel),
   set_default_error_handler_(8, error_handler/2, sepia_kernel),
   set_default_error_handler_(11, true/0, sepia_kernel), % set in meta.pl
   set_default_error_handler_(15, fail/0, sepia_kernel),
   set_default_error_handler_(16, fail/0, sepia_kernel),
   set_default_error_handler_(17, error_handler/2, sepia_kernel),
   set_default_error_handler_(20, error_handler/2, sepia_kernel),
   set_default_error_handler_(21, error_handler/4, sepia_kernel),
   set_default_error_handler_(23, compare_handler/4, sepia_kernel),
   set_default_error_handler_(24, error_handler/2, sepia_kernel),
   set_default_error_handler_(25, integer_overflow_handler/2, sepia_kernel),
   set_default_error_handler_(30, error_handler/2, sepia_kernel),
   set_default_error_handler_(31, error_handler/2, sepia_kernel),
   set_default_error_handler_(32, warning_handler/2, sepia_kernel),
   set_default_error_handler_(33, error_handler/2, sepia_kernel),
   set_default_error_handler_(40, error_handler/2, sepia_kernel),
   set_default_error_handler_(41, undef_array_handler/3, sepia_kernel),
   set_default_error_handler_(42, make_array_handler/4, sepia_kernel),
   set_default_error_handler_(43, error_handler/2, sepia_kernel),
   set_default_error_handler_(44, error_handler/2, sepia_kernel),
   set_default_error_handler_(45, undef_record_handler/2, sepia_kernel),
   set_default_error_handler_(50, error_handler/2, sepia_kernel),
   set_default_error_handler_(60, error_handler/4, sepia_kernel),
   set_default_error_handler_(61, error_handler/4, sepia_kernel),
   set_default_error_handler_(62, error_handler/4, sepia_kernel), 
   set_default_error_handler_(63, error_handler/4, sepia_kernel), 
   set_default_error_handler_(64, dynamic_handler/3, sepia_kernel), 
   set_default_error_handler_(65, error_handler/4, sepia_kernel), 
   set_default_error_handler_(66, error_handler/4, sepia_kernel), 
   set_default_error_handler_(67, error_handler/4, sepia_kernel),
   set_default_error_handler_(68, call_handler/4, sepia_kernel),
   set_default_error_handler_(69, autoload_handler/4, sepia_kernel),
   set_default_error_handler_(70, undef_dynamic_handler/3, sepia_kernel),
   set_default_error_handler_(71, error_handler/2, sepia_kernel),
   set_default_error_handler_(72, error_handler/2, sepia_kernel),
   set_default_error_handler_(73, true/0, sepia_kernel),
   set_default_error_handler_(74, true/0, sepia_kernel),
   set_default_error_handler_(75, declaration_warning_handler/3, sepia_kernel),
   set_default_error_handler_(76, declaration_warning_handler/3, sepia_kernel),
   set_default_error_handler_(77, declaration_warning_handler/3, sepia_kernel),
   set_default_error_handler_(78, error_handler/2, sepia_kernel),
   set_default_error_handler_(79, call_dynamic_/3, sepia_kernel),
   set_default_error_handler_(80, error_handler/2, sepia_kernel),
   set_default_error_handler_(81, error_handler/2, sepia_kernel),
   set_default_error_handler_(82, locked_access_handler/2, sepia_kernel),
   set_default_error_handler_(83, warning_handler/2, sepia_kernel), 
   set_default_error_handler_(84, declaration_warning_handler/3, sepia_kernel),
   set_default_error_handler_(85, declaration_warning_handler/3, sepia_kernel),
   set_default_error_handler_(86, no_lookup_module_handler/4, sepia_kernel),
   set_default_error_handler_(87, warning_handler/3, sepia_kernel),
   set_default_error_handler_(88, warning_handler/3, sepia_kernel),
   set_default_error_handler_(89, warning_handler/3, sepia_kernel),
   set_default_error_handler_(90, error_handler/4, sepia_kernel),
   set_default_error_handler_(91, error_handler/2, sepia_kernel),
   set_default_error_handler_(92, error_handler/4, sepia_kernel),
   set_default_error_handler_(93, error_handler/4, sepia_kernel),
   set_default_error_handler_(94, error_handler/4, sepia_kernel),
   set_default_error_handler_(96, ambiguous_import_resolve/3, sepia_kernel),
   set_default_error_handler_(97, error_handler/2, sepia_kernel),
   set_default_error_handler_(98, error_handler/2, sepia_kernel),
   set_default_error_handler_(99, ambiguous_import_warn/3, sepia_kernel),
   set_default_error_handler_(100, undef_dynamic_handler/3, sepia_kernel),
   set_default_error_handler_(101, error_handler/2, sepia_kernel),
   set_default_error_handler_(111, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(112, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(113, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(114, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(115, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(116, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(117, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(118, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(119, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(121, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(122, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(123, error_handler/4, sepia_kernel),
   set_default_error_handler_(125, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(126, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(127, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(128, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(129, parser_error_handler/3, sepia_kernel),
   set_default_error_handler_(130, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(131, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(133, true/0, sepia_kernel),
   set_default_error_handler_(134, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(135, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(136, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(137, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(138, singleton_in_loop/2, sepia_kernel),
   set_default_error_handler_(139, true/0, sepia_kernel),
   set_default_error_handler_(140, error_handler/2, sepia_kernel),
   set_default_error_handler_(141, error_handler/2, sepia_kernel),
   set_default_error_handler_(142, error_handler/2, sepia_kernel),
   set_default_error_handler_(143, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(145, redef_other_file_handler/2, sepia_kernel),
   set_default_error_handler_(146, true/0, sepia_kernel),
   set_default_error_handler_(147, compiler_abort_handler/3, sepia_kernel),
   set_default_error_handler_(148, pragma_handler/3, sepia_kernel),
   set_default_error_handler_(149, unit_loaded_handler/3, sepia_kernel),
   set_default_error_handler_(150, true/0, sepia_kernel),
   set_default_error_handler_(151, true/0, sepia_kernel),
   set_default_error_handler_(152, true/0, sepia_kernel),
   set_default_error_handler_(157, error_exit/0, sepia_kernel),
   set_default_error_handler_(160, macro_handler/3, sepia_kernel),
   set_default_error_handler_(161, macro_handler/3, sepia_kernel),
   set_default_error_handler_(162, warning_handler/2, sepia_kernel),
   set_default_error_handler_(163, error_handler/2, sepia_kernel),
   set_default_error_handler_(165, error_handler/2, sepia_kernel),
   set_default_error_handler_(166, record_compiled_file_handler/3, sepia_kernel),
   set_default_error_handler_(167, warning_handler/3, sepia_kernel),
   set_default_error_handler_(170, system_error_handler/4, sepia_kernel),
   set_default_error_handler_(171, error_handler/2, sepia_kernel),
   set_default_error_handler_(172, error_handler/2, sepia_kernel),
   set_default_error_handler_(173, error_handler/2, sepia_kernel),
   set_default_error_handler_(174, error_handler/2, sepia_kernel),
   set_default_error_handler_(175, error_handler/2, sepia_kernel),
   set_default_error_handler_(176, error_handler/2, sepia_kernel),
   set_default_error_handler_(177, error_handler/2, sepia_kernel),
   set_default_error_handler_(178, error_handler/2, sepia_kernel),
   set_default_error_handler_(179, error_handler/2, sepia_kernel),
   set_default_error_handler_(180, error_handler/2, sepia_kernel),
   set_default_error_handler_(181, error_handler/2, sepia_kernel),
   set_default_error_handler_(182, error_handler/2, sepia_kernel),
   set_default_error_handler_(183, error_handler/2, sepia_kernel),
   set_default_error_handler_(184, error_handler/2, sepia_kernel),
   set_default_error_handler_(190, eof_handler/4, sepia_kernel),
   set_default_error_handler_(192, error_handler/2, sepia_kernel),
   set_default_error_handler_(193, error_handler/2, sepia_kernel),
   set_default_error_handler_(194, error_handler/2, sepia_kernel),
   set_default_error_handler_(195, io_yield_handler/2, sepia_kernel),
   set_default_error_handler_(196, close_handler/2, sepia_kernel),
   set_default_error_handler_(197, error_handler/2, sepia_kernel),
   set_default_error_handler_(198, past_eof_handler/2, sepia_kernel),
   set_default_error_handler_(210, error_handler/2, sepia_kernel),
   set_default_error_handler_(211, error_handler/2, sepia_kernel),
   set_default_error_handler_(212, error_handler/2, sepia_kernel),
   set_default_error_handler_(214, error_handler/2, sepia_kernel),
   set_default_error_handler_(230, error_handler/2, sepia_kernel),
   set_default_error_handler_(231, fail/0, sepia_kernel),
   set_default_error_handler_(250, true/0, sepia_kernel),
   set_default_error_handler_(251, true/0, sepia_kernel),
   set_default_error_handler_(252, true/0, sepia_kernel),
   set_default_error_handler_(253, true/0, sepia_kernel),
   set_default_error_handler_(254, true/0, sepia_kernel),
   set_default_error_handler_(255, true/0, sepia_kernel),
   set_default_error_handler_(256, true/0, sepia_kernel),
   set_default_error_handler_(257, true/0, sepia_kernel),
   set_default_error_handler_(258, true/0, sepia_kernel),
   set_default_error_handler_(259, true/0, sepia_kernel),
   set_default_error_handler_(260, error_handler/2, sepia_kernel),
   set_default_error_handler_(261, error_handler/2, sepia_kernel),
   set_default_error_handler_(262, error_handler/2, sepia_kernel),
   set_default_error_handler_(263, error_handler/2, sepia_kernel),
   set_default_error_handler_(264, compiled_file_handler/3, sepia_kernel),
   set_default_error_handler_(265, compiled_file_handler/3, sepia_kernel),
   set_default_error_handler_(267, error_handler/2, sepia_kernel),
   set_default_error_handler_(268, error_handler/2, sepia_kernel),
   set_default_error_handler_(270, error_handler/2, sepia_kernel),
   set_default_error_handler_(271, error_handler/2, sepia_kernel),
   set_default_error_handler_(272, warning_handler/2, sepia_kernel),
   set_default_error_handler_(280, cost_handler/2, sepia_kernel).

/* default error handler for MegaLog errors */

'$transaction_deadlock'(317,_) :- throw(abort_transaction).

?- set_default_error_handler_(300, error_handler/2, sepia_kernel),
   set_default_error_handler_(301, error_handler/2, sepia_kernel),
   set_default_error_handler_(302, error_handler/2, sepia_kernel),
   set_default_error_handler_(303, error_handler/2, sepia_kernel),
   set_default_error_handler_(304, error_handler/2, sepia_kernel),
   set_default_error_handler_(305, error_handler/2, sepia_kernel),
   set_default_error_handler_(306, error_handler/2, sepia_kernel),
   set_default_error_handler_(307, error_handler/2, sepia_kernel),
   set_default_error_handler_(308, error_handler/2, sepia_kernel),
   set_default_error_handler_(309, error_handler/2, sepia_kernel),
   set_default_error_handler_(310, error_handler/2, sepia_kernel),
   set_default_error_handler_(311, error_handler/2, sepia_kernel),
   set_default_error_handler_(312, error_handler/2, sepia_kernel),
   set_default_error_handler_(313, error_handler/2, sepia_kernel),
   set_default_error_handler_(314, error_handler/2, sepia_kernel),
   set_default_error_handler_(315, error_handler/2, sepia_kernel),
   set_default_error_handler_(316, error_handler/2, sepia_kernel),
   set_default_error_handler_(317, '$transaction_deadlock'/2, sepia_kernel),
   set_default_error_handler_(318, error_handler/2, sepia_kernel),
   set_default_error_handler_(319, error_handler/2, sepia_kernel),
   set_default_error_handler_(320, error_handler/2, sepia_kernel),
   set_default_error_handler_(321, error_handler/2, sepia_kernel),
   set_default_error_handler_(322, error_handler/2, sepia_kernel),
   set_default_error_handler_(329, warning_handler/2, sepia_kernel),
   set_default_error_handler_(333, warning_handler/2, sepia_kernel).

?- set_event_handler(postponed, trigger/1),
   set_event_handler(requested_fail_event, trigger/1),
   set_event_handler(garbage_collect_dictionary, garbage_collect_dictionary/0),
   set_event_handler(abort, throw/1).

reset_error_handlers :-
	current_error(N),
	reset_error_handler(N),
	fail.
reset_error_handlers.

?- reset_error_handlers.		% set up the handlers

%-------------------------------------
% interrupt handling builtins
%-------------------------------------

current_interrupt(N, Name) :-
	var(N), var(Name), !,
	gen_interrupt_id(N, Name, 1).
current_interrupt(N, Name) :-
	(integer(N);var(N)),
	(atom(Name);var(Name)),
	!,
	interrupt_id_det(N, Name),
	Name \== '.'.
current_interrupt(N, Name) :-
	error(5, current_interrupt(N, Name)).

    gen_interrupt_id(Number, Name, N) :-
        ( interrupt_id_det(N, Name) ->
	    Name \== '.',
            Number = N
        ;
            !,
	    fail
	).
    gen_interrupt_id(Number, Name, N) :-
        N1 is N + 1,
        gen_interrupt_id(Number, Name, N1).


%----------------------------------------------------------------------
% Raising events from socket streams
%----------------------------------------------------------------------

io_event_handler :-
	findall(Event, ready_sigio_stream_event(Event), Events),
	event(Events),
	events_nodefer.

    ready_sigio_stream_event(Event) :-
	current_stream(S),
	get_stream_info(S, sigio, on),		% it is a sigio stream
	get_stream_info(S, event, Event),	% it wants an event
	stream_select([S], 0, [_]).		% it has data


?- ( current_interrupt(_, io) -> 
	set_interrupt_handler(io, event/1),
	set_event_handler(io, defers(io_event_handler/0))
%	set_interrupt_handler(io, internal/0)	% if socket events not needed
   ;
	true
   ).

?- ( current_interrupt(_, poll) -> 
	set_interrupt_handler(poll, event/1),
	set_event_handler(poll, defers(io_event_handler/0))
%	set_interrupt_handler(poll, internal/0)	% if socket events not needed
   ;
	true
   ).


%----------------------------------------------------------------------
% An event handler that reads exdr terms (atoms or strings)
% from a stream (typically socket) and posts them as events.
% We expect this handler to be set up with the defers-option.
%----------------------------------------------------------------------

:- export post_events_from_stream/1.

post_events_from_stream(Stream) :-
	( current_stream(Stream),
	  stream_select([Stream], 0, [_]),
	  read_exdr(Stream, EventName)
	->
	    ( atom(EventName) ->
		event(EventName)
	    ; string(EventName) ->
		atom_string(EventNameAtom, EventName),
		event(EventNameAtom)
	    ;
		type_of(EventName, BadType),
		printf(warning_output, 
		    "WARNING: ignoring %w on event posting stream %w%n%b",
		    [BadType,Stream])
	    ),
	    post_events_from_stream(Stream)
	;
	    events_nodefer
	).


%----------------------------------------------------------------------
% after events
%
% Implemented via a dedicated asynchronous engine, running after_loop.
% It accepts requests from other engines via the after_requests queue,
% and posts events to the requesting engine at the appropriate time.
% Timing is ultimately done via the timeouts provided by the operating
% system's primitive for waiting on a condition variable.
%----------------------------------------------------------------------

% The after_requests queue for requesting services from the timer engine.
% See serve_request/3.
?- local(record(after_requests)).


%
% The main loop executed by the timer-engine (in its own thread).
% The central data structure is a queue TEQ of pending events,
% sorted by due time, with entries of the form:
%
%	NextDueTime - after(FirstDue, Interval, Event, Engine)

% This is now done in C, so that the engine is hidden:
%:- local initialization(after_init).
%after_init :-
%	engine_create(E, [async,detached]),
%	engine_resume_async(E, after_loop).

after_loop :-
	repeat,
	catch((after_loop([]) -> R=true ; R=fail), B, R=throw(B)),
	printf(warning_output, "Timer engine stopped (%w), restarting%n", [R]),
	fail.

    after_loop(TEQ) :-
	% Post events that are due now.
	% Build list of remaining events, plus every-events that need repeating.
	statistics(event_time, CurrentTime),
	(
	    foreach(TE,TEQ),
	    fromto(TEQ1,RE1,RE2,[]),
	    param(CurrentTime)
	do
	    DueTime-Desc = TE,
	    ( DueTime =< CurrentTime ->
		after(_Delay, Interval, Event, Engine) = Desc,
		engine_post(Engine, event(Event)),
		( Interval > 0 ->
		    NextDue is CurrentTime+Interval,
		    RE1 = [NextDue-Desc|RE2]
		;
		    RE1 = RE2
		)
	    ;
		RE1 = [TE|RE2]
	    )
	),
	% Order the new queue, compute SleepTime until soonest due time
	sort(1, =<, TEQ1, TEQ2),
	( TEQ2 = [NextDueTime-_|_] ->
	    SleepTime is NextDueTime-CurrentTime
	;
	    SleepTime = block	% block indefinitely
	),
	% Wait for new requests or for next event's due time
	( record_wait_remove(after_requests, Request, SleepTime) ->
	    % incorporate new requests into queue
	    %writeln(serve_request(Request, TEQ2, TEQ3)),
	    serve_request(Request, TEQ2, TEQ3),
	    after_loop(TEQ3)
	;
	    % timeout, i.e. next event is due
	    after_loop(TEQ2)
	).


    serve_request([], TEQ0, TEQ) :- !, TEQ=TEQ0.
    serve_request(Request, TEQ0, TEQ) :- Request = [_|_], !,
	% Incorporate a list of new after-events
	( foreach(Desc,Request), foreach(FirstDue-Desc,NewTEs) do
	    after(FirstDue,_,_,_) = Desc
	),
	sort(1, =<, NewTEs, SortedNewTEs),
	% make sure old events remain first in case of equal due time
	merge(1, =<, TEQ0, SortedNewTEs, TEQ).

    serve_request(Request, TEQ0, TEQ) :- Request = after(FirstDue,_,_,_), !,
	% Incorporate a single new after-event
	% make sure old events remain first in case of equal due time
	merge(1, =<, TEQ0, [FirstDue-Request], TEQ).

    serve_request(Request, TEQ0, TEQ) :- Request = cancel(Event,Engine,AnswerShelf), !,
	% Cancel all after events that match the event id
	statistics(event_time, Now),
	(
	    foreach(TE,TEQ0),
	    fromto(TEQ,TEQ1,TEQ2,[]),
	    fromto(Cancelled,CEs1,CEs2,[]),
	    param(Event,Engine,Now)
	do
	    ( _-after(FirstDue,Interval,Event,Engine) = TE ->
		( Interval > 0 ->
		    CEs1 = [Event-every(Interval)|CEs2]
		;
		    CEs1 = [Event-Delay|CEs2],
		    Delay is max(0,FirstDue-Now)
		),
		TEQ1 = TEQ2
	    ;
	    	CEs1 = CEs2,
	    	TEQ1 = [TE|TEQ2]
	    )
	),
	shelf_set(AnswerShelf, 1, Cancelled),
	condition_signal(AnswerShelf, all).

    serve_request(Request, TEQ0, TEQ) :- Request = current(Engine,AnswerShelf), !,
	TEQ = TEQ0,
	statistics(event_time, Now),
	(
	    foreach(TE,TEQ0),
	    fromto(Currents,Cs1,Cs2,[]),
	    param(Engine,Now)
	do
	    ( Due-after(_FirstDue,Interval,Event,Engine) = TE ->
		( Interval > 0 ->
		    Cs1 = [due(Event-every(Interval),Due)|Cs2]
		;
		    % We use Now instead of the PostTime as in pre-7.0
		    Cs1 = [due(Event-Now,Due)|Cs2]
		)
	    ;
		Cs2 = Cs1
	    )
	    
	),
	shelf_set(AnswerShelf, 1, Currents),
	condition_signal(AnswerShelf, all).

    serve_request(finalize(AnswerShelf), _TEQ0, _TEQ) :- !,
	engine_self(Engine),
	with_mutex(AnswerShelf, (
	    shelf_set(AnswerShelf, 1, Engine),
	    condition_signal(AnswerShelf, all)
	)),
	exit(0).	% exit the engine/thread

    serve_request(Request, TEQ, TEQ) :-
	printf(warning_output, "Timer thread ignoring request: %w", [Request]).


event_after(Event, Delay) :-
	event_after(Event, Delay, _DueTime).

event_after(Event, Delay, DueTime) :-
	check_event(Event),
	check_interval(single, Delay),
	!,
	DueTime is statistics(event_time)+Delay,
	engine_self(Engine),
	record_wait_append(after_requests, after(DueTime,0,Event,Engine), block, 20).
event_after(Event, Delay, DueTime) :-
	bip_error(event_after(Event, Delay, DueTime)).
	

event_after_every(Event, Interval) :-
	check_event(Event),
	check_interval(every, Interval),
	!,
	FirstDue is statistics(event_time)+Interval,
	engine_self(Engine),
	record_wait_append(after_requests, after(FirstDue,Interval,Event,Engine), block, 20).
event_after_every(Event, Interval) :-
	bip_error(event_after_every(Event, Interval)).


events_after(Specs) :-
	engine_self(Engine),
	statistics(event_time, Now),
	check_proper_list(Specs),
	( foreach(Spec,Specs), foreach(Request,Requests), param(Now,Engine) do
	    check_nonvar(Spec),
	    ( Spec = Event-Delay ->
		check_event(Event),
		check_nonvar(Delay),
		( Delay = every(Interval) ->
		    check_interval(every, Interval),
		    Due is Now+Interval,
		    Request = after(Due,Interval,Event,Engine)
		;
		    check_interval(single, Delay),
		    Due is Now+Delay,
		    Request = after(Due,0,Event,Engine)
		)
	    ;
		set_bip_error(5)
	    )
	),
	!,
	record_wait_append(after_requests, Requests, block, 20).
events_after(Specs) :-
	bip_error(events_after(Specs)).


cancel_after_event(Event, Cancelled) :-
	check_event(Event),
	!,
	engine_self(Engine),
	shelf_create(answer(none), Shelf),
	record_wait_append(after_requests, cancel(Event,Engine,Shelf), block, 20),
	with_mutex(Shelf, await_answer(Shelf, Cancelled)).
cancel_after_event(Event, Cancelled) :-
	bip_error(cancel_after_event(Event, Cancelled)).


current_after_events(Currents) :-
	engine_self(Engine),
	shelf_create(answer(none), Shelf),
	record_wait_append(after_requests, current(Engine,Shelf), block, 20),
	with_mutex(Shelf, await_answer(Shelf, Currents)).

    await_answer(Shelf, Answer) :-
	shelf_get(Shelf, 1, Term),
	( Term==none ->
	    condition_wait(Shelf, block),
	    await_answer(Shelf, Answer)
	;
	    Answer = Term
	).


timer_engine_cleanup :-
	shelf_create(answer(none), Shelf),
	record_wait_append(after_requests, finalize(Shelf), 10, 20),
	with_mutex(Shelf, await_answer(Shelf, TimerEngine)),
	( engine_join(TimerEngine, 3, exited(0)) -> true ;
	    writeln(warning_output, "Warning: Timer engine did not exit normally!")
	).


current_after_event(_) :- fail.	% obsolete


cancel_after_event(Event) :-	% obsolete
	cancel_after_event(Event, Cancelled),
	Cancelled = [_|_].


    % check_interval(+Type, ?Interval)
    :- mode check_interval(+,?).
    check_interval(every, Interval) :-		% after-every: > 0
	check_time_type(Interval),
	( Interval > 0 -> true ; set_bip_error(6) ).
    check_interval(single, Interval) :-		% simple after: >= 0
	check_time_type(Interval),
	( Interval >= 0 -> true ; set_bip_error(6) ).
	
    check_time_type(X) :- var(X), !, set_bip_error(4).
    check_time_type(X) :- integer(X), !.
    check_time_type(X) :- float(X), !.
    check_time_type(X) :- rational(X), !.
    check_time_type(_) :- set_bip_error(5).

    check_event(E) :- var(E), !, set_bip_error(4).
    check_event(E) :- is_event(E), !.
    check_event(_) :- set_bip_error(5).


%-------------------------------------

error_(N, G, LM) :-
	error_(N, G, LM, LM).    % the context module for normal errors is not significant


error_(default(N), G, CM, LM) :-
	integer(N),
	!,
	Nneg is -N,
	syserror(Nneg, G, CM, LM).
error_(N, G, CM, LM) :-
	syserror(N, G, CM, LM).


event(Var) :- var(Var), !,
	error(4, event(Var)).
event([]) :- !.
event(Events) :- Events = [_|_], !,
	post_events(Events).
event(N) :- atom(N), !,
	post_events([N]).
event(N) :- is_handle(N), is_event(N), !,
	post_events([N]).
event(Junk) :-
	error(5, event(Junk)).


bip_error_(Goal, LM) :-		% for internal use
	get_bip_error(E),
	syserror(E, Goal, LM, LM).

bip_error_(Goal, CM, LM) :-	% for internal use
	get_bip_error(E),
	syserror(E, Goal, CM, LM).


:- unskipped			% handlers that re-call the culprit
	event/1,
	compare_handler/4.

:- untraceable
	error_exit/0,
	compare_handler/4,
	call_handler/4.

:- skipped
	call_handler/4,
	eof_handler/4,
	error_exit/0,
	error_handler/2,
	error_handler/3,
	error_handler/4,
	parser_error_handler/3,
	system_error_handler/4.
