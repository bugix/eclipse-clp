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
% The Original Code is	The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is	 Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: kernel.pl,v 1.71 2017/09/07 00:14:07 jschimpf Exp $
% ----------------------------------------------------------------------

%
% IDENTIFICATION:	kernel.pl
%
% DESCRIPTION:	Bootstrapping file for SEPIA/ECLiPSe.
%		It is the first Prolog file that an ECLiPSe ever sees.
%
% CONTENTS:	This file and the files it includes contain all the
%		Prolog definitions that go into sepia_kernel.
%		Note that the sepia_kernel module already exists: it
%		is created in C and already contains external predicates.
%
%		In this file, the difference between :- (directive) and
%		?- (query) matters: if something only makes sense at load-time,
%		use a query.

:-(begin_module(sepia_kernel)).

%
% global operator declarations
%

:-(op_(global, 1000, xfy, (',') , sepia_kernel)).
:-(op_(global, 1200,  fx, :-	, sepia_kernel)).
:- op_(global, 1200, xfx, ?-	, sepia_kernel),
   op_(global, 1200,  fx, ?-	, sepia_kernel),
   op_(global, 1200, xfx, :-	, sepia_kernel),
   op_(global, 1200, xfx, -->	, sepia_kernel),
   op_(global, 1200, xfx, if	, sepia_kernel),
   op_(global, 1190,  fy, help	, sepia_kernel),
%   op_(global, 1180, xfx, -?-> , sepia_kernel),
   op_(global, 1180,  fx, -?->	, sepia_kernel),
   op_(global, 1190,  fx, delay , sepia_kernel),
   op_(global, 1170, xfy, else	, sepia_kernel),
   op_(global, 1160,  fx, if	, sepia_kernel),
   op_(global, 1150, xfx, then	, sepia_kernel),
   op_(global, 1100, xfy, do	, sepia_kernel),
   op_(global, 1100, xfy, ;	, sepia_kernel),
   op_(global, 1100, xfy, '|'	, sepia_kernel),
   op_(global, 1050, xfy, ->	, sepia_kernel),
   op_(global, 1050, xfx, *->	, sepia_kernel),
   op_(global, 1050,  fy, import, sepia_kernel),
   op_(global, 1050,  fy, reexport, sepia_kernel),
   op_(global, 1050, xfx, from	, sepia_kernel),
   op_(global, 1050, xfx, except, sepia_kernel),
   op_(global, 1000,  fy, dynamic, sepia_kernel),
   op_(global, 1000,  fy, abolish, sepia_kernel),
   op_(global, 1000,  fy, mode	, sepia_kernel),
   op_(global, 1000,  fy, local , sepia_kernel),
   op_(global, 1000,  fy, global, sepia_kernel),
   op_(global, 1000,  fy, export, sepia_kernel),
   op_(global, 1000,  fy, parallel, sepia_kernel),
   op_(global, 1000,  fy, demon , sepia_kernel),
   op_(global,	900,  fy, ~	, sepia_kernel),
   op_(global, 1000,  fy, listing, sepia_kernel),
   op_(global,	900,  fy, once	, sepia_kernel),
   op_(global,	900,  fy, not	, sepia_kernel),
   op_(global,	900,  fy, \+	, sepia_kernel),
   op_(global, 1000,  fy, spy	, sepia_kernel),
   op_(global, 1000,  fy, nospy , sepia_kernel),
   op_(global, 1000,  fy, traceable, sepia_kernel),
   op_(global, 1000,  fy, untraceable, sepia_kernel),
   op_(global, 1000,  fy, skipped, sepia_kernel),
   op_(global, 1000,  fy, unskipped, sepia_kernel),
   op_(global,	700, xfx, ::	, sepia_kernel),
   op_(global,	700, xfx, #=	, sepia_kernel),
   op_(global,	700, xfx, #\=	, sepia_kernel),
   op_(global,	700, xfx, #>	, sepia_kernel),
   op_(global,	700, xfx, #<	, sepia_kernel),
   op_(global,	700, xfx, #>=	, sepia_kernel),
   op_(global,	700, xfx, #=<	, sepia_kernel),
   op_(global,	700, xfx, #<=	, sepia_kernel),
   op_(global,	700, xfx, =..	, sepia_kernel),
   op_(global,	700, xfx, =	, sepia_kernel),
   op_(global,	700, xfx, ~=	, sepia_kernel),
   op_(global,	700, xfx, \=	, sepia_kernel),
   op_(global,	700, xfx, ==	, sepia_kernel),
   op_(global,	700, xfx, \==	, sepia_kernel),
   op_(global,	700, xfx, @<	, sepia_kernel),
   op_(global,	700, xfx, @=<	, sepia_kernel),
   op_(global,	700, xfx, @>	, sepia_kernel),
   op_(global,	700, xfx, @>=	, sepia_kernel),
   op_(global,	700, xfx, is	, sepia_kernel),
   op_(global,	700, xfx, =:=	, sepia_kernel),
   op_(global,	700, xfx, =\=	, sepia_kernel),
   op_(global,	700, xfx, <	, sepia_kernel),
   op_(global,	700, xfx, =<	, sepia_kernel),
   op_(global,	700, xfx, >	, sepia_kernel),
   op_(global,	700, xfx, >=	, sepia_kernel),
   op_(global,	650, xfx, with	, sepia_kernel),
   op_(global,	650, xfx, of	, sepia_kernel),
   op_(global,	650, xfx, @	, sepia_kernel),
   op_(global,	600, xfy, :	, sepia_kernel),
   op_(global,	600, xfx, ..	, sepia_kernel),
   op_(global,	500, yfx, +	, sepia_kernel),
   op_(global,	500, yfx, -	, sepia_kernel),
   op_(global,	500, yfx, /\	, sepia_kernel),
   op_(global,	500, yfx, \/	, sepia_kernel),
   op_(global,	400, yfx, /	, sepia_kernel),
   op_(global,	400, yfx, *	, sepia_kernel),
   op_(global,	400, yfx, //	, sepia_kernel),
   op_(global,	400, yfx, >>	, sepia_kernel),
   op_(global,	400, yfx, <<	, sepia_kernel),
   op_(global,	400, yfx, rem	, sepia_kernel),
   op_(global,	400, yfx, div	, sepia_kernel),
   op_(global,	400, yfx, mod	, sepia_kernel),
%  op_(global,	300,  fx, *	, sepia_kernel),
   op_(global,	200, xfy, ^	, sepia_kernel),
   op_(global,	200,  fy, +	, sepia_kernel),
   op_(global,	200,  fy, -	, sepia_kernel),
   op_(global,	200,  fy, \	, sepia_kernel).


% Everything is this module is marked as 'built_in'
:- pragma(system).
:- pragma(nodebug).
:- pragma(noexpand).

% Set debug mode for the following tool declarations:
:- global_flags(16'00000080,0,_).		% debug_compile (DBGCOMP) off

:- tool_(tool/2, tool_/3, sepia_kernel).	% tool declarations
:- tool(store_pred/8, store_pred/9).		% needed when loading kernel.eco
:- tool((not)/1, fail_if_body/2),
   tool(setval/2, setval_body/3),
   tool(getval/2, getval_body/3),
   tool(setref/2, setref_/3),
   tool(getref/2, getref_/3),
   tool(swapref/3, swapref_/4),
   tool(use_module/1, use_module_body/2),
   tool((<)/2, (<)/3),
   tool((>)/2, (>)/3),
   tool((=<)/2, (=<)/3),
   tool((>=)/2, (>=)/3),
   tool((=:=)/2, (=:=)/3),
   tool((=\=)/2, (=\=)/3),
   tool(is/2, is_body/3),
   tool((^)/2, exquant_body/3),
   tool(bagof/3, bagof_body/4),
   tool(block/3, block/4),
   tool(block_atomic/3, block_atomic/4),
   tool(catch/3, catch_/4),
   tool(coverof/3, coverof_body/4),
   tool(untraced_block/3, block/4),
   tool(printf_with_current_modes/2, printf_with_current_modes_body/3),
   tool(printf_goal/2, printf_goal_body/3),
   tool(printf_goal/3, printf_goal_body/4),
   tool(readvar/3, readvar/4),
   tool(get_chtab/2, get_chtab_/3),
   tool(set_chtab/2, set_chtab_/3),
   tool(set_error_handler/2, set_error_handler_/3),
   tool(set_event_handler/2, set_error_handler_/3),
   tool(event_create/2, event_create_/3),
   tool(event_create/3, event_create_/4),
   tool(set_interrupt_handler/2, set_interrupt_handler_body/3),
   tool(get_flag/3, get_flag_body/4),
   tool(get_syntax/2, get_syntax_/3),
   tool((@)/2, (@)/3),
   tool((\+)/1, fail_if_body/2),
   tool(call/1, call_/2),
   tool(call/2, call2_/3),
   tool(call_local/1, call_local/2),
   tool(current_record/1, current_record_body/2),
   tool(define_macro/3, define_macro_/4),
   tool(set_syntax/2, set_syntax_/3),
   tool(engine_post/2, engine_post_/3),
   tool(engine_resume/3, engine_resume_/4),
   tool(engine_resume_thread/2, engine_resume_thread_/3),
   tool(ensure_loaded/1, ensure_loaded/2),
   tool(erase/2, erase_body/3),
   tool(erase_all/1, erase_all_body/2),
   tool(erase_all/2, erase_all_body/3),
   tool(erase_module/1, erase_module/2),
   tool(error/2, error_/3),
   tool(error/3, error_/4),
   tool(bip_error/1, bip_error_/2),
   tool(bip_error/2, bip_error_/3),
   tool(findall/3, findall_body/4),
   tool(get_flag/2, get_flag_body/3),
   tool(lock/0, lock/1),
   tool(lock_pass/1, lock_pass_/2),
   tool(local_record/1, local_record_body/2),
   tool(mutex_init/1, mutex_init_body/2),
   tool(mutex/2, mutex_body/3),
   tool(with_mutex/2, with_mutex_/3),
   tool(name_to_handle/3, name_to_handle_/4),
   tool(nested_compile_term/1, nested_compile_term_/2),
   tool(nested_compile_term_annotated/2, nested_compile_term_annotated_/3),
   tool(number_string/2, number_string_/3),
   tool(par_all/2, par_all_body/3),
   tool(par_findall/4, par_findall_body/5),
   tool(par_once/2, par_once_body/3),
   tool(printf/2, printf_body/3),
   tool(printf/3, printf_body/4),
   tool(sprintf/3, sprintf_/4),
   tool(is_predicate/1, is_predicate_/2),
   tool(is_record/1, is_record_body/2),
   tool(incval/1, incval_body/2),
   tool(decval/1, decval_body/2),
   tool((tool)/1, tool_/2),
   tool(read/1, read_/2),
   tool(read/2, read_/3),
   tool(read_token/2, read_token_/3),
   tool(record/2, recordz_body/3),
   tool(recorda/2, recorda_body/3),
   tool(recorda/3, recorda_body/4),	
   tool(recorded/2, recorded_body/3),
   tool(recorded/3, recorded_body/4),
   tool(recordedchk/2, recordedchk_body/3),
   tool(recordedchk/3, recordedchk_body/4),
   tool(recorded_count/2, recorded_count_/3),
   tool(recorded_list/2, recorded_list_body/3),
   tool(recorded_refs/3, recorded_refs_body/4),
   tool(recordz/2, recordz_body/3),
   tool(recordz/3, recordz_body/4),
   tool(rerecord/2, rerecord_body/3),
   tool(set_default_error_handler/2, set_default_error_handler_/3),
   tool(set_flag/3, set_flag_body/4),
   tool(setof/3, setof_body/4),

   tool(shelf_dec/2, shelf_dec_/3),
   tool(shelf_get/3, shelf_get_/4),
   tool(shelf_inc/2, shelf_inc_/3),
   tool(shelf_set/3, shelf_set_/4),
   tool(shelf_size/2, shelf_size_/3),
   tool(shelf_inc_and_get/3, shelf_inc_and_get_/4),
   tool(shelf_get_and_dec/3, shelf_get_and_dec_/4),
   tool(shelf_test_and_set/4, shelf_test_and_set_/5),

   tool(store_contains/2, store_contains_/3),
   tool(store_count/2, store_count_/3),
   tool(store_create_named/1, store_create_named_/2),
   tool(store_delete/2, store_delete_/3),
   tool(store_erase/1, store_erase_/2),
   tool(store_get/3, store_get_/4),
   tool(store_inc/2, store_inc_/3),
   tool(store_info/1, store_info_/2),
   tool(store_insert/3, store_insert_/4),
   tool(stored_keys/2, stored_keys_/3),
   tool(stored_keys_and_values/2, stored_keys_and_values_/3),
   tool(store_remove/3, store_remove_/4),
   tool(store_set/3, store_set_/4),
   tool(store_test_and_set/4, store_test_and_set_/5),
   tool(store_update/4, store_update_/5),

   tool(bytes_to_term/2, bytes_to_term_/3),
   tool(term_to_bytes/2, term_to_bytes_/3),
   tool(term_string/2, term_string_body/3),
   tool(test_and_setval/3, test_and_setval_body/4),
   tool(write/1, write_/2),
   tool(write/2, write_/3),
   tool(writeclause/1, writeclause_body/2),
   tool(writeclause/2, writeclause_body/3),
   tool(writeln/1, writeln_body/2),
   tool(writeln/2, writeln_body/3),
   tool(writeq/1, writeq_/2),
   tool(writeq/2, writeq_/3),
   tool(write_canonical/1, write_canonical_/2),
   tool(write_canonical/2, write_canonical_/3),
   tool((mode)/1, mode_/2).

:- global_flags(0,16'00000880,_).	% debug_compile (GOALEXPAND|DBGCOMP) on
:- tool(trace/1, trace_body/2).		% must be traceable
:- tool(debug/1, debug_body/2).		% must be traceable
:- set_proc_flags(trace/1, spy, off, sepia_kernel). % spy was inherited...


%------------------------------
% basic system initialisation
%------------------------------

?-	get_sys_flag(13, EclipseDir),	% initialized in C
	concat_strings(EclipseDir, "/lib", Lib),
	make_array_(library, prolog, local, sepia_kernel),
	setval(library, Lib),
	make_array_(library_path, prolog, local, sepia_kernel),
	setval(library_path, [Lib]).

?-	make_array_(break_level, prolog, local, sepia_kernel),
	setval(break_level, 0),
	make_array_(prolog_suffix, prolog, local, sepia_kernel),
	setval(prolog_suffix, ["", ".ecl", ".pl"]),
	make_array_(eclipse_object_suffix, prolog, local, sepia_kernel),
	setval(eclipse_object_suffix, ".eco"),
	make_array_(eclipse_info_suffix, prolog, local, sepia_kernel),
	setval(eclipse_info_suffix, ".eci"),
	make_array_(version_cache, prolog, local, sepia_kernel).


?- local_record(compiled_modules/0).

:- store_create_named_(name_ctr, sepia_kernel).

% Default language determined by: option, envvar, command line
?- make_array_(default_language, prolog, local, sepia_kernel),
    get_sys_flag(12, LanguageOption),
    ( LanguageOption \== '' ->
	Language = LanguageOption
    ; getenv("ECLIPSEDEFAULTLANGUAGE", LangString) ->
	atom_string(Language, LangString)
    ;
	Language = eclipse_language
    ),
    setval(default_language, Language).

?- make_array_(toplevel_module, prolog, local, sepia_kernel),
    setval(toplevel_module, eclipse).	% initialized in standalone_toplevel/0
?- make_array_(toplevel_trace_mode, prolog, local, sepia_kernel),
    setval(toplevel_trace_mode, nodebug).
?- make_array_(compiled_stream, prolog, local, sepia_kernel),
    setval(compiled_stream, _).
?- makeref_(compile_stack, [], sepia_kernel).

% ignore_eof is 'on' for Windows, because ^C acts like eof (in Command Prompt)
?- make_array_(ignore_eof, prolog, local, sepia_kernel),
   get_sys_flag(8, Arch),	% hostarch
   ( (Arch == "i386_nt" ; Arch == "x86_64_nt") -> setval(ignore_eof, on) ; setval(ignore_eof, off)).
 
% Hack for Java/Linux: if eclipse was loaded by a Java host program, then its
% symbols may not be visible (loaded without RTLD_GLOBAL). In this case,
% try to re-load the eclipse shared library (now with the right options).
?-  ( get_sys_flag(9, "so") ->		% object_suffix
	( symbol_address("ec_",_) ->	% look for any symbol from C kernel
	    true
        ;
	    get_sys_flag(13, EclipseDir),
	    get_sys_flag(8, Arch),	% hostarch
	    concat_string([EclipseDir,"/lib/",Arch,"/libeclipse.so"], EclLib),
	    ( sys_file_flag(EclLib, 17 /*readable*/, on) ->
		load(EclLib)
	    ;
		true
	    )
	)
    ;
	true
    ).


%------------------------------------
% Definitions for ,/2 ;/2 ->/2.
% The definitions here are only used for waking such goals.
% Occurrences in compiled code are expanded by the compiler,
% and metacalls are handled by the emulator.
%------------------------------------

:- tool((',')/2, ',_body'/3),
   tool((;)/2, ';_body'/3),
   tool((*->)/2, ',_body'/3),
   tool((->)/2, '->_body'/3).

',_body'(A, B, M) :- get_cut(Cut), ','(A, B, M, Cut).
';_body'(A->B, C, M) :- -?-> !, get_cut(Cut), ';'(A, B, M, Cut, C).
';_body'(A, B, M) :- get_cut(Cut), ';'(A, B, M, Cut).
'->_body'(A, B, M) :- get_cut(Cut), '->'(A, B, M, Cut).


%----------------------------------------------------------------------
% main/1 is invoked whenever the system is started or restarted.
% This is the code that accepts posted goals, executes them,
% and yields with the proper return codes.
%----------------------------------------------------------------------

main(Boot) :-
	default_module(M),
	( Boot == 1 ->
	    % licence_check,		% NOT ENABLED
	    startup_init,
	    restart_init(M)
	;
	    restart_init(M),
	    error(151, _)		% extension hook: restart
	),
	embed_catch(1, []).		% 1 == PFAIL

    embed_catch(YieldType, ToC) :-
	catch(embed_repeat(YieldType,ToC),
	    ExitCode,
	    embed_catch(2,ExitCode)).	% 2 == PTHROW

    embed_repeat(YieldType, ToC) :-
	embed_loop(YieldType, ToC).
    embed_repeat(_, _) :-
	embed_repeat(1, []).		% 1 == PFAIL

    embed_loop(YieldType, ToC) :-
	get_cut(Cut),
	yield(YieldType, ToC, Goals, Module),	% YieldType in 0,1,2
	% Posted events are handled HERE!
	( Module == [] ->
	    embed_loop(YieldType, ToC) % event handling only, re-yield
	;
	    call(Goals)@Module,		% call the new goals
	    embed_loop(0, Cut)		% 0 == PSUCCEED
	).


yield(ToC, FromC) :-
	yield(4, ToC, FromC).		% 4 == PYIELD == EC_yield

yield(YieldType, ToC, FromC) :-
	yield(YieldType, ToC, FromC1, Module),
	% Posted events handled HERE!
	( Module == [] ->
%	    yield(YieldType, ToC, FromC) % event handling only, re-yield
	    yield(0, [], FromC) % event handling only, re-yield
	;
	    FromC = FromC1		% let yield/2,3 succeed
	).


startup_init :-
	% explicitly run kernel's initialization goals
	run_stored_goals(initialization_goals, sepia_kernel),
	% explicitly trigger first dictionary gc (subsequent ones use interval)
	garbage_collect_dictionary.

restart_init(M) :-
	% create module if necessary
	( is_a_module(M) ->
	    true
	;
	    getval(default_language, Language),
	    create_module(M, [], Language)
	).


%----------------------------------------------------------------------
% RPC server
%----------------------------------------------------------------------

% must be run on the server engine
rpc_server_init :-
	open(queue(""),read,ec_rpc_in,[event(ec_rpc)]),
	% open(queue(""),read,ec_rpc_in), set_stream_prop_(ec_rpc_in, 17, ec_rpc),
	open(queue(""),update,ec_rpc_out),
	set_event_handler(ec_rpc, ec_rpc_in_handler/1).


ec_rpc_in_handler(Base) :-
	concat_atom([Base, '_in'], In),
	concat_atom([Base, '_out'], Out),
	ec_rpc_in_handler1(In, Out).

ec_rpc_in_handler1(In, Out) :-
	( at_eof(In) ->
	    flush(Out)
	;
	    empty_stream(Out),
	    catch((read_exdr_last(In, Goal),execute_rpc(Out, Goal, true)),
		    _, (write_exdr(Out, throw),flush(Out))),
	    ec_rpc_in_handler1(In, Out)
	).

    empty_stream(Stream) :-
	( at_eof(Stream) -> true ; get(Stream,_), empty_stream(Stream) ).

    read_exdr_last(Stream, Goal) :-
	read_exdr(Stream, Goal0),
	( at_eof(Stream) -> Goal=Goal0 ; read_exdr_last(Stream, Goal) ).

    execute_rpc(Out, GoalString, Extra) :-
	string(GoalString), !, 
	default_module(M),
	term_string(Goal, GoalString)@M,
	execute_rpc(Out, Goal, Extra).
    execute_rpc(Out, Goal, Extra) :-
	default_module(M),
	( call(Goal)@M ->
	    call(Extra),
	    % write_exdr might fail if Goal is not valid EXDR!
	    (write_exdr(Out, Goal) -> true;true), flush(Out)
	;
	    call(Extra), 
	    write_exdr(Out, fail), flush(Out)	% PFAIL
	),
	fail.
    execute_rpc(_, _, _).


%---------------------------------------------------------
% Parallel execution
%---------------------------------------------------------

% When recomputation goes wrong, we loop (and the worker is lost).
% This is still better than aborting the whole session. A more clever
% recovery strategy would require special support from the scheduler.
hang :- hang.

slave :-
	get_par_goal(pargoal(InitGoal, ParGoal)),
	(catch(InitGoal, _, fail)@eclipse -> true ; true),
	catch(
	    (install_pending_oracle, worker_boundary, ParGoal),
	    _,
	    (install_oracle(0),hang)
	),
	fail.

all_sol(Goal, Module) :-
	call(Goal)@Module,
	fail.

par_all_body(InitGoal, Goal, Module) :-
	set_par_goal(pargoal(InitGoal, all_sol(Goal, Module))),
	(
	    worker_boundary,		% recomputing starts here
	    all_sol(Goal, Module)	% fails
	;
	    true
	).


gather_instances(Template, Generator, Module, Ref) :-
	call(Generator)@Module,
	true,				% force waking before recording
	dbag_enter(Ref, Template),
	fail.
 
par_findall_body(InitGoal, Template, Generator, List, Module) :-
	% check_nesting
	dbag_create(Ref),		% on worker 1
	set_par_goal(pargoal(InitGoal,
			gather_instances(Template, Generator, Module, Ref))),
	(
	    worker_boundary,		% recomputing starts here
	    gather_instances(Template, Generator, Module, Ref)	% fails
	;
	    dbag_dissolve(Ref, List)	% on worker 1
	).


find_solution(Goal, Module, Ref) :-
	call(Goal)@Module,
	true,				% force waking before recording
	!,
	dbag_enter(Ref, Goal),
	fail.
 
par_once_body(InitGoal, Goal, Module) :-
	% check_nesting
	dbag_create(Ref),		% on worker 1
	set_par_goal(pargoal(InitGoal, find_solution(Goal, Module, Ref))),
	(
	    worker_boundary,		% recomputing starts here
	    find_solution(Goal, Module, Ref)	% fails
	;
	    dbag_dissolve(Ref, [Goal])	% on worker 1
	).

engine_join(Engine, Status) :-
	engine_join(Engine, block, Status).


%---------------------------------------------------------
% defaults handlers for start/restart/end events
%---------------------------------------------------------

extension(X):-
	extension(X,0).

configuration(C) :-
	open("", string, S),
	write(S, kernel),
	(
	    extension(E),
	    E \== dfid, E \== occur_check,
	    put(S, 0' ),
	    write(S, E),
	    fail
	;
	    stream_info_(S, 0, C),	% name
	    close(S)
	).

sepia_version(List, Stage, Date) :-
	getval(version_cache, Cached),
	( var(Cached) ->
	    get_sys_flag(11, MajorMinorVersionAtom),
	    getval(library,Lib),
	    concat_string([Lib, "/version.pl"], VersionFile),
	    open(VersionFile, read, S),
	    read(S, sepia_date(Date0)),
	    read(S, sepia_stage(Stage)),
	    read(S, sepia_build(Build)),
	    close(S),
	    concat_string([MajorMinorVersionAtom,".",Build], VersionString),
	    split_string(VersionString, ".", " ", List0),
	    strings_to_numbers(List0, List1),
	    Cached = version(List1,Stage,Date0),
	    setval(version_cache, Cached)
	;
	    true
	),
	version(List,Stage,Date) = Cached.

    strings_to_numbers([], []).
    strings_to_numbers([S|Ss], [N|Ns]) :-
	number_string(N, S),
	strings_to_numbers(Ss, Ns).

sepia_version_banner(Text, Date) :-
	get_sys_flag(11, Version),
	get_sys_flag(8, Arch),
	sepia_version(List, Stage, Date),
	append(_, [Build], List), !,
	configuration(Conf),
	( extension(development) ->
	    get_sys_flag(3, Pid),
	    concat_string([", PID=", Pid], PidInfo)
	;
	    PidInfo = ""
	),
	( bignum(0,_) ->
	    GmpCopyright = "\nGMP library copyright Free Software Foundation, see legal/lgpl.txt"
	;
	    GmpCopyright = ""
	),
	concat_string([
	    "ECLiPSe Constraint Logic Programming System [", Conf, "]"
	    "\nKernel and basic libraries copyright Cisco Systems, Inc."
	    "\nand subject to the Cisco-style Mozilla Public Licence 1.1"
	    "\n(see legal/cmpl.txt or http://eclipseclp.org/licence)"
	    "\nSource available at www.sourceforge.org/projects/eclipse-clp",
	    GmpCopyright,
	    "\nFor other libraries see their individual copyright notices"
	    "\nVersion ", Version, Stage, " #", Build, " (", Arch, "), ",
	    Date, PidInfo, "\n"
	], Text).


%------------------------------
% Licensing
%------------------------------

licence_check :-
	LicStream = error,

	% Check whether we have a licence file
	get_sys_flag(13, EclipseDir),
	concat_string([EclipseDir,"/lib/licence.ecl"], LicFile0),
	( existing_file(LicFile0, [""], [readable], LicFile) ->

	    % Open licence file and backtrack over all licence entries in it
	    open(LicFile, read, S),
	    repeat,
	    catch(read(S, SignedLicenceTerm), _, SignedLicenceTerm=junk),

	    ( SignedLicenceTerm \== end_of_file ->

		% Check signature
		( valid_signature(SignedLicenceTerm, LicenceTerm),
		  memberchk(licensee:Licensee, LicenceTerm) ->
		    true
		;
		    writeln(LicStream, "Invalid licence file entry"),
		    fail	% warn but continue
		),

		% Check host restriction, if any
		( memberchk(host:Host, LicenceTerm) ->
		    get_sys_flag(1, Host)	% check host
		;
		    true	% no host restriction
		),
		!,		% commit to this entry

		% Check expiry date, if any
		( memberchk(expiry:Expiry, LicenceTerm) ->
		    local_time_string(Expiry, "%c", ExpiryDate),
		    ( get_sys_flag(5) > Expiry ->
			printf(LicStream, "ECLiPSe: Licence expired %s, exiting%n", ExpiryDate),
			fail	% expired
		    ;
			true	% not expired
		    )
		;
		    ExpiryDate = "never"	% no expiry date
		),

		% Check if the licence applies to this version
		( memberchk(version:MaxVersion, LicenceTerm) ->
		    sepia_version([Major,Minor|_], _, _),
		    ( [Major,Minor] @=< MaxVersion ->
			printf(LicStream, "ECLiPSe: Licence only valid up to version %w, exiting%n", MaxVersion),
			fail	% invalid
		    ;
			true	% valid
		    )
		;
		    true	% no version limit
		),

		printf(LicStream, "ECLiPSe licensed to: %s (expires %s)%n", [Licensee,ExpiryDate])

	    ;

		% No valid licence found, cut the repeat, close and fail
		!,
		close(S),
		writeln(LicStream, "ECLiPSe: No Licence found, exiting"),
		fail
	    )

	;
	    writeln(LicStream, ">>> ECLiPSe Academic Version - strictly not for commercial use! <<<"),
	    true
	).


% This is a naive implementation of the RSA algorithm
% sign:		Signature is powm(Digest,D,N)	with private_key(D,N)
% validate:	Digest =:= powm(Signature,E,N)	with public_key(E,N)
% For the corresponding sign/2 and private key see lib(licensing)

valid_signature(signed(Term, SignatureString), Term) :-
	string(SignatureString),
	number_string(Signature, SignatureString),
	hash_secure(Term, Digest, sha),
	public_key(E, N),	% could succeed with alternative keys
	Digest =:= powm(Signature,E,N),
	!.

public_key(65737, N) :-
	% convert the bignum at runtime, so we don't require gmp for compiling
	number_string(N, "21914161071951772490417739500054678264714316157992140467021105282300879910358542740162430501913497561468260342080059381256137594184082254908360199026967589435446562798562242943975279574163853396385755498066856539655902646718824668922469051215343559030281711267234935602376733839726736220820352137086182611433").


%------------------------------
% Halting the system - this can happen in two ways:
%
% If exit/1 or halt/0 is called from Prolog:
%	- run Prolog level finalization directly (to avoid nested emulator)
%	- call low-level cleanup via exit0/1 builtin
%
% If ec_cleanup() is called from a host program:
%	- run Prolog level finalization cleanup_before_exit/0 via new emulator
%	- call low-level cleanup directly from host program
%------------------------------

% exit all engines
halt :-
	ExitCode = 0,			% unfortunate, but quasi-standard
	broadcast_exit(ExitCode),	% all excluding self
	exit(ExitCode).

% exit engine
exit(N) :-
	check_integer_ge(N, 0), !,
	% like yield(3,N,_) but also untrails the engine, 3 == PEXITED
	sys_return(N).
exit(N) :-
	bip_error(exit(N)).

% This one is called from ec_cleanup() in C.
% It is executed on the aux-engine.
% All Prolog-level cleanup goes here!
cleanup_before_exit :-
	N=0,
	%writeln(error, cleanup_before_exit(N)),

	% Call user handler first, so it can abort the exit if desired
	% !!! abort no longer supported (with multi-engines)
	( catch(error(152, N),_,fail) -> true ; true ),

	% disable dictionary gc, so it doesn't interfere with shutdown
	set_flag(gc_interval_dict, 0),

	% engine_self(Self), writeln(cleanup_by:Self),

	% terminate engines
	timer_engine_cleanup,
	broadcast_exit(N),
	(
	    % Wait until all (non-hidden) engines have disappeared or exited.
	    between(1,10,_),
	    (
		leftover_engines([])
	    ;
		sleep(0.1),
		fail
	    )
	->
	    true
	;
	    leftover_engines(LEs),
	    ( LEs == [] ->
		true
	    ;
		writeln(warning_output, "Cleanup ended with non-exited engines:"),
		( foreach(ES,LEs) do
		    writeln(warning_output, ES)
		),
		flush(warning_output),
		fail	% don'erase modules, might cause crash
	    )
	),
	erase_modules.

    leftover_engines(LEs) :-
	current_engines(Es),
	( foreach(E,Es), fromto(LEs,LEs2,LEs1,[]) do
	    get_engine_property(E, status, Status),
	    ( Status = exited(_) ->
		LEs2 = LEs1
	    ;
		LEs2 = [E:Status|LEs1]
	    )
	).


%----------------------------------------
% Goal executed by the standalone system
%----------------------------------------

standalone_toplevel :-
	default_module(M0),
	setval(toplevel_module, M0),	% initialize toplevel module from 
	argv(all, [_|Args]),
	process_command_line(Args, 1, Goal, M0, M),
	( var(Goal) ->
	    ensure_loaded(library(toplevel)),
	    call(toplevel:toplevel_init(tty)),
	    call(toplevel:toplevel)

	% In the following, Goal is negated to make sure we always fail and
	% untrail everything before exiting. Do not simplify this code!
	; catch(\+call(Goal)@M, T, top_throw(T)) ->
	    fail
	;
	    true
	).

    top_throw(Tag) :-
	( stack_overflow_message(Tag) ->
	    true
	;
	    writeln(error, Tag)
	),
	throw(Tag).
	
:- mode process_command_line(+,+,-,+,-).
process_command_line([], _I, _Goal, M, M) :- !.
process_command_line(["-f"|Args], I, Goal, M, M1) :- !,
	process_command_line(["-b"|Args], I, Goal, M, M1).
process_command_line(["-b", Arg |Args], I, Goal, M, M1) :- !,
	os_file_name(File, Arg),
	catch(ensure_loaded(File, M), Tag, top_throw(Tag)),
	MI is -I, argv(MI,2),	% delete the 2 arguments
	process_command_line(Args, I, Goal, M, M1).
process_command_line(["-e", Arg |Args], I, Goal, M, M1) :- !,
	open(Arg, string, Stream),
	read(Stream, ArgTerm),
	close(Stream),
	( var(Goal) -> Goal=ArgTerm ; true ),
	MI is -I, argv(MI,2),	% delete the 2 arguments
	process_command_line(Args, I, Goal, M, M1).
process_command_line(["-L",Arg|Args], I, Goal, M, M1) :- !,
        atom_string(Language, Arg),
        setval(default_language, Language),
	update_toplevel_module(M),	% with new language
	MI is -I, argv(MI,2),	% delete the 2 arguments
	process_command_line(Args, I, Goal, M, M1).
process_command_line(["-t",Arg|Args], I, Goal, _M, M1) :- !,
        atom_string(M, Arg),
	update_toplevel_module(M),
	MI is -I, argv(MI,2),	% delete the 2 arguments
	process_command_line(Args, I, Goal, M, M1).
process_command_line(["--" |_], I, _Goal, M, M) :- !,
	argv(-1, I).	% delete args 1 to I
process_command_line([_ |Args], I, Goal, M, M1) :-
	J is I+1,
	process_command_line(Args, J, Goal, M, M1).




printf_with_current_modes_body(Stream, Value, Module) :-
	printf_current(Stream, Value, '', Module).

printf_goal_body(Stream, Value, Module) :-
	printf_current(Stream, Value, 'G', Module).

printf_goal_body(Stream, Modifiers, Value, Module) :-
	concat_strings(Modifiers, "G", Modifiers1),
	printf_current(Stream, Value, Modifiers1, Module).

printf_current(Stream, Value, Modifiers, Module) :-
	output_mode(Mode),
	concat_string(['%', Mode, Modifiers, 'w'], Format),
	printf_body(Stream, Format, [Value], Module).


%------------------------------------------------------------------------
% numbers corresponding to permissions for a process's read/write/execute 
% permissions on a file used by sys_file_flag/3.
% Need to be accessed in several places
%------------------------------------------------------------------------
process_file_permission(readable,   17).
process_file_permission(writable,   18).
process_file_permission(executable, 19).


%--------------------------------
% Mutual exclusion for parallel system
%--------------------------------

mutex_init_body(Mutex, Module) :-
	setval_body(Mutex, 0, Module).

mutex_body(Mutex, Goal, Module) :-
	get_sys_flag(10, Worker),
	( getval_body(Mutex, Worker, Module) -> % already ours (if nested)
	    ( call(Goal)@Module -> true ; fail )
	;
	    catch(mutex_body(Mutex, Goal, Module, Worker), T,
		mutex_exit(T, Mutex, Worker, Module))
	).

mutex_body(Mutex, Goal, Module, Worker) :-
	( test_and_setval_body(Mutex, 0, Worker, Module) ->
	    ( call(Goal)@Module ->
		setval_body(Mutex, 0, Module)
	    ;
		setval_body(Mutex, 0, Module),
		fail
	    )
	; 
	    sleep(0.01),
	    mutex_body(Mutex, Goal, Module, Worker)
	).

mutex_exit(T, Mutex, Worker, Module) :-
	% We don't know whether the lock was grabbed or not!
	(test_and_setval_body(Mutex, Worker, 0, Module) -> true ; true),
	throw(T).


with_mutex_(Mutex, Goal, Module) :-
	handle_lock_trailed(Mutex, UnlockHandle),
	once(Goal)@Module,
	handle_unlock_free(UnlockHandle).

%:- inline(with_mutex/2, t_with_mutex/2).	% can't use inline/2 yet
:- define_macro(with_mutex/2, t_with_mutex/2, [goal]).
t_with_mutex(with_mutex(Mutex,Goal), (
	sepia_kernel:handle_lock_trailed(Mutex, UnlockHandle),
	once(Goal),
	sepia_kernel:handle_unlock_free(UnlockHandle))
).


%--------------------------------
% Miscellaneous
%--------------------------------

:- tool(fail_if/1, fail_if_body/2).
fail_if_body(X, M) :- call(X)@M, !, fail.
fail_if_body(_, _).

:- tool((once)/1, once_body/2).
once_body(X, M):- call(X)@M, !.

default.		% dummy definition

untraced_true.

!.

(delay X) :- error(78, delay X).

'?-'(H, B) :- error(78, (H ?- B)). % dummy

'-->'(A, B) :- error(78, (A --> B)). % dummy

X \= X :- true, !, fail.
_ \= _.

% obsolete
event_retrieve(Event, Goal) :-
	event_retrieve(Event, Goal, _).


% Utility predicates for embedding
exec_string(GoalString,Vars,Module) :-
	open(GoalString,string,Stream),
	readvar(Stream,Goal,Vars,Module),
	close(Stream),
	call(Goal)@Module.

exec_exdr(GoalString,Module) :-
	open(string(GoalString),read,Stream),
	read_exdr(Stream, Goal),
	close(Stream),
	call_any(Goal, Module).

    call_any(String, Module) :- string(String), !,
	term_string(Goal, String)@Module,
	call(Goal)@Module.
    call_any(Goal, Module) :-
	call(Goal)@Module.

%------------------------------------------
% Some aliases (aliases for tools should
% be made using duplicate tool definitions)
%------------------------------------------

false :- fail.


%------------------------------------------
% Recorded database
% The related C code is in bip_record.c
%------------------------------------------


% current_record_body/2 succeeds iff Key is a key of the indexed database
% (This is terribly inefficient if Key is uninstantiated)

current_record_body(Key, Module):-
	var(Key), !,
	current_functor(Functor, Arity, 1, 0),
	functor(Key, Functor, Arity),
	is_record_body(Key, Module).
current_record_body(Key, Module):-
	( valid_key(Key) ->
	    is_record_body(Key, Module)
	;
	    bip_error(current_record(Key), Module)
	).


% rerecord_body/3 removes all values associated with the first argument before 
% associating the second argument with the first

rerecord_body(Key, Value, Module):-
	( valid_key(Key) ->
	    erase_all_body(Key, Module),
	    recorda_body(Key, Value, Module)
	;
	    bip_error(rerecord(Key, Value), Module)
	).


% erase_body/3 removes an indexed database entry that has been asserted 
% by record or rerecord. It erases the first matching value only, so we
% don't need to worry about logical update semantics.

erase_body(Key, Value, Module):-
	( valid_key(Key) ->
	    first_recorded_(Key, Value, DbRef, Module),
	    erase_first_matching(DbRef, Value)
	;
	    bip_error(erase(Key, Value), Module)
	).

    erase_first_matching(DbRef, Value) :-
	( referenced_record(DbRef, Value) ->
	    erase(DbRef)
	;
	    next_recorded(DbRef, Value, DbRef1),
	    erase_first_matching(DbRef1, Value)
	).

erase_all_body(Key, Value, Module):-
	( valid_key(Key) ->
	    ( first_recorded_(Key, Value, DbRef, Module) ->
		erase_matching(DbRef, Value)
	    ;
		true
	    )
	;
	    bip_error(erase(Key, Value), Module)
	).

    erase_matching(end, _Value) :- !.
    erase_matching(DbRef, Value) :-
	( next_recorded(DbRef, Value, DbRef1) -> true ; DbRef1 = end ),
	( \+ referenced_record(DbRef, Value) ->
	    true
	;
	    erase(DbRef)
	),
	erase_matching(DbRef1, Value).

recorded_body(Key, Value, Module) :-
	recorded_body(Key, Value, _DbRef, Module).


recorded_body(Key, Value, DbRef, Module) :-
	( valid_key(Key) ->
            /* Value used as a filter to reduce DbRef returned */
	    recorded_refs_body(Key, Value, DbRefs, Module),
	    member(DbRef, DbRefs),
	    referenced_record(DbRef, Value)
	;
	    bip_error(recorded(Key, Value, DbRef), Module)
	).


% recordedchk/2,3 find only the first matching record,
% so no need to worry about logical update semantics

recordedchk_body(Key, Value, Module) :-
	recordedchk_body(Key, Value, _DbRef, Module).


recordedchk_body(Key, Value, DbRef, Module) :-
	( valid_key(Key) ->
	    first_recorded_(Key, Value, DbRef0, Module),
	    recorded_member(DbRef0, Value, DbRef)
	;
	    bip_error(recordedchk(Key, Value, DbRef), Module)
	).

    recorded_member(DbRef0, Value, DbRef) :-
	( referenced_record(DbRef0, Value) ->
	    DbRef = DbRef0
	;
	    next_recorded(DbRef0, Value, DbRef1),
	    recorded_member(DbRef1, Value, DbRef)
	).


check_record(NameOrHandle, Handle, Module) :-
	name_to_handle(record, NameOrHandle, H)@Module, !, Handle=H.
check_record(_, _, _) :-
	set_bip_error(45).


:- tool(record_wait_append/4, record_wait_append_/5).
record_wait_append_(Queue, Msg, Timeout, Max, Module) :-
	check_integer_ge(Max, 1),
	check_record(Queue, Handle, Module),
	!,
	with_mutex(Handle, q_snd_(Handle, Msg, Timeout, Max, Module)).
record_wait_append_(Queue, Msg, Timeout, Max, Module) :-
	bip_error(record_wait_append(Queue, Msg, Timeout, Max), Module).

    q_snd_(Handle, Msg, Timeout, Max, Module) :-
	recorded_count(Handle, C),
	( C < Max ->
	    recordz(Handle, Msg)@Module,
	    % always signal, because someone may be waiting for matching entry
	    condition_signal(Handle, all)
	;
	    record_set_max(Handle, Max),
	    condition_wait(Handle, Timeout),	% fail on timeout
	    q_snd_(Handle, Msg, Timeout, Max, Module)
	).

:- tool(record_wait_remove/3, record_wait_remove_/4).
record_wait_remove_(Queue, Msg, Timeout, Module) :-
	check_record(Queue, Handle, Module),
	!,
	with_mutex(Handle, q_rcv_(Handle, Msg, Timeout, Module)).
record_wait_remove_(Queue, Msg, Timeout, Module) :-
	bip_error(record_wait_remove(Queue, Msg, Timeout), Module).

    q_rcv_(Handle, Msg, Timeout, Module) :-
	( erase(Handle, Msg)@Module ->
	    ( record_below_max(Handle) ->
		condition_signal(Handle, all)
	    ;
		true
	    )
	;
	    condition_wait(Handle, Timeout),	% fail on timeout
	    q_rcv_(Handle, Msg, Timeout, Module)
	).



% Erase all Store entries whose keys match Module:_
store_erase_qualified(Store, Module) :-
	stored_keys(Store, Entries),
	Key = Module:_,
	member(Key, Entries),
	store_delete(Store, Key),
	fail.
store_erase_qualified(_, _).


%----------------------------------------------------------------------
% Compiling and loading
%----------------------------------------------------------------------

% ensure_loaded(FileNameOrList, Module)

ensure_loaded([H|T], Module) :-
	-?->
	!,
	ensure_loaded(H, Module),
	ensure_loaded(T, Module).
ensure_loaded([], _) :- -?-> !.
ensure_loaded(File, Module) :-
	get_file(File, yes, FileAtom),
	!,
	ensure_loaded1(FileAtom, Module).
ensure_loaded(File, Module) :-
	bip_error(ensure_loaded(File), Module).

ensure_loaded1(FileAtom, Module) :-
	(
	    current_compiled_file(FileAtom, Time, _Module, _Goal),
	    get_file_info(FileAtom, mtime, FTime),
	    ( FTime =< Time ->
		true
	    ;
		printf(warning_output,
			"WARNING: reloading %w because file has changed (%d -> %d)%n",
			[FileAtom, Time, FTime]),
		fail
	    )
	->
	    true
	;
	    compile_or_load(FileAtom, Module)
	).


% Load compiler predicates lazily
% We can't use import-from currently because they are tools.
compile_term(Term) :- ecl_compiler:compile_term(Term).	% @sepia_kernel
compile_term(Term,Options) :- ecl_compiler:compile_term(Term,Options).	% @sepia_kernel


compile_or_load(FileAtom, Module) :-
	(
	    get_flag(eclipse_object_suffix, ECO),
	    suffix(FileAtom, ECO)
	->
	    load_eco(FileAtom, Module)
	;
	    ecl_compiler:compile_(FileAtom,Module)
	).


% For loading kernel.eco at boot time, we use the C-level load_eco/4 directly.
% Subsequently, we use this code here, which is more complete in the sense
% that it raises all the events, changes directory, etc.

load_eco(FileAtom, Module) :-
	error(146, FileAtom, Module),	% COMPILER_START
	pathname(FileAtom, ParentDir),
	getcwd(OldPath),
	cd(ParentDir),
	cputime(Time0),
	( catch(load_eco(FileAtom, 0, Module, FileModule),
		Tag,
		(cd(OldPath),
		 (error(147, FileAtom) -> true; true),	% COMPILER_ABORT
		 throw(Tag)))
	->
	    Time is cputime - Time0,
	    error(149, end_of_file, FileModule),	% CODE_UNIT_LOADED
	    error(139, (FileAtom,-1,Time), FileModule),	% COMPILED_FILE
	    cd(OldPath),
	    error(166, FileAtom-(sepia_kernel:load_eco(FileAtom,Module)), Module)
	;
	    cd(OldPath),
	    fail
	).


compiled_stream(S) :-
	check_var_or_stream_spec(S), !,
	getval(compiled_stream, CS),
	nonvar(CS),	% fails if nothing is being compiled
	( var(S) -> S = CS ; get_stream(S, CS) ).
compiled_stream(S) :-
	bip_error(compiled_stream(S)).


% This is the body of ./2, no module checking necessary.
% When ./2 occurs as a directive, it is taken as include/1.
% If it is called, we use this code here, and either load or compile.
compile_list_body(H, T, Module) :-	%local to the kernel (tool body)
	Files = [H|T],
	is_list(Files), !,
	comp_or_load_list(Files, Module).
compile_list_body(H, T, Module) :-
	error(5, [H|T], Module).

    comp_or_load_list([], _).
    comp_or_load_list([File|Files], M) :-
	( get_file(File, yes, FileAtom) ->
	    compile_or_load(FileAtom, M)
	;
	    bip_error([File], M)
	),
	comp_or_load_list(Files, M).


%----------------------------------------------------------------------
% File handling primitives
%----------------------------------------------------------------------

exists(File) :-
	check_atom_string(File),
	!,
	expand_filename(File, FileNameS, 1),	% EXPAND_STANDARD
	existing_path(FileNameS, _any).
exists(File) :-
	bip_error(exists(File)).


existing_file(_, _, _, _) :-
	set_bip_error(0).	% reset bip_error, always fails
existing_file(Base0, Extensions, Permissions, FileName) :-
	check_proper_list(Extensions),
	check_proper_list(Permissions),
	expand_wrapper(Base0, Base, ReturnType),
	member(Ext, Extensions),	% Caution: fails to bip_error/1
	check_basic_atomic(Ext),
	concat_string([Base, Ext], FileNameS0),
	expand_filename(FileNameS0, FileNameS, 1),	% EXPAND_STANDARD
	existing_path(FileNameS, file),	 /* must not be a directory */
	check_permissions(Permissions, FileNameS), 
	% FileNameS may be absolute, but we want to return
	% a relative one if a relative one was given
	expand_filename(FileNameS0, FileNameS1, 0),	% EXPAND_SYNTACTIC
	( string(ReturnType) -> FileName = FileNameS1
	; atom_string(FileName, FileNameS1)
	).
existing_file(Base, Exts, Perms, File) :-
	% we may fail here normally, that's why we set_bip_error(0) above
	bip_error(existing_file(Base, Exts, Perms, File)).

existing_path(Path, Type) :-
	% the atime-request fails for nonexisting files and
	% for the pseudo-files aux,con,nul,prn on Windows
	sys_file_flag(Path, 6, _),	% atime
	sys_file_flag(Path, 0, Mode),	% mode
	(8'40000 =:= Mode /\ 8'170000 ->
	     Type = dir
	;
	     Type = file
	).

    check_permissions([], _) :- !.
    check_permissions([P|Ps], FileNameS) :-
	((atom(P), process_file_permission(P, N)) ->
	    sys_file_flag(FileNameS, N, on),
	    check_permissions(Ps, FileNameS)
	;   set_bip_error(6)
	).

    expand_wrapper(library(File), PathFile, ReturnType) :- -?->
	!,
	check_atom_string(File),
	ReturnType = File,
	getval(library_path, Path),
	member(Lib, Path),
	concat_string([Lib, '/', File], PathFile0), 
	(   PathFile = PathFile0
	;
	    pathname(File, _, ModuleS),
	    concat_string([PathFile0, '/', ModuleS], PathFile)
	).
    expand_wrapper(File, File, File) :-
	check_atom_string(File).


canonical_path_name(Path, CanPath) :-
	check_atom_string(Path),
	!,
	expand_filename(Path, CanPathString0, 3),	% EXPAND_NORMALISE
	string_length(CanPathString0, L),
	( get_string_code(L, CanPathString0, 0'/) ->
	    CanPathString = CanPathString0
	; sys_file_flag(CanPathString0, 0) /\ 8'170000 =:= 8'40000 ->
	    % it's a directory
	    concat_strings(CanPathString0, "/", CanPathString)
	;
	    CanPathString = CanPathString0
	),
	( atom(Path) ->
	    atom_string(CanPathAtom, CanPathString),
	    CanPath = CanPathAtom
	;
	    CanPath = CanPathString
	).
canonical_path_name(Path, CanPath) :-
	bip_error(canonical_path_name(Path, CanPath)).


% Get source or precompiled file for compilation, loading, etc.
% suceeds or fail with bip error set
get_file(Var, _, _) :-
	var(Var),
	!,
	set_bip_error(4).
get_file(user, _, user) :- !,
	( get_stream_info(stdin, device, queue) -> set_bip_error(193) ; true ).
get_file(Base, WithObj, FullFileAtom) :-
	getval(prolog_suffix, Sufs0),
	(WithObj == yes ->
	    getval(eclipse_object_suffix, Obj),
	    append([Obj], Sufs0, Sufs)
	;   Sufs0 = Sufs
	),
	(existing_file(Base, Sufs, [readable], FullFile0) ->
	    % only the first choice
	    canonical_path_name(FullFile0, FullFile),
	    (atom(FullFile) -> 
		FullFile = FullFileAtom ; atom_string(FullFileAtom, FullFile)
	    )
	;
	    nonvar(Base), 
	    (Base = library(_) -> set_bip_error(173) ; set_bip_error(171))
	),
	!.
get_file(_, _, _) :-
	set_bip_error(5).


%----------------------------------------------------------------------
% Checks to be done at the end of a compilation:
%
% For all modules into which we have compiled something, check for
% predicates which are
% - declared (demon,tool,visibility,call_type...) but not defined (no code)
% - referenced (call compiled) but not declared not defined
% Note that this check is only done at the end of the toplevel compilation.
% If it were done at the end of every compiled file we would possibly
% check incomplete modules and get lots of unjustified warnings.
% Instead compiled_file_handler/3 just records every module and we
% check them all here in one go.
%----------------------------------------------------------------------

declaration_checks :-
	recorded_list(compiled_modules, Modules0),
	erase_all(compiled_modules),
	sort(Modules0, Modules),	% remove duplicates
	declaration_checks(Modules).

    declaration_checks([]).
    declaration_checks([M|Ms]) :-
	declaration_check(M),
	declaration_checks(Ms).

    declaration_check(M) :-
	atom(M),
	current_module(M),
%	writeln(declaration_check(M)),
	\+ is_locked(M),
	predicate_class_and_error(Class, Error, DisablingPragma),
	\+ current_pragma_(DisablingPragma, M),
	current_module_predicate(Class, P, M),
	\+ deprecated_reexported(Class, P, M),
	error(Error, P, M),
	fail.
    declaration_check(_).

    predicate_class_and_error(undefined,  76, undefined_warnings(off)).
    predicate_class_and_error(undeclared, 77, undeclared_warnings(off)).
    predicate_class_and_error(no_module,  85, no_module_warnings(off)).
    predicate_class_and_error(no_export,  84, no_export_warnings(off)).
    predicate_class_and_error(deprecated, 75, deprecated_warnings(off)).

    % Suppress deprecation warnings for reexported predicates
    % if pragma(deprecated_warnings(not_reexports)) is active
    deprecated_reexported(deprecated, P, M) :-
	current_pragma_(deprecated_warnings(not_reexports), M),
	get_flag_body(P, visibility, reexported, M).
	

%----------------------------------------------------------------------
% Pragmas
%
% Pragmas are initially seen and interpreted by the compiler. If the
% compiler doesn't understand a pragma, it raises error 148 BAD_PRAGMA.
% The handler then records the pragma (together with its module context)
% for later retrieval via current_pragma/1.  Pragmas can be either:
%
% Compound terms: any pragma with identical functor name overrides any
% previously given pragma with the same functor, e.g. in
% :- pragma(verbose(little)).
% :- pragma(verbose(very)).
% the second will override the first. It can't be erased completely.
%
% Atoms: a pragma called 'noxxx' replaces a previously given pragma 'xxx',
% a pragma called 'xxx' replaces a previously given pragma 'noxxx'.
%
%----------------------------------------------------------------------

?- store_create_named(pragmas).

record_pragma(Pragma, Module) :-
	atom(Pragma),
	atom_string(Pragma, PragmaString),
	( substring(PragmaString, "no", 1) ->
	    substring(PragmaString, 2, _, 0, YesPragmaString),
	    atom_string(YesPragma, YesPragmaString),
	    store_delete(pragmas, Module:YesPragma),
	    store_set(pragmas, Module:Pragma, Pragma)
	;
	    concat_atoms(no, Pragma, NoPragma),
	    store_delete(pragmas, Module:NoPragma),
	    store_set(pragmas, Module:Pragma, Pragma)
	).
record_pragma(Pragma, Module) :-
	compound(Pragma),
	functor(Pragma, Name, Arity),
	store_set(pragmas, Module:Name/Arity, Pragma).


:- tool(current_pragma/1, current_pragma_/2).
current_pragma_(Pragma, Module) :-
	var(Pragma),
	stored_keys_and_values(pragmas, Pragmas),
	member((Module:_)-Pragma, Pragmas).
current_pragma_(Pragma, Module) :-
	atom(Pragma),
	store_get(pragmas, Module:Pragma, Pragma).
current_pragma_(Pragma, Module) :-
	compound(Pragma),
	functor(Pragma, Name, Arity),
	store_get(pragmas, Module:Name/Arity, Pragma).


erase_module_pragmas(Module) :-
	reset_name_ctr(Module),
	store_erase_qualified(pragmas, Module).


%----------------------------------------------------------------------
% Compiled-file database
% We record tuples of the form:
%   .(AtomicCanonicalFile,Module,Time,CompId,RecompilationGoal)
%----------------------------------------------------------------------

?- local_record(compiled_file/0).

% File is assumed to be an atom, and the canonical name
record_compiled_file(File, Goal, Module) :-
	( exists(File) ->
	    get_file_info(File, mtime, Time),
	    (recordedchk(compiled_file, .(File, _, _, _), Ref) ->
		erase(Ref)
	    ;
		true
	    ),
	    recorda(compiled_file, .(File, Module, Time, Goal))
	;
	    % some phony file name, like 'user'
	    true
	).


current_compiled_file(File, Time, Module, Goal) :-
	( var(File) ->
	    true
	;
	    ( string(File) ->
		atom_string(FileA, File)
	    ;
		FileA = File
	    ),
	    canonical_path_name(FileA, CanonicalFileA)
	),
	recorded(compiled_file, .(CanonicalFileA, Module, Time, Goal)),
	% don't leave a choicepoint in + mode
	( var(File) -> File = CanonicalFileA ; File = CanonicalFileA, ! ).


% change the module-field of a record
change_compiled_file_module(FileAtom, FileMod) :-
	( recordedchk(compiled_file, .(FileAtom, _Module, Time, Goal), Ref) ->
	    erase(Ref),
	    recorda(compiled_file, .(FileAtom, FileMod, Time, Goal))
	;
	    true
	).


% erase information about which files were compiled into Module
forget_module_files(Module) :-
	(
	    recorded(compiled_file, .(_File, Module, _Time, _Goal), Ref),
	    erase(Ref),
	    fail
	;
	    true
	).


%----------------------------------------------------------------------
% Initialization and finalization Goals
%----------------------------------------------------------------------

?- store_create_named(initialization_goals).
?- store_create_named(finalization_goals).

store_goals(Which, Goal, Module) :-
	check_callable(Goal),	% may fail with bip_error set
	( store_get(Which, Module, Bag) ->
	    true
	;
	    bag_create(Bag),
	    store_set(Which, Module, Bag)
	),
	bag_enter(Bag, Goal).
	

run_stored_goals(Which, Module) :-
	( store_get(Which, Module, Bag) ->
	    store_delete(Which, Module),
	    bag_dissolve(Bag, Goals),
	    run_list_of_goals(Goals, Module)
	;
	    true
	).

    run_list_of_goals([], _).
    run_list_of_goals([Goal|Goals], Module) :-
	    ( catch(call(Goal)@Module, _Tag, fail) ->
		true
	    ;
		error(167, Goal, Module)
	    ),
	    run_list_of_goals(Goals, Module).


forget_stored_goals(Which, Module) :-
	store_delete(Which, Module).


%----------------------------------------------------------------------
% Discontiguous predicates (ISO)
%
% Discontiguous predicates are handled by initially recording their
% (annotated) source, rather than compiling them immediately.
% Clauses are stored in a bag which itself is stored in a hash store
% which maps:	 module:name/arity -> BagHandle
% At the end of a compilation unit, collect_discontiguous_predicates/2
% is invoked, and all discontiguous clauses for this unit compiled.
% The source store entries are removed.  We could make it possible to
% call the predicates (e.g. in a file query) before the end of file
% is reached by invoking demand-driven compilation in the undefined-handler.
%----------------------------------------------------------------------

?- store_create_named(discontiguous_clauses).


% discontiguous declaration
:- tool(discontiguous/1, discontiguous_/2).

discontiguous_(X, Module) :- -?-> X = [_|_], !,
	discontiguous_list(X, Module).
discontiguous_(X, Module) :- -?-> X = (_,_), !,
	discontiguous_seq(X, Module).
discontiguous_(X, Module) :-
	discontiguous1(X, Module).

    discontiguous_list(X, Module) :- var(X), !,
	error(4, discontiguous(X), Module).
    discontiguous_list([], _).
    discontiguous_list([P|Ps], Module) :-
	discontiguous1(P, Module),
	discontiguous_list(Ps, Module).
    discontiguous_list(X, Module) :-
	error(5, discontiguous(X), Module).

    discontiguous_seq((P,Ps), Module) :- -?-> !,
	discontiguous1(P, Module),
	discontiguous_seq(Ps, Module).
    discontiguous_seq(X, Module) :-
	discontiguous1(X, Module).

    discontiguous1(PredSpec, Module) :- var(PredSpec), !,
	error(4, discontiguous(PredSpec), Module).
    discontiguous1(PredSpec, Module) :-
	PredSpec = _/_,
	!,
	( get_flag(PredSpec, stability, dynamic)@Module ->
	    true	% ignore discontiguous declaration
	;
	    % Various cases:
	    % - already declared (ok)
	    % - has clauses from previous compilation of the same file
	    %   (silently replace)
	    % - has clauses that were compiled earlier in this file
	    %   (silently replace, since we can't distinguish from previous case)
	    % - already has clauses from other file
	    %   (will raise multifile-event when compiled later)
	    ( get_flag(PredSpec, declared, on)@Module ->
		true
	    ;
		local(PredSpec)@Module
	    ),
	    Key = Module:PredSpec,
	    ( store_contains(discontiguous_clauses, Key) ->
		% ISO allows multiple declarations for the same predicate
		true
	    ;
		% Start collecting clauses from now on
		bag_create(Bag),
		store_set(discontiguous_clauses, Key, Bag)
	    )
	).
    discontiguous1(PredSpec, Module) :-
	error(5, discontiguous(PredSpec), Module).

record_discontiguous_predicate(Pred, Clauses, AnnClauses, Module) :-
	store_get(discontiguous_clauses, Module:Pred, Bag),	% may fail
	record_discontiguous_clauses(Bag, Clauses, AnnClauses).

    record_discontiguous_clauses(_Bag, [], _).
    record_discontiguous_clauses(Bag, [Clause|Clauses], AnnClauses0) :-
	( nonvar(AnnClauses0) -> AnnClauses0 = [AnnClause|AnnClauses1] ; true ),
	bag_enter(Bag, Clause-AnnClause),
	record_discontiguous_clauses(Bag, Clauses, AnnClauses1).

collect_discontiguous_predicates(Module, Preds) :-
	stored_keys(discontiguous_clauses, Keys),
	collect_discontiguous_predicates(Keys, Module, Preds, []).

    collect_discontiguous_predicates([], _Module, Preds, Preds).
    collect_discontiguous_predicates([Key|Keys], Module, Preds0, Preds) :-
	( Key = Module:Pred ->
	    store_get(discontiguous_clauses, Key, Bag),
	    store_delete(discontiguous_clauses, Key),
	    bag_dissolve(Bag, Clauses),
	    Preds0 = [Pred-Clauses|Preds1]
	;
	    Preds0 = Preds1
	),
	collect_discontiguous_predicates(Keys, Module, Preds1, Preds).

% module has been erased: forget the declarations and bagged clauses
forget_discontiguous_predicates(Module) :-
	stored_keys(discontiguous_clauses, Keys),
	forget_discontiguous_predicates(Keys, Module).

    forget_discontiguous_predicates([], _Module).
    forget_discontiguous_predicates([Key|Keys], Module) :-
	( Key = Module:_ ->
	    % the clause macro is already gone because the module was erased!
	    store_get(discontiguous_clauses, Key, Bag),
	    bag_abolish(Bag),
	    store_delete(discontiguous_clauses, Key)
	;
	    true	% other module, ignore
	),
	forget_discontiguous_predicates(Keys, Module).


%----------------------------------------------------------------------
% Inlined predicates
%
% Inlined predicates are handled by recording their (normalised) source
% while they are being compiled, and using that via the normal inline
% (goal expansion) mechanism.  The transformation predicate is unfold/6.
%----------------------------------------------------------------------

?- store_create_named(inlined_predicates).

inline_(Proc, Module) :-
	define_macro_(Proc, unfold/6, [goal], Module),
	store_delete(inlined_predicates, Module:Proc).

inline_(Proc, Trans, Module) :-
	define_macro_(Proc, Trans, [goal], Module).


unfold(Goal, Unfolded, AnnGoal, AnnUnfolded, _CM, LM) :-
	functor(Goal, F, N),
	store_get(inlined_predicates, LM:F/N, Stored), % may fail
	Stored = source(Head, Body, AnnBody),
	( Goal=Head -> Unfolded=Body ; Unfolded=true ),
	( var(AnnGoal) ->
	    % leave AnnUnfolded uninstantiated
	    true
	; var(AnnBody) ->
	    % inherit Goal's annotation for everything
	    term_to_annotated(Unfolded, AnnGoal, AnnUnfolded, false)
	;
	    % Body keeps its annotations. CAUTION: the Goal=Head unification
	    % above may instantiate variables, and thus render the 'var'
	    % annotations invalid.  However, currently the AnnBody returned
	    % by the compiler does not contain annotated variable, so we are ok.
	    % repair_annotation(AnnBody, AnnUnfolded)
	    AnnUnfolded = AnnBody
	).
	/*
	% conservative expansion, ever useful?
	Unfolded = (Goal=Head, Body),
	( var(AnnGoal) ->
	    % leave AnnUnfolded uninstantiated
	    true
	; var(AnnBody) ->
	    % inherit Goal's annotation for everything
	    term_to_annotated(Unfolded, AnnGoal, AnnUnfolded, false)
	;
	    % Argument unification inherits Goal's annotation
	    term_to_annotated(Head, AnnGoal, AnnHead, false),
	    annotated_create(AnnGoal=AnnHead, AnnGoal, AnnUnify),
	    % Body keeps its annotations, comma inherits Body's annotation,
	    annotated_create((AnnUnify,AnnBody), AnnBody, AnnUnfolded)
	)
	*/


% Called by the compiler
record_inline_source(Head, Body, AnnBody, Module) :-
	functor(Head, F, N),
	store_set(inlined_predicates, Module:F/N, source(Head,Body,AnnBody)).


% module has been erased: forget the stored source
forget_inlined_predicates(Module) :-
	store_erase_qualified(inlined_predicates, Module).


%--------------------------------
% Environment
%--------------------------------

abort :-
	get_sys_flag(10, W),	% get_flag(worker, W)
	( W==0 ->
	    Where = ""
	;
	    concat_string([" on worker ", W], Where)
	),
	printf(log_output, "Aborting execution%s ...\n%b", Where),
	throw(abort).


%:- system.
use_module_body([H|T], Module) :-
	-?->
	!,
	use_module_body(H, Module),
	use_module_body(T, Module).
use_module_body([], _) :- -?-> !.
use_module_body(File, Module) :-
	get_module_name(File, FileMod, IsModuleName),
	( load_module_if_needed(File, FileMod, Module) ->
	    true
	;
	    % backward compatibility: if only a module name was specified,
	    % and such a module exists, use it even if there is no such file
	    IsModuleName == true,
	    is_a_module(FileMod),
	    (ignore_bip_error(171) -> true ; ignore_bip_error(173))
	),
	import_(FileMod, Module),
	import_interface(FileMod, Module),
	!.
use_module_body(File, Module) :-
	bip_error(use_module(File), Module).

    ignore_bip_error(Ignored) :-
	get_bip_error(Err),
	( Err == Ignored -> true ; set_bip_error(Err) ).

% May fail with bip_error set
load_module_if_needed(_, _, Module) :-
	illegal_unlocked_module(Module, Err),
	!,
	set_bip_error(Err).
load_module_if_needed(File, FileMod, Module) :-
	get_file(File, yes, FileAtom),
	ensure_loaded1(FileAtom, Module),
	!,
	(is_a_module(FileMod) ->
	    % fix the compiled_file-record to refer to the module that the
	    % file defines rather than the one from which it was loaded.
	    % This is necessary to erase the record when we erase the module.
	    change_compiled_file_module(FileAtom, FileMod)
	;
	    set_bip_error(80)
	).
load_module_if_needed(_, _, _) :-
	set_bip_error(173).



% Extract the module name from a File/Library specification

get_module_name(File, _, _) :-
	var(File),
	!,
	set_bip_error(4).
get_module_name(File, Module, IsModName) :-
	(string(File); atom(File)),
	!,
	pathname(File, Path, ModuleS, Suffix),
	atom_string(Module, ModuleS),
	( Path="", Suffix="", atom(File) -> IsModName=true ; IsModName=false ).
get_module_name(library(File), Module, IsModName) :-
	-?->
	!,
	get_module_name(File, Module, IsModName).
get_module_name(_, _, _) :-
	set_bip_error(5).


% If module LibModule already exists, succeed.
% Otherwise load library(LibModule) and check that LibModule was created.
% Fails with bip_error set.

check_module_or_load_library(LibModule, _ContextModule) :-
	illegal_module(LibModule, Err), !,
	set_bip_error(Err).
check_module_or_load_library(LibModule, _ContextModule) :-
	is_a_module(LibModule), !.
check_module_or_load_library(LibModule, ContextModule) :-
	Library = library(LibModule),
	get_file(Library, yes, FileAtom),
	ensure_loaded1(FileAtom, ContextModule),
	!,
	(is_a_module(LibModule) ->
	    true		% it worked
	;
	    set_bip_error(80)
	).
check_module_or_load_library(_, _) :-
	set_bip_error(173).


lib(Library, Module) :-		% obsolete
	lib_(Library, Module).

lib_(Library, Module) :-
	use_module_body(library(Library), Module).


current_module_predicate(Which, Pred, M) :-
	module_predicates(Which, Preds, M),
	% don't leave a choicepoint in ++ mode
	( ground(Pred) -> memberchk(Pred, Preds) ; member(Pred, Preds) ).


% this predicate is called on macro transformation
% trans_term( <trans_pred>(OldTerm, NewTerm, Module), <trans_module>)

trans_term(Goal, Module) :-
	subcall_init,			% expanded subcall
	untraced_call(Goal, Module),
	!,
	subcall_fini(DG),
	( DG == [] ->
	    true
	;
	    error(129, Goal, Module)
	).
trans_term(Goal, _) :-
	arg(1, Goal, Term),	% if it fails return the old term
	arg(2, Goal, Term).

%----------------------------------------------------------------
% subcall(Goal, Delayed)
% call a goal, return the remaining delayed goals and undelay them
%----------------------------------------------------------------

:- tool(subcall/2, subcall/3).

subcall(Goal, Delayed,	Module) :-
	subcall_init,
	untraced_call(Goal, Module),
	true,			% force all wakings
	subcall_fini(Delayed).

% call_priority(Goal, Prio, Module)
% call the specified goal with the given priority, on return force waking
:- tool(call_priority/2, call_priority/3).
call_priority(Goal, Prio, Module) :-
	integer(Prio), !,
	get_priority(P),
	( Prio < P ->
	    set_priority(Prio, 1),
	    call(Goal)@Module,
	    set_priority(P, 1),
	    wake
	; Prio > P ->
	    make_suspension(Goal, Prio, S, Module),
	    schedule_suspensions(1, s([S]))
	    % no wake/0 necessary
	;
	    call(Goal)@Module
	).
call_priority(Goal, Prio, Module) :-
	( var(Prio) -> E=4 ; E=5 ),
	error(E, call_priority(Goal,Prio), Module).


inline_calls(subcall(Goal, Delayed), Inlined, Module) :- -?->
	nonvar(Goal),
	tr_goals(Goal, TrGoal, Module),
	Inlined = (
	    sepia_kernel:subcall_init,
	    TrGoal,
	    true,			% force all wakings
	    sepia_kernel:subcall_fini(Delayed)
	).
inline_calls(call_priority(Goal, Prio), Inlined, Module) :- -?->
	nonvar(Goal),
	tr_goals(Goal, TrGoal, Module),
	Inlined0 = (
	    get_priority(P),
	    ( Prio =< P ->
		sepia_kernel:set_priority(Prio),
		TrGoal, % expand Goal only once, could be big!
		sepia_kernel:set_priority(P),
		wake
	    ;
		make_suspension(Goal, Prio, S, Module),
		schedule_suspensions(1, s([S]))
	    )
	),
	(integer(Prio) -> 
	    Inlined = Inlined0
	;
	    Inlined = (
		integer(Prio) -> 
		    Inlined0
		; var(Prio) ->
		    error(4, call_priority(Goal, Prio), Module)
		;
		    error(5, call_priority(Goal, Prio), Module)
	    )
	).
inline_calls(call_explicit(Goal, LM), Inlined, Module) :- -?->
	tr_goals(LM:Goal, Inlined, Module).


% call_local(Goal, Module)
% [ This used to call Goal in an independent local computation, separating
%   its woken goals from the current ones. That does not seem to make much
%   sense though, since the saved goals temporarily effectively disappear from
%   the resolvent, ie they are there but don't run even when woken again.]
% We are now just creating a local postponed-list.
call_local(Goal, Module) :-
	reinit_postponed(OldPL),
	call(Goal)@Module,
	trigger_postponed,
	reset_postponed(OldPL).


call_explicit_body(Goal, DefMod, CallerMod) :-
	:@(DefMod, Goal, CallerMod).

'[]:@'(X, Goal, CallerMod) :- var(X), !,
	error(4, X:Goal, CallerMod).
'[]:@'([], _Goal, _CallerMod) :- !.
'[]:@'([LookupMod|LookupMods], Goal, CallerMod) :- !,
	:@(LookupMod, Goal, CallerMod),
	'[]:@'(LookupMods, Goal, CallerMod).
'[]:@'(LookupMod, Goal, CallerMod) :-
	:@(LookupMod, Goal, CallerMod).


% Backward compatibility:
call2_(Goal, CM, _) :-
        atom(CM),
        is_a_module(CM),
        !,
        call(Goal)@CM.
call2_(Goal, Arg, CM) :-
        call_(Goal, Arg, CM).


%
% call_boxed(Goal, OnCall, OnExit, OnRedo, OnFail)
%	wrap a goal into four port actions
%
% Careful: this is all quite tricky and easy to break!
%
% The actions OnCall, OnExit, OnRedo, OnFail should always succeed without
% leaving choicepoints.	 Order of these actions:
%
% OnCall is done after requesting OnFail (if other order is needed, you can
%	always call OnCall' before call_boxed and set OnCall to true).
% OnExit is done before requesting OnRedo (if other order is needed, you can
%	always call OnExit' after call_boxed and set OnExit to true).
%
% Item serves two purposes: (1) it is the container for the timestamp.
% (2) it indicates to the GC that the fail-event trail frames are garbage
% when Item becomes garbage (the trail frames contain a weak pointer to Item).
% It is therefore important that there is an occurrence of Item in the code
% _after_ the call to Goal (otherwise Item could become garbage too early).
%
% OnFailEvent is not conditional on a choicepoint (always timestamp=old).
% OnFailEvent is disabled on exit and reenabled on redo.
% OnFailEvent is garbage collected after Item becomes garbage.
% OnRedoEvent is conditional on a choicepoint in Goal (timestamp=old/current).
% OnRedoEvent is garbage collected when its timestamp becomes current or when
%	Item becomes garbage (which will normally happen simultaneously).
%
% The Age = current test is just an optimisation. Doing the else-case would
% also work: request_fail_event wouldn't do anything because of the timestamp.
%


call_boxed_(Goal, OnCall, OnExit, OnRedo, OnFail, Module) :-
	call_boxed_(Goal, OnCall, OnExit, OnRedo, OnFail, Module, Module).

call_boxed_(Goal, OnCall, OnExit, OnRedo, OnFail, GoalModule, ActionModule) :-

	Item = f(_F), timestamp_init(Item, 1),
	event_create(OnFail, OnFailEvent)@ActionModule,
	request_fail_event(Item, Item, 1, OnFailEvent),

	call(OnCall)@ActionModule,

	timestamp_update(Item, 1),
	call(Goal)@GoalModule,

	call(OnExit)@ActionModule,
	event_disable(OnFailEvent),

	timestamp_age(Item, 1, Age),	% don't merge this line with the next!
	( Age = current ->
	    true
	;
	    event_create((event_enable(OnFailEvent),OnRedo), OnRedoEvent)@ActionModule,
	    request_fail_event(Item, Item, 1, OnRedoEvent)
	).



%--------------------------------
% Stuff moved here from the list library because the kernel needs it.
% Will be reexeported through lists later.
%--------------------------------

% member/2
% (This version doesn't leave a choicepoint after the last result)
member(X, [H|T]) :- member(X, H, T).
member(X, X, _).
member(X, _, [H|T]) :- member(X, H, T).


memberchk(X,[X|_]) :- true, !.
memberchk(X,[_|T]):- memberchk(X,T).


nonmember(Arg,[Arg|_]) :- true, !,
	fail.
nonmember(Arg,[_|Tail]) :- !,
	nonmember(Arg,Tail).
nonmember(_,[]).


% delete (?Element, ?List, ?Result)
% Result is List with Element removed
delete(A, [A|C], C).
delete(A, [B|C], [B|D]) :-
	delete(A, C, D).


append([], Ys, Ys).
append([X|Xs], Ys, [X|XsYs]) :- append(Xs, Ys, XsYs).


reverse(List, Rev) :-
	reverse(List, Rev, []).

    reverse([], L, L).
    reverse([H|T], L, SoFar) :-
	reverse(T, L, [H|SoFar]).


% length(?List, ?Length)
% succeeds iff List is a list of length Length

length(List, Length) :-
	var(Length),
	!,
	length(List, 0, Length).
length(List, Length) :-
	integer(Length),
	Length >= 0,
	length1(List, Length).

    :- mode length(?,+,?).
    length([], Length, Length).
    length([_|L], N, Length) :-
	+(N, 1, N1),		% because no inlining yet
	length(L, N1, Length).

    :- mode length1(?,+).
    length1(L, 0) :- !, L=[].
    length1([_|L], Length) :-
	-(Length, 1, N1),	% because no inlining yet
	length1(L, N1).


% subtract(L1, L2, L3)
% L3 = L1 - L2

subtract([], _, []).
subtract([Head|L1tail], L2, L3) :-
	memberchk(Head, L2),
	!,
	subtract(L1tail, L2, L3).
subtract([Head|L1tail], L2, [Head|L3tail]) :-
	subtract(L1tail, L2, L3tail).


same_length([], []).
same_length([_|Xs], [_|Ys]) :-
	same_length(Xs, Ys).

%-----------------------------
% Module system
%-----------------------------

% The compiler wraps queries inside module_interfaces
% into calls to record_interface/2

record_interface((G1,G2), Module) :- -?->
	record_interface(G1, Module),
	record_interface(G2, Module).
record_interface(Goal, Module) :-
	interpret_obsolete_queries(Goal, IGoal), !,
	( IGoal == true ->
	    true
	;
	    record_interface_directive(IGoal, Module)
	),
	call(Goal)@Module.
record_interface(Goal, Module) :-
%	printf(warning_output,
%	    "WARNING: not a proper interface query in interface of %w: %w%n",
%	    [Module,Goal]),
	call(Goal)@Module.


    % How to interpret queries in old-style module interfaces
    % in terms of new export directives
    % Non-interface export/reexport are interpreted as-is.

    :- mode interpret_obsolete_queries(?,-).
    interpret_obsolete_queries(Var, _) :- var(Var), !, fail.
    interpret_obsolete_queries(global(_), true).
    interpret_obsolete_queries(local(_), true).
    interpret_obsolete_queries(export(_), true).
    interpret_obsolete_queries(reexport(_), true).
    interpret_obsolete_queries(call(_), true).
    interpret_obsolete_queries(use_module(M), use_module(M)).
    interpret_obsolete_queries(lib(M), use_module(library(M))).
    interpret_obsolete_queries(import(From), import(From)).
    interpret_obsolete_queries(op(A,B,C), export op(A,B,C)).
    interpret_obsolete_queries(set_chtab(A,B), export chtab(A,B)).
    interpret_obsolete_queries(define_macro(A,B,C), export macro(A,B,C)).
    interpret_obsolete_queries(set_flag(syntax_option,A), export syntax_option(A)).
    interpret_obsolete_queries(meta_attribute(A,B), global meta_attribute(A,B)).
    interpret_obsolete_queries(call_explicit(Goal,sepia_kernel), IGoal) :-
	    interpret_obsolete_queries(Goal, IGoal).
    interpret_obsolete_queries(sepia_kernel:Goal, IGoal) :-
	    interpret_obsolete_queries(Goal, IGoal).


% The interface is recorded as follows:
%	- The interface queries of module M are recorded
%	  under the key M/1 (predicate exports are not recorded)
%	- If M1 uses M2, the record M2 is recorded under the key M1/2

record_interface_directive((export _/_), _Module) :- -?-> !.
record_interface_directive((export macro(F,TransPred,Options)), Module) :- -?-> !,
	qualify_(TransPred, QualTransPred, Module),
	init_module_record(1, (export macro(F,QualTransPred,Options)), Module).
record_interface_directive((export portray(F,TransPred,Options)), Module) :- -?-> !,
	qualify_(TransPred, QualTransPred, Module),
	init_module_record(1, (export portray(F,QualTransPred,Options)), Module).
record_interface_directive(Directive, Module) :-
	init_module_record(1, Directive, Module).

    unqualify(Thing, CM, CM, Thing) :- var(Thing), !.
    unqualify(LM:Thing, _, LM, Thing) :- !.
    unqualify(Thing, CM, CM, Thing).


    init_module_record(N, Value, Module) :-
	functor(Key, Module, N),
	( is_record(Key) -> true ; local_record(Module/N) ),
	( recorded(Key, Old), compare_instances(=, Old, Value, _) -> 
	    true
	;
	    recordz(Key, Value)
	).

recorded_interface_directive(Module, Directive) :-
	functor(Key, Module, 1),
	recorded(Key, Directive).


record_module_import(Import, Module) :-
	init_module_record(2, Import, Module).

recorded_module_import(Module, Import) :-
	functor(Key, Module, 2),
	recorded(Key, Import).
	
erase_module_related_records(Module) :-
	% erase information about Module's interface queries
	functor(Key1, Module, 1),
	( is_record(Key1) -> erase_all(Key1) ; true ),

	% erase information about which modules were imported into Module
	functor(Key, Module, 2),
	( is_record(Key) -> erase_all(Key) ; true ),

	% erase any information stored on behalf of the module
	erase_module_structs(Module),
	erase_module_domains(Module),
	erase_module_pragmas(Module),
	erase_deprecation_advice(Module),
	erase_meta_predicates(Module),
	forget_discontiguous_predicates(Module),
	forget_inlined_predicates(Module),
	forget_stored_goals(initialization_goals, Module),
	forget_stored_goals(finalization_goals, Module),
	reset_name_ctr(Module),

	% erase information about which files were compiled into Module
	forget_module_files(Module).

erase_module(Mod, From_mod) :-
	check_atom(Mod),
	check_module(From_mod),
	( is_a_module(Mod) ->
	    ( Mod == From_mod ->
	    	set_bip_error(101)
	    ; is_locked(Mod), From_mod\==sepia_kernel, \+authorized_module(From_mod) ->
		% locked modules can only be deleted from sepia_kernel
		% (needed only for system cleanup, i.e. erase_modules/0)
		set_bip_error(82)
	    ;
		erase_module_unchecked(Mod, From_mod)
	    )
	;
	    true
	),
	!.
erase_module(Mod, From_mod) :-
	get_bip_error(Error),
	error(Error, erase_module(Mod), From_mod).


% may fail with bip_error set
erase_module_unchecked(Mod, From_mod) :-
	run_stored_goals(finalization_goals, Mod),
	erase_module_attribute_handlers(Mod),
	erase_module_(Mod, From_mod),
	erase_module_related_records(Mod).


% Cleanup: Erase all modules except sepia_kernel, and finalize sepia_kernel.
% Because we currently don't keep track of module dependencies, we first
% finalize all modules, and then delete them. This should avoid problems
% caused by finalizers that assume the existence of other modules.
erase_modules :-
	module_tag(sepia_kernel, Self),
	(
	    current_module(Module), Module \== Self,
	    run_stored_goals(finalization_goals, Module),
	    erase_module_attribute_handlers(Module),
	    fail
	;
	    current_module(Module), Module \== Self,
	    % erase_module won't run the finalizers again
	    ( erase_module_unchecked(Module, Self) -> true ; get_bip_error(_) ),
	    fail
	;
	    run_stored_goals(finalization_goals, Self)
	).


%
% get_module_info(+Module, +What, -Info)
% Built-in to query the module interface and other properties
%

get_module_info(Module, What, Info) :-
	illegal_existing_module(Module, Error), !,
	error(Error, get_module_info(Module, What, Info)).
get_module_info(Module, raw_interface, Info) :-
	findall(D, raw_interface(Module, D), Info).
get_module_info(Module, interface, Info) :-
	findall(D, interface_closure(Module, [Module], D), Info).
get_module_info(Module, imports, Info) :-
	findall(D, recorded_module_import(Module, D), Info).
get_module_info(Module, locked, Info) :-
	( is_locked(Module) -> Info=on ; Info=off).
% no range check because of get_module_info(+,-,-) mode

    raw_interface(Module, (export Pred)) :-
	current_module_predicate(exported, Pred, Module).
    raw_interface(Module, Directive) :-
	recorded_interface_directive(Module, Directive).


%
% Primitives to enumerate the module interface, expanding
% reexports and applying 'from' and 'except' filters:
%
% interface_closure(+Module, +VisitedModules, -Directive) is nondet
% interface_closure_only(+Module, +Preds, +Others, +VisitedModules, -Directive) is nondet
% interface_closure_except(+Module, +Preds, +Others, +VisitedModules, -Directive) is nondet
%

interface_closure(Module, Visited, Directive) :-
	interface_closure_preds(Module, Visited, Directive).
interface_closure(Module, Visited, Directive) :-
	interface_closure_nopreds(Module, Visited, Directive).

interface_closure_preds(Module, _, (export Pred)) :-
	current_module_predicate(exported_reexported, Pred, Module).

interface_closure_nopreds(Module, Visited, Directive) :-
	recorded_interface_directive(Module, D),
	( D = (reexport Items from M) ->
	    nonmember(M, Visited), % prevent looping
	    split_export_list(Items, _Preds, [], Other, []),
	    interface_closure_nopreds_only(M, Other, [M|Visited], Directive)
	; D = (reexport M except Except) ->
	    nonmember(M, Visited), % prevent looping
	    split_export_list(Except, _Preds, [], Other, []),
	    interface_closure_nopreds_except(M, Other, [M|Visited], Directive)
	; D = (reexport M) ->
	    nonmember(M, Visited), % prevent looping
	    interface_closure_nopreds(M, [M|Visited], Directive)
	;
	    Directive = D
	).

interface_closure_preds_only(_Module, Preds, _Visited, (export Pred)) :-
	member(Pred, Preds).
%	current_module_predicate(exported_reexported, Pred, Module).

interface_closure_nopreds_only(Module, Other, Visited, Directive) :-
	interface_closure_nopreds(Module, Visited, Directive),
	Directive = (export Item),
	not nonmember(Item, Other).

interface_closure_preds_except(Module, Preds, _Visited, (export Pred)) :-
	current_module_predicate(exported_reexported, Pred, Module),
	nonmember(Pred, Preds).

interface_closure_nopreds_except(Module, Other, Visited, Directive) :-
	interface_closure_nopreds(Module, Visited, Directive),
	( Directive = (export Item) ->
	    nonmember(Item, Other)
	;
	    true
	).


%
% Import Module's interface into Where
% This only needs to deal with the non-predicate directives,
% because the predicate visibility is implemented on a lower level.
%

import_interface(Module, Where) :-		% may fail with bip_error
	( recorded_module_import(Where, Module) ->
	    true				% already imported
	;
	    (
		interface_closure(Module, [Module], Goal),
		( import_interface_directive(Goal, Module, Where) -> true ; ! ),
		fail
	    ;
		true
	    ),
	    record_module_import(Module, Where)
	).


    % Doesn't have to deal with reexports, they are expanded before

    import_interface_directive(export(Items), From, M) :- -?-> !,
	import_exported(Items, From, M).
    import_interface_directive(global(_), _From, _M) :- -?-> !.
    import_interface_directive(use_module(File), _From, M) :- -?-> !,	% compatibility
	use_module(File)@M.
    import_interface_directive(import(From), _From, M) :- -?-> !,	% compatibility
	import(From)@M.
    import_interface_directive((A,B), F, M) :- -?-> !,
	import_interface_directive(A, F, M),
	import_interface_directive(B, F, M).
    import_interface_directive(Goal, _From, _Module) :-
	write(error, "Unrecognized interface spec (ignored): "),
	write(error, Goal), nl(error).


    % Split a comma-list of reexport exceptions into predicates
    % and others, and return them in two proper lists
    % may fail with bip_error
    split_export_list((Except,Excepts), Preds, Preds0, Other, Other0) :- -?-> !,
	split_export_list(Except, Preds, Preds1, Other, Other1),
	split_export_list(Excepts, Preds1, Preds0, Other1, Other0).
    split_export_list(N/A, Preds, Preds0, Other, Other0) :- -?-> !,
	check_partial_predspec(N/A),
	Preds = [N/A|Preds0], Other = Other0.
    split_export_list(Except, Preds, Preds0, Other, Other0) :-
	valid_export_spec(Except), !,
	Preds = Preds0, Other = [Except|Other0].
    split_export_list(_Except, _Preds, _Preds0, _Other, _Other0) :-
	set_bip_error(6).


% The compiler calls this for both module/1 and module_interface/1
% It erases the module and re-creates it

module_directive(New_module, From_module, Exports, Language) :-
	(
	    check_atom(New_module),
	    erase_module_unchecked(New_module, From_module)
	->
	    create_module(New_module, Exports, Language)
	;
	    bip_error(module(New_module))
	).

module(M):-
	error(81, module(M)).

get_unqualified_goal(_QM:Goal, UGoal) :- -?-> !, UGoal=Goal.
get_unqualified_goal(Goal, Goal).

create_module_if_did_not_exist(M, Language) :-
	(is_a_module(M) -> true ; create_module(M, [], Language) ).

create_module(M) :-
	create_module(M, [], eclipse_language).

create_module(M, Exports, Language) :-
	create_module_(M),
	import_body(Language, M),
	export_list(Exports, M).

% update toplevel module with new language if possible
update_toplevel_module(M) :-
	getval(default_language, Language),
	( is_a_module(M) ->
	    (
		\+is_locked(M),
		M \== sepia_kernel,
		get_module_info(M, imports, Imports),
		nonmember(Language, Imports)
	    ->
		erase_module(M),
		create_module(M, [], Language)
	    ;
		true
	    )
	;
	    create_module(M, [], Language)
	),
	setval(toplevel_module, M).

set_toplevel_module(M) :-	% fails on error with bip_error set
	( var(M) ->
	    set_bip_error(4)
	; \+atom(M) ->
	    set_bip_error(5)
	; is_a_module(M) ->
	    ( is_locked(M) ->
		set_bip_error(82)
	    ;
		true
	    )
	;
	    error(83, module(M)),
	    getval(default_language, Language),
	    create_module(M, [], Language)
	),
	setval(toplevel_module, M).


%-----------------------------

prepend_user_path(List0, List) :-
	getenv("ECLIPSELIBRARYPATH", Dirs),
	!,
	open(Dirs, string, Stream),
	prepend_user_path(Stream, List0, List).
prepend_user_path(List, List).

prepend_user_path(S, List0, List) :-
	read_string(S, ":", _, Dir) ->
	    prepend_user_path(S, List0, List1),
	    List = [Dir|List1]
	;
	    close(S),
	    List = List0.


stack_overflow_message(global_trail_overflow) :-
	write(error, "*** Overflow of the global/trail stack"),
	( get_flag(gc, off) ->
	    writeln(error, "!"),
	    writeln(error, "Switch on the garbage collector with \"set_flag(gc,on).\"")
	;
	    writeln(error, " in spite of garbage collection!")
	),
	statistics(global_stack_peak, G),
	statistics(trail_stack_peak, T),
	( G+T >= get_flag(max_global_trail) ->
	    writeln(error, "You can use the \"-g kBytes\" (GLOBALSIZE) option to have a larger stack.")
	;
	    writeln(error, "You are probably out of virtual memory (swap space).")
	),
	GK is G//1024, TK is T//1024,
	printf(error, "Peak sizes were: global stack %d kbytes, trail stack %d kbytes%n",
		[GK,TK]).
stack_overflow_message(local_control_overflow) :-
	writeln(error, "*** Overflow of the local/control stack!"),
	statistics(local_stack_peak, L),
	statistics(control_stack_peak, C),
	( L+C >= get_flag(max_local_control) ->
	    writeln(error, "You can use the \"-l kBytes\" (LOCALSIZE) option to have a larger stack.")
	;
	    writeln(error, "You are probably out of virtual memory (swap space).")
	),
	LK is L//1024, CK is C//1024,
	printf(error, "Peak sizes were: local stack %d kbytes, control stack %d kbytes%n",
		[LK,CK]).
stack_overflow_message(fatal_signal_caught) :-
	write(error, "Segmentation violation - possible reasons are:\n"
	    "- a faulty external C function\n"
	    "- certain operations on circular terms\n"
	    "- machine stack overflow\n"
	    "- an internal error in ECLiPSe\n"
	    "ECLiPSe may have become unstable, restart recommended\n"
	),
	flush(error).
stack_overflow_message(error(IsoError,ImpDefTerm)) :-
	nonvar(IsoError),
	( IsoError = syntax_error(Description) ->
	    print_syntax_error(Description, ImpDefTerm)
	;
	    ( iso_print_error(IsoError, String, Params) ->
		printf(error, String, Params)
	    ;
		printf(error, "Error \"%w\"", [IsoError])
	    ),
	    ( var(ImpDefTerm) ->
		nl(error)
	    ;
		output_mode(Mode),
		concat_string([" in %", Mode, "w%n"], Format),
		printf(error, Format, [ImpDefTerm])
	    ),
	    flush(error)
	).

iso_print_error(instantiation_error, "instantiation fault", []).
iso_print_error(uninstantiation_error(Actual), "variable expected, found %w", [Actual]).
iso_print_error(type_error(Expected,Actual), "type error: expected %w, found %w", [Expected,Actual]).
iso_print_error(domain_error(Expected,Actual), "domain error: expected %w, found %w", [Expected,Actual]).
iso_print_error(existence_error(ObjectType, Culprit), "%w does not exist: %w", [ObjectType, Culprit]).
iso_print_error(permission_error(Operation, PermissionType, Culprit), "permission error during %w of %w: %w", [Operation,PermissionType,Culprit]).
iso_print_error(representation_error(Flag), "cannot represent %w", [Flag]).
iso_print_error(evaluation_error(Error), "arithmetic exception %w", [Error]).
iso_print_error(resource_error(Resource), "resource %w exhausted", [Resource]).
iso_print_error(syntax_error(Description), "syntax error: %w", [Description]).
iso_print_error(system_error, "unspecified system error", []).


is_macro(Type, Pred, List, PredModule, Module) :-
	% CAUTION: 12 == TRANS_PROP, 17 == WRITE_CLAUSE_TRANS_PROP
	between(12, 17, Prop),
	is_macro(Type, Pred, List, PredModule, Module, Prop).
	
current_type(compound).
current_type(string).
current_type(rational).
current_type(breal).
current_type(goal).
current_type(integer).
current_type(float).
current_type(atom).
current_type(handle).


%-----------------------------
% autoload declarations
%-----------------------------

autoload(File, List) :-
	autoload(File, List, File, []).

autoload_tool(File, List) :-
	error(267, autoload_tool(File, List)).

autoload_system(File, List) :-
	autoload(File, List, File, [system]).


autoload(File, Var, Module, _) :-
	(var(File) ; var(Var)),
	!,
	error(4, autoload(File, Var), Module).
autoload(File, Procs, Module, Flags) :-
	atom(File),
	create_module_if_did_not_exist(Module, eclipse_language),
	set_procs_flags(Procs, Module, [autoload|Flags]),
	!.
autoload(File, Nonsense, _, _):-
	error(5, autoload(File, Nonsense)).


set_procs_flags([], _, _).
set_procs_flags([F/A->TF/TA|Rest], Module, Flags) :- !,
	export_body(F/A, Module),
	tool_(F/A, TF/TA, Module),
	set_flags(Flags, F, A, Module),
	set_flags(Flags, TF, TA, Module),
	set_procs_flags(Rest, Module, Flags).
set_procs_flags([F/A|Rest], Module, Flags) :-
	export_body(F/A, Module),
	set_flags(Flags, F, A, Module),
	set_procs_flags(Rest, Module, Flags).

set_flags([], _, _, _).
set_flags([Flag|Flags], F, A, Module) :-
	set_proc_flags(F/A, Flag, on, Module),
	set_flags(Flags, F, A, Module).


%--------------------------------
% I/O
%--------------------------------

tyi(X) :- tyi(input, X).
tyo(X) :- tyo(output, X).
get_char(X) :- get_char(input, X).
put_char(X) :- put_char(output, X).
display(X) :- display(output, X).


printf_body(Format, List, Module) :- 
	printf_(output, Format, List, Module, 0'%, ErrF, ErrL, Res),
	(Res = 0 ->
		true
	;
		error(Res, printf(ErrF, ErrL), Module)
	).

printf_body(Stream, Format, List, Module) :- 
	printf_(Stream, Format, List, Module, 0'%, ErrF, ErrL, Res),
	(Res = 0 ->
		true
	;
		error(Res, printf(Stream, ErrF, ErrL), Module)
	).

sprintf_(String, Format, List, Module) :-
	( check_var_or_string(String) ->
	    open(string(""), write, Stream),
	    printf_(Stream, Format, List, Module, 0'%, ErrF, ErrL, Res),
	    (Res == 0 ->
		get_stream_info(Stream, name, Written),
		close(Stream),
		String = Written
	    ;
		close(Stream),
		error(Res, sprintf(String, ErrF, ErrL), Module)
	    )
	;
	    bip_error(sprintf(String, Format, List), Module)
	).


read_token_(Token, Class, Module) :-
	read_token_(input, Token, Class, Module).


pathname(Name, Path) :-
	pathname(Name, Path, _).

pathname(DirBaseSuffix, Dir, Base, Suffix) :-
	pathname(DirBaseSuffix, Dir, BaseSuffix),
	suffix(BaseSuffix, Suffix),
	BaseLen is string_length(BaseSuffix) - string_length(Suffix),
	substring(BaseSuffix, 1, BaseLen, Base).

writeln_body(X, M) :- writeln_body(output, X, M).

nl :-	nl(output).
 
compiled_file(File, Line) :-
	compiled_stream(Stream),
	get_stream_info(Stream, name, File),
	get_stream_info(Stream, line, Line).


%--------------------------------
% Arithmetic
%--------------------------------

% the general evaluation predicate is/2
% Note that it is usually optimised away by the compiler

is_body(R, X, M) :-
	var(X), !,
	( coroutining ->		% delay R is X if var(X).
	    make_suspension(R is X, 0, Susp, M),
	    insert_suspension(X, Susp, 1 /*inst*/, suspend)
	;
	    error(4, R is X, M)
	).
is_body(R, X, M) :- callable(X), !, eval(X, R, M).
is_body(R, X, _) :- number(X), !, R=X.
is_body(R, X, M) :- error(24, R is X, M).


% eval(X, R, M) - evaluate an arithmetic expression.
%
% This is used by is/2 and compare_handler/4.
% The arithmetic expression X must be syntactically valid, 
% ie. (number(X) ; compound(X) ; atom(X)).
% eval/3 itself does not raise errors. This is done to ensure that
% the errors are reported in the builtin that tries to use
% the result (to make it consistent with the expanded arithmetic).

:- mode eval(?,?,+).

eval(X, R, _) :- var(X), !, R=X.
eval(X, R, _) :- number(X), !, R=X.
eval(eval(X), R, M) :-	!, eval(X,R,M).
eval(+X, R, M) :-	!, eval(X,X1,M), +(X1, R).
eval(-X, R, M) :-	!, eval(X,X1,M), -(X1, R).
eval(abs(X), R, M) :-	!, eval(X,X1,M), abs(X1, R).
eval(sgn(X), R, M) :-	!, eval(X,X1,M), sgn(X1, R).
eval(fix(X), R, M) :-	!, eval(X,X1,M), fix(X1, R).
eval(integer(X), R, M) :-   !, eval(X,X1,M), integer(X1, R).
eval(rational(X), R, M) :- !, eval(X,X1,M), rational(X1, R).
eval(rationalize(X), R, M) :- !, eval(X,X1,M), rationalize(X1, R).
eval(numerator(X), R, M) :- !, eval(X,X1,M), numerator(X1, R).
eval(denominator(X), R, M) :- !, eval(X,X1,M), denominator(X1, R).
eval(float(X), R, M) :- !, eval(X,X1,M), float(X1, R).
eval(breal(X), R, M) :- !, eval(X,X1,M), breal(X1, R).
eval(breal_from_bounds(L, U), R, M) :- !, eval(L,L1,M), eval(U,U1,M), breal_from_bounds(L1, U1, R).
eval(breal_min(X), R, M) :- !, eval(X,X1,M), breal_min(X1, R).
eval(breal_max(X), R, M) :- !, eval(X,X1,M), breal_max(X1, R).
eval(floor(X), R, M) :- !, eval(X,X1,M), floor(X1, R).
eval(ceiling(X), R, M) :- !, eval(X,X1,M), ceiling(X1, R).
eval(round(X), R, M) :- !, eval(X,X1,M), round(X1, R).
eval(truncate(X), R, M) :- !, eval(X,X1,M), truncate(X1, R).
eval(copysign(X,Y), R, M) :- !, eval(X,X1,M), eval(Y,Y1,M), copysign(X1, Y1, R).
eval(nexttoward(X,Y), R, M) :- !, eval(X,X1,M), eval(Y,Y1,M), nexttoward(X1, Y1, R).
eval(random(X), R, M) :- !, eval(X,X1,M), random(X1, R).
eval(\X, R, M) :-	!, eval(X,X1,M), \(X1, R).
eval(X + Y, R, M) :-	!, eval(X,X1,M), eval(Y,Y1,M), +(X1, Y1, R).
eval(X - Y, R, M) :-	!, eval(X,X1,M), eval(Y,Y1,M), -(X1, Y1, R).
eval(X * Y, R, M) :-	!, eval(X,X1,M), eval(Y,Y1,M), *(X1, Y1, R).
eval(X / Y, R, M) :-	!, eval(X,X1,M), eval(Y,Y1,M), /(X1, Y1, R).
eval(X // Y, R, M) :-	!, eval(X,X1,M), eval(Y,Y1,M), //(X1, Y1, R).
eval(X rem Y, R, M) :-	!, eval(X,X1,M), eval(Y,Y1,M), rem(X1, Y1, R).
eval(X div Y, R, M) :-	!, eval(X,X1,M), eval(Y,Y1,M), div(X1, Y1, R).
eval(X mod Y, R, M) :-	!, eval(X,X1,M), eval(Y,Y1,M), mod(X1, Y1, R).
eval(X ^ Y, R, M) :-	!, eval(X,X1,M), eval(Y,Y1,M), ^(X1, Y1, R).
eval(min(X,Y), R, M) :- !, eval(X,X1,M), eval(Y,Y1,M), min(X1, Y1, R).
eval(max(X,Y), R, M) :- !, eval(X,X1,M), eval(Y,Y1,M), max(X1, Y1, R).
eval(gcd(X,Y), R, M) :- !, eval(X,X1,M), eval(Y,Y1,M), gcd(X1, Y1, R).
eval(lcm(X,Y), R, M) :- !, eval(X,X1,M), eval(Y,Y1,M), lcm(X1, Y1, R).
eval(X /\ Y, R, M) :-	!, eval(X,X1,M), eval(Y,Y1,M), /\(X1, Y1, R).
eval(X \/ Y, R, M) :-	!, eval(X,X1,M), eval(Y,Y1,M), \/(X1, Y1, R).
eval(xor(X,Y), R, M) :- !, eval(X,X1,M), eval(Y,Y1,M), xor(X1, Y1, R).
eval(X >> Y, R, M) :-	!, eval(X,X1,M), eval(Y,Y1,M), >>(X1, Y1, R).
eval(X << Y, R, M) :-	!, eval(X,X1,M), eval(Y,Y1,M), <<(X1, Y1, R).
eval(setbit(X,Y), R, M) :- !, eval(X,X1,M), eval(Y,Y1,M), setbit(X1, Y1, R).
eval(getbit(X,Y), R, M) :- !, eval(X,X1,M), eval(Y,Y1,M), getbit(X1, Y1, R).
eval(clrbit(X,Y), R, M) :- !, eval(X,X1,M), eval(Y,Y1,M), clrbit(X1, Y1, R).
eval(sin(X), R, M) :-	!, eval(X,X1,M), sin(X1, R).
eval(cos(X), R, M) :-	!, eval(X,X1,M), cos(X1, R).
eval(tan(X), R, M) :-	!, eval(X,X1,M), tan(X1, R).
eval(atan(X,Y), R, M) :-   !, eval(X,X1,M), eval(Y,Y1,M), atan(X1, Y1, R).
eval(asin(X), R, M) :-	!, eval(X,X1,M), asin(X1, R).
eval(acos(X), R, M) :-	!, eval(X,X1,M), acos(X1, R).
eval(atan(X), R, M) :-	!, eval(X,X1,M), atan(X1, R).
eval(exp(X), R, M) :-	!, eval(X,X1,M), exp(X1, R).
eval(ln(X), R, M) :-	!, eval(X,X1,M), ln(X1, R).
eval(sqrt(X), R, M) :-	!, eval(X,X1,M), sqrt(X1, R).
eval(sum(X), R, M) :-	!, sum_body(X, R, M).
eval(min(X), R, M) :-	!, min_body(X, R, M).
eval(max(X), R, M) :-	!, max_body(X, R, M).
eval(pi, R, _) :-	!, pi(R).
eval(e, R, _) :-	!, e(R).
eval(LM:X, R, CM) :-	!, (evaluating_goal(X, R, CM, LM, Goal) ->
			    :@(LM,Goal,CM)	% same as LM:Goal@CM
			;
			    R=LM:X).
eval(X, R, M) :-	evaluating_goal(X, R, M, M, Goal) ->
			    call(Goal)@M
			;
			    R=X.

:- mode evaluating_goal(?,?,+,+,-).
evaluating_goal(X, R, CM, LM, _Goal) :-
	var(X),
	( LM == CM ->
	    error(4, (R is X), CM)	% no evaluating predicate
	;
	    error(4, (R is LM:X), CM)	% no evaluating predicate
	).
evaluating_goal(X, R, CM, LM, Goal) :-
	nonvar(X),
	functor(X, F, A),
	atom(F),			% fails for strings etc.
	+(A, 1, A1),			% because no inlining yet
	functor(Goal, F, A1),
	( is_predicate_(F/A1, LM) ->
	    unify_args(A, X, Goal),
	    arg(A1, Goal, R)
	; LM = CM ->
	    error(21, (R is X), CM)	% no evaluating predicate
	;
	    error(21, (R is LM:X), CM)	% no evaluating predicate
	).

% unify the first N arguments of two structures

:- mode unify_args(+,+,+).

unify_args(0, _, _) :- !.
unify_args(N, S1, S2) :-
	arg(N, S1, Arg),
	arg(N, S2, Arg),
	-(N, 1, N1),
	unify_args(N1, S1, S2).


%-------------------------------
% checking utilities
%-------------------------------

check_predspec(Functor, Module) :-
	check_predspec(Functor),
	( is_predicate_(Functor, Module) -> true ; set_bip_error(60) ).

check_predspec(X) :- var(X), !,
	set_bip_error(4).
check_predspec(N/A) :- !,
	check_atom(N),
	check_arity(A).
check_predspec(_) :-
	set_bip_error(5).

check_partial_predspec(X) :- var(X), !,
	set_bip_error(4).
check_partial_predspec(N/A) :- !,
	check_var_or_atom(N),
	check_var_or_arity(A).
check_partial_predspec(_) :-
	set_bip_error(5).

check_var_or_partial_predspec(X) :- var(X), !.
check_var_or_partial_predspec(X) :-
	check_partial_predspec(X).

check_var_or_partial_qual_predspec(X) :- var(X), !.
check_var_or_partial_qual_predspec(M:NA) :- !,
	check_var_or_atom(M),
	check_var_or_partial_predspec(NA).
check_var_or_partial_qual_predspec(X) :-
	check_partial_predspec(X).

check_var_or_partial_macro_spec(X) :- var(X), !.
check_var_or_partial_macro_spec(type(Type)) :- !,
	check_var_or_type(Type).
check_var_or_partial_macro_spec(X) :-
	check_partial_predspec(X).

check_var_or_atom(X) :- var(X), !.
check_var_or_atom(X) :- check_atom(X).

check_var_or_integer(X) :- var(X), !.
check_var_or_integer(X) :- integer(X), !.
check_var_or_integer(_) :- set_bip_error(5).

check_var_or_atomic(X) :- var(X), !.
check_var_or_atomic(X) :- atomic(X), !.
check_var_or_atomic(_) :- set_bip_error(5).

check_var_or_arity(A) :- var(A), !.
check_var_or_arity(A) :- check_arity(A).

check_atom(X) :- var(X), !, set_bip_error(4).
check_atom(X) :- atom(X), !.
check_atom(_) :- set_bip_error(5).

check_functor(X,_,_) :- var(X), !, set_bip_error(4).
check_functor(X,N,A) :- functor(X,N,A), !.
check_functor(_,_,_) :- set_bip_error(5).

check_fieldspecs(X) :- var(X), !, set_bip_error(4).
check_fieldspecs(N:_) :- atom(N), !.
check_fieldspecs([N:_|More]) :- -?-> atom(N), !, check_fieldspecs(More).
check_fieldspecs([]) :- !.
check_fieldspecs(_) :- set_bip_error(5).

check_nonvar(X) :- var(X), !, set_bip_error(4).
check_nonvar(_).

check_var(X) :- var(X), !.
check_var(_) :- set_bip_error(5).

check_integer(X) :- var(X), !, set_bip_error(4).
check_integer(X) :- integer(X), !.
check_integer(X) :- number(X), !, set_bip_error(5).
check_integer(_) :- set_bip_error(24).

check_arity(A) :- check_integer_ge(A, 0).

check_integer_ge(A, _) :- var(A), !, set_bip_error(4).
check_integer_ge(A, Min) :- integer(A), !, ( A>=Min -> true ; set_bip_error(6) ).
check_integer_ge(_, _) :- set_bip_error(5).

check_string(X) :- var(X), !, set_bip_error(4).
check_string(X) :- string(X), !.
check_string(_) :- set_bip_error(5).

check_var_or_atom_string(X) :- var(X), !.
check_var_or_atom_string(X) :- check_atom_string(X).

check_atom_string(X) :- var(X), !, set_bip_error(4).
check_atom_string(X) :- atom(X), !.
check_atom_string(X) :- string(X), !.
check_atom_string(_) :- set_bip_error(5).

% basic_atomic excludes `atomic' types such as handles and suspensions
check_basic_atomic(X) :- var(X), !, set_bip_error(4).
check_basic_atomic(X) :- atom(X), !.
check_basic_atomic(X) :- string(X), !.
check_basic_atomic(X) :- number(X), !.
check_basic_atomic(_) :- set_bip_error(5).

check_var_or_string(X) :- var(X), !.
check_var_or_string(X) :- check_string(X).

check_compound(X) :- var(X), !, set_bip_error(4).
check_compound(X) :- compound(X), !.
check_compound(_) :- set_bip_error(5).

check_callable(X) :- var(X), !, set_bip_error(4).
check_callable(X) :- callable(X), !.
check_callable(_) :- set_bip_error(5).

check_var_or_type(X) :- var(X), !.
check_var_or_type(X) :-
	check_atom(X),
	( current_type(X) -> true ; set_bip_error(6) ).

check_module(X) :-
	check_atom(X),
	( is_a_module(X) -> true ; set_bip_error(80) ).

check_var_or_stream_spec(X) :- var(X), !.
check_var_or_stream_spec(X) :- check_stream_spec(X).

check_var_or_partial_list(X) :- var(X), !.
check_var_or_partial_list([]) :- !.
check_var_or_partial_list([_|T]) :- !,
	check_var_or_partial_list(T).
check_var_or_partial_list(_) :-
	set_bip_error(5).

check_proper_list(Xs) :-
	list_end(Xs, T),
	( var(T) -> set_bip_error(4)
	; T==[] -> true
	; set_bip_error(5)
	).

:- mode illegal_module(?, -).
illegal_module(Module, 4) :-
	var(Module).
illegal_module(Module, 5) :-
	nonvar(Module),
	\+atom(Module).

% illegal_or_nonexisting_module
:- mode illegal_existing_module(?, -).
illegal_existing_module(Module, 4) :-
	var(Module).
illegal_existing_module(Module, 5) :-
	nonvar(Module),
	not atom(Module).
illegal_existing_module(Module, 80) :-
	atom(Module),
	\+is_a_module(Module).

% illegal_or_nonexisting_or_locked_module
:- mode illegal_unlocked_module(?, -).
illegal_unlocked_module(Module, 4) :-
	var(Module).
illegal_unlocked_module(Module, 5) :-
	nonvar(Module),
	not atom(Module).
illegal_unlocked_module(Module, 80) :-
	atom(Module),
	\+is_a_module(Module).
illegal_unlocked_module(Module, 82) :-
	atom(Module),
	\+authorized_module(Module).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% the local declaration
%


:- tool((local)/1, local_body/2).

local_body(X, M) :-
	var(X), !,
	error(4, local(X), M).
local_body((X,Y), M):- !,
	local_body(X, M),
	local_body(Y, M).
local_body(domain(S), M) :-
	define_domain(S, M, local), !.
local_body(record(Key), M) :- !,
	local_record_body(Key, M).
local_body(store(Key), M) :-
	store_create_named_(Key, M), !.
local_body(shelf(Name,Init), M) :-
	check_compound(Init),
	shelf_create(Init, Handle),
	shelf_name(Name, Handle, M), !.
local_body(struct(S), M) :-
	define_struct(S, M, local), !.
local_body(reference(Name,Init), M) :-
	makeref_(Name, Init, M), !.
local_body(reference(Name), M) :-
	makeref_(Name, 0, M), !.	% backward compatibility
local_body(variable(Name), M) :-
	check_atom(Name),
	make_array_(Name, prolog, local, M), !.
local_body(variable(Name,Init), M) :-
	check_atom(Name),
	make_array_(Name, prolog, local, M), !,
	setval(Name, Init)@M.
local_body(array(Name), M) :-
	check_compound(Name),
	make_array_(Name, prolog, local, M), !.
local_body(array(Name,Type), M) :-
	check_compound(Name),
	make_array_(Name, Type, local, M), !.
local_body(op(Pred,Assoc,Name), M) :-
       local_op_body(Pred, Assoc, Name, M), !.
local_body(macro(Functor,Trans,Options), M) :- !,
	define_macro_(Functor, Trans, [local|Options], M).
local_body(portray(Functor,Trans,Options), M) :- !,
	define_macro_(Functor, Trans, [local,write|Options], M).
local_body(chtab(Char,Class), M) :- !,
	set_chtab_(Char, Class, M).
local_body(syntax_option(Option), M) :- !,
	set_flag_body(syntax_option, Option, M).
local_body(initialization(Goal), M) :-
	store_goals(initialization_goals, Goal, M), !.
local_body(finalization(Goal), M) :-
	store_goals(finalization_goals, Goal, M), !.
local_body(X, M) :- X = _/_,
	local_(X, M), !.
local_body(X, _M) :-
	\+ valid_local_spec(X),
	set_bip_error(5).
local_body(X, M) :-
	bip_error(local(X), M).

    :- mode valid_local_spec(+).
    valid_local_spec(domain(_)).
    valid_local_spec(record(_)).
    valid_local_spec(shelf(_,_)).
    valid_local_spec(store(_)).
    valid_local_spec(struct(_)).
    valid_local_spec(reference(_)).
    valid_local_spec(reference(_,_)).
    valid_local_spec(variable(_)).
    valid_local_spec(variable(_,_)).
    valid_local_spec(array(_)).
    valid_local_spec(array(_,_)).
    valid_local_spec(op(_,_,_)).
    valid_local_spec(macro(_,_,_)).
    valid_local_spec(portray(_,_,_)).
    valid_local_spec(chtab(_,_)).
    valid_local_spec(syntax_option(_)).
    valid_local_spec(initialization(_)).
    valid_local_spec(_/_).

%
% the global declaration
%

:- tool((global)/1, global_body/2).

global_body(X, M) :- var(X), !,
	error(4, global(X), M).
global_body((X,Y), M):- !,
	global_body(X, M),
	global_body(Y, M).
global_body(X, M):-
	valid_global_spec(X), !,
	record_interface_directive(global(X), M),
	global_item(X, M).
global_body(X, M) :-
	error(5, global(X), M).

global_item(record(Key), M) :- !,
	global_record_body(Key, M).
global_item(struct(S), M) :-
	define_struct(S, M, export), !.
global_item(reference(_), _) :-
	set_bip_error(141).
global_item(variable(Name), M) :-
	( atom(Name) -> true ; var(Name) -> set_bip_error(4) ; set_bip_error(5) ),
	make_array_(Name, prolog, global, M), !.
global_item(array(Name), M) :-
	make_array_(Name, prolog, global, M), !.
global_item(array(Name,Type), M) :-
	make_array_(Name, Type, global, M), !.
global_item(op(Pred,Assoc,Name), M) :-
       global_op_body(Pred, Assoc, Name, M), !.
global_item(macro(Functor,Trans,Options), M) :- !,
	define_macro_(Functor, Trans, [global|Options], M).
global_item(portray(Functor,Trans,Options), M) :- !,
	define_macro_(Functor, Trans, [global,write|Options], M).
global_item(meta_attribute(Name,Handlers), M) :- !,
	meta_attribute_body(Name, Handlers, M).
global_item(X, M) :- X = _/_,
	printf(warning_output, "WARNING: Global predicates no longer supported%n", []),
	printf(warning_output, "    (using export instead): %w%n", [global(X)@M]),
	export_(X, M), !.
global_item(X, M) :-
	bip_error(global(X), M).

    :- mode valid_global_spec(+).
    valid_global_spec(record(_)).
    valid_global_spec(struct(_)).
    valid_global_spec(reference(_)).
    valid_global_spec(variable(_)).
    valid_global_spec(array(_)).
    valid_global_spec(array(_,_)).
    valid_global_spec(op(_,_,_)).
    valid_global_spec(macro(_,_,_)).
    valid_global_spec(portray(_,_,_)).
    valid_global_spec(meta_attribute(_,_)).
    valid_global_spec(_/_).

%
% the export declaration
%

:- tool((export)/1, export_body/2).

export_body(X, M) :- var(X), !,
	error(4, export(X), M).
export_body((X,Y), M):- !,
	export_body(X, M),
	export_body(Y, M).
export_body(X, M):-
	valid_export_spec(X), !,
	record_interface_directive(export(X), M),
	export_item(X, M).
export_body(X, M) :-
	error(5, export(X), M).

export_list(X, M) :- var(X), !,
	error(4, export(X), M).
export_list([], _M) :- !.
export_list([X|Xs], M):- !,
	( valid_export_spec(X) ->
	    record_interface_directive(export(X), M),
	    export_item(X, M),
	    export_list(Xs, M)
	;
	    error(5, export(X), M)
	).
export_list(X, M) :-
	error(5, export(X), M).

export_item(domain(S), M) :-
	define_domain(S, M, export), !.
export_item(struct(S), M) :-
	define_struct(S, M, export), !.
export_item(op(Pred,Assoc,Name), M) :-
       local_op_body(Pred, Assoc, Name, M), !.
export_item(macro(Functor,Trans,Options), M) :- !,
	define_macro_(Functor, Trans, [local|Options], M).
export_item(portray(Functor,Trans,Options), M) :- !,
	define_macro_(Functor, Trans, [local,write|Options], M).
export_item(chtab(Char,Class), M) :- !,
	set_chtab_(Char, Class, M).
export_item(syntax_option(Option), M) :- !,
	set_flag_body(syntax_option, Option, M).
export_item(initialization(_Goal), _M) :- !.
	% Not called, since typically it is not desirable to call
	% the same goal for local and import initialization.
export_item(X, M) :- X = _/_,
	export_(X, M), !.
export_item(X, M) :-
	bip_error(export(X), M).

    valid_export_spec(X) :- var(X), !, fail.
    valid_export_spec(domain(_)).
    valid_export_spec(struct(_)).
    valid_export_spec(op(_,_,_)).
    valid_export_spec(macro(_,_,_)).
    valid_export_spec(portray(_,_,_)).
    valid_export_spec(chtab(_,_)).
    valid_export_spec(syntax_option(_)).
    valid_export_spec(initialization(_)).
    valid_export_spec(_/_).


% import_exported/3 is applied to export-declarations in module interfaces

import_exported(X, Mi, M) :-
	var(X), !,
	error(4, import(from(X, Mi)), M).
import_exported(domain(S), Mi, M) :-
	import_domain(S, Mi, M), !.
import_exported(struct(S), Mi, M) :-
	import_struct(S, Mi, M), !.
import_exported(op(Pred,Assoc,Name), _Mi, M) :-
	local_op_body(Pred, Assoc, Name, M), !.
import_exported(macro(Functor,Trans,Options), _Mi, M) :-
	define_macro_(Functor, Trans, [local|Options], M).
import_exported(portray(Functor,Trans,Options), _Mi, M) :-
	define_macro_(Functor, Trans, [local,write|Options], M).
import_exported(chtab(Char,Class), _Mi, M) :- !,
	set_chtab_(Char, Class, M).
import_exported(syntax_option(Option), _Mi, M) :- !,
	set_flag_body(syntax_option, Option, M).
import_exported(initialization(Goal), _Mi, M) :- !,
	run_list_of_goals([Goal], M).
import_exported(X, _Mi, _M) :- X = _/_, !.
import_exported(X, _Mi, _M) :-
	\+ valid_export_spec(X),
	set_bip_error(5).
import_exported(X, Mi, M) :-
	bip_error(import(from(X, Mi)), M).


%
% the reexport declaration
%

:- tool((reexport)/1, reexport_body/2).

reexport_body(X, M) :- var(X), !,
	error(4, reexport(X), M).
reexport_body(Things from Module, M) :-
	record_interface_directive(reexport(Things from Module), M),
	check_module_or_load_library(Module, M),
	reexport_only(Module, M, Things),
	!.
reexport_body(Module except Except, M) :-
	record_interface_directive(reexport(Module except Except), M),
	check_module_or_load_library(Module, M),
	reexport_except(Module, M, Except),
	!.
reexport_body(Module, M):-
	Module \= (_ except _),
	Module \= (_ from _),
	record_interface_directive(reexport(Module), M),
	check_module_or_load_library(Module, M),
	reexport_all(Module, M),
	!.
reexport_body(Any, M):-
	bip_error(reexport(Any), M).

    reexport_only(Module, Where, Things) :-	
	split_export_list(Things, Preds, [], Other, []),
	(
	    member(Pred, Preds),
	    ( reexport_from_(Module, Pred, Where) -> 
		fail ; !, fail % error as pred. list is explicit
	    )
	;
	    interface_closure_nopreds_only(Module, Other, [Module], Goal),
	    ( import_interface_directive(Goal, Module, Where) -> 
		fail ; !, fail
	    )
	;
	    true
	).


    reexport_except(Module, Where, Except) :-	
	split_export_list(Except, Preds, [], Other, []),
	(
	    interface_closure_preds_except(Module, Preds, [Module], (export Pred)),
	    ( reexport_from_(Module, Pred, Where) -> 
		fail ; reexport_error_warning(Module, Pred, Where), fail
	    )
	;
	    interface_closure_nopreds_except(Module, Other, [Module], Goal),
	    ( import_interface_directive(Goal, Module, Where) -> 
		fail ; reexport_error_warning(Module, Goal, Where), fail  
	    )
	;
	    true
	).


    reexport_all(Module, Where) :-		
	(
	    interface_closure_preds(Module, [Module], (export Pred)),
	    ( reexport_from_(Module, Pred, Where) -> 
		fail ; reexport_error_warning(Module, Pred, Where), fail 
	    )
	;
	    interface_closure_nopreds(Module, [Module], Goal),
	    ( import_interface_directive(Goal, Module, Where) -> 
		fail ; reexport_error_warning(Module, Goal, Where), fail 
	    )
	;
	    true
	).

    reexport_error_warning(Module, Pred, Where) :-
	get_bip_error(ErrorId),
	error_id(ErrorId, ErrorMsg),
	write(warning_output, "WARNING: "),
	write(warning_output, ErrorMsg), 
	write(warning_output, " in reexport "),
	write(warning_output, Pred)@Where,
	write(warning_output, " from "),
	write(warning_output, Module),
	write(warning_output, " in module "),
	write(warning_output, Where),
	nl(warning_output).

%
% the import declaration 
%

:- tool((import)/1, import_body/2).

import_body(X, M) :-
	var(X), !,
	error(4, import(X), M).
import_body(from(X, Mi), M) :- !,
	import_from_body(Mi, X, M).
import_body(X, M):-
	import_module_list(X, M).

    import_module_list(X, M) :- var(X), !,
	error(4, import(X), M).
    import_module_list([], _M) :- !.
    import_module_list([X|Xs], M) :- !,
	import_module_body(X, M),
	import_module_list(Xs, M).
    import_module_list(X, M) :-
	import_module_body(X, M).

    import_module_body(LibMod, M) :-
	( check_module_or_load_library(LibMod, M) ->
	    ( LibMod == M ->
		true				% don't import into yourself
	    ; import_(LibMod, M), import_interface(LibMod, M) ->
		true
	    ;
		bip_error(import(LibMod), M)
	    )
	;
	    bip_error(import(LibMod), M)
	).

    import_from_body(Mi, (X, Y), M) :- -?-> !,
	import_from_body(Mi, X, M),
	import_from_body(Mi, Y, M).
    import_from_body(Mi, X, M) :-
	( import_from_(Mi, X, M) ->
	    true
	;
	    bip_error(import(from(X, Mi)), M)
	).



%
% Various predicate property declarations
% They all implicitly create the predicate if it doesn't exist
%

:- tool((traceable)/1, traceable_body/2).
traceable_body(PredSpec, Module) :-
	declaration(PredSpec, leash, stop, Module), !.
traceable_body(PredSpec, Module) :-
	bip_error(traceable(PredSpec), Module).

:- tool((untraceable)/1, untraceable_body/2).
untraceable_body(PredSpec, Module) :-
	declaration(PredSpec, leash, notrace, Module), !.
untraceable_body(PredSpec, Module) :-
	bip_error(untraceable(PredSpec), Module).

:- tool((skipped)/1, skipped_body/2).
skipped_body(PredSpec, Module) :-
	declaration(PredSpec, skip, on, Module), !.
skipped_body(PredSpec, Module) :-
	bip_error(skipped(PredSpec), Module).

:- tool((unskipped)/1, unskipped_body/2).
unskipped_body(PredSpec, Module) :-
	declaration(PredSpec, skip, off, Module), !.
unskipped_body(PredSpec, Module) :-
	bip_error(unskipped(PredSpec), Module).

:- tool((parallel)/1, parallel_body/2).
parallel_body(PredSpec, Module) :-
	declaration(PredSpec, parallel, on, Module), !.
parallel_body(PredSpec, Module) :-
	bip_error(parallel(PredSpec), Module).

:- tool((demon)/1, demon_body/2).
demon_body(PredSpec, Module) :-
	declaration(PredSpec, demon, on, Module), !.
demon_body(PredSpec, Module) :-
	bip_error(demon(PredSpec), Module).

% comment declares the predicate so you get
% a warning if you don't define it
:- tool(comment/2, comment_body/3).
comment_body(N/A, C, Module) :- -?-> !,
	(
	    check_predspec(N/A),
	    ( get_flag_body(N/A, visibility, _Any, Module) ->
		true	% already declared
	    ;
		local_(N/A, Module)
	    )
	->
	    true
	;
	    bip_error(comment(N/A, C), Module)
	).
comment_body(_,_,_).


    declaration(PredSpec, _Flag, _Value, _Module) :-
	var(PredSpec), !,
	set_bip_error(4).
    declaration((A,B), Flag, Value, Module) :- !,
	declaration(A, Flag, Value, Module),
	declaration(B, Flag, Value, Module).
    declaration(PredSpec, Flag, Value, M) :-
	check_predspec(PredSpec),
	( get_flag_body(PredSpec, definition_module, M, M) ->
	    true
	;
	    local_(PredSpec, M)			% may fail with bip_error
	),
	set_proc_flags(PredSpec, Flag, Value, M). % may fail with bip_error


%
% deprecated/2 declaration
%

?- store_create_named(deprecation_advice).

:- export deprecated/2.
:- tool(deprecated/2, deprecated_body/3).
deprecated_body(PredSpec, Advice, Module) :-
	check_predspec(PredSpec),
	check_string(Advice),
	( get_flag_body(PredSpec, definition_module, Module, Module) ->
	    true	% already declared
	;
	    local_(PredSpec, Module)
	),
	!,
	set_flag_body(PredSpec, deprecated, on, Module),
	store_set(deprecation_advice, Module:PredSpec, Advice).
deprecated_body(PredSpec, Advice, Module) :-
	bip_error(deprecated(PredSpec, Advice), Module).


get_deprecation_advice(PredSpec, Module, Advice) :-
	store_get(deprecation_advice, Module:PredSpec, Advice).


erase_deprecation_advice(Module) :-
	store_erase_qualified(deprecation_advice, Module).


%
% get_flag/3
%

get_flag_body(Proc, Flag, Value, Module) :-
	check_predspec(Proc),
	check_var_or_atom(Flag),
	%check_var_or_flag_value(Flag),
	!,
	pri_flag_code(Flag, Code),
	( integer(Code),
	    proc_flags(Proc, Code, Value, Module)
	; atom(Code),
	    proc_flags(Proc, 0/*definition_module*/, DM, Module),
	    store_get(Code, DM:Proc, Value)
	).
get_flag_body(Proc, Flag, Value, Module) :-
	bip_error(get_flag(Proc, Flag, Value), Module).

proc_flags(P, C, V, M) :-
	local_proc_flags(P, C, V, M, G),
	G = global.


% The numbers here have to match those in local_proc_flags/5 in bip_db.c

pri_flag_code(mode,		 6).	% name and visibility
pri_flag_code(meta_predicate,	meta_predicate).
pri_flag_code(visibility,	23).
pri_flag_code(definition_module, 0).
pri_flag_code(declared,		12).
pri_flag_code(defined,		14).

pri_flag_code(autoload,		13).	% various flags, alphabetic
pri_flag_code(auxiliary,	9).
pri_flag_code(call_type,	10).
pri_flag_code(demon,		25).
pri_flag_code(deprecated,	16).
pri_flag_code(inline,		 8).
pri_flag_code(invisible,	27).
pri_flag_code(parallel,		26).
pri_flag_code(priority,		24).
pri_flag_code(run_priority,	34).
pri_flag_code(stability,	20).
pri_flag_code(tool,		21).
pri_flag_code(type,		22).

pri_flag_code(debugged,		11).	% debugging-related, almost alphabetic
pri_flag_code(leash,		15).
pri_flag_code(skip,		17).
pri_flag_code(spy,		18).
pri_flag_code(start_tracing,	19).
pri_flag_code(source_file,	 3).
pri_flag_code(source_line,	 4).
pri_flag_code(source_offset,	 5).
pri_flag_code(port_calls,	32).
pri_flag_code(port_lines,	31).
pri_flag_code(break_lines,	30).

pri_flag_code(code_size,	29).	% statistics


check_var_or_flag_value(X) :- var(X), !.
check_var_or_flag_value(X) :- integer(X), !.
check_var_or_flag_value(X) :- atom(X), !.
check_var_or_flag_value(X) :- compound(X), !.
check_var_or_flag_value(_) :- set_bip_error(5).


%
% set_flag/3
%

set_flag_body([], _Name, _Value, _Module) :- !.
set_flag_body([Proc|Procs], Name, Value, Module) :-
	!,
	set_flag_body(Proc, Name, Value, Module),
	set_flag_body(Procs, Name, Value, Module).
set_flag_body(Proc, Name, Value, Module) :-
	(do_set_flag(Proc, Name, Value, Module) ->
	    true
	;
	    bip_error(set_flag(Proc, Name,Value), Module)
	).

do_set_flag(_, Flag, _, _) :- var(Flag),	!, set_bip_error(4).
do_set_flag(_, definition_module, _, _) :-	!, set_bip_error(30). %readonly
do_set_flag(_, visibility, _, _) :-		!, set_bip_error(30).
do_set_flag(_, tool, _, _) :-			!, set_bip_error(30).
do_set_flag(_, call_type, _, _) :-		!, set_bip_error(30).
do_set_flag(_, mode, _, _) :-			!, set_bip_error(30).
do_set_flag(_, debugged, _, _) :-		!, set_bip_error(30).
do_set_flag(_, defined, _, _) :-		!, set_bip_error(30).
do_set_flag(_, declared, _, _) :-		!, set_bip_error(30).
do_set_flag(_, type, user, _) :-		!, set_bip_error(30). % allow setting to built_in
do_set_flag(_, invisible, _, Module) :-
	Module \== sepia_kernel, !,
	set_bip_error(30).
do_set_flag(_, debug, _, _) :- !,
	set_bip_error(6).		% to protect set_proc_flags/4 below
do_set_flag(_, system, _, _) :- !,
	set_bip_error(6).		% to protect set_proc_flags/4 below
do_set_flag(_, break, _, _) :- !,
	set_bip_error(6).		% to protect set_proc_flags/4 below
do_set_flag(Proc, inline, Trans, Module) :- !,
	define_macro_(Proc, Trans, [goal], Module).
do_set_flag(Proc, Flag, Value, Module) :-
	set_proc_flags(Proc, Flag, Value, Module).



/****** Tool declarations *******/

:-	
	tool(abolish_record/1, abolish_record_body/2),
	tool((:)/2, '[]:@'/3),
	tool(call_boxed/5, call_boxed_/6),
	tool(call_boxed/6, call_boxed_/7),
	tool(call_explicit/2, call_explicit_body/3),
	tool('.'/2, compile_list_body/3),
	tool(erase_array/1, erase_array_body/2),
	tool(erase_macro/1, erase_macro_/2),
	tool(erase_macro/2, erase_macro_/3),
	tool(eval/2, eval/3),
	tool(exec_string/2, exec_string/3),
	tool(exec_exdr/1, exec_exdr/2),
	tool(external/2, external_/3),
	tool(expand_clause/2, expand_clause_/3),
	tool(expand_goal/2, expand_goal/3),
	tool(expand_goal_annotated/4, expand_goal_annotated_/5),
	tool(expand_macros/2, expand_macros_/3),
	tool(expand_macros_annotated/4, expand_macros_annotated_/5),
	tool(expand_clause_annotated/4, expand_clause_annotated_/5),
	tool(b_external/2, b_external_/3),
	tool(external/1, external_body/2),
	tool(b_external/1, b_external_body/2),
	tool(inline/2, inline_/3),
	tool(inline/1, inline_/2),
	tool(insert_suspension/3, insert_suspension/4),
	tool(add_attribute/2, add_attribute/3),
	tool(get_attribute/2, get_attribute/3),
	tool(get_attributes/3, get_attributes/4),
	tool(replace_attribute/2, replace_attribute/3),
	tool(tool_body/3, tool_body_/4),
	tool(lib/1, lib_/2),
	tool(make_suspension/3, make_suspension/4),
	tool(current_module_predicate/2, current_module_predicate/3),
	tool(remote_connect/3, remote_connect/4),
	tool(remote_connect_accept/6, remote_connect_accept/7),
	tool(print/1, print_/2),
	tool(print/2, print_/3),
	tool(read_token/3, read_token_/4),
	tool(set_proc_flags/3, set_proc_flags/4),
	tool(subscript/3, subscript/4).


/****** export declarations *******/


:- export				% undocumented exports
	record_discontiguous_predicate/4,
	record_inline_source/4,
	collect_discontiguous_predicates/2,
	valid_signature/2,
	printf_with_current_modes/2,
	proc_flags/4,
	sepia_version_banner/2,
	tr_match/4,
	trprotect/2,
	trdcg/5,
	call_local/1,
	check_callable/1,
	check_predspec/1,
	erase_module_pragmas/1,
	exec_exdr/1,
	exec_string/2,
	expand_clause_annotated/4,
	expand_goal_annotated/4,
	expand_macros_annotated/4,
	extension/1,
	replace_attribute/2,
	rpc_server_init/0,
	get_pager/1,
	illegal_macro/5,
	more/1,
	prof_predicate_list/3,
	tr_goals/3.

:- export				% exports for lib(lists)
	append/3,
	delete/3,
	length/2,
	member/2,
	memberchk/2,
	nonmember/2,
	subtract/3,
	reverse/2.

:- export				% built-ins
	(@)/2, 
	(:)/2, 
	(*->)/2, 
	'.'/2, 
	(\=)/2, 
	!/0,
	(\+)/1,
        (?-)/2,
        (-->)/2,
        abort/0, 
	abolish_record/1,
	add_attribute/2,
	add_attribute/3,
	autoload/2,
	autoload_tool/2,
	autoload_system/2,
	b_external/1,
	b_external/2,
	%between/3,
	between/4,
	block/3,
	block_atomic/3,
	bytes_to_term/2,
	call/1,
	call/2,
	call_boxed/5,
	call_boxed/6,
	call_explicit/2,
	call_priority/2,
	cancel_after_event/1,
	cancel_after_event/2,
	canonical_path_name/2,
	close_embed_queue_eclipseside/2,
	comment/2,
	compiled_stream/1,
	coroutine/0,
	create_module/1,
	create_module/3,
	current_error/1,
	current_pragma/1,
	current_after_event/1,
	current_after_events/1,
	current_interrupt/2,
	current_record/1,
	current_suspension/1,
	debug/1,
	decval/1,
	define_macro/3,
	(delay)/1,
	(demon)/1,
	discontiguous/1,
	display/1,
	e/1,
	ecl_create_embed_queue/3,
	engine_join/2,
	ensure_loaded/1,
	error/2, 
	error/3,
	erase/2,
	erase_all/1,
	erase_all/2,
	erase_array/1,
	erase_macro/1,
	erase_macro/2,
	erase_module/1,
	event/1,
	exit/1,
	exists/1,
	existing_file/4,
	expand_clause/2,
	expand_goal/2,
	expand_macros/2,
	(export)/1,
	external/1,
	external/2,
	eval/2,
	event_after/2,
	event_after/3,
	event_after_every/2,
	events_after/1,
	event_create/2,
	event_retrieve/2,
	event_retrieve/3,
	fail_if/1, 
	false/0,
	flatten_array/2,
	get_attribute/2,
	get_char/1,
	get_chtab/2,
	get_error_handler/3, 
	get_event_handler/3, 
	get_flag/3,
	get_interrupt_handler/3, 
	get_module_info/3, 
%	get_statistics/2,
	getval/2,
	(global)/1,
%	set_statistics/2,
	halt/0, 
	(help)/0, 
	(import)/1,
	incval/1,
	insert_suspension/3,
	inline/1,
	inline/2,
	(is)/2,
	is_predicate/1,
	kill_suspension/1,
	lib/1,
	lib/2,
	load_eco/2,
	(local)/1,
	local_record/1,
	lock/0,
	lock_pass/1,
	make_suspension/3,
	make_suspension/4,
	(mode)/1,
	module/1, 
	mutex/2,
	with_mutex/2,
	mutex_init/1,
	name_to_handle/3,
	nl/0,
	new_socket_server/3,
	(not)/1,
	(once)/1,
	(parallel)/1,
%	par_all/2,
%	par_findall/4,
%	par_once/2,
	pathname/2,
	pathname/4,
	pi/1,
	print/1,
	print/2,
	printf/2,
	printf/3,
	sprintf/3,
	put_char/1,
	read/1,
	read/2,
	read_token/2,
	readvar/3,
	recorda/2,
	recorda/3,
	recorded/2,
	recorded/3,
	recordedchk/2,
	recordedchk/3,
	recorded_list/2,
	record/2,
	recordz/2,
	recordz/3,
	rerecord/2,
	(reexport)/1,
	reset_error_handlers/0,
	read_token/3,
	remote_yield/1,
	remote_connect/3,
	remote_connect_setup/3,
	remote_connect_accept/6,
	remote_disconnect/1,
	set_chtab/2,
	set_default_error_handler/2,
	set_flag/3,
	set_embed_peer/2,
	set_error_handler/2, 
	set_event_handler/2, 
	set_interrupt_handler/2, 
	setval/2,
	stack_overflow_message/1,
	standalone_toplevel/0,
	subcall/2,
	subscript/3,
	(skipped)/1,
	term_to_bytes/2,
	test_and_setval/3,
	(tool)/1,
	(tool)/2,
	tool_body/3,
	trace/1,
	(traceable)/1,
	tyi/1,
	tyo/1,
	(unskipped)/1,
	(untraceable)/1,
	use_module/1,
	wait/2,
	wait/3,
	write/1,
	write/2,
	write_canonical/1,
	write_canonical/2,
	writeln/1,
	writeln/2,
	writeq/1,
	writeq/2,
	yield/2.


/******making the built-in procedures invisible to the debugger*******/

:- untraceable
	(.)/2,
	(',')/2,
	(;)/2,
	(->)/2,
	':'/2,
	'[]:@'/3,
	',_body'/3,
	';_body'/3,
	'->_body'/3,
	bip_error/1,
	bip_error/2,
	block/4,
	block_atomic/4,
	compile_list_body/3, 
	create_module_if_did_not_exist/2,
	dbgcomp/0,
	ensure_loaded/2,
	eval/3,
	evaluating_goal/5,
	fail_if_body/2,
	get_bip_error/1,
	get_file/3,
%	get_statistics/2, 
	(help)/0,
	insert_suspension/4,	% to hide it in delay clauses
	lib/1,
	set_bip_error/1,
%	set_statistics/2, 
	make_suspension/3,	% to hide it in delay clauses
	make_suspension/4,
	new_delays/2,
%	subcall_init/0,
%	subcall_fini/1,
	nodbgcomp/0,
	once_body/2,
%	print_statistics/0,
	(skipped)/1,
	syserror/4,
	(traceable)/1,
	debug_body/2,
	trace_body/2,
	trans_term/2,
	(unskipped)/1,
	(untraceable)/1,
	untraced_block/3,
	untraced_call/2,
	untraced_true/0,
	valid_error/1.

% dbgcomp procedures and tools must be made skipped explicitly

:- skipped
	(.)/2,
	(export)/1,
	(global)/1,
	(import)/1,
	(local)/1,
	(skipped)/1,
	(traceable)/1,
	(unskipped)/1,
	(untraceable)/1,
	abort/0,
	canonical_path_name/2,
	coroutine/0,
	current_interrupt/2,
	display/1,
	ensure_loaded/1,
	ensure_loaded/2,
	erase_array/1,
	erase_module/1,
	evaluating_goal/5,
	existing_file/4,
	exit/1,
	extension/1,
	false/0,
	get_char/1,
	get_error_handler/3, 
	get_event_handler/3, 
	get_file/3,
	get_flag/3,
	get_interrupt_handler/3, 
	halt/0,
	lib/1,
	lib/2,
	make/0,
	nl/0,
	(demon)/1,
	(parallel)/1,
	pathname/2,
	printf/2,
	printf/3,
	printf_goal_body/3,
	printf_goal_body/4,
	sprintf/3,
	proc_flags/4,
	put_char/1,
	read_token/2,
	reset_error_handler/1,
	reset_error_handlers/0,
	sepia_version_banner/2,
	set_default_error_handler/2, 
	set_error_handler/2, 
	set_interrupt_handler/2, 
	tyi/1,
	tyo/1,
	use_module/1,
	wait/2,
	wait/3,
	writeln/1,
	writeln/2.

:- traceable
	(is)/2,		% because it inherits untraceable from is_body/3
	use_module/1.

:- unskipped
	',_body'/3,
	';_body'/3,
	'->_body'/3.


:- set_flag([trace_body/2,debug_body/2], start_tracing, on).
:- set_flag(make_suspension/3, invisible, on).

:- set_flag(subcall/3, trace_meta, on).
:- set_flag(call_local/2, trace_meta, on).
:- set_flag(fail_if_body/2, trace_meta, on).
:- set_flag((not)/1, trace_meta, on).
:- set_flag((\+)/1, trace_meta, on).
:- set_flag(once_body/2, trace_meta, on).
:- set_flag(call_priority/3, trace_meta, on).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Profile support
%
%	Flags: 1		simples, not only prolog
%	Flags: 2		all, even locals, no substitution
%
% creates a list of
%	pred(StartAddress,	start of wam code
%		Index,		variable for normal preds
%				or index for module/replacement pred
%		Pred,		Name/Arity or ' '
%		Module)
%
?- make_array_(profile_module, prolog, local, sepia_kernel).
prof_predicate_list(Flags, Preds, Fixed) :-
    prof_fixed_entries(F),
    setval(profile_module, F),
    findall(pred(Start, I, P, M), prof_predicate(Flags, P, M, Start, I), Preds),
    getval(profile_module, Fixed).

prof_predicate(Flags, Pred, Module, Start, I) :-
    P = N/A,
    current_module(Module),
%   getval(profile_module, J),
    incval(profile_module),
    current_functor(N, A, 2, 0),	% functors with predicates only
    local_proc_flags(P, 0, Module, Module, Private),	% definition_module
    local_proc_flags(P, 14, on, Module, _Private),		% defined
    local_proc_flags(P, 1, ProcFlags, Module, _Private),	% flags
    (ProcFlags /\ 16'00000300 =:= 16'00000200 ->	% CODETYPE==VMCODE
	true
    ;
	Flags /\ 1 =:= 1
    ),
    local_proc_flags(P, 7, Start, Module, _),
    % If N/A is local to a locked Module, and the 'all'-flag is not given,
    % then try to map it to a more useful exported predicate name (using table).
    ( Private=local, Flags/\2 =:= 0, prof_replace_pred(N, A, Module, Pred, I) ->
    	true
    ;
	Pred = N/A
    ).

% prof_replace_pred(Name, Arity, Module, NewPred, Index)
:- mode prof_replace_pred(++, ++, ++, -, -).

prof_replace_pred(free_variables,	4, sepia_kernel, bagof_body/4,	0) :- !.
prof_replace_pred(free_variables,	5, sepia_kernel, bagof_body/4,	0) :- !.
prof_replace_pred(collect_instances,	4, sepia_kernel, bagof_body/4,	0) :- !.
prof_replace_pred(make_key,		3, sepia_kernel, bagof_body/4,	0) :- !.
prof_replace_pred(eval,			3, sepia_kernel, arithmetic,	1) :- !.
prof_replace_pred(compare_handler,	4, sepia_kernel, arithmetic,	1) :- !.
prof_replace_pred(evaluating_goal,	5, sepia_kernel, arithmetic,	1) :- !.
prof_replace_pred(recordz_instances,	3, sepia_kernel, all_solutions, 2) :- !.
prof_replace_pred(chk_nmbr_lst,		2, sepia_kernel, name/2,	3) :- !.
prof_replace_pred(susps_to_goals,	2, sepia_kernel, delayed_goals/2,4):- !.
prof_replace_pred(collect_goals,	3, sepia_kernel, coroutining,	5) :- !.
prof_replace_pred(collect_goals,	4, sepia_kernel, coroutining,	5) :- !.
prof_replace_pred(extract_goals,	4, sepia_kernel, coroutining,	5) :- !.
prof_replace_pred(wake_list,		1, sepia_kernel, coroutining,	5) :- !.
prof_replace_pred(untraced_call,	2, sepia_kernel, metacall,	6) :- !.
prof_replace_pred(call_priority,	3, sepia_kernel, metacall,	6) :- !.
prof_replace_pred((','),		4, sepia_kernel, metacall,	6) :- !.
prof_replace_pred((;),			4, sepia_kernel, metacall,	6) :- !.
prof_replace_pred((;),			5, sepia_kernel, metacall,	6) :- !.
prof_replace_pred(length1,		2, sepia_kernel, length/2,	7) :- !.
prof_replace_pred(length,		3, sepia_kernel, length/2,	7) :- !.
prof_replace_pred(member,		3, sepia_kernel, member/2,	8) :- !.
prof_replace_pred(reverse,		3, sepia_kernel, reverse/2,	9) :- !.
prof_replace_pred(subscript1,		5, sepia_kernel, subscript/3,  10) :- !.
prof_replace_pred(subscript3,		5, sepia_kernel, subscript/3,  10) :- !.
prof_replace_pred(subscript,		4, sepia_kernel, subscript/3,  10) :- !.
prof_replace_pred(forallc,		4, sepia_kernel, do/2,         11) :- !.

prof_fixed_entries(12).

:- local	% because the tool declaration has made them exported ...
	get_syntax_/3,
	set_syntax_/3,
	set_proc_flags/4.

%-----------------------------
% help
%-----------------------------

help :-
    error(231, help),
    !.
help :-
    writeln("\n\
	After the prompt [<module>]: ECLiPSe waits for a goal.\n\
	Don't forget to terminate your input with a full stop.\n\
	To type in clauses, call [user] or compile(user), and then\n\
	enter the clauses, ended by ^D (Unix) or ^Z (Windows).\n\n\
	Call help(Pred/Arity) or help(Pred) or help(String)\n\
	to get help on a specific built-in predicate."),
    get_sys_flag(13, Eclipsedir),
    printf("\n\
	To access the documentation in html-format, point your browser to\n\
	file:%s/doc/index.html\n", Eclipsedir),
    writeln("\n\
	This message can be modified by setting the handler for event 231.").


%----------------------------------------------------------------
% Operations on "optional annotated terms"
% These all allow input annotated terms to be uninstantiated,
% in which case the output is also uninstantiated.
%----------------------------------------------------------------

% Check an annotation
good_annotation(_TermIn, In) :- var(In), !.
good_annotation(Term, annotated_term(TermAnn,_,_,_,_,_)) :-
	( var(Term) -> true ; functor(Term, F, N), functor(TermAnn, F, N) ).


% Get the Ith annotated argument of AnnTerm (if any).
% If AnnTerm was uninstantiated, so is the output.
annotated_arg(_I, AnnTerm, _AnnArg) :- var(AnnTerm), !.
annotated_arg(I, annotated_term(TermAnn,_,_,_,_,_), AnnArg) :-
	arg(I, TermAnn, AnnArg).


% Get a term with annotated subterms of AnnTerm,
% or the annotated term itself (if atomic or variable).
% If AnnTerm was uninstantiated, so is the output.
annotated_match(AnnTerm, _TermAnn) :- var(AnnTerm), !.
annotated_match(annotated_term(TermAnn,_,_,_,_,_), TermAnn).


% Create one level of annotated term from its components.
% TermAnn is either: the variable or atomic term to be annotated;
% or: the compound term with annotated subterms (which may be
% still uninstantiated at this time).
% The location information is inherited from the annotated term Location.
% If Location was uninstantiated, so is the output.
% UseVarNames controls whether TNAME variable names may be used.
% If Location is an annotated variable term, and Term is the same
% variable, then it will inherit the name annotation as well.
annotated_create(TermAnn, Location, AnnTerm) :-
	annotated_create(TermAnn, Location, AnnTerm, true).

annotated_create(_TermOut, In, _Out, _UseVarNames) :- var(In), !.
annotated_create(TermOut,
	    annotated_term(TermIn,TypeIn,File,Line,From,To),
	    annotated_term(TermOut,TypeOut,File,Line,From,To), UseVarNames) :-
	( nonvar(TermOut) ->
	    type_of(TermOut, TypeOut)
	; TermOut==TermIn ->	% same variable?
	    TypeOut = TypeIn	% inherit name
	; UseVarNames==true, get_var_info(TermOut, name, Name) ->
	    % try to add the variable name if it is available from the parser
	    TypeOut = var(Name)
	;
	    type_of(TermOut, TypeOut)
	).


% Create a fully annotated version of the plain term Term, inheriting the
% location information for all subterms from the annotated term Location.
% Note that the Term must not be further instantiated afterwards,
% otherwise the var-annotations will become wrong!
% If Location is an annotated variable term, then any occurrences
% of this variable in Term will inherit the name as well.
term_to_annotated(Term, Location, AnnTerm) :-
	term_to_annotated(Term, Location, AnnTerm, true).

% If UseVarNames is false, do not try to add variable names. This is useful
% to suppress singleton warnings when the annotated term gets compiled.
term_to_annotated(_Term, Location, _Ann, _UseVarNames) :-
	var(Location), !.
term_to_annotated(Term, Location, Ann, UseVarNames) :-
	( compound(Term) ->
	    functor(Term, F, A), 
	    functor(TermAnn, F, A),
	    annotated_create(TermAnn, Location, Ann, UseVarNames),
	    ( for(I,1,A), param(Location,Term,TermAnn,UseVarNames) do
		arg(I, Term, Arg),
		arg(I, TermAnn, AnnArg),
		term_to_annotated(Arg, Location, AnnArg, UseVarNames)
	    )
	;
	    annotated_create(Term, Location, Ann, UseVarNames)
	).
	

% Make an annotated term for TermOut with exactly the same annotation
% as In (provided In was instantiated).  This works only if both terms
% have the same type, as both location and type are inhertited.
% If that is not the case, use instead:
%	annotated_match(In, TermIn),
%	annotated_create(TermOut, In, Out)

same_annotation(_TermIn, In, _TermOut, _Out) :- var(In), !.
same_annotation(TermIn, annotated_term(TermIn,Type,File,Line,From,To),
	TermOut, annotated_term(TermOut,Type,File,Line,From,To)).


% Reconstruct the plain term from the annotated term
:- export annotated_to_term/2.
annotated_to_term(AnnTerm, Term) :- var(AnnTerm), !,
	error(4, annotated_to_term(AnnTerm, Term)).
annotated_to_term(annotated_term(TermAnn,_,_,_,_,_), Term) :- !,
	( compound(TermAnn) ->
	    functor(TermAnn, F, N),
	    functor(Term, F, N),
	    ( for(I,1,N), param(TermAnn,Term) do
		arg(I, TermAnn, AnnArg),
		arg(I, Term, Arg),
		annotated_to_term(AnnArg, Arg)
	    )
	;
	    Term = TermAnn
	).
annotated_to_term(AnnTerm, Term) :-
	error(5, annotated_to_term(AnnTerm, Term)).
	

%----------------------------------------------------------------
% Predefined macros
%----------------------------------------------------------------

% The protecting functor no_macro_expansion/1
%
% Should just be
%	trprotect(no_macro_expansion(X), X).
% but to avoid problems we don't use no_macro_expansion/1 in the definition.

trprotect(In, Out) :- arg(1, In, Out).

:- define_macro(no_macro_expansion/1, trprotect/2, [protect_arg,global]).


/* Backward-compatibility transformation for matching clauses */

tr_match((Head ?- Body), (Head :- -?-> Body), AnnMatch, AnnTrans) :-
        same_annotation((AnnHead ?- AnnBody), AnnMatch, 
                        (AnnHead :- AnnMatchBody), AnnTrans),
        annotated_create(-?-> AnnBody, AnnMatch, AnnMatchBody). 

:- define_macro((?-)/2, tr_match/4, [clause, global]).


%
% Goal macros / Inlining of general goals
%
% We use a special convention for goal expansion (inlining) code:
% If it exits with a positive integer Tag, this is interpreted as
% an error number and the error will be raised in a higher level
% predicate, e.g. the compiler or expand_goal/2.
%
% Using annotated_term in raw form, as macro expansion not available yet!
%:- export struct(annotated_term(
%	term,		% var, atomic or compound
%	type,		% atom
%	file,		% atom
%	line,		% integer
%	from,		% integer
%	to		% integer
%	% may be extended in future
%    )).
% This is defined later in this file

expand_goal(Goal, Expanded, Module) :-
	expand_goal_annotated_(Goal, _, Expanded, _, Module).

expand_goal_annotated_(Goal, AnnGoal, Expanded, AnnExpanded, Module) :-
	catch(tr_goals_annotated(Goal, AnnGoal, Expanded, AnnExpanded, Module),
	    Tag,
	    ( integer(Tag), Tag > 0 ->
		error(Tag, Goal, Module)
	    ;
		throw(Tag)
	    )
	).

tr_goals(Goal, Expanded, Module) :-
	tr_goals_annotated(Goal, _, Expanded, _, Module).


tr_goals_annotated(G, Ann, GC, AnnGC, M) :-
	( current_pragma(inline_depth(D))@M, integer(D) -> true ; D=10 ),
	tr_goals_annotated(G, Ann, GC, AnnGC, D, M).

tr_goals_annotated(Var, Ann, Var, Ann, _, _) :- var(Var), !.
tr_goals_annotated((G1, G2), Ann, (GC1, GC2), AnnExp, D, M) :- !,
        same_annotation((AnnG1,AnnG2), Ann, (AnnGC1,AnnGC2), AnnExp),
	tr_goals_annotated(G1, AnnG1, GC1, AnnGC1, D, M),
	tr_goals_annotated(G2, AnnG2, GC2, AnnGC2, D, M).
tr_goals_annotated((G1*->G2;G3), Ann, Expanded, AnnExp, D, M) ?- !, Expanded = (GC1*->GC2;GC3),
	same_annotation((AnnLhs;AnnG3), Ann, (AnnLhsC;AnnGC3), AnnExp),
	same_annotation((AnnG1*->AnnG2), AnnLhs, (AnnGC1*->AnnGC2), AnnLhsC),
	tr_goals_annotated(G1, AnnG1, GC1, AnnGC1, D, M),
	tr_goals_annotated(G2, AnnG2, GC2, AnnGC2, D, M),
	tr_goals_annotated(G3, AnnG3, GC3, AnnGC3, D, M).
tr_goals_annotated((G1; G2), Ann, (GC1; GC2), AnnExp, D, M) :- !,
	same_annotation((AnnG1;AnnG2), Ann, (AnnGC1;AnnGC2), AnnExp),
	tr_goals_annotated(G1, AnnG1, GC1, AnnGC1, D, M),
	tr_goals_annotated(G2, AnnG2, GC2, AnnGC2, D, M).
tr_goals_annotated((G1 -> G2), Ann, (GC1 -> GC2), AnnExp, D, M) :- !,
	same_annotation((AnnG1->AnnG2), Ann, (AnnGC1->AnnGC2), AnnExp),
	tr_goals_annotated(G1, AnnG1, GC1, AnnGC1, D, M),
	tr_goals_annotated(G2, AnnG2, GC2, AnnGC2, D, M).
tr_goals_annotated(-?->(G), Ann, -?->(GC), AnnExp, D, M) :- !,
	same_annotation(-?->(AnnG), Ann, -?->(AnnGC), AnnExp),
	tr_goals_annotated(G, AnnG, GC, AnnGC, D, M).
tr_goals_annotated(once(G), Ann, once(GC), AnnExp, D, M) :-
	!,
	same_annotation(once(AnnG), Ann, once(AnnGC), AnnExp),
	tr_goals_annotated(G, AnnG, GC, AnnGC, D, M).
tr_goals_annotated(not(G), Ann, not(GC), AnnExp, D, M) :-
	!,
	same_annotation(not(AnnG), Ann, not(AnnGC), AnnExp),
	tr_goals_annotated(G, AnnG, GC, AnnGC, D, M).
tr_goals_annotated(\+(G), Ann, \+(GC), AnnExp, D, M) :-
	!,
	same_annotation(\+(AnnG), Ann, \+(AnnGC), AnnExp),
	tr_goals_annotated(G, AnnG, GC, AnnGC, D, M).
tr_goals_annotated(LM:G, Ann, GC, AnnGC, D, M) :- !,
	annotated_arg(2, Ann, AnnG),
	tr_colon(G, AnnG, GC, AnnGC, M, LM, D).
tr_goals_annotated(Goal, Ann, GC, AnnGC, D, M) :-
	( try_tr_goal(Goal, Ann, G1, AnnG1, M, M, D, D1) -> 
	    tr_goals_annotated(G1, AnnG1, GC, AnnGC, D1, M) 
	; 
	    GC = Goal,
	    AnnGC = Ann
	).


% Inlining of ModuleList:Goal

    tr_colon(G, AnnG, NewG, AnnNewG, _M, LM, _D) :- 
	var(LM), !, 
	NewG = LM:G,
	term_to_annotated(LM, AnnG, AnnLM),
	annotated_create((AnnLM:AnnG), AnnG, AnnNewG).
    tr_colon(_G, AnnG, NewG, AnnNewG, _M, [], _D) :- !, 
	NewG = true,
	annotated_create(NewG, AnnG, AnnNewG).
    tr_colon(G, AnnG, NewG, AnnNewG, M, [LM|LMs], D) :- !,
        ( try_tr_goal(G, AnnG, LMG0, AnnLMG0, LM, M, D, D1) ->
	    tr_goals_annotated(LMG0, AnnLMG0, LMG, AnnLMG, D1, M)
	;
	    LMG = LM:G,
	    term_to_annotated(LM, AnnG, AnnLM),
	    annotated_create((AnnLM:AnnG), AnnG, AnnLMG)
	),
	( LMs == [] ->
	    NewG = LMG,
	    AnnNewG = AnnLMG
	;
            NewG = (LMG,LMsG),
	    % make sure AnnLMsG inherits source position
	    annotated_create((AnnLMG,AnnLMsG), AnnG, AnnNewG),
            % like annotated_create(LMsG, AnnG, AnnLMsG) but no setting
            % of type for AnnLMsG, as LMsG not constructed yet
            (nonvar(AnnG) ->
                AnnG = annotated_term(_,_,File,Line,From,To),
                AnnLMsG = annotated_term(_,_,File,Line,From,To)
            ;
                true
            ),
            tr_colon(G, AnnG, LMsG, AnnLMsG, M, LMs, D)
	).
    tr_colon(G, AnnG, NewG, AnnNewG, M, LM, D) :-
	( try_tr_goal(G, AnnG, LMG, AnnLMG, LM, M, D, D1) -> 
	    tr_goals_annotated(LMG, AnnLMG, NewG, AnnNewG, D1, M) 
	; 
	    NewG = LM:G,
	    annotated_create(AnnLM:AnnG, AnnG, AnnNewG),
	    term_to_annotated(LM, AnnG, AnnLM)
	).


% Inline transformation of a standard goal

try_tr_goal(Goal, AnnGoal, NewGoal, AnnNewGoal, LM, CM, Depth, Depth1) :-
	visible_goal_macro(Goal, TransPred, TLM, LM),
	( succ(Depth1, Depth) ->
	    transform(Goal, AnnGoal, NewGoal, AnnNewGoal, TransPred, TLM, CM)
	;
	    functor(Goal, F, N),
	    printf(warning_output,
	    	"WARNING: inlining terminated at depth limit: %Kw%n",[LM:F/N]),
	    fail
	).

    % In C:
    % visible_goal_macro(Goal, TransPred, TLM, LM) :-
    %	callable(Goal),
    %	functor(Goal, N, A),
    %	get_flag(N/A, inline, TransPred)@LM,
    %	get_flag(N/A, definition_module, TLM)@LM,
    %	set referenced-flag for the procedure descriptor.


%
% This is called just after parsing (if the term contains read-macros).
% Transformations are done bottom-up.
% A transformation that fails leaves the corresponding subterm untransformed.
% A transformation that delays makes an error and leaves the subterm untransformed.
% A transformation that aborts aborts the whole read-predicate.
%

expand_macros_(Term, Expanded, ContextModule) :-
	expand_macros_term(Term, Expanded, ContextModule, none).

    expand_macros_term(Term, Expanded, _ContextModule, _Exclude) :-
	var(Term),
	Expanded = Term.
    expand_macros_term(Term, Expanded, ContextModule, Exclude) :-
	nonvar(Term),
	functor(Term, N, A),
	(
	  visible_term_macro(Term, TransPred, Options, TLM, ContextModule, 12 /*TRANS_PROP*/),
	  nonmember(Exclude, Options)
	->
	    ( memberchk(protect_arg, Options) ->
		ArgsExpanded = Term
	    ;
		% transform arguments
		functor(ArgsExpanded, N, A),
		expand_macros_args(1, A, Term, ArgsExpanded, ContextModule)
	    ),
	    ( transform(ArgsExpanded, _AnnArgsExpanded, Expanded, _AnnExpanded, TransPred, TLM, ContextModule) ->
		true
	    ;
		Expanded = ArgsExpanded
	    )
	;
	    functor(Expanded, N, A),
	    expand_macros_args(1, A, Term, Expanded, ContextModule)
	).

    expand_macros_args(I, A, Term, ArgsExpanded, ContextModule) :-
	( I > A ->
	    true
	;
	    I1 is I+1,
	    arg(I, Term, Arg),
	    arg(I, ArgsExpanded, ExpandedArg),
	    expand_macros_term(Arg, ExpandedArg, ContextModule, top_only),
	    expand_macros_args(I1, A, Term, ArgsExpanded, ContextModule)
	).


% And the same with annotated terms, called form read_annotated/2,3
% Keep this in sycnc with expand_macros_/3!

expand_macros_annotated_(Term, AnnTerm, Expanded, AnnExpanded, ContextModule) :-
	nonvar(AnnTerm),
	expand_macros_term(Term, AnnTerm, Expanded, AnnExpanded, ContextModule, none).

    expand_macros_term(Term, Ann, Expanded, AnnExpanded, _ContextModule, _Exclude) :-
	var(Term),
	Ann = AnnExpanded,
	Expanded = Term.
    expand_macros_term(Term, Ann, Expanded, AnnExpanded, ContextModule, Exclude) :-
	nonvar(Term),
	( good_annotation(Term, Ann) ->
	    functor(Term, N, A),
	    (
	      visible_term_macro(Term, TransPred, Options, TLM, ContextModule, 12 /*TRANS_PROP*/),
	      nonmember(Exclude, Options)
	    ->
		( memberchk(protect_arg, Options) ->
		    ArgsExpanded = Term,
		    AnnArgsExpanded = Ann
		;
		    % transform arguments
		    functor(ArgsExpanded, N, A),
		    functor(ArgsExpandedAnn, N, A),
		    same_annotation(TermAnn, Ann, ArgsExpandedAnn, AnnArgsExpanded),
		    expand_macros_args(1, A, Term, TermAnn, ArgsExpanded, ArgsExpandedAnn, ContextModule)
		),
		( transform(ArgsExpanded, AnnArgsExpanded, Expanded, AnnExpanded, TransPred, TLM, ContextModule) ->
		    true
		;
		    Expanded = ArgsExpanded,
		    AnnExpanded = AnnArgsExpanded
		)
	    ;
		functor(Expanded, N, A),
		functor(ExpandedAnn, N, A),
		same_annotation(TermAnn, Ann, ExpandedAnn, AnnExpanded),
		expand_macros_args(1, A, Term, TermAnn, Expanded, ExpandedAnn, ContextModule)
	    )
	;
	    % mismatch between Term and Ann, don't transform
	    Expanded = Term,
	    AnnExpanded = Ann
	).

    expand_macros_args(I, A, Term, TermAnn, ArgsExpanded, ArgsExpandedAnn, ContextModule) :-
	( I > A ->
	    true
	;
	    I1 is I+1,
	    arg(I, Term, Arg),
	    arg(I, ArgsExpanded, ExpandedArg),
	    arg(I, TermAnn, AnnArg),
	    arg(I, ArgsExpandedAnn, AnnExpandedArg),
	    expand_macros_term(Arg, AnnArg, ExpandedArg, AnnExpandedArg, ContextModule, top_only),
	    expand_macros_args(I1, A, Term, TermAnn, ArgsExpanded, ArgsExpandedAnn, ContextModule)
	).



% var(Ann) => var(AnnExpanded)
transform(Term, Ann, Expanded, AnnExpanded, TN/TA, TLM0, ContextModule) :-
	% construct goal <trans>(<in>, <out>[, <module>]) or
        %                <trans>(<in>, <out>, <inann>, <outann>[, <module>])
	functor(TransGoal, TN, TA),
	arg(1, TransGoal, Term),
	arg(2, TransGoal, Expanded),
        ( TA =< 2 ->
	    TLM = TLM0
        ; TA =< 3 ->
	    arg(3, TransGoal, ContextModule),
	    TLM = TLM0
	; 
	    /* with annotated goal, arity 4 or 5 */
	    arg(3, TransGoal, Ann),
	    arg(4, TransGoal, AnnExpanded),
	    ( TA =< 4 ->
		TLM = TLM0
	    ;
		arg(5, TransGoal, ContextModule),
		( TA =< 5 ->
		    TLM = TLM0
		;
		    % Sorry, hack: this only happens for unfold/6, which
		    % has a known lookup module, and gets an extra argument
		    arg(6, TransGoal, TLM0),
		    TLM = sepia_kernel
		)
	    )
	),
	% call toplevel transformation
	% TLM:TransGoal@ContextModule
	module_tag(TLM, MarkedTLM),
	subcall(MarkedTLM:TransGoal@ContextModule, Delayed),
	!,
	( Delayed = [] ->
            (var(AnnExpanded) ->
                % TransGoal did not annotate AnnExpanded
                term_to_annotated(Expanded, Ann, AnnExpanded)
            ;
                good_annotation(Expanded, AnnExpanded)
            )
	; 
	    error(129, TLM:TransGoal, ContextModule)
	).


expand_clause_(Clause, ClauseExpanded, ContextModule) :-
	expand_clause_annotated_(Clause, _, ClauseExpanded, _, ContextModule).


expand_clause_annotated_(Clause, AnnClause, ClauseExpanded,
    AnnClauseExpanded, ContextModule) :-
	clause_head(Clause, Head),
	(
	    nonvar(Head),
	    visible_term_macro(Head, TransPred, _Options, TLM, ContextModule, 16 /*CLAUSE_TRANS_PROP*/),
	    transform(Clause, AnnClause, ClauseExpanded, AnnClauseExpanded,
		 TransPred, TLM, ContextModule)
	->
	    true
	;
	    ClauseExpanded = Clause,
	    AnnClauseExpanded = AnnClause
	).


% Expand clauses and their body goals

expand_clauses(Clause, Clause, _Module) :-
	var(Clause), !.
expand_clauses([], [], _Module) :- !.
expand_clauses([Clause|Clauses], ExpClauses, Module) :- !,
	expand_clause_(Clause, StandardClauses, Module),
	expand_clause_bodies(StandardClauses, ExpClauses, ExpClauses0, Module),
	expand_clauses(Clauses, ExpClauses0, Module).
expand_clauses(Clause, ExpClauses, Module) :-
	expand_clause_(Clause, StandardClauses, Module),
	expand_clause_bodies(StandardClauses, ExpClauses, [], Module).

    expand_clause_bodies(Clause, [Clause|ExpClauses0], ExpClauses0, _Module) :-
	var(Clause), !.
    expand_clause_bodies([], ExpClauses, ExpClauses, _Module) :- !.
    expand_clause_bodies([Clause|Clauses], [ExpClause|ExpClauses1], ExpClauses0, Module) :- !,
	expand_clause_body(Clause, ExpClause, Module),
	expand_clause_bodies(Clauses, ExpClauses1, ExpClauses0, Module).
    expand_clause_bodies(Clause, [ExpClause|ExpClauses0], ExpClauses0, Module) :-
	expand_clause_body(Clause, ExpClause, Module).

    expand_clause_body((Head:-Body), Expanded, Module) ?- !,
	Expanded = (Head:-ExpandedBody),
	expand_goal(Body, ExpandedBody, Module).
    expand_clause_body(Clause, Clause, _Module).


:- export
	register_compiled_stream/1,
	register_compiler/1,
	deregister_compiler/0,
	nested_compile_term/1,
	nested_compile_term_annotated/2.

register_compiler(NestedCompileSpec) :-
	swapref(compile_stack, Stack, [NestedCompileSpec|Stack]).

deregister_compiler :-
	getref(compile_stack, Stack),
	( Stack = [_Old|Rest] ->
	    setref(compile_stack, Rest),
	    % If all compilations finished, do checks
	    ( Rest == [] -> declaration_checks ; true )
	;
	    true
	).

nested_compile_term_(Clauses, Module) :-
        nested_compile_term_annotated_(Clauses, _, Module).

nested_compile_term_annotated_(Clauses, AnnClauses, Module) :-
	getref(compile_stack, Stack),
	( Stack = [Top|_] ->
	    copy_term(Top, Args-Goal),
	    arg(1, Args, Clauses),
	    arg(2, Args, AnnClauses),
	    call(Goal)@Module
	;
	    ecl_compiler:compile_term_(Clauses, Module)
	).

nested_compile_load_flag(Loading) :-
	getref(compile_stack, Stack),
	( Stack = [Args-_Goal|_], arity(Args) >= 3 ->
	    arg(3, Args, Loading)
	;
	    Loading = all
	).

register_compiled_stream(Stream) :-
	setval(compiled_stream, Stream).

/*
register_compiled_stream(Stream) :-
	swapref(compiled_stream_stack, Stack, [Stream|Stack]).

:- export deregister_compiled_stream/0.
deregister_compiled_stream :-
	getref(compiled_stream_stack, Stack),
	( Stack = [_Old|Rest] ->
	    setref(compiled_stream_stack, Rest)
	;
	    true
	).
*/


:- define_macro('with attributes'/2, tr_with_attributes/3, [global]).
:- export tr_with_attributes/3.

tr_with_attributes(no_macro_expansion('with attributes'(X,Attrs)), X, Module) :-
	( meta(X) ->
	    error(122, X, Module)
%	    error(122, no_macro_expansion('with attributes'(X,Attrs)), Module)
	;
	    add_attributes(X, Attrs, Module)
	).

    add_attributes(_, [], _) ?- true.
    add_attributes(X, [Attr|Attrs], Module) ?-
	add_qualified_attribute(X, Attr, Module),
	add_attributes(X, Attrs, Module).

    add_qualified_attribute(X, Module:Attr, _Module) ?- !,
	add_attribute(X, Attr, Module).
    add_qualified_attribute(X, Attr, Module) :-
	add_attribute(X, Attr, Module).



clause_head((Head0 :- _), Head) ?- !, Head = Head0.
clause_head(Fact, Fact).


tr_clause(C, TC, _M) :- var(C), !,
	TC = C.
tr_clause(H :- B, H :- BC, M) :-
	!,
	tr_goals(B, BC, M).
tr_clause([H|T], [HC|TC], M) :-
	!,
	tr_clause(H, HC, M),
	tr_clause(T, TC, M).
tr_clause(C, C, _).



%----------------------------------------------------------------
% Goal portray transformations for builtin predicates
%----------------------------------------------------------------

:- export portray_control/3.
:- define_macro((',')/2, portray_control/3, [global,write,goal]).
:- define_macro((:)/2, portray_control/3, [global,write,goal]).
:- define_macro((@)/2, portray_control/3, [global,write,goal]).
:- define_macro('[]:@'/3, portray_control/3, [global,write,goal]).

portray_control((Goal1,Goal2), PortrayedGoal, CM) :- -?-> !,
	PortrayedGoal = (PGoal1,PGoal2),
	portray_goal(Goal1, PGoal1, CM),
	portray_goal(Goal2, PGoal2, CM).
portray_control(Goal@CM, PortrayedGoal, LM) :- -?-> !,
	PortrayedGoal = PortrayedGoal0@CM,
	portray_goal(Goal, PortrayedGoal0, CM, LM).
portray_control('[]:@'(LM,Goal,CM), PortrayedGoalAtCM, _) :- -?-> !,
	atom(LM), LM \= [],
	portray_goal(Goal, PortrayedGoal, CM, LM),
	PortrayedGoalAtCM = PortrayedGoal@CM.
portray_control(LM:Goal, PortrayedGoal, CM) :- -?->
	atom(LM), is_a_module(LM),
	portray_goal(Goal, PortrayedGoal0, CM, LM),
	( Goal == PortrayedGoal0 ->
	    % don't lose qualification if there was no change
	    PortrayedGoal = LM:PortrayedGoal0
	;
	    % re-qualify the expansion if necessary
	    qualify_goal_if_needed(PortrayedGoal0, CM, LM, PortrayedGoal, _)
	).

    % qualify_goal_if_needed(+Goal, +CM, +LM, -QGoal, -UsedLM)
    qualify_goal_if_needed(Goal, CM, _, QualGoal, M) :- var(Goal), !,
	QualGoal = Goal, M = CM.
    qualify_goal_if_needed(Goal, _, _, QualGoal, M) :- Goal = LM:_, !,
	QualGoal = Goal, M = LM.
    qualify_goal_if_needed(Goal, CM, LM, QualGoal, M) :-
	functor(Goal, N, A),
	( is_a_module(LM) ->
	    ( get_flag_body(N/A, definition_module, DM, LM) ->
		( atom(CM), is_a_module(CM), get_flag_body(N/A, definition_module, DM, CM) ->
		    % the correct N/A is visible anyway, no need to qualify
		    QualGoal = Goal, M = CM
		;
		    QualGoal = LM:Goal, M = LM
		)
	    ;
		% not visible in LM, no point qualifying
		QualGoal = Goal, M = CM
	    )
	;
	    QualGoal = LM:Goal, M = LM
	).



%----------------------------------------------------------------
% Interface to portray functionality
%----------------------------------------------------------------

:- export portray_goal/2.
:- tool(portray_goal/2, portray_goal/3).
portray_goal(Goal, PortrayedGoal, CM) :-
	portray_goal(Goal, PortrayedGoal, CM, CM).

    portray_goal(Goal, PortrayedGoal, CM, LM) :-
	callable(Goal),
	% if we can't lookup in LM, use at least CM
	( authorized_module(LM) -> MLM=LM ; MLM=CM ),
	visible_term_macro(Goal, TransPred, _Options, TLM, MLM, 15 /*WRITE_GOAL_TRANS_PROP*/),
	transform(Goal, _, PortrayedGoal, _, TransPred, TLM, CM),
	!.
    portray_goal(Goal, Goal, _, _).



:- export portray_term/3.
:- tool(portray_term/3, portray_term_/4).

portray_term_(Term, Portrayed, term, Module) ?- !,
	portray_term_term(Term, Portrayed, Module, no).
portray_term_(Term, Portrayed, top_term, Module) ?- !,
	portray_term_term(Term, Portrayed, Module, yes).
portray_term_(Term, Portrayed, goal, Module) ?- !,
	portray_goal(Term, Portrayed, Module, Module).
portray_term_(Term, Portrayed, clause, Module) ?- !,
	error(141, portray_term(Term, Portrayed, clause), Module).
portray_term_(Term, Portrayed, What, Module) :-
	error(6, portray_term(Term, Portrayed, What), Module).

    % this transformation is top-down, i.e. whole term before its arguments
    portray_term_term(Term, Portrayed, _ContextModule, _TopOnly) :-
	var(Term),
	Portrayed = Term.
    portray_term_term(Term, Portrayed, ContextModule, TopOnly) :-
	nonvar(Term),
	(
	    visible_term_macro(Term, TransPred, Options, TLM, ContextModule, 13), % WRITE_TRANS_PROP
	    transform(Term, _, TopPortrayed, _, TransPred, TLM, ContextModule)
	->
	    true
	;
	    Options = [],
	    TopPortrayed = Term
	),
	( memberchk(protect_arg, Options) ->
	    Portrayed = TopPortrayed
	; TopOnly == yes ->
	    Portrayed = TopPortrayed
	;
	    functor(TopPortrayed, PN, PA),
	    functor(Portrayed, PN, PA),
	    portray_term_args(1, PA, TopPortrayed, Portrayed, ContextModule)
	).

    portray_term_args(I, A, TopPortrayed, Portrayed, ContextModule) :-
	( I > A ->
	    true
	;
	    I1 is I+1,
	    arg(I, TopPortrayed, Arg),
	    arg(I, Portrayed, PortrayedArg),
	    portray_term_term(Arg, PortrayedArg, ContextModule, no),
	    portray_term_args(I1, A, TopPortrayed, Portrayed, ContextModule)
	).


:- pragma(expand).	% we can do it from now on!


% for the event handler
clause_spec(Clause, Name, Arity, Module) :-
	clause_head(Clause, OldHead),
	visible_term_macro(OldHead, TransPred, _Options, TLM, Module, 16 /*CLAUSE_TRANS_PROP*/),
	transform(Clause, _, TrClause, _, TransPred, TLM, Module),
	clause_head(TrClause, Head),
	functor(Head, Name, Arity).
clause_spec(Clause, Name, Arity, _) :-
	clause_head(Clause, Head),
	functor(Head, Name, Arity).

/***

:- inline((@)/2, tr_at/3).

tr_at(LookupModule:Goal@CallerModule, NewGoal, ContextModule) ?- !,
	nonvar(Goal), nonvar(LookupModule),
	functor(Goal, GoalN, GoalA),
	( get_flag(GoalN/GoalA, tool, on)@LookupModule ->
	    tool_body(GoalN/GoalA, ToolN/ToolA, ToolModule)@LookupModule,
	    Goal =.. [GoalN|Args],
	    append(Args, [CallerModule], BodyArgs),
	    BodyGoal =.. [ToolN|BodyArgs],
	    ( get_flag(ToolN/ToolA, definition_module, ToolModule)@ContextModule ->
%	    ( ToolModule = ContextModule ->
		tr_goals(BodyGoal, NewGoal, ContextModule)	% it's visible/defined here
	    ;
		tr_goals(call_explicit(BodyGoal, ToolModule), NewGoal, ContextModule)
	    )
	;
	    ( LookupModule = ContextModule ->
		tr_goals(Goal, NewGoal, ContextModule)
	    ;
		tr_goals(call_explicit(Goal, LookupModule), NewGoal, CallerModule)
	    )
	).
tr_at(Goal@ContextModule, NewGoal, ContextModule) ?- !,
	tr_goals(Goal, NewGoal, ContextModule).
tr_at(Goal@CallerModule, NewGoal, ContextModule) ?- !,
	tr_at(ContextModule:Goal@CallerModule, NewGoal, ContextModule).

***/


% Portray tool bodies as their interfaces

:- define_macro((=:=)/3, portray_builtin/2, [global,write,goal]).
:- define_macro((=\=)/3, portray_builtin/2, [global,write,goal]).
:- define_macro((>=)/3, portray_builtin/2, [global,write,goal]).
:- define_macro((=<)/3, portray_builtin/2, [global,write,goal]).
:- define_macro((>)/3, portray_builtin/2, [global,write,goal]).
:- define_macro((<)/3, portray_builtin/2, [global,write,goal]).

portray_builtin(=:=(X,Y,_M), X=:=Y).
portray_builtin(=\=(X,Y,_M), X=\=Y).
portray_builtin(>=(X,Y,_M), X>=Y).
portray_builtin(=<(X,Y,_M), X=<Y).
portray_builtin(>(X,Y,_M), X>Y).
portray_builtin(<(X,Y,_M), X<Y).


%----------------------------------------------------------------------
% Support for storing definitions and managing the visibility of
% module-aware named 'items' such as struct- and domain-definitions.
%
% Each type of item has two hash tables (stores) associated:
%	
% DefStore holds the item definition (which can be local or exported)
%	key		DefModule:Name
%	value		Scope:Definition
%
% ImpStore holds the import information
%	key		ImpModule:Name
%	value		DefModule
%
% where
%	Name		the name of the item (atom)
%	Definition	the item definition (a ground term)
%	DefModule	definition module (atom), always \= ImpModule
%	Scope		'local' or 'export'
%	ImpModule	importing module (atom)
%----------------------------------------------------------------------

% Define a new item, Scope is 'local' or 'export'.
% Allow duplicate, identical definitions.
% Set bip_error on error.
:- mode define_item(+,++,+,+,+,+,-).
define_item(Name, Definition, DefModule, Scope, DefStore, ImpStore, New) :-
	check_atom(Name),
	check_atom(DefModule),
	check_atom(Scope),
	( visible_item(Name, OldDef, DefModule, OldScope, DefStore, ImpStore) ->
	    ( OldDef == Definition, Scope == OldScope ->
		New = false
	    ;
		redef_error(OldScope)
	    )
	;
	    New = true,
	    % make a canonical, persistent copy of the term, so it can be
	    % shared and we don't need to make a copy on every retrieval
	    canonical_copy(Scope:Definition, StoredDefinition),
	    store_set(DefStore, DefModule:Name, StoredDefinition)
	).


% Import an item from ExpOrReexpModule into ImpModule.
% Allow duplicate, identical definitions.
% Set bip_error on error.
:- mode import_item(+,+,+,+,+).
import_item(Template, ExpOrReexpModule, ImpModule, DefStore, ImpStore) :-
	( compound(Template) -> true ; set_bip_error(5) ),
	functor(Template, Key, _),
	% first find the actual definition module
	( store_get(ImpStore, ExpOrReexpModule:Key, DefModule) ->
	    true
	;
	    DefModule = ExpOrReexpModule
	),
	% catch duplicate imports
	( visible_item(Key, _OldDef, ImpModule, OldScope, DefStore, ImpStore) ->
	    ( OldScope == from(DefModule) ->
		true			% identical, ignore
	    ;
		redef_error(OldScope)	% ambiguous, keep first one
	    )
	; ImpModule == DefModule ->
	    true			% ignore if local
	;
	    store_set(ImpStore, ImpModule:Key, DefModule)
	).

    redef_error(local) :-
	set_bip_error(87).
    redef_error(export) :-
	set_bip_error(88).
    redef_error(from(_)) :-
	set_bip_error(89).


% Lookup or enumerate visible items in LookupModule
% Scope is 'local', 'export' or from(DefModule).
% :- mode visible_item(+,-,+,-,+,+) is semidet
% :- mode visible_item(-,-,+,-,+,+) is nondet
visible_item(Key, Definition, LookupModule, Scope, DefStore, ImpStore) :-
	nonvar(Key),
	(
	    % first look for locally defined structs
	    store_get(DefStore, LookupModule:Key, Scope:Definition)
	->
	    true
	; 
	    % then look for imported structs
	    store_get(ImpStore, LookupModule:Key, DefModule), % may fail
	    store_get(DefStore, DefModule:Key, (export):Definition), % may fail
	    Scope = from(DefModule)
	).
visible_item(Key, Definition, LookupModule, Scope, DefStore, ImpStore) :-
	var(Key),
	(
	    % first look for locally defined structs
	    stored_keys(DefStore, DefModsKeys),
	    member(DefModKey, DefModsKeys),
	    DefModKey = LookupModule:Key,		% may fail
	    store_get(DefStore, DefModKey, Scope:Definition)
	; 
	    % then look for imported structs
	    stored_keys(ImpStore, ImpModsKeys),
	    member(ImpModKey, ImpModsKeys),
	    ImpModKey = LookupModule:Key,		% may fail
	    store_get(ImpStore, ImpModKey, DefModule),
	    store_get(DefStore, DefModule:Key, (export):Definition),
	    Scope = from(DefModule)
	).


% Erase all information about Module's definitions and imports of an item.
% Keep information about imports _from_ Module.
:- mode erase_module_item(+,+,+).
erase_module_item(Module, DefStore, ImpStore) :-
	store_erase_qualified(ImpStore, Module),
	store_erase_qualified(DefStore, Module).


%----------------------------------------------------------------------
% Structure declarations
%
% Information about struct declarations is stored in two hash tables:
%
% Table 'struct_def' holds the structure definitions (local or exported)
%	key		DefModule:Name
%	value		Scope:Prototype
%
% Table 'imported_struct' holds the import information
%	key		ImpModule:Name
%	value		DefModule
%
% where
%	Name		the name of the structure (atom)
%	Prototype	the struct definition (a ground structure)
%	DefModule	definition module (atom), always \= ImpModule
%	Scope		'local' or 'export'
%	ImpModule	importing module (atom)
%----------------------------------------------------------------------

:- export tr_with/5, tr_of/3.

:- define_macro((with)/2, tr_with/5, [global]),
   define_macro((of)/2,	  tr_of/3,   [global]).

?- store_create_named(struct_def).
?- store_create_named(imported_struct).


% Define a new structure, Scope is 'local' or 'export'.
% Set bip_error on error.
define_struct(Definition, DefModule, Scope) :-
	check_struct_def(Definition),
	functor(Definition, Name, _),
	define_item(Name, Definition, DefModule, Scope, struct_def, imported_struct, _New).

    check_struct_def(X) :- var(X), !, set_bip_error(4).
    check_struct_def(X) :- compound(X), !,
	arity(X, N),
	check_struct_def_arg(N, X, FieldNames),
	sort(0, <, FieldNames, FieldNamesNoDuplicates),
	( length(FieldNamesNoDuplicates, N) -> true ; set_bip_error(6) ).
    check_struct_def(_) :- set_bip_error(5).

    :- mode check_struct_def_arg(+,+,-).
    check_struct_def_arg(0, _, []) :- !.
    check_struct_def_arg(I, X, [N|Ns]) :-
	arg(I, X, A),
	check_field_def(A, N),
	I1 is I-1,
	check_struct_def_arg(I1, X, Ns).

    :- mode check_field_def(?,-).
    check_field_def(X, _) :- var(X), !, set_bip_error(4).
    check_field_def(N, N) :- atom(N), !.
    check_field_def(N:S, N) :- atom(N), atom(S), !.
    check_field_def(_, _) :- set_bip_error(5).


% Import a structure from an exporting or reexporting module.
% Set bip_error on error.
import_struct(Template, ExpOrReexpModule, ImpModule) :-
	import_item(Template, ExpOrReexpModule, ImpModule, struct_def, imported_struct).


% Lookup or enumerate visible structures in LookupModule
% Scope is 'local', 'export' or from(DefModule).
% :- mode visible_struct(+,-,+,-) is semidet
% :- mode visible_struct(-,-,+,-) is nondet
visible_struct(Key, Definition, LookupModule, Scope) :-
	visible_item(Key, Definition, LookupModule, Scope, struct_def, imported_struct).


% Erase all information about Module's definitions and imports.
% Keep information about imports from Module.
erase_module_structs(Module) :-
	erase_module_item(Module, struct_def, imported_struct).


% the current_struct/1 builtin (obsolete)
:- export current_struct/1.
:- tool(current_struct/1, current_struct_/2).
current_struct_(ProtoStruct, M) :- var(ProtoStruct),
	current_struct_(_Name, ProtoStruct, M).
current_struct_(ProtoStruct, M) :- nonvar(ProtoStruct),
	functor(ProtoStruct, Name, _),
	current_struct_(Name, ProtoStruct, M).


% the current_struct/2 builtin
:- export current_struct/2.
:- tool(current_struct/2, current_struct_/3).
current_struct_(Name, ProtoStruct, M) :- var(Name), !,
	visible_struct(Name, ProtoStruct, M, _Scope).
current_struct_(Name, ProtoStruct, M) :- atom(Name), !,
	visible_struct(Name, ProtoStruct, M, _Scope).
current_struct_(Name, ProtoStruct, M) :-
	error(5, current_struct(Name, ProtoStruct), M).



% the macro transformation for with/2

tr_with(Term, Struct, AnnTerm, AnnStruct, M) :-
	Term = no_macro_expansion(Functor with Args),
	atom(Functor),
	visible_struct(Functor, ProtoStruct, M, _Scope), !,
        annotated_match(AnnTerm, TermAnn),
        TermAnn = no_macro_expansion(AnnFunctor with _AnnArgs),
	functor(ProtoStruct, Functor, Arity),
	functor(Struct, Functor, Arity),
	(tr_and(Args, ProtoStruct, Struct, M) ->
	    ( no_duplicates(Args) ->
		 term_to_annotated(Struct, AnnFunctor, AnnStruct) 
	    ;
		 printf(warning_output,
		    "WARNING: Duplicate struct field name in module %w in%n    %w%n", [M,Term]),
		 fail
	    )
	;
	     printf(warning_output,
		"WARNING: Unrecognised or missing struct field name in module %w in%n	 %w%n", [M,Term]),
	     fail
	).
tr_with(Term, _Struct, _AnnTerm, _AnnStruct, M) :-
	printf(warning_output,
	    "WARNING: Unrecognized structure name in module %w in%n    %w%n", [M,Term]),
	fail.

    no_duplicates(Args) :- Args = [_|_], !,
	    sort(1, <, Args, Unique),
	    same_length(Args, Unique).
    no_duplicates(_).

tr_and([], _ProtoStruct, _Struct, _M) ?- !.
tr_and([Arg|Args], ProtoStruct, Struct, M) ?- !,
	tr_field(Arg, ProtoStruct, Struct, M),
	tr_and(Args, ProtoStruct, Struct, M).
tr_and(Arg, ProtoStruct, Struct, M) :-
	tr_field(Arg, ProtoStruct, Struct, M).

tr_field(FieldName:FieldValue, ProtoStruct, Struct, M) ?-
	atom(FieldName),
	struct_insert_field(ProtoStruct, FieldName, FieldValue, Struct, M).


% the macro transformation for of/2

tr_of(no_macro_expansion(Field of Functor), N, M) :-
	atom(Functor),
	visible_struct(Functor, ProtoStruct, M, _Scope), 
	!,
	( struct_lookup_field(ProtoStruct, Field, N, M) -> 
	      true
	; 
	      printf(warning_output, 
		     "WARNING: Unrecognized field name in '%w of %w' in module %w.%n%b", [Field,Functor,M]),
	      fail
	).
tr_of(Term, _N, M) :-
	printf(warning_output,
	    "WARNING: Unrecognized structure name in '%w' in module %w.%n%b", [Term,M]),
	fail.

    struct_lookup_field(ProtoStruct, Field, N, M) :-
	atom(Field), 
	struct_lookup_index(ProtoStruct, Field, N, M).
    struct_lookup_field(ProtoStruct, property(Prop), N, _M) :- -?->
	struct_lookup_property(ProtoStruct, Prop, N).


struct_lookup_index(ProtoStruct, FieldName, Index, M) :-
	arity(ProtoStruct, Arity),
	( proto_lookup_index(ProtoStruct, FieldName, Index, Arity) -> true
	; substruct_lookup_index(ProtoStruct, FieldName, Index, Arity, M)
	).

    struct_lookup_property(ProtoStruct, arity, Arity) :- -?->
	arity(ProtoStruct, Arity).
    struct_lookup_property(ProtoStruct, functor, Functor) :- -?->
	Functor = Name/Arity,
	functor(ProtoStruct, Name, Arity).


    proto_lookup_index(_ProtoStruct, _FieldName, _, 0) :- !, fail.
    proto_lookup_index(ProtoStruct, FieldName, Index, I) :-
	arg(I, ProtoStruct, FieldSpec),
	( FieldSpec = FieldName ->
	    Index = I
	; FieldSpec = FieldName:_SubStruct ->
	    Index = I
	;
	    I1 is I-1,
	    proto_lookup_index(ProtoStruct, FieldName, Index, I1)
	).

    substruct_lookup_index(_ProtoStruct, _FieldName, _, 0, _M) :- !, fail.
    substruct_lookup_index(ProtoStruct, FieldName, Index, I, M) :-
	arg(I, ProtoStruct, FieldSpec),
	(
	    FieldSpec = _SubFieldName:SubStructFunctor,
	    visible_struct(SubStructFunctor, ProtoSubStruct, M, _),
	    struct_lookup_index(ProtoSubStruct, FieldName, SubIndex, M)
	->
	    ( integer(SubIndex) -> Index = [I,SubIndex] ; Index = [I|SubIndex] )
	;
	    I1 is I-1,
	    substruct_lookup_index(ProtoStruct, FieldName, Index, I1, M)
	).


struct_insert_field(ProtoStruct, FieldName, FieldValue, Struct, M) :-
	arity(ProtoStruct, Arity),
	( proto_insert_field(ProtoStruct, FieldName, FieldValue, Struct, Arity) -> true
	; substruct_insert_field(ProtoStruct, FieldName, FieldValue, Struct, Arity, M)
	).

    proto_insert_field(_ProtoStruct, _FieldName, _FieldValue, _, 0) :- !, fail.
    proto_insert_field(ProtoStruct, FieldName, FieldValue, Struct, I) :-
	arg(I, ProtoStruct, FieldSpec),
	( FieldSpec = FieldName ->
	    arg(I, Struct, FieldValue)
	; FieldSpec = FieldName:_SubStruct ->
	    arg(I, Struct, FieldValue)
	;
	    I1 is I-1,
	    proto_insert_field(ProtoStruct, FieldName, FieldValue, Struct, I1)
	).

    substruct_insert_field(_ProtoStruct, _FieldName, _FieldValue, _Struct, 0, _M) :- !, fail.
    substruct_insert_field(ProtoStruct, FieldName, FieldValue, Struct, I, M) :-
	arg(I, ProtoStruct, FieldSpec),
	(
	    FieldSpec = _SubFieldName:SubStructFunctor,
	    visible_struct(SubStructFunctor, SubProtoStruct, M, _Scope),
	    functor(SubProtoStruct, SubStructFunctor, SubArity),
	    functor(SubStruct, SubStructFunctor, SubArity),
	    arg(I, Struct, SubStruct),
	    struct_insert_field(SubProtoStruct, FieldName, FieldValue, SubStruct, M)
	->
	    true
	;
	    I1 is I-1,
	    substruct_insert_field(ProtoStruct, FieldName, FieldValue, Struct, I1, M)
	).


:- tool(update_struct/4, update_struct/5).
:- inline(update_struct/4, tr_update_struct/3).
:- export update_struct/4.

update_struct(Name, Fields, OldStruct, MergeStruct, Module) :-
	tr_update_struct1(Name, Fields, OldStruct, MergeStruct, Goal, Module),
	!,
	Goal@Module.
update_struct(Name, Fields, OldStruct, MergeStruct, Module) :-
	bip_error(update_struct(Name, Fields, OldStruct, MergeStruct), Module).


tr_update_struct(update_struct(Name, Fields, OldStruct, MergeStruct), GoalOut, Module) :-
	tr_update_struct1(Name, Fields, OldStruct, MergeStruct, GoalOut, Module),
	!.
tr_update_struct(Goal, _, Module) :-
	get_bip_error(Err),
	( Err = 4 ->
	    % might work at runtime, no error
	    printf(warning_output, "WARNING: could not expand %w in module %w%n", [Goal,Module]),
	    fail
	;
	    error(Err, Goal, Module)
	).


tr_update_struct1(F, Fields, OldStruct, MergeStruct,
	    ( OldStruct=OldTemplate, MergeStruct=NewTemplate), Module) :-
	check_atom(F),
	check_nonvar(Fields),
	( Fields = [_|_] -> FieldList = Fields
	; Fields = [] -> FieldList = Fields
	; FieldList = [Fields] ),
	make_templates(F, FieldList, FieldList3, OldTemplate, NewTemplate, Module),
	( FieldList3 == [] ->
	    true
	;
	    check_fieldspecs(FieldList3),
	    printf(warning_output, "WARNING: Unrecognised field name(s) %w in struct '%w'%n",
		[FieldList3,F]),
	    set_bip_error(6)
	).

    % make the two templates for F (OldTemplate and NewTemplate) with the
    % fields from FieldList filled in accordingly and all the other fields
    % unified. The unrecognised remainder of FieldList is returned.
    make_templates(F, FieldList0, FieldList, OldTemplate, NewTemplate, Module) :-
	( current_struct(Declaration)@Module, functor(Declaration, F, N) ->
	    true
	;
	    printf(warning_output, "WARNING: Unrecognised structure name '%w'%n", [F]),
	    set_bip_error(6)
	),
	functor(OldTemplate, F, N),
	functor(NewTemplate, F, N),
	fillin_fields(1, N, FieldList0, FieldList1, OldTemplate, Declaration, NewTemplate, SubStructs),
	fillin_sub_fields(SubStructs, FieldList1, FieldList, OldTemplate, NewTemplate, Module).


    % Treat all the fields which are not in substructures and return
    % a list of substructures for subsequent processing of leftover fields.
    % This is breadth-first so that field names hide names in substructures.
    fillin_fields(I, N, FieldList1, FieldList, OldTemplate, Declaration, NewTemplate, SubStructs) :-
	( I > N ->
	    FieldList = FieldList1,
	    SubStructs = []
	;
	    arg(I, Declaration, FieldDecl),
	    ( FieldDecl = FieldName:SubStruct ->
		( find_field(FieldName, FieldList1, Arg, FieldList2) ->
		    SubStructs = SubStructs0
		;
		    SubStructs = [I-SubStruct|SubStructs0],
		    FieldList2 = FieldList1
		)
	    ;
		( find_field(FieldDecl, FieldList1, Arg, FieldList2) ->
		    SubStructs = SubStructs0
		;
		    SubStructs = SubStructs0,
		    FieldList2 = FieldList1,
		    arg(I, OldTemplate, Arg)
		)
	    ),
	    arg(I, NewTemplate, Arg),
	    I1 is I+1,
	    fillin_fields(I1, N, FieldList2, FieldList, OldTemplate, Declaration, NewTemplate, SubStructs0)
	).


    % try to find any fields in the list of substructures
    fillin_sub_fields([], FieldList, FieldList, _OldTemplate, _NewTemplate, _Module).
    fillin_sub_fields([I-SubF|SubStructs], FieldList0, FieldList, OldTemplate, NewTemplate, Module) :-
	make_templates(SubF, FieldList0, FieldList1, OldSubTemplate, NewSubTemplate, Module),
	( FieldList0 == FieldList1 ->
	    arg(I, OldTemplate, Arg),	% optimization: no field in this substruct
	    arg(I, NewTemplate, Arg)
	;
	    arg(I, OldTemplate, OldSubTemplate),
	    arg(I, NewTemplate, NewSubTemplate)
	),
	fillin_sub_fields(SubStructs, FieldList1, FieldList, OldTemplate, NewTemplate, Module).


    find_field(FieldName, [FieldName:Arg0|Rem0], Arg, Rem) ?-
	Arg = Arg0,
	Rem = Rem0.
    find_field(FieldName, [Field|Fields], Arg, Rem) ?-
	Rem = [Field|Rem0],
	find_field(FieldName, Fields, Arg, Rem0).



%----------------------------------------------------------------------
% Enums
%
% Enum declarations are stored in three hash tables:
%
% The two standard tables for items:
%
%	domain_def:		DefModule:Name	-> Scope:Definition
%	imported_domain:	ImpModule:Name	-> DefModule
%
% and an additional, redundant table to quickly map symbols to integers:
%
%	domain_symbols:		LookupMod:Value -> (DefMod:Name)-Index
%
% Within every module, all domain symbols must be unique, i.e. it must
% be possible to determine the symbol's type from looking at the value.
% We therefore need additional checks on definition and importation.
%----------------------------------------------------------------------

?- local store(domain_def).
?- local store(imported_domain).
?- local store(domain_symbols).

% Define a new domain, Scope is 'local' or 'export'.
% Allow duplicate, identical definitions.
% Make sure no symbol is already defined in this module
% Set bip_error on error.
define_domain(Definition, DefModule, Scope) :-
	check_domain_def(Definition, DefModule, DefModule),
	functor(Definition, Name, N),
	define_item(Name, Definition, DefModule, Scope, domain_def, imported_domain, New),
	( New = true ->
	    store_symbols(N, Definition, DefModule:Name, DefModule)
	;
	    true
	).

    check_domain_def(ValueArray, _DefModule, _Module) :- var(ValueArray), !,
	set_bip_error(4).
    check_domain_def(ValueArray, DefModule, Module) :- compound(ValueArray), !,
	ValueArray =.. [Name|Symbols],
	check_domain_def_args(Symbols, DefModule:Name, Module), 
	sort(0, <, Symbols, SymbolsNoDuplicates),
	arity(ValueArray, N),
	( length(SymbolsNoDuplicates, N) -> true ; set_bip_error(6) ).
    check_domain_def(_ValueArray, _DefModule, _Module) :-
	set_bip_error(5).

    :- mode check_domain_def_args(+,+,+).
    check_domain_def_args([], _, _).
    check_domain_def_args([X|Xs], QualName, Module) :-
	check_domain_symbol(X, QualName, Module),
	check_domain_def_args(Xs, QualName, Module).

    :- mode check_domain_symbol(?,+,+).
    check_domain_symbol(X, _, _) :- var(X), !,
	set_bip_error(4).
    check_domain_symbol(Symbol, QualName, Module) :- atomic(Symbol), !,
	( store_get(domain_symbols, Module:Symbol, OtherQualName-_) ->
	    ( QualName == OtherQualName ->
		true
	    ;
		printf(error, "Domain value %w not unique in module %w%n",
			[Symbol,Module]),
		set_bip_error(6)	% should have own number
	    )
	;
	    true).
    check_domain_symbol(_, _, _) :-
	set_bip_error(5).

    :- mode store_symbols(+,+,+,+).
    store_symbols(0, _Definition, _QualName, _Module) :- !.
    store_symbols(N, Definition, QualName, Module) :-
	arg(N, Definition, Symbol),
	store_set(domain_symbols, Module:Symbol, QualName-N),
	N1 is N-1,
	store_symbols(N1, Definition, QualName, Module).


% Import a domain
% Make sure no symbol is already defined in this module
% Allow duplicate, identical definitions.
% Set bip_error on error.
import_domain(Template, ExpOrReexpModule, ImpModule) :-
	functor(Template, Name, N),
	% get the definition we are going to import and check for clashing symbols
	visible_item(Name, Definition, ExpOrReexpModule, Scope, domain_def, imported_domain),
	( Scope = from(DefModule) -> true ; DefModule = ExpOrReexpModule ),
	check_domain_def(Definition, DefModule, ImpModule),
	import_item(Template, ExpOrReexpModule, ImpModule, domain_def, imported_domain),
	store_symbols(N, Definition, DefModule:Name, ImpModule).


% Erase all information about Module's domains
erase_module_domains(Module) :-
	erase_module_item(Module, domain_def, imported_domain),
	store_erase_qualified(domain_symbols, Module).


:- export domain_index/3.
:- tool(domain_index/3, domain_index_/4).
domain_index_(Symbol, QualName, Index, Module) :- var(Symbol), !,
	error(4, domain_index(Symbol, QualName, Index), Module).
domain_index_(Symbol, QualName, Index, Module) :- atomic(Symbol), !,
	store_get(domain_symbols, Module:Symbol, QualNameIndex),
	QualNameIndex = QualName-Index.
domain_index_(Symbol, QualName, Index, Module) :-
	error(5, domain_index(Symbol, QualName, Index), Module).


:- export current_domain/3.
:- tool(current_domain/3, current_domain_/4).
current_domain_(Name, DefModule, Definition, Module) :- var(Name), !,
	visible_item(Name, Definition, Module, Scope, domain_def, imported_domain),
	( Scope = from(DefModule) -> true ; DefModule = Module ).
current_domain_(Name, DefModule, Definition, Module) :- atomic(Name), !,
	visible_item(Name, Definition, Module, Scope, domain_def, imported_domain),
	( Scope = from(DefModule) -> true ; DefModule = Module ).
current_domain_(Name, DefModule, Definition, Module) :-
	error(5, current_domain(Name, DefModule, Definition), Module).


%-------------------------------
% coroutining
%-------------------------------

% NOTE: The positions of the suspend-arguments are hardcoded elsewhere
% in the kernel (and ic)!  _suspension_attribute() relies on bound being the
% last list, the inst list is a difference list, the bound list is normal.

:- export struct(suspend(inst,constrained,bound)).


coroutine :-			% backward compatibility
	global_flags(0,16'00000100,_).

coroutining :-			% local
	global_flags(0,0) /\ 16'00000100 =\= 0.

kill_suspension(S) :-
	kill_suspension(S, 1).

current_suspension(S) :-
	current_suspension(S, []).


% the sound negation

:- export (~)/1.
:- tool((~)/1, tilde_body/2).
:- set_flag(tilde_body/2, trace_meta, on).

tilde_body(Goal, Module) :-
	nonground(Goal, Var),
	!,
	make_suspension(~(Goal), 0, Susp, Module),
	insert_suspension([Var], Susp, 1, suspend).
tilde_body(Goal, Module) :-
	untraced_call(Goal,Module),
	!, fail.
tilde_body(_,_).


%----------------------------------------------------------------
% explicit suspension - suspend/2,3
%----------------------------------------------------------------

/*
One thing we can definitely do is a static mapping from symbolic names
to numeric priorities (which only gets changed when someone comes up with
a convincing use case for introducing a new level).  For propagators,
we could use Gecode's scheme, where the priorities are called
{unary, binary, ternary, linear, quadratic, cubic, veryslow}
i.e. they initially distinguish constraint arity, then complexity.
For ECLiPSe, where delayed goals can be used for things other than
propagators, I would extend this on both ends as follows:

1-debug (goals that always succeed and do not affect program semantics)
2-check (tests that succeed or fail or abort)
3-unary
4-binary
5-ternary
6-linear
7-quadratic
8-cubic
9-subsolver (e.g. the eplex demon)
10-mopup (bookkeeping to be done after all propagation, e.g. lib(changeset))
11-search (nondeterministic goals)
12-main program

This gives us the 12 levels we currently have.  Since we use 4 bits to store
priorities, it would be natural to extend to 15 (giving some flexibility
that can be used e.g. for the case of the ternary propagators in lib(ic)
which schedule themselves up/down one level depending on whether they
achieved some useful propagation or not. This kind of dynamic adjustment
may well be more important than a fine grained static classification).
*/

:- export
	suspend/3,
	suspend/4.
:- export
	tr_suspend/3.


:- inline(suspend/3, tr_suspend/3).
:- inline(suspend/4, tr_suspend/3).

% If tr_suspend should fail at compile time, we just
% don't expand and leave the error to runtime.
tr_suspend(no_macro_expansion(suspend(Goal, Prio, List)), Goals, Module) :-
    tr_suspend(no_macro_expansion(suspend(Goal, Prio, List, _Susp)), Goals, Module).
tr_suspend(no_macro_expansion(suspend(Goal, Prio, List, Susp)), Goals, Module) :-
    Goals = (make_suspension(Goal, Prio, Susp, Module), G1),
    tr_suspend1(Susp, List, Module, G1).

tr_suspend1(_Susp, [], _Module, Goals) ?- !,
	Goals = true.
tr_suspend1(Susp, [Spec|Specs], Module, Goals) ?- !,
    tr_suspend2(Susp, Spec, Module, Goals, Goals1),
    tr_suspend1(Susp, Specs, Module, Goals1).
tr_suspend1(Susp, Spec, Module, Goals) :-
    tr_suspend2(Susp, Spec, Module, Goals, true).

tr_suspend2(Susp, Vars->Select, Module, Goals, Goals0) ?-
    %find_susp_list(Select, Index, M, Module),
    %Goal = insert_suspension(Vars, Susp, Index, M).
    make_inserts_top(Select, Vars, Susp, Module, Goals, Goals0).
tr_suspend2(Susp, trigger(Event), _Module, Goals, Goals0) ?-
    Goals = (attach_suspensions(Event, Susp),Goals0).

    % make_inserts(+Spec, ?Vars, +Susp, +Module, -Goals, ?MoreGoals)
    %
    % Generate insert_suspension/4 goals.  Allowed forms of Spec:
    %	->min
    %	->fd:min
    %	->fd:3         could have been fd:(max of fd)
    %	->fd:[min,3]
    %	->[min,fd:max,fd:4,fd:[min,3]]
    %
    % Names are taken from meta_attribute-suspension_lists-declarations
    % (if present), or from a struct that has the same name as the attribute.
    % In any case, an attribute-named structure must be visible (we use the
    % struct-visibility as a proxy for the (global) attribute's visibility)!
    % Support for unqualified names, e.g. X->min works in the same way,
    % but requires a unique match for a specific attribute.
    % Ambiguity leads to a warning, and failure.

    make_inserts_top([], _Vars, _Susp, _Module, Gs, Gs0) ?- !,
    	Gs = Gs0.
    make_inserts_top([Spec|Specs], Vars, Susp, Module, Gs, Gs0) ?- !,
	make_inserts(Spec, Vars, Susp, Module, Gs, Gs1),
	make_inserts_top(Specs, Vars, Susp, Module, Gs1, Gs0).
    make_inserts_top(Spec, Vars, Susp, Module, Gs, Gs0) :-
	make_inserts(Spec, Vars, Susp, Module, Gs, Gs0).

    % accept unqualified atom, or qualified something
    make_inserts(SuspName, Vars, Susp, Module, Gs, Gs0) :- atom(SuspName), !,
	lookup_suspension_list(AttrName, SuspName, Slots, Module),
	make_inserts_slots(AttrName, Slots, Vars, Susp, Gs, Gs0).
    make_inserts(AttrName:Spec, Vars, Susp, Module, Gs, Gs0) ?- atom(AttrName),
	make_inserts_quals(AttrName, Spec, Vars, Susp, Module, Gs, Gs0).

    % attribute known: accept suspension name or integer, or list thereof
    make_inserts_quals(_, [], _, _, _, Gs, Gs0) ?- !,
    	Gs=Gs0.
    make_inserts_quals(AttrName, [Spec|Specs], Vars, Susp, Module, Gs, Gs0) ?- !,
	make_inserts_qual(AttrName, Spec, Vars, Susp, Module, Gs, Gs1),
	make_inserts_quals(AttrName, Specs, Vars, Susp, Module, Gs1, Gs0).
    make_inserts_quals(AttrName, Spec, Vars, Susp, Module, Gs, Gs0) :-
	make_inserts_qual(AttrName, Spec, Vars, Susp, Module, Gs, Gs0).

    % attribute known: accept suspension name or integer
    make_inserts_qual(AttrName, Slot, Vars, Susp, _Module, Gs, Gs0) :- integer(Slot),
	Gs = (insert_suspension(Vars, Susp, Slot, AttrName),Gs0).
    make_inserts_qual(AttrName, SuspName, Vars, Susp, Module, Gs, Gs0) :- atom(SuspName),
	lookup_suspension_list(AttrName, SuspName, Slots, Module),
	make_inserts_slots(AttrName, Slots, Vars, Susp, Gs, Gs0).

    % attribute known: accept integer list (no check)
    make_inserts_slots(_AttrName, [], _Vars, _Susp, Gs, Gs).
    make_inserts_slots(AttrName, [Slot|Slots], Vars, Susp, Gs, Gs0) :-
	Gs = (insert_suspension(Vars, Susp, Slot, AttrName),Gs1),
	make_inserts_slots(AttrName, Slots, Vars, Susp, Gs1, Gs0).


% Non-expanded version
:- tool(suspend/3, suspend_body/4).
suspend_body(Goal, Prio, List, Module) :-
    suspend_body(Goal, Prio, List, _Susp, Module).

:- tool(suspend/4, suspend_body/5).
suspend_body(Goal, Prio, List, Susp, Module) :-
    make_suspension(Goal, Prio, Susp, Module),
    ( tr_suspend1(Susp, List, Module, Goals) ->
	call(Goals)@Module
    ;
	error(6, suspend(Goal, Prio, List, Susp), Module)
    ).


%----------------------------------------------------------------
% Arithmetic preprocessing
%----------------------------------------------------------------

% transform a standalone is/2 or eval/2:
% - fail (do not transform) for variables
% - generate a simple unification for numbers

:- inline((is)/2, trans_is/2).

trans_is(Res is Expr, Code) :-
	trans_is(Expr, Res, Code).

    trans_is(Expr, Res, Code) :-
	number(Expr),
	Code = (Res = Expr).
    trans_is(Expr, Res, Code) :-
	callable(Expr),
	trans_function(Expr, Res, Call, Code, Call).


% transform a comparison
% fails if nothing to transform (otherwise we'll loop)

:- inline((>=)/2, trans_compare/2).
:- inline((>)/2, trans_compare/2).
:- inline((=<)/2, trans_compare/2).
:- inline((<)/2, trans_compare/2).
:- inline((=:=)/2, trans_compare/2).
:- inline((=\=)/2, trans_compare/2).

trans_compare(In, Code) :-
	functor(In, F, N),
	arg(1, In, X),
	arg(2, In, Y),
	functor(Out, F, N),
	arg(1, Out, RX),
	arg(2, Out, RY),
	trans_expr(X, RX, Code, Code1),
	trans_expr(Y, RY, Code1, sepia_kernel:Out),
	Out \== In.		% fail when nothing changed


% transform a sub-expression:
% The result variable Res is assumed to be "fresh" and may be unified!

trans_expr(M:Func, Res, Code, NextCode) ?-
	var(Func),			% special case, similar to eval
	!,
	Code = (eval(M:Func,Res),NextCode).
trans_expr(Expr, Res, Code, NextCode) :-
	callable(Expr),
	!,
	trans_function(Expr, Res, Call, Code, (Call,NextCode)).
trans_expr(Expr, Res, Code, NextCode) :-
	%  var(Expr) ; number(Expr) ; and error cases
	Res = Expr,			% bind at transformation time
	Code = NextCode.		% no code


trans_function(M:Expr, Res, Call, Code0, Code) :- !,
	Call = M:Pred,
	Code = Code0,
	nonvar(Expr),			% may fail
	functor(Expr, Op, Ar),
	+(Ar, 1, Ar1),
	functor(Pred, Op, Ar1),
	arg(Ar1, Pred, Res),
	unify_args(Ar, Expr, Pred).
trans_function(Expr, Res, Call, Code0, Code) :-
	functor(Expr, Op, Ar),
	+(Ar, 1, Ar1),
	functor(Pred, Op, Ar1),
	arg(Ar1, Pred, Res),
	( arith_builtin(Expr) ->
	    Call = sepia_kernel:Pred,
	    trans_args(1, Ar, Expr, Pred, Code0, Code)
	; inlined_arith_builtin(Expr) ->
	    Call = sepia_kernel:Pred,
	    Code = Code0,
	    unify_args(Ar, Expr, Pred)
	;
	    Call = Pred,
	    Code = Code0,
	    unify_args(Ar, Expr, Pred)
	).

    trans_args(N, Ar, Expr, Pred, Code0, Code) :-
	( N > Ar ->
	    Code = Code0
	;
	    arg(N, Expr, E1),
	    arg(N, Pred, R1),
	    trans_expr(E1, R1, Code0, Code1),
	    +(N, 1, N1),
	    trans_args(N1, Ar, Expr, Pred, Code1, Code)
	).



% The following is the list of "builtin" arithmetic functions.
% - their arguments get recursively evaluated
% - they are currently always qualified with sepia_kernel:...
%   because that's the semantics when the expression is interpreted in is/2

:- export arith_builtin/1.
arith_builtin(eval(_)).
arith_builtin(+_).
arith_builtin(-_).
arith_builtin(abs(_)).
arith_builtin(sgn(_)).
arith_builtin(fix(_)).
arith_builtin(integer(_)).
arith_builtin(rational(_)).
arith_builtin(rationalize(_)).
arith_builtin(numerator(_)).
arith_builtin(denominator(_)).
arith_builtin(float(_)).
arith_builtin(breal(_)).
arith_builtin(breal_from_bounds(_,_)).
arith_builtin(breal_min(_)).
arith_builtin(breal_max(_)).
arith_builtin(floor(_)).
arith_builtin(ceiling(_)).
arith_builtin(round(_)).
arith_builtin(truncate(_)).
arith_builtin(copysign(_,_)).
arith_builtin(nexttoward(_,_)).
arith_builtin(random(_)).
arith_builtin(\_).
arith_builtin(_ + _).
arith_builtin(_ - _).
arith_builtin(_ * _).
arith_builtin(_ / _).
arith_builtin(_ // _).
arith_builtin(_ rem _).
arith_builtin(_ div _).
arith_builtin(_ mod _).
arith_builtin(_ ^ _).
arith_builtin(min(_,_)).
arith_builtin(max(_,_)).
arith_builtin(gcd(_,_)).
arith_builtin(lcm(_,_)).
arith_builtin(_ /\ _).
arith_builtin(_ \/ _).
arith_builtin(xor(_,_)).
arith_builtin(_ >> _).
arith_builtin(_ << _).
arith_builtin(setbit(_,_)).
arith_builtin(getbit(_,_)).
arith_builtin(clrbit(_,_)).
arith_builtin(sin(_)).
arith_builtin(cos(_)).
arith_builtin(tan(_)).
arith_builtin(atan(_,_)).
arith_builtin(asin(_)).
arith_builtin(acos(_)).
arith_builtin(atan(_)).
arith_builtin(exp(_)).
arith_builtin(ln(_)).
arith_builtin(sqrt(_)).
arith_builtin(pi).
arith_builtin(e).

% These are also "builtin" arithmetic functions.
% - they have their own inlining transformation
% - they are always qualified with sepia_kernel:...
inlined_arith_builtin(sum(_)).
inlined_arith_builtin(min(_)).
inlined_arith_builtin(max(_)).


:- export peval/4.
peval(R, X, Code, NextCode) :-
	trans_expr(X, R, Code, NextCode).


%
% subscript(+Matrix, +IndexList, ?Element)
%
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
	    eval(IExpr, I, M),
	    xget(Mat, I, X)
	;
	    error(6, subscript(Mat,[IExpr|IExprs],X), M)
	).
    subscript3(Mat, IExpr, X, M, IExprs) :-
	string(Mat), !,
	( IExprs = [] ->
	    eval(IExpr, I, M),
	    string_code(Mat, I, X)
	;
	    error(6, subscript(Mat,[IExpr|IExprs],X), M)
	).
    subscript3(Mat, IExpr, X, M, IExprs) :-
	error(5, subscript(Mat,[IExpr|IExprs],X), M).

    subscript1(Mat, IExpr, X, M, IExprs) :- integer(IExpr), !,
	arg(IExpr, Mat, Row),
	subscript(Row, IExprs, X, M).
    subscript1(Mat, Min..Max, Xs, M, IExprs) ?- !,
	eval(Min, Imin, M),
	eval(Max, Imax, M),
%	functor(Mat, F, _),
	Offset is Imin-1,
	N is Imax-Offset,
	( N >= 0 ->
%	    functor(Xs, F, N),
	    functor(Xs, [], N),
	    ( foreacharg(X,Xs,J), param(Offset,Mat,IExprs,M) do
		I is J+Offset,
		arg(I, Mat, Row),
		subscript(Row, IExprs, X, M)
	    )
	;
	    error(6, subscript(Mat,[Min..Max|IExprs],Xs), M)
	).
    subscript1(Mat, *, Xs, M, IExprs) ?- !,
	( IExprs==[] ->	% could check for [*], [*,*], etc
	    Xs = Mat	% don't copy
	;
%	    functor(Mat, F, N),
	    arity(Mat, N),
	    functor(Xs, [], N),
	    ( foreacharg(X,Xs,I), param(Mat,IExprs,M) do
		arg(I, Mat, Row),
		subscript(Row, IExprs, X, M)
	    )
	).
    subscript1(Mat, IExpr, X, M, IExprs) :-
	eval(IExpr, I, M),
	arg(I, Mat, Row),
	subscript(Row, IExprs, X, M).


% Inlining for subscript/3: try to flatten
% arithmetic expressions within the index list

:- inline(subscript/3, t_subscript/2).
t_subscript(subscript(Mat, IndexList, Res), Code) :-
	trans_index_list(IndexList, EvalIndexList, Code, Code0),
	Code \== Code0,		% prevent looping
	Code0 = sepia_kernel:subscript(Mat, EvalIndexList, Res).

    trans_index_list([E|Es], RRs, Code0, Code) ?- !,
	RRs = [R|Rs],
	trans_index(E, R, Code0, Code1),
	trans_index_list(Es, Rs, Code1, Code).
    trans_index_list(VarNilJunk, VarNilJunk, Code, Code).

    trans_index(*, R, Code0, Code) ?- !, R = (*), Code0=Code.
    trans_index(From..To, R, Code0, Code) ?- !,
	R = EvalFrom..EvalTo,
	trans_expr(From, EvalFrom, Code0, Code1),
	trans_expr(To, EvalTo, Code1, Code).
    trans_index(E, R, Code0, Code) :-
	trans_expr(E, R, Code0, Code).


flatten_array(Array, List) :-
	var(Array),
	!,
	error(4, flatten_array(Array, List)).
flatten_array(Array, List) :-
	compound(Array),
	functor(Array, [], N),
	!,
	flatten_array(Array, N, List, []).
flatten_array(Array, List) :-
	error(5, flatten_array(Array, List)).

    flatten_array(_Array, 0, List, List0) :- !,
	List = List0.
    flatten_array(Array, I, List, List0) :-
	succ(I0, I),
	arg(I, Array, X),
	flatten_array(X, List1, List0),
	flatten_array(Array, I0, List, List1).

    flatten_array(Array, List, List0) :-
	compound(Array),
	functor(Array, [], N),
	!,
	flatten_array(Array, N, List, List0).
    flatten_array(X, [X|List0], List0).


/*
% Extension: accept expressions
:- tool(between/3, between_/4).
:- inline(between/3, t_between/2).
between_(LE, HE, X, M) :-
        eval(LE, L)@M,
        eval(HE, H)@M,
        between3(L, H, X).

:- tool(between/4, between_/5).
:- inline(between/4, t_between/2).
between_(LE, HE, SE, X, M) :-
        eval(LE, L)@M,
        eval(HE, H)@M,
        eval(SE, S)@M,
        between4(L, H, S, X).

t_between(between(LE,HE,X), Exp2) :-
        Exp2 = (L is LE, H is HE, between3(L, H, X)).
t_between(between(LE,HE,SE,X), Exp2) :-
        Exp2 = (L is LE, H is HE, S is SE, between4(L, H, S, X)).
*/

between(L, H, Step, X) :-
        check_integer(L),
        check_integer(H),
        check_integer(Step),
        !,
        ( Step > 0 ->
            ( var(X), L =< H, between_up(L, H, Step, X)
            ; integer(X), L=<X, X=<H, (X-L) mod Step =:= 0
            )
        ; Step < 0 ->
            ( var(X), L >= H, between_down(L, H, Step, X)
            ; integer(X), H=<X, X=<L, (X-L) mod Step =:= 0
            )
        ;
            error(6, between(L, H, Step, X))
        ).
between(L, H, Step, X) :-
        bip_error(between(L, H, Step, X)).

    between_up(L, H, Step, X) :-
        L1 is L+Step,
        ( L1 > H ->
            X = L       % last
        ;
            X = L
        ;
            between_up(L1, H, Step, X)
        ).

    between_down(L, H, Step, X) :-
        L1 is L+Step,
        ( L1 < H ->
            X = L       % last
        ;
            X = L
        ;
            between_down(L1, H, Step, X)
        ).


% Slightly faster specialised version for Step=1
between(L, H, X) :-
        check_integer(L),
        check_integer(H),
        !,
        ( var(X), L =< H, between_up(L, H, X)
        ; integer(X), L=<X, X=<H
        ).
between(L, H, X) :-
        bip_error(between(L, H, X)).

    between_up(L, H, X) :-
        ( L == H ->
            X = L
        ;
            X = L
        ;
            L1 is L+1,
            between_up(L1, H, X)
        ).


%----------------------------------------------------------------
% Other inlining optimisations
%----------------------------------------------------------------

t_bips(T =.. [F|Args], Goal, _) :- -?->			% =.. /2
	atom(F), is_list(Args), !,
	Term =.. [F|Args],
	Goal = (T=Term).
t_bips(setarg(Path,T,X), Goal, _) :- -?->		% setarg/3
	proper_path(Path,AB,C), !,
	( AB=[] -> Goal = setarg(C,T,X)
	; Goal = (arg(AB,T,S),setarg(C,S,X))
	).
t_bips(arg(Is,T0,X), Gs, _) :- -?->			% arg/3
	is_list(Is), !,
	% The point of this expansion is to avoid the construction
	% of the auxiliary list Is at runtime (time and garbage).
	(
	    fromto(T0,T1,T2,T3),
	    fromto(Is,[I|Is1],Is1,[I0]),
	    fromto(Gs,(arg(I,T1,T2),Gs1),Gs1,arg(I0,T3,X))
	do
	    true
	).


    % Auxiliaries

    proper_path([A],AB,C) :- -?-> !,
	AB=[], C=A.
    proper_path([A|BC], AB, C) :- -?->
	AB=[A|B],
	proper_path(BC,B,C).


% The inline declarations should be after the definition of t_bips/3
% to avoid attempted inlining of the calls inside t_bips/3

:- inline((=..)/2, t_bips/3).
:- inline(arg/3, t_bips/3).
:- inline(setarg/3, t_bips/3).
:- inline(call_priority/2, inline_calls/3).
:- inline(subcall/2, inline_calls/3).
%:- inline((not)/1, inline_calls/3).
%:- inline((\+)/1, inline_calls/3).
:- inline(call_explicit/2, inline_calls/3).
:- inline((:)/2, inline_calls/3).	% never used, just set the flag


%-----------------------------------------------------------------------
% Include other files that contain parts of the kernel
%-----------------------------------------------------------------------

:- include("kernel_loops.pl").
:- include("events.pl").
:- include("meta.pl").
:- include("array.pl").
:- include("pdb.pl").
:- include("debug.pl").
:- include("dynamic.pl").
:- include("environment.pl").
:- include("io.pl").
:- include("setof.pl").
:- include("tconv.pl").
:- include("kernel_vectors.pl").
:- include("kernel_dcg.pl").
:- include("kernel_bips.pl").
:- include("tracer.pl").


%--------------------------------------------
% List of deprecated builtins
%--------------------------------------------

:- deprecated(abolish_op/2,		"Use :- local op(0,...,...) to hide definition").
:- deprecated(abolish_record/1,		"Use erase_all/1").
:- deprecated(alarm/1,			"Use event_after/2").
:- deprecated(autoload/2,		"").	% no proper replacement yet
:- deprecated(autoload_tool/2,		"").	% no proper replacement yet
%:- deprecated(b_external/1,		"Write backtracking wrapper in ECLiPSe").
%:- deprecated(b_external/2,		"Write backtracking wrapper in ECLiPSe").
:- deprecated(call_c/2,			"Write an external predicate (see Embedding Manual)").
:- deprecated(call_explicit/2,		"Use Module:Goal").
:- deprecated(char_int/2,		"Use char_code/2").
:- deprecated(cancel_after_event/1,	"Use cancel_after_event/2").
%:- deprecated(coroutine/0,		"").
:- deprecated(current_after_event/1,	"Use current_after_events/1").
:- deprecated(current_stream/3,		"Use current_stream/1 and get_stream_info/3").
:- deprecated(current_struct/1,		"Use current_struct/2").
:- deprecated(dbgcomp/0,		"").
:- deprecated(date/1,			"Use local_time_string/3").
:- deprecated(pause/0,			"Use current_interrupt/2 and kill/2 (UNIX only)").
:- deprecated(define_error/2,		"Use atomic event names").
:- deprecated(define_macro/3,		"Use :- local macro(...) or :- export macro(...) or :- inline(...)").
:- deprecated(delay/2,			"Use suspend/3").
:- deprecated(erase_macro/2,		"Use :- local macro(...) to hide definition").
:- deprecated(errno_id/2,		"Use errno_id/1").
:- deprecated(event_create/2,		"Use event_create/3").
:- deprecated(event_retrieve/2,		"Use event_retrieve/3").
:- deprecated(fail_if/1,		"Use \\+ /1").
%:- deprecated(flatten_array/2,		"Use array_flat/3").
:- deprecated(get_char/1,		"Use iso:get_char/1 which returns an atom").
:- deprecated(get_char/2,		"Use iso:get_char/2 which returns an atom").
:- deprecated(get_error_handler/3,	"Use get_event_handler/3").
:- deprecated(get_prompt/3,		"Use get_stream_info/3").
:- deprecated(get_timer/2,		"Use after events").
:- deprecated((global)/1,		"Use export/1").
:- deprecated(global_op/3,		"Use :- export op(...)").
:- deprecated(is_built_in/1,		"Use current_built_in/1 or get_flag/3").
:- deprecated(is_locked/1,		"Use get_module_info/3").
:- deprecated(lib/2,			"Use lib/1").
:- deprecated(local_record/1,		"Use :- local record(...)").
:- deprecated(lock/1,			"Use lock for current module, or lock@Module").
:- deprecated(lock/2,			"Use lock_pass(Pass) for current module, or lock_pass(Pass)@Module").
:- deprecated(make_array/1,		"Use :- local variable(...) or :- local array(...)").
:- deprecated(make_array/2,		"Use :- local array(...)").
:- deprecated(make_local_array/1,	"Use :- local variable(...) or :- local array(...)").
:- deprecated(make_local_array/2,	"Use :- local array(...)").
%:- deprecated(meta_bind/2,		"").	% needed???
:- deprecated(name/2,			"Use string_list/2 with atom_string/2 or number_string/2").
:- deprecated(nodbgcomp/0,		"").
:- deprecated(pathname/2,		"Use pathname/3,4").
:- deprecated(portray_goal/2,		"Use portray_term/3").
:- deprecated(reset_error_handler/1,	"Use reset_event_handler/1").
:- deprecated(retract_all/1,		"Use retractall/1").
%:- deprecated(schedule_woken/1,		"").
:- deprecated(select/3,			"Use stream_select/3 or lists:select/3").
:- deprecated(set_chtab/2,		"Use local chtab declaration").
:- deprecated(set_error_handler/2,	"Use set_event_handler/2").
:- deprecated(set_prompt/3,		"Use set_stream_property/3").
:- deprecated(set_suspension_priority/2,"Use set_suspension_data/3").
:- deprecated(set_timer/2,		"Use after events").
:- deprecated(substring/4,		"Use substring/5").
:- deprecated(suffix/2,			"Use pathname/4").
:- deprecated(suspension_to_goal/3,	"Use get_suspension_data/3").


:- meta_predicate((
	-?->(0),
	@(0,*),
	:(*,0),
	','(0,0),
	;(0,0),
	->(0,0),
	*->(0,0),
	\+(0),
	~(0),
	block(0,*,0),
	call(0),
	call(0,*),
	call_priority(0,*),
	catch(0,*,0),
	do(*,0),
	is(*,1),
	make_suspension(:,*,*),
	mutex(*,0),
	not(0),
	once(0),
	phrase(2,*),
	phrase(2,*,*),
	subcall(0,*),
	suspend(:,*,*),
	suspend(:,*,*,*),
	set_event_handler(*,/),		% use
	set_interrupt_handler(*,/),	% use
	tool(*,/)			% use
    )).


%--------------------------------------------
% optional extension dependent initialisation
%--------------------------------------------

?- set_flag(variable_names, check_singletons).

present_libraries(_, [], []).
present_libraries(Sys, [Lib|L], [SysLib|T]) :-
	substring(Lib, "lib_", 1),
	concat_string([Sys, "/", Lib], SysLib),
	exists(SysLib),
	!,
	present_libraries(Sys, L, T).
present_libraries(Sys, [_|L], T) :-
	present_libraries(Sys, L, T).


% set the eclipse temporary directory
?-	make_array_(eclipse_tmp_dir, prolog, local, sepia_kernel),
	(	
	    getenv("ECLIPSETMP",OsTDir),
	    os_file_name(TDir, OsTDir)
	;
	    get_sys_flag(8, Arch),
            ( (Arch == "i386_nt" ; Arch == "x86_64_nt") ->
		(
		    getenv("TMP", OsTDir)
		;
		    getenv("TEMP", OsTDir)
		;
		    OsTDir = "C:\\WINDOWS\\Temp"
		),
		os_file_name(TDir, OsTDir)
	    ;
		TDir = "/tmp"
	    )
	;
	    getcwd(TDir)		  % last resort!
	),
	existing_path(TDir, dir),	  % must be a directory
	!,				  % assume we have write permission!
	canonical_path_name(TDir, CanonicalTDir),
	setval(eclipse_tmp_dir, CanonicalTDir).

% Now set the default library path

?-	get_sys_flag(13, EclipseDir),
	read_directory(EclipseDir, "", Files, _),
	present_libraries(EclipseDir, Files, Path0),
	concat_strings(EclipseDir, "/lib", Runlib),
	prepend_user_path([Runlib|Path0], Path),
	setval(library_path, Path),
	setval(library, Runlib).		% needed for load/2

?-
	(extension(development) ->
	    true
	;
	    lock_pass("Sepia")
	).
