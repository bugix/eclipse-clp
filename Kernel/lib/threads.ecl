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
% The Original Code is  The threads library for ECLiPSe.
% The Initial Developer of the Original Code is  Coninfer Ltd.
% Portions created by the Initial Developer are
% Copyright (C) 2014 Coninfer Ltd
% 
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: threads.ecl,v 1.6 2017/09/03 00:50:40 jschimpf Exp $
% ----------------------------------------------------------------------

% Semantic differences
%
% - ECLiPSe discourages thread aliases
% - ECLiPSe engines can be resumed/reused (from true/false/exception state)
% - ISO threads can only run one goal (it should be possible to deallocate
%   them on the goal's termination, apart from the heap-copied status)
% - ISO threads only disappear on join, unless detached
% - ECLiPSe's exited(Integer) state is mostly deallocated,
%   full deallocation happens on garbage collection or failure.
% - ECLiPSe's 'detached' engines go into exited() state when finished.

:- module(threads).

:- comment(categories, ["Compatibility"]).
:- comment(summary, "Prolog thread API (ISO/IEC Draft TR 13211-5:2007)").
:- comment(author, "Joachim Schimpf").
:- comment(copyright, "Coninfer Ltd").
:- comment(date, '$Date: 2017/09/03 00:50:40 $').
:- comment(desc, html("<P>
    This library implements most of the functionality described in
    the Draft Technical Recommendation ISO/IEC DTR 13211-5:2007
    'Prolog Multi-threading Support' (<A HREF=\"http://logtalk.org/plstd/threads.pdf\">http://logtalk.org/plstd/threads.pdf</A>),
    which is also the basis for multi-threading functionality in a
    number of Prolog systems (SWI, XSB, YAP).  To clarify notation,
    we will refer to
<UL>
    <LI>this library's 'threads' as <EM>DTR-threads</EM></LI>
    <LI>this library's 'queues' as <EM>DTR-queues</EM></LI>
</UL>
</P><P>
    Note that this library is just a thin layer on top of ECLiPSe's
    more general 'engine' functionality.  The main differences are:
<UL>
    <LI><EM>DTR-threads</EM> are implemented as <EM>ECLiPSe engines</EM>, and <EM>DTR-queues</EM> are
      implemented as <EM>ECLiPSe records</EM>.  While this in principle allows the
      mixing of ECLiPSe's native engine-API and this library's API, in the
      interest of readable code, programmers should commit to one of the APIs.
</LI>
    <LI>An <EM>ECLiPSe engine</EM> can optionally be associated with an
      operating system thread, while <EM>DTR-threads</EM> are
      always associated with an operating system thread.
</LI>
    <LI>An <EM>ECLiPSe engine</EM> can stop (with success, failure, etc),
      retain its state, and later be resumed.  In contrast, <EM>DTR-threads</EM>
      execute a single goal, are terminated as soon as this goal succeeds
      or fails or exits, and disappear when the thread is joined.
</LI>
    <LI>Although this library implements alias names for <EM>DTR-threads</EM>
      and <EM>queues</EM>, they should be used sparingly because they prevent
      automatic garbage collection of the named object.  For efficiency
      reasons, only one alias per item is supported.
</LI>
</UL>
</P>
")).

:- lib(lists).
:- lib(error).

:- import
	current_engines/1
   from sepia_kernel.


:- local store(thread_aliases).
:- local store(mutex_aliases).


:- comment(is_thread/1, [
    amode:is_thread(?),
    args:["Term":"Term to be tested"],
    summary:"Succeeds if Term is a DTR-thread alias or thread/engine handle",
    see_also:[is_handle/2]
]).
:- export is_thread/1.
is_thread(Alias) :- atom(Alias), !,
	store_get(thread_aliases, Alias, Handle),
	is_handle(Handle, engine).
is_thread(Handle) :-
	is_handle(Handle, engine).


:- comment(thread_default/1, [
    amode:(thread_default(+) is det),
    amode:(thread_default(-) is multi),
    args:["Option":"A term with arity 1, or variable"],
    summary:"Get default options for thread/engine creation",
    desc:html("Accesses the default thread/engine creation options:
    local(KBytes), global(KBytes) and detached(Bool).
    "),
    eg:"
    % This predicate is defined as:
    thread_default(local(KB))  :- get_flag(default_localsize,  KB).
    thread_default(global(KB)) :- get_flag(default_globalsize, KB).
    thread_default(detached(false)).
    ",
    see_also:[thread_create/2,thread_property/2,engine_create/2,get_flag/2]
]).
:- export thread_default/1.
thread_default(local(X)) :- get_flag(default_localsize, X).
thread_default(global(X)) :- get_flag(default_globalsize, X).
thread_default(detached(false)).


:- comment(thread_set_default/1, [
    amode:(thread_set_default(++) is det),
    args:["Option":"A thread-option term"],
    summary:"Set default options for thread/engine creation",
    desc:html("Sets default thread/engine creation options.
    The supported options are:
<DL>
    <DT>local(KBytes)</DT>
        <DD>Set size of local/control stack in kBytes.
	Equivalent to set_flag(default_localsize, kBytes).</DD>
    <DT>global(KBytes)</DT>
        <DD>Set size of global/trail stack in kBytes.
	Equivalent to set_flag(default_globalsize, kBytes).</DD>
</DL>
    "),
    see_also:[thread_create/2,engine_create/2,set_flag/2]
]).
:- export thread_set_default/1.
thread_set_default(local(X)) ?- !, set_flag(default_localsize, X).
thread_set_default(global(X)) ?- !, set_flag(default_globalsize, X).
thread_set_default(detached(X)) ?- !, throw(error(permission_error(modify,thread_option,X),_)).
thread_set_default(Option) :- must_be(thread_option, Option).


:- comment(thread_create/3, [
    amode:(thread_create(+,-,+) is det),
    args:["Goal":"A callable term",
    	"Thread":"Output: a handle for the created DTR-thread",
	"Options":"A list of option terms"],
    summary:"Create a DTR-thread",
    desc:html("<P>
    Create a thread (according to DTR 13211-5), and start it by executing
    (a copy of) Goal.  The following options are supported:
    <DL>
    <DT><B>alias(Name)</B></DT><DD>
        make the DTR-thread available under the alias Name (an atom).
    </DD>
    <DT><B>detached(Bool)</B></DT><DD>
        when true, free all DTR-thread resources on completion of Goal, and
	do not wait for thread_join/2
    </DD>
    <DT><B>all options of engine_create/2</B></DT><DD>
    </DD>
    </DL>
    </P><P>
    Relationship to ECLiPSe engines: a DTR-thread is an ECLiPSe engine
    associated with a record queue. So, thread_create/3 is essentially a
    combination of engine_create/2, engine_resume_thread/2 and record_create/1.
    </P>"),
    see_also:[engine_create/2,engine_resume_thread/2,record_create/1]
]).
:- export thread_create/3.
:- tool(thread_create/3, thread_create_/4).
thread_create_(Goal, Thread, Options, Module) :-
	must_be(callable, Goal),
	must_be(var, Thread),
	must_be(list(callable), Options),
	extract_aliases(Options, Aliases, Options1),
	engine_create(Thread, [thread|Options1]),
	make_aliases(Aliases, Thread, thread_aliases, thread),
	get_engine_property(Thread, store, Store),
	message_queue_create(Queue),
	store_set(Store, queue, Queue),
	engine_resume_thread(Thread, Goal)@Module.


:- comment(thread_create/2, [
    amode:(thread_create(+,-) is det),
    args:["Goal":"A callable term",
    	"Thread":"Output: a handle for the created DTR-thread"],
    summary:"Create a DTR-thread with default options",
    desc:html("<P>
    Equivalent to thread_create(Goal, Thread, []).
</P>"),
    see_also:[thread_create/3,engine_create/2]
]).
:- export thread_create/2.
:- tool(thread_create/2, thread_create_/3).
thread_create_(Goal, Thread, Module) :-
	thread_create_(Goal, Thread, [], Module).


:- comment(thread_create/1, [
    amode:(thread_create(+) is det),
    args:["Goal":"A callable term"],
    summary:"Create a detached DTR-thread with default options",
    desc:html("<P>
    Equivalent to thread_create(Goal, _, [detached(true)]).
</P>"),
    see_also:[thread_create/3,engine_create/2]
]).
:- export thread_create/1.
:- tool(thread_create/1, thread_create_/2).
thread_create_(Goal, Module) :-
	thread_create_(Goal, _, [detached(true)], Module).


:- comment(thread_join/2, [
    amode:(thread_join(+,-) is det),
    args:["Thread":"A DTR-thread alias or handle",
    	"Status":"Output: result status"],
    summary:"Wait for termination of Thread",
    desc:html("<P>
    Block until Thread terminates, and unify Status with its result status.
    For the possible status values, see get_engine_property/3, with the
    difference that here exited(Term) may contain an arbitrary term.
</P><P>
    After this call, the DTR-thread is destroyed and its handles and alias
    are invalid.
</P>"),
    see_also:[engine_join/3,get_engine_property/3,thread_create/3]
]).
:- export thread_join/2.
thread_join(ThreadId, Status) :-
	get_engine_handle(ThreadId, Thread),
	engine_join(Thread, block, EngineStatus),
	map_status(Thread, EngineStatus, Status),
	engine_post(Thread, exit(3)),
	delete_item(ThreadId, Thread, thread_aliases).

    map_status(Thread, EngineStatus, Status) :-
	(
	    EngineStatus = exited(_ExitCode),
	    get_engine_property(Thread, store, Store),
	    store_get(Store, exited, ExitTerm),	% may fail
	    store_erase(Store)
	->
	    Status = exited(ExitTerm)
	;
	    Status = EngineStatus
	).


:- comment(thread_exit/1, [
    amode:thread_exit(+),
    args:["Term":"A term"],
    summary:"Exit the executing DTR-thread with status exited(Term)",
    desc:html("<P>
    This compatibility predicate executes exit(3) and stores Term such
    that the result status exited(Term) can be retrieved via thread_join/2
    or thread_property/2.
</P>"),
    see_also:[exit/1,thread_join/2,thread_property/2]
]).
:- export thread_exit/1.
thread_exit(ExitTerm) :-
	must_be(nonvar, ExitTerm),
	engine_self(Self),
	get_engine_property(Self, store, Store),
	store_set(Store, exited, ExitTerm),
	exit(3).			% exit/1 accepts only integers


:- comment(thread_self/1, [
    amode:(thread_self(-) is det),
    amode:(thread_self(+) is semidet),
    args:["Thread":"A variable, DTR-thread alias or thread/engine handle"],
    summary:"True if Thread is the calling DTR-thread",
    desc:html("<P>
    If Thread is uninstantiated, it is unified with a thread/engine handle
    for the calling thread.  If Thread is a DTR-thread alias or a thread/engine
    handle for the calling thread, the predicate succeeds, otherwise fails.
</P><P>
    Apart from alias handling, this is the same as engine_self/1.
</P>"),
    see_also:[engine_self/1]
]).
:- export thread_self/1.
thread_self(Thread) :- var(Thread), !,
	engine_self(Thread).
thread_self(ThreadId) :-
	get_engine_handle(ThreadId, Thread),
	engine_self(Thread).


:- comment(thread_sleep/1, [
    amode:(thread_sleep(+) is det),
    args:["Seconds":"A float or integer"],
    summary:"Suspends the calling thread/engine for Seconds seconds",
    desc:html("<P>
    Equivalent to sleep(Seconds).
</P>"),
    see_also:[sleep/1]
]).
:- export thread_sleep/1.
thread_sleep(Seconds) :-
	sleep(Seconds).


:- comment(thread_signal/2, [
    amode:(thread_signal(+,+) is det),
    args:["Thread":"A DTR-thread handle or alias",
    	"Goal":"A callable term"],
    summary:"Makes Thread execute Goal at the next opportunity",
    desc:html("<P>
    Equivalent to engine_post(Thread, Goal).
</P>"),
    see_also:[engine_post/2]
]).
:- export thread_signal/2.
:- tool(thread_signal/2, thread_signal_/3).
thread_signal_(ThreadId, Goal, Module) :-
	must_be(callable, Goal),
	get_engine_handle(ThreadId, Thread),
	engine_post(Thread, Goal)@Module.


:- comment(thread_cancel/1, [
    amode:(thread_cancel(+) is det),
    args:["Thread":"A DTR-thread handle or alias"],
    summary:"Makes Thread exit at the next opportunity",
    desc:html("<P>
    Equivalent to engine_post(Thread, exit(3)) plus deletion of
    Thread's alias.
</P>"),
    see_also:[engine_post/2]
]).
:- export thread_cancel/1.
thread_cancel(ThreadId) :-
	get_engine_handle(ThreadId, Thread),
	engine_post(Thread, exit(3)),
	% we make the thread no longer joinable (not clear from spec)
	delete_item(ThreadId, Thread, thread_aliases).


:- comment(thread_detach/1, [
    amode:(thread_detach(+) is det),
    args:["Thread":"A DTR-thread alias or handle"],
    summary:"Thread ",
    see_also:[engine_post/2]
]).
:- export thread_detach/1.
thread_detach(ThreadId) :-
	get_engine_handle(ThreadId, Thread),
%	engine_post(Thread, set_flag(detached,true)),
	delete_item(ThreadId, Thread, thread_aliases).


:- comment(thread_property/2, [
    amode:(thread_property(-,-) is multi),
    amode:(thread_property(+,-) is multi),
    amode:(thread_property(+,++) is semidet),
    args:["Thread":"A thread/engine handle or DTR-thread alias, or variable",
    	"Property":"Variable or compound term"],
    summary:"Enumerates threads and their properties",
    desc:html("<P>
    If Thread is a thread/engine handle, enumerates the thread/engine's
    properties and unifies them with Property.  If Thread is a variable,
    enumerates all currently existing thread/engines  and their properties.
    The properties are as defined in get_engine_property/3, plus the
    alias(Name) option from thread_create/3.
</P>"),
    see_also:[get_engine_property/3,thread_create/3]
]).
:- export thread_property/2.
thread_property(ThreadId, Property) :-
	( var(ThreadId) ->
	    current_engines(Es),
	    member(Thread, Es),
	    ThreadId = Thread
	;
	    get_engine_handle(ThreadId, Thread)
	),
	get_engine_property(Thread, all, Properties),
	(
	    member(Property0, Properties),
	    ( Property0 = status(EngineStatus) ->
		Property = status(Status),
		map_status(Thread, EngineStatus, Status)
	    ;
		Property = Property0
	    )
	;
	    Property = alias(Alias),
	    stored_keys_and_values(thread_aliases, AsEs),
	    member(Alias-Thread, AsEs)
	).


% Message queues

:- comment(message_queue_create/1, [
    amode:(message_queue_create(-) is det),
    args:["Queue":"Output: queue handle"],
    summary:"Creates a DTR message queue",
    desc:html("<P>
    Equivalent to message_queue_create(Queue,[]) and record_create(Queue).
</P>"),
    see_also:[record_create/1,message_queue_create/2]
]).
:- export message_queue_create/1.
message_queue_create(Queue) :-
	must_be(var, Queue),
	record_create(Queue).
	

:- comment(message_queue_create/2, [
    amode:(message_queue_create(-,++) is det),
    args:["Queue":"Output: queue handle",
    	"Options":"A list of Options"],
    summary:"Creates a DTR message queue",
    desc:html("<P>
    Creates a message queue with options.  The only supported option
    is alias(Name).  Otherwise equivalent to record_create(Queue).
</P>"),
    see_also:[record_create/1,message_queue_create/1]
]).
:-export message_queue_create/2.
message_queue_create(Queue, Options) :-
	must_be(var, Queue),
	must_be(list(callable), Options),
	extract_aliases(Options, Aliases, Options1),
	( Options1=[Option|_] -> domain_error(queue_option, Option) ; true ),
	record_create(Queue),
	make_aliases(Aliases, Queue, thread_aliases, queue).
	

:- comment(message_queue_destroy/1, [
    amode:(message_queue_destroy(+) is det),
    args:["Queue":"Queue handle or alias"],
    summary:"Destroy a DTR message queue",
    see_also:[message_queue_create/1,message_queue_create/2]
]).
:-export message_queue_destroy/1.
message_queue_destroy(QueueId) :-
	get_queue_handle(QueueId, Queue),
	delete_item(QueueId, Queue, thread_aliases).


% SWI versions with timeout:
% - no options: wait forever
% - timeout>0:  wait max time
% - timeout=0:  do not wait
% - timeout<0:  fail

:- comment(thread_send_message/2, [
    amode:(thread_send_message(+,?) is det),
    args:["Queue":"A DTR-thread or DTR-queue handle or alias",
    	"Message":"Any term"],
    summary:"Sends a message via Queue",
    desc:html("<P>
    Essentially equivalent to record_wait_append4/4.
</P>"),
    see_also:[record_wait_append/4,message_queue_create/2]
]).
:- export thread_send_message/2.
thread_send_message(QueueId, Msg) :-
	get_queue_handle(QueueId, Queue),
	record_wait_append(Queue, Msg, block, 9999999).


:- comment(thread_get_message/2, [
    amode:(thread_get_message(+,?) is det),
    args:["Queue":"A DTR-thread or DTR-queue handle or alias",
    	"Message":"Message term, possibly partially instantiated"],
    summary:"Wait for and remove a message from Queue",
    desc:html("<P>
    Waits until Queue contains an entry that unifies with Message,
    then removes and unifies this message with Message.
</P><P>
    Essentially equivalent to record_wait_remove/3.
</P>"),
    see_also:[record_wait_remove/3,message_queue_create/2]
]).
:- export thread_get_message/2.
thread_get_message(QueueId, Msg) :-
	get_queue_handle(QueueId, Queue),
	record_wait_remove(Queue, Msg, block).


:- comment(thread_peek_message/2, [
    amode:(thread_peek_message(+,-) is det),
    args:["Queue":"A DTR-thread or DTR-queue handle or alias",
    	"Message":"Message term, possibly partially instantiated"],
    summary:"Check for Message in Queue",
    desc:html("<P>
    Unifies Message with (a copy of) an entry in Queue, if possible.
</P><P>
    Essentially equivalent to recorded/2.
</P>"),
    see_also:[record_wait_remove/3,message_queue_create/2]
]).
:- export thread_peek_message/2.
thread_peek_message(QueueId, Msg) :-
	get_queue_handle(QueueId, Queue),
	recorded(Queue, Msg), !.


:- comment(thread_send_message/1, [
    amode:(thread_send_message(?) is det),
    args:["Message":"Any term"],
    summary:"Sends a message via the calling DTR-thread's message queue",
    see_also:[thread_send_message/2]
]).
:- export thread_send_message/1.
thread_send_message(Msg) :-
	engine_self(QueueId),
	thread_send_message(QueueId, Msg).


:- comment(thread_get_message/1, [
    amode:(thread_get_message(?) is det),
    args:["Message":"Message term, possibly partially instantiated"],
    summary:"Wait for and remove a message from the calling DTR-thread's message queue",
    see_also:[thread_get_message/2]
]).
:- export thread_get_message/1.
thread_get_message(Msg) :-
	engine_self(QueueId),
	thread_get_message(QueueId, Msg).


:- comment(thread_peek_message/1, [
    amode:(thread_peek_message(-) is det),
    args:["Message":"Message term, possibly partially instantiated"],
    summary:"Check for Message in the calling DTR-thread's message queue",
    see_also:[thread_peek_message/2]
]).
:- export thread_peek_message/1.
thread_peek_message(Msg) :-
	engine_self(QueueId),
	thread_peek_message(QueueId, Msg).


% Mutex

:- comment(mutex_create/2, [
    amode:(mutex_create(-,++) is det),
    args:["Mutex":"Output: mutex handle",
    	"Options":"A list of Options"],
    summary:"Creates a mutual exclusion object",
    see_also:[mutex_create/1,mutex_destroy/1,with_mutex/2]
]).
:- export mutex_create/2.
mutex_create(Mutex, Options) :-
	must_be(var, Mutex),
	must_be(list(callable), Options),
	extract_aliases(Options, Aliases, Options1),
	( Options1=[Option|_] -> domain_error(mutex_option, Option) ; true ),
	bag_create(Mutex),
	make_aliases(Aliases, Mutex, mutex_aliases, mutex).


:- comment(mutex_create/1, [
    amode:(mutex_create(?) is det),
    args:["Mutex":"Mutex handle (output) or alias"],
    summary:"Creates a mutual exclusion object with alias",
    see_also:[mutex_create/2,mutex_destroy/1,with_mutex/2]
]).
:- export mutex_create/1.
mutex_create(Mutex) :-
	( atom(Mutex) ->
	    bag_create(Handle),
	    make_aliases([Mutex], Handle, mutex_aliases, mutex)
	;
	    mutex_create(Mutex, [])
	).


:- comment(mutex_destroy/1, [
    amode:(mutex_destroy(+) is det),
    args:["Mutex":"Mutex handle or alias"],
    summary:"Destroys a mutual exclusion object",
    see_also:[mutex_create/1,mutex_create/2,with_mutex/2]
]).
:- export mutex_destroy/1.
mutex_destroy(MutexId) :-
	get_mutex_handle(MutexId, Handle),
	delete_item(MutexId, Handle, mutex_aliases).


:- export with_mutex/2.
:- export initialization(import with_mutex/2 from threads).	% resolve ambiguity
:- tool(with_mutex/2, with_mutex_/3).
:- comment(with_mutex/2, [
    amode:with_mutex(+,+),
    args:["Mutex":"Mutex handle or alias",
	  "Goal":"A callable term"],
    summary:"Equivalent to once(Goal) but with mutual exclusion",
    desc:html("<P>
    Equivalent to the with_mutex/2 built-in predicate, but also accepts
    an atomic mutex alias.
</P>"),
    see_also:[mutex_create/1,mutex_create/2,_:with_mutex/2]
]).
with_mutex_(MutexId, Goal, Module) :-
	get_mutex_handle(MutexId, Handle),
	eclipse_language:with_mutex(Handle, Goal)@Module.



% Auxiliaries

get_engine_handle(Handle, Engine) :- is_handle(Handle, engine), !,
	Engine=Handle.
get_engine_handle(Alias, Engine) :- atom(Alias), !,
	( store_get(thread_aliases, Alias, Engine) -> true
	; existence_error(thread, Alias)
	).
get_engine_handle(Other, _Engine) :-
	( var(Other) -> instantiation_error(Other)
	; domain_error(thread_or_alias, Other)
	).


get_queue_handle(Handle, Queue) :- is_handle(Handle, record), !,
	Queue=Handle.
get_queue_handle(Handle, Queue) :- is_handle(Handle, engine), !,
	get_engine_property(Handle, store, Store),
	store_get(Store, queue, Queue).
get_queue_handle(Alias, Queue) :- atom(Alias), !,
	( store_get(thread_aliases, Alias, Handle) ->
	    get_queue_handle(Handle, Queue)
	; existence_error(queue, Alias)
	).
get_queue_handle(Other, _Queue) :-
	( var(Other) -> instantiation_error(Other)
	; domain_error(queue_or_alias, Other)
	).


get_mutex_handle(Handle, Mutex) :- is_handle(Handle, bag), !,
	Mutex = Handle.
get_mutex_handle(Alias, Mutex) :- atom(Alias), !,
	( store_get(mutex_aliases, Alias, Mutex) -> true
	; existence_error(mutex, Alias)
	).
get_mutex_handle(Other, _Mutex) :-
	( var(Other) -> instantiation_error(Other)
	; domain_error(mutex_or_alias, Other)
	).


extract_aliases(Options, Aliases, OtherOptions) :-
	(
	    foreach(Option,Options),
	    fromto(OtherOptions,Options2,Options3,[]),
	    fromto([],Aliases2,Aliases3,Aliases)
	do
	    ( Option = alias(Alias) ->
		must_be(atom, Alias),
% not using this (allow only one alias)
%		Options2=Options3, Aliases3=[Alias|Aliases2]
		( Aliases2 == [] ->
		    Options2=Options3, Aliases3=[Alias]
		;
		    permission_error(create,alias,Alias)
		)
	    ;
		Options2=[Option|Options3], Aliases3=Aliases2
	    )
	).

make_aliases(Aliases, Handle, StoreName, ObjectName) :-
	( foreach(Alias,Aliases), param(Handle,StoreName,ObjectName) do
	    ( store_insert(StoreName, Alias, Handle) -> true ;
		permission_error(create, ObjectName, alias(Alias))
	    )
	).

delete_item(Alias, Handle, StoreName) :-
	( atom(Alias) -> store_delete(StoreName, Alias) ; true ),
% not using this (assuming only one alias)
%	stored_keys_and_values(StoreName, KVs),
%	( member(Alias-Handle, KVs), store_delete(StoreName, Alias), fail ; true ),
	handle_close(Handle).
	
