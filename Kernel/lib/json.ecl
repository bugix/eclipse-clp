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
% The Original Code is  The ECLiPSe JSON Library
% The Initial Developer of the Original Code is  Coninfer Ltd
% Portions created by the Initial Developer are
% Copyright (C) 2019 Coninfer Ltd.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK

:- module(json).

:- comment(categories, ["Interfacing"]).
:- comment(summary, "Read and write JSON format").
:- comment(author, "Joachim Schimpf").
:- comment(copyright, "Joachim Schimpf, Coninfer Ltd").
:- comment(date, "$2019$").

% Ideas:
% json_read:  null_as_var, object_as_hash
% json_write: var_as_null, struct_as_object
%
% Extension to write/read Prolog terms:
%   { "$name":"foo", "$args":[...] }
% Extension to write/read named structs:
%   { "$name":"foo", "fieldname1":..., ... }


:- lib(hash).


:- comment(json_read/3, [
    summary:"Read a JSON element from a stream",
    args:["Stream":"Input stream",
        "JsonTerm":"Term describing JSON object read",
        "Options":"List of options"],
    amode:json_read(+,-,+),
    desc:html("
    Read a JSON element.  In the resulting data structure, JSON elements are
    represented as follows:
    <DL>
    <DT>object</DT>
        <DD>Structure with functor {}/1 whose single argument is a list of
        Name:Value terms, where Name is a string and Value a JSON element,
        e.g. <CODE>{[\"a\":123,\"b\":[4,5]]}</CODE>, or the empty object <CODE>{}</CODE>.</DD>
    <DT>array</DT>
        <DD>A list whose elements are JSON elements, e.g. <CODE>[1,{\"a\":true}]</CODE></DD>
    <DT>string</DT>
        <DD>A string</DD>
    <DT>number</DT>
        <DD>An integer or float</DD>
    <DT>true</DT>
        <DD>the atom <CODE>true</CODE></DD>
    <DT>false</DT>
        <DD>the atom <CODE>false</CODE></DD>
    <DT>null</DT>
        <DD>the atom <CODE>null</CODE></DD>
    </DL>
    Options is a list that can contain
    <DL>
    <DT>names_as_atoms</DT>
        <DD>return object key names as atoms instead of strings</DD>
    </DL>
    "),
    see_also:[json_write/3],
    eg:"
    ?- json_read(input, JsonTerm, []).
     {
       \"foo\":123,
       \"bar\":[\"hello\",3,4.5],
       \"baz\":true,
       \"zut\":null
     }
     
    JsonTerm = {[\"foo\":123, \"bar\":[\"hello\",3,4.5], \"baz\":true, \"zut\":null]}
    Yes (0.00s cpu)


    ?- json_read(input, JsonTerm, [names_as_atoms]).
     {
       \"foo\":123,
       \"bar\":\"hello\",
       \"baz\":true
     }
     
    JsonTerm = {[foo:123, bar:\"hello\", baz:true]}
    Yes (0.00s cpu)


    ?- json_read(input, JsonTerm, []).
     foobar

     syntax error: invalid JSON syntax in json_read / 3
    Abort
"]).
:- export json_read/3.
json_read(Stream, Json, Options) :-
        read(Stream, Json0)@json_syntax,
        (
            ( memberchk(names_as_atoms, Options) ->
                json_atomify(Json0, Json1)
            ;
                json_term(Json0), Json1=Json0
            )
        ->
            Json=Json1
        ;
            throw(error(syntax_error("invalid JSON syntax"),json_read/3))
        ).

% Use this to test result of read@json_syntax
json_term(X) :- var(X), !, fail.
json_term({}).
json_term({Xs}) :- json_members(Xs). % parser guarantees proper list!
json_term([]).
json_term(Xs) :- Xs=[_|_], json_terms(Xs).
json_term(String) :- string(String).
json_term(Int) :- integer(Int).
json_term(Float) :- float(Float).
json_term(true).
json_term(false).
json_term(null).

    json_terms([]) ?- true.
    json_terms([X|Xs]) ?- json_term(X), json_terms(Xs).

    json_members([]) ?- true.
    json_members([Name:X|Xs]) ?- string(Name), json_term(X), json_members(Xs).


% Convert all JSON field keys from strings to atoms.
% Do all the checks that json_term/1 does, failing for invalid terms.
:- mode json_atomify(?,-).
json_atomify(X, _) :- var(X), !, fail.
json_atomify({}, {}).
json_atomify({Xs}, {Ys}) :- json_atomify_members(Xs, Ys).
json_atomify([], []).
json_atomify(Xs, Ys) :- Xs=[_|_], json_atomify_terms(Xs, Ys).
json_atomify(String, String) :- string(String).
json_atomify(Int, Int) :- integer(Int).
json_atomify(Float, Float) :- float(Float).
json_atomify(true, true).
json_atomify(false, false).
json_atomify(null, null).

    json_atomify_terms([], Y) ?- Y=[].
    json_atomify_terms([X|Xs], YYs) ?- YYs=[Y|Ys],
        json_atomify(X, Y),
        json_atomify_terms(Xs, Ys).

    json_atomify_members([], Y) ?- Y=[].
    json_atomify_members([NameS:X|Xs], YYs) ?-
        string(NameS), atom_string(Name, NameS),
        YYs=[Name:Y|Ys],
        json_atomify(X, Y),
        json_atomify_members(Xs, Ys).


:- comment(json_write/3, [
    summary:"Write a JSON element to a stream",
    args:["Stream":"Output stream",
          "JsonTerm":"Term describing JSON object to write",
          "Options":"List of options"],
    amode:json_write(+,++,+),
    desc:html("
    Write a JSON element.  JSON elements are described by an ECLiPSe term
    whose structure mimics the JSON element, as follows:
    <DL>
    <DT>object</DT>
        <DD>Structure with functor {}/1 whose single argument is either
        a comma-sequence of Name:Value terms, a list of Name:Value terms,
        or a single Name:Value term.  Name is a string or atom, and Value
        is another JSON element.  E.g. <CODE>{[\"a\":123,\"b\":[4,5]]}</CODE>
        or <CODE>{\"a\":123,\"b\":[4,5]}</CODE>.
        The empty object <CODE>{}</CODE> is also allowed.
    <DT>array</DT>
        <DD>A list or array whose elements are JSON elements, e.g.
        <CODE>[1,{\"a\":true}]</CODE> or <CODE>[](1,{\"a\":true})</CODE></DD>
    <DT>string</DT>
        <DD>A string or atom</DD>
    <DT>number</DT>
        <DD>An integer or float</DD>
    <DT>true</DT>
        <DD>the atom <CODE>true</CODE></DD>
    <DT>false</DT>
        <DD>the atom <CODE>false</CODE></DD>
    <DT>null</DT>
        <DD>the atom <CODE>null</CODE></DD>
    </DL>
    Options is a list that can contain
    <DL>
    <DT>indent(N) - default 1</DT>
        <DD>Indent each structure level by N spaces.
        indent(0) suppresses all extra spaces and newlines.</DD>
    <DT>float_format(String) - default \"%q\"</DT>
        <DD>use the given printf-format to print floating point numbers,
        e.g. \"%.3f\"</DD>
    </DL>
    "),
    see_also:[json_read/3],
    eg:"
    ?- json_write(output, [1, 2.3, foo, \"bar\"], []).
    [
     1,
     2.3,
     \"foo\",
     \"bar\"
    ]

    ?- json_write(output, {foo:123, \"bar\":4.5, baz:true}, []).
    {
     \"foo\":123,
     \"bar\":4.5,
     \"baz\":true
    }

    ?- json_write(output, {[foo:123, \"bar\":4.5, baz:true]}, []).
    {
     \"foo\":123,
     \"bar\":4.5,
     \"baz\":true
    }

    ?- json_write(output, hello(world), []),
    type error: expected json_term, found hello(world) in json_write / 3
    Abort
"]).
:- export json_write/3.
json_write(Stream, Term, Options) :-
        ( memberchk(indent(II), Options) -> true ; II=1 ),
        ( II==0 -> NL="" ; get_stream_info(Stream, end_of_line, crlf) -> NL="\r\n" ; NL="\n" ),
        ( memberchk(float_format(FF), Options) -> string(FF) ; FF="%q" ),
        json_write(Stream, Term, FF, 0, II, NL),
        write(Stream, NL).
        
json_write(_S, X, _FF, _I, _II, _NL) :- var(X), !,
        throw(error(instantiation_error,json_write/3)).
json_write(S, {Xs}, FF, I, II, NL) :- !,
        I1 is I+II,
        ( is_list(Xs) ->
            ( foreach(Name:X,Xs), fromto("{",Sep,",",_), param(S,FF,I1,II,NL) do
                text_to_string(Name, NameS),
                printf(S, "%s%s%*c%q:", [Sep,NL,I1,0' ,NameS]),
                json_write(S, X, FF, I1, II, NL)
            )
        ;
            % alternatively, allow comma-term
            ( fromto(Xs,Xs1,Xs2,_), fromto("{",Sep,Sep1,""), param(S,FF,I1,II,NL) do
                ( (Name:X,Xs2) = Xs1 -> Sep1=","
                ; (Name:X) = Xs1, Sep1="" ),
                text_to_string(Name, NameS),
                printf(S, "%s%s%*c%q:", [Sep,NL,I1,0' ,NameS]),
                json_write(S, X, FF, I1, II, NL)
            )
        ),
        printf(S, "%s%*c%s", [NL,I,0' ,"}"]).
json_write(S, {}, _FF, _I, _II, _NL) :- !,
        write(S, {}).
json_write(S, [], _FF, _I, _II, _NL) :- !,
        write(S, []).
json_write(S, true, _FF, _I, _II, _NL) :- !,
        write(S, true).
json_write(S, false, _FF, _I, _II, _NL) :- !,
        write(S, false).
json_write(S, null, _FF, _I, _II, _NL) :- !,
        write(S, null).
json_write(S, Xs, FF, I, II, NL) :- is_list(Xs), !,
        I1 is I+II,
        ( foreach(X,Xs), fromto("[",Sep,",",_), param(S,FF,I1,II,NL) do
            printf(S, "%s%s%*c", [Sep,NL,I1,0' ]),
            json_write(S, X, FF, I1, II, NL)
        ),
        printf(S, "%s%*c%s", [NL,I,0' ,"]"]).
json_write(S, Xz, FF, I, II, NL) :- is_array(Xz), !,
        I1 is I+II,
        ( foreacharg(X,Xz), fromto("[",Sep,",",_), param(S,FF,I1,II,NL) do
            printf(S, "%s%s%*c", [Sep,NL,I1,0' ]),
            json_write(S, X, FF, I1, II, NL)
        ),
        printf(S, "%s%*c%s", [NL,I,0' ,"]"]).
json_write(S, X, _FF, _I, _II, _NL) :- atom(X), !,
        atom_string(X, String),
        writeq(S, String).
json_write(S, X, _FF, _I, _II, _NL) :- string(X), !,
        writeq(S, X).
json_write(S, X, _FF, _I, _II, _NL) :- integer(X), !,
        writeq(S, X).
json_write(S, X, FF, _I, _II, _NL) :- float(X), !,
        printf(S, FF, [X]).
json_write(_S, Other, _FF, _I, _II, _NL) :-
        throw(error(type_error(json_term,Other), json_write/3)).


/*
% Encode a Prolog term such that it can be read back as with write_canonical.
%
%    Variable: {"type":"var", "name":<string>}
%    Atom: {"type":"atom", "value":<string>}
%    Integer: {"type":"integer", "value":<integer>}
%    Float: {"type":"float", "value":<float>}
%    List: JSON array
%    Dict: a JSON object. Values are processed recursively. (the tag is ignored)
%    json([Key=Value, ...]): a JSON object Values are processed recursively.
%    compound: {"type":"compound", "functor":<string>, "args":<array>}

:- export json_encode/2.
json_encode(X, J) :-
        term_variables(X, Vs),
        ( foreach(V,Vs), foreach(Name=V,VNs), count(I,1,_) do
            concat_atom(["_",I], Name)
        ),
        json_encode(X, J, [variable_names(VNs)]).

:- export json_encode/3.
json_encode(X, J, VNs) :- var(X),  !, J = {["type":"var", "name":Name]},
        term_string(X, Name, VNs).
json_encode(X, J, _) :- string(X), !, J = {["type":"string", "value":X]}.
json_encode(X, J, _) :- integer(X),!, J = {["type":"integer", "value":X]}.
json_encode(X, J, _) :- float(X),  !, J = {["type":"float", "value":X]}.
json_encode(X, J, _) :- atom(X),   !, J = {["type":"atom", "value":S]},
        atom_string(X, S).
json_encode(Xs, Js, VNs) :- is_list(Xs), !,
        ( foreach(X,Xs), foreach(J,Js), param(VNs) do
            json_encode(X, J, VNs)
        ).
json_encode(Term, J, VNs) :- compound(Term), !,
        J = {["type":"compound", "functor":FS, "args":Js]},
        functor(Term, F, _),
        atom_string(F, FS),
        ( foreacharg(X,Term), foreach(J,Js), param(VNs) do
            json_encode(X, J, VNs)
        ).
json_encode(X, {["type":TS, "value":XS]}, VNs) :-
        type_of(X, T),  % rational, breal, and everything else
        atom_string(T, TS),
        term_string(X, XS, VNs).


:- export json_decode/2.
json_decode(J, Term) :-
        hash_create(VMap),
        json_decode(J, Term, VMap).

json_decode({"type":"integer","value":Val}, Term, _) :- !, Term=Val.
json_decode({"type":"float","value":Val}, Term, _) :- !, Term=Val.
json_decode({"type":"string","value":Val}, Term, _) :- !, Term=Val.
json_decode({"type":"atom","value":Val}, Term, _) :- !, atom_string(Term, Val).
json_decode({"type":"var","name":Name}, Term, VMap) :- !,
        ( hash_get(VMap, Name, Var) -> Term=Var
        ; hash_set(VMap, Name, Term)
        ).
json_decode(Js, Terms, VMap) :- is_list(Js), !,
        ( foreach(J,Js), foreach(T,Terms), param(VMap) do
            json_decode(J, T, VMap)
        ).
json_decode({"type":"compound", "functor":FS, "args":Js}, Term, VMap) :- !,
        atom_string(F, FS),
        length(Js, N),
        functor(Term, F, N),
        ( foreacharg(X,Term), foreach(J,Js), param(VMap) do
            json_decode(J, X, VMap)
        ).
json_decode({"type":_, "value":Val}, Term, _VMap) :-
        term_string(Term, Val).
*/

% ----------------------------------------------------------------------
:- module(json_syntax).
% ----------------------------------------------------------------------

% All syntax settings are done as initialisation
% to avoid problems while parsing this file itself!

:- local initialization((

    (
	between(32, 255, 1, Char),
	get_chtab(Char, upper_case),
	local(chtab(Char, lower_case)),
	fail
    ;
	true
    ),

    local(chtab(0'-, solo)),    % to allow e.g. "foo":-1 without spaces

    local(chtab(8, underline)),		% we need a (dummy) underline character
    local(chtab(0'_, lower_case)),	% identifiers can start with underscore

    local(syntax_option(not(syntax_errors_fail))),       % throw syntax_error
    local(syntax_option(plus_is_no_sign)),               % JSON
    local(syntax_option(not(float_needs_point))),        % JSON
    local(syntax_option(curly_args_as_list)),            % for convenience

    % hide all global operator definitions
    (
	current_op(_, A, Op),
	local(op(0, A, Op)),
	fail
    ;
	true
    ),

    local(op(800, xfx, (','))),
    local(op(800, xfx, (:))),

    % disable struct notation macros
    local(macro((with)/2, (=)/2, [])),
    local(macro((of)/2, (=)/2, []))

    )).

