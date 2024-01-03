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
% The Original Code is  The csv library for ECLiPSe.
% The Initial Developer of the Original Code is  Joachim Schimpf.
% Portions created by the Initial Developer are
% Copyright (C) 2010.  All Rights Reserved.
% 
% END LICENSE BLOCK

%
% Utilities for working with comma-separated format (csv).
%
% We have used as reference http://www.rfc-editor.org/rfc/rfc4180.txt
%
% Grammar from rfc4180:
%
%   file = [header CRLF] record *(CRLF record) [CRLF]
%   header = name *(COMMA name)
%   record = field *(COMMA field)
%   name = field
%   field = (escaped / non-escaped)
%   escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
%   non-escaped = *TEXTDATA
%   COMMA = %x2C
%   CR = %x0D ;as per section 6.1 of RFC 2234 [2]
%   DQUOTE =  %x22 ;as per section 6.1 of RFC 2234 [2]
%   LF = %x0A ;as per section 6.1 of RFC 2234 [2]
%   CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]
%   TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
%
% Notes:
% - Line ends are supposed to be CRLF, but e.g. OpenOffice has only LF,
%   so we allow that as well.
% - Can have optional header line with field names (special file type),
%   but there is no way of indicating this in the format
% - Spaces are part of the field (not sure about ..., "a" ,...)
% - Fields containing line breaks (CRLF), double quotes, and commas
%       should be enclosed in double-quotes
% - Otherwise no double quotes allowed
%

:- module(csv).

:- comment(summary, "Utilities to manipulate comma-separated (csv) format").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2015/01/14 01:31:08 $").
:- comment(copyright, "2010 by author").
:- comment(categories, ["Interfacing"]).

:- export struct(csv_options(
        strip,
        convert,
        type
)).

:- lib(module_options).

valid_option_field(strip, strip of csv_options).
valid_option_field(convert, convert of csv_options).
valid_option_field(type, type of csv_options).

valid_option_value(strip, Value) :- (Value==true;Value==false), !.
valid_option_value(convert, Value) :- (Value==true;Value==false), !.
valid_option_value(type, Value) :- ( Value==list ; functor(Value,F,N), atom(F), integer(N) ), !.

default_options(csv_options{strip:false,convert:true,type:list}).


:- comment(csv_read/3, [
    summary:"Read a file containing comma separated values (csv format)",
    args:["File":"File name (string or atom)",
        "Rows":"List of lists or structures (output)",
        "Options":"List of options"],
    amode:(csv_read(+,-,+) is det),
    desc:html("<p>
        Reads a file containing comma separated values, and returns the
        file content as a list.  The file may have an optional .csv suffix.
    </p><p>
        The result list contains one element for each record/row in the
        file.  By default, each list element is itself a list, containing
        the row's field values.  Alternatively, the type-option can be used
        to return structures instead of lists.
    </p><p>
        The data elements are strings, unless they can be interpreted as
        numbers (by ECLiPSe's number_string/2 predicate) and the 'convert'
        option is true (the default).
    </p><p>
        Options are:
        <dl>
        <dt>convert:Bool (default true)</dt>
            <dd>If a field can be converted to an integer or float using
            number_string/2, return this number.  Otherwise return a string.</dd>
        <dt>strip:Bool (default false)</dt>
            <dd>Strip leading and trailing space from the field value.
            By default this is part of the data.</dd>
        <dt>type:Type (default 'list')</dt>
            <dd>Data type of returned rows: 'list' for a list of fields,
            name/arity for terms with this functor (arity can be left
            uninstantiated).</dd>
        </dl>
        
    </p>
    "),
    see_also:[number_string/2,csv_read_row/3],
    eg:"
    % Given file data.csv containing the line:
    % a, b, 123, c d,\" e f \"

    ?- csv_read(\"data.csv\", Rows, []).
    Rows = [[\"a\", \" b\", 123, \" c d\", \" e f \"]]

    ?- csv_read(\"data.csv\", Rows, [strip:true, convert:false]).
    Rows = [[\"a\", \"b\", \"123\", \"c d\", \" e f \"]]

    ?- csv_read(\"data.csv\", Rows, [strip:true, type:row/N]).
    Rows = [row(\"a\", \"b\", 123, \"c d\", \" e f \")]
    N = 5
"
]).

:- export csv_read/3.
csv_read(File, Rows, OptionList) :-
        ( get_options(OptionList, Options) ->
            ( existing_file(File, ["",".csv"], [readable], XFile) ->
                open(XFile, read, S),
                ( read_string(S, end_of_file, _, String) ->
                    close(S),
                    % add a terminating LF, if it is missing...
                    string_length(String, Len),
                    ( substring(String, "\012", Len) ->
                        string_list(String, Line, utf8)
                    ;
                        append_strings(String, "\012", StringLf),
                        string_list(StringLf, Line, utf8)
                    ),
                    ( bom_records(Rows, Options, Line, []) ->
                        true
                    ;
                        printf(error, "Invalid csv file: %w%n", [File]),
                        abort
                    )
                ;
                    close(S),
                    Rows = []
                )
            ;
                %throw(error(existence_error(file,File), csv_read/2))
                printf(error, "No such file: %w%n", [File]),
                abort
            )
        ;
            printf(error, "Invalid option list: %w%n", [OptionList]),
            print_default_options(error),
            abort
        ).


:- comment(csv_options/2, [
    summary:"Precompile an option list for csv_read_row/3.",
    args:["Options":"Variable to be bound to csv_options/2 structure",
        "OptionList":"List of options"],
    amode:(csv_options(-,+) is det),
    see_also:[csv_read_row/3,struct(csv_options)]
]).
:- export csv_options/2.    % SWI-compatible
csv_options(Options, OptionList) :-
        ( get_options(OptionList, Options0) ->
            Options = Options0
        ;
            printf(error, "Invalid option list: %w%n", [OptionList]),
            print_default_options(error),
            abort
        ).


:- comment(csv_read_row/3, [
    summary:"Read a line containing comma separated values (csv format)",
    args:["Stream":"Input stream",
        "Row":"List or structure (output)",
        "Options":"List of options, or cvs_options-structure"],
    amode:(csv_read_row(+,-,+) is det),
    desc:html("<p>
        Reads a single row from Stream containing comma separated values.
    </p><p>
        By default, the result is a list containing the row's field values.
        Alternatively, the type-option can be used to return a structure
        instead of list.
    </p><p>
        For other details see csv_read/3.
    </p><p>
        If csv_read_row/3 is called numerous times with complex options,
        the option list should be precompiled using csv_options/2, and
        the resulting structure passed as Options argument.
    </p>
    "),
    see_also:[csv_read/3,csv_options/2],
    eg:"
    % Given file data.csv containing the line:
    % a, b, 123, c d,\" e f \"
    % 99, aa, bb, 123, C d

    ?- open(\"data.csv\", read, S),
       csv_read_row(S, Row1, []),
       csv_read_row(S, Row2, []).
    Row1 = [\"a\", \" b\", 123, \" c d\", \" e f \"]
    Row2 = [99, \" aa\", \" bb\", 123, \" C d\"]

    ?- open(\"data.csv\", read, S),
       csv_options(Options, [strip:true, type:row/_]),
       csv_read_row(S, Row1, Options),
       csv_read_row(S, Row2, Options).
    Row1 = row(\"a\", \"b\", 123, \"c d\", \"e f\")
    Row2 = row(99, \"aa\", \"bb\", 123, \"C d\")
"
]).

:- export csv_read_row/3.
csv_read_row(S, Row, Options) :-
        nonvar(Options), Options = csv_options{}, !,
        ( read_string(S, end_of_line, _, String) ->
            csv_read_row_retry(S, Row, String, Options)
        ;
            Row = end_of_file
        ).
csv_read_row(Stream, Row, OptionList) :-
        ( get_options(OptionList, Options) ->
            csv_read_row(Stream, Row, Options)
        ;
            printf(error, "Invalid option list: %w%n", [OptionList]),
            print_default_options(error),
            abort
        ).

csv_read_row_retry(S, Row, String, Options) :-
        string_list(String, Line, utf8),
        ( catch(bom_single_record(Fields, Options, Line, []), escaped_newline_found, (
                read_string(S, end_of_line, _, String1),      % fail on EOF
                concat_string([String,"\n",String1], String2),
                csv_read_row_retry(S, Row, String2, Options)
            ))
        ->
            csv_options{type:Type} = Options,
            ( Type == list ->
                Row = Fields
            ; Type = F/N ->
                Row =.. [F|Fields],
                arity(Row, NRow),
                ( NRow=N -> true ;
                    get_stream_info(S, name, Name),
                    get_stream_info(S, line, LineNr),
                    printf(warning_output, "%w:%d: Arity mismatch: %d, expected %d%n", [Name,LineNr,NRow,N])
                )
            )
        ;
            get_stream_info(S, name, Name),
            get_stream_info(S, line, LineNr),
            printf(error, "%w:%d: Invalid csv row: %w%n", [Name,LineNr,String]),
            abort
        ).


bom_records(Records, Options) -->
        ( [16'FEFF] -> [] ; [] ),
	records(Records, Options).

records(Records, Options) -->
        ( record(Record, Options) ->
            {
                Records = [RecordTerm|Records1],
                csv_options{type:Type} = Options,
                ( Type == list ->
                    RecordTerm = Record
                ; Type = F/N ->
                    RecordTerm =.. [F|Record],
                    arity(RecordTerm, NRow),
                    ( NRow=N  -> true ;
                        printf(warning_output, "Arity mismatch: %d, expected %d%n", [NRow,N])
                    )
                )
            },
            records(Records1, Options)
        ;
            { Records = [] }
        ).

bom_single_record(Record, Options) -->
        ( [16'FEFF] -> [] ; [] ),
        single_record(Record, Options).

single_record([Field|Fields], Options) -->
        field(Field, Options),
        ( [0',] ->
            single_record(Fields, Options)
        ;
            { Fields = [] }
        ).

record([Field|Fields], Options) -->
        field(Field, Options),
        ( [0',] ->
            record(Fields, Options)
        ;
            crlf,
            { Fields = [] }
        ).

field(Field, csv_options{strip:Strip,convert:Convert}) -->
        ( [0'"] ->
            escaped(FieldChars),
            { string_list(Field, FieldChars, utf8) }
        ;
            non_escaped(FieldChars), % may be empty field
            {
                string_list(FieldString, FieldChars, utf8),
                ( Convert==true ->
                    split_string(FieldString, "", " \t", [StrippedString]),
                    ( number_string(Number, StrippedString), (integer(Number);float(Number)) ->
                        Field = Number
                    ; Strip==true ->
                        Field = StrippedString
                    ;
                        Field = FieldString
                    )
                ; Strip==true ->
                    split_string(FieldString, "", " \t", [Field])
                ;
                    Field = FieldString
                )
            }
        ).

escaped(Cs) -->
        ( [16'22] ->
            ( [16'22] ->
                { Cs = [16'22|Cs1] },
                escaped(Cs1)
            ;
                { Cs = [] }
            )
        ; [C] ->
            { Cs = [C|Cs1] },
            escaped(Cs1)
        ;
            { throw(escaped_newline_found) }
        ).


non_escaped(Cs) -->
        ( textdata(C) ->
            { Cs = [C|Cs1] },
            non_escaped(Cs1)
        ;
            { Cs = [] }
        ).
 
textdata(C) -->
        [C],
        { C >= 16'20, C \== 16'22, C \== 16'2C }.

crlf --> [16'D, 16'A], !.
crlf --> [16'A], !.             % occurs in OpenOffice csv output
crlf --> [16'D].                % also occurs in the wild



%----------------------------------------------------------------------
% Output data structure
%----------------------------------------------------------------------

:- lib(hash).

:- export headers_map/2.
headers_map(Headers, Map) :-
        hash_create(Map),
        ( is_list(Headers) ->
            ( foreach(ColName,Headers), count(I,1,_), param(Map) do
                string(ColName),
                hash_insert(Map, ColName, I)
            )
        ; compound(Headers),
            ( foreacharg(ColName,Headers), count(I,1,_), param(Map) do
                string(ColName),
                hash_insert(Map, ColName, I)
            )
        ).

:- export get_column/4.
get_column(ColName, Row, Map, Field) :-
        hash_get(Map, ColName, I),
        I =< arity(Row),
        arg(I, Row, Field).


%----------------------------------------------------------------------
% Approximate reading of single rows, using ECLiPSe parser
% We do this by declaring LF a terminator, changing character classes,
% and reading as a term with read/2.
% Limitations:
%       - cannot handle empty fields ,,
%----------------------------------------------------------------------

:- comment(csv_read_row/2, [
    summary:"Read one row of comma separated values (approximate)",
    args:["Stream":"Stream name or handle",
        "RowList":"List of numbers and strings, or 'end_of_file' (output)"],
    amode:(csv_read_row(+,-) is det),
    desc:html("<p>
        DEPRECATED - use csv_read_row/3.
    </p><p>
        Reads one row of comma separated values from Stream, and returns
        the result as a list.  On end of file, returns the atom 'end_of_file'.
    </p><p>
        The result list contains one element for each field in the record.
        The data elements are either numbers (if they can be interpreted
        as numbers by ECLiPSe's number_string/2 predicate), or otherwise
        strings.
    </p><p>
        Shortcomings: as opposed to csv_read_row/3, this predicate here uses the
        ECLiPSe parser to read rows, and does not implement the csv format
        fully.  E.g. empty fields are not handled and yield a syntax error.
        However, when applicable, it will be faster than csv_read_row/3.
    </p>
    "),
    see_also:[number_string/2,csv_read/3,csv_read_row/3]
]).

:- export csv_read_row/2.
csv_read_row(Stream, RowList) :-
        read(Stream, RowTerm)@csv_syntax,
        ( RowTerm == end_of_file ->
            RowList = RowTerm
        ;
            comma_to_list(RowTerm, RowList)
        ).

comma_to_list((A,BC), ABCs) ?- !, ABCs = [AN|BCs],
        convert_type(A, AN),
        comma_to_list(BC, BCs).
comma_to_list(A, [AN]) :-
        convert_type(A, AN).

    convert_type(AS, NS) :-
        ( atom(AS) -> atom_string(AS, S) ; S = AS ),
        ( number_string(N, S), (integer(N);float(N)) -> NS=N ; NS=S ).


:- module(csv_syntax).

:- local initialization((
        (
            % make dummy underline and atom_quote characters, because
            % ECLiPSe doesn't allow them to be undefined.
            local(chtab(0,underline)),
            local(chtab(1,atom_quote)),
            between(2,255,1,C),
            char_table(C, Class),
            local(chtab(C,Class)),
            fail
        ;
            true
        ),
        local(syntax_option(doubled_quote_is_quote))
    )).

char_table(0'\n, terminator) :- !.
char_table(0'\r, blank_space) :- !.     % ignored
char_table(0',, special) :- !.
char_table(0'", string_quote) :- !.
char_table(_Other, lower_case).         % treat everything else like a letter

