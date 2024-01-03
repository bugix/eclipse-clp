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
% Copyright (C) 2002 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Andrew Sadler, IC-Parc
% 
% END LICENSE BLOCK

% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: graphviz.ecl,v 1.5 2016/07/24 19:34:45 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(graphviz).

:- comment(categories, ["Interfacing","Visualisation"]).
:- comment(summary, "Interface to Graphviz Graph Drawing Programs from AT&T").
:- comment(author, "Andrew J Sadler and Joachim Schimpf, IC-Parc").
:- comment(date, "$Id: graphviz.ecl,v 1.5 2016/07/24 19:34:45 jschimpf Exp $").
:- comment(desc, html("
    This library provides an interface to the Graphviz Graph Drawing Programs
    from AT&T (<A HREF=\"http://www.graphviz.org\">www.graphviz.org</A>).
<P>
    Graphviz itself must be downloaded and installed separately.  It is
    readily available for most operating systems, and subject to the
    open-source <A HREF=\"http://www.graphviz.org/License.php\">Eclipse Public Licence</A>.
<P>
    The predicates in this library take graph data structures as defined by
    library(graph_algorithms), invoke graphviz's layout tools, and output
    the generated drawing in various file formats (which can then be viewed
    in a browser of dedicated viewer), or invoke a simple viewer directly.
    ")).

:- lib(module_options).
:- lib(graph_algorithms).

:- lib(graphviz_license).

:- local struct(options(graph_attrs,
                        default_edge_attrs,
                        default_node_attrs,
                        node_attrs_generator,
                        edge_attrs_generator,
                        layout)).

:- local variable(graph_counter, 0).

valid_option_field(graph_attrs, graph_attrs of options).
valid_option_field(default_edge_attrs, default_edge_attrs of options).
valid_option_field(default_node_attrs, default_node_attrs of options).
valid_option_field(node_attrs_generator, node_attrs_generator of options).
valid_option_field(edge_attrs_generator, edge_attrs_generator of options).
valid_option_field(layout, layout of options).


valid_option_value(graph_attrs, Value) :- valid_attrs(Value).
valid_option_value(default_edge_attrs, Value) :- valid_attrs(Value).
valid_option_value(default_node_attrs, Value) :- valid_attrs(Value).
valid_option_value(node_attrs_generator, Value) :- (atom(Value);compound(Value)).
valid_option_value(edge_attrs_generator, Value) :- (atom(Value);compound(Value)).
valid_option_value(layout, none).
valid_option_value(layout, dot).
valid_option_value(layout, neato).
valid_option_value(layout, twopi).
valid_option_value(layout, force_directed).
valid_option_value(layout, radial).
valid_option_value(layout, tree).
valid_option_value(layout, top_to_bottom).
valid_option_value(layout, left_to_right).
valid_option_value(layout, [_|_]).

default_options(options with [
                                 graph_attrs:[],
                                 default_edge_attrs:[],
                                 default_node_attrs:[],
                                 node_attrs_generator:default_attrs_generator,
                                 edge_attrs_generator:default_attrs_generator,
                                 layout:dot
                             ]).



valid_attrs(V):-var(V),!.
valid_attrs(Attrs):-
        (foreach(Attr,Attrs) do
             functor(Attr,_,2)
        ).

validate_options(Options, OptionStruct):-
        ( get_options(Options, OptionStruct) ->
              true
        ;
              printf(error, "Invalid option list: %w%n", [Options]),
              print_default_options(error),
              abort
        ).

:- comment(view_graph/1, [
    summary:"Display a given graph in a window (using default options)",
    amode:view_graph(+),
    args:[
	"Graph":"A graph structure"
    ],
    see_also:[view_graph/2,library(graph_algorithms)],
    eg:"
    ?- lib(graph_algorithms), lib(graphviz).
    Yes (1.17s cpu)

    ?- make_random_graph(10,30,true,true,true,G), view_graph(G).
    G = graph(...)
    Yes (0.03s cpu)
    "
]).
:-export view_graph/1.
:-tool(view_graph/1, view_graph_/2).
view_graph_(Graph,Module):-
        view_graph_(Graph,[],Module).

:- comment(view_graph/2, [
    summary:"Display a given graph in a window",
    amode:view_graph(+,+),
    args:[
	"Graph":"A graph structure",
	"Options":"A list of Option:Value pairs"
    ],
    see_also:[view_graph/1,library(graph_algorithms)],
    desc:html("
    This predicate takes a graph, applies one of the graphviz layout routines
    to it, and displays the result in a viewer window.
    <P>
    The viewer is an application called grappa, which requires Java.
    <P>
    Possible options are:
    <DL>
    <DT>graph_attrs</DT>
	<DD>a list of Name=Value pairs which specify the graph attributes</DD>
    <DT>default_edge_attrs</DT>
	<DD>a list of Name=Value pairs which specify the default edge attributes</DD>
    <DT>default_node_attrs</DT>
	<DD>a list of Name=Value pairs which specify the default node attributes</DD>
    <DT>node_attrs_generator</DT>
	<DD>a partialpredicate specification pred(ExtraArgs,...) that
	will generate node attributes for specific nodes. This
	predicate will be invoked for every node in the graph with the
	arguments pred(ExtraArgs,...,+Graph, +Node, -AttrList). It is
	expected to compute an attribute list for a particular
	node. If it fails, the node will be displayed using the
	default node attributes.</DD>

    <DT>edge_attrs_generator</DT>
	<DD>a partialpredicate specification pred(ExtraArgs,...) that
	will generate edge attributes for specific edges. This
	predicate will be invoked for every edge in the graph with the
	arguments pred(ExtraArgs,...,+Graph, +Edge, -AttrList). It is
	expected to compute an attribute list for a particular
	edge. If it fails, the edge will be displayed using the
	default edge attributes.</DD>
    <DT>layout</DT>
	<DD>One of the atoms: none, dot, neato, twopi, force_directed, radial,
	tree, top_to_bottom, left_to_right.
	Alternatively, a list specifying a layout command (see exec/3).
    </DL>
    For the exact definition of graph, node and edge attributes, see the
    specification of the DOT language in the graphviz documentation.
    "),
    eg:"
    ?- lib(graph_algorithms), lib(graphviz).
    Yes (1.17s cpu)

    ?- make_random_graph(10, 30, true, true, true, G),
       view_graph(G, [layout:left_to_right]).
    G = graph(...)
    Yes (0.03s cpu)

    ?- make_random_graph(10, 30, true, true, true, G),
       view_graph(G, [layout:left_to_right]).
    G = graph(...)
    Yes (0.03s cpu)


% Sample node attribute generator

node_colour(Graph, Node, Attrs) :-
	( Node mod 2 =:= 0 -> Attrs = [color=red] ; Attrs = [color=green] ).

% Sample run

    ?- make_random_graph(10, 30, true, true, true, G),
       view_graph(G, [node_attrs_generator:node_colour]).
    G = graph(...)
    Yes (0.03s cpu)
    "
]).
:-export view_graph/2.
:-tool(view_graph/2, view_graph_/3).
view_graph_(Graph,Options,Module):-
        view_graph_(Graph,Options,_File,Module).


:- comment(view_graph/3, [
    summary:"Display a given graph in an existing window",
    amode:view_graph(+,+,?),
    args:[
	"Graph":"A graph structure",
	"Options":"A list of Option:Value pairs",
        "HandleFile":"String or variable"
    ],
    see_also:[view_graph/1,view_graph/2,library(graph_algorithms)],
    desc:html("
    This predicate takes a graph, applies one of the graphviz layout
    routines to it, and displays the result in a viewer window.  If
    the HandleFile option is a variable it will be bound the name of
    the file which the viewer is rendering.  By calling this predicate
    again with the same file name, the viewer will re-load the file
    WITHOUT creating a new window.<P>

    This predicate can be very useful for displaying graph structures
    which change over time.<P>

    NOTE: This predicate will sleep for one second to give the viewer
    a chance to notice that the file has changed and to re-read it.

    <P>
    "
)]).
:-export view_graph/3.
:-tool(view_graph/3, view_graph_/4).
view_graph_(Graph,Options,File,Module):-
        validate_options(Options,ValidOptions),
        (var(File) ->
            get_temp_filename(File)
        ;
            % Use the filename given
            true
        ),
	get_temp_filename(AuxFile),
        (exists(File) ->
            % do not start a new viewer
            write_graph_valid(Graph,AuxFile,dot,ValidOptions, Module),
	    fix_syntax_for_grappa(AuxFile, File),
	    delete(AuxFile),
            sleep(1)
        ;
            write_graph_valid(Graph,AuxFile,dot,ValidOptions, Module),
	    fix_syntax_for_grappa(AuxFile, File),
	    delete(AuxFile),
            start_viewer(File)
        ),
        true.


:- comment(write_graph/2, [
    summary:"Write a picture of a graph as a postscript file",
    amode:write_graph(+,+),
    args:[
	"Graph":"A graph structure",
	"File":"A file name"
    ],
    see_also:[write_graph/3,write_graph/4,view_graph/2,library(graph_algorithms)],
    eg:"
    ?- lib(graph_algorithms), lib(graphviz).
    Yes (1.17s cpu)

    % will create a file mygraph.ps
    ?- make_random_graph(10,30,true,true,true,G),
       write_graph(G, \"mygraph\").
    G = graph(...)
    Yes (0.03s cpu)
    "
]).
:-export write_graph/2.
:-tool(write_graph/2, write_graph_/3).
write_graph_(Graph,File,Module):-
        write_graph_(Graph, File, ps, Module).

:- comment(write_graph/3, [
    summary:"Write a picture of a graph as a file of a given format",
    amode:write_graph(+,+,+),
    args:[
	"Graph":"A graph structure",
	"File":"A file name",
	"Format":"An atom (dot,gv,eps,gif,jpg,pdf,png,svg,...)"
    ],
    see_also:[write_graph/2,write_graph/4,view_graph/2,library(graph_algorithms)],
    desc:html("
    This predicate takes a graph, applies one of the graphviz layout routines
    to it, and writes the result to a file is a given format.
    <P>
    Some possible formats are:
    <DL>
    <DT>dot,gv,xdot</DT><DD>
        Graphviz own formats</DD>
    <DT>eps</DT><DD>
        Encapsulated Postscript</DD>
    <DT>gif</DT><DD>
        GIF bitmaps</DD>
    <DT>jpg</DT><DD>
        JPEG compressed images</DD>
    <DT>pdf</DT><DD>
        PDF Portable Document Format</DD>
    <DT>plain</DT><DD>
        A human-readable representation</DD>
    <DT>png</DT><DD>
	PNG (Portable Network Graphics)</DD>
    <DT>svg</DT><DD>
	SVG Scalable Vector Graphics</DD>
    </DL>
    Many of these formats can be displayed by web browsers or by dedicated
    viewers.  See the graphviz documentation for details and additional formats.
    "),
    eg:"
    ?- lib(graph_algorithms), lib(graphviz).
    Yes (1.17s cpu)

    ?- make_random_graph(10,30,true,true,true,G),
       write_graph(G, \"mygraph\", png).
    G = graph(...)
    Yes (0.03s cpu)

    ?- make_random_graph(10,30,true,true,true,G),
       write_graph(G, \"mygraph\", dot).
    G = graph(...)
    Yes (0.03s cpu)
    "
]).
:-export write_graph/3.
:-tool(write_graph/3, write_graph_/4).
write_graph_(Graph,File,Format,Module):-
        write_graph_(Graph, File, Format, [], Module).

:- comment(write_graph/4, [
    summary:"Write a picture of a graph as a file of a given format",
    amode:write_graph(+,+,+,+),
    args:[
	"Graph":"A graph structure",
	"File":"A file name",
	"Format":"An atom (dot,gv,eps,gif,jpg,pdf,png,svg,...)",
	"Options":"A list of Option:Value pairs"

    ],
    see_also:[write_graph/2,write_graph/3,view_graph/2,library(graph_algorithms)],
    desc:html("
    This predicate takes a graph, applies one of the graphviz layout routines
    to it, and writes the result to a file is a given format.
    <P>
    Possible options are as specified in view_graph/2.
    <P>
    Some possible formats are:
    <DL>
    <DT>dot,gv,xdot</DT><DD>
        Graphviz own formats</DD>
    <DT>eps</DT><DD>
        Encapsulated Postscript</DD>
    <DT>gif</DT><DD>
        GIF bitmaps</DD>
    <DT>jpg</DT><DD>
        JPEG compressed images</DD>
    <DT>pdf</DT><DD>
        PDF Portable Document Format</DD>
    <DT>plain</DT><DD>
        A human-readable representation</DD>
    <DT>png</DT><DD>
	PNG (Portable Network Graphics)</DD>
    <DT>svg</DT><DD>
	SVG Scalable Vector Graphics</DD>
    </DL>
    Many of these formats can be displayed by web browsers or by dedicated
    viewers.  See the graphviz documentation for details and additional formats.
    "),
    eg:"
    ?- lib(graph_algorithms), lib(graphviz).
    Yes (1.17s cpu)

    ?- make_random_graph(10,30,true,true,true,G),
       write_graph(G, \"mygraph\", png).
    G = graph(...)
    Yes (0.03s cpu)

    ?- make_random_graph(10,30,true,true,true,G),
       write_graph(G, \"mygraph\", dot).
    G = graph(...)
    Yes (0.03s cpu)
    "
]).
:-export write_graph/4.
:-tool(write_graph/4, write_graph_/5).
write_graph_(Graph,File,Format,Options,Module):-
        validate_options(Options,ValidOptions),
	( pathname(File, _, _, "") ->
	    concat_string([File,.,Format], File1)
	;
	    File1 = File
	),
        write_graph_valid(Graph, File1, Format, ValidOptions, Module).


get_layout_command(Layout,[Command]):-
        memberchk(Layout,[dot,neato,twopi]),
	!,
	getval(cmd_prefix, CmdPrefix),
	concat_string([CmdPrefix,Layout], Command).
get_layout_command([Layout|Rest],Command):-
        !,
        get_layout_command(Layout,LayoutCommand),
        append(LayoutCommand,Rest,Command).
get_layout_command(radial,Command):-
        !,
        get_layout_command(twopi,Command).
get_layout_command(network,Command):-
        !,
        get_layout_command(neato,Command).
get_layout_command(force_directed,Command):-
        !,
        get_layout_command(neato,Command).
get_layout_command(tree,Command):-
        !,
        get_layout_command(dot,Command).
get_layout_command(top_to_bottom,Command):-
        !,
        get_layout_command(dot, C1),
        append(C1,["-Grankdir=TB"],Command).
get_layout_command(left_to_right,Command):-
        !,
        get_layout_command(dot, C1),
        append(C1,["-Grankdir=LR"],Command).
get_layout_command(Layout,[Layout]).



write_graph_valid(Graph, File, Format, ValidOptions, Module):-
        ValidOptions = options with [layout:Layout],
        (Layout == none ->
             open(File, write, Stream),
	     write_graph_stream(Graph, Stream, ValidOptions, Module),
	     close(Stream)
        ;
             concat_string(["-T",Format],FormatStr),
             get_layout_command(Layout,LayoutCommand),
	     os_file_name(File, FileOS),
             append(LayoutCommand, [FormatStr, "-o", FileOS], Command),
             exec(Command, [Stream], Pid),
	     write_graph_stream(Graph, Stream, ValidOptions, Module),
	     close(Stream),
	     wait(Pid, _Status)
        ).
        
write_graph_stream(Graph, S, ValidOptions, Module):-
        ValidOptions = options with [
                                        graph_attrs:GAs,
                                        default_edge_attrs:EAs,
                                        default_node_attrs:NAs,
                                        node_attrs_generator:NAg,
                                        edge_attrs_generator:EAg
                                    ],
        writeln(S, "digraph g {"),
        write_graph_attrs(S, GAs),
        writeln_attrs(S, "node", NAs),
        writeln_attrs(S, "edge", EAs),
        graph_get_maxnode(Graph,MaxNode),
	(for(I,1,MaxNode), param(Graph,S,NAg,Module) do
	    default_node_label(Graph, I, NLabel),
	    (
		NAg \= default_attrs_generator,
                (NAg = Module:Pred0 ->
                    Pred0 =.. [NAfunctor|NAargs],
                    append(NAargs, [Graph, I, NAttrs1], NAargs2),
                    Pred =.. [NAfunctor|NAargs2],
                    Goal = Module:Pred
                ;
                    NAg =.. [NAfunctor|NAargs],
                    append(NAargs, [Graph, I, NAttrs1], NAargs2),
                    Goal =.. [NAfunctor|NAargs2]
                ),                    
		call(Goal)@Module
	    ->
		NAttrs = [label=NLabel|NAttrs1]
	    ;
		NAttrs = [label=NLabel]
	    ),
	    writeln_attrs(S, I, NAttrs)
	),
	graph_get_all_edges(Graph,Edges),
	(foreach(Edge,Edges), param(Graph,S,EAg,Module) do
	    Edge=e(From,To,_),
	    (
		EAg \= default_attrs_generator,
                (EAg = Module:Pred0 ->
                    Pred0 =.. [EAfunctor|EAargs],
                    append(EAargs, [Graph, Edge, EAttrs], EAargs2),
                    Pred =.. [EAfunctor|EAargs2],
                    Goal = Module:Pred
                ;
                    EAg =.. [EAfunctor|EAargs],
                    append(EAargs, [Graph, Edge, EAttrs], EAargs2),
                    Goal =.. [EAfunctor|EAargs2]
                ),                    
		call(Goal)@Module
	    ->
                (member(from_port=FromPort,EAttrs) ->
                    concat_string([From,":",FromPort], FromString)
                ;
                    FromString=From
                ),
                (member(to_port=ToPort,EAttrs) ->
                    concat_string([To,":",ToPort], ToString)
                ;
                    ToString=To
                ),
		true
	    ;
		EAttrs = [],
                FromString=From,
                ToString=To                
	    ),
	    writeln_attrs(S, FromString->ToString, EAttrs)
	),
        writeln(S, "}").


% default_node_label(+Graph, +Node, -LabelString)
default_node_label(Graph, Node, Label) :-
    	( node_to_nodename(Graph, Node, Name) ->
	    ( string(Name) -> Label = Name
	    ; atom(Name) -> atom_string(Name, Label)	% avoid single quotes
	    ; term_string(Name, Label)
	    )
	;
	    number_string(Node, Label)
	).


write_graph_attrs(S, GAs):-
        (foreach(Attr, GAs), param(S) do
             arg(1,Attr,Key),
             arg(2,Attr,Value),
             write(S,Key),write(S,'='),writeq(S,Value),writeln(S,';')
        ).

writeln_attrs(S, Head, Attrs):-
        write(S, Head),write(S," ["),
        write_attrs(S, Attrs),
        writeln(S, "];").

write_attrs(_S, []).
write_attrs(S, [A|Attrs]):-
        arg(1,A,Key),
        arg(2,A,Value),
        write(S,Key),write(S,'='),writeq(S,Value),
        (foreach(Attr,Attrs), param(S) do
             write(S,", "),
             arg(1,Attr,Key),
             arg(2,Attr,Value),
             write(S,Key),write(S,'='),writeq(S,Value)
        ).

get_temp_filename(Filename):-
        getval(graph_counter,Num),
        incval(graph_counter),
	get_flag(tmp_dir, TmpDir),
	get_flag(pid, Pid),
        concat_string([TmpDir,"eclipse_",Pid,"_graphviz_",Num,".dot"],Filename).

start_viewer(File):-
	% set up command according to architecture
	os_file_name(File, FileOS),
	get_java_command("com.parctechnologies.eclipse.jdotview.JDotView",
                         [FileOS],  ViewerCommand),
	% execute command 
%	run_viewer_command(["dotty", File]).
	run_viewer_command(ViewerCommand).


run_viewer_command(Command):-
	writeln(log_output, "Launching graph viewer..."),
	exec_group(Command, [], _Pid).

        
get_java_command(MainClass, Args,
		[Java, Size, "-classpath", ClassPath, MainClass|Args]) :-

	get_flag(hostarch, ARCH), 
	get_flag(installation_directory, ECLIPSEDIR),
	( getenv("JRE_HOME", JRE_HOME_OS)  ->
	    os_file_name(JRE_HOME, JRE_HOME_OS)
        ;
             writeln(error, "Can't find JRE: Environment variable JRE_HOME, specifying location of the Java"),
             writeln(error, "Runtime Environment, has not been set."), 
	     abort
        ),

	concat_string([JRE_HOME, "/bin/"], JavaBin),
	( existing_file(JavaBin, ['javaw.exe','java.exe',java], [executable], Java) ->
	    true
	;
             printf(error, "Can't find Java executable in %w%n", [JavaBin]), 
	     abort
	),

        java_vm_size(JVM_SIZE),
	concat_string(["-Xmx", JVM_SIZE, "m"], Size),

	concat_string([ECLIPSEDIR, "/lib/grappa1_2.jar"], GrappaJar),
	os_file_name(GrappaJar, GrappaJarOS),
	concat_string([ECLIPSEDIR, "/lib/visualisation.jar"], VizJar),
	os_file_name(VizJar, VizJarOS),
	concat_string([JRE_HOME, "/lib/rt.jar"], RtJar),
	os_file_name(RtJar, RtJarOS),

	java_path_sep(ARCH, Sep),
	concat_string([GrappaJarOS,Sep,VizJarOS,Sep,RtJarOS,Sep], ClassPath).


    java_path_sep("i386_nt", (;)) :- !.
    java_path_sep("x86_64_nt", (;)) :- !.
    java_path_sep(_, (:)).

    % max size of the virtual machine in megabytes
    java_vm_size(256).



% Find out how to call dot, and whether it is installed at all.
% Store required path prefix in cmd_prefix.

:- local initialization(find_graphviz_dir).
:- local variable(cmd_prefix, "").

find_graphviz_dir :-
	(
	    (
		% first check in our own lib directory
		get_flag(hostarch, ARCH),
		get_flag(installation_directory, ECLIPSEDIR),
		join_string([ECLIPSEDIR,lib,ARCH], "/", BinDir)
	    ; 
		% Try Windows install locations
		( Att=[] ; Att=["ATT"] /*older*/ ),
		( getenv("PROGRAMFILES",Dir) ; getenv("PROGRAMFILES(X86)",Dir) ),
		join_string([Dir|Att], "/", ProgramDir),
		exists(ProgramDir),
		read_directory(ProgramDir, "", SubDirs, _),
		member(SubDir, SubDirs),
		substring(SubDir, "Graphviz", 1),
		join_string([Dir,SubDir,bin], "/", BinDir)
	    ),
	    concat_strings(BinDir, "/", CmdPrefix),
	    existing_file(CmdPrefix, ["dot","dot.exe"], [executable], Command)
	;
	    % rely on PATH
	    CmdPrefix = "", Command = "dot"
	),
	catch(exec([Command,"-V"], [null,null,Err]), _, (
		printf(warning_output, "Could not find GraphViz installation."
		    "Please download from graphviz.org and install before continuing.", []),
		    fail)),
	setval(cmd_prefix, CmdPrefix),
	read_string(Err, end_of_file, _, Msg),
	writeln(log_output, Msg).
		

% Fix dot-file syntax by quoting all unquoted float attributes.
% This workaround is needed because starting with about graphviz 2.30,
% dot outputs unquoted floats, which grappa doesn't understand.
% It would be better to fix grappa, but that isn't straightforward.

fix_syntax_for_grappa(InFile, OutFile) :-
	open(InFile, read, In),	
	open(OutFile, write, Out),	
	get(In, C),
	repair_start(C, In, Out),
	close(In),
	close(Out).

    repair_start(-1, _In, _Out) :- !.
    repair_start(0'=, In, Out) :- !, put(Out, 0'=), get(In, C), repair_attr(C, In, Out).
    repair_start(0'", In, Out) :- !, put(Out, 0'"), get(In, C), repair_quoted(C, In, Out).
    repair_start(C, In, Out) :- put(Out, C), get(In, C1), repair_start(C1, In, Out).

    repair_quoted(-1, _In, _Out) :- !.
    repair_quoted(0'", In, Out) :- !, put(Out, 0'"), get(In, C), repair_start(C, In, Out).
    repair_quoted(0'\, In, Out) :- !, put(Out, 0'\), get(In, C), repair_escaped(C, In, Out).
    repair_quoted(C, In, Out) :- put(Out, C), get(In, C1), repair_quoted(C1, In, Out).
    	
    repair_escaped(-1, _In, _Out) :- !.
    repair_escaped(EC, In, Out) :- put(Out, EC), get(In, C), repair_quoted(C, In, Out).
    	
    repair_attr(N, In, Out) :- 0'0 =< N, N =< 0'9, !, get(In, C), repair_number(C, In, Out, [N|Ns], Ns).
    repair_attr(C, In, Out) :- repair_start(C, In, Out).

    repair_number(-1, _In, Out, Ns, []) :- !, output_chars(Out, Ns).
    repair_number(N,   In, Out, Ns, [N|Ns0]) :- 0'0 =< N, N =< 0'9, !, get(In, C), repair_number(C, In, Out, Ns, Ns0).
    repair_number(0'., In, Out, Ns, [0'.|Ns0]) :- !, get(In, C), repair_float(C, In, Out, Ns, Ns0).
    repair_number(C, In, Out, Ns, [C]) :- output_chars(Out, Ns), get(In, C1), repair_start(C1, In, Out).

    repair_float(-1, _In, Out, Ns, []) :- !, output_quoted_chars(Out, Ns).
    repair_float(N,   In, Out, Ns, [N|Ns0]) :- 0'0 =< N, N =< 0'9, !, get(In, C), repair_float(C, In, Out, Ns, Ns0).
    repair_float(T,   In, Out, Ns, []) :- (T=0', ; T=0'; ; T=0']), !, output_quoted_chars(Out, Ns), put(Out, T), get(In, C), repair_start(C, In, Out).
    repair_float(C, In, Out, Ns, [C]) :- output_chars(Out, Ns), get(In, C1), repair_start(C1, In, Out).

    output_chars(_Out, []).
    output_chars(Out, [C|Cs]) :- put(Out, C), output_chars(Out, Cs).

    output_quoted_chars(Out, Cs) :- put(Out, 0'"), output_chars(Out, Cs), put(Out, 0'").



/*
:-export test/1.
test(Graph):-
        make_graph_symbolic(
                               [](a,b,c,d,e,f,g,h,i,j,k,l,m),
                               [ edge(a,f,1),edge(a,b,1),edge(a,g,1),edge(c,a,1),edge(d,f,1),edge(e,d,1),
                                 edge(f,e,1),edge(g,e,1),edge(g,j,1),edge(g,c,1),edge(h,g,1),edge(h,i,1),edge(i,h,1),
                                 edge(j,k,1),edge(j,l,1),edge(j,m,1),edge(l,g,1),edge(l,m,1),edge(m,l,1) ],
                               Graph).
*/
