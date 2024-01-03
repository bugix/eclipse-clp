%----------------------------------------------------------------------
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
% The Original Code is  The Zinc Modelling interface for ECLiPSe
% The Initial Developer of the Original Code is  Joachim Schimpf
% with support from Cisco Systems and NICTA Victoria.
% Portions created by the Initial Developer are
% Copyright (C) 2007 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf
% 
% END LICENSE BLOCK
%----------------------------------------------------------------------

:- module(minizinc).

:- comment(date, "$Date: 2016/07/24 19:34:45 $").
:- comment(categories, ["Interfacing","Constraints"]).
:- comment(summary, "Utilities for using MiniZinc with ECLiPSe").
:- comment(author, "Joachim Schimpf, supported by Cisco Systems and NICTA Victoria").
:- comment(copyright, "Cisco Systems Inc, licensed under CMPL").
:- comment(see_also, [
	flatzinc:struct(zn_options),
	library(flatzinc),
	library(fzn_ic),
	library(fzn_fd),
	library(fzn_eplex)
    ]).
:- comment(status, prototype).
:- comment(desc, html("
<H3>
Overview
</H3>
<P>
This module allows to run MiniZinc models with ECLiPSe.
MiniZinc models can be either read from a file or stream,
or they can be embedded as strings into ECLiPSe code.
The implementation relies on an external MiniZinc-to-FlatZinc converter,
e.g. mzn2fzn, and on the FlatZinc interpreter lib(flatzinc).
Mappings to different ECLiPSe solvers are possible via the solver
mapping libraries fzn_ic, fzn_fd, fzn_eplex, etc.
</P>

<H3>
Running MiniZinc Models without using this Library
</H3>
<P>
You can run a MiniZinc model by first converting it to FlatZinc yourself,
and then using the lib(flatzinc) library. This can be done either via
an intermediate .fzn file, or by piping the resulting FlatZinc model
into the ECLiPSe-FlatZinc interpreter using e.g.
<PRE>
% mzn2fzn --output-to-stdout model.mzn | eclipse -e \"flatzinc:fzn_run(fzn_ic)\"
</PRE>
This should work as long as the mzn2fzn command is in your PATH.
Note that mzn2fzn is currently not included with ECLiPSe but comes
with the Melbourne MiniZinc distribution.  You must also make sure that
the correct specialised global constraint definitions are used,
by including e.g. lib/fzn_ic in mzn2fzn's search path via its -I option.
For more details see lib(flatzinc).
</P>

<H3>
Running MiniZinc Models using this Library
</H3>
<P>
This library allows you to do everything from within ECLiPSe and let ECLiPSe
invoke the MiniZinc to FlatZinc translator (mzn2fzn) internally with the
correct arguments.  The model can be contained in a file:
<PRE>
?- mzn_run(\"model.mzn\", fzn_ic).
</PRE>
or, if a data instance file is used
<PRE>
?- mzn_run(\"model.mzn\", \"instance.dzn\", fzn_ic).
</PRE>
Since MiniZinc models are typically small, they can also be embedded as
a string into ECLiPSe code. For example:
<PRE>
    queens8 :-
	mzn_run_string(\"
		int: n = 8;
		array [1..n] of var 1..n: q;
		constraint
		    forall (i in 1..n, j in i+1..n) (
			q[i]     != q[j]     /\\\\
			q[i] + i != q[j] + j /\\\\
			q[i] - i != q[j] - j
		    );
		solve satisfy;
	    \", fzn_ic).
</PRE>
Note that, because of the rules for escaping characters within
ECLiPSe strings, the backslashes had to be doubled!
</P>

<H3>
Installation
</H3>
<P>
This version is intended to to work with Minizinc 1.6 or later!
<P>
In order to be found by lib(minizinc), the Melbourne Minizinc-to-Flatzinc
converter mzn2fzn (and the corresponding output processor solns2out) must be
installed in a directory called <CODE>minizinc-&lt;version&gt;</CODE> (or similar)
in one of the following locations (where we write &lt;ECLIPSEDIR&gt; for
the ECLiPSe installation directory, and &lt;ECLIPSEARCH&gt; for
the name for the machine architecture, e.g. i386_nt for Windows, i386_linux
for Linux):
<OL>
<LI>Directory specified by <CODE>$ECLIPSEMZN</CODE> environment variable</LI>
<LI>The user's home directory, as indicated by $HOME or $HOMEPATH</LI>
<LI><CODE>&lt;location of lib(minizinc)&gt;/&lt;ECLIPSEARCH&gt;</CODE></LI>
<LI><CODE>&lt;ECLIPSEDIR&gt;/lib_public/&lt;ECLIPSEARCH&gt;</CODE></LI>
<LI><CODE>&lt;ECLIPSEDIR&gt;/lib/&lt;ECLIPSEARCH&gt;</CODE></LI>
<LI><CODE>&lt;ECLIPSEDIR&gt;</CODE></LI>
<LI>Parent of <CODE>&lt;ECLIPSEDIR&gt;</CODE> (e.g. \"C:/Program Files\" on Windows)</LI>
<LI>Directory specified by <CODE>$PROGRAMFILES</CODE> environment variable</LI>
</OL>
<P>
You can also set the environment variable ECLIPSEMZN (on Windows alternatively
the registry entry HKLM/SOFTWARE/IC-Parc/Eclipse/<version>/ECLIPSEMZN)
to the Minizinc installation directory (or to its parent).


<H3>
Combining a MiniZinc model with Search or I/O in ECLiPSe
</H3>
<P>
There are several reasons why one might want to embed a MiniZinc model
into an ECLiPSe program:
<UL>
<LI>Passing parameters from the ECLiPSe program to the MiniZinc model</LI>
<LI>Getting the model solutions back into ECLiPSe</LI>
<LI>Programming custom search in ECLiPSe</LI>
<LI>Doing custom output beyond what the Zinc output annotations can do</LI>
</UL>
</P><P>
To pass a parameter into a MiniZinc model, a generic MiniZinc model must
be provided, together with a parameter map.
This map is an ECLiPSe list that corresponds to a MiniZinc (actually
FlatZinc) instance file:
<PRE>
queens(N) :-
	mzn_run_string(\"
		int: n;
		array [1..n] of var 1..n: q;
		constraint
		    forall (i in 1..n, j in i+1..n) (
			q[i]     != q[j]     /\\\\
			q[i] + i != q[j] + j /\\\\
			q[i] - i != q[j] - j
		    );
		solve satisfy;
	    \",
	    [n=N],	% parameter map: ZincId=EclipseValue
	    fzn_ic).
</PRE>
Alternatively, the generic model can be kept separately in a MiniZinc file:
<PRE>
queens(N) :-
	mzn_run(\"n_queens.mzn\", [n=N], fzn_ic).
</PRE>
<P>
With the above exmples, search and output are still completely specified
in MiniZinc.
</P><P>
To add your own search routine and/or output, use mzn_load_string/5 or
mzn_load/5. This has the effect of only loading the MiniZinc model
(i.e. setting up the constraints), but then returning to ECLiPSe without
executing any MiniZinc solve or output primitives.  The rest of the work
can then be done in ECLiPSe:
</P>
<PRE>
queens(N, Q) :-
	mzn_load(\"n_queens.mzn\", fzn_ic, [n=N], [q=Q], FznState),
	labeling(Q),
	fzn_output(FznState).
</PRE>
The [q=Q] mapping gives access to the ECLiPSe array Q corresponding to
the MiniZinc array q. This is a normal ECLiPSe array of lib(ic) domain
variables, and can be used for doing search, or outputting the results.
In the example however, we have fallen back onto the FlatZinc output
routine to display the results after search has finished.
</P><P>
Note that even if you do your own search in ECLiPSe, your MiniZinc model
must contain a solve item to be syntactically correct (and to specify
the objective, if any).
</P>

<H3>
Options
</H3>
Instead of just the name of the solver mapping (<CODE>fzn_ic</CODE> in
our examples), a <CODE>zn_options{}</CODE> structure can be given to
customize the behaviour further, e.g.
<PRE>
	mzn_run(File, zn_options{solver:fzn_eplex,var_names:on}.
</PRE>
<DL>
<DT>solver (default: fzn_ic)</DT><DD>
    Determines which ECLiPSe solvers are used.  The name is the
    name of a library implementing the mapping, e.g. fzn_ic,
    fzn_fd or fzn_eplex.
</DD>
<DT>solutions (default: 1)</DT><DD>
    The maximum number of solutions computed. Only effective if using
    builtin search and not optimizing. (0 or all = all solutions)
</DD>
<DT>setup_prio (default: 0)</DT><DD>
    The priority under which the constraint setup will be executed
    (see call_priority/2 and get_priority/1). Possible values are
    the ECLiPSe priorities 1 to 12, or 0 (the default) which stands
    for the current priority of the calling code.  A sensible value
    for this option is 2, which means that the setup code is executed
    under high priority (still allowing debug/visualisation goals).
    The effect of such a setting is that no propagation occurs until
    all constraints are fully set up, possibly leading to time savings.
</DD>
<DT>parser (default: fast)</DT><DD>
    Whether to use a 'strict' or 'fast' parser for FlatZinc input.
</DD>
<DT>optimize (default: on)</DT><DD>
    If 'off', pass --no-optimize to the Minizinc-Flatzinc translator.
    This can improve performance on large models.
</DD>
<DT>timeout (default: 0)</DT><DD>
    Time limit in seconds. (0 means no timeout),
</DD>
<DT>var_names (default: off)</DT><DD>
    Use lib(var_name) to label ECLiPSe variables with their Zinc names.
    This is useful for debugging.
</DD>
<DT>fzn_tmp (default: file)</DT><DD>
    Use a 'pipe' or intermediate 'file' for FlatZinc.
</DD>
</DL>

<H3>
Mapping between MiniZinc/FlatZinc Data and ECLiPSe Data
</H3>
<P>
When using ECLiPSe with a Mini/FlatZinc model, one needs to be aware of
the mapping from MiniZinc to FlatZinc (e.g. flattening of arrays),
and the representation of FlatZinc data in ECLiPSe.
</P><P>
Note that the ECLiPSe-side representation depends in part on the chosen
solver mapping. The following table shows the mapping used with fzn_ic
(which employs the lib(ic) and lib(ic_sets) solver libraries):
<PRE>
	FlatZinc Type/Syntax		ECLiPSe Type/Syntax
	-----------------------------------------------------------
	string				string
	e.g.	\"abc\"			\"abc\"

	bool (false/true)		integer (0/1)
	e.g.	false			0

	int				integer
	e.g.	33			33

	float				float or breal
	e.g.	3.4			3.399__3.401

	set of int			ordered list of integer
	e.g.	{1,5,4}			[1,4,5]
		1..3			[1,2,3]

	array[1..N] of T		structure with functor []/N
	e.g.	[23,54,0]		[](23,54,0)

	var bool			lib(ic) integer variable

	var int				lib(ic) integer variable

	var float			lib(ic) continuous variable

	var set of int			lib(ic_sets) set variable
</PRE>
</P>
")).


% The location of this file, when loaded.
% Used to find the ECLiPSe/Solver specific globals.mzn file
:- local variable(here).
?- getcwd(Cwd), setval(here, Cwd).

% Location of MiniZinc installation (with bin and lib subdirectories)
% We try a couple of locations heuristically
:- local
	variable(minizinc_dir, ''),
	variable(mzn2fzn_exe, "mzn2fzn"),
	variable(solns2out_exe, "solns2out"),
	initialization((
	    get_flag(installation_directory, EclDir),
	    get_flag(hostarch, Arch),
	    getval(here, Here),
	    findall(Dir2, (
		    ( getenv("ECLIPSEMZN", Dir1OS), os_file_name(Dir1, Dir1OS)
		    ; getenv("HOME", Dir1OS), os_file_name(Dir1, Dir1OS)
		    ; getenv("HOMEPATH", Dir1OS), os_file_name(Dir1, Dir1OS)
		    ; concat_string([Here,Arch], Dir1)
		    ; concat_string([EclDir,"/lib_public/",Arch], Dir1)
		    ; concat_string([EclDir,"/lib/",Arch], Dir1)
		    ; Dir1=EclDir
		    ; concat_string([EclDir,"/.."], Dir1)
		    ; getenv("PROGRAMFILES", Dir1OS), os_file_name(Dir1, Dir1OS)
		    ),
		    canonical_path_name(Dir1, Dir2)
		), Dirs),
	    (
                (
		    canonical_path_name("$ECLIPSEMZN/", MznDirOS0),
		    os_file_name(MznDir, MznDirOS0)
                ;
                    member(Dir, Dirs),
                    exists(Dir),
                    read_directory(Dir, "", SubDirs0, _),
                    sort(0, >=, SubDirs0, SubDirs),	% attempt to prefer newer ones
                    member(Sub, SubDirs),
                    member(Prefix, ["minizinc-","MiniZinc ","MiniZinc-","MiniZincIDE-"]),
                    substring(Sub, Prefix, 1),
                    concat_string([Dir,Sub,/], MznDir)
                ),
                ( ( substring(MznDir, "minizinc-0", _)
                  ; substring(MznDir, "minizinc-1.0", _)) -> % require 1.1 at least
                    printf(warning_output, "%% Ignoring old version %w%n", [MznDir]),
                    fail
                ;
                    true
                ),
		( concat_string([MznDir,"bin/private/mzn2fzn"], Mzn2Fzn)
		; concat_string([MznDir,"bin/actual/mzn2fzn"], Mzn2Fzn)
		; concat_string([MznDir,"bin/private/mzn2fzn-actual"], Mzn2Fzn)
		; concat_string([MznDir,"bin/mzn2fzn"], Mzn2Fzn)
		; concat_string([MznDir,"mzn2fzn"], Mzn2Fzn)
		),
		existing_file(Mzn2Fzn, ["",".exe"], [readable], _Mzn2FznExe),

		( concat_string([MznDir,"bin/solns2out"], Solns2Out)
		; concat_string([MznDir,"solns2out"], Solns2Out)
		),
		existing_file(Solns2Out, ["",".exe"], [readable], _Solns2OutExe)
	    ->
		setval(minizinc_dir, MznDir),
		setval(mzn2fzn_exe, Mzn2Fzn),
		setval(solns2out_exe, Solns2Out),
		os_file_name(MznDir, MznDirOS),
		( current_module(toplevel) ->
		    printf(log_output, "%% Using minizinc installation at %w%n", [MznDirOS])
		; true	% be quiet unless interactive
		)
	    ;
		printf(warning_output, "%% No usable minizinc installation found in either of:%n", []),
		( foreach(Dir,Dirs) do printf(warning_output, "%% %w%n", [Dir]) ),
		printf(warning_output, "%% Will rely on PATH instead%n", [])
	    )
	)).

% Global counter for generating temp file name
:- local variable(tmpcnt, 1).

:- lib(lists).
:- use_module(flatzinc).
:- reexport struct(_) from flatzinc.


% redefined the of/2 expansion to avoid warnings for "array/set of ..."
:- export macro((of)/2, tr_of/2, []).
tr_of(OfTerm, Expanded) :-
	OfTerm =..[of,_,B],
	atom(B),
	\+ fzn_simple_type(B),
	expand_macros(OfTerm, Expanded)@eclipse_language.


%----------------------------------------------------------------------
% Top level predicates
%----------------------------------------------------------------------

:- export mzn_run/2.
:- comment(mzn_run/2, [
    summary:"Run a MiniZinc model from a given file",
    amode:(mzn_run(+,++) is det),
    args:["File":"File name (extension defaults to .mzn)",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure"],
    see_also:[fzn_run/2, mzn_run/3, mzn_run_string/2, struct(zn_options)],
    desc:html("<P>
	Reads a MiniZinc model from a file, and interprets it using
	the solver mapping defined in SolverOrOptions.  At the end of
	solving, results are printed to the output stream, timing and
	progress messages are printed to the log_output stream, warnings
	to the warning_output stream, and error messages the error stream.
	This predicate always succeeds.
    </P>"),
    eg:"
    ?- mzn_run(\"mymodel.mzn\", fzn_ic).
    Found a solution with cost 10
    Found no solution with cost 7.0 .. 9.0
    end = 10
    b1 = 1
    b2 = 0
    b3 = 1
    b4 = 0
    Objective value = 10
    Total time 0.031s cpu (0.016 setup + 0.000 search)

    ?- mzn_run(queens8, zn_options{solver:fzn_ic,solutions:3}).
    Starting search
    q = [1,5,8,6,3,7,2,4]
    Total time 0.016s cpu (0.016 setup + 0.000 search)
    q = [1,6,8,3,7,4,2,5]
    Total time 0.016s cpu (0.016 setup + 0.000 search)
    q = [1,7,4,6,8,2,5,3]
    Total time 0.016s cpu (0.016 setup + 0.000 search)
"]).
mzn_run(ModelFile, SolverOrOptions) :-
	mzn_run(ModelFile, [], SolverOrOptions).


:- export mzn_run/3.
:- comment(mzn_run/3, [
    summary:"Run a MiniZinc model from a given model and instance file",
    amode:(mzn_run(+,+,++) is det),
    args:["ModelFile":"File name (extension defaults to .mzn)",
	"InstFileOrParMap":"Instance file name (extension defaults to .dzn), or list of Id=Term correspondences",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure"],
    see_also:[mzn_run/2, mzn_run_string/2, struct(zn_options)],
    desc:html("<P>
	Reads a MiniZinc model (given a model file and an instance
	file) and interprets it using the solver mapping defined in
	SolverOrOptions.  At the end of solving, results are printed
	to the output stream, timing and progress messages are printed
	to the log_output stream, warnings to the warning_output
	stream, and error messages the error stream.  This predicate
	always succeeds.
    </P>"),
    eg:"
    ?- mzn_run(\"mymodel.mzn\", \"myinstance.mzn\", fzn_ic).
    Found a solution with cost 10
    Found no solution with cost 7.0 .. 9.0
    end = 10
    b1 = 1
    b2 = 0
    b3 = 1
    b4 = 0
    Objective value = 10
    Total time 0.031s cpu (0.016 setup + 0.000 search)

    ?- mzn_run(\"queens.mzn\", [n=8], fzn_ic).
    Starting search
    q = [1,5,8,6,3,7,2,4]
    Total time 0.015s cpu (0.000 setup + 0.000 search)
"]).
mzn_run(ModelFile0, ParMapOrFile, SolverOrOptions) :-
	zn_options(SolverOrOptions, Options),
	( is_list(ParMapOrFile) ->
	    pars_to_instancefile(ParMapOrFile, MznInstFile, Options),
	    mzn2fzn(ModelFile0, MznInstFile, Options, FznStream, PidOrFile, OznFile),
	    delete_file(MznInstFile)
	;
	    mzn2fzn(ModelFile0, ParMapOrFile, Options, FznStream, PidOrFile, OznFile)
	),
	zn_options{mzn_output:SolOut,fzn_output:RawOut} = Options,
	( nonvar(RawOut) ->
	    fzn_run_stream(FznStream, Options)
	;
	    % pass fzn output through solns2out
	    getval(solns2out_exe, Solns2Out),
	    start_filter([Solns2Out,OznFile], RawOut, SolOut, Filter),
	    fzn_run_stream(FznStream, Options),
	    finalize_filter(RawOut, Filter)
	),
	mzn2fzn_cleanup(PidOrFile),
	delete_file(OznFile).


:- export mzn_run_string/2.
:- comment(mzn_run_string/2, [
    summary:"Run a MiniZinc model given as a string or list",
    amode:(mzn_run_string(++,++) is det),
    args:["MznModel":"String, Atom or List of constants",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure"],
    see_also:[mzn_run/2, mzn_run/3, struct(zn_options)],
    desc:html("<P>
	Solves the MiniZinc model MznModel, given in the simplest form
	as a string in MiniZInc syntax.  The problem is solved using
	a mapping to a concrete ECLiPSe solver, as specified in the
	SolverOrOptions argument.  Search and output are done according
	to the model's solve and output items.
    </P><P>
	Note that, because of the rules for escaping characters within
	ECLiPSe strings, any backslashes in the MiniZinc source have
	to be doubled, and double quotes must be escaped with a backslash!
    </P><P>
	Obviously, one would like to pass parameters into a model.  The
	model can therefore  be given as a list of strings in MiniZinc
	syntax, interleaved with ECLiPSe ground terms that serve as 
	parameter instantiations.  The actual MiniZinc model then
	consists of the concatenation of all these parts.
    </P>"),
    eg:"
    ?- mzn_run_string(\"
		int: n = 8;
		array [1..n] of var 1..n: q;
		constraint
		    forall (i in 1..n, j in i+1..n) (
			q[i]     != q[j]     /\\\\
			q[i] + i != q[j] + j /\\\\
			q[i] - i != q[j] - j
		    );
		solve satisfy;
	    \", fzn_ic).

    Starting search
    q = [1,5,8,6,3,7,2,4]
    Total time 0.020s cpu (0.020 setup+ 0.000 search)
    Yes (0.02s cpu, solution 1, maybe more)


    ?- N=8, mzn_run_string([\"
		int: n = \",
	    N, \";
		array [1..n] of var 1..n: q;
		constraint
		    forall (i in 1..n, j in i+1..n) (
			q[i]     != q[j]     /\\\\
			q[i] + i != q[j] + j /\\\\
			q[i] - i != q[j] - j
		    );
		solve satisfy;
	    \"], fzn_ic).

    Starting search
    q = [1,5,8,6,3,7,2,4]
    Total time 0.020s cpu (0.020 setup+ 0.000 search)
    N = 8
    Yes (0.02s cpu, solution 1, maybe more)
</PRE>
"]).

mzn_run_string(MznModel, SolverOrOptions) :-
	mzn_run_string(MznModel, SolverOrOptions, []).

mzn_run_string(MznModel, SolverOrOptions, ParMap) :-
	(
	    mzn_load_string(MznModel, SolverOrOptions, ParMap, [], State),
	    fzn_search(State),
	    fzn_output(State),
	    writeln(----------),
	    fzn_last(State),
	    !
	;
	    writeln(==========)
	).


:- export mzn_load_string/5.
:- comment(mzn_load_string/5, [
    summary:"Load a MiniZinc model given as a string or list",
    amode:(mzn_load_string(++,++,++,+,-) is semidet),
    args:["MznModel":"String, Atom or List of constants",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure",
	"ParMap":"List of FznId=ECLiPSeGroundTerm correspondences",
	"VarMap":"List of FznId=ECLiPSeVarTerm correspondences",
	"FznState":"FlatZinc state descriptor"],
    fail_if:"Fails if the constraint setup fails",
    see_also:[mzn_run/2, mzn_run/3, mzn_run_string/2, struct(zn_options)],
    desc:html("<P>
	Loads the MiniZinc model MznModel, given in the simplest form
	as a string in MiniZinc syntax.  The problem is set up using
	a mapping to a concrete ECLiPSe solver, as specified in the
	SolverOrOptions argument.  Neither search nor output are done.
    </P><P>
	Note that, because of the rules for escaping characters within
	ECLiPSe strings, any backslashes in the MiniZinc source have
	to be doubled, and double quotes must be escaped with a backslash!
    </P><P>
	To pass parameters into the model, a ParMap can be given, consisting
	of a list of FznId=ECLiPSeGroundTerm correspondences.  Here, FznId
	is an atom (the FlatZinc parameter identifier within the model),
	and ECLiPSeGroundTerm is the corresponding ECLiPSe constant.
    </P><P>
    	To access the ECLiPSe variables corresponding to the model's
	variables, VarMap can be given, consisting of a list of
	FznId=ECLiPSeTerm correspondences.  Here, FznId is an atom
	(the FlatZinc variable identifier within the model), and
	ECLiPSeTerm is the corresponding ECLiPSe constant, variable
	or array.
    </P><P>
    	The mzn_load_string/5 predicate returns a FlatZinc solver
	state which can be used to lookup further information about
	the model (fzn_var_lookup/3, fzn_obj_lookup/2), to perform
	the standard search (fzn_search/1), or to perform the model's
	output actions (fzn_output/1).
    </P>"),
    eg:"
    ?- mzn_load_string(\"
		int: n;
		array [1..n] of var 1..n: q;
		constraint
		    forall (i in 1..n, j in i+1..n) (
			q[i]     != q[j]     /\\\\
			q[i] + i != q[j] + j /\\\\
			q[i] - i != q[j] - j
		    );
		solve satisfy;
	    \",
	    fzn_ic,
	    [n=8],
	    [q=Q],
	    FznState).

    Q = [](_2492{1..8}, _2512{1..8}, _2532{1..8}, _2552{1..8}, ...]
    FznState = state(...)
    There are 84 delayed goals.
    Yes (0.02s cpu)


    ?- mzn_load_string(\"...\", fzn_ic, [n=8], [q=Q], FznState),
       ic:labeling(Q).

    Q = [](1, 5, 8, 6, 3, 7, 2, 4)
    FznState = state(...)
    Yes (0.03s cpu, solution 1, maybe more)


    ?- mzn_load_string(\"...\", fzn_ic, [n=8], [q=Q], FznState),
       ic:labeling(Q),
       fzn_output(FznState).

    % output from fzn_output:
    q = [1,5,8,6,3,7,2,4];
    % Total time 0.030s cpu (0.020 setup)

    % output from ECLiPSe toplevel:
    Q = [](1, 5, 8, 6, 3, 7, 2, 4)
    FznState = state(...)
    Yes (0.03s cpu, solution 1, maybe more)
</PRE>
"]).
mzn_load_string(MznModel, SolverOrOptions, ParMap, VarMap, State) :-
	zn_options(SolverOrOptions, Options),
	model_to_modelfile(MznModel, MznFile),
	pars_to_instancefile(ParMap, MznInstFile, Options),
	mzn2fzn(MznFile, MznInstFile, Options, FznStream, PidOrFile, OznFile),
	fzn_init(Options, State),
	( block(fzn_load_stream(FznStream, State), Tag,
		(mzn_load_cleanup(PidOrFile, MznInstFile, OznFile),
		delete_file(MznFile),
		exit_block(Tag)))
	->
	    mzn_load_cleanup(PidOrFile, MznInstFile, OznFile),
	    delete_file(MznFile)
	;
	    mzn_load_cleanup(PidOrFile, MznInstFile, OznFile),
	    delete_file(MznFile),
	    fail
	),
	fzn_ids_to_ecl_vars(VarMap, State).


:- export mzn_load/5.
:- comment(mzn_load/5, [
    summary:"Load a MiniZinc model from a file",
    amode:(mzn_load(++,++,++,+,-) is semidet),
    args:["ModelFile":"File name (extension defaults to .mzn)",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure",
	"InstFileOrParMap":"Instance file name (extension defaults to .dzn), or list of FznId=ECLiPSeGroundTerm correspondences",
	"VarMap":"List of FznId=ECLiPSeVarTerm correspondences",
	"FznState":"FlatZinc state descriptor"],
    fail_if:"Fails if the constraint setup fails",
    see_also:[mzn_run/2, mzn_run/3, mzn_load_string/5, struct(zn_options)],
    desc:html("<P>
	Loads a MiniZinc from ModelFile.  The problem is set up using
	a mapping to a concrete ECLiPSe solver, as specified in the
	SolverOrOptions argument.  Neither search nor output are done.
    </P><P>
	To pass parameters into the model, a ParMap can be given, consisting
	of a list of FznId=ECLiPSeGroundTerm correspondences.  Here, FznId
	is an atom (the FlatZinc parameter identifier within the model),
	and ECLiPSeGroundTerm is the corresponding ECLiPSe constant.
	Alternatively, an instance file can be specified.
    </P><P>
    	To access the ECLiPSe variables corresponding to the model's
	variables, VarMap can be given, consisting of a list of
	FznId=ECLiPSeTerm correspondences.  Here, FznId is an atom
	(the FlatZinc variable identifier within the model), and
	ECLiPSeTerm is the corresponding ECLiPSe constant, variable
	or array.
    </P><P>
    	The mzn_load/5 predicate returns a FlatZinc solver
	state which can be used to lookup further information about
	the model (fzn_var_lookup/3, fzn_obj_lookup/2), to perform
	the standard search (fzn_search/1), or to perform the model's
	output actions (fzn_output/1).
    </P>"),
    eg:"
    ?- mzn_load(\"queens\", fzn_ic, [n=8], [q=Q], FznState).

    Q = [](_2492{1..8}, _2512{1..8}, _2532{1..8}, _2552{1..8}, ...]
    FznState = state(...)
    There are 84 delayed goals.
    Yes (0.02s cpu)


    ?- mzn_load(\"queens\", fzn_ic, [n=8], [q=Q], FznState),
       ic:labeling(Q).

    Q = [](1, 5, 8, 6, 3, 7, 2, 4)
    FznState = state(...)
    Yes (0.03s cpu, solution 1, maybe more)


    ?- mzn_load(\"queens\", fzn_ic, [n=8], [q=Q], FznState),
       ic:labeling(Q),
       fzn_output(FznState).

    % output from fzn_output:
    q = [1,5,8,6,3,7,2,4];
    % Total time 0.030s cpu (0.020 setup)

    % output from ECLiPSe toplevel:
    Q = [](1, 5, 8, 6, 3, 7, 2, 4)
    FznState = state(...)
    Yes (0.03s cpu, solution 1, maybe more)
</PRE>
"]).
mzn_load(MznFile, SolverOrOptions, ParMapOrFile, VarMap, State) :-
	zn_options(SolverOrOptions, Options),
	( is_list(ParMapOrFile) ->
	    pars_to_instancefile(ParMapOrFile, TmpMznInstFile, Options),
	    mzn2fzn(MznFile, TmpMznInstFile, Options, FznStream, PidOrFile, OznFile)
	;
	    mzn2fzn(MznFile, ParMapOrFile, Options, FznStream, PidOrFile, OznFile)
	),
	fzn_init(Options, State),
	( block(fzn_load_stream(FznStream, State), Tag,
		(mzn_load_cleanup(PidOrFile, TmpMznInstFile, OznFile), exit_block(Tag)))
	->
	    mzn_load_cleanup(PidOrFile, TmpMznInstFile, OznFile)
	;
	    mzn_load_cleanup(PidOrFile, TmpMznInstFile, OznFile),
	    fail
	),
	fzn_ids_to_ecl_vars(VarMap, State).


    model_to_modelfile(MznModel, MznFile) :-
	make_tmpfile(mod, MznFile),
	open(MznFile, write, MznStream),
	(
	    ( MznModel = [_|_] ->
		% Crude way to insert ECLiPSe parameters into MiniZinc source
		( foreach(Part,MznModel), param(MznStream) do
		    fzn_write(MznStream, Part)
		)
	    ;
		(string(MznModel);atom(MznModel)),
		write(MznStream, MznModel)
	    )
	->
	    close(MznStream)
	;
	    close(MznStream),
	    delete_file(MznFile),
	    fzn_error("Malformed Model", [MznModel])
	).


    pars_to_instancefile([], _MznInstFile, _Options) ?- !.
    pars_to_instancefile(ParMap, MznInstFile, zn_options{solver:Solver}) :-
	% Check the ParMap
	ground(ParMap),
	is_list(ParMap),
	( foreach(TyId=_Value,ParMap) do
	    ( TyId = (Ty:Id) ->
	    	atom(Id),
		fzn_type(Ty)
	    ;
		atom(TyId)
	    )
	),
	!,
	make_tmpfile(inst, MznInstFile),
	open(MznInstFile, write, Stream),
	writeln(Stream, "% Generated instance file"),
	( foreach(TyId=Value,ParMap), param(Stream,Solver) do
	    ( TyId = (Ty:Id) ->
		printf(Stream, "%w = ", [Id]),
		fzn_write(Stream, Value, Ty, Solver)
	    ;
		printf(Stream, "%w = ", [TyId]),
		fzn_write(Stream, Value)
	    ),
	    writeln(Stream, ";")
	),
	close(Stream).
    pars_to_instancefile(ParMap, _MznInstFile, _Options) :-
	fzn_error("Illegal ParMap: %w", [ParMap]).

    % 
    fzn_type(no_macro_expansion(array(_) of T)) ?- !,
	fzn_simple_type(T).
    fzn_type(no_macro_expansion(set of T)) ?- !,
	fzn_scalar_type(T).
    fzn_type(T) :-
	fzn_simple_type(T).

    fzn_scalar_type(bool).
    fzn_scalar_type(int).

    fzn_simple_type(bool).
    fzn_simple_type(int).
    fzn_simple_type(float).


    mzn2fzn_cleanup(PidOrFile) :-
	( number(PidOrFile) ->
	    ( wait(PidOrFile, _Status) -> true ; true )
	;
	    delete_file(PidOrFile)
	).

    make_tmpfile(What, File) :-
	getval(tmpcnt, I),
	incval(tmpcnt),
	get_flag(tmp_dir, Dir),
	get_flag(pid, Pid),
	get_flag(unix_time, Time),
	minizinc_suffix(What, Suffix),
	concat_string([Dir,ecl_,What,I,"_",Time,"_",Pid,Suffix], File).

    minizinc_suffix(mod,  '.mzn').
    minizinc_suffix(inst, '.dzn').


    delete_file(File) :-
	( nonvar(File), exists(File) -> delete(File) ; true ).

    fzn_ids_to_ecl_vars(VarMap, State) :-
	( foreach(Id=EclVar,VarMap), param(State) do
	    ( fzn_var_lookup(State, Id, EclVar) ->
	    	true
	    ;
		fzn_error("No such id in the model: %w", [Id])
	    )
	).

    mzn_load_cleanup(PidOrFile, MznFile, OznFile) :-
	mzn2fzn_cleanup(PidOrFile),
	delete_file(MznFile),
	delete_file(OznFile).


%----------------------------------------------------------------------
% Invoke the MiniZinc->FlatZinc converter
% ModelFile should be file name with or without .mzn extension.
% DataFile can be a variable, or like ModelFile.
% Pipe FlatZinc output into FznStream, or produce intermediate .fzn file,
% depending on the flag UseFznFile.
% If output is piped, PidOrFile is process id to be waited for.
% If output is via file, PidOrFile is .fzn file to be deleted.
%----------------------------------------------------------------------

mzn2fzn(ModelFile0, DataFile0,
		zn_options{solver:Solver,fzn_tmp:OutFlag,optimize:Optimize},
		FznStream, PidOrFile, OznFile) :-
	( existing_file(ModelFile0, ["",".mzn"], [readable], ModelFile) ->
	    os_file_name(ModelFile, ModelFileOS)
	;
	    fzn_error("No such file: %w", [ModelFile0])
	),
	( (atom(DataFile0);string(DataFile0)) ->
	    ( existing_file(DataFile0, ["",".dzn"], [readable], DataFile) ->
		os_file_name(DataFile, DataFileOS),
		Params0 = ["--data",DataFileOS,ModelFileOS]
	    ;
		fzn_error("No such file: %w", [DataFile0])
	    )
	;
	    Params0 = [ModelFileOS]
	),
	getval(here, EclZincLib),
	concat_string([EclZincLib,Solver], EclZincSolverSpecificLib),
	os_file_name(EclZincSolverSpecificLib, EclZincSolverSpecificLibOS),
	getval(minizinc_dir, MznDir),
	getval(mzn2fzn_exe, Mzn2Fzn),
	( Optimize==on -> Params1 = Params0
	; Params1 = ["--no-optimize"|Params0]
	),
	(
	    MznDir \== '',
	    ( SubDir="lib" ; SubDir="share" ),
	    concat_string([MznDir, SubDir, "/minizinc"], ZincDefaultLib),
	    exists(ZincDefaultLib)
	->
	    % Assume we are calling the mzn2fzn exectuable
	    % without any environment variable setting
	    os_file_name(ZincDefaultLib, ZincDefaultLibOS),
	    Params = ["-I",EclZincSolverSpecificLibOS,
		      "--stdlib-dir",ZincDefaultLibOS|Params1]
	;
	    % Hope the exectuable knows its stdlib-dir
	    Params = ["-I",EclZincSolverSpecificLibOS|Params1]
	),
	( OutFlag==file ->
	    % use intermediate fzn file, and echo any stderr on error
	    ( var(PidOrFile) ->
		pathname(ModelFile, Path, Base, _Mzn),
		concat_string([Path,Base,".fzn"], PidOrFile)
	    ;
		pathname(PidOrFile, Path, Base, _Fzn)
	    ),
	    os_file_name(PidOrFile, FznFileOS),
	    concat_string([Path,Base,".ozn"], OznFile),
	    os_file_name(OznFile, OznFileOS),
%	    writeln(exec([Mzn2Fzn,"--output-to-file",FznFileOS|Params], [null,null,Err], Pid)),
	    exec([Mzn2Fzn,
		"--output-fzn-to-file",FznFileOS,
		"--output-ozn-to-file",OznFileOS
	    	|Params], [null,null,Err], Pid),
	    read_string(Err, end_of_file, "", _, Message),
	    write(error, Message),
	    wait(Pid, Status),
	    ( Status == 0 -> true ;
		fzn_error("mzn2fzn exited with status %16r", [Status])
	    ),
	    open(PidOrFile, read, FznStream)
	;
	    pathname(ModelFile, Path, Base, _Mzn),
	    concat_string([Path,Base,".ozn"], OznFile),
	    os_file_name(OznFile, OznFileOS),
	    % pipe the fzn - we can't easily handle the error output
	    % without running the risk of blocking
%	    writeln(exec([Mzn2Fzn,"--output-to-stdout"|Params], [null,FznStream], PidOrFile)),
	    exec([Mzn2Fzn,"--output-to-stdout",
		"--output-ozn-to-file",OznFileOS
		|Params], [null,FznStream], PidOrFile)
	).


:- export mzn2fzn/4.
:- comment(mzn2fzn/4, [
    summary:"Convert a MiniZinc model into a FlatZinc model",
    amode:(mzn2fzn(+,+,++,?) is det),
    args:["ModelFile":"File name (extension defaults to .mzn)",
	"InstFileOrParMap":"Instance file name (extension defaults to .dzn), or list of Id=Term correspondences",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure",
	"FznFile":"Name of generated FlatZinc file (will be generated if variable)"],
    see_also:[mzn_run/3, flatzinc:fzn_run/2, struct(zn_options)],
    desc:html("<P>
	Converts a MiniZinc model (given a model file and an instance
	file or parameter map) into a FlatZinc model, by invoking the
	external mzn2fzn converter with the appropriate arguments.
	If no output file name is specified (FznFile uninstantiated),
	the name of the output file is the same as the input file, with
	the extension changed to .fzn.  The options should specify the
	solver that is intended to be used on the FlatZinc model (so that
	the correct version of globals.mzn is used), and the fzn_tmp
	option should be set to 'file' (the default).
    </P><P>
	A file with output instructions is also generated.  It has the same
	base name as FznFile (if given) or ModelFile, with the extension
	changed to .ozn.
    </P>"),
    eg:"
    ?- mzn2fzn(mymodel, [], zn_options{solver:fzn_ic,fzn_tmp:file}, FznFile).
    FznFile = \"mymodel.fzn\"
    Yes (0.00s cpu)
"]).

mzn2fzn(MznFile, ParMapOrFile, SolverOrOptions, FznFile) :-
	zn_options(SolverOrOptions, Options),
	Options = zn_options{fzn_tmp:OutFlag},
	( OutFlag == file ->
	    ( is_list(ParMapOrFile) ->
		pars_to_instancefile(ParMapOrFile, MznInstFile, Options),
		mzn2fzn(MznFile, MznInstFile, Options, FznStream, FznFile, _OznFile),
		delete_file(MznInstFile)
	    ;
		mzn2fzn(MznFile, ParMapOrFile, Options, FznStream, FznFile, _OznFile)
	    ),
	    close(FznStream)
	;
	    fzn_error("Unsupported option fzn_tmp:%w", [OutFlag])
	).


%----------------------------------------------------------------------
% Support for filtering a stream through an external process (solns2out)
% This is complicated in the case where the filter output has to be
% forwarded to another Eclipse stream, in particular a yield-queue
% (as in tkeclipse).
%----------------------------------------------------------------------

% Filter stream From through Command with output to To.
% The To stream must exist (anything, including queue).
% The From stream will be newly created (a pipe) and should be closed later.
% Handle is used to refer to this filter.
% The caller should flush To occasionally to see the filter output.
start_filter(Command, From, To, Handle) :-
	( get_stream_info(To, fd, 1) ->
	    % The filter output can go directly to its stdout
	    exec(Command, [From], Handle)
	;
	    % Assume To is a queue stream (e.g. in tkeclipse):
	    % Use a thread to forward the filter output to To.
	    % Don't flush To within thread, as it may yield and block!
	    exec(Command, [From,Processed], Pid),
	    %set_stream_property(From, flush, end_of_line),
	    engine_create(Handle, [local(1000),global(10000)]),
	    engine_resume_thread(Handle, do_transfer(Processed,To,Pid))
	).

    do_transfer(From, To, Pid) :-
	read_string(From, "\n", "", Sep, String),
	write(To, String),	% just copy to buffer (including \r, if any)
	( Sep >= 0 ->
	    put(To, Sep),	% don't flush (would block for yield-queue)
	    do_transfer(From, To, Pid)
	;
	    wait(Pid, Status),
	    exit(Status)
	).

finalize_filter(From, Handle) :-
	close(From),		% should cause filter to terminate
	( integer(Handle) ->
	    wait(Handle, _Status)
	; engine_join(Handle, 10, _Status) ->
	    true
	;
	    printf(warning_output,
	    	"%% Timeout while waiting for output filter termination%n", [])
	).


%----------------------------------------------------------------------
% Toplevel predicate for use with command-line option
% We expect eclipse to be invoked as
% eclipse -e minizinc:mzn_run -- <minizinc-compatible options>
% 
%			mzn_run				fzn_run
% -b <backend>		solver:<backend>
% -d <datafile>		(argument)
% -n <num>		->				solutions:<num>
% -a			->				solutions:all
% -s			->				statistics:on
% --no-optimize		optimize:off
% -t <sec>		->				timeout:<sec>
% -o <outputfile>	output:Stream
% --raw			output:null,fzn_output:Stream
% --no-log		log_output=null
%----------------------------------------------------------------------

:- export mzn_run/0.
:- comment(mzn_run/0, [
    summary:"Run a MiniZinc model using command line arguments",
    amode:(mzn_run is det),
    args:[],
    see_also:[mzn_run/2, mzn_run/3, struct(zn_options)],
    desc:html("<P>
    Runs a MiniZinc model, taking all the necessary parameters from
    the process command line.  The recommended way to use this
    predicate is by calling eclipse from a command line as:
<PRE>
        eclipse -e minizinc:mzn_run -- [&lt;options&gt;] &lt;model&gt;.mzn [&lt;data&gt;.dzn]
</PRE>
    (note the <CODE>--</CODE> that separates eclipse's own arguments from
    the arguments interpreted by mzn_run/0).
</P><P>
    The following options are recognized:
<DL>
    <DT>&lt;model&gt;.mzn</DT><DD>
        a minizinc model file, with mandatory .mzn extension.
    </DD>
    <DT>&lt;data&gt;.dzn</DT><DD>
        a minizinc data file, with mandatory .dzn extension.
    </DD>
    <DT>-a</DT><DD>
        compute all solutions. This is currently only meaningful with 
	satisfaction problems.
    </DD>
    <DT>-b &lt;solver&gt;</DT><DD>
        selects the solver backend (fzn_ic, fzn_eplex, fzn_fd).
        The default is fzn_ic.
    </DD>
    <DT>-d &lt;datafile&gt;</DT><DD>
        an alternative way to specify a data file.
    </DD>
    <DT>-n &lt;num&gt;</DT><DD>
        compute at most the given number of solutions.  This is currently
	only meaningful with satisfaction problems.  Default is 1.
    </DD>
    <DT>--no-optimize</DT><DD>
        pass the --no-optimize option to mzn2fzn, which can improve
	performance on large models.
    </DD>
    <DT>--no-log</DT><DD>
        suppress all output printed to the log_output stream.  Equivalent
	to calling set_stream(log_output,null).
    </DD>
    <DT>-o &lt;outputfile&gt;</DT><DD>
        redirect regular solver output to the given file.  Note that
	log_output, warning_output and error output are not redirected.
    </DD>
    <DT>--raw</DT><DD>
        print the raw flatzinc output (corresponding to the flatzinc output
	annotations) instead of the postprocessed (via solns2out) output
	corresponding to the minizinc output items.
    </DD>
    <DT>-s</DT><DD>
        after each solution, print a statistics-comment as part of the
	output.  Without this option, this information goes to the log_output
	stream, which can be suppressed if desired.
    </DD>
    <DT>-t &lt;seconds&gt;</DT><DD>
        solver timeout in seconds.  This does not apply to the Minizinc-to-
	Flatzinc translation (mzn2fzn), but only to the solver setup and
	search phases.  With satisfaction problems, a timeout may lead to
	fewer solutions, with optimization problems to a suboptimal solution,
	or no solution at all.
    </DD>
</DL>
    Note: currently, only a single data file can be specified.
    </P>"),
    eg:"
   $ eclipse -e minizinc:mzn_run -- --no-log -a send-more-money.mzn 
      9567
   +  1085
   = 10652
   ----------
   ==========
"]).

mzn_run :-
	argv(all, [_Cmd|Opts]),
	extract_solver_options(Opts, Opts1, ModelFileOS, DataFileOS, OutFileOS, ZnOptions),
	( var(ModelFileOS) ->
	    fzn_error("Model file argument <file>.mzn expected%n", [])
	;
	    os_file_name(ModelFile, ModelFileOS)
	),
	( Opts1 = [] -> true ;
	    fzn_error("Unrecognized command line option(s):%n%w%n", [Opts1])
	),
	( var(DataFileOS) -> DataFile = [] ;
	    os_file_name(DataFile, DataFileOS)
	),
	% Open output file, if any
	( var(OutFileOS) -> true ;
	    zn_options{output:OutStream} = ZnOptions,
	    var(OutStream),
	    os_file_name(OutFile, OutFileOS),
	    open(OutFile, write, OutStream)
	),
	mzn_run(ModelFile, DataFile, ZnOptions),	% may abort
	( var(OutFileOS) -> true ;
	    close(OutStream)
	).


extract_solver_options(Opts, Others, ModelFile, DataFile, OutFile, ZnOptions) :-
	ZnOptions = zn_options{},
	(
	    fromto(Opts,Opts1,Opts2,[]),
	    fromto(Others,Others1,Others2,[]),
%	    fromto(DataFiles,DataFiles1,DataFiles2,[]),
	    param(ZnOptions,ModelFile,DataFile,OutFile)
	do
	    (
		( Opts1 = ["-a"|Opts2] ->
		    ZnOptions = zn_options{solutions:0},
		    Others1 = Others2
		; Opts1 = ["-b",BackendS|Opts2], atom_string(Backend,BackendS) ->
		    ZnOptions = zn_options{solver:Backend},
		    Others1 = Others2
		; Opts1 = ["-d",File|Opts2] ->
		    DataFile = File,
		    Others1 = Others2
		; Opts1 = ["-n",NSolS|Opts2], number_string(NSol,NSolS), integer(NSol), NSol>0 ->
		    ZnOptions = zn_options{solutions:NSol},
		    Others1 = Others2
		; Opts1 = ["-o",OutFile|Opts2] ->
		    Others1 = Others2
		; Opts1 = ["--no-log"|Opts2] ->
		    set_stream(log_output, null),
		    Others1 = Others2
		; Opts1 = ["--raw"|Opts2] ->
		    ZnOptions = zn_options{output:Out,mzn_output:null,fzn_output:Out},
		    Others1 = Others2
		; Opts1 = ["-s"|Opts2] ->
		    ZnOptions = zn_options{statistics:on},
		    Others1 = Others2
		; Opts1 = ["-t",TimeoutS|Opts2], number_string(Timeout,TimeoutS), Timeout>=0 ->
		    ZnOptions = zn_options{timeout:Timeout},
		    Others1 = Others2
		; Opts1 = [File|Opts2], pathname(File,_,_,".mzn") ->
		    ModelFile = File,
		    Others1 = Others2
		; Opts1 = [File|Opts2], pathname(File,_,_,".dzn") ->
		    DataFile = File,
		    Others1 = Others2
		; Opts1 = [Other|Opts2] ->
		    Others1 = [Other|Others2]
		)
	    ->
		true
	    ;
		fzn_error("Unrecognized or incompatible command line option:%n%w%n", [Opts1])
	    )
	).

