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
% The Original Code is  the MDD support library for ECLiPSe
% The Initial Developer of the Original Code is  Joachim Schimpf
% Portions created by the Initial Developer are
% Copyright (C) 2009 - 2017 Joachim Schimpf
% 
% Contributor(s): Joachim Schimpf

% END LICENSE BLOCK


:- module(mdd_support).

:- comment(summary, "Operations on Multi-Valued Decision Diagrams").
:- comment(categories, ["Data Structures"]).
:- comment(author, "Joachim Schimpf").
:- comment(copyright, "Joachim Schimpf, Coninfer Ltd").
:- comment(date, "$Date: 2017/09/28 22:43:43 $").

:- lib(hash).
:- lib(graph_algorithms).


%----------------------------------------------------------------------
% MDD data structure
% We use hash tables during construction, otherwise a graph.
% Node numbering in either case:
%		0	false (only used during construction)
%		1	true
%		2..	proper node
%----------------------------------------------------------------------

% Structure used during MDD construction
:- local struct(mdd_aux(
   	 root,		% the root node (integer)
	 dd,		% map: NodeNr -> node(VarId, SortedArcs)
	 		% realised as a non-logical 'store' or logical hash table
			% (used as an extensible array)
	 rdd		% reverse map: node(VarId, SortedArcs) -> NodeNr
     )).

mdd_init(Root, DD, mdd_aux{root:Root,dd:DD,rdd:RDD}) :-
	store_create(DD),
%	store_set(DD, 1, node(0,[])),	% implicit 'true'-node
	store_create(RDD).


mdd_nodes(mdd_aux{dd:DD}, NNodes) :-
	store_count(DD, NNodes).

mdd_stat(MDD) :-
	mdd_nodes(MDD, NNodes),
	writeln(nodes=NNodes).

mdd_new_node(mdd_aux{dd:DD}, K, SortedArcs, NodeNr) :-
	NodeNr is store_count(DD) + 2,	% node 1 not in table
	Node = node(K, SortedArcs),
	store_set(DD, NodeNr, Node).

% All nodes for variable K
node_for_var(DD, K, NodeNr, Arcs) :-
	stored_keys_and_values(DD, KVs),
	member(NodeNr-node(K,Arcs), KVs).

% Node for K with Arcs, if it exists already
mdd_existing_node(mdd_aux{rdd:RDD}, K, Arcs, NodeNr) :-
	store_get(RDD, node(K,Arcs), NodeNr).

mdd_new_node_rev(mdd_aux{dd:DD,rdd:RDD}, K, SortedArcs, NodeNr) :-
	NodeNr is store_count(DD) + 2,	% node 1 not in table
	Node = node(K, SortedArcs),
	store_set(DD, NodeNr, Node),
	store_set(RDD, Node, NodeNr).

mdd_mk_node(K, Arcs, NodeNr, MDD) :-
    	Arcs = [_|_],	% don't make fail nodes
    	sort(1, =<, Arcs, SortedArcs),
%	( fail ->
	( mdd_existing_node(MDD, K, SortedArcs, NodeNrOld) ->
	    NodeNr = NodeNrOld
	;
	    mdd_new_node_rev(MDD, K, SortedArcs, NodeNr)
	).

mdd_finalise(mdd_aux{rdd:RDD}) :-
	store_erase(RDD).	% free memory (only needed during construction)


%----------------------------------------------------------------------
% Same operations, but with backtrackable hash tables
%----------------------------------------------------------------------

mdd_det_init(Root, DD, mdd_aux{root:Root,dd:DD,rdd:RDD}) :-
	hash_create(DD),
%	hash_set(DD, 1, node(0,[])),	% implicit 'true'-node
	hash_create(RDD).

mdd_det_nodes(mdd_aux{dd:DD}, NNodes) :-
	hash_count(DD, NNodes).

mdd_det_stat(MDD) :-
	mdd_det_nodes(MDD, NNodes),
	writeln(nodes=NNodes).

% Node for K with Arcs, if it exists already
mdd_det_existing_node(mdd_aux{rdd:RDD}, K, Arcs, NodeNr) :-
	hash_get(RDD, node(K,Arcs), NodeNr).

mdd_det_mk_node(K, Arcs, NodeNr, mdd_aux{dd:DD,rdd:RDD}) :-
    	Arcs = [_|_],	% don't make fail nodes
    	sort(1, =<, Arcs, SortedArcs),
	NodeKey = node(K,SortedArcs),
	( hash_get(RDD, NodeKey, NodeNrOld) ->
	    NodeNr = NodeNrOld			% node exists
	;
	    NodeNr is hash_count(DD) + 2,	% node 1 not in table
	    hash_set(DD, NodeNr, NodeKey),
	    hash_set(RDD, NodeKey, NodeNr)
	).



mdd_to_graph(mdd_aux{dd:DD}, Graph) :-
	( is_handle(DD, store) ->
	    stored_keys_and_values(DD, NodeNrNodes),
	    NN is store_count(DD) + 1
	;
	    hash_list(DD, NodeNrNodes),
	    NN is hash_count(DD) + 1
	),
	dim(NodeArr, [NN]),
	arg(1, NodeArr, 0),	% 'true'-node with dummy VarId 0
	(
	    foreach(NodeNr-node(VarId,Arcs),NodeNrNodes),
	    fromto(Edges,Edges1,Edges3,[]),
	    param(NodeArr)
	do
	    arg(NodeNr, NodeArr, VarId),
	    (
		foreach(Vals-SubNode,Arcs),
		fromto(Edges1,Edges1,Edges2,Edges3),
		param(NodeNr)
	    do
		Edges1 = [e(NodeNr,SubNode,Vals)|Edges2]
	    )
	),
	make_graph(NN, Edges, Graph),
	graph_set_nodenames(Graph, NodeArr).


%----------------------------------------------------------------------
% MDD construction from an ordered table of tuples
% The order is only relevant in the sense that tuples with identical
% prefixes must be consecutive.  The MDD is constructed considering
% the tuple arguments left-to-right.
%----------------------------------------------------------------------

:- export ordered_tuples_to_mdd/2.
ordered_tuples_to_mdd([], MDDGraph) :- !,
	make_graph(0, [], MDDGraph).
ordered_tuples_to_mdd(Tuplez, MDDGraph) :- is_array(Tuplez), !,
	array_list(Tuplez, Tuples),
	ordered_tuples_to_mdd(Tuples, MDDGraph).
ordered_tuples_to_mdd(Tuples0, MDDGraph) :-
	[SampleTuple|_] = Tuples0,
	( is_array(SampleTuple) ->
	    Tuples=Tuples0
	;
	    % list of lists -> list of arrays
	    ( foreach(List,Tuples0), foreach(Array,Tuples) do
		array_list(Array, List)
	    )
	),
	[DummyTuple|_] = Tuples,
	mdd_det_init(Root, _DD, MDD),
	level_node(Tuples, TuplesOut, DummyTuple, 1, MDD, Root),
	( TuplesOut == [] -> true ; abort ),
	mdd_to_graph(MDD, MDDGraph).


    % Make sub-MDD node for the sub-table with prefix PrefixTuple[1..K-1],
    % starting at Tuples[I1].
    % K in 1..Arity+1 (if K=1, PrefixTuple is dummy).
    level_node(Tuples0, Tuples, PrefixTuple, K, MDD, Node) :-
	( K > arity(PrefixTuple) ->
	    [_|Tuples] = Tuples0,
	    Node = 1
	;
	    collect_arcs(Tuples0, Tuples, PrefixTuple, K, MDD, Arcs),
	    ( Arcs == [] -> abort ; true ),
	    mdd_det_mk_node(K, Arcs, Node, MDD)
	).

    % Collect arcs for all the different values at argument position K
    % with identical prefix 1..K-1.
    collect_arcs(Tuples0, Tuples, PrefixTuple, K, MDD, Arcs) :-
	(
	    [Tuple|_] = Tuples0,
	    matches_prefix(Tuple, PrefixTuple, K)
	->
	    arg(K, Tuple, Xk),
	    Arcs = [Xk-SubNode|Arcs1],
	    K1 is K+1,
	    % make mdd for sub-table with prefix Tuple[1..K]
	    level_node(Tuples0, Tuples1, Tuple, K1, MDD, SubNode),
	    collect_arcs(Tuples1, Tuples, PrefixTuple, K, MDD, Arcs1)
	;
	    Tuples = Tuples0,
	    Arcs = []
	).
    % Two tuples are identical on position 1..K-1
    matches_prefix(Tuple, PrefixTuple, K) :-
    	( for(I,1,K-1), param(Tuple,PrefixTuple) do
	    arg(I, PrefixTuple, X),
	    arg(I, Tuple, X)
	).


%----------------------------------------------------------------------
% Retrieve all solutions from an MDD
% mdd_solutions(+MDD, ?Xz) is nondet
%----------------------------------------------------------------------

:- export mdd_solutions/2.
mdd_solutions(G, Xz) :-
%	stat_root,
	graph_get_nodenames(G, Nodez),
	arity(Nodez, Root),	% 1=true, NN=root
	( var(Xz) ->
	    N is max(Nodez),
	    dim(Xz, [N])
	;
	    is_array(Xz)
	),
	mdd_solutions(G, Root, Xz),
%	stat_solution,
	true.
mdd_solutions(_, _) :-
%	stat_print,
	fail.

    mdd_solutions(_G, 1, _Xz) :- !.
    mdd_solutions(G, NodeNr, Xz) :-
	NodeNr > 0,
	graph_get_adjacent_edges(G, NodeNr, Edges),
	node_to_nodename(G, NodeNr, K),
	arg(K, Xz, X),
%	( Edges = [_,_|_] -> true ; writeln(trivial_node(Arcs)) ),
%	( Edges = [_,_|_] -> stat_node ; true ),
	member(e(_,SubNodeNr,X), Edges),
%	stat_branch,
	mdd_solutions(G, SubNodeNr, Xz).


%----------------------------------------------------------------------
% Visualize MDD
%----------------------------------------------------------------------

:- export view_mdd/1.
view_mdd(Graph) :-
	call(graphviz:view_graph(Graph, [
	    layout:tree,
%	    layout:left_to_right,
	    graph_attrs:[ordering=out],
	    default_edge_attrs:[color=green],
	    node_attrs_generator:node_attr,
	    edge_attrs_generator:edge_attr])).

node_attr(Graph, NodeNr, [label=Label]) :-
	( NodeNr==1 ->
	    Label="true"
	;
	    node_to_nodename(Graph, NodeNr, VarId),
	    sprintf(Label, "X%d", [VarId])
	).

edge_attr(_Graph, e(_,_,Attr), [label=Attr]).


