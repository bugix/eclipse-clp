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
% The Original Code is  The best_first_search library for ECLiPSe.
% The Initial Developer of the Original Code is  Joachim Schimpf.
% Portions created by the Initial Developer are
% Copyright (C) 2010 Joachim Schimpf.
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------

%
% TODO:
%       - automatically detect modified domains (lib(changeset))
%       - pass reduced variables argument to the separator (how?)
% Options:
%	- delta
%	- separate-priority (so partial propagation can be used for cost estimate)
%	- solution-test goal
%	- heuristic argument (replacing normal argument)
%	- bestsofar variable (useful?)
%       - choice of recording full domains OR only branching decisions


:- module(best_first_search).

:- lib(heaps).          % for priority queue of open nodes

:- local struct(node(
	cost_lwb,       % number: lwb for internal nodes, cost for leaves
	heuristic,      % number: >= cost_lwb
	constraint,     % ground descriptor for branching constraint
	                % leading to this node
	parent          % struct(node), or [] at root
    )).

verify(Goal) :- (Goal -> true ; printf(error, "Check failed: %w%n", [Goal]), abort).

:- export bfs_minimize/3.
:- tool(bfs_minimize/3,bfs_minimize_/4).
bfs_minimize_(Xs, Cost, Separator, Module) :-
        bfs_minimize_(Xs, Cost, Cost, Separator, Module).

:- export bfs_minimize/4.
:- tool(bfs_minimize/4,bfs_minimize_/5).
bfs_minimize_(Xs, Cost, Heur, Separator, Module) :-
	get_var_bounds(Cost, CostLwb, CostUpb),
	set_var_bounds(Heur, CostLwb, CostUpb),
	get_var_bounds(Heur, HeurLwb, _),
	Root = node{parent:[], constraint:true, cost_lwb:CostLwb, heuristic:HeurLwb},
	list_to_heap([HeurLwb-Root], Nodes),
	bfs_iterate(Xs, Cost, Heur, Nodes, Separator, CostUpb, nonode, 1, Module).

bfs_iterate(Xs, Cost, Heur, Nodes, Separator, CostUpb, Best, Delta, Module) :-
%	heap_size(Nodes, NNodes), printf(log_output, "Heap size %w%n", [NNodes]),
	( get_from_heap(Nodes, _ThisHeur, ThisNode, Nodes1) ->
            ThisNode = node{cost_lwb:ThisCost},
	    ( Best = node{cost_lwb:BestCost}, BestCost =< ThisCost ->
		% No longer interesting, we already have a better solution.
		bfs_iterate(Xs, Cost, Heur, Nodes1, Separator, CostUpb, Best, Delta, Module)
	    ;
%		writeln(picked(ThisNode)),
		findall(Child,
		    child_node(ThisNode, CostUpb, Xs, Cost, Heur, Separator, Module, Child),
		    Children),
                ( Children = [solved(SolCost)] ->
                    % Fix ThisNode's cost and make it a leaf
                    update_struct(node, [cost_lwb:SolCost], ThisNode, SolNode),
                    update_best(SolNode, Best, Best3, CostUpb, CostUpb3, Delta, Cost),
                    Nodes4 = Nodes1
                ;
                    sort(1, >=, Children, LeavesFirst),
                    (
                        foreach(Type-ChildNode,LeavesFirst),
                        fromto(Best,Best1,Best2,Best3),
                        fromto(CostUpb,CostUpb1,CostUpb2,CostUpb3),
                        fromto(Nodes1,Nodes2,Nodes3,Nodes4),
                        param(ThisNode,Delta,Cost)
                    do
                        ( Type == leaf, 	% early detection of a solution
                            Nodes3 = Nodes2,
                            ChildNode = node{parent:ThisNode},
                            update_best(ChildNode, Best1, Best2, CostUpb1, CostUpb2, Delta, Cost)

                        ; Type == branch,        % new node with heuristics
                            Best2 = Best1, CostUpb2 = CostUpb1,
                            ChildNode = node{parent:ThisNode,heuristic:ChildHeur,cost_lwb:ChildLwb},
                            ( ChildLwb =< CostUpb1 ->
                                add_to_heap(Nodes2, ChildHeur, ChildNode, Nodes3)
                            ;
                                Nodes3 = Nodes2         % no longer useful
                            )
                        )
                    )
                ),
		bfs_iterate(Xs, Cost, Heur, Nodes4, Separator, CostUpb3, Best3, Delta, Module)
	    )
	;
	    % finished (no more nodes)
            Best = node{cost_lwb:BestCost},  % fail if no solution at all
            reconstruct_state(Best, BestCost, Xs, Cost, Module)
	).


child_node(Node, CostUpb, Xs, Cost, Heur, Separator, Module, Child) :-
	reconstruct_state(Node, CostUpb, Xs, Cost, Module),    % may fail
	call(Separator)@Module,			% may fail
        arity(Separator, N),
	arg(N, Separator, IdxVal),
        ( IdxVal == true ->
            verify(number(Cost)),
            Child = solved(Cost)
        ;
            ( ground(Cost-Xs) ->        % should be user-defined
%            ( nonvar(Cost), is_solution(Xs) ->
                Child = leaf-node{heuristic:Cost,cost_lwb:Cost,constraint:IdxVal}
            ;
                get_var_bounds(Cost, Lwb, Upb),
                get_var_bounds(Heur, HeurLwb, _),
                % force heuristic value into the cost bounds
                ChildHeur is min(max(Lwb,HeurLwb),Upb),
                Child = branch-node{heuristic:ChildHeur,cost_lwb:Lwb,constraint:IdxVal}
            )
        ).


update_best(NewNode, OldBest, NewBest, OldUpb, NewUpb, Delta, _Cost) :-
        NewNode = node{cost_lwb:NewCost},
        ( OldBest = node{cost_lwb:OldBestCost}, NewCost >= OldBestCost ->
            % forget this solution
%           printf(log_output, "Found inferior solution with cost %w%n", [NewCost]),
            NewBest = OldBest, NewUpb = OldUpb
        ;
            % Improvement over old best
            printf(log_output, "Found a solution with cost %w%n", [NewCost]),
            % The following line is redundant, since the cost bound
            % gets imposed in reconstruct_state/5.
            % It could affect propagation times, but either way.
%            set_var_bounds(_Cost, -1.0Inf, NewCost),
            NewBest = NewNode,
            NewUpb is min(OldUpb, NewCost-Delta)
        ).


% Establish domain state at Node, and impose CostUpb.
% Fail if Node=nonode or if state leads to failure.
reconstruct_state(Node, CostUpb, Xs, Cost, Module) :-
        call_priority((
            (
                fromto(Node,node{constraint:Cons,parent:Parent},Parent,[]),
                param(Xs,Module) 
            do
                apply_cons(Cons, Xs, Module)
            ),
            set_var_bounds(Cost, -1.0Inf, CostUpb)
        ), 2).

    apply_cons(true, _Xs, _).
    apply_cons(I-Val, Xs, Module) ?-
        arg(I, Xs, X),
        ( var(Val) -> true
        ; atomic(Val) -> X=Val
        ; Val=ge(Min) -> set_var_bounds(X, Min, 1.0Inf)
        ; Val=le(Max) -> set_var_bounds(X, -1.0Inf, Max)
        ; Val=gt(Min) -> call(X#>Min)@Module
        ; Val=lt(Max) -> call(X#<Max)@Module
        ; Val=ne(Num) -> call(X#\=Num)@Module
        ; Val=(G1,G2) -> apply_cons(X,G1,Module), apply_cons(X,G2,Module)
        ). 

%----------------------------------------------------------------------
% Documentation
%----------------------------------------------------------------------

:- comment(categories, ["Algorithms"]).
:- comment(summary, "Best first search").
:- comment(author, "Joachim Schimpf, Monash University").
:- comment(date, "$Date: 2016/08/12 10:57:29 $").
:- comment(index, ["branch-and-bound","search","best-first search"]).

:- comment(bfs_minimize/4, [
    summary:"Minimization using best-first search",
    args:[
        "Vars":"Array of problem variables",
        "Cost":"Cost variable",
        "Heur":"Heuristic variable",
        "Separator":"Separator goal"],
    desc:html("<P>
        Cost is the problem's cost variable: whenever we have a solution,
        Cost must be instantiated and represent the cost of the solution.
        At all other times, Cost's lower bound is a lower bound on the
        cost in the current state. If Cost is not updated by propagation,
        it can be passed as an argument into the separation goal, which
        can then instantiate it to the solution cost, resp. the lower
        bound of the cost of a branch.
</P><P>
        Heur is the problem's heuristic variable: its lower bound is
        taken as an estimate for the cost that a solution derived from
        the current computation state might have. Like Cost, it can either
        be computed by propagation, or the separation goal can instantiate
        it explicitly for every branch. The heuristic value should always
        lie between the bounds of the cost variable. In the simplest case,
        Heur can be the same as Cost.
</P><P>
        The Separator goal has two purposes: to detect when we have a
	solution, and to nondeterministically generate branches (together
	with their cost bound and a heuristic estimate of their quality).
        The separation goal has user-defined arguments, except that it
        must return its results in the last argument.  It will be called
        with the instantiation state corresponding to the current search
        node, and should succeed once for every branch, returning:
	<DL>
        <DT><STRONG>true</STRONG></DT><DD>
	    iff the node is already a solution without further branching.
            Cost must then be instantiated to the solution's cost.
            Heur is ignored in this case and can remain undefined.

        <DT><STRONG>I-C</STRONG></DT><DD>
	    for a branch that imposes constraint C onto variable X[I].
	    Cost must have a lower bound corresponding to this choice,
	    and Heur must a have a lower bound (or be instantiated to)
	    a heuristics estimate for a solution cost that this branch
	    could lead to.  Heur should lie between the cost lower and
	    upper bounds (if it is outside, it will be rounded into
	    this interval).
	</DL>
</P><P>
	The branching constraint descriptor I-C can have the following forms:
<PRE>	
	I-Val        imposes the constraint X[I]=Val (Val is atomic)
	I-ge(Val)    imposes lower numeric bound Val onto X[I]
	I-le(Val)    imposes upper numeric bound Val onto X[I]
	I-ne(Val)    imposes the constraint X[I] #\\= Val
	I-lt(Val)    imposes the constraint X[I] #< Val
	I-gt(Val)    imposes the constraint X[I] #> Val
	(C1,C2)      imposes conjunction of descriptor C1 and descriptor C2
</PRE>	
</P>"),
    eg:"
    % A sample separation goal.
    % The first argument is expected to be an Index-Variable list
    % of the problem variables.  The separation result(s) are
    % returned in the last argument.  The cost variable is expected
    % to be affected by propagation following indomain(X).

	separate([], true).
	separate(IXs, IX) :-
	    delete(I-X, IXs, IXs1, 2, first_fail),
	    ( nonvar(X) -> separate(IXs1, IX)
	    ; IX=I-X, indomain(X)
	    ).

    % Sample call:
    ?- model(XArr),
       ( foreacharg(X,XArr,I), foreach(I-X,IXs) do true ),
       bfs_minimize(XArr, Cost, Cost, separate(IXs,_)).


    % A separation goal that computes a more elaborate heuristic,
    % based on the number of remaining uninstantiated variables.

	separate([], _, _, true).
	separate(IXs, Cost, Est, IX) :-
	    delete(I-X, IXs, IXs1, 2, first_fail),
	    ( nonvar(X) -> separate(IXs1, Cost, Est, IX)
	    ; IX=I-X,
	      indomain(X),
	      term_variables(IXs1,Vs),
	      get_var_bounds(Cost, Lwb, Upb),
	      Est is Upb - (Upb-Lwb)/(length(Vs)+1)
	    ).

    % Sample call:
    ?- model(XArr),
       ( foreacharg(X,XArr,I), foreach(I-X,IXs) do true ),
       bfs_minimize(XArr, Cost, Heur, separate(IXs,Cost,Heur,_)).

    "
    ]).

