%%%-------------------------------------------------------------------
%%% @doc Trait Hierarchy Checking (Task 1.2.6.1)
%%%
%%% Validates trait hierarchies to ensure extends relationships form
%%% a valid directed acyclic graph (DAG) without cycles.
%%%
%%% Provides functions to:
%%% - Check for cycles in trait hierarchies
%%% - Get immediate and transitive supertraits
%%% - Perform topological sort of traits
%%% - Validate extends references exist
%%% @end
%%%-------------------------------------------------------------------
-module(catena_trait_hierarchy).

-export([
    %% Core validation
    check_hierarchy/1,
    is_valid_hierarchy/1,

    %% Trait graph queries
    get_extends/2,
    get_all_supertraits/2,
    topological_sort/1,

    %% Error types
    format_error/1
]).

%%====================================================================
%% Type Definitions
%%====================================================================

%% Trait definition: {trait_name, [supertrait_names], location}
-type trait_def() :: {atom(), [atom()], term()}.

%% Map of trait names to their definitions
-type trait_defs() :: #{atom() => trait_def()}.

%% Error types for hierarchy validation
-type hierarchy_error() ::
    {cycle_detected, [atom()]} |
    {unknown_supertrait, atom(), atom()} |
    {self_extends, atom()}.

-export_type([trait_def/0, trait_defs/0, hierarchy_error/0]).

%%====================================================================
%% Core Validation Functions
%%====================================================================

%% @doc Check that trait definitions form a valid hierarchy (DAG)
%%
%% Returns {ok, valid} if hierarchy is valid, or {error, Errors} if
%% cycles or invalid references are detected.
-spec check_hierarchy(trait_defs()) -> {ok, valid} | {error, [hierarchy_error()]}.
check_hierarchy(TraitDefs) ->
    Errors1 = check_self_extends(TraitDefs),
    Errors2 = check_unknown_supertraits(TraitDefs),
    Errors3 = find_cycles(TraitDefs),
    AllErrors = Errors1 ++ Errors2 ++ Errors3,
    case AllErrors of
        [] -> {ok, valid};
        _ -> {error, AllErrors}
    end.

%% @doc Quick check if hierarchy is valid (no errors)
-spec is_valid_hierarchy(trait_defs()) -> boolean().
is_valid_hierarchy(TraitDefs) ->
    case check_hierarchy(TraitDefs) of
        {ok, valid} -> true;
        {error, _} -> false
    end.

%%====================================================================
%% Trait Graph Queries
%%====================================================================

%% @doc Get immediate supertraits for a trait
-spec get_extends(atom(), trait_defs()) -> {ok, [atom()]} | {error, not_found}.
get_extends(TraitName, TraitDefs) ->
    case maps:get(TraitName, TraitDefs, undefined) of
        undefined -> {error, not_found};
        {_Name, Extends, _Loc} -> {ok, Extends}
    end.

%% @doc Get all supertraits transitively (including indirect)
%%
%% Returns supertraits in topological order (immediate first).
%% Handles cycles by stopping when revisiting a trait.
-spec get_all_supertraits(atom(), trait_defs()) -> {ok, [atom()]} | {error, cycle | not_found}.
get_all_supertraits(TraitName, TraitDefs) ->
    case maps:is_key(TraitName, TraitDefs) of
        false -> {error, not_found};
        true ->
            case collect_supertraits(TraitName, TraitDefs, sets:new(), []) of
                {error, cycle} -> {error, cycle};
                Supertraits -> {ok, lists:reverse(Supertraits)}
            end
    end.

%% Internal: Collect supertraits via DFS
collect_supertraits(TraitName, TraitDefs, Visited, Acc) ->
    case sets:is_element(TraitName, Visited) of
        true -> {error, cycle};
        false ->
            case maps:get(TraitName, TraitDefs, undefined) of
                undefined -> Acc;
                {_Name, Extends, _Loc} ->
                    NewVisited = sets:add_element(TraitName, Visited),
                    collect_extends(Extends, TraitDefs, NewVisited, Acc)
            end
    end.

collect_extends([], _TraitDefs, _Visited, Acc) ->
    Acc;
collect_extends([Super | Rest], TraitDefs, Visited, Acc) ->
    case collect_supertraits(Super, TraitDefs, Visited, [Super | Acc]) of
        {error, cycle} -> {error, cycle};
        NewAcc -> collect_extends(Rest, TraitDefs, Visited, NewAcc)
    end.

%% @doc Perform topological sort of traits
%%
%% Returns traits in dependency order (traits with no dependencies first).
%% Fails with {error, cycle_detected} if cycles exist.
-spec topological_sort(trait_defs()) -> {ok, [atom()]} | {error, {cycle_detected, [atom()]}}.
topological_sort(TraitDefs) ->
    %% Build adjacency list (trait -> supertraits)
    Graph = maps:fold(
        fun(Name, {_N, Extends, _L}, Acc) ->
            maps:put(Name, Extends, Acc)
        end,
        #{},
        TraitDefs
    ),
    kahn_sort(Graph).

%% Kahn's algorithm for topological sort
%% We want base traits (no dependencies) first, then traits that extend them.
%% If B extends A, then A must come before B in the sorted list.
kahn_sort(Graph) ->
    %% Build reverse graph: for each node, track which traits extend it
    %% Original: B -> [A] means B extends A
    %% Reverse: A -> [B] means A is extended by B
    ReverseGraph = build_reverse_graph(Graph),
    %% Out-degrees: count how many supertraits each node has
    OutDegrees = maps:fold(
        fun(N, Edges, Acc) -> maps:put(N, length(Edges), Acc) end,
        #{},
        Graph
    ),
    %% Start with traits that have no supertraits (out-degree 0)
    Queue = [N || {N, D} <- maps:to_list(OutDegrees), D == 0],
    kahn_sort_loop(Queue, ReverseGraph, OutDegrees, []).

build_reverse_graph(Graph) ->
    %% Initialize all nodes
    Initial = maps:fold(fun(N, _, Acc) -> maps:put(N, [], Acc) end, #{}, Graph),
    %% Build reverse edges
    maps:fold(
        fun(Node, Edges, Acc) ->
            lists:foldl(
                fun(Edge, A) ->
                    %% Edge is a supertrait of Node, so add Node to Edge's list
                    maps:update_with(Edge, fun(L) -> [Node | L] end, [Node], A)
                end,
                Acc,
                Edges
            )
        end,
        Initial,
        Graph
    ).

kahn_sort_loop([], _ReverseGraph, OutDegrees, Sorted) ->
    %% Check if all nodes processed
    Remaining = [N || {N, D} <- maps:to_list(OutDegrees), D > 0],
    case Remaining of
        [] -> {ok, lists:reverse(Sorted)};
        _ -> {error, {cycle_detected, Remaining}}
    end;
kahn_sort_loop([Node | Queue], ReverseGraph, OutDegrees, Sorted) ->
    %% Get traits that extend this node
    Dependents = maps:get(Node, ReverseGraph, []),
    %% Decrement their out-degrees (they have one less unsatisfied dependency)
    {NewOutDegrees, NewQueue} = lists:foldl(
        fun(Dependent, {Degrees, Q}) ->
            NewDegree = maps:get(Dependent, Degrees, 1) - 1,
            NewDegrees = maps:put(Dependent, NewDegree, Degrees),
            NewQ = case NewDegree of
                0 -> [Dependent | Q];
                _ -> Q
            end,
            {NewDegrees, NewQ}
        end,
        {OutDegrees, Queue},
        Dependents
    ),
    kahn_sort_loop(NewQueue, ReverseGraph, NewOutDegrees, [Node | Sorted]).

%%====================================================================
%% Error Checking Functions
%%====================================================================

%% Check for self-extends (trait extends itself)
check_self_extends(TraitDefs) ->
    maps:fold(
        fun(Name, {_N, Extends, _L}, Acc) ->
            case lists:member(Name, Extends) of
                true -> [{self_extends, Name} | Acc];
                false -> Acc
            end
        end,
        [],
        TraitDefs
    ).

%% Check for references to unknown supertraits
check_unknown_supertraits(TraitDefs) ->
    KnownTraits = maps:keys(TraitDefs),
    %% Also include built-in traits that don't need to be defined
    BuiltinTraits = [
        'Eq', 'Ord', 'Show', 'Read',
        'Functor', 'Applicative', 'Monad',
        'Semigroup', 'Monoid',
        'Comparable', 'Combiner', 'Accumulator', 'Mapper', 'Workflow'
    ],
    AllKnown = KnownTraits ++ BuiltinTraits,
    maps:fold(
        fun(Name, {_N, Extends, _L}, Acc) ->
            lists:foldl(
                fun(Super, A) ->
                    case lists:member(Super, AllKnown) of
                        true -> A;
                        false -> [{unknown_supertrait, Name, Super} | A]
                    end
                end,
                Acc,
                Extends
            )
        end,
        [],
        TraitDefs
    ).

%% Find cycles in the trait graph using DFS
find_cycles(TraitDefs) ->
    TraitNames = maps:keys(TraitDefs),
    find_cycles_dfs(TraitNames, TraitDefs, sets:new(), sets:new(), []).

find_cycles_dfs([], _TraitDefs, _Visited, _RecStack, Cycles) ->
    Cycles;
find_cycles_dfs([Trait | Rest], TraitDefs, Visited, RecStack, Cycles) ->
    case sets:is_element(Trait, Visited) of
        true ->
            find_cycles_dfs(Rest, TraitDefs, Visited, RecStack, Cycles);
        false ->
            case dfs_visit(Trait, TraitDefs, Visited, RecStack, [Trait]) of
                {ok, NewVisited} ->
                    find_cycles_dfs(Rest, TraitDefs, NewVisited, RecStack, Cycles);
                {cycle, CyclePath} ->
                    NewCycles = [{cycle_detected, CyclePath} | Cycles],
                    NewVisited = sets:add_element(Trait, Visited),
                    find_cycles_dfs(Rest, TraitDefs, NewVisited, RecStack, NewCycles)
            end
    end.

dfs_visit(Trait, TraitDefs, Visited, RecStack, Path) ->
    NewRecStack = sets:add_element(Trait, RecStack),
    case maps:get(Trait, TraitDefs, undefined) of
        undefined ->
            %% Unknown trait (maybe builtin), mark as visited
            {ok, sets:add_element(Trait, Visited)};
        {_Name, Extends, _Loc} ->
            case visit_extends(Extends, TraitDefs, Visited, NewRecStack, Path) of
                {cycle, CyclePath} ->
                    {cycle, CyclePath};
                {ok, NewVisited} ->
                    {ok, sets:add_element(Trait, NewVisited)}
            end
    end.

visit_extends([], _TraitDefs, Visited, _RecStack, _Path) ->
    {ok, Visited};
visit_extends([Super | Rest], TraitDefs, Visited, RecStack, Path) ->
    case sets:is_element(Super, RecStack) of
        true ->
            %% Found a cycle
            CycleStart = lists:dropwhile(fun(X) -> X =/= Super end, lists:reverse(Path)),
            {cycle, lists:reverse([Super | CycleStart])};
        false ->
            case sets:is_element(Super, Visited) of
                true ->
                    visit_extends(Rest, TraitDefs, Visited, RecStack, Path);
                false ->
                    case dfs_visit(Super, TraitDefs, Visited, RecStack, [Super | Path]) of
                        {cycle, CyclePath} ->
                            {cycle, CyclePath};
                        {ok, NewVisited} ->
                            visit_extends(Rest, TraitDefs, NewVisited, RecStack, Path)
                    end
            end
    end.

%%====================================================================
%% Error Formatting
%%====================================================================

%% @doc Format a hierarchy error for display
-spec format_error(hierarchy_error()) -> string().
format_error({cycle_detected, Cycle}) ->
    CycleStr = string:join([atom_to_list(T) || T <- Cycle], " -> "),
    lists:flatten(io_lib:format(
        "Circular trait hierarchy detected: ~s~n"
        "Traits cannot directly or indirectly extend themselves.",
        [CycleStr]
    ));
format_error({unknown_supertrait, Trait, Super}) ->
    lists:flatten(io_lib:format(
        "Trait '~s' extends unknown trait '~s'~n"
        "The supertrait must be defined before it can be extended.",
        [atom_to_list(Trait), atom_to_list(Super)]
    ));
format_error({self_extends, Trait}) ->
    lists:flatten(io_lib:format(
        "Trait '~s' cannot extend itself~n"
        "A trait cannot be its own supertrait.",
        [atom_to_list(Trait)]
    )).
