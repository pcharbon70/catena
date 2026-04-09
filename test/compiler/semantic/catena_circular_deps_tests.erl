%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_circular_deps
%%% @end
%%%-------------------------------------------------------------------
-module(catena_circular_deps_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Data
%%%=============================================================================

%% Simple linear dependency graph: A -> B -> C
linear_graph() ->
    #{
        module_a => [module_b],
        module_b => [module_c],
        module_c => []
    }.

%% Graph with a cycle: A -> B -> C -> A
cyclic_graph() ->
    #{
        module_a => [module_b],
        module_b => [module_c],
        module_c => [module_a]
    }.

%% Graph with self-cycle: A -> A
self_cycle_graph() ->
    #{
        module_a => [module_a]
    }.

%% Complex graph with multiple cycles
multi_cycle_graph() ->
    #{
        module_a => [module_b],
        module_b => [module_c],
        module_c => [module_a, module_d],
        module_d => [module_b]
    }.

%% Diamond dependency (no cycle): A -> B, A -> C, B -> D, C -> D
diamond_graph() ->
    #{
        module_a => [module_b, module_c],
        module_b => [module_d],
        module_c => [module_d],
        module_d => []
    }.

%% Empty graph
empty_graph() ->
    #{}.

%%%=============================================================================
%%% build_dependency_graph/1 Tests
%%%=============================================================================

build_dependency_graph_empty_test() ->
    SymbolTables = #{},
    Graph = catena_circular_deps:build_dependency_graph(SymbolTables),
    ?assertEqual(0, map_size(Graph)).

build_dependency_graph_no_imports_test() ->
    SymbolTables = #{
        module_a => #{
            exports => [],
            imports => [],
            definitions => #{}
        }
    },
    Graph = catena_circular_deps:build_dependency_graph(SymbolTables),
    ?assertEqual(#{module_a => []}, Graph).

build_dependency_graph_with_imports_test() ->
    SymbolTables = #{
        module_a => #{
            exports => [],
            imports => [
                {import, module_b, all, false, undefined, {1, 0}},
                {import, module_c, all, false, undefined, {2, 0}}
            ],
            definitions => #{}
        },
        module_b => #{
            exports => [],
            imports => [],
            definitions => #{}
        },
        module_c => #{
            exports => [],
            imports => [],
            definitions => #{}
        }
    },
    Graph = catena_circular_deps:build_dependency_graph(SymbolTables),
    ?assertEqual([module_b, module_c], maps:get(module_a, Graph)),
    ?assertEqual([], maps:get(module_b, Graph)),
    ?assertEqual([], maps:get(module_c, Graph)).

%%%=============================================================================
%%% detect_cycles/1 Tests
%%%=============================================================================

detect_cycles_no_cycle_test() ->
    Graph = linear_graph(),
    Cycles = catena_circular_deps:detect_cycles(Graph),
    ?assertEqual([], Cycles).

detect_cycles_simple_cycle_test() ->
    Graph = cyclic_graph(),
    Cycles = catena_circular_deps:detect_cycles(Graph),
    ?assertEqual(1, length(Cycles)),
    [Cycle | _] = Cycles,
    %% Check that the cycle contains all three modules
    ?assert(lists:member(module_a, Cycle)),
    ?assert(lists:member(module_b, Cycle)),
    ?assert(lists:member(module_c, Cycle)).

detect_cycles_self_cycle_test() ->
    Graph = self_cycle_graph(),
    Cycles = catena_circular_deps:detect_cycles(Graph),
    ?assertEqual(1, length(Cycles)),
    [Cycle | _] = Cycles,
    %% Self-cycle is detected as a single-element list
    ?assertEqual([module_a], Cycle).

detect_cycles_diamond_no_cycle_test() ->
    Graph = diamond_graph(),
    Cycles = catena_circular_deps:detect_cycles(Graph),
    ?assertEqual([], Cycles).

detect_cycles_empty_graph_test() ->
    Graph = empty_graph(),
    Cycles = catena_circular_deps:detect_cycles(Graph),
    ?assertEqual([], Cycles).

detect_cycles_multi_cycle_test() ->
    Graph = multi_cycle_graph(),
    Cycles = catena_circular_deps:detect_cycles(Graph),
    ?assert(length(Cycles) >= 1).

%%%=============================================================================
%%% topological_sort/1 Tests
%%%=============================================================================

topological_sort_linear_test() ->
    Graph = linear_graph(),
    Result = catena_circular_deps:topological_sort(Graph),
    ?assertMatch({ok, _}, Result),
    {ok, Sorted} = Result,
    %% Check that dependencies come before dependents
    PosA = index_of(module_a, Sorted),
    PosB = index_of(module_b, Sorted),
    PosC = index_of(module_c, Sorted),
    ?assert(PosB > PosC),  %% B depends on C
    ?assert(PosA > PosB).  %% A depends on B

topological_sort_diamond_test() ->
    Graph = diamond_graph(),
    Result = catena_circular_deps:topological_sort(Graph),
    ?assertMatch({ok, _}, Result),
    {ok, Sorted} = Result,
    %% Check that D comes before B and C
    PosD = index_of(module_d, Sorted),
    PosB = index_of(module_b, Sorted),
    PosC = index_of(module_c, Sorted),
    ?assert(PosB > PosD),
    ?assert(PosC > PosD).

topological_sort_cycle_error_test() ->
    Graph = cyclic_graph(),
    Result = catena_circular_deps:topological_sort(Graph),
    ?assertMatch({error, {cycle, _}}, Result),
    {error, {cycle, Cycle}} = Result,
    ?assert(length(Cycle) > 0).

topological_sort_empty_test() ->
    Graph = empty_graph(),
    Result = catena_circular_deps:topological_sort(Graph),
    ?assertMatch({ok, []}, Result).

%%%=============================================================================
%%% format_cycle_error/1 Tests
%%%=============================================================================

format_cycle_error_test() ->
    Cycle = {cycle, [module_a, module_b, module_c, module_a]},
    Result = catena_circular_deps:format_cycle_error(Cycle),
    ?assert(is_list(Result)),
    ResultString = lists:flatten(Result),
    ?assert(string:str(ResultString, "Circular dependency") > 0).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

index_of(Element, List) ->
    index_of(Element, List, 1).

index_of(Element, [Element | _], Index) ->
    Index;
index_of(Element, [_ | Rest], Index) ->
    index_of(Element, Rest, Index + 1);
index_of(_, _, _) ->
    not_found.
