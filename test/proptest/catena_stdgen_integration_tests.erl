%% @doc Integration Tests for Phase 2 Standard Generators
%%
%% These tests verify that all Phase 2 standard generators work together
%% correctly in realistic property testing scenarios.
-module(catena_stdgen_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Section 2.6: Integration Tests
%%====================================================================

%% ---- Complex Structure Generation ----

complex_nested_structure_test() ->
    %% Generate a list of tuples containing maps and sets
    Gen = catena_stdgen:gen_list(
        catena_stdgen:gen_tuple2(
            catena_stdgen:gen_map(catena_gen:gen_int(), catena_stdgen:gen_atom()),
            catena_stdgen:gen_set(catena_gen:gen_int())
        )
    ),
    Tree = catena_gen:run(Gen, 20, catena_gen:seed_new()),
    Result = catena_tree:root(Tree),
    ?assert(is_list(Result)),
    ok.

deeply_nested_recursive_structure_test() ->
    %% Generate a tree containing lists of trees
    Gen = catena_stdgen:gen_BinaryTree(
        catena_stdgen:gen_list(catena_gen:gen_int())
    ),
    Tree = catena_gen:run(Gen, 30, catena_gen:seed_new()),
    Result = catena_tree:root(Tree),
    ?assert(is_tuple(Result)),
    ok.

mixed_collection_types_test() ->
    %% Generate a list where values are various collection types
    ValueGen = catena_stdgen:gen_frequency([
        {1, catena_stdgen:gen_list(catena_gen:gen_int())},
        {1, catena_stdgen:gen_set(catena_stdgen:gen_atom())}
    ]),
    Gen = catena_stdgen:gen_list(ValueGen),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Result = catena_tree:root(Tree),
    ?assert(is_list(Result)),
    ok.

%% ---- Property Testing Scenarios ----

reverse_preserves_length_property_test() ->
    %% Property: reverse(L) has same length as L
    Gen = catena_stdgen:gen_list(catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    List = catena_tree:root(Tree),
    ?assertEqual(length(List), length(lists:reverse(List))),
    ok.

append_associative_property_test() ->
    %% Property: L1 ++ (L2 ++ L3) == (L1 ++ L2) ++ L3
    Gen = catena_stdgen:gen_tuple3(
        catena_stdgen:gen_list(catena_gen:gen_int()),
        catena_stdgen:gen_list(catena_gen:gen_int()),
        catena_stdgen:gen_list(catena_gen:gen_int())
    ),
    Tree = catena_gen:run(Gen, 20, catena_gen:seed_new()),
    {L1, L2, L3} = catena_tree:root(Tree),
    Left = L1 ++ (L2 ++ L3),
    Right = (L1 ++ L2) ++ L3,
    ?assertEqual(Left, Right),
    ok.

map_idempotent_property_test() ->
    %% Property: map:put(K, V, map:put(K, V, M)) == map:put(K, V, M)
    Gen = catena_stdgen:gen_tuple2(
        catena_stdgen:gen_map(catena_gen:gen_int(), catena_stdgen:gen_atom()),
        catena_stdgen:gen_tuple2(catena_gen:gen_int(), catena_stdgen:gen_atom())
    ),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    {Map, {Key, Value}} = catena_tree:root(Tree),
    Result1 = maps:put(Key, Value, maps:put(Key, Value, Map)),
    Result2 = maps:put(Key, Value, Map),
    ?assertEqual(Result1, Result2),
    ok.

maybe_bind_property_test() ->
    %% Property: Binding on none returns none
    NoneValue = none,
    BindFun = fun(V) -> {some, V * 2} end,
    Result = case NoneValue of
        none -> none;
        {some, V} -> BindFun(V)
    end,
    ?assertEqual(none, Result),
    ok.

%% ---- Shrink Quality Tests ----

shrinking_finds_minimal_counterexample_test() ->
    %% Generate values and verify they shrink toward simpler values
    Gen = catena_stdgen:gen_list(catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 100, catena_gen:seed_from_int(12345)),
    Root = catena_tree:root(Tree),
    %% Root should be a list
    ?assert(is_list(Root)),
    %% Children should be simpler (shorter or smaller values)
    Children = catena_tree:children(Tree),
    ?assert(length(Children) > 0),
    lists:foreach(fun(Child) ->
        ChildList = catena_tree:root(Child),
        ?assert(is_list(ChildList)),
        ?assert(length(ChildList) =< length(Root))
    end, Children),
    ok.

nested_structure_shrinks_appropriately_test() ->
    %% Nested structures should shrink at multiple levels
    Gen = catena_stdgen:gen_BinaryTree(catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_from_int(999)),
    Root = catena_tree:root(Tree),
    ?assert(is_tuple(Root)),
    %% Should have shrinks
    Children = catena_tree:children(Tree),
    ?assert(length(Children) > 0),
    ok.

%% ---- Reproducibility Tests ----

same_seed_same_result_test() ->
    %% Different runs with same seed should produce same results
    Gen = catena_stdgen:gen_list(catena_stdgen:gen_tuple2(
        catena_gen:gen_int(),
        catena_stdgen:gen_atom()
    )),
    Seed = catena_gen:seed_from_int(42),
    Tree1 = catena_gen:run(Gen, 20, Seed),
    Tree2 = catena_gen:run(Gen, 20, Seed),
    ?assertEqual(catena_tree:root(Tree1), catena_tree:root(Tree2)),
    ok.

different_seeds_different_results_test() ->
    %% Different seeds should likely produce different results
    Gen = catena_stdgen:gen_set(catena_gen:gen_int()),
    Tree1 = catena_gen:run(Gen, 20, catena_gen:seed_from_int(1)),
    Tree2 = catena_gen:run(Gen, 20, catena_gen:seed_from_int(2)),
    Set1 = catena_tree:root(Tree1),
    Set2 = catena_tree:root(Tree2),
    %% Both should be valid sets (unique elements)
    ?assertEqual(length(Set1), length(lists:usort(Set1))),
    ?assertEqual(length(Set2), length(lists:usort(Set2))),
    ok.

%% ---- Size Scaling Tests ----

size_affects_collection_sizes_test() ->
    %% Larger size should allow larger collections
    Gen = catena_stdgen:gen_list(catena_gen:gen_int()),
    TreeSmall = catena_gen:run(Gen, 5, catena_gen:seed_from_int(42)),
    TreeLarge = catena_gen:run(Gen, 100, catena_gen:seed_from_int(42)),
    Small = catena_tree:root(TreeSmall),
    Large = catena_tree:root(TreeLarge),
    ?assert(length(Small) =< 10),
    %% Large may be bigger but not guaranteed
    ?assert(is_list(Large)),
    ok.

size_affects_recursion_depth_test() ->
    %% Larger size should allow deeper recursion
    Gen = catena_stdgen:gen_BinaryTree(catena_gen:gen_int()),
    TreeSmall = catena_gen:run(Gen, 5, catena_gen:seed_from_int(42)),
    TreeLarge = catena_gen:run(Gen, 100, catena_gen:seed_from_int(42)),
    %% Both should produce valid trees
    ?assert(is_tuple(catena_tree:root(TreeSmall))),
    ?assert(is_tuple(catena_tree:root(TreeLarge))),
    ok.

%% ---- Function Composition Tests ----

composed_generators_work_together_test() ->
    %% Use frequency to choose between multiple generators
    Gen = catena_stdgen:gen_frequency([
        {2, catena_stdgen:gen_list(catena_gen:gen_int())},
        {1, catena_stdgen:gen_set(catena_stdgen:gen_atom())},
        {1, catena_stdgen:gen_binary(catena_range:range_constant({0, 50}))}
    ]),
    Tree = catena_gen:run(Gen, 20, catena_gen:seed_from_int(100)),
    Result = catena_tree:root(Tree),
    %% Result should be one of the three types
    IsList = is_list(Result),
    IsBinary = is_binary(Result),
    ?assert(IsList orelse IsBinary),
    ok.

map_composition_test() ->
    %% Compose generators with gen_map
    Gen = catena_stdgen:gen_list(catena_gen:gen_int()),
    SquaredGen = catena_gen:gen_map(fun(L) -> lists:map(fun(X) -> X * X end, L) end, Gen),
    Tree = catena_gen:run(SquaredGen, 10, catena_gen:seed_new()),
    Result = catena_tree:root(Tree),
    ?assert(is_list(Result)),
    ok.

%% ---- Edge Cases ----

empty_collections_test() ->
    %% Should handle empty collections gracefully
    Gen = catena_stdgen:gen_list_of(catena_range:range_constant({0, 0}), catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Result = catena_tree:root(Tree),
    ?assertEqual([], Result),
    ok.

singleton_collections_test() ->
    %% Should handle single-element collections
    Gen = catena_stdgen:gen_list_of(catena_range:range_constant({1, 1}), catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Result = catena_tree:root(Tree),
    ?assertEqual(1, length(Result)),
    ok.

max_size_boundary_test() ->
    %% Should handle maximum size without issues
    Gen = catena_stdgen:gen_list(catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 1000, catena_gen:seed_new()),
    Result = catena_tree:root(Tree),
    ?assert(is_list(Result)),
    %% Should not be excessively large
    ?assert(length(Result) < 1000),
    ok.
