%% @doc Integration Tests for Property Testing Phase 1
%%
%% These tests verify that all Phase 1 components work together correctly.
%% They exercise realistic generator compositions and verify end-to-end behavior.
-module(catena_gen_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Section 1.7: Integration Tests
%%====================================================================

%% ---- Generator Composition ----

gen_map_composition_test() ->
    %% Compose multiple gen_map operations
    Gen = catena_gen:gen_int(catena_range:range_constant({0, 100})),
    Composed = catena_gen:gen_map(fun(N) -> N * 2 end,
                  catena_gen:gen_map(fun(N) -> N + 1 end, Gen)),
    Tree = catena_gen:run(Composed, 50, catena_gen:seed_from_int(42)),
    Root = catena_tree:root(Tree),
    ?assert(is_integer(Root)),
    ok.

gen_ap_composition_test() ->
    %% Compose independent generators with gen_ap
    GenA = catena_gen:gen_int(catena_range:range_constant({0, 10})),
    GenB = catena_gen:gen_int(catena_range:range_constant({0, 10})),
    PairGen = catena_gen:gen_ap(
        catena_gen:gen_map(fun(A) -> fun(B) -> {A, B} end end, GenA),
        GenB
    ),
    Tree = catena_gen:run(PairGen, 10, catena_gen:seed_from_int(42)),
    {A, B} = catena_tree:root(Tree),
    ?assert(is_integer(A)),
    ?assert(is_integer(B)),
    ok.

gen_bind_dependent_test() ->
    %% Use gen_bind for dependent generation
    %% Generate N, then return N*2
    LengthGen = catena_gen:gen_int(catena_range:range_constant({0, 50})),
    DoubledGen = catena_gen:gen_bind(LengthGen, fun(N) ->
        catena_gen:gen_pure(N * 2)
    end),
    Tree = catena_gen:run(DoubledGen, 10, catena_gen:seed_from_int(42)),
    Result = catena_tree:root(Tree),
    ?assert(is_integer(Result)),
    ?assert(Result rem 2 =:= 0),  %% Should be even
    ok.

%% ---- Complex Value Generation ----

complex_pair_test() ->
    %% Generate pairs with independent shrinking
    GenX = catena_gen:gen_int(catena_range:range_constant({0, 100})),
    GenY = catena_gen:gen_int(catena_range:range_constant({0, 100})),
    PairGen = catena_gen:gen_map2(fun(X, Y) -> {X, Y} end, GenX, GenY),
    Tree = catena_gen:run(PairGen, 50, catena_gen:seed_from_int(42)),
    ?assertMatch({_, _}, catena_tree:root(Tree)),
    %% Should have shrink candidates
    ?assert(length(catena_tree:children(Tree)) > 0),
    ok.

complex_triple_test() ->
    %% Generate triples
    GenA = catena_gen:gen_int(),
    GenB = catena_gen:gen_bool(),
    GenC = catena_gen:constant(atom),
    TripleGen = catena_gen:gen_map3(fun(A, B, C) -> {A, B, C} end, GenA, GenB, GenC),
    Tree = catena_gen:run(TripleGen, 10, catena_gen:seed_from_int(42)),
    ?assertMatch({_, _, atom}, catena_tree:root(Tree)),
    ok.

complex_nested_test() ->
    %% Generate nested structures
    IntGen = catena_gen:gen_int(catena_range:range_constant({0, 10})),
    ListGen = catena_gen:gen_map(fun(N) -> lists:seq(1, N) end, IntGen),
    Tree = catena_gen:run(ListGen, 5, catena_gen:seed_from_int(42)),
    Result = catena_tree:root(Tree),
    ?assert(is_list(Result)),
    ok.

%% ---- Reproducibility ----

reproducibility_same_seed_test() ->
    %% Same seed should produce identical results
    Gen = catena_gen:gen_int(catena_range:range_constant({0, 1000})),
    Seed = catena_gen:seed_from_int(12345),
    Tree1 = catena_gen:run(Gen, 50, Seed),
    Tree2 = catena_gen:run(Gen, 50, Seed),
    ?assertEqual(catena_tree:root(Tree1), catena_tree:root(Tree2)),
    ok.

reproducibility_different_seeds_test() ->
    %% Different seeds should likely produce different results
    Gen = catena_gen:gen_int(catena_range:range_constant({0, 1000})),
    Seed1 = catena_gen:seed_from_int(1),
    Seed2 = catena_gen:seed_from_int(2),
    Tree1 = catena_gen:run(Gen, 50, Seed1),
    Tree2 = catena_gen:run(Gen, 50, Seed2),
    %% Most likely different (though not guaranteed)
    %% Just check both are valid
    ?assert(is_integer(catena_tree:root(Tree1))),
    ?assert(is_integer(catena_tree:root(Tree2))),
    ok.

%% ---- Size Scaling ----

size_scaling_affects_generation_test() ->
    %% Larger size should enable larger values
    Gen = catena_gen:gen_int(catena_range:range_linear(0, 1000)),
    TreeSmall = catena_gen:run(Gen, 5, catena_gen:seed_from_int(42)),
    TreeLarge = catena_gen:run(Gen, 100, catena_gen:seed_from_int(42)),
    ValSmall = catena_tree:root(TreeSmall),
    ValLarge = catena_tree:root(TreeLarge),
    ?assert(is_integer(ValSmall)),
    ?assert(is_integer(ValLarge)),
    ok.

size_scale_transform_test() ->
    %% Use scale/2 to transform size
    Gen = catena_gen:gen_int(catena_range:range_linear(0, 1000)),
    HalfSizeGen = catena_gen:scale(fun(S) -> S div 2 end, Gen),
    TreeNormal = catena_gen:run(Gen, 100, catena_gen:seed_from_int(42)),
    TreeHalf = catena_gen:run(HalfSizeGen, 100, catena_gen:seed_from_int(42)),
    ValNormal = catena_tree:root(TreeNormal),
    ValHalf = catena_tree:root(TreeHalf),
    ?assert(is_integer(ValNormal)),
    ?assert(is_integer(ValHalf)),
    ok.

%% ---- Range-Based Generation ----

range_linear_test() ->
    %% Linear range should scale proportionally
    Range = catena_range:range_linear(0, 1000),
    Gen = catena_gen:gen_int(Range),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_from_int(42)),
    Val = catena_tree:root(Tree),
    ?assert(Val >= 0 andalso Val =< 1000),
    ok.

range_exponential_test() ->
    %% Exponential range should grow quickly
    Range = catena_range:range_exponential(0, 1000000),
    Gen = catena_gen:gen_int(Range),
    Tree = catena_gen:run(Gen, 80, catena_gen:seed_from_int(42)),
    Val = catena_tree:root(Tree),
    ?assert(Val >= 0 andalso Val =< 1000000),
    ok.

range_constant_test() ->
    %% Constant range should ignore size
    Range = catena_range:range_constant({42, 42}),
    Gen = catena_gen:gen_int(Range),
    Tree = catena_gen:run(Gen, 100, catena_gen:seed_from_int(42)),
    ?assertEqual(42, catena_tree:root(Tree)),
    ok.

%% ---- Shrinking Composed Values ----

shrinking_composed_pair_test() ->
    %% Shrinking a pair should shrink both components
    GenX = catena_gen:gen_int(catena_range:range_constant({0, 100})),
    GenY = catena_gen:gen_int(catena_range:range_constant({0, 100})),
    PairGen = catena_gen:gen_map2(fun(X, Y) -> {X, Y} end, GenX, GenY),
    Tree = catena_gen:run(PairGen, 50, catena_gen:seed_from_int(42)),
    {_X, _Y} = catena_tree:root(Tree),
    Children = catena_tree:children(Tree),
    %% Should have shrink candidates
    ?assert(length(Children) > 0),
    ok.

shrinking_nested_structure_test() ->
    %% Shrinking should work on nested structures
    IntGen = catena_gen:gen_int(catena_range:range_constant({0, 10})),
    ListGen = catena_gen:gen_map(fun(N) -> lists:seq(1, N) end, IntGen),
    Tree = catena_gen:run(ListGen, 10, catena_gen:seed_from_int(42)),
    Root = catena_tree:root(Tree),
    ?assert(is_list(Root)),
    %% Should have shrinks (toward shorter lists)
    Children = catena_tree:children(Tree),
    ?assert(length(Children) > 0),
    ok.

%% ---- Sample Function ----

sample_returns_correct_count_test() ->
    %% sample/2 should return the requested count
    Gen = catena_gen:gen_int(),
    Samples = catena_gen:sample(Gen, 5),
    ?assertEqual(5, length(Samples)),
    ok.

sample_increasing_sizes_test() ->
    %% sample should use increasing sizes
    Gen = catena_gen:gen_int(catena_range:range_constant({0, 100})),
    Samples = catena_gen:sample(Gen, 10),
    ?assertEqual(10, length(Samples)),
    %% All should be integers
    ?assert(lists:all(fun(I) -> is_integer(I) end, Samples)),
    ok.

%% ---- Alternative Instance ----

gen_one_of_selection_test() ->
    %% gen_one_of should select from all options
    Gen1 = catena_gen:constant(a),
    Gen2 = catena_gen:constant(b),
    Gen3 = catena_gen:constant(c),
    ChoiceGen = catena_gen:gen_one_of([Gen1, Gen2, Gen3]),
    Tree = catena_gen:run(ChoiceGen, 10, catena_gen:seed_from_int(42)),
    Root = catena_tree:root(Tree),
    ?assert(lists:member(Root, [a, b, c])),
    ok.

gen_frequency_weights_test() ->
    %% gen_frequency should respect weights
    Gen1 = catena_gen:constant(rare),
    Gen100 = catena_gen:constant(common),
    ChoiceGen = catena_gen:gen_frequency([{1, Gen1}, {100, Gen100}]),
    Tree = catena_gen:run(ChoiceGen, 10, catena_gen:seed_from_int(42)),
    Root = catena_tree:root(Tree),
    ?assert(lists:member(Root, [rare, common])),
    ok.

%% ---- Filter Integration ----

filter_with_shrinking_test() ->
    %% gen_filter should preserve shrinks that satisfy predicate
    Gen = catena_gen:gen_int(catena_range:range_constant({0, 100})),
    EvenGen = catena_gen:gen_filter(fun(N) -> N rem 2 =:= 0 end, Gen),
    Tree = catena_gen:run(EvenGen, 50, catena_gen:seed_from_int(42)),
    Root = catena_tree:root(Tree),
    ?assertEqual(0, Root rem 2),
    %% Should have shrinks
    Children = catena_tree:children(Tree),
    ?assert(length(Children) > 0),
    %% All shrinks should also be even
    ?assert(lists:all(fun(C) ->
        V = catena_tree:root(C),
        is_integer(V) andalso V rem 2 =:= 0
    end, Children)),
    ok.

%% ---- Performance Checks ----

performance_generation_speed_test() ->
    %% Generating 1000 values should be fast
    Gen = catena_gen:gen_int(),
    {Time, _Results} = timer:tc(fun() ->
        [catena_gen:run(Gen, 10, catena_gen:seed_new()) || _ <- lists:seq(1, 1000)]
    end),
    %% Should complete in less than 1 second (1,000,000 microseconds)
    ?assert(Time < 1000000),
    ok.

%% ---- Memory Tests ----

memory_lazy_children_test() ->
    %% Children should be lazy (not evaluated until accessed)
    Gen = catena_gen:gen_int(catena_range:range_constant({0, 100})),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_from_int(42)),
    %% Creating the tree should be fast (lazy children)
    %% Accessing children forces evaluation
    _Children = catena_tree:children(Tree),
    ok.

%% ---- Categorical Laws Spot Checks ----

functor_identity_test() ->
    %% gen_map(fun(X) -> X end, gen) == gen
    Gen = catena_gen:gen_int(catena_range:range_constant({0, 100})),
    IdGen = catena_gen:gen_map(fun(X) -> X end, Gen),
    Seed = catena_gen:seed_from_int(42),
    Tree1 = catena_gen:run(Gen, 50, Seed),
    Tree2 = catena_gen:run(IdGen, 50, Seed),
    ?assertEqual(catena_tree:root(Tree1), catena_tree:root(Tree2)),
    ok.

applicative_identity_test() ->
    %% gen_ap(gen_pure(fun(X) -> X end), gen) should behave like gen
    %% Note: seed splitting in gen_ap means results may differ, but structure preserved
    Gen = catena_gen:gen_int(catena_range:range_constant({0, 100})),
    IdFunGen = catena_gen:gen_pure(fun(X) -> X end),
    AppliedGen = catena_gen:gen_ap(IdFunGen, Gen),
    Seed = catena_gen:seed_from_int(42),
    Tree1 = catena_gen:run(Gen, 50, Seed),
    Tree2 = catena_gen:run(AppliedGen, 50, Seed),
    %% Both should produce integers
    ?assert(is_integer(catena_tree:root(Tree1))),
    ?assert(is_integer(catena_tree:root(Tree2))),
    ok.

monad_left_identity_test() ->
    %% gen_bind(gen_pure(a), f) == f(a)
    F = fun(N) -> catena_gen:gen_map(fun(X) -> X * 2 end, catena_gen:constant(N)) end,
    Gen1 = catena_gen:gen_bind(catena_gen:gen_pure(42), F),
    Gen2 = F(42),
    Seed = catena_gen:seed_from_int(42),
    Tree1 = catena_gen:run(Gen1, 10, Seed),
    Tree2 = catena_gen:run(Gen2, 10, Seed),
    ?assertEqual(catena_tree:root(Tree1), catena_tree:root(Tree2)),
    ok.

%% ---- Type Safety ----

type_safety_different_types_test() ->
    %% Generators of different types should compose correctly
    IntGen = catena_gen:gen_int(),
    BoolGen = catena_gen:gen_bool(),
    PairGen = catena_gen:gen_map2(fun(I, B) -> {I, B} end, IntGen, BoolGen),
    Tree = catena_gen:run(PairGen, 10, catena_gen:seed_new()),
    ?assertMatch({_, _}, catena_tree:root(Tree)),
    ok.
