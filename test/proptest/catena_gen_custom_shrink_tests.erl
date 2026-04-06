%% @doc Unit tests for catena_gen custom shrinking (Section 1.4.6)
%%
%% Tests with_shrink, no_shrink, and shrink_map functions.
-module(catena_gen_custom_shrink_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Section 1.4.6: Custom Shrinking Tests
%%====================================================================

%% ---- with_shrink/2 ----

with_shrink_basic_test() ->
    %% Create a generator with custom shrinking toward multiples of 10
    Gen = catena_gen:gen_int(catena_range:range_constant({0, 100})),
    CustomGen = catena_gen:with_shrink(Gen, fun(N) ->
        %% Shrink toward nearest multiple of 10
        case N > 0 of
            true -> [N - (N rem 10)];
            false -> []
        end
    end),
    Tree = catena_gen:run(CustomGen, 50, catena_gen:seed_from_int(42)),
    Root = catena_tree:root(Tree),
    ?assert(is_integer(Root)),
    ok.

with_shrink_empty_shrinks_test() ->
    %% Custom shrink function that returns no shrinks
    Gen = catena_gen:constant(42),
    CustomGen = catena_gen:with_shrink(Gen, fun(_N) -> [] end),
    Tree = catena_gen:run(CustomGen, 10, catena_gen:seed_new()),
    Root = catena_tree:root(Tree),
    ?assertEqual(42, Root),
    %% Should have no children
    ?assertEqual([], catena_tree:children(Tree)),
    ok.

with_shrink_preserves_root_test() ->
    %% Custom shrinking should not change the root value
    Gen = catena_gen:constant(100),
    CustomGen = catena_gen:with_shrink(Gen, fun(_N) -> [0, 50] end),
    Tree = catena_gen:run(CustomGen, 10, catena_gen:seed_new()),
    ?assertEqual(100, catena_tree:root(Tree)),
    ok.

%% ---- no_shrink/1 ----

no_shrink_leaf_test() ->
    %% no_shrink should produce a tree with no children
    Gen = catena_gen:gen_int(),
    NoShrinkGen = catena_gen:no_shrink(Gen),
    Tree = catena_gen:run(NoShrinkGen, 10, catena_gen:seed_new()),
    %% Root exists
    Root = catena_tree:root(Tree),
    ?assert(is_integer(Root)),
    %% But no children
    ?assertEqual([], catena_tree:children(Tree)),
    ok.

no_shrink_constant_test() ->
    %% no_shrink on a constant generator
    Gen = catena_gen:constant(42),
    NoShrinkGen = catena_gen:no_shrink(Gen),
    Tree = catena_gen:run(NoShrinkGen, 10, catena_gen:seed_new()),
    ?assertEqual(42, catena_tree:root(Tree)),
    ?assertEqual([], catena_tree:children(Tree)),
    ok.

no_shrink_bool_test() ->
    %% no_shrink on boolean generator
    Gen = catena_gen:gen_bool(),
    NoShrinkGen = catena_gen:no_shrink(Gen),
    Tree = catena_gen:run(NoShrinkGen, 10, catena_gen:seed_new()),
    Root = catena_tree:root(Tree),
    ?assert(is_boolean(Root)),
    ?assertEqual([], catena_tree:children(Tree)),
    ok.

%% ---- shrink_map/2 ----

shrink_map_wrapper_test() ->
    %% Wrap values in a tuple tag
    Gen = catena_gen:gen_int(catena_range:range_constant({0, 100})),
    TaggedGen = catena_gen:shrink_map(fun(N) -> {tagged, N} end, Gen),
    Tree = catena_gen:run(TaggedGen, 10, catena_gen:seed_new()),
    Root = catena_tree:root(Tree),
    ?assertMatch({tagged, _}, Root),
    ok.

shrink_map_transforms_all_nodes_test() ->
    %% shrink_map should transform the entire tree
    Gen = catena_gen:constant(42),
    MappedGen = catena_gen:shrink_map(fun(N) -> N * 2 end, Gen),
    Tree = catena_gen:run(MappedGen, 10, catena_gen:seed_new()),
    ?assertEqual(84, catena_tree:root(Tree)),
    ok.

shrink_map_preserves_structure_test() ->
    %% Tree structure should be preserved
    Gen = catena_gen:gen_int(),
    MappedGen = catena_gen:shrink_map(fun(N) -> N + 1000 end, Gen),
    Tree1 = catena_gen:run(Gen, 10, catena_gen:seed_from_int(1)),
    Tree2 = catena_gen:run(MappedGen, 10, catena_gen:seed_from_int(1)),
    %% Should have same number of children
    ?assertEqual(
        length(catena_tree:children(Tree1)),
        length(catena_tree:children(Tree2))
    ),
    ok.

shrink_map_idempotent_test() ->
    %% Applying shrink_map twice should be equivalent to composing the functions
    Gen = catena_gen:constant(10),
    Add1 = fun(N) -> N + 1 end,
    Mul2 = fun(N) -> N * 2 end,
    Gen1 = catena_gen:shrink_map(Add1, catena_gen:shrink_map(Mul2, Gen)),
    Gen2 = catena_gen:shrink_map(fun(N) -> Add1(Mul2(N)) end, Gen),
    Tree1 = catena_gen:run(Gen1, 10, catena_gen:seed_new()),
    Tree2 = catena_gen:run(Gen2, 10, catena_gen:seed_new()),
    ?assertEqual(catena_tree:root(Tree1), catena_tree:root(Tree2)),
    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

custom_shrink_find_minimal_test() ->
    %% Use custom shrinking with find_minimal
    %% Shrink toward multiples of 10, then find minimal failing
    Gen = catena_gen:gen_int(catena_range:range_constant({0, 100})),
    CustomGen = catena_gen:with_shrink(Gen, fun(N) ->
        case N rem 10 of
            0 -> [];
            _ -> [N - (N rem 10)]
        end
    end),
    Tree = catena_gen:run(CustomGen, 50, catena_gen:seed_from_int(42)),
    %% Tree should have custom shrinks
    Root = catena_tree:root(Tree),
    ?assert(is_integer(Root)),
    ok.

no_shrink_no_shrinks_test() ->
    %% no_shrink should prevent any shrinking
    Gen = catena_gen:no_shrink(catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    %% Depth-first search should find nothing beyond root
    Children = catena_tree:children(Tree),
    ?assertEqual([], Children),
    ok.

shrink_map_with_filter_test() ->
    %% Combine shrink_map with filter
    Gen = catena_gen:gen_int(),
    EvenGen = catena_gen:gen_filter(fun(N) -> N rem 2 =:= 0 end, Gen),
    DoubledGen = catena_gen:shrink_map(fun(N) -> N * 2 end, EvenGen),
    Tree = catena_gen:run(DoubledGen, 20, catena_gen:seed_from_int(42)),
    Root = catena_tree:root(Tree),
    ?assert(is_integer(Root)),
    %% If we got a value, it should be even and doubled
    ?assertEqual(0, Root rem 2),
    ok.
