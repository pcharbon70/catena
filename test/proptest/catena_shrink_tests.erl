%% @doc Unit tests for catena_shrink (Section 1.6)
%%
%% Tests shrink strategies and tree traversal for the property testing
%% framework.
-module(catena_shrink_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Data Generation Helpers
%%====================================================================

%% @doc Build a simple integer tree that shrinks toward a target.
build_int_tree(Value, Target) ->
    catena_tree:tree(Value, fun() ->
        [build_int_tree(Candidate, Target) || Candidate <- catena_shrink:shrink_towards(Value, Target)]
    end).

%% @doc Build a test tree from a list of shrink candidates.
build_tree_from_shrinks(Value, ShrinkFn) ->
    catena_tree:tree(Value, fun() ->
        [build_tree_from_shrinks(Candidate, ShrinkFn) || Candidate <- ShrinkFn(Value)]
    end).

%%====================================================================
%% Section 1.6.1: Shrink Strategies Tests
%%====================================================================

%% ---- shrink_towards/2 ----

shrink_towards_toward_zero_test() ->
    %% Shrink positive value toward zero
    %% Should include 0 (target) and intermediate values
    Result = catena_shrink:shrink_towards(100, 0),
    ?assert(lists:member(0, Result)),
    ?assert(lists:member(50, Result)),
    %% Should have fewer than 20 candidates (not linear)
    ?assert(length(Result) < 20),
    ok.

shrink_towards_negative_toward_zero_test() ->
    %% Shrink negative value toward zero
    Result = catena_shrink:shrink_towards(-100, 0),
    ?assert(lists:member(0, Result)),
    ?assert(lists:member(-50, Result)),
    ok.

shrink_towards_at_target_test() ->
    %% Already at target returns empty list
    [] = catena_shrink:shrink_towards(42, 42),
    [] = catena_shrink:shrink_towards(0, 0),
    ok.

shrink_towards_small_values_test() ->
    %% Small positive value toward zero - should include 0
    Result1 = catena_shrink:shrink_towards(1, 0),
    ?assert(lists:member(0, Result1)),
    %% Small negative value toward zero - should include 0
    Result2 = catena_shrink:shrink_towards(-1, 0),
    ?assert(lists:member(0, Result2)),
    ok.

shrink_towards_custom_target_test() ->
    %% Shrink toward non-zero target
    [_ | _] = catena_shrink:shrink_towards(100, 50),
    ok.

%% ---- shrink_binary/2 ----

shrink_binary_basic_test() ->
    %% Binary shrink from 1000 toward 0
    ShrinkResult = catena_shrink:shrink_binary(1000, 0),
    %% Should include 0 (origin)
    ?assert(lists:member(0, ShrinkResult)),
    %% Should include midpoint
    ?assert(lists:member(500, ShrinkResult)),
    %% Should include step toward origin
    ?assert(lists:member(999, ShrinkResult)),
    ok.

shrink_binary_at_origin_test() ->
    [] = catena_shrink:shrink_binary(42, 42),
    [] = catena_shrink:shrink_binary(0, 0),
    ok.

shrink_binary_negative_test() ->
    %% Binary shrink from negative toward 0
    ShrinkResult = catena_shrink:shrink_binary(-100, 0),
    ?assert(lists:member(0, ShrinkResult)),
    ?assert(lists:member(-50, ShrinkResult)),
    ok.

%% ---- shrink_list/1 ----

shrink_list_empty_test() ->
    [] = catena_shrink:shrink_list([]),
    ok.

shrink_list_single_element_test() ->
    %% Single element - removing it results in empty list
    [[]] = catena_shrink:shrink_list([1]),
    ok.

shrink_list_multiple_elements_test() ->
    %% Three elements
    Shrinks = catena_shrink:shrink_list([1, 2, 3]),
    %% Should have 3 shrinks (one for each removal position)
    ?assertEqual(3, length(Shrinks)),
    %% Each shrink should have 2 elements
    ?assert(lists:all(fun(S) -> length(S) =:= 2 end, Shrinks)),
    %% Check specific removals
    ?assert(lists:member([2, 3], Shrinks)),  %% remove first
    ?assert(lists:member([1, 3], Shrinks)),  %% remove middle
    ?assert(lists:member([1, 2], Shrinks)),  %% remove last
    ok.

shrink_list_preserves_order_test() ->
    %% List shrinking should preserve order of remaining elements
    Shrinks = catena_shrink:shrink_list([a, b, c]),
    ?assert(lists:member([b, c], Shrinks)),
    ?assert(lists:member([a, c], Shrinks)),
    ?assert(lists:member([a, b], Shrinks)),
    ok.

%% ---- shrink_halves/1 ----

shrink_halves_positive_test() ->
    %% Halves sequence for 100
    [50, 25, 12, 6, 3, 1, 0] = catena_shrink:shrink_halves(100),
    ok.

shrink_halves_negative_test() ->
    %% Halves sequence for -100
    Result = catena_shrink:shrink_halves(-100),
    [First | _] = Result,
    ?assertEqual(-50, First),
    %% Should end at 0
    ?assert(lists:member(0, Result)),
    ok.

shrink_halves_zero_test() ->
    [] = catena_shrink:shrink_halves(0),
    ok.

shrink_halves_small_positive_test() ->
    %% Small values that reach 0 quickly
    [1, 0] = catena_shrink:shrink_halves(2),
    [0] = catena_shrink:shrink_halves(1),
    ok.

shrink_halves_power_of_two_test() ->
    %% Power of 2 should halve cleanly
    [64, 32, 16, 8, 4, 2, 1, 0] = catena_shrink:shrink_halves(128),
    ok.

%%====================================================================
%% Section 1.6.2: Shrink Tree Traversal Tests
%%====================================================================

%% ---- find_minimal/2 ----

find_minimal_root_fails_test() ->
    %% Root fails (predicate returns true), can shrink to minimal
    Tree = build_int_tree(100, 0),
    Pred = fun(N) -> N >= 50 end,  %% Values >= 50 "fail" the property
    {found, Min, _Path} = catena_shrink:find_minimal(Pred, Tree),
    %% Should find the minimal failing value
    ?assert(Min >= 50),
    ok.

find_minimal_root_passes_test() ->
    %% Root passes predicate (returns false, no failure)
    Tree = build_int_tree(10, 0),
    Pred = fun(N) -> N > 100 end,
    {no_shrink, 10} = catena_shrink:find_minimal(Pred, Tree),
    ok.

find_minimal_no_failing_shrinks_test() ->
    %% All shrinks pass predicate
    Tree = build_int_tree(5, 0),
    Pred = fun(N) -> N > 100 end,
    {no_shrink, _} = catena_shrink:find_minimal(Pred, Tree),
    ok.

find_minimal_exact_match_test() ->
    %% Find exactly 0 as the minimal value when all values "fail"
    Tree = build_int_tree(100, 0),
    Pred = fun(_N) -> true end,  %% All values "fail" - should find minimal
    {found, Min, _Path} = catena_shrink:find_minimal(Pred, Tree),
    %% Should find 0 as the minimal value
    ?assertEqual(0, Min),
    ok.

%% ---- find_minimal/3 with options ----

find_minimal_with_max_attempts_test() ->
    Tree = build_int_tree(1000, 0),
    Pred = fun(_N) -> true end,
    %% Limit attempts
    {found, _Min, _Path} = catena_shrink:find_minimal(Pred, Tree, #{max_attempts => 10}),
    ok.

find_minimal_without_path_tracking_test() ->
    Tree = build_int_tree(100, 0),
    Pred = fun(N) -> N >= 50 end,  %% Values >= 50 "fail"
    {found, _Min, Path} = catena_shrink:find_minimal(Pred, Tree, #{track_path => false}),
    %% Path should be empty when not tracking
    ?assertEqual([], Path),
    ok.

%% ---- shrink_path/1 ----

shrink_path_leaf_test() ->
    %% Leaf tree has only root value
    Tree = catena_tree:singleton(42),
    [42] = catena_shrink:shrink_path(Tree),
    ok.

shrink_path_with_children_test() ->
    %% Tree with children
    Tree = catena_tree:tree(10, fun() ->
        [catena_tree:singleton(5), catena_tree:singleton(0)]
    end),
    Path = catena_shrink:shrink_path(Tree),
    %% Should start with root
    ?assertEqual(10, hd(Path)),
    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

shrink_strategies_termination_test() ->
    %% All shrink strategies should terminate (not loop forever)
    %% shrink_towards
    [] = catena_shrink:shrink_towards(0, 0),
    %% shrink_binary
    [] = catena_shrink:shrink_binary(0, 0),
    %% shrink_list
    [] = catena_shrink:shrink_list([]),
    %% shrink_halves
    [] = catena_shrink:shrink_halves(0),
    ok.

shrink_strategies_no_duplicates_test() ->
    %% shrink_towards should not have duplicates
    L1 = catena_shrink:shrink_towards(100, 0),
    ?assertEqual(length(L1), length(lists:usort(L1))),
    %% shrink_binary should not have duplicates
    L2 = catena_shrink:shrink_binary(100, 0),
    ?assertEqual(length(L2), length(lists:usort(L2))),
    ok.
