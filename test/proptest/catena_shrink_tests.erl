%% @doc Unit Tests for shrink strategies and shrink-tree traversal.
-module(catena_shrink_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test: Shrink strategies
%%====================================================================

shrink_towards_produces_linear_sequence_test() ->
    ?assertEqual([0, 1, 2, 3, 4], catena_shrink:shrink_towards(0, 5)),
    ?assertEqual([0, -1, -2, -3, -4], catena_shrink:shrink_towards(0, -5)).

shrink_binary_finds_values_efficiently_test() ->
    Linear = catena_shrink:shrink_towards(0, 100),
    Binary = catena_shrink:shrink_binary(0, 100),

    ?assertEqual([0, 50, 75, 88, 94, 97, 99], Binary),
    ?assert(length(Binary) < length(Linear)).

shrink_halves_repeatedly_halves_toward_zero_test() ->
    ?assertEqual([10, 5, 2, 1, 0], catena_shrink:shrink_halves(20)),
    ?assertEqual([-10, -5, -2, -1, 0], catena_shrink:shrink_halves(-20)).

shrink_list_removes_and_shrinks_elements_test() ->
    Shrinks = catena_shrink:shrink_list([10, 20]),

    ?assert(lists:member([], Shrinks)),
    ?assert(lists:member([20], Shrinks)),
    ?assert(lists:member([10], Shrinks)),
    ?assert(lists:member([0, 20], Shrinks)),
    ?assert(lists:member([10, 0], Shrinks)).

%%====================================================================
%% Test: Shrink tree traversal
%%====================================================================

find_minimal_finds_smallest_failing_value_test() ->
    Tree = integer_shrink_tree(20),

    {ok, Result} = catena_shrink:find_minimal(Tree, fun(Value) -> Value > 3 end),

    ?assertEqual(4, maps:get(value, Result)),
    ?assertEqual([2, 2, 3], maps:get(path, Result)),
    ?assertEqual([10, 5, 4], maps:get(trail, Result)),
    ?assertEqual(false, maps:get(limit_hit, Result)).

find_minimal_respects_shrink_limits_test() ->
    Tree = integer_shrink_tree(20),

    {ok, Result} =
        catena_shrink:find_minimal(
            Tree,
            fun(Value) -> Value > 3 end,
            #{max_attempts => 2}
        ),

    ?assertEqual(10, maps:get(value, Result)),
    ?assertEqual([2], maps:get(path, Result)),
    ?assertEqual([10], maps:get(trail, Result)),
    ?assertEqual(2, maps:get(attempts, Result)),
    ?assertEqual(true, maps:get(limit_hit, Result)).

find_minimal_requires_a_failing_root_test() ->
    Tree = integer_shrink_tree(3),

    ?assertEqual(
        {error, root_passed},
        catena_shrink:find_minimal(Tree, fun(Value) -> Value > 3 end)
    ).

%%====================================================================
%% Helpers
%%====================================================================

integer_shrink_tree(Value) ->
    catena_tree:unfold(Value, fun(N) ->
        {N, catena_shrink:shrink_binary(0, N)}
    end).
