-module(catena_state_copy_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_state_copy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"deep copy primitives", fun test_deep_copy_primitives/0},
        {"deep copy nested structures", fun test_deep_copy_nested/0},
        {"deep copy with depth limit", fun test_deep_copy_depth_limit/0},
        {"shallow copy", fun test_shallow_copy/0},
        {"selective copy", fun test_selective_copy/0},
        {"selective copy with exclude", fun test_selective_copy_exclude/0},
        {"selective copy with include only", fun test_selective_copy_include_only/0},
        {"strategy specification", fun test_strategy_specification/0},
        {"validation", fun test_validation/0},
        {"copy value utility", fun test_copy_value/0},
        {"copy value with strategy spec", fun test_copy_value_with_strategy_spec/0},
        {"merge results", fun test_merge/0},
        {"optimize strategy", fun test_optimize_strategy/0},
        {"should share", fun test_should_share/0},
        {"algebraic laws", fun test_algebraic_laws/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Deep Copy Primitives Tests
%%%---------------------------------------------------------------------

test_deep_copy_primitives() ->
    %% Primitives are shared or copied based on share_immutables
    ?assertEqual(42, catena_state_copy:deep_copy(42)),
    ?assertEqual(atom, catena_state_copy:deep_copy(atom)),
    ?assertEqual(<<"binary">>, catena_state_copy:deep_copy(<<"binary">>)),
    ?assertEqual([1, 2, 3], catena_state_copy:deep_copy([1, 2, 3])).

%%%---------------------------------------------------------------------
%%% Deep Copy Nested Structures Tests
%%%---------------------------------------------------------------------

test_deep_copy_nested() ->
    NestedMap = #{a => #{b => #{c => 1}}},
    Copied = catena_state_copy:deep_copy(NestedMap),
    ?assertEqual(NestedMap, Copied),

    NestedList = [[1, 2], [3, 4]],
    CopiedList = catena_state_copy:deep_copy(NestedList),
    ?assertEqual(NestedList, CopiedList),

    NestedTuple = {{1, 2}, {3, 4}},
    CopiedTuple = catena_state_copy:deep_copy(NestedTuple),
    ?assertEqual(NestedTuple, CopiedTuple).

%%%---------------------------------------------------------------------
%%% Deep Copy Depth Limit Tests
%%%---------------------------------------------------------------------

test_deep_copy_depth_limit() ->
    Nested = #{a => #{b => #{c => #{d => 1}}}},
    Options = #{max_depth => 2},
    Copied = catena_state_copy:deep_copy(Nested, Options),
    ?assert(is_map(Copied)),
    ?assert(is_map(maps:get(a, Copied))),
    ?assert(is_map(maps:get(b, maps:get(a, Copied)))).

%%%---------------------------------------------------------------------
%%% Shallow Copy Tests
%%%---------------------------------------------------------------------

test_shallow_copy() ->
    Original = #{a => 1, b => #{nested => 2}},
    Shallow = catena_state_copy:shallow_copy(Original),
    ?assertEqual(Original, Shallow),

    List = [1, 2, [3, 4]],
    ShallowList = catena_state_copy:shallow_copy(List),
    ?assertEqual(List, ShallowList).

%%%---------------------------------------------------------------------
%%% Selective Copy Tests
%%%---------------------------------------------------------------------

test_selective_copy() ->
    Map = #{a => 1, b => 2, c => #{nested => 3}},
    Copied = catena_state_copy:selective_copy(Map),
    ?assertEqual(Map, Copied).

%%%---------------------------------------------------------------------
%%% Selective Copy Exclude Tests
%%%---------------------------------------------------------------------

test_selective_copy_exclude() ->
    Map = #{a => 1, b => #{nested => 2}, c => 3},
    Spec = #{exclude => [b]},
    Copied = catena_state_copy:selective_copy(Map, Spec),
    ?assertEqual(1, maps:get(a, Copied)),
    ?assertEqual(#{nested => 2}, maps:get(b, Copied)),
    ?assertEqual(3, maps:get(c, Copied)).

%%%---------------------------------------------------------------------
%%% Selective Copy Include Only Tests
%%%---------------------------------------------------------------------

test_selective_copy_include_only() ->
    Map = #{a => 1, b => #{nested => 2}, c => 3},
    Spec = #{include_only => [a, c]},
    Copied = catena_state_copy:selective_copy(Map, Spec),
    ?assertEqual(1, maps:get(a, Copied)),
    ?assertEqual(#{nested => 2}, maps:get(b, Copied)),
    ?assertEqual(3, maps:get(c, Copied)).

%%%---------------------------------------------------------------------
%%% Strategy Specification Tests
%%%---------------------------------------------------------------------

test_strategy_specification() ->
    Default = catena_state_copy:strategy(),
    ?assertEqual(selective, maps:get(strategy, Default)),
    ?assertEqual([], maps:get(exclude, Default)),
    ?assertEqual(infinity, maps:get(max_depth, Default)),

    Custom = catena_state_copy:strategy(#{strategy => deep, max_depth => 5}),
    ?assertEqual(deep, maps:get(strategy, Custom)),
    ?assertEqual(5, maps:get(max_depth, Custom)).

%%%---------------------------------------------------------------------
%%% Validation Tests
%%%---------------------------------------------------------------------

test_validation() ->
    ?assert(catena_state_copy:is_valid_strategy(deep)),
    ?assert(catena_state_copy:is_valid_strategy(shallow)),
    ?assert(catena_state_copy:is_valid_strategy(selective)),
    ?assertNot(catena_state_copy:is_valid_strategy(other)),
    ?assertNot(catena_state_copy:is_valid_strategy(invalid)).

%%%---------------------------------------------------------------------
%%% Copy Value Utility Tests
%%%---------------------------------------------------------------------

test_copy_value() ->
    Value = #{a => 1, b => 2},
    ?assertEqual(Value, catena_state_copy:copy_value(Value, deep)),
    ?assertEqual(Value, catena_state_copy:copy_value(Value, shallow)),
    ?assertEqual(Value, catena_state_copy:copy_value(Value, selective)).

test_copy_value_with_strategy_spec() ->
    Value = #{a => #{b => [1, 2, 3]}, c => 4},
    DeepSpec = #{strategy => deep, max_depth => 1},
    ShallowSpec = #{strategy => shallow},
    SelectiveSpec = #{strategy => selective, exclude => [a]},

    ?assertEqual(Value, catena_state_copy:copy_value(Value, DeepSpec)),
    ?assertEqual(Value, catena_state_copy:copy_value(Value, ShallowSpec)),
    ?assertEqual(Value, catena_state_copy:copy_value(Value, SelectiveSpec)).

%%%---------------------------------------------------------------------
%%% Merge Results Tests
%%%---------------------------------------------------------------------

test_merge() ->
    ?assertEqual({ok, #{a => 1, b => 2}},
        catena_state_copy:merge({ok, #{a => 1}}, {ok, #{b => 2}})),
    ?assertEqual({ok, #{a => 1}},
        catena_state_copy:merge({ok, #{a => 1}}, {ok, not_a_map})),
    ?assertEqual({error, reason},
        catena_state_copy:merge({error, reason}, {ok, #{}})).

%%%---------------------------------------------------------------------
%%% Optimize Strategy Tests
%%%---------------------------------------------------------------------

test_optimize_strategy() ->
    SmallMap = #{a => 1, b => 2},
    ?assertEqual(shallow, catena_state_copy:optimize_strategy(SmallMap, deep)),

    LargeMap = lists:foldl(fun(I, Acc) ->
        Acc#{I => I}
    end, #{}, lists:seq(1, 10)),
    ?assertEqual(deep, catena_state_copy:optimize_strategy(LargeMap, deep)).

%%%---------------------------------------------------------------------
%%% Should Share Tests
%%%---------------------------------------------------------------------

test_should_share() ->
    ?assert(catena_state_copy:should_share(atom)),
    ?assert(catena_state_copy:should_share(42)),
    ?assert(catena_state_copy:should_share(<<"binary">>)),
    ?assert(catena_state_copy:should_share(self())),
    ?assert(catena_state_copy:should_share(make_ref())),
    ?assert(catena_state_copy:should_share(#{})),
    ?assert(catena_state_copy:should_share([])),
    ?assert(catena_state_copy:should_share({})).

%%%---------------------------------------------------------------------
%%% Algebraic Laws Tests
%%%---------------------------------------------------------------------

test_algebraic_laws() ->
    %% Associative
    ?assert(catena_state_copy:associative(deep, deep)),
    ?assert(catena_state_copy:associative(shallow, shallow)),
    ?assertNot(catena_state_copy:associative(deep, shallow)),

    %% Commutative
    ?assert(catena_state_copy:commutative(deep, deep)),
    ?assert(catena_state_copy:commutative(shallow, shallow)),
    ?assert(catena_state_copy:commutative(deep, deep)),

    %% Identity
    ?assert(catena_state_copy:identity(shallow, shallow)),

    %% Idempotent
    ?assert(catena_state_copy:idempotent(deep)),
    ?assert(catena_state_copy:idempotent(shallow)),
    ?assert(catena_state_copy:idempotent(selective)).
