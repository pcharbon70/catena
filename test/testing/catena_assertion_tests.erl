%% @doc Tests for Catena Assertion Functions (Phase 2.3)
%%
%% These tests verify that the assertion functions in the prelude work
%% correctly for both passing and failing cases.
-module(catena_assertion_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% assert/1 Tests
%%====================================================================

assert_true_passes_test() ->
    ?assertEqual(ok, catena_prelude:assert(true)).

assert_false_throws_test() ->
    ?assertThrow({assertion_failed, _}, catena_prelude:assert(false)).

%%====================================================================
%% assert_equals/2 Tests
%%====================================================================

assert_equals_same_integer_test() ->
    ?assertEqual(ok, catena_prelude:assert_equals(42, 42)).

assert_equals_same_string_test() ->
    ?assertEqual(ok, catena_prelude:assert_equals("hello", "hello")).

assert_equals_same_list_test() ->
    ?assertEqual(ok, catena_prelude:assert_equals([1, 2, 3], [1, 2, 3])).

assert_equals_same_tuple_test() ->
    ?assertEqual(ok, catena_prelude:assert_equals({ok, 42}, {ok, 42})).

assert_equals_different_values_throws_test() ->
    ?assertThrow(
        {assertion_failed, {expected, 5, actual, 4}},
        catena_prelude:assert_equals(5, 4)
    ).

assert_equals_different_types_throws_test() ->
    ?assertThrow(
        {assertion_failed, _},
        catena_prelude:assert_equals(1, "1")
    ).

assert_equals_empty_vs_nonempty_list_throws_test() ->
    ?assertThrow(
        {assertion_failed, _},
        catena_prelude:assert_equals([], [1])
    ).

%%====================================================================
%% assert_not_equals/2 Tests
%%====================================================================

assert_not_equals_different_values_passes_test() ->
    ?assertEqual(ok, catena_prelude:assert_not_equals(1, 2)).

assert_not_equals_different_types_passes_test() ->
    ?assertEqual(ok, catena_prelude:assert_not_equals(1, "1")).

assert_not_equals_same_value_throws_test() ->
    ?assertThrow(
        {assertion_failed, {should_not_equal, 42, 42}},
        catena_prelude:assert_not_equals(42, 42)
    ).

assert_not_equals_same_list_throws_test() ->
    ?assertThrow(
        {assertion_failed, {should_not_equal, [1, 2], [1, 2]}},
        catena_prelude:assert_not_equals([1, 2], [1, 2])
    ).

%%====================================================================
%% assert_true/1 Tests
%%====================================================================

assert_true_true_passes_test() ->
    ?assertEqual(ok, catena_prelude:assert_true(true)).

assert_true_false_throws_test() ->
    ?assertThrow(
        {assertion_failed, {expected_true, false}},
        catena_prelude:assert_true(false)
    ).

assert_true_non_boolean_throws_test() ->
    %% Passing a non-boolean should still throw (not match true clause)
    ?assertThrow(
        {assertion_failed, {expected_true, 1}},
        catena_prelude:assert_true(1)
    ).

%%====================================================================
%% assert_false/1 Tests
%%====================================================================

assert_false_false_passes_test() ->
    ?assertEqual(ok, catena_prelude:assert_false(false)).

assert_false_true_throws_test() ->
    ?assertThrow(
        {assertion_failed, {expected_false, true}},
        catena_prelude:assert_false(true)
    ).

assert_false_non_boolean_throws_test() ->
    ?assertThrow(
        {assertion_failed, {expected_false, 0}},
        catena_prelude:assert_false(0)
    ).

%%====================================================================
%% Prelude Bindings Tests
%%====================================================================

assert_in_prelude_bindings_test() ->
    Bindings = catena_prelude:prelude_bindings(),
    ?assert(maps:is_key(assert, Bindings)).

assert_equals_in_prelude_bindings_test() ->
    Bindings = catena_prelude:prelude_bindings(),
    ?assert(maps:is_key(assert_equals, Bindings)).

assert_not_equals_in_prelude_bindings_test() ->
    Bindings = catena_prelude:prelude_bindings(),
    ?assert(maps:is_key(assert_not_equals, Bindings)).

assert_true_in_prelude_bindings_test() ->
    Bindings = catena_prelude:prelude_bindings(),
    ?assert(maps:is_key(assert_true, Bindings)).

assert_false_in_prelude_bindings_test() ->
    Bindings = catena_prelude:prelude_bindings(),
    ?assert(maps:is_key(assert_false, Bindings)).

%%====================================================================
%% Integration Tests
%%====================================================================

assertion_function_callable_from_binding_test() ->
    Bindings = catena_prelude:prelude_bindings(),
    {AssertFun, 1, _Type} = maps:get(assert, Bindings),
    ?assertEqual(ok, AssertFun(true)).

assert_equals_callable_from_binding_test() ->
    Bindings = catena_prelude:prelude_bindings(),
    {AssertEqFun, 2, _Type} = maps:get(assert_equals, Bindings),
    ?assertEqual(ok, AssertEqFun(42, 42)).
