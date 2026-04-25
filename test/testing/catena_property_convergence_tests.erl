%% @doc Validation tests for Phase 1 property-engine convergence.
-module(catena_property_convergence_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_property.hrl").

delegated_property_honors_num_tests_test() ->
    PropDecl = {property_decl, "always true",
        {property_forall,
            [{x, 'Bool'}],
            {literal, true, bool, {line, 1}},
            {line, 1}},
        {line, 1}},
    {ok, Prop} = catena_property_adapter:from_property_decl(PropDecl, #{}),
    {passed, Result} = catena_runner:run_property(Prop, #{
        num_tests => 17,
        seed => catena_gen:seed_from_int(101)
    }),
    ?assertEqual(17, Result#property_result.tests_run).

delegated_property_failure_exposes_counterexample_test() ->
    PropDecl = {property_decl, "less than three",
        {property_forall,
            [{n, 'Natural'}],
            {binary_op, lt, {var, n, {line, 1}}, {literal, 3, integer, {line, 1}}, {line, 1}},
            {line, 1}},
        {line, 1}},
    {ok, Prop} = catena_property_adapter:from_property_decl(PropDecl, #{}),
    {failed, Result} = catena_runner:run_property(Prop, #{
        num_tests => 50,
        seed => catena_gen:seed_from_int(21)
    }),
    ?assertEqual(failure, Result#property_result.kind),
    ?assertMatch(#{n := {value, _}}, Result#property_result.original_counterexample),
    ?assertMatch(#{n := {value, _}}, Result#property_result.shrunk_counterexample).

discarded_property_is_reported_through_delegated_path_test() ->
    PropDecl = {property_decl, "always discard",
        {property_forall,
            [],
            {var, discard, {line, 1}},
            {line, 1}},
        {line, 1}},
    {ok, Prop} = catena_property_adapter:from_property_decl(PropDecl, #{discard => discard}),
    {failed, Result} = catena_runner:run_property(Prop, #{
        num_tests => 5,
        seed => catena_gen:seed_from_int(9)
    }),
    ?assertEqual(discarded, Result#property_result.kind),
    ?assert(Result#property_result.tests_discarded > 0).

run_test_with_same_seed_produces_same_failure_details_test() ->
    PropDecl = {property_decl, "less than fifty",
        {property_forall,
            [{n, 'Natural'}],
            {binary_op, lt, {var, n, {line, 1}}, {literal, 50, integer, {line, 1}}, {line, 1}},
            {line, 1}},
        {line, 1}},
    Opts = #{property_iterations => 100, property_seed => 33},
    Result1 = catena_test_runner:run_test(PropDecl, #{}, Opts),
    Result2 = catena_test_runner:run_test(PropDecl, #{}, Opts),
    ?assertEqual(Result1, Result2).

run_test_uses_rich_delegated_property_result_shape_test() ->
    PropDecl = {property_decl, "less than five",
        {property_forall,
            [{n, 'Natural'}],
            {binary_op, lt, {var, n, {line, 1}}, {literal, 5, integer, {line, 1}}, {line, 1}},
            {line, 1}},
        {line, 1}},
    Result = catena_test_runner:run_test(
        PropDecl,
        #{},
        #{property_iterations => 20, property_seed => 41}
    ),
    ?assertMatch(
        {fail, "less than five", {property_counterexample, #{
            tests_run := _,
            shrunk_counterexample := _,
            labels := #{},
            output := _
        }}},
        Result
    ).

shrunk_counterexample_is_stable_for_fixed_seed_test() ->
    PropDecl = {property_decl, "less than ten",
        {property_forall,
            [{n, 'Natural'}],
            {binary_op, lt, {var, n, {line, 1}}, {literal, 10, integer, {line, 1}}, {line, 1}},
            {line, 1}},
        {line, 1}},
    {ok, Prop} = catena_property_adapter:from_property_decl(PropDecl, #{}),
    Seed = catena_gen:seed_from_int(77),
    {failed, Result1} = catena_runner:run_property(Prop, #{num_tests => 50, seed => Seed}),
    {failed, Result2} = catena_runner:run_property(Prop, #{num_tests => 50, seed => Seed}),
    ?assertEqual(Result1#property_result.original_counterexample, Result2#property_result.original_counterexample),
    ?assertEqual(Result1#property_result.shrunk_counterexample, Result2#property_result.shrunk_counterexample).
