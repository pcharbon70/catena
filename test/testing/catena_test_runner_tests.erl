%% @doc Tests for the Catena Test Runner (Phase 2.3)
%%
%% These tests verify that the test execution framework works correctly
%% for both unit tests and property tests.
-module(catena_test_runner_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Collection Tests
%%====================================================================

collect_tests_empty_test() ->
    ?assertEqual([], catena_test_runner:collect_tests([])).

collect_tests_filters_non_tests_test() ->
    Decls = [
        {type_decl, 'Foo', [], []},
        {transform_decl, foo, undefined, [], {line, 1}}
    ],
    ?assertEqual([], catena_test_runner:collect_tests(Decls)).

collect_tests_finds_test_decls_test() ->
    TestDecl = {test_decl, "my test", {literal, true, bool, {line, 1}}, {line, 1}},
    Decls = [
        {type_decl, 'Foo', [], []},
        TestDecl,
        {transform_decl, foo, undefined, [], {line, 1}}
    ],
    ?assertEqual([TestDecl], catena_test_runner:collect_tests(Decls)).

collect_tests_finds_property_decls_test() ->
    PropDecl = {property_decl, "my prop",
        {property_forall, [{x, 'Bool'}], {var, x, {line, 1}}, {line, 1}},
        {line, 1}},
    Decls = [PropDecl],
    ?assertEqual([PropDecl], catena_test_runner:collect_tests(Decls)).

%%====================================================================
%% Unit Test Execution Tests
%%====================================================================

run_passing_test_test() ->
    TestDecl = {test_decl, "passes", {literal, true, bool, {line, 1}}, {line, 1}},
    Result = catena_test_runner:run_test(TestDecl, #{}),
    ?assertEqual({pass, "passes"}, Result).

run_failing_test_test() ->
    TestDecl = {test_decl, "fails", {literal, false, bool, {line, 1}}, {line, 1}},
    Result = catena_test_runner:run_test(TestDecl, #{}),
    ?assertMatch({fail, "fails", _}, Result).

run_test_with_arithmetic_test() ->
    %% Test: 2 + 2 == 4
    TestDecl = {test_decl, "arithmetic",
        {binary_op, eq,
            {binary_op, plus, {literal, 2, integer, {line, 1}}, {literal, 2, integer, {line, 1}}, {line, 1}},
            {literal, 4, integer, {line, 1}},
            {line, 1}},
        {line, 1}},
    Result = catena_test_runner:run_test(TestDecl, #{}),
    ?assertEqual({pass, "arithmetic"}, Result).

run_test_with_failing_arithmetic_test() ->
    %% Test: 2 + 2 == 5 (should fail)
    TestDecl = {test_decl, "bad arithmetic",
        {binary_op, eq,
            {binary_op, plus, {literal, 2, integer, {line, 1}}, {literal, 2, integer, {line, 1}}, {line, 1}},
            {literal, 5, integer, {line, 1}},
            {line, 1}},
        {line, 1}},
    Result = catena_test_runner:run_test(TestDecl, #{}),
    ?assertMatch({fail, "bad arithmetic", _}, Result).

run_test_with_prelude_test() ->
    %% Test using identity from prelude: identity(true)
    TestDecl = {test_decl, "uses prelude",
        {app, {var, identity, {line, 1}}, [{literal, true, bool, {line, 1}}], {line, 1}},
        {line, 1}},
    Env = catena_prelude:prelude_bindings(),
    Result = catena_test_runner:run_test(TestDecl, Env),
    ?assertEqual({pass, "uses prelude"}, Result).

%%====================================================================
%% Run Tests Aggregate Tests
%%====================================================================

run_tests_empty_test() ->
    Results = catena_test_runner:run_tests([]),
    ?assertEqual(#{passed => 0, failed => 0, total => 0, results => []}, Results).

run_tests_all_pass_test() ->
    Tests = [
        {test_decl, "test1", {literal, true, bool, {line, 1}}, {line, 1}},
        {test_decl, "test2", {literal, true, bool, {line, 2}}, {line, 2}}
    ],
    Results = catena_test_runner:run_tests(Tests),
    ?assertEqual(2, maps:get(passed, Results)),
    ?assertEqual(0, maps:get(failed, Results)),
    ?assertEqual(2, maps:get(total, Results)).

run_tests_some_fail_test() ->
    Tests = [
        {test_decl, "pass", {literal, true, bool, {line, 1}}, {line, 1}},
        {test_decl, "fail", {literal, false, bool, {line, 2}}, {line, 2}},
        {test_decl, "pass2", {literal, true, bool, {line, 3}}, {line, 3}}
    ],
    Results = catena_test_runner:run_tests(Tests),
    ?assertEqual(2, maps:get(passed, Results)),
    ?assertEqual(1, maps:get(failed, Results)),
    ?assertEqual(3, maps:get(total, Results)).

%%====================================================================
%% Property Test Execution Tests
%%====================================================================

run_property_always_true_test() ->
    %% Property: forall x : Bool. true
    PropDecl = {property_decl, "always true",
        {property_forall,
            [{x, 'Bool'}],
            {literal, true, bool, {line, 1}},
            {line, 1}},
        {line, 1}},
    Result = catena_test_runner:run_test(PropDecl, #{}, #{property_iterations => 10}),
    ?assertEqual({pass, "always true"}, Result).

run_property_always_false_test() ->
    %% Property: forall x : Bool. false
    PropDecl = {property_decl, "always false",
        {property_forall,
            [{x, 'Bool'}],
            {literal, false, bool, {line, 1}},
            {line, 1}},
        {line, 1}},
    Result = catena_test_runner:run_test(PropDecl, #{}, #{
        property_iterations => 10,
        property_seed => 7
    }),
    ?assertMatch(
        {fail, "always false", {property_counterexample, #{
            tests_run := 1,
            shrunk_counterexample := #{x := _}
        }}},
        Result
    ).

run_property_with_natural_test() ->
    %% Property: forall n : Natural. n >= 0
    PropDecl = {property_decl, "naturals non-negative",
        {property_forall,
            [{n, 'Natural'}],
            {binary_op, gte, {var, n, {line, 1}}, {literal, 0, integer, {line, 1}}, {line, 1}},
            {line, 1}},
        {line, 1}},
    Result = catena_test_runner:run_test(PropDecl, #{}, #{property_iterations => 50}),
    ?assertEqual({pass, "naturals non-negative"}, Result).

run_property_failing_with_counterexample_test() ->
    %% Property: forall n : Natural. n < 50 (will fail for n >= 50)
    PropDecl = {property_decl, "less than 50",
        {property_forall,
            [{n, 'Natural'}],
            {binary_op, lt, {var, n, {line, 1}}, {literal, 50, integer, {line, 1}}, {line, 1}},
            {line, 1}},
        {line, 1}},
    %% Run with enough iterations to likely hit a value >= 50
    Result = catena_test_runner:run_test(PropDecl, #{}, #{
        property_iterations => 200,
        property_seed => 13
    }),
    %% This should fail with a counterexample
    ?assertMatch(
        {fail, "less than 50", {property_counterexample, #{
            tests_run := _,
            original_counterexample := #{n := _}
        }}},
        Result
    ).

%%====================================================================
%% Test Result Formatting Tests
%%====================================================================

format_pass_result_test() ->
    Result = catena_test_runner:format_result({pass, "my test"}),
    ?assert(string:find(Result, "✓") =/= nomatch),
    ?assert(string:find(Result, "my test") =/= nomatch).

format_fail_result_test() ->
    Result = catena_test_runner:format_result({fail, "my test", {expected_true, false}}),
    ?assert(string:find(Result, "✗") =/= nomatch),
    ?assert(string:find(Result, "my test") =/= nomatch).

format_property_fail_result_test() ->
    Result = catena_test_runner:format_result({fail, "property fails", {property_counterexample, #{
        tests_run => 3,
        seed => seeded,
        shrunk_counterexample => #{x => false}
    }}}),
    ?assert(string:find(Result, "property fails") =/= nomatch),
    ?assert(string:find(Result, "Counterexample") =/= nomatch).

format_results_all_pass_test() ->
    Results = #{
        passed => 3,
        failed => 0,
        total => 3,
        results => [
            {pass, "test1"},
            {pass, "test2"},
            {pass, "test3"}
        ]
    },
    Output = catena_test_runner:format_results(Results),
    ?assert(string:find(Output, "3 passed") =/= nomatch),
    ?assert(string:find(Output, "all tests passed") =/= nomatch).

format_results_with_failures_test() ->
    Results = #{
        passed => 2,
        failed => 1,
        total => 3,
        results => [
            {pass, "test1"},
            {fail, "test2", {expected_true, false}},
            {pass, "test3"}
        ]
    },
    Output = catena_test_runner:format_results(Results),
    ?assert(string:find(Output, "2 passed") =/= nomatch),
    ?assert(string:find(Output, "1 failed") =/= nomatch).

%%====================================================================
%% Expression Evaluation Tests
%%====================================================================

eval_list_expr_test() ->
    TestDecl = {test_decl, "list",
        {binary_op, eq,
            {list_expr, [
                {literal, 1, integer, {line, 1}},
                {literal, 2, integer, {line, 1}},
                {literal, 3, integer, {line, 1}}
            ], {line, 1}},
            {list_expr, [
                {literal, 1, integer, {line, 1}},
                {literal, 2, integer, {line, 1}},
                {literal, 3, integer, {line, 1}}
            ], {line, 1}},
            {line, 1}},
        {line, 1}},
    Result = catena_test_runner:run_test(TestDecl, #{}),
    ?assertEqual({pass, "list"}, Result).

eval_let_expr_test() ->
    %% Test: let x = 5 in x + 1 == 6
    TestDecl = {test_decl, "let binding",
        {binary_op, eq,
            {let_expr,
                [{pat_var, x, {line, 1}}, {literal, 5, integer, {line, 1}}],
                {binary_op, plus, {var, x, {line, 1}}, {literal, 1, integer, {line, 1}}, {line, 1}},
                {line, 1}},
            {literal, 6, integer, {line, 1}},
            {line, 1}},
        {line, 1}},
    Result = catena_test_runner:run_test(TestDecl, #{}),
    ?assertEqual({pass, "let binding"}, Result).

eval_comparison_operators_test() ->
    Tests = [
        {"lt", lt, 1, 2, true},
        {"gt", gt, 2, 1, true},
        {"lte eq", lte, 2, 2, true},
        {"lte lt", lte, 1, 2, true},
        {"gte eq", gte, 2, 2, true},
        {"gte gt", gte, 2, 1, true},
        {"neq", neq, 1, 2, true}
    ],
    lists:foreach(
        fun({Name, Op, L, R, Expected}) ->
            TestDecl = {test_decl, Name,
                {binary_op, eq,
                    {binary_op, Op,
                        {literal, L, integer, {line, 1}},
                        {literal, R, integer, {line, 1}},
                        {line, 1}},
                    {literal, Expected, bool, {line, 1}},
                    {line, 1}},
                {line, 1}},
            Result = catena_test_runner:run_test(TestDecl, #{}),
            ?assertEqual({pass, Name}, Result)
        end,
        Tests
    ).

eval_lambda_application_test() ->
    Loc = {line, 1},
    TestDecl = {test_decl, "lambda application",
        {binary_op, eq,
            {app,
                {lambda,
                    [{pat_var, x, Loc}],
                    {binary_op, plus,
                        {var, x, Loc},
                        {literal, 1, integer, Loc},
                        Loc},
                    Loc},
                [{literal, 41, integer, Loc}],
                Loc},
            {literal, 42, integer, Loc},
            Loc},
        Loc},
    Result = catena_test_runner:run_test(TestDecl, #{}),
    ?assertEqual({pass, "lambda application"}, Result).

eval_partial_application_test() ->
    Loc = {line, 1},
    Env = #{
        add => {fun(A, B) -> A + B end, 2, runtime}
    },
    TestDecl = {test_decl, "partial application",
        {binary_op, eq,
            {app,
                {app,
                    {var, add, Loc},
                    [{literal, 1, integer, Loc}],
                    Loc},
                [{literal, 41, integer, Loc}],
                Loc},
            {literal, 42, integer, Loc},
            Loc},
        Loc},
    Result = catena_test_runner:run_test(TestDecl, Env),
    ?assertEqual({pass, "partial application"}, Result).

%%====================================================================
%% Runtime Declaration Execution Tests
%%====================================================================

build_runtime_env_exposes_constructors_and_transforms_test() ->
    Loc = {line, 1},
    Decls = [
        {type_decl, 'Maybe', [],
            [
                {constructor, 'None', [], Loc},
                {constructor, 'Some', [{type_con, 'Int', Loc}], Loc}
            ],
            [],
            Loc},
        {transform_decl, id, undefined,
            [
                {transform_clause,
                    [{pat_var, x, Loc}],
                    undefined,
                    {var, x, Loc},
                    Loc}
            ],
            Loc}
    ],
    Env = catena_test_runner:build_runtime_env(Decls),
    ?assertEqual(none, maps:get('None', Env)),
    {SomeFun, 1, runtime} = maps:get('Some', Env),
    ?assertEqual({some, 42}, SomeFun(42)),
    {IdFun, 1, runtime} = maps:get(id, Env),
    ?assertEqual(42, IdFun(42)).

run_test_value_passes_test() ->
    TestValue = #{
        name => "law passes",
        run => fun(_Unit) -> passed end
    },
    ?assertEqual({pass, "law passes"}, catena_test_runner:run_test_value(TestValue)).

run_test_value_fails_with_message_test() ->
    TestValue = #{
        name => "law fails",
        run => fun(_Unit) -> {failed, "Law verification failed"} end
    },
    ?assertEqual(
        {fail, "law fails", "Law verification failed"},
        catena_test_runner:run_test_value(TestValue)
    ).

run_property_value_passes_test() ->
    PropertyValue = #{
        name => "naturals are non-negative",
        body => fun(_Unit) ->
            {for_all, gen_natural, fun(N) -> N >= 0 end}
        end,
        config => #{iterations => 10, seed => {some, 7}, labels => []}
    },
    ?assertEqual(
        {pass, "naturals are non-negative"},
        catena_test_runner:run_property_value(PropertyValue)
    ).

run_property_value_honors_property_iterations_test() ->
    erase(property_runs),
    PropertyValue = #{
        name => "single configured run",
        body => fun(_Unit) ->
            {for_all, {gen_constant_int, 0}, fun(_Value) ->
                Count = case get(property_runs) of
                    undefined -> 0;
                    Existing -> Existing
                end,
                put(property_runs, Count + 1),
                Count =:= 0
            end}
        end,
        config => #{iterations => 1, seed => {some, 11}, labels => []}
    },
    ?assertEqual(
        {pass, "single configured run"},
        catena_test_runner:run_property_value(PropertyValue)
    ),
    ?assertEqual(1, get(property_runs)),
    erase(property_runs).

run_property_value_fails_with_counterexample_test() ->
    PropertyValue = #{
        name => "less than three",
        body => fun(_Unit) ->
            {for_all, {gen_int_range, 0, 10}, fun(N) -> N < 3 end}
        end,
        config => #{iterations => 10, seed => {some, 5}, labels => []}
    },
    Result = catena_test_runner:run_property_value(PropertyValue),
    ?assertMatch(
        {fail, "less than three", {property_counterexample, #{
            tests_run := _,
            original_counterexample := _
        }}},
        Result
    ).

run_test_value_delegates_property_values_test() ->
    PropertyValue = #{
        name => "delegated property",
        body => fun(_Unit) ->
            {for_all, gen_bool, fun(_Bool) -> true end}
        end,
        config => #{iterations => 3, seed => {some, 3}, labels => []}
    },
    ?assertEqual(
        {pass, "delegated property"},
        catena_test_runner:run_test_value(PropertyValue)
    ).

run_suite_value_aggregates_results_test() ->
    SuiteValue = #{
        name => "Law Suite",
        tests => [
            #{name => "identity", run => fun(_Unit) -> passed end},
            #{name => "associativity", run => fun(_Unit) -> {failed, "Law verification failed"} end}
        ]
    },
    Results = catena_test_runner:run_suite_value(SuiteValue),
    ?assertEqual(1, maps:get(passed, Results)),
    ?assertEqual(1, maps:get(failed, Results)),
    ?assertEqual(2, maps:get(total, Results)),
    ?assert(lists:member({pass, "identity"}, maps:get(results, Results))),
    ?assert(lists:member({fail, "associativity", "Law verification failed"}, maps:get(results, Results))).

run_suite_value_ignores_empty_properties_field_test() ->
    SuiteValue = #{
        name => "Mixed Suite",
        tests => [
            #{name => "identity", run => fun(_Unit) -> passed end}
        ],
        properties => []
    },
    Results = catena_test_runner:run_suite_value(SuiteValue),
    ?assertEqual(1, maps:get(passed, Results)),
    ?assertEqual(0, maps:get(failed, Results)),
    ?assertEqual(1, maps:get(total, Results)).

run_suite_value_runs_properties_test() ->
    SuiteValue = #{
        name => "Mixed Suite",
        tests => [
            #{name => "identity", run => fun(_Unit) -> passed end}
        ],
        properties => [
            #{
                name => "property passes",
                body => fun(_Unit) ->
                    {for_all, gen_bool, fun(_Bool) -> true end}
                end,
                config => #{iterations => 5, seed => {some, 17}, labels => []}
            }
        ]
    },
    Results = catena_test_runner:run_suite_value(SuiteValue),
    ?assertEqual(2, maps:get(passed, Results)),
    ?assertEqual(0, maps:get(failed, Results)),
    ?assertEqual(2, maps:get(total, Results)).
