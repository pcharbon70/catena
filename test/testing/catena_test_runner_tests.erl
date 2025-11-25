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
    Result = catena_test_runner:run_test(PropDecl, #{}, #{property_iterations => 10}),
    ?assertMatch({fail, "always false", {counterexample, 1, _}}, Result).

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
    Result = catena_test_runner:run_test(PropDecl, #{}, #{property_iterations => 200}),
    %% This should fail with a counterexample
    ?assertMatch({fail, "less than 50", {counterexample, _, _}}, Result).

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
