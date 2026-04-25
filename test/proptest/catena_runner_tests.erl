%% @doc Unit tests for catena_runner (Section 3.2)
%%
%% Tests the test runner infrastructure including test execution,
%% result types, orchestration, and EUnit integration.
-module(catena_runner_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_property.hrl").
-include("../../src/proptest/catena_gen.hrl").

%%====================================================================
%% Section 3.2.1: Test Execution Engine
%%====================================================================

run_property_with_default_config_test() ->
    Prop = catena_property:property("always_pass", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    Result = catena_runner:run_property(Prop, #{num_tests => 10}),
    ?assertMatch({passed, _}, Result),
    ok.

run_property_with_custom_num_tests_test() ->
    Prop = catena_property:property("test", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    {passed, Result} = catena_runner:run_property(Prop, #{num_tests => 25}),
    ?assertEqual(25, Result#property_result.tests_run),
    ok.

run_property_early_exit_on_failure_test() ->
    Prop = catena_property:property("fail_early", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> false end)
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 100}),
    %% Should fail before all tests complete
    ?assert(Result#property_result.tests_run < 100),
    ok.

size_scaling_test() ->
    %% Test that size is passed correctly to generators
    Prop = catena_property:property("size_test", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    {passed, _Result} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

%%====================================================================
%% Section 3.2.2: Result Types
%%====================================================================

result_type_success_test() ->
    Prop = catena_property:property("success", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    {passed, Result} = catena_runner:run_property(Prop, #{num_tests => 5}),
    ?assertEqual(success, Result#property_result.kind),
    ?assertEqual(5, Result#property_result.tests_run),
    ?assertEqual(0, Result#property_result.tests_discarded),
    ok.

result_type_failure_test() ->
    Prop = catena_property:property("failure", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> false end)
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 10}),
    ?assertEqual(failure, Result#property_result.kind),
    ?assert(is_integer(Result#property_result.original_counterexample)),
    ok.

result_type_discarded_test() ->
    %% Property that always discards
    Prop = catena_property:property("discard", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> discard end)
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 5}),
    ?assertEqual(discarded, Result#property_result.kind),
    ?assert(Result#property_result.tests_discarded > 0),
    ok.

result_includes_seed_test() ->
    Prop = catena_property:property("seed_test", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    {passed, Result} = catena_runner:run_property(Prop, #{num_tests => 5}),
    ?assertMatch(#seed{}, Result#property_result.seed),
    ok.

result_includes_static_label_counts_test() ->
    Prop0 = catena_property:property("labeled", fun() ->
        catena_property:forall(catena_gen:constant(1), fun(_) -> true end)
    end),
    Prop = catena_property:with_label(<<"regression">>, Prop0),
    {passed, Result} = catena_runner:run_property(Prop, #{num_tests => 4}),
    ?assertEqual(#{<<"regression">> => 4}, Result#property_result.labels),
    ?assert(binary:match(Result#property_result.output, <<"labels=regression=4">>) =/= nomatch),
    ok.

result_includes_classification_counts_test() ->
    Prop0 = catena_property:property("classified", fun() ->
        catena_property:forall(catena_gen:constant(1), fun(_) -> true end)
    end),
    Prop = catena_property:classify(<<"sign">>, fun(N) when N > 0 -> positive end, Prop0),
    {passed, Result} = catena_runner:run_property(Prop, #{num_tests => 3}),
    ?assertEqual(#{<<"sign:positive">> => 3}, Result#property_result.labels),
    ?assert(binary:match(Result#property_result.output, <<"sign:positive=3">>) =/= nomatch),
    ok.

result_includes_discard_summary_test() ->
    Prop = catena_property:property("discard_summary", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> discard end)
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 5}),
    ?assertEqual(discarded, Result#property_result.kind),
    ?assert(binary:match(Result#property_result.output, <<"discards=">>) =/= nomatch),
    ok.

%%====================================================================
%% Section 3.2.3: Test Orchestration
%%====================================================================

run_properties_sequential_test() ->
    Prop1 = catena_property:property("prop1", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    Prop2 = catena_property:property("prop2", fun() ->
        catena_property:forall(catena_gen:gen_bool(), fun(_) -> true end)
    end),
    BatchResult = catena_runner:run_properties([Prop1, Prop2], #{num_tests => 5}),
    ?assertEqual(2, maps:get(total, BatchResult)),
    ?assertEqual(2, maps:get(passed, BatchResult)),
    ?assertEqual(0, maps:get(failed, BatchResult)),
    ok.

run_properties_aggregates_results_test() ->
    Prop1 = catena_property:property("pass", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    Prop2 = catena_property:property("fail", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> false end)
    end),
    BatchResult = catena_runner:run_properties([Prop1, Prop2], #{num_tests => 5}),
    ?assertEqual(2, maps:get(total, BatchResult)),
    ?assertEqual(1, maps:get(passed, BatchResult)),
    ?assertEqual(1, maps:get(failed, BatchResult)),
    ?assertEqual(2, length(maps:get(results, BatchResult))),
    ok.

run_properties_empty_list_test() ->
    BatchResult = catena_runner:run_properties([]),
    ?assertEqual(0, maps:get(total, BatchResult)),
    ?assertEqual(0, maps:get(passed, BatchResult)),
    ?assertEqual(0, maps:get(failed, BatchResult)),
    ok.

%%====================================================================
%% Section 3.2.4: Integration with Test Framework
%%====================================================================

eunit_wrapper_passing_test() ->
    TestFun = fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end,
    ?assertEqual(ok, catena_runner:eunit_wrapper(<<"test">>, TestFun)),
    ok.

property_test_function_returns_fun_test() ->
    TestFun = fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end,
    PropTest = catena_runner:property_test(<<"test">>, TestFun),
    ?assert(is_function(PropTest, 0)),
    ok.

property_test_function_executes_test() ->
    TestFun = fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end,
    PropTest = catena_runner:property_test(<<"test">>, TestFun),
    ?assertEqual(ok, PropTest()),
    ok.

%%====================================================================
%% Edge Cases
%%====================================================================

run_all_tests_discarded_test() ->
    %% Property that always discards should eventually error
    Prop = catena_property:property("always_discard", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> discard end)
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 5}),
    ?assertEqual(discarded, Result#property_result.kind),
    ok.

run_with_reproducible_seed_test() ->
    Seed = catena_gen:seed_from_int(42),
    Prop = catena_property:property("seeded", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    {passed, Result1} = catena_runner:run_property(Prop, #{num_tests => 10, seed => Seed}),
    {passed, Result2} = catena_runner:run_property(Prop, #{num_tests => 10, seed => Seed}),
    ?assertEqual(Result1#property_result.seed, Result2#property_result.seed),
    ok.
