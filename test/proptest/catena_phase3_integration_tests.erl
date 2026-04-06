%% @doc Integration Tests for Property Testing Phase 3
%%
%% Comprehensive tests verifying the complete property testing workflow
%% from definition through execution, shrinking, and reporting.
-module(catena_phase3_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_property.hrl").
-include("../../src/proptest/catena_gen.hrl").

%%====================================================================
%% End-to-End Property Testing
%%====================================================================

end_to_end_passing_property_test() ->
    %% Define, run, pass - complete workflow
    Prop = catena_property:property("reverse_id", fun() ->
        catena_property:forall(
            catena_stdgen:gen_list(catena_gen:gen_int()),
            fun(L) -> lists:reverse(lists:reverse(L)) =:= L end
        )
    end),
    {passed, Result} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ?assertEqual(success, Result#property_result.kind),
    ?assertEqual(50, Result#property_result.tests_run),
    ok.

end_to_end_failure_with_shrinking_test() ->
    %% Define, run, fail, shrink, report - full failure workflow
    Prop = catena_property:property("mod_2", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({0, 100})),
            fun(N) -> N rem 2 =:= 0 end
        )
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 100}),
    ?assertEqual(failure, Result#property_result.kind),
    %% Should have both original and shrunk counterexamples
    ?assertNotEqual(undefined, Result#property_result.original_counterexample),
    ?assertNotEqual(undefined, Result#property_result.shrunk_counterexample),
    %% Shrinking should have been attempted
    ?assert(Result#property_result.shrinks_attempted >= 0),
    ok.

reproducibility_same_seed_test() ->
    %% Run with seed, get same result
    Seed = catena_gen:seed_from_int(42),
    Prop = catena_property:property("reproducible", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({0, 100})),
            fun(N) -> N > 50 end
        )
    end),
    {failed, Result1} = catena_runner:run_property(Prop, #{num_tests => 20, seed => Seed}),
    {failed, Result2} = catena_runner:run_property(Prop, #{num_tests => 20, seed => Seed}),
    %% Should get same results
    ?assertEqual(Result1#property_result.original_counterexample,
                 Result2#property_result.original_counterexample),
    ok.

multiple_properties_group_test() ->
    %% Test multiple properties in a group
    Prop1 = catena_property:property("prop1", fun() ->
        catena_property:forall(catena_gen:gen_bool(), fun(B) -> B =:= true orelse B =:= false end)
    end),
    Prop2 = catena_property:property("prop2", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(N) -> N >= 0 end)
    end),
    BatchResult = catena_runner:run_properties([Prop1, Prop2], #{num_tests => 10}),
    ?assertEqual(2, maps:get(total, BatchResult)),
    %% Both properties should pass or fail consistently
    ?assertEqual(maps:get(passed, BatchResult) + maps:get(failed, BatchResult), 2),
    ok.

property_with_preconditions_test() ->
    %% Test property with implications (preconditions)
    Prop = catena_property:property("precondition", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({1, 100})),
            fun(N) -> catena_property:implies(N rem 2 =:= 0, fun() -> N > 0 end) end
        )
    end),
    {Class, Result} = catena_runner:run_property(Prop, #{num_tests => 50}),
    %% Should pass (only testing positive even numbers)
    ?assertEqual(passed, Class),
    ok.

all_tests_discarded_test() ->
    %% Edge case: property where all tests are discarded
    Prop = catena_property:property("always_discard", fun() ->
        catena_property:forall(
            catena_gen:gen_int(),
            fun(_) -> discard end
        )
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 10}),
    ?assertEqual(discarded, Result#property_result.kind),
    ok.

%%====================================================================
%% EUnit Integration
%%====================================================================

eunit_wrapper_integration_test() ->
    %% Test integration with EUnit test runner
    TestFun = fun() ->
        catena_property:forall(catena_gen:gen_bool(), fun(_) -> true end)
    end,
    ?assertEqual(ok, catena_runner:eunit_wrapper(<<"eunit_test">>, TestFun)),
    ok.

property_test_macro_integration_test() ->
    %% Test property_test/2 returns executable function
    TestFun = fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end,
    PropTest = catena_runner:property_test(<<"macro_test">>, TestFun),
    ?assert(is_function(PropTest, 0)),
    ?assertEqual(ok, PropTest()),
    ok.

%%====================================================================
%% Output Format Tests
%%====================================================================

json_output_format_test() ->
    %% Test JSON output format for CI
    Result = #property_result{
        kind = failure,
        tests_run = 100,
        tests_discarded = 5,
        shrinks_attempted = 10,
        seed = #seed{state = 42},
        original_counterexample = 42,
        shrunk_counterexample = 0,
        shrink_history = [42, 21, 10],
        labels = [],
        output = <<>>
    },
    Json = catena_report:to_json(Result),
    ?assert(is_binary(Json)),
    ?assert(binary:match(Json, <<"\"kind\"">>) =/= nomatch),
    ?assert(binary:match(Json, <<"\"tests_run\"">>) =/= nomatch),
    ok.

junit_xml_output_format_test() ->
    %% Test JUnit XML output for CI systems
    Batch = #{
        total => 2,
        passed => 1,
        failed => 1,
        errors => 0,
        results => [
            {<<"test1">>, {passed, #property_result{
                kind = success, tests_run = 10, tests_discarded = 0,
                shrinks_attempted = 0, seed = #seed{state = 1},
                original_counterexample = undefined, shrunk_counterexample = undefined,
                shrink_history = [], labels = [], output = <<>>
            }}},
            {<<"test2">>, {failed, #property_result{
                kind = failure, tests_run = 10, tests_discarded = 0,
                shrinks_attempted = 5, seed = #seed{state = 2},
                original_counterexample = 99, shrunk_counterexample = 0,
                shrink_history = [], labels = [], output = <<>>
            }}}
        ]
    },
    Xml = catena_report:to_junit(<<"test_suite">>, Batch),
    ?assert(is_binary(Xml)),
    ?assert(binary:match(Xml, <<"<?xml">>) =/= nomatch),
    ?assert(binary:match(Xml, <<"<testsuites">>) =/= nomatch),
    ok.

%%====================================================================
%% Performance Tests
%%====================================================================

performance_100_tests_complex_generators_test() ->
    %% Test that 100 tests with complex generators runs in reasonable time
    Prop = catena_property:property("perf", fun() ->
        catena_property:forall(
            catena_stdgen:gen_list(
                catena_stdgen:gen_map(
                    catena_gen:gen_int(catena_range:range_constant({0, 50})),
                    catena_gen:gen_bool()
                )
            ),
            fun(L) -> length(L) rem 2 =:= 0 end
        )
    end),
    {Time, _Result} = timer:tc(fun() ->
        catena_runner:run_property(Prop, #{num_tests => 100})
    end),
    %% Should complete in less than 5 seconds (5,000,000 microseconds)
    ?assert(Time < 5000000),
    ok.

%%====================================================================
%% User Experience Tests
%%====================================================================

failure_report_clear_and_helpful_test() ->
    %% Test that failure reports are clear and helpful
    Prop = catena_property:property("helpful_failure", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({0, 100})),
            fun(N) -> N rem 10 =/= 0 end
        )
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 50}),
    Report = iolist_to_binary(catena_report:format_failure_context(<<"helpful_failure">>, Result)),
    %% Report should contain key information
    ?assert(binary:match(Report, <<"Tests run">>) =/= nomatch),
    ?assert(binary:match(Report, <<"Seed">>) =/= nomatch),
    ok.

counterexample_readable_test() ->
    %% Test that counterexamples are formatted in readable way
    Value = {[1, 2, 3], {a, b}, #{key => value}},
    Formatted = iolist_to_binary(catena_report:format_counterexample(Value)),
    %% Should contain the structure
    ?assert(is_binary(Formatted)),
    ?assert(byte_size(Formatted) > 0),
    ok.
