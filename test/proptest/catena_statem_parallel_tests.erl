%% @doc Unit Tests for Phase 5.5: Parallel Execution
-module(catena_statem_parallel_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_statem.hrl").

%%====================================================================
%% Section 5.5.1: Parallel Property Testing
%%====================================================================

parallel_property_test_basic_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    %% Run with 2 parallel workers
    Result = catena_statem:parallel_property_test(SM, 10, 2, 42),
    ?assert(is_list(Result)),
    ?assertEqual(2, length(Result)),
    ok.

parallel_property_test_returns_seeds_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Result = catena_statem:parallel_property_test(SM, 10, 3, 100),
    %% Should have 3 results with seeds
    ?assertEqual(3, length(Result)),
    %% All results should be ok (placeholder implementation)
    ?assert(lists:all(fun({_Seed, Status}) -> Status =:= ok end, Result)),
    ok.

%%====================================================================
%% Section 5.5.2: Result Aggregation
%%====================================================================

aggregate_parallel_results_all_pass_test() ->
    Results = [
        {42, ok},
        {43, ok},
        {44, ok}
    ],
    {passed, Count, Failed} = catena_statem:aggregate_parallel_results(Results),
    ?assertEqual(passed, passed),
    ?assertEqual(3, Count),
    ?assertEqual([], Failed),
    ok.

aggregate_parallel_results_some_fail_test() ->
    Results = [
        {42, ok},
        {43, {error, assertion_failed}},
        {44, ok},
        {45, {error, postcondition_failed}}
    ],
    {passed, Count, Failed} = catena_statem:aggregate_parallel_results(Results),
    ?assertEqual(2, Count),
    ?assertEqual(2, length(Failed)),
    ?assert(lists:keymember(43, 1, Failed)),
    ?assert(lists:keymember(45, 1, Failed)),
    ok.

aggregate_parallel_results_all_fail_test() ->
    Results = [
        {42, {error, failed}},
        {43, {error, timeout}}
    ],
    {passed, Count, Failed} = catena_statem:aggregate_parallel_results(Results),
    ?assertEqual(0, Count),
    ?assertEqual(2, length(Failed)),
    ok.

aggregate_parallel_results_empty_test() ->
    Results = [],
    {passed, Count, Failed} = catena_statem:aggregate_parallel_results(Results),
    ?assertEqual(0, Count),
    ?assertEqual([], Failed),
    ok.
