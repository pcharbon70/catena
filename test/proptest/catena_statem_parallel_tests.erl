%% @doc Unit Tests for Phase 5.5: Parallel Execution
-module(catena_statem_parallel_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_statem.hrl").

%%====================================================================
%% Section 5.5.1: Parallel Command Generation
%%====================================================================

gen_parallel_commands_shape_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"runtime">>, runtime_counter_statem, Options),
    {Prefix, Tracks, _Seed} = catena_statem:gen_parallel_commands(SM, 4, 2, 42),
    ?assert(is_list(Prefix)),
    ?assertEqual(2, length(Tracks)),
    ?assert(lists:all(fun is_list/1, Tracks)),
    ok.

%%====================================================================
%% Section 5.5.2: Parallel Execution
%%====================================================================

execute_parallel_tracks_runs_tracks_test() ->
    {ok, System} = runtime_counter_statem:setup(),
    try
        Tracks = [
            [{call, runtime_counter_statem, increment, []}],
            [{call, runtime_counter_statem, increment, []}]
        ],
        Observed = catena_statem:execute_parallel_tracks(runtime_counter_statem, System, #{count => 0}, Tracks, 1000),
        ?assertEqual(2, length(Observed)),
        ?assert(lists:all(
            fun(#{result := Result}) ->
                case Result of
                    {ok, [ok], _Bindings} -> true;
                    _ -> false
                end
            end,
            Observed
        )),
        ?assertEqual(2, runtime_counter_statem:value(System))
    after
        runtime_counter_statem:cleanup(System)
    end,
    ok.

parallel_property_test_basic_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"runtime">>, runtime_counter_statem, Options),
    Result = catena_statem:parallel_property_test(SM, 2, 2, 42),
    ?assert(is_list(Result)),
    ?assertEqual(2, length(Result)),
    ?assert(lists:all(fun({_Seed, Status}) -> Status =:= ok end, Result)),
    ok.

parallel_property_test_returns_seeds_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"runtime">>, runtime_counter_statem, Options),
    Result = catena_statem:parallel_property_test(SM, 2, 3, 100),
    ?assertEqual(3, length(Result)),
    ?assert(lists:all(fun({Seed, _Status}) -> is_tuple(Seed) end, Result)),
    ok.

%%====================================================================
%% Section 5.5.3: Linearizability
%%====================================================================

check_linearizable_accepts_valid_ordering_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"runtime">>, runtime_counter_statem, Options),
    Observed = [
        #{track => 1, commands => [{call, runtime_counter_statem, increment, []}], result => {ok, [ok], []}},
        #{track => 2, commands => [{call, runtime_counter_statem, increment, []}], result => {ok, [ok], []}}
    ],
    ?assertEqual(ok, catena_statem:check_linearizable(SM, #{count => 0}, Observed)),
    ok.

check_linearizable_rejects_invalid_ordering_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"runtime">>, runtime_counter_statem, Options),
    Observed = [
        #{track => 1, commands => [{call, runtime_counter_statem, decrement, []}], result => {ok, [ok], []}}
    ],
    ?assertMatch({error, {non_linearizable, _}}, catena_statem:check_linearizable(SM, #{count => 0}, Observed)),
    ok.

%%====================================================================
%% Section 5.5.4: Race Detection
%%====================================================================

detect_races_reports_conflicts_test() ->
    Tracks = [
        [{call, runtime_counter_statem, increment, []}],
        [{call, runtime_counter_statem, reset, []}]
    ],
    Races = catena_statem:detect_races(Tracks),
    ?assert(length(Races) > 0),
    ok.

%%====================================================================
%% Section 5.5.5: Result Aggregation
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
