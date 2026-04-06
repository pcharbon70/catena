%% @doc Property Testing Phase 3, Section 3.2: Test Runner Infrastructure
%%
%% This module implements the test runner that executes properties, generates
%% random test cases, checks predicates, and manages test orchestration.
%%
%% == Test Execution ==
%%
%% The runner executes properties by:
%% 1. Generating test cases using the property's generator
%% 2. Scaling size from 0 to 100 across test runs
%% 3. Checking the predicate against each generated value
%% 4. Early exiting on first failure
%% 5. Tracking discards from failed preconditions
%%
%% @see catena_property for property definition
%% @see catena_gen for generator infrastructure
-module(catena_runner).

-include_lib("eunit/include/eunit.hrl").
-include("catena_property.hrl").

%% API exports - Section 3.2.1: Test Execution Engine
-export([
    run_property/1,
    run_property/2,
    run_test_case/3
]).

%% API exports - Section 3.2.3: Test Orchestration
-export([
    run_properties/1,
    run_properties/2
]).

%% API exports - Section 3.2.4: Integration with Test Framework
-export([
    property_test/2,
    eunit_wrapper/2
]).

%% Type exports
-export_type([
    run_options/0,
    run_result/0,
    discard/0
]).

%%====================================================================
%% Types
%%====================================================================

%% Options for running properties.
-record(run_options, {
    num_tests :: pos_integer(),
    max_shrinks :: pos_integer(),
    seed :: catena_gen:seed() | undefined,
    timeout :: pos_integer() | infinity,
    parallel :: boolean()
}).

-type run_options() :: #run_options{}.

%% Result of running a single property.
-type run_result() :: {passed, catena_property:property_result()} | {failed, catena_property:property_result()}.

%% Discard marker for failed preconditions.
-type discard() :: discard.

%% Result of running multiple properties.
-type batch_result() :: #{
    total := non_neg_integer(),
    passed := non_neg_integer(),
    failed := non_neg_integer(),
    errors := non_neg_integer(),
    results := [{binary(), run_result()}]
}.

%%====================================================================
%% Section 3.2.1: Test Execution Engine
%%====================================================================

%% @doc Run a property with default configuration.
%%
%% Uses 100 tests, 1000 shrinks, random seed, 5 second timeout.
%%
%% == Example ==
%%
%% ```
%% Prop = catena_property:property("reverse_id", fun() ->
%%     catena_property:forall(gen_list(gen_int()), fun(L) ->
%%         lists:reverse(lists:reverse(L)) =:= L
%%     end)
%% end),
%% {passed, Result} = catena_runner:run_property(Prop).
%% ```
-spec run_property(catena_property:property()) -> run_result().
run_property(Property) ->
    run_property(Property, #run_options{
        num_tests = 100,
        max_shrinks = 1000,
        seed = catena_gen:seed_new(),
        timeout = 5000,
        parallel = false
    }).

%% @doc Run a property with custom configuration.
%%
%% Accepts a proplist or map with options:
%% - `num_tests`: Number of test cases to generate (default: 100)
%% - `max_shrinks`: Maximum shrinking attempts (default: 1000)
%% - `seed`: Specific seed for reproducibility (default: random)
%% - `timeout`: Milliseconds before timeout (default: 5000)
%% - `parallel`: Enable parallel test execution (default: false)
%%
-spec run_property(catena_property:property(), proplist | map | run_options()) -> run_result().
run_property(Property, Options) when is_list(Options); is_map(Options) ->
    RunOpts = normalize_options(Options),
    run_property_with_opts(Property, RunOpts);
run_property(Property, #run_options{} = RunOpts) ->
    run_property_with_opts(Property, RunOpts).

%% @private Run property with normalized options.
-spec run_property_with_opts(catena_property:property(), run_options()) -> run_result().
run_property_with_opts(_Property, RunOpts) ->
    %% Run options take precedence over property config
    NumTests = RunOpts#run_options.num_tests,
    _MaxShrinks = RunOpts#run_options.max_shrinks,
    Seed = RunOpts#run_options.seed,

    %% Run the test loop
    case run_test_loop(_Property, NumTests, 0, 0, Seed, 1, []) of
        {ok, TestsRun, Discards, SeedUsed} ->
            Result = #property_result{
                kind = success,
                tests_run = TestsRun,
                tests_discarded = Discards,
                shrinks_attempted = 0,
                seed = SeedUsed,
                original_counterexample = undefined,
                shrunk_counterexample = undefined,
                shrink_history = [],
                labels = [],
                output = <<>>
            },
            {passed, Result};
        {failure, Counterexample, TestsRun, Discards, Shrinks, SeedUsed, ShrinkHistory} ->
            Result = #property_result{
                kind = failure,
                tests_run = TestsRun,
                tests_discarded = Discards,
                shrinks_attempted = Shrinks,
                seed = SeedUsed,
                original_counterexample = Counterexample,
                shrunk_counterexample = Counterexample,
                shrink_history = ShrinkHistory,
                labels = [],
                output = <<>>
            },
            {failed, Result};
        {error, Reason, TestsRun, SeedUsed} ->
            Result = #property_result{
                kind = error,
                tests_run = TestsRun,
                tests_discarded = 0,
                shrinks_attempted = 0,
                seed = SeedUsed,
                original_counterexample = Reason,
                shrunk_counterexample = Reason,
                shrink_history = [],
                labels = [],
                output = <<>>
            },
            {failed, Result};
        {discarded, TestsRun, Discards, SeedUsed} ->
            Result = #property_result{
                kind = discarded,
                tests_run = TestsRun,
                tests_discarded = Discards,
                shrinks_attempted = 0,
                seed = SeedUsed,
                original_counterexample = undefined,
                shrunk_counterexample = undefined,
                shrink_history = [],
                labels = [],
                output = <<>>
            },
            {failed, Result}
    end.

%% @private Main test execution loop.
-spec run_test_loop(
    catena_property:property(),
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer(),
    catena_gen:seed(),
    pos_integer(),
    [term()]
) -> {ok, non_neg_integer(), non_neg_integer(), catena_gen:seed()} |
    {failure, term(), non_neg_integer(), non_neg_integer(), non_neg_integer(), catena_gen:seed(), [term()]} |
    {error, term(), non_neg_integer(), catena_gen:seed()} |
    {discarded, non_neg_integer(), non_neg_integer(), catena_gen:seed()}.
run_test_loop(_Property, TargetTests, TestsRun, Discards, Seed, _Size, _History)
        when TestsRun >= TargetTests ->
    %% All tests passed
    {ok, TestsRun, Discards, Seed};
run_test_loop(_Property, TargetTests, TestsRun, Discards, Seed, _Size, _History)
        when Discards > (TargetTests * 10) ->
    %% Too many discards - likely problematic property
    {discarded, TestsRun, Discards, Seed};
run_test_loop(Property, TargetTests, TestsRun, Discards, Seed, Size, History) ->
    %% Calculate size for this test (0 to 100, then hold at 100)
    CurrentSize = min(Size, 100),

    %% Run a single test case
    case run_test_case(Property, CurrentSize, Seed) of
        {pass, NewSeed} ->
            run_test_loop(Property, TargetTests, TestsRun + 1, Discards, NewSeed, Size + 1, History);
        {fail, Counterexample, NewSeed} ->
            {failure, Counterexample, TestsRun + 1, Discards, 0, NewSeed, lists:reverse([Counterexample | History])};
        {discard, NewSeed} ->
            run_test_loop(Property, TargetTests, TestsRun, Discards + 1, NewSeed, Size + 1, History);
        {error, Reason, NewSeed} ->
            {error, Reason, TestsRun + 1, NewSeed}
    end.

%% @doc Run a single test case with given size and seed.
%%
%% Generates a value using the property's generator and checks the predicate.
%% Returns the result and a new seed for the next test.
%%
-spec run_test_case(catena_property:property(), pos_integer(), catena_gen:seed()) ->
    {pass, catena_gen:seed()} |
    {fail, term(), catena_gen:seed()} |
    {discard, catena_gen:seed()} |
    {error, term(), catena_gen:seed()}.
run_test_case(Property, Size, Seed) ->
    Generator = Property#property.generator,
    Predicate = Property#property.predicate,

    %% Split seed for generator and next test
    {GenSeed, NextSeed} = catena_gen:seed_split(Seed),

    try
        %% Generate a value
        Tree = catena_gen:run(Generator, Size, GenSeed),
        Value = catena_tree:root(Tree),

        %% Check the predicate
        case apply_predicate(Predicate, Value) of
            true ->
                {pass, NextSeed};
            {discard, _} ->
                {discard, NextSeed};
            false ->
                {fail, Value, NextSeed}
        end
    catch
        Kind:Reason:Stack ->
            {error, {Kind, Reason, Stack}, NextSeed}
    end.

%% @private Apply predicate to value, handling discard markers.
-spec apply_predicate(fun((_) -> boolean()), term()) -> boolean() | {discard, term()}.
apply_predicate(Predicate, Value) ->
    try Predicate(Value) of
        discard -> {discard, Value};
        Result -> Result
    catch
        _:_:_ -> false
    end.

%%====================================================================
%% Section 3.2.3: Test Orchestration
%%====================================================================

%% @doc Run a list of properties and aggregate results.
%%
%% Runs properties sequentially and returns a summary with counts.
%%
-spec run_properties([catena_property:property()]) -> batch_result().
run_properties(Properties) ->
    run_properties(Properties, #run_options{
        num_tests = 100,
        max_shrinks = 1000,
        seed = catena_gen:seed_new(),
        timeout = 5000,
        parallel = false
    }).

%% @doc Run a list of properties with custom configuration.
%%
%% Options are the same as for `run_property/2`.
%%
-spec run_properties([catena_property:property()], proplist | map | run_options()) -> batch_result().
run_properties(Properties, Options) when is_list(Options); is_map(Options) ->
    RunOpts = normalize_options(Options),
    run_properties_with_opts(Properties, RunOpts);
run_properties(Properties, #run_options{} = RunOpts) ->
    run_properties_with_opts(Properties, RunOpts).

%% @private Run properties with normalized options.
-spec run_properties_with_opts([catena_property:property()], run_options()) -> batch_result().
run_properties_with_opts(Properties, RunOpts) ->
    Results = [{P#property.name, run_property(P, RunOpts)} || P <- Properties],

    Passed = length([R || {_, {passed, _R}} = R <- Results]),
    Failed = length([R || {_, {failed, _R}} = R <- Results]),
    Total = length(Results),

    #{
        total => Total,
        passed => Passed,
        failed => Failed,
        errors => 0,
        results => Results
    }.

%%====================================================================
%% Section 3.2.4: Integration with Test Framework
%%====================================================================

%% @doc Create an EUnit-compatible test from a property.
%%
%% Returns a function that runs the property and asserts it passes.
%% Use with EUnit's test generation:
%%
%% ```
%% prop_reverse_test() ->
%%     catena_runner:property_test("reverse_id", fun() ->
%%         catena_property:forall(
%%             catena_gen:gen_list(catena_gen:gen_int()),
%%             fun(L) -> lists:reverse(lists:reverse(L)) =:= L end
%%         )
%%     end).
%% ```
%%
-spec property_test(binary(), fun(() -> {forall, _, _})) -> fun(() -> ok | no_return()).
property_test(Name, BodyFun) ->
    fun() ->
        Prop = catena_property:property(Name, BodyFun),
        case run_property(Prop) of
            {passed, _Result} -> ok;
            {failed, Result} ->
                erlang:error({property_failed, Name, Result})
        end
    end.

%% @doc EUnit wrapper for property tests.
%%
%% Alternative integration that returns EUnit test result directly.
%%
-spec eunit_wrapper(binary(), fun(() -> {forall, _, _})) -> ok.
eunit_wrapper(Name, BodyFun) ->
    Prop = catena_property:property(Name, BodyFun),
    case run_property(Prop) of
        {passed, _Result} -> ok;
        {failed, Result} ->
            ?debugFmt("Property ~s failed: ~p~n", [Name, Result]),
            throw({property_failed, Name})
    end.

%%====================================================================
%% Internal Helpers
%%====================================================================

%% @private Normalize options from various formats to run_options record.
-spec normalize_options(proplist | map | run_options()) -> run_options().
normalize_options(Options) when is_list(Options) ->
    normalize_options(maps:from_list(Options));
normalize_options(Options) when is_map(Options) ->
    #run_options{
        num_tests = maps:get(num_tests, Options, 100),
        max_shrinks = maps:get(max_shrinks, Options, 1000),
        seed = maps:get(seed, Options, catena_gen:seed_new()),
        timeout = maps:get(timeout, Options, 5000),
        parallel = maps:get(parallel, Options, false)
    }.

%%====================================================================
%% Unit Tests - Section 3.2
%%====================================================================

%% Note: These are basic validation tests. Full integration tests
%% are in catena_runner_tests.erl

normalize_options_defaults_test() ->
    Opts = normalize_options([]),
    ?assertEqual(100, Opts#run_options.num_tests),
    ?assertEqual(1000, Opts#run_options.max_shrinks),
    ?assertEqual(5000, Opts#run_options.timeout),
    ?assertEqual(false, Opts#run_options.parallel),
    ok.

normalize_options_proplist_test() ->
    Opts = normalize_options([{num_tests, 50}, {parallel, true}]),
    ?assertEqual(50, Opts#run_options.num_tests),
    ?assertEqual(true, Opts#run_options.parallel),
    ok.

normalize_options_map_test() ->
    Opts = normalize_options(#{num_tests => 75}),
    ?assertEqual(75, Opts#run_options.num_tests),
    ok.

run_property_passing_test() ->
    Prop = catena_property:property("always_pass", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    {passed, _Result} = run_property(Prop, #{num_tests => 10}),
    ok.

run_property_failing_test() ->
    Prop = catena_property:property("always_fail", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> false end)
    end),
    {failed, _Result} = run_property(Prop, #{num_tests => 10}),
    ok.

run_properties_aggregates_results_test() ->
    Prop1 = catena_property:property("pass", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    Prop2 = catena_property:property("fail", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> false end)
    end),
    Result = run_properties([Prop1, Prop2], #{num_tests => 5}),
    ?assertEqual(2, maps:get(total, Result)),
    ?assertEqual(1, maps:get(passed, Result)),
    ?assertEqual(1, maps:get(failed, Result)),
    ok.

apply_predicate_true_test() ->
    Result = apply_predicate(fun(_) -> true end, some_value),
    ?assertEqual(true, Result),
    ok.

apply_predicate_false_test() ->
    Result = apply_predicate(fun(_) -> false end, some_value),
    ?assertEqual(false, Result),
    ok.

apply_predicate_discard_test() ->
    Result = apply_predicate(fun(_) -> discard end, some_value),
    ?assertEqual({discard, some_value}, Result),
    ok.

apply_predicate_error_test() ->
    Result = apply_predicate(fun(_) -> error(bad) end, some_value),
    ?assertEqual(false, Result),
    ok.
