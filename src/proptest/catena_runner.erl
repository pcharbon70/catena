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
run_property_with_opts(Property, RunOpts) ->
    %% Run options take precedence over property config
    NumTests = RunOpts#run_options.num_tests,
    MaxShrinks = RunOpts#run_options.max_shrinks,
    Seed = RunOpts#run_options.seed,

    %% Run the test loop
    case run_test_loop(Property, NumTests, 0, 0, Seed, 1, [], #{}) of
        {ok, TestsRun, Discards, SeedUsed, LabelCounts} ->
            Result = #property_result{
                kind = success,
                tests_run = TestsRun,
                tests_discarded = Discards,
                shrinks_attempted = 0,
                seed = SeedUsed,
                original_counterexample = undefined,
                shrunk_counterexample = undefined,
                shrink_history = [],
                labels = property_result_labels(LabelCounts),
                output = property_result_output(Discards, LabelCounts)
            },
            {passed, Result};
        {failure, Counterexample, Tree, TestsRun, Discards, SeedUsed, LabelCounts} ->
            %% Attempt to shrink the counterexample
            case shrink_counterexample(Property, Counterexample, Tree, MaxShrinks) of
                {found, Shrunk, ShrinkHistory} ->
                    Result = #property_result{
                        kind = failure,
                        tests_run = TestsRun,
                        tests_discarded = Discards,
                        shrinks_attempted = length(ShrinkHistory),
                        seed = SeedUsed,
                        original_counterexample = Counterexample,
                        shrunk_counterexample = Shrunk,
                        shrink_history = ShrinkHistory,
                        labels = property_result_labels(LabelCounts),
                        output = property_result_output(Discards, LabelCounts)
                    },
                    {failed, Result};
                {no_shrink, _} ->
                    Result = #property_result{
                        kind = failure,
                        tests_run = TestsRun,
                        tests_discarded = Discards,
                        shrinks_attempted = 0,
                        seed = SeedUsed,
                        original_counterexample = Counterexample,
                        shrunk_counterexample = Counterexample,
                        shrink_history = [],
                        labels = property_result_labels(LabelCounts),
                        output = property_result_output(Discards, LabelCounts)
                    },
                    {failed, Result}
            end;
        {error, Reason, TestsRun, Discards, SeedUsed, LabelCounts} ->
            Result = #property_result{
                kind = error,
                tests_run = TestsRun,
                tests_discarded = Discards,
                shrinks_attempted = 0,
                seed = SeedUsed,
                original_counterexample = Reason,
                shrunk_counterexample = Reason,
                shrink_history = [],
                labels = property_result_labels(LabelCounts),
                output = property_result_output(Discards, LabelCounts)
            },
            {failed, Result};
        {discarded, TestsRun, Discards, SeedUsed, LabelCounts} ->
            Result = #property_result{
                kind = discarded,
                tests_run = TestsRun,
                tests_discarded = Discards,
                shrinks_attempted = 0,
                seed = SeedUsed,
                original_counterexample = undefined,
                shrunk_counterexample = undefined,
                shrink_history = [],
                labels = property_result_labels(LabelCounts),
                output = property_result_output(Discards, LabelCounts)
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
    [term()],
    map()
) -> {ok, non_neg_integer(), non_neg_integer(), catena_gen:seed(), map()} |
    {failure, term(), catena_tree:tree(term()), non_neg_integer(), non_neg_integer(), catena_gen:seed(), map()} |
    {error, term(), non_neg_integer(), non_neg_integer(), catena_gen:seed(), map()} |
    {discarded, non_neg_integer(), non_neg_integer(), catena_gen:seed(), map()}.
run_test_loop(_Property, TargetTests, TestsRun, Discards, Seed, _Size, _History, LabelCounts)
        when TestsRun >= TargetTests ->
    %% All tests passed
    {ok, TestsRun, Discards, Seed, LabelCounts};
run_test_loop(_Property, TargetTests, TestsRun, Discards, Seed, _Size, _History, LabelCounts)
        when Discards > (TargetTests * 10) ->
    %% Too many discards - likely problematic property
    {discarded, TestsRun, Discards, Seed, LabelCounts};
run_test_loop(Property, TargetTests, TestsRun, Discards, Seed, Size, History, LabelCounts) ->
    %% Calculate size for this test (0 to 100, then hold at 100)
    CurrentSize = min(Size, 100),

    %% Run a single test case
    case run_test_case(Property, CurrentSize, Seed) of
        {pass, NewSeed, CaseLabels} ->
            run_test_loop(
                Property,
                TargetTests,
                TestsRun + 1,
                Discards,
                NewSeed,
                Size + 1,
                History,
                add_case_labels(LabelCounts, CaseLabels)
            );
        {fail, Counterexample, Tree, _NewSeed, CaseLabels} ->
            {failure, Counterexample, Tree, TestsRun + 1, Discards, Seed,
                add_case_labels(LabelCounts, CaseLabels)};
        {discard, NewSeed} ->
            run_test_loop(Property, TargetTests, TestsRun, Discards + 1, NewSeed, Size + 1, History, LabelCounts);
        {error, Reason, NewSeed, CaseLabels} ->
            {error, Reason, TestsRun + 1, Discards, NewSeed, add_case_labels(LabelCounts, CaseLabels)}
    end.

%% @doc Run a single test case with given size and seed.
%%
%% Generates a value using the property's generator and checks the predicate.
%% Returns the result and a new seed for the next test.
%%
-spec run_test_case(catena_property:property(), pos_integer(), catena_gen:seed()) ->
    {pass, catena_gen:seed(), [binary()]} |
    {fail, term(), catena_tree:tree(term()), catena_gen:seed(), [binary()]} |
    {discard, catena_gen:seed()} |
    {error, term(), catena_gen:seed(), [binary()]}.
run_test_case(Property, Size, Seed) ->
    Generator = Property#property.generator,
    Predicate = Property#property.predicate,

    %% Split seed for generator and next test
    {GenSeed, NextSeed} = catena_gen:seed_split(Seed),

    try
        %% Generate a value
        Tree = catena_gen:run(Generator, Size, GenSeed),
        Value = catena_tree:root(Tree),
        CaseLabels = property_case_labels(Property, Value),

        %% Check the predicate
        case apply_predicate(Predicate, Value) of
            true ->
                {pass, NextSeed, CaseLabels};
            {discard, _} ->
                {discard, NextSeed};
            false ->
                {fail, Value, Tree, NextSeed, CaseLabels}
        end
    catch
        Kind:Reason:Stack ->
            {error, {Kind, Reason, Stack}, NextSeed, []}
    end.

%% @private Shrink a counterexample to find minimal failing value.
-spec shrink_counterexample(catena_property:property(), term(), catena_tree:tree(term()), non_neg_integer()) ->
    {found, term(), [term()]} | {no_shrink, term()}.
shrink_counterexample(Property, Counterexample, Tree, MaxShrinks) ->
    Predicate = Property#property.predicate,
    %% Use catena_shrink to find minimal failure
    case catena_shrink:find_minimal(Predicate, Tree, #{max_attempts => MaxShrinks}) of
        {found, MinValue, Path} ->
            {found, MinValue, lists:reverse(Path)};
        {no_shrink, _} ->
            {no_shrink, Counterexample}
    end.

%% @private Apply predicate to value, handling discard markers.
%%
%% Supports both single-argument predicates (fun(A) -> ...) and
%% multi-argument predicates where Value is a tuple (fun(A, B, C) -> ...).
%% For multi-argument predicates, the tuple elements are applied as arguments.
%% Lists are always passed as single arguments (for list generators).
-spec apply_predicate(fun(), term()) -> boolean() | {discard, term()}.
apply_predicate(Predicate, Value) ->
    try
        {arity, Arity} = erlang:fun_info(Predicate, arity),
        case Arity of
            1 ->
                %% Single argument - apply directly (handles lists and other values)
                apply_predicate_result(Predicate(Value), Value);
            _ when is_tuple(Value) ->
                %% Multi-argument predicate - apply tuple elements as arguments
                Args = tuple_to_list(Value),
                case length(Args) of
                    Arity ->
                        apply_predicate_result(apply(Predicate, Args), Value);
                    _ ->
                        %% Arity mismatch - error
                        false
                end;
            _ ->
                %% Predicate expects multiple args but Value isn't a tuple
                %% (e.g., Value is a list but Arity > 1)
                %% Try to apply anyway - will fail if wrong arity
                apply_predicate_result(Predicate(Value), Value)
        end
    catch
        error:undef ->
            %% Function clause mismatch - predicate doesn't accept this value
            false;
        _:_:_ ->
            false
    end.

%% @private Helper to handle predicate results including discard markers.
-spec apply_predicate_result(boolean() | discard, term()) -> boolean() | {discard, term()}.
apply_predicate_result(discard, Value) -> {discard, Value};
apply_predicate_result(Result, _Value) -> Result.

add_case_labels(LabelCounts, []) ->
    LabelCounts;
add_case_labels(LabelCounts, [Label | Rest]) ->
    Updated = maps:update_with(Label, fun(Count) -> Count + 1 end, 1, LabelCounts),
    add_case_labels(Updated, Rest).

property_case_labels(Property, Value) ->
    Config = Property#property.config,
    StaticLabels = [normalize_label(Label) || Label <- Config#property_config.labels],
    StaticLabels ++ classification_labels(Config#property_config.classify_fun, Value).

classification_labels(undefined, _Value) ->
    [];
classification_labels({Prefix, ClassifyFun}, Value) when is_function(ClassifyFun, 1) ->
    try
        [classified_label(Prefix, ClassifyFun(Value))]
    catch
        _:_ ->
            []
    end;
classification_labels(_Other, _Value) ->
    [].

classified_label(Prefix, Classification) ->
    PrefixBin = normalize_label(Prefix),
    ClassificationBin = normalize_label(Classification),
    <<PrefixBin/binary, ":", ClassificationBin/binary>>.

normalize_label(Label) when is_binary(Label) ->
    Label;
normalize_label(Label) when is_atom(Label) ->
    atom_to_binary(Label, utf8);
normalize_label(Label) when is_list(Label) ->
    unicode:characters_to_binary(Label);
normalize_label(Label) ->
    unicode:characters_to_binary(io_lib:format("~p", [Label])).

property_result_labels(LabelCounts) when map_size(LabelCounts) =:= 0 ->
    [];
property_result_labels(LabelCounts) ->
    maps:from_list(lists:sort(maps:to_list(LabelCounts))).

property_result_output(Discards, LabelCounts) ->
    Parts0 = case Discards of
        0 -> [];
        _ -> [lists:flatten(io_lib:format("discards=~B", [Discards]))]
    end,
    Parts1 = case format_label_counts(LabelCounts) of
        "" -> Parts0;
        Labels -> Parts0 ++ ["labels=" ++ Labels]
    end,
    case Parts1 of
        [] -> <<>>;
        _ -> unicode:characters_to_binary(string:join(Parts1, "; "))
    end.

format_label_counts(LabelCounts) when map_size(LabelCounts) =:= 0 ->
    "";
format_label_counts(LabelCounts) ->
    Pairs = lists:sort(maps:to_list(LabelCounts)),
    string:join(
        [
            lists:flatten(io_lib:format("~s=~B", [binary_to_list(Label), Count]))
         || {Label, Count} <- Pairs
        ],
        ", "
    ).

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
run_properties_with_opts(Properties, #run_options{parallel = true} = RunOpts) ->
    Results = run_properties_parallel(Properties, RunOpts),
    build_batch_result(Results);
run_properties_with_opts(Properties, RunOpts) ->
    Results = [{P#property.name, run_property(P, RunOpts)} || P <- Properties],
    build_batch_result(Results).

build_batch_result(Results) ->
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

run_properties_parallel(Properties, RunOpts) ->
    Parent = self(),
    Pending = maps:from_list([
        begin
            Name = Property#property.name,
            {Pid, Ref} = spawn_monitor(fun() ->
                Result = run_property(Property, RunOpts#run_options{parallel = false}),
                Parent ! {property_result, self(), Index, Name, Result}
            end),
            _ = Ref,
            {Pid, {Index, Name}}
        end
        || {Index, Property} <- lists:zip(lists:seq(1, length(Properties)), Properties)
    ]),
    Collected = gather_parallel_results(Pending, [], RunOpts#run_options.timeout),
    [
        {Name, Result}
        || {_Index, Name, Result} <- lists:sort(fun({IndexA, _, _}, {IndexB, _, _}) ->
            IndexA =< IndexB
        end, Collected)
    ].

gather_parallel_results(Pending, Acc, _Timeout) when map_size(Pending) =:= 0 ->
    Acc;
gather_parallel_results(Pending, Acc, Timeout) ->
    receive
        {property_result, Pid, Index, Name, Result} ->
            gather_parallel_results(
                maps:remove(Pid, Pending),
                [{Index, Name, Result} | Acc],
                Timeout
            );
        {'DOWN', _Ref, process, Pid, Reason} ->
            case maps:take(Pid, Pending) of
                {{Index, Name}, Remaining} ->
                    gather_parallel_results(
                        Remaining,
                        [{Index, Name, parallel_crash_result(Reason)} | Acc],
                        Timeout
                    );
                error ->
                    gather_parallel_results(Pending, Acc, Timeout)
            end
    after Timeout ->
        TimeoutResults = [
            {Index, Name, timeout_result(Timeout)}
            || {_Pid, {Index, Name}} <- maps:to_list(Pending)
        ],
        Acc ++ TimeoutResults
    end.

parallel_crash_result(Reason) ->
    {failed, #property_result{
        kind = error,
        tests_run = 0,
        tests_discarded = 0,
        shrinks_attempted = 0,
        seed = undefined,
        original_counterexample = {parallel_worker_crashed, Reason},
        shrunk_counterexample = {parallel_worker_crashed, Reason},
        shrink_history = [],
        labels = [],
        output = <<"parallel worker crashed">>
    }}.

timeout_result(Timeout) ->
    {failed, #property_result{
        kind = error,
        tests_run = 0,
        tests_discarded = 0,
        shrinks_attempted = 0,
        seed = undefined,
        original_counterexample = {parallel_timeout, Timeout},
        shrunk_counterexample = {parallel_timeout, Timeout},
        shrink_history = [],
        labels = [],
        output = <<"parallel execution timed out">>
    }}.

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
