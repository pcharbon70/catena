%% @doc Law Testing Framework - Integration (Section 4.5)
%%
%% This module integrates law testing with the broader test ecosystem:
%% CI pipelines, test runners, REPL workflows, and documentation generation.
%%
%% == CI Integration ==
%%
%% Supports deterministic seeds for reproducible CI runs and generates
%% JUnit XML output for CI systems.
%%
%% @see catena_law_tests for test generation
-module(catena_law_integration).

-include("catena_laws.hrl").
-include("catena_property.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API Exports
%%====================================================================

-export([
    run_with_seed/3,
    run_ci_batch/2,
    check_laws/2,
    generate_report/1,
    compliance_badge/1,
    to_junit/2
]).

%%====================================================================
%% Types
%%====================================================================

-type ci_options() :: #{
    seed => non_neg_integer() | undefined,
    num_tests => pos_integer(),
    output_format => json | junit | text,
    verbose => boolean()
}.

-type ci_result() :: #{
    module := atom() | binary(),
    total := non_neg_integer(),
    passed := non_neg_integer(),
    failed := non_neg_integer(),
    duration := non_neg_integer(),
    results := [map()]
}.

%%====================================================================
%% Section 4.5.1: Test Runner Integration
%%====================================================================

%% @doc Run law tests with a specific seed for reproducibility.
%%
%% Takes a module/type name, config, and seed value. Returns
%% detailed results with the seed used for reproducibility.
%%
-spec run_with_seed(atom() | binary(), map(), non_neg_integer()) -> ci_result().
run_with_seed(ModuleName, Config, Seed) ->
    StartTime = erlang:monotonic_time(millisecond),
    %% Merge options properly with the existing options
    ExistingOptions = maps:get(options, Config, #{}),
    NewOptions = maps:merge(ExistingOptions, #{
        seed => Seed,
        num_tests => maps:get(num_tests, ExistingOptions, 100)
    }),
    ConfigWithSeed = Config#{options => NewOptions},
    Result = catena_law_tests:run_law_tests(ModuleName, ConfigWithSeed),
    EndTime = erlang:monotonic_time(millisecond),

    #{
        module => ModuleName,
        total => maps:get(total, Result),
        passed => maps:get(passed, Result),
        failed => maps:get(failed, Result),
        duration => EndTime - StartTime,
        seed => Seed,
        results => maps:get(results, Result)
    }.

%% @doc Run a batch of law test suites for CI.
%%
%% Takes a list of test configurations and runs them all, returning
%% a combined result.
%%
-spec run_ci_batch([{atom() | binary(), map()}], ci_options()) -> [ci_result()].
run_ci_batch(Tests, Options) ->
    Seed = maps:get(seed, Options),
    lists:map(fun({ModuleName, Config}) ->
        run_with_seed(ModuleName, Config, Seed)
    end, Tests).

%%====================================================================
%% Section 4.5.2: CI Integration
%%====================================================================

%% @doc Generate JUnit XML output for CI systems.
%%
%% Returns a binary containing JUnit XML format test results.
%%
-spec to_junit([ci_result()], binary()) -> binary().
to_junit(Results, SuiteName) ->
    Tests = lists:map(fun(R) ->
        ModName = case maps:get(module, R, unknown) of
            Bin when is_binary(Bin) -> Bin;
            Atom when is_atom(Atom) -> atom_to_binary(Atom)
        end,
        #{
            <<"name">> => ModName,
            <<"tests">> => maps:get(total, R),
            <<"failures">> => maps:get(failed, R),
            <<"errors">> => 0,
            <<"time">> => maps:get(duration, R) / 1000
        }
    end, Results),

    TotalTests = lists:sum([maps:get(total, R) || R <- Results]),
    TotalFailures = lists:sum([maps:get(failed, R) || R <- Results]),
    TotalDuration = lists:sum([maps:get(duration, R) || R <- Results]) / 1000,

    Xml = [
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n">>,
        <<"<testsuites name=\"">>, SuiteName,
        <<"\" tests=\"">>, (integer_to_binary(TotalTests)),
        <<"\" failures=\"">>, (integer_to_binary(TotalFailures)),
        <<"\" time=\"">>, (format_float("~.2f", [TotalDuration])),
        <<"\">\n">>,
        [format_test_suite(R) || R <- Results],
        <<"</testsuites>\n">>
    ],
    iolist_to_binary(Xml).

%% @private Format a single test suite for JUnit XML.
-spec format_test_suite(ci_result()) -> iolist().
format_test_suite(Result) ->
    ModuleName = case maps:get(module, Result) of
        Bin when is_binary(Bin) -> Bin;
        Atom when is_atom(Atom) -> atom_to_binary(Atom)
    end,
    [
        <<"  <testsuite name=\"">>, ModuleName,
        <<"\" tests=\"">>, (integer_to_binary(maps:get(total, Result))),
        <<"\" failures=\"">>, (integer_to_binary(maps:get(failed, Result))),
        <<"\" time=\"">>, (format_float("~.2f", [maps:get(duration, Result) / 1000])),
        <<"\">\n">>,
        format_test_cases(Result),
        <<"  </testsuite>\n">>
    ].

%% @private Format test cases for JUnit XML.
-spec format_test_cases(ci_result()) -> iolist().
format_test_cases(Result) ->
    Results = maps:get(results, Result),
    lists:map(fun({Name, {Status, _PropertyResult}}) ->
        StatusAttr = case Status of
            passed -> <<"passed=\"true\"">>;
            _ -> <<"failure=\"Law violation\"">>
        end,
        [
            <<"    <testcase name=\"">>, Name,
            <<"\" ">>, StatusAttr,
            <<" />\n">>
        ]
    end, Results).

%% @private Format a float value.
-spec format_float(string(), [float()]) -> iolist().
format_float(Format, [Value]) ->
    io_lib:format(Format, [Value]).

%%====================================================================
%% Section 4.5.3: Development Workflow
%%====================================================================

%% @doc Quick law check for REPL usage.
%%
%% Runs a small number of tests (default 10) for fast iteration.
%%
-spec check_laws(atom() | binary(), map()) -> ok | {error, map()}.
check_laws(ModuleName, Config) ->
    QuickConfig = maps:merge(Config, #{options => #{
        num_tests => 10
    }}),
    Result = catena_law_tests:run_law_tests(ModuleName, QuickConfig),
    case maps:get(failed, Result) of
        0 -> ok;
        _ ->
            {error, #{
                failed => maps:get(failed, Result),
                results => maps:get(results, Result)
            }}
    end.

%% @doc Generate a law compliance report for a module.
%%
%% Returns a formatted binary with test results and compliance status.
%%
-spec generate_report(ci_result()) -> binary().
generate_report(Result) ->
    ModuleName = maps:get(module, Result),
    Total = maps:get(total, Result),
    Passed = maps:get(passed, Result),
    Failed = maps:get(failed, Result),
    Duration = maps:get(duration, Result),

    Status = case Failed of
        0 -> <<"PASSED">>;
        _ -> <<"FAILED">>
    end,

    Report = [
        <<"========================================\n">>,
        <<"Law Compliance Report: ">>, format_name(ModuleName), <<"\n">>,
        <<"========================================\n">>,
        <<"\n">>,
        <<"Status:   ">>, Status, <<"\n">>,
        <<"Total:    ">>, (integer_to_binary(Total)), <<" tests\n">>,
        <<"Passed:   ">>, (integer_to_binary(Passed)), <<"\n">>,
        <<"Failed:   ">>, (integer_to_binary(Failed)), <<"\n">>,
        <<"Duration: ">>, (integer_to_binary(Duration)), <<" ms\n">>,
        <<"\n">>,
        format_result_details(Result),
        <<"========================================\n">>
    ],
    iolist_to_binary(Report).

%% @private Format module name.
-spec format_name(atom() | binary()) -> binary().
format_name(Name) when is_atom(Name) -> atom_to_binary(Name);
format_name(Name) when is_binary(Name) -> Name.

%% @private Format detailed results.
-spec format_result_details(ci_result()) -> iolist().
format_result_details(Result) ->
    Results = maps:get(results, Result),
    lists:map(fun({Name, {Status, _PropertyResult}}) ->
        StatusStr = case Status of
            passed -> <<"  ✓ ">>;
            _ -> <<"  ✗ ">>
        end,
        [StatusStr, Name, <<"\n">>]
    end, Results).

%%====================================================================
%% Section 4.5.4: Documentation Generation
%%====================================================================

%% @doc Generate a badge for law compliance status.
%%
%% Returns a simple text badge indicating whether all laws passed.
%%
-spec compliance_badge(ci_result()) -> binary().
compliance_badge(Result) ->
    case maps:get(failed, Result) of
        0 -> <<"✅ All laws verified">>;
        N when N > 0 ->
            iolist_to_binary(io_lib:format(<<"⚠️ ~p laws violated">>, [N]))
    end.

%%====================================================================
%% Unit Tests - Section 4.5
%%====================================================================

run_with_seed_reproducible_test() ->
    Adapter = #{
        map => fun(F, X) -> lists:map(F, X) end,
        pure => fun(X) -> [X] end,
        equals => fun(A, B) -> A =:= B end
    },
    Config = #{
        generator => fun() -> catena_stdgen:gen_list(catena_gen:gen_int()) end,
        adapter => Adapter,
        traits => [functor]
    },
    R1 = run_with_seed(<<"test">>, Config, 42),
    R2 = run_with_seed(<<"test">>, Config, 42),
    ?assertEqual(maps:get(passed, R1), maps:get(passed, R2)),
    ok.

run_ci_batch_returns_results_test() ->
    Adapter = #{
        map => fun(F, X) -> lists:map(F, X) end,
        equals => fun(A, B) -> A =:= B end
    },
    Config = #{
        generator => fun() -> catena_stdgen:gen_list(catena_gen:gen_int()) end,
        adapter => Adapter,
        traits => [functor]
    },
    Results = run_ci_batch([{<<"t1">>, Config}], #{seed => 42}),
    ?assert(length(Results) >= 1),
    ok.

to_junit_valid_xml_test() ->
    Result = #{
        module => <<"test_module">>,
        total => 10,
        passed => 8,
        failed => 2,
        duration => 100,
        results => [
            {<<"law1">>, {passed, #property_result{}}},
            {<<"law2">>, {failed, #property_result{}}}
        ]
    },
    Xml = to_junit([Result], <<"test_suite">>),
    ?assert(is_binary(Xml)),
    ?assert(binary:match(Xml, <<"<?xml">>) =/= nomatch),
    ?assert(binary:match(Xml, <<"<testsuites">>) =/= nomatch),
    ok.

check_laws_passing_test() ->
    Adapter = #{
        map => fun(F, X) -> lists:map(F, X) end,
        equals => fun(A, B) -> A =:= B end
    },
    Config = #{
        generator => fun() -> catena_stdgen:gen_list(catena_gen:gen_int()) end,
        adapter => Adapter,
        traits => [functor]
    },
    Result = check_laws(<<"test">>, Config),
    ?assertEqual(ok, Result),
    ok.

generate_report_includes_status_test() ->
    Result = #{
        module => <<"test">>,
        total => 10,
        passed => 10,
        failed => 0,
        duration => 50,
        results => []
    },
    Report = generate_report(Result),
    ?assert(is_binary(Report)),
    ?assert(binary:match(Report, <<"PASSED">>) =/= nomatch),
    ok.

compliance_badge_passed_test() ->
    Result = #{
        module => <<"test">>,
        total => 10,
        passed => 10,
        failed => 0,
        duration => 50,
        results => []
    },
    Badge = compliance_badge(Result),
    ?assert(is_binary(Badge)),
    ok.

compliance_badge_failed_test() ->
    Result = #{
        module => <<"test">>,
        total => 10,
        passed => 8,
        failed => 2,
        duration => 50,
        results => []
    },
    Badge = compliance_badge(Result),
    ?assert(is_binary(Badge)),
    ?assert(binary:match(Badge, <<"violated">>) =/= nomatch),
    ok.
