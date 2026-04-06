%% @doc Unit tests for catena_report (Section 3.3)
%%
%% Tests failure reporting with counterexample display, terminal formatting,
%% and structured output for CI integration.
-module(catena_report_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_report.hrl").
-include("../../src/proptest/catena_property.hrl").
-include("../../src/proptest/catena_gen.hrl").

%%====================================================================
%% Section 3.3.1: Counterexample Display
%%====================================================================

format_integer_test() ->
    NoColorOpts = #format_options{colors = false, max_depth = 10, max_length = 50, truncate = true},
    Formatted = iolist_to_binary(catena_report:format_value(42, NoColorOpts)),
    ?assertEqual(<<"42">>, Formatted),
    ok.

format_atom_test() ->
    NoColorOpts = #format_options{colors = false, max_depth = 10, max_length = 50, truncate = true},
    Formatted = iolist_to_binary(catena_report:format_value(hello, NoColorOpts)),
    ?assertEqual(<<"hello">>, Formatted),
    ok.

format_string_test() ->
    NoColorOpts = #format_options{colors = false, max_depth = 10, max_length = 50, truncate = true},
    Formatted = iolist_to_binary(catena_report:format_value("hello", NoColorOpts)),
    ?assertEqual(<<"\"hello\"">>, Formatted),
    ok.

format_list_test() ->
    NoColorOpts = #format_options{colors = false, max_depth = 10, max_length = 50, truncate = true},
    Formatted = iolist_to_binary(catena_report:format_value([1, 2, 3], NoColorOpts)),
    ?assertEqual(<<"[1, 2, 3]">>, Formatted),
    ok.

format_tuple_test() ->
    NoColorOpts = #format_options{colors = false, max_depth = 10, max_length = 50, truncate = true},
    Formatted = iolist_to_binary(catena_report:format_value({a, b}, NoColorOpts)),
    ?assertEqual(<<"{a, b}">>, Formatted),
    ok.

format_nested_structure_test() ->
    NoColorOpts = #format_options{colors = false, max_depth = 10, max_length = 50, truncate = true},
    Formatted = iolist_to_binary(catena_report:format_value([[1, 2], [3, 4]], NoColorOpts)),
    ?assertEqual(<<"[[1, 2], [3, 4]]">>, Formatted),
    ok.

format_truncates_long_list_test() ->
    LongList = lists:seq(1, 100),
    NoColorOpts = #format_options{colors = false, max_depth = 10, max_length = 50, truncate = true},
    Formatted = iolist_to_binary(catena_report:format_value(LongList, NoColorOpts)),
    ?assert(binary:match(Formatted, <<"...">>) =/= nomatch),
    ok.

%%====================================================================
%% Section 3.3.2: Failure Context
%%====================================================================

format_failure_includes_seed_test() ->
    Result = #property_result{
        kind = failure,
        tests_run = 10,
        tests_discarded = 0,
        shrinks_attempted = 0,
        seed = #seed{state = 42},
        original_counterexample = 123,
        shrunk_counterexample = 123,
        shrink_history = [],
        labels = [],
        output = <<>>
    },
    Formatted = iolist_to_binary(catena_report:format_failure_context(<<"test_prop">>, Result)),
    ?assert(binary:match(Formatted, <<"Seed">>) =/= nomatch),
    ok.

format_failure_shows_counterexample_test() ->
    Result = #property_result{
        kind = failure,
        tests_run = 10,
        tests_discarded = 0,
        shrinks_attempted = 0,
        seed = #seed{state = 42},
        original_counterexample = [1, 2, 3],
        shrunk_counterexample = [1],
        shrink_history = [],
        labels = [],
        output = <<>>
    },
    Formatted = iolist_to_binary(catena_report:format_failure_context(<<"test_prop">>, Result)),
    %% Should contain either "Original counterexample" or "Counterexample"
    ?assert(binary:match(Formatted, <<"counterexample">>) =/= nomatch),
    ok.

%%====================================================================
%% Section 3.3.3: Terminal Output
%%====================================================================

color_red_test() ->
    Formatted = catena_report:color(red, "FAIL"),
    ?assert(is_list(Formatted)),
    ok.

format_summary_test() ->
    Batch = #{
        total => 10,
        passed => 8,
        failed => 2,
        errors => 0,
        results => []
    },
    Formatted = iolist_to_binary(catena_report:format_summary(Batch)),
    ?assert(binary:match(Formatted, <<"Total">>) =/= nomatch),
    ok.

%%====================================================================
%% Section 3.3.4: Structured Output
%%====================================================================

to_json_includes_kind_test() ->
    Result = #property_result{
        kind = failure,
        tests_run = 10,
        tests_discarded = 0,
        shrinks_attempted = 0,
        seed = #seed{state = 42},
        original_counterexample = 123,
        shrunk_counterexample = 123,
        shrink_history = [],
        labels = [],
        output = <<>>
    },
    Json = catena_report:to_json(Result),
    ?assert(binary:match(Json, <<"kind">>) =/= nomatch),
    ?assert(binary:match(Json, <<"failure">>) =/= nomatch),
    ok.

to_junit_valid_xml_test() ->
    Batch = #{
        total => 1,
        passed => 0,
        failed => 1,
        errors => 0,
        results => [{<<"test">>, {failed, #property_result{
            kind = failure,
            tests_run = 1,
            tests_discarded = 0,
            shrinks_attempted = 0,
            seed = #seed{state = 42},
            original_counterexample = 123,
            shrunk_counterexample = 123,
            shrink_history = [],
            labels = [],
            output = <<>>
        }}}]
    },
    Xml = catena_report:to_junit(<<"testsuite">>, Batch),
    ?assertMatch(<<"<?xml", _/binary>>, Xml),
    ok.
