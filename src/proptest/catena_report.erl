%% @doc Property Testing Phase 3, Section 3.3: Failure Reporting
%%
%% This module implements failure reporting with clear counterexample display,
%% terminal formatting, and structured output for CI integration.
%%
%% == Failure Reports ==
%%
%% Failure reports include:
%% - Property name and source location
%% - Original counterexample (before shrinking)
%% - Shrunk counterexample (minimal failing case)
%% - Shrink history showing attempted shrinks
%% - Reproducible seed for re-running
%% - Label distribution statistics
%%
%% @see catena_runner for test execution
%% @see catena_property for property definition
-module(catena_report).

-include_lib("eunit/include/eunit.hrl").
-include("catena_property.hrl").
-include("catena_gen.hrl").
-include("catena_report.hrl").

%% API exports - Section 3.3.1: Counterexample Display
-export([
    format_counterexample/1,
    format_value/1,
    format_value/2
]).

%% API exports - Section 3.3.2: Failure Context
-export([
    format_failure/2,
    format_failure_context/2
]).

%% API exports - Section 3.3.3: Terminal Output Formatting
-export([
    format_result/1,
    format_summary/1,
    color/2,
    ansi/0
]).

%% API exports - Section 3.3.4: Structured Output
-export([
    to_json/1,
    to_junit/2
]).

%% Type exports
-export_type([
    format_options/0,
    output_format/0
]).

%%====================================================================
%% Types
%%====================================================================

-type format_options() :: #format_options{}.

%% Output format types.
-type output_format() :: text | json | junit.

%% Color names for terminal output.
-type color_name() :: reset | bold | grey | red | bold_red | green | bold_green | yellow | cyan | magenta | blue.

%%====================================================================
%% Section 3.3.1: Counterexample Display
%%====================================================================

%% @doc Format a counterexample value for display.
%%
%% Uses default formatting options with colors enabled.
%%
-spec format_counterexample(term()) -> iolist().
format_counterexample(Value) ->
    format_counterexample(Value, default_options()).

%% @doc Format a counterexample with custom options.
%%
%% Options control:
%% - `colors`: Enable ANSI color codes (default: true)
%% - `max_depth`: Maximum nesting depth (default: 10)
%% - `max_length`: Maximum list length before truncating (default: 50)
%% - `truncate`: Enable truncation of large values (default: true)
%%
-spec format_counterexample(term(), format_options()) -> iolist().
format_counterexample(Value, Options) ->
    format_value(Value, 0, Options).

%% @doc Format a value with default options.
%%
-spec format_value(term()) -> iolist().
format_value(Value) ->
    format_value(Value, default_options()).

%% @doc Format a value with custom options.
%%
-spec format_value(term(), format_options()) -> iolist().
format_value(Value, Options) ->
    format_value(Value, 0, Options).

%% @private Format a value with depth tracking.
-spec format_value(term(), non_neg_integer(), format_options()) -> iolist().
format_value(Value, Depth, Options) when Depth > Options#format_options.max_depth ->
    ["...", color_opt(grey, "<<max depth>>", Options), "..."];
format_value(Value, _Depth, Options) when is_integer(Value) ->
    [color_opt(cyan, integer_to_list(Value), Options)];
format_value(Value, _Depth, Options) when is_float(Value) ->
    [color_opt(cyan, float_to_list(Value, [{decimals, 4}, compact]), Options)];
format_value(Value, _Depth, Options) when is_atom(Value) ->
    [color_opt(yellow, atom_to_list(Value), Options)];
format_value(Value, _Depth, Options) when is_binary(Value) ->
    case byte_size(Value) > Options#format_options.max_length of
        true ->
            Truncated = binary:part(Value, 0, Options#format_options.max_length),
            [color_opt(green, ["<<\"", Truncated, "\">>"], Options), color_opt(grey, "...", Options)];
        false ->
            [color_opt(green, ["<<\"", Value, "\">>"], Options)]
    end;
format_value(Value, _Depth, Options) when is_bitstring(Value) ->
    [color_opt(green, ["<<\"", bitstring_to_list(Value), "\">>"], Options)];
format_value(Value, Depth, Options) when is_list(Value) ->
    case io_lib:printable_list(Value) of
        true ->
            case length(Value) > Options#format_options.max_length of
                true ->
                    Truncated = lists:sublist(Value, Options#format_options.max_length),
                    [color_opt(green, ["\"", Truncated, "\""], Options), color_opt(grey, "...", Options)];
                false ->
                    [color_opt(green, ["\"", Value, "\""], Options)]
            end;
        false ->
            format_list(Value, Depth, Options)
    end;
format_value(Value, Depth, Options) when is_tuple(Value) ->
    format_tuple(Value, Depth, Options);
format_value(Value, Depth, Options) when is_map(Value) ->
    format_map(Value, Depth, Options);
format_value(Value, _Depth, Options) when is_pid(Value) ->
    [color_opt(magenta, pid_to_list(Value), Options)];
format_value(Value, _Depth, Options) when is_reference(Value) ->
    [color_opt(magenta, ref_to_list(Value), Options)];
format_value(Value, _Depth, Options) when is_port(Value) ->
    [color_opt(magenta, port_to_list(Value), Options)];
format_value(Value, _Depth, Options) when is_function(Value) ->
    Arity = erlang:fun_info(Value, arity),
    [color_opt(magenta, ["fun/", integer_to_list(element(2, Arity))], Options)];
format_value(Value, _Depth, Options) ->
    [color_opt(magenta, ["?", io_lib:format("~p", [Value])], Options)].

%% @private Format a list.
-spec format_list(list(), non_neg_integer(), format_options()) -> iolist().
format_list([], _Depth, _Options) ->
    ["[", "]"];
format_list(List, Depth, Options) ->
    MaxLen = Options#format_options.max_length,
    case length(List) > MaxLen andalso Options#format_options.truncate of
        true ->
            Truncated = lists:sublist(List, MaxLen),
            ["[",
             format_list_items(Truncated, Depth + 1, Options),
             color_opt(grey, "...", Options),
             "]"];
        false ->
            ["[",
             format_list_items(List, Depth + 1, Options),
             "]"]
    end.

-spec format_list_items(list(), non_neg_integer(), format_options()) -> iolist().
format_list_items([], _Depth, _Options) ->
    [];
format_list_items([Item], Depth, Options) ->
    [format_value(Item, Depth + 1, Options)];
format_list_items([Item | Rest], Depth, Options) ->
    [format_value(Item, Depth + 1, Options),
     ", ",
     format_list_items(Rest, Depth, Options)].

%% @private Format a tuple.
-spec format_tuple(tuple(), non_neg_integer(), format_options()) -> iolist().
format_tuple(Tuple, Depth, Options) ->
    ["{",
     format_tuple_items(Tuple, 1, tuple_size(Tuple), Depth, Options),
     "}"].

-spec format_tuple_items(tuple(), pos_integer(), pos_integer(), non_neg_integer(), format_options()) -> iolist().
format_tuple_items(_Tuple, Pos, Max, _Depth, _Options) when Pos > Max ->
    [];
format_tuple_items(Tuple, Pos, Max, Depth, Options) ->
    Item = element(Pos, Tuple),
    case Pos of
        Max -> [format_value(Item, Depth + 1, Options)];
        _ -> [format_value(Item, Depth + 1, Options), ", ",
              format_tuple_items(Tuple, Pos + 1, Max, Depth, Options)]
    end.

%% @private Format a map.
-spec format_map(map(), non_neg_integer(), format_options()) -> iolist().
format_map(Map, _Depth, _Options) when map_size(Map) =:= 0 ->
    ["#{", "}"];
format_map(Map, Depth, Options) ->
    Items = maps:to_list(Map),
    MaxLen = Options#format_options.max_length,
    Formatted = case length(Items) > MaxLen andalso Options#format_options.truncate of
        true ->
            Truncated = lists:sublist(Items, MaxLen),
            format_map_items(Truncated, Depth + 1, Options) ++ [color_opt(grey, "...", Options)];
        false ->
            format_map_items(Items, Depth + 1, Options)
    end,
    ["#{", Formatted, "}"].

-spec format_map_items([{term(), term()}], non_neg_integer(), format_options()) -> iolist().
format_map_items([], _Depth, _Options) ->
    [];
format_map_items([{K, V}], Depth, Options) ->
    [format_value(K, Depth + 1, Options),
     " => ",
     format_value(V, Depth + 1, Options)];
format_map_items([{K, V} | Rest], Depth, Options) ->
    [format_value(K, Depth + 1, Options),
     " => ",
     format_value(V, Depth + 1, Options),
     ", ",
     format_map_items(Rest, Depth, Options)].

%%====================================================================
%% Section 3.3.2: Failure Context
%%====================================================================

%% @doc Format a complete failure report for a property.
%%
-spec format_failure(binary(), catena_property:property_result()) -> iolist().
format_failure(PropertyName, Result) ->
    format_failure(PropertyName, Result, default_options()).

%% @private Format failure with options.
-spec format_failure(binary(), catena_property:property_result(), format_options()) -> iolist().
format_failure(PropertyName, Result, Options) ->
    ["\n",
     color(red, "*** Property Failed: "),
     color(bold, PropertyName), "\n",
     format_failure_context(PropertyName, Result, Options),
     "\n"].

%% @doc Format the failure context with all relevant information.
%%
-spec format_failure_context(binary(), catena_property:property_result()) -> iolist().
format_failure_context(PropertyName, Result) ->
    format_failure_context(PropertyName, Result, default_options()).

%% @private Format failure context with options.
-spec format_failure_context(binary(), catena_property:property_result(), format_options()) -> iolist().
format_failure_context(_PropertyName, Result, Options) ->
    Counterexample = Result#property_result.shrunk_counterexample,
    Original = Result#property_result.original_counterexample,
    Seed = Result#property_result.seed,
    TestsRun = Result#property_result.tests_run,
    Shrinks = Result#property_result.shrinks_attempted,
    History = Result#property_result.shrink_history,

    ["\n",
     "  Tests run: ", color(cyan, integer_to_list(TestsRun)),
     case Shrinks > 0 of
         true -> ["  Shrinks: ", color(cyan, integer_to_list(Shrinks))];
         false -> []
     end,
     "\n",

     case Original =/= undefined andalso Original =/= Counterexample of
         true ->
             ["\n",
              "  Original counterexample:\n    ",
              format_counterexample(Original, Options),
              "\n",
              "\n",
              "  Shrunk counterexample:\n    ",
              format_counterexample(Counterexample, Options),
              "\n"];
         false when Counterexample =/= undefined ->
             ["\n",
              "  Counterexample:\n    ",
              format_counterexample(Counterexample, Options),
              "\n"];
         false ->
             []
     end,

     case History of
         [] -> [];
         _ ->
             ["\n",
              "  Shrink history (", color(cyan, integer_to_list(length(History))), " steps):\n",
              format_shrink_history(History, 5, Options),
              "\n"]
     end,

     "\n",
     "  Seed for reproduction: ",
     color(yellow, format_seed(Seed)),
     "\n"].

-spec format_shrink_history([term()], non_neg_integer(), format_options()) -> iolist().
format_shrink_history([], _N, _Options) ->
    [];
format_shrink_history(History, N, Options) when length(History) > N ->
    Shown = lists:sublist(History, N),
    [format_shrink_items(Shown, Options),
     color(grey, ["  ... (", integer_to_list(length(History) - N), " more steps)\n"])];
format_shrink_history(History, _N, Options) ->
    format_shrink_items(History, Options).

-spec format_shrink_items([term()], format_options()) -> iolist().
format_shrink_items([], _Options) ->
    [];
format_shrink_items([Item], Options) ->
    ["    ", format_counterexample(Item, Options), "\n"];
format_shrink_items([Item | Rest], Options) ->
    ["    ", format_counterexample(Item, Options), "\n",
     format_shrink_items(Rest, Options)].

%%====================================================================
%% Section 3.3.3: Terminal Output Formatting
%%====================================================================

%% @doc Format a property test result for terminal display.
%%
-spec format_result({binary(), catena_runner:run_result()}) -> iolist().
format_result({Name, {passed, _Result}}) ->
    [color(green, "[OK] "), " ", Name, "\n"];
format_result({Name, {failed, Result}}) ->
    Kind = Result#property_result.kind,
    Label = case Kind of
        failure -> color(red, "[FAILED]");
        discarded -> color(yellow, "[DISCARDED]");
        error -> color(red, "[ERROR]")
    end,
    [Label, " ", Name, "\n",
     format_failure_context(Name, Result, default_options())].

%% @doc Format a summary of batch test results.
%%
-spec format_summary(catena_runner:batch_result()) -> iolist().
format_summary(#{total := Total, passed := Passed, failed := Failed}) ->
    Status = case Failed of
        0 -> color(bold_green, "PASSED");
        _ -> color(bold_red, "FAILED")
    end,
    ["\n",
     "======== ", Status, " ========\n",
     "  Total:   ", color(cyan, integer_to_list(Total)), "\n",
     "  Passed:  ", color(green, integer_to_list(Passed)), "\n",
     "  Failed:  ", color(red, integer_to_list(Failed)), "\n",
     "\n"].

%% @doc Apply color formatting if enabled.
%%
-spec color(color_name(), iolist()) -> iolist().
color(_Name, Text) ->
    case ansi() of
        true -> [ansi_code(_Name), Text, reset_code()];
        false -> Text
    end.

%% @private Apply color formatting based on options.
-spec color_opt(color_name(), iolist(), format_options()) -> iolist().
color_opt(_Name, Text, Options) ->
    case Options#format_options.colors of
        true -> [ansi_code(_Name), Text, reset_code()];
        false -> Text
    end.

%% @doc Check if ANSI colors should be enabled.
%%
%% Returns true if running in a terminal and colors are supported.
%%
-spec ansi() -> boolean().
ansi() ->
    case os:getenv("NO_COLOR") of
        false -> check_terminal();
        _ -> false
    end.

%% @private Check if we're in a terminal.
-spec check_terminal() -> boolean().
check_terminal() ->
    case os:type() of
        {unix, _} ->
            %% Check if stdout is a tty
            case io:format("~s~n", ["\e[0m"]) of
                ok -> true;
                _ -> false
            end;
        _ -> false
    end.

%% @private Get ANSI code for color.
-spec ansi_code(color_name()) -> iolist().
ansi_code(reset) -> "\e[0m";
ansi_code(bold) -> "\e[1m";
ansi_code(grey) -> "\e[90m";
ansi_code(red) -> "\e[31m";
ansi_code(bold_red) -> "\e[1;31m";
ansi_code(green) -> "\e[32m";
ansi_code(bold_green) -> "\e[1;32m";
ansi_code(yellow) -> "\e[33m";
ansi_code(cyan) -> "\e[36m";
ansi_code(magenta) -> "\e[35m";
ansi_code(blue) -> "\e[34m".

%% @private Get reset ANSI code.
-spec reset_code() -> iolist().
reset_code() -> "\e[0m".

%%====================================================================
%% Section 3.3.4: Structured Output
%%====================================================================

%% @doc Convert result to JSON format.
%%
%% Returns a binary containing valid JSON.
%%
-spec to_json(catena_property:property_result()) -> binary().
to_json(Result) ->
    jsx_encode(#{
        kind => atom_to_binary(Result#property_result.kind, utf8),
        tests_run => Result#property_result.tests_run,
        tests_discarded => Result#property_result.tests_discarded,
        shrinks_attempted => Result#property_result.shrinks_attempted,
        seed => format_seed(Result#property_result.seed),
        original_counterexample => format_term(Result#property_result.original_counterexample),
        shrunk_counterexample => format_term(Result#property_result.shrunk_counterexample),
        shrink_history => [format_term(H) || H <- Result#property_result.shrink_history],
        labels => [{Label, Count} || {Label, Count} <- Result#property_result.labels]
    }).

%% @private Encode to JSON.
-spec jsx_encode(map()) -> binary().
jsx_encode(Map) ->
    %% Simple JSON encoding without external dependency
    iolist_to_binary(lists:flatten(json_encode(Map))).

%% JSON encoding - defined together to avoid forward reference issues
-spec json_encode(term()) -> iolist().
json_encode(true) -> "true";
json_encode(false) -> "false";
json_encode(null) -> "null";
json_encode(N) when is_integer(N) -> integer_to_list(N);
json_encode(N) when is_float(N) -> float_to_list(N, [{decimals, 10}, compact]);
json_encode(S) when is_binary(S) -> ["\"", json_encode_string(S), "\""];
json_encode(A) when is_atom(A) -> ["\"", atom_to_list(A), "\""];
json_encode(L) when is_list(L) ->
    case io_lib:printable_list(L) of
        true -> ["\"", json_encode_string(L), "\""];
        false -> ["[", json_encode_list(L), "]"]
    end;
json_encode({}) -> ["{", "}"];
json_encode(M) when is_map(M) ->
    Items = maps:to_list(M),
    ["{", json_encode_items(Items), "}"];
json_encode(T) when is_tuple(T) ->
    L = tuple_to_list(T),
    ["[", json_encode_list(L), "]"].

%% @private Encode a string with JSON escaping.
-spec json_encode_string(binary() | list()) -> iolist().
json_encode_string(S) when is_binary(S) ->
    json_encode_string(binary_to_list(S));
json_encode_string([]) -> [];
json_encode_string([$" | Rest]) -> ["\\\"" | json_encode_string(Rest)];
json_encode_string([$\\ | Rest]) -> ["\\\\\\" | json_encode_string(Rest)];
json_encode_string([$\b | Rest]) -> ["\\b" | json_encode_string(Rest)];
json_encode_string([$\f | Rest]) -> ["\\f" | json_encode_string(Rest)];
json_encode_string([$\n | Rest]) -> ["\\n" | json_encode_string(Rest)];
json_encode_string([$\r | Rest]) -> ["\\r" | json_encode_string(Rest)];
json_encode_string([$\t | Rest]) -> ["\\t" | json_encode_string(Rest)];
json_encode_string([C | Rest]) when C < 16 -> ["\\u000", integer_to_list(C, 16) | json_encode_string(Rest)];
json_encode_string([C | Rest]) when C < 32 -> ["\\u00", integer_to_list(C, 16) | json_encode_string(Rest)];
json_encode_string([C | Rest]) -> [C | json_encode_string(Rest)].

%% @private Encode a list as JSON array.
-spec json_encode_list([term()]) -> iolist().
json_encode_list([]) -> [];
json_encode_list([Item]) -> [json_encode(Item)];
json_encode_list([Item | Rest]) ->
    [json_encode(Item), "," | json_encode_list(Rest)].

%% @private Encode map items as JSON object.
-spec json_encode_items([{term(), term()}]) -> iolist().
json_encode_items([]) -> [];
json_encode_items([{K, V}]) -> [json_encode(K), ": ", json_encode(V)];
json_encode_items([{K, V} | Rest]) ->
    [json_encode(K), ": ", json_encode(V), "," | json_encode_items(Rest)].

%% @doc Convert batch results to JUnit XML format.
%%
%% Returns a binary containing valid JUnit XML for CI systems.
%%
-spec to_junit(binary(), catena_runner:batch_result()) -> binary().
to_junit(TestSuite, Batch) ->
    Total = maps:get(total, Batch),
    Failed = maps:get(failed, Batch),
    Results = maps:get(results, Batch),

    TestsXml = lists:map(fun({Name, {passed, _Result}}) ->
        ["  <testcase name=\"", xml_escape(Name), "\" />\n"];
       ({Name, {failed, Result}}) ->
        Counterexample = format_term(Result#property_result.shrunk_counterexample),
        ["  <testcase name=\"", xml_escape(Name), "\">\n",
         "    <failure message=\"Property failed\">\n",
         "      Counterexample: ", xml_escape(Counterexample), "\n",
         "      Seed: ", xml_escape(format_seed(Result#property_result.seed)), "\n",
         "    </failure>\n",
         "  </testcase>\n"]
    end, Results),

    iolist_to_binary([
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
        "<testsuites>\n",
        "  <testsuite name=\"", xml_escape(TestSuite), "\" tests=\"", integer_to_list(Total),
        "\" failures=\"", integer_to_list(Failed), "\">\n",
        TestsXml,
        "  </testsuite>\n",
        "</testsuites>\n"
    ]).

%% @private Escape XML special characters.
-spec xml_escape(iolist()) -> iolist().
xml_escape(Text) when is_list(Text); is_binary(Text) ->
    L = binary_to_list(iolist_to_binary(Text)),
    xml_escape_list(L, []).

-spec xml_escape_list(string(), iolist()) -> iolist().
xml_escape_list([], Acc) ->
    lists:reverse(Acc);
xml_escape_list([$< | Rest], Acc) ->
    xml_escape_list(Rest, ["&lt;" | Acc]);
xml_escape_list([$> | Rest], Acc) ->
    xml_escape_list(Rest, ["&gt;" | Acc]);
xml_escape_list([$& | Rest], Acc) ->
    xml_escape_list(Rest, ["&amp;" | Acc]);
xml_escape_list([$" | Rest], Acc) ->
    xml_escape_list(Rest, ["&quot;" | Acc]);
xml_escape_list([$' | Rest], Acc) ->
    xml_escape_list(Rest, ["&apos;" | Acc]);
xml_escape_list([C | Rest], Acc) ->
    xml_escape_list(Rest, [C | Acc]).

%%====================================================================
%% Internal Helpers
%%====================================================================

%% @private Get default formatting options.
-spec default_options() -> format_options().
default_options() ->
    #format_options{
        colors = ansi(),
        max_depth = 10,
        max_length = 50,
        truncate = true
    }.

%% @private Format a seed value for display.
-spec format_seed(catena_gen:seed()) -> iolist().
format_seed(#seed{state = State}) ->
    io_lib:format("~.16b", [State]).

%% @private Format a term for JSON/XML output.
-spec format_term(term()) -> iolist().
format_term(undefined) -> "undefined";
format_term(Term) ->
    io_lib:format("~p", [Term]).
