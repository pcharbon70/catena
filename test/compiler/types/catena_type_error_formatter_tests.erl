%%%-------------------------------------------------------------------
%%% @doc Tests for catena_type_error_formatter module
%%%
%%% Tests the enhanced type error formatting with highlighting,
%%% including type mismatch formatting, structural diff detection,
%%% and human-readable explanations.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_type_error_formatter_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixture Setup
%%%===================================================================

setup() ->
    %% Disable color for predictable test output
    application:set_env(catena, use_color, false),
    ok.

cleanup(_) ->
    application:unset_env(catena, use_color),
    ok.

formatter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Basic type mismatch formatting", fun test_basic_type_mismatch/0},
      {"Function type mismatch", fun test_function_type_mismatch/0},
      {"Record type mismatch", fun test_record_type_mismatch/0},
      {"Variant type mismatch", fun test_variant_type_mismatch/0},
      {"Tuple type mismatch", fun test_tuple_type_mismatch/0},
      {"Missing trait instance", fun test_missing_instance/0},
      {"Arity mismatch", fun test_arity_mismatch/0},
      {"Occurs check error", fun test_occurs_check/0},
      {"Type difference detection", fun test_find_differences/0},
      {"Nested type differences", fun test_nested_differences/0},
      {"Effect set differences", fun test_effect_differences/0},
      {"Error with expression context", fun test_error_with_context/0},
      {"Multiple error formatting", fun test_multiple_errors/0},
      {"Color highlighting", fun test_color_highlighting/0},
      {"Verbosity levels", fun test_verbosity_levels/0},
      {"Depth limiting protection", fun test_depth_limiting/0}
     ]}.

%%%===================================================================
%%% Individual Test Functions
%%%===================================================================

test_basic_type_mismatch() ->
    Expected = {tcon, integer},
    Actual = {tcon, string},

    Result = catena_type_error_formatter:format_type_mismatch(
               Expected, Actual, #{}),
    Output = lists:flatten(Result),

    ?assertMatch("Type mismatch:" ++ _, Output),
    ?assert(string:str(Output, "Expected: integer") > 0),
    ?assert(string:str(Output, "Got:      string") > 0),
    ?assert(string:str(Output, "These types are incompatible") > 0).

test_function_type_mismatch() ->
    Expected = {tfun, {tcon, integer}, {tcon, string}, {effect_set, []}},
    Actual = {tfun, {tcon, string}, {tcon, integer}, {effect_set, []}},

    Result = catena_type_error_formatter:format_type_mismatch(
               Expected, Actual, #{}),
    Output = lists:flatten(Result),

    ?assert(string:str(Output, "integer -> string") > 0),
    ?assert(string:str(Output, "string -> integer") > 0).

test_record_type_mismatch() ->
    Expected = {trecord, [{x, {tcon, integer}}, {y, {tcon, string}}], closed},
    Actual = {trecord, [{x, {tcon, string}}, {z, {tcon, bool}}], closed},

    Result = catena_type_error_formatter:format_type_mismatch(
               Expected, Actual, #{}),
    Output = lists:flatten(Result),

    ?assert(string:str(Output, "{x: integer, y: string}") > 0),
    ?assert(string:str(Output, "{x: string, z: bool}") > 0),
    ?assert(string:str(Output, "missing") > 0 orelse
            string:str(Output, "unexpected") > 0).

test_variant_type_mismatch() ->
    Expected = {tvariant, [{ok, {tcon, integer}}, {error, {tcon, string}}]},
    Actual = {tvariant, [{some, {tcon, integer}}, {none_val, {ttuple, []}}]},

    Result = catena_type_error_formatter:format_type_mismatch(
               Expected, Actual, #{}),
    Output = lists:flatten(Result),

    ?assert(string:str(Output, "ok(integer) | error(string)") > 0),
    ?assert(string:str(Output, "some(integer) | none_val") > 0).

test_tuple_type_mismatch() ->
    Expected = {ttuple, [{tcon, integer}, {tcon, string}]},
    Actual = {ttuple, [{tcon, string}, {tcon, integer}, {tcon, bool}]},

    Result = catena_type_error_formatter:format_type_mismatch(
               Expected, Actual, #{}),
    Output = lists:flatten(Result),

    ?assert(string:str(Output, "(integer, string)") > 0),
    ?assert(string:str(Output, "(string, integer, bool)") > 0).

test_missing_instance() ->
    Result = catena_type_error_formatter:format_missing_instance(
               'Ord', [{tcon, 'Person'}]),
    Output = lists:flatten(Result),

    ?assert(string:str(Output, "No instance of 'Ord'") > 0),
    ?assert(string:str(Output, "Person") > 0),
    ?assert(string:str(Output, "instance Ord Person") > 0),
    ?assert(string:str(Output, "derives [Ord]") > 0).

test_arity_mismatch() ->
    Result = catena_type_error_formatter:format_arity_mismatch(
               2, 3, "add", #{}),
    Output = lists:flatten(Result),

    ?assert(string:str(Output, "Function 'add' expects 2 arguments but got 3") > 0),
    ?assert(string:str(Output, "Remove 1 extra argument") > 0).

test_occurs_check() ->
    Var = {tvar, "a"},
    Type = {tapp, {tcon, list}, [{tvar, "a"}]},

    Result = catena_type_error_formatter:format_occurs_check(
               Var, Type, #{}),
    Output = lists:flatten(Result),

    ?assert(string:str(Output, "Cannot construct infinite type") > 0),
    %% Check for some form of infinite type representation
    ?assert(string:str(Output, "a =") > 0),
    ?assert(string:str(Output, "recursive") > 0).

test_find_differences() ->
    T1 = {tcon, integer},
    T2 = {tcon, string},

    Diffs = catena_type_error_formatter:find_type_differences(T1, T2),

    ?assert(lists:member({mismatch, constructor}, Diffs) orelse
            lists:member({mismatch, root}, Diffs)).

test_nested_differences() ->
    T1 = {tapp, {tcon, list}, {tcon, integer}},
    T2 = {tapp, {tcon, maybe_type}, {tcon, string}},

    Diffs = catena_type_error_formatter:find_type_differences(T1, T2),

    %% Should find differences in both the constructor and the argument
    ?assert(length(Diffs) >= 2).

test_effect_differences() ->
    T1 = {tfun, {tcon, integer}, {tcon, string}, {effect_set, [io]}},
    T2 = {tfun, {tcon, integer}, {tcon, string}, {effect_set, [network, io]}},

    Diffs = catena_type_error_formatter:find_type_differences(T1, T2),

    %% Should detect effect set differences
    HasEffectDiff = lists:any(
        fun({mismatch, effects}) -> true;
           ({path, Path}) -> lists:member(effects, Path);
           (_) -> false
        end, Diffs),
    ?assert(HasEffectDiff).

test_error_with_context() ->
    Expected = {tcon, integer},
    Actual = {tcon, string},
    Context = #{expr => {app, loc, {var, loc, foo}, []}},

    Result = catena_type_error_formatter:format_type_mismatch(
               Expected, Actual, Context),
    Output = lists:flatten(Result),

    ?assert(string:str(Output, "function application 'foo'") > 0).

test_multiple_errors() ->
    %% Test formatting multiple different error types
    Errors = [
        {unification_error, {tcon, integer}, {tcon, string}},
        {unbound_variable, x},
        {arity_mismatch, 2, 3, foo}
    ],

    Results = [catena_type_error_formatter:format_error(E) || E <- Errors],

    ?assertEqual(3, length(Results)),

    %% Each should produce non-empty output
    lists:foreach(fun(R) ->
        ?assert(length(lists:flatten(R)) > 0)
    end, Results).

test_color_highlighting() ->
    %% Enable color for this test
    application:set_env(catena, use_color, true),

    Expected = {tcon, integer},
    Actual = {tcon, string},

    Result = catena_type_error_formatter:format_type_mismatch(
               Expected, Actual, #{}),
    Output = lists:flatten(Result),

    %% Should contain ANSI escape codes when color is enabled
    ?assert(string:str(Output, "\033[") > 0),

    %% Clean up
    application:set_env(catena, use_color, false).

test_verbosity_levels() ->
    T1 = {tcon, integer},
    T2 = {tcon, string},
    Context = #{},

    %% Test terse mode
    TerseOpts = #{verbosity => terse},
    TerseResult = catena_type_error_formatter:format_type_mismatch(T1, T2, Context, TerseOpts),
    TerseOutput = lists:flatten(TerseResult),
    ?assertEqual("Expected integer, got string", TerseOutput),

    %% Test normal mode (default)
    NormalOpts = #{},
    NormalResult = catena_type_error_formatter:format_type_mismatch(T1, T2, Context, NormalOpts),
    NormalOutput = lists:flatten(NormalResult),
    ?assert(string:str(NormalOutput, "Type mismatch") > 0),
    ?assert(string:str(NormalOutput, "Expected:") > 0),
    ?assert(string:str(NormalOutput, "Got:") > 0),

    %% Test verbose mode
    VerboseOpts = #{verbosity => verbose},
    VerboseResult = catena_type_error_formatter:format_type_mismatch(T1, T2, Context, VerboseOpts),
    VerboseOutput = lists:flatten(VerboseResult),
    ?assert(string:str(VerboseOutput, "Fix suggestions:") > 0),
    ?assert(string:str(VerboseOutput, "Examples:") > 0),
    ?assert(string:str(VerboseOutput, "String.to_integer") > 0).

test_depth_limiting() ->
    %% Create a deeply nested type structure (more than 10 levels)
    DeepType1 = make_deeply_nested(15, {tcon, integer}),
    DeepType2 = make_deeply_nested(15, {tcon, string}),

    %% This should not crash and should return a too_deep mismatch
    Diffs = catena_type_error_formatter:find_type_differences(DeepType1, DeepType2),

    %% Verify we get a too_deep mismatch
    HasTooDeep = lists:any(
        fun({mismatch, too_deep}) -> true;
           (_) -> false
        end, Diffs),
    ?assert(HasTooDeep),

    %% Test that normal depth still works correctly
    Normal1 = make_deeply_nested(3, {tcon, integer}),
    Normal2 = make_deeply_nested(3, {tcon, string}),
    NormalDiffs = catena_type_error_formatter:find_type_differences(Normal1, Normal2),

    %% Should find the actual difference, not too_deep
    NoTooDeep = not lists:any(
        fun({mismatch, too_deep}) -> true;
           (_) -> false
        end, NormalDiffs),
    ?assert(NoTooDeep),

    %% Should find the constructor mismatch at the innermost level
    HasConstructor = lists:any(
        fun({mismatch, constructor}) -> true;
           (_) -> false
        end, NormalDiffs),
    ?assert(HasConstructor).

%% Helper function to create deeply nested types
make_deeply_nested(0, Base) ->
    Base;
make_deeply_nested(N, Base) ->
    {tapp, {tcon, list}, make_deeply_nested(N - 1, Base)}.

%%%===================================================================
%%% Property Tests
%%%===================================================================

%% Note: Property tests would go here if using PropEr/QuickCheck
%% For now, we rely on the comprehensive unit tests above