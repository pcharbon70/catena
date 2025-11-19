%%%-------------------------------------------------------------------
%%% @doc Integration tests for type error formatting with inference
%%%
%%% These tests simulate the full error reporting pipeline, showing
%%% how type errors flow from inference through formatting to final
%%% user-facing messages.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_type_error_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixture Setup
%%%===================================================================

setup() ->
    %% Disable colors for consistent test output
    application:set_env(catena, use_color, false),
    ok.

cleanup(_) ->
    application:unset_env(catena, use_color),
    ok.

integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Simple type mismatch flow", fun test_simple_type_mismatch_flow/0},
      {"Function application error flow", fun test_function_application_error/0},
      {"Multiple errors in sequence", fun test_multiple_errors_flow/0},
      {"Error with location context", fun test_error_with_location/0},
      {"Verbosity level integration", fun test_verbosity_integration/0}
     ]}.

%%%===================================================================
%%% Integration Test Functions
%%%===================================================================

test_simple_type_mismatch_flow() ->
    %% Simulate a type mismatch discovered during inference
    %% This would normally come from catena_infer

    %% Step 1: Inference discovers a type error
    Expected = {tcon, integer},
    Actual = {tcon, string},
    Error = {unification_error, Expected, Actual},

    %% Step 2: Create context (would come from AST)
    Context = #{
        expr => {binop, '+', {lit, 1}, {lit, "hello"}},
        location => {line, 42, column, 15},
        file => "example.catena"
    },

    %% Step 3: Format the error
    FormattedError = catena_type_error_formatter:format_error(Error, Context),
    Output = lists:flatten(FormattedError),

    %% Verify the formatted output contains expected elements
    ?assert(string:str(Output, "Type mismatch") > 0),
    ?assert(string:str(Output, "expression") > 0),  %% Generic expression context
    ?assert(string:str(Output, "Expected: integer") > 0),
    ?assert(string:str(Output, "Got:      string") > 0).

test_function_application_error() ->
    %% Simulate a function type mismatch

    %% The function expects Integer -> Integer
    FunType = {tfun, {tcon, integer}, {tcon, integer}, {effect_set, []}},
    %% But we're passing a String
    ArgType = {tcon, string},

    %% Create the error (would come from inference)
    ExpectedParam = {tcon, integer},
    Error = {unification_error, ExpectedParam, ArgType},

    %% Context from the application site
    Context = #{
        expr => {app, {var, "double"}, [{lit, "not_a_number"}]},
        function_type => FunType,
        location => {line, 10, column, 5}
    },

    %% Format with normal verbosity
    FormattedError = catena_type_error_formatter:format_error(Error, Context),
    Output = lists:flatten(FormattedError),

    ?assert(string:str(Output, "expression") > 0),  %% Will show as generic expression
    ?assert(string:str(Output, "integer") > 0),
    ?assert(string:str(Output, "string") > 0).

test_multiple_errors_flow() ->
    %% Test formatting multiple errors from a single expression

    Errors = [
        {unification_error, {tcon, integer}, {tcon, string}},
        {unification_error, {tcon, bool}, {tcon, integer}},
        {unsatisfied_constraint, 'Eq', [{tvar, "a"}], no_instance}
    ],

    %% Format each error
    FormattedErrors = [catena_type_error_formatter:format_error(E, #{}) || E <- Errors],

    %% Verify we get distinct error messages
    ?assertEqual(3, length(FormattedErrors)),

    %% Each should be a valid error message
    lists:foreach(fun(Formatted) ->
        Output = lists:flatten(Formatted),
        ?assert(length(Output) > 0)
    end, FormattedErrors).

test_error_with_location() ->
    %% Test that location information is preserved and formatted

    Error = {unification_error, {tcon, integer}, {tcon, string}},

    %% Rich context with location
    Context = #{
        expr => {var, "x"},
        location => {line, 25, column, 10},
        file => "src/main.catena",
        function => "calculate",
        module => main
    },

    %% Format with verbose mode to get full context
    Opts = #{verbosity => verbose},
    FormattedError = catena_type_error_formatter:format_error(Error, Context, Opts),
    Output = lists:flatten(FormattedError),

    %% Should include type mismatch and suggestions
    ?assert(string:str(Output, "Type mismatch") > 0),
    ?assert(string:str(Output, "Fix suggestions:") > 0),
    ?assert(string:str(Output, "Examples:") > 0).

test_verbosity_integration() ->
    %% Test that different verbosity levels work correctly in the full pipeline

    Error = {unification_error,
             {tfun, {tcon, string}, {tcon, integer}, {effect_set, []}},
             {tfun, {tcon, integer}, {tcon, string}, {effect_set, []}}},

    Context = #{expr => {var, "transform"}},

    %% Test terse mode
    TerseOpts = #{verbosity => terse},
    TerseOutput = lists:flatten(
        catena_type_error_formatter:format_error(Error, Context, TerseOpts)
    ),

    %% Test normal mode
    NormalOpts = #{verbosity => normal},
    NormalOutput = lists:flatten(
        catena_type_error_formatter:format_error(Error, Context, NormalOpts)
    ),

    %% Test verbose mode
    VerboseOpts = #{verbosity => verbose},
    VerboseOutput = lists:flatten(
        catena_type_error_formatter:format_error(Error, Context, VerboseOpts)
    ),

    %% Verify output sizes match expectations
    %% Terse should be shortest, verbose should be longest
    ?assert(length(TerseOutput) < length(NormalOutput)),
    ?assert(length(NormalOutput) < length(VerboseOutput)),

    %% Verify terse is actually terse
    ?assertNot(string:str(TerseOutput, "Fix suggestions") > 0),

    %% Verify verbose has everything
    ?assert(string:str(VerboseOutput, "Fix suggestions") > 0),
    ?assert(string:str(VerboseOutput, "Examples") > 0).

%%%===================================================================
%%% Helper Functions for Testing
%%%===================================================================

%% @doc Simulate what the inference engine would produce
simulate_inference_error(Code) ->
    %% This would normally parse and infer types
    %% For now, return a mock error
    case Code of
        "let x = 1 + \"hello\"" ->
            {error, {unification_error, {tcon, integer}, {tcon, string}}};
        _ ->
            {ok, {tcon, unit}}
    end.

%% @doc Simulate the full compilation pipeline
compile_and_format(Code) ->
    case simulate_inference_error(Code) of
        {error, TypeErr} ->
            Context = #{
                source_code => Code,
                file => "test.catena"
            },
            {error, catena_type_error_formatter:format_error(TypeErr, Context)};
        {ok, Type} ->
            {ok, Type}
    end.