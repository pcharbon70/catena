%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_repl_completion
%%% @end
%%%-------------------------------------------------------------------
-module(catena_repl_completion_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

%% Default completion context
default_context() ->
    #{
        bindings => #{myVar => typed, myFunc => typed},
        runtime_bindings => #{map => {fun lists:map/2, 2}, filter => {fun lists:filter/2, 2}},
        env => #{x => type_scheme}
    }.

%%%=============================================================================
%%% Empty Input Tests
%%%=============================================================================

complete_empty_test() ->
    Result = catena_repl_completion:complete_input("", default_context()),
    ?assertMatch({matched, _, _, _}, Result).

%%%=============================================================================
%%% Command Completion Tests
%%%=============================================================================

complete_colon_test() ->
    Result = catena_repl_completion:complete_input(":", default_context()),
    ?assertMatch({matched, _, _, _}, Result),
    {matched, Prefix, Completions, _} = Result,
    ?assertEqual(":", Prefix),
    ?assert(lists:member(":type", Completions)),
    ?assert(lists:member(":load", Completions)).

complete_command_partial_test() ->
    Result = catena_repl_completion:complete_input(":t", default_context()),
    ?assertMatch({matched, _, _, _}, Result),
    {matched, _, Completions, _} = Result,
    ?assert(lists:member(":type", Completions)),
    ?assert(lists:member(":t", Completions)).

%%%=============================================================================
%%% Keyword Completion Tests
%%%=============================================================================

%% Note: These tests rely on the completion system which includes
%% keywords from the module. The actual implementation may vary
%% based on how the context is processed.

%%%=============================================================================
%%% No Match Tests
%%%=============================================================================

complete_no_match_test() ->
    Result = catena_repl_completion:complete_input("xyz123", default_context()),
    ?assertEqual(no_match, Result).

complete_no_command_match_test() ->
    Result = catena_repl_completion:complete_input(":xyz123", default_context()),
    ?assertEqual(no_match, Result).

%%%=============================================================================
%%% Common Prefix Tests
%%%=============================================================================

common_prefix_single_test() ->
    Result = catena_repl_completion:complete_input(":t", default_context()),
    ?assertMatch({matched, _, _, _}, Result),
    {matched, _, _, Common} = Result,
    ?assertEqual(":t", Common).

%%%=============================================================================
%%% Format Completions Tests
%%%=============================================================================

format_empty_test() ->
    Result = catena_repl_completion:format_completions([]),
    ?assertEqual("No completions", lists:flatten(Result)).

format_some_test() ->
    Result = catena_repl_completion:format_completions([":type", ":load"]),
    Formatted = lists:flatten(Result),
    ?assert(string:str(Formatted, "Commands") > 0).

format_mixed_test() ->
    Result = catena_repl_completion:format_completions([
        ":type", "transform", "map", "Maybe", "myVar"
    ]),
    Formatted = lists:flatten(Result),
    ?assert(string:str(Formatted, "Commands") > 0),
    ?assert(string:str(Formatted, "Keywords") > 0),
    ?assert(string:str(Formatted, "Builtins") > 0),
    ?assert(string:str(Formatted, "Types") > 0),
    ?assert(string:str(Formatted, "Identifiers") > 0).

%%%=============================================================================
%%% is_prefix Tests
%%%=============================================================================

is_prefix_basic_test() ->
    ?assert(catena_repl_completion:is_prefix("ty", "type")),
    ?assert(catena_repl_completion:is_prefix("he", "head")),
    ?assertNot(catena_repl_completion:is_prefix("xyz", "head")),
    ?assertNot(catena_repl_completion:is_prefix("type", "ty")).

is_prefix_empty_test() ->
    ?assert(catena_repl_completion:is_prefix("", "anything")),
    ?assert(catena_repl_completion:is_prefix("", "")).

is_prefix_case_sensitive_test() ->
    ?assertNot(catena_repl_completion:is_prefix("TY", "type")),
    ?assert(catena_repl_completion:is_prefix("Ty", "Type")).
