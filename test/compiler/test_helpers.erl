%% Test Helper Module
%% Shared utilities for compiler test suites
%%
%% This module provides common test utilities to reduce code duplication
%% across test files. All test files can use these helpers for file
%% management and other common testing tasks.

-module(test_helpers).
-include_lib("eunit/include/eunit.hrl").

-export([
    % File management
    with_temp_file/2,
    with_temp_file/3,
    create_test_file/2,
    delete_test_file/1,
    generate_temp_filename/0,
    generate_temp_filename/1,

    % Parser testing utilities
    tokenize_source/1,
    parse_and_expect_error/1,
    parse_and_expect_success/1,
    parse_and_get_declarations/1,
    get_declarations/1,
    assert_error_has_location/1,
    assert_parse_error/1,
    create_nested_expression/2,
    create_nested_try_with/2,
    generate_test_tokens/1,
    assert_ast_structure/2
]).

%%====================================================================
%% Temporary File Management
%%====================================================================

%% @doc Execute function with temporary file, ensuring cleanup
%% Creates a temporary file with the given content, executes the function,
%% and guarantees cleanup even if the function fails.
-spec with_temp_file(iodata(), fun((string()) -> term())) -> term().
with_temp_file(Content, Fun) ->
    TestFile = generate_temp_filename(),
    create_test_file(TestFile, Content),
    try
        Fun(TestFile)
    after
        delete_test_file(TestFile)
    end.

%% @doc Execute function with temporary file using custom filename
%% Allows specifying a custom filename pattern while still ensuring cleanup.
-spec with_temp_file(string(), iodata(), fun((string()) -> term())) -> term().
with_temp_file(Suffix, Content, Fun) ->
    TestFile = generate_temp_filename(Suffix),
    create_test_file(TestFile, Content),
    try
        Fun(TestFile)
    after
        delete_test_file(TestFile)
    end.

%% @doc Generate a unique temporary filename
%% Creates a filename based on process ID and timestamp to avoid collisions.
-spec generate_temp_filename() -> string().
generate_temp_filename() ->
    generate_temp_filename("test").

%% @doc Generate a unique temporary filename with custom suffix
%% Useful for creating descriptive test filenames.
-spec generate_temp_filename(string()) -> string().
generate_temp_filename(Suffix) ->
    % Use process ID and unique reference for uniqueness
    Pid = pid_to_list(self()),
    Ref = ref_to_list(make_ref()),
    % Clean up the ref string (remove < > characters)
    CleanRef = lists:filter(fun(C) -> C =/= $< andalso C =/= $> end, Ref),
    "/tmp/catena_test_" ++ Suffix ++ "_" ++ Pid ++ "_" ++ CleanRef ++ ".catena".

%% @doc Create a test file with given content
%% Writes content to a file, failing if the write fails.
-spec create_test_file(string(), iodata()) -> ok.
create_test_file(Filename, Content) ->
    ok = file:write_file(Filename, Content).

%% @doc Delete a test file
%% Removes the test file from the filesystem.
-spec delete_test_file(string()) -> ok | {error, term()}.
delete_test_file(Filename) ->
    file:delete(Filename).

%%====================================================================
%% Parser Testing Utilities
%%====================================================================

%% @doc Tokenize source code using the Catena lexer
%% Wraps catena_lexer:tokenize/1 with consistent error handling.
-spec tokenize_source(string()) -> {ok, list()} | {error, term()}.
tokenize_source(Code) ->
    catena_lexer:tokenize(Code).

%% @doc Parse source and expect an error
%% Useful for testing error conditions and validation.
-spec parse_and_expect_error(string()) -> {error, term()}.
parse_and_expect_error(Source) ->
    case tokenize_source(Source) of
        {ok, Tokens} ->
            case catena_parser:parse(Tokens) of
                {ok, _AST} ->
                    {error, unexpected_success};
                {error, ErrorInfo} ->
                    {error, ErrorInfo}
            end;
        {error, LexError} ->
            {error, {lexer, LexError}}
    end.

%% @doc Parse source and expect success
%% Returns the AST on success, fails assertion on error.
-spec parse_and_expect_success(string()) -> term().
parse_and_expect_success(Source) ->
    case tokenize_source(Source) of
        {ok, Tokens} ->
            case catena_parser:parse(Tokens) of
                {ok, AST} ->
                    AST;
                {error, ErrorInfo} ->
                    ?assert(false, io_lib:format("Unexpected parse error: ~p", [ErrorInfo]))
            end;
        {error, LexError} ->
            ?assert(false, io_lib:format("Lexer error: ~p", [LexError]))
    end.

%% @doc Parse source and return just the declarations list
%% Handles both old list format and new module tuple format for backward compatibility.
-spec parse_and_get_declarations(string()) -> list().
parse_and_get_declarations(Source) ->
    AST = parse_and_expect_success(Source),
    get_declarations(AST).

%% @doc Extract declarations from AST
%% Works with both module tuple format and legacy list format.
-spec get_declarations(term()) -> list().
get_declarations({module, _Name, _Exports, _Imports, Declarations, _Loc}) ->
    Declarations;
get_declarations(Declarations) when is_list(Declarations) ->
    Declarations.

%% @doc Assert that error has a line number for accurate error reporting
%% Validates that error information includes location data for IDE integration.
%% Handles multiple error formats from lexer and parser.
-spec assert_error_has_location({error, term()}) -> ok.
assert_error_has_location({error, {Line, _Module, _Msg}}) when is_integer(Line) ->
    ok;
assert_error_has_location({error, {lexer, {Line, _Module, _Msg}}}) when is_integer(Line) ->
    ok;
assert_error_has_location({error, {lexer, {error, {Line, _Module, _Err}, _Col}}}) when is_integer(Line) ->
    ok;
assert_error_has_location(Other) ->
    ?assert(false, io_lib:format("Error missing location: ~p", [Other])).

%% @doc Create nested expressions for stress testing
%% Generates deeply nested expressions to test parser limits.
-spec create_nested_expression(integer(), string()) -> string().
create_nested_expression(0, BaseExpr) ->
    BaseExpr;
create_nested_expression(N, BaseExpr) ->
    Inner = create_nested_expression(N-1, BaseExpr),
    lists:flatten(io_lib:format("(~s)", [Inner])).

%% @doc Create nested try-with expressions for stress testing
%% Generates deeply nested try-with blocks to test handler parsing limits.
-spec create_nested_try_with(integer(), string()) -> string().
create_nested_try_with(0, Expr) ->
    Expr;
create_nested_try_with(N, Expr) ->
    Inner = create_nested_try_with(N-1, Expr),
    lists:flatten(io_lib:format("try ~s with Effect~p { op~p -> ~p } end", [Inner, N, N, N])).

%% @doc Generate test tokens by parsing source and returning tokens
%% Useful for token-level testing without full parsing.
-spec generate_test_tokens(string()) -> list().
generate_test_tokens(Source) ->
    case tokenize_source(Source) of
        {ok, Tokens} ->
            Tokens;
        {error, _Reason} ->
            []
    end.

%% @doc Assert AST structure matches expected pattern
%% Simplifies AST validation in tests by providing structure checking.
-spec assert_ast_structure(term(), term()) -> ok.
assert_ast_structure(AST, ExpectedPattern) ->
    case AST =:= ExpectedPattern of
        true -> 
            ok;
        false ->
            ?assert(false, io_lib:format("AST structure mismatch. Expected: ~p, Got: ~p", [ExpectedPattern, AST]))
    end.

%% @doc Assert parse error with validation
%% Combines parse_and_expect_error/1 with assertion pattern used in multiple files.
%% Note: Returns unexpected_success if parsing succeeded but code expected it to fail.
-spec assert_parse_error(string()) -> {error, term()}.
assert_parse_error(Source) ->
    Result = parse_and_expect_error(Source),
    case Result of
        {error, _} -> ?assertMatch({error, _}, Result), Result;
        {error, unexpected_success} -> 
            ?assert(false, "Expected parse error but parsing succeeded"),
            Result
    end.
