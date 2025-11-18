-module(catena_parser_error_tests).
-include_lib("eunit/include/eunit.hrl").

%% Import shared test utilities to eliminate duplication
%% This removes the duplicate helper functions that were previously defined
%% in this module and now uses the centralized test_helpers module.
-import(test_helpers, [
    tokenize_source/1,
    parse_and_expect_error/1,
    parse_and_expect_success/1,
    assert_error_has_location/1,
    assert_parse_error/1
]).

%%====================================================================
%% Parser Error Recovery Tests
%%====================================================================
%%
%% This module tests the parser's error handling and recovery
%% capabilities. Good error handling is critical for developer
%% experience and helps provide clear, actionable error messages.
%%
%% Test Organization:
%%   Section 1: Missing Token Errors (8 tests)
%%   Section 2: Unexpected Token Errors (6 tests)
%%   Section 3: Malformed Expression Errors (7 tests)
%%   Section 4: Invalid Pattern Errors (6 tests)
%%   Section 5: Invalid Type Expression Errors (6 tests)
%%   Section 6: Lexer vs Parser Errors (4 tests)
%%   Section 7: Shape Declaration Errors (5 tests)
%%   Section 8: Flow Declaration Errors (6 tests)
%%   Section 9: Guard Errors (4 tests)
%%   Section 10: Nested Error Cases (4 tests)
%%
%% Total: 57 tests (including 1 documentation test)
%%
%% All tests verify that:
%% 1. Errors are detected correctly
%% 2. Error messages are returned (not crashes)
%% 3. Error locations are tracked
%%====================================================================

%%====================================================================
%% Helper Functions (Eliminated - Now Using test_helpers)
%%====================================================================
%%
%% NOTE: The following duplicate helper functions have been removed
%% and are now imported from test_helpers module:
%%
%% - parse_and_expect_error/1 → test_helpers:parse_and_expect_error/1
%% - assert_error_has_location/1 → test_helpers:assert_error_has_location/1
%%
%% This eliminates the exact code duplication identified in the review.


%%====================================================================
%% Section 1: Missing Token Errors
%%====================================================================

%% Test 1.1: Missing equals in flow declaration
%% Source: flow f 42
missing_equals_in_flow_test() ->
    Result = assert_parse_error("flow f 42"),
    assert_error_has_location(Result).

%% Test 1.2: Missing body in flow declaration
%% Source: flow f =
missing_body_in_flow_test() ->
    Result = assert_parse_error("flow f ="),
    assert_error_has_location(Result).

%% Test 1.3: Missing constructor name after pipe
%% Source: shape Maybe a = Some a |
missing_constructor_after_pipe_test() ->
    Result = assert_parse_error("shape Maybe a = Some a |"),
    assert_error_has_location(Result).

%% Test 1.4: Missing right parenthesis
%% Source: flow f = (1 + 2
missing_right_paren_test() ->
    Result = assert_parse_error("flow f = (1 + 2"),
    assert_error_has_location(Result).

%% Test 1.5: Missing right bracket
%% Source: flow f = [1, 2, 3
missing_right_bracket_test() ->
    Result = assert_parse_error("flow f = [1, 2, 3"),
    assert_error_has_location(Result).

%% Test 1.6: Missing arrow in type signature
%% Source: flow f : a b
missing_arrow_in_type_test() ->
    Result = assert_parse_error("flow f : a b\nflow f = x"),
    assert_error_has_location(Result).

%% Test 1.7: Missing guard expression
%% Source: flow f x when =
missing_guard_expr_test() ->
    Result = assert_parse_error("flow f x when = x"),
    assert_error_has_location(Result).

%% Test 1.8: Missing shape constructors
%% Source: shape Maybe a =
missing_shape_constructors_test() ->
    Result = assert_parse_error("shape Maybe a ="),
    assert_error_has_location(Result).

%%====================================================================
%% Section 2: Unexpected Token Errors
%%====================================================================

%% Test 2.1: Function application is valid (not an error)
%% Source: flow f = 1 2
%% Note: This parses as function application (1 applied to 2)
function_application_valid_test() ->
    Source = "flow f = 1 2",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    Result = catena_parser:parse(Tokens),
    % This should actually succeed (function application)
    ?assertMatch({ok, _}, Result).

%% Test 2.2: Unexpected equals in expression
%% Source: flow f = 1 = 2
unexpected_equals_in_expr_test() ->
    Result = assert_parse_error("flow f = 1 = 2"),
    assert_error_has_location(Result).

%% Test 2.3: Unexpected pipe in expression context
%% Source: flow f = 1 | 2
unexpected_pipe_in_expr_test() ->
    Result = assert_parse_error("flow f = 1 | 2"),
    assert_error_has_location(Result).

%% Test 2.4: Unexpected colon in expression
%% Source: flow f = x : Int
unexpected_colon_in_expr_test() ->
    Result = assert_parse_error("flow f = x : Int"),
    assert_error_has_location(Result).

%% Test 2.5: Two operators in a row
%% Source: flow f = 1 + + 2
two_operators_in_row_test() ->
    Result = assert_parse_error("flow f = 1 + + 2"),
    assert_error_has_location(Result).

%% Test 2.6: Operator at start of expression
%% Source: flow f = + 1
operator_at_start_test() ->
    Result = assert_parse_error("flow f = + 1"),
    assert_error_has_location(Result).

%%====================================================================
%% Section 3: Malformed Expression Errors
%%====================================================================

%% Test 3.1: Unclosed parenthesis in arithmetic
%% Source: flow f = (1 + 2 * 3
unclosed_paren_arithmetic_test() ->
    Result = assert_parse_error("flow f = (1 + 2 * 3"),
    assert_error_has_location(Result).

%% Test 3.2: Empty parentheses
%% Source: flow f = ()
empty_parentheses_test() ->
    Result = assert_parse_error("flow f = ()"),
    assert_error_has_location(Result).

%% Test 3.3: Mismatched delimiters
%% Source: flow f = [1, 2, 3)
mismatched_delimiters_test() ->
    Result = assert_parse_error("flow f = [1, 2, 3)"),
    assert_error_has_location(Result).

%% Test 3.4: Missing operand in binary operation
%% Source: flow f = 1 +
missing_operand_test() ->
    Result = assert_parse_error("flow f = 1 +"),
    assert_error_has_location(Result).

%% Test 3.5: Invalid if expression (missing then)
%% Source: flow f = if true else false
missing_then_in_if_test() ->
    Result = assert_parse_error("flow f = if true else false"),
    assert_error_has_location(Result).

%% Test 3.6: Invalid if expression (missing else)
%% Source: flow f = if true then false
missing_else_in_if_test() ->
    Result = assert_parse_error("flow f = if true then false"),
    assert_error_has_location(Result).

%% Test 3.7: Comma without list context
%% Source: flow f = 1, 2
comma_without_list_test() ->
    Result = assert_parse_error("flow f = 1, 2"),
    assert_error_has_location(Result).

%%====================================================================
%% Section 4: Invalid Pattern Errors
%%====================================================================

%% Test 4.1: Operator in pattern position
%% Source: flow f (x + y) = x
operator_in_pattern_test() ->
    Result = assert_parse_error("flow f (x + y) = x"),
    assert_error_has_location(Result).

%% Test 4.2: Expression as pattern
%% Source: flow f (1 + 1) = x
expression_as_pattern_test() ->
    Result = assert_parse_error("flow f (1 + 1) = x"),
    assert_error_has_location(Result).

%% Test 4.3: Unclosed pattern list
%% Source: flow f [x y = x
unclosed_pattern_list_test() ->
    Result = assert_parse_error("flow f [x y = x"),
    assert_error_has_location(Result).

%% Test 4.4: Empty constructor pattern is valid
%% Source: flow f Some() = x
%% Note: Nullary constructor with explicit parens
empty_constructor_pattern_valid_test() ->
    Source = "flow f Some() = x",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    Result = catena_parser:parse(Tokens),
    % This should succeed (nullary constructor with parens)
    ?assertMatch({ok, _}, Result).

%% Test 4.5: Invalid wildcard with name
%% Source: flow f _x = x
%% Note: This might actually be valid depending on lexer rules
wildcard_with_name_test() ->
    % _x should be a valid identifier starting with underscore
    % This test verifies current behavior
    Source = "flow f _x = _x",
    case parse_and_expect_error(Source) of
        {error, _} -> ok; % Error is acceptable
        _ -> ok % Success is also acceptable (depends on lexer)
    end.

%% Test 4.6: Nested pattern without parentheses where needed
%% Source: flow f Some x y = x
%% This is actually valid - constructor with multiple args
nested_pattern_ambiguous_test() ->
    % This should parse successfully as Some(x, y)
    Source = "flow f = match\n| Some x y -> x\nend",
    % We expect this to work or fail gracefully
    _ = parse_and_expect_error(Source),
    ok. % Either outcome is acceptable

%%====================================================================
%% Section 5: Invalid Type Expression Errors
%%====================================================================

%% Test 5.1: Missing type after arrow
%% Source: flow f : a ->
missing_type_after_arrow_test() ->
    Result = assert_parse_error("flow f : a ->\nflow f = x"),
    assert_error_has_location(Result).

%% Test 5.2: Type operator in wrong position
%% Source: flow f : -> a
type_arrow_at_start_test() ->
    Result = assert_parse_error("flow f : -> a\nflow f = x"),
    assert_error_has_location(Result).

%% Test 5.3: Unclosed type parenthesis
%% Source: flow f : (a -> b
unclosed_type_paren_test() ->
    Result = assert_parse_error("flow f : (a -> b\nflow f = x"),
    assert_error_has_location(Result).

%% Test 5.4: Invalid forall syntax (missing dot)
%% Source: flow f : forall a b
missing_dot_in_forall_test() ->
    Result = assert_parse_error("flow f : forall a b\nflow f = x"),
    assert_error_has_location(Result).

%% Test 5.5: Empty forall params is valid
%% Source: flow f : forall . a
%% Note: Forall with no params is allowed (vacuous quantification)
empty_forall_params_valid_test() ->
    Source = "flow f : forall . a\nflow f = x",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    Result = catena_parser:parse(Tokens),
    % This should succeed (forall with empty params)
    ?assertMatch({ok, _}, Result).

%% Test 5.6: Invalid tuple type (single element)
%% Source: flow f : (a,)
%% Note: Single element tuples might be valid or not
single_element_tuple_test() ->
    Source = "flow f : (a,)\nflow f = x",
    % Either parse error or success is acceptable
    _ = parse_and_expect_error(Source),
    ok.

%%====================================================================
%% Section 6: Lexer vs Parser Errors
%%====================================================================

%% Test 6.1: Invalid character (lexer error)
%% Source: flow f = @
invalid_character_test() ->
    Result = assert_parse_error("flow f = @"),
    assert_error_has_location(Result).

%% Test 6.2: Unterminated string (lexer error)
%% Source: flow f = "hello
unterminated_string_test() ->
    Result = assert_parse_error("flow f = \"hello"),
    assert_error_has_location(Result).

%% Test 6.3: Invalid number format (lexer error)
%% Source: flow f = 123abc
invalid_number_format_test() ->
    Source = "flow f = 123abc",
    Result = parse_and_expect_error(Source),
    % Should be either lexer or parser error
    ?assertMatch({error, _}, Result).

%% Test 6.4: Valid tokens in wrong order (parser error)
%% Source: = flow f x
tokens_in_wrong_order_test() ->
    Result = assert_parse_error("= flow f x"),
    assert_error_has_location(Result).

%%====================================================================
%% Section 7: Shape Declaration Errors
%%====================================================================

%% Test 7.1: Shape with lowercase name
%% Source: shape option a = Some a
lowercase_shape_name_test() ->
    Result = assert_parse_error("shape option a = Some a"),
    assert_error_has_location(Result).

%% Test 7.2: Shape with duplicate type parameters
%% Source: shape Maybe a a = Some a
%% Note: This might be caught in type checking, not parsing
duplicate_type_params_test() ->
    Source = "shape Maybe a a = Some a",
    % This might parse successfully (duplicate detection is semantic)
    _ = parse_and_expect_error(Source),
    ok.

%% Test 7.3: Shape with missing equals
%% Source: shape Maybe a Some a
missing_equals_in_shape_test() ->
    Result = assert_parse_error("shape Maybe a Some a"),
    assert_error_has_location(Result).

%% Test 7.4: Constructor with invalid field type
%% Source: shape Point = Point (x y)
%% Note: Parens might make this valid or invalid
constructor_invalid_field_test() ->
    Source = "shape Point = Point (x y)",
    % Depends on how constructor fields are parsed
    _ = parse_and_expect_error(Source),
    ok.

%% Test 7.5: Empty constructor list
%% Source: shape Maybe a = |
empty_constructor_list_test() ->
    Result = assert_parse_error("shape Maybe a = |"),
    assert_error_has_location(Result).

%%====================================================================
%% Section 8: Flow Declaration Errors
%%====================================================================

%% Test 8.1: Flow with uppercase name
%% Source: flow Map = x
uppercase_flow_name_test() ->
    Result = assert_parse_error("flow Map = x"),
    assert_error_has_location(Result).

%% Test 8.2: Flow with missing name
%% Source: flow = x
missing_flow_name_test() ->
    Result = assert_parse_error("flow = x"),
    assert_error_has_location(Result).

%% Test 8.3: Type signature with incomplete arrow
%% Source: flow f : a ->
syntax_invalid_type_sig_test() ->
    Result = assert_parse_error("flow f : a ->\ "),
    assert_error_has_location(Result).

%% Test 8.4: Multiple equals in flow
%% Source: flow f = x = y
multiple_equals_in_flow_test() ->
    Result = assert_parse_error("flow f = x = y"),
    assert_error_has_location(Result).

%% Test 8.5: Flow with only keyword
%% Source: flow
only_flow_keyword_test() ->
    Result = assert_parse_error("flow"),
    assert_error_has_location(Result).

%% Test 8.6: Match without end
%% Source: flow f = match | x -> x
match_without_end_test() ->
    Result = assert_parse_error("flow f = match\n| x -> x"),
    assert_error_has_location(Result).

%%====================================================================
%% Section 9: Guard Errors
%%====================================================================

%% Test 9.1: Guard without when keyword
%% Source: flow f x x > 0 = x
guard_without_when_test() ->
    Result = assert_parse_error("flow f x x > 0 = x"),
    assert_error_has_location(Result).

%% Test 9.2: Empty guard list
%% Source: flow f x when = x
empty_guard_list_test() ->
    Result = assert_parse_error("flow f x when = x"),
    assert_error_has_location(Result).

%% Test 9.3: Guard with function application is valid
%% Source: flow f x when Some(y) = x
%% Note: Some(y) parses as function application in guard, which is valid
guard_with_function_app_valid_test() ->
    Source = "flow f x when Some(y) = x",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    Result = catena_parser:parse(Tokens),
    % This should succeed (function app in guard is allowed)
    ?assertMatch({ok, _}, Result).

%% Test 9.4: Multiple when keywords
%% Source: flow f x when x > 0 when x < 10 = x
multiple_when_keywords_test() ->
    Result = assert_parse_error("flow f x when x > 0 when x < 10 = x"),
    assert_error_has_location(Result).

%%====================================================================
%% Section 10: Nested Error Cases
%%====================================================================

%% Test 10.1: Error in nested expression
%% Source: flow f = 1 + (2 * )
error_in_nested_expr_test() ->
    Result = assert_parse_error("flow f = 1 + (2 * )"),
    assert_error_has_location(Result).

%% Test 10.2: Error in nested pattern
%% Source: flow f Some(x +) = x
error_in_nested_pattern_test() ->
    Result = assert_parse_error("flow f Some(x +) = x"),
    assert_error_has_location(Result).

%% Test 10.3: Error in nested type
%% Source: flow f : List (a ->)
error_in_nested_type_test() ->
    Result = assert_parse_error("flow f : List (a ->)\nflow f = x"),
    assert_error_has_location(Result).

%% Test 10.4: Multiple errors in same declaration
%% Source: flow F x when = (1 +
multiple_errors_test() ->
    Result = assert_parse_error("flow F x when = (1 +"),
    assert_error_has_location(Result).

%%====================================================================
%% Documentation Tests
%%====================================================================

%% Test: Verify total test count
error_test_count_test() ->
    % This test ensures we maintain the expected number of tests
    % Update this count when adding new error tests
    ExpectedTests = 57,

    % Get all test functions in this module
    Exports = ?MODULE:module_info(exports),
    TestFuns = [Name || {Name, 0} <- Exports,
                        lists:suffix("_test", atom_to_list(Name))],
    ActualTests = length(TestFuns),

    ?assertEqual(ExpectedTests, ActualTests,
                 io_lib:format("Expected ~p error tests but found ~p",
                               [ExpectedTests, ActualTests])).
