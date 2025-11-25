-module(catena_parser_pattern_tests).
-include_lib("eunit/include/eunit.hrl").

%% Import shared parser test helpers
-import(catena_parser_test_helpers, [parse_transform/1, get_patterns/1, get_guards/1]).

%%====================================================================
%% Pattern Matching Parser Tests
%%====================================================================
%%
%% This module tests the parser's ability to parse pattern matching
%% in transform declarations. Pattern matching is a core feature of Catena,
%% allowing destructuring of data in function arguments.
%%
%% Test Organization:
%%   Section 1: Variable Patterns (5 tests)
%%   Section 2: Wildcard Patterns (3 tests)
%%   Section 3: Literal Patterns (6 tests)
%%   Section 4: Constructor Patterns (6 tests)
%%   Section 5: List Patterns (5 tests)
%%   Section 6: Nested Patterns (4 tests)
%%   Section 7: Guard Tests (5 tests)
%%   Section 8: Pattern Location Tracking (3 tests)
%%   Section 9: Complex Pattern Combinations (3 tests)
%%
%% Total: 41 tests (including 1 documentation test)
%%
%% All tests use the extended parser (catena_parser) which supports
%% full pattern matching capabilities.
%%====================================================================

%%====================================================================
%% Helper Functions - Imported from catena_parser_test_helpers
%%====================================================================
%% parse_transform/1, get_patterns/1, get_guards/1
%%====================================================================
%% Section 1: Variable Patterns
%%====================================================================

%% Test 1.1: Single variable pattern
%% Pattern: x
single_variable_pattern_test() ->
    FlowDecl = parse_transform("transform id x = x"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch({pat_var, x, _}, Pattern).

%% Test 1.2: Multiple variable patterns
%% Pattern: x y
multiple_variable_patterns_test() ->
    FlowDecl = parse_transform("transform add x y = x + y"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(2, length(Patterns)),
    [P1, P2] = Patterns,
    ?assertMatch({pat_var, x, _}, P1),
    ?assertMatch({pat_var, y, _}, P2).

%% Test 1.3: Three variable patterns
%% Pattern: x y z
three_variable_patterns_test() ->
    FlowDecl = parse_transform("transform sum3 x y z = x + y + z"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(3, length(Patterns)),
    [P1, P2, P3] = Patterns,
    ?assertMatch({pat_var, x, _}, P1),
    ?assertMatch({pat_var, y, _}, P2),
    ?assertMatch({pat_var, z, _}, P3).

%% Test 1.4: Variable pattern with underscore in name
%% Pattern: some_var
variable_with_underscore_test() ->
    FlowDecl = parse_transform("transform check some_var = some_var"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch({pat_var, some_var, _}, Pattern).

%% Test 1.5: Empty pattern list
%% Pattern: (none)
empty_pattern_list_test() ->
    FlowDecl = parse_transform("transform answer = 42"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(0, length(Patterns)),
    ?assertEqual([], Patterns).

%%====================================================================
%% Section 2: Wildcard Patterns
%%====================================================================

%% Test 2.1: Single wildcard pattern
%% Pattern: _
single_wildcard_pattern_test() ->
    FlowDecl = parse_transform("transform ignore _ = 42"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch({pat_wildcard, _}, Pattern).

%% Test 2.2: Wildcard with variable pattern
%% Pattern: x _
wildcard_with_variable_test() ->
    FlowDecl = parse_transform("transform const x _ = x"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(2, length(Patterns)),
    [P1, P2] = Patterns,
    ?assertMatch({pat_var, x, _}, P1),
    ?assertMatch({pat_wildcard, _}, P2).

%% Test 2.3: Multiple wildcards
%% Pattern: _ _ _
multiple_wildcards_test() ->
    FlowDecl = parse_transform("transform ignoreAll _ _ _ = 0"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(3, length(Patterns)),
    [P1, P2, P3] = Patterns,
    ?assertMatch({pat_wildcard, _}, P1),
    ?assertMatch({pat_wildcard, _}, P2),
    ?assertMatch({pat_wildcard, _}, P3).

%%====================================================================
%% Section 3: Literal Patterns
%%====================================================================

%% Test 3.1: Integer literal pattern
%% Pattern: 0
integer_literal_pattern_test() ->
    FlowDecl = parse_transform("transform isZero 0 = true"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch({pat_literal, 0, integer, _}, Pattern).

%% Test 3.2: Large integer pattern
%% Pattern: 42
large_integer_pattern_test() ->
    FlowDecl = parse_transform("transform isFortyTwo 42 = true"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch({pat_literal, 42, integer, _}, Pattern).

%% Test 3.3: Float literal pattern
%% Pattern: 3.14
float_literal_pattern_test() ->
    FlowDecl = parse_transform("transform isPi 3.14 = true"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch({pat_literal, 3.14, float, _}, Pattern).

%% Test 3.4: String literal pattern
%% Pattern: "hello"
string_literal_pattern_test() ->
    FlowDecl = parse_transform("transform greet \"hello\" = \"world\""),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch({pat_literal, "hello", string, _}, Pattern).

%% Test 3.5: Empty string pattern
%% Pattern: ""
empty_string_pattern_test() ->
    FlowDecl = parse_transform("transform isEmpty \"\" = true"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch({pat_literal, "", string, _}, Pattern).

%% Test 3.6: Multiple literal patterns
%% Pattern: 0 1
multiple_literal_patterns_test() ->
    FlowDecl = parse_transform("transform isBinary 0 1 = true"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(2, length(Patterns)),
    [P1, P2] = Patterns,
    ?assertMatch({pat_literal, 0, integer, _}, P1),
    ?assertMatch({pat_literal, 1, integer, _}, P2).

%%====================================================================
%% Section 4: Constructor Patterns
%%====================================================================

%% Test 4.1: Nullary constructor pattern
%% Pattern: None
nullary_constructor_pattern_test() ->
    FlowDecl = parse_transform("transform isNone None = true"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch({pat_constructor, 'None', [], _}, Pattern).

%% Test 4.2: Constructor with single argument
%% Pattern: Some(x)
constructor_with_single_arg_test() ->
    FlowDecl = parse_transform("transform unwrap Some(x) = x"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch(
        {pat_constructor, 'Some', [{pat_var, x, _}], _},
        Pattern
    ).

%% Test 4.3: Constructor with multiple arguments
%% Pattern: Point(x, y)
constructor_with_multiple_args_test() ->
    FlowDecl = parse_transform("transform getX Point(x y) = x"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch(
        {pat_constructor, 'Point', [
            {pat_var, x, _},
            {pat_var, y, _}
        ], _},
        Pattern
    ).

%% Test 4.4: Constructor with wildcard argument
%% Pattern: Some(_)
constructor_with_wildcard_test() ->
    FlowDecl = parse_transform("transform isSome Some(_) = true"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch(
        {pat_constructor, 'Some', [{pat_wildcard, _}], _},
        Pattern
    ).

%% Test 4.5: Constructor with literal argument
%% Pattern: Some(0)
constructor_with_literal_test() ->
    FlowDecl = parse_transform("transform isSomeZero Some(0) = true"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch(
        {pat_constructor, 'Some', [{pat_literal, 0, integer, _}], _},
        Pattern
    ).

%% Test 4.6: Multiple constructor patterns
%% Pattern: Left(x) Right(y)
multiple_constructor_patterns_test() ->
    FlowDecl = parse_transform("transform merge Left(x) Right(y) = x + y"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(2, length(Patterns)),
    [P1, P2] = Patterns,
    ?assertMatch(
        {pat_constructor, 'Left', [{pat_var, x, _}], _},
        P1
    ),
    ?assertMatch(
        {pat_constructor, 'Right', [{pat_var, y, _}], _},
        P2
    ).

%%====================================================================
%% Section 5: List Patterns
%%====================================================================

%% Test 5.1: Empty list pattern
%% Pattern: []
empty_list_pattern_test() ->
    FlowDecl = parse_transform("transform isEmpty [] = true"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch({pat_list, [], _}, Pattern).

%% Test 5.2: List with single element pattern
%% Pattern: [x]
single_element_list_pattern_test() ->
    FlowDecl = parse_transform("transform single [x] = x"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch(
        {pat_list, [{pat_var, x, _}], _},
        Pattern
    ).

%% Test 5.3: List with multiple elements
%% Pattern: [x y z]
multiple_element_list_pattern_test() ->
    FlowDecl = parse_transform("transform triple [x y z] = x + y + z"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch(
        {pat_list, [
            {pat_var, x, _},
            {pat_var, y, _},
            {pat_var, z, _}
        ], _},
        Pattern
    ).

%% Test 5.4: List with wildcard elements
%% Pattern: [x _ z]
list_with_wildcards_test() ->
    FlowDecl = parse_transform("transform firstAndThird [x _ z] = x + z"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch(
        {pat_list, [
            {pat_var, x, _},
            {pat_wildcard, _},
            {pat_var, z, _}
        ], _},
        Pattern
    ).

%% Test 5.5: List with literal elements
%% Pattern: [1 2 3]
list_with_literals_test() ->
    FlowDecl = parse_transform("transform isOneTwoThree [1 2 3] = true"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch(
        {pat_list, [
            {pat_literal, 1, integer, _},
            {pat_literal, 2, integer, _},
            {pat_literal, 3, integer, _}
        ], _},
        Pattern
    ).

%%====================================================================
%% Section 6: Nested Patterns
%%====================================================================

%% Test 6.1: Nested constructor patterns
%% Pattern: Some(Some(x))
nested_constructor_pattern_test() ->
    FlowDecl = parse_transform("transform unwrapTwice Some(Some(x)) = x"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch(
        {pat_constructor, 'Some', [
            {pat_constructor, 'Some', [
                {pat_var, x, _}
            ], _}
        ], _},
        Pattern
    ).

%% Test 6.2: Constructor with list pattern
%% Pattern: Node([x y])
constructor_with_list_pattern_test() ->
    FlowDecl = parse_transform("transform getFirst Node([x y]) = x"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch(
        {pat_constructor, 'Node', [
            {pat_list, [
                {pat_var, x, _},
                {pat_var, y, _}
            ], _}
        ], _},
        Pattern
    ).

%% Test 6.3: List with constructor elements
%% Pattern: [Some(x) None]
list_with_constructor_elements_test() ->
    FlowDecl = parse_transform("transform check [Some(x) None] = x"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch(
        {pat_list, [
            {pat_constructor, 'Some', [{pat_var, x, _}], _},
            {pat_constructor, 'None', [], _}
        ], _},
        Pattern
    ).

%% Test 6.4: Deeply nested pattern
%% Pattern: Pair(Some([x]) None)
deeply_nested_pattern_test() ->
    FlowDecl = parse_transform("transform extract Pair(Some([x]) None) = x"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch(
        {pat_constructor, 'Pair', [
            {pat_constructor, 'Some', [
                {pat_list, [{pat_var, x, _}], _}
            ], _},
            {pat_constructor, 'None', [], _}
        ], _},
        Pattern
    ).

%%====================================================================
%% Section 7: Guard Tests
%%====================================================================

%% Test 7.1: Simple guard with comparison
%% Pattern: x  when x > 0
simple_guard_test() ->
    FlowDecl = parse_transform("transform positive x when x > 0 = true"),
    Guards = get_guards(FlowDecl),
    ?assertMatch([{binary_op, gt, {var, x, _}, {literal, 0, integer, _}, _}], Guards).

%% Test 7.2: Guard with equality check
%% Pattern: x  when x == 0
guard_with_equality_test() ->
    FlowDecl = parse_transform("transform isZero x when x == 0 = true"),
    Guards = get_guards(FlowDecl),
    ?assertMatch([{binary_op, eq, {var, x, _}, {literal, 0, integer, _}, _}], Guards).

%% Test 7.3: Guard with multiple conditions
%% Pattern: x y  when x > 0, y > 0
multiple_guard_conditions_test() ->
    FlowDecl = parse_transform("transform bothPositive x y when x > 0, y > 0 = true"),
    Guards = get_guards(FlowDecl),
    ?assertEqual(2, length(Guards)),
    [G1, G2] = Guards,
    ?assertMatch({binary_op, gt, {var, x, _}, {literal, 0, integer, _}, _}, G1),
    ?assertMatch({binary_op, gt, {var, y, _}, {literal, 0, integer, _}, _}, G2).

%% Test 7.4: Guard with arithmetic
%% Pattern: x  when x + 1 > 10
guard_with_arithmetic_test() ->
    FlowDecl = parse_transform("transform check x when x + 1 > 10 = true"),
    Guards = get_guards(FlowDecl),
    ?assertEqual(1, length(Guards)),
    [Guard] = Guards,
    ?assertMatch(
        {binary_op, gt,
            {binary_op, plus, {var, x, _}, {literal, 1, integer, _}, _},
            {literal, 10, integer, _},
            _},
        Guard
    ).

%% Test 7.5: Constructor pattern with guard
%% Pattern: Some(x)  when x > 0
constructor_with_guard_test() ->
    FlowDecl = parse_transform("transform positiveSome Some(x) when x > 0 = x"),
    Patterns = get_patterns(FlowDecl),
    Guards = get_guards(FlowDecl),
    ?assertMatch([{pat_constructor, 'Some', [{pat_var, x, _}], _}], Patterns),
    ?assertMatch([{binary_op, gt, {var, x, _}, {literal, 0, integer, _}, _}], Guards).

%%====================================================================
%% Section 8: Pattern Location Tracking
%%====================================================================

%% Test 8.1: Variable pattern has location
variable_pattern_location_test() ->
    FlowDecl = parse_transform("transform id x = x"),
    Patterns = get_patterns(FlowDecl),
    [Pattern] = Patterns,
    {pat_var, x, Loc} = Pattern,
    ?assertMatch({location, _, _}, Loc).

%% Test 8.2: Constructor pattern has location
constructor_pattern_location_test() ->
    FlowDecl = parse_transform("transform check Some(x) = x"),
    Patterns = get_patterns(FlowDecl),
    [Pattern] = Patterns,
    {pat_constructor, 'Some', _, Loc} = Pattern,
    ?assertMatch({location, _, _}, Loc).

%% Test 8.3: List pattern has location
list_pattern_location_test() ->
    FlowDecl = parse_transform("transform check [x y] = x"),
    Patterns = get_patterns(FlowDecl),
    [Pattern] = Patterns,
    {pat_list, _, Loc} = Pattern,
    ?assertMatch({location, _, _}, Loc).

%%====================================================================
%% Section 9: Complex Pattern Combinations
%%====================================================================

%% Test 9.1: Multiple patterns of different types
%% Pattern: 0 None() [] _
%% Note: Constructor syntax None() needed to prevent it consuming [] _ as arguments
mixed_pattern_types_test() ->
    FlowDecl = parse_transform("transform complex 0 None() [] _ = true"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(4, length(Patterns)),
    [P1, P2, P3, P4] = Patterns,
    ?assertMatch({pat_literal, 0, integer, _}, P1),
    ?assertMatch({pat_constructor, 'None', [], _}, P2),
    ?assertMatch({pat_list, [], _}, P3),
    ?assertMatch({pat_wildcard, _}, P4).

%% Test 9.2: Constructor with mixed nested patterns
%% Pattern: Pair([1 x] Some(_))
complex_nested_mixed_test() ->
    FlowDecl = parse_transform("transform check Pair([1 x] Some(_)) = x"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(1, length(Patterns)),
    [Pattern] = Patterns,
    ?assertMatch(
        {pat_constructor, 'Pair', [
            {pat_list, [
                {pat_literal, 1, integer, _},
                {pat_var, x, _}
            ], _},
            {pat_constructor, 'Some', [{pat_wildcard, _}], _}
        ], _},
        Pattern
    ).

%% Test 9.3: All pattern types in one transform
%% Pattern: x _ 0 "hi" None() [a] Point(p)
%% Note: Constructor syntax None() needed to prevent it consuming [a] Point(p) as arguments
all_pattern_types_test() ->
    FlowDecl = parse_transform("transform kitchen_sink x _ 0 \"hi\" None() [a] Point(p) = x"),
    Patterns = get_patterns(FlowDecl),
    ?assertEqual(7, length(Patterns)),
    [P1, P2, P3, P4, P5, P6, P7] = Patterns,
    ?assertMatch({pat_var, x, _}, P1),
    ?assertMatch({pat_wildcard, _}, P2),
    ?assertMatch({pat_literal, 0, integer, _}, P3),
    ?assertMatch({pat_literal, "hi", string, _}, P4),
    ?assertMatch({pat_constructor, 'None', [], _}, P5),
    ?assertMatch({pat_list, [{pat_var, a, _}], _}, P6),
    ?assertMatch({pat_constructor, 'Point', [{pat_var, p, _}], _}, P7).

%%====================================================================
%% Documentation Tests
%%====================================================================

%% Test: Verify total test count
pattern_test_count_test() ->
    % This test ensures we maintain the expected number of tests
    % Update this count when adding new pattern tests
    ExpectedTests = 41,

    % Get all test functions in this module
    Exports = ?MODULE:module_info(exports),
    TestFuns = [Name || {Name, 0} <- Exports,
                        lists:suffix("_test", atom_to_list(Name))],
    ActualTests = length(TestFuns),

    ?assertEqual(ExpectedTests, ActualTests,
                 io_lib:format("Expected ~p pattern tests but found ~p",
                               [ExpectedTests, ActualTests])).
