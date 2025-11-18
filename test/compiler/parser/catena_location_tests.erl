-module(catena_location_tests).
-include_lib("eunit/include/eunit.hrl").

%%============================================================================
%% Test Suite for catena_location
%%
%% Tests location tracking for precise error reporting.
%%============================================================================

%%----------------------------------------------------------------------------
%% Section 1: Location Construction
%%----------------------------------------------------------------------------

%% Test 1.1: Create single-position location
new_simple_test() ->
    Loc = catena_location:new(10, 5),
    ?assertEqual({location, 10, 5}, Loc),
    ?assertEqual(10, catena_location:get_line(Loc)),
    ?assertEqual(5, catena_location:get_column(Loc)).

%% Test 1.2: Create span location
new_span_test() ->
    Loc = catena_location:new(10, 5, 12, 20),
    ?assertEqual({location, 10, 5, 12, 20}, Loc),
    ?assertEqual(10, catena_location:get_line(Loc)),
    ?assertEqual(5, catena_location:get_column(Loc)),
    ?assertEqual(12, catena_location:get_end_line(Loc)),
    ?assertEqual(20, catena_location:get_end_column(Loc)).

%% Test 1.3: Create location from token (2-tuple)
from_token_simple_test() ->
    Token = {flow, 15},
    Loc = catena_location:from_token(Token),
    ?assertEqual({location, 15, 0}, Loc).

%% Test 1.4: Create location from token (3-tuple)
from_token_with_value_test() ->
    Token = {lower_ident, 20, "myFunction"},
    Loc = catena_location:from_token(Token),
    ?assertEqual({location, 20, 0}, Loc).

%% Test 1.5: Create location from token with length
from_token_with_length_test() ->
    Token = {flow, 10},
    Loc = catena_location:from_token(Token, 4),
    ?assertEqual({location, 10, 0, 10, 4}, Loc).

%% Test 1.6: Create location from token with value and length
from_token_value_length_test() ->
    Token = {lower_ident, 5, "hello"},
    Loc = catena_location:from_token(Token, 0),  % Length from value
    ?assertEqual({location, 5, 0, 5, 5}, Loc).

%%----------------------------------------------------------------------------
%% Section 2: Location Spanning
%%----------------------------------------------------------------------------

%% Test 2.1: Span two simple locations (same line)
span_same_line_test() ->
    Loc1 = catena_location:new(10, 5),
    Loc2 = catena_location:new(10, 15),
    Span = catena_location:span(Loc1, Loc2),
    ?assertEqual({location, 10, 5, 10, 15}, Span).

%% Test 2.2: Span two simple locations (different lines)
span_different_lines_test() ->
    Loc1 = catena_location:new(10, 5),
    Loc2 = catena_location:new(12, 3),
    Span = catena_location:span(Loc1, Loc2),
    ?assertEqual({location, 10, 5, 12, 3}, Span).

%% Test 2.3: Span two span locations
span_two_spans_test() ->
    Span1 = catena_location:new(10, 0, 10, 10),
    Span2 = catena_location:new(10, 15, 10, 25),
    Span = catena_location:span(Span1, Span2),
    ?assertEqual({location, 10, 0, 10, 25}, Span).

%% Test 2.4: Span simple and span location
span_mixed_test() ->
    Loc1 = catena_location:new(8, 0),
    Span2 = catena_location:new(10, 5, 12, 10),
    Span = catena_location:span(Loc1, Span2),
    ?assertEqual({location, 8, 0, 12, 10}, Span).

%% Test 2.5: Span in reverse order
span_reverse_test() ->
    Loc1 = catena_location:new(15, 10),
    Loc2 = catena_location:new(10, 5),
    Span = catena_location:span(Loc1, Loc2),
    ?assertEqual({location, 10, 5, 15, 10}, Span).

%%----------------------------------------------------------------------------
%% Section 3: Location Accessors
%%----------------------------------------------------------------------------

%% Test 3.1: Get line from simple location
get_line_simple_test() ->
    Loc = catena_location:new(42, 10),
    ?assertEqual(42, catena_location:get_line(Loc)).

%% Test 3.2: Get column from simple location
get_column_simple_test() ->
    Loc = catena_location:new(10, 25),
    ?assertEqual(25, catena_location:get_column(Loc)).

%% Test 3.3: Get line from span location
get_line_span_test() ->
    Loc = catena_location:new(10, 5, 12, 20),
    ?assertEqual(10, catena_location:get_line(Loc)).

%% Test 3.4: Get column from span location
get_column_span_test() ->
    Loc = catena_location:new(10, 5, 12, 20),
    ?assertEqual(5, catena_location:get_column(Loc)).

%% Test 3.5: Get end line from span location
get_end_line_span_test() ->
    Loc = catena_location:new(10, 5, 12, 20),
    ?assertEqual(12, catena_location:get_end_line(Loc)).

%% Test 3.6: Get end column from span location
get_end_column_span_test() ->
    Loc = catena_location:new(10, 5, 12, 20),
    ?assertEqual(20, catena_location:get_end_column(Loc)).

%% Test 3.7: Get end line from simple location (same as start)
get_end_line_simple_test() ->
    Loc = catena_location:new(42, 10),
    ?assertEqual(42, catena_location:get_end_line(Loc)).

%% Test 3.8: Get end column from simple location (same as start)
get_end_column_simple_test() ->
    Loc = catena_location:new(10, 25),
    ?assertEqual(25, catena_location:get_end_column(Loc)).

%%----------------------------------------------------------------------------
%% Section 4: Location Validation
%%----------------------------------------------------------------------------

%% Test 4.1: Valid simple location
is_location_simple_valid_test() ->
    Loc = catena_location:new(10, 5),
    ?assert(catena_location:is_location(Loc)).

%% Test 4.2: Valid span location
is_location_span_valid_test() ->
    Loc = catena_location:new(10, 5, 12, 20),
    ?assert(catena_location:is_location(Loc)).

%% Test 4.3: Invalid - not a tuple
is_location_invalid_atom_test() ->
    ?assertNot(catena_location:is_location(invalid)).

%% Test 4.4: Invalid - wrong arity
is_location_invalid_arity_test() ->
    ?assertNot(catena_location:is_location({location, 10})).

%% Test 4.5: Invalid - negative line
is_location_invalid_line_test() ->
    ?assertNot(catena_location:is_location({location, -1, 0})).

%% Test 4.6: Invalid - negative column
is_location_invalid_column_test() ->
    ?assertNot(catena_location:is_location({location, 1, -1})).

%% Test 4.7: Invalid - wrong tag
is_location_invalid_tag_test() ->
    ?assertNot(catena_location:is_location({line, 10, 5})).

%%----------------------------------------------------------------------------
%% Section 5: Location Formatting
%%----------------------------------------------------------------------------

%% Test 5.1: Format simple location with column 0
format_simple_no_col_test() ->
    Loc = catena_location:new(10, 0),
    Formatted = lists:flatten(catena_location:format(Loc)),
    ?assertEqual("10", Formatted).

%% Test 5.2: Format simple location with column
format_simple_with_col_test() ->
    Loc = catena_location:new(10, 5),
    Formatted = lists:flatten(catena_location:format(Loc)),
    ?assertEqual("10:5", Formatted).

%% Test 5.3: Format span on same line
format_span_same_line_test() ->
    Loc = catena_location:new(10, 5, 10, 15),
    Formatted = lists:flatten(catena_location:format(Loc)),
    ?assertEqual("10:5-15", Formatted).

%% Test 5.4: Format span across lines
format_span_multiline_test() ->
    Loc = catena_location:new(10, 5, 12, 20),
    Formatted = lists:flatten(catena_location:format(Loc)),
    ?assertEqual("10:5-12:20", Formatted).

%% Test 5.5: Format span same line no start col
format_span_no_start_col_test() ->
    Loc = catena_location:new(10, 0, 10, 20),
    Formatted = lists:flatten(catena_location:format(Loc)),
    ?assertEqual("10", Formatted).

%%----------------------------------------------------------------------------
%% Section 6: Real-World Usage Scenarios
%%----------------------------------------------------------------------------

%% Test 6.1: Track function definition span
track_function_definition_test() ->
    %% Simulate: flow add x y = x + y
    %%          Line 10, cols 0-21
    FlowToken = {flow, 10},
    _NameToken = {lower_ident, 10, "add"},

    FlowLoc = catena_location:from_token(FlowToken, 4),  % "flow"

    %% Span from flow keyword to end of expression (simulated)
    EndLoc = catena_location:new(10, 21),
    FullSpan = catena_location:span(FlowLoc, EndLoc),

    ?assertEqual({location, 10, 0, 10, 21}, FullSpan).

%% Test 6.2: Track multi-line match expression
track_multiline_match_test() ->
    %% Simulate:
    %% flow length xs = match
    %%   | Nil -> 0
    %%   | Cons _ rest -> 1 + length rest
    %% end

    StartLoc = catena_location:new(10, 0),      % "flow"
    EndLoc = catena_location:new(13, 3),        % "end"

    Span = catena_location:span(StartLoc, EndLoc),
    ?assertEqual({location, 10, 0, 13, 3}, Span),

    Formatted = lists:flatten(catena_location:format(Span)),
    ?assertEqual("10:0-13:3", Formatted).

%% Test 6.3: Track nested expression
track_nested_expression_test() ->
    %% ((x + 1) * 2)
    InnerStart = catena_location:new(5, 2),   % x
    InnerEnd = catena_location:new(5, 7),     % 1
    InnerSpan = catena_location:span(InnerStart, InnerEnd),

    OuterStart = catena_location:new(5, 1),   % (
    OuterEnd = catena_location:new(5, 13),    % )
    OuterSpan = catena_location:span(OuterStart, OuterEnd),

    ?assertEqual({location, 5, 2, 5, 7}, InnerSpan),
    ?assertEqual({location, 5, 1, 5, 13}, OuterSpan).

%% Test 6.4: Combine locations from different AST nodes
combine_ast_nodes_test() ->
    %% Binary operation: x + y
    LeftLoc = catena_location:new(8, 10),      % x
    RightLoc = catena_location:new(8, 14),     % y

    ExprSpan = catena_location:span(LeftLoc, RightLoc),
    ?assertEqual({location, 8, 10, 8, 14}, ExprSpan).
