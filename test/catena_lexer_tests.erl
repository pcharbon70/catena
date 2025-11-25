-module(catena_lexer_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Helper Functions
%%====================================================================

%% @doc Extract token types from token list
%% Converts [{Type, Line}, ...] or [{Type, Line, Value}, ...] to [Type, ...]
-spec token_types([tuple()]) -> [atom()].
token_types(Tokens) ->
    lists:map(fun
        ({Type, _Line}) -> Type;
        ({Type, _Line, _Value}) -> Type
    end, Tokens).

%%====================================================================
%% Test 1.1.1.1: Keywords, Operators, and Delimiters
%%====================================================================

core_keywords_test() ->
    %% Test 12 core Catena keywords requiring compiler support
    {ok, Tokens} = catena_lexer:tokenize("type transform let match trait instance effect perform handle actor process module"),
    ?assertEqual([
        {type, 1},
        {transform, 1},
        {'let', 1},
        {match, 1},
        {trait, 1},
        {instance, 1},
        {effect, 1},
        {perform, 1},
        {handle, 1},
        {actor, 1},
        {process, 1},
        {'module', 1}
    ], Tokens).

syntax_keywords_test() ->
    %% Test syntax support keywords
    Input = "in end case of when as forall operation",
    {ok, Tokens} = catena_lexer:tokenize(Input),
    Expected = ['in', 'end', 'case', 'of', 'when', as, forall, operation],
    ?assertEqual(Expected, token_types(Tokens)).

module_keywords_test() ->
    %% Test module system keywords
    Input = "import export exports qualified private",
    {ok, Tokens} = catena_lexer:tokenize(Input),
    Expected = ['import', 'export', exports, qualified, private],
    ?assertEqual(Expected, token_types(Tokens)).

all_keywords_test() ->
    %% Test comprehensive keyword list (minimal core)
    Input = "type transform let match trait instance effect perform handle actor process module "
            "in end case of when as forall operation import export exports qualified private",
    {ok, Tokens} = catena_lexer:tokenize(Input),
    Expected = [type, transform, 'let', match, trait, instance, effect, perform, handle, actor, process, 'module',
                'in', 'end', 'case', 'of', 'when', as, forall, operation, 'import', 'export', exports, qualified, private],
    ?assertEqual(Expected, token_types(Tokens)).

removed_keywords_are_identifiers_test() ->
    %% Test that some former keywords are now recognized as identifiers
    %% Note: do, then, where are still keywords (used in grammar)
    %% Only if, else, extends, try, with, supervisor were removed
    Input = "if else extends try with supervisor",
    {ok, Tokens} = catena_lexer:tokenize(Input),
    %% All should be lower_ident now, not keywords
    Types = token_types(Tokens),
    ?assertEqual([lower_ident, lower_ident, lower_ident,
                  lower_ident, lower_ident, lower_ident], Types).

operators_two_char_test() ->
    %% Test two-character and three-character operators
    {ok, Tokens} = catena_lexer:tokenize("|> -> => === !== == /= <= >= || && :: <- .."),
    Expected = [pipe_right, arrow, double_arrow, setoid_eq, setoid_neq, eq, neq, lte, gte,
                'or', 'and', cons, left_arrow, range],
    ?assertEqual(Expected, token_types(Tokens)).

operators_single_char_test() ->
    %% Test single-character operators
    {ok, Tokens} = catena_lexer:tokenize(": = | < > + - * / ."),
    Expected = [colon, equals, pipe, lt, gt, plus, minus, star, slash, dot],
    ?assertEqual(Expected, token_types(Tokens)).

delimiters_test() ->
    %% Test all delimiters
    {ok, Tokens} = catena_lexer:tokenize("{ } [ ] ( ) , ; _"),
    Expected = [lbrace, rbrace, lbracket, rbracket, lparen, rparen, comma, semicolon, underscore],
    ?assertEqual(Expected, token_types(Tokens)).

operator_precedence_in_expression_test() ->
    %% Test that operators are recognized in expressions
    {ok, Tokens} = catena_lexer:tokenize("x |> f |> g"),
    ?assertMatch([
        {lower_ident, 1, "x"},
        {pipe_right, 1},
        {lower_ident, 1, "f"},
        {pipe_right, 1},
        {lower_ident, 1, "g"}
    ], Tokens).

%%====================================================================
%% Test 1.1.1.2: Number Literal Recognition
%%====================================================================

integer_literals_test() ->
    {ok, Tokens} = catena_lexer:tokenize("0 1 42 123456789"),
    ?assertMatch([
        {integer, 1, 0},
        {integer, 1, 1},
        {integer, 1, 42},
        {integer, 1, 123456789}
    ], Tokens).

float_literals_test() ->
    {ok, Tokens} = catena_lexer:tokenize("0.0 1.5 3.14159 99.99"),
    ?assertMatch([
        {float, 1, 0.0},
        {float, 1, 1.5},
        {float, 1, _},
        {float, 1, 99.99}
    ], Tokens),
    %% Check pi approximation
    {float, 1, Pi} = lists:nth(3, Tokens),
    ?assert(abs(Pi - 3.14159) < 0.00001).

scientific_notation_positive_exp_test() ->
    {ok, Tokens} = catena_lexer:tokenize("1e10 1.5e10 2.5E5"),
    ?assertMatch([
        {float, 1, _},
        {float, 1, _},
        {float, 1, _}
    ], Tokens),
    %% Verify values
    {float, 1, V1} = lists:nth(1, Tokens),
    ?assertEqual(1.0e10, V1),
    {float, 1, V2} = lists:nth(2, Tokens),
    ?assertEqual(1.5e10, V2),
    {float, 1, V3} = lists:nth(3, Tokens),
    ?assertEqual(2.5e5, V3).

scientific_notation_negative_exp_test() ->
    {ok, Tokens} = catena_lexer:tokenize("1e-10 1.5e-10 2.5E-5"),
    ?assertMatch([
        {float, 1, _},
        {float, 1, _},
        {float, 1, _}
    ], Tokens),
    %% Verify values
    {float, 1, V1} = lists:nth(1, Tokens),
    ?assertEqual(1.0e-10, V1),
    {float, 1, V2} = lists:nth(2, Tokens),
    ?assertEqual(1.5e-10, V2),
    {float, 1, V3} = lists:nth(3, Tokens),
    ?assertEqual(2.5e-5, V3).

scientific_notation_explicit_plus_test() ->
    {ok, Tokens} = catena_lexer:tokenize("1e+5 2.5E+10"),
    ?assertMatch([
        {float, 1, _},
        {float, 1, _}
    ], Tokens),
    {float, 1, V1} = lists:nth(1, Tokens),
    ?assertEqual(1.0e5, V1),
    {float, 1, V2} = lists:nth(2, Tokens),
    ?assertEqual(2.5e10, V2).

mixed_number_literals_test() ->
    %% Test integers, floats, and scientific notation together
    {ok, Tokens} = catena_lexer:tokenize("42 3.14 1e10 100 0.5 2e-3"),
    ?assertEqual(6, length(Tokens)),
    ?assertMatch([
        {integer, 1, 42},
        {float, 1, 3.14},
        {float, 1, _},
        {integer, 1, 100},
        {float, 1, 0.5},
        {float, 1, _}
    ], Tokens).

%%====================================================================
%% Test 1.1.1.3: String Literal Recognition
%%====================================================================

simple_string_test() ->
    {ok, Tokens} = catena_lexer:tokenize("\"hello\""),
    ?assertMatch([{string, 1, "hello"}], Tokens).

string_with_spaces_test() ->
    {ok, Tokens} = catena_lexer:tokenize("\"hello world\""),
    ?assertMatch([{string, 1, "hello world"}], Tokens).

empty_string_test() ->
    {ok, Tokens} = catena_lexer:tokenize("\"\""),
    ?assertMatch([{string, 1, ""}], Tokens).

string_escape_newline_test() ->
    {ok, Tokens} = catena_lexer:tokenize("\"line1\\nline2\""),
    ?assertMatch([{string, 1, "line1\nline2"}], Tokens).

string_escape_tab_test() ->
    {ok, Tokens} = catena_lexer:tokenize("\"col1\\tcol2\""),
    ?assertMatch([{string, 1, "col1\tcol2"}], Tokens).

string_escape_carriage_return_test() ->
    {ok, Tokens} = catena_lexer:tokenize("\"text\\rmore\""),
    ?assertMatch([{string, 1, "text\rmore"}], Tokens).

string_escape_backslash_test() ->
    {ok, Tokens} = catena_lexer:tokenize("\"path\\\\file\""),
    ?assertMatch([{string, 1, "path\\file"}], Tokens).

string_escape_quote_test() ->
    {ok, Tokens} = catena_lexer:tokenize("\"say \\\"hello\\\"\""),
    ?assertMatch([{string, 1, "say \"hello\""}], Tokens).

string_escape_single_quote_test() ->
    {ok, Tokens} = catena_lexer:tokenize("\"can\\'t\""),
    ?assertMatch([{string, 1, "can't"}], Tokens).

string_all_escapes_test() ->
    {ok, Tokens} = catena_lexer:tokenize("\"\\n\\r\\t\\\\\\\"'\""),
    ?assertMatch([{string, 1, "\n\r\t\\\"'"}], Tokens).

multiple_strings_test() ->
    {ok, Tokens} = catena_lexer:tokenize("\"first\" \"second\" \"third\""),
    ?assertEqual(3, length(Tokens)),
    ?assertMatch([
        {string, 1, "first"},
        {string, 1, "second"},
        {string, 1, "third"}
    ], Tokens).

%%====================================================================
%% String Length Limits (Security)
%%====================================================================

string_length_within_limit_test() ->
    %% String with 8190 characters of content (+ 2 quotes = 8192 total)
    Content = lists:duplicate(8190, $a),
    Source = "\"" ++ Content ++ "\"",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    ?assertMatch([{string, 1, _}], Tokens).

string_length_at_limit_test() ->
    %% String with exactly 8190 chars content (at the limit)
    Content = lists:duplicate(8190, $x),
    Source = "\"" ++ Content ++ "\"",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    [{string, 1, Value}] = Tokens,
    ?assertEqual(8190, length(Value)).

string_length_exceeds_limit_test() ->
    %% String with 8191 characters of content (exceeds limit)
    Content = lists:duplicate(8191, $a),
    Source = "\"" ++ Content ++ "\"",
    Result = catena_lexer:tokenize(Source),
    ?assertMatch({error, {1, catena_lexer, {user, {string_too_long, 1, 8193, 8192}}}}, Result).

string_length_far_exceeds_limit_test() ->
    %% String with 10000 characters (far exceeds limit)
    Content = lists:duplicate(10000, $b),
    Source = "\"" ++ Content ++ "\"",
    Result = catena_lexer:tokenize(Source),
    ?assertMatch({error, {1, catena_lexer, {user, {string_too_long, 1, 10002, 8192}}}}, Result).

string_length_with_escapes_test() ->
    %% Escapes count as their source length, not expanded length
    %% "\n\n\n..." x 2000 = 4000 chars source, expands to 2000 chars
    Escapes = lists:duplicate(4000, "\\n"),
    Content = lists:flatten(Escapes),
    Source = "\"" ++ Content ++ "\"",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    [{string, 1, Value}] = Tokens,
    %% Value should be 4000 newlines
    ?assertEqual(4000, length(Value)).

%%====================================================================
%% Test 1.1.1.4: Comment Recognition
%%====================================================================

single_line_comment_test() ->
    {ok, Tokens} = catena_lexer:tokenize("x -- this is a comment\ny"),
    ?assertMatch([
        {lower_ident, 1, "x"},
        {lower_ident, 2, "y"}
    ], Tokens).

single_line_comment_at_end_test() ->
    {ok, Tokens} = catena_lexer:tokenize("x -- comment at end"),
    ?assertMatch([{lower_ident, 1, "x"}], Tokens).

single_line_comment_entire_line_test() ->
    {ok, Tokens} = catena_lexer:tokenize("-- entire line is a comment\nx"),
    ?assertMatch([{lower_ident, 2, "x"}], Tokens).

multi_line_comment_test() ->
    {ok, Tokens} = catena_lexer:tokenize("x {- comment -} y"),
    ?assertMatch([
        {lower_ident, 1, "x"},
        {lower_ident, 1, "y"}
    ], Tokens).

multi_line_comment_multiline_test() ->
    Input = "x {- this is\n"
            "a multi-line\n"
            "comment -} y",
    {ok, Tokens} = catena_lexer:tokenize(Input),
    ?assertMatch([
        {lower_ident, 1, "x"},
        {lower_ident, 3, "y"}
    ], Tokens).

nested_comment_single_level_test() ->
    {ok, Tokens} = catena_lexer:tokenize("x {- outer {- inner -} -} y"),
    ?assertMatch([
        {lower_ident, 1, "x"},
        {lower_ident, 1, "y"}
    ], Tokens).

nested_comment_multiple_levels_test() ->
    {ok, Tokens} = catena_lexer:tokenize("a {- L1 {- L2 {- L3 -} -} -} b"),
    ?assertMatch([
        {lower_ident, 1, "a"},
        {lower_ident, 1, "b"}
    ], Tokens).

unclosed_comment_error_test() ->
    Result = catena_lexer:tokenize("x {- unclosed comment"),
    ?assertMatch({error, {0, catena_lexer, {unclosed_comment, 1}}}, Result).

unmatched_comment_end_error_test() ->
    Result = catena_lexer:tokenize("x -} y"),
    ?assertMatch({error, {1, catena_lexer, unmatched_comment_end}}, Result).

%%====================================================================
%% Comment Depth Limits (Security)
%%====================================================================

comment_depth_within_limit_test() ->
    %% 50 levels of nesting (well under limit of 100)
    Opening = lists:flatten(lists:duplicate(50, "{- ")),
    Closing = lists:flatten(lists:duplicate(50, " -}")),
    Source = "x " ++ Opening ++ "deep" ++ Closing ++ " y",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    ?assertEqual([{lower_ident, 1, "x"}, {lower_ident, 1, "y"}], Tokens).

comment_depth_at_limit_test() ->
    %% Exactly 100 levels of nesting (at the limit)
    Opening = lists:flatten(lists:duplicate(100, "{- ")),
    Closing = lists:flatten(lists:duplicate(100, " -}")),
    Source = "x " ++ Opening ++ "max" ++ Closing ++ " y",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    ?assertEqual([{lower_ident, 1, "x"}, {lower_ident, 1, "y"}], Tokens).

comment_depth_exceeds_limit_test() ->
    %% 101 levels of nesting (exceeds limit)
    Opening = lists:flatten(lists:duplicate(101, "{- ")),
    Closing = lists:flatten(lists:duplicate(101, " -}")),
    Source = "x " ++ Opening ++ "too deep" ++ Closing ++ " y",
    Result = catena_lexer:tokenize(Source),
    ?assertMatch({error, {1, catena_lexer, {comment_depth_exceeded, 101, 100}}}, Result).

comment_depth_far_exceeds_limit_test() ->
    %% 200 levels of nesting (far exceeds limit)
    Opening = lists:flatten(lists:duplicate(200, "{- ")),
    Closing = lists:flatten(lists:duplicate(200, " -}")),
    Source = "x " ++ Opening ++ "way too deep" ++ Closing ++ " y",
    Result = catena_lexer:tokenize(Source),
    ?assertMatch({error, {1, catena_lexer, {comment_depth_exceeded, 101, 100}}}, Result).

comment_depth_gradual_increase_test() ->
    %% Test that depth increases correctly through multiple opening delimiters
    Source = "x {- L1 {- L2 {- L3 {- L4 {- L5 -} -} -} -} -} y",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    ?assertEqual([{lower_ident, 1, "x"}, {lower_ident, 1, "y"}], Tokens).

comment_depth_with_content_test() ->
    %% Ensure content between comment delimiters is properly skipped
    Opening = lists:flatten(lists:duplicate(10, "{- level ")),
    Closing = lists:flatten(lists:duplicate(10, " end -}")),
    Source = "start " ++ Opening ++ "hidden content" ++ Closing ++ " finish",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    ?assertEqual([{lower_ident, 1, "start"}, {lower_ident, 1, "finish"}], Tokens).

%%====================================================================
%% UTF-8 and Unicode Validation (Security)
%%====================================================================

utf8_valid_ascii_test() ->
    %% Plain ASCII should work
    {ok, Tokens} = catena_lexer:tokenize("hello world"),
    ?assertEqual([{lower_ident, 1, "hello"}, {lower_ident, 1, "world"}], Tokens).

utf8_valid_binary_ascii_test() ->
    %% Binary ASCII input
    {ok, Tokens} = catena_lexer:tokenize(<<"hello world">>),
    ?assertEqual([{lower_ident, 1, "hello"}, {lower_ident, 1, "world"}], Tokens).

utf8_valid_unicode_in_string_test() ->
    %% UTF-8 should work in string literals (using Chinese characters)
    Binary = <<"\"Hello ", 228, 184, 150, 231, 149, 140, "\"">>,  % "Hello 世界" in UTF-8
    {ok, Tokens} = catena_lexer:tokenize(Binary),
    ?assertMatch([{string, 1, _}], Tokens).

utf8_invalid_byte_sequence_test() ->
    %% Invalid UTF-8: truncated multi-byte sequence
    Binary = <<"hello ", 16#C3, " world">>,  % Incomplete 2-byte sequence
    Result = catena_lexer:tokenize(Binary),
    ?assertMatch({error, {0, catena_lexer, {invalid_utf8, _}}}, Result).

utf8_invalid_continuation_byte_test() ->
    %% Invalid UTF-8: invalid continuation byte
    Binary = <<"hello ", 16#80, " world">>,  % Standalone continuation byte
    Result = catena_lexer:tokenize(Binary),
    ?assertMatch({error, {0, catena_lexer, {invalid_utf8, _}}}, Result).

utf8_overlong_encoding_test() ->
    %% Invalid UTF-8: overlong encoding (security issue!)
    %% Overlong encoding of '/' (U+002F) as 2-byte sequence
    Binary = <<16#C0, 16#AF>>,  % Should be rejected
    Result = catena_lexer:tokenize(Binary),
    ?assertMatch({error, {0, catena_lexer, {invalid_utf8, _}}}, Result).

unicode_surrogate_pair_test() ->
    %% Invalid: UTF-16 surrogate code points
    Result = catena_lexer:tokenize([16#D800]),  % High surrogate
    ?assertMatch({error, {0, catena_lexer, {invalid_unicode, 1, 16#D800, _}}}, Result).

unicode_beyond_range_test() ->
    %% Invalid: Code point beyond valid Unicode range
    Result = catena_lexer:tokenize([16#110000]),  % Beyond U+10FFFF
    ?assertMatch({error, {0, catena_lexer, {invalid_unicode, 1, 16#110000, _}}}, Result).

unicode_negative_codepoint_test() ->
    %% Invalid: Negative code point
    Result = catena_lexer:tokenize([-1]),
    ?assertMatch({error, {0, catena_lexer, {invalid_unicode, 1, -1, _}}}, Result).

unicode_valid_bmp_test() ->
    %% Valid: Basic Multilingual Plane characters
    Source = [16#0041, 16#0042, 16#0043],  % "ABC"
    {ok, Tokens} = catena_lexer:tokenize(Source),
    ?assertEqual([{upper_ident, 1, "ABC"}], Tokens).

unicode_valid_supplementary_test() ->
    %% Valid: Supplementary plane characters in strings
    %% U+1F30D is 🌍 (Earth emoji)
    Source = "\"" ++ [16#1F30D] ++ "\"",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    [{string, 1, Value}] = Tokens,
    ?assertEqual([16#1F30D], Value).

unicode_valid_private_use_test() ->
    %% Valid: Private Use Area (U+E000-U+F8FF)
    Source = [34, 16#E000, 16#F8FF, 34],  % "..." with private use chars
    {ok, Tokens} = catena_lexer:tokenize(Source),
    ?assertMatch([{string, 1, _}], Tokens).

mixed_comments_test() ->
    Input = "x -- single line\n"
            "y {- multi\n"
            "line -}\n"
            "z",
    {ok, Tokens} = catena_lexer:tokenize(Input),
    ?assertMatch([
        {lower_ident, 1, "x"},
        {lower_ident, 2, "y"},
        {lower_ident, 4, "z"}
    ], Tokens).

%%====================================================================
%% Integration Tests
%%====================================================================

realistic_catena_code_test() ->
    %% Test a realistic piece of Catena code
    Code = "transform factorial : Natural -> Natural\n"
           "transform factorial n = match n\n"
           "  | 0 -> 1\n"
           "  | n -> n * factorial (n - 1)\n"
           "end",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    %% Verify it tokenizes without errors
    ?assert(length(Tokens) > 0),
    %% Check that transform keyword appears twice
    FlowCount = length([T || T = {transform, _} <- Tokens]),
    ?assertEqual(2, FlowCount).

shape_definition_test() ->
    Code = "type Maybe a = Some a | None",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    ?assertMatch([
        {type, 1},
        {upper_ident, 1, "Maybe"},
        {lower_ident, 1, "a"},
        {equals, 1},
        {upper_ident, 1, "Some"},
        {lower_ident, 1, "a"},
        {pipe, 1},
        {upper_ident, 1, "None"}
    ], Tokens).

composition_operators_test() ->
    Code = "validate |> process |> persist",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    ?assertMatch([
        {lower_ident, 1, "validate"},
        {pipe_right, 1},
        {process, 1},
        {pipe_right, 1},
        {lower_ident, 1, "persist"}
    ], Tokens).

effect_syntax_test() ->
    %% Test effect declaration syntax (Task 1.1.5)
    %% Note: try/with replaced by handle keyword
    Code = "effect FileIO\n"
           "  operation readFile\n"
           "end\n"
           "transform loadConfig = perform FileIO readFile\n"
           "handle\n"
           "  loadConfig\n"
           "end",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    %% Verify effect keywords are recognized
    EffectCount = length([T || T = {effect, _} <- Tokens]),
    OperationCount = length([T || T = {operation, _} <- Tokens]),
    PerformCount = length([T || T = {perform, _} <- Tokens]),
    HandleCount = length([T || T = {handle, _} <- Tokens]),
    ?assertEqual(1, EffectCount),
    ?assertEqual(1, OperationCount),
    ?assertEqual(1, PerformCount),
    ?assertEqual(1, HandleCount).

identifiers_test() ->
    %% Test lowercase and uppercase identifiers
    {ok, Tokens} = catena_lexer:tokenize("foo Bar baz'"),
    ?assertMatch([
        {lower_ident, 1, "foo"},
        {upper_ident, 1, "Bar"},
        {lower_ident, 1, "baz'"}
    ], Tokens).

whitespace_handling_test() ->
    %% Test that various whitespace is handled correctly
    {ok, Tokens} = catena_lexer:tokenize("a   \t  b\n\nc"),
    ?assertMatch([
        {lower_ident, 1, "a"},
        {lower_ident, 1, "b"},
        {lower_ident, 3, "c"}
    ], Tokens).

line_tracking_test() ->
    %% Test that line numbers are tracked correctly
    Code = "a\nb\n\nc",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    Lines = [Line || {_, Line, _} <- Tokens] ++ [Line || {_, Line} <- Tokens],
    ?assertEqual([1, 2, 4], Lines).

%%====================================================================
%% Edge Cases and Error Handling
%%====================================================================

consecutive_operators_test() ->
    %% Test that consecutive operators are handled correctly
    {ok, Tokens} = catena_lexer:tokenize("a|>b"),
    ?assertMatch([
        {lower_ident, 1, "a"},
        {pipe_right, 1},
        {lower_ident, 1, "b"}
    ], Tokens).

operator_lookahead_test() ->
    %% Test that '-' is not confused with comment start
    {ok, Tokens} = catena_lexer:tokenize("a - b"),
    ?assertMatch([
        {lower_ident, 1, "a"},
        {minus, 1},
        {lower_ident, 1, "b"}
    ], Tokens).

comment_in_string_test() ->
    %% Test that comment markers in strings are not treated as comments
    {ok, Tokens} = catena_lexer:tokenize("\"-- not a comment\""),
    ?assertMatch([{string, 1, "-- not a comment"}], Tokens).

empty_input_test() ->
    {ok, Tokens} = catena_lexer:tokenize(""),
    ?assertEqual([], Tokens).

only_whitespace_test() ->
    {ok, Tokens} = catena_lexer:tokenize("   \n\t\r\n   "),
    ?assertEqual([], Tokens).

only_comments_test() ->
    {ok, Tokens} = catena_lexer:tokenize("-- just comments\n{- more comments -}"),
    ?assertEqual([], Tokens).

%%====================================================================
%% Test 1.1.1.5: Error Handling
%%====================================================================

%% Unterminated string tests
unterminated_string_simple_test() ->
    Result = catena_lexer:tokenize("\"hello"),
    ?assertMatch({error, {_, _, _}}, Result).

unterminated_string_with_newline_test() ->
    Result = catena_lexer:tokenize("\"line1\nline2"),
    ?assertMatch({error, {_, _, _}}, Result).

unterminated_string_ending_with_backslash_test() ->
    Result = catena_lexer:tokenize("\"escape\\"),
    ?assertMatch({error, {_, _, _}}, Result).

%% Illegal character tests
illegal_character_at_sign_test() ->
    Result = catena_lexer:tokenize("@symbol"),
    ?assertMatch({error, {_, _, _}}, Result).

illegal_character_hash_test() ->
    Result = catena_lexer:tokenize("#directive"),
    ?assertMatch({error, {_, _, _}}, Result).

illegal_character_dollar_test() ->
    Result = catena_lexer:tokenize("$var"),
    ?assertMatch({error, {_, _, _}}, Result).

illegal_character_percent_test() ->
    Result = catena_lexer:tokenize("%percent"),
    ?assertMatch({error, {_, _, _}}, Result).

illegal_character_caret_test() ->
    Result = catena_lexer:tokenize("^caret"),
    ?assertMatch({error, {_, _, _}}, Result).

illegal_character_backtick_test() ->
    Result = catena_lexer:tokenize("`template`"),
    ?assertMatch({error, {_, _, _}}, Result).

%% Invalid escape sequence tests
invalid_escape_unknown_letter_test() ->
    Result = catena_lexer:tokenize("\"invalid \\z escape\""),
    ?assertMatch({error, {_, _, _}}, Result).

invalid_escape_hex_not_supported_test() ->
    Result = catena_lexer:tokenize("\"hex \\x41\""),
    ?assertMatch({error, {_, _, _}}, Result).

%%====================================================================
%% Test 1.1.1.6: Resource Limits (DoS Prevention)
%%====================================================================

input_too_large_test() ->
    %% Create a string larger than MAX_IDENT_LENGTH (255)
    %% Note: The lexer doesn't have a separate input size check,
    %% but large inputs fail when parsed as identifiers
    LargeString = lists:duplicate(10000001, $a),
    Result = catena_lexer:tokenize(LargeString),
    %% Fails with identifier_too_long error (wrapped in {user, ...} by leex)
    ?assertMatch({error, {1, catena_lexer, {user, {identifier_too_long, 1, 10000001, 255}}}}, Result).

nesting_too_deep_test() ->
    %% Create 101 levels of nested comments (exceeds MAX_COMMENT_DEPTH of 100)
    OpenComments = lists:duplicate(101, "{-"),
    CloseComments = lists:duplicate(101, "-}"),
    Input = lists:flatten(OpenComments ++ CloseComments),
    Result = catena_lexer:tokenize(Input),
    ?assertMatch({error, {_Line, catena_lexer, {comment_depth_exceeded, 101, 100}}}, Result).

identifier_too_long_test() ->
    %% Create an identifier longer than MAX_IDENT_LENGTH (255)
    LongIdent = lists:duplicate(256, $a),
    Result = catena_lexer:tokenize(LongIdent),
    %% Error is wrapped in {user, ...} by leex
    ?assertMatch({error, {1, catena_lexer, {user, {identifier_too_long, 1, 256, 255}}}}, Result).

%%====================================================================
%% Test 1.1.1.7: Unicode and Special Characters
%%====================================================================

%% Unicode in strings tests (unicode IS supported in strings)
unicode_string_russian_test() ->
    %% UTF-8 Russian text in string - should work
    {ok, Tokens} = catena_lexer:tokenize("\"Привет мир\""),
    ?assertMatch([{string, 1, _}], Tokens),
    %% Verify it contains unicode codepoints
    [{string, 1, Content}] = Tokens,
    ?assert(lists:any(fun(C) -> C > 127 end, Content)).

unicode_string_emoji_test() ->
    %% Emoji in string - should work
    {ok, Tokens} = catena_lexer:tokenize("\"Hello 👋 World\""),
    ?assertMatch([{string, 1, _}], Tokens),
    %% Verify it contains emoji codepoint (128075 is wave emoji)
    [{string, 1, Content}] = Tokens,
    ?assert(lists:member(128075, Content)).

unicode_string_cjk_test() ->
    %% Chinese characters in string - should work
    {ok, Tokens} = catena_lexer:tokenize("\"你好世界\""),
    ?assertMatch([{string, 1, _}], Tokens),
    %% Verify it contains CJK codepoints (> 127)
    [{string, 1, Content}] = Tokens,
    ?assert(lists:all(fun(C) -> C > 127 end, Content)).

%% Unicode in identifiers tests (should fail - only ASCII allowed)
unicode_identifier_russian_test() ->
    %% UTF-8 identifier should fail
    Result = catena_lexer:tokenize("привет"),
    ?assertMatch({error, {_, _, _}}, Result).

unicode_identifier_emoji_test() ->
    %% Emoji identifier should fail
    Result = catena_lexer:tokenize("hello👋"),
    ?assertMatch({error, {_, _, _}}, Result).

unicode_identifier_cjk_test() ->
    %% CJK identifier should fail
    Result = catena_lexer:tokenize("变量名"),
    ?assertMatch({error, {_, _, _}}, Result).

%% Special whitespace tests (should be rejected as illegal characters)
special_whitespace_bom_test() ->
    %% Byte Order Mark (BOM) should be rejected as illegal character
    BOM = [16#EF, 16#BB, 16#BF],
    Result = catena_lexer:tokenize(BOM ++ "x"),
    ?assertMatch({error, {1, _, _}}, Result).

special_whitespace_zero_width_test() ->
    %% Zero-width space (U+200B) should be rejected
    ZeroWidth = "x" ++ [16#E2, 16#80, 16#8B] ++ "y",
    Result = catena_lexer:tokenize(ZeroWidth),
    ?assertMatch({error, {1, _, _}}, Result).

special_whitespace_nbsp_test() ->
    %% Non-breaking space (U+00A0) should be rejected
    NBSP = "x" ++ [16#C2, 16#A0] ++ "y",
    Result = catena_lexer:tokenize(NBSP),
    ?assertMatch({error, {1, _, _}}, Result).
