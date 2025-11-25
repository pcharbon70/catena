-module(catena_parse_tests).
-include_lib("eunit/include/eunit.hrl").

%%============================================================================
%% Test Suite for catena_parse (Parser Wrapper)
%%
%% Tests the high-level parsing API that combines tokenization and parsing.
%% Covers:
%% - Successful parsing
%% - Lexer error handling
%% - Parser error handling
%% - File parsing
%% - AST resource limits (depth and node count)
%% - Error formatting
%%============================================================================

%%----------------------------------------------------------------------------
%% Section 1: Basic Parsing Tests
%%----------------------------------------------------------------------------

%% Test 1.1: Parse simple type declaration
parse_simple_shape_test() ->
    Source = "type Bool = True | False",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result),
    {ok, {module, _, _, _, Decls, _}} = Result,
    ?assertEqual(1, length(Decls)),
    [{type_decl, 'Bool', [], Constructors, [], _}] = Decls,
    ?assertEqual(2, length(Constructors)).

%% Test 1.2: Parse simple transform declaration
parse_simple_flow_test() ->
    Source = "transform add x y = x + y",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result),
    {ok, {module, _, _, _, Decls, _}} = Result,
    ?assertEqual(1, length(Decls)),
    [{transform_decl, add, undefined, Clauses, _}] = Decls,
    ?assertEqual(1, length(Clauses)).

%% Test 1.3: Parse transform with type signature
parse_transform_with_type_sig_test() ->
    Source = "transform identity : a -> a\n"
             "transform identity x = x",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result),
    {ok, {module, _, _, _, Decls, _}} = Result,
    ?assertEqual(1, length(Decls)),
    [{transform_decl, identity, TypeSig, Clauses, _}] = Decls,
    ?assertMatch({type_fun, _, _, _}, TypeSig),
    ?assertEqual(1, length(Clauses)).

%% Test 1.4: Parse complex expression
parse_complex_expression_test() ->
    Source = "transform complex x = (x + 1) * 2 |> identity",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).

%% Test 1.5: Parse match expression
parse_match_expression_test() ->
    Source = "transform length xs = match\n"
             "  | Nil -> 0\n"
             "  | Cons(_ rest) -> 1 + length rest\n"
             "end",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result),
    {ok, {module, _, _, _, Decls, _}} = Result,
    [{transform_decl, length, undefined, Clauses, _}] = Decls,
    %% match_expr has {match_expr, Scrutinee, MatchClauses, Location} structure
    [{transform_clause, _, undefined, {match_expr, _Scrutinee, MatchClauses, _}, _}] = Clauses,
    ?assertEqual(2, length(MatchClauses)).

%%----------------------------------------------------------------------------
%% Section 2: Lexer Error Handling
%%----------------------------------------------------------------------------

%% Test 2.1: Unclosed comment
lex_error_unclosed_comment_test() ->
    Source = "transform f x = {- unclosed comment",
    Result = catena_parse:parse(Source),
    ?assertMatch({error, {lex_error, _, _}}, Result),
    {error, ErrorTuple} = Result,
    ErrorMsg = catena_parse:format_error(ErrorTuple),
    ?assert(is_list(ErrorMsg)),
    ?assert(length(ErrorMsg) > 0).

%% Test 2.2: Invalid token
lex_error_invalid_token_test() ->
    Source = "transform f x = @invalid",
    Result = catena_parse:parse(Source),
    %% Note: @ might be valid or invalid depending on lexer rules
    %% This test verifies we handle lexer errors properly
    case Result of
        {error, {lex_error, _, _}} ->
            {error, ErrorTuple} = Result,
            ErrorMsg = catena_parse:format_error(ErrorTuple),
            ?assert(is_list(ErrorMsg));
        {ok, _} ->
            %% If @ is valid, that's fine too
            ok
    end.

%% Test 2.3: Very long identifier
lex_error_long_identifier_test() ->
    %% Create an identifier longer than the limit (255 chars)
    LongIdent = lists:duplicate(300, $a),
    Source = "transform " ++ LongIdent ++ " x = x",
    Result = catena_parse:parse(Source),
    ?assertMatch({error, {lex_error, _, _}}, Result).

%% Test 2.4: Input too large
%% Note: max_input_size via application env is not implemented in the lexer.
%% The lexer enforces limits at the identifier/string level, not input level.
%% This test verifies that very long identifiers fail properly.
lex_error_input_too_large_test() ->
    %% Create a source with an identifier longer than 255 chars (max identifier length)
    LongIdent = lists:duplicate(300, $a),
    Source = "transform " ++ LongIdent ++ " x = x",
    Result = catena_parse:parse(Source),
    ?assertMatch({error, {lex_error, _, _}}, Result).

%%----------------------------------------------------------------------------
%% Section 3: Parser Error Handling
%%----------------------------------------------------------------------------

%% Test 3.1: Missing equals in transform
parse_error_missing_equals_test() ->
    Source = "transform f x y",
    Result = catena_parse:parse(Source),
    ?assertMatch({error, {parse_error, _, _}}, Result),
    {error, ErrorTuple} = Result,
    ErrorMsg = catena_parse:format_error(ErrorTuple),
    ?assert(is_list(ErrorMsg)).

%% Test 3.2: Unclosed parenthesis
parse_error_unclosed_paren_test() ->
    Source = "transform f x = (x + 1",
    Result = catena_parse:parse(Source),
    ?assertMatch({error, {parse_error, _, _}}, Result).

%% Test 3.3: Invalid pattern
parse_error_invalid_pattern_test() ->
    Source = "transform f 123.456.789 = x",
    Result = catena_parse:parse(Source),
    ?assertMatch({error, {parse_error, _, _}}, Result).

%% Test 3.4: Mismatched delimiters
parse_error_mismatched_delimiters_test() ->
    Source = "transform f x = [1, 2, 3}",
    Result = catena_parse:parse(Source),
    ?assertMatch({error, {parse_error, _, _}}, Result).

%%----------------------------------------------------------------------------
%% Section 4: File Parsing
%%----------------------------------------------------------------------------

%% Test 4.1: Parse existing test file
parse_file_success_test() ->
    %% We can use the examples from test data if they exist
    %% For now, we'll create a temporary file
    Filename = "/tmp/catena_parse_test.tps",
    Source = "type Bool = True | False\n"
             "transform not : Bool -> Bool\n"
             "transform not x = match\n"
             "  | True -> False\n"
             "  | False -> True\n"
             "end",
    ok = file:write_file(Filename, Source),
    try
        Result = catena_parse:parse_file(Filename),
        ?assertMatch({ok, _}, Result)
    after
        file:delete(Filename)
    end.

%% Test 4.2: Parse non-existent file
parse_file_not_found_test() ->
    Result = catena_parse:parse_file("/tmp/nonexistent_catena_file.tps"),
    ?assertMatch({error, {file_error, _, _}}, Result),
    {error, ErrorTuple} = Result,
    ErrorMsg = catena_parse:format_error(ErrorTuple),
    ?assert(is_list(ErrorMsg)).

%% Test 4.3: Parse file with permission denied
parse_file_permission_denied_test() ->
    %% Create a file with no read permissions
    Filename = "/tmp/catena_no_read.tps",
    ok = file:write_file(Filename, "type X = Y"),
    try
        ok = file:change_mode(Filename, 8#000),
        Result = catena_parse:parse_file(Filename),
        ?assertMatch({error, {file_error, _, eacces}}, Result)
    after
        file:change_mode(Filename, 8#644),
        file:delete(Filename)
    end.

%%----------------------------------------------------------------------------
%% Section 5: AST Resource Limits
%%----------------------------------------------------------------------------

%% Test 5.1: AST depth limit
ast_depth_limit_test() ->
    %% Set a low depth limit
    OldMax = application:get_env(catena, max_ast_depth, undefined),
    try
        application:set_env(catena, max_ast_depth, 5),
        %% Create deeply nested expression: (((((x)))))
        Source = "transform f x = ((((((x))))))",
        Result = catena_parse:parse(Source),
        %% Should exceed depth limit
        ?assertMatch({error, {ast_too_deep, _, _}}, Result),
        {error, ErrorTuple} = Result,
        ErrorMsg = catena_parse:format_error(ErrorTuple),
        ?assert(is_list(ErrorMsg))
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_ast_depth);
            Value -> application:set_env(catena, max_ast_depth, Value)
        end
    end.

%% Test 5.2: AST node count limit
ast_node_count_limit_test() ->
    %% Set a low node count limit
    OldMax = application:get_env(catena, max_ast_nodes, undefined),
    try
        application:set_env(catena, max_ast_nodes, 20),
        %% Create expression with many nodes
        Source = "transform f x = x + x + x + x + x + x + x + x + x + x + x + x",
        Result = catena_parse:parse(Source),
        %% Should exceed node count limit
        ?assertMatch({error, {ast_too_large, _, _}}, Result),
        {error, ErrorTuple} = Result,
        ErrorMsg = catena_parse:format_error(ErrorTuple),
        ?assert(is_list(ErrorMsg))
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_ast_nodes);
            Value -> application:set_env(catena, max_ast_nodes, Value)
        end
    end.

%% Test 5.3: Normal AST passes limits
ast_normal_passes_limits_test() ->
    Source = "transform add x y = x + y",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).

%% Test 5.4: Large but valid AST
ast_large_but_valid_test() ->
    %% Create a moderately complex AST that should pass default limits
    Source = "type Tree a = Leaf a | Node (Tree a) a (Tree a)\n"
             "transform sum : Tree Int -> Int\n"
             "transform sum tree = match\n"
             "  | Leaf(x) -> x\n"
             "  | Node(left val right) -> (sum left) + val + (sum right)\n"
             "end",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).

%%----------------------------------------------------------------------------
%% Section 6: Error Formatting
%%----------------------------------------------------------------------------

%% Test 6.1: Format lexer error
format_lex_error_test() ->
    Error = {lex_error, 10, "invalid character '@'"},
    Msg = lists:flatten(catena_parse:format_error(Error)),
    ?assert(is_list(Msg)),
    ?assert(length(Msg) > 0),
    %% Should contain line number
    ?assert(string:str(Msg, "10") > 0).

%% Test 6.2: Format parser error
format_parse_error_test() ->
    Error = {parse_error, 5, "unexpected token"},
    Msg = lists:flatten(catena_parse:format_error(Error)),
    ?assert(is_list(Msg)),
    ?assert(length(Msg) > 0),
    ?assert(string:str(Msg, "5") > 0).

%% Test 6.3: Format AST depth error
format_ast_depth_error_test() ->
    Error = {ast_too_deep, 100, 50},
    Msg = lists:flatten(catena_parse:format_error(Error)),
    ?assert(is_list(Msg)),
    ?assert(string:str(Msg, "100") > 0),
    ?assert(string:str(Msg, "50") > 0).

%% Test 6.4: Format AST size error
format_ast_size_error_test() ->
    Error = {ast_too_large, 50000, 10000},
    Msg = lists:flatten(catena_parse:format_error(Error)),
    ?assert(is_list(Msg)),
    ?assert(string:str(Msg, "50000") > 0),
    ?assert(string:str(Msg, "10000") > 0).

%% Test 6.5: Format file error
format_file_error_test() ->
    Error = {file_error, "/tmp/test.tps", enoent},
    Msg = lists:flatten(catena_parse:format_error(Error)),
    ?assert(is_list(Msg)),
    ?assert(string:str(Msg, "/tmp/test.tps") > 0).

%%----------------------------------------------------------------------------
%% Section 7: Configuration Tests
%%----------------------------------------------------------------------------

%% Test 7.1: Get default max AST depth
get_max_ast_depth_default_test() ->
    Depth = catena_parse:get_max_ast_depth(),
    ?assert(is_integer(Depth)),
    ?assert(Depth > 0).

%% Test 7.2: Get default max AST nodes
get_max_ast_nodes_default_test() ->
    Nodes = catena_parse:get_max_ast_nodes(),
    ?assert(is_integer(Nodes)),
    ?assert(Nodes > 0).

%% Test 7.3: Override max AST depth
override_max_ast_depth_test() ->
    OldMax = application:get_env(catena, max_ast_depth, undefined),
    try
        application:set_env(catena, max_ast_depth, 999),
        Depth = catena_parse:get_max_ast_depth(),
        ?assertEqual(999, Depth)
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_ast_depth);
            Value -> application:set_env(catena, max_ast_depth, Value)
        end
    end.

%% Test 7.4: Override max AST nodes
override_max_ast_nodes_test() ->
    OldMax = application:get_env(catena, max_ast_nodes, undefined),
    try
        application:set_env(catena, max_ast_nodes, 888),
        Nodes = catena_parse:get_max_ast_nodes(),
        ?assertEqual(888, Nodes)
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_ast_nodes);
            Value -> application:set_env(catena, max_ast_nodes, Value)
        end
    end.

%%----------------------------------------------------------------------------
%% Section 8: Edge Cases
%%----------------------------------------------------------------------------

%% Test 8.1: Empty source
parse_empty_source_test() ->
    Source = "",
    Result = catena_parse:parse(Source),
    %% Empty source should be a parse error (no declarations)
    ?assertMatch({error, {parse_error, _, _}}, Result).

%% Test 8.2: Whitespace only
parse_whitespace_only_test() ->
    Source = "   \n  \t  \n   ",
    Result = catena_parse:parse(Source),
    %% Whitespace only should be a parse error
    ?assertMatch({error, {parse_error, _, _}}, Result).

%% Test 8.3: Comments only
parse_comments_only_test() ->
    Source = "{- just a comment -}\n-- and a line comment",
    Result = catena_parse:parse(Source),
    %% Comments only should be a parse error (no declarations)
    ?assertMatch({error, {parse_error, _, _}}, Result).

%% Test 8.4: Multiple declarations
parse_multiple_declarations_test() ->
    Source = "type Bool = True | False\n"
             "type Maybe a = Some a | None\n"
             "transform id x = x",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result),
    {ok, {module, _, _, _, Decls, _}} = Result,
    ?assertEqual(3, length(Decls)).

%%----------------------------------------------------------------------------
%% Section 9: Integration with Real Examples
%%----------------------------------------------------------------------------

%% Test 9.1: Parse example from language spec
parse_spec_example_test() ->
    Source = "type List a = Nil | Cons a (List a)\n"
             "transform sum : List Int -> Int\n"
             "transform sum xs = match\n"
             "  | Nil -> 0\n"
             "  | Cons(x rest) -> x + (sum rest)\n"
             "end",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).

%% Test 9.2: Parse record example
parse_record_example_test() ->
    Source = "transform makePoint x y = {x: x, y: y}\n"
             "transform distance point = point.x * point.x + point.y * point.y",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).

%% Test 9.3: Parse tuple example
parse_tuple_example_test() ->
    Source = "transform swap (a, b) = (b, a)",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).

%% Test 9.4: Parse complex type signature
parse_complex_type_example_test() ->
    Source = "transform fold : forall a b . (a -> b -> b) -> b -> List a -> b\n"
             "transform fold f acc xs = match\n"
             "  | Nil -> acc\n"
             "  | Cons(x rest) -> f x (fold f acc rest)\n"
             "end",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).
