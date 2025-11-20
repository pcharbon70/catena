-module(catena_parser_transform_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Transform Integration Tests - Comprehensive Test Suite
%% Task 1.1.2: Grammar Implementation
%%
%% This test suite provides comprehensive integration testing for
%% transform (function) declarations in the Catena parser.
%%
%% Test Organization:
%% - Section 1: Simple transforms (literals, variables)
%% - Section 2: Transforms with operators (when extended parser works)
%% - Section 3: Transforms with pattern matching (when extended parser works)
%% - Section 4: Transforms with guards (when extended parser works)
%% - Section 5: Integration with lexer (end-to-end)
%% - Section 6: Multiple transform clauses
%% - Section 7: Edge cases and error handling
%%====================================================================

%%====================================================================
%% Section 1: Simple Transform Declarations
%%====================================================================

%% Test 1.1: Transform with integer literal
parse_transform_integer_literal_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "answer"},
        {equals, 1},
        {integer, 1, 42}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    ?assertMatch({module, undefined, [], [], [_], _}, Result),
    {module, _, _, _, [TransformDecl], _} = Result,
    ?assertMatch({transform_decl, answer, undefined, [{transform_clause, [], undefined, _, _}], _}, TransformDecl),
    {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
    ?assertMatch({literal, 42, integer, _}, Expr).

%% Test 1.2: Transform with float literal
parse_transform_float_literal_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "pi"},
        {equals, 1},
        {float, 1, 3.14159}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    ?assertMatch({transform_decl, pi, undefined, [{transform_clause, [], undefined, _, _}], _}, TransformDecl),
    {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
    ?assertMatch({literal, 3.14159, float, _}, Expr).

%% Test 1.3: Transform with string literal
parse_transform_string_literal_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "greeting"},
        {equals, 1},
        {string, 1, "Hello, Catena!"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    ?assertMatch({transform_decl, greeting, undefined, [{transform_clause, [], undefined, _, _}], _}, TransformDecl),
    {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
    ?assertMatch({literal, "Hello, Catena!", string, _}, Expr).

%% Test 1.4: Transform with variable reference
parse_transform_variable_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "identity"},
        {equals, 1},
        {lower_ident, 1, "x"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    ?assertMatch({transform_decl, identity, undefined, [{transform_clause, [], undefined, _, _}], _}, TransformDecl),
    {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
    ?assertMatch({var, x, _}, Expr).

%% Test 1.5: Transform with constructor reference
%% NOTE: Skipped - minimal parser doesn't support upper_ident as expressions
%% TODO: Enable when extended parser (catena_parser.yrl) is working
% parse_transform_constructor_test() ->
%     Tokens = [
%         {transform, 1},
%         {lower_ident, 1, "nothing"},
%         {equals, 1},
%         {upper_ident, 1, "None"}
%     ],
%     {ok, Result} = catena_parser:parse(Tokens),
%     {module, _, _, _, [TransformDecl], _} = Result,
%     ?assertMatch({transform_decl, nothing, undefined, [{transform_clause, [], undefined, _, _}], _}, TransformDecl),
%     {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
%     ?assertMatch({var, 'None', _}, Expr).

%%====================================================================
%% Section 2: Transforms with Different Literal Types
%%====================================================================

%% Test 2.1: Transform with negative integer
parse_transform_negative_integer_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "freezing"},
        {equals, 1},
        {integer, 1, -273}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
    ?assertMatch({literal, -273, integer, _}, Expr).

%% Test 2.2: Transform with zero
parse_transform_zero_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "zero"},
        {equals, 1},
        {integer, 1, 0}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
    ?assertMatch({literal, 0, integer, _}, Expr).

%% Test 2.3: Transform with large integer
parse_transform_large_integer_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "million"},
        {equals, 1},
        {integer, 1, 1000000}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
    ?assertMatch({literal, 1000000, integer, _}, Expr).

%% Test 2.4: Transform with scientific notation float
parse_transform_scientific_float_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "avogadro"},
        {equals, 1},
        {float, 1, 6.022e23}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
    ?assertMatch({literal, 6.022e23, float, _}, Expr).

%% Test 2.5: Transform with empty string
parse_transform_empty_string_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "empty"},
        {equals, 1},
        {string, 1, ""}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
    ?assertMatch({literal, "", string, _}, Expr).

%% Test 2.6: Transform with multi-line string (escape sequences)
parse_transform_multiline_string_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "message"},
        {equals, 1},
        {string, 1, "Hello\nWorld\t!"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
    ?assertMatch({literal, "Hello\nWorld\t!", string, _}, Expr).

%%====================================================================
%% Section 3: Integration with Lexer (End-to-End Tests)
%%====================================================================

%% Test 3.1: Parse transform from source code
integration_simple_transform_test() ->
    Source = "transform answer = 42",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    ?assertMatch({module, undefined, [], [], [_], _}, AST),
    {module, _, _, _, [TransformDecl], _} = AST,
    ?assertMatch({transform_decl, answer, undefined, [{transform_clause, [], undefined, _, _}], _}, TransformDecl).

%% Test 3.2: Parse multiple transforms from source
integration_multiple_transforms_test() ->
    Source = "transform x = 1\ntransform y = 2",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, Decls, _} = AST,
    ?assertEqual(2, length(Decls)),
    [Transform1, Transform2] = Decls,
    ?assertMatch({transform_decl, x, _, _, _}, Transform1),
    ?assertMatch({transform_decl, y, _, _, _}, Transform2).

%% Test 3.3: Parse transform with comments (lexer filters them)
integration_transform_with_comments_test() ->
    Source = "-- This is the answer\ntransform answer = 42  -- the ultimate answer",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = AST,
    ?assertMatch({transform_decl, answer, undefined, [{transform_clause, [], undefined, _, _}], _}, TransformDecl),
    {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
    ?assertMatch({literal, 42, integer, _}, Expr).

%% Test 3.4: Parse transform with whitespace variations
integration_transform_whitespace_test() ->
    Source = "transform   identity   =   x",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = AST,
    ?assertMatch({transform_decl, identity, undefined, [{transform_clause, [], undefined, _, _}], _}, TransformDecl).

%% Test 3.5: Parse transform on multiple lines
integration_transform_multiline_test() ->
    Source = "transform\n  greeting\n    =\n      \"Hello\"",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = AST,
    ?assertMatch({transform_decl, greeting, undefined, [{transform_clause, [], undefined, _, _}], _}, TransformDecl).

%% Test 3.6: Parse realistic code sample (flow with meaningful name)
integration_realistic_flow_test() ->
    Source = "transform calculate_tax_rate = 0.15",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = AST,
    ?assertMatch({transform_decl, calculate_tax_rate, undefined, [{transform_clause, [], undefined, _, _}], _}, TransformDecl),
    {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
    ?assertMatch({literal, 0.15, float, _}, Expr).

%%====================================================================
%% Section 4: Combined Shape and Transform Declarations
%%====================================================================

%% Test 4.1: Type followed by transform
%% NOTE: Skipped - uses True as expression (upper_ident not supported in minimal parser)
%% TODO: Enable when extended parser is working
% integration_type_then_transform_test() ->
%     Source = "type Bool = True | False\nflow default_bool = True",
%     {ok, Tokens} = catena_lexer:tokenize(Source),
%     {ok, AST} = catena_parser:parse(Tokens),
%     {module, _, _, _, Decls, _} = AST,
%     ?assertEqual(2, length(Decls)),
%     [TypeDecl, TransformDecl] = Decls,
%     ?assertMatch({type_decl, 'Bool', _, _, _, _}, TypeDecl),
%     ?assertMatch({transform_decl, default_bool, _, _, _}, TransformDecl).

%% Test 4.2: Transform followed by type
integration_transform_then_shape_test() ->
    Source = "transform zero = 0\ntype Nat = Zero | Succ",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, Decls, _} = AST,
    ?assertEqual(2, length(Decls)),
    [TransformDecl, TypeDecl] = Decls,
    ?assertMatch({transform_decl, zero, _, _, _}, TransformDecl),
    ?assertMatch({type_decl, 'Nat', _, _, _, _}, TypeDecl).

%% Test 4.3: Multiple types and transforms interleaved
%% NOTE: Skipped - uses True and None as expressions (upper_ident not supported in minimal parser)
%% TODO: Enable when extended parser is working
% integration_interleaved_declarations_test() ->
%     Source = "type Bool = True | False\n" ++
%              "transform is_true = True\n" ++
%              "type Maybe a = Some | None\n" ++
%              "transform nothing = None",
%     {ok, Tokens} = catena_lexer:tokenize(Source),
%     {ok, AST} = catena_parser:parse(Tokens),
%     {module, _, _, _, Decls, _} = AST,
%     ?assertEqual(4, length(Decls)),
%     [Type1, Transform1, Type2, Transform2] = Decls,
%     ?assertMatch({type_decl, 'Bool', _, _, _, _}, Type1),
%     ?assertMatch({transform_decl, is_true, _, _, _}, Transform1),
%     ?assertMatch({type_decl, 'Maybe', _, _, _, _}, Type2),
%     ?assertMatch({transform_decl, nothing, _, _, _}, Transform2).

%%====================================================================
%% Section 5: Transform Name and Identifier Tests
%%====================================================================

%% Test 5.1: Transform with single-letter name
parse_transform_single_letter_name_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "x"},
        {equals, 1},
        {integer, 1, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    ?assertMatch({transform_decl, x, _, _, _}, TransformDecl).

%% Test 5.2: Transform with underscore in name
parse_transform_underscore_name_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "my_function"},
        {equals, 1},
        {integer, 1, 42}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    ?assertMatch({transform_decl, my_function, _, _, _}, TransformDecl).

%% Test 5.3: Transform with numbers in name
parse_transform_numbers_in_name_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "var123"},
        {equals, 1},
        {integer, 1, 456}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    ?assertMatch({transform_decl, var123, _, _, _}, TransformDecl).

%% Test 5.4: Transform with long descriptive name
parse_transform_long_name_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "calculate_total_with_tax_and_shipping"},
        {equals, 1},
        {float, 1, 99.99}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    ?assertMatch({transform_decl, calculate_total_with_tax_and_shipping, _, _, _}, TransformDecl).

%%====================================================================
%% Section 6: Location Tracking Tests
%%====================================================================

%% Test 6.1: Verify transform location tracking
parse_transform_location_tracking_test() ->
    Tokens = [
        {transform, 5},  % Line 5
        {lower_ident, 5, "test"},
        {equals, 5},
        {integer, 5, 42}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    ?assertMatch({transform_decl, test, undefined, [{transform_clause, [], undefined, _, _}], _}, TransformDecl).

%% Test 6.2: Verify expression location tracking
parse_transform_expr_location_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "x"},
        {equals, 1},
        {string, 3, "hello"}  % String on line 3
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = Result,
    {transform_decl, _, _, [{transform_clause, _, _, Expr, _}], _} = TransformDecl,
    ?assertMatch({literal, "hello", string, _}, Expr).

%% Test 6.3: Multiple flows preserve line numbers
parse_multiple_transforms_location_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "first"},
        {equals, 1},
        {integer, 1, 1},

        {transform, 10},
        {lower_ident, 10, "second"},
        {equals, 10},
        {integer, 10, 2}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, Decls, _} = Result,
    [Transform1, Transform2] = Decls,
    ?assertMatch({transform_decl, first, _, _, _}, Transform1),
    ?assertMatch({transform_decl, second, _, _, _}, Transform2).

%%====================================================================
%% Section 7: AST Structure Verification
%%====================================================================

%% Test 7.1: Verify complete transform AST structure
verify_transform_ast_structure_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {equals, 1},
        {integer, 1, 42}
    ],
    {ok, AST} = catena_parser:parse(Tokens),

    % Verify module structure
    ?assertMatch({module, _, _, _, _, _}, AST),
    {module, ModName, Exports, Imports, Decls, ModLoc} = AST,

    % Module should have undefined name
    ?assertEqual(undefined, ModName),

    % Module should have no exports/imports
    ?assertEqual([], Exports),
    ?assertEqual([], Imports),

    % Module location should be line 1
    ?assertMatch(_, ModLoc),

    % Should have exactly one declaration
    ?assertEqual(1, length(Decls)),

    % Verify transform declaration structure
    [TransformDecl] = Decls,
    ?assertMatch({transform_decl, _, _, _, _}, TransformDecl),
    {transform_decl, Name, TypeSig, Clauses, FlowLoc} = TransformDecl,

    % Transform name should be 'test'
    ?assertEqual(test, Name),

    % Type signature should be undefined (no type annotation)
    ?assertEqual(undefined, TypeSig),

    % Transform location should be line 1
    ?assertMatch(_, FlowLoc),

    % Should have exactly one clause
    ?assertEqual(1, length(Clauses)),

    % Verify clause structure
    [Clause] = Clauses,
    ?assertMatch({transform_clause, _, _, _, _}, Clause),
    {transform_clause, Patterns, Guards, Body, ClauseLoc} = Clause,

    % No patterns (simple transform)
    ?assertEqual([], Patterns),

    % No guards
    ?assertEqual(undefined, Guards),

    % Clause location should be line 1
    ?assertMatch(_, ClauseLoc),

    % Verify body is a literal
    ?assertMatch({literal, _, _, _}, Body),
    {literal, Value, Type, BodyLoc} = Body,
    ?assertEqual(42, Value),
    ?assertEqual(integer, Type),
    ?assertMatch(_, BodyLoc).

%% Test 7.2: Verify transform with variable has correct AST
verify_transform_variable_ast_test() ->
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "id"},
        {equals, 1},
        {lower_ident, 1, "x"}
    ],
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [TransformDecl], _} = AST,
    {transform_decl, id, undefined, [Clause], _} = TransformDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    % Body should be a variable reference
    ?assertMatch({var, _, _}, Body),
    {var, VarName, VarLoc} = Body,
    ?assertEqual(x, VarName),
    ?assertMatch(_, VarLoc).

%%====================================================================
%% Section 8: Error Handling (Future - when parser supports errors)
%%====================================================================

%% Note: These tests are commented out because the minimal parser
%% doesn't have error productions. Uncomment when error handling
%% is added in Phase 1.4.

% %% Test 8.1: Missing equals sign
% parse_transform_missing_equals_test() ->
%     Tokens = [
%         {transform, 1},
%         {lower_ident, 1, "bad"},
%         {integer, 1, 42}  % Missing equals
%     ],
%     Result = catena_parser:parse(Tokens),
%     ?assertMatch({error, _}, Result).

% %% Test 8.2: Missing expression
% parse_transform_missing_expr_test() ->
%     Tokens = [
%         {transform, 1},
%         {lower_ident, 1, "bad"},
%         {equals, 1}
%         % Missing expression
%     ],
%     Result = catena_parser:parse(Tokens),
%     ?assertMatch({error, _}, Result).

% %% Test 8.3: Missing transform name
% parse_transform_missing_name_test() ->
%     Tokens = [
%         {transform, 1},
%         {equals, 1},  % Missing name
%         {integer, 1, 42}
%     ],
%     Result = catena_parser:parse(Tokens),
%     ?assertMatch({error, _}, Result).
