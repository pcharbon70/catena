%% @doc Parser tests for Testing Framework (Phase 2.3)
%%
%% These tests verify that the parser correctly handles test and property
%% declaration syntax.
-module(catena_parser_testing_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Declaration Parsing Tests
%%====================================================================

parse_simple_test_decl_test() ->
    Code = "test \"simple test\" = true",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, Decls, _} = AST,
    ?assertMatch([{test_decl, "simple test", _, _}], Decls).

parse_test_with_expression_test() ->
    Code = "test \"arithmetic\" = 2 + 2 == 4",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [TestDecl], _} = AST,
    {test_decl, Name, Body, _} = TestDecl,
    ?assertEqual("arithmetic", Name),
    ?assertMatch({binary_op, eq, _, _, _}, Body).

parse_test_with_comparison_test() ->
    Code = "test \"comparison\" = 5 > 3",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [TestDecl], _} = AST,
    {test_decl, Name, Body, _} = TestDecl,
    ?assertEqual("comparison", Name),
    ?assertMatch({binary_op, gt, _, _, _}, Body).

parse_test_with_list_test() ->
    Code = "test \"list test\" = [1, 2, 3] == [1, 2, 3]",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [TestDecl], _} = AST,
    {test_decl, Name, Body, _} = TestDecl,
    ?assertEqual("list test", Name),
    ?assertMatch({binary_op, eq, _, _, _}, Body).

parse_test_with_function_call_test() ->
    Code = "test \"with function\" = identity true",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [TestDecl], _} = AST,
    {test_decl, Name, Body, _} = TestDecl,
    ?assertEqual("with function", Name),
    ?assertMatch({app, _, _, _}, Body).

parse_multiple_tests_test() ->
    Code = "test \"first\" = true\ntest \"second\" = false",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, Decls, _} = AST,
    ?assertEqual(2, length(Decls)),
    [{test_decl, "first", _, _}, {test_decl, "second", _, _}] = Decls.

%%====================================================================
%% Property Declaration Parsing Tests
%%====================================================================

parse_simple_property_test() ->
    Code = "property \"always true\" = forall x : Bool. true",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [PropDecl], _} = AST,
    ?assertMatch({property_decl, "always true", _, _}, PropDecl).

parse_property_forall_body_test() ->
    Code = "property \"boolean identity\" = forall x : Bool. x == x",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [{property_decl, Name, Body, _}], _} = AST,
    ?assertEqual("boolean identity", Name),
    ?assertMatch({property_forall, _, _, _}, Body).

parse_property_single_binding_test() ->
    Code = "property \"natural test\" = forall n : Natural. n >= 0",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [{property_decl, _, {property_forall, Bindings, _, _}, _}], _} = AST,
    ?assertEqual([{n, 'Natural'}], Bindings).

parse_property_multiple_bindings_test() ->
    Code = "property \"commutative\" = forall x : Natural, y : Natural. x + y == y + x",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [{property_decl, _, {property_forall, Bindings, _, _}, _}], _} = AST,
    ?assertEqual([{x, 'Natural'}, {y, 'Natural'}], Bindings).

parse_property_with_list_binding_test() ->
    Code = "property \"list length\" = forall xs : List. length xs >= 0",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [{property_decl, _, {property_forall, Bindings, _, _}, _}], _} = AST,
    ?assertEqual([{xs, 'List'}], Bindings).

parse_property_body_expression_test() ->
    Code = "property \"plus associative\" = forall a : Natural, b : Natural, c : Natural. (a + b) + c == a + (b + c)",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [{property_decl, _, {property_forall, Bindings, Expr, _}, _}], _} = AST,
    ?assertEqual(3, length(Bindings)),
    ?assertMatch({binary_op, eq, _, _, _}, Expr).

%%====================================================================
%% Mixed Declaration Tests
%%====================================================================

parse_test_and_transform_test() ->
    Code = "transform inc x = x + 1\ntest \"inc works\" = inc 5 == 6",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, Decls, _} = AST,
    ?assertEqual(2, length(Decls)),
    ?assertMatch({transform_decl, _, _, _, _}, hd(Decls)),
    ?assertMatch({test_decl, _, _, _}, lists:last(Decls)).

parse_test_and_property_test() ->
    Code = "test \"unit test\" = true\nproperty \"prop test\" = forall x : Bool. x == x",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, Decls, _} = AST,
    ?assertEqual(2, length(Decls)),
    ?assertMatch({test_decl, _, _, _}, hd(Decls)),
    ?assertMatch({property_decl, _, _, _}, lists:last(Decls)).

parse_type_transform_test_property_test() ->
    Code = "type Bool = True | False\n"
           "transform not b = match b of | True -> False | False -> True end\n"
           "test \"not true\" = not True == False\n"
           "property \"not involutive\" = forall b : Bool. not (not b) == b",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, Decls, _} = AST,
    ?assertEqual(4, length(Decls)),
    [TypeDecl, TransformDecl, TestDecl, PropDecl] = Decls,
    ?assertMatch({type_decl, 'Bool', _, _, _, _}, TypeDecl),
    ?assertMatch({transform_decl, _, _, _, _}, TransformDecl),
    ?assertMatch({test_decl, "not true", _, _}, TestDecl),
    ?assertMatch({property_decl, "not involutive", _, _}, PropDecl).

%%====================================================================
%% Edge Cases
%%====================================================================

parse_test_with_escaped_string_test() ->
    Code = "test \"test with \\\"quotes\\\"\" = true",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [{test_decl, Name, _, _}], _} = AST,
    %% String should include escaped quotes
    ?assertEqual("test with \"quotes\"", Name).

parse_property_with_bool_generator_test() ->
    Code = "property \"bool id\" = forall b : Bool. b == b",
    {ok, Tokens} = catena_lexer:tokenize(Code),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [{property_decl, _, {property_forall, [{b, 'Bool'}], _, _}, _}], _} = AST,
    ok.
