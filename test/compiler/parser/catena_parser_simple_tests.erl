-module(catena_parser_simple_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test 1.1.2.1: Shape Declaration Parsing
%%====================================================================

parse_simple_shape_test() ->
    Tokens = [
        {shape, 1},
        {upper_ident, 1, "Bool"},
        {equals, 1},
        {upper_ident, 1, "True"},
        {pipe, 1},
        {upper_ident, 1, "False"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    ?assertMatch({module, undefined, [], [], [_], _}, Result),
    {module, _, _, _, [ShapeDecl], _} = Result,
    ?assertMatch({shape_decl, 'Bool', [], [_, _], [], _}, ShapeDecl).

%%====================================================================
%% Test 1.1.2.2: Flow Declaration Parsing
%%====================================================================

parse_simple_flow_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "id"},
        {equals, 1},
        {integer, 1, 42}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    ?assertMatch({module, undefined, [], [], [_], _}, Result),
    {module, _, _, _, [FlowDecl], _} = Result,
    ?assertMatch({flow_decl, id, undefined, [{flow_clause, [], undefined, _, _}], _}, FlowDecl).

%%====================================================================
%% Test 1.1.2.3: Expression Parsing
%%====================================================================

parse_literal_integer_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "x"},
        {equals, 1},
        {integer, 1, 123}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, _, _, [{flow_clause, _, _, Expr, _}], _} = FlowDecl,
    ?assertMatch({literal, 123, integer, _}, Expr).

parse_literal_float_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "pi"},
        {equals, 1},
        {float, 1, 3.14}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, _, _, [{flow_clause, _, _, Expr, _}], _} = FlowDecl,
    ?assertMatch({literal, 3.14, float, _}, Expr).

parse_literal_string_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "greeting"},
        {equals, 1},
        {string, 1, "Hello"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, _, _, [{flow_clause, _, _, Expr, _}], _} = FlowDecl,
    ?assertMatch({literal, "Hello", string, _}, Expr).

parse_variable_test() ->
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "y"},
        {equals, 1},
        {lower_ident, 1, "x"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, _, _, [{flow_clause, _, _, Expr, _}], _} = FlowDecl,
    ?assertMatch({var, x, _}, Expr).

%%====================================================================
%% Test 1.1.2.4: Multiple Declarations
%%====================================================================

parse_multiple_declarations_test() ->
    Tokens = [
        {shape, 1},
        {upper_ident, 1, "Nat"},
        {equals, 1},
        {upper_ident, 1, "Zero"},
        {pipe, 1},
        {upper_ident, 1, "Succ"},

        {flow, 2},
        {lower_ident, 2, "zero"},
        {equals, 2},
        {integer, 2, 0}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, Decls, _} = Result,
    ?assertEqual(2, length(Decls)),
    [ShapeDecl, FlowDecl] = Decls,
    ?assertMatch({shape_decl, 'Nat', _, _, _, _}, ShapeDecl),
    ?assertMatch({flow_decl, zero, _, _, _}, FlowDecl).
