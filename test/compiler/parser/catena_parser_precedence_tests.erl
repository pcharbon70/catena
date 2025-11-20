-module(catena_parser_precedence_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/compiler/parser/catena_ast.hrl").

%%====================================================================
%% Operator Precedence Tests
%% Comprehensive test suite validating operator precedence table
%%====================================================================

%%--------------------------------------------------------------------
%% Precedence Table (from catena_parser.yrl) - Current as of Task 1.1.7
%%--------------------------------------------------------------------
%% Right   100 arrow (->)
%% Right   160 pipe_right (|>)
%% Nonassoc 300 eq neq setoid_eq setoid_neq (==, /=, ===, !==)
%% Nonassoc 310 lt gt lte gte (<, >, <=, >=)
%% Left    400 plus minus (+, -)
%% Left    500 star slash (*, /)
%%
%% NOTE: bind (>>=) and concat (<>) were removed in Task 1.1.7
%% NOTE: dot (.) record access not yet implemented
%%--------------------------------------------------------------------


%%====================================================================
%% Arithmetic vs Equality Precedence (500/400 vs 300)
%%====================================================================

parse_multiplication_before_equality_test() ->
    %% transform test : Bool
    %% transform test = x * 2 === y * 2
    %% Should parse as: (x * 2) === (y * 2)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {star, 2},
        {integer, 2, 2},
        {setoid_eq, 2},
        {lower_ident, 2, "y"},
        {star, 2},
        {integer, 2, 2}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: (x * 2) === (y * 2)
    ?assertMatch({binary_op, setoid_eq, _, _, _}, Body),
    {binary_op, setoid_eq, Left, Right, _} = Body,
    ?assertMatch({binary_op, star, _, _, _}, Left),
    ?assertMatch({binary_op, star, _, _, _}, Right).

parse_addition_before_equality_test() ->
    %% transform test : Bool
    %% transform test = x + 1 === y + 1
    %% Should parse as: (x + 1) === (y + 1)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {plus, 2},
        {integer, 2, 1},
        {setoid_eq, 2},
        {lower_ident, 2, "y"},
        {plus, 2},
        {integer, 2, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: (x + 1) === (y + 1)
    ?assertMatch({binary_op, setoid_eq, _, _, _}, Body),
    {binary_op, setoid_eq, Left, Right, _} = Body,
    ?assertMatch({binary_op, plus, _, _, _}, Left),
    ?assertMatch({binary_op, plus, _, _, _}, Right).

parse_subtraction_before_equality_test() ->
    %% transform test : Bool
    %% transform test = x - 1 === y - 1
    %% Should parse as: (x - 1) === (y - 1)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {minus, 2},
        {integer, 2, 1},
        {setoid_eq, 2},
        {lower_ident, 2, "y"},
        {minus, 2},
        {integer, 2, 1}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: (x - 1) === (y - 1)
    ?assertMatch({binary_op, setoid_eq, _, _, _}, Body),
    {binary_op, setoid_eq, Left, Right, _} = Body,
    ?assertMatch({binary_op, minus, _, _, _}, Left),
    ?assertMatch({binary_op, minus, _, _, _}, Right).

parse_division_before_equality_test() ->
    %% transform test : Bool
    %% transform test = x / 2 === y / 2
    %% Should parse as: (x / 2) === (y / 2)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {slash, 2},
        {integer, 2, 2},
        {setoid_eq, 2},
        {lower_ident, 2, "y"},
        {slash, 2},
        {integer, 2, 2}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: (x / 2) === (y / 2)
    ?assertMatch({binary_op, setoid_eq, _, _, _}, Body),
    {binary_op, setoid_eq, Left, Right, _} = Body,
    ?assertMatch({binary_op, slash, _, _, _}, Left),
    ?assertMatch({binary_op, slash, _, _, _}, Right).

%%====================================================================
%% Comparison vs Equality Precedence (310 vs 300)
%%====================================================================

parse_comparison_before_equality_test() ->
    %% transform test : Bool
    %% transform test = x < y === z < w
    %% Should parse as: (x < y) === (z < w)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {lt, 2},
        {lower_ident, 2, "y"},
        {setoid_eq, 2},
        {lower_ident, 2, "z"},
        {lt, 2},
        {lower_ident, 2, "w"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: (x < y) === (z < w)
    ?assertMatch({binary_op, setoid_eq, _, _, _}, Body),
    {binary_op, setoid_eq, Left, Right, _} = Body,
    ?assertMatch({binary_op, lt, _, _, _}, Left),
    ?assertMatch({binary_op, lt, _, _, _}, Right).

parse_greater_than_before_equality_test() ->
    %% transform test : Bool
    %% transform test = x > y === z > w
    %% Should parse as: (x > y) === (z > w)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {gt, 2},
        {lower_ident, 2, "y"},
        {setoid_eq, 2},
        {lower_ident, 2, "z"},
        {gt, 2},
        {lower_ident, 2, "w"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: (x > y) === (z > w)
    ?assertMatch({binary_op, setoid_eq, _, _, _}, Body),
    {binary_op, setoid_eq, Left, Right, _} = Body,
    ?assertMatch({binary_op, gt, _, _, _}, Left),
    ?assertMatch({binary_op, gt, _, _, _}, Right).

parse_mixed_comparison_before_equality_test() ->
    %% transform test : Bool
    %% transform test = x <= y === z >= w
    %% Should parse as: (x <= y) === (z >= w)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {lte, 2},
        {lower_ident, 2, "y"},
        {setoid_eq, 2},
        {lower_ident, 2, "z"},
        {gte, 2},
        {lower_ident, 2, "w"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: (x <= y) === (z >= w)
    ?assertMatch({binary_op, setoid_eq, _, _, _}, Body),
    {binary_op, setoid_eq, Left, Right, _} = Body,
    ?assertMatch({binary_op, lte, _, _, _}, Left),
    ?assertMatch({binary_op, gte, _, _, _}, Right).

%%====================================================================
%% Comparison vs Arithmetic Precedence (310 vs 400/500)
%%====================================================================

parse_arithmetic_before_comparison_test() ->
    %% transform test : Bool
    %% transform test = x + 1 < y * 2
    %% Should parse as: (x + 1) < (y * 2)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {plus, 2},
        {integer, 2, 1},
        {lt, 2},
        {lower_ident, 2, "y"},
        {star, 2},
        {integer, 2, 2}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: (x + 1) < (y * 2)
    ?assertMatch({binary_op, lt, _, _, _}, Body),
    {binary_op, lt, Left, Right, _} = Body,
    ?assertMatch({binary_op, plus, _, _, _}, Left),
    ?assertMatch({binary_op, star, _, _, _}, Right).

%%====================================================================
%% Multi-Level Precedence (500 > 400 > 310 > 300)
%%====================================================================

parse_complex_precedence_chain_test() ->
    %% transform test : Bool
    %% transform test = x * 2 + 1 < y - 3 === z / 4 > w
    %% Should parse as: ((x * 2) + 1) < (y - 3)) === ((z / 4) > w)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {star, 2},
        {integer, 2, 2},
        {plus, 2},
        {integer, 2, 1},
        {lt, 2},
        {lower_ident, 2, "y"},
        {minus, 2},
        {integer, 2, 3},
        {setoid_eq, 2},
        {lower_ident, 2, "z"},
        {slash, 2},
        {integer, 2, 4},
        {gt, 2},
        {lower_ident, 2, "w"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Top level: ===
    ?assertMatch({binary_op, setoid_eq, _, _, _}, Body),
    {binary_op, setoid_eq, LeftSide, RightSide, _} = Body,

    %% Left side: (x * 2 + 1) < (y - 3)
    ?assertMatch({binary_op, lt, _, _, _}, LeftSide),
    {binary_op, lt, LeftLt, RightLt, _} = LeftSide,
    ?assertMatch({binary_op, plus, _, _, _}, LeftLt),
    ?assertMatch({binary_op, minus, _, _, _}, RightLt),

    %% Right side: (z / 4) > w
    ?assertMatch({binary_op, gt, _, _, _}, RightSide),
    {binary_op, gt, LeftGt, _, _} = RightSide,
    ?assertMatch({binary_op, slash, _, _, _}, LeftGt).

%%====================================================================
%% Pipe Operator Precedence (160)
%%====================================================================

parse_pipe_before_equality_test() ->
    %% transform test : Bool
    %% transform test = x |> f === y |> g
    %% Note: Pipe (160) is looser than === (300), so parses right-to-left
    %% Actual parse: x |> (f === (y |> g))
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {pipe_right, 2},
        {lower_ident, 2, "f"},
        {setoid_eq, 2},
        {lower_ident, 2, "y"},
        {pipe_right, 2},
        {lower_ident, 2, "g"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Pipe is right-associative and loosest, so: x |> (f === (y |> g))
    ?assertMatch({binary_op, pipe_right, _, _, _}, Body),
    {binary_op, pipe_right, Left, Right, _} = Body,
    ?assertMatch({var, x, _}, Left),
    ?assertMatch({binary_op, pipe_right, _, _, _}, Right).

%%====================================================================
%% Parentheses Override Precedence
%%====================================================================

parse_parentheses_override_arithmetic_test() ->
    %% transform test : Nat
    %% transform test = 2 * (3 + 4)
    %% Should parse as: 2 * (3 + 4), NOT (2 * 3) + 4
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Nat"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {integer, 2, 2},
        {star, 2},
        {lparen, 2},
        {integer, 2, 3},
        {plus, 2},
        {integer, 2, 4},
        {rparen, 2}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: 2 * (3 + 4)
    ?assertMatch({binary_op, star, _, _, _}, Body),
    {binary_op, star, Left, Right, _} = Body,
    ?assertMatch({literal, 2, integer, _}, Left),
    ?assertMatch({binary_op, plus, _, _, _}, Right).

parse_parentheses_override_comparison_test() ->
    %% transform test : Bool
    %% transform test = (x === y) < (z === w)
    %% Should parse as: (x === y) < (z === w)
    %% Parentheses force === to bind before <
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lparen, 2},
        {lower_ident, 2, "x"},
        {setoid_eq, 2},
        {lower_ident, 2, "y"},
        {rparen, 2},
        {lt, 2},
        {lparen, 2},
        {lower_ident, 2, "z"},
        {setoid_eq, 2},
        {lower_ident, 2, "w"},
        {rparen, 2}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: (x === y) < (z === w)
    ?assertMatch({binary_op, lt, _, _, _}, Body),
    {binary_op, lt, Left, Right, _} = Body,
    ?assertMatch({binary_op, setoid_eq, _, _, _}, Left),
    ?assertMatch({binary_op, setoid_eq, _, _, _}, Right).

%%====================================================================
%% Left vs Right Associativity
%%====================================================================

parse_left_associative_addition_test() ->
    %% transform test : Nat
    %% transform test = x + y + z
    %% Should parse as: (x + y) + z (left associative)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Nat"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {plus, 2},
        {lower_ident, 2, "y"},
        {plus, 2},
        {lower_ident, 2, "z"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: (x + y) + z
    ?assertMatch({binary_op, plus, _, _, _}, Body),
    {binary_op, plus, Left, Right, _} = Body,
    ?assertMatch({binary_op, plus, _, _, _}, Left),
    ?assertMatch({var, z, _}, Right).

parse_left_associative_multiplication_test() ->
    %% transform test : Nat
    %% transform test = x * y * z
    %% Should parse as: (x * y) * z (left associative)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Nat"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {star, 2},
        {lower_ident, 2, "y"},
        {star, 2},
        {lower_ident, 2, "z"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: (x * y) * z
    ?assertMatch({binary_op, star, _, _, _}, Body),
    {binary_op, star, Left, Right, _} = Body,
    ?assertMatch({binary_op, star, _, _, _}, Left),
    ?assertMatch({var, z, _}, Right).

parse_right_associative_arrow_test() ->
    %% transform test : a -> (b -> c)
    %% Type arrow is right associative: a -> b -> c means a -> (b -> c)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {lower_ident, 1, "a"},
        {arrow, 1},
        {lower_ident, 1, "b"},
        {arrow, 1},
        {lower_ident, 1, "c"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, TypeSig, _, _} = FlowDecl,

    %% Should be: a -> (b -> c) (right associative)
    ?assertMatch({type_fun, _, _, _}, TypeSig),
    {type_fun, Arg, Ret, _} = TypeSig,
    ?assertMatch({type_var, a, _}, Arg),
    ?assertMatch({type_fun, _, _, _}, Ret),
    {type_fun, InnerArg, InnerRet, _} = Ret,
    ?assertMatch({type_var, b, _}, InnerArg),
    ?assertMatch({type_var, c, _}, InnerRet).

%%====================================================================
%% Mixed Operators at Same Precedence Level
%%====================================================================

parse_mixed_arithmetic_same_level_test() ->
    %% transform test : Nat
    %% transform test = x + y - z
    %% Both + and - have precedence 400, left associative
    %% Should parse as: (x + y) - z
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Nat"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {plus, 2},
        {lower_ident, 2, "y"},
        {minus, 2},
        {lower_ident, 2, "z"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: (x + y) - z
    ?assertMatch({binary_op, minus, _, _, _}, Body),
    {binary_op, minus, Left, Right, _} = Body,
    ?assertMatch({binary_op, plus, _, _, _}, Left),
    ?assertMatch({var, z, _}, Right).

parse_mixed_multiplicative_same_level_test() ->
    %% transform test : Nat
    %% transform test = x * y / z
    %% Both * and / have precedence 500, left associative
    %% Should parse as: (x * y) / z
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Nat"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {star, 2},
        {lower_ident, 2, "y"},
        {slash, 2},
        {lower_ident, 2, "z"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Should be: (x * y) / z
    ?assertMatch({binary_op, slash, _, _, _}, Body),
    {binary_op, slash, Left, Right, _} = Body,
    ?assertMatch({binary_op, star, _, _, _}, Left),
    ?assertMatch({var, z, _}, Right).

%%====================================================================
%% Comprehensive Multi-Operator Chain
%%====================================================================

parse_full_precedence_chain_test() ->
    %% transform test : Bool
    %% transform test = a * b + c - d < e + f === g * h > i
    %% Expected precedence: ((((a * b) + c) - d) < (e + f)) === ((g * h) > i)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "a"},
        {star, 2},
        {lower_ident, 2, "b"},
        {plus, 2},
        {lower_ident, 2, "c"},
        {minus, 2},
        {lower_ident, 2, "d"},
        {lt, 2},
        {lower_ident, 2, "e"},
        {plus, 2},
        {lower_ident, 2, "f"},
        {setoid_eq, 2},
        {lower_ident, 2, "g"},
        {star, 2},
        {lower_ident, 2, "h"},
        {gt, 2},
        {lower_ident, 2, "i"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, _, [Clause], _} = FlowDecl,
    {transform_clause, [], undefined, Body, _} = Clause,

    %% Top level: ===
    ?assertMatch({binary_op, setoid_eq, _, _, _}, Body),
    {binary_op, setoid_eq, LeftEq, RightEq, _} = Body,

    %% Left side of ===: (a * b + c - d) < (e + f)
    ?assertMatch({binary_op, lt, _, _, _}, LeftEq),
    {binary_op, lt, LeftLt, RightLt, _} = LeftEq,

    %% Left of <: ((a * b) + c) - d
    ?assertMatch({binary_op, minus, _, _, _}, LeftLt),
    {binary_op, minus, LeftMinus, _, _} = LeftLt,
    ?assertMatch({binary_op, plus, _, _, _}, LeftMinus),

    %% Right of <: e + f
    ?assertMatch({binary_op, plus, _, _, _}, RightLt),

    %% Right side of ===: (g * h) > i
    ?assertMatch({binary_op, gt, _, _, _}, RightEq),
    {binary_op, gt, LeftGt, _, _} = RightEq,
    ?assertMatch({binary_op, star, _, _, _}, LeftGt).
