-module(catena_parser_negative_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/compiler/parser/catena_ast.hrl").

%%====================================================================
%% Negative Tests - Parser Error Handling
%% These tests verify that the parser correctly rejects invalid input
%%====================================================================

%%--------------------------------------------------------------------
%% Non-Associativity Tests (Type-Level Equality)
%%--------------------------------------------------------------------

parse_setoid_eq_chaining_fails_test() ->
    %% flow test : Bool
    %% flow test = x === y === z
    %% Should fail: === is non-associative
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {flow, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {setoid_eq, 2},
        {lower_ident, 2, "y"},
        {setoid_eq, 2},  %% Second === should cause error
        {lower_ident, 2, "z"}
    ],
    Result = catena_parser:parse(Tokens),
    ?assertMatch({error, _}, Result).

parse_setoid_neq_chaining_fails_test() ->
    %% flow test : Bool
    %% flow test = x !== y !== z
    %% Should fail: !== is non-associative
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {flow, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {setoid_neq, 2},
        {lower_ident, 2, "y"},
        {setoid_neq, 2},  %% Second !== should cause error
        {lower_ident, 2, "z"}
    ],
    Result = catena_parser:parse(Tokens),
    ?assertMatch({error, _}, Result).

parse_mixed_equality_chaining_fails_test() ->
    %% flow test : Bool
    %% flow test = x === y !== z
    %% Should fail: cannot chain different equality operators
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {flow, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {setoid_eq, 2},
        {lower_ident, 2, "y"},
        {setoid_neq, 2},  %% Mixing === and !==
        {lower_ident, 2, "z"}
    ],
    Result = catena_parser:parse(Tokens),
    ?assertMatch({error, _}, Result).

%%--------------------------------------------------------------------
%% Removed Operators - Verification Tests
%%--------------------------------------------------------------------

parse_removed_fmap_operator_test() ->
    %% Verify <$> does not tokenize as single fmap operator
    %% Note: $ is an illegal character, so this will return a lexer error
    Source = "x <$> y",
    Result = catena_lexer:string(Source),

    %% Should return a lexer error (illegal character $)
    ?assertMatch({error, _, _}, Result).

parse_removed_concat_operator_test() ->
    %% Verify <> is tokenized as separate < > tokens
    Source = "x <> y",
    {ok, Tokens, _} = catena_lexer:string(Source),

    %% Should NOT contain a single concat token
    HasConcatToken = lists:any(
        fun({Token, _}) when is_atom(Token) -> Token =:= concat;
           (_) -> false
        end,
        Tokens
    ),
    ?assertEqual(false, HasConcatToken),

    %% Should contain separate < and > tokens
    ?assert(lists:member({lt, 1}, Tokens)),
    ?assert(lists:member({gt, 1}, Tokens)).

parse_removed_bind_operator_test() ->
    %% Verify >>= is tokenized as >> and =
    Source = "x >>= y",
    {ok, Tokens, _} = catena_lexer:string(Source),

    %% Should NOT contain a single bind token
    HasBindToken = lists:any(
        fun({Token, _}) when is_atom(Token) -> Token =:= bind;
           (_) -> false
        end,
        Tokens
    ),
    ?assertEqual(false, HasBindToken).

parse_removed_ap_operator_test() ->
    %% Verify <*> is tokenized as separate tokens
    Source = "x <*> y",
    {ok, Tokens, _} = catena_lexer:string(Source),

    %% Should NOT contain a single ap token
    HasApToken = lists:any(
        fun({Token, _}) when is_atom(Token) -> Token =:= ap;
           (_) -> false
        end,
        Tokens
    ),
    ?assertEqual(false, HasApToken).

parse_removed_kleisli_operators_test() ->
    %% Verify >=> and <=< are tokenized as separate tokens
    Source1 = "x >=> y",
    {ok, Tokens1, _} = catena_lexer:string(Source1),

    HasKleisliLR = lists:any(
        fun({Token, _}) when is_atom(Token) -> Token =:= kleisli_lr;
           (_) -> false
        end,
        Tokens1
    ),
    ?assertEqual(false, HasKleisliLR),

    Source2 = "x <=< y",
    {ok, Tokens2, _} = catena_lexer:string(Source2),

    HasKleisliRL = lists:any(
        fun({Token, _}) when is_atom(Token) -> Token =:= kleisli_rl;
           (_) -> false
        end,
        Tokens2
    ),
    ?assertEqual(false, HasKleisliRL).

%%--------------------------------------------------------------------
%% Invalid Type Signatures
%%--------------------------------------------------------------------

parse_invalid_one_element_tuple_type_test() ->
    %% Type signatures like (a) should parse as parenthesized type, not tuple
    %% This test verifies we don't accidentally create 1-element tuples
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {lparen, 1},
        {lower_ident, 1, "a"},
        {rparen, 1},
        {arrow, 1},
        {lower_ident, 1, "a"},
        {flow, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {flow_decl, test, TypeSig, _, _} = FlowDecl,

    %% Should be a function type, NOT a tuple
    ?assertMatch({type_fun, _, _, _}, TypeSig),

    %% Verify the first argument is a type_var, not a tuple
    {type_fun, FirstArg, _, _} = TypeSig,
    ?assertMatch({type_var, a, _}, FirstArg).

%%--------------------------------------------------------------------
%% Precedence Interaction Tests
%%--------------------------------------------------------------------

parse_setoid_eq_with_arithmetic_test() ->
    %% flow test : Bool
    %% flow test = (x + 1) === (y + 1)
    %% Should parse successfully with correct precedence
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {flow, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lparen, 2},
        {lower_ident, 2, "x"},
        {plus, 2},
        {integer, 2, 1},
        {rparen, 2},
        {setoid_eq, 2},
        {lparen, 2},
        {lower_ident, 2, "y"},
        {plus, 2},
        {integer, 2, 1},
        {rparen, 2}
    ],
    Result = catena_parser:parse(Tokens),
    ?assertMatch({ok, _}, Result).

parse_setoid_eq_precedence_without_parens_test() ->
    %% flow test : Bool
    %% flow test = x + 1 === y + 1
    %% Should parse as (x + 1) === (y + 1) due to precedence
    %% (+ has higher precedence than ===)
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {flow, 2},
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
    Result = catena_parser:parse(Tokens),
    ?assertMatch({ok, _}, Result).

parse_setoid_eq_with_comparison_test() ->
    %% flow test : Bool
    %% flow test = x < y === z < w
    %% Actually parses as: (x < y) === (z < w)
    %% Both < and === are nonassoc but at different precedence levels
    %% < binds tighter (precedence 310) than === (precedence 300)
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {flow, 2},
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
    Result = catena_parser:parse(Tokens),
    %% This should actually parse successfully (corrected expectation)
    ?assertMatch({ok, _}, Result).

%%--------------------------------------------------------------------
%% Invalid Trait Method Signatures
%%--------------------------------------------------------------------

parse_trait_method_missing_parentheses_fails_test() ->
    %% trait Functor f where
    %%   fmap : a -> b -> f a -> f b
    %% Should succeed (this is actually valid - no parens needed)
    %% But let's test an actually invalid case: unclosed parenthesis
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Test"},
        {where, 1},
        {lower_ident, 2, "test"},
        {colon, 2},
        {lparen, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "b"}
        %% Missing rparen!
        ,{arrow, 2},
        {lower_ident, 2, "c"},
        {'end', 3}
    ],
    Result = catena_parser:parse(Tokens),
    ?assertMatch({error, _}, Result).

parse_trait_method_invalid_type_application_test() ->
    %% Test type application without proper spacing/structure
    %% This should actually parse (lowercase idents can be type vars)
    %% Let's test something that's genuinely invalid
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Test"},
        {where, 1},
        {lower_ident, 2, "test"},
        {colon, 2},
        {lparen, 2},
        {lparen, 2},  %% Double lparen without matching structure
        {lower_ident, 2, "a"},
        {rparen, 2},
        {arrow, 2},
        {lower_ident, 2, "b"},
        {'end', 3}
    ],
    Result = catena_parser:parse(Tokens),
    ?assertMatch({error, _}, Result).

%%--------------------------------------------------------------------
%% Malformed Expressions with Operators
%%--------------------------------------------------------------------

parse_operator_without_right_operand_test() ->
    %% flow test : Bool
    %% flow test = x ===
    %% Should fail: missing right operand
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {flow, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {setoid_eq, 2}
        %% Missing right operand
    ],
    Result = catena_parser:parse(Tokens),
    ?assertMatch({error, _}, Result).

parse_operator_without_left_operand_test() ->
    %% flow test : Bool
    %% flow test = === y
    %% Should fail: missing left operand
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {flow, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {setoid_eq, 2},  %% No left operand
        {lower_ident, 2, "y"}
    ],
    Result = catena_parser:parse(Tokens),
    ?assertMatch({error, _}, Result).

%%--------------------------------------------------------------------
%% Edge Cases
%%--------------------------------------------------------------------

parse_empty_parentheses_in_type_test() ->
    %% flow test : () -> a
    %% Empty parens should parse as unit type (if supported)
    %% or error (if not supported)
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {lparen, 1},
        {rparen, 1},
        {arrow, 1},
        {lower_ident, 1, "a"},
        {flow, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"}
    ],
    %% This might parse or error depending on unit type support
    Result = catena_parser:parse(Tokens),
    %% We expect an error since () is not currently a valid type
    ?assertMatch({error, _}, Result).

parse_nested_empty_tuples_test() ->
    %% flow test : ((,)) -> a
    %% Should fail: malformed tuple syntax
    Tokens = [
        {flow, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {lparen, 1},
        {lparen, 1},
        {comma, 1},
        {rparen, 1},
        {rparen, 1},
        {arrow, 1},
        {lower_ident, 1, "a"},
        {flow, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"}
    ],
    Result = catena_parser:parse(Tokens),
    ?assertMatch({error, _}, Result).
