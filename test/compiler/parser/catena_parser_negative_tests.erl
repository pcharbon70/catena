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
    %% transform test : Bool
    %% transform test = x === y === z
    %% Should fail: === is non-associative
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
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
    %% transform test : Bool
    %% transform test = x !== y !== z
    %% Should fail: !== is non-associative
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
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
    %% transform test : Bool
    %% transform test = x === y !== z
    %% Should fail: cannot chain different equality operators
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
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
%% Category Theory Operators - Now Restored (Section 1.5.6)
%% These operators were removed in Task 1.1.7 but restored in Section 1.5.6
%% for Effect Integration with Kleisli Arrows
%%--------------------------------------------------------------------

parse_restored_fmap_operator_test() ->
    %% Verify <$> tokenizes as fmap operator (restored in 1.5.6)
    Source = "x <$> y",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    TokenTypes = [element(1, T) || T <- Tokens],
    ?assert(lists:member(fmap, TokenTypes)).

parse_restored_mappend_operator_test() ->
    %% Verify <> tokenizes as mappend operator (restored in 1.5.6)
    Source = "x <> y",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    TokenTypes = [element(1, T) || T <- Tokens],
    ?assert(lists:member(mappend, TokenTypes)).

parse_restored_bind_operator_test() ->
    %% Verify >>= tokenizes as bind operator (restored in 1.5.6)
    Source = "x >>= y",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    TokenTypes = [element(1, T) || T <- Tokens],
    ?assert(lists:member(bind, TokenTypes)).

parse_restored_ap_operator_test() ->
    %% Verify <*> tokenizes as ap operator (restored in 1.5.6)
    Source = "x <*> y",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    TokenTypes = [element(1, T) || T <- Tokens],
    ?assert(lists:member(ap, TokenTypes)).

parse_restored_kleisli_operator_test() ->
    %% Verify >=> tokenizes as kleisli operator (restored in 1.5.6)
    Source = "x >=> y",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    TokenTypes = [element(1, T) || T <- Tokens],
    ?assert(lists:member(kleisli, TokenTypes)).

%%--------------------------------------------------------------------
%% Invalid Type Signatures
%%--------------------------------------------------------------------

parse_invalid_one_element_tuple_type_test() ->
    %% Type signatures like (a) should parse as parenthesized type, not tuple
    %% This test verifies we don't accidentally create 1-element tuples
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {lparen, 1},
        {lower_ident, 1, "a"},
        {rparen, 1},
        {arrow, 1},
        {lower_ident, 1, "a"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, test, TypeSig, _, _} = FlowDecl,

    %% Should be a function type, NOT a tuple
    ?assertMatch({type_fun, _, _, _}, TypeSig),

    %% Verify the first argument is a type_var, not a tuple
    {type_fun, FirstArg, _, _} = TypeSig,
    ?assertMatch({type_var, a, _}, FirstArg).

%%--------------------------------------------------------------------
%% Precedence Interaction Tests
%%--------------------------------------------------------------------

parse_setoid_eq_with_arithmetic_test() ->
    %% transform test : Bool
    %% transform test = (x + 1) === (y + 1)
    %% Should parse successfully with correct precedence
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
    %% transform test : Bool
    %% transform test = x + 1 === y + 1
    %% Should parse as (x + 1) === (y + 1) due to precedence
    %% (+ has higher precedence than ===)
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
    Result = catena_parser:parse(Tokens),
    ?assertMatch({ok, _}, Result).

parse_setoid_eq_with_comparison_test() ->
    %% transform test : Bool
    %% transform test = x < y === z < w
    %% Actually parses as: (x < y) === (z < w)
    %% Both < and === are nonassoc but at different precedence levels
    %% < binds tighter (precedence 310) than === (precedence 300)
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
    %% transform test : Bool
    %% transform test = x ===
    %% Should fail: missing right operand
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {setoid_eq, 2}
        %% Missing right operand
    ],
    Result = catena_parser:parse(Tokens),
    ?assertMatch({error, _}, Result).

parse_operator_without_left_operand_test() ->
    %% transform test : Bool
    %% transform test = === y
    %% Should fail: missing left operand
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {upper_ident, 1, "Bool"},
        {transform, 2},
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
    %% transform test : () -> a
    %% Empty parens should parse as unit type (if supported)
    %% or error (if not supported)
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {lparen, 1},
        {rparen, 1},
        {arrow, 1},
        {lower_ident, 1, "a"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"}
    ],
    %% This might parse or error depending on unit type support
    Result = catena_parser:parse(Tokens),
    %% We expect an error since () is not currently a valid type
    ?assertMatch({error, _}, Result).

parse_nested_empty_tuples_test() ->
    %% transform test : ((,)) -> a
    %% Should fail: malformed tuple syntax
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "test"},
        {colon, 1},
        {lparen, 1},
        {lparen, 1},
        {comma, 1},
        {rparen, 1},
        {rparen, 1},
        {arrow, 1},
        {lower_ident, 1, "a"},
        {transform, 2},
        {lower_ident, 2, "test"},
        {equals, 2},
        {lower_ident, 2, "x"}
    ],
    Result = catena_parser:parse(Tokens),
    ?assertMatch({error, _}, Result).
