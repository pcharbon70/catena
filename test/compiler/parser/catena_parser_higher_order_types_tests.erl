-module(catena_parser_higher_order_types_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Higher-Order Function Type Tests
%% These tests verify that the parser can handle parenthesized
%% function types like (a -> b) -> f a -> f b
%%====================================================================

%%--------------------------------------------------------------------
%% Basic Parenthesized Function Types
%%--------------------------------------------------------------------

parse_simple_parenthesized_function_test() ->
    %% trait Functor f where
    %%   fmap : (a -> b) -> f a -> f b
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Functor"},
        {lower_ident, 1, "f"},
        {where, 1},
        {lower_ident, 2, "fmap"},
        {colon, 2},
        {lparen, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "b"},
        {rparen, 2},
        {arrow, 2},
        {lower_ident, 2, "f"},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "f"},
        {lower_ident, 2, "b"},
        {'end', 3}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    %% Parser returns tuple format: {trait_decl, Name, TypeParams, Extends, Methods, Location}
    {trait_decl, 'Functor', [f], _Extends, Methods, _Loc} = TraitDecl,

    %% Verify method signature - methods are {trait_sig, Name, Type, Loc} tuples
    [{trait_sig, fmap, TypeSig, _}] = Methods,

    %% Should be: (a -> b) -> f a -> f b
    %% Which is: fun(fun(a, b), fun(f a, f b))
    ?assertMatch({type_fun, _, _, _}, TypeSig),

    %% First argument should be a function type (a -> b)
    {type_fun, FirstArg, _Rest, _} = TypeSig,
    ?assertMatch({type_fun, _, _, _}, FirstArg).

parse_monad_bind_signature_test() ->
    %% trait Monad m where
    %%   bind : m a -> (a -> m b) -> m b
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Monad"},
        {lower_ident, 1, "m"},
        {where, 1},
        {lower_ident, 2, "bind"},
        {colon, 2},
        {lower_ident, 2, "m"},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lparen, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "m"},
        {lower_ident, 2, "b"},
        {rparen, 2},
        {arrow, 2},
        {lower_ident, 2, "m"},
        {lower_ident, 2, "b"},
        {'end', 3}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    {trait_decl, 'Monad', [m], _Extends, Methods, _Loc} = TraitDecl,

    %% Verify method signature
    [{trait_sig, bind, TypeSig, _}] = Methods,

    %% Should parse successfully
    ?assertMatch({type_fun, _, _, _}, TypeSig).

parse_foldable_signature_test() ->
    %% trait Foldable t where
    %%   foldl : (b -> a -> b) -> b -> t a -> b
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Foldable"},
        {lower_ident, 1, "t"},
        {where, 1},
        {lower_ident, 2, "foldl"},
        {colon, 2},
        {lparen, 2},
        {lower_ident, 2, "b"},
        {arrow, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "b"},
        {rparen, 2},
        {arrow, 2},
        {lower_ident, 2, "b"},
        {arrow, 2},
        {lower_ident, 2, "t"},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "b"},
        {'end', 3}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    {trait_decl, 'Foldable', [t], _Extends, Methods, _Loc} = TraitDecl,

    [{trait_sig, foldl, TypeSig, _}] = Methods,
    ?assertMatch({type_fun, _, _, _}, TypeSig).

%%--------------------------------------------------------------------
%% Nested Parenthesized Types
%%--------------------------------------------------------------------

parse_deeply_nested_function_types_test() ->
    %% trait Compose where
    %%   compose : ((b -> c) -> (a -> b) -> (a -> c))
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Compose"},
        {where, 1},
        {lower_ident, 2, "compose"},
        {colon, 2},
        {lparen, 2},
        {lower_ident, 2, "b"},
        {arrow, 2},
        {lower_ident, 2, "c"},
        {rparen, 2},
        {arrow, 2},
        {lparen, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "b"},
        {rparen, 2},
        {arrow, 2},
        {lparen, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "c"},
        {rparen, 2},
        {'end', 3}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    {trait_decl, _Name, _TypeParams, _Extends, Methods, _Loc} = TraitDecl,

    [{trait_sig, compose, TypeSig, _}] = Methods,
    ?assertMatch({type_fun, _, _, _}, TypeSig).

%%--------------------------------------------------------------------
%% Tuple Types (Must Have Comma)
%%--------------------------------------------------------------------

parse_tuple_type_two_elements_test() ->
    %% trait Pair where
    %%   first : (a, b) -> a
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Pair"},
        {where, 1},
        {lower_ident, 2, "first"},
        {colon, 2},
        {lparen, 2},
        {lower_ident, 2, "a"},
        {comma, 2},
        {lower_ident, 2, "b"},
        {rparen, 2},
        {arrow, 2},
        {lower_ident, 2, "a"},
        {'end', 3}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    {trait_decl, _Name, _TypeParams, _Extends, Methods, _Loc} = TraitDecl,

    [{trait_sig, first, TypeSig, _}] = Methods,

    %% Should be: (a, b) -> a
    {type_fun, TupleType, RetType, _} = TypeSig,
    ?assertMatch({type_tuple, [_, _], _}, TupleType),
    ?assertMatch({type_var, a, _}, RetType).

parse_tuple_type_three_elements_test() ->
    %% trait Triple where
    %%   mk : (a, b, c) -> Triple a b c
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Triple"},
        {where, 1},
        {lower_ident, 2, "mk"},
        {colon, 2},
        {lparen, 2},
        {lower_ident, 2, "a"},
        {comma, 2},
        {lower_ident, 2, "b"},
        {comma, 2},
        {lower_ident, 2, "c"},
        {rparen, 2},
        {arrow, 2},
        {upper_ident, 2, "Triple"},
        {lower_ident, 2, "a"},
        {lower_ident, 2, "b"},
        {lower_ident, 2, "c"},
        {'end', 3}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    {trait_decl, _Name, _TypeParams, _Extends, Methods, _Loc} = TraitDecl,

    [{trait_sig, mk, TypeSig, _}] = Methods,
    {type_fun, TupleType, _, _} = TypeSig,
    ?assertMatch({type_tuple, [_, _, _], _}, TupleType).

%%--------------------------------------------------------------------
%% Disambiguated: Parenthesized vs. Tuple
%%--------------------------------------------------------------------

parse_parenthesized_not_tuple_test() ->
    %% transform id : (a -> a)
    %% This should parse as a parenthesized function type, NOT a tuple
    Tokens = [
        {transform, 1},
        {lower_ident, 1, "id"},
        {colon, 1},
        {lparen, 1},
        {lower_ident, 1, "a"},
        {arrow, 1},
        {lower_ident, 1, "a"},
        {rparen, 1},
        {transform, 2},
        {lower_ident, 2, "id"},
        {equals, 2},
        {lower_ident, 2, "x"}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = Result,
    {transform_decl, id, TypeSig, _, _} = FlowDecl,

    %% Should be a function type, not a tuple
    ?assertMatch({type_fun, _, _, _}, TypeSig),

    %% Should NOT be a tuple
    ?assertNotMatch({type_tuple, _, _}, TypeSig).

%%--------------------------------------------------------------------
%% Complex Real-World Examples
%%--------------------------------------------------------------------

parse_applicative_ap_signature_test() ->
    %% trait Applicative f where
    %%   ap : f (a -> b) -> f a -> f b
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Applicative"},
        {lower_ident, 1, "f"},
        {where, 1},
        {lower_ident, 2, "ap"},
        {colon, 2},
        {lower_ident, 2, "f"},
        {lparen, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "b"},
        {rparen, 2},
        {arrow, 2},
        {lower_ident, 2, "f"},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "f"},
        {lower_ident, 2, "b"},
        {'end', 3}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    {trait_decl, 'Applicative', _TypeParams, _Extends, Methods, _Loc} = TraitDecl,

    [{trait_sig, ap, TypeSig, _}] = Methods,
    ?assertMatch({type_fun, _, _, _}, TypeSig).

parse_traversable_signature_test() ->
    %% trait Traversable t where
    %%   traverse : (a -> f b) -> t a -> f (t b)
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Traversable"},
        {lower_ident, 1, "t"},
        {where, 1},
        {lower_ident, 2, "traverse"},
        {colon, 2},
        {lparen, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "f"},
        {lower_ident, 2, "b"},
        {rparen, 2},
        {arrow, 2},
        {lower_ident, 2, "t"},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "f"},
        {lparen, 2},
        {lower_ident, 2, "t"},
        {lower_ident, 2, "b"},
        {rparen, 2},
        {'end', 3}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    {trait_decl, 'Traversable', _TypeParams, _Extends, Methods, _Loc} = TraitDecl,

    [{trait_sig, traverse, TypeSig, _}] = Methods,
    ?assertMatch({type_fun, _, _, _}, TypeSig).

%%--------------------------------------------------------------------
%% Error Cases (Should Still Parse But With Potential Semantic Errors)
%%--------------------------------------------------------------------

parse_multiple_methods_with_higher_order_types_test() ->
    %% trait Category c where
    %%   id : c a a,
    %%   compose : c b c -> c a b -> c a c
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Category"},
        {lower_ident, 1, "c"},
        {where, 1},
        {lower_ident, 2, "id"},
        {colon, 2},
        {lower_ident, 2, "c"},
        {lower_ident, 2, "a"},
        {lower_ident, 2, "a"},
        {comma, 2},
        {lower_ident, 3, "compose"},
        {colon, 3},
        {lower_ident, 3, "c"},
        {lower_ident, 3, "b"},
        {lower_ident, 3, "c"},
        {arrow, 3},
        {lower_ident, 3, "c"},
        {lower_ident, 3, "a"},
        {lower_ident, 3, "b"},
        {arrow, 3},
        {lower_ident, 3, "c"},
        {lower_ident, 3, "a"},
        {lower_ident, 3, "c"},
        {'end', 4}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    {trait_decl, 'Category', _TypeParams, _Extends, Methods, _Loc} = TraitDecl,

    %% Should have 2 methods
    ?assertEqual(2, length(Methods)).
