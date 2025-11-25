-module(catena_parser_operator_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Operator Tests - Type-Level Equality
%% These tests verify that type-level equality operators (===, !==)
%% parse correctly in realistic trait and instance declarations.
%%====================================================================

%%--------------------------------------------------------------------
%% Setoid Equality Operators
%%--------------------------------------------------------------------

parse_setoid_eq_in_trait_test() ->
    %% trait Setoid a where
    %%   eq : a -> a -> Bool
    %% end
    Tokens = [
        {trait, 1},
        {upper_ident, 1, "Setoid"},
        {lower_ident, 1, "a"},
        {where, 1},
        {lower_ident, 2, "eq"},
        {colon, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {lower_ident, 2, "a"},
        {arrow, 2},
        {upper_ident, 2, "Bool"},
        {'end', 3}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [TraitDecl], _} = Result,
    %% Parser returns tuple format: {trait_decl, Name, TypeParams, Extends, Methods, Location}
    {trait_decl, 'Setoid', [a], _Extends, Methods, _Loc} = TraitDecl,
    [{trait_sig, eq, _, _}] = Methods.

parse_instance_with_setoid_operators_test() ->
    %% instance Setoid Bool where
    %%   transform eq x y = x === y
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Setoid"},
        {upper_ident, 1, "Bool"},
        {where, 1},
        {transform, 2},
        {lower_ident, 2, "eq"},
        {lower_ident, 2, "x"},
        {lower_ident, 2, "y"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {setoid_eq, 2},
        {lower_ident, 2, "y"},
        {'end', 3}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,
    %% Parser returns tuple format: {instance_decl, Trait, Types, Constraints, Methods, Location}
    {instance_decl, 'Setoid', _Types, _Constraints, Methods, _Loc} = InstanceDecl,
    %% Methods are [{Name, Lambda}] tuples
    [{eq, {lambda, _Params, Body, _}}] = Methods,
    ?assertMatch({binary_op, setoid_eq, _, _, _}, Body).

parse_instance_with_setoid_neq_test() ->
    %% instance Test T where
    %%   transform neq x y = x !== y
    %% end
    Tokens = [
        {instance, 1},
        {upper_ident, 1, "Test"},
        {upper_ident, 1, "T"},
        {where, 1},
        {transform, 2},
        {lower_ident, 2, "neq"},
        {lower_ident, 2, "x"},
        {lower_ident, 2, "y"},
        {equals, 2},
        {lower_ident, 2, "x"},
        {setoid_neq, 2},
        {lower_ident, 2, "y"},
        {'end', 3}
    ],
    {ok, Result} = catena_parser:parse(Tokens),
    {module, _, _, _, [InstanceDecl], _} = Result,
    %% Parser returns tuple format: {instance_decl, Trait, Types, Constraints, Methods, Location}
    {instance_decl, 'Test', _Types, _Constraints, Methods, _Loc} = InstanceDecl,
    %% Methods are [{Name, Lambda}] tuples
    [{neq, {lambda, _Params, Body, _}}] = Methods,
    ?assertMatch({binary_op, setoid_neq, _, _, _}, Body).

%%--------------------------------------------------------------------
%% Comprehensive Tests - Type-Level Equality Operators
%%--------------------------------------------------------------------

parse_all_operators_comprehensive_test() ->
    %% Verify type-level equality operators tokenize correctly
    OpTests = [
        {"x === y", setoid_eq},
        {"x !== y", setoid_neq}
    ],

    lists:foreach(
        fun({ExprStr, ExpectedOp}) ->
            {ok, Tokens} = catena_lexer:tokenize(ExprStr),
            %% Verify the operator token is present
            ?assert(lists:any(fun({Op, _}) -> Op =:= ExpectedOp; (_) -> false end, Tokens),
                    "Operator " ++ atom_to_list(ExpectedOp) ++ " should tokenize in: " ++ ExprStr)
        end,
        OpTests
    ).
