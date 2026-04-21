%%%-------------------------------------------------------------------
%%% @doc Focused tests for Phase 14.2 row-polymorphism integration.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_row_poly_integration_tests).

-include_lib("eunit/include/eunit.hrl").

type_int() ->
    catena_types:tcon(integer).

state() ->
    catena_infer_state:new().

add_row_vars_to_mono_scheme_test() ->
    RowVarId = {row_var, 1},
    Scheme = catena_row_poly_integration:add_row_vars_to_scheme(
        catena_type_scheme:mono(type_int()),
        [RowVarId]
    ),
    ?assertEqual({poly, [], [RowVarId], type_int()}, Scheme).

get_and_detect_row_vars_test() ->
    RowVarId = {row_var, 7},
    Scheme = {poly, [], [RowVarId], type_int()},
    ?assertEqual([RowVarId], catena_row_poly_integration:get_row_vars_from_scheme(Scheme)),
    ?assert(catena_row_poly_integration:scheme_has_row_vars(Scheme)),
    ?assertNot(catena_row_poly_integration:scheme_has_row_vars(catena_type_scheme:mono(type_int()))).

row_constraint_and_satisfaction_test() ->
    RowVarId = {row_var, 1},
    Constraint = catena_row_poly_integration:row_constraint(RowVarId, ['IO']),
    EffectRow = catena_row_types:effect_row(['IO', 'State']),
    ?assertMatch({row_constraint, {row_var, 1}, _}, Constraint),
    ?assert(catena_row_poly_integration:satisfy_row_constraints([Constraint], EffectRow)).

merge_row_constraints_unions_required_effects_test() ->
    RowVarId = {row_var, 1},
    C1 = catena_row_poly_integration:row_constraint(RowVarId, ['IO']),
    C2 = catena_row_poly_integration:row_constraint(RowVarId, ['State']),
    [{row_constraint, {row_var, 1}, MergedRow}] =
        catena_row_poly_integration:merge_row_constraints([C1], [C2]),
    ?assertEqual(['IO', 'State'], catena_row_types:row_to_list(MergedRow)).

instantiate_standard_poly_scheme_test() ->
    Scheme = catena_type_scheme:poly([1], catena_types:tvar(1)),
    {Instantiated, _State} =
        catena_row_poly_integration:instantiate_with_row_poly(Scheme, catena_infer_state:new(10)),
    ?assertEqual({tvar, 10}, Instantiated).

extend_with_row_var_test() ->
    {NewScheme, _State} =
        catena_row_poly_integration:extend_with_row_var(catena_type_scheme:mono(type_int()), state()),
    ?assert(catena_row_poly_integration:scheme_has_row_vars(NewScheme)).

generalize_with_row_poly_without_rows_is_mono_test() ->
    {Scheme, _State} = catena_row_poly_integration:generalize_with_row_poly(type_int(), state()),
    ?assertEqual({mono, type_int()}, Scheme).

infer_with_row_poly_delegates_to_inference_test() ->
    Expr = {literal, integer, 42, {location, 1, 1}},
    {Type, _State} =
        catena_row_poly_integration:infer_with_row_poly(Expr, catena_type_env:empty(), state()),
    ?assertEqual({tcon, int}, Type).
