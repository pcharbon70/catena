%%%-------------------------------------------------------------------
%%% @doc Catena Row Polymorphism Integration Tests (Phase 14.2.2)
%%%
%%% Tests for row polymorphism integration in the type system.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_row_poly_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    State = catena_infer_state:new(),
    State.

cleanup(_) ->
    ok.

%%====================================================================
%% Row Variable Management Tests
%%====================================================================

row_variable_tests_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(State) ->
            [
                {"Add row vars to mono scheme", fun test_add_row_vars_mono/0},
                {"Add row vars to poly scheme", fun test_add_row_vars_poly/0},
                {"Get row vars from scheme", fun test_get_row_vars/0},
                {"Scheme has row vars", fun test_has_row_vars/0}
            ]
        end
    }.

test_add_row_vars_mono() ->
    Type = catena_types:tcon(integer),
    RowVarId = {row_var, 1},
    Scheme = catena_row_poly_integration:add_row_vars_to_scheme(
        catena_type_scheme:mono(Type),
        [RowVarId]
    ),
    ?assertEqual({poly, [], [RowVarId], Type}, Scheme).

test_add_row_vars_poly() ->
    Type = catena_types:tcon(integer),
    RowVarId = {row_var, 1},
    Scheme = catena_row_poly_integration:add_row_vars_to_scheme(
        catena_type_scheme:poly([1], Type),
        [RowVarId]
    ),
    ?assertEqual({poly, [1], [RowVarId], Type}, Scheme).

test_get_row_vars() ->
    Type = catena_types:tcon(integer),
    RowVarId = {row_var, 1},
    Scheme = {poly, [], [RowVarId], Type},
    RowVars = catena_row_poly_integration:get_row_vars_from_scheme(Scheme),
    ?assertEqual([RowVarId], RowVars).

test_has_row_vars() ->
    Type = catena_types:tcon(integer),
    RowVarId = {row_var, 1},
    SchemeWith = {poly, [], [RowVarId], Type},
    SchemeWithout = catena_type_scheme:mono(Type),
    ?assert(catena_row_poly_integration:scheme_has_row_vars(SchemeWith)),
    ?assertNot(catena_row_poly_integration:scheme_has_row_vars(SchemeWithout)).

%%====================================================================
%% Row Polymorphic Scheme Tests
%%====================================================================

row_poly_scheme_tests_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(State) ->
            [
                {"Create row poly scheme", fun test_row_poly_scheme/0},
                {"Generalize row vars", fun test_generalize_row_vars/0}
            ]
        end
    }.

test_row_poly_scheme() ->
    Type = catena_types:tcon(integer),
    RowVarId = {row_var, 1},
    Scheme = catena_row_poly_integration:row_poly_scheme([], [RowVarId], Type),
    ?assertEqual({poly, [], [RowVarId], Type}, Scheme).

test_generalize_row_vars() ->
    Type = catena_types:tcon(integer),
    RowVarId = {row_var, 1},
    Scheme = catena_row_poly_integration:generalize_row_vars(Type, [RowVarId]),
    ?assertEqual({poly, [], [RowVarId], Type}, Scheme).

%%====================================================================
%% Row Constraint Tests
%%====================================================================

row_constraint_tests_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(State) ->
            [
                {"Create row constraint", fun test_row_constraint/0},
                {"Satisfy row constraints", fun test_satisfy_constraints/0},
                {"Merge row constraints", fun test_merge_constraints/0}
            ]
        end
    }.

test_row_constraint() ->
    RowVarId = {row_var, 1},
    Constraint = catena_row_poly_integration:row_constraint(RowVarId, [state, io]),
    ?assertMatch({row_constraint, RowVarId, _}, Constraint).

test_satisfy_constraints() ->
    RowVarId = {row_var, 1},
    Constraint = catena_row_poly_integration:row_constraint(RowVarId, [state]),
    EffectRow = catena_row_types:effect_row([state, io, error]),
    ?assert(catena_row_poly_integration:satisfy_row_constraints([Constraint], EffectRow)).

test_merge_constraints() ->
    RowVarId = {row_var, 1},
    C1 = catena_row_poly_integration:row_constraint(RowVarId, [state]),
    C2 = catena_row_poly_integration:row_constraint(RowVarId, [io]),
    Merged = catena_row_poly_integration:merge_row_constraints([C1], [C2]),
    ?assertEqual(1, length(Merged)),
    [{row_constraint, RowVarId, MergedRow}] = Merged,
    ?assertEqual(3, length(catena_row_types:row_to_list(MergedRow))).

%%====================================================================
%% Instantiation Tests
%%====================================================================

instantiation_tests_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(State) ->
            [
                {"Instantiate mono scheme", fun test_instantiate_mono/0},
                {"Instantiate poly scheme without rows", fun test_instantiate_poly_no_rows/0},
                {"Instantiate row poly scheme", fun test_instantiate_row_poly/0}
            ]
        end
    }.

test_instantiate_mono() ->
    Type = catena_types:tcon(integer),
    Scheme = catena_type_scheme:mono(Type),
    {ResultType, _NewState} = catena_row_poly_integration:instantiate_with_row_poly(
        Scheme,
        catena_infer_state:new()
    ),
    ?assertEqual(Type, ResultType).

test_instantiate_poly_no_rows() ->
    Type = catena_types:tcon(integer),
    Scheme = catena_type_scheme:poly([1], Type),
    {ResultType, _NewState} = catena_row_poly_integration:instantiate_with_row_poly(
        Scheme,
        catena_infer_state:new()
    ),
    ?assert(is_tuple(ResultType)).

test_instantiate_row_poly() ->
    Type = catena_types:tcon(integer),
    RowVarId = {row_var, 1},
    Scheme = {poly, [], [RowVarId], Type},
    {ResultType, _NewState} = catena_row_poly_integration:instantiate_row_poly_scheme(
        Scheme,
        catena_infer_state:new()
    ),
    ?assert(is_tuple(ResultType)).

%%====================================================================
%% Extension Tests
%%====================================================================

extension_tests_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(State) ->
            [
                {"Extend scheme with row var", fun test_extend_with_row_var/0}
            ]
        end
    }.

test_extend_with_row_var() ->
    Type = catena_types:tcon(integer),
    Scheme = catena_type_scheme:mono(Type),
    {NewScheme, _NewState} = catena_row_poly_integration:extend_with_row_var(
        Scheme,
        catena_infer_state:new()
    ),
    ?assert(catena_row_poly_integration:scheme_has_row_vars(NewScheme)).

%%====================================================================
%% Inference Integration Tests
%%====================================================================

inference_integration_tests_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(State) ->
            [
                {"Infer with row poly", fun test_infer_with_row_poly/0},
                {"Generalize with row poly", fun test_generalize_with_row_poly/0}
            ]
        end
    }.

test_infer_with_row_poly() ->
    Expr = {literal, {int, 42}, {1, 1}},
    Env = catena_type_env:empty(),
    {Type, _NewState} = catena_row_poly_integration:infer_with_row_poly(
        Expr,
        Env,
        catena_infer_state:new()
    ),
    ?assert(is_tuple(Type)).

test_generalize_with_row_poly() ->
    Type = catena_types:tcon(integer),
    {Scheme, _NewState} = catena_row_poly_integration:generalize_with_row_poly(
        Type,
        catena_infer_state:new()
    ),
    ?assert(is_tuple(Scheme)).
