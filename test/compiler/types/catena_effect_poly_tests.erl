%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_effect_poly (Phase 6.1)
%%%
%%% Tests for effect polymorphism including:
%%% - Effect variable creation and manipulation
%%% - Effect set operations with polymorphism
%%% - Effect constraints (row and absence)
%%% - Effect unification with occurs check
%%% - Effect quantification and instantiation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_poly_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Effect Variable Tests
%%%=============================================================================

evar_creation_test() ->
    EVar = catena_effect_poly:evar(1),
    ?assertEqual({evar, 1}, EVar),
    ?assert(catena_effect_poly:is_effect_var(EVar)).

fresh_evar_test() ->
    State = catena_infer_state:new(),
    {{evar, _Id}, NewState} = catena_effect_poly:fresh_evar(State),
    ?assertNotEqual(State, NewState).

is_effect_var_test() ->
    ?assert(catena_effect_poly:is_effect_var({evar, 1})),
    ?assertNot(catena_effect_poly:is_effect_var({effect_set, [io]})),
    ?assertNot(catena_effect_poly:is_effect_var({effect_set, []})).

%%%=============================================================================
%%% Effect Set Operations Tests
%%%=============================================================================

empty_effects_test() ->
    Empty = catena_effect_poly:empty_effects(),
    ?assertEqual({effect_set, []}, Empty),
    ?assert(catena_effect_poly:is_pure(Empty)).

singleton_effect_test() ->
    Single = catena_effect_poly:singleton_effect(io),
    ?assertEqual({effect_set, [io]}, Single),
    ?assertNot(catena_effect_poly:is_pure(Single)).

effect_set_normalization_test() ->
    Set1 = catena_effect_poly:effect_set([io, process, io]),
    ?assertEqual({effect_set, [io, process]}, Set1),

    Set2 = catena_effect_poly:effect_set([process, io]),
    ?assertEqual({effect_set, [io, process]}, Set2).

union_effects_test() ->
    E1 = {effect_set, [io]},
    E2 = {effect_set, [process]},
    Union = catena_effect_poly:union_effects(E1, E2),
    ?assertEqual({effect_set, [io, process]}, Union).

union_effects_with_vars_test() ->
    EVar = {evar, 1},
    Empty = {effect_set, []},
    ?assertEqual({evar, 1}, catena_effect_poly:union_effects(EVar, Empty)),
    ?assertEqual({evar, 1}, catena_effect_poly:union_effects(Empty, EVar)).

is_pure_test() ->
    ?assert(catena_effect_poly:is_pure({effect_set, []})),
    ?assertNot(catena_effect_poly:is_pure({effect_set, [io]})),
    ?assertNot(catena_effect_poly:is_pure({evar, 1})).

effects_equal_test() ->
    E1 = {effect_set, [io, process]},
    E2 = {effect_set, [process, io]},
    ?assert(catena_effect_poly:effects_equal(E1, E2)),

    E3 = {effect_set, [io]},
    E4 = {effect_set, [io, process]},
    ?assertNot(catena_effect_poly:effects_equal(E3, E4)).

substitute_evar_test() ->
    ?assertEqual({effect_set, [io]},
        catena_effect_poly:substitute_evar(1, {effect_set, [io]}, {evar, 1})).

%%%=============================================================================
%%% Effect Constraint Tests
%%%=============================================================================

row_constraint_test() ->
    C = catena_effect_poly:row_constraint([io, file]),
    ?assertEqual({row, [file, io]}, C),
    ?assert(catena_effect_poly:is_row_constraint(C)),
    ?assertNot(catena_effect_poly:is_absence_constraint(C)).

absence_constraint_test() ->
    C = catena_effect_poly:absence_constraint([process]),
    ?assertEqual({absence, [process]}, C),
    ?assert(catena_effect_poly:is_absence_constraint(C)),
    ?assertNot(catena_effect_poly:is_row_constraint(C)).

constraint_effects_test() ->
    ?assertEqual([io, file],
        catena_effect_poly:constraint_effects({row, [io, file]})),
    ?assertEqual([process],
        catena_effect_poly:constraint_effects({absence, [process]})).

satisfies_row_constraint_test() ->
    Expr = {effect_set, [io, process]},
    C = {row, [io]},
    ?assert(catena_effect_poly:satisfies_constraint(Expr, C)),

    C2 = {row, [io, file]},
    ?assertNot(catena_effect_poly:satisfies_constraint(Expr, C2)).

satisfies_absence_constraint_test() ->
    Expr = {effect_set, [io]},
    C = {absence, [process]},
    ?assert(catena_effect_poly:satisfies_constraint(Expr, C)),

    C2 = {absence, [io]},
    ?assertNot(catena_effect_poly:satisfies_constraint(Expr, C2)).

%%%=============================================================================
%%% Effect Unification Tests
%%%=============================================================================

unify_concrete_sets_test() ->
    E1 = {effect_set, [io]},
    E2 = {effect_set, [io]},
    ?assertEqual({ok, #{}}, catena_effect_poly:unify_effects(E1, E2)).

unify_concrete_sets_fail_test() ->
    E1 = {effect_set, [io]},
    E2 = {effect_set, [process]},
    ?assertEqual({error, effect_mismatch},
        catena_effect_poly:unify_effects(E1, E2)).

unify_var_with_concrete_test() ->
    Var = {evar, 1},
    Concrete = {effect_set, [io]},
    ?assertEqual({ok, #{1 => {effect_set, [io]}}},
        catena_effect_poly:unify_effects(Var, Concrete)).

occurs_check_test() ->
    Var = {evar, 1},
    ?assertEqual({error, {effect_occurs, 1}},
        catena_effect_poly:unify_effects(Var, Var)).

occurs_in_evar_test() ->
    ?assert(catena_effect_poly:occurs_in_evar(1, {evar, 1})),
    ?assertNot(catena_effect_poly:occurs_in_evar(1, {effect_set, [io]})).

%%%=============================================================================
%%% Effect Quantification Tests
%%%=============================================================================

quantify_effects_test() ->
    Type = {tfun, {tvar, 1}, {tvar, 2}, {evar, 1}},
    Quantified = catena_effect_poly:quantify_effects(Type, [1]),
    ?assertMatch({tfun, {tvar, 1}, {tvar, 2}, {evar, 1}}, Quantified).

instantiate_effects_test() ->
    Type = {tfun, {tvar, 1}, {tvar, 2}, {evar, 1}},
    State = catena_infer_state:new(),
    {NewType, NewState} = catena_effect_poly:instantiate_effects(Type, State),
    ?assertMatch({tfun, {tvar, 1}, {tvar, 2}, {evar, _}}, NewType),
    ?assertNotEqual(State, NewState).

instantiate_concrete_effects_test() ->
    Type = {tfun, {tvar, 1}, {tvar, 2}, {effect_set, [io]}},
    State = catena_infer_state:new(),
    {NewType, NewState} = catena_effect_poly:instantiate_effects(Type, State),
    % Concrete effects should remain unchanged
    ?assertEqual(Type, NewType).

%%%=============================================================================
%%% Polymorphic Effect Examples
%%%=============================================================================

generic_map_function_test() ->
    % map : (a -> b / ε) -> List a -> List b / ε
    % Function is polymorphic over effect set ε

    % Create effect variable for ε
    Epsilon = {evar, 1},

    % Function type: a -> b / ε
    A = {tvar, 10},
    B = {tvar, 11},
    FuncType = {tfun, A, B, Epsilon},

    % List a -> List b / ε
    ListA = {tapp, {tcon, 'List'}, [A]},
    ListB = {tapp, {tcon, 'List'}, [B]},
    MapType = {tfun, {tapp, {tcon, 'List'}, [FuncType]}, ListB, Epsilon},

    ?assertMatch({tfun, _, _, {evar, 1}}, MapType).

constrained_function_test() ->
    % readFile : String -> String / {IO | ε}
    % Function requires at least IO effect

    C = catena_effect_poly:row_constraint([io]),

    % Create constrained effect variable
    Constrained = {constrained, 1, [C]},

    String = {tcon, string},
    ReadFileType = {tfun, String, String, Constrained},

    ?assertMatch({tfun, _, _, {constrained, 1, _}}, ReadFileType).
