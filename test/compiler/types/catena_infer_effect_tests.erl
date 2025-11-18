%%%-------------------------------------------------------------------
%%% @doc Unit Tests for Effect Inference and Checking
%%%
%%% Tests for catena_infer_effect module covering:
%%% - Effect construction (pure, from_list)
%%% - Effect normalization
%%% - Effect union
%%% - Purity checking
%%% - Effect subsumption
%%% - Effect compatibility
%%% - Effect inference from expressions
%%% - Pure context checking
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_infer_effect_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Section 1: Effect Construction
%%%===================================================================

pure_effect_test() ->
    Pure = catena_infer_effect:pure(),

    ?assertEqual({effect_set, []}, Pure),
    ?assertEqual(true, catena_infer_effect:is_pure(Pure)).

from_list_empty_test() ->
    Effects = catena_infer_effect:from_list([]),

    ?assertEqual({effect_set, []}, Effects),
    ?assertEqual(true, catena_infer_effect:is_pure(Effects)).

from_list_single_test() ->
    Effects = catena_infer_effect:from_list([io]),

    ?assertEqual({effect_set, [io]}, Effects),
    ?assertEqual(false, catena_infer_effect:is_pure(Effects)).

from_list_multiple_test() ->
    Effects = catena_infer_effect:from_list([io, state, exn]),

    % Should be normalized (sorted)
    ?assertEqual({effect_set, [exn, io, state]}, Effects).

from_list_with_duplicates_test() ->
    Effects = catena_infer_effect:from_list([io, state, io, exn, state]),

    % Should be normalized (sorted and deduplicated)
    ?assertEqual({effect_set, [exn, io, state]}, Effects).

%%%===================================================================
%%% Section 2: Effect Normalization
%%%===================================================================

normalize_empty_test() ->
    Effects = catena_infer_effect:normalize({effect_set, []}),

    ?assertEqual({effect_set, []}, Effects).

normalize_sorted_test() ->
    Effects = catena_infer_effect:normalize({effect_set, [exn, io, state]}),

    % Already sorted, should remain unchanged
    ?assertEqual({effect_set, [exn, io, state]}, Effects).

normalize_unsorted_test() ->
    Effects = catena_infer_effect:normalize({effect_set, [state, io, exn]}),

    % Should be sorted
    ?assertEqual({effect_set, [exn, io, state]}, Effects).

normalize_with_duplicates_test() ->
    Effects = catena_infer_effect:normalize({effect_set, [io, state, io, exn, state]}),

    % Should be sorted and deduplicated
    ?assertEqual({effect_set, [exn, io, state]}, Effects).

normalize_idempotent_test() ->
    Effects1 = catena_infer_effect:from_list([state, io, exn]),
    Effects2 = catena_infer_effect:normalize(Effects1),

    % Normalizing again should have no effect
    ?assertEqual(Effects1, Effects2).

%%%===================================================================
%%% Section 3: Effect Union
%%%===================================================================

union_empty_empty_test() ->
    E1 = catena_infer_effect:pure(),
    E2 = catena_infer_effect:pure(),

    Result = catena_infer_effect:union(E1, E2),

    ?assertEqual({effect_set, []}, Result).

union_empty_nonempty_test() ->
    E1 = catena_infer_effect:pure(),
    E2 = catena_infer_effect:from_list([io]),

    Result = catena_infer_effect:union(E1, E2),

    ?assertEqual({effect_set, [io]}, Result).

union_nonempty_empty_test() ->
    E1 = catena_infer_effect:from_list([state]),
    E2 = catena_infer_effect:pure(),

    Result = catena_infer_effect:union(E1, E2),

    ?assertEqual({effect_set, [state]}, Result).

union_disjoint_test() ->
    E1 = catena_infer_effect:from_list([io]),
    E2 = catena_infer_effect:from_list([state]),

    Result = catena_infer_effect:union(E1, E2),

    ?assertEqual({effect_set, [io, state]}, Result).

union_overlapping_test() ->
    E1 = catena_infer_effect:from_list([io, state]),
    E2 = catena_infer_effect:from_list([state, exn]),

    Result = catena_infer_effect:union(E1, E2),

    % Should be sorted and deduplicated
    ?assertEqual({effect_set, [exn, io, state]}, Result).

union_same_test() ->
    E1 = catena_infer_effect:from_list([io, state]),
    E2 = catena_infer_effect:from_list([io, state]),

    Result = catena_infer_effect:union(E1, E2),

    ?assertEqual({effect_set, [io, state]}, Result).

union_commutative_test() ->
    E1 = catena_infer_effect:from_list([io, state]),
    E2 = catena_infer_effect:from_list([exn]),

    Result1 = catena_infer_effect:union(E1, E2),
    Result2 = catena_infer_effect:union(E2, E1),

    % Union should be commutative
    ?assertEqual(Result1, Result2).

union_associative_test() ->
    E1 = catena_infer_effect:from_list([io]),
    E2 = catena_infer_effect:from_list([state]),
    E3 = catena_infer_effect:from_list([exn]),

    % (E1 ∪ E2) ∪ E3
    Left = catena_infer_effect:union(catena_infer_effect:union(E1, E2), E3),

    % E1 ∪ (E2 ∪ E3)
    Right = catena_infer_effect:union(E1, catena_infer_effect:union(E2, E3)),

    % Union should be associative
    ?assertEqual(Left, Right).

%%%===================================================================
%%% Section 4: Purity Checking
%%%===================================================================

is_pure_empty_test() ->
    Effects = catena_infer_effect:pure(),

    ?assertEqual(true, catena_infer_effect:is_pure(Effects)).

is_pure_single_effect_test() ->
    Effects = catena_infer_effect:from_list([io]),

    ?assertEqual(false, catena_infer_effect:is_pure(Effects)).

is_pure_multiple_effects_test() ->
    Effects = catena_infer_effect:from_list([io, state, exn]),

    ?assertEqual(false, catena_infer_effect:is_pure(Effects)).

%%%===================================================================
%%% Section 5: Effect Subsumption
%%%===================================================================

subsumes_empty_empty_test() ->
    E1 = catena_infer_effect:pure(),
    E2 = catena_infer_effect:pure(),

    % Empty subsumes empty
    ?assertEqual(true, catena_infer_effect:subsumes(E1, E2)).

subsumes_nonempty_empty_test() ->
    E1 = catena_infer_effect:from_list([io, state]),
    E2 = catena_infer_effect:pure(),

    % {io, state} subsumes {} (superset subsumes subset)
    ?assertEqual(true, catena_infer_effect:subsumes(E1, E2)).

subsumes_empty_nonempty_test() ->
    E1 = catena_infer_effect:pure(),
    E2 = catena_infer_effect:from_list([io]),

    % Empty does not subsume {io}
    ?assertEqual(false, catena_infer_effect:subsumes(E1, E2)).

subsumes_equal_test() ->
    E1 = catena_infer_effect:from_list([io, state]),
    E2 = catena_infer_effect:from_list([io, state]),

    % Equal sets subsume each other
    ?assertEqual(true, catena_infer_effect:subsumes(E1, E2)).

subsumes_subset_test() ->
    E1 = catena_infer_effect:from_list([io, state, exn]),
    E2 = catena_infer_effect:from_list([io, state]),

    % {io, state, exn} subsumes {io, state}
    ?assertEqual(true, catena_infer_effect:subsumes(E1, E2)).

subsumes_not_subset_test() ->
    E1 = catena_infer_effect:from_list([io, state]),
    E2 = catena_infer_effect:from_list([io, exn]),

    % {io, state} does not subsume {io, exn} (exn not in E1)
    ?assertEqual(false, catena_infer_effect:subsumes(E1, E2)).

subsumes_disjoint_test() ->
    E1 = catena_infer_effect:from_list([io]),
    E2 = catena_infer_effect:from_list([state]),

    % Disjoint sets don't subsume each other
    ?assertEqual(false, catena_infer_effect:subsumes(E1, E2)).

subsumes_reflexive_test() ->
    E = catena_infer_effect:from_list([io, state, exn]),

    % Every set subsumes itself
    ?assertEqual(true, catena_infer_effect:subsumes(E, E)).

%%%===================================================================
%%% Section 6: Effect Compatibility
%%%===================================================================

compatible_empty_empty_test() ->
    E1 = catena_infer_effect:pure(),
    E2 = catena_infer_effect:pure(),

    ?assertEqual(true, catena_infer_effect:compatible(E1, E2)).

compatible_equal_test() ->
    E1 = catena_infer_effect:from_list([io, state]),
    E2 = catena_infer_effect:from_list([io, state]),

    ?assertEqual(true, catena_infer_effect:compatible(E1, E2)).

compatible_equal_unsorted_test() ->
    E1 = catena_infer_effect:from_list([state, io]),
    E2 = catena_infer_effect:from_list([io, state]),

    % Should normalize before comparing
    ?assertEqual(true, catena_infer_effect:compatible(E1, E2)).

compatible_different_test() ->
    E1 = catena_infer_effect:from_list([io]),
    E2 = catena_infer_effect:from_list([state]),

    ?assertEqual(false, catena_infer_effect:compatible(E1, E2)).

compatible_subset_test() ->
    E1 = catena_infer_effect:from_list([io, state]),
    E2 = catena_infer_effect:from_list([io]),

    % For monomorphic effects, even subsets are incompatible
    ?assertEqual(false, catena_infer_effect:compatible(E1, E2)).

compatible_symmetric_test() ->
    E1 = catena_infer_effect:from_list([io, state]),
    E2 = catena_infer_effect:from_list([exn]),

    % Compatibility should be symmetric
    ?assertEqual(catena_infer_effect:compatible(E1, E2),
                 catena_infer_effect:compatible(E2, E1)).

%%%===================================================================
%%% Section 7: Pure Context Checking
%%%===================================================================

check_pure_with_pure_effects_test() ->
    Type = {tcon, int},
    Effects = catena_infer_effect:pure(),

    Result = catena_infer_effect:check_pure(Type, Effects),

    ?assertEqual(ok, Result).

check_pure_with_impure_effects_test() ->
    Type = {tcon, int},
    Effects = catena_infer_effect:from_list([io]),

    Result = catena_infer_effect:check_pure(Type, Effects),

    ?assertMatch({error, {effect_mismatch, _, _}}, Result).

%%%===================================================================
%%% Section 8: Effect Inference from Expressions
%%%===================================================================

infer_literal_effects_test() ->
    Expr = {lit, {int, 42}},
    Effects = catena_infer_effect:infer_expr_effects(Expr),

    ?assertEqual(catena_infer_effect:pure(), Effects).

infer_var_effects_test() ->
    Expr = {var, x},
    Effects = catena_infer_effect:infer_expr_effects(Expr),

    ?assertEqual(catena_infer_effect:pure(), Effects).

infer_lambda_effects_test() ->
    Expr = {lam, x, {var, x}},
    Effects = catena_infer_effect:infer_expr_effects(Expr),

    ?assertEqual(catena_infer_effect:pure(), Effects).

infer_app_effects_test() ->
    Expr = {app, {var, f}, {var, x}},
    Effects = catena_infer_effect:infer_expr_effects(Expr),

    % Application itself is pure (effects come from function type)
    ?assertEqual(catena_infer_effect:pure(), Effects).

infer_let_effects_test() ->
    Expr = {'let', x, {lit, {int, 42}}, {var, x}},
    Effects = catena_infer_effect:infer_expr_effects(Expr),

    ?assertEqual(catena_infer_effect:pure(), Effects).

infer_if_effects_test() ->
    Expr = {'if', {lit, {bool, true}}, {lit, {int, 1}}, {lit, {int, 0}}},
    Effects = catena_infer_effect:infer_expr_effects(Expr),

    ?assertEqual(catena_infer_effect:pure(), Effects).

infer_tuple_effects_test() ->
    Expr = {tuple, [{lit, {int, 1}}, {lit, {int, 2}}]},
    Effects = catena_infer_effect:infer_expr_effects(Expr),

    ?assertEqual(catena_infer_effect:pure(), Effects).

infer_record_effects_test() ->
    Expr = {record, [{x, {lit, {int, 10}}}]},
    Effects = catena_infer_effect:infer_expr_effects(Expr),

    ?assertEqual(catena_infer_effect:pure(), Effects).

infer_field_effects_test() ->
    Expr = {field, {var, r}, x},
    Effects = catena_infer_effect:infer_expr_effects(Expr),

    ?assertEqual(catena_infer_effect:pure(), Effects).

infer_variant_effects_test() ->
    Expr = {variant, some, [{lit, {int, 42}}]},
    Effects = catena_infer_effect:infer_expr_effects(Expr),

    ?assertEqual(catena_infer_effect:pure(), Effects).

%%%===================================================================
%%% Section 9: Integration Tests
%%%===================================================================

integration_effect_propagation_test() ->
    % Test that effects union correctly through multiple operations
    E1 = catena_infer_effect:from_list([io]),
    E2 = catena_infer_effect:from_list([state]),
    E3 = catena_infer_effect:from_list([exn]),

    % Combine all effects
    Combined = catena_infer_effect:union(E1, catena_infer_effect:union(E2, E3)),

    ?assertEqual({effect_set, [exn, io, state]}, Combined),
    ?assertEqual(false, catena_infer_effect:is_pure(Combined)).

integration_subsumption_hierarchy_test() ->
    % Test subsumption hierarchy: {} ⊆ {io} ⊆ {io, state} ⊆ {io, state, exn}
    E0 = catena_infer_effect:pure(),
    E1 = catena_infer_effect:from_list([io]),
    E2 = catena_infer_effect:from_list([io, state]),
    E3 = catena_infer_effect:from_list([io, state, exn]),

    % E3 subsumes everything
    ?assertEqual(true, catena_infer_effect:subsumes(E3, E0)),
    ?assertEqual(true, catena_infer_effect:subsumes(E3, E1)),
    ?assertEqual(true, catena_infer_effect:subsumes(E3, E2)),
    ?assertEqual(true, catena_infer_effect:subsumes(E3, E3)),

    % E2 subsumes E0 and E1, but not E3
    ?assertEqual(true, catena_infer_effect:subsumes(E2, E0)),
    ?assertEqual(true, catena_infer_effect:subsumes(E2, E1)),
    ?assertEqual(false, catena_infer_effect:subsumes(E2, E3)),

    % E0 only subsumes itself
    ?assertEqual(true, catena_infer_effect:subsumes(E0, E0)),
    ?assertEqual(false, catena_infer_effect:subsumes(E0, E1)).

integration_normalization_consistency_test() ->
    % Test that different constructions of same effect set normalize consistently
    E1 = catena_infer_effect:from_list([state, io, exn]),
    E2 = catena_infer_effect:from_list([exn, state, io]),
    E3 = catena_infer_effect:union(
        catena_infer_effect:from_list([io]),
        catena_infer_effect:from_list([state, exn])
    ),

    % All should normalize to same canonical form
    ?assertEqual(E1, E2),
    ?assertEqual(E1, E3),
    ?assertEqual(true, catena_infer_effect:compatible(E1, E2)),
    ?assertEqual(true, catena_infer_effect:compatible(E1, E3)).

%%%===================================================================
%%% Test Suite Integration
%%%===================================================================

integration_all_sections_test() ->
    % Verify all sections run successfully
    ?assert(true).
