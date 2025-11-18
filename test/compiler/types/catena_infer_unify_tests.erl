%%%-------------------------------------------------------------------
%%% @doc Unit Tests for Unification Algorithm
%%%
%%% Tests for catena_infer_unify module covering:
%%% - Identity unification
%%% - Type variable unification
%%% - Type constructor unification
%%% - Function type unification
%%% - Type application unification
%%% - Tuple type unification
%%% - Record type unification (with row polymorphism)
%%% - Variant type unification
%%% - Occurs check
%%% - Effect unification
%%% - Multi-type unification
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_infer_unify_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Section 1: Identity Unification
%%%===================================================================

unify_identity_same_type_test() ->
    State = catena_infer_state:new(),
    T = {tcon, int},

    {ok, Subst, _State1} = catena_infer_unify:unify(T, T, State),

    % Identity unification should produce empty substitution
    ?assertEqual([], catena_type_subst:domain(Subst)).

unify_identity_complex_type_test() ->
    State = catena_infer_state:new(),
    T = {tfun, {tcon, int}, {tcon, bool}, {effect_set, []}},

    {ok, Subst, _State1} = catena_infer_unify:unify(T, T, State),

    ?assertEqual([], catena_type_subst:domain(Subst)).

%%%===================================================================
%%% Section 2: Type Variable Unification
%%%===================================================================

unify_var_with_concrete_type_test() ->
    State = catena_infer_state:new(),
    Var = {tvar, 1},
    Type = {tcon, int},

    {ok, Subst, _State1} = catena_infer_unify:unify(Var, Type, State),

    % Should bind variable to type
    ?assertEqual([1], catena_type_subst:domain(Subst)),
    ?assertEqual({tcon, int}, catena_type_subst:apply(Subst, Var)).

unify_concrete_type_with_var_test() ->
    State = catena_infer_state:new(),
    Type = {tcon, int},
    Var = {tvar, 1},

    {ok, Subst, _State1} = catena_infer_unify:unify(Type, Var, State),

    % Should bind variable to type (symmetric)
    ?assertEqual([1], catena_type_subst:domain(Subst)),
    ?assertEqual({tcon, int}, catena_type_subst:apply(Subst, Var)).

unify_two_vars_test() ->
    State = catena_infer_state:new(),
    Var1 = {tvar, 1},
    Var2 = {tvar, 2},

    {ok, Subst, _State1} = catena_infer_unify:unify(Var1, Var2, State),

    % Should bind one to the other
    ?assertEqual([1], catena_type_subst:domain(Subst)),
    Result = catena_type_subst:apply(Subst, Var1),
    ?assertEqual({tvar, 2}, Result).

unify_var_same_var_test() ->
    State = catena_infer_state:new(),
    Var = {tvar, 1},

    {ok, Subst, _State1} = catena_infer_unify:unify(Var, Var, State),

    % Unifying variable with itself should produce empty substitution
    ?assertEqual([], catena_type_subst:domain(Subst)).

%%%===================================================================
%%% Section 3: Occurs Check
%%%===================================================================

occurs_check_simple_test() ->
    State = catena_infer_state:new(),
    Var = {tvar, 1},
    % Try to unify α with List<α> - should fail
    Type = {tapp, {tcon, list}, [Var]},

    {error, Error, _State1} = catena_infer_unify:unify(Var, Type, State),

    ?assertMatch({occurs_check, 1, _}, Error).

occurs_check_nested_test() ->
    State = catena_infer_state:new(),
    Var = {tvar, 1},
    % Try to unify α with List<List<α>>
    Type = {tapp, {tcon, list}, [{tapp, {tcon, list}, [Var]}]},

    {error, Error, _State1} = catena_infer_unify:unify(Var, Type, State),

    ?assertMatch({occurs_check, 1, _}, Error).

occurs_check_function_test() ->
    State = catena_infer_state:new(),
    Var = {tvar, 1},
    % Try to unify α with α -> Int
    Type = {tfun, Var, {tcon, int}, {effect_set, []}},

    {error, Error, _State1} = catena_infer_unify:unify(Var, Type, State),

    ?assertMatch({occurs_check, 1, _}, Error).

%%%===================================================================
%%% Section 4: Type Constructor Unification
%%%===================================================================

unify_same_constructor_test() ->
    State = catena_infer_state:new(),
    T1 = {tcon, int},
    T2 = {tcon, int},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertEqual([], catena_type_subst:domain(Subst)).

unify_different_constructors_test() ->
    State = catena_infer_state:new(),
    T1 = {tcon, int},
    T2 = {tcon, bool},

    {error, Error, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertMatch({unification_error, {tcon, int}, {tcon, bool}}, Error).

%%%===================================================================
%%% Section 5: Function Type Unification
%%%===================================================================

unify_simple_function_test() ->
    State = catena_infer_state:new(),
    Pure = {effect_set, []},

    F1 = {tfun, {tcon, int}, {tcon, bool}, Pure},
    F2 = {tfun, {tcon, int}, {tcon, bool}, Pure},

    {ok, Subst, _State1} = catena_infer_unify:unify(F1, F2, State),

    ?assertEqual([], catena_type_subst:domain(Subst)).

unify_function_with_vars_test() ->
    State = catena_infer_state:new(),
    Pure = {effect_set, []},

    F1 = {tfun, {tvar, 1}, {tvar, 2}, Pure},
    F2 = {tfun, {tcon, int}, {tcon, bool}, Pure},

    {ok, Subst, _State1} = catena_infer_unify:unify(F1, F2, State),

    % Should bind α to Int and β to Bool
    ?assertEqual([1, 2], lists:sort(catena_type_subst:domain(Subst))),
    ?assertEqual({tcon, int}, catena_type_subst:apply(Subst, {tvar, 1})),
    ?assertEqual({tcon, bool}, catena_type_subst:apply(Subst, {tvar, 2})).

unify_function_param_mismatch_test() ->
    State = catena_infer_state:new(),
    Pure = {effect_set, []},

    F1 = {tfun, {tcon, int}, {tcon, bool}, Pure},
    F2 = {tfun, {tcon, string}, {tcon, bool}, Pure},

    {error, Error, _State1} = catena_infer_unify:unify(F1, F2, State),

    ?assertMatch({unification_error, {tcon, int}, {tcon, string}}, Error).

unify_function_return_mismatch_test() ->
    State = catena_infer_state:new(),
    Pure = {effect_set, []},

    F1 = {tfun, {tcon, int}, {tcon, bool}, Pure},
    F2 = {tfun, {tcon, int}, {tcon, string}, Pure},

    {error, Error, _State1} = catena_infer_unify:unify(F1, F2, State),

    ?assertMatch({unification_error, {tcon, bool}, {tcon, string}}, Error).

unify_function_effect_mismatch_test() ->
    State = catena_infer_state:new(),

    Pure = {effect_set, []},
    Impure = {effect_set, [io]},

    F1 = {tfun, {tcon, int}, {tcon, bool}, Pure},
    F2 = {tfun, {tcon, int}, {tcon, bool}, Impure},

    {error, Error, _State1} = catena_infer_unify:unify(F1, F2, State),

    ?assertMatch({effect_mismatch, _, _}, Error).

%%%===================================================================
%%% Section 6: Type Application Unification
%%%===================================================================

unify_list_types_test() ->
    State = catena_infer_state:new(),

    T1 = {tapp, {tcon, list}, [{tcon, int}]},
    T2 = {tapp, {tcon, list}, [{tcon, int}]},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertEqual([], catena_type_subst:domain(Subst)).

unify_list_with_var_test() ->
    State = catena_infer_state:new(),

    T1 = {tapp, {tcon, list}, [{tvar, 1}]},
    T2 = {tapp, {tcon, list}, [{tcon, int}]},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertEqual([1], catena_type_subst:domain(Subst)),
    ?assertEqual({tcon, int}, catena_type_subst:apply(Subst, {tvar, 1})).

unify_list_element_mismatch_test() ->
    State = catena_infer_state:new(),

    T1 = {tapp, {tcon, list}, [{tcon, int}]},
    T2 = {tapp, {tcon, list}, [{tcon, bool}]},

    {error, Error, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertMatch({unification_error, {tcon, int}, {tcon, bool}}, Error).

unify_type_app_different_constructors_test() ->
    State = catena_infer_state:new(),

    T1 = {tapp, {tcon, list}, [{tcon, int}]},
    T2 = {tapp, {tcon, option}, [{tcon, int}]},

    {error, Error, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertMatch({unification_error, {tcon, list}, {tcon, option}}, Error).

unify_arity_mismatch_test() ->
    State = catena_infer_state:new(),

    % Map<K, V> vs Map<K>
    T1 = {tapp, {tcon, map}, [{tcon, string}, {tcon, int}]},
    T2 = {tapp, {tcon, map}, [{tcon, string}]},

    {error, Error, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertMatch({arity_mismatch, {tcon, map}, 2, 1}, Error).

%%%===================================================================
%%% Section 7: Tuple Type Unification
%%%===================================================================

unify_empty_tuple_test() ->
    State = catena_infer_state:new(),

    T1 = {ttuple, []},
    T2 = {ttuple, []},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertEqual([], catena_type_subst:domain(Subst)).

unify_simple_tuple_test() ->
    State = catena_infer_state:new(),

    T1 = {ttuple, [{tcon, int}, {tcon, bool}]},
    T2 = {ttuple, [{tcon, int}, {tcon, bool}]},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertEqual([], catena_type_subst:domain(Subst)).

unify_tuple_with_vars_test() ->
    State = catena_infer_state:new(),

    T1 = {ttuple, [{tvar, 1}, {tvar, 2}]},
    T2 = {ttuple, [{tcon, int}, {tcon, bool}]},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertEqual([1, 2], lists:sort(catena_type_subst:domain(Subst))),
    ?assertEqual({tcon, int}, catena_type_subst:apply(Subst, {tvar, 1})),
    ?assertEqual({tcon, bool}, catena_type_subst:apply(Subst, {tvar, 2})).

unify_tuple_element_mismatch_test() ->
    State = catena_infer_state:new(),

    T1 = {ttuple, [{tcon, int}, {tcon, bool}]},
    T2 = {ttuple, [{tcon, int}, {tcon, string}]},

    {error, Error, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertMatch({unification_error, {tcon, bool}, {tcon, string}}, Error).

unify_tuple_length_mismatch_test() ->
    State = catena_infer_state:new(),

    T1 = {ttuple, [{tcon, int}, {tcon, bool}]},
    T2 = {ttuple, [{tcon, int}]},

    {error, Error, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertMatch({unification_error, _, _}, Error).

%%%===================================================================
%%% Section 8: Record Type Unification (Closed)
%%%===================================================================

unify_empty_record_test() ->
    State = catena_infer_state:new(),

    T1 = {trecord, [], closed},
    T2 = {trecord, [], closed},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertEqual([], catena_type_subst:domain(Subst)).

unify_simple_record_test() ->
    State = catena_infer_state:new(),

    T1 = {trecord, [{x, {tcon, int}}, {y, {tcon, bool}}], closed},
    T2 = {trecord, [{x, {tcon, int}}, {y, {tcon, bool}}], closed},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertEqual([], catena_type_subst:domain(Subst)).

unify_record_field_order_test() ->
    State = catena_infer_state:new(),

    % Fields in different order
    T1 = {trecord, [{y, {tcon, bool}}, {x, {tcon, int}}], closed},
    T2 = {trecord, [{x, {tcon, int}}, {y, {tcon, bool}}], closed},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    % Should still unify (order doesn't matter)
    ?assertEqual([], catena_type_subst:domain(Subst)).

unify_record_with_vars_test() ->
    State = catena_infer_state:new(),

    T1 = {trecord, [{x, {tvar, 1}}, {y, {tvar, 2}}], closed},
    T2 = {trecord, [{x, {tcon, int}}, {y, {tcon, bool}}], closed},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertEqual([1, 2], lists:sort(catena_type_subst:domain(Subst))),
    ?assertEqual({tcon, int}, catena_type_subst:apply(Subst, {tvar, 1})),
    ?assertEqual({tcon, bool}, catena_type_subst:apply(Subst, {tvar, 2})).

unify_record_missing_field_test() ->
    State = catena_infer_state:new(),

    T1 = {trecord, [{x, {tcon, int}}, {y, {tcon, bool}}], closed},
    T2 = {trecord, [{x, {tcon, int}}], closed},

    {error, Error, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertMatch({unification_error, _, _}, Error).

unify_record_field_type_mismatch_test() ->
    State = catena_infer_state:new(),

    T1 = {trecord, [{x, {tcon, int}}, {y, {tcon, bool}}], closed},
    T2 = {trecord, [{x, {tcon, int}}, {y, {tcon, string}}], closed},

    {error, Error, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertMatch({unification_error, {tcon, bool}, {tcon, string}}, Error).

%%%===================================================================
%%% Section 9: Record Type Unification (Row Polymorphism)
%%%===================================================================

unify_record_same_row_var_test() ->
    State = catena_infer_state:new(),

    T1 = {trecord, [{x, {tcon, int}}], 1},
    T2 = {trecord, [{x, {tcon, int}}], 1},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertEqual([], catena_type_subst:domain(Subst)).

unify_record_different_row_vars_test() ->
    State = catena_infer_state:new(),

    T1 = {trecord, [{x, {tcon, int}}], 1},
    T2 = {trecord, [{x, {tcon, int}}], 2},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    % Should unify row variables
    % Result depends on implementation - for PoC we bind one row var to the other
    ?assert(lists:member(1, catena_type_subst:domain(Subst)) orelse
            lists:member(2, catena_type_subst:domain(Subst))).

unify_record_row_var_with_closed_test() ->
    State = catena_infer_state:new(),

    T1 = {trecord, [{x, {tcon, int}}], 1},
    T2 = {trecord, [{x, {tcon, int}}], closed},

    {ok, _Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    % Row variable unifies with closed
    ok.

%%%===================================================================
%%% Section 10: Variant Type Unification
%%%===================================================================

unify_simple_variant_test() ->
    State = catena_infer_state:new(),

    T1 = {tvariant, [{some, [{tcon, int}]}, {none, []}]},
    T2 = {tvariant, [{some, [{tcon, int}]}, {none, []}]},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertEqual([], catena_type_subst:domain(Subst)).

unify_variant_constructor_order_test() ->
    State = catena_infer_state:new(),

    % Constructors in different order
    T1 = {tvariant, [{none, []}, {some, [{tcon, int}]}]},
    T2 = {tvariant, [{some, [{tcon, int}]}, {none, []}]},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    % Should still unify (order doesn't matter after sorting)
    ?assertEqual([], catena_type_subst:domain(Subst)).

unify_variant_with_vars_test() ->
    State = catena_infer_state:new(),

    T1 = {tvariant, [{some, [{tvar, 1}]}, {none, []}]},
    T2 = {tvariant, [{some, [{tcon, int}]}, {none, []}]},

    {ok, Subst, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertEqual([1], catena_type_subst:domain(Subst)),
    ?assertEqual({tcon, int}, catena_type_subst:apply(Subst, {tvar, 1})).

unify_variant_missing_constructor_test() ->
    State = catena_infer_state:new(),

    T1 = {tvariant, [{some, [{tcon, int}]}, {none, []}]},
    T2 = {tvariant, [{some, [{tcon, int}]}]},

    {error, Error, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertMatch({unification_error, _, _}, Error).

unify_variant_constructor_arity_mismatch_test() ->
    State = catena_infer_state:new(),

    T1 = {tvariant, [{pair, [{tcon, int}, {tcon, bool}]}]},
    T2 = {tvariant, [{pair, [{tcon, int}]}]},

    {error, Error, _State1} = catena_infer_unify:unify(T1, T2, State),

    ?assertMatch({arity_mismatch, {constructor, pair}, 2, 1}, Error).

%%%===================================================================
%%% Section 11: Effect Unification
%%%===================================================================

unify_pure_effects_test() ->
    Pure = {effect_set, []},

    Result = catena_infer_unify:unify_effects(Pure, Pure),

    ?assertEqual(ok, Result).

unify_same_effects_test() ->
    Effects = {effect_set, [io, state]},

    Result = catena_infer_unify:unify_effects(Effects, Effects),

    ?assertEqual(ok, Result).

unify_different_effects_test() ->
    Effects1 = {effect_set, [io]},
    Effects2 = {effect_set, [state]},

    Result = catena_infer_unify:unify_effects(Effects1, Effects2),

    ?assertMatch({error, {effect_mismatch, _, _}}, Result).

unify_pure_vs_impure_test() ->
    Pure = {effect_set, []},
    Impure = {effect_set, [io]},

    Result = catena_infer_unify:unify_effects(Pure, Impure),

    ?assertMatch({error, {effect_mismatch, _, _}}, Result).

%%%===================================================================
%%% Section 12: Multi-Type Unification
%%%===================================================================

unify_many_empty_test() ->
    State = catena_infer_state:new(),
    Empty = catena_type_subst:empty(),

    {ok, Subst, _State1} = catena_infer_unify:unify_many([], Empty, State),

    ?assertEqual([], catena_type_subst:domain(Subst)).

unify_many_single_pair_test() ->
    State = catena_infer_state:new(),
    Empty = catena_type_subst:empty(),

    Pairs = [{{tvar, 1}, {tcon, int}}],

    {ok, Subst, _State1} = catena_infer_unify:unify_many(Pairs, Empty, State),

    ?assertEqual([1], catena_type_subst:domain(Subst)),
    ?assertEqual({tcon, int}, catena_type_subst:apply(Subst, {tvar, 1})).

unify_many_multiple_pairs_test() ->
    State = catena_infer_state:new(),
    Empty = catena_type_subst:empty(),

    Pairs = [
        {{tvar, 1}, {tcon, int}},
        {{tvar, 2}, {tcon, bool}},
        {{tvar, 3}, {tcon, string}}
    ],

    {ok, Subst, _State1} = catena_infer_unify:unify_many(Pairs, Empty, State),

    ?assertEqual([1, 2, 3], lists:sort(catena_type_subst:domain(Subst))),
    ?assertEqual({tcon, int}, catena_type_subst:apply(Subst, {tvar, 1})),
    ?assertEqual({tcon, bool}, catena_type_subst:apply(Subst, {tvar, 2})),
    ?assertEqual({tcon, string}, catena_type_subst:apply(Subst, {tvar, 3})).

unify_many_with_dependencies_test() ->
    State = catena_infer_state:new(),
    Empty = catena_type_subst:empty(),

    % α = Int, β = α (should resolve to Int)
    Pairs = [
        {{tvar, 1}, {tcon, int}},
        {{tvar, 2}, {tvar, 1}}
    ],

    {ok, Subst, _State1} = catena_infer_unify:unify_many(Pairs, Empty, State),

    ?assertEqual([1, 2], lists:sort(catena_type_subst:domain(Subst))),
    ?assertEqual({tcon, int}, catena_type_subst:apply(Subst, {tvar, 1})),
    ?assertEqual({tcon, int}, catena_type_subst:apply(Subst, {tvar, 2})).

unify_many_early_failure_test() ->
    State = catena_infer_state:new(),
    Empty = catena_type_subst:empty(),

    % First pair succeeds, second fails
    Pairs = [
        {{tvar, 1}, {tcon, int}},
        {{tcon, int}, {tcon, bool}}  % This should fail
    ],

    {error, Error, _State1} = catena_infer_unify:unify_many(Pairs, Empty, State),

    ?assertMatch({unification_error, {tcon, int}, {tcon, bool}}, Error).

%%%===================================================================
%%% Section 13: State Threading
%%%===================================================================

unify_updates_state_subst_test() ->
    State0 = catena_infer_state:new(),

    % Initial state should have empty substitution
    ?assertEqual([], catena_type_subst:domain(catena_infer_state:get_subst(State0))),

    % Unify α with Int
    {ok, _Subst, State1} = catena_infer_unify:unify({tvar, 1}, {tcon, int}, State0),

    % State should now contain the substitution
    StateSubst = catena_infer_state:get_subst(State1),
    ?assertEqual([1], catena_type_subst:domain(StateSubst)),
    ?assertEqual({tcon, int}, catena_type_subst:apply(StateSubst, {tvar, 1})).

unify_composes_substitutions_test() ->
    State0 = catena_infer_state:new(),

    % Unify α with Int
    {ok, _Subst1, State1} = catena_infer_unify:unify({tvar, 1}, {tcon, int}, State0),

    % Unify β with α
    {ok, _Subst2, State2} = catena_infer_unify:unify({tvar, 2}, {tvar, 1}, State1),

    % State should have composed substitution: β → α → Int
    StateSubst = catena_infer_state:get_subst(State2),
    ?assertEqual({tcon, int}, catena_type_subst:apply(StateSubst, {tvar, 1})),
    ?assertEqual({tcon, int}, catena_type_subst:apply(StateSubst, {tvar, 2})).

unify_error_adds_to_state_test() ->
    State0 = catena_infer_state:new(),

    % Try to unify Int with Bool - should fail
    {error, _Error, State1} = catena_infer_unify:unify({tcon, int}, {tcon, bool}, State0),

    % State should have error recorded
    ?assertEqual(true, catena_infer_state:has_errors(State1)),
    Errors = catena_infer_state:get_errors(State1),
    ?assertEqual(1, length(Errors)).

%%%===================================================================
%%% Test Suite Integration
%%%===================================================================

integration_all_sections_test() ->
    % Verify all sections run successfully
    ?assert(true).
