%%%-------------------------------------------------------------------
%%% @doc Unit Tests for catena_type_scheme module
%%% @end
%%%-------------------------------------------------------------------
-module(catena_type_scheme_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Type Scheme Construction Tests
%%====================================================================

construction_test_() ->
    [
     ?_test(test_mono()),
     ?_test(test_poly())
    ].

test_mono() ->
    IntType = catena_types:tcon(integer),
    Scheme = catena_type_scheme:mono(IntType),

    ?assertEqual({mono, {tcon, integer}}, Scheme).

test_poly() ->
    % ∀α. α -> α
    VarAlpha = catena_types:tvar(1),
    FunType = catena_types:tfun(VarAlpha, VarAlpha, catena_types:empty_effects()),

    Scheme = catena_type_scheme:poly([1], FunType),
    ?assertMatch({poly, [1], {tfun, _, _, _}}, Scheme).

%%====================================================================
%% Generalization Tests
%%====================================================================

generalization_test_() ->
    [
     ?_test(test_generalize_no_free_env()),
     ?_test(test_generalize_with_free_env()),
     ?_test(test_generalize_all_bound()),
     ?_test(test_generalize_concrete_type())
    ].

test_generalize_no_free_env() ->
    % Type: α -> β
    % Env free vars: {}
    % Expected: ∀α β. α -> β
    Type = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tvar(2),
        catena_types:empty_effects()
    ),

    EnvFreeVars = sets:new(),
    Scheme = catena_type_scheme:generalize(Type, EnvFreeVars),

    ?assertMatch({poly, [1, 2], _}, Scheme).

test_generalize_with_free_env() ->
    % Type: α -> β
    % Env free vars: {β}
    % Expected: ∀α. α -> β  (β not quantified because it's free in env)
    Type = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tvar(2),
        catena_types:empty_effects()
    ),

    EnvFreeVars = sets:from_list([2]),  % β is free in environment
    Scheme = catena_type_scheme:generalize(Type, EnvFreeVars),

    ?assertMatch({poly, [1], _}, Scheme),  % Only α is quantified

    % Verify β is still in the type but not quantified
    {poly, QuantVars, _} = Scheme,
    ?assertEqual([1], QuantVars).

test_generalize_all_bound() ->
    % Type: α -> β
    % Env free vars: {α, β}
    % Expected: α -> β (monomorphic because all vars are free in env)
    Type = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tvar(2),
        catena_types:empty_effects()
    ),

    EnvFreeVars = sets:from_list([1, 2]),  % Both free in environment
    Scheme = catena_type_scheme:generalize(Type, EnvFreeVars),

    ?assertMatch({mono, _}, Scheme).

test_generalize_concrete_type() ->
    % Type: Int -> String
    % Env free vars: {}
    % Expected: Int -> String (monomorphic, no type variables)
    Type = catena_types:tfun(
        catena_types:tcon(integer),
        catena_types:tcon(string),
        catena_types:empty_effects()
    ),

    EnvFreeVars = sets:new(),
    Scheme = catena_type_scheme:generalize(Type, EnvFreeVars),

    ?assertMatch({mono, _}, Scheme).

%%====================================================================
%% Instantiation Tests
%%====================================================================

instantiation_test_() ->
    [
     ?_test(test_instantiate_mono()),
     ?_test(test_instantiate_poly()),
     ?_test(test_instantiate_creates_fresh())
    ].

test_instantiate_mono() ->
    % Monomorphic schemes instantiate to themselves
    State = catena_infer_state:new(),
    Type = catena_types:tcon(integer),
    Scheme = catena_type_scheme:mono(Type),

    {Instantiated, Constraints, _NewState} = catena_type_scheme:instantiate(Scheme, State),
    ?assertEqual(Type, Instantiated),
    % Monomorphic types have no constraints
    ?assertEqual([], Constraints).

test_instantiate_poly() ->
    State = catena_infer_state:new(),

    % ∀α. α -> α (using ID 100 to distinguish from fresh vars)
    VarAlpha = catena_types:tvar(100),
    FunType = catena_types:tfun(VarAlpha, VarAlpha, catena_types:empty_effects()),
    Scheme = catena_type_scheme:poly([100], FunType),

    % Instantiate should create fresh variables
    {Instantiated, Constraints, _NewState} = catena_type_scheme:instantiate(Scheme, State),

    % Should be a function type with same structure
    ?assertMatch({tfun, _, _, _}, Instantiated),

    % But with fresh variables (should be ID 1, not 100)
    {tfun, From, To, _} = Instantiated,
    ?assertMatch({tvar, 1}, From),
    ?assertMatch({tvar, 1}, To),

    % From and To should be the same variable (both were α)
    {tvar, FromId} = From,
    {tvar, ToId} = To,
    ?assertEqual(FromId, ToId),

    % Polymorphic types without explicit constraints return empty list
    ?assertEqual([], Constraints).

test_instantiate_creates_fresh() ->
    State0 = catena_infer_state:new(),

    % ∀α β. (α -> β) (using IDs 100, 200)
    Type = catena_types:tfun(
        catena_types:tvar(100),
        catena_types:tvar(200),
        catena_types:empty_effects()
    ),
    Scheme = catena_type_scheme:poly([100, 200], Type),

    % Two instantiations should create different fresh variables
    {Inst1, _Constraints1, State1} = catena_type_scheme:instantiate(Scheme, State0),
    {Inst2, _Constraints2, _State2} = catena_type_scheme:instantiate(Scheme, State1),

    ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, Inst1),
    ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, Inst2),

    % First instantiation should use IDs 1 and 2
    ?assertMatch({tfun, {tvar, 1}, {tvar, 2}, _}, Inst1),

    % Second instantiation should use IDs 3 and 4 (different from first)
    ?assertMatch({tfun, {tvar, 3}, {tvar, 4}, _}, Inst2).

%%====================================================================
%% Free Type Variables Tests
%%====================================================================

ftv_test_() ->
    [
     ?_test(test_ftv_mono()),
     ?_test(test_ftv_poly_no_free()),
     ?_test(test_ftv_poly_with_free())
    ].

test_ftv_mono() ->
    % Mono type: α -> β
    % All variables are free
    Type = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tvar(2),
        catena_types:empty_effects()
    ),
    Scheme = catena_type_scheme:mono(Type),

    Ftv = catena_type_scheme:ftv_scheme(Scheme),
    FtvList = lists:sort(sets:to_list(Ftv)),

    ?assertEqual([1, 2], FtvList).

test_ftv_poly_no_free() ->
    % ∀α β. α -> β
    % All variables are quantified, no free variables
    Type = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tvar(2),
        catena_types:empty_effects()
    ),
    Scheme = catena_type_scheme:poly([1, 2], Type),

    Ftv = catena_type_scheme:ftv_scheme(Scheme),
    FtvList = lists:sort(sets:to_list(Ftv)),

    ?assertEqual([], FtvList).

test_ftv_poly_with_free() ->
    % ∀α. α -> β
    % β is free (not quantified)
    Type = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tvar(2),
        catena_types:empty_effects()
    ),
    Scheme = catena_type_scheme:poly([1], Type),  % Only quantify α

    Ftv = catena_type_scheme:ftv_scheme(Scheme),
    FtvList = lists:sort(sets:to_list(Ftv)),

    ?assertEqual([2], FtvList).  % Only β is free

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
     ?_test(test_generalize_then_instantiate())
    ].

test_generalize_then_instantiate() ->
    State0 = catena_infer_state:new(),

    % Use fresh_var to create variables (this advances the counter)
    {{tvar, Var1}, State1} = catena_types:fresh_var(State0),
    {{tvar, Var2}, State2} = catena_types:fresh_var(State1),

    % Create type: α₁ -> α₂
    Type = catena_types:tfun(
        catena_types:tvar(Var1),
        catena_types:tvar(Var2),
        catena_types:empty_effects()
    ),

    % Generalize with empty environment
    EnvFreeVars = sets:new(),
    Scheme = catena_type_scheme:generalize(Type, EnvFreeVars),

    % Should be polymorphic with both vars quantified
    ?assertMatch({poly, _, _}, Scheme),

    % Instantiate should create fresh variables
    {Inst, _Constraints, _State3} = catena_type_scheme:instantiate(Scheme, State2),

    % Should have structure with fresh vars
    ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, Inst),

    {tfun, {tvar, FromId}, {tvar, ToId}, _} = Inst,

    % Fresh variables should be different from each other
    ?assertNotEqual(FromId, ToId),

    % And different from the original variables
    ?assertNotEqual(Var1, FromId),
    ?assertNotEqual(Var1, ToId),
    ?assertNotEqual(Var2, FromId),
    ?assertNotEqual(Var2, ToId).
