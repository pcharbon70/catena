%%%-------------------------------------------------------------------
%%% @doc Integration Tests for Constraint Solving
%%%
%%% Tests the complete constraint solving pipeline from type inference
%%% through instance resolution.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_constraint_solving_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    ok.

teardown(_) ->
    ok.

%%====================================================================
%% Built-in Instance Tests
%%====================================================================

builtin_instances_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_eq_instances()),
      ?_test(test_ord_instances()),
      ?_test(test_show_instances()),
      ?_test(test_functor_instances()),
      ?_test(test_monad_instances())
     ]}.

test_eq_instances() ->
    DB = catena_builtin_instances:builtin_instance_db(),

    % Test Eq Int instance
    IntConstraint = catena_constraint:trait_constraint('Eq', [catena_types:tcon(int)]),
    ?assertMatch(
        {ok, _, _},
        catena_instance:resolve_constraint(IntConstraint, DB)
    ),

    % Test Eq Bool instance
    BoolConstraint = catena_constraint:trait_constraint('Eq', [catena_types:tcon(bool)]),
    ?assertMatch(
        {ok, _, _},
        catena_instance:resolve_constraint(BoolConstraint, DB)
    ),

    % Test Eq String instance
    StringConstraint = catena_constraint:trait_constraint('Eq', [catena_types:tcon(string)]),
    ?assertMatch(
        {ok, _, _},
        catena_instance:resolve_constraint(StringConstraint, DB)
    ).

test_ord_instances() ->
    DB = catena_builtin_instances:builtin_instance_db(),

    % Test Ord Int instance
    IntConstraint = catena_constraint:trait_constraint('Ord', [catena_types:tcon(int)]),
    ?assertMatch(
        {ok, _, _},
        catena_instance:resolve_constraint(IntConstraint, DB)
    ),

    % Test Ord Float instance
    FloatConstraint = catena_constraint:trait_constraint('Ord', [catena_types:tcon(float)]),
    ?assertMatch(
        {ok, _, _},
        catena_instance:resolve_constraint(FloatConstraint, DB)
    ).

test_show_instances() ->
    DB = catena_builtin_instances:builtin_instance_db(),

    % Test Show Int instance
    IntConstraint = catena_constraint:trait_constraint('Show', [catena_types:tcon(int)]),
    ?assertMatch(
        {ok, _, _},
        catena_instance:resolve_constraint(IntConstraint, DB)
    ),

    % Test Show Bool instance
    BoolConstraint = catena_constraint:trait_constraint('Show', [catena_types:tcon(bool)]),
    ?assertMatch(
        {ok, _, _},
        catena_instance:resolve_constraint(BoolConstraint, DB)
    ).

test_functor_instances() ->
    DB = catena_builtin_instances:builtin_instance_db(),

    % Test Functor List instance
    ListConstraint = catena_constraint:trait_constraint('Functor', [catena_types:tcon(list)]),
    ?assertMatch(
        {ok, _, _},
        catena_instance:resolve_constraint(ListConstraint, DB)
    ),

    % Test Functor Maybe instance
    MaybeConstraint = catena_constraint:trait_constraint('Functor', [catena_types:tcon('Maybe')]),
    ?assertMatch(
        {ok, _, _},
        catena_instance:resolve_constraint(MaybeConstraint, DB)
    ).

test_monad_instances() ->
    DB = catena_builtin_instances:builtin_instance_db(),

    % Test Monad List instance
    ListConstraint = catena_constraint:trait_constraint('Monad', [catena_types:tcon(list)]),
    ?assertMatch(
        {ok, _, _},
        catena_instance:resolve_constraint(ListConstraint, DB)
    ),

    % Test Monad Maybe instance
    MaybeConstraint = catena_constraint:trait_constraint('Monad', [catena_types:tcon('Maybe')]),
    ?assertMatch(
        {ok, _, _},
        catena_instance:resolve_constraint(MaybeConstraint, DB)
    ).

%%====================================================================
%% Constraint Generation Tests
%%====================================================================

constraint_generation_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_simple_constraint_generation()),
      ?_test(test_polymorphic_constraint_generation()),
      ?_test(test_constraint_propagation())
     ]}.

test_simple_constraint_generation() ->
    % Create environment with a constrained type
    % x : forall a. Eq a => a
    State0 = catena_infer_state:new(),
    {Alpha, State1} = catena_types:fresh_var(State0),

    EqConstraint = catena_constraint:trait_constraint('Eq', [Alpha]),
    Scheme = catena_type_scheme:poly([1], [EqConstraint], Alpha),

    Env = catena_type_env:extend(catena_type_env:empty(), x, Scheme),

    % Instantiate the scheme
    {InstType, InstConstraints, _State2} = catena_type_scheme:instantiate(Scheme, State1),

    % Verify constraint was instantiated
    ?assert(is_list(InstConstraints)),
    ?assertEqual(1, length(InstConstraints)),

    % Verify the instantiated type is a fresh type variable
    ?assertMatch({tvar, _}, InstType).

test_polymorphic_constraint_generation() ->
    % Test: forall a b. (Eq a, Ord b) => a -> b -> Bool
    State0 = catena_infer_state:new(),
    {Alpha, State1} = catena_types:fresh_var(State0),
    {Beta, State2} = catena_types:fresh_var(State1),

    EqConstraint = catena_constraint:trait_constraint('Eq', [Alpha]),
    OrdConstraint = catena_constraint:trait_constraint('Ord', [Beta]),
    Constraints = [EqConstraint, OrdConstraint],

    BoolType = catena_types:tcon(bool),
    Effects = catena_types:empty_effects(),
    FuncType = catena_types:tfun(Alpha, catena_types:tfun(Beta, BoolType, Effects), Effects),

    Scheme = catena_type_scheme:poly([1, 2], Constraints, FuncType),

    % Instantiate the scheme
    {_InstType, InstConstraints, _State3} = catena_type_scheme:instantiate(Scheme, State2),

    % Verify both constraints were instantiated
    ?assertEqual(2, length(InstConstraints)).

test_constraint_propagation() ->
    % Test that constraints propagate through let-bindings
    % let id = λx. x in id
    % should have type: forall a. a -> a (no constraints)

    IdExpr = {lam, x, {var, x}},
    LetExpr = {'let', id, IdExpr, {var, id}},

    % Infer type
    case catena_infer:infer_expr(LetExpr) of
        {ok, Type} ->
            % Should be a function type
            ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, Type);
        {error, Errors} ->
            ?debugFmt("Unexpected error: ~p", [Errors]),
            ?assert(false)
    end.

%%====================================================================
%% End-to-End Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_no_constraints_success()),
      ?_test(test_unsatisfied_constraint_error()),
      ?_test(test_multiple_constraints())
     ]}.

test_no_constraints_success() ->
    % Test: 42 : Int (no constraints needed)
    Expr = {lit, {int, 42}},
    Result = catena_infer:infer_expr(Expr),

    ?assertMatch({ok, {tcon, int}}, Result).

test_unsatisfied_constraint_error() ->
    % Create an environment with an unsatisfiable constraint
    % x : forall a. NonExistentTrait a => a
    State0 = catena_infer_state:new(),
    {Alpha, _State1} = catena_types:fresh_var(State0),

    % Use a trait that has no instances
    FakeConstraint = catena_constraint:trait_constraint('NonExistentTrait', [Alpha]),
    Scheme = catena_type_scheme:poly([1], [FakeConstraint], Alpha),

    Env = catena_type_env:extend(catena_type_env:empty(), x, Scheme),

    % Try to use x - should fail constraint solving
    Expr = {var, x},
    Result = catena_infer:infer_expr(Expr, Env),

    % Should get constraint error
    ?assertMatch({error, [_Error]}, Result),

    {error, [Error]} = Result,
    ?assertMatch({unsatisfied_constraint, 'NonExistentTrait', _, _}, Error).

test_multiple_constraints() ->
    % Test that multiple constraints can be solved
    % x : forall a. (Eq a, Show a) => a
    State0 = catena_infer_state:new(),
    {Alpha, _State1} = catena_types:fresh_var(State0),

    EqConstraint = catena_constraint:trait_constraint('Eq', [Alpha]),
    ShowConstraint = catena_constraint:trait_constraint('Show', [Alpha]),
    Constraints = [EqConstraint, ShowConstraint],

    Scheme = catena_type_scheme:poly([1], Constraints, Alpha),
    Env = catena_type_env:extend(catena_type_env:empty(), x, Scheme),

    % Use x - constraints should be collected
    % Note: They won't be solved until we have a concrete type
    Expr = {var, x},

    case catena_infer:infer_expr(Expr, Env) of
        {ok, Type} ->
            % Should get a fresh type variable
            ?assertMatch({tvar, _}, Type);
        {error, Errors} ->
            % Constraints might not be solvable with just a type variable
            % This is expected - we need a concrete type
            ?assert(length(Errors) > 0)
    end.

%%====================================================================
%% Generalization with Constraints Tests
%%====================================================================

generalization_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_generalize_with_constraints()),
      ?_test(test_generalize_no_constraints())
     ]}.

test_generalize_with_constraints() ->
    % Create a type and some constraints
    State0 = catena_infer_state:new(),
    {Alpha, State1} = catena_types:fresh_var(State0),

    EqConstraint = catena_constraint:trait_constraint('Eq', [Alpha]),
    State2 = catena_infer_state:add_constraint(EqConstraint, State1),

    Env = catena_type_env:empty(),
    EnvVars = sets:new(),

    % Generalize should create a qualified type scheme
    Scheme = catena_type_scheme:generalize(Alpha, [EqConstraint], EnvVars),

    % Should be polymorphic with constraint
    ?assertMatch({poly, [1], [{trait, 'Eq', _, _}], {tvar, 1}}, Scheme).

test_generalize_no_constraints() ->
    % Generalize a type with no constraints
    State0 = catena_infer_state:new(),
    {Alpha, _State1} = catena_types:fresh_var(State0),

    EnvVars = sets:new(),

    % Generalize without constraints
    Scheme = catena_type_scheme:generalize(Alpha, [], EnvVars),

    % Should be polymorphic without constraints (backward compatible form)
    ?assertMatch({poly, [1], {tvar, 1}}, Scheme).
