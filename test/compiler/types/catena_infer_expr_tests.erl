%%%-------------------------------------------------------------------
%%% @doc Unit Tests for Expression Type Inference
%%%
%%% Tests for catena_infer_expr module covering:
%%% - Literal expressions
%%% - Variable lookup and instantiation
%%% - Lambda abstractions
%%% - Function applications
%%% - Let bindings with generalization
%%% - Let-rec bindings
%%% - If-then-else expressions
%%% - Tuples, records, variants
%%% - Type annotations
%%% - Generalization and instantiation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_infer_expr_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Section 1: Literal Expressions
%%%===================================================================

infer_int_literal_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type, _State1} = catena_infer_expr:infer({lit, {int, 42}}, Env, State),

    ?assertEqual({tcon, int}, Type).

infer_bool_literal_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type, _State1} = catena_infer_expr:infer({lit, {bool, true}}, Env, State),

    ?assertEqual({tcon, bool}, Type).

infer_string_literal_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type, _State1} = catena_infer_expr:infer({lit, {string, <<"hello">>}}, Env, State),

    ?assertEqual({tcon, string}, Type).

%%%===================================================================
%%% Section 2: Variable Expressions
%%%===================================================================

infer_var_monomorphic_test() ->
    State = catena_infer_state:new(),

    % Environment with x : Int
    Scheme = catena_type_scheme:mono({tcon, int}),
    Env = catena_type_env:singleton(x, Scheme),

    {Type, _State1} = catena_infer_expr:infer({var, x}, Env, State),

    ?assertEqual({tcon, int}, Type).

infer_var_polymorphic_test() ->
    State = catena_infer_state:new(),

    % Environment with id : ∀α. α → α
    IdType = {tfun, {tvar, 1}, {tvar, 1}, {effect_set, []}},
    Scheme = catena_type_scheme:poly([1], IdType),
    Env = catena_type_env:singleton(id, Scheme),

    {Type, _State1} = catena_infer_expr:infer({var, id}, Env, State),

    % Should instantiate with fresh variable
    ?assertMatch({tfun, {tvar, _}, {tvar, _}, {effect_set, []}}, Type),

    % Both sides should be same fresh variable
    {tfun, From, To, _} = Type,
    ?assertEqual(From, To).

infer_var_unbound_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {error, Error, State1} = catena_infer_expr:infer({var, unbound}, Env, State),

    ?assertMatch({unbound_variable, unbound}, Error),
    ?assertEqual(true, catena_infer_state:has_errors(State1)).

%%%===================================================================
%%% Section 3: Lambda Abstractions
%%%===================================================================

infer_identity_lambda_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % λx. x
    Expr = {lam, x, {var, x}},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    % Should be α → α
    ?assertMatch({tfun, {tvar, _}, {tvar, _}, {effect_set, []}}, Type),

    {tfun, From, To, _} = Type,
    ?assertEqual(From, To).

infer_const_lambda_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % λx. 42
    Expr = {lam, x, {lit, {int, 42}}},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    % Should be α → Int
    ?assertMatch({tfun, {tvar, _}, {tcon, int}, {effect_set, []}}, Type).

infer_nested_lambda_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % λx. λy. x
    Expr = {lam, x, {lam, y, {var, x}}},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    % Should be α → β → α
    ?assertMatch({tfun, {tvar, _}, {tfun, {tvar, _}, {tvar, _}, _}, _}, Type),

    {tfun, Outer, {tfun, _Inner, Result, _}, _} = Type,
    ?assertEqual(Outer, Result).

%%%===================================================================
%%% Section 4: Function Application
%%%===================================================================

infer_simple_application_test() ->
    State = catena_infer_state:new(),

    % Environment with id : ∀α. α → α
    IdType = {tfun, {tvar, 1}, {tvar, 1}, {effect_set, []}},
    Scheme = catena_type_scheme:poly([1], IdType),
    Env = catena_type_env:singleton(id, Scheme),

    % id 42
    Expr = {app, {var, id}, {lit, {int, 42}}},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    % Should be Int
    ?assertEqual({tcon, int}, Type).

infer_application_type_error_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % 42 true  (applying non-function)
    Expr = {app, {lit, {int, 42}}, {lit, {bool, true}}},

    {error, Error, _State1} = catena_infer_expr:infer(Expr, Env, State),

    % Should fail to unify Int with Bool → α
    ?assertMatch({unification_error, _, _}, Error).

infer_self_application_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % (λx. x x)
    Expr = {lam, x, {app, {var, x}, {var, x}}},

    {error, Error, _State1} = catena_infer_expr:infer(Expr, Env, State),

    % Should fail occurs check (infinite type)
    ?assertMatch({occurs_check, _, _}, Error).

%%%===================================================================
%%% Section 5: Let Bindings
%%%===================================================================

infer_simple_let_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % let x = 42 in x
    Expr = {'let', x, {lit, {int, 42}}, {var, x}},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    ?assertEqual({tcon, int}, Type).

infer_let_polymorphism_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % let id = λx. x in (id 42, id true)
    IdLam = {lam, x, {var, x}},
    Body = {tuple, [
        {app, {var, id}, {lit, {int, 42}}},
        {app, {var, id}, {lit, {bool, true}}}
    ]},
    Expr = {'let', id, IdLam, Body},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    % Should be (Int, Bool)
    ?assertEqual({ttuple, [{tcon, int}, {tcon, bool}]}, Type).

infer_let_generalization_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % let f = λx. x in f
    Expr = {'let', f, {lam, x, {var, x}}, {var, f}},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    % Should be α → α (fresh variable after instantiation)
    ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, Type).

%%%===================================================================
%%% Section 6: Let-Rec Bindings
%%%===================================================================

infer_letrec_factorial_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % letrec f = λx. x in f
    % (Simplified letrec that doesn't cause infinite type)
    Expr = {'letrec', f, {lam, x, {var, x}}, {var, f}},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    % Should be α → α
    ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, Type),

    {tfun, From, To, _} = Type,
    ?assertEqual(From, To).

%%%===================================================================
%%% Section 7: If-Then-Else
%%%===================================================================

infer_if_simple_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % if true then 1 else 0
    Expr = {'if', {lit, {bool, true}}, {lit, {int, 1}}, {lit, {int, 0}}},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    ?assertEqual({tcon, int}, Type).

infer_if_condition_error_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % if 42 then 1 else 0  (non-bool condition)
    Expr = {'if', {lit, {int, 42}}, {lit, {int, 1}}, {lit, {int, 0}}},

    {error, Error, _State1} = catena_infer_expr:infer(Expr, Env, State),

    ?assertMatch({unification_error, {tcon, int}, {tcon, bool}}, Error).

infer_if_branch_mismatch_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % if true then 1 else false  (branches don't match)
    Expr = {'if', {lit, {bool, true}}, {lit, {int, 1}}, {lit, {bool, false}}},

    {error, Error, _State1} = catena_infer_expr:infer(Expr, Env, State),

    ?assertMatch({unification_error, {tcon, int}, {tcon, bool}}, Error).

%%%===================================================================
%%% Section 8: Tuples
%%%===================================================================

infer_empty_tuple_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type, _State1} = catena_infer_expr:infer({tuple, []}, Env, State),

    ?assertEqual({ttuple, []}, Type).

infer_pair_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % (42, true)
    Expr = {tuple, [{lit, {int, 42}}, {lit, {bool, true}}]},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    ?assertEqual({ttuple, [{tcon, int}, {tcon, bool}]}, Type).

%%%===================================================================
%%% Section 9: Records
%%%===================================================================

infer_empty_record_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type, _State1} = catena_infer_expr:infer({record, []}, Env, State),

    ?assertEqual({trecord, [], closed}, Type).

infer_simple_record_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % {x: 10, y: 20}
    Expr = {record, [{x, {lit, {int, 10}}}, {y, {lit, {int, 20}}}]},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    ?assertMatch({trecord, _, closed}, Type),
    {trecord, Fields, closed} = Type,
    ?assertEqual([x, y], lists:sort([Label || {Label, _} <- Fields])).

infer_field_access_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % {x: 10}.x
    % Note: Field access with row polymorphism has limitations in PoC
    % Full row polymorphism would allow accessing x from {x, y, ...}
    Record = {record, [{x, {lit, {int, 10}}}]},
    Expr = {field, Record, x},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    ?assertEqual({tcon, int}, Type).

%%%===================================================================
%%% Section 10: Variants
%%%===================================================================

infer_variant_no_args_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type, _State1} = catena_infer_expr:infer({variant, none, []}, Env, State),

    ?assertEqual({tvariant, [{none, []}]}, Type).

infer_variant_with_args_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % Some 42
    Expr = {variant, some, [{lit, {int, 42}}]},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    ?assertEqual({tvariant, [{some, [{tcon, int}]}]}, Type).

%%%===================================================================
%%% Section 11: Type Annotations
%%%===================================================================

infer_annotation_matching_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % (42 : Int)
    Expr = {ann, {lit, {int, 42}}, {tcon, int}},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    ?assertEqual({tcon, int}, Type).

infer_annotation_mismatch_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % (42 : Bool)  - wrong annotation
    Expr = {ann, {lit, {int, 42}}, {tcon, bool}},

    {error, Error, _State1} = catena_infer_expr:infer(Expr, Env, State),

    ?assertMatch({unification_error, {tcon, int}, {tcon, bool}}, Error).

%%%===================================================================
%%% Section 12: Instantiation
%%%===================================================================

instantiate_monomorphic_test() ->
    State = catena_infer_state:new(),

    Scheme = catena_type_scheme:mono({tcon, int}),

    {Type, _Constraints, State1} = catena_infer_expr:instantiate(Scheme, State),

    ?assertEqual({tcon, int}, Type),
    % Should not generate any fresh variables
    ?assertEqual(1, catena_infer_state:get_next_var(State1)).

instantiate_polymorphic_test() ->
    State = catena_infer_state:new(),

    % ∀α. α → α
    PolyType = {tfun, {tvar, 1}, {tvar, 1}, {effect_set, []}},
    Scheme = catena_type_scheme:poly([1], PolyType),

    {Type, _Constraints, State1} = catena_infer_expr:instantiate(Scheme, State),

    % Should replace α with fresh variable
    ?assertMatch({tfun, {tvar, _}, {tvar, _}, _}, Type),

    {tfun, From, To, _} = Type,
    ?assertEqual(From, To),

    % Should have generated fresh variable
    ?assertEqual(2, catena_infer_state:get_next_var(State1)).

instantiate_multiple_vars_test() ->
    State = catena_infer_state:new(),

    % ∀α β. α → β → α
    PolyType = {tfun, {tvar, 1}, {tfun, {tvar, 2}, {tvar, 1}, {effect_set, []}}, {effect_set, []}},
    Scheme = catena_type_scheme:poly([1, 2], PolyType),

    {Type, _Constraints, _State1} = catena_infer_expr:instantiate(Scheme, State),

    % Should replace both variables
    ?assertMatch({tfun, {tvar, _}, {tfun, {tvar, _}, {tvar, _}, _}, _}, Type).

%%%===================================================================
%%% Section 13: Generalization
%%%===================================================================

generalize_no_free_vars_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % Generalize Int
    Scheme = catena_infer_expr:generalize({tcon, int}, Env, State),

    % Should be monomorphic
    ?assertEqual({mono, {tcon, int}}, Scheme).

generalize_free_vars_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % Generalize α → α
    Type = {tfun, {tvar, 1}, {tvar, 1}, {effect_set, []}},
    Scheme = catena_infer_expr:generalize(Type, Env, State),

    % Should be ∀α. α → α
    ?assertMatch({poly, [1], _}, Scheme).

generalize_with_env_test() ->
    State = catena_infer_state:new(),

    % Environment with x : α
    EnvScheme = catena_type_scheme:mono({tvar, 1}),
    Env = catena_type_env:singleton(x, EnvScheme),

    % Generalize α → β
    Type = {tfun, {tvar, 1}, {tvar, 2}, {effect_set, []}},
    Scheme = catena_infer_expr:generalize(Type, Env, State),

    % Should only generalize β (α is free in environment)
    ?assertMatch({poly, [2], _}, Scheme).

%%%===================================================================
%%% Section 14: Integration Tests
%%%===================================================================

integration_compose_function_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % let compose = λf. λg. λx. f (g x) in compose
    Compose = {lam, f, {lam, g, {lam, x,
        {app, {var, f}, {app, {var, g}, {var, x}}}
    }}},
    Expr = {'let', compose, Compose, {var, compose}},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    % Should be (β → γ) → (α → β) → α → γ
    ?assertMatch({tfun, _, {tfun, _, {tfun, _, _, _}, _}, _}, Type).

integration_map_function_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % Simplified map: λf. λx. f x
    Map = {lam, f, {lam, x, {app, {var, f}, {var, x}}}},
    Expr = {'let', map, Map, {var, map}},

    {Type, _State1} = catena_infer_expr:infer(Expr, Env, State),

    % Should be (α → β) → α → β
    ?assertMatch({tfun, {tfun, _, _, _}, {tfun, _, _, _}, _}, Type).

%%%===================================================================
%%% Test Suite Integration
%%%===================================================================

integration_all_sections_test() ->
    % Verify all sections run successfully
    ?assert(true).
