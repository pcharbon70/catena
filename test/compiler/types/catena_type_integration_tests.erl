%%%-------------------------------------------------------------------
%%% @doc Integration Tests for Type System
%%%
%%% Tests the interaction between all type system modules:
%%% - catena_types (type construction)
%%% - catena_type_subst (substitution)
%%% - catena_type_scheme (generalization/instantiation)
%%% - catena_type_env (environments)
%%% - catena_type_pp (pretty-printing)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_type_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    % No global state to initialize with explicit state threading
    ok.

teardown(_) ->
    ok.

%%====================================================================
%% Complete Type Inference Workflow Tests
%%====================================================================

workflow_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_identity_function()),
      ?_test(test_const_function()),
      ?_test(test_compose_function()),
      ?_test(test_map_function())
     ]}.

test_identity_function() ->
    % Simulate type checking: let id = λx. x
    State0 = catena_infer_state:new(),

    % Create type for λx. x: α₁ -> α₁
    {{tvar, Alpha}, State1} = catena_types:fresh_var(State0),
    IdType = catena_types:tfun(
        catena_types:tvar(Alpha),
        catena_types:tvar(Alpha),
        catena_types:empty_effects()
    ),

    % Generalize in empty environment
    EmptyEnv = catena_type_env:empty(),
    EnvFreeVars = catena_type_env:ftv_env(EmptyEnv),
    IdScheme = catena_type_scheme:generalize(IdType, EnvFreeVars),

    % Should be polymorphic: ∀α₁. α₁ -> α₁
    ?assertMatch({poly, [_], _}, IdScheme),

    % Add to environment
    Env = catena_type_env:extend(EmptyEnv, id, IdScheme),

    % Look up and instantiate
    {ok, SchemeFound} = catena_type_env:lookup(Env, id),
    {Inst1, _Constraints1, State2} = catena_type_scheme:instantiate(SchemeFound, State1),
    {Inst2, _Constraints2, _State3} = catena_type_scheme:instantiate(SchemeFound, State2),

    % Both instantiations should be function types
    ?assertMatch({tfun, _, _, _}, Inst1),
    ?assertMatch({tfun, _, _, _}, Inst2),

    % But with different fresh variables
    {tfun, {tvar, From1}, {tvar, To1}, _} = Inst1,
    {tfun, {tvar, From2}, {tvar, To2}, _} = Inst2,

    ?assertNotEqual(From1, From2),
    ?assertEqual(From1, To1),  % Same var in each instance
    ?assertEqual(From2, To2),

    % Pretty-print the scheme
    PP = catena_type_pp:pp_scheme(IdScheme),
    ?assertEqual("∀α1. α1 -> α1", PP).

test_const_function() ->
    % Simulate: let const = λx. λy. x
    % Type: ∀α β. α -> β -> α
    State0 = catena_infer_state:new(),

    {{tvar, Alpha}, State1} = catena_types:fresh_var(State0),
    {{tvar, Beta}, _State2} = catena_types:fresh_var(State1),

    ConstType = catena_types:tfun(
        catena_types:tvar(Alpha),
        catena_types:tfun(
            catena_types:tvar(Beta),
            catena_types:tvar(Alpha),
            catena_types:empty_effects()
        ),
        catena_types:empty_effects()
    ),

    % Generalize
    EmptyEnv = catena_type_env:empty(),
    EnvFreeVars = catena_type_env:ftv_env(EmptyEnv),
    ConstScheme = catena_type_scheme:generalize(ConstType, EnvFreeVars),

    % Should be: ∀α β. α -> β -> α
    ?assertMatch({poly, [_, _], _}, ConstScheme),

    % Pretty-print
    PP = catena_type_pp:pp_scheme(ConstScheme),
    ?assertEqual("∀α1 α2. α1 -> α2 -> α1", PP),

    % Instantiate and verify fresh variables
    StateInst = catena_infer_state:new(),
    {Inst, _Constraints, _StateInst1} = catena_type_scheme:instantiate(ConstScheme, StateInst),
    ?assertMatch({tfun, {tvar, _}, {tfun, {tvar, _}, {tvar, _}, _}, _}, Inst).

test_compose_function() ->
    % Simulate: let compose = λf. λg. λx. f (g x)
    % Type: ∀α β γ. (β -> γ) -> (α -> β) -> α -> γ
    State0 = catena_infer_state:new(),

    {{tvar, Alpha}, State1} = catena_types:fresh_var(State0),
    {{tvar, Beta}, State2} = catena_types:fresh_var(State1),
    {{tvar, Gamma}, _State3} = catena_types:fresh_var(State2),

    % β -> γ
    FType = catena_types:tfun(
        catena_types:tvar(Beta),
        catena_types:tvar(Gamma),
        catena_types:empty_effects()
    ),

    % α -> β
    GType = catena_types:tfun(
        catena_types:tvar(Alpha),
        catena_types:tvar(Beta),
        catena_types:empty_effects()
    ),

    % α -> γ
    ResultType = catena_types:tfun(
        catena_types:tvar(Alpha),
        catena_types:tvar(Gamma),
        catena_types:empty_effects()
    ),

    % (β -> γ) -> (α -> β) -> α -> γ
    ComposeType = catena_types:tfun(
        FType,
        catena_types:tfun(
            GType,
            ResultType,
            catena_types:empty_effects()
        ),
        catena_types:empty_effects()
    ),

    % Generalize
    EmptyEnv = catena_type_env:empty(),
    EnvFreeVars = catena_type_env:ftv_env(EmptyEnv),
    ComposeScheme = catena_type_scheme:generalize(ComposeType, EnvFreeVars),

    % Should be polymorphic with 3 type variables
    ?assertMatch({poly, [_, _, _], _}, ComposeScheme),

    % Pretty-print
    PP = catena_type_pp:pp_scheme(ComposeScheme),
    Expected = "∀α1 α2 α3. (a2 -> α3) -> (a1 -> α2) -> α1 -> α3",
    ?assertEqual(Expected, PP).

test_map_function() ->
    % Simulate: let map = λf. λlist. ...
    % Type: ∀α β. (α -> β) -> List<α> -> List<β>
    State0 = catena_infer_state:new(),

    {{tvar, Alpha}, State1} = catena_types:fresh_var(State0),
    {{tvar, Beta}, _State2} = catena_types:fresh_var(State1),

    % α -> β
    FType = catena_types:tfun(
        catena_types:tvar(Alpha),
        catena_types:tvar(Beta),
        catena_types:empty_effects()
    ),

    % List<α>
    ListAlpha = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(Alpha)]
    ),

    % List<β>
    ListBeta = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(Beta)]
    ),

    % (α -> β) -> List<α> -> List<β>
    MapType = catena_types:tfun(
        FType,
        catena_types:tfun(
            ListAlpha,
            ListBeta,
            catena_types:empty_effects()
        ),
        catena_types:empty_effects()
    ),

    % Generalize
    EmptyEnv = catena_type_env:empty(),
    EnvFreeVars = catena_type_env:ftv_env(EmptyEnv),
    MapScheme = catena_type_scheme:generalize(MapType, EnvFreeVars),

    % Pretty-print
    PP = catena_type_pp:pp_scheme(MapScheme),
    Expected = "∀α1 α2. (a1 -> α2) -> List<a1> -> List<a2>",
    ?assertEqual(Expected, PP).

%%====================================================================
%% Substitution and Generalization Integration Tests
%%====================================================================

subst_integration_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_substitution_before_generalization()),
      ?_test(test_environment_with_substitution())
     ]}.

test_substitution_before_generalization() ->
    % Simulate unification: α₁ = Int, then generalize α₁ -> α₂
    State0 = catena_infer_state:new(),

    {{tvar, Alpha}, State1} = catena_types:fresh_var(State0),
    {{tvar, Beta}, _State2} = catena_types:fresh_var(State1),

    % Create type: α₁ -> α₂
    Type = catena_types:tfun(
        catena_types:tvar(Alpha),
        catena_types:tvar(Beta),
        catena_types:empty_effects()
    ),

    % Apply substitution {α₁ ↦ Int}
    Subst = catena_type_subst:singleton(Alpha, catena_types:tcon(integer)),
    SubstitutedType = catena_type_subst:apply(Subst, Type),

    % Should be: Int -> α₂
    ?assertMatch({tfun, {tcon, integer}, {tvar, _}, _}, SubstitutedType),

    % Generalize (only α₂ should be quantified)
    EmptyEnv = catena_type_env:empty(),
    EnvFreeVars = catena_type_env:ftv_env(EmptyEnv),
    Scheme = catena_type_scheme:generalize(SubstitutedType, EnvFreeVars),

    % Should be: ∀α₂. Int -> α₂
    ?assertMatch({poly, [_], _}, Scheme),

    PP = catena_type_pp:pp_scheme(Scheme),
    ?assertEqual("∀a2. integer -> α2", PP).

test_environment_with_substitution() ->
    % Build environment with multiple bindings, apply substitution
    State0 = catena_infer_state:new(),

    {{tvar, Alpha}, State1} = catena_types:fresh_var(State0),
    {{tvar, Beta}, _State2} = catena_types:fresh_var(State1),

    % x: α₁
    XScheme = catena_type_scheme:mono(catena_types:tvar(Alpha)),

    % y: α₂
    YScheme = catena_type_scheme:mono(catena_types:tvar(Beta)),

    % Build environment
    Env = catena_type_env:from_list([
        {x, XScheme},
        {y, YScheme}
    ]),

    % FTV should be {α₁, α₂}
    Ftv = catena_type_env:ftv_env(Env),
    FtvList = lists:sort(sets:to_list(Ftv)),
    ?assertEqual([Alpha, Beta], FtvList).

%%====================================================================
%% Complex Type Scenarios
%%====================================================================

complex_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_record_with_polymorphism()),
      ?_test(test_effectful_map()),
      ?_test(test_nested_type_application())
     ]}.

test_record_with_polymorphism() ->
    % ∀α. {x: α, y: α}
    State0 = catena_infer_state:new(),

    {{tvar, Alpha}, _State1} = catena_types:fresh_var(State0),

    RecordType = catena_types:trecord([
        {x, catena_types:tvar(Alpha)},
        {y, catena_types:tvar(Alpha)}
    ], closed),

    EmptyEnv = catena_type_env:empty(),
    EnvFreeVars = catena_type_env:ftv_env(EmptyEnv),
    Scheme = catena_type_scheme:generalize(RecordType, EnvFreeVars),

    PP = catena_type_pp:pp_scheme(Scheme),
    ?assertEqual("∀α1. {x: α1, y: α1}", PP).

test_effectful_map() ->
    % ∀α β. (α -> β / {io}) -> List<α> -> List<β> / {io}
    State0 = catena_infer_state:new(),

    {{tvar, Alpha}, State1} = catena_types:fresh_var(State0),
    {{tvar, Beta}, _State2} = catena_types:fresh_var(State1),

    % α -> β / {io}
    FType = catena_types:tfun(
        catena_types:tvar(Alpha),
        catena_types:tvar(Beta),
        catena_types:singleton_effect(io)
    ),

    ListAlpha = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(Alpha)]
    ),

    ListBeta = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(Beta)]
    ),

    % (α -> β / {io}) -> List<α> -> List<β> / {io}
    MapType = catena_types:tfun(
        FType,
        catena_types:tfun(
            ListAlpha,
            ListBeta,
            catena_types:singleton_effect(io)
        ),
        catena_types:empty_effects()
    ),

    EmptyEnv = catena_type_env:empty(),
    EnvFreeVars = catena_type_env:ftv_env(EmptyEnv),
    Scheme = catena_type_scheme:generalize(MapType, EnvFreeVars),

    PP = catena_type_pp:pp_scheme(Scheme),
    Expected = "∀α1 α2. (a1 -> α2 / {io}) -> List<a1> -> List<a2> / {io}",
    ?assertEqual(Expected, PP).

test_nested_type_application() ->
    % List<List<Int>>
    ListListInt = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tapp(
            catena_types:tcon('List'),
            [catena_types:tcon(integer)]
        )]
    ),

    PP = catena_type_pp:pp_type(ListListInt),
    ?assertEqual("List<List<integer>>", PP).

%%====================================================================
%% End-to-End Scenario: Multiple Let Bindings
%%====================================================================

end_to_end_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_multiple_let_bindings())
     ]}.

test_multiple_let_bindings() ->
    % Simulate:
    % let id = λx. x in
    % let const = λx. λy. x in
    % let app = λf. λx. f x in
    % ...
    State0 = catena_infer_state:new(),

    % id: ∀α. α -> α
    {{tvar, Id_Alpha}, State1} = catena_types:fresh_var(State0),
    IdType = catena_types:tfun(
        catena_types:tvar(Id_Alpha),
        catena_types:tvar(Id_Alpha),
        catena_types:empty_effects()
    ),
    EmptyEnv = catena_type_env:empty(),
    IdScheme = catena_type_scheme:generalize(IdType, catena_type_env:ftv_env(EmptyEnv)),

    % const: ∀α β. α -> β -> α
    {{tvar, Const_Alpha}, State2} = catena_types:fresh_var(State1),
    {{tvar, Const_Beta}, State3} = catena_types:fresh_var(State2),
    ConstType = catena_types:tfun(
        catena_types:tvar(Const_Alpha),
        catena_types:tfun(
            catena_types:tvar(Const_Beta),
            catena_types:tvar(Const_Alpha),
            catena_types:empty_effects()
        ),
        catena_types:empty_effects()
    ),
    ConstScheme = catena_type_scheme:generalize(ConstType, catena_type_env:ftv_env(EmptyEnv)),

    % app: ∀α β. (α -> β) -> α -> β
    {{tvar, App_Alpha}, State4} = catena_types:fresh_var(State3),
    {{tvar, App_Beta}, _State5} = catena_types:fresh_var(State4),
    AppType = catena_types:tfun(
        catena_types:tfun(
            catena_types:tvar(App_Alpha),
            catena_types:tvar(App_Beta),
            catena_types:empty_effects()
        ),
        catena_types:tfun(
            catena_types:tvar(App_Alpha),
            catena_types:tvar(App_Beta),
            catena_types:empty_effects()
        ),
        catena_types:empty_effects()
    ),
    AppScheme = catena_type_scheme:generalize(AppType, catena_type_env:ftv_env(EmptyEnv)),

    % Build environment
    Env = catena_type_env:from_list([
        {id, IdScheme},
        {const, ConstScheme},
        {app, AppScheme}
    ]),

    % Verify all can be looked up and instantiated
    {ok, IdFound} = catena_type_env:lookup(Env, id),
    {ok, ConstFound} = catena_type_env:lookup(Env, const),
    {ok, AppFound} = catena_type_env:lookup(Env, app),

    StateInst0 = catena_infer_state:new(),
    {IdInst, _Constraints1, StateInst1} = catena_type_scheme:instantiate(IdFound, StateInst0),
    {ConstInst, _Constraints2, StateInst2} = catena_type_scheme:instantiate(ConstFound, StateInst1),
    {AppInst, _Constraints3, _StateInst3} = catena_type_scheme:instantiate(AppFound, StateInst2),

    % All should be function types
    ?assertMatch({tfun, _, _, _}, IdInst),
    ?assertMatch({tfun, _, _, _}, ConstInst),
    ?assertMatch({tfun, _, _, _}, AppInst),

    % Environment should have no free type variables (all are polymorphic)
    EnvFtv = catena_type_env:ftv_env(Env),
    ?assertEqual([], sets:to_list(EnvFtv)),

    % Pretty-print all schemes
    IdPP = catena_type_pp:pp_scheme(IdScheme),
    ConstPP = catena_type_pp:pp_scheme(ConstScheme),
    AppPP = catena_type_pp:pp_scheme(AppScheme),

    ?assertEqual("∀α1. α1 -> α1", IdPP),
    ?assertEqual("∀a2 α3. α2 -> α3 -> α2", ConstPP),
    ?assertEqual("∀α4 α5. (α4 -> α5) -> α4 -> α5", AppPP).
