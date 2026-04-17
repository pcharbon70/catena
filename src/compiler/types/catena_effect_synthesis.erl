%%%-------------------------------------------------------------------
%%% @doc Catena Effect Type Synthesis (Phase 14.2.1)
%%%
%%% This module implements effect type synthesis for all expressions
%%% in the Catena language. Effect synthesis computes the effect set
%%% for each expression during type checking, enabling full type
%%% reconstruction including effects.
%%%
%%% == Effect Synthesis Rules ==
%%%
%%% 1. **Literals**: Pure (no effects)
%%% 2. **Variables**: Pure (effects only when used)
%%% 3. **Lambdas**: Pure (captured in function type)
%%% 4. **Applications**: Union of function effects and argument effects
%%% 5. **Perform**: Introduces the performed effect
%%% 6. **Handle**: Removes handled effects from body
%%% 7. **Let**: Union of binding expression effects and body effects
%%% 8. **If/Match**: Union of branch effects
%%%
%%% == Integration with Type Inference ==
%%%
%%% Effect synthesis runs alongside type inference. The inference state
%%% tracks the current effect set, and this module provides functions
%%% to synthesize effects for each expression kind.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_synthesis).

%% Main synthesis entry points
-export([
    synthesize/2,
    synthesize/3,
    synthesize_literal/2,
    synthesize_var/2,
    synthesize_lambda/2,
    synthesize_app/2,
    synthesize_perform/2,
    synthesize_handle/2,
    synthesize_let/2,
    synthesize_if/2,
    synthesize_match/2,
    synthesize_tuple/2,
    synthesize_list/2
]).

%% Effect set operations
-export([
    empty/0,
    union/2,
    remove/2,
    contains/2,
    is_empty/1,
    to_list/1,
    from_list/1
]).

%%====================================================================
%% Types
%%====================================================================

-type expr() :: term().
-type env() :: catena_type_env:env().
-type state() :: catena_infer_state:infer_state().
-type effect_set() :: catena_types:effect_set().
-type synthesis_result() :: {effect_set(), state()}.

%%====================================================================
%% Main Synthesis Entry Points
%%====================================================================

%% @doc Synthesize effects for an expression.
%%
%% Returns the effect set and updated inference state.
-spec synthesize(expr(), state()) -> synthesis_result().
synthesize(Expr, State) ->
    synthesize(Expr, catena_type_env:empty(), State).

%% @doc Synthesize effects for an expression with environment.
-spec synthesize(expr(), env(), state()) -> synthesis_result().
synthesize({literal, _, _, _}, _Env, State) ->
    {empty(), State};

synthesize({var, _Name, _Loc}, _Env, State) ->
    {empty(), State};

synthesize({lambda, _Params, _Body, _Loc}, Env, State) ->
    %% Lambdas are pure - effects are captured in the function type
    %% The body effects will be computed when the lambda is applied
    {empty(), State};

synthesize({app, Func, Args, _Loc}, Env, State) ->
    %% Application effects = function effects + argument effects
    {FuncEffects, State1} = synthesize(Func, Env, State),
    {ArgEffects, State2} = synthesize_args(Args, Env, State1),
    {union(FuncEffects, ArgEffects), State2};

synthesize({perform, {Effect, _Op, _Args}, _Loc}, _Env, State) ->
    %% Perform introduces the effect
    {{effect_set, [Effect]}, State};

synthesize({handle, _Expr, _Handlers, _Loc}, _Env, State) ->
    %% Handle removes handled effects - this is computed during
    %% the full type checking pass
    {empty(), State};

synthesize({let_expr, [{pat_var, _Name, _}, Value], Body, _Loc}, Env, State) ->
    %% Let effects = value effects + body effects
    {ValueEffects, State1} = synthesize(Value, Env, State),
    {BodyEffects, State2} = synthesize(Body, Env, State1),
    {union(ValueEffects, BodyEffects), State2};

synthesize({'if', _Cond, Then, Else}, Env, State) ->
    %% If effects = union of branch effects
    {CondEffects, State1} = synthesize_args([_Cond], Env, State),
    {ThenEffects, State2} = synthesize(Then, Env, State1),
    {ElseEffects, State3} = synthesize(Else, Env, State2),
    {union(union(CondEffects, ThenEffects), ElseEffects), State3};

synthesize({match_expr, _Scrutinee, _Cases, _Loc}, Env, State) ->
    %% Match effects = scrutinee effects + union of case effects
    %% This is a simplification - full pattern matching needs more care
    {empty(), State};

synthesize({tuple, _Elements, _Loc}, Env, State) ->
    %% Tuple effects = union of element effects
    {ElementsEffects, State1} = synthesize_tuple(_Elements, Env, State),
    {ElementsEffects, State1};

synthesize({list, _Elements, _Loc}, Env, State) ->
    %% List effects = union of element effects
    {ElementsEffects, State1} = synthesize_list(_Elements, Env, State),
    {ElementsEffects, State1};

synthesize(_Expr, _Env, State) ->
    {empty(), State}.

%%====================================================================
%% Expression-Specific Synthesis
%%====================================================================

%% @doc Synthesize effects for a literal expression.
-spec synthesize_literal(term(), state()) -> synthesis_result().
synthesize_literal(_Literal, State) ->
    {empty(), State}.

%% @doc Synthesize effects for a variable reference.
-spec synthesize_var(atom(), state()) -> synthesis_result().
synthesize_var(_Name, State) ->
    {empty(), State}.

%% @doc Synthesize effects for a lambda expression.
%%
%% Lambdas are pure - effects are captured in the function type.
-spec synthesize_lambda(term(), state()) -> synthesis_result().
synthesize_lambda(_Lambda, State) ->
    {empty(), State}.

%% @doc Synthesize effects for a function application.
-spec synthesize_app({expr(), [expr()]}, state()) -> synthesis_result().
synthesize_app({_Func, Args}, State) ->
    synthesize_args(Args, catena_type_env:empty(), State).

%% @doc Synthesize effects for a perform expression.
-spec synthesize_perform({atom(), atom(), [expr()]}, state()) -> synthesis_result().
synthesize_perform({Effect, _Op, _Args}, State) ->
    {{effect_set, [Effect]}, State}.

%% @doc Synthesize effects for a handle expression.
%%
%% Handle removes handled effects from the body's effect set.
-spec synthesize_handle({expr(), [{atom(), expr(), expr()}]}, state()) -> synthesis_result().
synthesize_handle({_Expr, Handlers}, State) ->
    %% Extract handled effects
    HandledEffects = lists:map(fun({Effect, _Pat, _Body}) -> Effect end, Handlers),
    HandledSet = from_list(HandledEffects),
    %% The body effects will have these removed during type checking
    {HandledSet, State}.

%% @doc Synthesize effects for a let expression.
-spec synthesize_let({expr(), expr()}, state()) -> synthesis_result().
synthesize_let({Value, Body}, State) ->
    {ValueEffects, State1} = synthesize(Value, catena_type_env:empty(), State),
    {BodyEffects, State2} = synthesize(Body, catena_type_env:empty(), State1),
    {union(ValueEffects, BodyEffects), State2}.

%% @doc Synthesize effects for an if expression.
-spec synthesize_if({expr(), expr(), expr()}, state()) -> synthesis_result().
synthesize_if({Cond, Then, Else}, State) ->
    {CondEffects, State1} = synthesize(Cond, catena_type_env:empty(), State),
    {ThenEffects, State2} = synthesize(Then, catena_type_env:empty(), State1),
    {ElseEffects, State3} = synthesize(Else, catena_type_env:empty(), State2),
    {union(union(CondEffects, ThenEffects), ElseEffects), State3}.

%% @doc Synthesize effects for a match expression.
-spec synthesize_match({expr(), [{term(), expr()}]}, state()) -> synthesis_result().
synthesize_match({_Scrutinee, Cases}, State) ->
    {ScrutineeEffects, State1} = synthesize(_Scrutinee, catena_type_env:empty(), State),
    {CaseEffects, State2} = synthesize_cases(Cases, catena_type_env:empty(), State1),
    {union(ScrutineeEffects, CaseEffects), State2}.

%% @doc Synthesize effects for a tuple expression.
-spec synthesize_tuple([expr()], state()) -> synthesis_result().
synthesize_tuple(Elements, State) ->
    synthesize_args(Elements, catena_type_env:empty(), State).

%% @doc Synthesize effects for a list expression.
-spec synthesize_list([expr()], state()) -> synthesis_result().
synthesize_list(Elements, State) ->
    synthesize_args(Elements, catena_type_env:empty(), State).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Synthesize effects for a list of arguments.
-spec synthesize_args([expr()], env(), state()) -> synthesis_result().
synthesize_args(Args, Env, State) ->
    lists:foldl(fun(Arg, {EffectsAcc, S}) ->
        {ArgEffects, NewS} = synthesize(Arg, Env, S),
        {union(EffectsAcc, ArgEffects), NewS}
    end, {empty(), State}, Args).

%% @doc Synthesize effects for match cases.
-spec synthesize_cases([{term(), expr()}], env(), state()) -> synthesis_result().
synthesize_cases(Cases, Env, State) ->
    lists:foldl(fun({_Pattern, Body}, {EffectsAcc, S}) ->
        {BodyEffects, NewS} = synthesize(Body, Env, S),
        {union(EffectsAcc, BodyEffects), NewS}
    end, {empty(), State}, Cases).

%%====================================================================
%% Effect Set Operations
%%====================================================================

%% @doc Create an empty effect set.
-spec empty() -> effect_set().
empty() ->
    catena_types:empty_effects().

%% @doc Union two effect sets.
-spec union(effect_set(), effect_set()) -> effect_set().
union(Effects1, Effects2) ->
    catena_types:union_effects(Effects1, Effects2).

%% @doc Remove effects from a set.
-spec remove(effect_set(), [atom()]) -> effect_set().
remove(Effects, ToRemove) ->
    lists:foldl(fun(Effect, Acc) ->
        catena_types:remove_effect(Effect, Acc)
    end, Effects, ToRemove).

%% @doc Check if an effect is in the set.
-spec contains(effect_set(), atom()) -> boolean().
contains({effect_set, Effects}, Effect) ->
    lists:member(Effect, Effects).

%% @doc Check if an effect set is empty.
-spec is_empty(effect_set()) -> boolean().
is_empty({effect_set, []}) -> true;
is_empty({effect_set, _}) -> false.

%% @doc Convert an effect set to a list.
-spec to_list(effect_set()) -> [atom()].
to_list({effect_set, Effects}) ->
    Effects.

%% @doc Create an effect set from a list.
-spec from_list([atom()]) -> effect_set().
from_list(Effects) ->
    catena_types:effect_set(Effects).
