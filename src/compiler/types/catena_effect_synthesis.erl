%%%-------------------------------------------------------------------
%%% @doc Effect synthesis for parser and inference expressions.
%%%
%%% Phase 14.2 needs synthesis that matches the AST shapes used by the
%%% current parser (`perform_expr`, `handle_expr`, `tuple_expr`, etc.)
%%% while still handling the smaller internal IR used by `catena_compile`.
%%% This module only computes effect sets.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_synthesis).

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

-export([
    empty/0,
    union/2,
    remove/2,
    contains/2,
    is_empty/1,
    to_list/1,
    from_list/1
]).

-type expr() :: term().
-type env() :: catena_type_env:env().
-type state() :: catena_infer_state:infer_state().
-type effect_set() :: catena_types:effect_set().
-type synthesis_result() :: {effect_set(), state()}.

%%====================================================================
%% Main Synthesis Entry Points
%%====================================================================

-spec synthesize(expr(), state()) -> synthesis_result().
synthesize(Expr, State) ->
    synthesize(Expr, catena_type_env:empty(), State).

-spec synthesize(expr(), env(), state()) -> synthesis_result().
synthesize({lit, _Literal}, _Env, State) ->
    {empty(), State};
synthesize({literal, _Type, _Value, _Loc}, _Env, State) ->
    {empty(), State};
synthesize({var, _Name}, _Env, State) ->
    {empty(), State};
synthesize({var, _Name, _Loc}, _Env, State) ->
    {empty(), State};
synthesize({identifier, _Name, _Loc}, _Env, State) ->
    {empty(), State};
synthesize({lam, _Param, _Body}, _Env, State) ->
    {empty(), State};
synthesize({lambda, _Params, _Body, _Loc}, _Env, State) ->
    {empty(), State};
synthesize({app, Func, Arg}, Env, State) ->
    synthesize_app({Func, [Arg]}, Env, State);
synthesize({app, Func, Args, _Loc}, Env, State) when is_list(Args) ->
    synthesize_app({Func, Args}, Env, State);
synthesize({perform_expr, Effect, _Operation, Args, _Loc}, Env, State) ->
    synthesize_perform({Effect, undefined, Args}, Env, State);
synthesize({perform, {Effect, _Operation, Args}, _Loc}, Env, State) ->
    synthesize_perform({Effect, undefined, Args}, Env, State);
synthesize({handle_expr, Body, Handlers, _Loc}, Env, State) ->
    synthesize_handle({Body, Handlers}, Env, State);
synthesize({handle, Body, Handlers, _Loc}, Env, State) ->
    synthesize_handle({Body, Handlers}, Env, State);
synthesize({'let', _Name, Value, Body}, Env, State) ->
    synthesize_let({Value, Body}, Env, State);
synthesize({let_expr, [{pat_var, _Name, _}, Value], Body, _Loc}, Env, State) ->
    synthesize_let({Value, Body}, Env, State);
synthesize({if_expr, Cond, Then, Else, _Loc}, Env, State) ->
    synthesize_if({Cond, Then, Else}, Env, State);
synthesize({'if', Cond, Then, Else}, Env, State) ->
    synthesize_if({Cond, Then, Else}, Env, State);
synthesize({match_expr, Scrutinee, Cases, _Loc}, Env, State) ->
    synthesize_match({Scrutinee, Cases}, Env, State);
synthesize({'match', Scrutinee, Cases, _Loc}, Env, State) ->
    synthesize_match({Scrutinee, Cases}, Env, State);
synthesize({match, Scrutinee, Cases}, Env, State) ->
    synthesize_match({Scrutinee, Cases}, Env, State);
synthesize({tuple_expr, Elements, _Loc}, Env, State) ->
    synthesize_tuple(Elements, Env, State);
synthesize({tuple, Elements}, Env, State) ->
    synthesize_tuple(Elements, Env, State);
synthesize({tuple, Elements, _Loc}, Env, State) ->
    synthesize_tuple(Elements, Env, State);
synthesize({list_expr, Elements, _Loc}, Env, State) ->
    synthesize_list(Elements, Env, State);
synthesize({list, Elements, _Loc}, Env, State) ->
    synthesize_list(Elements, Env, State);
synthesize({record_expr, Fields, _Base, _Loc}, Env, State) ->
    synthesize_record_fields(Fields, Env, State);
synthesize({record, Fields}, Env, State) ->
    synthesize_record_fields(Fields, Env, State);
synthesize({field_access, Expr, _Field, _Loc}, Env, State) ->
    synthesize(Expr, Env, State);
synthesize({field, Expr, _Field}, Env, State) ->
    synthesize(Expr, Env, State);
synthesize({binary_op, _Op, Left, Right, _Loc}, Env, State) ->
    synthesize_args([Left, Right], Env, State);
synthesize({unary_op, _Op, Expr, _Loc}, Env, State) ->
    synthesize(Expr, Env, State);
synthesize({constructor, _Name, Args, _Loc}, Env, State) ->
    synthesize_args(Args, Env, State);
synthesize({variant, _Name, Args}, Env, State) ->
    synthesize_args(Args, Env, State);
synthesize({ann, Expr, _Type}, Env, State) ->
    synthesize(Expr, Env, State);
synthesize({do_expr, Steps, _Loc}, Env, State) ->
    synthesize_do_steps(Steps, Env, State);
synthesize(_Expr, _Env, State) ->
    {empty(), State}.

%%====================================================================
%% Expression-Specific Entry Points
%%====================================================================

-spec synthesize_literal(term(), state()) -> synthesis_result().
synthesize_literal(_Literal, State) ->
    {empty(), State}.

-spec synthesize_var(atom(), state()) -> synthesis_result().
synthesize_var(_Name, State) ->
    {empty(), State}.

-spec synthesize_lambda(term(), state()) -> synthesis_result().
synthesize_lambda(_Lambda, State) ->
    {empty(), State}.

-spec synthesize_app({expr(), [expr()]}, state()) -> synthesis_result().
synthesize_app({Func, Args}, State) ->
    synthesize_app({Func, Args}, catena_type_env:empty(), State).

-spec synthesize_app({expr(), [expr()]}, env(), state()) -> synthesis_result().
synthesize_app({Func, Args}, Env, State) ->
    {FuncEffects, State1} = synthesize(Func, Env, State),
    {ArgEffects, State2} = synthesize_args(Args, Env, State1),
    {union(FuncEffects, ArgEffects), State2}.

-spec synthesize_perform({atom(), atom(), [expr()]}, state()) -> synthesis_result().
synthesize_perform({Effect, Operation, Args}, State) ->
    synthesize_perform({Effect, Operation, Args}, catena_type_env:empty(), State).

-spec synthesize_perform({atom(), term(), [expr()]}, env(), state()) -> synthesis_result().
synthesize_perform({Effect, _Operation, Args}, Env, State) ->
    {ArgEffects, State1} = synthesize_args(Args, Env, State),
    {union(ArgEffects, from_list([Effect])), State1}.

-spec synthesize_handle({expr(), [term()]}, state()) -> synthesis_result().
synthesize_handle({Expr, Handlers}, State) ->
    synthesize_handle({Expr, Handlers}, catena_type_env:empty(), State).

-spec synthesize_handle({expr(), [term()]}, env(), state()) -> synthesis_result().
synthesize_handle({Expr, Handlers}, Env, State) ->
    {BodyEffects, State1} = synthesize(Expr, Env, State),
    {HandlerEffects, State2} = synthesize_handlers(Handlers, Env, State1),
    HandledEffects = handled_effects(Handlers),
    {union(remove(BodyEffects, HandledEffects), HandlerEffects), State2}.

-spec synthesize_let({expr(), expr()}, state()) -> synthesis_result().
synthesize_let({Value, Body}, State) ->
    synthesize_let({Value, Body}, catena_type_env:empty(), State).

-spec synthesize_let({expr(), expr()}, env(), state()) -> synthesis_result().
synthesize_let({Value, Body}, Env, State) ->
    {ValueEffects, State1} = synthesize(Value, Env, State),
    {BodyEffects, State2} = synthesize(Body, Env, State1),
    {union(ValueEffects, BodyEffects), State2}.

-spec synthesize_if({expr(), expr(), expr()}, state()) -> synthesis_result().
synthesize_if({Cond, Then, Else}, State) ->
    synthesize_if({Cond, Then, Else}, catena_type_env:empty(), State).

-spec synthesize_if({expr(), expr(), expr()}, env(), state()) -> synthesis_result().
synthesize_if({Cond, Then, Else}, Env, State) ->
    {CondEffects, State1} = synthesize(Cond, Env, State),
    {ThenEffects, State2} = synthesize(Then, Env, State1),
    {ElseEffects, State3} = synthesize(Else, Env, State2),
    {union(union(CondEffects, ThenEffects), ElseEffects), State3}.

-spec synthesize_match({expr(), [term()]}, state()) -> synthesis_result().
synthesize_match({Scrutinee, Cases}, State) ->
    synthesize_match({Scrutinee, Cases}, catena_type_env:empty(), State).

-spec synthesize_match({expr(), [term()]}, env(), state()) -> synthesis_result().
synthesize_match({Scrutinee, Cases}, Env, State) ->
    {ScrutineeEffects, State1} = synthesize(Scrutinee, Env, State),
    {CaseEffects, State2} = synthesize_cases(Cases, Env, State1),
    {union(ScrutineeEffects, CaseEffects), State2}.

-spec synthesize_tuple([expr()], state()) -> synthesis_result().
synthesize_tuple(Elements, State) ->
    synthesize_tuple(Elements, catena_type_env:empty(), State).

-spec synthesize_tuple([expr()], env(), state()) -> synthesis_result().
synthesize_tuple(Elements, Env, State) ->
    synthesize_args(Elements, Env, State).

-spec synthesize_list([expr()], state()) -> synthesis_result().
synthesize_list(Elements, State) ->
    synthesize_list(Elements, catena_type_env:empty(), State).

-spec synthesize_list([expr()], env(), state()) -> synthesis_result().
synthesize_list(Elements, Env, State) ->
    synthesize_args(Elements, Env, State).

%%====================================================================
%% Helpers
%%====================================================================

-spec synthesize_args([expr()], env(), state()) -> synthesis_result().
synthesize_args(Args, Env, State) ->
    lists:foldl(
        fun(Arg, {EffectsAcc, S}) ->
            {ArgEffects, NewS} = synthesize(Arg, Env, S),
            {union(EffectsAcc, ArgEffects), NewS}
        end,
        {empty(), State},
        Args
    ).

-spec synthesize_cases([term()], env(), state()) -> synthesis_result().
synthesize_cases(Cases, Env, State) ->
    lists:foldl(
        fun(Case, {EffectsAcc, S}) ->
            {CaseEffects, NewS} = synthesize_case(Case, Env, S),
            {union(EffectsAcc, CaseEffects), NewS}
        end,
        {empty(), State},
        Cases
    ).

-spec synthesize_case(term(), env(), state()) -> synthesis_result().
synthesize_case({match_clause, _Pattern, undefined, Body, _Loc}, Env, State) ->
    synthesize(Body, Env, State);
synthesize_case({match_clause, _Pattern, Guard, Body, _Loc}, Env, State) ->
    {GuardEffects, State1} = synthesize(Guard, Env, State),
    {BodyEffects, State2} = synthesize(Body, Env, State1),
    {union(GuardEffects, BodyEffects), State2};
synthesize_case({_Pattern, Body}, Env, State) ->
    synthesize(Body, Env, State);
synthesize_case({_Pattern, Guard, Body}, Env, State) ->
    {GuardEffects, State1} = synthesize(Guard, Env, State),
    {BodyEffects, State2} = synthesize(Body, Env, State1),
    {union(GuardEffects, BodyEffects), State2};
synthesize_case(_Other, _Env, State) ->
    {empty(), State}.

-spec synthesize_handlers([term()], env(), state()) -> synthesis_result().
synthesize_handlers(Handlers, Env, State) ->
    lists:foldl(
        fun(Handler, {EffectsAcc, S}) ->
            {HandlerEffects, NewS} = synthesize_handler(Handler, Env, S),
            {union(EffectsAcc, HandlerEffects), NewS}
        end,
        {empty(), State},
        Handlers
    ).

-spec synthesize_handler(term(), env(), state()) -> synthesis_result().
synthesize_handler({handler_clause, _Effect, Operations, _Loc}, Env, State) ->
    synthesize_handler_operations(Operations, Env, State);
synthesize_handler(_Other, _Env, State) ->
    {empty(), State}.

-spec synthesize_handler_operations([term()], env(), state()) -> synthesis_result().
synthesize_handler_operations(Operations, Env, State) ->
    lists:foldl(
        fun(Operation, {EffectsAcc, S}) ->
            {OperationEffects, NewS} = synthesize_handler_operation(Operation, Env, S),
            {union(EffectsAcc, OperationEffects), NewS}
        end,
        {empty(), State},
        Operations
    ).

-spec synthesize_handler_operation(term(), env(), state()) -> synthesis_result().
synthesize_handler_operation({operation_case, _Name, _Params, Body, _Loc}, Env, State) ->
    synthesize(Body, Env, State);
synthesize_handler_operation(_Other, _Env, State) ->
    {empty(), State}.

-spec synthesize_record_fields([{atom(), expr()}], env(), state()) -> synthesis_result().
synthesize_record_fields(Fields, Env, State) ->
    synthesize_args([Expr || {_Name, Expr} <- Fields], Env, State).

-spec synthesize_do_steps([term()], env(), state()) -> synthesis_result().
synthesize_do_steps(Steps, Env, State) ->
    lists:foldl(
        fun(Step, {EffectsAcc, S}) ->
            {StepEffects, NewS} = synthesize_do_step(Step, Env, S),
            {union(EffectsAcc, StepEffects), NewS}
        end,
        {empty(), State},
        Steps
    ).

-spec synthesize_do_step(term(), env(), state()) -> synthesis_result().
synthesize_do_step({do_bind, _Pattern, Expr, _Loc}, Env, State) ->
    synthesize(Expr, Env, State);
synthesize_do_step({do_action, Expr, _Loc}, Env, State) ->
    synthesize(Expr, Env, State);
synthesize_do_step({do_return, Expr, _Loc}, Env, State) ->
    synthesize(Expr, Env, State);
synthesize_do_step(_Other, _Env, State) ->
    {empty(), State}.

-spec handled_effects([term()]) -> [atom()].
handled_effects(Handlers) ->
    lists:usort(
        lists:filtermap(
            fun
                ({handler_clause, Effect, _Operations, _Loc}) -> {true, Effect};
                (_) -> false
            end,
            Handlers
        )
    ).

%%====================================================================
%% Effect Set Operations
%%====================================================================

-spec empty() -> effect_set().
empty() ->
    catena_types:empty_effects().

-spec union(effect_set(), effect_set()) -> effect_set().
union(Effects1, Effects2) ->
    catena_types:union_effects(Effects1, Effects2).

-spec remove(effect_set(), [atom()]) -> effect_set().
remove(Effects, ToRemove) ->
    lists:foldl(
        fun(Effect, Acc) ->
            catena_types:remove_effect(Effect, Acc)
        end,
        Effects,
        ToRemove
    ).

-spec contains(effect_set(), atom()) -> boolean().
contains({effect_set, Effects}, Effect) ->
    lists:member(Effect, Effects).

-spec is_empty(effect_set()) -> boolean().
is_empty({effect_set, []}) ->
    true;
is_empty({effect_set, _}) ->
    false.

-spec to_list(effect_set()) -> [atom()].
to_list({effect_set, Effects}) ->
    Effects.

-spec from_list([atom()]) -> effect_set().
from_list(Effects) ->
    catena_types:effect_set(Effects).
