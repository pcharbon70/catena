%%%-------------------------------------------------------------------
%%% @doc Dedicated effect/runtime code generation helpers.
%%%
%%% This module centralizes Core Erlang generation for perform/handle
%%% expressions and runtime bootstrap calls so the effect translation
%%% path is explicit rather than being embedded ad hoc in
%%% `catena_codegen_expr`.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_codegen).

-export([
    translate_perform/2,
    translate_handle/2,
    translate_try_with/2,
    translate_handlers/2,
    translate_handler/2,
    runtime_context_var/0,
    with_runtime_call/1,
    runtime_init_call/0,
    runtime_shutdown_call/0
]).

-include_lib("compiler/src/core_parse.hrl").

-spec translate_perform(term(), catena_codegen_utils:codegen_state()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.
translate_perform({perform_expr, Effect, Operation, Args, _Loc}, State) ->
    {CoreArgs, State1} = catena_codegen_expr:translate_exprs(Args, State),
    PerformCall = cerl:c_call(
        cerl:c_atom(catena_effect_runtime),
        cerl:c_atom(perform),
        [
            runtime_context_var(),
            cerl:c_atom(Effect),
            cerl:c_atom(Operation),
            build_list(CoreArgs)
        ]
    ),
    {PerformCall, State1}.

-spec translate_handle(term(), catena_codegen_utils:codegen_state()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.
translate_handle({handle_expr, Body, Handlers, _Loc}, State) ->
    {CoreBody, State1} = catena_codegen_expr:translate_expr(Body, State),
    {HandlerSpecs, State2} = translate_handlers(Handlers, State1),
    BodyFun = cerl:c_fun([runtime_context_var()], CoreBody),
    WithHandlers = cerl:c_call(
        cerl:c_atom(catena_effect_runtime),
        cerl:c_atom(with_handlers),
        [runtime_context_var(), HandlerSpecs, BodyFun]
    ),
    {WithHandlers, State2};
translate_handle({try_with_expr, Body, Handlers, Loc}, State) ->
    translate_handle({handle_expr, Body, Handlers, Loc}, State).

-spec translate_try_with(term(), catena_codegen_utils:codegen_state()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.
translate_try_with(Expr, State) ->
    translate_handle(Expr, State).

-spec translate_handlers([term()], catena_codegen_utils:codegen_state()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.
translate_handlers(Handlers, State) ->
    {CoreHandlers, FinalState} = lists:mapfoldl(
        fun(Handler, St) ->
            translate_handler(Handler, St)
        end,
        State,
        Handlers
    ),
    {build_list(CoreHandlers), FinalState}.

-spec translate_handler(term(), catena_codegen_utils:codegen_state()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.
translate_handler({handler_clause, Effect, Operations, _Loc}, State) ->
    {OpCases, State1} = lists:mapfoldl(
        fun({operation_case, OpName, Params, Body, _OpLoc}, St) ->
            ParamVars = [cerl:c_var(param_name(P)) || P <- Params],
            {CoreBody, St1} = catena_codegen_expr:translate_expr(Body, St),
            HandlerFun = cerl:c_fun(ParamVars, CoreBody),
            OpSpec = cerl:c_tuple([cerl:c_atom(OpName), HandlerFun]),
            {OpSpec, St1}
        end,
        State,
        Operations
    ),
    HandlerSpec = cerl:c_tuple([
        cerl:c_atom(Effect),
        build_list(OpCases)
    ]),
    {HandlerSpec, State1}.

-spec runtime_context_var() -> cerl:cerl().
runtime_context_var() ->
    cerl:c_var('__catena_ctx__').

-spec with_runtime_call(cerl:cerl()) -> cerl:cerl().
with_runtime_call(CoreBody) ->
    BodyFun = cerl:c_fun([runtime_context_var()], CoreBody),
    cerl:c_call(
        cerl:c_atom(catena_effect_system),
        cerl:c_atom(with_runtime),
        [BodyFun]
    ).

-spec runtime_init_call() -> cerl:cerl().
runtime_init_call() ->
    cerl:c_call(
        cerl:c_atom(catena_effect_system),
        cerl:c_atom(start_runtime),
        []
    ).

-spec runtime_shutdown_call() -> cerl:cerl().
runtime_shutdown_call() ->
    cerl:c_call(
        cerl:c_atom(catena_effect_system),
        cerl:c_atom(stop_runtime),
        []
    ).

param_name({var, Name, _}) -> Name;
param_name({typed_var, Name, _, _}) -> Name;
param_name({pat_var, Name, _}) -> Name;
param_name(_) -> '_'.

build_list(Elements) ->
    lists:foldr(fun cerl:c_cons/2, cerl:c_nil(), Elements).
