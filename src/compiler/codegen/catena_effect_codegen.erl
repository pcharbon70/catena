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
    with_runtime_call/2,
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
            current_context(State),
            cerl:c_atom(Effect),
            cerl:c_atom(Operation),
            build_list(CoreArgs)
        ]
    ),
    {PerformCall, State1}.

-spec translate_handle(term(), catena_codegen_utils:codegen_state()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.
translate_handle({handle_expr, Body, Handlers, _Loc}, State) ->
    {HandlerSpecs, State1} = translate_handlers(Handlers, State),
    {ChildContext, State2} = catena_codegen_utils:fresh_var(State1),
    {CoreBody, State3} = catena_codegen_utils:with_runtime_context(
        ChildContext,
        fun(ScopedState) ->
            catena_codegen_expr:translate_expr(Body, ScopedState)
        end,
        State2
    ),
    BodyFun = cerl:c_fun([ChildContext], CoreBody),
    WithHandlers = cerl:c_call(
        cerl:c_atom(catena_effect_runtime),
        cerl:c_atom(with_handlers),
        [current_context(State), HandlerSpecs, BodyFun]
    ),
    {WithHandlers, State3};
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
translate_handler({handler_clause, Effect, Operations, HandlerLocation}, State) ->
    {OpCases, State1} = lists:mapfoldl(
        fun(
            {operation_case, OpName, Params, Body, OperationLocation},
            St
        ) ->
            {HandlerFun, St1} = translate_operation_handler(
                Params,
                Body,
                OperationLocation,
                St
            ),
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
    _ = HandlerLocation,
    {HandlerSpec, State1}.

translate_operation_handler([], Body, _Location, State) ->
    {CoreBody, State1} = catena_codegen_expr:translate_expr(
        Body,
        State
    ),
    {cerl:c_fun([], CoreBody), State1};
translate_operation_handler(Params, Body, Location, State) ->
    {Arguments, State1} = catena_codegen_utils:fresh_vars(
        length(Params),
        State
    ),
    {Scrutinee, Pattern} = handler_match_shape(
        Arguments,
        Params,
        Location
    ),
    {CoreBody, State2} = catena_codegen_pattern:compile_match(
        Scrutinee,
        [{clause, [Pattern], [], Body}],
        State1,
        #{optimize => true, warn_incomplete => true}
    ),
    {cerl:c_fun(Arguments, CoreBody), State2}.

handler_match_shape([Argument], [Pattern], _Location) ->
    {Argument, normalize_handler_pattern(Pattern)};
handler_match_shape(Arguments, Params, Location) ->
    {
        cerl:c_tuple(Arguments),
        {pat_tuple, [normalize_handler_pattern(Param) || Param <- Params], Location}
    }.

%% Legacy raw-codegen fixtures represented a handler variable parameter with
%% the expression node used for variable references. The parser-native AST
%% uses `pat_var`; normalize only this historical shape at the handler
%% boundary so all other patterns retain the ordinary pattern compiler's
%% fail-closed behavior.
normalize_handler_pattern({var, Name, Location}) ->
    {pat_var, Name, Location};
normalize_handler_pattern(Pattern) ->
    Pattern.

-spec runtime_context_var() -> cerl:cerl().
runtime_context_var() ->
    cerl:c_var('__catena_ctx__').

-spec with_runtime_call(cerl:cerl()) -> cerl:cerl().
with_runtime_call(CoreBody) ->
    with_runtime_call(runtime_context_var(), CoreBody).

-spec with_runtime_call(cerl:cerl(), cerl:cerl()) -> cerl:cerl().
with_runtime_call(ContextVar, CoreBody) ->
    BodyFun = cerl:c_fun([ContextVar], CoreBody),
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

current_context(State) ->
    case catena_codegen_utils:runtime_context(State) of
        undefined -> runtime_context_var();
        Context -> Context
    end.

build_list(Elements) ->
    lists:foldr(fun cerl:c_cons/2, cerl:c_nil(), Elements).
