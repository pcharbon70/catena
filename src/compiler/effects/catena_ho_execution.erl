%%%-------------------------------------------------------------------
%%% @doc Catena Higher-Order Effect Execution (Phase 13.4)
%%%
%%% This module implements execution semantics for higher-order effects.
%%% When operations can take effectful functions as arguments, the
%%% execution semantics must handle invoking those functions with proper
%%% effect tracking and propagation.
%%%
%%% == Higher-Order Operation Execution ==
%%%
%%% Higher-order operations like `iterate` or `catch` take effectful
%%% functions as parameters. When such operations are executed, the
%%% parameter functions must be invoked with their effects properly
%%% tracked and propagated to the calling context.
%%%
%%% == Effectual Handler Invocation ==
%%%
%%% Handlers that perform effects during their execution must preserve
%%% the effect row and propagate effects correctly. This module provides
%%% mechanisms for:
%%% 1. Capturing the current effect context
%%% 2. Executing effectual handler code
%%% 3. Propagating handler effects to the continuation
%%%
%%% == Higher-Order Effect Optimization ==
%%%
%%% Optimization strategies for higher-order effects include:
%%% 1. Effectual handler fusion - combine sequential handler calls
%%% 2. Effectual handler inlining - inline simple handlers
%%% 3. Higher-order effect specialization - specialize based on known effects
%%% 4. Dead code elimination - remove unused effect paths
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_ho_execution).

%% Higher-order operation execution
-export([
    execute_ho_op/4,
    execute_with_params/3,
    invoke_effectful_param/2
]).

%% Effectual handler invocation
-export([
    invoke_effectual_handler/3,
    propagate_handler_effects/2,
    capture_effect_context/1,
    restore_effect_context/2
]).

%% Higher-order effect optimization
-export([
    fuse_effectual_handlers/1,
    inline_simple_handlers/1,
    specialize_ho_effects/2,
    eliminate_dead_effects/1
]).

%% Execution context management
-export([
    new_context/0,
    context_effects/1,
    context_add_effects/2,
    context_merge/2,
    context_clone/1
]).

%% Error handling
-export([
    handle_execution_error/3,
    recover_from_error/2,
    is_recoverable/1
]).

%% Accessor functions
-export([
    context_error_state/1,
    context_with_error/1,
    context_with_effects/1
]).

-export_type([
    execution_context/0,
    ho_result/0,
    handler_result/0
]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

%% @doc Execution context for higher-order effects.
-record(execution_context, {
    effects :: map(),
    handler_stack :: [term()],
    error_state :: normal | {error, term()}
}).

-type execution_context() :: #execution_context{}.

%% @doc Result of higher-order operation execution.
-type ho_result() :: {ok, term(), execution_context()} |
                     {error, term(), execution_context()}.

%% @doc Result of effectual handler invocation.
-type handler_result() :: {ok, term(), map()} |
                          {error, term()}.

%%%---------------------------------------------------------------------
%%% Higher-Order Operation Execution
%%%---------------------------------------------------------------------

%% @doc Execute a higher-order operation with parameters.
-spec execute_ho_op(atom(), atom(), [term()], execution_context()) -> ho_result().
execute_ho_op(Effect, Operation, Args, Context) ->
    try
        case classify_operation(Effect, Operation) of
            higher_order ->
                execute_ho(Effect, Operation, Args, Context);
            first_order ->
                execute_first_order(Effect, Operation, Args, Context)
        end
    catch
        error:{execution_error, Reason} ->
            {error, Reason, Context}
    end.

%% @private
classify_operation(Effect, Operation) ->
    %% In a real implementation, this would check the operation signature
    %% to determine if it's higher-order. For now, we use a heuristic.
    Arity = get_operation_arity(Effect, Operation),
    case Arity of
        unknown -> first_order;
        _ when Arity > 1 -> higher_order;
        _ -> first_order
    end.

%% @private
get_operation_arity(_Effect, _Operation) ->
    %% In a real implementation, this would look up the operation signature
    unknown.

%% @private
execute_ho(Effect, Operation, Args, Context) ->
    %% Execute a higher-order operation
    %% This involves identifying effectful parameters and executing them
    case Args of
        [EffectfulFun | Rest] when is_function(EffectfulFun, 1) ->
            %% Check if the function is effectful
            case is_effectful_function(EffectfulFun, Context) of
                true ->
                    invoke_effectful_param(EffectfulFun, Rest);
                false ->
                    {ok, EffectfulFun(hd(Rest)), Context}
            end;
        _ ->
            execute_first_order(Effect, Operation, Args, Context)
    end.

%% @private
has_arity(Element, Arity) ->
    is_function(Element) andalso erlang:fun_info(Element, arity) =:= {arity, Arity}.

%% @private
is_effectful_function(_Fun, _Context) ->
    %% In a real implementation, this would check the function's type
    false.

%% @private
execute_first_order(_Effect, _Operation, _Args, Context) ->
    %% Execute a first-order operation
    {ok, undefined, Context}.

%% @doc Execute with parameters that may be effectful.
-spec execute_with_params(atom(), atom(), [term()]) -> ho_result().
execute_with_params(Effect, Operation, Params) ->
    Context = new_context(),
    execute_ho_op(Effect, Operation, Params, Context).

%% @doc Invoke an effectful parameter function.
-spec invoke_effectful_param(function(), [term()]) -> {ok, term()} | {error, term()}.
invoke_effectful_param(Fun, Args) when is_function(Fun), is_list(Args) ->
    try
        Result = apply(Fun, Args),
        {ok, Result}
    catch
        _:Reason ->
            {error, Reason}
    end.

%%%---------------------------------------------------------------------
%%% Effectual Handler Invocation
%%%---------------------------------------------------------------------

%% @doc Invoke an effectual handler with proper effect tracking.
-spec invoke_effectual_handler(function(), [term()], execution_context()) -> handler_result().
invoke_effectual_handler(HandlerFn, Args, Context) ->
    %% Capture current effect context
    OriginalEffects = context_effects(Context),

    %% Invoke the handler
    case safe_apply(HandlerFn, Args) of
        {ok, Result} ->
            %% Get effects from handler execution
            HandlerEffects = extract_handler_effects(Result),
            %% Merge effects
            MergedEffects = merge_effect_rows(OriginalEffects, HandlerEffects),
            {ok, Result, MergedEffects};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Propagate effects from handler execution to continuation.
-spec propagate_handler_effects(map(), execution_context()) -> execution_context().
propagate_handler_effects(Effects, Context) ->
    CurrentEffects = context_effects(Context),
    Merged = merge_effect_rows(CurrentEffects, Effects),
    Context#execution_context{effects = Merged}.

%% @doc Capture the current effect context.
-spec capture_effect_context(execution_context()) -> map().
capture_effect_context(Context) ->
    context_effects(Context).

%% @doc Restore a previously captured effect context.
-spec restore_effect_context(map(), execution_context()) -> execution_context().
restore_effect_context(Effects, Context) ->
    Context#execution_context{effects = Effects}.

%%%---------------------------------------------------------------------
%%% Higher-Order Effect Optimization
%%%---------------------------------------------------------------------

%% @doc Fuse sequential effectual handler calls.
-spec fuse_effectual_handlers([term()]) -> [term()].
fuse_effectual_handlers(Handlers) when is_list(Handlers) ->
    case Handlers of
        [] -> [];
        [Single] -> [Single];
        [H1, H2 | Rest] ->
            case can_fuse(H1, H2) of
                true ->
                    Fused = fuse_two(H1, H2),
                    fuse_effectual_handlers([Fused | Rest]);
                false ->
                    [H1 | fuse_effectual_handlers([H2 | Rest])]
            end
    end.

%% @private
can_fuse(_H1, _H2) ->
    %% In a real implementation, this would check if two handlers can be fused
    false.

%% @private
fuse_two(H1, H2) ->
    %% In a real implementation, this would fuse two handlers
    {fused, H1, H2}.

%% @doc Inline simple effectual handlers.
-spec inline_simple_handlers([term()]) -> [term()].
inline_simple_handlers(Handlers) when is_list(Handlers) ->
    [case is_simple_handler(H) of
        true -> inline_handler(H);
        false -> H
    end || H <- Handlers].

%% @private
is_simple_handler(_Handler) ->
    %% In a real implementation, this would check if a handler is simple
    false.

%% @private
inline_handler(_Handler) ->
    %% In a real implementation, this would inline the handler
    inlined.

%% @doc Specialize higher-order effects based on known effects.
-spec specialize_ho_effects(term(), map()) -> term().
specialize_ho_effects(HOType, KnownEffects) ->
    %% In a real implementation, this would specialize based on known effects
    HOType.

%% @doc Eliminate dead effect paths.
-spec eliminate_dead_effects(term()) -> term().
eliminate_dead_effects(Term) ->
    %% In a real implementation, this would eliminate unused effect paths
    Term.

%%%---------------------------------------------------------------------
%%% Execution Context Management
%%%---------------------------------------------------------------------

%% @doc Create a new execution context.
-spec new_context() -> execution_context().
new_context() ->
    #execution_context{
        effects = empty_effect_row(),
        handler_stack = [],
        error_state = normal
    }.

%% @doc Get effects from execution context.
-spec context_effects(execution_context()) -> map().
context_effects(#execution_context{effects = Effects}) ->
    Effects.

%% @doc Add effects to execution context.
-spec context_add_effects(execution_context(), map()) -> execution_context().
context_add_effects(Context, NewEffects) ->
    CurrentEffects = context_effects(Context),
    Merged = merge_effect_rows(CurrentEffects, NewEffects),
    Context#execution_context{effects = Merged}.

%% @doc Merge two execution contexts.
-spec context_merge(execution_context(), execution_context()) -> execution_context().
context_merge(C1, C2) ->
    Effects1 = context_effects(C1),
    Effects2 = context_effects(C2),
    MergedEffects = merge_effect_rows(Effects1, Effects2),
    #execution_context{
        effects = MergedEffects,
        handler_stack = lists:usort(
            C1#execution_context.handler_stack ++
            C2#execution_context.handler_stack
        ),
        error_state = case {C1#execution_context.error_state, C2#execution_context.error_state} of
            {normal, normal} -> normal;
            {{error, _}, _} -> C1#execution_context.error_state;
            {_, {error, _}} -> C2#execution_context.error_state
        end
    }.

%% @doc Clone an execution context.
-spec context_clone(execution_context()) -> execution_context().
context_clone(Context) ->
    Context#execution_context{
        handler_stack = lists:reverse(Context#execution_context.handler_stack)
    }.

%% @doc Get the error state from an execution context.
-spec context_error_state(execution_context()) -> normal | {error, term()}.
context_error_state(#execution_context{error_state = ErrorState}) ->
    ErrorState.

%% @doc Create a context with an error state.
-spec context_with_error(term()) -> execution_context().
context_with_error(Reason) ->
    #execution_context{
        effects = empty_effect_row(),
        handler_stack = [],
        error_state = {error, Reason}
    }.

%% @doc Create a context with specific effects.
-spec context_with_effects(map()) -> execution_context().
context_with_effects(Effects) ->
    #execution_context{
        effects = Effects,
        handler_stack = [],
        error_state = normal
    }.

%%%---------------------------------------------------------------------
%%% Error Handling
%%%---------------------------------------------------------------------

%% @doc Handle an execution error with recovery.
-spec handle_execution_error(term(), execution_context(), function()) -> ho_result().
handle_execution_error(Reason, Context, RecoveryFn) when is_function(RecoveryFn, 1) ->
    case is_recoverable(Reason) of
        true ->
            try RecoveryFn(Reason) of
                {ok, Value} ->
                    {ok, Value, Context};
                {error, NewReason} ->
                    {error, NewReason, Context#execution_context{error_state = {error, NewReason}}}
            catch
                _:CatchReason ->
                    {error, CatchReason, Context#execution_context{error_state = {error, CatchReason}}}
            end;
        false ->
            {error, Reason, Context#execution_context{error_state = {error, Reason}}}
    end.

%% @doc Recover from an error in effect execution.
-spec recover_from_error(execution_context(), function()) -> execution_context().
recover_from_error(#execution_context{error_state = {error, Reason}} = Context, RecoveryFn) ->
    case is_recoverable(Reason) of
        true ->
            try RecoveryFn(Reason) of
                {ok, _} ->
                    Context#execution_context{error_state = normal};
                {error, _} ->
                    Context
            catch
                _:_ ->
                    Context
            end;
        false ->
            Context
    end;
recover_from_error(Context, _RecoveryFn) ->
    Context.

%% @doc Check if an error is recoverable.
-spec is_recoverable(term()) -> boolean().
is_recoverable({error, Reason}) ->
    is_recoverable(Reason);
is_recoverable(Reason) when is_atom(Reason) ->
    case Reason of
        temporary -> true;
        timeout -> true;
        retryable -> true;
        _ -> false
    end;
is_recoverable(_) ->
    false.

%%%---------------------------------------------------------------------
%%% Internal Helpers
%%%---------------------------------------------------------------------

%% @private
safe_apply(Fun, Args) when is_function(Fun), is_list(Args) ->
    try
        Arity = erlang:fun_info(Fun, arity),
        case {Arity, length(Args)} of
            {{arity, Expected}, Expected} ->
                {ok, apply(Fun, Args)};
            _ ->
                {error, arity_mismatch}
        end
    catch
        _:Reason ->
            {error, Reason}
    end.

%% @private
extract_handler_effects(Result) ->
    %% In a real implementation, this would extract effects from the result
    empty_effect_row().

%% @private
merge_effect_rows(#{kind := effect_row, elements := E1, row_var := RV1},
                   #{kind := effect_row, elements := E2, row_var := RV2}) ->
    #{
        kind => effect_row,
        elements => lists:usort(E1 ++ E2),
        row_var => case {RV1, RV2} of
            {undefined, undefined} -> undefined;
            {undefined, _} -> RV2;
            {_, undefined} -> RV1;
            _ -> RV1
        end
    };
merge_effect_rows(E1, _) ->
    E1.

%% @private
empty_effect_row() ->
    #{
        kind => effect_row,
        elements => [],
        row_var => undefined
    }.
