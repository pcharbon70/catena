%%%-------------------------------------------------------------------
%%% @doc Catena Shallow Handler Implementation (Phase 9.2)
%%%
%%% This module implements shallow handlers that only handle operations
%%% at their direct scope level. Operations performed in nested function
%%% calls bypass shallow handlers and propagate upward.
%%%
%%% == Shallow Handler Semantics ==
%%%
%%% Shallow handlers create an effect boundary - they only catch and
%%% handle operations that are performed directly within their scope.
%%% Operations performed in nested function calls will propagate through
%%% shallow handlers without being handled.
%%%
%%% == Scope Behavior ==
%%%
%%% ```erlang
%%% %% This operation IS handled
%%% with_shallow_handler(State, fun() ->
%%%     perform(State, get)  % Direct operation
%%% end).
%%%
%%% %% This operation is NOT handled (propagates upward)
%%% with_shallow_handler(State, fun() ->
%%%     nested_function()  % State operations inside propagate
%%% end).
%%% ```
%%%
%%% == Performance ==
%%%
%%% Shallow handlers have O(1) operation lookup since they only check
%%% the current scope. This provides significant performance benefits
%%% for scoped effects.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_shallow_handler).

%% Shallow handler execution
-export([
    scope_effects_shallow/2,
    with_shallow_handler/3,
    execute_shallow/3,
    wrap/1
]).

%% Shallow handler scoping
-export([
    shallow_scope_boundary/1,
    is_in_shallow_scope/1,
    current_shallow_depth/0
]).

%% Shallow handler composition
-export([
    compose_shallow/2,
    shallow_precedence/2
]).

%%====================================================================
%% Types
%%====================================================================

-type effect_operation() :: {effect_name(), atom(), [term()]}.
-type effect_name() :: atom().
-type shallow_handler() :: #{
    effect => effect_name(),
    handler => function(),
    scope => pid() | reference(),
    depth => non_neg_integer(),
    serial => non_neg_integer()
}.
-type shallow_context() :: #{
    handlers => [shallow_handler()],
    depth => non_neg_integer(),
    serial => non_neg_integer(),
    trace => [map()]
}.

-export_type([
    shallow_handler/0,
    shallow_context/0
]).

%%====================================================================
%% Shallow Handler Execution
%%====================================================================

%% @doc Scope effects for shallow handler execution.
%% Only operations at the current scope level are handled.
-spec scope_effects_shallow(shallow_context(), function()) -> any().
scope_effects_shallow(Context, Fun) ->
    Previous = get_shallow_context(),
    Installed = normalize_context(Context, Previous),
    try
        put_shallow_context(Installed),
        Fun()
    after
        put_shallow_context(Previous)
    end.

%% @doc Execute a function with a shallow handler.
%% The handler only catches operations performed directly in Fun.
-spec with_shallow_handler(effect_name(), function(), function()) -> any().
with_shallow_handler(EffectName, HandlerFun, UserFun) ->
    Context = get_shallow_context(),
    Serial = maps:get(serial, Context, 0) + 1,
    ShallowHandler = #{
        effect => EffectName,
        handler => HandlerFun,
        scope => make_ref(),
        depth => maps:get(depth, Context, 0),
        serial => Serial
    },
    UpdatedContext = Context#{
        handlers => [ShallowHandler | maps:get(handlers, Context, [])],
        serial => Serial
    },
    put_shallow_context(UpdatedContext),
    try
        UserFun()
    after
        put_shallow_context(Context)
    end.

%% @doc Execute a shallow handler for an operation.
%% Returns {handled, Result} if handled, {unhandled, Operation} if not.
-spec execute_shallow(effect_operation(), shallow_context(), non_neg_integer()) ->
    {handled, term()} | {unhandled, effect_operation()}.
execute_shallow({EffectName, Op, Args}, Context, OperationDepth) ->
    Handlers = matching_handlers(EffectName, maps:get(handlers, Context, []), OperationDepth),
    try_handle(EffectName, Op, Args, Context, Handlers).

%% @private Try to handle an operation with available shallow handlers.
try_handle(EffectName, Op, Args, Context, Handlers) ->
    case Handlers of
        [Handler | _] ->
            HandlerFun = maps:get(handler, Handler),
            record_trace(Context, EffectName, Op, Args, Handler),
            try
                Result = HandlerFun(Op, Args),
                {handled, Result}
            catch
                _:_ ->
                    {unhandled, {EffectName, Op, Args}}
            end;
        [] ->
            {unhandled, {EffectName, Op, Args}}
    end.

%%====================================================================
%% Shallow Handler Scoping
%%====================================================================

%% @doc Create a shallow scope boundary.
%% Operations performed within the returned function will be
%% scoped to not propagate out of shallow handlers at outer levels.
-spec shallow_scope_boundary(function()) -> function().
shallow_scope_boundary(Fun) ->
    fun() ->
        Context = get_shallow_context(),
        BoundaryContext = Context#{depth => maps:get(depth, Context, 0) + 1},
        put_shallow_context(BoundaryContext),
        try
            Fun()
        after
            put_shallow_context(Context)
        end
    end.

%% @doc Check if currently in a shallow handler scope.
-spec is_in_shallow_scope(shallow_context()) -> boolean().
is_in_shallow_scope(Context) ->
    length(maps:get(handlers, Context, [])) > 0.

%% @doc Get the current shallow depth.
-spec current_shallow_depth() -> non_neg_integer().
current_shallow_depth() ->
    Context = get_shallow_context(),
    maps:get(depth, Context, 0).

%%====================================================================
%% Shallow Handler Composition
%%====================================================================

%% @doc Compose two shallow handlers.
%% The resulting handler uses both handlers with precedence rules.
-spec compose_shallow(shallow_handler(), shallow_handler()) -> shallow_handler().
compose_shallow(H1, H2) ->
    Effect1 = maps:get(effect, H1),
    Effect2 = maps:get(effect, H2),
    case Effect1 =:= Effect2 of
        true ->
            #{
                effect => Effect1,
                handler => fun(Op, Args) -> composed_handler(H1, H2, Op, Args) end,
                scope => make_ref(),
                depth => max(maps:get(depth, H1, 0), maps:get(depth, H2, 0)),
                serial => max(maps:get(serial, H1, 0), maps:get(serial, H2, 0))
            };
        false ->
            error({cannot_compose_different_effects, Effect1, Effect2})
    end.

%% @private Composed handler that delegates to both handlers.
composed_handler(H1, H2, Op, Args) ->
    Handler1 = maps:get(handler, H1),
    Handler2 = maps:get(handler, H2),
    try
        Handler1(Op, Args)
    catch
        _:_ -> Handler2(Op, Args)
    end.

%% @doc Determine precedence between two shallow handlers.
%% Returns the handler that should take precedence.
-spec shallow_precedence(shallow_handler(), shallow_handler()) ->
    {first, shallow_handler()} | {second, shallow_handler()} | {equal, both}.
shallow_precedence(H1, H2) ->
    Depth1 = maps:get(depth, H1, 0),
    Depth2 = maps:get(depth, H2, 0),
    Serial1 = maps:get(serial, H1, 0),
    Serial2 = maps:get(serial, H2, 0),
    Scope1 = maps:get(scope, H1, undefined),
    Scope2 = maps:get(scope, H2, undefined),
    case {Depth1, Depth2, Scope1 =:= Scope2, Serial1, Serial2} of
        {D, D, true, _, _} -> {equal, both};
        {D1, D2, _, _, _} when D1 > D2 -> {first, H1};
        {D1, D2, _, _, _} when D2 > D1 -> {second, H2};
        {_, _, _, S1, S2} when S1 >= S2 -> {first, H1};
        _ -> {second, H2}
    end.

%% @doc Wrap a resumption with shallow-handler metadata.
-spec wrap(term()) -> map().
wrap(Resumption) ->
    #{
        depth => shallow,
        lookup => catena_handler_depth:lookup_strategy(shallow),
        resumption => Resumption
    }.

%%====================================================================
%% Private Functions
%%====================================================================

%% @private Get the current shallow context from process dictionary.
-spec get_shallow_context() -> shallow_context().
get_shallow_context() ->
    case get(shallow_context) of
        undefined -> #{handlers => [], depth => 0, serial => 0, trace => []};
        Context -> Context
    end.

%% @private Put the shallow context into process dictionary.
-spec put_shallow_context(shallow_context()) -> true.
put_shallow_context(Context) ->
    put(shallow_context, Context).

%% @private Normalize a caller-provided context against the current one.
-spec normalize_context(shallow_context(), shallow_context()) -> shallow_context().
normalize_context(Context, Previous) ->
    #{
        handlers => maps:get(handlers, Context, maps:get(handlers, Previous, [])),
        depth => maps:get(depth, Context, maps:get(depth, Previous, 0)),
        serial => maps:get(serial, Context, maps:get(serial, Previous, 0)),
        trace => maps:get(trace, Context, maps:get(trace, Previous, []))
    }.

%% @private Select handlers whose effect and depth match shallow semantics.
-spec matching_handlers(effect_name(), [shallow_handler()], non_neg_integer()) -> [shallow_handler()].
matching_handlers(EffectName, Handlers, OperationDepth) ->
    [
        Handler || Handler <- Handlers,
        maps:get(effect, Handler) =:= EffectName,
        catena_handler_depth:handles_at_depth(
            shallow,
            maps:get(depth, Handler, 0),
            OperationDepth
        )
    ].

%% @private Record handler telemetry in the process-local shallow context.
-spec record_trace(shallow_context(), effect_name(), atom(), [term()], shallow_handler()) -> ok.
record_trace(Context, EffectName, Op, Args, Handler) ->
    ActiveContext = get_shallow_context(),
    TraceEntry = #{
        effect => EffectName,
        operation => Op,
        args => Args,
        handler_scope => maps:get(scope, Handler, undefined),
        depth => maps:get(depth, Handler, 0)
    },
    put_shallow_context(ActiveContext#{trace => [TraceEntry | maps:get(trace, Context, [])]}),
    ok.
