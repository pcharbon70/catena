%%%-------------------------------------------------------------------
%%% @doc Catena Deep Handler Implementation (Phase 9.3)
%%%
%%% This module implements deep handlers that handle operations at all
%%% nested scope levels. Operations performed deep within nested function
%%% calls will be caught by the nearest deep handler on the stack.
%%%
%%% == Deep Handler Semantics ==
%%%
%%% Deep handlers provide the expected algebraic effects behavior similar
%%% to handlers in Koka, Eff, or other effect systems. An operation performed
%%% at any nesting level within a deep handler's scope will be handled by
%%% that handler.
%%%
%%% == Scope Behavior ==
%%%
%%% ```erlang
%%% %% This operation IS handled
%%% with_deep_handler(State, fun() ->
%%%     perform(State, get)  % Direct operation
%%% end).
%%%
%%% %% This operation IS ALSO handled
%%% with_deep_handler(State, fun() ->
%%%     nested_function()  % State operations inside are caught too
%%% end).
%%% ```
%%%
%%% == Traversal ==
%%%
%%% Deep handlers traverse the call stack to find operations. This has
%%% O(n) complexity where n is the stack depth, but optimizations like
%%% handler caching can mitigate costs.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_deep_handler).

%% Deep handler execution
-export([
    scope_effects_deep/2,
    with_deep_handler/3,
    execute_deep/3,
    wrap/1
]).

%% Deep handler scoping
-export([
    deep_scope_boundary/1,
    is_in_deep_scope/1,
    current_deep_depth/0,
    handler_stack_depth/0
]).

%% Deep handler traversal
-export([
    find_handler/2,
    traverse_handlers/2,
    cached_handler_lookup/2
]).

%%====================================================================
%% Types
%%====================================================================

-type effect_operation() :: {effect_name(), atom(), [term()]}.
-type effect_name() :: atom().
-type deep_handler() :: #{
    effect => effect_name(),
    handler => function(),
    scope => pid() | reference(),
    depth => non_neg_integer(),
    serial => non_neg_integer()
}.
-type deep_context() :: #{
    handlers => [deep_handler()],
    stack => [{effect_name(), deep_handler()}],
    cache => #{effect_name() => deep_handler()},
    serial => non_neg_integer(),
    trace => [map()]
}.

-export_type([
    deep_handler/0,
    deep_context/0
]).

%%====================================================================
%% Deep Handler Execution
%%====================================================================

%% @doc Scope effects for deep handler execution.
%% Operations at all nesting levels are handled.
-spec scope_effects_deep(deep_context(), function()) -> any().
scope_effects_deep(Context, Fun) ->
    Previous = get_deep_context(),
    Installed = normalize_context(Context, Previous),
    try
        put_deep_context(Installed),
        Fun()
    after
        put_deep_context(Previous)
    end.

%% @doc Execute a function with a deep handler.
%% The handler catches operations at all nesting levels within Fun.
-spec with_deep_handler(effect_name(), function(), function()) -> any().
with_deep_handler(EffectName, HandlerFun, UserFun) ->
    Context = get_deep_context(),
    Serial = maps:get(serial, Context, 0) + 1,
    Handler = create_deep_handler(EffectName, HandlerFun, current_deep_depth(), Serial),
    Handlers = [Handler | maps:get(handlers, Context, [])],
    Stack = [{EffectName, Handler} | maps:get(stack, Context, [])],
    Cache = maps:put(EffectName, Handler, maps:get(cache, Context, #{})),
    UpdatedContext = Context#{
        handlers => Handlers,
        stack => Stack,
        cache => Cache,
        serial => Serial
    },
    put_deep_context(UpdatedContext),
    try
        UserFun()
    after
        put_deep_context(Context)
    end.

%% @doc Execute a deep handler for an operation.
%% Returns {handled, Result} if handled, {unhandled, Operation} if not.
%% Deep handlers handle operations at any nesting level.
-spec execute_deep(effect_operation(), deep_context(), non_neg_integer()) ->
    {handled, term()} | {unhandled, effect_operation()}.
execute_deep({EffectName, Op, Args}, Context, OperationDepth) ->
    case cached_handler_lookup(EffectName, Context) of
        {value, Handler} ->
            case catena_handler_depth:handles_at_depth(
                deep,
                maps:get(depth, Handler, 0),
                OperationDepth
            ) of
                true ->
                    try_handle_deep(EffectName, Op, Args, Context, Handler);
                false ->
                    {unhandled, {EffectName, Op, Args}}
            end;
        false ->
            {unhandled, {EffectName, Op, Args}}
    end.

%% @private Try to handle with deep semantics (any handler on stack).
try_handle_deep(EffectName, Op, Args, Context, Handler) ->
    HandlerFun = maps:get(handler, Handler),
    record_trace(Context, EffectName, Op, Args, Handler),
    try
        Result = HandlerFun(Op, Args),
        {handled, Result}
    catch
        _:_ ->
            {unhandled, {EffectName, Op, Args}}
    end.

%%====================================================================
%% Deep Handler Scoping
%%====================================================================

%% @doc Create a deep scope boundary.
%% Unlike shallow boundaries, deep boundaries don't isolate operations.
-spec deep_scope_boundary(function()) -> function().
deep_scope_boundary(Fun) ->
    fun() ->
        Context = get_deep_context(),
        put_deep_context(Context),
        try
            Fun()
        after
            put_deep_context(Context)
        end
    end.

%% @doc Check if currently in a deep handler scope.
-spec is_in_deep_scope(deep_context()) -> boolean().
is_in_deep_scope(Context) ->
    length(maps:get(handlers, Context, [])) > 0.

%% @doc Get the current deep handler stack depth.
-spec current_deep_depth() -> non_neg_integer().
current_deep_depth() ->
    Context = get_deep_context(),
    length(maps:get(handlers, Context, [])).

%% @doc Get the depth of the handler stack.
-spec handler_stack_depth() -> non_neg_integer().
handler_stack_depth() ->
    Context = get_deep_context(),
    length(maps:get(stack, Context, [])).

%%====================================================================
%% Deep Handler Traversal
%%====================================================================

%% @doc Find a handler for an effect by searching the handler stack.
-spec find_handler(effect_name(), [deep_handler()]) ->
    {value, deep_handler()} | false.
find_handler(EffectName, Handlers) ->
    find_handler_for_effect(EffectName, Handlers).

%% @doc Traverse handlers to find one matching the effect.
-spec traverse_handlers(effect_name(), [deep_handler()]) ->
    {value, deep_handler()} | false.
traverse_handlers(EffectName, Handlers) ->
    lists:search(fun(H) -> maps:get(effect, H) =:= EffectName end, Handlers).

%% @doc Find handler using cache for O(1) lookup.
-spec cached_handler_lookup(effect_name(), deep_context()) ->
    {value, deep_handler()} | false.
cached_handler_lookup(EffectName, Context) ->
    Cache = maps:get(cache, Context, #{}),
    case maps:find(EffectName, Cache) of
        {ok, Handler} ->
            validate_cached_handler(EffectName, Handler, Context);
        error ->
            maybe_cache_handler(EffectName, traverse_handlers(EffectName, maps:get(handlers, Context, [])))
    end.

%% @doc Wrap a resumption with deep-handler metadata.
-spec wrap(term()) -> map().
wrap(Resumption) ->
    #{
        depth => deep,
        lookup => catena_handler_depth:lookup_strategy(deep),
        resumption => Resumption
    }.

%%====================================================================
%% Private Functions
%%====================================================================

%% @private Create a new deep handler with depth tracking.
create_deep_handler(EffectName, HandlerFun, Depth, Serial) ->
    #{
        effect => EffectName,
        handler => HandlerFun,
        scope => make_ref(),
        depth => Depth,
        serial => Serial
    }.

%% @private Find a handler for a specific effect.
find_handler_for_effect(EffectName, Handlers) ->
    lists:search(fun(H) -> maps:get(effect, H) =:= EffectName end, Handlers).

%% @private Get the current deep context from process dictionary.
get_deep_context() ->
    case get(deep_context) of
        undefined -> #{handlers => [], stack => [], cache => #{}, serial => 0, trace => []};
        Context -> Context
    end.

%% @private Put the deep context into process dictionary.
put_deep_context(Context) ->
    put(deep_context, Context).

%% @private Normalize a caller-provided deep context against the current one.
-spec normalize_context(deep_context(), deep_context()) -> deep_context().
normalize_context(Context, Previous) ->
    #{
        handlers => maps:get(handlers, Context, maps:get(handlers, Previous, [])),
        stack => maps:get(stack, Context, maps:get(stack, Previous, [])),
        cache => maps:get(cache, Context, maps:get(cache, Previous, #{})),
        serial => maps:get(serial, Context, maps:get(serial, Previous, 0)),
        trace => maps:get(trace, Context, maps:get(trace, Previous, []))
    }.

%% @private Ensure a cached handler is still active and semantically valid.
-spec validate_cached_handler(effect_name(), deep_handler(), deep_context()) ->
    {value, deep_handler()} | false.
validate_cached_handler(EffectName, Handler, Context) ->
    Handlers = maps:get(handlers, Context, []),
    case lists:member(Handler, Handlers) andalso maps:get(effect, Handler) =:= EffectName of
        true -> {value, Handler};
        false -> maybe_cache_handler(EffectName, traverse_handlers(EffectName, Handlers))
    end.

%% @private Update the process-local cache when a handler is found.
-spec maybe_cache_handler(effect_name(), {value, deep_handler()} | false) ->
    {value, deep_handler()} | false.
maybe_cache_handler(_EffectName, false) ->
    false;
maybe_cache_handler(EffectName, {value, Handler} = Result) ->
    Context = get_deep_context(),
    Cache = maps:get(cache, Context, #{}),
    put_deep_context(Context#{cache => maps:put(EffectName, Handler, Cache)}),
    Result.

%% @private Record handler telemetry in the process-local deep context.
-spec record_trace(deep_context(), effect_name(), atom(), [term()], deep_handler()) -> ok.
record_trace(Context, EffectName, Op, Args, Handler) ->
    ActiveContext = get_deep_context(),
    TraceEntry = #{
        effect => EffectName,
        operation => Op,
        args => Args,
        handler_scope => maps:get(scope, Handler, undefined),
        depth => maps:get(depth, Handler, 0)
    },
    put_deep_context(ActiveContext#{trace => [TraceEntry | maps:get(trace, Context, [])]}),
    ok.
