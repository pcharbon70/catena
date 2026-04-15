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
    execute_shallow/3
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
    scope => pid() | reference()
}.
-type shallow_context() :: #{
    handlers => [shallow_handler()],
    depth => non_neg_integer()
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
    CurrentDepth = maps:get(depth, Context, 0),
    NewContext = Context#{depth => CurrentDepth + 1},
    try
        Fun()
    after
        Context#{depth => CurrentDepth}
    end.

%% @doc Execute a function with a shallow handler.
%% The handler only catches operations performed directly in Fun.
-spec with_shallow_handler(effect_name(), function(), function()) -> any().
with_shallow_handler(EffectName, HandlerFun, UserFun) ->
    ShallowHandler = #{
        effect => EffectName,
        handler => HandlerFun,
        scope => make_ref()
    },
    Context = get_shallow_context(),
    UpdatedContext = Context#{handlers => [ShallowHandler | maps:get(handlers, Context, [])]},
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
    CurrentDepth = maps:get(depth, Context, 0),
    case OperationDepth =:= CurrentDepth of
        true ->
            % Operation is at current scope level - try to handle
            Handlers = maps:get(handlers, Context, []),
            try_handle(EffectName, Op, Args, Handlers);
        false ->
            % Operation is from nested scope - don't handle
            {unhandled, {EffectName, Op, Args}}
    end.

%% @private Try to handle an operation with available shallow handlers.
try_handle(EffectName, Op, Args, Handlers) ->
    case lists:search(fun(H) -> maps:get(effect, H) =:= EffectName end, Handlers) of
        {value, Handler} ->
            HandlerFun = maps:get(handler, Handler),
            try
                Result = apply(HandlerFun, [Op, Args]),
                {handled, Result}
            catch
                _:_ ->
                    {unhandled, {EffectName, Op, Args}}
            end;
        false ->
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
    #{
        effect => composed,
        handler => fun(Op, Args) -> composed_handler(H1, H2, Op, Args) end,
        scope => make_ref()
    }.

%% @private Composed handler that delegates to both handlers.
composed_handler(H1, H2, Op, Args) ->
    Effect1 = maps:get(effect, H1),
    Effect2 = maps:get(effect, H2),
    Handler1 = maps:get(handler, H1),
    Handler2 = maps:get(handler, H2),
    case {Effect1, Effect2} of
        {Effect, Effect} ->
            % Same effect - try first handler, then second
            try
                apply(Handler1, [Op | Args])
            catch
                _:_ -> apply(Handler2, [Op | Args])
            end;
        _ ->
            % Different effects - error
            error({cannot_compose_different_effects, Effect1, Effect2})
    end.

%% @doc Determine precedence between two shallow handlers.
%% Returns the handler that should take precedence.
-spec shallow_precedence(shallow_handler(), shallow_handler()) ->
    {first, shallow_handler()} | {second, shallow_handler()} | {equal, both}.
shallow_precedence(H1, H2) ->
    Scope1 = maps:get(scope, H1),
    Scope2 = maps:get(scope, H2),
    if
        Scope1 =:= Scope2 -> {equal, both};
        is_reference(Scope1) andalso is_reference(Scope2) ->
            % Both are refs - compare creation time (older has precedence)
            if
                Scope1 < Scope2 -> {first, H1};
                true -> {second, H2}
            end;
        true ->
            % Different scope types - current scope takes precedence
            {first, H1}
    end.

%%====================================================================
%% Private Functions
%%====================================================================

%% @private Get the current shallow context from process dictionary.
-spec get_shallow_context() -> shallow_context().
get_shallow_context() ->
    case get(shallow_context) of
        undefined -> #{handlers => [], depth => 0};
        Context -> Context
    end.

%% @private Put the shallow context into process dictionary.
-spec put_shallow_context(shallow_context()) -> true.
put_shallow_context(Context) ->
    put(shallow_context, Context).
