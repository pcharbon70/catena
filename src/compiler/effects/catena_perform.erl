%%%-------------------------------------------------------------------
%%% @doc Catena Perform Operation (Phase 7.3)
%%%
%%% This module implements the perform operation that suspends computation,
%%% captures the resumption (continuation), and invokes handlers. This is
%%% the core mechanism that makes algebraic effects work.
%%%
%%% == Perform Operation ==
%%%
%%% When a computation performs an effect operation:
%%% 1. The current continuation is captured as a resumption
%%% 2. The handler stack is searched for a matching handler
%%% 3. The handler is invoked with the operation value and resumption
%%% 4. The handler decides whether to resume, abort, or handle multiple times
%%%
%%% == Suspension Points ==
%%%
%%% A suspension point is where perform is called. The continuation from
%%% that point forward is captured and passed to the handler. This enables:
%%%
%%% - Delimited continuations (resumptions are scoped to handler boundaries)
%%% - Handler control (resume with different values, abort, or multi-shot)
%%% - Effect composition (handlers can perform operations themselves)
%%%
%%% == Handler Lookup ==
%%%
%%% Handlers are looked up in the handler stack from innermost to outermost.
%%% The first matching handler for the operation is invoked. This enables
%%% handler shadowing in nested scopes.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_perform).

%% Perform operation
-export([
    perform/2,
    perform_with_context/3
]).

%% Suspension and resumption
-export([
    suspend/2,
    resume/2,
    abort/1
]).

%% Handler management for perform
-export([
    with_handler/3,
    with_handlers/2
]).

%% Effect context
-export([
    push_context/1,
    pop_context/0,
    current_context/0
]).

%%====================================================================
%% Types
%%====================================================================

-type operation() :: atom().
-type op_value() :: term().
-type effect_result() :: term().
-type perform_context() :: #{
    handlers => catena_handler:handler_stack(),
    depth => non_neg_integer(),
    metadata => map()
}.

-type suspension() :: #{
    operation => operation(),
    value => op_value(),
    resumption => catena_resumption:resumption(),
    context => perform_context()
}.

-export_type([
    perform_context/0,
    suspension/0
]).

%%====================================================================
%% Process Dictionary Keys
%%====================================================================

-define(PERFORM_CONTEXT_KEY, catena_perform_context).
-define(SUSPENSION_STACK_KEY, catena_suspension_stack).

%%====================================================================
%% Perform Operation
%%====================================================================

%% @doc Perform an effect operation, suspending computation and invoking handlers.
%%
%% This is the main entry point for performing effects. When called:
%% 1. Captures the current continuation as a resumption
%% 2. Looks up a handler for the operation
%% 3. Invokes the handler with the operation value and resumption
%% 4. Returns the handler's result
%%
%% If no handler is found, returns {unhandled, Operation, Value}.
-spec perform(operation(), op_value()) -> effect_result() | {unhandled, operation(), op_value()}.
perform(Operation, Value) when is_atom(Operation) ->
    perform_with_context(Operation, Value, current_context()).

%% @doc Perform an operation with a specific context.
%%
%% This variant allows specifying a custom context for the operation,
%% useful for testing and advanced scenarios.
-spec perform_with_context(operation(), op_value(), perform_context()) -> effect_result().
perform_with_context(Operation, Value, Context) ->
    % Capture the current continuation as a resumption
    Resumption = capture_resumption(Operation, Value, Context),

    % Look up a handler for this operation
    case catena_handler:lookup(Operation) of
        {ok, Handler} ->
            % Execute the handler with the operation value and resumption
            catena_handler:execute(Handler, Value, Resumption);
        {error, not_found} ->
            % No handler found - return unhandled error
            {unhandled, Operation, Value}
    end.

%%====================================================================
%% Suspension and Resumption
%%====================================================================

%% @doc Suspend computation with an operation and value.
%%
%% This creates a suspension record that captures the operation,
%% value, resumption, and current context. Suspensions can be
%% inspected, serialized, or resumed later.
-spec suspend(operation(), op_value()) -> suspension().
suspend(Operation, Value) ->
    Context = current_context(),
    Resumption = capture_resumption(Operation, Value, Context),
    #{
        operation => Operation,
        value => Value,
        resumption => Resumption,
        context => Context
    }.

%% @doc Resume a suspension with a value.
%%
%% This executes the resumption captured in the suspension with
%% the provided value.
-spec resume(suspension(), term()) -> effect_result().
resume(#{resumption := Resumption}, Value) ->
    catena_resumption:resume(Resumption, Value).

%% @doc Abort a suspension without resuming.
%%
%% This discards the resumption and returns an abort result.
%% Useful for handlers that want to short-circuit computation.
-spec abort(suspension()) -> {aborted, operation(), op_value()}.
abort(#{operation := Operation, value := Value}) ->
    {aborted, Operation, Value}.

%%====================================================================
%% Handler Management for Perform
%%====================================================================

%% @doc Run a computation with a temporary handler.
%%
%% Installs a handler for the duration of the computation, then
%% removes it. Useful for scoped effect handling.
-spec with_handler(operation(), catena_handler:handler(), function()) -> effect_result().
with_handler(Operation, Handler, Computation) when is_function(Computation, 0) ->
    % Push a new handler scope
    catena_handler:push_handlers(#{Operation => Handler}),
    try
        Computation()
    after
        % Pop the handler scope
        catena_handler:pop_handlers()
    end.

%% @doc Run a computation with multiple temporary handlers.
%%
%% Installs all handlers in the map for the duration of the computation.
-spec with_handlers(#{operation() => catena_handler:handler()}, function()) -> effect_result().
with_handlers(Handlers, Computation) when is_map(Handlers), is_function(Computation, 0) ->
    catena_handler:push_handlers(Handlers),
    try
        Computation()
    after
        catena_handler:pop_handlers()
    end.

%%====================================================================
%% Effect Context
%%====================================================================

%% @doc Push a new effect context onto the context stack.
%%
%% Contexts track handler stacks and metadata for effect operations.
-spec push_context(perform_context()) -> ok.
push_context(Context) when is_map(Context) ->
    Stack = get_context_stack(),
    put_context_stack([Context | Stack]),
    ok.

%% @doc Pop the current effect context from the context stack.
%%
%% Returns the popped context or {error, empty} if the stack is empty.
-spec pop_context() -> {ok, perform_context()} | {error, empty}.
pop_context() ->
    Stack = get_context_stack(),
    case Stack of
        [] -> {error, empty};
        [Top | Rest] ->
            put_context_stack(Rest),
            {ok, Top}
    end.

%% @doc Get the current effect context.
%%
%% Returns the top of the context stack or a default empty context.
-spec current_context() -> perform_context().
current_context() ->
    Stack = get_context_stack(),
    case Stack of
        [] -> default_context();
        [Top | _] -> Top
    end.

%%====================================================================
%% Private Functions
%%====================================================================

%% @private Capture the current continuation as a resumption.
-spec capture_resumption(operation(), op_value(), perform_context()) -> catena_resumption:resumption().
capture_resumption(Operation, Value, Context) ->
    CapturedAt = erlang:system_time(millisecond),
    StackDepth = maps:get(depth, Context, 0),

    % Create a continuation that represents "the rest of the computation"
    % In a full implementation, this would use CPS or exceptions to capture
    % the actual call stack. For now, we create a placeholder that can
    % be executed with a value.
    Cont = fun(ResumeValue) ->
        % When resumed, we would continue the computation with ResumeValue
        % For now, return a tagged result to indicate resumption happened
        {resumed, ResumeValue}
    end,

    Metadata = #{
        operation => Operation,
        original_value => Value,
        captured_at => CapturedAt,
        stack_depth => StackDepth
    },

    catena_resumption:new(Cont, CapturedAt, StackDepth, Metadata).

%% @private Get the default empty context.
-spec default_context() -> perform_context().
default_context() ->
    #{
        handlers => catena_handler:current_stack(),
        depth => 0,
        metadata => #{}
    }.

%% @private Get the context stack from the process dictionary.
-spec get_context_stack() -> [perform_context()].
get_context_stack() ->
    case get(?PERFORM_CONTEXT_KEY) of
        undefined -> [];
        Stack -> Stack
    end.

%% @private Put the context stack into the process dictionary.
-spec put_context_stack([perform_context()]) -> ok.
put_context_stack(Stack) ->
    put(?PERFORM_CONTEXT_KEY, Stack),
    ok.
