%%%-------------------------------------------------------------------
%%% @doc Catena Handler Type and Interface (Phase 7.2)
%%%
%%% This module defines the handler type and interface for handlers that
%%% receive both operation values and resumptions. This is the core of
%%% algebraic effects, enabling handlers to decide whether to resume,
%%% abort, or handle operations multiple times.
%%%
%%% == Handler Type ==
%%%
%%% A handler is a function that receives an operation value and a
%%% resumption, returning a result. The handler can:
%%%
%%% - Resume: Call the resumption with a value to continue computation
%%% - Abort: Return a value without resuming
%%% - Multi-shot: Resume the resumption multiple times
%%%
%%% - type handler() :: #handler{}
%%%   - operation: The operation this handler handles (atom)
%%%   - handler_fn: The handler function (arity 2: value and resumption)
%%%   - metadata: Additional metadata about the handler
%%%
%%% == Handler Registration ==
%%%
%%% Handlers are registered for specific operations using the handler
%%% registry. The registry maintains a stack of handlers for nested
%%% scopes.
%%%
%%% == Handler Execution Protocol ==
%%%
%%% When an effect is performed, the handler lookup finds the appropriate
%%% handler and invokes it with the operation value and resumption.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_handler).

%% Handler type and constructors
-export([
    new/2,
    new/3,
    is_handler/1,
    operation_of/1,
    handler_fn_of/1,
    metadata_of/1
]).

%% Handler registration and lookup
-export([
    register/2,
    unregister/1,
    lookup/1
]).

%% Handler stack management
-export([
    push_handlers/1,
    pop_handlers/0,
    current_stack/0,
    stack_depth/0
]).

%% Handler execution
-export([
    execute/3,
    execute_with_timeout/4
]).

%%====================================================================
%% Types
%%====================================================================

-record(handler, {
    operation :: atom(),
    handler_fn :: {function(), arity()},
    metadata :: map()
}).

-opaque handler() :: #handler{}.

-type handler_fn() :: function().
-type handler_registry() :: #{atom() => handler()}.
-type handler_stack() :: [handler_registry()].

-export_type([handler/0]).

%%====================================================================
%% Process Dictionary Keys
%%====================================================================

-define(HANDLER_STACK_KEY, catena_handler_stack).

%%====================================================================
%% Handler Type and Constructors
%%====================================================================

%% @doc Create a new handler for an operation.
-spec new(atom(), handler_fn()) -> handler().
new(Operation, HandlerFn) when is_atom(Operation), is_function(HandlerFn, 2) ->
    new(Operation, HandlerFn, #{}).

%% @doc Create a new handler with metadata.
-spec new(atom(), handler_fn(), map()) -> handler().
new(Operation, HandlerFn, Metadata) when is_atom(Operation), is_function(HandlerFn, 2), is_map(Metadata) ->
    #handler{
        operation = Operation,
        handler_fn = {HandlerFn, 2},
        metadata = Metadata
    }.

%% @doc Check if a term is a handler.
-spec is_handler(term()) -> boolean().
is_handler(#handler{}) -> true;
is_handler(_) -> false.

%% @doc Get the operation this handler handles.
-spec operation_of(handler()) -> atom().
operation_of(#handler{operation = Operation}) ->
    Operation.

%% @doc Get the handler function.
-spec handler_fn_of(handler()) -> handler_fn().
handler_fn_of(#handler{handler_fn = {Fn, _}}) ->
    Fn.

%% @doc Get the metadata associated with a handler.
-spec metadata_of(handler()) -> map().
metadata_of(#handler{metadata = Metadata}) ->
    Metadata.

%%====================================================================
%% Handler Registration
%%====================================================================

%% @doc Register a handler for an operation.
%%
%% Replaces any existing handler for the same operation in the current scope.
-spec register(atom(), handler()) -> ok.
register(Operation, #handler{operation = Operation} = Handler) ->
    case catena_handler_stack:current() of
        undefined -> {error, no_current_scope};
        _ -> catena_handler_stack:register_in_current(Operation, Handler)
    end.

%% @doc Unregister a handler for an operation.
-spec unregister(atom()) -> ok.
unregister(Operation) ->
    case catena_handler_stack:current() of
        undefined -> {error, no_current_scope};
        _ -> catena_handler_stack:unregister_in_current(Operation)
    end.

%% @doc Look up a handler for an operation.
%%
%% Searches through the handler stack from current scope upward.
-spec lookup(atom()) -> {ok, handler()} | {error, not_found}.
lookup(Operation) ->
    catena_handler_stack:lookup(Operation).

%%====================================================================
%% Handler Stack Management
%%====================================================================

%%====================================================================
%% Handler Stack Management
%%====================================================================

%% @doc Push a new handler scope onto the stack.
%%
%% This creates a new empty registry for handlers in the nested scope.
-spec push_handlers(handler_registry()) -> ok.
push_handlers(Registry) when is_map(Registry) ->
    catena_handler_stack:push(Registry).

%% @doc Pop the current handler scope from the stack.
%%
%% Returns the popped registry.
-spec pop_handlers() -> {ok, handler_registry()} | {error, empty}.
pop_handlers() ->
    catena_handler_stack:pop().

%% @doc Get the current handler stack.
-spec current_stack() -> handler_stack().
current_stack() ->
    catena_handler_stack:to_list().

%% @doc Get the current stack depth.
-spec stack_depth() -> non_neg_integer().
stack_depth() ->
    catena_handler_stack:depth().

%%====================================================================
%% Handler Execution
%%====================================================================

%% @doc Execute a handler with an operation value and resumption.
%%
%% This invokes the handler function with the operation value and
%% resumption, returning the result or an error.
-spec execute(handler(), term(), catena_resumption:resumption()) -> term().
execute(#handler{handler_fn = {Fn, _}}, OpValue, Resumption) ->
    try
        Fn(OpValue, Resumption)
    catch
        Kind:Reason:Stack ->
            {handler_error, Kind, Reason, Stack}
    end.

%% @doc Execute a handler with a timeout.
%%
%% If the handler doesn't complete within the timeout, returns
%% {timeout, OpValue}.
-spec execute_with_timeout(handler(), term(), catena_resumption:resumption(), timeout()) ->
    term() | {timeout, term()}.
execute_with_timeout(Handler, OpValue, Resumption, Timeout) ->
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
        Result = execute(Handler, OpValue, Resumption),
        Parent ! {handler_result, Result}
    end),
    receive
        {handler_result, Result} ->
            demonitor(Ref, [flush]),
            Result;
        {'DOWN', Ref, process, Pid, Reason} ->
            {error, Reason}
    after Timeout ->
        demonitor(Ref, [flush]),
        exit(Pid, kill),
        {timeout, OpValue}
    end.

