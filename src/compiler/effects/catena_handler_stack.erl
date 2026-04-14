%%%-------------------------------------------------------------------
%%% @doc Catena Handler Stack Management (Phase 7.2)
%%%
%%% This module manages the stack of handler registries for nested effect
%%% handling. When handlers are nested (e.g., outer handler for IO, inner
%%% handler for State), the stack maintains the proper scope and lookup
%%% order.
%%%
%%% == Handler Stack ==
%%%
%%% The handler stack is a list of handler registries, where each registry
%%% maps operations to handlers in a particular scope. The top of the stack
%%% (head of the list) is the current scope.
%%%
%%% == Scope Management ==
%%%
%%% - push/1: Create a new scope with empty or provided handlers
%%% - pop/0: Exit the current scope, returning to the parent scope
%%% - lookup/1: Look up a handler, searching from current scope upward
%%%
%%% == Handler Shadowing ==
%%%
%%% Inner scopes can shadow handlers from outer scopes. When looking up
%%% a handler, the current scope is checked first, then parent scopes.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_handler_stack).

%% Stack manipulation
-export([
    new/0,
    new/1,
    push/0,
    push/1,
    pop/0,
    pop/1
]).

%% Stack inspection
-export([
    current/0,
    depth/0,
    is_empty/0,
    to_list/0
]).

%% Handler lookup across stack
-export([
    lookup/1,
    lookup_in_current/1,
    lookup_all/1
]).

%% Handler registry operations
-export([
    register_in_current/2,
    unregister_in_current/1
]).

%%====================================================================
%% Types
%%====================================================================

-type handler() :: catena_handler:handler().
-type handler_registry() :: #{atom() => handler()}.
-type handler_stack() :: [handler_registry()].

-export_type([]).

%%====================================================================
%% Stack Creation
%%====================================================================

%% @doc Create a new empty handler stack.
-spec new() -> handler_stack().
new() ->
    [].

%% @doc Create a new handler stack with an initial registry.
-spec new(handler_registry()) -> handler_stack().
new(Registry) when is_map(Registry) ->
    [Registry].

%%====================================================================
%% Stack Manipulation
%%====================================================================

%% @doc Push an empty registry onto the stack (enter a new scope).
-spec push() -> ok.
push() ->
    push(#{}).

%% @doc Push a registry onto the stack (enter a new scope with handlers).
-spec push(handler_registry()) -> ok.
push(Registry) when is_map(Registry) ->
    Stack = get_stack(),
    put_stack([Registry | Stack]),
    ok.

%% @doc Pop the top registry from the stack (exit current scope).
-spec pop() -> {ok, handler_registry()} | {error, empty}.
pop() ->
    Stack = get_stack(),
    case Stack of
        [] -> {error, empty};
        [Top | Rest] ->
            put_stack(Rest),
            {ok, Top}
    end.

%% @doc Pop with a default value if stack is empty.
-spec pop(handler_registry()) -> handler_stack().
pop(Default) ->
    case pop() of
        {ok, Top} -> Top;
        {error, empty} -> Default
    end.

%%====================================================================
%% Stack Inspection
%%====================================================================

%% @doc Get the current (top) registry from the stack.
-spec current() -> handler_registry() | undefined.
current() ->
    Stack = get_stack(),
    case Stack of
        [] -> undefined;
        [Top | _] -> Top
    end.

%% @doc Get the depth of the stack.
-spec depth() -> non_neg_integer().
depth() ->
    length(get_stack()).

%% @doc Check if the stack is empty.
-spec is_empty() -> boolean().
is_empty() ->
    get_stack() =:= [].

%% @doc Convert the stack to a list (top to bottom).
-spec to_list() -> handler_stack().
to_list() ->
    get_stack().

%%====================================================================
%% Handler Lookup Across Stack
%%====================================================================

%% @doc Look up a handler, searching from current scope upward.
-spec lookup(atom()) -> {ok, handler()} | {error, not_found}.
lookup(Operation) ->
    Stack = get_stack(),
    lookup_in_stack(Operation, Stack).

%% @doc Look up a handler in the current scope only.
-spec lookup_in_current(atom()) -> {ok, handler()} | {error, not_found}.
lookup_in_current(Operation) ->
    case current() of
        undefined -> {error, not_found};
        Registry ->
            case maps:find(Operation, Registry) of
                {ok, Handler} -> {ok, Handler};
                error -> {error, not_found}
            end
    end.

%% @doc Look up all handlers for an operation across all scopes.
-spec lookup_all(atom()) -> [handler()].
lookup_all(Operation) ->
    Stack = get_stack(),
    lookup_in_stack_all(Operation, Stack, []).

%%====================================================================
%% Handler Registry Operations
%%====================================================================

%% @doc Register a handler in the current scope.
-spec register_in_current(atom(), handler()) -> ok.
register_in_current(Operation, Handler) ->
    Stack = get_stack(),
    case Stack of
        [] -> {error, no_current_scope};
        [Current | Rest] ->
            NewRegistry = Current#{Operation => Handler},
            put_stack([NewRegistry | Rest]),
            ok
    end.

%% @doc Unregister a handler from the current scope.
-spec unregister_in_current(atom()) -> ok.
unregister_in_current(Operation) ->
    Stack = get_stack(),
    case Stack of
        [] -> {error, no_current_scope};
        [Current | Rest] ->
            NewRegistry = maps:remove(Operation, Current),
            put_stack([NewRegistry | Rest]),
            ok
    end.

%%====================================================================
%% Private Functions
%%====================================================================

%% @private Get the handler stack from process dictionary.
-spec get_stack() -> handler_stack().
get_stack() ->
    case get(catena_handler_stack) of
        undefined -> [];
        Stack -> Stack
    end.

%% @private Put the handler stack into the process dictionary.
-spec put_stack(handler_stack()) -> ok.
put_stack(Stack) ->
    put(catena_handler_stack, Stack),
    ok.

%% @private Look up a handler in a stack of registries.
-spec lookup_in_stack(atom(), handler_stack()) -> {ok, handler()} | {error, not_found}.
lookup_in_stack(_Operation, []) ->
    {error, not_found};
lookup_in_stack(Operation, [Registry | Rest]) ->
    case maps:find(Operation, Registry) of
        {ok, Handler} -> {ok, Handler};
        error -> lookup_in_stack(Operation, Rest)
    end.

%% @private Look up all handlers for an operation across all scopes.
-spec lookup_in_stack_all(atom(), handler_stack(), [handler()]) -> [handler()].
lookup_in_stack_all(_Operation, [], Acc) ->
    lists:reverse(Acc);
lookup_in_stack_all(Operation, [Registry | Rest], Acc) ->
    NewAcc = case maps:find(Operation, Registry) of
        {ok, Handler} -> [Handler | Acc];
        error -> Acc
    end,
    lookup_in_stack_all(Operation, Rest, NewAcc).
