%%%-------------------------------------------------------------------
%%% @doc Catena Handler Depth Semantics (Phase 9.1)
%%%
%%% This module defines the semantics and types for deep vs shallow
%%% handlers, enabling precise control over which operations are
%%% handled at which scope levels.
%%%
%%% == Handler Depth ==
%%%
%%% Handlers can be either deep or shallow:
%%%
%%% - **Deep handlers**: Handle operations at all nested scope levels.
%%%   An operation performed deep within nested function calls will
%%%   be caught by the nearest deep handler on the stack.
%%%
%%% - **Shallow handlers**: Handle only operations at their direct
%%%   scope level. Operations performed in nested function calls
%%%   bypass shallow handlers and propagate upward.
%%%
%%% == Semantics ==
%%%
%%% Deep handlers are the default and provide the expected algebraic
%%% effects behavior similar to handlers in Koka, Eff, or other
%%% effect systems.
%%%
%%% Shallow handlers are useful for:
%%% - Implementing scoped effects that shouldn't leak
%%% - Creating effect boundaries
%%% - Performance optimization (limited scope traversal)
%%% - Compositional effect handling
%%%
%%% == Examples ==
%%%
%%% ```erlang
%%% %% Deep handler (default) - catches nested operations
%%% with_deep_handler(State, fun() ->
%%%     nested_function()  % State operations here are caught
%%% end).
%%%
%%% %% Shallow handler - only catches direct operations
%%% with_shallow_handler(State, fun() ->
%%%     nested_function()  % State operations here propagate up
%%% end).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_handler_depth).

%% Depth type and constructors
-export([
    depth/0,
    deep/0,
    shallow/0,
    is_deep/1,
    is_shallow/1
]).

%% Depth validation and conversion
-export([
    validate_depth/1,
    to_deep/1,
    to_shallow/1,
    invert_depth/1
]).

%% Depth semantics documentation
-export([
    depth_description/1,
    scope_behavior/1,
    performance_implications/1,
    use_cases/1
]).

%%====================================================================
%% Types
%%====================================================================

-type handler_depth() :: deep | shallow.
-type depth_result() :: {ok, handler_depth()} | {error, term()}.

-export_type([
    handler_depth/0
]).

%%====================================================================
%% Depth Type and Constructors
%%====================================================================

%% @doc The default handler depth.
%% Returns the depth type used when no explicit depth is specified.
-spec depth() -> handler_depth().
depth() -> deep.

%% @doc Create a deep handler depth specification.
-spec deep() -> handler_depth().
deep() -> deep.

%% @doc Create a shallow handler depth specification.
-spec shallow() -> handler_depth().
shallow() -> shallow.

%% @doc Check if a depth specification is deep.
-spec is_deep(handler_depth() | term()) -> boolean().
is_deep(deep) -> true;
is_deep(_) -> false.

%% @doc Check if a depth specification is shallow.
-spec is_shallow(handler_depth() | term()) -> boolean().
is_shallow(shallow) -> true;
is_shallow(_) -> false.

%%====================================================================
%% Depth Validation and Conversion
%%====================================================================

%% @doc Validate a depth specification.
%% Returns ok if valid, error otherwise.
-spec validate_depth(term()) -> depth_result().
validate_depth(deep) -> {ok, deep};
validate_depth(shallow) -> {ok, shallow};
validate_depth(Other) -> {error, {invalid_depth, Other}}.

%% @doc Convert any depth to deep.
%% If already deep, returns deep. If shallow, converts to deep.
-spec to_deep(handler_depth()) -> handler_depth().
to_deep(deep) -> deep;
to_deep(shallow) -> deep.

%% @doc Convert any depth to shallow.
%% If already shallow, returns shallow. If deep, converts to shallow.
-spec to_shallow(handler_depth()) -> handler_depth().
to_shallow(shallow) -> shallow;
to_shallow(deep) -> shallow.

%% @doc Invert a depth specification.
%% Deep becomes shallow, shallow becomes deep.
-spec invert_depth(handler_depth()) -> handler_depth().
invert_depth(deep) -> shallow;
invert_depth(shallow) -> deep.

%%====================================================================
%% Depth Semantics Documentation
%%====================================================================

%% @doc Get a description of a handler depth.
-spec depth_description(handler_depth()) -> string().
depth_description(deep) ->
    "Deep handlers catch operations at all nested scope levels. "
    "When an operation is performed within a deep handler's scope, "
    "including nested function calls, it will be handled by the nearest "
    "deep handler on the stack. This is the default and expected behavior "
    "for algebraic effects.";
depth_description(shallow) ->
    "Shallow handlers only catch operations at their direct scope level. "
    "Operations performed in nested function calls will bypass shallow "
    "handlers and propagate upward to find a handler. This is useful for "
    "creating effect boundaries and limiting handler scope."

.

%% @doc Get the scope behavior description for a depth.
-spec scope_behavior(handler_depth()) -> string().
scope_behavior(deep) ->
    "Operations at any nesting level within the handler's scope are handled. "
    "The handler will traverse the entire call stack to find operations.";
scope_behavior(shallow) ->
    "Only operations directly within the handler's scope are handled. "
    "Operations in nested function calls bypass this handler."

.

%% @doc Get performance implications for a depth.
-spec performance_implications(handler_depth()) -> string().
performance_implications(deep) ->
    "Deep handlers require stack traversal to find nested operations, "
    "which has O(n) complexity where n is stack depth. However, this "
    "is the expected behavior for most effect handlers and optimizations "
    "like handler caching can mitigate costs.";
performance_implications(shallow) ->
    "Shallow handlers have O(1) operation lookup since they only check "
    "the current scope. This can provide significant performance benefits "
    "when handlers are used for scoped effects that shouldn't propagate."

.

%% @doc Get common use cases for a depth.
-spec use_cases(handler_depth()) -> [string()].
use_cases(deep) ->
    [
        "Standard effect handlers (State, Reader, Writer, Error)",
        "Exception handling and error propagation",
        "Resource management (acquire/release patterns)",
        "Logging and monitoring across call boundaries",
        "Default behavior for most algebraic effects"
    ];
use_cases(shallow) ->
    [
        "Scoped resource cleanup",
        "Effect boundaries and sandboxes",
        "Performance-critical scoped effects",
        "Compositional effect handling",
        "Preventing effect leakage from subcomputations"
    ].
