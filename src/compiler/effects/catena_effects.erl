%%%-------------------------------------------------------------------
%%% @doc Catena Unified Effect Library (Phase 14.1)
%%%
%%% This module provides a unified API for the complete algebraic effects
%%% system, integrating all components from Phases 7-13. It serves as the
%%% main user-facing API for effectful programming in Catena.
%%%
%%% == Architecture ==
%%%
%%% This module now orchestrates the complete effect system:
%%%
%%% - **catena_effect_system**: Central orchestration and lifecycle
%%% - **catena_handler**: Handler type and execution (Phase 7)
%%% - **catena_resumption**: Continuation capture and resumption (Phase 7)
%%% - **catena_equations**: Algebraic laws and optimization (Phase 8)
%%% - **catena_deep_handler**: Deep handler semantics (Phase 9)
%%% - **catena_shallow_handler**: Shallow handler semantics (Phase 9)
%%% - **catena_one_shot**: One-shot continuation semantics (Phase 10)
%%% - **catena_multi_shot**: Multi-shot continuation semantics (Phase 10)
%%% - **catena_row_types**: Row polymorphism for effect sets (Phase 11)
%%% - **catena_handler_infer**: Typed handler inference (Phase 12)
%%% - **catena_op_signatures**: Operation signature polymorphism (Phase 13)
%%% - **catena_hefty**: Higher-order effects and hefty algebras (Phase 13)
%%%
%%% == Effect Library ==
%%%
%%% Standard effects with implementations:
%%%
%%% - **State**: Mutable state without side effects
%%% - **Reader**: Read-only environment access
%%% - **Writer**: Monoid output accumulation
%%% - **Error**: Typed exceptions
%%% - **Async**: Asynchronous computations
%%%
%%% == Unified API ==
%%%
%%% The API provides both low-level and high-level interfaces:
%%%
%%% - **Low-level**: `perform/2`, `handle/3` for direct effect operations
%%% - **High-level**: `run_state/2`, `run_reader/2` for concrete effects
%%% - **System-level**: `init/0`, `configure/1` for system configuration
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effects).

%%====================================================================
%% System Initialization and Configuration
%%====================================================================

-export([
    init/0,
    init/1,
    shutdown/0,
    configure/1,
    is_initialized/0
]).

%%====================================================================
%% Effect Registration
%%====================================================================

-export([
    register_effect/2,
    register_effect/3,
    unregister_effect/1,
    list_effects/0
]).

%%====================================================================
%% Low-Level Effect Operations
%%====================================================================

-export([
    perform/2,
    perform/3,
    try_perform/2,
    handle/3,
    handle/4,
    handle_deep/3,
    handle_shallow/3
]).

%%====================================================================
%% Handler Management
%%====================================================================

-export([
    register_handler/2,
    register_handler/3,
    unregister_handler/1,
    lookup_handler/1,
    current_handlers/0
]).

%%====================================================================
%% Continuation Management
%%====================================================================

-export([
    capture_resumption/0,
    resume/2,
    abort/1,
    is_one_shot/0,
    is_multi_shot/0
]).

%%====================================================================
%% Effect Optimization
%%====================================================================

-export([
    optimize/1,
    optimize/2,
    add_equation/2,
    apply_equations/1
]).

%%====================================================================
%% Built-in Effect: State
%%====================================================================

-export([
    %% State operations
    state_get/0,
    state_put/1,
    state_modify/1,
    state_get_and_put/1,
    %% State runners
    run_state/2,
    run_state/3,
    eval_state/2
]).

%%====================================================================
%% Built-in Effect: Reader
%%====================================================================

-export([
    %% Reader operations
    reader_ask/0,
    reader_local/1,
    reader_ask_local/1,
    %% Reader runners
    run_reader/2,
    ask/1
]).

%%====================================================================
%% Built-in Effect: Writer
%%====================================================================

-export([
    %% Writer operations
    writer_tell/1,
    writer_listen/1,
    writer_pass/1,
    %% Writer runners
    run_writer/1,
    run_writer/2,
    tell/1
]).

%%====================================================================
%% Built-in Effect: Error
%%====================================================================

-export([
    %% Error operations
    error_throw/1,
    error_catch/2,
    %% Error runners
    run_error/2,
    throw/1,
    catch_error/2
]).

%%====================================================================
%% Built-in Effect: Async
%%====================================================================

-export([
    %% Async operations
    async_spawn/1,
    async_await/1,
    async_yield/1,
    %% Async runners
    run_async/0,
    run_async/1
]).

%%====================================================================
%% Type System Integration
%%====================================================================

-export([
    infer_effect_type/1,
    check_effect_type/2,
    generalize_effects/1
]).

%%====================================================================
%% Diagnostics and Debugging
%%====================================================================

-export([
    diagnostics/0,
    stats/0,
    handler_stack/0,
    trace/1,
    untrace/0
]).

%%====================================================================
%% Types
%%====================================================================

-type effect_name() :: atom().
-type operation_name() :: atom().
-type operation_value() :: term().
-type effect_result() :: term().
-type handler_fn() :: function().
-type computation() :: function().
-type state() :: term().
-type reader_env() :: term().
-type writer_monoid() :: term().

-type effect() ::
    {state, state()} |
    {reader, reader_env()} |
    {writer, writer_monoid()} |
    {error, term()} |
    {async}.

-type handler_options() :: #{
    deep => boolean(),
    one_shot => boolean(),
    metadata => map()
}.

-type optimization_level() :: 0..3.

-type system_config() :: #{
    optimization_level => optimization_level(),
    enable_equations => boolean(),
    enable_hefty => boolean(),
    trace => boolean()
}.

%% Deprecated types (for backward compatibility)
-type effect_operation() :: {effect, {atom(), term()}}.
-export_type([effect/0, effect_operation/0]).

%%====================================================================
%% Internal Constants
%%====================================================================

-define(EFFECT_SYSTEM, catena_effect_system).

%%====================================================================
%% System Initialization and Configuration
%%====================================================================

%% @doc Initialize the effect system with default configuration.
%%
%% This must be called before using any effect operations. It sets up
%% the handler stack, effect registry, and equation system.
%%
%% @example
%% ```erlang
%% catena_effects:init(),
%% ... effect operations ...
%% catena_effects:shutdown().
%% '''
-spec init() -> ok.
init() ->
    ?EFFECT_SYSTEM:init(),
    register_builtin_effects(),
    ok.

%% @doc Initialize the effect system with custom configuration.
%%
%% @param Config Map of configuration options
%%
%% @example
%% ```erlang
%% catena_effects:init(#{
%%     optimization_level => 3,
%%     enable_equations => true,
%%     trace => false
%% }).
%% '''
-spec init(system_config()) -> ok.
init(Config) when is_map(Config) ->
    Options = config_to_options(Config),
    ?EFFECT_SYSTEM:init(Options),
    register_builtin_effects(),
    ok.

%% @doc Shutdown the effect system and release resources.
%%
%% Resets handler stacks, clears registries, and cleans up any
%% captured continuations.
-spec shutdown() -> ok.
shutdown() ->
    ?EFFECT_SYSTEM:shutdown().

%% @doc Configure the effect system after initialization.
-spec configure(system_config()) -> ok.
configure(Config) when is_map(Config) ->
    Options = config_to_options(Config),
    ?EFFECT_SYSTEM:configure(Options).

%% @doc Check if the effect system is initialized.
-spec is_initialized() -> boolean().
is_initialized() ->
    ?EFFECT_SYSTEM:is_initialized().

%% @private Convert config map to system options.
config_to_options(Config) ->
    maps:fold(fun(Key, Value, Acc) ->
        case Key of
            optimization_level -> [{optimization_level, Value} | Acc];
            enable_equations -> [{enable_equations, Value} | Acc];
            enable_hefty -> [{enable_hefty, Value} | Acc];
            enable_row_poly -> [{enable_row_poly, Value} | Acc];
            trace_operations -> [{trace_operations, Value} | Acc];
            _ -> Acc
        end
    end, [], Config).

%% @private Register all built-in effects.
register_builtin_effects() ->
    %% State effect
    ?EFFECT_SYSTEM:register_effect(state, [get, put, modify, get_and_put]),
    %% Reader effect
    ?EFFECT_SYSTEM:register_effect(reader, [ask, local, ask_local]),
    %% Writer effect
    ?EFFECT_SYSTEM:register_effect(writer, [tell, listen, pass]),
    %% Error effect
    ?EFFECT_SYSTEM:register_effect(error, [throw, 'catch']),
    %% Async effect
    ?EFFECT_SYSTEM:register_effect(async, [spawn, await, yield]),
    ok.

%%====================================================================
%% Effect Registration
%%====================================================================

%% @doc Register a custom effect with its operations.
%%
%% @param Name Name of the effect
%% @param Operations List of operation atoms
%%
%% @example
%% ```erlang
%% catena_effects:register_effect(logging, [log, debug, info]).
%% '''
-spec register_effect(effect_name(), [operation_name()]) -> ok.
register_effect(Name, Operations) ->
    ?EFFECT_SYSTEM:register_effect(Name, Operations).

%% @doc Register a custom effect with metadata.
-spec register_effect(effect_name(), [operation_name()], map()) -> ok.
register_effect(Name, Operations, Metadata) ->
    ?EFFECT_SYSTEM:register_effect(Name, Operations, Metadata).

%% @doc Unregister an effect.
-spec unregister_effect(effect_name()) -> ok.
unregister_effect(Name) ->
    ?EFFECT_SYSTEM:unregister_effect(Name).

%% @doc List all registered effects.
-spec list_effects() -> [effect_name()].
list_effects() ->
    ?EFFECT_SYSTEM:list_effects().

%%====================================================================
%% Low-Level Effect Operations
%%====================================================================

%% @doc Perform an effect operation with a value.
%%
%% This is the core operation for effectful programming. When called,
%% it captures the current continuation and looks up a handler for
%% the operation.
%%
%% @param Operation The effect operation to perform
%% @param Value The value to pass to the handler
%%
%% @example
%% ```erlang
%% %% Perform a state get operation
%% CurrentState = catena_effects:perform(state, get).
%%
%% %% Perform a state put operation
%% catena_effects:perform(state, {put, NewState}).
%% '''
-spec perform(operation_name(), operation_value()) -> effect_result().
perform(Operation, Value) ->
    ?EFFECT_SYSTEM:perform(Operation, Value).

%% @doc Perform an effect operation with options.
-spec perform(operation_name(), operation_value(), list()) -> effect_result().
perform(Operation, Value, Options) ->
    ?EFFECT_SYSTEM:perform(Operation, Value, Options).

%% @doc Try to perform an effect operation, returning error on failure.
-spec try_perform(operation_name(), operation_value()) ->
    {ok, effect_result()} | {error, term()}.
try_perform(Operation, Value) ->
    ?EFFECT_SYSTEM:try_perform(Operation, Value).

%% @doc Handle a computation with a handler (default: shallow, multi-shot).
%%
%% @param Operation The operation to handle
%% @param HandlerFn Handler function (arity 2: value and resumption)
%% @param Computation The effectful computation to run
%%
%% @example
%% ```erlang
%% catena_effects:handle(state,
%%     fun({get, S}, Res) -> Res(S) end,
%%     fun() -> ... end
%% ).
%% '''
-spec handle(operation_name(), handler_fn(), computation()) -> term().
handle(Operation, HandlerFn, Computation) ->
    handle(Operation, HandlerFn, Computation, #{}).

%% @doc Handle a computation with a handler and options.
%%
%% Options:
%% - deep: Use deep handler semantics (default: false)
%% - one_shot: Use one-shot continuations (default: false)
%%
%% @example
%% ```erlang
%% %% Deep handler with one-shot continuations
%% catena_effects:handle(state, Handler, Computation, #{
%%     deep => true,
%%     one_shot => true
%% }).
%% '''
-spec handle(operation_name(), handler_fn(), computation(), handler_options()) -> term().
handle(Operation, HandlerFn, Computation, Options) when is_function(HandlerFn, 2), is_function(Computation, 0) ->
    %% Use appropriate handler type based on options
    UseDeep = maps:get(deep, Options, false),
    UseOneShot = maps:get(one_shot, Options, false),

    %% Apply continuation kind wrapper if needed
    WrappedHandler = case {UseDeep, UseOneShot} of
        {true, true} -> wrap_deep_one_shot(HandlerFn);
        {true, false} -> wrap_deep_multi_shot(HandlerFn);
        {false, true} -> wrap_shallow_one_shot(HandlerFn);
        {false, false} -> HandlerFn  %% Default: shallow, multi-shot
    end,

    ?EFFECT_SYSTEM:handle_with(Operation, WrappedHandler, Computation, maps:get(metadata, Options, #{})).

%% @doc Handle with deep handler semantics.
%%
%% Deep handlers handle operations in the entire continuation,
%% including nested handlers.
-spec handle_deep(operation_name(), handler_fn(), computation()) -> term().
handle_deep(Operation, HandlerFn, Computation) ->
    handle(Operation, HandlerFn, Computation, #{deep => true}).

%% @doc Handle with shallow handler semantics.
%%
%% Shallow handlers only handle operations directly in their scope,
%% leaving nested operations to their own handlers.
-spec handle_shallow(operation_name(), handler_fn(), computation()) -> term().
handle_shallow(Operation, HandlerFn, Computation) ->
    handle(Operation, HandlerFn, Computation, #{deep => false}).

%%====================================================================
%% Handler Management
%%====================================================================

%% @doc Register a handler for an operation in the current scope.
-spec register_handler(operation_name(), handler_fn()) -> ok.
register_handler(Operation, HandlerFn) when is_function(HandlerFn, 2) ->
    ?EFFECT_SYSTEM:register_handler(Operation, HandlerFn).

%% @doc Register a handler with metadata.
-spec register_handler(operation_name(), handler_fn(), map()) -> ok.
register_handler(Operation, HandlerFn, Metadata) when is_function(HandlerFn, 2) ->
    ?EFFECT_SYSTEM:register_handler(Operation, HandlerFn, Metadata).

%% @doc Unregister a handler for an operation.
-spec unregister_handler(operation_name()) -> ok.
unregister_handler(Operation) ->
    ?EFFECT_SYSTEM:unregister_handler(Operation).

%% @doc Look up a handler for an operation.
-spec lookup_handler(operation_name()) -> {ok, term()} | {error, not_found}.
lookup_handler(Operation) ->
    ?EFFECT_SYSTEM:lookup_handler(Operation).

%% @doc Get all handlers in the current scope.
-spec current_handlers() -> #{operation_name() => term()}.
current_handlers() ->
    ?EFFECT_SYSTEM:current_handlers().

%%====================================================================
%% Continuation Management
%%====================================================================

%% @doc Capture the current continuation as a resumption.
-spec capture_resumption() -> term().
capture_resumption() ->
    catena_resumption:capture_continuation().

%% @doc Resume a computation with a value.
-spec resume(term(), term()) -> term().
resume(Resumption, Value) ->
    catena_resumption:resume(Resumption, Value).

%% @doc Abort the computation with a value.
-spec abort(term()) -> no_return().
abort(Value) ->
    exit({normal_abort, Value}).

%% @doc Check if the current continuation is one-shot.
-spec is_one_shot() -> boolean().
is_one_shot() ->
    catena_continuation_kind:is_one_shot().

%% @doc Check if the current continuation is multi-shot.
-spec is_multi_shot() -> boolean().
is_multi_shot() ->
    catena_continuation_kind:is_multi_shot().

%%====================================================================
%% Effect Optimization
%%====================================================================

%% @doc Optimize an effectful program using default settings.
-spec optimize(term()) -> term().
optimize(Program) ->
    ?EFFECT_SYSTEM:optimize_program(Program).

%% @doc Optimize with a specific optimization level (0-3).
-spec optimize(term(), optimization_level()) -> term().
optimize(Program, Level) ->
    configure(#{optimization_level => Level}),
    ?EFFECT_SYSTEM:optimize_program(Program).

%% @doc Add an equation for operation-based optimization.
-spec add_equation(operation_name(), term()) -> ok.
add_equation(Operation, Equation) ->
    ?EFFECT_SYSTEM:add_equation(Operation, Equation).

%% @doc Apply equation-based rewrites to an expression.
-spec apply_equations(term()) -> term().
apply_equations(Expr) ->
    ?EFFECT_SYSTEM:apply_equations(Expr).

%%====================================================================
%% Built-in Effect: State
%%====================================================================

%% @doc Get the current state value.
-spec state_get() -> term().
state_get() ->
    perform(state, get).

%% @doc Set the state to a new value.
-spec state_put(term()) -> term().
state_put(Value) ->
    perform(state, {put, Value}).

%% @doc Modify the state using a function.
-spec state_modify(function()) -> term().
state_modify(Fun) when is_function(Fun, 1) ->
    perform(state, {modify, Fun}).

%% @doc Get the current state and put a new value atomically.
-spec state_get_and_put(function()) -> term().
state_get_and_put(Fun) when is_function(Fun, 1) ->
    perform(state, {get_and_put, Fun}).

%% @doc Run a stateful computation with an initial state.
%%
%% Returns the final result and the final state.
%%
%% @example
%% ```erlang
%% {Result, FinalState} = catena_effects:run_state(
%%     fun() ->
%%         S = state_get(),
%%         state_put(S * 2),
%%         state_get()
%%     end,
%%     5
%% ),
%% %% Result = 10, FinalState = 10
%% '''
-spec run_state(computation(), state()) -> {term(), state()}.
run_state(Computation, InitialState) ->
    run_state(Computation, InitialState, #{}).

%% @doc Run a stateful computation with options.
-spec run_state(computation(), state(), handler_options()) -> {term(), state()}.
run_state(Computation, InitialState, Options) ->
    Ref = make_ref(),
    Handler = fun
        (get, Resume) ->
            Initial = get({state_ref, Ref}),
            Resume(Initial);
        ({put, Value}, _Resume) ->
            put({state_ref, Ref}, Value),
            Value;
        ({modify, Fun}, _Resume) ->
            Current = get({state_ref, Ref}),
            NewState = Fun(Current),
            put({state_ref, Ref}, NewState),
            NewState;
        ({get_and_put, Fun}, Resume) ->
            Current = get({state_ref, Ref}),
            NewState = Fun(Current),
            put({state_ref, Ref}, NewState),
            Resume(Current)
    end,
    put({state_ref, Ref}, InitialState),
    try
        Result = handle(state, Handler, Computation, Options),
        FinalState = erase({state_ref, Ref}),
        {Result, FinalState}
    catch
        _:Error ->
            erase({state_ref, Ref}),
            error(Error)
    end.

%% @doc Run a stateful computation, returning only the result.
-spec eval_state(computation(), state()) -> term().
eval_state(Computation, InitialState) ->
    {Result, _} = run_state(Computation, InitialState),
    Result.

%%====================================================================
%% Built-in Effect: Reader
%%====================================================================

%% @doc Ask for the reader environment value.
-spec reader_ask() -> term().
reader_ask() ->
    perform(reader, ask).

%% @doc Run a computation with a modified local environment.
-spec reader_local(computation()) -> term().
reader_local(Computation) when is_function(Computation, 0) ->
    perform(reader, {local, Computation}).

%% @doc Ask for the environment and run a function with it.
-spec reader_ask_local(function()) -> term().
reader_ask_local(Fun) when is_function(Fun, 1) ->
    perform(reader, {ask_local, Fun}).

%% @doc Run a reader computation with an environment.
-spec run_reader(computation(), reader_env()) -> term().
run_reader(Computation, Env) ->
    run_reader(Computation, Env, #{}).

%% @doc Run a reader computation with options.
-spec run_reader(computation(), reader_env(), handler_options()) -> term().
run_reader(Computation, Env, Options) ->
    Ref = make_ref(),
    Handler = fun
        (ask, Resume) ->
            Value = get({reader_ref, Ref}),
            Resume(Value);
        ({local, LocalComp}, _Resume) ->
            CurrentEnv = get({reader_ref, Ref}),
            put({reader_ref, Ref}, CurrentEnv),
            Result = LocalComp(),
            put({reader_ref, Ref}, CurrentEnv),
            Result;
        ({ask_local, Fun}, Resume) ->
            Value = get({reader_ref, Ref}),
            Resume(Fun(Value))
    end,
    put({reader_ref, Ref}, Env),
    try
        handle(reader, Handler, Computation, Options)
    after
        erase({reader_ref, Ref})
    end.

%% @doc Convenience alias for reader_ask.
-spec ask(reader_env()) -> term().
ask(Env) ->
    reader_ask(),
    Env.

%%====================================================================
%% Built-in Effect: Writer
%%====================================================================

%% @doc Tell a value to the writer monoid.
-spec writer_tell(term()) -> term().
writer_tell(Value) ->
    perform(writer, {tell, Value}).

%% @doc Listen to the output during a computation.
-spec writer_listen(computation()) -> term().
writer_listen(Computation) when is_function(Computation, 0) ->
    perform(writer, {listen, Computation}).

%% @doc Pass through without accumulating output.
-spec writer_pass(term()) -> term().
writer_pass(Value) ->
    perform(writer, {pass, Value}).

%% @doc Run a writer computation, collecting output in a list.
-spec run_writer(computation()) -> {term(), [term()]}.
run_writer(Computation) ->
    run_writer(Computation, #{}).

%% @doc Run a writer computation with options.
-spec run_writer(computation(), handler_options()) -> {term(), [term()]}.
run_writer(Computation, Options) ->
    Ref = make_ref(),
    Handler = fun
        ({tell, Value}, Resume) ->
            Output = get({writer_ref, Ref}),
            put({writer_ref, Ref}, [Value | Output]),
            Resume(ok);
        ({listen, InnerComp}, Resume) ->
            CurrentOutput = get({writer_ref, Ref}),
            put({writer_ref, Ref}, []),
            Result = InnerComp(),
            InnerOutput = lists:reverse(get({writer_ref, Ref})),
            put({writer_ref, Ref}, CurrentOutput),
            Resume({Result, InnerOutput});
        ({pass, Value}, Resume) ->
            Resume(Value)
    end,
    put({writer_ref, Ref}, []),
    try
        Result = handle(writer, Handler, Computation, Options),
        Output = lists:reverse(get({writer_ref, Ref})),
        {Result, Output}
    after
        erase({writer_ref, Ref})
    end.

%% @doc Convenience alias for writer_tell.
-spec tell(term()) -> term().
tell(Value) ->
    writer_tell(Value).

%%====================================================================
%% Built-in Effect: Error
%%====================================================================

%% @doc Throw an error effect.
-spec error_throw(term()) -> no_return().
error_throw(Error) ->
    perform(error, {throw, Error}),
    error(unreachable).

%% @doc Catch an error with a handler.
-spec error_catch(computation(), function()) -> term().
error_catch(Computation, Handler) when is_function(Computation, 0), is_function(Handler, 1) ->
    perform(error, {'catch', Computation, Handler}).

%% @doc Run an error-handling computation.
%%
%% Returns the result or calls the error handler with the error value.
%%
%% @example
%% ```erlang
%% catena_effects:run_error(
%%     fun() ->
%%         ...,
%%         error_throw(some_error)
%%     end,
%%     fun(Error) -> {error, Error} end
%% ).
%% '''
-spec run_error(computation(), function()) -> term().
run_error(Computation, ErrorHandler) ->
    run_error(Computation, ErrorHandler, #{}).

%% @doc Run an error-handling computation with options.
-spec run_error(computation(), function(), handler_options()) -> term().
run_error(Computation, ErrorHandler, Options) ->
    Handler = fun
        ({throw, Error}, _Resume) ->
            ErrorHandler(Error);
        ({'catch', InnerComp, _InnerHandler}, Resume) ->
            try InnerComp() of
                Result -> Resume(Result)
            catch
                _:_ -> error_handler_failed
            end
    end,
    handle(error, Handler, fun() ->
        try Computation()
        catch
            {normal_abort, _} = Abort -> exit(Abort)
        end
    end, Options).

%% @doc Convenience alias for error_throw.
-spec throw(term()) -> no_return().
throw(Error) ->
    error_throw(Error).

%% @doc Convenience alias for run_error.
-spec catch_error(computation(), function()) -> term().
catch_error(Computation, ErrorHandler) ->
    run_error(Computation, ErrorHandler).

%%====================================================================
%% Built-in Effect: Async
%%====================================================================

%% @doc Spawn an asynchronous computation.
-spec async_spawn(computation()) -> term().
async_spawn(Computation) when is_function(Computation, 0) ->
    perform(async, {spawn, Computation}).

%% @doc Await an async computation result.
-spec async_await(term()) -> term().
async_await(Future) ->
    perform(async, {await, Future}).

%% @doc Yield control during async computation.
-spec async_yield(term()) -> term().
async_yield(Value) ->
    perform(async, {yield, Value}).

%% @doc Run an async computation (spawns tasks).
-spec run_async() -> term().
run_async() ->
    run_async(#{}).

%% @doc Run an async computation with options.
-spec run_async(handler_options()) -> term().
run_async(_Options) ->
    %% TODO: Implement async runner properly
    ok.

%%====================================================================
%% Type System Integration
%%====================================================================

%% @doc Infer the effect type of an expression.
-spec infer_effect_type(term()) -> {ok, term()} | {error, term()}.
infer_effect_type(Expr) ->
    ?EFFECT_SYSTEM:infer_effect_type(Expr).

%% @doc Check if an effect type is valid.
-spec check_effect_type(term(), term()) -> {ok, term()} | {error, term()}.
check_effect_type(Expr, Env) ->
    ?EFFECT_SYSTEM:check_effect_type(Expr, Env).

%% @doc Generalize effect types in a type scheme.
-spec generalize_effects(term()) -> term().
generalize_effects(Type) ->
    ?EFFECT_SYSTEM:generalize_effects(Type).

%%====================================================================
%% Diagnostics and Debugging
%%====================================================================

%% @doc Get system diagnostics.
-spec diagnostics() -> map().
diagnostics() ->
    ?EFFECT_SYSTEM:diagnostics().

%% @doc Get system statistics.
-spec stats() -> map().
stats() ->
    %% Get stats from effect system and convert to map
    %% The stats() function returns a #stats{} record
    S = ?EFFECT_SYSTEM:stats(),
    %% Extract fields using record access
    Performs = element(2, S),  %% performs is at index 2
    HandlerExecs = element(3, S),  %% handler_executions is at index 3
    Resumptions = element(4, S),  %% resumptions is at index 4
    Optimizations = element(5, S),  %% optimizations is at index 5
    #{
        performs => Performs,
        handler_executions => HandlerExecs,
        resumptions => Resumptions,
        optimizations => Optimizations
    }.

%% @doc Dump the current handler stack.
-spec handler_stack() -> [map()].
handler_stack() ->
    ?EFFECT_SYSTEM:handler_stack_dump().

%% @doc Enable tracing for effect operations.
-spec trace(atom() | [atom()]) -> ok.
trace(Operations) when is_atom(Operations) ->
    trace([Operations]);
trace(Operations) when is_list(Operations) ->
    configure(#{trace => true}),
    %% TODO: Implement per-operation tracing
    ok.

%% @doc Disable tracing.
-spec untrace() -> ok.
untrace() ->
    configure(#{trace => false}).

%%====================================================================
%% Internal Handler Wrappers
%%====================================================================

%% @private Wrap handler for deep one-shot semantics.
wrap_deep_one_shot(HandlerFn) ->
    fun(Value, Resumption) ->
        WrappedResumption = catena_one_shot:wrap(catena_deep_handler:wrap(Resumption)),
        HandlerFn(Value, WrappedResumption)
    end.

%% @private Wrap handler for deep multi-shot semantics.
wrap_deep_multi_shot(HandlerFn) ->
    fun(Value, Resumption) ->
        WrappedResumption = catena_deep_handler:wrap(Resumption),
        HandlerFn(Value, WrappedResumption)
    end.

%% @private Wrap handler for shallow one-shot semantics.
wrap_shallow_one_shot(HandlerFn) ->
    fun(Value, Resumption) ->
        WrappedResumption = catena_one_shot:wrap(catena_shallow_handler:wrap(Resumption)),
        HandlerFn(Value, WrappedResumption)
    end.
