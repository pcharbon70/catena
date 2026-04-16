%%%-------------------------------------------------------------------
%%% @doc Catena Effect System Orchestration (Phase 14.1)
%%%
%%% This module provides the central orchestration layer for the complete
%%% algebraic effects system, integrating all components from Phases 7-13:
%%%
%%% - Handler/Resumption Model (Phase 7)
%%% - Deep/Shallow Handlers (Phase 9)
%%% - One-shot/Multi-shot Continuations (Phase 10)
%%% - Row Polymorphism (Phase 11)
%%% - Typed Handlers (Phase 12)
%%% - Signature Polymorphism & Higher-Order Effects (Phase 13)
%%%
%%% == System Architecture ==
%%%
%%% The effect system is organized as follows:
%%%
%%% ```
%%% ┌─────────────────────────────────────────────────────────┐
%%% │              catena_effect_system (Orchestration)        │
%%% ├─────────────────────────────────────────────────────────┤
%%% │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐ │
%%% │  │  Handlers   │  │ Resumptions │  │ Row Polymorphism│ │
%%% │  │  (Phase 7)  │  │ (Phase 7)   │  │   (Phase 11)    │ │
%%% │  └─────────────┘  └─────────────┘  └─────────────────┘ │
%%% │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐ │
%%% │  │  Equations  │  │  Hefty      │  │  Op Signatures  │ │
%%% │  │  (Phase 8)  │  │  (Phase 13) │  │   (Phase 13)    │ │
%%% │  └─────────────┘  └─────────────┘  └─────────────────┘ │
%%% │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐ │
%%% │  │ Deep/Shallow│  │ One/Multi   │  │  Typed Handlers │ │
%%% │  │ (Phase 9)   │  │ (Phase 10)  │  │   (Phase 12)    │ │
%%% │  └─────────────┘  └─────────────┘  └─────────────────┘ │
%%% └─────────────────────────────────────────────────────────┘
%%% ```
%%%
%%% == System Lifecycle ==
%%%
%%% 1. **Initialization**: Configure effect system with default handlers
%%% 2. **Compilation**: Type check with row polymorphism, validate handlers
%%% 3. **Execution**: Run effectful code with handler stack management
%%% 4. **Optimization**: Apply equation-based rewrites and effect fusion
%%% 5. **Cleanup**: Release resources and clear handler stacks
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_system).

%% System initialization and lifecycle
-export([
    new/0,
    new/1,
    init/0,
    init/1,
    shutdown/0,
    is_initialized/0
]).

%% System configuration
-export([
    configure/1,
    get_config/1,
    set_default_handler/2,
    get_default_handler/1
]).

%% Handler stack management
-export([
    push_handler_scope/0,
    push_handler_scope/1,
    pop_handler_scope/0,
    current_scope/0,
    scope_depth/0,
    reset_scopes/0
]).

%% Effect registration and lookup
-export([
    register_effect/2,
    register_effect/3,
    unregister_effect/1,
    lookup_effect/1,
    list_effects/0,
    effect_exists/1
]).

%% Handler registration and lookup
-export([
    register_handler/2,
    register_handler/3,
    unregister_handler/1,
    lookup_handler/1,
    current_handlers/0
]).

%% Equation management
-export([
    add_equation/2,
    add_equations/1,
    remove_equation/1,
    find_equations/1,
    all_equations/0,
    apply_equations/1
]).

%% Effect operations
-export([
    perform/2,
    perform/3,
    handle_with/3,
    handle_with/4,
    try_perform/2
]).

%% Optimization
-export([
    optimize_program/1,
    optimize_with_equations/1,
    fuse_effects/1,
    inline_handlers/1
]).

%% Type system integration
-export([
    check_effect_type/2,
    infer_effect_type/1,
    validate_row_var/1,
    generalize_effects/1
]).

%% System state and diagnostics
-export([
    system_state/0,
    handler_stack_dump/0,
    effect_registry_dump/0,
    equation_registry_dump/0,
    diagnostics/0,
    stats/0
]).

%%====================================================================
%% Types
%%====================================================================

-record(system_config, {
    default_handlers :: #{atom() => handler()},
    optimization_level :: 0..3,
    enable_equations :: boolean(),
    enable_hefty :: boolean(),
    enable_row_poly :: boolean(),
    trace_operations :: boolean()
}).

-record(system_state, {
    config :: #system_config{},
    handler_stack :: catena_handler_stack:stack(),
    effect_registry :: #{atom() => effect_definition()},
    equation_registry :: #{atom() => [catena_equations:equation()]},
    stats :: stats()
}).

-record(effect_definition, {
    name :: atom(),
    operations :: [atom()],
    row_var :: catena_row_types:row_var() | undefined,
    signature :: catena_op_signatures:op_signature() | undefined,
    metadata :: map()
}).

-record(handler, {
    operation :: atom(),
    handler_fn :: function(),
    metadata :: map()
}).

-record(stats, {
    performs = 0 :: non_neg_integer(),
    handler_executions = 0 :: non_neg_integer(),
    resumptions = 0 :: non_neg_integer(),
    optimizations = 0 :: non_neg_integer()
}).

-type effect_definition() :: #effect_definition{}.
-type handler() :: #handler{}.
-type system_config() :: #system_config{}.
-type system_state() :: #system_state{}.
-type operation_value() :: term().
-type effect_result() :: term().
-type optimization_level() :: 0..3.
-type stats() :: #stats{}.

-export_type([effect_definition/0, handler/0, system_config/0, system_state/0, optimization_level/0]).

%% Configuration options
-type config_option() ::
    {default_handlers, #{atom() => handler()}} |
    {optimization_level, optimization_level()} |
    {enable_equations, boolean()} |
    {enable_hefty, boolean()} |
    {enable_row_poly, boolean()} |
    {trace_operations, boolean()}.

%%====================================================================
%% Process Dictionary Keys
%%====================================================================

-define(SYSTEM_STATE_KEY, catena_effect_system_state).
-define(HANDLER_STACK_KEY, catena_handler_stack).
-define(EFFECT_REGISTRY_KEY, catena_effect_registry).
-define(EQUATION_REGISTRY_KEY, catena_equation_registry).

%%====================================================================
%% System Initialization and Lifecycle
%%====================================================================

%% @doc Create a new system configuration with defaults.
-spec new() -> system_config().
new() ->
    new([]).

%% @doc Create a new system configuration with options.
-spec new([config_option()]) -> system_config().
new(Options) ->
    Defaults = #system_config{
        default_handlers = #{},
        optimization_level = 2,
        enable_equations = true,
        enable_hefty = true,
        enable_row_poly = true,
        trace_operations = false
    },
    apply_options(Defaults, Options).

%% @doc Initialize the effect system with default configuration.
-spec init() -> ok.
init() ->
    init(new()).

%% @doc Initialize the effect system with a custom configuration.
-spec init(system_config() | [config_option()]) -> ok.
init(Config) when is_record(Config, system_config) ->
    State = #system_state{
        config = Config,
        handler_stack = catena_handler_stack:new(),
        effect_registry = #{},
        equation_registry = #{},
        stats = #stats{}
    },
    put(?SYSTEM_STATE_KEY, State),
    ok;
init(Options) when is_list(Options) ->
    init(new(Options)).

%% @doc Check if the effect system is initialized.
-spec is_initialized() -> boolean().
is_initialized() ->
    get(?SYSTEM_STATE_KEY) =/= undefined.

%% @doc Shutdown the effect system and cleanup resources.
-spec shutdown() -> ok.
shutdown() ->
    %% Reset handler stack
    catena_handler_stack:reset(),
    %% Clear registries
    erase(?SYSTEM_STATE_KEY),
    erase(?EFFECT_REGISTRY_KEY),
    erase(?EQUATION_REGISTRY_KEY),
    ok.

%%====================================================================
%% System Configuration
%%====================================================================

%% @doc Configure the effect system with options.
-spec configure([config_option()]) -> ok.
configure(Options) when is_list(Options) ->
    State = get_state(),
    Config = apply_options(State#system_state.config, Options),
    put(?SYSTEM_STATE_KEY, State#system_state{config = Config}),
    ok.

%% @doc Get a configuration value.
-spec get_config(atom()) -> term().
get_config(Key) ->
    State = get_state(),
    Config = State#system_state.config,
    case Key of
        default_handlers -> Config#system_config.default_handlers;
        optimization_level -> Config#system_config.optimization_level;
        enable_equations -> Config#system_config.enable_equations;
        enable_hefty -> Config#system_config.enable_hefty;
        enable_row_poly -> Config#system_config.enable_row_poly;
        trace_operations -> Config#system_config.trace_operations;
        _ -> undefined
    end.

%% @doc Set a default handler for an operation.
-spec set_default_handler(atom(), function()) -> ok.
set_default_handler(Operation, HandlerFn) when is_function(HandlerFn, 2) ->
    State = get_state(),
    Handler = #handler{
        operation = Operation,
        handler_fn = HandlerFn,
        metadata = #{default => true}
    },
    Config = State#system_state.config,
    DefaultHandlers = maps:put(Operation, Handler, Config#system_config.default_handlers),
    NewConfig = Config#system_config{default_handlers = DefaultHandlers},
    put(?SYSTEM_STATE_KEY, State#system_state{config = NewConfig}),
    ok.

%% @doc Get the default handler for an operation.
-spec get_default_handler(atom()) -> {ok, handler()} | {error, not_found}.
get_default_handler(Operation) ->
    State = get_state(),
    Config = State#system_state.config,
    case maps:find(Operation, Config#system_config.default_handlers) of
        {ok, Handler} -> {ok, Handler};
        error -> {error, not_found}
    end.

%%====================================================================
%% Handler Stack Management
%%====================================================================

%% @doc Push a new empty handler scope onto the stack.
-spec push_handler_scope() -> ok.
push_handler_scope() ->
    push_handler_scope(#{}).

%% @doc Push a new handler scope with initial handlers onto the stack.
-spec push_handler_scope(#{atom() => handler()}) -> ok.
push_handler_scope(Handlers) ->
    catena_handler_stack:push(Handlers),
    ok.

%% @doc Pop the current handler scope from the stack.
-spec pop_handler_scope() -> {ok, #{atom() => handler()}} | {error, empty}.
pop_handler_scope() ->
    catena_handler_stack:pop().

%% @doc Get the current handler scope.
-spec current_scope() -> #{atom() => handler()} | undefined.
current_scope() ->
    catena_handler_stack:current().

%% @doc Get the current handler stack depth.
-spec scope_depth() -> non_neg_integer().
scope_depth() ->
    catena_handler_stack:depth().

%% @doc Reset all handler scopes.
-spec reset_scopes() -> ok.
reset_scopes() ->
    catena_handler_stack:reset(),
    ok.

%%====================================================================
%% Effect Registration and Lookup
%%====================================================================

%% @doc Register an effect with its operations.
-spec register_effect(atom(), [atom()]) -> ok.
register_effect(Name, Operations) when is_atom(Name), is_list(Operations) ->
    register_effect(Name, Operations, #{}).

%% @doc Register an effect with operations and metadata.
-spec register_effect(atom(), [atom()], map()) -> ok.
register_effect(Name, Operations, Metadata) when is_atom(Name), is_list(Operations), is_map(Metadata) ->
    EffectDef = #effect_definition{
        name = Name,
        operations = Operations,
        row_var = undefined,
        signature = undefined,
        metadata = Metadata
    },
    State = get_state(),
    Registry = State#system_state.effect_registry,
    NewRegistry = maps:put(Name, EffectDef, Registry),
    put(?SYSTEM_STATE_KEY, State#system_state{effect_registry = NewRegistry}),
    ok.

%% @doc Unregister an effect.
-spec unregister_effect(atom()) -> ok.
unregister_effect(Name) ->
    State = get_state(),
    Registry = maps:remove(Name, State#system_state.effect_registry),
    put(?SYSTEM_STATE_KEY, State#system_state{effect_registry = Registry}),
    ok.

%% @doc Look up an effect definition.
-spec lookup_effect(atom()) -> {ok, effect_definition()} | {error, not_found}.
lookup_effect(Name) ->
    State = get_state(),
    case maps:find(Name, State#system_state.effect_registry) of
        {ok, EffectDef} -> {ok, EffectDef};
        error -> {error, not_found}
    end.

%% @doc List all registered effects.
-spec list_effects() -> [atom()].
list_effects() ->
    State = get_state(),
    maps:keys(State#system_state.effect_registry).

%% @doc Check if an effect is registered.
-spec effect_exists(atom()) -> boolean().
effect_exists(Name) ->
    case lookup_effect(Name) of
        {ok, _} -> true;
        {error, not_found} -> false
    end.

%%====================================================================
%% Handler Registration and Lookup
%%====================================================================

%% @doc Register a handler for an operation in the current scope.
-spec register_handler(atom(), function()) -> ok.
register_handler(Operation, HandlerFn) when is_function(HandlerFn, 2) ->
    register_handler(Operation, HandlerFn, #{}).

%% @doc Register a handler with metadata in the current scope.
-spec register_handler(atom(), function(), map()) -> ok.
register_handler(Operation, HandlerFn, Metadata) when is_function(HandlerFn, 2), is_map(Metadata) ->
    Handler = catena_handler:new(Operation, HandlerFn, Metadata),
    case catena_handler:register(Operation, Handler) of
        ok -> ok;
        {error, no_current_scope} ->
            %% Auto-create a scope if none exists
            push_handler_scope(#{Operation => Handler}),
            ok
    end.

%% @doc Unregister a handler for an operation in the current scope.
-spec unregister_handler(atom()) -> ok.
unregister_handler(Operation) ->
    case catena_handler:unregister(Operation) of
        ok -> ok;
        {error, no_current_scope} -> ok
    end.

%% @doc Look up a handler for an operation.
-spec lookup_handler(atom()) -> {ok, handler()} | {error, not_found}.
lookup_handler(Operation) ->
    catena_handler:lookup(Operation).

%% @doc Get all handlers in the current scope.
-spec current_handlers() -> #{atom() => handler()}.
current_handlers() ->
    case catena_handler_stack:current() of
        undefined -> #{};
        Scope -> Scope
    end.

%%====================================================================
%% Equation Management
%%====================================================================

%% @doc Add an equation for an operation.
-spec add_equation(atom(), catena_equations:equation()) -> ok.
add_equation(Operation, Equation) ->
    State = get_state(),
    Registry = State#system_state.equation_registry,
    Equations = maps:get(Operation, Registry, []),
    NewRegistry = maps:put(Operation, [Equation | Equations], Registry),
    put(?SYSTEM_STATE_KEY, State#system_state{equation_registry = NewRegistry}),
    ok.

%% @doc Add multiple equations for operations.
-spec add_equations([{atom(), catena_equations:equation()}]) -> ok.
add_equations(Equations) ->
    lists:foreach(fun({Op, Eq}) -> add_equation(Op, Eq) end, Equations),
    ok.

%% @doc Remove equations for an operation.
-spec remove_equation(atom()) -> ok.
remove_equation(Operation) ->
    State = get_state(),
    Registry = maps:remove(Operation, State#system_state.equation_registry),
    put(?SYSTEM_STATE_KEY, State#system_state{equation_registry = Registry}),
    ok.

%% @doc Find equations for an operation.
-spec find_equations(atom()) -> [catena_equations:equation()].
find_equations(Operation) ->
    State = get_state(),
    maps:get(Operation, State#system_state.equation_registry, []).

%% @doc Get all registered equations.
-spec all_equations() -> [{atom(), [catena_equations:equation()]}].
all_equations() ->
    State = get_state(),
    maps:to_list(State#system_state.equation_registry).

%% @doc Apply equations to optimize an expression.
-spec apply_equations(term()) -> term().
apply_equations(Expr) ->
    State = get_state(),
    case State#system_state.config#system_config.enable_equations of
        true ->
            Equations = all_equations(),
            lists:foldl(fun({_Op, Eqs}, Acc) ->
                lists:foldl(fun(Eq, E) ->
                    case catena_equation_apply:apply(Eq, E) of
                        {ok, NewExpr} -> NewExpr;
                        {error, _} -> E
                    end
                end, Acc, Eqs)
            end, Expr, Equations);
        false ->
            Expr
    end.

%%====================================================================
%% Effect Operations
%%====================================================================

%% @doc Perform an effect operation with a value.
-spec perform(atom(), operation_value()) -> effect_result().
perform(Operation, Value) ->
    perform(Operation, Value, []).

%% @doc Perform an effect operation with a value and options.
-spec perform(atom(), operation_value(), list()) -> effect_result().
perform(Operation, Value, _Options) ->
    %% Update stats
    State = get_state(),
    Stats = State#system_state.stats,
    NewStats = Stats#stats{performs = Stats#stats.performs + 1},
    put(?SYSTEM_STATE_KEY, State#system_state{stats = NewStats}),

    %% Trace if enabled
    trace_if_enabled(Operation, Value),

    %% Capture continuation
    Cont = catena_resumption:capture_continuation(),

    %% Look up handler
    case catena_handler:lookup(Operation) of
        {ok, Handler} ->
            %% Execute handler
            Resumption = catena_resumption:new(Cont, erlang:system_time(millisecond), scope_depth()),
            case catena_handler:execute(Handler, Value, Resumption) of
                {handler_error, Kind, Reason, Stack} ->
                    error({handler_crash, Operation, Kind, Reason, Stack});
                Result ->
                    %% Update handler execution stats
                    Stats2 = get_state(),
                    CurStats = Stats2#system_state.stats,
                    put(?SYSTEM_STATE_KEY, Stats2#system_state{
                        stats = CurStats#stats{handler_executions = CurStats#stats.handler_executions + 1}
                    }),
                    Result
            end;
        {error, not_found} ->
            %% Try default handler
            case get_default_handler(Operation) of
                {ok, DefaultHandler} ->
                    Resumption = catena_resumption:new(Cont, erlang:system_time(millisecond), scope_depth()),
                    catena_handler:execute(DefaultHandler, Value, Resumption);
                {error, not_found} ->
                    error({unhandled_effect, Operation})
            end
    end.

%% @doc Try to perform an effect operation, returning error on failure.
-spec try_perform(atom(), operation_value()) -> {ok, effect_result()} | {error, term()}.
try_perform(Operation, Value) ->
    try
        {ok, perform(Operation, Value)}
    catch
        _:Reason -> {error, Reason}
    end.

%% @doc Handle a computation with a handler.
-spec handle_with(atom(), function(), function()) -> term().
handle_with(Operation, HandlerFn, Computation) ->
    handle_with(Operation, HandlerFn, Computation, #{}).

%% @doc Handle a computation with a handler and options.
-spec handle_with(atom(), function(), function(), map()) -> term().
handle_with(Operation, HandlerFn, Computation, Options) when is_function(HandlerFn, 2), is_function(Computation, 0) ->
    %% Create handler
    Handler = catena_handler:new(Operation, HandlerFn, Options),

    %% Push new scope
    push_handler_scope(#{Operation => Handler}),

    try
        %% Run computation
        Computation()
    after
        %% Pop scope
        pop_handler_scope()
    end.

%%====================================================================
%% Optimization
%%====================================================================

%% @doc Optimize a program using all available optimization passes.
-spec optimize_program(term()) -> term().
optimize_program(Program) ->
    State = get_state(),
    Level = State#system_state.config#system_config.optimization_level,

    %% Update stats
    Stats = State#system_state.stats,
    NewStats = Stats#stats{optimizations = Stats#stats.optimizations + 1},
    put(?SYSTEM_STATE_KEY, State#system_state{stats = NewStats}),

    Result = case Level of
        0 -> Program;
        1 -> optimize_level_1(Program);
        2 -> optimize_level_2(Program);
        3 -> optimize_level_3(Program)
    end,

    %% Apply equation-based optimizations if enabled
    case State#system_state.config#system_config.enable_equations of
        true -> apply_equations(Result);
        false -> Result
    end.

%% @doc Optimize using only equation-based rewrites.
-spec optimize_with_equations(term()) -> term().
optimize_with_equations(Expr) ->
    apply_equations(Expr).

%% @doc Fuse effect operations in a program.
-spec fuse_effects(term()) -> term().
fuse_effects(Program) ->
    case catena_effect_opt:fuse_effects(Program) of
        {optimized, NewProgram} -> NewProgram;
        {no_change, _} -> Program
    end.

%% @doc Inline effect handlers in a program.
-spec inline_handlers(term()) -> term().
inline_handlers(Program) ->
    case catena_effect_opt:inline_handlers(Program) of
        {optimized, NewProgram} -> NewProgram;
        {no_change, _} -> Program
    end.

%%====================================================================
%% Type System Integration
%%====================================================================

%% @doc Check if an effect type is valid.
-spec check_effect_type(term(), term()) -> {ok, term()} | {error, term()}.
check_effect_type(Expr, Env) ->
    catena_effect_infer:check(Expr, Env).

%% @doc Infer the effect type of an expression.
-spec infer_effect_type(term()) -> {ok, term()} | {error, term()}.
infer_effect_type(_Expr) ->
    %% TODO: Integrate with type inference
    {ok, empty_effects}.

%% @doc Validate a row variable.
-spec validate_row_var(catena_row_types:row_var()) -> ok | {error, term()}.
validate_row_var(RowVar) ->
    catena_row_types:is_valid_row(RowVar).

%% @doc Generalize effect types in a type scheme.
-spec generalize_effects(term()) -> term().
generalize_effects(Type) ->
    catena_type_scheme:generalize_effects(Type).

%%====================================================================
%% System State and Diagnostics
%%====================================================================

%% @doc Get the current system state.
-spec system_state() -> system_state().
system_state() ->
    get_state().

%% @doc Dump the current handler stack.
-spec handler_stack_dump() -> [map()].
handler_stack_dump() ->
    catena_handler_stack:to_list().

%% @doc Dump the effect registry.
-spec effect_registry_dump() -> [{atom(), effect_definition()}].
effect_registry_dump() ->
    State = get_state(),
    maps:to_list(State#system_state.effect_registry).

%% @doc Dump the equation registry.
-spec equation_registry_dump() -> [{atom(), [catena_equations:equation()]}].
equation_registry_dump() ->
    all_equations().

%% @doc Get system diagnostics.
-spec diagnostics() -> map().
diagnostics() ->
    State = get_state(),
    #{
        initialized => is_initialized(),
        scope_depth => scope_depth(),
        num_effects => length(list_effects()),
        num_equations => length(all_equations()),
        config => diagnostics_config(State#system_state.config),
        stats => diagnostics_stats(State#system_state.stats)
    }.

%% @doc Get system statistics.
-spec stats() -> stats().
stats() ->
    State = get_state(),
    State#system_state.stats.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Get the current system state or error.
get_state() ->
    case get(?SYSTEM_STATE_KEY) of
        undefined -> error({effect_system_not_initialized});
        State -> State
    end.

%% @private Apply configuration options to a config record.
apply_options(Config, []) ->
    Config;
apply_options(Config, [{Key, Value} | Rest]) ->
    NewConfig = case Key of
        default_handlers when is_map(Value) ->
            Config#system_config{default_handlers = Value};
        optimization_level when is_integer(Value), Value >= 0, Value =< 3 ->
            Config#system_config{optimization_level = Value};
        enable_equations when is_boolean(Value) ->
            Config#system_config{enable_equations = Value};
        enable_heavy when is_boolean(Value) ->
            Config#system_config{enable_hefty = Value};
        enable_row_poly when is_boolean(Value) ->
            Config#system_config{enable_row_poly = Value};
        trace_operations when is_boolean(Value) ->
            Config#system_config{trace_operations = Value};
        _ ->
            Config
    end,
    apply_options(NewConfig, Rest).

%% @private Trace an effect operation if tracing is enabled.
trace_if_enabled(Operation, Value) ->
    State = get_state(),
    case State#system_state.config#system_config.trace_operations of
        true ->
            io:format("[Effect] ~p: ~p~n", [Operation, Value]);
        false ->
            ok
    end.

%% @private Optimize at level 1 (basic optimizations).
optimize_level_1(Program) ->
    %% Basic effect fusion
    fuse_effects(Program).

%% @private Optimize at level 2 (includes handler inlining).
optimize_level_2(Program) ->
    Program1 = fuse_effects(Program),
    inline_handlers(Program1).

%% @private Optimize at level 3 (aggressive optimizations).
optimize_level_3(Program) ->
    Program1 = optimize_level_2(Program),
    %% Additional hefty tree optimizations if enabled
    State = get_state(),
    case State#system_state.config#system_config.enable_hefty of
        true ->
            catena_hefty:optimize(Program1);
        false ->
            Program1
    end.

%% @private Convert config to diagnostics map.
diagnostics_config(Config) ->
    #{
        optimization_level => Config#system_config.optimization_level,
        enable_equations => Config#system_config.enable_equations,
        enable_hefty => Config#system_config.enable_hefty,
        enable_row_poly => Config#system_config.enable_row_poly,
        trace_operations => Config#system_config.trace_operations
    }.

%% @private Convert stats to diagnostics map.
diagnostics_stats(Stats) ->
    #{
        performs => Stats#stats.performs,
        handler_executions => Stats#stats.handler_executions,
        resumptions => Stats#stats.resumptions,
        optimizations => Stats#stats.optimizations
    }.
