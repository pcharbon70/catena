%%%-------------------------------------------------------------------
%%% @doc Catena Effect Optimizations (Phase 14.1)
%%%
%%% This module implements optimization passes for effect operations.
%%% These transformations improve performance while preserving semantics.
%%%
%%% == Effect Fusion ==
%%%
%%% Combines multiple effect operations of the same type into a single
%%% operation, reducing handler overhead.
%%%
%%% Example: Two sequential State.put operations fuse into one.
%%%
%%% == Effect Inlining ==
%%%
%%% Replaces effect handler abstractions with direct implementations
%%% when the handler is known at compile time.
%%%
%%% == Static Effect Resolution ==
%%%
%%% Resolves effect handlers statically when possible, eliminating
%%% dynamic handler lookup overhead.
%%%
%%% == Equation-Based Optimization (Phase 14.1 Integration) ==
%%%
%%% Applies algebraic equations from catena_equations to optimize
%%% effectful programs based on algebraic laws.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_opt).

%% Effect fusion
-export([
    fuse_effects/1,
    fuse_state_effects/1,
    fuse_writer_effects/1,
    can_fuse/2
]).

%% Effect inlining
-export([
    inline_handlers/1,
    inline_state_handler/1,
    inline_reader_handler/1,
    should_inline/1
]).

%% Static effect resolution
-export([
    resolve_static_effects/1,
    resolve_handler/2,
    is_static_handler/1
]).

%% Equation-based optimization (Phase 14.1)
-export([
    apply_equations/1,
    apply_equations/2,
    add_equation/2,
    remove_equation/1,
    list_equations/0,
    optimize_with_equations/1
]).

%% Hefty tree optimization (Phase 14.1)
-export([
    optimize_hefty/1,
    fuse_hefty_operations/1,
    inline_hefty_handlers/1
]).

%%====================================================================
%% Types
%%====================================================================

-type effect_op() :: {effect, {EffectType :: atom(), Op :: term()}}.
-type effect_program() :: [effect_op() | term()].
-type handler_id() :: atom().

-type optimization_result() ::
    {optimized, effect_program()} |
    {no_change, effect_program()}.

-type equation() :: term().

%% Equation registry state
-record(eq_registry, {
    equations :: #{atom() => [equation()]},
    enabled :: boolean()
}).

-export_type([effect_op/0, effect_program/0, optimization_result/0]).

%%====================================================================
%% Process Dictionary Keys
%%====================================================================

-define(EQ_REGISTRY_KEY, catena_eq_registry).

%%====================================================================
%% Effect Fusion
%%====================================================================

%% @doc Fuse effect operations in a program.
%%
%% Scans the program for adjacent operations of the same effect type
%% and combines them where possible.
%%
%% @param Program The effect program to optimize
%% @returns {optimized, NewProgram} or {no_change, Program}
%%
%% @example
%% ```
%% %% Fuse two sequential State.put operations
%% Program = [
%%     {effect, {state, {put, 1}}},
%%     {effect, {state, {put, 2}}}
%% ],
%% {optimized, Fused} = catena_effect_opt:fuse_effects(Program).
%% '''
-spec fuse_effects(effect_program()) -> optimization_result().
fuse_effects(Program) when is_list(Program) ->
    case fuse_pass(Program) of
        Program -> {no_change, Program};
        FusedProgram -> {optimized, FusedProgram}
    end.

%% @doc Fuse State effect operations.
%%
%% Combines sequential State.put operations (last write wins).
%% Combines State.get followed by State.put into get_and_put.
%%
%% @param Ops List of state effect operations
%% @returns Fused operation list
%%
%% @example
%% ```
%% %% {put, 1} followed by {put, 2} fuses to {put, 2}
%% [{put, 2}] = catena_effect_opt:fuse_state_effects([{put, 1}, {put, 2}]).
%% '''
-spec fuse_state_effects([term()]) -> [term()].
fuse_state_effects(Ops) when is_list(Ops) ->
    fuse_state_pass(Ops, []).

%% @doc Fuse Writer effect operations.
%%
%% Combines sequential Writer.tell operations by concatenating values.
%%
%% @param Ops List of writer effect operations
%% @returns Fused operation list
%%
%% @example
%% ```
%% %% {tell, "hello"} followed by {tell, " world"} fuses to {tell, "hello world"}
%% [{tell, "hello world"}] = catena_effect_opt:fuse_writer_effects([
%%     {tell, "hello"}, {tell, " world"}
%% ]).
%% '''
-spec fuse_writer_effects([term()]) -> [term()].
fuse_writer_effects(Ops) when is_list(Ops) ->
    fuse_writer_pass(Ops, []).

%% @doc Check if two effect operations can be fused.
%%
%% @param Op1 First operation
%% @param Op2 Second operation
%% @returns true if operations can be fused
%%
%% @example
%% ```
%% true = catena_effect_opt:can_fuse({put, 1}, {put, 2}).
%% false = catena_effect_opt:can_fuse({put, 1}, {get}).
%% '''
-spec can_fuse(term(), term()) -> boolean().
can_fuse({put, _V1}, {put, _V2}) -> true;
can_fuse({tell, _V1}, {tell, _V2}) -> true;
can_fuse({get}, {put, _V}) -> true;
can_fuse(_Op1, _Op2) -> false.

%%====================================================================
%% Effect Inlining
%%====================================================================

%% @doc Inline effect handlers in a program.
%%
%% Replaces abstract handler calls with direct implementations
%% when the handler is known and simple enough.
%%
%% @param Program The effect program to optimize
%% @returns {optimized, NewProgram} or {no_change, Program}
%%
%% @example
%% ```
%% %% Inline a simple state handler
%% {optimized, Inlined} = catena_effect_opt:inline_handlers(Program).
%% '''
-spec inline_handlers(effect_program()) -> optimization_result().
inline_handlers(Program) when is_list(Program) ->
    case inline_pass(Program) of
        Program -> {no_change, Program};
        InlinedProgram -> {optimized, InlinedProgram}
    end.

%% @doc Inline State effect handler.
%%
%% Replaces State effect operations with direct state variable access.
%%
%% @param Ops List of state effect operations
%% @returns Inlined operation list
%%
%% @example
%% ```
%% %% Inline state operations to variable references
%% Inlined = catena_effect_opt:inline_state_handler(StateOps).
%% '''
-spec inline_state_handler([term()]) -> [term()].
inline_state_handler(Ops) when is_list(Ops) ->
    lists:map(fun inline_state_op/1, Ops).

%% @doc Inline Reader effect handler.
%%
%% Replaces Reader.ask operations with direct environment variable access.
%%
%% @param Ops List of reader effect operations
%% @returns Inlined operation list
%%
%% @example
%% ```
%% %% Inline reader operations to environment references
%% Inlined = catena_effect_opt:inline_reader_handler(ReaderOps).
%% '''
-spec inline_reader_handler([term()]) -> [term()].
inline_reader_handler(Ops) when is_list(Ops) ->
    lists:map(fun inline_reader_op/1, Ops).

%% @doc Check if a handler should be inlined.
%%
%% Handlers are inlined if they are:
%% - Small (few operations)
%% - Simple (no complex control flow)
%% - Static (known at compile time)
%%
%% @param Handler The handler to check
%% @returns true if handler should be inlined
%%
%% @example
%% ```
%% true = catena_effect_opt:should_inline({simple_handler, 3}).
%% false = catena_effect_opt:should_inline({complex_handler, 100}).
%% '''
-spec should_inline(term()) -> boolean().
should_inline({HandlerSize, _}) when is_integer(HandlerSize), HandlerSize =< 5 ->
    true;
should_inline({_, HandlerSize}) when is_integer(HandlerSize), HandlerSize =< 5 ->
    true;
should_inline(_) ->
    false.

%%====================================================================
%% Static Effect Resolution
%%====================================================================

%% @doc Resolve effect handlers statically.
%%
%% Analyzes the program to determine which effect handlers can be
%% resolved at compile time rather than runtime.
%%
%% @param Program The effect program to analyze
%% @returns {optimized, NewProgram} or {no_change, Program}
%%
%% @example
%% ```
%% %% Resolve static handlers
%% {optimized, Resolved} = catena_effect_opt:resolve_static_effects(Program).
%% '''
-spec resolve_static_effects(effect_program()) -> optimization_result().
resolve_static_effects(Program) when is_list(Program) ->
    case resolve_pass(Program) of
        Program -> {no_change, Program};
        ResolvedProgram -> {optimized, ResolvedProgram}
    end.

%% @doc Resolve a specific effect handler.
%%
%% Attempts to resolve the handler implementation statically.
%%
%% @param HandlerId The identifier of the handler
%% @param Context Compilation context for looking up handlers
%% @returns Resolved handler or 'dynamic' if cannot resolve
%%
%% @example
%% ```
%% %% Resolve a known handler
%% {ok, ResolvedHandler} = catena_effect_opt:resolve_handler(state_handler, Context).
%% '''
-spec resolve_handler(handler_id(), term()) -> {ok, term()} | dynamic.
resolve_handler(HandlerId, _Context) ->
    case is_known_handler(HandlerId) of
        true -> {ok, HandlerId};
        false -> dynamic
    end.

%% @doc Check if a handler is statically resolvable.
%%
%% @param Handler The handler to check
%% @returns true if handler is static (resolvable at compile time)
%%
%% @example
%% ```
%% true = catena_effect_opt:is_static_handler({builtin, state}).
%% false = catena_effect_opt:is_static_handler({user, custom_handler}).
%% '''
-spec is_static_handler(term()) -> boolean().
is_static_handler({builtin, _Name}) -> true;
is_static_handler({primitive, _Name}) -> true;
is_static_handler(_) ->
    false.

%%====================================================================
%% Equation-Based Optimization (Phase 14.1 Integration)
%%====================================================================

%% @doc Apply registered equations to optimize a program.
%%
%% This integrates with catena_equations to apply algebraic laws
%% for optimization. Equations are applied in multiple passes until
%% a fixpoint is reached.
%%
%% @param Program The effect program to optimize
%% @returns Optimized program
%%
%% @example
%% ```
%% %% Apply equations to optimize state operations
%% Optimized = catena_effect_opt:apply_equations(Program).
%% '''
-spec apply_equations(effect_program()) -> effect_program().
apply_equations(Program) ->
    apply_equations(Program, 10).  %% Max 10 passes to prevent infinite loops

%% @doc Apply equations with a maximum number of passes.
%%
%% @param Program The effect program to optimize
%% @param MaxPasses Maximum number of optimization passes
%% @returns Optimized program
-spec apply_equations(effect_program(), non_neg_integer()) -> effect_program().
apply_equations(Program, MaxPasses) when is_list(Program), is_integer(MaxPasses), MaxPasses > 0 ->
    Registry = get_eq_registry(),
    case Registry#eq_registry.enabled of
        false ->
            Program;
        true ->
            apply_equation_passes(Program, MaxPasses, Registry)
    end.

%% @doc Add an equation for an operation.
%%
%% @param Operation The operation this equation applies to
%% @param Equation The equation to add
%%
%% @example
%% ```
%% %% Add equation: get() >>= put == pure ()
%% catena_effect_opt:add_equation(state,
%%     catena_equations:new(
%%         catena_equations:seq([
%%             catena_equations:op(get, [], catena_equations:wildcard()),
%%             catena_equations:op(put, [], catena_equations:wildcard())
%%         ]),
%%         catena_equations:lit(undefined)
%%     )
%% ).
%% '''
-spec add_equation(atom(), equation()) -> ok.
add_equation(Operation, Equation) ->
    Registry = get_eq_registry(),
    Equations = maps:get(Operation, Registry#eq_registry.equations, []),
    NewEquations = maps:put(Operation, [Equation | Equations], Registry#eq_registry.equations),
    NewRegistry = Registry#eq_registry{equations = NewEquations},
    put(?EQ_REGISTRY_KEY, NewRegistry),
    ok.

%% @doc Remove all equations for an operation.
%%
%% @param Operation The operation to remove equations for
-spec remove_equation(atom()) -> ok.
remove_equation(Operation) ->
    Registry = get_eq_registry(),
    NewEquations = maps:remove(Operation, Registry#eq_registry.equations),
    NewRegistry = Registry#eq_registry{equations = NewEquations},
    put(?EQ_REGISTRY_KEY, NewRegistry),
    ok.

%% @doc List all registered equations.
%%
%% @returns List of {Operation, [Equations]} tuples
-spec list_equations() -> [{atom(), [equation()]}].
list_equations() ->
    Registry = get_eq_registry(),
    maps:to_list(Registry#eq_registry.equations).

%% @doc Optimize a program using only equation-based rewrites.
%%
%% @param Program The effect program to optimize
%% @returns {optimized, NewProgram} or {no_change, Program}
-spec optimize_with_equations(effect_program()) -> optimization_result().
optimize_with_equations(Program) ->
    case apply_equations(Program) of
        Program -> {no_change, Program};
        Optimized -> {optimized, Optimized}
    end.

%%====================================================================
%% Hefty Tree Optimization (Phase 14.1 Integration)
%%====================================================================

%% @doc Optimize a hefty tree representation.
%%
%% Integrates with catena_hefty for optimizing higher-order effect trees.
%%
%% @param HeftyTree The hefty tree to optimize
%% @returns Optimized hefty tree
-spec optimize_hefty(term()) -> term().
optimize_hefty(HeftyTree) ->
    case code:is_loaded(catena_hefty) of
        false ->
            HeftyTree;  %% Module not available, return as-is
        _ ->
            try
                catena_hefty:optimize(HeftyTree)
            catch
                _:_ -> HeftyTree
            end
    end.

%% @doc Fuse operations in a hefty tree.
%%
%% @param HeftyTree The hefty tree to optimize
%% @returns Optimized hefty tree
-spec fuse_hefty_operations(term()) -> term().
fuse_hefty_operations(HeftyTree) ->
    case code:is_loaded(catena_hefty) of
        false ->
            HeftyTree;
        _ ->
            try
                catena_hefty:fuse_operations(HeftyTree)
            catch
                _:_ -> HeftyTree
            end
    end.

%% @doc Inline handlers in a hefty tree.
%%
%% @param HeftyTree The hefty tree to optimize
%% @returns Optimized hefty tree
-spec inline_hefty_handlers(term()) -> term().
inline_hefty_handlers(HeftyTree) ->
    case code:is_loaded(catena_hefty) of
        false ->
            HeftyTree;
        _ ->
            try
                catena_hefty:inline_handlers(HeftyTree)
            catch
                _:_ -> HeftyTree
            end
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Get or create the equation registry.
get_eq_registry() ->
    case get(?EQ_REGISTRY_KEY) of
        undefined ->
            #eq_registry{
                equations = #{},
                enabled = true
            };
        Registry ->
            Registry
    end.

%% @doc Apply equation passes until fixpoint or max passes reached.
apply_equation_passes(Program, MaxPasses, Registry) ->
    apply_equation_passes(Program, MaxPasses, MaxPasses, Registry).

apply_equation_passes(Program, 0, _OriginalMax, _Registry) ->
    Program;
apply_equation_passes(Program, PassesLeft, OriginalMax, Registry) ->
    %% Apply all registered equations
    NewProgram = maps:fold(fun(Operation, Equations, AccProgram) ->
        lists:foldl(fun(Equation, Acc) ->
            apply_single_equation(Operation, Equation, Acc)
        end, AccProgram, Equations)
    end, Program, Registry#eq_registry.equations),

    %% Check if we made progress
    case NewProgram of
        Program ->
            %% Fixpoint reached
            Program;
        _ ->
            %% Continue optimizing
            apply_equation_passes(NewProgram, PassesLeft - 1, OriginalMax, Registry)
    end.

%% @doc Apply a single equation to a program.
apply_single_equation(_Operation, _Equation, Program) ->
    case code:is_loaded(catena_equation_apply) of
        false ->
            Program;  %% catena_equation_apply not available
        _ ->
            try
                case catena_equation_apply:apply(_Equation, Program) of
                    {ok, NewProgram} -> NewProgram;
                    {error, _} -> Program
                end
            catch
                _:_ -> Program
            end
    end.

%% @doc Perform a single fusion pass over the program.
-spec fuse_pass(effect_program()) -> effect_program().
fuse_pass(Program) ->
    fuse_pass(Program, []).

fuse_pass([], Acc) ->
    lists:reverse(Acc);
fuse_pass([Op | Rest], Acc) ->
    case Rest of
        [NextOp | _] when element(1, Op) =:= effect, element(1, NextOp) =:= effect ->
            {effect, {Type1, Op1}} = Op,
            {effect, {Type2, Op2}} = NextOp,
            case Type1 =:= Type2 andalso can_fuse(Op1, Op2) of
                true ->
                    Fused = fuse_ops(Type1, Op1, Op2),
                    fuse_pass(tl(Rest), [Fused | Acc]);
                false ->
                    fuse_pass(Rest, [Op | Acc])
            end;
        _ ->
            fuse_pass(Rest, [Op | Acc])
    end.

%% @doc Fuse two operations of the same type.
-spec fuse_ops(atom(), term(), term()) -> effect_op().
fuse_ops(state, {put, _V1}, {put, V2}) ->
    {effect, {state, {put, V2}}};
fuse_ops(state, {get}, {put, V}) ->
    {effect, {state, {get_and_put, fun(_S) -> {V, V} end}}};
fuse_ops(writer, {tell, V1}, {tell, V2}) ->
    {effect, {writer, {tell, fuse_values(V1, V2)}}};
fuse_ops(Type, Op1, Op2) ->
    %% Default: keep both operations
    [{effect, {Type, Op1}}, {effect, {Type, Op2}}].

%% @doc Fuse values for Writer.tell operations.
-spec fuse_values(term(), term()) -> term().
fuse_values(V1, V2) when is_list(V1) andalso is_list(V2) ->
    V1 ++ V2;
fuse_values(V1, V2) when is_binary(V1) andalso is_binary(V2) ->
    <<V1/binary, V2/binary>>;
fuse_values(V1, V2) when is_integer(V1) andalso is_integer(V2) ->
    V1 + V2;
fuse_values(V1, V2) ->
    [V1, V2].

%% @doc Fuse state operations in a pass.
-spec fuse_state_pass([term()], [term()]) -> [term()].
fuse_state_pass([], Acc) ->
    lists:reverse(Acc);
fuse_state_pass([{put, _V1}, {put, V2} | Rest], Acc) ->
    fuse_state_pass([{put, V2} | Rest], Acc);
fuse_state_pass([{get}, {put, V} | Rest], Acc) ->
    fuse_state_pass([{get_and_put, fun(_S) -> {V, V} end} | Rest], Acc);
fuse_state_pass([Op | Rest], Acc) ->
    fuse_state_pass(Rest, [Op | Acc]).

%% @doc Fuse writer operations in a pass.
-spec fuse_writer_pass([term()], [term()]) -> [term()].
fuse_writer_pass([], Acc) ->
    lists:reverse(Acc);
fuse_writer_pass([{tell, V1}, {tell, V2} | Rest], Acc) ->
    fuse_writer_pass([{tell, fuse_values(V1, V2)} | Rest], Acc);
fuse_writer_pass([Op | Rest], Acc) ->
    fuse_writer_pass(Rest, [Op | Acc]).

%% @doc Inline a single state operation.
-spec inline_state_op(term()) -> term().
inline_state_op({get}) ->
    {inline_var, state};
inline_state_op({put, _V} = Op) ->
    {inline_assign, state, Op};
inline_state_op({modify, F}) ->
    {inline_modify, state, F};
inline_state_op({get_and_put, F}) ->
    {inline_get_and_put, state, F};
inline_state_op(Op) ->
    Op.

%% @doc Inline a single reader operation.
-spec inline_reader_op(term()) -> term().
inline_reader_op(ask) ->
    {inline_var, env};
inline_reader_op({local, _F} = Op) ->
    {inline_local, env, Op};
inline_reader_op({ask_local, F}) ->
    {inline_apply, env, F};
inline_reader_op(Op) ->
    Op.

%% @doc Perform an inlining pass over the program.
-spec inline_pass(effect_program()) -> effect_program().
inline_pass(Program) ->
    lists:map(fun
        ({effect, {state, Op}}) ->
            {effect, {state, inline_state_op(Op)}};
        ({effect, {reader, Op}}) ->
            {effect, {reader, inline_reader_op(Op)}};
        (Other) ->
            Other
    end, Program).

%% @doc Perform a static resolution pass over the program.
-spec resolve_pass(effect_program()) -> effect_program().
resolve_pass(Program) ->
    lists:map(fun
        ({effect, {run_with, {effect, {Type, _}}, _Comp}} = _Op) when Type =:= state; Type =:= reader ->
            %% Try to resolve builtin handlers
            {effect, {run_with_static, Type}};
        (Other) ->
            Other
    end, Program).

%% @doc Check if a handler is a known builtin.
-spec is_known_handler(term()) -> boolean().
is_known_handler(state_handler) -> true;
is_known_handler(reader_handler) -> true;
is_known_handler(writer_handler) -> true;
is_known_handler(async_handler) -> true;
is_known_handler(_) -> false.
