%%%-------------------------------------------------------------------
%%% @doc Catena Effect Optimizations (Phase 6.3)
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

%%====================================================================
%% Types
%%====================================================================

-type effect_op() :: {effect, {EffectType :: atom(), Op :: term()}}.
-type effect_program() :: [effect_op() | term()].
-type handler_id() :: atom().

-type optimization_result() ::
    {optimized, effect_program()} |
    {no_change, effect_program()}.

-export_type([effect_op/0, effect_program/0, optimization_result/0]).

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
%% Internal Functions
%%====================================================================

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
    % Default: keep both operations
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
fuse_state_pass([{put, V1}, {put, V2} | Rest], Acc) ->
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
        ({effect, {run_with, {effect, {Type, _}}, _Comp}} = Op) when Type =:= state; Type =:= reader ->
            % Try to resolve builtin handlers
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
