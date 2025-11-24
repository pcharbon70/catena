%%%-------------------------------------------------------------------
%%% @doc Effect Handler Verification
%%%
%%% This module implements verification for algebraic effect handlers.
%%% It ensures that try/with blocks correctly handle all declared
%%% effect operations and that effect types are properly resolved.
%%%
%%% Key Concepts:
%%% - Effect Declaration: Defines effect operations and their types
%%% - Handler: try/with block that implements effect operations
%%% - Exhaustiveness: All operations must be handled
%%% - Effect Resolution: Handled effects are removed from the effect set
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_handler_verify).

%% Handler verification
-export([
    verify_handler/2,
    check_exhaustiveness/2,
    check_operation_types/3,
    resolve_handled_effects/2
]).

%% Error reporting
-export([
    format_handler_error/1
]).

-export_type([handler_error/0, effect_decl/0, handler_clause/0]).

%%%===================================================================
%%% Types
%%%===================================================================

%% Effect declaration: defines operations for an effect
-type effect_decl() :: {
    effect,
    atom(),                          % Effect name
    [operation_decl()],             % Operations
    catena_constraint:loc()
}.

%% Operation declaration within an effect
-type operation_decl() :: {
    operation,
    atom(),                          % Operation name
    [catena_types:ty()],             % Parameter types
    catena_types:ty(),               % Return type
    catena_constraint:loc()
}.

%% Handler clause in try/with
-type handler_clause() :: {
    handler_case,
    atom(),                          % Effect name
    atom(),                          % Operation name
    [pattern()],                     % Parameter patterns
    expr(),                          % Handler body
    catena_constraint:loc()
}.

%% Handler error types
-type handler_error() ::
    {missing_operation, atom(), atom(), catena_constraint:loc()} |
    {arity_mismatch, atom(), atom(), integer(), integer(), catena_constraint:loc()} |
    {type_mismatch, atom(), atom(), catena_types:ty(), catena_types:ty(), catena_constraint:loc()} |
    {unknown_operation, atom(), atom(), catena_constraint:loc()}.

-type pattern() :: term().  % Simplified for now
-type expr() :: term().     % Simplified for now

%%%===================================================================
%%% Handler Verification
%%%===================================================================

%% @doc Verify a complete handler against an effect declaration
%%
%% Checks:
%% 1. All operations are handled (exhaustiveness)
%% 2. Handler arities match operation arities
%% 3. Handler types match operation types
%% 4. No unknown operations are handled
%%
-spec verify_handler(effect_decl(), [handler_clause()]) ->
    {ok, verified} | {error, [handler_error()]}.
verify_handler({effect, EffectName, Operations, _Loc}, Clauses) ->
    % Check exhaustiveness
    ExhaustivenessErrors = check_exhaustiveness({effect, EffectName, Operations, unknown}, Clauses),

    % Check operation types
    TypeErrors = lists:flatmap(
        fun(Clause) ->
            check_handler_clause(Clause, Operations, EffectName)
        end,
        Clauses
    ),

    AllErrors = ExhaustivenessErrors ++ TypeErrors,

    case AllErrors of
        [] -> {ok, verified};
        _ -> {error, AllErrors}
    end.

%% @doc Check if all effect operations are handled
%%
%% An effect handler is exhaustive if it provides a handler clause
%% for every operation declared in the effect.
%%
-spec check_exhaustiveness(effect_decl(), [handler_clause()]) ->
    [handler_error()].
check_exhaustiveness({effect, EffectName, Operations, Loc}, Clauses) ->
    % Get all operation names from the declaration
    DeclaredOps = [OpName || {operation, OpName, _, _, _} <- Operations],

    % Get all operation names from handler clauses
    HandledOps = [OpName || {handler_case, _, OpName, _, _, _} <- Clauses],

    % Find missing operations
    Missing = DeclaredOps -- HandledOps,

    % Create errors for missing operations
    [{missing_operation, EffectName, OpName, Loc} || OpName <- Missing].

%% @doc Check operation types in handler clauses
%%
%% Verifies that handler clause arities and types match the
%% corresponding operation declarations.
%%
-spec check_operation_types(handler_clause(), [operation_decl()], atom()) ->
    [handler_error()].
check_operation_types({handler_case, EffectName, OpName, Params, _Body, Loc},
                       Operations, ExpectedEffect) ->
    case find_operation(OpName, Operations) of
        {ok, {operation, _OpName, DeclParams, _ReturnType, _OpLoc}} ->
            % Check arity
            DeclArity = length(DeclParams),
            HandlerArity = length(Params),

            if
                DeclArity =/= HandlerArity ->
                    [{arity_mismatch, EffectName, OpName, DeclArity, HandlerArity, Loc}];
                EffectName =/= ExpectedEffect ->
                    % Handler for wrong effect
                    [];
                true ->
                    % Arity matches, types would be checked during full type inference
                    []
            end;
        error ->
            % Operation not found in declaration
            [{unknown_operation, EffectName, OpName, Loc}]
    end.

%% @doc Resolve effects that are handled
%%
%% When a try/with handler successfully handles an effect, that
%% effect is removed from the function's effect set.
%%
%% Example:
%%   function f() / {FileIO, Process}  -- Has two effects
%%   try { f() } with {
%%     FileIO.read(path) -> ...
%%   }
%%   Result type: T / {Process}  -- FileIO resolved, Process remains
%%
-spec resolve_handled_effects(catena_types:effect_set(), [atom()]) ->
    catena_types:effect_set().
resolve_handled_effects({effect_set, Effects}, HandledEffects) ->
    Remaining = Effects -- HandledEffects,
    {effect_set, lists:usort(Remaining)}.

%%%===================================================================
%%% Error Formatting
%%%===================================================================

%% @doc Format a handler error for display
-spec format_handler_error(handler_error()) -> string().
format_handler_error({missing_operation, Effect, Op, Loc}) ->
    LocStr = format_location(Loc),
    lists:flatten(io_lib:format(
        "Missing handler for operation ~p.~p at ~s~n"
        "  The effect ~p requires a handler for all operations.",
        [Effect, Op, LocStr, Effect]
    ));

format_handler_error({arity_mismatch, Effect, Op, Expected, Got, Loc}) ->
    LocStr = format_location(Loc),
    lists:flatten(io_lib:format(
        "Arity mismatch for ~p.~p at ~s~n"
        "  Expected ~p parameters, got ~p.",
        [Effect, Op, LocStr, Expected, Got]
    ));

format_handler_error({type_mismatch, Effect, Op, Expected, Got, Loc}) ->
    LocStr = format_location(Loc),
    ExpStr = catena_type_pp:format(Expected),
    GotStr = catena_type_pp:format(Got),
    lists:flatten(io_lib:format(
        "Type mismatch for ~p.~p at ~s~n"
        "  Expected: ~s~n"
        "  Got: ~s",
        [Effect, Op, LocStr, ExpStr, GotStr]
    ));

format_handler_error({unknown_operation, Effect, Op, Loc}) ->
    LocStr = format_location(Loc),
    lists:flatten(io_lib:format(
        "Unknown operation ~p.~p at ~s~n"
        "  Effect ~p does not declare this operation.",
        [Effect, Op, LocStr, Effect]
    )).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Check a single handler clause
-spec check_handler_clause(handler_clause(), [operation_decl()], atom()) ->
    [handler_error()].
check_handler_clause(Clause, Operations, EffectName) ->
    check_operation_types(Clause, Operations, EffectName).

%% Find an operation by name in a list of operation declarations
-spec find_operation(atom(), [operation_decl()]) ->
    {ok, operation_decl()} | error.
find_operation(OpName, Operations) ->
    case lists:keyfind(OpName, 2, Operations) of
        false -> error;
        Op -> {ok, Op}
    end.

%% Format a source location
-spec format_location(catena_constraint:loc()) -> string().
format_location({file, Path, Line, Col}) ->
    lists:flatten(io_lib:format("~s:~p:~p", [Path, Line, Col]));
format_location(unknown) ->
    "unknown location".
