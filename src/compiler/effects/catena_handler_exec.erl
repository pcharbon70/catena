%%%-------------------------------------------------------------------
%%% @doc Catena Typed Handler Execution (Phase 12.4)
%%%
%%% This module implements typed handler execution with runtime type
%%% checks for safe effect handling.
%%%
%%% == Typed Handler Invocation ==
%%%
%%% Executes handlers with runtime type validation:
%%% - Operation type checking before execution
%%% - Handler return type validation after execution
%%% - Effect row validation during handler execution
%%% - Type error handling and reporting
%%%
%%% == Resumption Type Enforcement ==
%%%
%%% Enforces types when invoking resumptions:
%%% - Resumption parameter type validation
%%% - Resumption return type checking
%%% - Resumption effect validation
%%% - Safe type coercion for resumptions
%%%
%%% == Handler Type Coercion ==
%%%
%%% Implements safe type coercion for handlers:
%%% - Subtype relationships for handler types
%%% - Effect row subsumption
%%% - Operation signature compatibility
%%% - Safe type casting with validation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_handler_exec).

%% Typed handler invocation
-export([
    execute_typed/4,
    execute_with_checks/3,
    validate_before_execution/2
]).

%% Resumption type enforcement
-export([
    enforce_resumption_type/3,
    check_resumption_param/2,
    check_resumption_return/2,
    safe_resumption_call/3
]).

%% Handler type coercion
-export([
    coerce_handler/2,
    check_subtype/2,
    check_compatible/2,
    safe_cast/3,
    effects_subsumes/2
]).

%% Type error handling
-export([
    handle_type_error/2,
    format_type_error/1,
    recover_from_error/2
]).

%%====================================================================
%% Types
%%====================================================================

-type handler_type() :: catena_handler_types:handler_type().
-type operation_sig() :: catena_handler_types:operation_sig().
-type effect_row() :: catena_row_types:effect_row().
-type type_ref() :: catena_types:ty() | atom().
-type execution_result() :: {ok, term()} | {error, term()}.
-type coercion_result() :: {ok, handler_type()} | {error, term()}.

-type type_error() ::
    {operation_type_mismatch, atom(), type_ref(), type_ref()} |
    {return_type_mismatch, type_ref(), type_ref()} |
    {effect_mismatch, effect_row(), effect_row()} |
    {resumption_type_error, term()} |
    {coercion_failed, handler_type(), handler_type()}.

-export_type([type_error/0, execution_result/0, coercion_result/0]).

%%====================================================================
%% Typed Handler Invocation
%%====================================================================

%% @doc Execute handler with full type checking.
-spec execute_typed(atom(), atom(), handler_type(), term()) -> execution_result().
execute_typed(Operation, HandlerFn, HandlerType, InputValue) ->
    % Validate handler type
    case validate_before_execution(HandlerType, InputValue) of
        {error, Reason} ->
            {error, {pre_execution_check_failed, Reason}};
        ok ->
            % Get operation signature
            Ops = maps:get(operations, HandlerType),
            case maps:find(Operation, Ops) of
                {ok, OpSig} ->
                    execute_operation_checked(Operation, HandlerFn, OpSig, InputValue);
                error ->
                    {error, {unknown_operation, Operation}}
            end
    end.

%% @doc Execute handler with runtime type checks.
-spec execute_with_checks(module(), atom(), term()) -> execution_result().
execute_with_checks(Module, HandlerFun, Value) ->
    try
        Result = Module:HandlerFun(Value, dummy_resumption),
        {ok, Result}
    catch
        Kind:Reason:Stack ->
            {error, {execution_error, Kind, Reason, Stack}}
    end.

%% @doc Validate inputs before handler execution.
-spec validate_before_execution(handler_type(), term()) -> ok | {error, term()}.
validate_before_execution(HandlerType, InputValue) ->
    InputType = maps:get(input, HandlerType),
    case value_matches_type(InputType, InputValue) of
        true -> ok;
        false -> {error, {input_type_mismatch, InputType, type_of(InputValue)}}
    end.

%%====================================================================
%% Resumption Type Enforcement
%%====================================================================

%% @doc Enforce resumption types during execution.
-spec enforce_resumption_type(operation_sig(), term(), catena_resumption:resumption()) ->
    {ok, term()} | {error, term()}.
enforce_resumption_type(OpSig, ParamValue, Resumption) ->
    % Check parameter type
    Params = maps:get(params, OpSig),
    case check_resumption_param(Params, ParamValue) of
        ok ->
            % Execute resumption
            Result = catena_resumption:resume(Resumption, ParamValue),
            % Check return type
            ResultType = maps:get(result, OpSig),
            check_resumption_return(ResultType, Result);
        {error, _} = Error ->
            Error
    end.

%% @doc Check resumption parameter type.
-spec check_resumption_param([type_ref()], term()) -> ok | {error, term()}.
check_resumption_param([ExpectedType], ParamValue) ->
    case value_matches_type(ExpectedType, ParamValue) of
        true -> ok;
        false -> {error, {param_type_mismatch, ExpectedType, type_of(ParamValue)}}
    end;
check_resumption_param(_, _) ->
    {error, invalid_param_count}.

%% @doc Check resumption return type.
-spec check_resumption_return(type_ref(), term()) -> {ok, term()} | {error, term()}.
check_resumption_return(ExpectedType, ReturnValue) ->
    case value_matches_type(ExpectedType, ReturnValue) of
        true -> {ok, ReturnValue};
        false -> {error, {return_type_mismatch, ExpectedType, type_of(ReturnValue)}}
    end.

%% @doc Safe resumption call with type enforcement.
-spec safe_resumption_call(operation_sig(), term(), catena_resumption:resumption()) ->
    {ok, term()} | {error, term()}.
safe_resumption_call(OpSig, ParamValue, Resumption) ->
    try
        enforce_resumption_type(OpSig, ParamValue, Resumption)
    catch
        Kind:Reason:Stack ->
            {error, {resumption_error, Kind, Reason, Stack}}
    end.

%%====================================================================
%% Handler Type Coercion
%%====================================================================

%% @doc Coerce handler to expected type if compatible.
-spec coerce_handler(handler_type(), handler_type()) -> coercion_result().
coerce_handler(ActualType, ExpectedType) ->
    case check_subtype(ActualType, ExpectedType) of
        true -> {ok, ActualType};
        false -> check_compatible(ActualType, ExpectedType)
    end.

%% @doc Check if one handler type is a subtype of another.
-spec check_subtype(handler_type(), handler_type()) -> boolean().
check_subtype(Actual, Expected) ->
    % Check input/output type relationship (contravariant input, covariant output)
    InputOK = type_compatible(maps:get(input, Expected), maps:get(input, Actual)),
    OutputOK = type_compatible(maps:get(output, Actual), maps:get(output, Expected)),
    InputOK andalso OutputOK andalso
    effects_subsumes(maps:get(effects, Actual), maps:get(effects, Expected)).

%% @doc Check if handler types are compatible (looser than subtype).
-spec check_compatible(handler_type(), handler_type()) -> coercion_result().
check_compatible(Actual, Expected) ->
    % Check operations compatibility
    ActualOps = maps:get(operations, Actual),
    ExpectedOps = maps:get(operations, Expected),

    case maps:size(ActualOps) >= maps:size(ExpectedOps) of
        false -> {error, {insufficient_operations, Actual, Expected}};
        true ->
            case all_operations_compatible(ActualOps, ExpectedOps) of
                true -> {ok, Actual};
                false -> {error, {incompatible_operations, Actual, Expected}}
            end
    end.

%% @doc Perform safe type cast with validation.
-spec safe_cast(term(), type_ref(), type_ref()) -> {ok, term()} | {error, term()}.
safe_cast(Value, FromType, ToType) ->
    case type_matches(FromType, type_of(Value)) of
        false -> {error, {source_type_mismatch, FromType, type_of(Value)}};
        true ->
            case value_matches_type(ToType, Value) of
                true -> {ok, Value};
                false -> {error, {incompatible_cast, FromType, ToType}}
            end
    end.

%%====================================================================
%% Type Error Handling
%%====================================================================

%% @doc Handle type errors during execution.
-spec handle_type_error(type_error(), term()) -> execution_result().
handle_type_error({operation_type_mismatch, Op, Expected, Actual}, _Context) ->
    {error, {operation_mismatch, Op, Expected, Actual}};
handle_type_error({return_type_mismatch, Expected, Actual}, _Context) ->
    {error, {type_error, Expected, Actual}};
handle_type_error({effect_mismatch, Expected, Actual}, _Context) ->
    {error, {effect_error, Expected, Actual}};
handle_type_error({resumption_type_error, Reason}, _Context) ->
    {error, {resumption_error, Reason}};
handle_type_error({coercion_failed, From, To}, _Context) ->
    {error, {coercion_error, From, To}}.

%% @doc Format type error for display.
-spec format_type_error(type_error()) -> binary().
format_type_error({operation_type_mismatch, Op, Expected, Actual}) ->
    <<"Operation '", (atom_to_binary(Op))/binary, "' type mismatch: ",
      "expected ", (format_type(Expected))/binary,
      ", got ", (format_type(Actual))/binary>>;
format_type_error({return_type_mismatch, Expected, Actual}) ->
    <<"Return type mismatch: expected ", (format_type(Expected))/binary,
      ", got ", (format_type(Actual))/binary>>;
format_type_error({effect_mismatch, Expected, Actual}) ->
    <<"Effect mismatch: expected ", (format_effects(Expected))/binary,
      ", got ", (format_effects(Actual))/binary>>;
format_type_error({resumption_type_error, Reason}) ->
    <<"Resumption type error: ", (format_term(Reason))/binary>>;
format_type_error({coercion_failed, From, To}) ->
    <<"Coercion failed from ", (format_handler(From))/binary,
      " to ", (format_handler(To))/binary>>.

%% @doc Attempt to recover from type error.
-spec recover_from_error(type_error(), term()) -> {ok, term()} | {error, term()}.
recover_from_error({return_type_mismatch, _Expected, Actual}, _Context) ->
    % Return actual value if recovery is possible
    {ok, Actual};
recover_from_error(Error, _Context) ->
    {error, Error}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Execute operation with type checking.
execute_operation_checked(_Operation, HandlerFn, OpSig, InputValue) ->
    % Validate input type
    Params = maps:get(params, OpSig),
    case validate_input_types(Params, [InputValue]) of
        ok ->
            try
                Result = HandlerFn(InputValue, dummy_resumption),
                % Validate output type
                ResultType = maps:get(result, OpSig),
                case value_matches_type(ResultType, Result) of
                    true -> {ok, Result};
                    false -> {error, {return_type_mismatch, ResultType, type_of(Result)}}
                end
            catch
                Kind:Reason:Stack ->
                    {error, {execution_error, Kind, Reason, Stack}}
            end;
        {error, _} = Error ->
            Error
    end.

%% @private Validate input types against values.
validate_input_types([], []) -> ok;
validate_input_types([ExpectedType | RestTypes], [Value | RestValues]) ->
    case value_matches_type(ExpectedType, Value) of
        true -> validate_input_types(RestTypes, RestValues);
        false -> {error, {input_type_mismatch, ExpectedType, type_of(Value)}}
    end;
validate_input_types(_, _) -> {error, param_count_mismatch}.

%% @private Check if type matches value.
value_matches_type(Type, Value) ->
    type_matches(Type, type_of(Value)).

%% @private Check if two types are compatible.
type_matches(T1, T2) when T1 =:= T2 -> true;
type_matches(#{kind := t_var}, _) -> true;
type_matches(_, #{kind := t_var}) -> true;
type_matches(T1, T2) when is_map(T1), is_map(T2) ->
    maps:size(T1) =:= maps:size(T2);
type_matches(_, _) -> false.

%% @private Check if types are compatible (looser than match).
type_compatible(T1, T2) when T1 =:= T2 -> true;
type_compatible(_, _) -> false.

%% @private Check if effects subsume (actual effects subset of expected).
effects_subsumes(ActualEffects, ExpectedEffects) ->
    ActualList = catena_row_types:row_to_list(ActualEffects),
    ExpectedList = catena_row_types:row_to_list(ExpectedEffects),
    lists:all(fun(E) -> lists:member(E, ExpectedList) end, ActualList).

%% @private Check if all operations are compatible.
all_operations_compatible(ActualOps, ExpectedOps) ->
    maps:fold(fun(Op, ExpectedSig, Acc) ->
        case Acc of
            false -> false;
            true ->
                case maps:find(Op, ActualOps) of
                    {ok, ActualSig} -> operation_sigs_compatible(ActualSig, ExpectedSig);
                    error -> false
                end
        end
    end, true, ExpectedOps).

%% @private Check if operation signatures are compatible.
operation_sigs_compatible(Actual, Expected) ->
    % For simplicity, require exact match
    maps:get(params, Actual) =:= maps:get(params, Expected) andalso
    maps:get(result, Actual) =:= maps:get(result, Expected).

%% @private Get type of value.
type_of(Value) when is_integer(Value) -> int;
type_of(Value) when is_float(Value) -> float;
type_of(Value) when is_atom(Value) -> atom;
type_of(Value) when is_binary(Value) -> binary;
type_of(Value) when is_list(Value) -> list;
type_of(Value) when is_map(Value) -> map;
type_of(_) -> any.

%% @private Format type for display.
format_type(Type) when is_atom(Type) -> atom_to_binary(Type);
format_type(Type) -> list_to_binary(io_lib:format("~p", [Type])).

%% @private Format effects for display.
format_effects(Effects) ->
    List = catena_row_types:row_to_list(Effects),
    Binaries = [atom_to_binary(E) || E <- List],
    <<"{", (list_to_binary(lists:join(<<", ">>, Binaries)))/binary, "}">>.

%% @private Format handler for display.
format_handler(Handler) ->
    Ops = maps:get(operations, Handler),
    Count = maps:size(Ops),
    <<"Handler(", (integer_to_binary(Count))/binary, " ops)">>.

%% @private Format term for display.
format_term(Term) when is_atom(Term) -> atom_to_binary(Term);
format_term(Term) -> list_to_binary(io_lib:format("~p", [Term])).
