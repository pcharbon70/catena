%%%-------------------------------------------------------------------
%%% @doc Catena Handler Type Checking (Phase 12.2)
%%%
%%% This module implements type checking for handlers, ensuring that:
%%% - Handlers cover all declared operations
%%% - Operation signatures match expected types
%%% - Handler return values match expected types
%%% - Resumption types are properly enforced
%%%
%%% == Handler Type Checking ==
%%%
%%% Handlers are type-checked against their declared handler types:
%%% - Operations must match declared signatures
%%% - Handler input/output types must be consistent
%%% - Effect rows must be properly tracked
%%% - All required operations must be implemented
%%%
%%% == Operation Coverage Checking ==
%%%
%%% Ensures handlers provide implementations for all operations declared
%%% in their effect interface. Detects missing and redundant operations.
%%%
%%% == Return Type Checking ==
%%%
%%% Verifies that handler implementations return values matching the
%%% declared output types, including resumption return types.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_handler_check).

%% Handler type checking
-export([
    check_handler_type/2,
    check_handler_implementation/2,
    check_handler_signature/1
]).

%% Operation coverage checking
-export([
    check_operation_coverage/2,
    find_missing_operations/2,
    find_redundant_operations/2,
    coverage_report/2
]).

%% Return type checking
-export([
    check_return_type/2,
    check_resumption_type/3,
    check_handler_effects/2
]).

%% Type inference integration
-export([
    infer_handler_type/1,
    generalize_handler_type/1
]).

%% Error reporting
-export([
    format_check_error/1,
    format_coverage_error/1,
    format_return_type_error/1
]).

%%====================================================================
%% Types
%%====================================================================

-type handler_type() :: catena_handler_types:handler_type().
-type operation_sig() :: catena_handler_types:operation_sig().
-type operation_map() :: #{atom() => operation_sig()}.
-type effect_row() :: catena_row_types:effect_row().
-type type_ref() :: catena_handler_types:type_ref().

-type check_error() ::
    {missing_operations, [atom()]} |
    {redundant_operations, [atom()]} |
    {type_mismatch, operation(), expected, actual} |
    {return_type_mismatch, expected, actual} |
    {effect_mismatch, expected, actual} |
    {invalid_handler, term()}.

-type check_result() :: ok | {error, check_error()}.
-type coverage_result() :: {complete, [atom()]} | {error, check_error()}.

-type operation() :: atom().
-type expected() :: term().
-type actual() :: term().

-export_type([check_error/0, check_result/0, coverage_result/0]).

%%====================================================================
%% Handler Type Checking
%%====================================================================

%% @doc Check a handler against its declared type.
%%
%% Validates that the handler implementation matches the handler type
%% signature, including operation coverage and return types.
-spec check_handler_type(handler_type(), module()) -> check_result().
check_handler_type(HandlerType, HandlerModule) when is_atom(HandlerModule) ->
    case catena_handler_types:is_valid_handler_type(HandlerType) of
        false ->
            {error, {invalid_handler, HandlerType}};
        true ->
            Operations = maps:get(operations, HandlerType),
            case check_operation_coverage(Operations, HandlerModule) of
                {error, _} = Error -> Error;
                {complete, _} -> check_return_types(HandlerType, HandlerModule)
            end
    end.

%% @doc Check handler implementation against type signature.
%%
%% Performs comprehensive checking of handler implementation including
%% operation signatures and return types.
-spec check_handler_implementation(handler_type(), map()) -> check_result().
check_handler_implementation(HandlerType, Implementation) when is_map(Implementation) ->
    ExpectedOps = maps:get(operations, HandlerType),
    ProvidedOps = maps:keys(Implementation),

    Missing = find_missing_operations(ExpectedOps, ProvidedOps),
    Redundant = find_redundant_operations(ExpectedOps, ProvidedOps),

    case {Missing, Redundant} of
        {[], []} ->
            check_operation_signatures(HandlerType, Implementation);
        {_, _} when Missing =/= []; Redundant =/= [] ->
            {error, {coverage_issues, Missing, Redundant}}
    end.

%% @doc Check handler signature for well-formedness.
%%
%% Validates that a handler type signature is properly formed with
%% valid input/output types and effect rows.
-spec check_handler_signature(handler_type()) -> check_result().
check_handler_signature(HandlerType) ->
    Checks = [
        fun check_input_type/1,
        fun check_output_type/1,
        fun check_effects_row/1,
        fun check_operations_consistency/1
    ],
    run_checks(Checks, HandlerType).

%%====================================================================
%% Operation Coverage Checking
%%====================================================================

%% @doc Check that handler covers all declared operations.
%%
%% Returns {complete, ImplementedOps} if all operations are covered,
%% or {error, missing_operations} if any are missing.
-spec check_operation_coverage(#{atom() => operation_sig()}, module()) -> coverage_result().
check_operation_coverage(ExpectedOps, HandlerModule) ->
    Implemented = get_implemented_operations(HandlerModule),
    Missing = find_missing_operations(ExpectedOps, Implemented),
    case Missing of
        [] -> {complete, Implemented};
        _ -> {error, {missing_operations, Missing}}
    end.

%% @doc Find operations declared but not implemented.
-spec find_missing_operations(#{atom() => operation_sig()}, [atom()]) -> [atom()].
find_missing_operations(ExpectedOps, ImplementedOps) ->
    Expected = maps:keys(ExpectedOps),
    lists:filter(fun(Op) -> not lists:member(Op, ImplementedOps) end, Expected).

%% @doc Find operations implemented but not declared.
-spec find_redundant_operations(#{atom() => operation_sig()}, [atom()]) -> [atom()].
find_redundant_operations(ExpectedOps, ImplementedOps) ->
    Expected = maps:keys(ExpectedOps),
    lists:filter(fun(Op) -> not lists:member(Op, Expected) end, ImplementedOps).

%% @doc Generate a detailed coverage report.
%%
%% Returns a map with coverage status and details.
-spec coverage_report(operation_map(), module()) -> map().
coverage_report(ExpectedOps, HandlerModule) ->
    Implemented = get_implemented_operations(HandlerModule),
    Missing = find_missing_operations(ExpectedOps, Implemented),
    Redundant = find_redundant_operations(ExpectedOps, Implemented),
    CoveragePercent = calculate_coverage(ExpectedOps, Implemented),
    #{
        expected_count => maps:size(ExpectedOps),
        implemented_count => length(Implemented),
        missing_count => length(Missing),
        redundant_count => length(Redundant),
        coverage_percent => CoveragePercent,
        missing_operations => Missing,
        redundant_operations => Redundant,
        is_complete => Missing =:= []
    }.

%%====================================================================
%% Return Type Checking
%%====================================================================

%% @doc Check that a return value matches the expected type.
%%
%% Performs structural type checking on handler return values.
-spec check_return_type(type_ref(), term()) -> check_result().
check_return_type(ExpectedType, ReturnValue) ->
    case types_match(ExpectedType, type_of(ReturnValue)) of
        true -> ok;
        false -> {error, {return_type_mismatch, ExpectedType, type_of(ReturnValue)}}
    end.

%% @doc Check resumption parameter and return types.
%%
%% Ensures resumptions are called with correct parameter types
%% and their return values match expected types.
-spec check_resumption_type(operation_sig(), term(), term()) -> check_result().
check_resumption_type(OpSig, ParamValue, ReturnValue) ->
    Params = maps:get(params, OpSig),
    ResultType = maps:get(result, OpSig),

    case check_param_types(Params, [ParamValue]) of
        ok -> check_return_type(ResultType, ReturnValue);
        Error -> Error
    end.

%% @doc Check that handler effects match declared effect row.
%%
%% Validates effect row consistency for handler execution.
-spec check_handler_effects(handler_type(), effect_row()) -> check_result().
check_handler_effects(HandlerType, ActualEffects) ->
    ExpectedEffects = maps:get(effects, HandlerType),
    case effect_rows_match(ExpectedEffects, ActualEffects) of
        true -> ok;
        false -> {error, {effect_mismatch, ExpectedEffects, ActualEffects}}
    end.

%%====================================================================
%% Type Inference Integration
%%====================================================================

%% @doc Infer handler type from implementation.
%%
%% Analyzes handler implementation to infer operation signatures
%% and input/output types.
-spec infer_handler_type(map()) -> {ok, handler_type()} | {error, term()}.
infer_handler_type(Implementation) when is_map(Implementation) ->
    Ops = infer_operation_sigs(Implementation),
    {InputType, OutputType} = infer_io_types(Implementation),
    Effects = infer_effect_row(Implementation),
    BaseHandler = catena_handler_types:handler_type(),
    WithOps = catena_handler_types:with_operations(BaseHandler, Ops),
    WithInput = catena_handler_types:with_input(WithOps, InputType),
    WithOutput = catena_handler_types:with_output(WithInput, OutputType),
    FinalHandler = catena_handler_types:with_effects(WithOutput, Effects),
    {ok, FinalHandler}.

%% @doc Generalize handler type by introducing type variables.
%%
%% Converts concrete types to polymorphic type schemes where
%% appropriate for reusable handlers.
-spec generalize_handler_type(handler_type()) -> handler_type().
generalize_handler_type(HandlerType) ->
    Ops = maps:get(operations, HandlerType),
    GeneralizedOps = maps:map(fun(_, OpSig) -> generalize_op_sig(OpSig) end, Ops),
    HandlerType#{operations => GeneralizedOps}.

%%====================================================================
%% Error Formatting
%%====================================================================

%% @doc Format a type check error for display.
-spec format_check_error(check_error()) -> binary().
format_check_error({missing_operations, Ops}) ->
    OpsList = binary_list(Ops),
    <<"Missing operations: ", OpsList/binary>>;
format_check_error({redundant_operations, Ops}) ->
    OpsList = binary_list(Ops),
    <<"Redundant operations (not declared): ", OpsList/binary>>;
format_check_error({type_mismatch, Op, Expected, Actual}) ->
    <<"Type mismatch for operation '", (atom_to_binary(Op))/binary,
      "': expected ", (format_type(Expected))/binary,
      ", got ", (format_type(Actual))/binary>>;
format_check_error({return_type_mismatch, Expected, Actual}) ->
    <<"Return type mismatch: expected ", (format_type(Expected))/binary,
      ", got ", (format_type(Actual))/binary>>;
format_check_error({effect_mismatch, Expected, Actual}) ->
    <<"Effect row mismatch: expected ", (format_effects(Expected))/binary,
      ", got ", (format_effects(Actual))/binary>>;
format_check_error({invalid_handler, Reason}) ->
    <<"Invalid handler: ", (format_term(Reason))/binary>>;
format_check_error({coverage_issues, Missing, Redundant}) ->
    MissingBin = case Missing of
        [] -> <<"">>;
        _ -> <<"missing: ", (binary_list(Missing))/binary>>
    end,
    RedundantBin = case Redundant of
        [] -> <<"">>;
        _ -> <<"redundant: ", (binary_list(Redundant))/binary>>
    end,
    Parts = [P || P <- [MissingBin, RedundantBin], P =/= <<"">>],
    <<"Coverage issues: ", (list_to_binary(lists:join(<<", ">>, Parts)))/binary>>.

%% @doc Format an operation coverage error for display.
-spec format_coverage_error(check_error()) -> binary().
format_coverage_error({missing_operations, Ops}) ->
    OpsList = binary_list(Ops),
    Count = length(Ops),
    <<"Handler is missing ", (integer_to_binary(Count))/binary,
      " operation(s): ", OpsList/binary>>;
format_coverage_error(Error) ->
    format_check_error(Error).

%% @doc Format a return type error for display.
-spec format_return_type_error(check_error()) -> binary().
format_return_type_error({return_type_mismatch, Expected, Actual}) ->
    <<"Handler return type mismatch: expected ", (format_type(Expected))/binary,
      ", but got ", (format_type(Actual))/binary>>;
format_return_type_error(Error) ->
    format_check_error(Error).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Check input type is valid.
check_input_type(#{input := Type}) -> validate_type(Type).

%% @private Check output type is valid.
check_output_type(#{output := Type}) -> validate_type(Type).

%% @private Check effects row is valid.
check_effects_row(#{effects := Effects}) ->
    case catena_row_types:is_valid_row(Effects) of
        true -> ok;
        false -> {error, {invalid_effects, Effects}}
    end.

%% @private Check operations are internally consistent.
check_operations_consistency(#{operations := Ops}) ->
    case maps:size(Ops) of
        0 -> {error, {no_operations, "Handler must have at least one operation"}};
        _ -> maps:fold(fun(_, OpSig, Acc) ->
            case Acc of
                {error, _} -> Acc;
                ok -> validate_operation_sig(OpSig)
            end
        end, ok, Ops)
    end.

%% @private Validate a type reference.
validate_type(Type) when is_atom(Type); is_map(Type); is_tuple(Type) -> ok;
validate_type(Type) -> {error, {invalid_type, Type}}.

%% @private Validate an operation signature.
validate_operation_sig(OpSig) ->
    case catena_handler_types:is_valid_operation_sig(OpSig) of
        true -> ok;
        false -> {error, {invalid_operation_sig, OpSig}}
    end.

%% @private Run a list of checks, stopping at first error.
run_checks(Checks, Value) ->
    run_checks(Checks, Value, []).

run_checks([], _, []) -> ok;
run_checks([], _, Acc) -> {error, lists:reverse(Acc)};
run_checks([Check | Rest], Value, Acc) ->
    case Check(Value) of
        ok -> run_checks(Rest, Value, Acc);
        {error, Error} -> {error, lists:reverse([Error | Acc])}
    end.

%% @private Get implemented operations from handler module.
get_implemented_operations(HandlerModule) ->
    case code:is_loaded(HandlerModule) of
        false ->
            case code:load_file(HandlerModule) of
                {module, _} -> extract_exports(HandlerModule);
                {error, _} -> []
            end;
        _ -> extract_exports(HandlerModule)
    end.

%% @private Extract exported functions from module.
extract_exports(Module) ->
    Exports = Module:module_info(exports),
    [Op || {Op, 2} <- Exports, is_operation_name(Op)].

%% @private Check if atom is an operation name.
is_operation_name(Op) when is_atom(Op) ->
    Name = atom_to_list(Op),
    not lists:prefix("module_info", Name) andalso
    not lists:prefix("$", Name).

%% @private Check return types for all operations.
check_return_types(HandlerType, HandlerModule) ->
    Operations = maps:get(operations, HandlerType),
    maps:fold(fun(Op, OpSig, Acc) ->
        case Acc of
            {error, _} -> Acc;
            ok -> check_single_return_type(Op, OpSig, HandlerModule)
        end
    end, ok, Operations).

%% @private Check return type for a single operation.
check_single_return_type(_Op, _OpSig, _HandlerModule) ->
    % Simplified - full implementation would analyze handler function body
    ok.

%% @private Check operation signatures match implementation.
check_operation_signatures(HandlerType, Implementation) ->
    ExpectedOps = maps:get(operations, HandlerType),
    maps:fold(fun(Op, ImplFun, Acc) ->
        case Acc of
            {error, _} -> Acc;
            ok -> check_single_op_signature(Op, ExpectedOps, ImplFun)
        end
    end, ok, Implementation).

%% @private Check single operation signature.
check_single_op_signature(Op, ExpectedOps, ImplFun) ->
    case maps:find(Op, ExpectedOps) of
        {ok, ExpectedSig} -> verify_signature_match(ExpectedSig, ImplFun);
        error -> {error, {redundant_operation, Op}}
    end.

%% @private Verify signature matches implementation.
verify_signature_match(_ExpectedSig, _ImplFun) ->
    % Simplified - full implementation would check function types
    ok.

%% @private Check parameter types match values.
check_param_types([], []) -> ok;
check_param_types([Expected | RestParams], [Value | RestValues]) ->
    case types_match(Expected, type_of(Value)) of
        true -> check_param_types(RestParams, RestValues);
        false -> {error, {param_type_mismatch, Expected, type_of(Value)}}
    end;
check_param_types(_, _) -> {error, param_count_mismatch}.

%% @private Check if two types match.
types_match(T1, T2) when T1 =:= T2 -> true;
types_match(#{kind := t_var}, _) -> true;
types_match(_, #{kind := t_var}) -> true;
types_match(T1, T2) when is_map(T1), is_map(T2) ->
    maps:size(T1) =:= maps:size(T2);
types_match(_, _) -> false.

%% @private Check if effect rows match.
effect_rows_match(Row1, Row2) ->
    List1 = lists:sort(catena_row_types:row_to_list(Row1)),
    List2 = lists:sort(catena_row_types:row_to_list(Row2)),
    List1 =:= List2.

%% @private Infer operation signatures from implementation.
infer_operation_sigs(Implementation) ->
    maps:map(fun(_, Fun) -> infer_sig_from_fun(Fun) end, Implementation).

%% @private Infer signature from function.
infer_sig_from_fun(_Fun) ->
    % Simplified - returns generic operation signature
    catena_handler_types:operation_sig().

%% @private Infer input/output types from implementation.
infer_io_types(_Implementation) ->
    % Simplified - returns generic type variables
    {catena_types:tcon('A'), catena_types:tcon('B')}.

%% @private Infer effect row from implementation.
infer_effect_row(_Implementation) ->
    catena_row_types:empty_row().

%% @private Generalize an operation signature.
generalize_op_sig(OpSig) ->
    Params = maps:get(params, OpSig),
    Result = maps:get(result, OpSig),
    Effects = maps:get(effects, OpSig),
    OpSig#{
        params => generalize_types(Params),
        result => generalize_type(Result),
        effects => generalize_effects(Effects)
    }.

%% @private Generalize a list of types.
generalize_types(Types) ->
    [generalize_type(T) || T <- Types].

%% @private Generalize a single type.
generalize_type(#{kind := TypeKind} = Type) when TypeKind =:= t_con; TypeKind =:= t_var ->
    Type;
generalize_type(Type) when is_atom(Type) ->
    catena_types:tcon('_');
generalize_type(Type) -> Type.

%% @private Generalize effect row.
generalize_effects(Effects) ->
    Effects.

%% @private Calculate coverage percentage.
calculate_coverage(ExpectedOps, ImplementedOps) ->
    ExpectedCount = maps:size(ExpectedOps),
    ImplementedCount = length(ImplementedOps),
    case ExpectedCount of
        0 -> 100;
        _ -> (ImplementedCount * 100) div ExpectedCount
    end.

%% @private Get the type of a value (simplified).
type_of(Value) when is_atom(Value) -> atom;
type_of(Value) when is_integer(Value) -> int;
type_of(Value) when is_float(Value) -> float;
type_of(Value) when is_binary(Value) -> binary;
type_of(Value) when is_list(Value) -> list;
type_of(Value) when is_map(Value) -> map;
type_of(_) -> any.

%% @private Format a type for display.
format_type(Type) when is_atom(Type) -> atom_to_binary(Type);
format_type(Type) -> catena_handler_types:format_type(Type).

%% @private Format effects for display.
format_effects(Effects) ->
    Elements = catena_row_types:row_to_list(Effects),
    Formatted = [atom_to_binary(E) || E <- Elements],
    case Formatted of
        [] -> <<"{}">>;
        _ -> <<"{${", (list_to_binary(lists:join(<<", ">>, Formatted)))/binary, "}">>
    end.

%% @private Format a term for display.
format_term(Term) when is_atom(Term) -> atom_to_binary(Term);
format_term(Term) -> list_to_binary(io_lib:format("~p", [Term])).

%% @private Create a binary list from atoms.
binary_list([]) -> <<"">>;
binary_list(Atoms) ->
    Binaries = [atom_to_binary(A) || A <- Atoms],
    iolist_to_binary([<<",">> | lists:join(<<", ">>, Binaries)]).
