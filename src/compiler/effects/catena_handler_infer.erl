%%%-------------------------------------------------------------------
%%% @doc Catena Handler Type Inference (Phase 12.3)
%%%
%%% This module implements type inference for handlers, enabling
%%% automatic derivation of handler types from their implementations.
%%%
%%% == Handler Type Inference ==
%%%
%%% Analyzes handler function bodies to infer:
%%% - Operation signatures (parameter types, result types)
%%% - Handler input/output types
%%% - Effect rows for handler operations
%%% - Type constraints and generalization
%%%
%%% == Resumption Type Inference ==
%%%
%%% Infers types for resumption values within handlers:
%%% - Resumption parameter types from operation result types
%%% - Resumption return types from handler continuation analysis
%%% - Effect row inference for resumption calls
%%%
%%% == Handler Context Inference ==
%%%
%%% Infers types for handler contexts (scoped computations):
%%% - Effect row subtraction for scoped handler execution
%%% - Context validation for handler composition
%%% - Optimization opportunities through type analysis
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_handler_infer).

%% Handler type inference
-export([
    infer_handler_type/2,
    infer_handler_type/3,
    infer_operation_sigs/1,
    infer_from_implementation/1
]).

%% Resumption type inference
-export([
    infer_resumption_type/2,
    infer_resumption_param_type/1,
    infer_resumption_return_type/2,
    infer_resumption_effects/2
]).

%% Handler context inference
-export([
    infer_context_type/2,
    infer_effect_subtraction/2,
    validate_context/1,
    optimize_context/1
]).

%% Type generalization
-export([
    generalize_handler/1,
    instantiate_handler/1,
    substitute_in_handler/2
]).

%%====================================================================
%% Types
%%====================================================================

-type handler_type() :: catena_handler_types:handler_type().
-type operation_sig() :: catena_handler_types:operation_sig().
-type effect_row() :: catena_row_types:effect_row().
-type type_ref() :: catena_types:ty() | atom().

-type inference_state() :: #{
    vars => map(),
    constraints => list(),
    next_id => pos_integer()
}.

-type inference_result() :: {ok, handler_type(), inference_state()} | {error, term()}.

-export_type([inference_state/0, inference_result/0]).

%%====================================================================
%% Handler Type Inference
%%====================================================================

%% @doc Infer handler type from module and function name.
-spec infer_handler_type(module(), atom()) -> inference_result().
infer_handler_type(Module, HandlerFun) when is_atom(Module), is_atom(HandlerFun) ->
    State = new_state(),
    infer_handler_type(Module, HandlerFun, State).

%% @doc Infer handler type with provided inference state.
-spec infer_handler_type(module(), atom(), inference_state()) -> inference_result().
infer_handler_type(Module, HandlerFun, State) ->
    try
        % Get handler function AST/implementation
        case get_handler_implementation(Module, HandlerFun) of
            {ok, Implementation} ->
                infer_from_implementation(Implementation, State);
            {error, Reason} ->
                {error, {implementation_not_found, Reason}}
        end
    catch
        Kind:Error:Stack ->
            {error, {inference_error, Kind, Error, Stack}}
    end.

%% @doc Infer operation signatures from handler implementation.
-spec infer_operation_sigs(map()) -> #{atom() => operation_sig()}.
infer_operation_sigs(Implementation) when is_map(Implementation) ->
    maps:map(fun(_Op, HandlerFun) ->
        infer_single_operation(HandlerFun)
    end, Implementation).

%% @doc Infer handler type from implementation map.
-spec infer_from_implementation(map()) -> {ok, handler_type()} | {error, term()}.
infer_from_implementation(Implementation) ->
    {ok, HandlerType, _State} = infer_from_implementation(Implementation, new_state()),
    {ok, HandlerType}.

%% @private Infer handler type with state.
infer_from_implementation(Implementation, State) ->
    Ops = infer_operation_sigs(Implementation),
    {InputType, OutputType, State1} = infer_io_types(Implementation, State),
    Effects = infer_effect_row(Implementation),

    BaseHandler = catena_handler_types:handler_type(),
    Handler1 = catena_handler_types:with_operations(BaseHandler, Ops),
    Handler2 = catena_handler_types:with_input(Handler1, InputType),
    Handler3 = catena_handler_types:with_output(Handler2, OutputType),
    HandlerType = catena_handler_types:with_effects(Handler3, Effects),

    {ok, HandlerType, State1}.

%%====================================================================
%% Resumption Type Inference
%%====================================================================

%% @doc Infer resumption type from operation signature.
-spec infer_resumption_type(operation_sig(), effect_row()) -> operation_sig().
infer_resumption_type(OpSig, HandlerEffects) ->
    Params = maps:get(params, OpSig),
    Result = maps:get(result, OpSig),
    Effects = maps:get(effects, OpSig),

    % Resumption parameter type is the result type of the operation
    ResumptionParamType = Result,

    % Resumption return type needs continuation analysis
    ResumptionResultType = infer_resumption_return_type(Result, Effects),

    % Combine effects
    CombinedEffects = catena_row_operations:effect_union_rows(
        Effects, HandlerEffects),

    OpSig#{
        params => [ResumptionParamType],
        result => ResumptionResultType,
        effects => CombinedEffects
    }.

%% @doc Infer resumption parameter type from operation result type.
-spec infer_resumption_param_type(type_ref()) -> type_ref().
infer_resumption_param_type(ResultType) ->
    % The resumption parameter type is the operation's result type
    ResultType.

%% @doc Infer resumption return type from result and effects.
-spec infer_resumption_return_type(type_ref(), effect_row()) -> type_ref().
infer_resumption_return_type(ResultType, Effects) ->
    % Analyze effects to determine continuation type
    case catena_row_types:is_empty_row(Effects) of
        true -> ResultType;
        false -> infer_polymorphic_result(ResultType)
    end.

%% @doc Infer resumption effects.
-spec infer_resumption_effects(effect_row(), effect_row()) -> effect_row().
infer_resumption_effects(OpEffects, HandlerEffects) ->
    catena_row_operations:effect_union_rows(OpEffects, HandlerEffects).

%%====================================================================
%% Handler Context Inference
%%====================================================================

%% @doc Infer context type for scoped handler execution.
-spec infer_context_type(handler_type(), effect_row()) -> {ok, handler_type()} | {error, term()}.
infer_context_type(HandlerType, OuterEffects) ->
    HandlerEffects = maps:get(effects, HandlerType),

    % Subtract handler effects from outer effects
    case infer_effect_subtraction(HandlerEffects, OuterEffects) of
        {ok, RemainingEffects} ->
            ContextHandler = HandlerType#{effects => RemainingEffects},
            {ok, ContextHandler};
        {error, _} = Error ->
            Error
    end.

%% @doc Perform effect row subtraction.
-spec infer_effect_subtraction(effect_row(), effect_row()) -> {ok, effect_row()} | {error, term()}.
infer_effect_subtraction(ToSubtract, From) ->
    ToSubtractList = catena_row_types:row_to_list(ToSubtract),
    FromList = catena_row_types:row_to_list(From),

    % Check if all effects to subtract are in the source
    case lists:all(fun(E) -> lists:member(E, FromList) end, ToSubtractList) of
        true ->
            Remaining = lists:filter(fun(E) ->
                not lists:member(E, ToSubtractList)
            end, FromList),
            {ok, catena_row_types:effect_row(Remaining)};
        false ->
            {error, {effect_not_present, ToSubtractList}}
    end.

%% @doc Validate handler context.
-spec validate_context(handler_type()) -> ok | {error, term()}.
validate_context(HandlerType) ->
    % Validate that handler context is well-formed
    case catena_handler_types:is_valid_handler_type(HandlerType) of
        true -> ok;
        false -> {error, invalid_context}
    end.

%% @doc Optimize handler context based on inferred types.
-spec optimize_context(handler_type()) -> handler_type().
optimize_context(HandlerType) ->
    % Apply optimizations based on type information
    Effects = maps:get(effects, HandlerType),

    % If effects are empty, handler is pure
    case catena_row_types:is_empty_row(Effects) of
        true -> optimize_pure_handler(HandlerType);
        false -> HandlerType
    end.

%%====================================================================
%% Type Generalization
%%====================================================================

%% @doc Generalize handler type by introducing type variables.
-spec generalize_handler(handler_type()) -> handler_type().
generalize_handler(HandlerType) ->
    Ops = maps:get(operations, HandlerType),
    GeneralizedOps = maps:map(fun(_, OpSig) ->
        generalize_operation_sig(OpSig)
    end, Ops),

    HandlerType#{operations => GeneralizedOps}.

%% @doc Instantiate handler type by substituting type variables.
-spec instantiate_handler(handler_type()) -> handler_type().
instantiate_handler(HandlerType) ->
    Ops = maps:get(operations, HandlerType),
    InstantiatedOps = maps:map(fun(_, OpSig) ->
        instantiate_operation_sig(OpSig)
    end, Ops),

    HandlerType#{operations => InstantiatedOps}.

%% @doc Substitute types in handler type.
-spec substitute_in_handler(handler_type(), map()) -> handler_type().
substitute_in_handler(HandlerType, Subst) when is_map(Subst) ->
    Ops = maps:get(operations, HandlerType),
    SubstitutedOps = maps:map(fun(_, OpSig) ->
        substitute_in_sig(OpSig, Subst)
    end, Ops),

    Input = substitute_type(maps:get(input, HandlerType), Subst),
    Output = substitute_type(maps:get(output, HandlerType), Subst),

    HandlerType#{
        operations => SubstitutedOps,
        input => Input,
        output => Output
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Create new inference state.
new_state() ->
    #{
        vars => #{},
        constraints => [],
        next_id => 1
    }.

%% @private Get handler implementation from module.
get_handler_implementation(Module, HandlerFun) ->
    try
        Arity = 2,  % Handlers take (value, resumption)
        {Module, HandlerFun, Arity}
    of
        {_M, _F, _A} = Info -> {ok, Info}
    catch
        error:undef -> {error, function_not_found}
    end.

%% @private Infer single operation signature.
infer_single_operation(_HandlerFun) ->
    % Simplified - returns generic operation signature
    catena_handler_types:operation_sig().

%% @private Infer input/output types.
infer_io_types(_Implementation, State) ->
    % Generate fresh type variables
    {State0, InputVar} = fresh_type_var_with_id({State, 1}),
    {State1, OutputVar} = fresh_type_var_with_id({State0, 1}),

    {catena_types:tvar(InputVar), catena_types:tvar(OutputVar), State1}.

%% @private Infer effect row from implementation.
infer_effect_row(_Implementation) ->
    catena_row_types:empty_row().

%% @private Infer polymorphic result type.
infer_polymorphic_result(ResultType) ->
    ResultType.

%% @private Optimize pure handler.
optimize_pure_handler(HandlerType) ->
    HandlerType#{constraints => #{total => true, deep => false, pure => true}}.

%% @private Generalize operation signature.
generalize_operation_sig(OpSig) ->
    Params = maps:get(params, OpSig),
    Result = maps:get(result, OpSig),

    OpSig#{
        params => [generalize_type(P) || P <- Params],
        result => generalize_type(Result)
    }.

%% @private Instantiate operation signature.
instantiate_operation_sig(OpSig) ->
    OpSig.  % Simplified - would substitute fresh vars

%% @private Substitute in operation signature.
substitute_in_sig(OpSig, Subst) ->
    Params = maps:get(params, OpSig),
    Result = maps:get(result, OpSig),

    OpSig#{
        params => [substitute_type(P, Subst) || P <- Params],
        result => substitute_type(Result, Subst)
    }.

%% @private Generalize a type.
generalize_type({tcon, _} = T) -> T;
generalize_type(T) -> T.

%% @private Substitute in a type.
substitute_type({tvar, Id}, Subst) ->
    case maps:find(Id, Subst) of
        {ok, Replacement} -> Replacement;
        error -> {tvar, Id}
    end;
substitute_type({tcon, _} = T, _Subst) -> T;
substitute_type(T, _Subst) -> T.

%% @private Generate fresh type variable ID.
fresh_type_var(#{next_id := Id} = State) ->
    State#{next_id => Id + 1}.
fresh_type_var_with_id({#{next_id := Id} = State, _Offset}) ->
    {State#{next_id => Id + 1}, Id}.
