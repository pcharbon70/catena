%%%-------------------------------------------------------------------
%%% @doc Catena Higher-Order Effect Execution (Phase 13.4)
%%%
%%% This module executes higher-order effect operations against explicit
%%% higher-order operation signatures. It keeps execution-state effects,
%%% registered operation signatures, and handler-side effect propagation
%%% in one runtime context.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_ho_execution).

%% Higher-order operation execution
-export([
    execute_ho_op/4,
    execute_with_params/3,
    invoke_effectful_param/2
]).

%% Effectual handler invocation
-export([
    invoke_effectual_handler/3,
    propagate_handler_effects/2,
    capture_effect_context/1,
    restore_effect_context/2
]).

%% Higher-order effect optimization
-export([
    fuse_effectual_handlers/1,
    inline_simple_handlers/1,
    specialize_ho_effects/2,
    eliminate_dead_effects/1
]).

%% Execution context management
-export([
    new_context/0,
    context_effects/1,
    context_add_effects/2,
    context_merge/2,
    context_clone/1,
    context_register_signature/4,
    context_lookup_signature/3,
    context_signatures/1
]).

%% Error handling
-export([
    handle_execution_error/3,
    recover_from_error/2,
    is_recoverable/1
]).

%% Accessor functions
-export([
    context_error_state/1,
    context_with_error/1,
    context_with_effects/1
]).

-export_type([
    execution_context/0,
    ho_result/0,
    handler_result/0
]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

-record(execution_context, {
    effects :: catena_row_types:effect_row(),
    handler_stack :: [term()],
    error_state :: normal | {error, term()},
    signatures :: #{{atom(), atom()} => term()},
    history :: [{atom(), atom(), [term()]}]
}).

-type execution_context() :: #execution_context{}.
-type ho_result() :: {ok, term(), execution_context()} | {error, term(), execution_context()}.
-type handler_result() :: {ok, term(), catena_row_types:effect_row()} | {error, term()}.

%%%---------------------------------------------------------------------
%%% Higher-Order Operation Execution
%%%---------------------------------------------------------------------

-spec execute_ho_op(atom(), atom(), [term()], execution_context()) -> ho_result().
execute_ho_op(Effect, Operation, Args, Context) ->
    Signature = context_lookup_signature(Context, Effect, Operation),
    BaseContext = append_history(Context, Effect, Operation, Args),
    try
        case classify_operation(Signature, Args) of
            higher_order ->
                execute_higher_order(Signature, Effect, Operation, Args, BaseContext);
            first_order ->
                execute_first_order(Signature, Effect, Operation, Args, BaseContext)
        end
    catch
        Class:Reason ->
            ErrorContext = BaseContext#execution_context{error_state = {error, {Class, Reason}}},
            {error, Reason, ErrorContext}
    end.

-spec execute_with_params(atom(), atom(), [term()]) -> ho_result().
execute_with_params(Effect, Operation, Params) ->
    execute_ho_op(Effect, Operation, Params, new_context()).

-spec invoke_effectful_param(function(), [term()]) -> {ok, term()} | {error, term()}.
invoke_effectful_param(Fun, Args) when is_function(Fun), is_list(Args) ->
    try
        {ok, invoke_fun(Fun, Args)}
    catch
        _:Reason ->
            {error, Reason}
    end.

%%%---------------------------------------------------------------------
%%% Effectual Handler Invocation
%%%---------------------------------------------------------------------

-spec invoke_effectual_handler(function(), [term()], execution_context()) -> handler_result().
invoke_effectual_handler(HandlerFn, Args, Context) ->
    try
        Result = invoke_fun(HandlerFn, Args),
        {Value, HandlerEffects} = extract_handler_result(Result),
        FinalEffects = catena_row_types:row_union(context_effects(Context), HandlerEffects),
        {ok, Value, FinalEffects}
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec propagate_handler_effects(catena_row_types:effect_row(), execution_context()) -> execution_context().
propagate_handler_effects(Effects, Context) ->
    context_add_effects(Context, Effects).

-spec capture_effect_context(execution_context()) -> catena_row_types:effect_row().
capture_effect_context(Context) ->
    context_effects(Context).

-spec restore_effect_context(catena_row_types:effect_row(), execution_context()) -> execution_context().
restore_effect_context(Effects, Context) ->
    Context#execution_context{effects = normalize_effect_row(Effects)}.

%%%---------------------------------------------------------------------
%%% Higher-Order Effect Optimization
%%%---------------------------------------------------------------------

-spec fuse_effectual_handlers([term()]) -> [term()].
fuse_effectual_handlers(Handlers) when is_list(Handlers) ->
    lists:reverse(
        lists:foldl(
            fun
                ({handler, Name, Fun}, [{handler, Name, _OldFun} | Rest]) ->
                    [{handler, Name, Fun} | Rest];
                (Handler, Acc) ->
                    [Handler | Acc]
            end,
            [],
            Handlers
        )
    ).

-spec inline_simple_handlers([term()]) -> [term()].
inline_simple_handlers(Handlers) when is_list(Handlers) ->
    [case Handler of
        {handler, Name, Fun} when is_function(Fun) ->
            {inlined_handler, Name, Fun};
        Other ->
            Other
    end || Handler <- Handlers].

-spec specialize_ho_effects(term(), catena_row_types:effect_row()) -> term().
specialize_ho_effects(HOType, KnownEffects) ->
    case catena_ho_effects:is_ho_op(HOType) of
        true -> catena_ho_effects:substitute_ho_effects(HOType, KnownEffects);
        false -> HOType
    end.

-spec eliminate_dead_effects(term()) -> term().
eliminate_dead_effects({dead_effect, _Effect}) ->
    none;
eliminate_dead_effects(List) when is_list(List) ->
    [Item || Item <- List, eliminate_dead_effects(Item) =/= none];
eliminate_dead_effects(Term) ->
    Term.

%%%---------------------------------------------------------------------
%%% Execution Context Management
%%%---------------------------------------------------------------------

-spec new_context() -> execution_context().
new_context() ->
    #execution_context{
        effects = catena_row_types:empty_row(),
        handler_stack = [],
        error_state = normal,
        signatures = #{},
        history = []
    }.

-spec context_effects(execution_context()) -> catena_row_types:effect_row().
context_effects(#execution_context{effects = Effects}) ->
    Effects.

-spec context_add_effects(execution_context(), catena_row_types:effect_row()) -> execution_context().
context_add_effects(Context, NewEffects) ->
    Merged = catena_row_types:row_union(context_effects(Context), normalize_effect_row(NewEffects)),
    Context#execution_context{effects = Merged}.

-spec context_merge(execution_context(), execution_context()) -> execution_context().
context_merge(Context1, Context2) ->
    #execution_context{
        effects = catena_row_types:row_union(context_effects(Context1), context_effects(Context2)),
        handler_stack = lists:usort(Context1#execution_context.handler_stack ++ Context2#execution_context.handler_stack),
        error_state = merge_error_state(context_error_state(Context1), context_error_state(Context2)),
        signatures = maps:merge(context_signatures(Context1), context_signatures(Context2)),
        history = Context1#execution_context.history ++ Context2#execution_context.history
    }.

-spec context_clone(execution_context()) -> execution_context().
context_clone(Context) ->
    Context.

-spec context_register_signature(execution_context(), atom(), atom(), term()) -> execution_context().
context_register_signature(Context, Effect, Operation, Signature) ->
    Signatures = context_signatures(Context),
    Context#execution_context{
        signatures = Signatures#{{Effect, Operation} => Signature}
    }.

-spec context_lookup_signature(execution_context(), atom(), atom()) -> term() | undefined.
context_lookup_signature(Context, Effect, Operation) ->
    case maps:find({Effect, Operation}, context_signatures(Context)) of
        {ok, Signature} ->
            Signature;
        error ->
            lookup_effect_system_signature(Effect, Operation)
    end.

-spec context_signatures(execution_context()) -> #{{atom(), atom()} => term()}.
context_signatures(#execution_context{signatures = Signatures}) ->
    Signatures.

%%%---------------------------------------------------------------------
%%% Error handling
%%%---------------------------------------------------------------------

-spec handle_execution_error(term(), execution_context(), function()) -> ho_result().
handle_execution_error(Reason, Context, RecoveryFn) ->
    case is_recoverable(Reason) of
        true ->
            case RecoveryFn(Reason) of
                {ok, Value} ->
                    {ok, Value, Context#execution_context{error_state = normal}};
                {error, RecoveryReason} ->
                    {error, RecoveryReason, Context#execution_context{error_state = {error, RecoveryReason}}};
                Value ->
                    {ok, Value, Context#execution_context{error_state = normal}}
            end;
        false ->
            {error, Reason, Context#execution_context{error_state = {error, Reason}}}
    end.

-spec recover_from_error(execution_context(), function()) -> execution_context().
recover_from_error(#execution_context{error_state = normal} = Context, _RecoveryFn) ->
    Context;
recover_from_error(#execution_context{error_state = {error, Reason}} = Context, RecoveryFn) ->
    case handle_execution_error(Reason, Context, RecoveryFn) of
        {ok, _Value, RecoveredContext} -> RecoveredContext;
        {error, _Reason, FailedContext} -> FailedContext
    end.

-spec is_recoverable(term()) -> boolean().
is_recoverable(temporary) -> true;
is_recoverable(timeout) -> true;
is_recoverable(retryable) -> true;
is_recoverable(_) -> false.

%%%---------------------------------------------------------------------
%%% Accessor functions
%%%---------------------------------------------------------------------

-spec context_error_state(execution_context()) -> normal | {error, term()}.
context_error_state(#execution_context{error_state = ErrorState}) ->
    ErrorState.

-spec context_with_error(term()) -> execution_context().
context_with_error(Reason) ->
    (new_context())#execution_context{error_state = {error, Reason}}.

-spec context_with_effects(catena_row_types:effect_row()) -> execution_context().
context_with_effects(Effects) ->
    (new_context())#execution_context{effects = normalize_effect_row(Effects)}.

%%%---------------------------------------------------------------------
%%% Internal helpers
%%%---------------------------------------------------------------------

-spec classify_operation(term() | undefined, [term()]) -> higher_order | first_order.
classify_operation(Signature, Args) ->
    case is_higher_order_signature(Signature) orelse lists:any(fun is_callable/1, Args) of
        true -> higher_order;
        false -> first_order
    end.

-spec execute_first_order(term() | undefined, atom(), atom(), [term()], execution_context()) -> ho_result().
execute_first_order(Signature, _Effect, _Operation, _Args, Context) ->
    Context1 = merge_signature_effects(Signature, Context),
    {ok, undefined, Context1}.

-spec execute_higher_order(term() | undefined, atom(), atom(), [term()], execution_context()) -> ho_result().
execute_higher_order(Signature, Effect, Operation, Args, Context) ->
    Context1 = merge_signature_effects(Signature, Context),
    case Signature of
        undefined ->
            {ok, execute_best_effort_callbacks(Args), Context1};
        SignatureTerm when is_tuple(SignatureTerm); is_map(SignatureTerm) ->
            execute_signature_callbacks(SignatureTerm, Effect, Operation, Args, Context1)
    end.

-spec execute_signature_callbacks(term(), atom(), atom(), [term()], execution_context()) -> ho_result().
execute_signature_callbacks(Signature, _Effect, _Operation, Args, Context) ->
    case catena_ho_effects:is_ho_op(Signature) of
        true ->
            run_ho_callbacks(Signature, Args, Context);
        false ->
            {ok, undefined, Context}
    end.

-spec run_ho_callbacks(term(), [term()], execution_context()) -> ho_result().
run_ho_callbacks(HOType, Args, Context) ->
    Params = extract_ho_params(HOType),
    execute_callback_params(Params, Args, Args, Context, []).

-spec execute_callback_params([term()], [term()], [term()], execution_context(), [term()]) -> ho_result().
execute_callback_params([], _AllArgs, _RemainingArgs, Context, Results) ->
    {ok, finalize_callback_results(Results), Context};
execute_callback_params([Param | Rest], AllArgs, RemainingArgs, Context, Results) ->
    case {catena_ho_effects:is_effectful_param(Param), RemainingArgs} of
        {true, [Fun | Tail]} when is_function(Fun) ->
            InvocationArgs = callback_invocation_args(Fun, Tail),
            case invoke_effectful_param(Fun, InvocationArgs) of
                {ok, Value} ->
                    ParamEffects = catena_ho_effects:param_effects(Param),
                    NextContext = context_add_effects(Context, ParamEffects),
                    execute_callback_params(Rest, AllArgs, Tail, NextContext, Results ++ [Value]);
                {error, Reason} ->
                    {error, Reason, Context#execution_context{error_state = {error, Reason}}}
            end;
        {true, [_NonCallable | Tail]} ->
            execute_callback_params(Rest, AllArgs, Tail, Context, Results);
        {true, []} ->
            execute_callback_params(Rest, AllArgs, [], Context, Results);
        {false, [_ | Tail]} ->
            execute_callback_params(Rest, AllArgs, Tail, Context, Results);
        {false, []} ->
            execute_callback_params(Rest, AllArgs, [], Context, Results)
    end.

-spec execute_best_effort_callbacks([term()]) -> term().
execute_best_effort_callbacks(Args) ->
    case [Arg || Arg <- Args, is_callable(Arg)] of
        [Fun | _Rest] ->
            InvocationArgs = [Arg || Arg <- Args, not is_callable(Arg)],
            case invoke_effectful_param(Fun, InvocationArgs) of
                {ok, Value} -> Value;
                {error, Reason} -> {error, Reason}
            end;
        [] ->
            undefined
    end.

-spec callback_invocation_args(function(), [term()]) -> [term()].
callback_invocation_args(Fun, RemainingArgs) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    lists:sublist(RemainingArgs, Arity).

-spec invoke_fun(function(), [term()]) -> term().
invoke_fun(Fun, Args) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    case Arity of
        0 ->
            Fun();
        _ when length(Args) >= Arity ->
            apply(Fun, lists:sublist(Args, Arity));
        _ ->
            error({arity_mismatch, Arity, Args})
    end.

-spec merge_signature_effects(term() | undefined, execution_context()) -> execution_context().
merge_signature_effects(undefined, Context) ->
    Context;
merge_signature_effects(Signature, Context) ->
    SignatureEffects = signature_effects(Signature),
    context_add_effects(Context, SignatureEffects).

-spec signature_effects(term()) -> catena_row_types:effect_row().
signature_effects(Signature) ->
    case catena_ho_effects:is_ho_op(Signature) of
        true ->
            extract_ho_effects(Signature);
        false ->
            case catch catena_op_signatures:sig_effects(Signature) of
                {'EXIT', _} -> catena_row_types:empty_row();
                Effects -> normalize_effect_row(Effects)
            end
    end.

-spec extract_ho_params(term()) -> [term()].
extract_ho_params(HOType) ->
    element(3, HOType).

-spec extract_ho_effects(term()) -> catena_row_types:effect_row().
extract_ho_effects(HOType) ->
    element(5, HOType).

-spec is_higher_order_signature(term() | undefined) -> boolean().
is_higher_order_signature(undefined) ->
    false;
is_higher_order_signature(Signature) ->
    catena_ho_effects:is_ho_op(Signature) andalso
    catena_ho_effects:count_effectful_params(Signature) > 0.

-spec is_callable(term()) -> boolean().
is_callable(Term) ->
    is_function(Term).

-spec extract_handler_result(term()) -> {term(), catena_row_types:effect_row()}.
extract_handler_result({with_effects, Value, Effects}) ->
    {Value, normalize_effect_row(Effects)};
extract_handler_result({with_context, Value, #execution_context{effects = Effects}}) ->
    {Value, Effects};
extract_handler_result({Value, Effects}) when is_map(Effects) ->
    {Value, normalize_effect_row(Effects)};
extract_handler_result(Value) ->
    {Value, catena_row_types:empty_row()}.

-spec lookup_effect_system_signature(atom(), atom()) -> term() | undefined.
lookup_effect_system_signature(_Effect, _Operation) ->
    undefined.

-spec merge_error_state(normal | {error, term()}, normal | {error, term()}) -> normal | {error, term()}.
merge_error_state(normal, Other) ->
    Other;
merge_error_state(Error, normal) ->
    Error;
merge_error_state(Error, _Other) ->
    Error.

-spec append_history(execution_context(), atom(), atom(), [term()]) -> execution_context().
append_history(Context, Effect, Operation, Args) ->
    Context#execution_context{
        history = Context#execution_context.history ++ [{Effect, Operation, Args}]
    }.

-spec normalize_effect_row(catena_row_types:effect_row()) -> catena_row_types:effect_row().
normalize_effect_row(Effects) ->
    catena_row_types:row_normalize(Effects).

-spec finalize_callback_results([term()]) -> term().
finalize_callback_results([]) ->
    undefined;
finalize_callback_results([Single]) ->
    Single;
finalize_callback_results(Results) ->
    Results.
