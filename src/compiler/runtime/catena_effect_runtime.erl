%%%-------------------------------------------------------------------
%%% @doc Effect Runtime System (Task 1.3.5)
%%%
%%% Implements process-based effect handlers leveraging BEAM's
%%% lightweight processes. Each try/with handler spawns a handler
%%% process that receives perform messages, executes handler operations,
%%% and sends results back.
%%%
%%% Uses explicit context passing instead of process dictionary for
%%% better composability across processes and proper nesting support.
%%%
%%% This module handles:
%%% - Handler process spawning (1.3.5.1)
%%% - Perform operation execution (1.3.5.2)
%%% - Effect message protocol (1.3.5.3)
%%% - Builtin IO effect handler (1.3.5.4)
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_runtime).

-export([
    %% Context creation
    empty_context/0,
    new_context/0,
    new_context/1,
    version/0,
    features/0,

    %% Main API
    perform/4,
    perform_cps/5,
    perform_cps/6,
    with_handlers/3,
    with_value_provider/3,
    with_resumable_handler/3,
    resume/2,
    resume/3,
    discard/1,

    %% Resumable handler construction
    control_case/3,
    control_case/4,
    value_case/3,
    value_case/4,
    control_closure/2,
    control_closure/3,
    apply_control/4,

    %% Builtin effects
    io_handler/0,
    process_handler/0
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type context_entry() ::
    #{
        kind := local_resumable,
        effect := atom(),
        cases := [resumable_case()],
        delimiter := reference(),
        frame_identity := reference(),
        depth := deep | shallow,
        resumption_kind := one_shot | multi_shot,
        resumption_budget := map(),
        owner := pid(),
        origin := term()
    }
    | #{
        kind := local_value_provider,
        effect := atom(),
        operations := [{atom(), function()}],
        origin := term()
    }
    | #{
        kind := process_provider,
        effect := atom(),
        operations := [{atom(), function()}],
        provider := pid(),
        origin := term()
    }.

-type resumable_case() :: #{
    operation := atom(),
    arity := non_neg_integer(),
    mode := control | value,
    handler := function(),
    origin := term()
}.

-type effect_context() :: #{
    handlers := #{atom() => pid()},
    entries := [context_entry()],
    parent := effect_context() | undefined,
    timeout := pos_integer(),
    resumption_budget := map()
}.

-export_type([effect_context/0, context_entry/0, resumable_case/0]).

%% Effect handler timeout (5 seconds)
-define(EFFECT_TIMEOUT, 5000).

%% Default maximum allowed processes to prevent DoS
%% Note: Erlang default limit is 262144, max is ~134 million
-define(DEFAULT_MAX_PROCESS_COUNT, 50000).

-define(CONTROL_CLOSURE_VERSION, 1).
-define(RUNTIME_VERSION, 3).

%% Default maximum file size for readFile (10 MB)
-define(DEFAULT_MAX_FILE_SIZE, 10485760).

%%====================================================================
%% Context Creation
%%====================================================================

-spec version() -> pos_integer().
version() -> ?RUNTIME_VERSION.

-spec features() -> [atom()].
features() ->
    [
        explicit_contexts,
        local_resumable_handlers,
        shallow_handlers,
        depth_aware_context_restoration,
        multi_shot_resumptions,
        isolated_resumption_branches,
        bounded_resumption_branches,
        versioned_control_closures
    ].

%% @doc Create an empty effect context
-spec empty_context() -> effect_context().
empty_context() ->
    #{
        handlers => #{},
        entries => [],
        parent => undefined,
        timeout => ?EFFECT_TIMEOUT,
        resumption_budget => catena_resumption_runtime:default_budget()
    }.

%% @doc Create a new effect context (alias for empty_context)
-spec new_context() -> effect_context().
new_context() ->
    empty_context().

%% @doc Create a context with runtime options.
-spec new_context(map()) -> effect_context().
new_context(Options) when is_map(Options) ->
    Timeout = maps:get(timeout, Options, ?EFFECT_TIMEOUT),
    ResumptionBudget = maps:get(
        resumption_budget,
        Options,
        catena_resumption_runtime:default_budget()
    ),
    true = is_integer(Timeout) andalso Timeout > 0,
    true = is_map(ResumptionBudget),
    #{
        handlers => #{},
        entries => [],
        parent => undefined,
        timeout => Timeout,
        resumption_budget => ResumptionBudget
    }.

%%====================================================================
%% Main API (1.3.5.1, 1.3.5.2)
%%====================================================================

%% @doc Perform an effect operation with explicit context
%%
%% Looks up handler in context (walking parent chain if needed),
%% sends a perform message and waits for the result.
%%
%% Message protocol (1.3.5.3):
%%   Send: {perform, Effect, Operation, Args, ReplyPid}
%%   Recv: {effect_result, Value}
-spec perform(effect_context(), atom(), atom(), list()) -> term().
perform(Ctx, Effect, Operation, Args)
        when is_atom(Effect), is_atom(Operation), is_list(Args) ->
    case lookup_entry(Ctx, Effect, Operation, length(Args)) of
        {ok, #{kind := local_value_provider} = Entry, _HandlerCtx} ->
            invoke_value_provider(Entry, Operation, Args);
        {ok, #{kind := process_provider} = Entry, _HandlerCtx} ->
            request_process_provider(Ctx, Entry, Operation, Args);
        {ok, #{kind := local_resumable}, _HandlerCtx} ->
            erlang:error({
                effect_runtime_error,
                missing_compiler_continuation,
                Effect,
                Operation
            });
        {error, Reason} ->
            erlang:error({effect_runtime_error, Reason});
        none ->
            perform_builtin(Effect, Operation, Args)
    end.

%% @doc Suspend a selective-CPS computation at an effect operation.
%%
%% Local resumable cases receive an opaque resumption. Value cases
%% automatically tail-resume. Direct providers compute only the operation
%% result; the continuation always runs afterward on the capturing process.
-spec perform_cps(
    effect_context(),
    atom(),
    atom(),
    list(),
    fun((term(), effect_context()) -> term())
) -> term().
perform_cps(Ctx, Effect, Operation, Args, Continuation)
        when
            is_atom(Effect),
            is_atom(Operation),
            is_list(Args),
            is_function(Continuation, 2)
        ->
    perform_cps(
        Ctx,
        Effect,
        Operation,
        Args,
        Continuation,
        {runtime_effect_operation, Effect, Operation}
    ).

-spec perform_cps(
    effect_context(),
    atom(),
    atom(),
    list(),
    fun((term(), effect_context()) -> term()),
    term()
) -> term().
perform_cps(Ctx, Effect, Operation, Args, Continuation, Origin)
        when
            is_atom(Effect),
            is_atom(Operation),
            is_list(Args),
            is_function(Continuation, 2)
        ->
    try
        perform_cps_i(Ctx, Effect, Operation, Args, Continuation, Origin)
    catch
        Class:Reason:_Stack ->
            {error, catena_resumption_runtime:normalize_exception(
                Class,
                Reason,
                Origin
            )}
    end.

-spec perform_cps_i(
    effect_context(),
    atom(),
    atom(),
    list(),
    fun((term(), effect_context()) -> term()),
    term()
) -> term().
perform_cps_i(Ctx, Effect, Operation, Args, Continuation, Origin) ->
    case lookup_entry(Ctx, Effect, Operation, length(Args)) of
        {ok, #{kind := local_resumable} = Frame, HandlerCtx} ->
            perform_local_resumable(
                Ctx,
                HandlerCtx,
                Frame,
                Operation,
                Args,
                Continuation,
                Origin
            );
        {ok, #{kind := local_value_provider} = Entry, _HandlerCtx} ->
            Value = invoke_value_provider(Entry, Operation, Args),
            Continuation(Value, Ctx);
        {ok, #{kind := process_provider} = Entry, _HandlerCtx} ->
            Value = request_process_provider(Ctx, Entry, Operation, Args),
            Continuation(Value, Ctx);
        {error, Reason} ->
            erlang:error({effect_runtime_error, Reason});
        none ->
            Value = perform_builtin(Effect, Operation, Args),
            Continuation(Value, Ctx)
    end.

%% @doc Execute body with effect handlers
%%
%% Spawns handler processes for each effect, creates a child context
%% with the new handlers, executes the body, and cleans up handler processes.
-spec with_handlers(effect_context(), list(), fun((effect_context()) -> T)) -> T.
with_handlers(Ctx, HandlerSpecs, BodyFun) ->
    %% Spawn handler processes and collect their PIDs
    {HandlerPids, NewHandlers} = spawn_handlers(HandlerSpecs),
    ProcessEntries = process_provider_entries(HandlerSpecs, NewHandlers),

    %% Create child context with new handlers merged in
    ChildCtx0 = #{
        handlers => maps:merge(maps:get(handlers, Ctx), NewHandlers),
        entries => ProcessEntries,
        parent => Ctx,
        timeout => maps:get(timeout, Ctx, ?EFFECT_TIMEOUT),
        resumption_budget => maps:get(
            resumption_budget,
            Ctx,
            catena_resumption_runtime:default_budget()
        )
    },
    ChildCtx = inherit_deadline(Ctx, ChildCtx0),

    try
        %% Execute body with child context
        BodyFun(ChildCtx)
    after
        %% Cleanup handler processes
        cleanup_handlers(HandlerPids)
    end.

%% @doc Execute a body under a same-process, non-resumable value provider.
-spec with_value_provider(
    effect_context(),
    {atom(), [{atom(), function()}]},
    fun((effect_context()) -> T)
) -> T.
with_value_provider(Ctx, {Effect, Operations}, BodyFun)
        when
            is_atom(Effect),
            is_list(Operations),
            is_function(BodyFun, 1)
        ->
    validate_operations(Operations),
    Entry = #{
        kind => local_value_provider,
        effect => Effect,
        operations => Operations,
        origin => {runtime_value_provider, Effect}
    },
    BodyFun(child_context(Ctx, Entry)).

%% @doc Execute a body under a same-process resumable handler frame.
-spec with_resumable_handler(
    effect_context(),
    map(),
    fun((effect_context()) -> T)
) -> T.
with_resumable_handler(Ctx, #{
    effect := Effect,
    cases := Cases,
    origin := Origin
} = Spec, BodyFun)
        when
            is_atom(Effect),
            is_list(Cases),
            is_function(BodyFun, 1)
        ->
    ok = validate_resumable_cases(Cases),
    Depth = maps:get(depth, Spec, deep),
    ResumptionKind = maps:get(
        resumption_kind,
        Spec,
        one_shot
    ),
    ResumptionBudget = maps:get(
        resumption_budget,
        Spec,
        maps:get(
            resumption_budget,
            Ctx,
            catena_resumption_runtime:default_budget()
        )
    ),
    ok = validate_handler_mode(Depth, ResumptionKind),
    Frame = #{
        kind => local_resumable,
        effect => Effect,
        cases => Cases,
        delimiter => make_ref(),
        frame_identity => make_ref(),
        depth => Depth,
        resumption_kind => ResumptionKind,
        resumption_budget => ResumptionBudget,
        owner => self(),
        origin => Origin
    },
    BodyFun(child_context(Ctx, Frame)).

-spec validate_handler_mode(term(), term()) -> ok.
validate_handler_mode(Depth, ResumptionKind)
        when
            (Depth =:= deep orelse Depth =:= shallow),
            (ResumptionKind =:= one_shot orelse
                ResumptionKind =:= multi_shot)
        ->
    ok;
validate_handler_mode(Depth, ResumptionKind) ->
    erlang:error({effect_runtime_error, {
        unsupported_handler_mode,
        Depth,
        ResumptionKind
    }}).

%% @doc Invoke an opaque resumption and return its delimiter result.
-spec resume(term(), term()) -> term().
resume(Resumption, Value) ->
    case catena_resumption_runtime:resume(Resumption, Value) of
        {ok, Result} ->
            Result;
        {error, Failure} ->
            {error, Failure}
    end.

%% @doc Invoke with a same-process runtime deadline.
-spec resume(term(), term(), timeout()) -> term().
resume(Resumption, Value, Timeout) ->
    case catena_resumption_runtime:resume(Resumption, Value, Timeout) of
        {ok, Result} ->
            Result;
        {error, Failure} ->
            {error, Failure}
    end.

%% @doc Idempotently abandon a retained resumption.
-spec discard(term()) -> ok | {error, map()}.
discard(Resumption) ->
    catena_resumption_runtime:discard(Resumption).

%% @doc Construct a control case for a resumable source handler frame.
-spec control_case(
    atom(),
    non_neg_integer(),
    fun(([term()], term(), effect_context()) -> term())
) -> resumable_case().
control_case(Operation, Arity, Handler)
        when
            is_atom(Operation),
            is_integer(Arity),
            Arity >= 0,
            is_function(Handler, 3)
        ->
    control_case(
        Operation,
        Arity,
        Handler,
        {runtime_control_case, Operation}
    ).

-spec control_case(
    atom(),
    non_neg_integer(),
    fun(([term()], term(), effect_context()) -> term()),
    term()
) -> resumable_case().
control_case(Operation, Arity, Handler, Origin)
        when
            is_atom(Operation),
            is_integer(Arity),
            Arity >= 0,
            is_function(Handler, 3)
        ->
    #{
        operation => Operation,
        arity => Arity,
        mode => control,
        handler => Handler,
        origin => Origin
    }.

%% @doc Construct an auto-resuming value case for a source handler frame.
-spec value_case(
    atom(),
    non_neg_integer(),
    fun(([term()], effect_context()) -> term())
) -> resumable_case().
value_case(Operation, Arity, Handler)
        when
            is_atom(Operation),
            is_integer(Arity),
            Arity >= 0,
            is_function(Handler, 2)
        ->
    value_case(
        Operation,
        Arity,
        Handler,
        {runtime_value_case, Operation}
    ).

-spec value_case(
    atom(),
    non_neg_integer(),
    fun(([term()], effect_context()) -> term()),
    term()
) -> resumable_case().
value_case(Operation, Arity, Handler, Origin)
        when
            is_atom(Operation),
            is_integer(Arity),
            Arity >= 0,
            is_function(Handler, 2)
        ->
    #{
        operation => Operation,
        arity => Arity,
        mode => value,
        handler => Handler,
        origin => Origin
    }.

%% @doc Wrap a generated first-class callable with its control convention.
-spec control_closure(
    direct | resumable,
    fun(([term()], effect_context(), fun((term(), effect_context()) -> term())) ->
        term())
) -> tuple().
control_closure(Mode, Callable)
        when
            (Mode =:= direct orelse Mode =:= resumable),
            is_function(Callable, 3)
        ->
    {catena_control_closure, ?CONTROL_CLOSURE_VERSION, Mode, Callable}.

-spec control_closure(
    direct | resumable,
    fun(([term()], effect_context(), fun((term(), effect_context()) -> term())) ->
        term()),
    term()
) -> tuple().
control_closure(Mode, Callable, Origin)
        when
            (Mode =:= direct orelse Mode =:= resumable),
            is_function(Callable, 3)
        ->
    {catena_control_closure, ?CONTROL_CLOSURE_VERSION, Mode, Callable, Origin}.

%% @doc Invoke a generated control closure or a source-arity BEAM function.
%%
%% Generated closures share one list/context/continuation convention. Plain
%% BEAM functions remain accepted at public interoperability boundaries and
%% are lifted through the supplied continuation as direct callables.
-spec apply_control(
    term(),
    [term()],
    effect_context(),
    fun((term(), effect_context()) -> term())
) -> term().
apply_control(
    {catena_control_closure, ?CONTROL_CLOSURE_VERSION, Mode, Callable, Origin},
    Arguments,
    Context,
    Continuation
) when
    (Mode =:= direct orelse Mode =:= resumable),
    is_function(Callable, 3),
    is_list(Arguments),
    is_function(Continuation, 2)
->
    try
        Callable(Arguments, Context, Continuation)
    catch
        Class:Reason:_Stack ->
            {error, catena_resumption_runtime:normalize_exception(
                Class,
                Reason,
                Origin
            )}
    end;
apply_control(
    {catena_control_closure, ?CONTROL_CLOSURE_VERSION, Mode, Callable},
    Arguments,
    Context,
    Continuation
) when
    (Mode =:= direct orelse Mode =:= resumable),
    is_function(Callable, 3),
    is_list(Arguments),
    is_function(Continuation, 2)
->
    Callable(Arguments, Context, Continuation);
apply_control(Callable, Arguments, Context, Continuation)
        when
            is_function(Callable),
            is_list(Arguments),
            is_function(Continuation, 2)
        ->
    {arity, Arity} = erlang:fun_info(Callable, arity),
    case Arity =:= length(Arguments) of
        true ->
            Continuation(erlang:apply(Callable, Arguments), Context);
        false ->
            erlang:error({effect_runtime_error, {
                control_closure_arity_mismatch,
                Arity,
                length(Arguments)
            }})
    end;
apply_control(_Callable, Arguments, _Context, _Continuation) ->
    erlang:error({effect_runtime_error, {
        invalid_control_closure,
        length_or_unknown(Arguments)
    }}).

length_or_unknown(Arguments) when is_list(Arguments) -> length(Arguments);
length_or_unknown(_Arguments) -> unknown.

%%====================================================================
%% Handler Lookup
%%====================================================================

%% @doc Find the innermost entry compatible with effect, operation, and arity.
-spec lookup_entry(effect_context(), atom(), atom(), non_neg_integer()) ->
    {ok, context_entry(), effect_context()}
    | {error, term()}
    | none.
lookup_entry(Ctx, Effect, Operation, Arity) ->
    case lookup_current_entries(
        maps:get(entries, Ctx, []),
        Effect,
        Operation,
        Arity
    ) of
        {ok, Entry} ->
            {ok, Entry, maps:get(parent, Ctx, Ctx)};
        {error, _} = Error ->
            Error;
        none ->
            case maps:get(parent, Ctx, undefined) of
                undefined ->
                    none;
                ParentCtx ->
                    lookup_entry(ParentCtx, Effect, Operation, Arity)
            end
    end.

-spec lookup_current_entries(
    [context_entry()],
    atom(),
    atom(),
    non_neg_integer()
) -> {ok, context_entry()} | {error, term()} | none.
lookup_current_entries([], _Effect, _Operation, _Arity) ->
    none;
lookup_current_entries(
    [#{effect := Effect} = Entry | Rest],
    Effect,
    Operation,
    Arity
) ->
    case entry_operation_match(Entry, Operation, Arity) of
        match ->
            {ok, Entry};
        arity_mismatch ->
            {error, {
                operation_arity_mismatch,
                Effect,
                Operation,
                Arity
            }};
        no_operation ->
            lookup_current_entries(Rest, Effect, Operation, Arity)
    end;
lookup_current_entries([_Entry | Rest], Effect, Operation, Arity) ->
    lookup_current_entries(Rest, Effect, Operation, Arity).

-spec entry_operation_match(context_entry(), atom(), non_neg_integer()) ->
    match | arity_mismatch | no_operation.
entry_operation_match(#{kind := local_resumable, cases := Cases}, Operation, Arity) ->
    operation_match(Cases, Operation, Arity);
entry_operation_match(#{kind := process_provider}, _Operation, _Arity) ->
    %% Preserve the request/response contract: a selected process provider
    %% owns unknown-operation and bad-arity diagnostics for its effect.
    match;
entry_operation_match(#{operations := Operations}, Operation, Arity) ->
    operation_match(Operations, Operation, Arity).

-spec operation_match(list(), atom(), non_neg_integer()) ->
    match | arity_mismatch | no_operation.
operation_match(Items, Operation, Arity) ->
    Named = [
        Item
        || Item <- Items,
           operation_name(Item) =:= Operation
    ],
    case Named of
        [] ->
            no_operation;
        _ ->
            case lists:any(
                fun(Item) -> operation_arity(Item) =:= Arity end,
                Named
            ) of
                true -> match;
                false -> arity_mismatch
            end
    end.

-spec operation_name(map() | {atom(), function()}) -> atom().
operation_name(#{operation := Operation}) ->
    Operation;
operation_name({Operation, _Handler}) ->
    Operation.

-spec operation_arity(map() | {atom(), function()}) -> non_neg_integer().
operation_arity(#{arity := Arity}) ->
    Arity;
operation_arity({_Operation, Handler}) ->
    {arity, Arity} = erlang:fun_info(Handler, arity),
    Arity.

-spec child_context(effect_context(), context_entry()) -> effect_context().
child_context(Ctx, Entry) ->
    ChildCtx = #{
        handlers => maps:get(handlers, Ctx, #{}),
        entries => [Entry],
        parent => Ctx,
        timeout => maps:get(timeout, Ctx, ?EFFECT_TIMEOUT),
        resumption_budget => maps:get(
            resumption_budget,
            Ctx,
            catena_resumption_runtime:default_budget()
        )
    },
    inherit_deadline(Ctx, ChildCtx).

%%====================================================================
%% Same-Process Resumable Execution
%%====================================================================

-spec perform_local_resumable(
    effect_context(),
    effect_context(),
    context_entry(),
    atom(),
    [term()],
    fun((term(), effect_context()) -> term()),
    term()
) -> term().
perform_local_resumable(
    CapturedCtx,
    HandlerCtx,
    Frame,
    Operation,
    Args,
    Continuation,
    PerformOrigin
) ->
    Case = select_resumable_case(
        maps:get(cases, Frame),
        Operation,
        length(Args)
    ),
    CaptureSpec = #{
        context => CapturedCtx,
        parent_context => HandlerCtx,
        delimiter => maps:get(delimiter, Frame),
        depth => maps:get(depth, Frame),
        kind => maps:get(resumption_kind, Frame),
        budget => maps:get(resumption_budget, Frame),
        origin => #{
            perform => PerformOrigin,
            handler_case => maps:get(origin, Case, undefined),
            delimiter => maps:get(origin, Frame)
        },
        metadata => #{
            effect => maps:get(effect, Frame),
            operation => Operation,
            frame_owner => maps:get(owner, Frame)
        },
        type_identity => {
            maps:get(effect, Frame),
            Operation,
            length(Args)
        },
        providers => required_providers(CapturedCtx),
        frame_identity => maps:get(frame_identity, Frame)
    },
    case catena_resumption_runtime:capture(Continuation, CaptureSpec) of
        {ok, Resumption} ->
            invoke_resumable_case(Case, Args, Resumption, HandlerCtx);
        {error, Failure} ->
            {error, Failure}
    end.

-spec select_resumable_case([resumable_case()], atom(), non_neg_integer()) ->
    resumable_case().
select_resumable_case(Cases, Operation, Arity) ->
    [Case] = [
        Candidate
        || Candidate <- Cases,
           maps:get(operation, Candidate) =:= Operation,
           maps:get(arity, Candidate) =:= Arity
    ],
    Case.

-spec invoke_resumable_case(
    resumable_case(),
    [term()],
    term(),
    effect_context()
) -> term().
invoke_resumable_case(
    #{mode := control, handler := Handler} = Case,
    Args,
    Resumption,
    HandlerCtx
) ->
    Result = safe_handler_call(
        fun() -> Handler(Args, Resumption, HandlerCtx) end,
        maps:get(origin, Case)
    ),
    finalize_case_result(Result, Resumption);
invoke_resumable_case(
    #{mode := value, handler := Handler} = Case,
    Args,
    Resumption,
    HandlerCtx
) ->
    Result = safe_handler_call(
        fun() ->
            OperationValue = Handler(Args, HandlerCtx),
            resume(Resumption, OperationValue)
        end,
        maps:get(origin, Case)
    ),
    finalize_case_result(Result, Resumption).

-spec safe_handler_call(fun(() -> term()), term()) -> term().
safe_handler_call(Fun, Origin) ->
    try
        Fun()
    catch
        Class:Reason:_Stack ->
            {error, catena_resumption_runtime:normalize_exception(
                Class,
                Reason,
                Origin
            )}
    end.

-spec finalize_case_result(term(), term()) -> term().
finalize_case_result(Result, Resumption) ->
    case contains_handle(Result, Resumption) of
        true ->
            Result;
        false ->
            case catena_resumption_runtime:discard(Resumption) of
                ok ->
                    Result;
                {error, Failure} ->
                    {error, Failure}
            end
    end.

-spec contains_handle(term(), term()) -> boolean().
contains_handle(Handle, Handle) ->
    true;
contains_handle(Term, Handle) when is_tuple(Term) ->
    contains_handle(tuple_to_list(Term), Handle);
contains_handle(Terms, Handle) when is_list(Terms) ->
    lists:any(fun(Term) -> contains_handle(Term, Handle) end, Terms);
contains_handle(Term, Handle) when is_map(Term) ->
    contains_handle(maps:to_list(Term), Handle);
contains_handle(_Term, _Handle) ->
    false.

-spec required_providers(effect_context()) -> [pid()].
required_providers(Ctx) ->
    Current = [
        maps:get(provider, Entry)
        || Entry <- maps:get(entries, Ctx, []),
           maps:get(kind, Entry) =:= process_provider
    ],
    Parent = case maps:get(parent, Ctx, undefined) of
        undefined -> [];
        ParentCtx -> required_providers(ParentCtx)
    end,
    lists:usort(Current ++ Parent).

%%====================================================================
%% Value And Process Providers
%%====================================================================

-spec invoke_value_provider(context_entry(), atom(), [term()]) -> term().
invoke_value_provider(#{operations := Operations}, Operation, Args) ->
    {Operation, Handler} = lists:keyfind(Operation, 1, Operations),
    apply(Handler, Args).

-spec request_process_provider(
    effect_context(),
    context_entry(),
    atom(),
    [term()]
) -> term().
request_process_provider(Ctx, Entry, Operation, Args) ->
    HandlerPid = maps:get(provider, Entry),
    Effect = maps:get(effect, Entry),
    HandlerPid ! {perform, Effect, Operation, Args, self()},
    Timeout = effective_timeout(Ctx),
    receive
        {effect_result, Value} ->
            Value;
        {effect_error, Reason} ->
            erlang:error({effect_error, Effect, Operation, Reason})
    after Timeout ->
        erlang:error({effect_timeout, Effect, Operation})
    end.

-spec inherit_deadline(effect_context(), effect_context()) -> effect_context().
inherit_deadline(Ctx, ChildCtx) ->
    case maps:find(runtime_deadline, Ctx) of
        {ok, Deadline} ->
            ChildCtx#{runtime_deadline => Deadline};
        error ->
            ChildCtx
    end.

-spec effective_timeout(effect_context()) -> non_neg_integer().
effective_timeout(Ctx) ->
    Configured = maps:get(timeout, Ctx, ?EFFECT_TIMEOUT),
    case maps:find(runtime_deadline, Ctx) of
        {ok, Deadline} ->
            Remaining = Deadline - erlang:monotonic_time(millisecond),
            erlang:max(0, erlang:min(Configured, Remaining));
        error ->
            Configured
    end.

-spec process_provider_entries(
    [{atom(), [{atom(), function()}]}],
    #{atom() => pid()}
) -> [context_entry()].
process_provider_entries(HandlerSpecs, PidMap) ->
    [
        #{
            kind => process_provider,
            effect => Effect,
            operations => Operations,
            provider => maps:get(Effect, PidMap),
            origin => {runtime_process_provider, Effect}
        }
        || {Effect, Operations} <- HandlerSpecs
    ].

-spec validate_operations([{atom(), function()}]) -> ok.
validate_operations(Operations) ->
    true = lists:all(
        fun
            ({Operation, Handler}) ->
                is_atom(Operation) andalso is_function(Handler);
            (_) ->
                false
        end,
        Operations
    ),
    ok.

-spec validate_resumable_cases([resumable_case()]) -> ok.
validate_resumable_cases(Cases) ->
    true = Cases =/= [],
    true = lists:all(fun valid_resumable_case/1, Cases),
    Identities = [
        {maps:get(operation, Case), maps:get(arity, Case)}
        || Case <- Cases
    ],
    true = length(Identities) =:= length(lists:usort(Identities)),
    ok.

-spec valid_resumable_case(term()) -> boolean().
valid_resumable_case(#{
    operation := Operation,
    arity := Arity,
    mode := control,
    handler := Handler,
    origin := Origin
}) ->
    is_atom(Operation) andalso
        is_integer(Arity) andalso
        Arity >= 0 andalso
        is_function(Handler, 3) andalso
        Origin =/= undefined;
valid_resumable_case(#{
    operation := Operation,
    arity := Arity,
    mode := value,
    handler := Handler,
    origin := Origin
}) ->
    is_atom(Operation) andalso
        is_integer(Arity) andalso
        Arity >= 0 andalso
        is_function(Handler, 2) andalso
        Origin =/= undefined;
valid_resumable_case(_) ->
    false.

%%====================================================================
%% Handler Spawning (1.3.5.1)
%%====================================================================

%% Spawn handler processes from handler specifications
%% Returns {ListOfPids, MapOfEffectToPid}
-spec spawn_handlers(list()) -> {list({atom(), pid()}), #{atom() => pid()}}.
spawn_handlers(HandlerSpecs) ->
    lists:foldl(
        fun({Effect, Operations}, {PidList, PidMap}) ->
            Pid = spawn_link(fun() ->
                handler_loop(Effect, Operations)
            end),
            {[{Effect, Pid} | PidList], maps:put(Effect, Pid, PidMap)}
        end,
        {[], #{}},
        HandlerSpecs
    ).

%% Handler process loop
-spec handler_loop(atom(), [{atom(), fun()}]) -> ok.
handler_loop(Effect, Operations) ->
    receive
        {perform, Effect, Operation, Args, ReplyPid} ->
            %% Find operation handler
            case lists:keyfind(Operation, 1, Operations) of
                {Operation, HandlerFun} ->
                    %% Execute handler
                    try
                        Result = apply(HandlerFun, Args),
                        ReplyPid ! {effect_result, Result}
                    catch
                        Class:Reason:_Stack ->
                            ReplyPid ! {effect_error, {Class, Reason}}
                    end;
                false ->
                    ReplyPid ! {effect_error, {unknown_operation, Operation}}
            end,
            handler_loop(Effect, Operations);

        stop ->
            ok;

        _Other ->
            handler_loop(Effect, Operations)
    end.

%% Cleanup handler processes
-spec cleanup_handlers([{atom(), pid()}]) -> ok.
cleanup_handlers(HandlerPids) ->
    lists:foreach(
        fun({_Effect, Pid}) ->
            Ref = erlang:monitor(process, Pid),
            unlink(Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, _Reason} ->
                    ok
            end
        end,
        HandlerPids
    ),
    ok.

%%====================================================================
%% Builtin Effects (1.3.5.4)
%%====================================================================

%% Perform builtin effect operations
-spec perform_builtin(atom(), atom(), list()) -> term().
perform_builtin('IO', Operation, Args) ->
    perform_io(Operation, Args);
perform_builtin('Process', Operation, Args) ->
    perform_process(Operation, Args);
perform_builtin(Effect, Operation, _Args) ->
    erlang:error({no_handler_for_effect, Effect, Operation}).

%%====================================================================
%% IO Effect Handler (1.3.5.4)
%%====================================================================

%% @doc Get the builtin IO effect handler specification
-spec io_handler() -> {atom(), list()}.
io_handler() ->
    {'IO', [
        {print, fun io_print/1},
        {println, fun io_println/1},
        {readFile, fun io_read_file/1},
        {writeFile, fun io_write_file/2},
        {getLine, fun io_get_line/0}
    ]}.

%% Perform IO operations directly (for builtin handler)
-spec perform_io(atom(), list()) -> term().
perform_io(print, [Text]) ->
    io_print(Text);
perform_io(println, [Text]) ->
    io_println(Text);
perform_io(readFile, [Path]) ->
    io_read_file(Path);
perform_io(writeFile, [Path, Content]) ->
    io_write_file(Path, Content);
perform_io(getLine, []) ->
    io_get_line();
perform_io(Operation, _Args) ->
    erlang:error({unknown_io_operation, Operation}).

%% IO operation implementations
-spec io_print(term()) -> ok.
io_print(Text) ->
    io:format("~s", [to_string(Text)]),
    ok.

-spec io_println(term()) -> ok.
io_println(Text) ->
    io:format("~s~n", [to_string(Text)]),
    ok.

-spec io_read_file(term()) -> binary().
io_read_file(Path) ->
    PathStr = path_to_string(Path),
    case validate_io_path(PathStr) of
        {ok, ValidPath} ->
            %% Check file size before reading
            MaxSize = application:get_env(catena, max_file_size, ?DEFAULT_MAX_FILE_SIZE),
            case file:read_file_info(ValidPath) of
                {ok, FileInfo} ->
                    FileSize = element(2, FileInfo),  %% size field
                    case FileSize > MaxSize of
                        true ->
                            erlang:error({io_error, readFile, {file_too_large, FileSize, MaxSize}});
                        false ->
                            case file:read_file(ValidPath) of
                                {ok, Content} ->
                                    Content;
                                {error, Reason} ->
                                    erlang:error({io_error, readFile, Reason})
                            end
                    end;
                {error, Reason} ->
                    erlang:error({io_error, readFile, Reason})
            end;
        {error, Reason} ->
            erlang:error({io_error, readFile, Reason})
    end.

-spec io_write_file(term(), term()) -> ok.
io_write_file(Path, Content) ->
    PathStr = path_to_string(Path),
    case validate_io_path(PathStr) of
        {ok, ValidPath} ->
            case file:write_file(ValidPath, to_string(Content)) of
                ok ->
                    ok;
                {error, Reason} ->
                    erlang:error({io_error, writeFile, Reason})
            end;
        {error, Reason} ->
            erlang:error({io_error, writeFile, Reason})
    end.

-spec io_get_line() -> binary().
io_get_line() ->
    case io:get_line("") of
        eof ->
            <<>>;
        {error, Reason} ->
            erlang:error({io_error, getLine, Reason});
        Line ->
            %% Remove trailing newline
            list_to_binary(string:trim(Line, trailing, "\n"))
    end.

%%====================================================================
%% Process Effect Handler
%%====================================================================

%% @doc Get the builtin Process effect handler specification
-spec process_handler() -> {atom(), list()}.
process_handler() ->
    {'Process', [
        {spawn, fun process_spawn/1},
        {spawn_link, fun process_spawn_link/1},
        {send, fun process_send/2},
        {self, fun process_self/0},
        {link, fun process_link/1},
        {unlink, fun process_unlink/1},
        {monitor, fun process_monitor/1},
        {demonitor, fun process_demonitor/1},
        {whereis, fun process_whereis/1},
        {register, fun process_register/2},
        {is_process_alive, fun process_is_process_alive/1},
        {trap_exit, fun process_trap_exit/1}
    ]}.

%% Perform Process operations directly (for builtin handler)
-spec perform_process(atom(), list()) -> term().
perform_process(spawn, [Fun]) ->
    process_spawn(Fun);
perform_process(spawn_link, [Fun]) ->
    process_spawn_link(Fun);
perform_process(send, [Pid, Msg]) ->
    process_send(Pid, Msg);
perform_process(self, []) ->
    process_self();
perform_process(link, [Pid]) ->
    process_link(Pid);
perform_process(unlink, [Pid]) ->
    process_unlink(Pid);
perform_process(monitor, [Pid]) ->
    process_monitor(Pid);
perform_process(demonitor, [Ref]) ->
    process_demonitor(Ref);
perform_process(whereis, [Name]) ->
    process_whereis(Name);
perform_process(register, [Name, Pid]) ->
    process_register(Name, Pid);
perform_process(is_process_alive, [Pid]) ->
    process_is_process_alive(Pid);
perform_process(trap_exit, [Flag]) ->
    process_trap_exit(Flag);
perform_process(Operation, _Args) ->
    erlang:error({unknown_process_operation, Operation}).

%% Process operation implementations
-spec process_spawn(fun(() -> term())) -> pid().
process_spawn(Fun) ->
    %% Check process count to prevent DoS
    MaxCount = application:get_env(catena, max_process_count, ?DEFAULT_MAX_PROCESS_COUNT),
    case erlang:system_info(process_count) of
        Count when Count >= MaxCount ->
            erlang:error({process_limit_exceeded, Count, MaxCount});
        _ ->
            spawn(fun() -> Fun() end)
    end.

-spec process_send(pid(), term()) -> ok.
process_send(Pid, Msg) ->
    Pid ! Msg,
    ok.

-spec process_self() -> pid().
process_self() ->
    self().

%% Additional Process effect operations for Phase 5 (Actor Model Integration)

-spec process_spawn_link(fun(() -> term())) -> pid().
process_spawn_link(Fun) ->
    spawn_link(Fun).

-spec process_link(pid()) -> true.
process_link(Pid) ->
    erlang:link(Pid).

-spec process_unlink(pid()) -> true.
process_unlink(Pid) ->
    erlang:unlink(Pid).

-spec process_monitor(pid()) -> reference().
process_monitor(Pid) ->
    erlang:monitor(process, Pid).

-spec process_demonitor(reference()) -> true.
process_demonitor(Ref) ->
    erlang:demonitor(Ref).

-spec process_whereis(atom()) -> pid() | undefined.
process_whereis(Name) ->
    erlang:whereis(Name).

-spec process_register(atom(), pid()) -> true.
process_register(Name, Pid) ->
    erlang:register(Name, Pid).

-spec process_is_process_alive(pid()) -> boolean().
process_is_process_alive(Pid) ->
    erlang:is_process_alive(Pid).

-spec process_trap_exit(boolean()) -> boolean().
process_trap_exit(Flag) ->
    erlang:process_flag(trap_exit, Flag).

%%====================================================================
%% Utilities
%%====================================================================

%% Validate IO path for security
%% Blocks: path traversal (..), null bytes, system paths, symlinks to restricted paths
%% Allows: absolute paths to /tmp and user directories
-spec validate_io_path(string()) -> {ok, string()} | {error, term()}.
validate_io_path(Path) ->
    NormalizedPath = filename:absname(Path),
    case is_safe_io_path(Path, NormalizedPath) of
        true ->
            %% Also validate symlink target if path is a symlink
            case resolve_symlinks(NormalizedPath, 10) of
                {ok, ResolvedPath} ->
                    case is_system_path(ResolvedPath) of
                        true -> {error, {symlink_to_restricted_path, Path, ResolvedPath}};
                        false -> {ok, NormalizedPath}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        false -> {error, {path_security_error, Path}}
    end.

%% Resolve symlinks to get final target path
%% MaxDepth prevents infinite loops from circular symlinks
-spec resolve_symlinks(string(), non_neg_integer()) -> {ok, string()} | {error, term()}.
resolve_symlinks(_Path, 0) ->
    {error, symlink_loop_detected};
resolve_symlinks(Path, MaxDepth) ->
    case file:read_link_info(Path) of
        {ok, FileInfo} ->
            case element(3, FileInfo) of  %% type field
                symlink ->
                    case file:read_link(Path) of
                        {ok, Target} ->
                            %% Resolve relative symlinks
                            AbsTarget = case Target of
                                [$/ | _] -> Target;
                                _ -> filename:absname(Target, filename:dirname(Path))
                            end,
                            resolve_symlinks(AbsTarget, MaxDepth - 1);
                        {error, Reason} ->
                            {error, {symlink_read_error, Reason}}
                    end;
                _ ->
                    {ok, Path}
            end;
        {error, enoent} ->
            %% File doesn't exist yet (for writes), that's ok
            {ok, Path};
        {error, Reason} ->
            {error, {file_info_error, Reason}}
    end.

-spec is_safe_io_path(string(), string()) -> boolean().
is_safe_io_path(OriginalPath, NormalizedPath) ->
    not has_path_traversal(OriginalPath) andalso
    not has_null_bytes(OriginalPath) andalso
    not is_system_path(NormalizedPath).

%% Check for path traversal sequences
-spec has_path_traversal(string()) -> boolean().
has_path_traversal(Path) ->
    string:find(Path, "..") =/= nomatch.

%% Check for null bytes (used to obfuscate paths)
-spec has_null_bytes(string()) -> boolean().
has_null_bytes(Path) ->
    lists:member(0, Path).

%% Check if path is a protected system directory
-spec is_system_path(string()) -> boolean().
is_system_path(Path) ->
    SystemPaths = ["/etc", "/sys", "/proc", "/dev", "/root", "/boot", "/var/log"],
    lists:any(fun(Prefix) ->
        lists:prefix(Prefix, Path)
    end, SystemPaths).

%% Convert various types to string/binary for IO
-spec to_string(term()) -> binary().
to_string(Bin) when is_binary(Bin) -> Bin;
to_string(List) when is_list(List) -> list_to_binary(List);
to_string(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
to_string(Int) when is_integer(Int) -> integer_to_binary(Int);
to_string(Float) when is_float(Float) -> float_to_binary(Float);
to_string(Other) -> list_to_binary(io_lib:format("~p", [Other])).

%% Convert path to string (list) for validation
-spec path_to_string(term()) -> string().
path_to_string(Bin) when is_binary(Bin) -> binary_to_list(Bin);
path_to_string(List) when is_list(List) -> List;
path_to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom).
