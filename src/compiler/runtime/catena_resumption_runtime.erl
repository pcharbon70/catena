%%%-------------------------------------------------------------------
%%% @doc Opaque process-affine resumption runtime.
%%%
%%% Compiler-reified continuations are registered behind an opaque,
%%% versioned handle.  Private continuation and context data live in this
%%% runtime authority and never appear in the public handle.  Authorization
%%% is serialized by the registry, while continuation execution happens in
%%% the calling owner process.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_resumption_runtime).
-behaviour(gen_server).

-export([
    capture/2,
    resume/2,
    resume/3,
    discard/1,
    expire_delimiter/1,
    is_resumption/1,
    status/1,
    describe/1,
    lease_status/1,
    branch_stats/1,
    configure_trace/1,
    trace/0,
    clear_trace/0,
    default_budget/0,
    control_failure/3,
    normalize_exception/3,
    version/0,
    features/0,
    reset_for_test/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export_type([
    handle/0,
    capture_spec/0,
    control_failure/0
]).

-define(SERVER, catena_resumption_runtime_registry).
-define(VERSION, 3).

-define(DEFAULT_MAX_INVOCATIONS, 64).
-define(DEFAULT_MAX_RETAINED_WORDS, 262144).
-define(DEFAULT_MAX_REDUCTIONS, 1000000).
-define(DEFAULT_BRANCH_TIMEOUT, 5000).
-define(DEFAULT_MAX_BRANCH_DEPTH, 16).

-opaque handle() ::
    {catena_resumption, ?VERSION, reference()}.

-type capture_spec() :: #{
    context := map(),
    delimiter := reference(),
    depth := deep | shallow,
    kind := one_shot | multi_shot,
    parent_context => map(),
    origin := term(),
    metadata => map(),
    type_identity => term(),
    providers => [pid()],
    frame_identity => term(),
    budget => map()
}.

-type control_failure() :: #{
    category := atom(),
    origin := term(),
    details := map()
}.

-type entry() :: #{
    owner := pid(),
    kind := one_shot | multi_shot,
    state := fresh | running | consumed,
    continuation => fun((term(), map()) -> term()),
    context => map(),
    parent_context => map(),
    delimiter := reference(),
    delimiter_status := live | expired,
    depth := deep | shallow,
    origin := term(),
    metadata := map(),
    type_identity := term(),
    providers := [pid()],
    frame_identity := term(),
    lease := map(),
    owner_monitor => reference(),
    provider_monitors => #{reference() => pid()},
    expired => owner | delimiter | provider,
    run_token => reference(),
    consumed_reason => normal | exceptional | abandoned,
    budget := map(),
    retained_words := non_neg_integer(),
    captured_branch_depth := non_neg_integer(),
    invocation_count := non_neg_integer(),
    completed_branches := non_neg_integer(),
    failed_branches := non_neg_integer(),
    current_branch => map(),
    last_branch => map()
}.

-type registry_state() :: #{
    entries := #{reference() => entry()},
    traces := #{pid() => map()},
    next_resumption_id := pos_integer()
}.

%%====================================================================
%% Public API
%%====================================================================

%% @doc Current opaque runtime representation version.
-spec version() -> pos_integer().
version() ->
    ?VERSION.

%% @doc Handler-frame capabilities implemented by this runtime ABI.
-spec features() -> [atom()].
features() ->
    [
        deep_handlers,
        shallow_handlers,
        depth_aware_context_restoration,
        explicit_contexts,
        one_shot_resumptions,
        multi_shot_resumptions,
        isolated_resumption_branches,
        bounded_resumption_branches,
        retained_resumptions,
        same_process_resume
    ].

%% @doc Default fail-closed limits for repeated continuation branches.
-spec default_budget() -> map().
default_budget() ->
    #{
        max_invocations => ?DEFAULT_MAX_INVOCATIONS,
        max_retained_words => ?DEFAULT_MAX_RETAINED_WORDS,
        max_reductions => ?DEFAULT_MAX_REDUCTIONS,
        timeout => ?DEFAULT_BRANCH_TIMEOUT,
        max_branch_depth => ?DEFAULT_MAX_BRANCH_DEPTH
    }.

%% @doc Register a compiler-reified continuation.
%%
%% The continuation receives the operation result and its captured explicit
%% context.  This API is a compiler/runtime ABI; Catena source has no
%% constructor for the returned handle.
-spec capture(fun((term(), map()) -> term()), capture_spec()) ->
    {ok, handle()} | {error, control_failure()}.
capture(Continuation, Spec) ->
    case validate_capture(Continuation, Spec) of
        ok ->
            ok = ensure_started(),
            Ref = make_ref(),
            Budget = normalized_budget(Spec),
            Entry = #{
                owner => self(),
                kind => maps:get(kind, Spec),
                state => fresh,
                continuation => Continuation,
                context => maps:get(context, Spec),
                parent_context => maps:get(
                    parent_context,
                    Spec,
                    maps:get(context, Spec)
                ),
                delimiter => maps:get(delimiter, Spec),
                delimiter_status => live,
                depth => maps:get(depth, Spec),
                origin => maps:get(origin, Spec),
                metadata => maps:get(metadata, Spec, #{}),
                type_identity => maps:get(type_identity, Spec, dynamic),
                providers => lists:usort(maps:get(providers, Spec, [])),
                frame_identity => maps:get(
                    frame_identity,
                    Spec,
                    standalone
                ),
                budget => Budget,
                retained_words => retained_words(Continuation, Spec),
                captured_branch_depth => captured_branch_depth(Spec),
                invocation_count => 0,
                completed_branches => 0,
                failed_branches => 0,
                lease => #{
                    status => active,
                    delimiter => maps:get(delimiter, Spec),
                    frame_identity => maps:get(
                        frame_identity,
                        Spec,
                        standalone
                    )
                }
            },
            {ok, _PublicId} = gen_server:call(?SERVER, {capture, Ref, Entry}),
            {ok, {catena_resumption, ?VERSION, Ref}};
        {error, _} = Error ->
            Error
    end.

%% @doc Invoke a depth-aware resumption on its capturing process.
-spec resume(term(), term()) ->
    {ok, term()} | {error, control_failure()}.
resume(Handle, Value) ->
    resume_with_timeout(Handle, Value, default).

%% @doc Invoke with a same-process cooperative runtime timeout.
%%
%% The continuation is never moved to a worker process. The deadline is
%% attached to the restored explicit context for provider waits and checked
%% again when the continuation returns.
-spec resume(term(), term(), timeout()) ->
    {ok, term()} | {error, control_failure()}.
resume(Handle, Value, Timeout)
        when
            Timeout =:= infinity;
            is_integer(Timeout) andalso Timeout >= 0
        ->
    resume_with_timeout(Handle, Value, Timeout).

resume_with_timeout(Handle, Value, RequestedTimeout) ->
    case decode_handle(Handle) of
        {ok, Ref} ->
            ok = ensure_started(),
            case gen_server:call(?SERVER, {authorize, Ref, self()}) of
                {ok, Token, Invocation} ->
                    invoke(
                        Ref,
                        Token,
                        Value,
                        Invocation,
                        invocation_timeout(RequestedTimeout, Invocation)
                    );
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Idempotently abandon a fresh resumption and release its lease.
-spec discard(term()) -> ok | {error, control_failure()}.
discard(Handle) ->
    with_registered_handle(Handle, fun(Ref) ->
        gen_server:call(?SERVER, {discard, Ref})
    end).

%% @doc Expire the logical delimiter retained by a resumption.
-spec expire_delimiter(term()) -> ok | {error, control_failure()}.
expire_delimiter(Handle) ->
    with_registered_handle(Handle, fun(Ref) ->
        gen_server:call(?SERVER, {expire_delimiter, Ref})
    end).

%% @doc Recognize only a well-shaped handle for this runtime version.
%%
%% Registration is intentionally checked only by operations such as
%% `resume/2`; this predicate does not expose private registry state.
-spec is_resumption(term()) -> boolean().
is_resumption({catena_resumption, ?VERSION, Ref}) when is_reference(Ref) ->
    true;
is_resumption(_) ->
    false.

%% @doc Return the authoritative one-shot state without exposing internals.
-spec status(term()) ->
    {ok, fresh | running | consumed} | {error, control_failure()}.
status(Handle) ->
    case decode_handle(Handle) of
        {ok, Ref} ->
            ok = ensure_started(),
            gen_server:call(?SERVER, {status, Ref});
        {error, _} = Error ->
            Error
    end.

%% @doc Describe a registered resumption using public, non-authoritative data.
%%
%% No continuation, explicit context, PID, reference, private handle, or
%% forgeable runtime authority is returned by this API.
-spec describe(term()) -> {ok, map()} | {error, control_failure()}.
describe(Handle) ->
    case decode_handle(Handle) of
        {ok, Ref} ->
            ok = ensure_started(),
            gen_server:call(?SERVER, {describe, Ref, self()});
        {error, _} = Error ->
            Error
    end.

%% @doc Return only whether the private retention lease is active.
-spec lease_status(term()) ->
    {ok, active | released} | {error, control_failure()}.
lease_status(Handle) ->
    with_registered_handle(Handle, fun(Ref) ->
        gen_server:call(?SERVER, {lease_status, Ref})
    end).

%% @doc Return branch counters and budgets without exposing continuations.
-spec branch_stats(term()) -> {ok, map()} | {error, control_failure()}.
branch_stats(Handle) ->
    with_registered_handle(Handle, fun(Ref) ->
        gen_server:call(?SERVER, {branch_stats, Ref})
    end).

%% @doc Configure process-owned, bounded control tracing.
%%
%% `false` disables tracing. A map enables tracing and accepts `max_events`
%% and an optional `events` allow-list. Trace state belongs to the calling
%% process and never grants resumption authority.
-spec configure_trace(false | map()) -> ok | {error, term()}.
configure_trace(false) ->
    ok = ensure_started(),
    gen_server:call(?SERVER, {configure_trace, self(), false});
configure_trace(Options) when is_map(Options) ->
    case normalize_trace_options(Options) of
        {ok, Config} ->
            ok = ensure_started(),
            gen_server:call(?SERVER, {configure_trace, self(), Config});
        {error, _} = Error ->
            Error
    end;
configure_trace(Options) ->
    {error, {invalid_trace_options, Options}}.

%% @doc Return this process's redacted trace in evaluation order.
-spec trace() -> {ok, [map()]}.
trace() ->
    ok = ensure_started(),
    gen_server:call(?SERVER, {trace, self()}).

%% @doc Clear this process's retained trace without changing its configuration.
-spec clear_trace() -> ok.
clear_trace() ->
    ok = ensure_started(),
    gen_server:call(?SERVER, {clear_trace, self()}).

%% @doc Construct a stable runtime control failure.
-spec control_failure(atom(), term(), map()) -> control_failure().
control_failure(Category, Origin, Details) ->
    failure(Category, Origin, Details).

%% @doc Normalize an Erlang exception without retaining private runtime terms.
-spec normalize_exception(atom(), term(), term()) -> control_failure().
normalize_exception(
    error,
    {catena_runtime_control, #{category := _} = Failure},
    _Origin
) ->
    Failure;
normalize_exception(error, {effect_timeout, Effect, Operation}, Origin) ->
    failure(handler_failure, Origin, #{
        reason => timeout,
        effect => Effect,
        operation => Operation
    });
normalize_exception(error, {effect_error, Effect, Operation, _Reason}, Origin) ->
    failure(handler_failure, Origin, #{
        reason => provider_failure,
        effect => Effect,
        operation => Operation
    });
normalize_exception(
    error,
    {effect_runtime_error, {
        operation_arity_mismatch,
        Effect,
        Operation,
        ActualArity
    }},
    Origin
) ->
    failure(handler_failure, Origin, #{
        reason => operation_arity_mismatch,
        effect => Effect,
        operation => Operation,
        actual_arity => ActualArity
    });
normalize_exception(
    error,
    {no_handler_for_effect, Effect, Operation},
    Origin
) ->
    failure(unhandled_effect, Origin, #{
        effect => Effect,
        operation => Operation
    });
normalize_exception(Class, Reason, Origin) ->
    failure(handler_failure, Origin, #{
        class => Class,
        reason => sanitize_reason(Reason)
    }).

%% @doc Clear runtime authority between isolated component tests.
-spec reset_for_test() -> ok.
reset_for_test() ->
    ok = ensure_started(),
    gen_server:call(?SERVER, reset).

%%====================================================================
%% Invocation
%%====================================================================

-spec invoke(reference(), reference(), term(), map(), timeout()) ->
    {ok, term()} | {error, control_failure()}.
invoke(Ref, Token, Value, Invocation = #{
    continuation := Continuation,
    context := Context,
    origin := Origin
}, Timeout) ->
    StartedAt = erlang:monotonic_time(millisecond),
    StartedReductions = process_reductions(),
    BranchContext = context_with_branch(Context, Invocation),
    RestoredContext = context_with_deadline(
        BranchContext,
        StartedAt,
        Timeout
    ),
    InitialOutcome =
        try
            {ok, Continuation(Value, RestoredContext)}
        catch
            Class:Reason:_Stack ->
                {error, normalize_exception(Class, Reason, Origin)}
        end,
    TimedOutcome = apply_timeout(
        InitialOutcome,
        StartedAt,
        Timeout,
        Invocation
    ),
    Outcome = apply_reduction_budget(
        TimedOutcome,
        StartedReductions,
        Invocation
    ),
    CompletionReason = case Outcome of
        {ok, _} -> normal;
        {error, Failure} -> {exceptional, Failure}
    end,
    case gen_server:call(?SERVER, {complete, Ref, Token, CompletionReason}) of
        ok ->
            Outcome;
        {error, _} = CompletionError ->
            CompletionError
    end.

invocation_timeout(Requested, #{kind := one_shot}) ->
    case Requested of
        default -> infinity;
        Timeout -> Timeout
    end;
invocation_timeout(Requested, #{kind := multi_shot, budget := Budget}) ->
    BudgetTimeout = maps:get(timeout, Budget),
    case Requested of
        default -> BudgetTimeout;
        infinity -> BudgetTimeout;
        Timeout -> erlang:min(Timeout, BudgetTimeout)
    end.

context_with_branch(Context, #{kind := one_shot}) ->
    Context;
context_with_branch(Context, #{kind := multi_shot, branch := Branch}) ->
    Stack = maps:get(runtime_branch_stack, Context, []),
    Context#{
        runtime_branch => Branch,
        runtime_branch_stack => [Branch | Stack]
    }.

-spec context_with_deadline(map(), integer(), timeout()) -> map().
context_with_deadline(Context, _StartedAt, infinity) ->
    Context;
context_with_deadline(Context, StartedAt, Timeout) ->
    Context#{runtime_deadline => StartedAt + Timeout}.

-spec apply_timeout(
    {ok, term()} | {error, control_failure()},
    integer(),
    timeout(),
    map()
) -> {ok, term()} | {error, control_failure()}.
apply_timeout({ok, _Value}, StartedAt, Timeout, Invocation)
        when is_integer(Timeout) ->
    FinishedAt = erlang:monotonic_time(millisecond),
    case FinishedAt - StartedAt >= Timeout of
        true ->
            timeout_failure(Invocation, Timeout);
        false ->
            {ok, _Value}
    end;
apply_timeout(Outcome, _StartedAt, _Timeout, _Invocation) ->
    Outcome.

timeout_failure(#{kind := multi_shot, origin := Origin}, Limit) ->
    {error, budget_failure(Origin, timeout, Limit, Limit)};
timeout_failure(#{origin := Origin}, _Limit) ->
    {error, failure(handler_failure, Origin, #{reason => timeout})}.

apply_reduction_budget(
    Outcome,
    StartedReductions,
    #{kind := multi_shot, budget := Budget, origin := Origin}
) ->
    Used = process_reductions() - StartedReductions,
    Limit = maps:get(max_reductions, Budget),
    case Used > Limit of
        true -> {error, budget_failure(Origin, reductions, Limit, Used)};
        false -> Outcome
    end;
apply_reduction_budget(Outcome, _StartedReductions, _Invocation) ->
    Outcome.

process_reductions() ->
    {reductions, Reductions} = process_info(self(), reductions),
    Reductions.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(list()) -> {ok, registry_state()}.
init([]) ->
    {ok, #{entries => #{}, traces => #{}, next_resumption_id => 1}}.

handle_call({capture, Ref, Entry}, _From, State) ->
    Entries0 = maps:get(entries, State),
    PublicId = maps:get(next_resumption_id, State),
    Entry1 = install_lifetime_monitors(Entry#{public_id => PublicId}),
    State1 = State#{
        entries := Entries0#{Ref => Entry1},
        next_resumption_id := PublicId + 1
    },
    State2 = append_entry_event(Entry1, capture, #{}, State1),
    State3 = append_entry_event(
        Entry1,
        handler_selection,
        handler_selection_details(Entry1),
        State2
    ),
    {reply, {ok, PublicId}, State3};
handle_call({authorize, Ref, Caller}, _From, State) ->
    Entries0 = maps:get(entries, State),
    case maps:find(Ref, Entries0) of
        error ->
            {reply, invalid_registered_handle(), State};
        {ok, Entry} ->
            case authorize_entry(Entry, Caller) of
                {ok, Token, Entry1} ->
                    Invocation = invocation_data(Entry1),
                    Entries1 = Entries0#{Ref := Entry1},
                    State1 = State#{entries := Entries1},
                    State2 = append_entry_event(Entry1, resume, #{}, State1),
                    State3 = case maps:get(kind, Entry1) of
                        multi_shot ->
                            append_entry_event(Entry1, branch, #{
                                phase => start,
                                branch => public_branch(
                                    maps:get(current_branch, Entry1)
                                )
                            }, State2);
                        one_shot ->
                            State2
                    end,
                    {reply, {ok, Token, Invocation}, State3};
                {error, #{category := Category}} = Error
                        when
                            Category =:= expired_resumption_owner;
                            Category =:= handler_failure
                        ->
                    Entry1 = expire_for_category(Category, Entry),
                    Entries1 = Entries0#{Ref := Entry1},
                    {reply, Error, State#{entries := Entries1}};
                {error, _} = Error ->
                    {reply, Error, State}
            end
    end;
handle_call({complete, Ref, Token, Reason}, _From, State) ->
    Entries0 = maps:get(entries, State),
    case maps:find(Ref, Entries0) of
        {ok, #{
            state := running,
            run_token := Token,
            kind := one_shot
        } = Entry} ->
            PublicReason = public_completion_reason(Reason),
            Entry1 = release_entry(
                Entry#{
                    state := consumed,
                    consumed_reason => PublicReason
                }
            ),
            State1 = State#{entries := Entries0#{Ref := Entry1}},
            State2 = append_timeout_event(Entry1, Reason, State1),
            State3 = append_entry_event(Entry1, consumption, #{
                reason => PublicReason
            }, State2),
            State4 = append_entry_event(Entry1, cleanup, #{
                reason => PublicReason
            }, State3),
            {reply, ok, State4};
        {ok, #{
            state := running,
            run_token := Token,
            kind := multi_shot
        } = Entry} ->
            PublicReason = public_completion_reason(Reason),
            Entry1 = complete_multishot_branch(Entry, PublicReason),
            State1 = State#{entries := Entries0#{Ref := Entry1}},
            State2 = append_timeout_event(Entry1, Reason, State1),
            State3 = append_entry_event(Entry1, branch, #{
                phase => complete,
                branch => public_branch(maps:get(last_branch, Entry1))
            }, State2),
            {reply, ok, State3};
        {ok, Entry} ->
            Error = failure(
                invalid_resumption,
                maps:get(origin, Entry, undefined),
                #{reason => invalid_completion_authority}
            ),
            {reply, {error, Error}, State};
        error ->
            {reply, invalid_registered_handle(), State}
    end;
handle_call({discard, Ref}, _From, State) ->
    Entries0 = maps:get(entries, State),
    case maps:find(Ref, Entries0) of
        {ok, #{state := consumed}} ->
            {reply, ok, State};
        {ok, #{expired := _}} ->
            {reply, ok, State};
        {ok, #{state := fresh} = Entry} ->
            Entry1 = release_entry(
                Entry#{
                    state := consumed,
                    consumed_reason => abandoned
                }
            ),
            State1 = State#{entries := Entries0#{Ref := Entry1}},
            State2 = append_entry_event(Entry1, abort, #{
                reason => abandoned
            }, State1),
            State3 = append_entry_event(Entry1, consumption, #{
                reason => abandoned
            }, State2),
            State4 = append_entry_event(Entry1, cleanup, #{
                reason => abandoned
            }, State3),
            {reply, ok, State4};
        {ok, #{state := running} = Entry} ->
            Error = failure(handler_failure, maps:get(origin, Entry), #{
                reason => cleanup_while_running
            }),
            {reply, {error, Error}, State};
        error ->
            {reply, invalid_registered_handle(), State}
    end;
handle_call({expire_delimiter, Ref}, _From, State) ->
    Entries0 = maps:get(entries, State),
    case maps:find(Ref, Entries0) of
        {ok, #{state := consumed}} ->
            {reply, ok, State};
        {ok, Entry} ->
            Entry1 = release_entry(
                Entry#{
                    delimiter_status := expired,
                    expired => delimiter
                }
            ),
            State1 = State#{entries := Entries0#{Ref := Entry1}},
            State2 = append_entry_event(Entry1, abort, #{
                reason => expired_delimiter
            }, State1),
            State3 = append_entry_event(Entry1, cleanup, #{
                reason => expired_delimiter
            }, State2),
            {reply, ok, State3};
        error ->
            {reply, invalid_registered_handle(), State}
    end;
handle_call({status, Ref}, _From, State) ->
    case maps:find(Ref, maps:get(entries, State)) of
        {ok, Entry} ->
            {reply, {ok, maps:get(state, Entry)}, State};
        error ->
            {reply, invalid_registered_handle(), State}
    end;
handle_call({describe, Ref, Caller}, _From, State) ->
    case maps:find(Ref, maps:get(entries, State)) of
        {ok, Entry} ->
            {reply, {ok, public_description(Entry, Caller)}, State};
        error ->
            {reply, invalid_registered_handle(), State}
    end;
handle_call({lease_status, Ref}, _From, State) ->
    case maps:find(Ref, maps:get(entries, State)) of
        {ok, Entry} ->
            Lease = maps:get(lease, Entry),
            {reply, {ok, maps:get(status, Lease)}, State};
        error ->
            {reply, invalid_registered_handle(), State}
    end;
handle_call({branch_stats, Ref}, _From, State) ->
    case maps:find(Ref, maps:get(entries, State)) of
        {ok, Entry} ->
            {reply, {ok, public_branch_stats(Entry)}, State};
        error ->
            {reply, invalid_registered_handle(), State}
    end;
handle_call({configure_trace, Owner, false}, _From, State) ->
    Traces0 = maps:get(traces, State),
    case maps:find(Owner, Traces0) of
        {ok, Trace0} ->
            Trace1 = Trace0#{enabled := false},
            {reply, ok, State#{traces := Traces0#{Owner := Trace1}}};
        error ->
            {reply, ok, State}
    end;
handle_call({configure_trace, Owner, Config}, _From, State) ->
    Traces0 = maps:get(traces, State),
    Previous = maps:get(Owner, Traces0, #{}),
    Trace = Config#{
        next_sequence => maps:get(next_sequence, Previous, 1),
        events_rev => maps:get(events_rev, Previous, []),
        dropped => maps:get(dropped, Previous, 0)
    },
    {reply, ok, State#{traces := Traces0#{Owner => Trace}}};
handle_call({trace, Owner}, _From, State) ->
    Trace = maps:get(Owner, maps:get(traces, State), #{}),
    {reply, {ok, lists:reverse(maps:get(events_rev, Trace, []))}, State};
handle_call({clear_trace, Owner}, _From, State) ->
    Traces0 = maps:get(traces, State),
    case maps:find(Owner, Traces0) of
        {ok, Trace0} ->
            Trace1 = Trace0#{events_rev := [], dropped := 0},
            {reply, ok, State#{traces := Traces0#{Owner := Trace1}}};
        error ->
            {reply, ok, State}
    end;
handle_call(reset, _From, State) ->
    release_all_entries(maps:values(maps:get(entries, State))),
    {reply, ok, #{entries => #{}, traces => #{}, next_resumption_id => 1}};
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', Monitor, process, _Pid, _Reason}, State) ->
    Entries0 = maps:get(entries, State),
    Entries1 = maps:map(
        fun(_Ref, Entry) ->
            expire_monitored_entry(Monitor, Entry)
        end,
        Entries0
    ),
    {noreply, State#{entries := Entries1}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    release_all_entries(maps:values(maps:get(entries, State, #{}))),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Validation and state transitions
%%====================================================================

-spec authorize_entry(entry(), pid()) ->
    {ok, reference(), entry()} | {error, control_failure()}.
authorize_entry(#{expired := owner} = Entry, _Caller) ->
    {error, failure(
        expired_resumption_owner,
        maps:get(origin, Entry),
        #{}
    )};
authorize_entry(#{expired := delimiter} = Entry, _Caller) ->
    {error, failure(
        stale_resumption_delimiter,
        maps:get(origin, Entry),
        #{reason => expired_frame}
    )};
authorize_entry(#{expired := provider} = Entry, _Caller) ->
    {error, failure(
        handler_failure,
        maps:get(origin, Entry),
        #{reason => provider_unavailable}
    )};
authorize_entry(Entry, Caller) ->
    Origin = maps:get(origin, Entry),
    Owner = maps:get(owner, Entry),
    case is_process_alive(Owner) of
        false ->
            {error, failure(expired_resumption_owner, Origin, #{})};
        true when Caller =/= Owner ->
            {error, failure(wrong_resumption_owner, Origin, #{})};
        true ->
            case providers_alive(maps:get(providers, Entry, [])) of
                false ->
                    {error, failure(handler_failure, Origin, #{
                        reason => provider_unavailable
                    })};
                true ->
                    authorize_live_entry(Entry, Origin)
            end
    end.

-spec authorize_live_entry(entry(), term()) ->
    {ok, reference(), entry()} | {error, control_failure()}.
authorize_live_entry(#{delimiter_status := Status}, Origin)
        when Status =/= live ->
    {error, failure(stale_resumption_delimiter, Origin, #{})};
authorize_live_entry(#{kind := Kind}, Origin)
        when Kind =/= one_shot, Kind =/= multi_shot ->
    {error, failure(unsupported_semantic_mode, Origin, #{
        kind => Kind
    })};
authorize_live_entry(#{depth := Depth}, Origin)
        when Depth =/= deep, Depth =/= shallow ->
    {error, failure(unsupported_semantic_mode, Origin, #{
        depth => Depth
    })};
authorize_live_entry(#{state := running}, Origin) ->
    {error, failure(resumption_reentrant, Origin, #{})};
authorize_live_entry(#{state := consumed}, Origin) ->
    {error, failure(resumption_already_consumed, Origin, #{})};
authorize_live_entry(#{state := fresh, kind := one_shot} = Entry, _Origin) ->
    Token = make_ref(),
    {ok, Token, Entry#{
        state := running,
        run_token => Token,
        invocation_count := 1
    }};
authorize_live_entry(#{state := fresh, kind := multi_shot} = Entry, Origin) ->
    Budget = maps:get(budget, Entry),
    Count = maps:get(invocation_count, Entry),
    MaxInvocations = maps:get(max_invocations, Budget),
    BranchDepth = maps:get(captured_branch_depth, Entry) + 1,
    MaxDepth = maps:get(max_branch_depth, Budget),
    case {Count < MaxInvocations, BranchDepth =< MaxDepth} of
        {false, _} ->
            {error, budget_failure(
                Origin,
                invocations,
                MaxInvocations,
                Count
            )};
        {_, false} ->
            {error, budget_failure(
                Origin,
                branch_depth,
                MaxDepth,
                BranchDepth
            )};
        {true, true} ->
            Token = make_ref(),
            Branch = #{
                id => Count + 1,
                depth => BranchDepth,
                status => running
            },
            {ok, Token, Entry#{
                state := running,
                run_token => Token,
                invocation_count := Count + 1,
                current_branch => Branch
            }}
    end.

-spec invocation_data(entry()) -> map().
invocation_data(Entry) ->
    #{
        continuation => maps:get(continuation, Entry),
        context => restored_context(Entry),
        delimiter => maps:get(delimiter, Entry),
        depth => maps:get(depth, Entry),
        kind => maps:get(kind, Entry),
        origin => maps:get(origin, Entry),
        metadata => maps:get(metadata, Entry),
        type_identity => maps:get(type_identity, Entry),
        budget => maps:get(budget, Entry),
        branch => maps:get(current_branch, Entry, none)
    }.

complete_multishot_branch(Entry, Reason) ->
    Branch0 = maps:get(current_branch, Entry),
    Branch = Branch0#{status => Reason},
    Failed0 = maps:get(failed_branches, Entry),
    Failed = case Reason of
        normal -> Failed0;
        exceptional -> Failed0 + 1
    end,
    maps:without(
        [run_token, current_branch],
        Entry#{
            state := fresh,
            completed_branches := maps:get(completed_branches, Entry) + 1,
            failed_branches := Failed,
            last_branch => Branch
        }
    ).

public_branch_stats(Entry) ->
    #{
        kind => maps:get(kind, Entry),
        state => maps:get(state, Entry),
        invocation_count => maps:get(invocation_count, Entry),
        completed_branches => maps:get(completed_branches, Entry),
        failed_branches => maps:get(failed_branches, Entry),
        retained_words => maps:get(retained_words, Entry),
        budget => maps:get(budget, Entry),
        current_branch => maps:get(current_branch, Entry, none),
        last_branch => maps:get(last_branch, Entry, none)
    }.

%%====================================================================
%% Public diagnostics and bounded tracing
%%====================================================================

public_description(Entry, Caller) ->
    Lease = maps:get(lease, Entry, #{status => released}),
    #{
        id => maps:get(public_id, Entry),
        type => public_term(maps:get(type_identity, Entry, dynamic)),
        kind => maps:get(kind, Entry),
        owner_relationship => case maps:get(owner, Entry) of
            Caller -> current_process;
            _Other -> foreign_process
        end,
        state => maps:get(state, Entry),
        depth => maps:get(depth, Entry),
        capture_location => public_origin(maps:get(origin, Entry)),
        lifetime => maps:get(expired, Entry, maps:get(status, Lease))
    }.

normalize_trace_options(Options) ->
    Enabled = maps:get(enabled, Options, true),
    MaxEvents = maps:get(max_events, Options, 256),
    Events = maps:get(events, Options, all),
    case {
        is_boolean(Enabled),
        is_integer(MaxEvents) andalso MaxEvents > 0,
        valid_trace_filter(Events)
    } of
        {true, true, true} ->
            {ok, #{
                enabled => Enabled,
                max_events => MaxEvents,
                events => Events
            }};
        _ ->
            {error, {invalid_trace_options, public_term(Options)}}
    end.

valid_trace_filter(all) ->
    true;
valid_trace_filter(Events) when is_list(Events) ->
    Allowed = trace_event_kinds(),
    lists:all(fun(Event) -> lists:member(Event, Allowed) end, Events);
valid_trace_filter(_Events) ->
    false.

trace_event_kinds() ->
    [
        capture,
        handler_selection,
        resume,
        abort,
        branch,
        consumption,
        timeout,
        cleanup
    ].

append_entry_event(Entry, Kind, Details, State) ->
    Owner = maps:get(owner, Entry),
    Traces0 = maps:get(traces, State),
    case maps:find(Owner, Traces0) of
        {ok, #{enabled := true} = Trace0} ->
            case trace_kind_enabled(Kind, Trace0) of
                true ->
                    Sequence = maps:get(next_sequence, Trace0),
                    Event = #{
                        sequence => Sequence,
                        event => Kind,
                        resumption_id => maps:get(public_id, Entry),
                        kind => maps:get(kind, Entry),
                        depth => maps:get(depth, Entry),
                        source => public_origin(maps:get(origin, Entry)),
                        details => public_term(Details)
                    },
                    Limit = maps:get(max_events, Trace0),
                    Events0 = maps:get(events_rev, Trace0),
                    Events1 = lists:sublist([Event | Events0], Limit),
                    Dropped = maps:get(dropped, Trace0) +
                        case length(Events0) >= Limit of
                            true -> 1;
                            false -> 0
                        end,
                    Trace1 = Trace0#{
                        next_sequence := Sequence + 1,
                        events_rev := Events1,
                        dropped := Dropped
                    },
                    State#{traces := Traces0#{Owner := Trace1}};
                false ->
                    State
            end;
        _ ->
            State
    end.

trace_kind_enabled(Kind, Trace) ->
    case maps:get(events, Trace) of
        all -> true;
        Events -> lists:member(Kind, Events)
    end.

handler_selection_details(Entry) ->
    Metadata = maps:get(metadata, Entry, #{}),
    maps:with([effect, operation], Metadata).

append_timeout_event(Entry, Reason, State) ->
    case timeout_details(Reason) of
        none ->
            State;
        Details ->
            append_entry_event(Entry, timeout, Details, State)
    end.

timeout_details({exceptional, #{details := #{reason := timeout} = Details}}) ->
    maps:with([reason, effect, operation], Details);
timeout_details({exceptional, #{details := #{resource := timeout} = Details}}) ->
    maps:with([resource, limit, observed], Details);
timeout_details(_Reason) ->
    none.

public_completion_reason(normal) ->
    normal;
public_completion_reason({exceptional, _Failure}) ->
    exceptional;
public_completion_reason(exceptional) ->
    exceptional.

public_branch(none) ->
    none;
public_branch(Branch) when is_map(Branch) ->
    maps:with([id, depth, status], Branch).

public_origin(Origin) when is_map(Origin) ->
    maps:from_list([
        {Key, public_origin_value(Key, Value)}
        || {Key, Value} <- maps:to_list(Origin),
           public_origin_key(Key)
    ]);
public_origin({location, Line, Column})
        when is_integer(Line), is_integer(Column) ->
    #{line => Line, column => Column};
public_origin({Line, Column}) when is_integer(Line), is_integer(Column) ->
    #{line => Line, column => Column};
public_origin(Origin) when is_atom(Origin); is_binary(Origin); is_list(Origin) ->
    public_term(Origin);
public_origin(_Origin) ->
    unknown.

public_origin_key(Key) ->
    lists:member(Key, [
        source,
        location,
        file,
        module,
        transform,
        construct,
        perform,
        handler_case,
        delimiter,
        effect,
        operation
    ]).

public_origin_value(Key, Value)
        when Key =:= source; Key =:= location; Key =:= perform;
             Key =:= handler_case; Key =:= delimiter ->
    public_origin(Value);
public_origin_value(_Key, Value) ->
    public_term(Value).

public_term({catena_resumption, _Version, _Authority}) ->
    resumption;
public_term(Term) when
    is_atom(Term);
    is_binary(Term);
    is_integer(Term);
    is_float(Term)
->
    Term;
public_term(Term) when is_list(Term) ->
    [public_term(Item) || Item <- Term];
public_term(Term) when is_tuple(Term) ->
    list_to_tuple([public_term(Item) || Item <- tuple_to_list(Term)]);
public_term(Term) when is_map(Term) ->
    maps:from_list([
        {public_term(Key), public_term(Value)}
        || {Key, Value} <- maps:to_list(Term)
    ]);
public_term(Term) when is_function(Term) ->
    closure;
public_term(Term) when is_pid(Term) ->
    process;
public_term(Term) when is_reference(Term) ->
    opaque_reference;
public_term(Term) when is_port(Term) ->
    port;
public_term(_Term) ->
    opaque.

-spec restored_context(entry()) -> map().
restored_context(#{depth := deep} = Entry) ->
    maps:get(context, Entry);
restored_context(#{depth := shallow} = Entry) ->
    maps:get(parent_context, Entry).

-spec validate_capture(term(), term()) ->
    ok | {error, control_failure()}.
validate_capture(Continuation, Spec)
        when is_function(Continuation, 2), is_map(Spec) ->
    Required = [context, delimiter, depth, kind, origin],
    case [Key || Key <- Required, not maps:is_key(Key, Spec)] of
        [_ | _] = Missing ->
            {error, failure(invalid_resumption, undefined, #{
                reason => missing_capture_fields,
                fields => Missing
            })};
        [] ->
            validate_capture_fields(Continuation, Spec)
    end;
validate_capture(_Continuation, _Spec) ->
    {error, failure(invalid_resumption, undefined, #{
        reason => invalid_compiler_capture
    })}.

-spec validate_capture_fields(function(), map()) ->
    ok | {error, control_failure()}.
validate_capture_fields(Continuation, Spec) ->
    Context = maps:get(context, Spec),
    Delimiter = maps:get(delimiter, Spec),
    Depth = maps:get(depth, Spec),
    Kind = maps:get(kind, Spec),
    Origin = maps:get(origin, Spec),
    Metadata = maps:get(metadata, Spec, #{}),
    ParentContext = maps:get(parent_context, Spec, Context),
    Providers = maps:get(providers, Spec, []),
    case {
        is_map(Context),
        is_map(ParentContext),
        is_reference(Delimiter),
        Depth,
        Kind,
        Origin =/= undefined,
        is_map(Metadata),
        is_list(Providers) andalso
            lists:all(fun is_pid/1, Providers)
    } of
        {false, _, _, _, _, _, _, _} ->
            invalid_capture_field(context);
        {_, false, _, _, _, _, _, _} ->
            invalid_capture_field(parent_context);
        {_, _, false, _, _, _, _, _} ->
            invalid_capture_field(delimiter);
        {_, _, true, Depth, Kind, true, true, true}
                when
                    (Depth =:= deep orelse Depth =:= shallow),
                    (Kind =:= one_shot orelse Kind =:= multi_shot)
                ->
            validate_capture_policy(Continuation, Spec);
        {_, _, true, UnsupportedDepth, Kind, true, true, true}
                when Kind =:= one_shot; Kind =:= multi_shot ->
            {error, failure(unsupported_semantic_mode, Origin, #{
                depth => UnsupportedDepth
            })};
        {_, _, true, Depth, UnsupportedKind, true, true, true}
                when Depth =:= deep; Depth =:= shallow ->
            {error, failure(unsupported_semantic_mode, Origin, #{
                kind => UnsupportedKind
            })};
        {_, _, _, _, _, false, _, _} ->
            invalid_capture_field(origin);
        {_, _, _, _, _, _, false, _} ->
            invalid_capture_field(metadata);
        {_, _, _, _, _, _, _, false} ->
            invalid_capture_field(providers)
    end.

validate_capture_policy(Continuation, Spec) ->
    case validate_budget(maps:get(budget, Spec, #{}), maps:get(origin, Spec)) of
        ok ->
            case maps:get(kind, Spec) of
                one_shot ->
                    ok;
                multi_shot ->
                    validate_multishot_capture(Continuation, Spec)
            end;
        {error, _} = Error ->
            Error
    end.

validate_budget(Override, Origin) when is_map(Override) ->
    Defaults = default_budget(),
    Unknown = maps:keys(Override) -- maps:keys(Defaults),
    Budget = maps:merge(Defaults, Override),
    ValidValues = lists:all(
        fun(Key) ->
            Value = maps:get(Key, Budget),
            is_integer(Value) andalso Value > 0
        end,
        maps:keys(Defaults)
    ),
    case {Unknown, ValidValues} of
        {[], true} ->
            ok;
        {[_ | _], _} ->
            {error, failure(invalid_resumption, Origin, #{
                reason => unknown_budget_fields,
                fields => Unknown
            })};
        {[], false} ->
            {error, failure(invalid_resumption, Origin, #{
                reason => invalid_resumption_budget,
                budget => sanitize_budget(Budget)
            })}
    end;
validate_budget(_Override, Origin) ->
    {error, failure(invalid_resumption, Origin, #{
        reason => invalid_resumption_budget
    })}.

validate_multishot_capture(Continuation, Spec) ->
    Origin = maps:get(origin, Spec),
    Providers = maps:get(providers, Spec, []),
    Context = maps:get(context, Spec),
    ParentContext = maps:get(parent_context, Spec, Context),
    RetainedWords = retained_words(Continuation, Spec),
    Limit = maps:get(max_retained_words, normalized_budget(Spec)),
    case {
        Providers,
        context_branch_safe(Context),
        context_branch_safe(ParentContext),
        lexical_environment_safe(Continuation),
        RetainedWords =< Limit
    } of
        {[_ | _], _, _, _, _} ->
            inadmissible_multishot_context(Origin, process_provider);
        {[], {error, Reason}, _, _, _} ->
            inadmissible_multishot_context(Origin, Reason);
        {[], ok, {error, Reason}, _, _} ->
            inadmissible_multishot_context(Origin, Reason);
        {[], ok, ok, false, _} ->
            inadmissible_multishot_context(Origin, lexical_capability);
        {[], ok, ok, true, false} ->
            {error, budget_failure(
                Origin,
                retained_words,
                Limit,
                RetainedWords
            )};
        {[], ok, ok, true, true} ->
            ok
    end.

context_branch_safe(Context) when is_map(Context) ->
    Handlers = maps:get(handlers, Context, #{}),
    Entries = maps:get(entries, Context, []),
    case {map_size(Handlers), branch_safe_entries(Entries)} of
        {Size, _} when Size > 0 ->
            {error, process_provider};
        {0, {error, _} = Error} ->
            Error;
        {0, ok} ->
            case maps:get(parent, Context, undefined) of
                undefined -> ok;
                Parent -> context_branch_safe(Parent)
            end
    end;
context_branch_safe(_Context) ->
    {error, invalid_context}.

branch_safe_entries([]) ->
    ok;
branch_safe_entries([#{kind := local_resumable} | Rest]) ->
    branch_safe_entries(Rest);
branch_safe_entries([#{kind := local_value_provider} | _Rest]) ->
    {error, local_provider_state};
branch_safe_entries([#{kind := process_provider} | _Rest]) ->
    {error, process_provider};
branch_safe_entries([_Entry | _Rest]) ->
    {error, unknown_handler_state}.

lexical_environment_safe(Continuation) ->
    {env, Environment} = erlang:fun_info(Continuation, env),
    not lists:any(fun unsafe_lexical_term/1, Environment).

unsafe_lexical_term(Term) when is_pid(Term); is_port(Term); is_reference(Term) ->
    true;
unsafe_lexical_term(Term) when is_tuple(Term) ->
    lists:any(fun unsafe_lexical_term/1, tuple_to_list(Term));
unsafe_lexical_term(Term) when is_list(Term) ->
    lists:any(fun unsafe_lexical_term/1, Term);
unsafe_lexical_term(Term) when is_map(Term) ->
    lists:any(fun unsafe_lexical_term/1, maps:to_list(Term));
unsafe_lexical_term(_Term) ->
    false.

inadmissible_multishot_context(Origin, Reason) ->
    {error, failure(inadmissible_multishot_context, Origin, #{
        reason => Reason
    })}.

normalized_budget(Spec) ->
    maps:merge(default_budget(), maps:get(budget, Spec, #{})).

sanitize_budget(Budget) ->
    maps:with(maps:keys(default_budget()), Budget).

retained_words(Continuation, Spec) ->
    erts_debug:flat_size({
        Continuation,
        maps:get(context, Spec),
        maps:get(parent_context, Spec, maps:get(context, Spec)),
        maps:get(metadata, Spec, #{}),
        maps:get(type_identity, Spec, dynamic)
    }).

captured_branch_depth(Spec) ->
    Context = maps:get(context, Spec),
    length(maps:get(runtime_branch_stack, Context, [])).

budget_failure(Origin, Resource, Limit, Observed) ->
    failure(resumption_budget_exceeded, Origin, #{
        resource => Resource,
        limit => Limit,
        observed => Observed
    }).

-spec invalid_capture_field(atom()) -> {error, control_failure()}.
invalid_capture_field(Field) ->
    {error, failure(invalid_resumption, undefined, #{
        reason => invalid_capture_field,
        field => Field
    })}.

-spec decode_handle(term()) ->
    {ok, reference()} | {error, control_failure()}.
decode_handle({catena_resumption, ?VERSION, Ref}) when is_reference(Ref) ->
    {ok, Ref};
decode_handle({catena_resumption, Version, _Opaque}) when is_integer(Version) ->
    {error, failure(invalid_resumption_version, undefined, #{
        version => Version,
        supported => ?VERSION
    })};
decode_handle(_Other) ->
    {error, failure(invalid_resumption, undefined, #{
        reason => malformed_or_forged
    })}.

-spec invalid_registered_handle() -> {error, control_failure()}.
invalid_registered_handle() ->
    {error, failure(invalid_resumption, undefined, #{
        reason => unregistered_authority
    })}.

-spec with_registered_handle(term(), fun((reference()) -> T)) ->
    T | {error, control_failure()}.
with_registered_handle(Handle, Fun) ->
    case decode_handle(Handle) of
        {ok, Ref} ->
            ok = ensure_started(),
            Fun(Ref);
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Lifetime monitors and leases
%%====================================================================

-spec install_lifetime_monitors(entry()) -> entry().
install_lifetime_monitors(Entry) ->
    OwnerMonitor = erlang:monitor(process, maps:get(owner, Entry)),
    ProviderMonitors = maps:from_list([
        {erlang:monitor(process, Provider), Provider}
        || Provider <- maps:get(providers, Entry, [])
    ]),
    Entry#{
        owner_monitor => OwnerMonitor,
        provider_monitors => ProviderMonitors
    }.

-spec release_entry(entry()) -> entry().
release_entry(Entry) ->
    demonitor_entry(Entry),
    Lease0 = maps:get(lease, Entry, #{status => active}),
    maps:without(
        [
            continuation,
            context,
            parent_context,
            run_token,
            owner_monitor,
            provider_monitors,
            providers
        ],
        Entry#{
            lease := Lease0#{status := released}
        }
    ).

-spec demonitor_entry(entry()) -> ok.
demonitor_entry(Entry) ->
    case maps:find(owner_monitor, Entry) of
        {ok, OwnerMonitor} ->
            erlang:demonitor(OwnerMonitor, [flush]);
        error ->
            ok
    end,
    lists:foreach(
        fun(Monitor) ->
            erlang:demonitor(Monitor, [flush])
        end,
        maps:keys(maps:get(provider_monitors, Entry, #{}))
    ),
    ok.

-spec release_all_entries([entry()]) -> ok.
release_all_entries(Entries) ->
    lists:foreach(fun demonitor_entry/1, Entries),
    ok.

-spec expire_monitored_entry(reference(), entry()) -> entry().
expire_monitored_entry(Monitor, Entry) ->
    case maps:get(owner_monitor, Entry, undefined) of
        Monitor ->
            release_entry(Entry#{expired => owner});
        _Other ->
            case maps:is_key(
                Monitor,
                maps:get(provider_monitors, Entry, #{})
            ) of
                true ->
                    release_entry(Entry#{expired => provider});
                false ->
                    Entry
            end
    end.

-spec expire_for_category(atom(), entry()) -> entry().
expire_for_category(expired_resumption_owner, Entry) ->
    release_entry(Entry#{expired => owner});
expire_for_category(handler_failure, Entry) ->
    release_entry(Entry#{expired => provider}).

-spec providers_alive([pid()]) -> boolean().
providers_alive(Providers) ->
    lists:all(fun erlang:is_process_alive/1, Providers).

-spec failure(atom(), term(), map()) -> control_failure().
failure(Category, Origin, Details) ->
    #{
        category => Category,
        origin => Origin,
        details => Details
    }.

-spec sanitize_reason(term()) -> term().
sanitize_reason(Reason) when
    is_atom(Reason);
    is_binary(Reason);
    is_integer(Reason);
    is_float(Reason)
->
    Reason;
sanitize_reason(_Reason) ->
    handler_failed.

-spec ensure_started() -> ok.
ensure_started() ->
    case whereis(?SERVER) of
        Pid when is_pid(Pid) ->
            ok;
        undefined ->
            case gen_server:start({local, ?SERVER}, ?MODULE, [], []) of
                {ok, _Pid} ->
                    ok;
                {error, {already_started, _Pid}} ->
                    ok
            end
    end.
