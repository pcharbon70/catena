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
    lease_status/1,
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
-define(VERSION, 2).

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
    frame_identity => term()
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
    consumed_reason => normal | exceptional | abandoned
}.

-type registry_state() :: #{
    entries := #{reference() => entry()}
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
        retained_resumptions,
        same_process_resume
    ].

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
            ok = gen_server:call(?SERVER, {capture, Ref, Entry}),
            {ok, {catena_resumption, ?VERSION, Ref}};
        {error, _} = Error ->
            Error
    end.

%% @doc Invoke a depth-aware one-shot resumption on its capturing process.
-spec resume(term(), term()) ->
    {ok, term()} | {error, control_failure()}.
resume(Handle, Value) ->
    resume(Handle, Value, infinity).

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
    case decode_handle(Handle) of
        {ok, Ref} ->
            ok = ensure_started(),
            case gen_server:call(?SERVER, {authorize, Ref, self()}) of
                {ok, Token, Invocation} ->
                    invoke(Ref, Token, Value, Invocation, Timeout);
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

%% @doc Return only whether the private retention lease is active.
-spec lease_status(term()) ->
    {ok, active | released} | {error, control_failure()}.
lease_status(Handle) ->
    with_registered_handle(Handle, fun(Ref) ->
        gen_server:call(?SERVER, {lease_status, Ref})
    end).

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
invoke(Ref, Token, Value, #{
    continuation := Continuation,
    context := Context,
    origin := Origin
}, Timeout) ->
    StartedAt = erlang:monotonic_time(millisecond),
    RestoredContext = context_with_deadline(Context, StartedAt, Timeout),
    InitialOutcome =
        try
            {ok, Continuation(Value, RestoredContext)}
        catch
            Class:Reason:_Stack ->
                {error, normalize_exception(Class, Reason, Origin)}
        end,
    Outcome = apply_timeout(
        InitialOutcome,
        StartedAt,
        Timeout,
        Origin
    ),
    CompletionReason = case Outcome of
        {ok, _} -> normal;
        {error, _} -> exceptional
    end,
    case gen_server:call(?SERVER, {complete, Ref, Token, CompletionReason}) of
        ok ->
            Outcome;
        {error, _} = CompletionError ->
            CompletionError
    end.

-spec context_with_deadline(map(), integer(), timeout()) -> map().
context_with_deadline(Context, _StartedAt, infinity) ->
    Context;
context_with_deadline(Context, StartedAt, Timeout) ->
    Context#{runtime_deadline => StartedAt + Timeout}.

-spec apply_timeout(
    {ok, term()} | {error, control_failure()},
    integer(),
    timeout(),
    term()
) -> {ok, term()} | {error, control_failure()}.
apply_timeout({ok, _Value}, StartedAt, Timeout, Origin)
        when is_integer(Timeout) ->
    FinishedAt = erlang:monotonic_time(millisecond),
    case FinishedAt - StartedAt >= Timeout of
        true ->
            {error, failure(handler_failure, Origin, #{
                reason => timeout
            })};
        false ->
            {ok, _Value}
    end;
apply_timeout(Outcome, _StartedAt, _Timeout, _Origin) ->
    Outcome.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(list()) -> {ok, registry_state()}.
init([]) ->
    {ok, #{entries => #{}}}.

handle_call({capture, Ref, Entry}, _From, State) ->
    Entries0 = maps:get(entries, State),
    Entry1 = install_lifetime_monitors(Entry),
    {reply, ok, State#{entries := Entries0#{Ref => Entry1}}};
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
                    {reply, {ok, Token, Invocation}, State#{entries := Entries1}};
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
        {ok, #{state := running, run_token := Token} = Entry} ->
            Entry1 = release_entry(
                Entry#{
                    state := consumed,
                    consumed_reason => Reason
                }
            ),
            {reply, ok, State#{entries := Entries0#{Ref := Entry1}}};
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
            {reply, ok, State#{entries := Entries0#{Ref := Entry1}}};
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
            {reply, ok, State#{entries := Entries0#{Ref := Entry1}}};
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
handle_call({lease_status, Ref}, _From, State) ->
    case maps:find(Ref, maps:get(entries, State)) of
        {ok, Entry} ->
            Lease = maps:get(lease, Entry),
            {reply, {ok, maps:get(status, Lease)}, State};
        error ->
            {reply, invalid_registered_handle(), State}
    end;
handle_call(reset, _From, State) ->
    release_all_entries(maps:values(maps:get(entries, State))),
    {reply, ok, #{entries => #{}}};
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
authorize_live_entry(#{kind := Kind}, Origin) when Kind =/= one_shot ->
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
authorize_live_entry(#{state := fresh} = Entry, _Origin) ->
    Token = make_ref(),
    {ok, Token, Entry#{state := running, run_token => Token}}.

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
        type_identity => maps:get(type_identity, Entry)
    }.

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
            validate_capture_fields(Spec)
    end;
validate_capture(_Continuation, _Spec) ->
    {error, failure(invalid_resumption, undefined, #{
        reason => invalid_compiler_capture
    })}.

-spec validate_capture_fields(map()) ->
    ok | {error, control_failure()}.
validate_capture_fields(Spec) ->
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
        {_, _, true, Depth, one_shot, true, true, true}
                when Depth =:= deep; Depth =:= shallow ->
            ok;
        {_, _, true, UnsupportedDepth, one_shot, true, true, true} ->
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
