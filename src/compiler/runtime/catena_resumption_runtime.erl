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
    is_resumption/1,
    status/1,
    version/0,
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
-define(VERSION, 1).

-opaque handle() ::
    {catena_resumption, ?VERSION, reference()}.

-type capture_spec() :: #{
    context := map(),
    delimiter := reference(),
    depth := deep,
    kind := one_shot,
    origin := term(),
    metadata => map(),
    type_identity => term()
}.

-type control_failure() :: #{
    category := atom(),
    origin := term(),
    details := map()
}.

-type entry() :: #{
    owner := pid(),
    kind := one_shot,
    state := fresh | running | consumed,
    continuation => fun((term(), map()) -> term()),
    context => map(),
    delimiter := reference(),
    delimiter_status := live | expired,
    depth := deep,
    origin := term(),
    metadata := map(),
    type_identity := term(),
    run_token => reference(),
    consumed_reason => normal | exceptional
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
                kind => one_shot,
                state => fresh,
                continuation => Continuation,
                context => maps:get(context, Spec),
                delimiter => maps:get(delimiter, Spec),
                delimiter_status => live,
                depth => deep,
                origin => maps:get(origin, Spec),
                metadata => maps:get(metadata, Spec, #{}),
                type_identity => maps:get(type_identity, Spec, dynamic)
            },
            ok = gen_server:call(?SERVER, {capture, Ref, Entry}),
            {ok, {catena_resumption, ?VERSION, Ref}};
        {error, _} = Error ->
            Error
    end.

%% @doc Invoke a deep one-shot resumption on its capturing process.
-spec resume(term(), term()) ->
    {ok, term()} | {error, control_failure()}.
resume(Handle, Value) ->
    case decode_handle(Handle) of
        {ok, Ref} ->
            ok = ensure_started(),
            case gen_server:call(?SERVER, {authorize, Ref, self()}) of
                {ok, Token, Invocation} ->
                    invoke(Ref, Token, Value, Invocation);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

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

%% @doc Clear runtime authority between isolated component tests.
-spec reset_for_test() -> ok.
reset_for_test() ->
    ok = ensure_started(),
    gen_server:call(?SERVER, reset).

%%====================================================================
%% Invocation
%%====================================================================

-spec invoke(reference(), reference(), term(), map()) ->
    {ok, term()} | {error, control_failure()}.
invoke(Ref, Token, Value, #{
    continuation := Continuation,
    context := Context,
    origin := Origin
}) ->
    Outcome =
        try
            {ok, Continuation(Value, Context)}
        catch
            Class:Reason:_Stack ->
                {error, failure(handler_failure, Origin, #{
                    class => Class,
                    reason => sanitize_reason(Reason)
                })}
        end,
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

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(list()) -> {ok, registry_state()}.
init([]) ->
    {ok, #{entries => #{}}}.

handle_call({capture, Ref, Entry}, _From, State) ->
    Entries0 = maps:get(entries, State),
    {reply, ok, State#{entries := Entries0#{Ref => Entry}}};
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
                {error, _} = Error ->
                    {reply, Error, State}
            end
    end;
handle_call({complete, Ref, Token, Reason}, _From, State) ->
    Entries0 = maps:get(entries, State),
    case maps:find(Ref, Entries0) of
        {ok, #{state := running, run_token := Token} = Entry} ->
            Entry1 = maps:without(
                [continuation, context, run_token],
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
handle_call({status, Ref}, _From, State) ->
    case maps:find(Ref, maps:get(entries, State)) of
        {ok, Entry} ->
            {reply, {ok, maps:get(state, Entry)}, State};
        error ->
            {reply, invalid_registered_handle(), State}
    end;
handle_call(reset, _From, _State) ->
    {reply, ok, #{entries => #{}}};
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Validation and state transitions
%%====================================================================

-spec authorize_entry(entry(), pid()) ->
    {ok, reference(), entry()} | {error, control_failure()}.
authorize_entry(Entry, Caller) ->
    Origin = maps:get(origin, Entry),
    Owner = maps:get(owner, Entry),
    case is_process_alive(Owner) of
        false ->
            {error, failure(expired_resumption_owner, Origin, #{})};
        true when Caller =/= Owner ->
            {error, failure(wrong_resumption_owner, Origin, #{})};
        true ->
            authorize_live_entry(Entry, Origin)
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
authorize_live_entry(#{depth := Depth}, Origin) when Depth =/= deep ->
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
        context => maps:get(context, Entry),
        delimiter => maps:get(delimiter, Entry),
        depth => maps:get(depth, Entry),
        kind => maps:get(kind, Entry),
        origin => maps:get(origin, Entry),
        metadata => maps:get(metadata, Entry),
        type_identity => maps:get(type_identity, Entry)
    }.

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
    case {
        is_map(Context),
        is_reference(Delimiter),
        Depth,
        Kind,
        Origin =/= undefined,
        is_map(Metadata)
    } of
        {false, _, _, _, _, _} ->
            invalid_capture_field(context);
        {_, false, _, _, _, _} ->
            invalid_capture_field(delimiter);
        {_, _, deep, one_shot, true, true} ->
            ok;
        {_, _, UnsupportedDepth, one_shot, true, true} ->
            {error, failure(unsupported_semantic_mode, Origin, #{
                depth => UnsupportedDepth
            })};
        {_, _, deep, UnsupportedKind, true, true} ->
            {error, failure(unsupported_semantic_mode, Origin, #{
                kind => UnsupportedKind
            })};
        {_, _, _, _, false, _} ->
            invalid_capture_field(origin);
        {_, _, _, _, _, false} ->
            invalid_capture_field(metadata)
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
