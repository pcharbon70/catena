%%%-------------------------------------------------------------------
%%% @doc Executable reference semantics for first-class resumptions.
%%%
%%% This module is deliberately independent of the production effect runtime
%%% and Core Erlang backend.  It evaluates a small computation language using
%%% explicit requests, delimiters, handler frames, and process-affine one-shot
%%% resumption authority.  Compiler phases use its deterministic results and
%%% traces as an oracle; generated code must not call this module.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_resumption_oracle).

-export([
    new/0,
    new/1,
    run/1,
    run/2,
    trace/1,
    pure/1,
    fail/2,
    bind/2,
    perform/3,
    handle/2,
    handle/4,
    control_case/3,
    value_case/3,
    resume/2,
    resume_as/3,
    expire_owner/2,
    expire_delimiter/2
]).

-export_type([
    computation/0,
    effect_case/0,
    oracle_state/0,
    resumption/0,
    trace_event/0
]).

-define(RESUMPTION_VERSION, 1).

-type owner() :: term().
-type delimiter_id() :: pos_integer().
-type resumption_id() :: pos_integer().
-type depth() :: deep | shallow.
-type kind() :: one_shot | multi_shot.
-type resumption() ::
    {catena_oracle_resumption, ?RESUMPTION_VERSION, resumption_id()}.
-type computation() ::
    {pure, term()}
    | {fail, atom(), term()}
    | {bind, computation(), fun((term()) -> computation())}
    | {perform, atom(), atom(), [term()]}
    | {handle, computation(), [effect_case()], depth(), kind()}
    | {resume, term(), term(), owner()}
    | tuple().
-type effect_case() :: #{
    effect := atom(),
    operation := atom(),
    mode := control | value,
    handler := function()
}.
-type trace_event() :: tuple().
-type oracle_state() :: map().
-type eval_result() ::
    {done, term(), oracle_state()}
    | {request, atom(), atom(), [term()], function(), oracle_state()}
    | {failed, atom(), term(), oracle_state()}.

%%====================================================================
%% Public state and computation API
%%====================================================================

%% @doc Create deterministic oracle state with the default logical owner.
-spec new() -> oracle_state().
new() ->
    new(oracle_owner).

%% @doc Create deterministic oracle state for a logical owner.
-spec new(owner()) -> oracle_state().
new(Owner) ->
    #{
        owner => Owner,
        next_delimiter => 1,
        next_resumption => 1,
        delimiters => #{},
        resumptions => #{},
        expired_owners => #{},
        trace_rev => []
    }.

%% @doc Evaluate a computation with fresh default state.
-spec run(computation()) ->
    {ok, term(), oracle_state()}
    | {error, atom(), term(), oracle_state()}.
run(Computation) ->
    run(Computation, new()).

%% @doc Evaluate a computation in existing state.
%%
%% Existing state permits a first-class resumption returned by one run to be
%% invoked in a later run without hiding its retained semantic resources.
-spec run(computation(), oracle_state()) ->
    {ok, term(), oracle_state()}
    | {error, atom(), term(), oracle_state()}.
run(Computation, State) ->
    case eval(Computation, State) of
        {done, Value, State1} ->
            {ok, Value, State1};
        {failed, Category, Details, State1} ->
            {error, Category, Details, State1};
        {request, Effect, Operation, Args, _Continuation, State1} ->
            Details = #{
                effect => Effect,
                operation => Operation,
                arguments => Args
            },
            State2 = emit({failure, unhandled_effect, Details}, State1),
            {error, unhandled_effect, Details, State2}
    end.

%% @doc Return trace events in evaluation order.
-spec trace(oracle_state()) -> [trace_event()].
trace(State) ->
    lists:reverse(maps:get(trace_rev, State, [])).

-spec pure(term()) -> computation().
pure(Value) ->
    {pure, Value}.

-spec fail(atom(), term()) -> computation().
fail(Category, Details) ->
    {fail, Category, Details}.

-spec bind(computation(), fun((term()) -> computation())) -> computation().
bind(Computation, Continuation) when is_function(Continuation, 1) ->
    {bind, Computation, Continuation}.

-spec perform(atom(), atom(), [term()]) -> computation().
perform(Effect, Operation, Args)
        when is_atom(Effect), is_atom(Operation), is_list(Args) ->
    {perform, Effect, Operation, Args}.

-spec handle(computation(), [effect_case()]) -> computation().
handle(Computation, Cases) ->
    handle(Computation, Cases, deep, one_shot).

-spec handle(computation(), [effect_case()], depth(), kind()) -> computation().
handle(Computation, Cases, Depth, Kind) when is_list(Cases) ->
    {handle, Computation, Cases, Depth, Kind}.

-spec control_case(atom(), atom(), fun(([term()], resumption()) -> computation())) ->
    effect_case().
control_case(Effect, Operation, Handler)
        when is_atom(Effect), is_atom(Operation), is_function(Handler, 2) ->
    #{
        effect => Effect,
        operation => Operation,
        mode => control,
        handler => Handler
    }.

-spec value_case(atom(), atom(), fun(([term()]) -> computation())) ->
    effect_case().
value_case(Effect, Operation, Handler)
        when is_atom(Effect), is_atom(Operation), is_function(Handler, 1) ->
    #{
        effect => Effect,
        operation => Operation,
        mode => value,
        handler => Handler
    }.

-spec resume(term(), term()) -> computation().
resume(Resumption, Value) ->
    {resume, Resumption, Value, current_owner}.

%% @doc Attempt invocation as a particular logical owner.
%%
%% This is an oracle-only hook for deterministic process-affinity evidence.
-spec resume_as(term(), term(), owner()) -> computation().
resume_as(Resumption, Value, Owner) ->
    {resume, Resumption, Value, Owner}.

%% @doc Mark an owner dead and release its retained operational state.
-spec expire_owner(owner(), oracle_state()) -> oracle_state().
expire_owner(Owner, State) ->
    Expired0 = maps:get(expired_owners, State),
    State1 = State#{expired_owners := Expired0#{Owner => true}},
    Resumptions0 = maps:get(resumptions, State1),
    Resumptions1 = maps:map(
        fun(_Id, Entry) ->
            case maps:get(owner, Entry) =:= Owner of
                true -> maps:without([continuation, cases], Entry#{expired => owner});
                false -> Entry
            end
        end,
        Resumptions0
    ),
    State1#{resumptions := Resumptions1}.

%% @doc Expire the delimiter referenced by a registered resumption.
-spec expire_delimiter(term(), oracle_state()) -> oracle_state().
expire_delimiter(
    {catena_oracle_resumption, ?RESUMPTION_VERSION, Id},
    State
) ->
    case maps:find(Id, maps:get(resumptions, State)) of
        {ok, Entry} ->
            Delimiter = maps:get(delimiter, Entry),
            Delimiters0 = maps:get(delimiters, State),
            case maps:find(Delimiter, Delimiters0) of
                {ok, DelimiterEntry} ->
                    State#{
                        delimiters := Delimiters0#{
                            Delimiter => DelimiterEntry#{status => expired}
                        }
                    };
                error ->
                    State
            end;
        error ->
            State
    end;
expire_delimiter(_Other, State) ->
    State.

%%====================================================================
%% Evaluation
%%====================================================================

-spec eval(computation(), oracle_state()) -> eval_result().
eval({pure, Value}, State) ->
    {done, Value, State};
eval({fail, Category, Details}, State) ->
    failure(Category, Details, State);
eval({bind, Computation, Continuation}, State) ->
    continue_bind(eval(Computation, State), Continuation);
eval({perform, Effect, Operation, Args}, State) ->
    State1 = emit({perform, Effect, Operation, Args}, State),
    Identity = fun(Value) -> pure(Value) end,
    {request, Effect, Operation, Args, Identity, State1};
eval({handle, Computation, Cases, deep, one_shot}, State) ->
    {Delimiter, State1} = allocate_delimiter(State),
    State2 = emit({delimiter_enter, Delimiter}, State1),
    continue_under(
        eval(Computation, State2),
        Cases,
        Delimiter,
        deep,
        one_shot,
        original
    );
eval({handle, _Computation, _Cases, Depth, Kind}, State) ->
    failure(
        unsupported_semantic_mode,
        #{depth => Depth, kind => Kind},
        State
    );
eval({resume, Resumption, Value, current_owner}, State) ->
    invoke_resume(Resumption, Value, maps:get(owner, State), State);
eval({resume, Resumption, Value, Invoker}, State) ->
    invoke_resume(Resumption, Value, Invoker, State);
eval(
    {oracle_under, Computation, Cases, Delimiter, Depth, Kind, Reason},
    State
) ->
    continue_under(
        eval(Computation, State),
        Cases,
        Delimiter,
        Depth,
        Kind,
        Reason
    );
eval({oracle_handler_case, Computation, Resumption, Delimiter}, State) ->
    finish_handler_result(
        eval(Computation, State),
        Resumption,
        Delimiter
    );
eval({oracle_resume_pending, Computation, Resumption}, State) ->
    finish_resume_result(eval(Computation, State), Resumption);
eval({oracle_auto_resume, Body, Resumption}, State) ->
    continue_auto_resume(eval(Body, State), Resumption);
eval(Other, State) ->
    failure(invalid_oracle_computation, #{term => stable_term(Other)}, State).

-spec continue_bind(eval_result(), function()) -> eval_result().
continue_bind({done, Value, State}, Continuation) ->
    case apply_callback(Continuation, [Value], continuation, State) of
        {ok, Next} -> eval(Next, State);
        {error, Result} -> Result
    end;
continue_bind(
    {request, Effect, Operation, Args, RequestContinuation, State},
    Continuation
) ->
    Wrapped = fun(Value) ->
        bind(RequestContinuation(Value), Continuation)
    end,
    {request, Effect, Operation, Args, Wrapped, State};
continue_bind({failed, _Category, _Details, _State} = Failed, _Continuation) ->
    Failed.

-spec continue_under(
    eval_result(),
    [effect_case()],
    delimiter_id(),
    depth(),
    kind(),
    original | resumed
) -> eval_result().
continue_under(
    {done, Value, State},
    _Cases,
    Delimiter,
    _Depth,
    _Kind,
    Reason
) ->
    State1 = emit({delimiter_return, Delimiter, Value, Reason}, State),
    State2 = maybe_close_delimiter(Delimiter, State1),
    {done, Value, State2};
continue_under(
    {failed, Category, Details, State},
    _Cases,
    Delimiter,
    _Depth,
    _Kind,
    _Reason
) ->
    State1 = maybe_close_delimiter(Delimiter, State),
    {failed, Category, Details, State1};
continue_under(
    {request, Effect, Operation, Args, Continuation, State},
    Cases,
    Delimiter,
    Depth,
    Kind,
    _Reason
) ->
    case find_case(Effect, Operation, Cases) of
        {ok, EffectCase} ->
            handle_request(
                Effect,
                Operation,
                Args,
                Continuation,
                EffectCase,
                Cases,
                Delimiter,
                Depth,
                Kind,
                State
            );
        error ->
            State1 = emit(
                {propagate, Delimiter, Effect, Operation},
                State
            ),
            Wrapped = fun(Value) ->
                {oracle_under,
                    Continuation(Value),
                    Cases,
                    Delimiter,
                    Depth,
                    Kind,
                    resumed}
            end,
            {request, Effect, Operation, Args, Wrapped, State1}
    end.

-spec handle_request(
    atom(),
    atom(),
    [term()],
    function(),
    effect_case(),
    [effect_case()],
    delimiter_id(),
    depth(),
    kind(),
    oracle_state()
) -> eval_result().
handle_request(
    Effect,
    Operation,
    Args,
    Continuation,
    EffectCase,
    Cases,
    Delimiter,
    Depth,
    Kind,
    State
) ->
    {Resumption, State1} = allocate_resumption(
        Continuation,
        Cases,
        Delimiter,
        Depth,
        Kind,
        State
    ),
    Mode = maps:get(mode, EffectCase),
    State2 = emit(
        {handler_select, Delimiter, Effect, Operation, Mode},
        State1
    ),
    Handler = maps:get(handler, EffectCase),
    case handler_body(Mode, Handler, Args, Resumption, State2) of
        {ok, Body} ->
            finish_handler_result(
                eval(Body, State2),
                Resumption,
                Delimiter
            );
        {error, Result} ->
            finish_handler_result(Result, Resumption, Delimiter)
    end.

-spec handler_body(
    control | value,
    function(),
    [term()],
    resumption(),
    oracle_state()
) -> {ok, computation()} | {error, eval_result()}.
handler_body(control, Handler, Args, Resumption, State) ->
    apply_callback(Handler, [Args, Resumption], handler, State);
handler_body(value, Handler, Args, Resumption, State) ->
    case apply_callback(Handler, [Args], handler, State) of
        {ok, Body} ->
            {ok, {oracle_auto_resume, Body, Resumption}};
        {error, Result} ->
            {error, Result}
    end.

-spec continue_auto_resume(eval_result(), resumption()) -> eval_result().
continue_auto_resume({done, Value, State}, Resumption) ->
    State1 = emit({auto_resume, resumption_id(Resumption), Value}, State),
    eval(resume(Resumption, Value), State1);
continue_auto_resume(
    {request, Effect, Operation, Args, Continuation, State},
    Resumption
) ->
    Wrapped = fun(Value) ->
        {oracle_auto_resume, Continuation(Value), Resumption}
    end,
    {request, Effect, Operation, Args, Wrapped, State};
continue_auto_resume(
    {failed, _Category, _Details, _State} = Failed,
    _Resumption
) ->
    Failed.

-spec finish_handler_result(
    eval_result(),
    resumption(),
    delimiter_id()
) -> eval_result().
finish_handler_result(
    {request, Effect, Operation, Args, Continuation, State},
    Resumption,
    Delimiter
) ->
    Wrapped = fun(Value) ->
        {oracle_handler_case, Continuation(Value), Resumption, Delimiter}
    end,
    {request, Effect, Operation, Args, Wrapped, State};
finish_handler_result(
    {failed, Category, Details, State},
    Resumption,
    Delimiter
) ->
    State1 = discard_if_fresh(Resumption, Delimiter, handler_failed, State),
    {failed, Category, Details, State1};
finish_handler_result(
    {done, Value, State},
    Resumption,
    Delimiter
) ->
    Id = resumption_id(Resumption),
    case resumption_state(Id, State) of
        fresh ->
            case contains_resumption(Resumption, Value) of
                true ->
                    State1 = retain_delimiter(Delimiter, State),
                    State2 = emit({retain, Id, Delimiter}, State1),
                    {done, Value, State2};
                false ->
                    State1 = emit({abort, Id, Delimiter, Value}, State),
                    State2 = consume(Id, aborted, State1),
                    State3 = maybe_close_delimiter(Delimiter, State2),
                    {done, Value, State3}
            end;
        _OtherState ->
            State1 = maybe_close_delimiter(Delimiter, State),
            {done, Value, State1}
    end.

%%====================================================================
%% Resume validation and execution
%%====================================================================

-spec invoke_resume(term(), term(), owner(), oracle_state()) -> eval_result().
invoke_resume(Resumption, Value, Invoker, State) ->
    case validate_resume(Resumption, Invoker, State) of
        {ok, Id, Entry} ->
            State1 = set_resumption_state(Id, running, State),
            State2 = emit({resume_begin, Id, Value}, State1),
            Continuation = maps:get(continuation, Entry),
            Cases = maps:get(cases, Entry),
            Delimiter = maps:get(delimiter, Entry),
            Depth = maps:get(depth, Entry),
            Kind = maps:get(kind, Entry),
            case apply_callback(
                Continuation,
                [Value],
                continuation,
                State2
            ) of
                {ok, Computation} ->
                    finish_resume_result(
                        continue_under(
                            eval(Computation, State2),
                            Cases,
                            Delimiter,
                            Depth,
                            Kind,
                            resumed
                        ),
                        Resumption
                    );
                {error, Result} ->
                    finish_resume_result(Result, Resumption)
            end;
        {error, Category, Details} ->
            failure(Category, Details, State)
    end.

-spec finish_resume_result(eval_result(), resumption()) -> eval_result().
finish_resume_result(
    {request, Effect, Operation, Args, Continuation, State},
    Resumption
) ->
    Wrapped = fun(Value) ->
        {oracle_resume_pending, Continuation(Value), Resumption}
    end,
    {request, Effect, Operation, Args, Wrapped, State};
finish_resume_result(
    {failed, Category, Details, State},
    Resumption
) ->
    Id = resumption_id(Resumption),
    State1 = consume(Id, failed, State),
    {failed, Category, Details, State1};
finish_resume_result({done, Value, State}, Resumption) ->
    Id = resumption_id(Resumption),
    State1 = consume(Id, completed, State),
    State2 = emit({resume_return, Id, Value}, State1),
    {done, Value, State2}.

-spec validate_resume(term(), owner(), oracle_state()) ->
    {ok, resumption_id(), map()} | {error, atom(), term()}.
validate_resume(
    {catena_oracle_resumption, Version, _Id},
    _Invoker,
    _State
) when Version =/= ?RESUMPTION_VERSION ->
    {error, invalid_resumption_version, #{version => Version}};
validate_resume(
    {catena_oracle_resumption, ?RESUMPTION_VERSION, Id},
    Invoker,
    State
) when is_integer(Id), Id > 0 ->
    case maps:find(Id, maps:get(resumptions, State)) of
        error ->
            {error, invalid_resumption, #{id => Id}};
        {ok, Entry} ->
            validate_registered_resume(Id, Entry, Invoker, State)
    end;
validate_resume(Other, _Invoker, _State) ->
    {error, invalid_resumption, #{term => stable_term(Other)}}.

-spec validate_registered_resume(
    resumption_id(),
    map(),
    owner(),
    oracle_state()
) -> {ok, resumption_id(), map()} | {error, atom(), term()}.
validate_registered_resume(Id, Entry, Invoker, State) ->
    Owner = maps:get(owner, Entry),
    ExpiredOwners = maps:get(expired_owners, State),
    case maps:is_key(Owner, ExpiredOwners) orelse
            maps:get(expired, Entry, false) =:= owner of
        true ->
            {error, expired_resumption_owner, #{id => Id}};
        false when Invoker =/= Owner ->
            {error, wrong_resumption_owner, #{
                id => Id,
                expected => Owner,
                actual => Invoker
            }};
        false ->
            validate_delimiter_and_mode(Id, Entry, State)
    end.

-spec validate_delimiter_and_mode(
    resumption_id(),
    map(),
    oracle_state()
) -> {ok, resumption_id(), map()} | {error, atom(), term()}.
validate_delimiter_and_mode(Id, Entry, State) ->
    Delimiter = maps:get(delimiter, Entry),
    Delimiters = maps:get(delimiters, State),
    case maps:find(Delimiter, Delimiters) of
        error ->
            {error, stale_resumption_delimiter, #{
                id => Id,
                delimiter => Delimiter
            }};
        {ok, #{status := Status}} when Status =:= closed; Status =:= expired ->
            {error, stale_resumption_delimiter, #{
                id => Id,
                delimiter => Delimiter,
                status => Status
            }};
        {ok, _DelimiterEntry} ->
            validate_mode_and_state(Id, Entry)
    end.

-spec validate_mode_and_state(resumption_id(), map()) ->
    {ok, resumption_id(), map()} | {error, atom(), term()}.
validate_mode_and_state(Id, #{depth := deep, kind := one_shot} = Entry) ->
    case maps:get(state, Entry) of
        fresh ->
            {ok, Id, Entry};
        running ->
            {error, resumption_reentrant, #{id => Id}};
        consumed ->
            {error, resumption_already_consumed, #{id => Id}}
    end;
validate_mode_and_state(Id, Entry) ->
    {error, unsupported_semantic_mode, #{
        id => Id,
        depth => maps:get(depth, Entry),
        kind => maps:get(kind, Entry)
    }}.

%%====================================================================
%% State helpers
%%====================================================================

-spec allocate_delimiter(oracle_state()) -> {delimiter_id(), oracle_state()}.
allocate_delimiter(State) ->
    Id = maps:get(next_delimiter, State),
    Delimiters0 = maps:get(delimiters, State),
    Entry = #{status => active},
    {Id, State#{
        next_delimiter := Id + 1,
        delimiters := Delimiters0#{Id => Entry}
    }}.

-spec allocate_resumption(
    function(),
    [effect_case()],
    delimiter_id(),
    depth(),
    kind(),
    oracle_state()
) -> {resumption(), oracle_state()}.
allocate_resumption(
    Continuation,
    Cases,
    Delimiter,
    Depth,
    Kind,
    State
) ->
    Id = maps:get(next_resumption, State),
    Owner = maps:get(owner, State),
    Entry = #{
        owner => Owner,
        kind => Kind,
        depth => Depth,
        state => fresh,
        continuation => Continuation,
        cases => Cases,
        delimiter => Delimiter
    },
    Resumptions0 = maps:get(resumptions, State),
    State1 = State#{
        next_resumption := Id + 1,
        resumptions := Resumptions0#{Id => Entry}
    },
    State2 = emit(
        {capture, Id, Delimiter, Owner, Depth, Kind},
        State1
    ),
    {
        {catena_oracle_resumption, ?RESUMPTION_VERSION, Id},
        State2
    }.

-spec set_resumption_state(
    resumption_id(),
    fresh | running | consumed,
    oracle_state()
) -> oracle_state().
set_resumption_state(Id, NewState, State) ->
    Resumptions0 = maps:get(resumptions, State),
    Entry = maps:get(Id, Resumptions0),
    State#{resumptions := Resumptions0#{Id => Entry#{state => NewState}}}.

-spec resumption_state(resumption_id(), oracle_state()) ->
    fresh | running | consumed | missing.
resumption_state(Id, State) ->
    case maps:find(Id, maps:get(resumptions, State)) of
        {ok, Entry} -> maps:get(state, Entry);
        error -> missing
    end.

-spec consume(resumption_id(), atom(), oracle_state()) -> oracle_state().
consume(Id, Reason, State) ->
    State1 = set_resumption_state(Id, consumed, State),
    emit({consume, Id, Reason}, State1).

-spec discard_if_fresh(
    resumption(),
    delimiter_id(),
    atom(),
    oracle_state()
) -> oracle_state().
discard_if_fresh(Resumption, Delimiter, Reason, State) ->
    Id = resumption_id(Resumption),
    case resumption_state(Id, State) of
        fresh ->
            State1 = emit({abort, Id, Delimiter, Reason}, State),
            State2 = consume(Id, Reason, State1),
            maybe_close_delimiter(Delimiter, State2);
        _ ->
            maybe_close_delimiter(Delimiter, State)
    end.

-spec retain_delimiter(delimiter_id(), oracle_state()) -> oracle_state().
retain_delimiter(Delimiter, State) ->
    set_delimiter_status(Delimiter, retained, State).

-spec maybe_close_delimiter(delimiter_id(), oracle_state()) -> oracle_state().
maybe_close_delimiter(Delimiter, State) ->
    case has_live_resumption(Delimiter, State) of
        true ->
            State;
        false ->
            set_delimiter_status(Delimiter, closed, State)
    end.

-spec has_live_resumption(delimiter_id(), oracle_state()) -> boolean().
has_live_resumption(Delimiter, State) ->
    lists:any(
        fun(Entry) ->
            maps:get(delimiter, Entry) =:= Delimiter andalso
                (maps:get(state, Entry) =:= fresh orelse
                    maps:get(state, Entry) =:= running)
        end,
        maps:values(maps:get(resumptions, State))
    ).

-spec set_delimiter_status(delimiter_id(), atom(), oracle_state()) ->
    oracle_state().
set_delimiter_status(Delimiter, Status, State) ->
    Delimiters0 = maps:get(delimiters, State),
    case maps:find(Delimiter, Delimiters0) of
        {ok, Entry} ->
            State#{
                delimiters := Delimiters0#{
                    Delimiter => Entry#{status => Status}
                }
            };
        error ->
            State
    end.

-spec find_case(atom(), atom(), [effect_case()]) ->
    {ok, effect_case()} | error.
find_case(_Effect, _Operation, []) ->
    error;
find_case(Effect, Operation, [EffectCase | Rest]) ->
    case {
        maps:get(effect, EffectCase),
        maps:get(operation, EffectCase)
    } of
        {Effect, Operation} -> {ok, EffectCase};
        _ -> find_case(Effect, Operation, Rest)
    end.

-spec apply_callback(function(), [term()], atom(), oracle_state()) ->
    {ok, term()} | {error, eval_result()}.
apply_callback(Callback, Args, Phase, State) ->
    try
        {ok, erlang:apply(Callback, Args)}
    catch
        Class:Reason ->
            Details = #{
                phase => Phase,
                class => Class,
                reason => stable_term(Reason)
            },
            {error, failure(oracle_callback_failure, Details, State)}
    end.

-spec failure(atom(), term(), oracle_state()) -> eval_result().
failure(Category, Details, State) ->
    State1 = emit({failure, Category, Details}, State),
    {failed, Category, Details, State1}.

-spec emit(trace_event(), oracle_state()) -> oracle_state().
emit(Event, State) ->
    Trace = maps:get(trace_rev, State),
    State#{trace_rev := [Event | Trace]}.

-spec resumption_id(resumption()) -> resumption_id().
resumption_id(
    {catena_oracle_resumption, ?RESUMPTION_VERSION, Id}
) ->
    Id.

-spec contains_resumption(resumption(), term()) -> boolean().
contains_resumption(Resumption, Resumption) ->
    true;
contains_resumption(Resumption, List) when is_list(List) ->
    lists:any(fun(Item) -> contains_resumption(Resumption, Item) end, List);
contains_resumption(Resumption, Tuple) when is_tuple(Tuple) ->
    contains_resumption(Resumption, tuple_to_list(Tuple));
contains_resumption(Resumption, Map) when is_map(Map) ->
    contains_resumption(Resumption, maps:to_list(Map));
contains_resumption(_Resumption, _Other) ->
    false.

-spec stable_term(term()) -> term().
stable_term(Term) when is_function(Term) ->
    function;
stable_term(Term) when is_pid(Term) ->
    pid;
stable_term(Term) when is_reference(Term) ->
    reference;
stable_term(Term) when is_port(Term) ->
    port;
stable_term(Term) ->
    Term.
