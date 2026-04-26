-module(catena_one_shot).
-behaviour(catena_algebraic_laws).

%% One-shot continuations can only be resumed once. After resumption,
%% the continuation is consumed and cannot be used again. This provides
%% precise resource management and cleanup guarantees.

-export([
    %% One-shot resumption type
    new/0,
    new/1,
    %% One-shot capture
    capture/0,
    capture/1,
    wrap/1,
    %% One-shot resume
    resume/2,
    try_resume/2,
    %% State inspection
    is_consumed/1,
    is_available/1,
    get_state/1,
    %% Kind access
    kind/0,
    %% Algebraic laws
    associative/2,
    commutative/2,
    identity/2,
    idempotent/1,
    %% Cleanup
    cleanup/1,
    reset_table/0
]).

-export_type([
    one_shot/0,
    state/0,
    resume_result/0
]).

%%%---------------------------------------------------------------------
%%% Macros
%%%---------------------------------------------------------------------

-define(CONSUMED_TABLE, catena_one_shot_consumed).
-define(TABLE_OPTIONS, [set, public, named_table, {read_concurrency, true}]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

%% @doc One-shot continuation state.
-opaque state() :: #{
    id => reference(),
    data => term(),
    consumed => boolean(),
    capture_time => integer()
}.

%% @doc One-shot continuation type.
-opaque one_shot() :: #{
    kind => one_shot,
    state => state(),
    metadata => map()
}.

%% @doc Result of resuming a one-shot continuation.
-type resume_result() ::
    {ok, term()} |
    {error, already_consumed | invalid_state | no_data | term()}.

%%%---------------------------------------------------------------------
%%% Table Management
%%%---------------------------------------------------------------------

%% @doc Initialize the consumed tracking table.
-spec init_table() -> ok.
init_table() ->
    case ets:info(?CONSUMED_TABLE) of
        undefined ->
            ets:new(?CONSUMED_TABLE, ?TABLE_OPTIONS),
            ok;
        _ ->
            ok
    end.

%% @doc Reset the consumed tracking table (for testing).
-spec reset_table() -> ok.
reset_table() ->
    case ets:info(?CONSUMED_TABLE) of
        undefined ->
            ok;
        _ ->
            ets:delete_all_objects(?CONSUMED_TABLE),
            ok
    end.

%%%---------------------------------------------------------------------
%%% Constructors
%%%---------------------------------------------------------------------

%% @doc Create a new empty one-shot continuation.
-spec new() -> one_shot().
new() ->
    new(#{}).

%% @doc Create a new one-shot continuation with initial data.
-spec new(map() | term()) -> one_shot().
new(Data) when is_map(Data) ->
    init_table(),
    Id = make_ref(),
    build_one_shot(Id, Data, #{
        created_at => erlang:system_time(millisecond),
        created_by => self()
    });
new(Data) ->
    new(#{data => Data}).

%%%---------------------------------------------------------------------
%%% One-Shot Capture
%%%---------------------------------------------------------------------

%% @doc Capture current computation state as a one-shot continuation.
-spec capture() -> {ok, one_shot()} | {error, term()}.
capture() ->
    capture(#{process_info => get_process_info()}).

%% @doc Capture with custom metadata.
-spec capture(map()) -> {ok, one_shot()} | {error, term()}.
capture(Metadata) when is_map(Metadata) ->
    try
        init_table(),
        Id = make_ref(),
        Continuation = build_one_shot(Id, capture_state(), Metadata#{
            captured_at => erlang:system_time(millisecond),
            captured_by => self()
        }),
        {ok, Continuation}
    catch
        Type:Error:Stack ->
            {error, {Type, Error, Stack}}
    end.

%% @doc Wrap an existing resumption or resumption wrapper in one-shot semantics.
-spec wrap(term()) -> one_shot().
wrap(Resumption) ->
    init_table(),
    Id = make_ref(),
    build_one_shot(Id, Resumption, #{
        wrapped => true,
        created_at => erlang:system_time(millisecond),
        created_by => self()
    }).

%%%---------------------------------------------------------------------
%%% One-Shot Resume
%%%---------------------------------------------------------------------

%% @doc Resume a one-shot continuation with a value.
%% Returns an error if the continuation has already been consumed.
-spec resume(one_shot(), term()) -> resume_result().
resume(#{kind := one_shot, state := #{id := Id} = State}, Value) ->
    case is_consumed_id(Id) of
        true ->
            {error, already_consumed};
        false ->
            mark_consumed(Id),
            do_resume(State, Value)
    end;
resume(_, _) ->
    {error, invalid_state}.

%% @doc Try to resume a one-shot continuation, returning a new consumed continuation.
-spec try_resume(one_shot(), term()) -> {resume_result(), one_shot()}.
try_resume(#{kind := one_shot, state := #{id := Id} = State} = Cont, Value) ->
    case is_consumed_id(Id) of
        true ->
            {{error, already_consumed}, Cont};
        false ->
            mark_consumed(Id),
            {Result, NewState} = do_resume_with_state(State, Value),
            NewCont = Cont#{state => NewState},
            {Result, NewCont}
    end;
try_resume(Cont, _) ->
    {{error, invalid_state}, Cont}.

%%%---------------------------------------------------------------------
%%% State Inspection
%%%---------------------------------------------------------------------

%% @doc Check if a one-shot continuation has been consumed.
-spec is_consumed(one_shot()) -> boolean().
is_consumed(#{kind := one_shot, state := #{id := Id}}) ->
    is_consumed_id(Id);
is_consumed(_) ->
    false.

%% @doc Check if a one-shot continuation is available for resumption.
-spec is_available(one_shot()) -> boolean().
is_available(#{kind := one_shot} = Cont) ->
    not is_consumed(Cont);
is_available(_) ->
    false.

%% @doc Get the data stored in a one-shot continuation.
-spec get_state(one_shot()) -> {ok, term()} | {error, term()}.
get_state(#{kind := one_shot, state := #{data := Data}}) ->
    {ok, Data};
get_state(_) ->
    {error, invalid_state}.

%% @doc Get the continuation kind.
-spec kind() -> one_shot.
kind() ->
    one_shot.

%% @doc Clean up a continuation from the tracking table.
-spec cleanup(one_shot()) -> ok.
cleanup(#{kind := one_shot, state := #{id := Id}}) ->
    ets:delete(?CONSUMED_TABLE, Id),
    ok;
cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Internal helpers
%%%---------------------------------------------------------------------

%% @doc Capture current process state.
-spec capture_state() -> map().
capture_state() ->
    Dict = get(),
    #{
        process_info => get_process_info(),
        stack_trace => safe_stack_trace(),
        dictionary => Dict,
        dictionary_size => length(Dict)
    }.

%% @doc Get process information safely.
-spec get_process_info() -> map().
get_process_info() ->
    SafeKeys = [
        memory, message_queue_len, heap_size, total_heap_size,
        stack_size, reductions, current_function, initial_call
    ],
    Pid = self(),
    lists:foldl(
        fun(Key, Acc) ->
            case erlang:process_info(Pid, Key) of
                {Key, Value} -> Acc#{Key => Value};
                _ -> Acc
            end
        end,
        #{},
        SafeKeys
    ).

%% @doc Get stack trace safely.
-spec safe_stack_trace() -> list().
safe_stack_trace() ->
    case erlang:process_info(self(), current_stacktrace) of
        {current_stacktrace, StackTrace} when is_list(StackTrace) ->
            StackTrace;
        _ ->
            []
    end.

%% @doc Check if a continuation ID has been consumed.
-spec is_consumed_id(reference()) -> boolean().
is_consumed_id(Id) ->
    case ets:lookup(?CONSUMED_TABLE, Id) of
        [{Id, _}] -> true;
        _ -> false
    end.

%% @doc Mark a continuation ID as consumed.
-spec mark_consumed(reference()) -> true.
mark_consumed(Id) ->
    ets:insert(?CONSUMED_TABLE, {Id, erlang:system_time(millisecond)}).

%% @doc Perform the actual resume operation.
-spec do_resume(state(), term()) -> resume_result().
do_resume(#{data := Data}, Value) ->
    resume_captured(Data, Value).

%% @doc Perform resume with state update.
-spec do_resume_with_state(state(), term()) -> {resume_result(), state()}.
do_resume_with_state(State, Value) ->
    ConsumedState = State#{consumed => true},
    Result = case State of
        #{data := Data} -> resume_captured(Data, Value);
        _ -> {ok, #{value => Value}}
    end,
    {Result, ConsumedState}.

%% @doc Build a one-shot continuation value.
-spec build_one_shot(reference(), term(), map()) -> one_shot().
build_one_shot(Id, Data, Metadata) ->
    #{
        kind => one_shot,
        state => #{
            id => Id,
            data => Data,
            consumed => false,
            capture_time => erlang:system_time(millisecond)
        },
        metadata => Metadata
    }.

%% @doc Resume captured state, delegating to a wrapped resumption when present.
-spec resume_captured(term(), term()) -> resume_result().
resume_captured(Data, Value) ->
    case extract_resumption(Data) of
        {ok, Resumption} ->
            case catena_resumption:resume(Resumption, Value) of
                {error, _, _, _} = Error -> {error, Error};
                Result -> {ok, Result}
            end;
        error ->
            resume_plain_data(Data, Value)
    end.

%% @doc Extract an underlying resumption from nested wrappers.
-spec extract_resumption(term()) -> {ok, term()} | error.
extract_resumption(Data) ->
    case catena_resumption:is_resumption(Data) of
        true ->
            {ok, Data};
        false when is_map(Data) ->
            case maps:find(resumption, Data) of
                {ok, Inner} -> extract_resumption(Inner);
                error -> error
            end;
        false ->
            error
    end.

%% @doc Resume plain captured data without a first-class resumption.
-spec resume_plain_data(term(), term()) -> resume_result().
resume_plain_data(Data, Value) ->
    case Data of
        #{process_info := _} ->
            {ok, #{value => Value, restored => Data}};
        _ when Data =:= #{} ->
            {ok, #{value => Value}};
        _ ->
            {ok, #{value => Value, previous => Data}}
    end.

%%%---------------------------------------------------------------------
%%% Algebraic Laws Callbacks
%%%---------------------------------------------------------------------

%% @doc Check associativity for one-shot operations.
%% One-shot continuations are not associative due to consumption.
-spec associative(one_shot(), one_shot()) -> boolean().
associative(_, _) ->
    false.

%% @doc Check commutativity for one-shot operations.
%% One-shot continuations are not commutative.
-spec commutative(one_shot(), one_shot()) -> boolean().
commutative(_, _) ->
    false.

%% @doc Check if value is identity for one-shot.
%% No true identity for one-shot continuations.
-spec identity(one_shot(), one_shot()) -> boolean().
identity(_, _) ->
    false.

%% @doc Check idempotence for one-shot operations.
%% One-shot resume is not idempotent (consumes on each call).
-spec idempotent(one_shot()) -> boolean().
idempotent(_) ->
    false.
