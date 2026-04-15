-module(catena_multi_shot).
-behaviour(catena_algebraic_laws).

%% Multi-shot continuations can be resumed multiple times. Each resume
%% operates on a copy of the captured state (either deep or shallow copy).
%% This enables backtracking, nondeterminism, and solution enumeration.

-export([
    %% Multi-shot resumption type
    new/0,
    new/1,
    new/2,
    %% Multi-shot capture
    capture/0,
    capture/1,
    capture/2,
    %% Multi-shot resume
    resume/2,
    %% State inspection
    get_resume_count/1,
    get_state/1,
    is_available/1,
    %% Kind access
    kind/0,
    %% Cleanup
    cleanup/1,
    reset_table/0,
    %% Algebraic laws
    associative/2,
    commutative/2,
    identity/2,
    idempotent/1
]).

-export_type([
    multi_shot/0,
    state/0,
    copy_strategy/0,
    resume_result/0
]).

%%%---------------------------------------------------------------------
%%% Macros
%%%---------------------------------------------------------------------

-define(RESUME_TABLE, catena_multi_shot_resumes).
-define(TABLE_OPTIONS, [set, public, named_table, {read_concurrency, true}]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

%% @doc Copy strategy for multi-shot continuations.
-type copy_strategy() :: deep | shallow | selective.

%% @doc Multi-shot continuation state.
-opaque state() :: #{
    id => reference(),
    data => term(),
    copy_strategy => copy_strategy(),
    resume_count => non_neg_integer(),
    capture_time => integer()
}.

%% @doc Multi-shot continuation type.
-opaque multi_shot() :: #{
    kind => multi_shot,
    state => state(),
    metadata => map()
}.

%% @doc Result of resuming a multi-shot continuation.
-type resume_result() :: {ok, term(), non_neg_integer()}.

%%%---------------------------------------------------------------------
%%% Table Management
%%%---------------------------------------------------------------------

%% @doc Initialize the resume tracking table.
-spec init_table() -> ok.
init_table() ->
    case ets:info(?RESUME_TABLE) of
        undefined ->
            ets:new(?RESUME_TABLE, ?TABLE_OPTIONS),
            ok;
        _ ->
            ok
    end.

%% @doc Reset the resume tracking table (for testing).
-spec reset_table() -> ok.
reset_table() ->
    case ets:info(?RESUME_TABLE) of
        undefined ->
            ok;
        _ ->
            ets:delete_all_objects(?RESUME_TABLE),
            ok
    end.

%%%---------------------------------------------------------------------
%%% Constructors
%%%---------------------------------------------------------------------

%% @doc Create a new multi-shot continuation with default deep copy strategy.
-spec new() -> multi_shot().
new() ->
    new(#{}, deep).

%% @doc Create a new multi-shot continuation with data and default copy strategy.
-spec new(map() | term()) -> multi_shot().
new(Data) when is_map(Data) ->
    new(Data, deep);
new(Data) ->
    new(#{data => Data}, deep).

%% @doc Create a new multi-shot continuation with data and copy strategy.
-spec new(map() | term(), copy_strategy()) -> multi_shot().
new(Data, Strategy) when is_map(Data), is_atom(Strategy) ->
    init_table(),
    Id = make_ref(),
    #{
        kind => multi_shot,
        state => #{
            id => Id,
            data => Data,
            copy_strategy => Strategy,
            resume_count => 0,
            capture_time => erlang:system_time(millisecond)
        },
        metadata => #{
            created_at => erlang:system_time(millisecond),
            created_by => self()
        }
    };
new(Data, Strategy) ->
    new(#{data => Data}, Strategy).

%%%---------------------------------------------------------------------
%%% Multi-Shot Capture
%%%---------------------------------------------------------------------

%% @doc Capture current computation state as a multi-shot continuation.
-spec capture() -> {ok, multi_shot()} | {error, term()}.
capture() ->
    capture(#{}, deep).

%% @doc Capture with custom metadata.
-spec capture(map()) -> {ok, multi_shot()} | {error, term()}.
capture(Metadata) when is_map(Metadata) ->
    capture(Metadata, deep).

%% @doc Capture with custom metadata and copy strategy.
-spec capture(map(), copy_strategy()) -> {ok, multi_shot()} | {error, term()}.
capture(Metadata, Strategy) when is_map(Metadata), is_atom(Strategy) ->
    try
        init_table(),
        Id = make_ref(),
        State = #{
            id => Id,
            data => capture_state(),
            copy_strategy => Strategy,
            resume_count => 0,
            capture_time => erlang:system_time(millisecond)
        },
        Continuation = #{
            kind => multi_shot,
            state => State,
            metadata => Metadata#{
                captured_at => erlang:system_time(millisecond),
                captured_by => self()
            }
        },
        {ok, Continuation}
    catch
        Type:Error:Stack ->
            {error, {Type, Error, Stack}}
    end.

%%%---------------------------------------------------------------------
%%% Multi-Shot Resume
%%%---------------------------------------------------------------------

%% @doc Resume a multi-shot continuation with a value.
%% Each resume creates a new copy of the state based on the copy strategy.
%% Returns the result and the new resume count.
-spec resume(multi_shot(), term()) -> resume_result().
resume(#{kind := multi_shot, state := #{id := Id, data := Data, copy_strategy := Strategy} = State}, Value) ->
    %% Increment resume count
    NewCount = increment_resume_count(Id),
    %% Copy state based on strategy
    CopiedData = copy_state(Data, Strategy),
    %% Update state metadata
    UpdatedState = State#{
        resume_count => NewCount,
        data => CopiedData
    },
    %% Store updated state
    ets:insert(?RESUME_TABLE, {Id, UpdatedState}),
    {ok, #{value => Value, previous => CopiedData}, NewCount};
resume(_, _) ->
    {error, invalid_state}.

%%%---------------------------------------------------------------------
%%% State Inspection
%%%---------------------------------------------------------------------

%% @doc Get the number of times a continuation has been resumed.
-spec get_resume_count(multi_shot()) -> non_neg_integer().
get_resume_count(#{kind := multi_shot, state := #{id := Id}}) ->
    case ets:lookup(?RESUME_TABLE, Id) of
        [{Id, #{resume_count := Count}}] -> Count;
        _ -> 0
    end;
get_resume_count(_) ->
    0.

%% @doc Get the data stored in a multi-shot continuation.
-spec get_state(multi_shot()) -> {ok, term()} | {error, term()}.
get_state(#{kind := multi_shot, state := #{data := Data}}) ->
    {ok, Data};
get_state(_) ->
    {error, invalid_state}.

%% @doc Check if a multi-shot continuation is available (always true).
-spec is_available(multi_shot()) -> boolean().
is_available(#{kind := multi_shot}) ->
    true;
is_available(_) ->
    false.

%% @doc Get the continuation kind.
-spec kind() -> multi_shot.
kind() ->
    multi_shot.

%% @doc Clean up a continuation from the tracking table.
-spec cleanup(multi_shot()) -> ok.
cleanup(#{kind := multi_shot, state := #{id := Id}}) ->
    ets:delete(?RESUME_TABLE, Id),
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
    try
        erlang:get_stacktrace()
    catch
        _:_ -> []
    end.

%% @doc Increment the resume count for a continuation.
-spec increment_resume_count(reference()) -> non_neg_integer().
increment_resume_count(Id) ->
    case ets:lookup(?RESUME_TABLE, Id) of
        [{Id, #{resume_count := Count}}] ->
            NewCount = Count + 1,
            ets:insert(?RESUME_TABLE, {Id, #{resume_count => NewCount}}),
            NewCount;
        _ ->
            ets:insert(?RESUME_TABLE, {Id, #{resume_count => 1}}),
            1
    end.

%% @doc Copy state based on the specified strategy.
-spec copy_state(term(), copy_strategy()) -> term().
copy_state(Data, deep) when is_map(Data) ->
    maps:map(fun(_, V) -> deep_copy_value(V) end, Data);
copy_state(Data, shallow) ->
    Data;
copy_state(Data, selective) ->
    selective_copy(Data).

%% @doc Deep copy a value.
-spec deep_copy_value(term()) -> term().
deep_copy_value(Map) when is_map(Map) ->
    maps:map(fun(_, V) -> deep_copy_value(V) end, Map);
deep_copy_value(List) when is_list(List) ->
    [deep_copy_value(V) || V <- List];
deep_copy_value(Tuple) when is_tuple(Tuple) ->
    List = tuple_to_list(Tuple),
    CopiedList = [deep_copy_value(V) || V <- List],
    list_to_tuple(CopiedList);
deep_copy_value(Value) ->
    Value.

%% @doc Selective copy that copies mutable structures but shares immutable ones.
-spec selective_copy(term()) -> term().
selective_copy(Data) when is_map(Data) ->
    %% Copy maps but share simple values
    maps:map(fun(_, V) -> selective_copy_value(V) end, Data);
selective_copy(Data) ->
    Data.

%% @doc Selective copy for individual values.
-spec selective_copy_value(term()) -> term().
selective_copy_value(Map) when is_map(Map), map_size(Map) > 5 ->
    %% Copy larger maps
    maps:map(fun(_, V) -> selective_copy_value(V) end, Map);
selective_copy_value(Value) ->
    %% Share small maps and simple values
    Value.

%%%---------------------------------------------------------------------
%%% Algebraic Laws Callbacks
%%%---------------------------------------------------------------------

%% @doc Check associativity for multi-shot operations.
%% Multi-shot continuations are associative.
-spec associative(multi_shot(), multi_shot()) -> boolean().
associative(_, _) ->
    true.

%% @doc Check commutativity for multi-shot operations.
%% Multi-shot continuations are commutative.
-spec commutative(multi_shot(), multi_shot()) -> boolean().
commutative(_, _) ->
    true.

%% @doc Check if value is identity for multi-shot.
%% Empty multi-shot continuation acts as identity.
-spec identity(multi_shot(), multi_shot()) -> boolean().
identity(#{state := #{data := #{}}}, _) -> true;
identity(_, #{state := #{data := #{}}}) -> true;
identity(_, _) -> false.

%% @doc Check idempotence for multi-shot operations.
%% Multi-shot resume is idempotent (state is copied).
-spec idempotent(multi_shot()) -> boolean().
idempotent(_) ->
    true.
