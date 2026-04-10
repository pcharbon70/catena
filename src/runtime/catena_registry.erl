%%%-------------------------------------------------------------------
%%% @doc Catena Process Registry (Phase 5.3)
%%%
%%% This module implements a process registry for registering and
%%% looking up processes by name. Unlike the built-in `register/2`,
%%% this registry supports:
%%% - Namespaced registries (multiple independent registries)
%%% - Automatic cleanup of dead processes
%%% - Pattern-based lookup
%%% - Metadata storage with process entries
%%%
%%% == Usage ==
%%%
%%% ```
%%% %% Start a registry
%%% {ok, RegPid} = catena_registry:start_link(my_registry).
%%%
%%% %% Register a process
%%% ok = catena_registry:register(my_registry, my_counter, Pid, #{type => counter}).
%%%
%%% %% Lookup a process
%%% {ok, FoundPid} = catena_registry:find(my_registry, my_counter).
%%%
%%% %% Pattern-based lookup
%%% [{counter, Pid1, _}, [{accumulator, Pid2, _}] =
%%%     catena_registry:match(my_registry, '_', #{type => counter}).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_registry).

%% API
-export([
    start_link/1,
    start_link/2,
    stop/1,
    register/4,
    register/3,
    unregister/2,
    find/2,
    whereis/2,
    match/3,
    match/2,
    list/1,
    list/2,
    update_metadata/3,
    get_metadata/2
]).

%%====================================================================
%% Types
%%====================================================================

-type registry_name() :: atom() | pid().
-type registry() :: registry_name() | {via, module(), term()}.
-type key() :: term().
-type metadata() :: map().

-export_type([registry_name/0, registry/0, key/0, metadata/0]).

-record(entry, {
    key :: key(),
    pid :: pid(),
    monitor :: reference(),
    metadata :: metadata()
}).

-record(state, {
    name :: term(),
    entries :: #{key => #entry{}},
    by_pid :: #{pid => key()}
}).

%%====================================================================
%% API - Start Functions
%%====================================================================

%% @doc Start a registry with a name.
-spec start_link(registry_name()) -> {ok, pid()} | {error, term()}.
start_link({local, Name}) when is_atom(Name) ->
    start_link(Name, []);
start_link(Name) ->
    start_link(Name, []).

%% @doc Start a registry with a name and options.
-spec start_link(registry_name(), list()) -> {ok, pid()} | {error, term()}.
start_link({local, Name}, _Options) when is_atom(Name) ->
    Parent = self(),
    Ref = make_ref(),
    InitFun = fun() ->
        try
            State = #state{name = Name, entries = #{}, by_pid = #{}},
            Parent ! {Ref, {ok, self()}},
            reg_loop(State)
        catch
            Class:ErrorReason:Stacktrace ->
                Parent ! {Ref, {error, {Class, ErrorReason, Stacktrace}}}
        end
    end,
    Pid = erlang:spawn(InitFun),
    case erlang:register(Name, Pid) of
        true ->
            MRef = erlang:monitor(process, Pid),
            receive
                {Ref, {ok, Pid}} ->
                    erlang:demonitor(MRef, [flush]),
                    {ok, Pid};
                {Ref, {error, Reason}} ->
                    {error, Reason}
            after 5000 ->
                erlang:demonitor(MRef, [flush]),
                {error, init_timeout}
            end;
        false ->
            exit(Pid, kill),
            {error, {already_registered, Name}}
    end;
start_link(Name, Options) ->
    Parent = self(),
    Ref = make_ref(),
    InitFun = fun() ->
        try
            State = #state{name = Name, entries = #{}, by_pid = #{}},
            Parent ! {Ref, {ok, self()}},
            reg_loop(State)
        catch
            Class:ErrorReason:Stacktrace ->
                Parent ! {Ref, {error, {Class, ErrorReason, Stacktrace}}}
        end
    end,
    Pid = erlang:spawn(InitFun),
    MRef = erlang:monitor(process, Pid),
    receive
        {Ref, {ok, Pid}} ->
            erlang:demonitor(MRef, [flush]),
            {ok, Pid};
        {Ref, {error, Reason}} ->
            {error, Reason}
    after 5000 ->
        erlang:demonitor(MRef, [flush]),
        {error, init_timeout}
    end.

%%====================================================================
%% API - Registry Operations
%%====================================================================

%% @doc Stop the registry.
-spec stop(registry()) -> ok.
stop(Reg) when is_pid(Reg); is_atom(Reg) ->
    Pid = get_pid(Reg),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', MRef, process, Pid, _Info} -> ok
    after 5000 ->
        case is_process_alive(Pid) of
            true -> exit(Pid, kill);
            false -> ok
        end
    end.

%% @doc Register a process with metadata.
-spec register(registry(), key(), pid(), metadata()) -> ok | {error, term()}.
register(Reg, Key, Pid, Metadata) when is_pid(Pid) ->
    call(Reg, {register, Key, Pid, Metadata}).

%% @doc Register a process without metadata.
-spec register(registry(), key(), pid()) -> ok | {error, term()}.
register(Reg, Key, Pid) ->
    register(Reg, Key, Pid, #{}).

%% @doc Unregister a process.
-spec unregister(registry(), key()) -> ok.
unregister(Reg, Key) ->
    call(Reg, {unregister, Key}).

%% @doc Find a process by key.
-spec find(registry(), key()) -> {ok, pid()} | {error, not_found}.
find(Reg, Key) ->
    call(Reg, {find, Key}).

%% @doc Find a process by key (returning pid directly).
-spec whereis(registry(), key()) -> pid() | undefined.
whereis(Reg, Key) ->
    call(Reg, {whereis, Key}).

%% @doc Match processes by pattern (Erlang match specification).
-spec match(registry(), key(), metadata()) -> [{key(), pid(), metadata()}].
match(Reg, KeyPattern, MetadataPattern) ->
    call(Reg, {match, KeyPattern, MetadataPattern}).

%% @doc Match all processes by metadata pattern.
-spec match(registry(), metadata()) -> [{key(), pid(), metadata()}].
match(Reg, MetadataPattern) ->
    match(Reg, '_', MetadataPattern).

%% @doc List all processes in the registry.
-spec list(registry()) -> [{key(), pid(), metadata()}].
list(Reg) ->
    list(Reg, '_').

%% @doc List processes matching a key pattern.
-spec list(registry(), key()) -> [{key(), pid(), metadata()}].
list(Reg, KeyPattern) ->
    call(Reg, {list, KeyPattern}).

%% @doc Update metadata for a registered process.
-spec update_metadata(registry(), key(), metadata()) -> ok | {error, not_found}.
update_metadata(Reg, Key, NewMetadata) ->
    call(Reg, {update_metadata, Key, NewMetadata}).

%% @doc Get metadata for a registered process.
-spec get_metadata(registry(), key()) -> {ok, metadata()} | {error, not_found}.
get_metadata(Reg, Key) ->
    call(Reg, {get_metadata, Key}).

%%====================================================================
%% Internal Functions
%%====================================================================

call(Reg, Request) when is_pid(Reg); is_atom(Reg) ->
    Pid = get_pid(Reg),
    Ref = make_ref(),
    Pid ! {call, {self(), Ref}, Request},
    receive
        {Ref, Reply} ->
            Reply
    after 5000 ->
        error({call_timeout, Pid, 5000})
    end.

get_pid(Pid) when is_pid(Pid) -> Pid;
get_pid(Name) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined -> error({no_process, Name});
        Pid -> Pid
    end.

%%====================================================================
%% Registry Loop
%%====================================================================

reg_loop(State) ->
    receive
        {call, From, Request} ->
            {Reply, NewState} = do_handle_call(Request, From, State),
            reply(From, Reply),
            reg_loop(NewState);

        {'DOWN', MRef, process, Pid, _Info} ->
            NewState = handle_down(MRef, Pid, State),
            reg_loop(NewState);

        _Info ->
            reg_loop(State)
    end.

reply({To, Tag}, Reply) ->
    To ! {Tag, Reply},
    ok.

%%====================================================================
%% Handle Calls
%%====================================================================

do_handle_call({register, Key, Pid, Metadata}, _From, State) ->
    #state{entries = Entries, by_pid = ByPid} = State,
    case maps:is_key(Key, Entries) of
        true ->
            {{error, already_registered}, State};
        false ->
            case maps:is_key(Pid, ByPid) of
                true ->
                    {{error, pid_already_registered}, State};
                false ->
                    MRef = erlang:monitor(process, Pid),
                    Entry = #entry{
                        key = Key,
                        pid = Pid,
                        monitor = MRef,
                        metadata = Metadata
                    },
                    NewEntries = maps:put(Key, Entry, Entries),
                    NewByPid = maps:put(Pid, Key, ByPid),
                    {ok, State#state{entries = NewEntries, by_pid = NewByPid}}
            end
    end;

do_handle_call({unregister, Key}, _From, State) ->
    #state{entries = Entries, by_pid = ByPid} = State,
    case maps:get(Key, Entries, undefined) of
        undefined ->
            {ok, State};
        #entry{pid = Pid, monitor = MRef} ->
            erlang:demonitor(MRef, [flush]),
            NewEntries = maps:remove(Key, Entries),
            NewByPid = maps:remove(Pid, ByPid),
            {ok, State#state{entries = NewEntries, by_pid = NewByPid}}
    end;

do_handle_call({find, Key}, _From, State) ->
    #state{entries = Entries} = State,
    case maps:get(Key, Entries, undefined) of
        undefined ->
            {{error, not_found}, State};
        #entry{pid = Pid} ->
            {{ok, Pid}, State}
    end;

do_handle_call({whereis, Key}, _From, State) ->
    #state{entries = Entries} = State,
    case maps:get(Key, Entries, undefined) of
        undefined ->
            {undefined, State};
        #entry{pid = Pid} ->
            {Pid, State}
    end;

do_handle_call({match, KeyPattern, MetadataPattern}, _From, State) ->
    #state{entries = Entries} = State,
    MatchFun = fun(_K, #entry{key = Key, pid = Pid, metadata = Metadata}) ->
        match_key(Key, KeyPattern) andalso match_metadata(Metadata, MetadataPattern)
    end,
    Result = [{K, P, M} || {K, #entry{pid = P, metadata = M}} <- maps:to_list(Entries),
                         MatchFun(K, maps:get(K, Entries))],
    {Result, State};

do_handle_call({list, KeyPattern}, _From, State) ->
    #state{entries = Entries} = State,
    Result = case KeyPattern of
        '_' ->
            [{K, P, M} || {K, #entry{pid = P, metadata = M}} <- maps:to_list(Entries)];
        _ ->
            [{K, P, M} || {K, #entry{pid = P, metadata = M}} <- maps:to_list(Entries),
                           match_key(K, KeyPattern)]
    end,
    {Result, State};

do_handle_call({update_metadata, Key, NewMetadata}, _From, State) ->
    #state{entries = Entries} = State,
    case maps:get(Key, Entries, undefined) of
        undefined ->
            {{error, not_found}, State};
        Entry ->
            UpdatedEntry = Entry#entry{metadata = NewMetadata},
            {ok, State#state{entries = maps:put(Key, UpdatedEntry, Entries)}}
    end;

do_handle_call({get_metadata, Key}, _From, State) ->
    #state{entries = Entries} = State,
    case maps:get(Key, Entries, undefined) of
        undefined ->
            {{error, not_found}, State};
        #entry{metadata = Metadata} ->
            {{ok, Metadata}, State}
    end;

do_handle_call(_Request, _From, State) ->
    {{error, unknown_request}, State}.

%%====================================================================
%% Handle DOWN
%%====================================================================

handle_down(MRef, Pid, State) ->
    #state{entries = Entries, by_pid = ByPid} = State,
    case maps:get(Pid, ByPid, undefined) of
        undefined ->
            State;
        Key ->
            Entry = maps:get(Key, Entries, #entry{}),
            case Entry#entry.monitor of
                MRef ->
                    NewEntries = maps:remove(Key, Entries),
                    NewByPid = maps:remove(Pid, ByPid),
                    State#state{entries = NewEntries, by_pid = NewByPid};
                _ ->
                    State
            end
    end.

%%====================================================================
%% Match Functions
%%====================================================================

match_key(_Key, '_') -> true;
match_key(Key, Key) -> true;
match_key(_Key, _Pattern) -> false.

match_metadata(_Metadata, '_') -> true;
match_metadata(Metadata, Pattern) when is_map(Pattern) ->
    maps:fold(fun(K, V, Acc) ->
        case maps:is_key(K, Metadata) of
            false -> false;
            _ -> Acc andalso maps:get(K, Metadata) =:= V
        end
    end, true, Pattern);
match_metadata(_Metadata, _Pattern) -> false.
