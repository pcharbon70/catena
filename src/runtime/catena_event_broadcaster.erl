%%%-------------------------------------------------------------------
%%% @doc Catena Event Broadcaster (Phase 5.3)
%%%
%%% This module implements an event broadcasting mechanism where events
%%% are dispatched to multiple listeners. Unlike pub/sub which uses
%%% topic-based routing, event broadcasters dispatch all events to all
%%% registered listeners.
%%%
%%% This is useful for:
%%% - Event logging/auditing
%%% - Metrics collection
%%% - Fan-out notification patterns
%%% - Debugging and tracing
%%%
%%% == Usage ==
%%%
%%% ```
%%% %% Start a broadcaster
%%% {ok, Broadcaster} = catena_event_broadcaster:start_link(my_events).
%%%
%%% %% Add a listener
%%% ok = catena_event_broadcaster:add_listener(my_events, self()).
%%%
%%% %% Broadcast an event
%%% ok = catena_event_broadcaster:broadcast(my_events, #{type => user_created, id => 123}).
%%%
%%% %% Listener receives:
%%% %% {event, #{type => user_created, id => 123}}
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_event_broadcaster).

%% API
-export([
    start_link/1,
    start_link/2,
    stop/1,
    add_listener/2,
    add_listener/3,
    remove_listener/2,
    broadcast/2,
    broadcast/3,
    list_listeners/1,
    listener_count/1
]).

%%====================================================================
%% Types
%%====================================================================

-type broadcaster_name() :: atom() | pid().
-type broadcaster() :: broadcaster_name() | {via, module(), term()}.
-type event() :: term().

-export_type([broadcaster_name/0, broadcaster/0, event/0]).

-record(listener, {
    pid :: pid(),
    monitor :: reference(),
    filter :: fun((event()) -> boolean()) | undefined
}).

-record(state, {
    name :: term(),
    listeners :: [#listener{}]
}).

%%====================================================================
%% API - Start Functions
%%====================================================================

%% @doc Start a broadcaster with a name.
-spec start_link(broadcaster_name()) -> {ok, pid()} | {error, term()}.
start_link({local, Name}) when is_atom(Name) ->
    start_link(Name, []);
start_link(Name) ->
    start_link(Name, []).

%% @doc Start a broadcaster with a name and options.
-spec start_link(broadcaster_name(), list()) -> {ok, pid()} | {error, term()}.
start_link({local, Name}, Options) when is_atom(Name) ->
    case do_start_link(Name, Options) of
        {ok, Pid} ->
            case erlang:register(Name, Pid) of
                true -> {ok, Pid};
                false -> exit(Pid, kill), {error, {already_registered, Name}}
            end;
        Error ->
            Error
    end;
start_link(Name, Options) ->
    do_start_link(Name, Options).

do_start_link(Name, Options) ->
    Parent = self(),
    Ref = make_ref(),
    InitFun = fun() ->
        try
            State = #state{
                name = Name,
                listeners = []
            },
            Parent ! {Ref, {ok, self()}},
            broadcaster_loop(State)
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
%% API - Broadcaster Operations
%%====================================================================

%% @doc Stop the broadcaster.
-spec stop(broadcaster()) -> ok.
stop(Broadcaster) when is_pid(Broadcaster); is_atom(Broadcaster) ->
    Pid = get_pid(Broadcaster),
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

%% @doc Add a listener without filter.
-spec add_listener(broadcaster(), pid()) -> ok | {error, term()}.
add_listener(Broadcaster, ListenerPid) ->
    add_listener(Broadcaster, ListenerPid, undefined).

%% @doc Add a listener with an optional filter function.
-spec add_listener(broadcaster(), pid(), fun((event()) -> boolean()) | undefined) -> ok | {error, term()}.
add_listener(Broadcaster, ListenerPid, Filter) when is_pid(ListenerPid) ->
    call(Broadcaster, {add_listener, ListenerPid, Filter}).

%% @doc Remove a listener.
-spec remove_listener(broadcaster(), pid()) -> ok.
remove_listener(Broadcaster, ListenerPid) ->
    call(Broadcaster, {remove_listener, ListenerPid}).

%% @doc Broadcast an event to all listeners.
-spec broadcast(broadcaster(), event()) -> ok.
broadcast(Broadcaster, Event) ->
    broadcast(Broadcaster, Event, #{}).

%% @doc Broadcast an event with options.
-spec broadcast(broadcaster(), event(), map()) -> ok.
broadcast(Broadcaster, Event, Options) ->
    call(Broadcaster, {broadcast, Event, Options}),
    ok.

%% @doc List all listeners.
-spec list_listeners(broadcaster()) -> [pid()].
list_listeners(Broadcaster) ->
    call(Broadcaster, list_listeners).

%% @doc Get the number of listeners.
-spec listener_count(broadcaster()) -> non_neg_integer().
listener_count(Broadcaster) ->
    call(Broadcaster, listener_count).

%%====================================================================
%% Internal Functions
%%====================================================================

call(Broadcaster, Request) when is_pid(Broadcaster); is_atom(Broadcaster) ->
    Pid = get_pid(Broadcaster),
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
%% Broadcaster Loop
%%====================================================================

broadcaster_loop(State) ->
    receive
        {call, From, Request} ->
            {Reply, NewState} = do_handle_call(Request, From, State),
            reply(From, Reply),
            broadcaster_loop(NewState);

        {'DOWN', MRef, process, Pid, _Info} ->
            NewState = handle_down(MRef, Pid, State),
            broadcaster_loop(NewState);

        _Info ->
            broadcaster_loop(State)
    end.

reply({To, Tag}, Reply) ->
    To ! {Tag, Reply},
    ok.

%%====================================================================
%% Handle Calls
%%====================================================================

do_handle_call({add_listener, ListenerPid, Filter}, _From, State) ->
    #state{listeners = Listeners} = State,
    case lists:keyfind(ListenerPid, #listener.pid, Listeners) of
        false ->
            MRef = erlang:monitor(process, ListenerPid),
            Listener = #listener{
                pid = ListenerPid,
                monitor = MRef,
                filter = Filter
            },
            {ok, State#state{listeners = [Listener | Listeners]}};
        _ ->
            {{error, already_a_listener}, State}
    end;

do_handle_call({remove_listener, ListenerPid}, _From, State) ->
    #state{listeners = Listeners} = State,
    case lists:keyfind(ListenerPid, #listener.pid, Listeners) of
        false ->
            {ok, State};
        #listener{monitor = MRef} ->
            erlang:demonitor(MRef, [flush]),
            NewListeners = lists:keydelete(ListenerPid, #listener.pid, Listeners),
            {ok, State#state{listeners = NewListeners}}
    end;

do_handle_call({broadcast, Event, Options}, _From, State) ->
    #state{listeners = Listeners} = State,
    lists:foreach(fun(#listener{pid = Pid, filter = Filter}) ->
        case apply_filter(Filter, Event) of
            true ->
                SendFun = maps:get(send_fun, Options, fun send_event/3),
                SendFun(Pid, Event, Options);
            false ->
                ok
        end
    end, Listeners),
    {ok, State};

do_handle_call(list_listeners, _From, State) ->
    #state{listeners = Listeners} = State,
    Pids = [P || #listener{pid = P} <- Listeners],
    {Pids, State};

do_handle_call(listener_count, _From, State) ->
    #state{listeners = Listeners} = State,
    {length(Listeners), State};

do_handle_call(_Request, _From, State) ->
    {{error, unknown_request}, State}.

%%====================================================================
%% Handle DOWN
%%====================================================================

handle_down(MRef, Pid, State) ->
    #state{listeners = Listeners} = State,
    NewListeners = lists:filter(fun(
        #listener{pid = P, monitor = M}) ->
        not (P =:= Pid andalso M =:= MRef)
    end, Listeners),
    State#state{listeners = NewListeners}.

%%====================================================================
%% Filter Functions
%%====================================================================

apply_filter(undefined, _Event) -> true;
apply_filter(Filter, Event) when is_function(Filter, 1) ->
    try Filter(Event)
    catch _:_ -> true
    end.

%% @private Default send function.
send_event(ListenerPid, Event, _Options) when is_pid(ListenerPid) ->
    ListenerPid ! {event, Event}.
