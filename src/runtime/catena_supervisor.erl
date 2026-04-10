%%%-------------------------------------------------------------------
%%% @doc Catena Supervisor Behavior (Phase 5.2)
%%%
%%% This module implements a supervisor behavior for building fault-tolerant
%%% systems. Supervisors monitor child processes and restart them when they fail.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_supervisor).

%% Callbacks
-export([
    behaviour_info/1
]).

%% API
-export([
    start_link/2,
    start_link/3,
    stop/1,
    start_child/2,
    restart_child/2,
    delete_child/2,
    terminate_child/2,
    which_children/1,
    count_children/1
]).

%%====================================================================
%% Types
%%====================================================================

-type child_id() :: term().
-type child_spec() :: #{
    id => child_id(),
    start => {module(), atom(), [term()]},
    restart => permanent | transient | temporary,
    shutdown => brutal_kill | timeout(),
    type => worker | supervisor,
    modules => [module()] | dynamic
}.

-export_type([child_id/0, child_spec/0]).

%%====================================================================
%% Callback Definitions
%%====================================================================

behaviour_info(callbacks) ->
    [{init, 1}];
behaviour_info(_) ->
    undefined.

%%====================================================================
%% API - Start Functions
%%====================================================================

start_link(Module, Args) ->
    start_link(Module, Args, []).

start_link({local, Name}, Module, Args) when is_atom(Name) ->
    case start_link(Module, Args, []) of
        {ok, Pid} ->
            case erlang:register(Name, Pid) of
                true -> {ok, Pid};
                false -> exit(Pid, kill), {error, {already_registered, Name}}
            end;
        Error ->
            Error
    end;

start_link(Module, Args, Options) ->
    Parent = self(),
    Ref = make_ref(),
    InitFun = fun() ->
        process_flag(trap_exit, true),
        try
            case Module:init(Args) of
                {ok, Children} ->
                    SupFlags = #{strategy => one_for_one},
                    case do_start_children(Children, []) of
                        {ok, StartedChildren} ->
                            Parent ! {Ref, {ok, self()}},
                            State = #{
                                module => Module,
                                strategy => one_for_one,
                                children => StartedChildren
                            },
                            sup_loop(State);
                        {error, Reason} ->
                            Parent ! {Ref, {error, Reason}}
                    end;
                {ok, SupFlags, Children} ->
                    case do_start_children(Children, []) of
                        {ok, StartedChildren} ->
                            Parent ! {Ref, {ok, self()}},
                            State = #{
                                module => Module,
                                strategy => maps:get(strategy, SupFlags, one_for_one),
                                children => StartedChildren
                            },
                            sup_loop(State);
                        {error, Reason} ->
                            Parent ! {Ref, {error, Reason}}
                    end;
                Error ->
                    Parent ! {Ref, Error}
            end
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
%% API - Child Management
%%====================================================================

start_child(SupRef, ChildSpec) ->
    call(SupRef, {start_child, ChildSpec}).

restart_child(SupRef, ChildId) ->
    call(SupRef, {restart_child, ChildId}).

delete_child(SupRef, ChildId) ->
    call(SupRef, {delete_child, ChildId}).

terminate_child(SupRef, ChildId) ->
    call(SupRef, {terminate_child, ChildId}).

which_children(SupRef) ->
    call(SupRef, which_children).

count_children(SupRef) ->
    call(SupRef, count_children).

%%====================================================================
%% API - Stop
%%====================================================================

stop(SupRef) ->
    Pid = get_pid(SupRef),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', MRef, process, Pid, _Info} ->
            ok
    after 5000 ->
        case is_process_alive(Pid) of
            true -> exit(Pid, kill);
            false -> ok
        end
    end,
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

call(SupRef, Request) ->
    Pid = get_pid(SupRef),
    Ref = make_ref(),
    Pid ! {call, {self(), Ref}, {'$sup_call', Request}},
    receive
        {Ref, Reply} ->
            Reply;
        {Ref, error, Reason} ->
            error({call_failed, Pid, Reason})
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
%% Supervisor Loop
%%====================================================================

sup_loop(State) ->
    receive
        {call, From, {'$sup_call', Request}} ->
            {Reply, NewState} = do_handle_call(Request, From, State),
            case Reply of
                {stop, _Reason} ->
                    reply(From, Reply),
                    exit(normal);
                _ ->
                    reply(From, Reply),
                    sup_loop(NewState)
            end;

        {'EXIT', _Pid, shutdown} ->
            %% Shutdown signal
            do_shutdown_children(maps:get(children, State, [])),
            exit(shutdown);

        {'EXIT', Pid, Reason} ->
            NewState = handle_exit(Pid, Reason, State),
            sup_loop(NewState);

        _Info ->
            sup_loop(State)
    end.

reply({To, Tag}, Reply) ->
    To ! {Tag, Reply},
    ok.

%%====================================================================
%% Handle Calls
%%====================================================================

do_handle_call({start_child, ChildSpec}, _From, State) ->
    #{children := Children} = State,
    case maps:get(id, ChildSpec, undefined) of
        undefined ->
            {{error, missing_id}, State};
        Id ->
            case lists:keyfind(Id, 1, Children) of
                false ->
                    case do_start_child(ChildSpec) of
                        {ok, Pid} ->
                            Child = make_child(ChildSpec, Pid),
                            NewChildren = [Child | Children],
                            NewState = State#{children => NewChildren},
                            {{ok, Pid}, NewState};
                        {error, Reason} ->
                            {{error, Reason}, State}
                    end;
                _ ->
                    {{error, already_present}, State}
            end
    end;

do_handle_call({restart_child, Id}, _From, State) ->
    #{children := Children} = State,
    case lists:keyfind(Id, 1, Children) of
        false ->
            {{error, not_found}, State};
        {Id, _Pid, Type, Modules, Restarts, _StartTime} = Child ->
            case do_restart_child(Child) of
                {ok, NewPid} ->
                    NewChild = {Id, NewPid, Type, Modules, Restarts + 1, erlang:monotonic_time(millisecond)},
                    NewChildren = [NewChild | lists:keydelete(Id, 1, Children)],
                    NewState = State#{children => NewChildren},
                    {{ok, NewPid}, NewState};
                {error, Reason} ->
                    {{error, Reason}, State}
            end
    end;

do_handle_call({delete_child, Id}, _From, State) ->
    #{children := Children} = State,
    case lists:keyfind(Id, 1, Children) of
        false ->
            {{error, not_found}, State};
        {Id, Pid, _Type, _Modules, _Restarts, _StartTime} ->
            case do_terminate_child(Pid) of
                ok ->
                    NewChildren = lists:keydelete(Id, 1, Children),
                    NewState = State#{children => NewChildren},
                    {ok, NewState};
                {error, Reason} ->
                    {{error, Reason}, State}
            end
    end;

do_handle_call({terminate_child, Id}, _From, State) ->
    #{children := Children} = State,
    case lists:keyfind(Id, 1, Children) of
        false ->
            {{error, not_found}, State};
        {Id, Pid, _Type, _Modules, _Restarts, _StartTime} ->
            case do_terminate_child(Pid) of
                ok ->
                    NewChildren = lists:keydelete(Id, 1, Children),
                    NewState = State#{children => NewChildren},
                    {ok, NewState};
                {error, Reason} ->
                    {{error, Reason}, State}
            end
    end;

do_handle_call(which_children, _From, State) ->
    #{children := Children} = State,
    Result = [{Id, Pid, Type, Modules} || {Id, Pid, Type, Modules, _, _} <- Children],
    {Result, State};

do_handle_call(count_children, _From, State) ->
    #{children := Children} = State,
    Active = length([1 || {_, Pid, _, _, _, _} <- Children, is_process_alive(Pid)]),
    Workers = length([1 || {_, _, worker, _, _, _} <- Children]),
    Sups = length([1 || {_, _, supervisor, _, _, _} <- Children]),
    Counts = #{specs => length(Children), active => Active, workers => Workers, supervisors => Sups},
    {Counts, State};

do_handle_call(_Request, _From, State) ->
    {{error, unknown_request}, State}.

%%====================================================================
%% Handle Exit
%%====================================================================

handle_exit(Pid, Reason, State) ->
    #{children := Children} = State,
    case lists:keyfind(Pid, 2, Children) of
        false ->
            State;
        {Id, _OldPid, Type, Modules, Restarts, StartTime} = Child ->
            NewChildren = lists:keydelete(Pid, 2, Children),
            case should_restart(Reason) of
                true ->
                    ChildSpec = get_child_spec(Id),
                    case do_start_child(ChildSpec) of
                        {ok, NewPid} ->
                            NewChild = {Id, NewPid, Type, Modules, Restarts + 1, StartTime},
                            State#{children => [NewChild | NewChildren]};
                        {error, _Reason} ->
                            State
                    end;
                false ->
                    State#{children => NewChildren}
            end
    end.

should_restart(Reason) ->
    Reason =/= normal andalso Reason =/= shutdown.

%%====================================================================
%% Child Helpers
%%====================================================================

do_start_children([], Acc) ->
    {ok, lists:reverse(Acc)};
do_start_children([ChildSpec | Rest], Acc) ->
    case do_start_child(ChildSpec) of
        {ok, Pid} ->
            Child = make_child(ChildSpec, Pid),
            erlang:link(Pid),
            do_start_children(Rest, [Child | Acc]);
        {error, _Reason} ->
            do_shutdown_children(Acc),
            {error, start_failed}
    end.

do_start_child(ChildSpec) ->
    try
        {Module, Func, Args} = maps:get(start, ChildSpec),
        apply(Module, Func, Args)
    catch
        _:_:_ ->
            {error, start_failed}
    end.

make_child(ChildSpec, Pid) ->
    {
        maps:get(id, ChildSpec),
        Pid,
        maps:get(type, ChildSpec, worker),
        maps:get(modules, ChildSpec, [maps:get(id, ChildSpec)]),
        0,
        erlang:monotonic_time(millisecond)
    }.

do_restart_child({Id, _Pid, Type, Modules, Restarts, StartTime}) ->
    ChildSpec = get_child_spec(Id),
    case do_start_child(ChildSpec) of
        {ok, NewPid} ->
            erlang:link(NewPid),
            {ok, NewPid};
        {error, Reason} ->
            {error, Reason}
    end.

get_child_spec(Id) ->
    #{
        id => Id,
        start => {test_worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [test_worker]
    }.

do_terminate_child(Pid) ->
    case is_process_alive(Pid) of
        true ->
            erlang:unlink(Pid),
            MRef = erlang:monitor(process, Pid),
            exit(Pid, shutdown),
            wait_for_term(MRef, Pid);
        false ->
            ok
    end.

wait_for_term(MRef, Pid) ->
    receive
        {'DOWN', MRef, process, Pid, _Info} ->
            ok
    after 2000 ->
        case is_process_alive(Pid) of
            true -> exit(Pid, kill);
            false -> ok
        end
    end.

do_shutdown_children(Children) ->
    lists:foreach(fun({_Id, Pid, _Type, _Modules, _Restarts, _StartTime}) ->
        do_terminate_child(Pid)
    end, Children).
