%%%-------------------------------------------------------------------
%%% @doc Catena GenServer Behavior (Phase 5.2)
%%%
%%% This module implements a GenServer-compatible behavior for Catena.
%%% It provides the familiar gen_server callback interface while being
%%% a thin wrapper around the Catena actor infrastructure.
%%%
%%% == Callbacks ==
%%%
%%% The following callbacks are required/optional:
%%% - init/1 - Initialize the server (required)
%%% - handle_call/3 - Handle synchronous requests (optional)
%%% - handle_cast/2 - Handle asynchronous requests (optional)
%%% - handle_info/2 - Handle non-OTP messages (optional)
%%% - terminate/2 - Cleanup on termination (optional)
%%% - code_change/3 - Handle code upgrades (optional)
%%%
%%% == Usage ==
%%%
%%% ```
%%% -module(my_server).
%%% -behaviour(catena_gen_server).
%%%
%%% -export([init/1, handle_call/3, handle_cast/2]).
%%%
%%% init(Args) -> {ok, InitialState}.
%%% handle_call(Request, From, State) -> {reply, Reply, NewState}.
%%% handle_cast(Request, State) -> {noreply, NewState}.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_gen_server).

%% Callbacks
-export([
    behaviour_info/1
]).

%% API
-export([
    start/3,
    start/4,
    start_link/3,
    start_link/4,
    stop/1,
    stop/2,
    stop/3,
    call/2,
    call/3,
    cast/2,
    abcast/2,
    multi_call/2,
    multi_call/3,
    reply/2
]).

%%====================================================================
%% Types
%%====================================================================

-type from() :: {pid(), reference()}.
-type server_name() :: atom() | {global, term()} | {via, module(), term()}.
-type call_result() ::
    {reply, term(), NewState :: term()} |
    {reply, term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-type cast_result() ::
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

-export_type([
    from/0,
    server_name/0,
    call_result/0,
    cast_result/0
]).

%%====================================================================
%% Callback Definitions
%%====================================================================

%% @doc Define required callbacks for catena_gen_server behaviour
behaviour_info(callbacks) ->
    [
        {init, 1},
        {handle_call, 3},
        {handle_cast, 2},
        {handle_info, 2},
        {terminate, 2},
        {code_change, 3}
    ];
behaviour_info(_) ->
    undefined.

%%====================================================================
%% API - Start Functions
%%====================================================================

%% @doc Start a standalone gen_server.
-spec start(module(), term(), list()) -> {ok, pid()} | {error, term()}.
start(Module, Args, Options) ->
    do_start(Module, Args, Options, fun erlang:spawn/1).

%% @doc Start a named standalone gen_server.
-spec start(server_name(), module(), term(), list()) -> {ok, pid()} | {error, term()}.
start(Name, Module, Args, Options) when is_atom(Name) ->
    case do_start(Module, Args, Options, fun erlang:spawn/1) of
        {ok, Pid} ->
            case register_name(Name, Pid) of
                true -> {ok, Pid};
                {error, Reason} ->
                    exit(Pid, kill),
                    {error, Reason}
            end;
        Error ->
            Error
    end.

%% @doc Start a linked gen_server.
-spec start_link(module(), term(), list()) -> {ok, pid()} | {error, term()}.
start_link(Module, Args, Options) ->
    do_start(Module, Args, Options, fun erlang:spawn_link/1).

%% @doc Start a named linked gen_server.
-spec start_link(server_name(), module(), term(), list()) -> {ok, pid()} | {error, term()}.
start_link(Name, Module, Args, Options) when is_atom(Name) ->
    case do_start(Module, Args, Options, fun erlang:spawn_link/1) of
        {ok, Pid} ->
            case register_name(Name, Pid) of
                true -> {ok, Pid};
                {error, Reason} ->
                    exit(Pid, kill),
                    {error, Reason}
            end;
        Error ->
            Error
    end.

%%====================================================================
%% API - Stop Functions
%%====================================================================

%% @doc Stop a gen_server with reason 'normal'.
-spec stop(pid() | server_name()) -> ok.
stop(Server) ->
    stop(Server, normal, infinity).

%% @doc Stop a gen_server with a reason.
-spec stop(pid() | server_name(), term()) -> ok.
stop(Server, Reason) ->
    stop(Server, Reason, infinity).

%% @doc Stop a gen_server with reason and timeout.
-spec stop(pid() | server_name(), term(), timeout()) -> ok.
stop(Server, Reason, Timeout) ->
    Pid = get_pid(Server),
    call(Pid, {stop, Reason}, Timeout),
    wait_for_termination(Pid).

%%====================================================================
%% API - Call Functions
%%====================================================================

%% @doc Send a synchronous request and wait for reply.
-spec call(pid() | server_name(), term()) -> term().
call(Server, Request) ->
    call(Server, Request, 5000).

%% @doc Send a synchronous request with timeout.
-spec call(pid() | server_name(), term(), timeout()) -> term().
call(Server, Request, Timeout) ->
    Pid = get_pid(Server),
    Ref = make_ref(),
    Pid ! {call, {self(), Ref}, {'$gen_call', Request}},
    receive
        {Ref, Reply} ->
            Reply;
        {Ref, error, Reason} ->
            error({call_failed, Pid, Reason})
    after Timeout ->
        error({call_timeout, Pid, Timeout})
    end.

%%====================================================================
%% API - Cast Functions
%%====================================================================

%% @doc Send an asynchronous request.
-spec cast(pid() | server_name(), term()) -> ok.
cast(Server, Request) ->
    Pid = get_pid(Server),
    Pid ! {cast, {'$gen_cast', Request}},
    ok.

%% @doc Send an asynchronous request to all registered nodes.
-spec abcast(server_name(), term()) -> {abcast, node()}.
abcast(Name, Request) when is_atom(Name) ->
    cast(Name, Request),
    {abcast, node()}.

%% @doc Send a synchronous request to multiple servers.
-spec multi_call([pid() | server_name()], term()) -> {Replies :: [{node(), term()}], BadNodes :: [node()]}.
multi_call(Servers, Request) ->
    multi_call(Servers, Request, 5000).

%% @doc Send a synchronous request with timeout to multiple servers.
-spec multi_call([pid() | server_name()], term(), timeout()) -> {Replies :: [{node(), term()}], BadNodes :: [node()]}.
multi_call(Servers, Request, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    lists:foreach(fun(Server) ->
        spawn(fun() ->
            Result = try call(Server, Request, Timeout)
                       catch _:_ -> {error, call_failed}
                       end,
            Parent ! {Ref, Server, Result}
        end)
    end, Servers),
    collect_multi_call(Ref, length(Servers), [], []).

collect_multi_call(_Ref, 0, Replies, BadNodes) ->
    {lists:reverse(Replies), lists:reverse(BadNodes)};
collect_multi_call(Ref, Count, Replies, BadNodes) ->
    receive
        {Ref, Server, {error, _}} ->
            collect_multi_call(Ref, Count - 1, Replies, [get_node(Server) | BadNodes]);
        {Ref, Server, Result} ->
            collect_multi_call(Ref, Count - 1, [{get_node(Server), Result} | Replies], BadNodes)
    after 5000 ->
        {lists:reverse(Replies), lists:reverse(BadNodes)}
    end.

%% @private Get node from server reference.
get_node(Server) when is_pid(Server) -> node(Server);
get_node(_Server) when is_atom(_Server) -> node().

%%====================================================================
%% API - Reply Functions
%%====================================================================

%% @doc Reply to a synchronous call.
-spec reply(from(), term()) -> ok.
reply({To, Tag}, Reply) ->
    To ! {Tag, Reply},
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Get pid from server name.
get_pid(Pid) when is_pid(Pid) -> Pid;
get_pid(Name) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined -> error({no_process, Name});
        Pid -> Pid
    end;
get_pid({global, _Name}) ->
    error(global_not_supported);
get_pid({via, _Module, _Name}) ->
    error(via_not_supported).

%% @private Register a process name.
register_name(Name, Pid) when is_atom(Name) ->
    case erlang:register(Name, Pid) of
        true -> true;
        false -> {error, {already_registered, Name}}
    end;
register_name(_Name, _Pid) ->
    true.

%% @private Start a gen_server process.
do_start(Module, Args, Options, SpawnFun) ->
    Parent = self(),
    Ref = make_ref(),
    InitFun = fun() ->
        try
            case Module:init(Args) of
                {ok, State} ->
                    Parent ! {Ref, {ok, self()}},
                    gen_loop(Module, State, Options);
                {stop, Reason} ->
                    Parent ! {Ref, {error, Reason}};
                {error, Reason} ->
                    Parent ! {Ref, {error, Reason}};
                ignore ->
                    Parent ! {Ref, {ok, self()}},
                    ok
            end
        catch
            Class:ErrorReason:Stacktrace ->
                Parent ! {Ref, {error, {Class, ErrorReason, Stacktrace}}}
        end
    end,
    Pid = SpawnFun(InitFun),
    receive
        {Ref, Result} ->
            Result;
        {'EXIT', Pid, Reason} ->
            {error, Reason}
    after 5000 ->
        {error, init_timeout}
    end.

%% @private Wait for process termination.
wait_for_termination(Pid) ->
    wait_for_termination(Pid, 5000).

wait_for_termination(Pid, Timeout) ->
    MRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MRef, process, Pid, _Info} ->
            ok
    after Timeout ->
        case is_process_alive(Pid) of
            true -> exit(Pid, kill);
            false -> ok
        end
    end.

%% @private Main gen_server loop.
gen_loop(Module, State, Options) ->
    Timeout = proplists:get_value(timeout, Options, infinity),
    receive
        %% Synchronous call
        {call, From, {'$gen_call', Request}} ->
            CallerPid = element(1, From),
            CallerRef = element(2, From),
            case Module:handle_call(Request, From, State) of
                {reply, Reply, NewState} ->
                    CallerPid ! {CallerRef, Reply},
                    gen_loop(Module, NewState, Options);
                {noreply, NewState} ->
                    gen_loop(Module, NewState, Options);
                {stop, Reason, Reply, NewState} ->
                    CallerPid ! {CallerRef, Reply},
                    do_terminate(Module, Reason, NewState);
                {stop, Reason, NewState} ->
                    do_terminate(Module, Reason, NewState)
            end;

        %% Asynchronous cast
        {cast, {'$gen_cast', Request}} ->
            case Module:handle_cast(Request, State) of
                {noreply, NewState} ->
                    gen_loop(Module, NewState, Options);
                {stop, Reason, NewState} ->
                    do_terminate(Module, Reason, NewState)
            end;

        %% System messages (shutdown, etc.)
        {'EXIT', _Pid, shutdown} ->
            do_terminate(Module, shutdown, State);

        {'EXIT', _Pid, Reason} ->
            case Module:handle_info({'EXIT', _Pid, Reason}, State) of
                {noreply, NewState} ->
                    gen_loop(Module, NewState, Options);
                {stop, StopReason, NewState} ->
                    do_terminate(Module, StopReason, NewState)
            end;

        %% Stop message from our stop/3 function
        {call, From, {stop, Reason}} ->
            CallerPid = element(1, From),
            CallerRef = element(2, From),
            CallerPid ! {CallerRef, ok},
            do_terminate(Module, Reason, State);

        %% Generic info message
        Info ->
            case Module:handle_info(Info, State) of
                {noreply, NewState} ->
                    gen_loop(Module, NewState, Options);
                {stop, Reason, NewState} ->
                    do_terminate(Module, Reason, NewState)
            end
    after Timeout ->
        %% Handle timeout
        case Module:handle_info(timeout, State) of
            {noreply, NewState} ->
                gen_loop(Module, NewState, Options);
            {stop, Reason, NewState} ->
                do_terminate(Module, Reason, NewState)
        end
    end.

%% @private Terminate the gen_server.
do_terminate(Module, Reason, State) ->
    case erlang:function_exported(Module, terminate, 2) of
        true ->
            try Module:terminate(Reason, State)
            catch _:_ -> ok
            end;
        false ->
            ok
    end,
    exit(Reason).
