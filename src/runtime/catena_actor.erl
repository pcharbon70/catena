%%%-------------------------------------------------------------------
%%% @doc Catena Actor Behavior (Phase 5.1.5)
%%%
%%% This module implements the actor pattern for Catena, providing:
%%% - Stateful message handling through actor loops
%%% - Standard behavior protocol for all actors
%%% - Integration with the effect system (actors as effect handlers)
%%% - Lifecycle management (spawn, stop, supervision)
%%%
%%% == Actor Model ==
%%%
%%% An actor is a process that:
%%% 1. Maintains private state
%%% 2. Processes messages one at a time
%%% 3. Can spawn new actors
%%% 4. Can send messages to other actors
%%% 5. May change state in response to messages
%%%
%%% == Usage ==
%%%
%%% Define an actor module:
%%% ```
%%% -module(my_actor).
%%% -behaviour(catena_actor).
%%%
%%% -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
%%%
%%% init(Args) -> {ok, InitialState}.
%%% handle_call(Request, From, State) -> {reply, Reply, NewState}.
%%% handle_cast(Request, State) -> {noreply, NewState}.
%%% handle_info(Info, State) -> {noreply, NewState}.
%%% terminate(Reason, State) -> ok.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_actor).

%% Callbacks for actor behavior
-export([
    behaviour_info/1
]).

%% Actor API
-export([
    start/2,
    start/3,
    start_link/2,
    start_link/3,
    stop/1,
    stop/2,
    call/2,
    call/3,
    cast/2,
    reply/2,
    send/2
]).

%%====================================================================
%% Types
%%====================================================================

-type from() :: {pid(), reference()}.
-type call_result() ::
    {reply, term(), NewState :: term()} |
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-type cast_result() ::
    {noreply, NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-type info_result() :: cast_result().

-export_type([
    from/0,
    call_result/0,
    cast_result/0
]).

%%====================================================================
%% Callback Definitions
%%====================================================================

%% @doc Define required callbacks for catena_actor behaviour
behaviour_info(callbacks) ->
    [
        {init, 1},
        {handle_call, 3},
        {handle_cast, 2},
        {handle_info, 2},
        {terminate, 2}
    ];
behaviour_info(_) ->
    undefined.

%%====================================================================
%% Actor API - Start Functions
%%====================================================================

%% @doc Start a standalone actor (not linked to parent).
-spec start(module(), term()) -> {ok, pid()} | {error, term()}.
start(Module, Args) ->
    start(Module, Args, []).

%% @doc Start a standalone actor with options.
-spec start(module(), term(), list()) -> {ok, pid()} | {error, term()}.
start(Module, Args, Options) ->
    do_start(Module, Args, Options, fun erlang:spawn/1).

%% @doc Start a linked actor (linked to parent).
-spec start_link(module(), term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Args) ->
    start_link(Module, Args, []).

%% @doc Start a linked actor with options.
-spec start_link(module(), term(), list()) -> {ok, pid()} | {error, term()}.
start_link(Module, Args, Options) ->
    do_start(Module, Args, Options, fun erlang:spawn_link/1).

%%====================================================================
%% Actor API - Stop Functions
%%====================================================================

%% @doc Stop an actor with reason 'normal'.
-spec stop(pid()) -> ok.
stop(Actor) ->
    stop(Actor, normal).

%% @doc Stop an actor with a reason.
-spec stop(pid(), term()) -> ok.
stop(Actor, Reason) ->
    Ref = make_ref(),
    Actor ! {stop, {self(), Ref}, Reason},
    receive
        {Ref, ok} -> ok
    after 5000 ->
        %% Timeout - force kill
        case is_process_alive(Actor) of
            true -> exit(Actor, kill);
            false -> ok
        end
    end.

%%====================================================================
%% Actor API - Messaging Functions
%%====================================================================

%% @doc Send a synchronous request and wait for reply.
-spec call(pid(), term()) -> term().
call(Actor, Request) ->
    call(Actor, Request, 5000).

%% @doc Send a synchronous request with timeout.
-spec call(pid(), term(), timeout()) -> term().
call(Actor, Request, Timeout) ->
    Ref = make_ref(),
    Actor ! {call, {self(), Ref}, Request},
    receive
        {Ref, Reply} ->
            Reply;
        {Ref, error, Reason} ->
            error({call_failed, Actor, Reason})
    after Timeout ->
        error({call_timeout, Actor, Timeout})
    end.

%% @doc Send an asynchronous message (fire and forget).
-spec cast(pid(), term()) -> ok.
cast(Actor, Request) ->
    Actor ! {cast, Request},
    ok.

%% @doc Reply to a synchronous call.
-spec reply(from(), term()) -> ok.
reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply},
    ok.

%% @doc Send a raw message to an actor (bypasses protocol).
-spec send(pid(), term()) -> ok.
send(Actor, Msg) ->
    Actor ! Msg,
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Start an actor process with given spawn function.
do_start(Module, Args, Options, SpawnFun) ->
    Parent = self(),
    Ref = make_ref(),
    InitFun = fun() ->
        try
            %% Initialize the actor
            case Module:init(Args) of
                {ok, State} ->
                    Parent ! {Ref, {ok, self()}},
                    actor_loop(Module, State);
                {stop, Reason} ->
                    Parent ! {Ref, {error, Reason}};
                {error, Reason} ->
                    Parent ! {Ref, {error, Reason}};
                ignore ->
                    Parent ! {Ref, {ok, self()}},
                    %% Exit immediately for ignore
                    ok
            end
        catch
            Class:ErrorReason:Stacktrace ->
                Parent ! {Ref, {error, {Class, ErrorReason, Stacktrace}}}
        end
    end,
    Pid = SpawnFun(InitFun),
    %% Wait for initialization result
    receive
        {Ref, Result} ->
            Result;
        {'EXIT', Pid, Reason} ->
            {error, Reason}
    after 5000 ->
        {error, init_timeout}
    end.

%%====================================================================
%% Actor Loop
%%====================================================================

%% @private Main actor loop - handles messages sequentially.
actor_loop(Module, State) ->
    receive
        %% Synchronous call
        {call, From, Request} ->
            CallerPid = element(1, From),
            CallerRef = element(2, From),
            case Module:handle_call(Request, From, State) of
                {reply, Reply, NewState} ->
                    CallerPid ! {CallerRef, Reply},
                    actor_loop(Module, NewState);
                {noreply, NewState} ->
                    actor_loop(Module, NewState);
                {stop, Reason, Reply, NewState} ->
                    CallerPid ! {CallerRef, Reply},
                    terminate(Module, Reason, NewState);
                {stop, Reason, NewState} ->
                    terminate(Module, Reason, NewState)
            end;

        %% Asynchronous cast
        {cast, Request} ->
            case Module:handle_cast(Request, State) of
                {noreply, NewState} ->
                    actor_loop(Module, NewState);
                {stop, Reason, NewState} ->
                    terminate(Module, Reason, NewState)
            end;

        %% Stop message
        {stop, {From, Ref}, Reason} ->
            From ! {Ref, ok},
            terminate(Module, Reason, State);

        %% System messages (for OTP compatibility)
        {'EXIT', _Parent, Reason} ->
            %% Handle linked process exit
            terminate(Module, Reason, State);

        %% Generic info message
        Info ->
            case Module:handle_info(Info, State) of
                {noreply, NewState} ->
                    actor_loop(Module, NewState);
                {stop, Reason, NewState} ->
                    terminate(Module, Reason, NewState)
            end
    end.

%% @private Terminate the actor process.
terminate(Module, Reason, State) ->
    case erlang:function_exported(Module, terminate, 2) of
        true ->
            try Module:terminate(Reason, State)
            catch _:_ -> ok
            end;
        false ->
            ok
    end,
    exit(Reason).
