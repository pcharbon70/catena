%%%-------------------------------------------------------------------
%%% @doc Catena Process Effect Operations (Phase 5.1.4)
%%%
%%% This module extends the Process effect with operations needed for
%%% building actors and OTP patterns. These operations map directly to
%%% BEAM primitives while providing type safety through the effect system.
%%%
%%% == Operations ==
%%%
%%% <ul>
%%%   <li><b>spawn</b> - Create a new process from a function</li>
%%%   <li><b>spawn_link</b> - Create a linked process</li>
%%%   <li><b>send (!)</b> - Send an asynchronous message</li>
%%%   <li><b>call (?)</b> - Send a synchronous message and wait for reply</li>
%%%   <li><b>link</b> - Create a bidirectional failure link</li>
%%%   <li><b>unlink</b> - Remove a failure link</li>
%%%   <li><b>monitor</b> - Create a unidirectional monitor</li>
%%%   <li><b>demonitor</b> - Remove a monitor</li>
%%%   <li><b>self</b> - Get current process PID</li>
%%%   <li><b>receive</b> - Receive a message with pattern matching</li>
%%%   <li><b>register</b> - Register a process with a name</li>
%%%   <li><b>whereis</b> - Look up a process by name</li>
%%%   <li><b>trap_exit</b> - Convert exit signals to messages</li>
%%% </ul>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_process).

%% Export list - modified to avoid receive/1,2 name conflict with reserved word
-export([
    %% Basic operations
    spawn/1,
    spawn_link/1,
    spawn_monitor/1,

    %% Messaging
    send/2,
    call/2,
    call/3,

    %% Process relationships
    link/1,
    unlink/1,
    monitor/1,
    demonitor/1,

    %% Process info
    self/0,
    whereis/1,
    register/2,
    unregister/1,

    %% Messaging - using recv instead of receive (reserved word)
    recv/2,

    %% Exit handling
    trap_exit/1,

    %% Process info
    is_process_alive/1,
    is_alive/0,
    exit/2,
    exit/1
]).

%% Types
-type message() :: term().
-type process_name() :: atom().
-type monitor_ref() :: reference().
-type result() :: term().
-spec spawn(fun(() -> result())) -> pid().
spawn(Fun) when is_function(Fun, 0) ->
    erlang:spawn(Fun).

%% @doc Spawn a linked process.
%% The new process is linked to the caller, so exit signals propagate.
-spec spawn_link(fun(() -> result())) -> pid().
spawn_link(Fun) when is_function(Fun, 0) ->
    erlang:spawn_link(Fun).

%% @doc Spawn a monitored process.
%% Returns {Pid, Ref} where Ref can be used with demonitor/1.
-spec spawn_monitor(fun(() -> result())) -> {pid(), monitor_ref()}.
spawn_monitor(Fun) when is_function(Fun, 0) ->
    erlang:spawn_monitor(Fun).

%%%=============================================================================
%%% Messaging Operations
%%%=============================================================================

%% @doc Send an asynchronous message (! operator).
%% Returns immediately without waiting for the message to be processed.
-spec send(pid(), message()) -> ok.
send(Pid, Msg) when is_pid(Pid) ->
    Pid ! Msg,
    ok;
send(Name, Msg) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined -> erlang:error({no_process, Name});
        Pid -> send(Pid, Msg)
    end.

%% @doc Send a synchronous request (? operator) and wait for reply.
%% Uses gen_server:call for timeout support and reply handling.
-spec call(pid(), message()) -> result().
call(Pid, Msg) when is_pid(Pid) ->
    call(Pid, Msg, 5000).

%% @doc Send a synchronous request (? operator) with timeout.
-spec call(pid(), message(), pos_integer()) -> result().
call(Pid, Msg, Timeout) when is_pid(Pid); is_pid(Msg), is_integer(Timeout) ->
    %% Support both Pid/Msg and Msg/Pid orders for flexibility
    ActualPid = if is_pid(Pid) -> Pid; true -> Msg end,
    ActualMsg = if is_pid(Msg) -> Pid; true -> Msg end,
    try
        gen_server:call(ActualPid, {'$gen_call', ActualMsg}, Timeout)
    catch
        exit:{noproc, _} -> erlang:error({no_process, ActualPid});
        exit:{timeout, _} -> erlang:error({call_timeout, ActualPid, Timeout});
        exit:{Reason, _} -> erlang:error({call_failed, ActualPid, Reason})
    end.

%%%=============================================================================
%%% Process Relationships
%%%=============================================================================

%% @doc Create a bidirectional link with a process.
%% If either process exits abnormally, the other receives an exit signal.
-spec link(pid()) -> true.
link(Pid) when is_pid(Pid) ->
    erlang:link(Pid).

%% @doc Remove a bidirectional link.
-spec unlink(pid()) -> true.
unlink(Pid) when is_pid(Pid) ->
    erlang:unlink(Pid).

%% @doc Monitor a process (unidirectional).
%% Returns a reference that can be used with demonitor/1.
%% When the monitored process exits, a {'DOWN', Ref, process, Pid, Reason}
%% message is sent to the monitoring process.
-spec monitor(pid()) -> monitor_ref().
monitor(Pid) when is_pid(Pid) ->
    erlang:monitor(process, Pid).

%% @doc Remove a monitor.
-spec demonitor(monitor_ref()) -> true.
demonitor(Ref) when is_reference(Ref) ->
    erlang:demonitor(Ref).

%%%=============================================================================
%%% Process Information
%%%=============================================================================

%% @doc Get the current process PID.
-spec self() -> pid().
self() ->
    erlang:self().

%% @doc Look up a process by registered name.
-spec whereis(process_name()) -> pid() | undefined.
whereis(Name) when is_atom(Name) ->
    erlang:whereis(Name).

%% @doc Register the current process with a name.
%% The name must be unique globally.
-spec register(process_name(), pid()) -> true.
register(Name, Pid) when is_atom(Name), is_pid(Pid) ->
    case erlang:register(Name, Pid) of
        true -> true;
        false -> erlang:error({name_already_registered, Name})
    end.

%% @doc Unregister a process name.
-spec unregister(process_name()) -> true.
unregister(Name) when is_atom(Name) ->
    erlang:unregister(Name).

%%%=============================================================================
%%% Receive Operations
%%%=============================================================================

%% @doc Receive a message with timeout and default.
%% Named 'recv' to avoid conflict with Erlang's 'receive' keyword.
-spec recv(non_neg_integer(), term()) -> term().
recv(TimeoutMs, Default) ->
    receive
        Msg -> Msg
    after TimeoutMs ->
        Default
    end.

%% @doc Receive a message matching a pattern.
%% This is a simplified version - full selective receive requires
%% compiler support for pattern matching.
-spec recv(fun((message()) -> boolean()), non_neg_integer(), term()) -> term().
recv(Pred, TimeoutMs, Default) ->
    Start = erlang:monotonic_time(milli_second),
    recv_loop(Pred, TimeoutMs, Start, Default).

recv_loop(_Pred, TimeoutMs, _Start, Default) when TimeoutMs =< 0 ->
    Default;
recv_loop(Pred, TimeoutMs, Start, Default) ->
    receive
        Msg ->
            case Pred(Msg) of
                true -> Msg;
                false -> recv_loop(Pred, 0, Start, Default)
            end
    after TimeoutMs ->
        Default
    end.

%%%=============================================================================
%%% Exit Handling
%%%=============================================================================

%% @doc Enable or disable trapping of exit signals.
%% When true, exit signals are converted to {'EXIT', From, Reason} messages.
-spec trap_exit(boolean()) -> boolean().
trap_exit(Flag) when is_boolean(Flag) ->
    erlang:process_flag(trap_exit, Flag).

%%%=============================================================================
%%% Process Utility Functions
%%%=============================================================================

%% @doc Check if a process is alive.
-spec is_process_alive(pid()) -> boolean().
is_process_alive(Pid) when is_pid(Pid) ->
    erlang:is_process_alive(Pid).

%% @doc Check if the current process is alive.
-spec is_alive() -> boolean().
is_alive() ->
    erlang:is_process_alive(erlang:self()).

%% @doc Exit with a reason.
-spec exit(term()) -> no_return().
exit(Reason) ->
    erlang:exit(Reason).

%% @doc Exit a specific process with a reason.
-spec exit(pid(), term()) -> true.
exit(Pid, Reason) when is_pid(Pid) ->
    erlang:exit(Pid, Reason).
