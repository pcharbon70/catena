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
    recv/3,

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
-spec send(pid() | process_name(), message()) -> ok.
send(Pid, Msg) when is_pid(Pid) ->
    Pid ! Msg,
    ok;
send(Name, Msg) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined -> erlang:error({no_process, Name});
        Pid -> send(Pid, Msg)
    end.

%% @doc Send a synchronous request (? operator) and wait for reply.
%% Uses the native OTP gen_server protocol for timeout support and replies.
-spec call(pid(), message()) -> result().
call(Pid, Msg) when is_pid(Pid) ->
    call(Pid, Msg, 5000).

%% @doc Send a synchronous request (? operator) with timeout.
-spec call(pid(), message(), timeout()) -> result().
call(Pid, Msg, Timeout)
        when is_pid(Pid),
             ((is_integer(Timeout) andalso Timeout >= 0) orelse
              Timeout =:= infinity) ->
    try
        gen_server:call(Pid, Msg, Timeout)
    catch
        exit:{noproc, _} -> erlang:error({no_process, Pid});
        exit:{timeout, _} -> erlang:error({call_timeout, Pid, Timeout});
        exit:Reason -> erlang:error({call_failed, Pid, Reason})
    end.

%%%=============================================================================
%%% Process Relationships
%%%=============================================================================

%% @doc Create a bidirectional link with a process.
%% If either process exits abnormally, the other receives an exit signal.
-spec link(pid()) -> ok.
link(Pid) when is_pid(Pid) ->
    _ = erlang:link(Pid),
    ok.

%% @doc Remove a bidirectional link.
-spec unlink(pid()) -> ok.
unlink(Pid) when is_pid(Pid) ->
    _ = erlang:unlink(Pid),
    ok.

%% @doc Monitor a process (unidirectional).
%% Returns a reference that can be used with demonitor/1.
%% When the monitored process exits, a {'DOWN', Ref, process, Pid, Reason}
%% message is sent to the monitoring process.
-spec monitor(pid()) -> monitor_ref().
monitor(Pid) when is_pid(Pid) ->
    erlang:monitor(process, Pid).

%% @doc Remove a monitor and flush an already-delivered DOWN message.
-spec demonitor(monitor_ref()) -> ok.
demonitor(Ref) when is_reference(Ref) ->
    _ = erlang:demonitor(Ref, [flush]),
    ok.

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

%% @doc Register a process with a name.
%% The name must be unique globally.
-spec register(process_name(), pid()) -> ok.
register(Name, Pid) when is_atom(Name), is_pid(Pid) ->
    case erlang:whereis(Name) of
        undefined ->
            try erlang:register(Name, Pid) of
                true -> ok
            catch
                error:badarg ->
                    case erlang:whereis(Name) of
                        undefined ->
                            erlang:error({registration_failed, Name, Pid});
                        _ExistingPid ->
                            erlang:error({name_already_registered, Name})
                    end
            end;
        _ExistingPid ->
            erlang:error({name_already_registered, Name})
    end.

%% @doc Unregister a process name. Missing names are already unregistered.
-spec unregister(process_name()) -> ok.
unregister(Name) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            ok;
        _Pid ->
            try erlang:unregister(Name) of
                true -> ok
            catch
                error:badarg -> ok
            end
    end.

%%%=============================================================================
%%% Receive Operations
%%%=============================================================================

%% @doc Receive a message with timeout and default.
%% Named 'recv' to avoid conflict with Erlang's 'receive' keyword.
-spec recv(non_neg_integer(), term()) -> term().
recv(TimeoutMs, Default) when is_integer(TimeoutMs), TimeoutMs >= 0 ->
    receive
        Msg -> Msg
    after TimeoutMs ->
        Default
    end.

%% @doc Receive the first message accepted by a dynamic predicate.
%% Messages rejected while searching are restored before this call returns.
%% Source-level selective receive still requires compiler pattern support.
-spec recv(fun((message()) -> boolean()), non_neg_integer(), term()) -> term().
recv(Pred, TimeoutMs, Default)
        when is_function(Pred, 1), is_integer(TimeoutMs), TimeoutMs >= 0 ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    recv_loop(Pred, Deadline, Default, []).

recv_loop(Pred, Deadline, Default, Rejected) ->
    Remaining = max(Deadline - erlang:monotonic_time(millisecond), 0),
    receive
        Msg ->
            case apply_receive_predicate(Pred, Msg, Rejected) of
                true ->
                    restore_messages(Rejected),
                    Msg;
                false ->
                    recv_loop(Pred, Deadline, Default, [Msg | Rejected])
            end
    after Remaining ->
        restore_messages(Rejected),
        Default
    end.

apply_receive_predicate(Pred, Msg, Rejected) ->
    try Pred(Msg) of
        true ->
            true;
        false ->
            false;
        Other ->
            erlang:error({invalid_receive_predicate_result, Other})
    catch
        Class:Reason:Stacktrace ->
            restore_messages([Msg | Rejected]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

restore_messages(Rejected) ->
    lists:foreach(
        fun(Message) -> erlang:self() ! Message end,
        lists:reverse(Rejected)
    ),
    ok.

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
-spec exit(pid(), term()) -> ok.
exit(Pid, Reason) when is_pid(Pid) ->
    _ = erlang:exit(Pid, Reason),
    ok.
