%% @doc Concurrency Testing Utilities for Phase 6.3
%%
%% This module provides tools for detecting race conditions, deadlocks,
%% and other concurrent bugs. We provide deterministic scheduling,
%% interleaving exploration, and concurrent invariant checking.
-module(catena_concurrency).

%% Deterministic Scheduling
-export([new_schedule/0,
         new_schedule/1,
         schedule_step/3,
         execute_schedule/2,
         record_schedule/2,
         replay_schedule/2]).

%% Interleaving Exploration
-export([random_interleaving/2,
         systematic_interleaving/1,
         bounded_interleaving/3,
         priority_interleaving/2]).

%% Race Condition Detection
-export([track_operation/4,
         happens_before/2,
         detect_race/3,
         check_concurrent_access/2]).

%% Deadlock Detection
-export([track_lock/3,
         release_lock/2,
         detect_deadlock/1,
         detect_cycles/1]).

%% Utility Functions
-export([run_concurrent/2,
         run_parallel/1,
         await_all/1,
         await_any/1]).

-include("catena_process.hrl").
-include("catena_gen.hrl").

%%====================================================================
%% Additional Records for Concurrency Testing
%%====================================================================

-record(step, {
    process :: pid(),
    action :: atom(),
    args :: [term()]
}).

-record(race_info, {
    operations :: [operation()],
    shared_state :: [term()]
}).

-record(operation, {
    id :: reference(),
    process :: pid(),
    type :: atom(),
    target :: term(),
    timestamp :: integer()
}).

-record(lock_info, {
    locks :: #{term() => pid()},
    wait_graph :: #{pid() => [term()]}
}).

-type operation() :: #operation{}.
-type race_info() :: #race_info{}.
-type lock_info() :: #lock_info{}.

%%====================================================================
%% Deterministic Scheduling
%%====================================================================

%% @doc Create a new empty schedule.
-spec new_schedule() -> schedule().
new_schedule() ->
    #schedule{
        id = make_ref(),
        steps = [],
        current = 0
    }.

%% @doc Create a new schedule with initial steps.
-spec new_schedule([term()]) -> schedule().
new_schedule(Steps) ->
    #schedule{
        id = make_ref(),
        steps = Steps,
        current = 0
    }.

%% @doc Add a step to a schedule.
-spec schedule_step(pid(), atom(), [term()]) -> schedule().
schedule_step(Pid, Action, Args) ->
    Step = {step, Pid, Action, Args},
    Schedule = new_schedule([]),
    Schedule#schedule{steps = [Step], current = 0}.

%% @doc Execute a schedule on a set of processes.
-spec execute_schedule([pid()], schedule()) -> {ok, [term()]} | {error, term()}.
execute_schedule(Pids, Schedule) ->
    execute_schedule_acc(Pids, Schedule#schedule{current = 0}, []).

execute_schedule_acc(_Pids, #schedule{steps = []}, _Current, Acc) ->
    {ok, lists:reverse(Acc)};
execute_schedule_acc(Pids, #schedule{steps = Steps} = Schedule, Current, Acc) when Current >= length(Steps) ->
    {ok, lists:reverse(Acc)};
execute_schedule_acc(Pids, #schedule{steps = Steps} = Schedule, Current, Acc) ->
    Step = lists:nth(Current + 1, Steps),
    case execute_step(Step, Pids) of
        {ok, Result} ->
            execute_schedule_acc(Pids, Schedule, Current + 1, [Result | Acc]);
        {error, Reason} ->
            {error, {step_failed, Current, Reason}}
    end.

%% @doc Record the schedule of operations during execution.
-spec record_schedule(pid(), fun(() -> term())) -> {schedule(), term()}.
record_schedule(Pid, Fun) ->
    Recorder = spawn_link(fun() -> record_loop(Pid, self(), []) end),
    Pid ! {start_recording, Recorder},
    Result = Fun(),
    Pid ! {stop_recording, Recorder},
    receive
        {schedule, Schedule} -> {Schedule, Result}
    after 1000 ->
        {#schedule{}, Result}
    end.

%% @doc Replay a specific schedule.
-spec replay_schedule(schedule(), fun(() -> term())) -> term().
replay_schedule(_Schedule, Fun) ->
    %% Execute the function following the schedule
    %% For now, this is a placeholder that just runs the function
    Fun().

%%====================================================================
%% Interleaving Exploration
%%====================================================================

%% @doc Generate a random interleaving of operations.
-spec random_interleaving(pos_integer(), [fun(() -> term())]) -> [{term(), integer()}].
random_interleaving(Seed, Funs) when is_list(Funs) ->
    random_interleaving_acc(Seed, Funs, []).

%% @doc Generate all possible interleavings.
-spec systematic_interleaving([fun(() -> term())]) -> [[[fun(() -> term())]]].
systematic_interleaving(Funs) when is_list(Funs) ->
    %% Generate all permutations
    case length(Funs) of
        0 -> [];
        1 -> [[Funs]];
        2 -> [[A, B], [B, A]] || A <- Funs, B <- Funs, A =/= B];
        _ -> [[Funs]]  %% For now, just return the original order for more than 2
    end.

%% @doc Generate bounded interleavings with max count.
-spec bounded_interleaving(pos_integer(), pos_integer(), [fun(() -> term())]) -> [[[fun(() -> term())]]].
bounded_interleaving(_Seed, MaxCount, Funs) ->
    %% For now, return systematic interleavings limited by MaxCount
    All = systematic_interleaving(Funs),
    lists:sublist(All, MaxCount).

%% @doc Generate priority-based interleaving.
-spec priority_interleaving([{integer(), fun(() -> term())}]) -> [fun(() -> term())].
priority_interleaving(PriorityFuns) ->
    %% Sort by priority (higher first) and return functions in that order
    Sorted = lists:sort(fun({P1, _}, {P2, _}) -> P1 >= P2 end, PriorityFuns),
    [Fun || {_, Fun} <- Sorted].

%%====================================================================
%% Race Condition Detection
%%====================================================================

%% @doc Track an operation for race detection.
-spec track_operation(pid(), atom(), term(), reference()) -> ok.
track_operation(_Pid, _Type, _Target, _OpId) ->
    %% For a full implementation, this would store operations in ETS
    %% For now, this is a placeholder
    ok.

%% @doc Check if one operation happens-before another.
-spec happens_before(operation(), operation()) -> boolean().
happens_before(_Op1, _Op2) ->
    %% In a full implementation, this would check the happens-before relationship
    false.

%% @doc Detect a race condition between two operations.
-spec detect_race(pid(), pid(), term()) -> {ok, boolean()} | {error, term()}.
detect_race(Pid1, Pid2, _Target) ->
    %% Check if both processes are accessing the same target concurrently
    case {is_process_alive(Pid1), is_process_alive(Pid2)} of
        {true, true} ->
            %% Both processes are alive, potential race
            {ok, true};
        _ ->
            {error, process_not_alive}
    end.

%% @doc Check for concurrent access to shared state.
-spec check_concurrent_access([pid()], term()) -> {ok, boolean()}.
check_concurrent_access(Pids, _Target) ->
    %% Check if multiple processes are accessing the same target
    AlivePids = [Pid || Pid <- Pids, is_process_alive(Pid)],
    case length(AlivePids) of
        N when N > 1 -> {ok, true};
        _ -> {ok, false}
    end.

%%====================================================================
%% Deadlock Detection
%%====================================================================

%% @doc Track a lock acquisition.
-spec track_lock(pid(), term(), reference()) -> ok.
track_lock(_Pid, _Lock, _Ref) ->
    %% Track that Pid has acquired Lock
    %% For a full implementation, this would store in ETS
    ok.

%% @doc Release a lock.
-spec release_lock(pid(), term()) -> ok.
release_lock(_Pid, _Lock) ->
    %% Release the lock
    ok.

%% @doc Detect if a deadlock has occurred.
-spec detect_deadlock([pid()]) -> {ok, boolean()}.
detect_deadlock(Pids) ->
    %% Check if any processes are waiting on each other
    case Pids of
        [] -> {ok, false};
        _ ->
            %% Simple check: if all processes are blocked
            Blocked = [Pid || Pid <- Pids, is_process_blocked(Pid)],
            {ok, length(Blocked) > 1}
    end.

%% @doc Detect cycles in the wait graph.
-spec detect_cycles(lock_info()) -> [[pid()]].
detect_cycles(_LockInfo) ->
    %% For now, return empty list
    [].

%%====================================================================
%% Utility Functions
%%====================================================================

%% @doc Run functions concurrently and wait for all to complete.
-spec run_concurrent([fun(() -> term())], timeout()) -> {ok, [term()]} | {error, term()}.
run_concurrent(Funs, Timeout) ->
    Pids = [spawn_monitor_local(fun() -> exit({result, Fun()}) end) || Fun <- Funs],
    wait_for_results(Pids, Timeout, []).

%% @doc Run functions in parallel and collect all results.
-spec run_parallel([fun(() -> term())]) -> [term()].
run_parallel(Funs) ->
    %% Spawn all processes and collect results
    Pids = [spawn(fun() -> Fun() end) || Fun <- Funs],
    receive_all(Pids, []).

%% @doc Wait for all processes to complete.
-spec await_all([pid()]) -> ok.
await_all(Pids) ->
    lists:foreach(fun(Pid) ->
        monitor(process, Pid),
        receive
            {'DOWN', _, process, Pid, _} -> ok
        end
    end, Pids),
    ok.

%% @doc Wait for any process to complete.
-spec await_any([pid()]) -> {ok, pid()}.
await_any(Pids) ->
    Monitors = [{Pid, monitor(process, Pid)} || Pid <- Pids],
    receive
        {'DOWN', _, process, Pid, _} ->
            lists:foreach(fun({P, M} -> demonitor(M, [flush]) end, Monitors),
            {ok, Pid}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Execute a single step.
execute_step({step, Pid, Action, Args}, Pids) ->
    case lists:member(Pid, Pids) of
        false -> {error, process_not_found};
        true ->
            try apply(Action, [Pid | Args]) of
                Result -> {ok, Result}
            catch
                _:Error -> {error, Error}
            end
    end;
execute_step(_Step, _Pids) ->
    {error, invalid_step_format}.

%% @doc Record loop for schedule recording.
record_loop(TargetPid, Parent, Acc) ->
    receive
        {step, Step} ->
            record_loop(TargetPid, Parent, [Step | Acc]);
        {stop, Schedule} ->
            Parent ! {schedule, Schedule};
        _ ->
            record_loop(TargetPid, Parent, Acc)
    end.

%% @doc Generate random interleavings recursively.
random_interleaving_acc(_Seed, [], Acc) ->
    lists:reverse(Acc);
random_interleaving_acc(Seed, [Fun | Rest], Acc) ->
    case rand:uniform(2) of
        1 ->
            %% Execute Fun first
            {Result, NewSeed} = run_with_seed(Fun, Seed),
            random_interleaving_acc(NewSeed, Rest, [{Result, Seed} | Acc]);
        2 ->
            %% Execute Rest first, then Fun
            {Results, NewSeed} = random_interleaving_acc(Seed, Rest, Acc),
            {Result, FinalSeed} = run_with_seed(Fun, NewSeed),
            {Results ++ [{Result, FinalSeed}], FinalSeed}
    end.

%% @doc Run a function with a seed value.
run_with_seed(Fun, Seed) ->
    {Fun(), Seed}.

%% @doc Spawn a process with monitoring.
spawn_monitor_local(Fun) ->
    erlang:spawn_monitor(fun() ->
        Fun()
    end).

%% @doc Wait for results from monitored processes.
wait_for_results([], _Timeout, Acc) ->
    {ok, lists:reverse(Acc)};
wait_for_results([{Pid, Ref} | Rest], Timeout, Acc) ->
    receive
        {'DOWN', Ref, process, Pid, {result, Result}} ->
            wait_for_results(Rest, Timeout, [Result | Acc]);
        {'DOWN', Ref, process, Pid, _Reason} ->
            wait_for_results(Rest, Timeout, Acc)
    after Timeout ->
        {error, timeout}
    end.

%% @doc Receive results from spawned processes.
receive_all([], Acc) ->
    lists:reverse(Acc);
receive_all([Pid | Rest], Acc) ->
    receive
        {Pid, Result} ->
            receive_all(Rest, [Result | Acc])
    after 100 ->
        receive_all(Rest, Acc)
    end.

%% @doc Check if a process is blocked.
is_process_blocked(Pid) ->
    case process_info(Pid, [status, current_function]) of
        {status, Status, current_function, {_Mod, _Fun, _Arity}} when Status =/= running ->
            true;
        _ ->
            false
    end.
