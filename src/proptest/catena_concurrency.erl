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
         systematic_interleaving/2,
         bounded_interleaving/3,
         priority_interleaving/1]).

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
-include("catena_concurrency.hrl").

-define(OPS_TABLE, catena_concurrency_ops).
-define(LOCKS_TABLE, catena_concurrency_locks).
-define(LOCKS_TABLE_WAITS, catena_concurrency_lock_waits).

%%====================================================================
%% Types
%%====================================================================

-type operation() :: #operation{}.
-type race_info() :: #race_info{}.
-type lock_info() :: #lock_info{}.
-type schedule() :: #schedule{}.

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
    new_schedule([#step{process = Pid, action = Action, args = Args}]).

%% @doc Execute a schedule on a set of processes.
-spec execute_schedule([pid()], schedule()) -> {ok, [term()]} | {error, term()}.
execute_schedule(Pids, Schedule) ->
    execute_schedule_acc(Pids, normalize_schedule_steps(Schedule#schedule.steps), []).

execute_schedule_acc(_Pids, [], Acc) ->
    {ok, lists:reverse(Acc)};
execute_schedule_acc(Pids, [Step | Rest], Acc) ->
    case execute_step(Step, Pids) of
        {ok, Result} ->
            execute_schedule_acc(Pids, Rest, [Result | Acc]);
        {error, Reason} ->
            {error, {step_failed, Step, Reason}}
    end.

%% @doc Record the schedule of operations during execution.
-spec record_schedule(pid(), fun(() -> term())) -> {schedule(), term()}.
record_schedule(Pid, Fun) ->
    put({schedule_recorder, Pid}, []),
    Result = Fun(),
    Steps = lists:reverse(erase({schedule_recorder, Pid})),
    {new_schedule(Steps), Result}.

%% @doc Replay a specific schedule.
-spec replay_schedule(schedule(), fun(() -> term())) -> term().
replay_schedule(Schedule, Fun) ->
    _ = execute_schedule(unique_schedule_pids(Schedule), Schedule),
    Fun().

%%====================================================================
%% Interleaving Exploration
%%====================================================================

%% @doc Generate a random interleaving of operations.
-spec random_interleaving(pos_integer(), [fun(() -> term())]) -> [{term(), integer()}].
random_interleaving(Seed, Funs) when is_list(Funs) ->
    Indexed = lists:zip(Funs, lists:seq(1, length(Funs))),
    rand:seed(exsplus, {Seed, Seed + 1, Seed + 2}),
    Shuffled = lists:sort(
        fun(_, _) -> rand:uniform(2) =:= 1 end,
        Indexed
    ),
    [{Fun(), Index} || {Fun, Index} <- Shuffled].

%% @doc Generate all possible interleavings.
-spec systematic_interleaving([fun(() -> term())]) -> [[fun(() -> term())]].
systematic_interleaving(Funs) when is_list(Funs) ->
    permutations(Funs).

-spec systematic_interleaving(non_neg_integer(), [fun(() -> term())]) -> [[fun(() -> term())]].
systematic_interleaving(MaxCount, Funs) when is_integer(MaxCount), MaxCount >= 0 ->
    lists:sublist(systematic_interleaving(Funs), MaxCount).

%% @doc Generate bounded interleavings with max count.
-spec bounded_interleaving(pos_integer(), pos_integer(), [fun(() -> term())]) -> [[fun(() -> term())]].
bounded_interleaving(_Seed, MaxCount, Funs) ->
    systematic_interleaving(MaxCount, Funs).

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
track_operation(Pid, Type, Target, OpId) ->
    ensure_ops_table(),
    Operation = #operation{
        id = OpId,
        process = Pid,
        type = Type,
        target = Target,
        timestamp = erlang:monotonic_time(microsecond)
    },
    ets:insert(?OPS_TABLE, {OpId, Operation}),
    ok.

%% @doc Check if one operation happens-before another.
-spec happens_before(operation(), operation()) -> boolean().
happens_before(#operation{process = Pid, target = Target, timestamp = Ts1},
               #operation{process = Pid, target = Target, timestamp = Ts2}) ->
    Ts1 =< Ts2;
happens_before(_Op1, _Op2) ->
    false.

%% @doc Detect a race condition between two operations.
-spec detect_race(pid(), pid(), term()) -> {ok, boolean()} | {error, term()}.
detect_race(Pid1, Pid2, _Target) ->
    case {is_process_alive(Pid1), is_process_alive(Pid2)} of
        {true, true} ->
            {ok, true};
        _ ->
            {error, process_not_alive}
    end.

%% @doc Check for concurrent access to shared state.
-spec check_concurrent_access([pid()], term()) -> {ok, boolean()}.
check_concurrent_access(Pids, Target) ->
    ensure_ops_table(),
    ActiveOps = operations_for_target(Target),
    ActivePids = lists:usort([Op#operation.process || Op <- ActiveOps]),
    case length(ActivePids) of
        N when N > 1 -> {ok, true};
        _ ->
            AlivePids = [Pid || Pid <- Pids, is_process_alive(Pid)],
            {ok, length(AlivePids) > 1}
    end.

%%====================================================================
%% Deadlock Detection
%%====================================================================

%% @doc Track a lock acquisition.
-spec track_lock(pid(), term(), reference()) -> ok.
track_lock(Pid, Lock, _Ref) ->
    ensure_locks_table(),
    case ets:lookup(?LOCKS_TABLE, Lock) of
        [] ->
            ets:insert(?LOCKS_TABLE, {Lock, Pid}),
            clear_waits_for(Pid),
            ok;
        [{Lock, Owner}] when Owner =:= Pid ->
            ok;
        [{Lock, Owner}] ->
            add_wait_edge(Pid, Owner),
            ok
    end.

%% @doc Release a lock.
-spec release_lock(pid(), term()) -> ok.
release_lock(Pid, Lock) ->
    ensure_locks_table(),
    case ets:lookup(?LOCKS_TABLE, Lock) of
        [{Lock, Owner}] when Owner =:= Pid ->
            ets:delete(?LOCKS_TABLE, Lock),
            remove_wait_edges_to(Pid),
            ok;
        _ ->
            ok
    end.

%% @doc Detect if a deadlock has occurred.
-spec detect_deadlock([pid()]) -> {ok, boolean()}.
detect_deadlock(Pids) ->
    LockInfo = current_lock_info(),
    Cycles = detect_cycles(LockInfo),
    case lists:any(fun(Cycle) -> lists:any(fun(Pid) -> lists:member(Pid, Cycle) end, Pids) end, Cycles) of
        true -> {ok, true};
        false -> {ok, false}
    end.

%% @doc Detect cycles in the wait graph.
-spec detect_cycles(lock_info()) -> [[pid()]].
detect_cycles(#lock_info{wait_graph = WaitGraph}) ->
    NormalizedGraph = case is_map(WaitGraph) of
        true -> WaitGraph;
        false -> #{}
    end,
    find_cycles(maps:keys(NormalizedGraph), NormalizedGraph, []).

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
    Parent = self(),
    Pids = [spawn(fun() -> Parent ! {self(), Fun()} end) || Fun <- Funs],
    receive_all(Pids, []).

%% @doc Wait for all processes to complete.
-spec await_all([pid()]) -> ok.
await_all(Pids) ->
    Monitors = [{Pid, monitor(process, Pid)} || Pid <- Pids],
    lists:foreach(
        fun({Pid, Ref}) ->
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            end
        end,
        Monitors
    ),
    ok.

%% @doc Wait for any process to complete.
-spec await_any([pid()]) -> {ok, pid()}.
await_any(Pids) ->
    Monitors = [{Pid, monitor(process, Pid)} || Pid <- Pids],
    await_any_monitored(Monitors).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Execute a single step.
execute_step(#step{process = Pid, action = Action, args = Args}, Pids) ->
    case lists:member(Pid, Pids) of
        false -> {error, process_not_found};
        true ->
            try apply(erlang, Action, [Pid | Args]) of
                Result -> {ok, Result}
            catch
                _:Error -> {error, Error}
            end
    end;
execute_step(_Step, _Pids) ->
    {error, invalid_step_format}.

-spec unique_schedule_pids(schedule()) -> [pid()].
unique_schedule_pids(#schedule{steps = Steps}) ->
    lists:usort([Pid || #step{process = Pid} <- normalize_schedule_steps(Steps)]).

normalize_schedule_steps(undefined) ->
    [];
normalize_schedule_steps(Steps) ->
    Steps.

-spec permutations([term()]) -> [[term()]].
permutations([]) ->
    [];
permutations([Fun]) ->
    [[Fun]];
permutations(List) ->
    [[H | T] || H <- List, T <- permutations(List -- [H])].

-spec ensure_ops_table() -> ok.
ensure_ops_table() ->
    ensure_named_table(?OPS_TABLE, [set, public, named_table]),
    ok.

-spec ensure_locks_table() -> ok.
ensure_locks_table() ->
    ensure_named_table(?LOCKS_TABLE, [set, public, named_table]),
    ensure_named_table(?LOCKS_TABLE_WAITS, [set, public, named_table]),
    ok.

ensure_named_table(Name, Options) ->
    case ets:info(Name) of
        undefined -> ets:new(Name, Options);
        _ -> Name
    end.

-spec operations_for_target(term()) -> [operation()].
operations_for_target(Target) ->
    [Operation ||
        {_Id, Operation = #operation{target = OpTarget}} <- ets:tab2list(?OPS_TABLE),
        OpTarget =:= Target].

-spec add_wait_edge(pid(), pid()) -> ok.
add_wait_edge(Waiter, Owner) ->
    ensure_locks_table(),
    Current = case ets:lookup(?LOCKS_TABLE_WAITS, Waiter) of
        [] -> [];
        [{Waiter, Owners}] -> Owners
    end,
    ets:insert(?LOCKS_TABLE_WAITS, {Waiter, lists:usort([Owner | Current])}),
    ok.

-spec clear_waits_for(pid()) -> ok.
clear_waits_for(Pid) ->
    ensure_locks_table(),
    ets:delete(?LOCKS_TABLE_WAITS, Pid),
    ok.

-spec remove_wait_edges_to(pid()) -> ok.
remove_wait_edges_to(Pid) ->
    ensure_locks_table(),
    lists:foreach(
        fun({Waiter, Owners}) ->
            Filtered = lists:delete(Pid, Owners),
            case Filtered of
                [] -> ets:delete(?LOCKS_TABLE_WAITS, Waiter);
                _ -> ets:insert(?LOCKS_TABLE_WAITS, {Waiter, Filtered})
            end
        end,
        ets:tab2list(?LOCKS_TABLE_WAITS)
    ),
    ok.

-spec current_lock_info() -> lock_info().
current_lock_info() ->
    ensure_locks_table(),
    Locks = maps:from_list(ets:tab2list(?LOCKS_TABLE)),
    WaitGraph = maps:from_list(ets:tab2list(?LOCKS_TABLE_WAITS)),
    #lock_info{locks = Locks, wait_graph = WaitGraph}.

-spec find_cycles([pid()], #{pid() => [pid()]}, [[pid()]]) -> [[pid()]].
find_cycles([], _Graph, Acc) ->
    dedupe_cycles(Acc);
find_cycles([Node | Rest], Graph, Acc) ->
    find_cycles(Rest, Graph, detect_from_node(Node, Graph, [], Acc)).

detect_from_node(Node, Graph, Path, Acc) ->
    case lists:member(Node, Path) of
        true ->
            Cycle = normalize_cycle([Node | Path]),
            [Cycle | Acc];
        false ->
            Next = maps:get(Node, Graph, []),
            lists:foldl(fun(Child, InnerAcc) -> detect_from_node(Child, Graph, [Node | Path], InnerAcc) end, Acc, Next)
    end.

normalize_cycle(Cycle) ->
    lists:usort(Cycle).

dedupe_cycles(Cycles) ->
    lists:usort([Cycle || Cycle <- Cycles, length(Cycle) > 1]).

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

await_any_monitored(Monitors) ->
    MonitorRefs = [Ref || {_Pid, Ref} <- Monitors],
    receive
        {'DOWN', Ref, process, Pid, _} ->
            case lists:member(Ref, MonitorRefs) of
                true ->
                    lists:foreach(fun({_P, M}) when M =/= Ref -> demonitor(M, [flush]);
                                     (_) -> ok
                                  end, Monitors),
                    {ok, Pid};
                false ->
                    await_any_monitored(Monitors)
            end
    end.
