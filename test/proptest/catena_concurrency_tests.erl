%% @doc Unit Tests for Phase 6.3: Concurrency Testing Utilities
-module(catena_concurrency_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_process.hrl").
-include("../../src/proptest/catena_gen.hrl").
-include("../../src/proptest/catena_concurrency.hrl").

%%====================================================================
%% Section 6.3.1: Deterministic Scheduling
%%====================================================================

new_schedule_creates_empty_schedule_test() ->
    Schedule = catena_concurrency:new_schedule(),
    ?assertMatch(#schedule{}, Schedule),
    ?assertEqual(0, Schedule#schedule.current),
    ok.

new_schedule_with_steps_creates_schedule_test() ->
    Step = #step{process = self(), action = test, args = []},
    Schedule = catena_concurrency:new_schedule([Step]),
    ?assertEqual(1, length(Schedule#schedule.steps)),
    ok.

schedule_step_creates_step_test() ->
    Schedule = catena_concurrency:schedule_step(self(), test_action, [arg1, arg2]),
    ?assertMatch(#schedule{steps = [#step{action = test_action}]}, Schedule),
    ok.

execute_schedule_empty_test() ->
    Pids = [self()],
    Schedule = #schedule{},
    ?assertEqual({ok, []}, catena_concurrency:execute_schedule(Pids, Schedule)),
    ok.

execute_schedule_single_step_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,

    Step = #step{process = Pid, action = is_process_alive, args = []},
    Schedule = catena_concurrency:new_schedule([Step]),

    ?assertMatch({ok, [_]}, catena_concurrency:execute_schedule([Pid], Schedule)),
    catena_process:stop_process(Proc),
    ok.

record_schedule_captures_execution_test() ->
    Fun = fun() -> 42 end,
    {Schedule, Result} = catena_concurrency:record_schedule(self(), Fun),
    ?assertMatch(#schedule{}, Schedule),
    ?assertEqual(42, Result),
    ok.

replay_schedule_executes_function_test() ->
    Fun = fun() -> 99 end,
    ?assertEqual(99, catena_concurrency:replay_schedule(#schedule{}, Fun)),
    ok.

%%====================================================================
%% Section 6.3.2: Interleaving Exploration
%%====================================================================

random_interleaving_empty_test() ->
    Result = catena_concurrency:random_interleaving(42, []),
    ?assertEqual([], Result),
    ok.

random_interleaving_single_test() ->
    Fun = fun() -> 1 end,
    Result = catena_concurrency:random_interleaving(42, [Fun]),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0),
    ok.

random_interleaving_multiple_test() ->
    Fun1 = fun() -> 1 end,
    Fun2 = fun() -> 2 end,
    Result = catena_concurrency:random_interleaving(42, [Fun1, Fun2]),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0),
    ok.

systematic_interleaving_empty_test() ->
    Result = catena_concurrency:systematic_interleaving(5, []),
    ?assertEqual([], Result),
    ok.

systematic_interleaving_single_test() ->
    Fun = fun() -> 1 end,
    Result = catena_concurrency:systematic_interleaving(5, [Fun]),
    ?assertEqual([[Fun]], Result),
    ok.

systematic_interleaving_two_test() ->
    Fun1 = fun() -> 1 end,
    Fun2 = fun() -> 2 end,
    Result = catena_concurrency:systematic_interleaving([Fun1, Fun2]),
    ?assertEqual(2, length(Result)),
    ok.

bounded_interleaving_limits_test() ->
    Fun1 = fun() -> 1 end,
    Fun2 = fun() -> 2 end,
    Fun3 = fun() -> 3 end,
    Result = catena_concurrency:bounded_interleaving(42, 2, [Fun1, Fun2, Fun3]),
    ?assert(length(Result) =< 3),
    ok.

priority_interleaving_sorts_test() ->
    Fun1 = fun() -> 1 end,
    Fun2 = fun() -> 2 end,
    Fun3 = fun() -> 3 end,
    PriorityFuns = [{1, Fun3}, {3, Fun1}, {2, Fun2}],
    Result = catena_concurrency:priority_interleaving(PriorityFuns),
    ?assertEqual(3, length(Result)),
    %% Should be sorted by priority (highest first): Fun3, Fun1, Fun2
    [First, Second, Third] = Result,
    ?assertEqual(1, First()),
    ?assertEqual(3, Second()),
    ?assertEqual(2, Third()),
    ok.

%%====================================================================
%% Section 6.3.3: Race Condition Detection
%%====================================================================

track_operation_records_operation_test() ->
    OpId = make_ref(),
    ?assertEqual(ok, catena_concurrency:track_operation(self(), read, state, OpId)),
    ok.

happens_before_returns_false_for_independent_ops_test() ->
    Op1 = #operation{id = make_ref(), process = self(), type = read, target = x, timestamp = 1},
    Op2 = #operation{id = make_ref(), process = self(), type = write, target = y, timestamp = 2},
    ?assertEqual(false, catena_concurrency:happens_before(Op1, Op2)),
    ok.

detect_race_both_alive_test() ->
    Fun1 = fun() -> receive after infinity -> ok end end,
    Fun2 = fun() -> receive after infinity -> ok end end,
    Proc1 = catena_process:spawn_test_process(Fun1),
    Proc2 = catena_process:spawn_test_process(Fun2),
    {ok, HasRace} = catena_concurrency:detect_race(Proc1#test_process.pid, Proc2#test_process.pid, shared_state),
    ?assert(HasRace),
    catena_process:stop_process(Proc1),
    catena_process:stop_process(Proc2),
    ok.

detect_race_one_dead_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Result = catena_concurrency:detect_race(Proc#test_process.pid, list_to_pid("<0.999.0>"), target),
    ?assertMatch({error, _}, Result),
    catena_process:stop_process(Proc),
    ok.

check_concurrent_access_multiple_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc1 = catena_process:spawn_test_process(Fun),
    Proc2 = catena_process:spawn_test_process(Fun),
    {ok, HasAccess} = catena_concurrency:check_concurrent_access(
        [Proc1#test_process.pid, Proc2#test_process.pid], target),
    ?assert(HasAccess),
    catena_process:stop_process(Proc1),
    catena_process:stop_process(Proc2),
    ok.

check_concurrent_access_single_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    {ok, HasAccess} = catena_concurrency:check_concurrent_access([Proc#test_process.pid], target),
    ?assertNot(HasAccess),
    catena_process:stop_process(Proc),
    ok.

%%====================================================================
%% Section 6.3.4: Deadlock Detection
%%====================================================================

track_lock_records_lock_test() ->
    Ref = make_ref(),
    ?assertEqual(ok, catena_concurrency:track_lock(self(), my_lock, Ref)),
    ok.

release_lock_releases_lock_test() ->
    ?assertEqual(ok, catena_concurrency:release_lock(self(), my_lock)),
    ok.

detect_deadlock_no_deadlock_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    {ok, HasDeadlock} = catena_concurrency:detect_deadlock([Proc#test_process.pid]),
    ?assertNot(HasDeadlock),
    catena_process:stop_process(Proc),
    ok.

detect_cycles_empty_test() ->
    LockInfo = #lock_info{},
    ?assertEqual([], catena_concurrency:detect_cycles(LockInfo)),
    ok.

%%====================================================================
%% Utility Functions Tests
%%====================================================================

run_concurrent_all_complete_test() ->
    Fun1 = fun() -> 1 end,
    Fun2 = fun() -> 2 end,
    Fun3 = fun() -> 3 end,
    {ok, Results} = catena_concurrency:run_concurrent([Fun1, Fun2, Fun3], 5000),
    ?assertEqual(3, length(Results)),
    ?assert(lists:member(1, Results)),
    ?assert(lists:member(2, Results)),
    ?assert(lists:member(3, Results)),
    ok.

run_parallel_returns_results_test() ->
    Fun1 = fun() -> 1 end,
    Fun2 = fun() -> 2 end,
    Results = catena_concurrency:run_parallel([Fun1, Fun2]),
    ?assertEqual(2, length(Results)),
    ok.

await_all_waits_for_all_test() ->
    Fun1 = fun() -> timer:sleep(100), 1 end,
    Fun2 = fun() -> timer:sleep(50), 2 end,
    Pid1 = spawn(Fun1),
    Pid2 = spawn(Fun2),
    ?assertEqual(ok, catena_concurrency:await_all([Pid1, Pid2])),
    ok.

await_any_returns_first_test() ->
    Fun1 = fun() -> timer:sleep(200), 1 end,
    Fun2 = fun() -> timer:sleep(50), 2 end,
    Pid1 = spawn(Fun1),
    Pid2 = spawn(Fun2),
    {ok, FirstPid} = catena_concurrency:await_any([Pid1, Pid2]),
    ?assertEqual(Pid2, FirstPid),
    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

concurrent_execution_test() ->
    %% Test that multiple processes can run concurrently
    CounterFun = fun(CounterPid) ->
        CounterPid ! {increment, self()},
        receive
            {result, Value} -> ok
        after infinity -> ok
        end
    end,

    %% Spawn counter process
    Counter = spawn(fun() -> counter_loop(0) end),

    %% Spawn processes that will increment the counter
    Pids = [spawn(CounterFun) || _ <- lists:seq(1, 3)],
    lists:foreach(fun(Pid) ->Pid ! Counter end, Pids),

    timer:sleep(200),
    Counter ! stop,
    receive
        {counter, _Value} -> ok
    after 500 -> ok
    end,

    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
    ok.

race_condition_example_test() ->
    %% This test demonstrates a potential race condition
    Shared = spawn(fun() -> shared_state_loop(#{}) end),

    %% Spawn two processes that try to update shared state
    UpdateFun = fun() ->
        Shared ! {update, self(), fun(S) -> maps:put(key, 1, S) end},
        receive
            {updated, _} -> ok
        after infinity -> ok
        end
    end,

    Pid1 = spawn(UpdateFun),
    Pid2 = spawn(UpdateFun),

    Shared ! {get, self()},
    receive
        {state, _State} -> ok
        after infinity -> ok
    end,

    timer:sleep(100),
    exit(Shared, kill),
    exit(Pid1, kill),
    exit(Pid2, kill),
    ok.

%%====================================================================
%% Internal helper functions
%%====================================================================

counter_loop(Count) ->
    receive
        {increment, Pid} ->
            Pid ! {result, Count + 1},
            counter_loop(Count + 1);
        stop ->
            ok;
        {get, Pid} ->
            Pid ! {state, Count},
            counter_loop(Count)
    end.

shared_state_loop(State) ->
    receive
        {update, Pid, UpdateFun} ->
            NewState = UpdateFun(State),
            Pid ! {updated, NewState},
            shared_state_loop(NewState);
        {get, Pid} ->
            Pid ! {state, State},
            shared_state_loop(State);
        stop ->
            ok
    end.
