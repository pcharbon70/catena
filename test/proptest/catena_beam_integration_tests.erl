%% @doc Integration Tests for Phase 6: BEAM Integration
%%
%% These tests verify that all BEAM integration features work correctly
%% in realistic scenarios, combining process testing, message passing,
%% concurrency utilities, distribution testing, and OTP patterns.
-module(catena_beam_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_process.hrl").
-include("../../src/proptest/catena_distribution.hrl").
-include("../../src/proptest/catena_gen.hrl").
-include("../../src/proptest/catena_otp.hrl").

%%====================================================================
%% Section 6.6.1: Complete GenServer Testing Workflow
%%====================================================================

genserver_workflow_test() ->
    Proc = catena_process:spawn_test_process(fun() -> counter_server(0) end),
    ServerPid = Proc#test_process.pid,
    ?assertEqual(ok, catena_process:assert_alive(Proc)),

    %% Initial state
    ServerPid ! {get, self()},
    receive
        {count, 0} -> ok
    after 500 -> ?assert(false, timeout_waiting_for_count_0)
    end,

    %% Increment
    ServerPid ! {increment, self()},
    ServerPid ! {get, self()},
    receive
        {count, 1} -> ok
    after 500 -> ?assert(false, timeout_waiting_for_count_1)
    end,

    %% Multiple increments
    ServerPid ! {increment, self()},
    ServerPid ! {increment, self()},
    ServerPid ! {get, self()},
    receive
        {count, 3} -> ok
    after 500 -> ?assert(false, timeout_waiting_for_count_3)
    end,

    catena_process:stop_process(Proc),
    ?assertEqual(ok, catena_process:assert_dead(ServerPid)),
    ok.

%%====================================================================
%% Section 6.6.2: Supervisor with Crashing Children
%%====================================================================

supervisor_crash_workflow_test() ->
    %% Test supervisor behavior with crashing children
    %% 1. Create a supervisor with children
    %% 2. Verify children can be added
    %% 3. Verify cleanup works

    %% Create a simple supervisor
    SupPid = spawn(fun() -> simple_supervisor(#{}) end),

    %% Add children
    Child1 = spawn(fun() -> child_loop(child1) end),
    Child2 = spawn(fun() -> child_loop(child2) end),

    SupPid ! {add_child, child1, Child1},
    SupPid ! {add_child, child2, Child2},

    %% Verify children are registered
    SupPid ! {get_children, self()},
    receive
        {children, Children} ->
            ?assertEqual(2, length(Children))
    after 500 -> ?assert(false, timeout_waiting_for_children)
    end,

    %% Verify child is still alive
    ?assert(is_process_alive(Child2)),

    %% Cleanup
    exit(Child2, kill),
    exit(SupPid, kill),
    ok.

%%====================================================================
%% Section 6.6.3: Message Protocol Between Processes
%%====================================================================

message_protocol_test() ->
    ProtocolProc = catena_process:spawn_test_process(fun protocol_loop/0),
    Pid = ProtocolProc#test_process.pid,
    {ok, _Tracer} = catena_message:start_trace(Pid),
    ?assert(catena_message:sends_message(Pid, ping, 200)),
    ?assert(catena_message:sends_messages_in_order(Pid, [msg1, msg2], 200)),
    {ok, Trace} = catena_message:get_trace(),
    ?assert(length(Trace) > 0),
    ?assertEqual({ok, true},
        catena_message:follows_protocol([ping, msg1, msg2], [ping, {repeat, msg1}, msg2], #{allow_extra => false, ordered => true})),
    ?assertEqual(ok, catena_message:stop_trace()),
    catena_process:stop_process(ProtocolProc),
    ok.

%%====================================================================
%% Section 6.6.4: Concurrent Access to Shared GenServer
%%====================================================================

concurrent_genserver_access_test() ->
    %% Test concurrent access to a shared server
    %% 1. Create a shared server
    %% 2. Spawn multiple concurrent clients
    %% 3. Verify all operations complete correctly
    %% 4. Verify final state is consistent

    %% Start a shared counter
    ServerPid = spawn(fun() -> counter_server(0) end),

    %% Spawn multiple clients
    NumClients = 10,
    IncrementsPerClient = 5,
    Results = catena_concurrency:run_parallel(
        [fun() -> client_loop(ServerPid, IncrementsPerClient) end
         || _ <- lists:seq(1, NumClients)]
    ),
    ?assertEqual(NumClients, length(Results)),

    %% Check final state
    ServerPid ! {get, self()},
    ExpectedCount = NumClients * IncrementsPerClient,
    receive
        {count, FinalCount} ->
            ?assertEqual(ExpectedCount, FinalCount)
    after 500 -> ?assert(false, timeout_waiting_for_final_count)
    end,

    ServerPid ! stop,
    ok.

%%====================================================================
%% Section 6.6.5: Race Condition Detection
%%====================================================================

race_condition_detection_test() ->
    %% Test race condition detection
    %% 1. Create a scenario with potential race condition
    %% 2. Use concurrency testing utilities to detect
    %% 3. Verify detection works

    %% Create shared state with potential race
    SharedPid = spawn(fun() -> shared_state_loop(0) end),
    ok = catena_concurrency:track_operation(self(), write, shared_state, make_ref()),

    %% Spawn multiple updaters while tracking concurrent writes
    Updaters = [spawn(fun() ->
                    catena_concurrency:track_operation(self(), write, shared_state, make_ref()),
                    update_shared(SharedPid, 1)
                end)
                || _ <- lists:seq(1, 5)],

    %% Wait for updates
    timer:sleep(100),

    %% Check final value
    SharedPid ! {get, self()},
    receive
        {value, Value} ->
            ?assert(Value >= 0)
    after 500 -> ?assert(false, timeout_waiting_for_value)
    end,

    ?assertEqual({ok, true}, catena_concurrency:check_concurrent_access(Updaters, shared_state)),

    %% Cleanup
    lists:foreach(fun(P) -> exit(P, kill) end, Updaters),
    exit(SharedPid, kill),
    ok.

distribution_partition_workflow_test() ->
    {ok, Node1} = catena_distribution:spawn_test_node(integration_node_1),
    {ok, Node2} = catena_distribution:spawn_test_node(integration_node_2),
    ok = catena_distribution:partition_nodes([Node1#test_node.node], [Node2#test_node.node]),
    ?assertEqual({ok, false},
        catena_distribution:strongly_consistent([Node1#test_node.node, Node2#test_node.node], test_state)),
    ok = catena_distribution:heal_partition([Node1#test_node.node, Node2#test_node.node]),
    ?assertEqual({ok, true},
        catena_distribution:eventually_consistent([Node1#test_node.node, Node2#test_node.node], test_state, 100)),
    catena_distribution:cleanup_nodes(),
    ok.

%%====================================================================
%% Section 6.6.6: Cleanup Handling
%%====================================================================

cleanup_normal_scenario_test() ->
    Proc = catena_process:spawn_test_process(fun() -> simple_loop() end),
    ?assertEqual(ok, catena_process:assert_alive(Proc)),
    ?assertEqual(ok, catena_process:stop_process(Proc)),
    ?assertEqual(ok, catena_process:assert_dead(Proc)),
    ok.

cleanup_crash_scenario_test() ->
    %% Test cleanup after crash
    Pid = spawn(fun() -> simple_loop() end),
    ?assert(is_process_alive(Pid)),
    exit(Pid, crash),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)),
    ok.

cleanup_timeout_scenario_test() ->
    %% Test cleanup after timeout
    Pid = spawn(fun() -> simple_loop() end),
    ?assert(is_process_alive(Pid)),
    exit(Pid, timeout),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)),
    ok.

cleanup_multiple_processes_test() ->
    %% Test cleanup of multiple processes
    Pids = [spawn(fun() -> simple_loop() end) || _ <- lists:seq(1, 5)],
    lists:foreach(fun(P) -> ?assert(is_process_alive(P)) end, Pids),
    lists:foreach(fun(P) -> exit(P, kill) end, Pids),
    timer:sleep(10),
    lists:foreach(fun(P) -> ?assertNot(is_process_alive(P)) end, Pids),
    ok.

%%====================================================================
%% Section 6.6.7: Performance Tests
%%====================================================================

performance_concurrent_test_test() ->
    %% Test that concurrent tests complete in reasonable time
    StartTime = erlang:monotonic_time(millisecond),

    %% Run 100 concurrent operations
    ServerPid = spawn(fun() -> counter_server(0) end),
    Pids = [spawn(fun() -> client_loop(ServerPid, 10) end)
            || _ <- lists:seq(1, 100)],

    wait_for_clients(Pids, 10000),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    %% Should complete within 5 seconds
    ?assert(Duration < 5000),

    ServerPid ! stop,
    ok.

performance_message_passing_test() ->
    %% Test message passing performance
    StartTime = erlang:monotonic_time(millisecond),

    %% Send 1000 messages
    Pid = spawn(fun() -> echo_loop() end),
    [Pid ! {ping, self()} || _ <- lists:seq(1, 1000)],
    receive_messages(1000, []),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    %% Should complete within 2 seconds
    ?assert(Duration < 2000),

    exit(Pid, kill),
    ok.

%%====================================================================
%% Section 6.6.8: Integration Scenarios
%%====================================================================

producer_consumer_pipeline_test() ->
    %% Test a complete producer-consumer pipeline
    %% Producer -> Buffer -> Consumer

    %% Create buffer
    BufferPid = spawn(fun() -> buffer_loop([]) end),

    %% Create consumer
    ConsumerPid = spawn(fun() -> consumer_loop_2(BufferPid) end),

    %% Create producer
    ProducerPid = spawn(fun() -> producer_loop_2(BufferPid, 10) end),

    %% Wait for pipeline to complete
    timer:sleep(500),

    %% Check buffer is empty (all consumed)
    BufferPid ! {get_count, self()},
    receive
        {count, Count} ->
            ?assertEqual(0, Count)
    after 500 -> ?assert(false, timeout_waiting_for_count)
    end,

    %% Cleanup
    exit(ProducerPid, kill),
    exit(ConsumerPid, kill),
    exit(BufferPid, kill),
    ok.

state_transition_test() ->
    %% Test state machine transitions
    StatePid = spawn(fun() -> simple_statem(disconnected) end),

    %% Initial state
    StatePid ! {get_state, self()},
    receive
        {state, disconnected} -> ok
    after 500 -> ?assert(false, timeout_waiting_for_disconnected)
    end,

    %% Transition to connected
    StatePid ! connect,
    StatePid ! {get_state, self()},
    receive
        {state, connected} -> ok
    after 500 -> ?assert(false, timeout_waiting_for_connected)
    end,

    %% Transition to authenticated
    StatePid ! authenticate,
    StatePid ! {get_state, self()},
    receive
        {state, authenticated} -> ok
    after 500 -> ?assert(false, timeout_waiting_for_authenticated)
    end,

    %% Disconnect
    StatePid ! disconnect,
    StatePid ! {get_state, self()},
    receive
        {state, disconnected} -> ok
    after 500 -> ?assert(false, timeout_waiting_for_disconnected_again)
    end,

    %% Cleanup
    exit(StatePid, kill),
    ok.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% Simple counter server
counter_server(Count) ->
    receive
        {get, Pid} ->
            Pid ! {count, Count},
            counter_server(Count);
        {increment, Pid} ->
            Pid ! {incremented, Count + 1},
            counter_server(Count + 1);
        stop ->
            ok
    end.

%% Simple child loop
child_loop(Name) ->
    receive
        stop -> ok;
        _ -> child_loop(Name)
    end.

%% Simple supervisor
simple_supervisor(Children) ->
    receive
        {add_child, Name, Pid} ->
            simple_supervisor(maps:put(Name, Pid, Children));
        {get_children, Pid} ->
            Pid ! {children, maps:values(Children)},
            simple_supervisor(Children);
        stop ->
            ok
    end.

%% Client loop for concurrent access
client_loop(_ServerPid, 0) ->
    ok;
client_loop(ServerPid, Count) ->
    ServerPid ! {increment, self()},
    client_loop(ServerPid, Count - 1).

%% Wait for all clients to finish
wait_for_clients([], _Timeout) ->
    ok;
wait_for_clients(Pids, Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_clients_loop(Pids, StartTime, Timeout).

wait_for_clients_loop([], _, _) ->
    ok;
wait_for_clients_loop(Pids, StartTime, Timeout) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    if CurrentTime - StartTime > Timeout ->
            timeout;
       true ->
            receive
            after 50 ->
                    AlivePids = [P || P <- Pids, is_process_alive(P)],
                    wait_for_clients_loop(AlivePids, StartTime, Timeout)
            end
    end.

%% Simple loop
simple_loop() ->
    receive
        stop -> ok;
        _ -> simple_loop()
    end.

%% Shared state loop
shared_state_loop(Value) ->
    receive
        {update, Delta} ->
            shared_state_loop(Value + Delta);
        {get, Pid} ->
            Pid ! {value, Value},
            shared_state_loop(Value);
        stop ->
            ok
    end.

%% Update shared state
update_shared(Pid, Delta) ->
    Pid ! {update, Delta}.

%% Protocol loop used by message utilities
protocol_loop() ->
    receive
        {From, Ref, ping} ->
            From ! {self(), Ref, pong},
            protocol_loop();
        {From, Ref, msg1} ->
            From ! {self(), Ref, ok},
            protocol_loop();
        {From, Ref, msg2} ->
            From ! {self(), Ref, ok},
            protocol_loop();
        stop ->
            ok
    end.

%% Echo loop
echo_loop() ->
    receive
        {ping, Pid} ->
            Pid ! pong,
            echo_loop();
        stop ->
            ok
    end.

%% Receive messages
receive_messages(0, Acc) ->
    lists:reverse(Acc);
receive_messages(Count, Acc) ->
    receive
        pong ->
            receive_messages(Count - 1, [pong | Acc])
    after 1000 ->
        lists:reverse(Acc)
    end.

%% Buffer loop
buffer_loop(Messages) ->
    receive
        {put, Msg} ->
            buffer_loop([Msg | Messages]);
        {get, Pid} ->
            case Messages of
                [] ->
                    Pid ! empty,
                    buffer_loop([]);
                [Msg | Rest] ->
                    Pid ! {message, Msg},
                    buffer_loop(Rest)
            end;
        {get_count, Pid} ->
            Pid ! {count, length(Messages)},
            buffer_loop(Messages);
        stop ->
            ok
    end.

%% Consumer loop 2
consumer_loop_2(BufferPid) ->
    BufferPid ! {get, self()},
    receive
        stop ->
            ok;
        {message, _Msg} ->
            consumer_loop_2(BufferPid);
        empty ->
            timer:sleep(10),
            consumer_loop_2(BufferPid)
    end.

%% Producer loop 2
producer_loop_2(BufferPid, Count) when Count > 0 ->
    BufferPid ! {put, Count},
    producer_loop_2(BufferPid, Count - 1);
producer_loop_2(_, 0) ->
    ok.

%% Simple state machine
simple_statem(State) ->
    receive
        {get_state, Pid} ->
            Pid ! {state, State},
            simple_statem(State);
        connect when State =:= disconnected ->
            simple_statem(connected);
        authenticate when State =:= connected ->
            simple_statem(authenticated);
        disconnect ->
            simple_statem(disconnected);
        _ ->
            simple_statem(State)
    end.
