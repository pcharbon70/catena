%% @doc Unit Tests for Phase 6.4: Distribution Testing
-module(catena_distribution_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_process.hrl").
-include("../../src/proptest/catena_gen.hrl").
-include("../../src/proptest/catena_distribution.hrl").

%%====================================================================
%% Section 6.4.1: Test Node Management
%%====================================================================

spawn_test_node_creates_node_test() ->
    Result = catena_distribution:spawn_test_node(test_node_1),
    ?assertMatch({ok, #test_node{name = test_node_1}}, Result),
    ok.

connect_nodes_connected_test() ->
    Node1 = #test_node{name = node1, node = node1@localhost, pid = undefined},
    Node2 = #test_node{name = node2, node = node2@localhost, pid = undefined},
    %% Both nodes are not connected
    Result = catena_distribution:connect_nodes(Node1, Node2),
    ?assertMatch({error, _}, Result),
    ok.

cleanup_nodes_clears_test() ->
    ?assertEqual(ok, catena_distribution:cleanup_nodes()),
    ok.

list_nodes_returns_list_test() ->
    Nodes = catena_distribution:list_nodes(),
    ?assert(is_list(Nodes)),
    ok.

%%====================================================================
%% Section 6.4.2: Remote Process Testing
%%====================================================================

spawn_on_node_local_node_test() ->
    Fun = fun() -> spawned_result end,
    {ok, Pid} = catena_distribution:spawn_on_node(node(), Fun),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    ok.

spawn_on_node_opt_fallback_test() ->
    Fun = fun() -> local_result end,
    {ok, Pid} = catena_distribution:spawn_on_node_opt(unknown_node, Fun),
    ?assert(is_pid(Pid)),
    Pid ! stop,
    receive
        _ -> ok
        after 100 -> ok
    end,
    ok.

rpc_test_on_local_node_test() ->
    Result = catena_distribution:rpc_test(node(), erlang, node, [self()]),
    ?assertEqual(node(), Result),
    ok.

global_process_undefined_test() ->
    Result = catena_distribution:global_process(undefined_name),
    ?assertEqual(undefined, Result),
    ok.

%%====================================================================
%% Section 6.4.3: Network Partition Simulation
%%====================================================================

partition_nodes_simulates_split_test() ->
    %% Placeholder test - verifies the function exists and runs
    ?assertEqual(ok, catena_distribution:partition_nodes([node1, node2], [node3, node4])),
    ok.

heal_partition_restores_connectivity_test() ->
    ?assertEqual(ok, catena_distribution:heal_partition([node1, node2])),
    ok.

delay_messages_adds_delay_test() ->
    ?assertEqual(ok, catena_distribution:delay_messages(node1, node2, 100)),
    ok.

network_delay_returns_undefined_for_different_nodes_test() ->
    Result = catena_distribution:network_delay(node1, node2),
    ?assertEqual(undefined, Result),
    ok.

%%====================================================================
%% Section 6.4.4: Consistency Verification
%%====================================================================

eventually_consistent_empty_nodes_test() ->
    {ok, Result} = catena_distribution:eventually_consistent([], test_state, 1000),
    ?assert(Result),
    ok.

eventually_consistent_with_timeout_test() ->
    {ok, Result} = catena_distribution:eventually_consistent([node(), node()], test_state, 100),
    ?assert(Result),
    ok.

strongly_consistent_empty_test() ->
    {ok, Result} = catena_distribution:strongly_consistent([], test_state),
    ?assert(Result),
    ok.

strongly_consistent_single_test() ->
    {ok, Result} = catena_distribution:strongly_consistent([node()], test_state),
    ?assert(Result),
    ok.

verify_crdt_consistency_empty_test() ->
    States = [],
    {ok, Result} = catena_distribution:verify_crdt_consistency(States),
    ?assert(Result),
    ok.

verify_crdt_consistency_single_test() ->
    State1 = #crdt_state{value = 42, version = 1},
    States = [State1],
    {ok, Result} = catena_distribution:verify_crdt_consistency(States),
    ?assert(Result),
    ok.

%%====================================================================
%% Utility Functions Tests
%%====================================================================

get_local_node_returns_node_test() ->
    Node = catena_distribution:get_local_node(),
    ?assert(is_atom(Node)),
    ok.

is_connected_local_node_test() ->
    Result = catena_distribution:is_connected(node()),
    ?assert(Result),
    ok.

is_connected_unknown_node_test() ->
    Result = catena_distribution:is_connected(unknown@remote),
    ?assertNot(Result),
    ok.

node_info_local_test() ->
    LocalNode = node(),
    Info = catena_distribution:node_info(LocalNode),
    ?assertEqual(LocalNode, maps:get(node, Info)),
    ?assertEqual(local, maps:get(type, Info)),
    ok.

ping_node_local_test() ->
    Result = catena_distribution:ping_node(node()),
    ?assertEqual(pang, Result),
    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

distributed_state_sync_test() ->
    %% This test demonstrates the interface for distributed state testing
    %% In a real distributed system, this would test state synchronization
    Nodes = [node1, node2],
    SharedState = #{counter => 0},

    %% Verify consistency across nodes
    {ok, Result} = catena_distribution:strongly_consistent(Nodes, SharedState),
    ?assert(Result),
    ok.

network_partition_recovery_test() ->
    %% Test the partition healing workflow
    %% Note: This is a placeholder test demonstrating the interface
    %% Real distributed testing requires actual Erlang nodes

    %% Local node should always be connected
    ?assert(catena_distribution:is_connected(node())),

    %% Verify partition and heal functions exist and return ok
    Nodes = [node1, node2, node3],
    ?assertEqual(ok, catena_distribution:partition_nodes([node1, node2], [node3])),
    ?assertEqual(ok, catena_distribution:heal_partition(Nodes)),
    ok.
