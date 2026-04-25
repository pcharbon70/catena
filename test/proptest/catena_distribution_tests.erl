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
    {ok, Node} = catena_distribution:spawn_test_node(test_node_1),
    ?assertMatch(#test_node{name = test_node_1}, Node),
    ?assert(is_pid(Node#test_node.pid)),
    ?assert(is_process_alive(Node#test_node.pid)),
    ?assert(lists:member(Node#test_node.node, catena_distribution:list_nodes())),
    catena_distribution:cleanup_nodes(),
    ok.

connect_nodes_connected_test() ->
    {ok, Node1} = catena_distribution:spawn_test_node(node1),
    {ok, Node2} = catena_distribution:spawn_test_node(node2),
    ?assertEqual(ok, catena_distribution:connect_nodes(Node1, Node2)),
    catena_distribution:cleanup_nodes(),
    ok.

cleanup_nodes_clears_test() ->
    {ok, _Node} = catena_distribution:spawn_test_node(cleanup_node),
    ?assertEqual(ok, catena_distribution:cleanup_nodes()),
    ?assertEqual([], catena_distribution:list_nodes()),
    ok.

list_nodes_returns_list_test() ->
    {ok, Node} = catena_distribution:spawn_test_node(list_node),
    Nodes = catena_distribution:list_nodes(),
    ?assert(is_list(Nodes)),
    ?assert(lists:member(Node#test_node.node, Nodes)),
    catena_distribution:cleanup_nodes(),
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
    Nodes = [make_node(node1), make_node(node2)],
    ?assertEqual(ok, catena_distribution:partition_nodes([hd(Nodes)], [lists:nth(2, Nodes)])),
    {ok, false} = catena_distribution:strongly_consistent(Nodes, test_state),
    catena_distribution:cleanup_nodes(),
    ok.

heal_partition_restores_connectivity_test() ->
    Node1 = make_node(node1),
    Node2 = make_node(node2),
    ok = catena_distribution:partition_nodes([Node1], [Node2]),
    ok = catena_distribution:heal_partition([Node1, Node2]),
    {ok, true} = catena_distribution:strongly_consistent([Node1, Node2], test_state),
    catena_distribution:cleanup_nodes(),
    ok.

delay_messages_adds_delay_test() ->
    Node1 = make_node(node1),
    Node2 = make_node(node2),
    ?assertEqual(ok, catena_distribution:delay_messages(Node1, Node2, 100)),
    ?assertEqual(100, catena_distribution:network_delay(Node1, Node2)),
    catena_distribution:cleanup_nodes(),
    ok.

network_delay_returns_undefined_for_different_nodes_test() ->
    Result = catena_distribution:network_delay(make_node(node1), make_node(node2)),
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
    Node1 = make_node(node1),
    Node2 = make_node(node2),
    ok = catena_distribution:partition_nodes([Node1], [Node2]),
    spawn(fun() ->
        timer:sleep(50),
        catena_distribution:heal_partition([Node1, Node2])
    end),
    {ok, Result} = catena_distribution:eventually_consistent([Node1, Node2], test_state, 200),
    ?assert(Result),
    catena_distribution:cleanup_nodes(),
    ok.

strongly_consistent_empty_test() ->
    {ok, Result} = catena_distribution:strongly_consistent([], test_state),
    ?assert(Result),
    ok.

strongly_consistent_single_test() ->
    {ok, _Node} = catena_distribution:spawn_test_node(single_node),
    {ok, Result} = catena_distribution:strongly_consistent([make_node(single_node)], test_state),
    ?assert(Result),
    catena_distribution:cleanup_nodes(),
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

verify_crdt_consistency_detects_divergence_test() ->
    States = [
        #crdt_state{value = 42, version = 1},
        #crdt_state{value = 7, version = 2}
    ],
    {ok, Result} = catena_distribution:verify_crdt_consistency(States),
    ?assertNot(Result),
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
    Nodes = [make_node(node1), make_node(node2)],
    SharedState = #{counter => 0},

    %% Verify consistency across nodes
    {ok, Result} = catena_distribution:strongly_consistent(Nodes, SharedState),
    ?assert(Result),
    catena_distribution:cleanup_nodes(),
    ok.

network_partition_recovery_test() ->
    %% Test the partition healing workflow
    %% Note: This is a placeholder test demonstrating the interface
    %% Real distributed testing requires actual Erlang nodes

    %% Local node should always be connected
    ?assert(catena_distribution:is_connected(node())),

    %% Verify partition and heal functions exist and return ok
    Nodes = [make_node(node1), make_node(node2), make_node(node3)],
    ?assertEqual(ok, catena_distribution:partition_nodes([lists:nth(1, Nodes), lists:nth(2, Nodes)], [lists:nth(3, Nodes)])),
    ?assertEqual(ok, catena_distribution:heal_partition(Nodes)),
    catena_distribution:cleanup_nodes(),
    ok.

make_node(Name) ->
    {ok, Node} = catena_distribution:spawn_test_node(Name),
    Node#test_node.node.
