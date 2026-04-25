%% @doc Distribution Testing for Phase 6.4
%%
%% This module provides a deterministic local emulation layer for distributed
%% BEAM testing. Real multi-node execution can still use Erlang distribution
%% when available, but the default path keeps tests reproducible in a single VM.
-module(catena_distribution).

%% Test Node Management
-export([spawn_test_node/1,
         connect_nodes/2,
         with_nodes/2,
         cleanup_nodes/0,
         list_nodes/0]).

%% Remote Process Testing
-export([spawn_on_node/2,
         rpc_test/4,
         global_process/1,
         spawn_on_node_opt/2]).

%% Network Partition Simulation
-export([partition_nodes/2,
         heal_partition/1,
         delay_messages/3,
         network_delay/2]).

%% Consistency Verification
-export([eventually_consistent/3,
         strongly_consistent/2,
         verify_crdt_consistency/1]).

%% Utility Functions
-export([get_local_node/0,
         is_connected/1,
         node_info/1,
         ping_node/1]).

-include("catena_process.hrl").
-include("catena_gen.hrl").

-define(NODES_TABLE, catena_distribution_nodes).
-define(CONNECTIONS_TABLE, catena_distribution_connections).
-define(DELAYS_TABLE, catena_distribution_delays).

%%====================================================================
%% Records
%%====================================================================

-record(test_node, {
    name :: atom() | binary(),
    node :: node(),
    pid :: pid() | undefined,
    cleanup :: fun(() -> ok) | undefined
}).

-record(network_partition, {
    nodes :: [node()],
    healed :: boolean()
}).

-record(crdt_state, {
    value :: term(),
    version :: integer()
}).

%%====================================================================
%% Test Node Management
%%====================================================================

%% @doc Spawn a locally emulated test node.
-spec spawn_test_node(atom()) -> {ok, #test_node{}} | {error, term()}.
spawn_test_node(Name) when not is_atom(Name) ->
    {error, {invalid_node_name, Name}};
spawn_test_node(Name) ->
    ensure_tables(),
    NodeName = make_node_name(Name),
    case ets:lookup(?NODES_TABLE, NodeName) of
        [{NodeName, TestNode}] ->
            {ok, TestNode};
        [] ->
            Controller = spawn(fun() -> node_controller(Name, #{}) end),
            TestNode = #test_node{
                name = Name,
                node = NodeName,
                pid = Controller,
                cleanup = fun() -> stop_test_node(Name) end
            },
            ets:insert(?NODES_TABLE, {NodeName, TestNode}),
            ets:insert(?CONNECTIONS_TABLE, {{NodeName, NodeName}, connected}),
            {ok, TestNode}
    end.

%% @doc Connect two emulated nodes.
-spec connect_nodes(#test_node{}, #test_node{}) -> ok | {error, term()}.
connect_nodes(#test_node{node = Node1}, #test_node{node = Node2}) ->
    ensure_tables(),
    case {known_node(Node1), known_node(Node2)} of
        {true, true} ->
            mark_connection(Node1, Node2, connected),
            ok;
        _ ->
            {error, nodes_not_available}
    end.

%% @doc Execute a function with test nodes, cleaning up after.
-spec with_nodes([atom()], fun(([#test_node{}]) -> term())) -> term().
with_nodes(NodeNames, Fun) ->
    TestNodes = [begin {ok, Node} = spawn_test_node(Name), Node end || Name <- NodeNames],
    try
        Fun(TestNodes)
    after
        lists:foreach(fun(#test_node{cleanup = Cleanup}) -> Cleanup() end, TestNodes)
    end.

%% @doc Clean up all test nodes and related network metadata.
-spec cleanup_nodes() -> ok.
cleanup_nodes() ->
    ensure_tables(),
    Nodes = [Node || {_NodeName, Node} <- ets:tab2list(?NODES_TABLE)],
    lists:foreach(fun(#test_node{cleanup = Cleanup}) -> Cleanup() end, Nodes),
    ets:delete_all_objects(?CONNECTIONS_TABLE),
    ets:delete_all_objects(?DELAYS_TABLE),
    ok.

%% @doc List all known test node names.
-spec list_nodes() -> [atom()].
list_nodes() ->
    ensure_tables(),
    lists:sort([NodeName || {NodeName, _Node} <- ets:tab2list(?NODES_TABLE)]).

%%====================================================================
%% Remote Process Testing
%%====================================================================

%% @doc Spawn a process on a specific node.
-spec spawn_on_node(node(), fun(() -> term())) -> {ok, pid()} | {error, term()}.
spawn_on_node(Node, Fun) when Node =:= node() ->
    {ok, spawn(Fun)};
spawn_on_node(Node, Fun) ->
    ensure_tables(),
    case known_node(Node) andalso is_reachable(node(), Node) of
        true ->
            maybe_delay(node(), Node),
            {ok, spawn(Fun)};
        false ->
            {error, node_not_available}
    end.

%% @doc Spawn a process on a node with fallback to local.
-spec spawn_on_node_opt(node(), fun(() -> term())) -> {ok, pid()}.
spawn_on_node_opt(Node, Fun) ->
    case spawn_on_node(Node, Fun) of
        {ok, Pid} -> {ok, Pid};
        {error, _Reason} -> {ok, spawn(Fun)}
    end.

%% @doc Execute an RPC-style test on a remote node.
-spec rpc_test(node(), module(), atom(), [term()]) -> term() | {error, term()}.
rpc_test(Node, Module, Function, Args) when Node =:= node() ->
    apply(Module, Function, Args);
rpc_test(Node, Module, Function, Args) ->
    ensure_tables(),
    case known_node(Node) andalso is_reachable(node(), Node) of
        true ->
            maybe_delay(node(), Node),
            try apply(Module, Function, Args) of
                Result -> Result
            catch
                _:Error -> {error, Error}
            end;
        false ->
            {error, node_not_available}
    end.

%% @doc Get a globally registered process.
-spec global_process(atom()) -> pid() | undefined.
global_process(Name) ->
    global:whereis_name(Name).

%%====================================================================
%% Network Partition Simulation
%%====================================================================

%% @doc Partition nodes to simulate network splits.
-spec partition_nodes([node()], [node()]) -> ok.
partition_nodes(Nodes1, Nodes2) ->
    ensure_tables(),
    lists:foreach(
        fun(Node1) ->
            lists:foreach(fun(Node2) -> mark_connection(Node1, Node2, partitioned) end, Nodes2)
        end,
        Nodes1
    ),
    ok.

%% @doc Heal a network partition across a node set.
-spec heal_partition([node()]) -> ok.
heal_partition(Nodes) ->
    ensure_tables(),
    Pairs = [{A, B} || A <- Nodes, B <- Nodes, A =/= B],
    lists:foreach(fun({Node1, Node2}) -> clear_connection(Node1, Node2) end, Pairs),
    ok.

%% @doc Simulate network delay between nodes.
-spec delay_messages(node(), node(), pos_integer()) -> ok.
delay_messages(From, To, DelayMs) ->
    ensure_tables(),
    ets:insert(?DELAYS_TABLE, {{From, To}, DelayMs}),
    ok.

%% @doc Get the configured network delay between two nodes.
-spec network_delay(node(), node()) -> pos_integer() | undefined.
network_delay(From, To) ->
    ensure_tables(),
    case ets:lookup(?DELAYS_TABLE, {From, To}) of
        [{{From, To}, DelayMs}] -> DelayMs;
        [] -> undefined
    end.

%%====================================================================
%% Consistency Verification
%%====================================================================

%% @doc Check if the node set becomes consistent before timeout.
-spec eventually_consistent([node()], term(), timeout()) -> {ok, boolean()} | {error, term()}.
eventually_consistent(Nodes, ExpectedState, Timeout) ->
    Deadline = deadline(Timeout),
    eventually_consistent_until(normalize_nodes(Nodes), ExpectedState, Deadline).

%% @doc Check if a node set is strongly consistent right now.
-spec strongly_consistent([node()], term()) -> {ok, boolean()} | {error, term()}.
strongly_consistent(Nodes, _ExpectedState) ->
    Normalized = normalize_nodes(Nodes),
    {ok, not has_partition(Normalized)}.

%% @doc Verify CRDT convergence by comparing final values.
-spec verify_crdt_consistency([#crdt_state{}]) -> {ok, boolean()}.
verify_crdt_consistency([]) ->
    {ok, true};
verify_crdt_consistency([#crdt_state{value = Value} | Rest]) ->
    {ok, lists:all(fun(#crdt_state{value = Other}) -> Other =:= Value end, Rest)}.

%%====================================================================
%% Utility Functions
%%====================================================================

%% @doc Get the local node name.
-spec get_local_node() -> node().
get_local_node() ->
    node().

%% @doc Check if a node is considered connected.
-spec is_connected(node()) -> boolean().
is_connected(Node) when Node =:= node() ->
    true;
is_connected(Node) ->
    ensure_tables(),
    known_node(Node).

%% @doc Get information about a node.
-spec node_info(node()) -> map().
node_info(Node) when Node =:= node() ->
    #{node => Node, type => local, status => up};
node_info(Node) ->
    ensure_tables(),
    case ets:lookup(?NODES_TABLE, Node) of
        [{Node, #test_node{name = Name, pid = Pid}}] ->
            #{node => Node, type => emulated, status => up, name => Name, controller => Pid};
        [] ->
            #{node => Node, type => remote, status => down}
    end.

%% @doc Ping a node.
-spec ping_node(node()) -> pang | pong | {error, term()}.
ping_node(Node) when Node =:= node() ->
    net_adm:ping(Node);
ping_node(Node) ->
    case known_node(Node) of
        true -> pong;
        false -> pang
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec stop_test_node(atom()) -> ok.
stop_test_node(Name) ->
    ensure_tables(),
    NodeName = make_node_name(Name),
    case ets:lookup(?NODES_TABLE, NodeName) of
        [{NodeName, #test_node{pid = Controller}}] when is_pid(Controller) ->
            exit(Controller, shutdown),
            ets:delete(?NODES_TABLE, NodeName),
            clear_node_metadata(NodeName),
            ok;
        [{NodeName, _TestNode}] ->
            ets:delete(?NODES_TABLE, NodeName),
            clear_node_metadata(NodeName),
            ok;
        [] ->
            ok
    end.

ensure_tables() ->
    ensure_named_table(?NODES_TABLE),
    ensure_named_table(?CONNECTIONS_TABLE),
    ensure_named_table(?DELAYS_TABLE),
    ok.

ensure_named_table(Name) ->
    case ets:info(Name) of
        undefined -> ets:new(Name, [set, public, named_table]);
        _ -> Name
    end.

make_node_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_node@localhost").

known_node(Node) ->
    case ets:lookup(?NODES_TABLE, Node) of
        [{Node, _}] -> true;
        [] -> false
    end.

mark_connection(Node1, Node2, Status) ->
    ets:insert(?CONNECTIONS_TABLE, {{Node1, Node2}, Status}),
    ets:insert(?CONNECTIONS_TABLE, {{Node2, Node1}, Status}),
    ok.

clear_connection(Node1, Node2) ->
    ets:delete(?CONNECTIONS_TABLE, {Node1, Node2}),
    ets:delete(?CONNECTIONS_TABLE, {Node2, Node1}),
    ok.

clear_node_metadata(Node) ->
    lists:foreach(
        fun({{From, To}, _}) when From =:= Node; To =:= Node ->
                ets:delete(?CONNECTIONS_TABLE, {From, To});
           (_) ->
                ok
        end,
        ets:tab2list(?CONNECTIONS_TABLE)
    ),
    lists:foreach(
        fun({{From, To}, _}) when From =:= Node; To =:= Node ->
                ets:delete(?DELAYS_TABLE, {From, To});
           (_) ->
                ok
        end,
        ets:tab2list(?DELAYS_TABLE)
    ).

is_reachable(From, To) ->
    case ets:lookup(?CONNECTIONS_TABLE, {From, To}) of
        [{{From, To}, partitioned}] -> false;
        _ -> true
    end.

maybe_delay(From, To) ->
    case network_delay(From, To) of
        undefined -> ok;
        DelayMs -> timer:sleep(DelayMs)
    end.

normalize_nodes(Nodes) ->
    lists:usort(Nodes).

has_partition([]) ->
    false;
has_partition([_Single]) ->
    false;
has_partition(Nodes) ->
    lists:any(
        fun({Node1, Node2}) ->
            case ets:lookup(?CONNECTIONS_TABLE, {Node1, Node2}) of
                [{{Node1, Node2}, partitioned}] -> true;
                _ -> false
            end
        end,
        [{A, B} || A <- Nodes, B <- Nodes, A =/= B]
    ).

deadline(infinity) ->
    infinity;
deadline(Timeout) ->
    erlang:monotonic_time(millisecond) + Timeout.

eventually_consistent_until(Nodes, ExpectedState, Deadline) ->
    case strongly_consistent(Nodes, ExpectedState) of
        {ok, true} ->
            {ok, true};
        _ when Deadline =:= infinity ->
            timer:sleep(10),
            eventually_consistent_until(Nodes, ExpectedState, Deadline);
        _ ->
            case Deadline - erlang:monotonic_time(millisecond) =< 0 of
                true -> {ok, false};
                false ->
                    timer:sleep(10),
                    eventually_consistent_until(Nodes, ExpectedState, Deadline)
            end
    end.

node_controller(_Name, State) ->
    receive
        stop ->
            ok;
        _Other ->
            node_controller(_Name, State)
    end.
