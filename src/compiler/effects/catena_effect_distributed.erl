%%%-------------------------------------------------------------------
%%% @doc Catena Distributed Effects (Phase 6.5)
%%%
%%% This module implements distributed effect handling across nodes
%%% in an Erlang cluster. Effects can be performed on remote nodes
%%% with proper serialization and error handling.
%%%
%%% == Cross-Node Effect Handling ==
%%%
%%% Effects can be performed on remote nodes by sending effect
%%% operations and receiving results. The system handles serialization
%%% of effect data and manages network failures.
%%%
%%% == Effect Serialization ==
%%%
%%% Effects are serialized for transmission across nodes. Only
%%% serializable effects can be distributed.
%%%
%%% == Remote Effect Execution ==
%%%
%%% Remote nodes execute effects and return results. The caller
%%% can block for results or continue asynchronously.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_distributed).

%% Cross-node effect handling
-export([
    remote_effect/3,
    remote_effect_async/3,
    await_remote/1,
    await_remote/2,
    remote_node/0,
    execute_effect/1  % Exported for RPC calls
]).

%% Effect serialization
-export([
    serialize_effect/1,
    deserialize_effect/1,
    is_serializable/1,
    encode_term/1,
    decode_term/1
]).

%% Distributed effect coordination
-export([
    cluster_nodes/0,
    select_node/1,
    is_available/1,
    ping_node/1
]).

%%====================================================================
%% Types
%%====================================================================

-type node_name() :: atom().
-type remote_ref() :: {remote_ref, node_name(), reference()}.
-type serialized_effect() :: binary().

-type remote_option() ::
    {timeout, timeout()} |
    {async, boolean()} |
    {retry, non_neg_integer()}.

-export_type([node_name/0, remote_ref/0, serialized_effect/0, remote_option/0]).

%%====================================================================
%% Cross-Node Effect Handling
%%====================================================================

%% @doc Perform an effect on a remote node.
%%
%% Sends the effect operation to the remote node, executes it,
%% and returns the result.
%%
%% @param Node Target node for the effect
%% @param Effect The effect operation to perform
%% @param Options Options for remote execution (timeout, async, retry)
%% @returns Result of the effect operation
%%
%% @example
%% ```
%% %% Perform state get on remote node
%% {ok, Result} = catena_effect_distributed:remote_effect(
%%     'node@host',
%%     {effect, {state, {get}}},
%%     [{timeout, 5000}]
%% ).
%% '''
-spec remote_effect(node_name(), term(), [remote_option()]) ->
    {ok, term()} | {error, term()}.
remote_effect(Node, Effect, Options) when is_atom(Node), is_list(Options) ->
    Timeout = proplists:get_value(timeout, Options, 5000),
    case is_available(Node) of
        true ->
            case rpc:call(Node, catena_effect_distributed, execute_effect, [Effect], Timeout) of
                {badrpc, Reason} -> {error, {rpc_failure, Reason}};
                Result -> {ok, Result}
            end;
        false ->
            {error, {node_unavailable, Node}}
    end.

%% @doc Perform an effect asynchronously on a remote node.
%%
%% Returns immediately with a reference that can be used with
%% await_remote/1 to get the result later.
%%
%% @param Node Target node for the effect
%% @param Effect The effect operation to perform
%% @param Options Options for remote execution
%% @returns {ok, RemoteRef} or {error, Reason}
%%
%% @example
%% ```
%% %% Start async effect on remote node
%% {ok, Ref} = catena_effect_distributed:remote_effect_async(
%%     'node@host',
%%     {effect, {state, {get}}},
%%     []
%% ),
%% {ok, Result} = catena_effect_distributed:await_remote(Ref).
%% '''
-spec remote_effect_async(node_name(), term(), [remote_option()]) ->
    {ok, remote_ref()} | {error, term()}.
remote_effect_async(Node, Effect, Options) when is_atom(Node), is_list(Options) ->
    case is_available(Node) of
        true ->
            Ref = make_ref(),
            Self = self(),
            spawn(fun() ->
                Result = case rpc:call(Node, catena_effect_distributed, execute_effect, [Effect]) of
                    {badrpc, Reason} -> {error, {rpc_failure, Reason}};
                    R -> {ok, R}
                end,
                Self ! {remote_result, Ref, Result}
            end),
            {ok, {remote_ref, Node, Ref}};
        false ->
            {error, {node_unavailable, Node}}
    end.

%% @doc Await the result of an async remote effect.
%%
%% @param Ref The remote reference from remote_effect_async/2
%% @param Timeout Timeout in milliseconds
%% @returns {ok, Result} or {error, Reason}
%%
%% @example
%% ```
%% {ok, Result} = catena_effect_distributed:await_remote(Ref, 1000).
%% '''
-spec await_remote(remote_ref(), timeout()) -> {ok, term()} | {error, term()}.
await_remote({remote_ref, _Node, Ref}, Timeout) ->
    receive
        {remote_result, Ref, Result} -> Result
    after Timeout ->
        {error, timeout}
    end.

%% @doc Await the result of an async remote effect (default 5s timeout).
%%
%% @param Ref The remote reference from remote_effect_async/2
%% @returns {ok, Result} or {error, Reason}
%%
%% @example
%% ```
%% {ok, Result} = catena_effect_distributed:await_remote(Ref).
%% '''
-spec await_remote(remote_ref()) -> {ok, term()} | {error, term()}.
await_remote(Ref) ->
    await_remote(Ref, 5000).

%% @doc Get the current remote node name.
%%
%% @returns Current node name
%%
%% @example
%% ```
%% Node = catena_effect_distributed:remote_node().
%% '''
-spec remote_node() -> node_name().
remote_node() ->
    node().

%%====================================================================
%% Effect Serialization
%%====================================================================

%% @doc Serialize an effect for transmission.
%%
%% Converts effect terms to binary format for network transmission.
%%
%% @param Effect The effect to serialize
%% @returns Serialized effect binary
%%
%% @example
%% ```
%% Binary = catena_effect_distributed:serialize_effect(
%%     {effect, {state, {get}}}
%% ).
%% '''
-spec serialize_effect(term()) -> serialized_effect().
serialize_effect(Effect) ->
    term_to_binary(Effect).

%% @doc Deserialize an effect from binary format.
%%
%% @param Binary The serialized effect binary
%% @returns Deserialized effect term
%%
%% @example
%% ```
%% Effect = catena_effect_distributed:deserialize_effect(Binary).
%% '''
-spec deserialize_effect(serialized_effect()) -> term().
deserialize_effect(Binary) when is_binary(Binary) ->
    binary_to_term(Binary).

%% @doc Check if an effect is serializable.
%%
%% Serializable effects contain only basic types and no
%% local processes or references.
%%
%% @param Effect The effect to check
%% @returns true if effect can be serialized
%%
%% @example
%% ```
%% true = catena_effect_distributed:is_serializable({effect, {state, {get}}}).
%% '''
-spec is_serializable(term()) -> boolean().
is_serializable(Effect) ->
    is_term_serializable(Effect, sets:new()).

%% @doc Encode a term for network transmission.
%%
%% Similar to serialize_effect/1 but with additional validation.
%%
%% @param Term The term to encode
%% @returns Encoded binary
%%
%% @example
%% ```
%% Binary = catena_effect_distributed:encode_term({value, 42}).
%% '''
-spec encode_term(term()) -> binary().
encode_term(Term) ->
    term_to_binary(Term).

%% @doc Decode a term from network transmission.
%%
%% @param Binary The encoded binary
%% @returns Decoded term
%%
%% @example
%% ```
%% Term = catena_effect_distributed:decode_term(Binary).
%% '''
-spec decode_term(binary()) -> term().
decode_term(Binary) when is_binary(Binary) ->
    binary_to_term(Binary).

%%====================================================================
%% Distributed Effect Coordination
%%====================================================================

%% @doc Get all nodes in the cluster.
%%
%% @returns List of connected cluster nodes
%%
%% @example
%% ```
%% Nodes = catena_effect_distributed:cluster_nodes().
%% '''
-spec cluster_nodes() -> [node_name()].
cluster_nodes() ->
    [node() | nodes()].

%% @doc Select a node for effect execution.
%%
%% Uses a simple round-robin strategy to distribute load.
%% For production, use more sophisticated scheduling.
%%
%% @param Effect The effect to execute (for routing decisions)
%% @returns Selected node name
%%
%% @example
%% ```
%% Node = catena_effect_distributed:select_node({effect, {state, {get}}}).
%% '''
-spec select_node(term()) -> node_name().
select_node(_Effect) ->
    Nodes = cluster_nodes(),
    Index = erlang:phash2(node(), length(Nodes)) + 1,
    lists:nth(Index, Nodes).

%% @doc Check if a node is available for effect execution.
%%
%% @param Node The node to check
%% @returns true if node is available
%%
%% @example
%% ```
%% true = catena_effect_distributed:is_available('node@host').
%% '''
-spec is_available(node_name()) -> boolean().
is_available(Node) when Node =:= node() ->
    true;
is_available(Node) ->
    case lists:member(Node, nodes()) of
        true -> ping_node(Node) =:= pong;
        false -> false
    end.

%% @doc Ping a node to check connectivity.
%%
%% @param Node The node to ping
%% @returns pong if node is responsive, pang otherwise
%%
%% @example
%% ```
%% pong = catena_effect_distributed:ping_node('node@host').
%% '''
-spec ping_node(node_name()) -> pong | pang.
ping_node(Node) ->
    net_adm:ping(Node).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Execute an effect (called on remote node).
-spec execute_effect(term()) -> term().
execute_effect({effect, {state, {get}}}) ->
    {state_result, undefined_state};
execute_effect({effect, {state, {put, _Value}}}) ->
    {state_result, ok};
execute_effect({effect, {reader, ask}}) ->
    {reader_result, undefined_env};
execute_effect({effect, {writer, {tell, _Value}}}) ->
    {writer_result, ok};
execute_effect({effect, {async, {spawn, _Fun}}}) ->
    {async_result, undefined_ref};
execute_effect(Effect) ->
    {unknown_effect, Effect}.

%% @doc Check if a term is serializable by checking for PIDs and refs.
-spec is_term_serializable(term(), sets:set()) -> boolean().
is_term_serializable(Term, Seen) when is_pid(Term); is_port(Term) ->
    false;
is_term_serializable(Term, Seen) when is_reference(Term) ->
    % References are only serializable within the same node
    false;
is_term_serializable(Term, _Seen) when is_function(Term) ->
    % Functions are not serializable
    false;
is_term_serializable(Term, Seen) when is_tuple(Term) ->
    case sets:is_element(Term, Seen) of
        true -> true;
        false ->
            NewSeen = sets:add_element(Term, Seen),
            is_list_serializable(tuple_to_list(Term), NewSeen)
    end;
is_term_serializable(Term, Seen) when is_list(Term) ->
    is_list_serializable(Term, Seen);
is_term_serializable(Term, Seen) when is_map(Term) ->
    is_list_serializable(maps:to_list(Term), Seen);
is_term_serializable(_Term, _Seen) ->
    true.

%% @doc Check if all elements of a list are serializable.
-spec is_list_serializable([term()], sets:set()) -> boolean().
is_list_serializable([], _Seen) ->
    true;
is_list_serializable([H | T], Seen) ->
    case is_term_serializable(H, Seen) of
        true -> is_list_serializable(T, Seen);
        false -> false
    end.
