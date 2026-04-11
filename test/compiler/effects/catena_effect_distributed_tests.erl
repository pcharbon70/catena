%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_effect_distributed (Phase 6.5)
%%%
%%% Tests for distributed effect handling including:
%%% - Cross-node effect execution (sync and async)
%%% - Effect serialization and deserialization
%%% - Cluster coordination and node selection
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_distributed_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Cross-Node Effect Handling Tests
%%%=============================================================================

remote_effect_on_current_node_test() ->
    % Test remote effect on the current node
    Effect = {effect, {state, {get}}},
    Result = catena_effect_distributed:remote_effect(node(), Effect, []),
    ?assertMatch({ok, {state_result, _}}, Result).

remote_effect_with_timeout_test() ->
    % Test remote effect with timeout option
    Effect = {effect, {state, {get}}},
    Result = catena_effect_distributed:remote_effect(node(), Effect, [{timeout, 1000}]),
    ?assertMatch({ok, {state_result, _}}, Result).

remote_effect_unavailable_node_test() ->
    % Test remote effect on unavailable node
    Effect = {effect, {state, {get}}},
    Result = catena_effect_distributed:remote_effect('nonexistent@node', Effect, []),
    ?assertMatch({error, {node_unavailable, _}}, Result).

remote_effect_async_test() ->
    % Test async remote effect
    Effect = {effect, {state, {get}}},
    Result = catena_effect_distributed:remote_effect_async(node(), Effect, []),
    ?assertMatch({ok, {remote_ref, _, _}}, Result).

remote_effect_async_and_await_test() ->
    % Test async remote effect with await
    Effect = {effect, {state, {get}}},
    {ok, Ref} = catena_effect_distributed:remote_effect_async(node(), Effect, []),
    Result = catena_effect_distributed:await_remote(Ref),
    ?assertMatch({ok, {state_result, _}}, Result).

remote_node_returns_current_node_test() ->
    Node = catena_effect_distributed:remote_node(),
    ?assertEqual(node(), Node).

%%%=============================================================================
%%% Effect Serialization Tests
%%%=============================================================================

serialize_effect_test() ->
    % Test effect serialization
    Effect = {effect, {state, {get}}},
    Binary = catena_effect_distributed:serialize_effect(Effect),
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0).

deserialize_effect_test() ->
    % Test effect deserialization
    Effect = {effect, {state, {put, 42}}},
    Binary = catena_effect_distributed:serialize_effect(Effect),
    Decoded = catena_effect_distributed:deserialize_effect(Binary),
    ?assertEqual(Effect, Decoded).

serialize_deserialize_roundtrip_test() ->
    % Test roundtrip serialization
    Effect = {effect, {reader, ask}},
    Binary = catena_effect_distributed:serialize_effect(Effect),
    Decoded = catena_effect_distributed:deserialize_effect(Binary),
    ?assertEqual(Effect, Decoded).

is_serializable_basic_types_test() ->
    % Basic types are serializable
    ?assert(catena_effect_distributed:is_serializable({effect, {state, {get}}})).

is_serializable_with_function_test() ->
    % Functions are not serializable
    Effect = {effect, {state, {modify, fun(X) -> X + 1 end}}},
    ?assertNot(catena_effect_distributed:is_serializable(Effect)).

is_serializable_nested_structures_test() ->
    % Nested structures with basic types are serializable
    Effect = {effect, {complex, {nested, [1, 2, 3]}}},
    ?assert(catena_effect_distributed:is_serializable(Effect)).

encode_term_test() ->
    % Test term encoding
    Term = {value, 42},
    Binary = catena_effect_distributed:encode_term(Term),
    ?assert(is_binary(Binary)).

decode_term_test() ->
    % Test term decoding
    Term = {value, 42},
    Binary = catena_effect_distributed:encode_term(Term),
    Decoded = catena_effect_distributed:decode_term(Binary),
    ?assertEqual(Term, Decoded).

encode_decode_roundtrip_test() ->
    % Test encode/decode roundtrip
    Term = {complex, {nested, [1, 2, 3]}},
    Binary = catena_effect_distributed:encode_term(Term),
    Decoded = catena_effect_distributed:decode_term(Binary),
    ?assertEqual(Term, Decoded).

%%%=============================================================================
%%% Distributed Effect Coordination Tests
%%%=============================================================================

cluster_nodes_includes_current_node_test() ->
    Nodes = catena_effect_distributed:cluster_nodes(),
    ?assert(lists:member(node(), Nodes)).

select_node_returns_valid_node_test() ->
    Effect = {effect, {state, {get}}},
    Node = catena_effect_distributed:select_node(Effect),
    ?assert(is_atom(Node)),
    ?assert(lists:member(Node, catena_effect_distributed:cluster_nodes())).

is_available_current_node_test() ->
    ?assert(catena_effect_distributed:is_available(node())).

is_available_nonexistent_node_test() ->
    ?assertNot(catena_effect_distributed:is_available('nonexistent@node')).

ping_node_current_test() ->
    % Pinging current node returns pang (net_adm:ping behavior)
    ?assertEqual(pang, catena_effect_distributed:ping_node(node())).

ping_node_nonexistent_test() ->
    % Pinging nonexistent node returns pang
    ?assertEqual(pang, catena_effect_distributed:ping_node('nonexistent@node')).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

distributed_effect_lifecycle_test() ->
    % Test full distributed effect lifecycle
    Effect = {effect, {state, {put, 42}}},

    % Serialize
    Serialized = catena_effect_distributed:serialize_effect(Effect),

    % Select node
    Node = catena_effect_distributed:select_node(Effect),

    % Execute remotely (on current node for testing)
    Result = catena_effect_distributed:remote_effect(Node, Effect, []),

    ?assertMatch({ok, {state_result, _}}, Result).

async_remote_effect_lifecycle_test() ->
    % Test async remote effect lifecycle
    Effect = {effect, {reader, ask}},

    % Start async
    {ok, Ref} = catena_effect_distributed:remote_effect_async(node(), Effect, []),

    % Await result
    Result = catena_effect_distributed:await_remote(Ref),

    ?assertMatch({ok, {reader_result, _}}, Result).

serialization_with_remote_execution_test() ->
    % Test combining serialization with remote execution
    OriginalEffect = {effect, {writer, {tell, "log"}}},

    % Serialize and deserialize
    Serialized = catena_effect_distributed:serialize_effect(OriginalEffect),
    Deserialized = catena_effect_distributed:deserialize_effect(Serialized),

    % Execute deserialized effect remotely
    Result = catena_effect_distributed:remote_effect(node(), Deserialized, []),

    ?assertEqual(OriginalEffect, Deserialized),
    ?assertMatch({ok, {writer_result, _}}, Result).

multiple_effects_serialization_test() ->
    % Test serializing multiple effects
    Effects = [
        {effect, {state, {get}}},
        {effect, {state, {put, 1}}},
        {effect, {reader, ask}}
    ],

    Serialized = lists:map(fun catena_effect_distributed:serialize_effect/1, Effects),
    Deserialized = lists:map(fun catena_effect_distributed:deserialize_effect/1, Serialized),

    ?assertEqual(Effects, Deserialized).

cluster_availability_test() ->
    % Test cluster availability checks
    Nodes = catena_effect_distributed:cluster_nodes(),

    % All nodes in cluster should be marked available (current node only in test)
    CurrentNode = node(),
    ?assert(catena_effect_distributed:is_available(CurrentNode)).

%%%=============================================================================
%%% Error Handling Tests
%%%=============================================================================

remote_effect_timeout_handling_test() ->
    % Test that timeout option is handled
    Effect = {effect, {state, {get}}},
    % Very short timeout on nonexistent node
    Result = catena_effect_distributed:remote_effect(
        'nonexistent@node',
        Effect,
        [{timeout, 100}]
    ),
    ?assertMatch({error, {node_unavailable, _}}, Result).

await_remote_timeout_test() ->
    % Test await timeout with bad ref (short timeout)
    BadRef = {remote_ref, node(), make_ref()},
    Result = catena_effect_distributed:await_remote(BadRef, 100),
    ?assertMatch({error, timeout}, Result).

serialize_complex_term_test() ->
    % Test serializing complex terms
    ComplexTerm = {
        effect,
        {complex,
            {nested, [1, 2, 3]},
            {map, #{key => value}}
        }
    },
    Binary = catena_effect_distributed:serialize_effect(ComplexTerm),
    Decoded = catena_effect_distributed:deserialize_effect(Binary),
    ?assertEqual(ComplexTerm, Decoded).
