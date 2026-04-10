%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_pubsub (Phase 5.3)
%%% @end
%%%-------------------------------------------------------------------
-module(catena_pubsub_tests).
-include_lib("eunit/include/eunit.hrl").

%% Export helper functions
-export([subscriber_loop/0, get_messages/1]).

%%%=============================================================================
%%% Setup and Cleanup
%%%=============================================================================

cleanup_pubsub() ->
    lists:foreach(fun(N) ->
        case whereis(N) of
            undefined -> ok;
            Pid -> exit(Pid, kill)
        end
    end, [test_pubsub, test_pubsub_named]).

%%%=============================================================================
%%% Start/Stop Tests
%%%=============================================================================

start_basic_test() ->
    {ok, Pid} = catena_pubsub:start_link(test_pubsub),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    catena_pubsub:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

start_named_test() ->
    cleanup_pubsub(),
    {ok, Pid} = catena_pubsub:start_link({local, test_pubsub_named}, test_pubsub),
    ?assertEqual(Pid, whereis(test_pubsub_named)),
    catena_pubsub:stop(test_pubsub_named),
    timer:sleep(10),
    ?assertEqual(undefined, whereis(test_pubsub_named)).

%%%=============================================================================
%%% Subscribe/Unsubscribe Tests
%%%=============================================================================

subscribe_test() ->
    {ok, PubSub} = catena_pubsub:start_link(test_pubsub),
    Pid = spawn(catena_pubsub_tests, subscriber_loop, []),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid, <<"test.topic">>)),
    catena_pubsub:stop(PubSub),
    exit(Pid, kill).

subscribe_with_options_test() ->
    {ok, PubSub} = catena_pubsub:start_link(test_pubsub),
    Pid = spawn(catena_pubsub_tests, subscriber_loop, []),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid, <<"test.topic">>, #{priority => high})),
    catena_pubsub:stop(PubSub),
    exit(Pid, kill).

unsubscribe_test() ->
    {ok, PubSub} = catena_pubsub:start_link(test_pubsub),
    Pid = spawn(catena_pubsub_tests, subscriber_loop, []),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid, <<"test.topic">>)),
    ?assertEqual(ok, catena_pubsub:unsubscribe(PubSub, Pid, <<"test.topic">>)),
    catena_pubsub:stop(PubSub),
    exit(Pid, kill).

%%%=============================================================================
%%% Publish Tests
%%%=============================================================================

publish_test() ->
    {ok, PubSub} = catena_pubsub:start_link(test_pubsub),
    Pid = spawn(catena_pubsub_tests, subscriber_loop, []),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid, <<"test.topic">>)),
    ?assertEqual(ok, catena_pubsub:publish(PubSub, <<"test.topic">>, #{message => hello})),
    timer:sleep(50),
    ?assertEqual(1, get_messages(Pid)),
    catena_pubsub:stop(PubSub),
    exit(Pid, kill).

publish_multiple_subscribers_test() ->
    {ok, PubSub} = catena_pubsub:start_link(test_pubsub),
    Pid1 = spawn(catena_pubsub_tests, subscriber_loop, []),
    Pid2 = spawn(catena_pubsub_tests, subscriber_loop, []),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid1, <<"test.topic">>)),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid2, <<"test.topic">>)),
    ?assertEqual(ok, catena_pubsub:publish(PubSub, <<"test.topic">>, #{message => hello})),
    timer:sleep(50),
    ?assertEqual(1, get_messages(Pid1)),
    ?assertEqual(1, get_messages(Pid2)),
    catena_pubsub:stop(PubSub),
    exit(Pid1, kill),
    exit(Pid2, kill).

%%%=============================================================================
%%% Topic Pattern Tests
%%%=============================================================================

publish_wildcard_single_test() ->
    {ok, PubSub} = catena_pubsub:start_link(test_pubsub),
    Pid = spawn(catena_pubsub_tests, subscriber_loop, []),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid, <<"test.*">>)),
    ?assertEqual(ok, catena_pubsub:publish(PubSub, <<"test.topic">>, #{message => hello})),
    timer:sleep(50),
    ?assertEqual(1, get_messages(Pid)),
    catena_pubsub:stop(PubSub),
    exit(Pid, kill).

publish_wildcard_multi_test() ->
    {ok, PubSub} = catena_pubsub:start_link(test_pubsub),
    Pid = spawn(catena_pubsub_tests, subscriber_loop, []),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid, <<"test.*.final">>)),
    ?assertEqual(ok, catena_pubsub:publish(PubSub, <<"test.middle.final">>, #{message => hello})),
    timer:sleep(50),
    ?assertEqual(1, get_messages(Pid)),
    catena_pubsub:stop(PubSub),
    exit(Pid, kill).

publish_all_below_test() ->
    {ok, PubSub} = catena_pubsub:start_link(test_pubsub),
    Pid = spawn(catena_pubsub_tests, subscriber_loop, []),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid, <<"test.>">>)),
    ?assertEqual(ok, catena_pubsub:publish(PubSub, <<"test.a.b.c">>, #{message => hello})),
    timer:sleep(50),
    ?assertEqual(1, get_messages(Pid)),
    catena_pubsub:stop(PubSub),
    exit(Pid, kill).

publish_no_match_test() ->
    {ok, PubSub} = catena_pubsub:start_link(test_pubsub),
    Pid = spawn(catena_pubsub_tests, subscriber_loop, []),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid, <<"other.topic">>)),
    ?assertEqual(ok, catena_pubsub:publish(PubSub, <<"test.topic">>, #{message => hello})),
    timer:sleep(50),
    ?assertEqual(0, get_messages(Pid)),
    catena_pubsub:stop(PubSub),
    exit(Pid, kill).

%%%=============================================================================
%%% List Tests
%%%=============================================================================

list_subscribers_test() ->
    {ok, PubSub} = catena_pubsub:start_link(test_pubsub),
    Pid1 = spawn(catena_pubsub_tests, subscriber_loop, []),
    Pid2 = spawn(catena_pubsub_tests, subscriber_loop, []),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid1, <<"test.topic">>)),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid2, <<"test.topic">>)),
    ?assertEqual(2, length(catena_pubsub:list_subscribers(PubSub, <<"test.topic">>))),
    catena_pubsub:stop(PubSub),
    exit(Pid1, kill),
    exit(Pid2, kill).

list_topics_test() ->
    {ok, PubSub} = catena_pubsub:start_link(test_pubsub),
    Pid = spawn(catena_pubsub_tests, subscriber_loop, []),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid, "test.topic1")),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid, "test.topic2")),
    Topics = catena_pubsub:list_topics(PubSub),
    ?assertEqual(2, length(Topics)),
    catena_pubsub:stop(PubSub),
    exit(Pid, kill).

%%%=============================================================================
%%% Auto Cleanup Tests
%%%=============================================================================

auto_cleanup_on_exit_test() ->
    {ok, PubSub} = catena_pubsub:start_link(test_pubsub),
    Pid = spawn(catena_pubsub_tests, subscriber_loop, []),
    ?assertEqual(ok, catena_pubsub:subscribe(PubSub, Pid, <<"test.topic">>)),
    ?assertEqual(1, length(catena_pubsub:list_subscribers(PubSub, <<"test.topic">>))),
    exit(Pid, kill),
    timer:sleep(50),
    ?assertEqual(0, length(catena_pubsub:list_subscribers(PubSub, <<"test.topic">>))),
    catena_pubsub:stop(PubSub).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

subscriber_loop() ->
    receive
        {pubsub, _Topic, _Message} ->
            M = case get(messages) of undefined -> 0; V -> V end,
            put(messages, M + 1),
            subscriber_loop();
        {get_messages, From} ->
            Count = case get(messages) of undefined -> 0; V -> V end,
            From ! {messages, Count},
            subscriber_loop();
        Other ->
            io:format("Unexpected message: ~p~n", [Other]),
            subscriber_loop()
    end.

get_messages(Pid) ->
    Pid ! {get_messages, self()},
    receive
        {messages, Count} -> Count
    after 100 -> 0
    end.
