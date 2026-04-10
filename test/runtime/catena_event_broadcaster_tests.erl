%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_event_broadcaster (Phase 5.3)
%%% @end
%%%-------------------------------------------------------------------
-module(catena_event_broadcaster_tests).
-include_lib("eunit/include/eunit.hrl").

%% Export helper functions
-export([listener_loop/0, get_event_count/1]).

%%%=============================================================================
%%% Setup and Cleanup
%%%=============================================================================

cleanup_broadcasters() ->
    lists:foreach(fun(N) ->
        case whereis(N) of
            undefined -> ok;
            Pid -> exit(Pid, kill)
        end
    end, [test_broadcaster, test_broadcaster_named]).

%%%=============================================================================
%%% Start/Stop Tests
%%%=============================================================================

start_basic_test() ->
    {ok, Pid} = catena_event_broadcaster:start_link(test_broadcaster),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    catena_event_broadcaster:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

start_named_test() ->
    cleanup_broadcasters(),
    {ok, Pid} = catena_event_broadcaster:start_link({local, test_broadcaster_named}, test_broadcaster),
    ?assertEqual(Pid, whereis(test_broadcaster_named)),
    catena_event_broadcaster:stop(test_broadcaster_named),
    timer:sleep(10),
    ?assertEqual(undefined, whereis(test_broadcaster_named)).

%%%=============================================================================
%%% Add/Remove Listener Tests
%%%=============================================================================

add_listener_test() ->
    {ok, Broadcaster} = catena_event_broadcaster:start_link(test_broadcaster),
    Pid = spawn(catena_event_broadcaster_tests, listener_loop, []),
    ?assertEqual(ok, catena_event_broadcaster:add_listener(Broadcaster, Pid)),
    ?assertEqual(1, catena_event_broadcaster:listener_count(Broadcaster)),
    catena_event_broadcaster:stop(Broadcaster),
    exit(Pid, kill).

add_duplicate_listener_test() ->
    {ok, Broadcaster} = catena_event_broadcaster:start_link(test_broadcaster),
    Pid = spawn(catena_event_broadcaster_tests, listener_loop, []),
    ?assertEqual(ok, catena_event_broadcaster:add_listener(Broadcaster, Pid)),
    ?assertEqual({error, already_a_listener}, catena_event_broadcaster:add_listener(Broadcaster, Pid)),
    catena_event_broadcaster:stop(Broadcaster),
    exit(Pid, kill).

remove_listener_test() ->
    {ok, Broadcaster} = catena_event_broadcaster:start_link(test_broadcaster),
    Pid = spawn(catena_event_broadcaster_tests, listener_loop, []),
    ?assertEqual(ok, catena_event_broadcaster:add_listener(Broadcaster, Pid)),
    ?assertEqual(1, catena_event_broadcaster:listener_count(Broadcaster)),
    ?assertEqual(ok, catena_event_broadcaster:remove_listener(Broadcaster, Pid)),
    ?assertEqual(0, catena_event_broadcaster:listener_count(Broadcaster)),
    catena_event_broadcaster:stop(Broadcaster),
    exit(Pid, kill).

remove_nonexistent_listener_test() ->
    {ok, Broadcaster} = catena_event_broadcaster:start_link(test_broadcaster),
    Pid = spawn(catena_event_broadcaster_tests, listener_loop, []),
    ?assertEqual(ok, catena_event_broadcaster:remove_listener(Broadcaster, Pid)),
    catena_event_broadcaster:stop(Broadcaster),
    exit(Pid, kill).

%%%=============================================================================
%%% Broadcast Tests
%%%=============================================================================

broadcast_test() ->
    {ok, Broadcaster} = catena_event_broadcaster:start_link(test_broadcaster),
    Pid = spawn(catena_event_broadcaster_tests, listener_loop, []),
    ?assertEqual(ok, catena_event_broadcaster:add_listener(Broadcaster, Pid)),
    ?assertEqual(ok, catena_event_broadcaster:broadcast(Broadcaster, #{event => test})),
    timer:sleep(50),
    ?assertEqual(1, get_event_count(Pid)),
    catena_event_broadcaster:stop(Broadcaster),
    exit(Pid, kill).

broadcast_multiple_listeners_test() ->
    {ok, Broadcaster} = catena_event_broadcaster:start_link(test_broadcaster),
    Pid1 = spawn(catena_event_broadcaster_tests, listener_loop, []),
    Pid2 = spawn(catena_event_broadcaster_tests, listener_loop, []),
    ?assertEqual(ok, catena_event_broadcaster:add_listener(Broadcaster, Pid1)),
    ?assertEqual(ok, catena_event_broadcaster:add_listener(Broadcaster, Pid2)),
    ?assertEqual(ok, catena_event_broadcaster:broadcast(Broadcaster, #{event => test})),
    timer:sleep(50),
    ?assertEqual(1, get_event_count(Pid1)),
    ?assertEqual(1, get_event_count(Pid2)),
    catena_event_broadcaster:stop(Broadcaster),
    exit(Pid1, kill),
    exit(Pid2, kill).

%%%=============================================================================
%%% Filter Tests
%%%=============================================================================

filter_pass_test() ->
    {ok, Broadcaster} = catena_event_broadcaster:start_link(test_broadcaster),
    Filter = fun(#{type := Type}) -> Type =:= important end,
    Pid = spawn(catena_event_broadcaster_tests, listener_loop, []),
    ?assertEqual(ok, catena_event_broadcaster:add_listener(Broadcaster, Pid, Filter)),
    ?assertEqual(ok, catena_event_broadcaster:broadcast(Broadcaster, #{type => important})),
    timer:sleep(50),
    ?assertEqual(1, get_event_count(Pid)),
    catena_event_broadcaster:stop(Broadcaster),
    exit(Pid, kill).

filter_fail_test() ->
    {ok, Broadcaster} = catena_event_broadcaster:start_link(test_broadcaster),
    Filter = fun(#{type := Type}) -> Type =:= important end,
    Pid = spawn(catena_event_broadcaster_tests, listener_loop, []),
    ?assertEqual(ok, catena_event_broadcaster:add_listener(Broadcaster, Pid, Filter)),
    ?assertEqual(ok, catena_event_broadcaster:broadcast(Broadcaster, #{type => normal})),
    timer:sleep(50),
    ?assertEqual(0, get_event_count(Pid)),
    catena_event_broadcaster:stop(Broadcaster),
    exit(Pid, kill).

%%%=============================================================================
%%% List Tests
%%%=============================================================================

list_listeners_test() ->
    {ok, Broadcaster} = catena_event_broadcaster:start_link(test_broadcaster),
    Pid1 = spawn(catena_event_broadcaster_tests, listener_loop, []),
    Pid2 = spawn(catena_event_broadcaster_tests, listener_loop, []),
    ?assertEqual(ok, catena_event_broadcaster:add_listener(Broadcaster, Pid1)),
    ?assertEqual(ok, catena_event_broadcaster:add_listener(Broadcaster, Pid2)),
    Listeners = catena_event_broadcaster:list_listeners(Broadcaster),
    ?assertEqual(2, length(Listeners)),
    ?assert(lists:member(Pid1, Listeners)),
    ?assert(lists:member(Pid2, Listeners)),
    catena_event_broadcaster:stop(Broadcaster),
    exit(Pid1, kill),
    exit(Pid2, kill).

listener_count_test() ->
    {ok, Broadcaster} = catena_event_broadcaster:start_link(test_broadcaster),
    ?assertEqual(0, catena_event_broadcaster:listener_count(Broadcaster)),
    Pid1 = spawn(catena_event_broadcaster_tests, listener_loop, []),
    ?assertEqual(ok, catena_event_broadcaster:add_listener(Broadcaster, Pid1)),
    ?assertEqual(1, catena_event_broadcaster:listener_count(Broadcaster)),
    Pid2 = spawn(catena_event_broadcaster_tests, listener_loop, []),
    ?assertEqual(ok, catena_event_broadcaster:add_listener(Broadcaster, Pid2)),
    ?assertEqual(2, catena_event_broadcaster:listener_count(Broadcaster)),
    catena_event_broadcaster:stop(Broadcaster),
    exit(Pid1, kill),
    exit(Pid2, kill).

%%%=============================================================================
%%% Auto Cleanup Tests
%%%=============================================================================

auto_cleanup_on_exit_test() ->
    {ok, Broadcaster} = catena_event_broadcaster:start_link(test_broadcaster),
    Pid = spawn(catena_event_broadcaster_tests, listener_loop, []),
    ?assertEqual(ok, catena_event_broadcaster:add_listener(Broadcaster, Pid)),
    ?assertEqual(1, catena_event_broadcaster:listener_count(Broadcaster)),
    exit(Pid, kill),
    timer:sleep(50),
    ?assertEqual(0, catena_event_broadcaster:listener_count(Broadcaster)),
    catena_event_broadcaster:stop(Broadcaster).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

listener_loop() ->
    receive
        {event, _Event} ->
            E = case get(events) of undefined -> 0; V -> V end,
            put(events, E + 1),
            listener_loop();
        {get_event_count, From} ->
            Count = case get(events) of undefined -> 0; V -> V end,
            From ! {event_count, Count},
            listener_loop();
        Other ->
            io:format("Unexpected message: ~p~n", [Other]),
            listener_loop()
    end.

get_event_count(Pid) ->
    Pid ! {get_event_count, self()},
    receive
        {event_count, Count} -> Count
    after 100 -> 0
    end.
