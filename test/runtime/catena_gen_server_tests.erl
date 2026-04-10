%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_gen_server (Phase 5.2)
%%% @end
%%%-------------------------------------------------------------------
-module(catena_gen_server_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Setup and Cleanup
%%%=============================================================================

cleanup_servers() ->
    lists:foreach(fun(N) ->
        case whereis(N) of
            undefined -> ok;
            Pid -> exit(Pid, kill)
        end
    end, [test_gs_reg, test_gs_named]).

%%%=============================================================================
%%% Start/Stop Tests
%%%=============================================================================

start_basic_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [], []),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    catena_gen_server:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

start_with_args_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [{value, 42}], []),
    ?assertEqual(42, catena_gen_server:call(Pid, get_value)),
    catena_gen_server:stop(Pid).

start_link_test() ->
    {ok, Pid} = catena_gen_server:start_link(test_gen_server, [], []),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    catena_gen_server:stop(Pid).

start_named_test() ->
    cleanup_servers(),
    {ok, Pid} = catena_gen_server:start(test_gs_reg, test_gen_server, [], []),
    ?assertEqual(Pid, whereis(test_gs_reg)),
    ?assertEqual(0, catena_gen_server:call(test_gs_reg, get_value)),
    catena_gen_server:stop(test_gs_reg).

stop_with_reason_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [], []),
    ok = catena_gen_server:stop(Pid, custom_reason),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

%%%=============================================================================
%%% Synchronous Call Tests
%%%=============================================================================

call_get_value_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [{value, 10}], []),
    ?assertEqual(10, catena_gen_server:call(Pid, get_value)),
    catena_gen_server:stop(Pid).

call_set_value_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [{value, 5}], []),
    ?assertEqual(ok, catena_gen_server:call(Pid, {set_value, 20})),
    ?assertEqual(20, catena_gen_server:call(Pid, get_value)),
    catena_gen_server:stop(Pid).

call_add_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [{value, 5}], []),
    ?assertEqual(8, catena_gen_server:call(Pid, {add, 3})),
    ?assertEqual(8, catena_gen_server:call(Pid, get_value)),
    catena_gen_server:stop(Pid).

call_with_timeout_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [], []),
    ?assertEqual(0, catena_gen_server:call(Pid, get_value, 1000)),
    catena_gen_server:stop(Pid).

call_stop_with_reply_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [], []),
    ?assertEqual(stopped, catena_gen_server:call(Pid, stop_with_reply)),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

call_stop_via_api_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [], []),
    ?assertEqual(ok, catena_gen_server:call(Pid, {stop, normal})),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

%%%=============================================================================
%%% Asynchronous Cast Tests
%%%=============================================================================

cast_add_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [{value, 5}], []),
    ok = catena_gen_server:cast(Pid, {add, 10}),
    timer:sleep(50),
    ?assertEqual(15, catena_gen_server:call(Pid, get_value)),
    catena_gen_server:stop(Pid).

cast_set_value_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [], []),
    ok = catena_gen_server:cast(Pid, {set_value, 99}),
    timer:sleep(50),
    ?assertEqual(99, catena_gen_server:call(Pid, get_value)),
    catena_gen_server:stop(Pid).

cast_stop_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [], []),
    ok = catena_gen_server:cast(Pid, stop),
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)).

%%%=============================================================================
%%% Info Message Tests
%%%=============================================================================

handle_info_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [], []),
    Pid ! hello,
    Pid ! world,
    timer:sleep(50),
    Messages = catena_gen_server:call(Pid, get_messages),
    ?assert(lists:member(world, Messages)),
    ?assert(lists:member(hello, Messages)),
    catena_gen_server:stop(Pid).

%%%=============================================================================
%%% State Persistence Tests
%%%=============================================================================

state_persistence_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [{value, 0}], []),
    ?assertEqual(5, catena_gen_server:call(Pid, {add, 5})),
    ?assertEqual(15, catena_gen_server:call(Pid, {add, 10})),
    ?assertEqual(15, catena_gen_server:call(Pid, get_value)),
    catena_gen_server:cast(Pid, {add, 5}),
    timer:sleep(50),
    ?assertEqual(20, catena_gen_server:call(Pid, get_value)),
    catena_gen_server:stop(Pid).

%%%=============================================================================
%%% Reply Tests
%%%=============================================================================

reply_test() ->
    {ok, Pid} = catena_gen_server:start(test_gen_server, [], []),
    From = {self(), make_ref()},
    ?assertEqual(ok, catena_gen_server:reply(From, test_reply)),
    receive
        {_, test_reply} -> ok
    after 100 ->
        ?assert(false, did_not_receive_reply)
    end,
    catena_gen_server:stop(Pid).

%%%=============================================================================
%%% Multi-call Tests
%%%=============================================================================

multi_call_test() ->
    cleanup_servers(),
    {ok, Pid1} = catena_gen_server:start(test_gs_reg, test_gen_server, [{value, 10}], []),
    {ok, Pid2} = catena_gen_server:start_link(test_gs_named, test_gen_server, [{value, 20}], []),
    {Replies, BadNodes} = catena_gen_server:multi_call([test_gs_reg, test_gs_named], get_value),
    ?assertEqual(2, length(Replies)),
    ?assertEqual(0, length(BadNodes)),
    ?assert(lists:keymember(node(), 1, Replies)),
    catena_gen_server:stop(Pid1),
    catena_gen_server:stop(Pid2).

%%%=============================================================================
%%% Named Server Tests
%%%=============================================================================

call_named_server_test() ->
    cleanup_servers(),
    {ok, _Pid} = catena_gen_server:start(test_gs_reg, test_gen_server, [{value, 42}], []),
    ?assertEqual(42, catena_gen_server:call(test_gs_reg, get_value)),
    catena_gen_server:stop(test_gs_reg).

cast_named_server_test() ->
    cleanup_servers(),
    {ok, _Pid} = catena_gen_server:start(test_gs_reg, test_gen_server, [{value, 5}], []),
    ok = catena_gen_server:cast(test_gs_reg, {add, 10}),
    timer:sleep(50),
    ?assertEqual(15, catena_gen_server:call(test_gs_reg, get_value)),
    catena_gen_server:stop(test_gs_reg).

stop_named_server_test() ->
    cleanup_servers(),
    {ok, _Pid} = catena_gen_server:start(test_gs_reg, test_gen_server, [], []),
    ok = catena_gen_server:stop(test_gs_reg),
    timer:sleep(10),
    ?assertEqual(undefined, whereis(test_gs_reg)).
