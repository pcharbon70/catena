%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_actor (Phase 5.1.5)
%%% @end
%%%-------------------------------------------------------------------
-module(catena_actor_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Setup and Cleanup
%%%=============================================================================

cleanup_actors() ->
    lists:foreach(fun(N) ->
        case whereis(N) of
            undefined -> ok;
            Pid -> exit(Pid, kill)
        end
    end, [test_actor_reg, test_actor_named]).

%%%=============================================================================
%%% Start/Stop Tests
%%%=============================================================================

start_basic_test() ->
    {ok, Pid} = catena_actor:start(test_actor, []),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    ok = catena_actor:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

start_with_args_test() ->
    {ok, Pid} = catena_actor:start(test_actor, [{value, 42}]),
    ?assertEqual(42, catena_actor:call(Pid, get_value)),
    ok = catena_actor:stop(Pid).

start_link_test() ->
    {ok, Pid} = catena_actor:start_link(test_actor, []),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    ok = catena_actor:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

start_init_stop_test() ->
    Result = catena_actor:start(test_actor_init_stop, fail),
    ?assertEqual({error, initialization_failed}, Result).

stop_basic_test() ->
    {ok, Pid} = catena_actor:start(test_actor, []),
    ?assert(is_process_alive(Pid)),
    ok = catena_actor:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

stop_with_reason_test() ->
    {ok, Pid} = catena_actor:start(test_actor, []),
    ok = catena_actor:stop(Pid, shutdown),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

%%%=============================================================================
%%% Synchronous Call Tests
%%%=============================================================================

call_get_value_test() ->
    {ok, Pid} = catena_actor:start(test_actor, [{value, 10}]),
    ?assertEqual(10, catena_actor:call(Pid, get_value)),
    ok = catena_actor:stop(Pid).

call_set_value_test() ->
    {ok, Pid} = catena_actor:start(test_actor, [{value, 5}]),
    ?assertEqual(ok, catena_actor:call(Pid, {set_value, 20})),
    ?assertEqual(20, catena_actor:call(Pid, get_value)),
    ok = catena_actor:stop(Pid).

call_add_test() ->
    {ok, Pid} = catena_actor:start(test_actor, [{value, 5}]),
    ?assertEqual(8, catena_actor:call(Pid, {add, 3})),
    ?assertEqual(8, catena_actor:call(Pid, get_value)),
    ok = catena_actor:stop(Pid).

call_with_timeout_test() ->
    {ok, Pid} = catena_actor:start(test_actor, []),
    ?assertEqual(0, catena_actor:call(Pid, get_value, 1000)),
    ok = catena_actor:stop(Pid).

call_stop_with_reply_test() ->
    {ok, Pid} = catena_actor:start(test_actor, []),
    ?assertEqual(stopped, catena_actor:call(Pid, stop_with_reply)),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

call_dead_process_test() ->
    Pid = spawn(fun() -> ok end),
    timer:sleep(10),
    ?assertError({call_timeout, _, _}, catena_actor:call(Pid, get_value, 100)).

%%%=============================================================================
%%% Asynchronous Cast Tests
%%%=============================================================================

cast_add_test() ->
    {ok, Pid} = catena_actor:start(test_actor, [{value, 5}]),
    ok = catena_actor:cast(Pid, {add, 10}),
    timer:sleep(50),
    ?assertEqual(15, catena_actor:call(Pid, get_value)),
    ok = catena_actor:stop(Pid).

cast_set_value_test() ->
    {ok, Pid} = catena_actor:start(test_actor, []),
    ok = catena_actor:cast(Pid, {set_value, 99}),
    timer:sleep(50),
    ?assertEqual(99, catena_actor:call(Pid, get_value)),
    ok = catena_actor:stop(Pid).

cast_stop_test() ->
    {ok, Pid} = catena_actor:start(test_actor, []),
    ok = catena_actor:cast(Pid, stop),
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)).

%%%=============================================================================
%%% Info Message Tests
%%%=============================================================================

handle_info_test() ->
    {ok, Pid} = catena_actor:start(test_actor, []),
    Pid ! hello,
    Pid ! world,
    timer:sleep(50),
    Messages = catena_actor:call(Pid, get_messages),
    ?assert(lists:member(world, Messages)),
    ?assert(lists:member(hello, Messages)),
    ok = catena_actor:stop(Pid).

cast_to_info_test() ->
    {ok, Pid} = catena_actor:start(test_actor, []),
    catena_actor:cast(Pid, cast_message),
    timer:sleep(50),
    Messages = catena_actor:call(Pid, get_messages),
    ?assert(lists:member(cast_message, Messages)),
    ok = catena_actor:stop(Pid).

%%%=============================================================================
%%% State Accumulation Tests
%%%=============================================================================

state_persistence_test() ->
    {ok, Pid} = catena_actor:start(test_actor, [{value, 0}]),
    ?assertEqual(5, catena_actor:call(Pid, {add, 5})),
    ?assertEqual(15, catena_actor:call(Pid, {add, 10})),
    ?assertEqual(15, catena_actor:call(Pid, get_value)),
    catena_actor:cast(Pid, {add, 5}),
    timer:sleep(50),
    ?assertEqual(20, catena_actor:call(Pid, get_value)),
    ok = catena_actor:stop(Pid).

%%%=============================================================================
%%% Concurrency Tests
%%%=============================================================================

concurrent_calls_test() ->
    {ok, Pid} = catena_actor:start(test_actor, [{value, 0}]),
    Parent = self(),
    %% Spawn multiple processes making concurrent calls
    lists:foreach(fun(N) ->
        spawn(fun() ->
            Result = catena_actor:call(Pid, {add, 1}),
            Parent ! {N, Result}
        end)
    end, lists:seq(1, 10)),
    %% Collect all results
    Results = collect_results(10),
    ?assertEqual(10, length(Results)),
    %% Give actor time to process all messages
    timer:sleep(50),
    %% All calls should succeed and state should be 10
    ?assertEqual(10, catena_actor:call(Pid, get_value)),
    ok = catena_actor:stop(Pid).

concurrent_casts_test() ->
    {ok, Pid} = catena_actor:start(test_actor, [{value, 0}]),
    %% Send many casts concurrently
    lists:foreach(fun(_) ->
        catena_actor:cast(Pid, {add, 1})
    end, lists:seq(1, 100)),
    timer:sleep(200),
    ?assertEqual(100, catena_actor:call(Pid, get_value)),
    ok = catena_actor:stop(Pid).

%%%=============================================================================
%%% Reply Tests
%%%=============================================================================

reply_test() ->
    {ok, Pid} = catena_actor:start(test_actor, []),
    From = {self(), make_ref()},
    ?assertEqual(ok, catena_actor:reply(From, test_reply)),
    receive
        {_, test_reply} -> ok
    after 100 ->
        ?assert(false, did_not_receive_reply)
    end,
    ok = catena_actor:stop(Pid).

%%%=============================================================================
%%% Utility Functions
%%%=============================================================================

collect_results(0) -> [];
collect_results(Count) ->
    receive
        {_N, _Result} = Msg ->
            [Msg | collect_results(Count - 1)]
    after 1000 ->
        timeout
    end.
