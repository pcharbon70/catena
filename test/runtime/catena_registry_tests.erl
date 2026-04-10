%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_registry (Phase 5.3)
%%% @end
%%%-------------------------------------------------------------------
-module(catena_registry_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Setup and Cleanup
%%%=============================================================================

cleanup_registries() ->
    lists:foreach(fun(N) ->
        case whereis(N) of
            undefined -> ok;
            Pid -> exit(Pid, kill)
        end
    end, [test_reg, test_reg_named]).

%%%=============================================================================
%%% Start/Stop Tests
%%%=============================================================================

start_basic_test() ->
    {ok, Pid} = catena_registry:start_link(test_reg),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    catena_registry:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

start_named_test() ->
    cleanup_registries(),
    {ok, Pid} = catena_registry:start_link({local, test_reg_named}, test_reg),
    ?assertEqual(Pid, whereis(test_reg_named)),
    catena_registry:stop(test_reg_named),
    timer:sleep(10),
    ?assertEqual(undefined, whereis(test_reg_named)).

%%%=============================================================================
%%% Registration Tests
%%%=============================================================================

register_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid)),
    ?assertMatch({ok, Pid}, catena_registry:find(RegPid, key1)),
    catena_registry:stop(RegPid).

register_with_metadata_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    Metadata = #{type => counter, value => 0},
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid, Metadata)),
    ?assertMatch({ok, Metadata}, catena_registry:get_metadata(RegPid, key1)),
    catena_registry:stop(RegPid).

register_duplicate_key_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid1 = spawn(fun() -> receive after infinity -> ok end end),
    Pid2 = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid1)),
    ?assertEqual({error, already_registered}, catena_registry:register(RegPid, key1, Pid2)),
    catena_registry:stop(RegPid).

register_same_pid_different_key_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid)),
    ?assertEqual({error, pid_already_registered}, catena_registry:register(RegPid, key2, Pid)),
    catena_registry:stop(RegPid).

unregister_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid)),
    ?assertEqual(ok, catena_registry:unregister(RegPid, key1)),
    ?assertEqual({error, not_found}, catena_registry:find(RegPid, key1)),
    catena_registry:stop(RegPid).

unregister_nonexistent_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    ?assertEqual(ok, catena_registry:unregister(RegPid, key1)),
    catena_registry:stop(RegPid).

%%%=============================================================================
%%% Lookup Tests
%%%=============================================================================

find_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid)),
    ?assertMatch({ok, Pid}, catena_registry:find(RegPid, key1)),
    ?assertEqual({error, not_found}, catena_registry:find(RegPid, key2)),
    catena_registry:stop(RegPid).

whereis_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid)),
    ?assertEqual(Pid, catena_registry:whereis(RegPid, key1)),
    ?assertEqual(undefined, catena_registry:whereis(RegPid, key2)),
    catena_registry:stop(RegPid).

%%%=============================================================================
%%% Pattern Matching Tests
%%%=============================================================================

match_all_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid1 = spawn(fun() -> receive after infinity -> ok end end),
    Pid2 = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid1, #{type => counter})),
    ?assertEqual(ok, catena_registry:register(RegPid, key2, Pid2, #{type => counter})),
    ?assertEqual(2, length(catena_registry:match(RegPid, '_', #{type => counter}))),
    catena_registry:stop(RegPid).

match_metadata_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid1 = spawn(fun() -> receive after infinity -> ok end end),
    Pid2 = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid1, #{type => counter, value => 0})),
    ?assertEqual(ok, catena_registry:register(RegPid, key2, Pid2, #{type => counter, value => 10})),
    Result = catena_registry:match(RegPid, '_', #{type => counter, value => 0}),
    ?assertEqual(1, length(Result)),
    [{key1, Pid1, #{type := counter, value := 0}}] = Result,
    catena_registry:stop(RegPid).

list_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid1 = spawn(fun() -> receive after infinity -> ok end end),
    Pid2 = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid1)),
    ?assertEqual(ok, catena_registry:register(RegPid, key2, Pid2)),
    ?assertEqual(2, length(catena_registry:list(RegPid))),
    catena_registry:stop(RegPid).

list_pattern_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid1 = spawn(fun() -> receive after infinity -> ok end end),
    Pid2 = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid1)),
    ?assertEqual(ok, catena_registry:register(RegPid, key2, Pid2)),
    ?assertEqual(1, length(catena_registry:list(RegPid, key1))),
    catena_registry:stop(RegPid).

%%%=============================================================================
%%% Metadata Tests
%%%=============================================================================

get_metadata_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    Metadata = #{type => counter, value => 42},
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid, Metadata)),
    ?assertMatch({ok, Metadata}, catena_registry:get_metadata(RegPid, key1)),
    catena_registry:stop(RegPid).

get_metadata_nonexistent_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    ?assertEqual({error, not_found}, catena_registry:get_metadata(RegPid, key1)),
    catena_registry:stop(RegPid).

update_metadata_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid, #{type => counter})),
    ?assertEqual(ok, catena_registry:update_metadata(RegPid, key1, #{type => counter, value => 100})),
    ?assertMatch({ok, #{type := counter, value := 100}}, catena_registry:get_metadata(RegPid, key1)),
    catena_registry:stop(RegPid).

update_metadata_nonexistent_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    ?assertEqual({error, not_found}, catena_registry:update_metadata(RegPid, key1, #{})),
    catena_registry:stop(RegPid).

%%%=============================================================================
%%% Process Cleanup Tests
%%%=============================================================================

auto_cleanup_on_exit_test() ->
    {ok, RegPid} = catena_registry:start_link(test_reg),
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ?assertEqual(ok, catena_registry:register(RegPid, key1, Pid)),
    ?assertMatch({ok, Pid}, catena_registry:find(RegPid, key1)),
    exit(Pid, kill),
    timer:sleep(50),
    ?assertEqual({error, not_found}, catena_registry:find(RegPid, key1)),
    catena_registry:stop(RegPid).
