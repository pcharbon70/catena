%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_supervisor (Phase 5.2)
%%% @end
%%%-------------------------------------------------------------------
-module(catena_supervisor_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Setup and Cleanup
%%%=============================================================================

cleanup_supervisors() ->
    lists:foreach(fun(N) ->
        case whereis(N) of
            undefined -> ok;
            Pid -> exit(Pid, kill)
        end
    end, [test_sup_reg, test_sup_named]).

%%%=============================================================================
%%% Start/Stop Tests
%%%=============================================================================

start_basic_test() ->
    {ok, Pid} = catena_supervisor:start_link(test_sup, []),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    catena_supervisor:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

start_named_test() ->
    cleanup_supervisors(),
    {ok, Pid} = catena_supervisor:start_link({local, test_sup_reg}, test_sup, []),
    ?assertEqual(Pid, whereis(test_sup_reg)),
    catena_supervisor:stop(test_sup_reg),
    timer:sleep(10),
    ?assertEqual(undefined, whereis(test_sup_reg)).

%%%=============================================================================
%%% Child Management Tests
%%%=============================================================================

start_child_test() ->
    {ok, SupPid} = catena_supervisor:start_link(test_sup, []),
    ChildSpec = #{
        id => test_child,
        start => {test_worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [test_worker]
    },
    ?assertMatch({ok, _Pid}, catena_supervisor:start_child(SupPid, ChildSpec)),
    catena_supervisor:stop(SupPid).

start_child_and_communicate_test() ->
    {ok, SupPid} = catena_supervisor:start_link(test_sup, []),
    ChildSpec = #{
        id => test_child,
        start => {test_worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [test_worker]
    },
    {ok, ChildPid} = catena_supervisor:start_child(SupPid, ChildSpec),
    ?assertEqual(#{}, catena_gen_server:call(ChildPid, get_state)),
    catena_supervisor:stop(SupPid).

which_children_test() ->
    {ok, SupPid} = catena_supervisor:start_link(test_sup, []),
    Children = catena_supervisor:which_children(SupPid),
    ?assertEqual(1, length(Children)),
    [{test_worker, _Pid, worker, [test_worker]}] = Children,
    catena_supervisor:stop(SupPid).

count_children_test() ->
    {ok, SupPid} = catena_supervisor:start_link(test_sup, []),
    Counts = catena_supervisor:count_children(SupPid),
    ?assertEqual(1, maps:get(specs, Counts)),
    ?assertEqual(1, maps:get(active, Counts)),
    ?assertEqual(1, maps:get(workers, Counts)),
    ?assertEqual(0, maps:get(supervisors, Counts)),
    catena_supervisor:stop(SupPid).

restart_child_test() ->
    {ok, SupPid} = catena_supervisor:start_link(test_sup, []),
    %% Get initial child pid
    [{test_worker, ChildPid1, worker, [test_worker]}] =
        catena_supervisor:which_children(SupPid),
    %% Restart the child
    ?assertMatch({ok, _Pid}, catena_supervisor:restart_child(SupPid, test_worker)),
    %% Verify new pid
    [{test_worker, ChildPid2, worker, [test_worker]}] =
        catena_supervisor:which_children(SupPid),
    ?assertNot(ChildPid1 =:= ChildPid2),
    catena_supervisor:stop(SupPid).

delete_child_test() ->
    {ok, SupPid} = catena_supervisor:start_link(test_sup, []),
    %% Verify child exists
    Children1 = catena_supervisor:which_children(SupPid),
    ?assertEqual(1, length(Children1)),
    %% Delete the child
    ok = catena_supervisor:delete_child(SupPid, test_worker),
    %% Verify child is gone
    Children2 = catena_supervisor:which_children(SupPid),
    ?assertEqual(0, length(Children2)),
    catena_supervisor:stop(SupPid).

terminate_child_test() ->
    {ok, SupPid} = catena_supervisor:start_link(test_sup, []),
    %% Verify child exists
    Children1 = catena_supervisor:which_children(SupPid),
    ?assertEqual(1, length(Children1)),
    %% Terminate the child
    ok = catena_supervisor:terminate_child(SupPid, test_worker),
    %% Verify child is gone
    Children2 = catena_supervisor:which_children(SupPid),
    ?assertEqual(0, length(Children2)),
    catena_supervisor:stop(SupPid).

%%%=============================================================================
%%% Child Restart Tests
%%%=============================================================================

child_auto_restart_test() ->
    %% Note: This test verifies that a crashed child is restarted
    {ok, SupPid} = catena_supervisor:start_link(test_sup, []),
    [{test_worker, ChildPid1, worker, [test_worker]}] =
        catena_supervisor:which_children(SupPid),
    %% Kill the child
    exit(ChildPid1, crash),
    timer:sleep(50),
    %% Verify child was restarted with new pid
    [{test_worker, ChildPid2, worker, [test_worker]}] =
        catena_supervisor:which_children(SupPid),
    ?assertNot(ChildPid1 =:= ChildPid2),
    ?assert(is_process_alive(ChildPid2)),
    catena_supervisor:stop(SupPid).

%%%=============================================================================
%%% Error Cases
%%%=============================================================================

start_duplicate_child_test() ->
    {ok, SupPid} = catena_supervisor:start_link(test_sup, []),
    ChildSpec = #{
        id => duplicate_test_worker,
        start => {test_worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [test_worker]
    },
    ?assertMatch({ok, _Pid}, catena_supervisor:start_child(SupPid, ChildSpec)),
    ?assertEqual({error, already_present}, catena_supervisor:start_child(SupPid, ChildSpec)),
    catena_supervisor:stop(SupPid).

restart_nonexistent_child_test() ->
    {ok, SupPid} = catena_supervisor:start_link(test_sup, []),
    ?assertEqual({error, not_found}, catena_supervisor:restart_child(SupPid, nonexistent)),
    catena_supervisor:stop(SupPid).

delete_nonexistent_child_test() ->
    {ok, SupPid} = catena_supervisor:start_link(test_sup, []),
    ?assertEqual({error, not_found}, catena_supervisor:delete_child(SupPid, nonexistent)),
    catena_supervisor:stop(SupPid).

start_child_missing_id_test() ->
    {ok, SupPid} = catena_supervisor:start_link(test_sup, []),
    ChildSpec = #{
        start => {test_worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [test_worker]
    },
    ?assertEqual({error, missing_id}, catena_supervisor:start_child(SupPid, ChildSpec)),
    catena_supervisor:stop(SupPid).
