%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_process (Phase 5.1.4 Process Primitives)
%%% @end
%%%-------------------------------------------------------------------
-module(catena_process_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Setup and Cleanup
%%%=============================================================================

setup() ->
    %% Clear any registered names
    lists:foreach(fun(N) -> catch unregister(N) end,
                  [test_proc1, test_proc2, test_reg]).

cleanup(_) ->
    %% Cleanup any spawned processes
    lists:foreach(fun(P) ->
        case is_process_alive(P) of
            true -> exit(P, kill);
            false -> ok
        end
    end, [P || P <- erlang:processes(), P =/= self()]),
    ok.

%%%=============================================================================
%%% Spawn Tests
%%%=============================================================================

spawn_test() ->
    Pid = catena_process:spawn(fun() -> 42 end),
    ?assert(is_process_alive(Pid)),
    ?assert(is_pid(Pid)).

spawn_with_result_test() ->
    Parent = self(),
    Pid = catena_process:spawn(fun() -> Parent ! result end),
    ?assertEqual(result, receive_result()).

spawn_link_test() ->
    Parent = self(),
    Child = catena_process:spawn_link(fun() ->
        %% Trap exit to avoid crashing parent
        process_flag(trap_exit, true),
        Parent ! child_started
    end),
    ?assert(is_process_alive(Child)),
    ?assert(lists:keymember(Child, 2, element(2, erlang:process_info(Child, links)))).

spawn_monitor_test() ->
    Parent = self(),
    {Pid, Ref} = catena_process:spawn_monitor(fun() ->
        Parent ! monitored
    end),
    ?assert(is_reference(Ref)),
    ?assertEqual(monitored, receive_result()),
    exit(Pid, normal),
    ?assertEqual({'DOWN', Ref, process, Pid, normal}, receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    after 1000 -> timeout
    end),
    %% Monitor automatically removed after DOWN
    ?assertError({badarg, _}, catena_process:demonitor(Ref)).

%%%=============================================================================
%%% Messaging Tests
%%%=============================================================================

send_test() ->
    Parent = self(),
    Pid = catena_process:spawn(fun() ->
        receive
            hello -> Parent ! got_hello
        end
    end),
    ?assertEqual(ok, catena_process:send(Pid, hello)),
    ?assertEqual(got_hello, receive_result()).

send_to_registered_test() ->
    Parent = self(),
    Pid = catena_process:spawn(fun() ->
        receive
            test_msg -> Parent ! got_test_msg
        end
    end),
    catena_process:register(test_reg, Pid),
    ?assertEqual(ok, catena_process:send(test_reg, test_msg)),
    ?assertEqual(got_test_msg, receive_result()),
    catena_process:unregister(test_reg).

send_to_dead_process_test() ->
    %% Spawn a process that dies immediately
    Pid = catena_process:spawn(fun() -> ok end),
    timer:sleep(10),  %% Let it finish
    %% Send to dead process should fail
    ?assertError({no_process, _}, catena_process:send(Pid, test)).

send_to_named_dead_process_test() ->
    %% Register and then kill process
    Pid = catena_process:spawn(fun() -> ok end),
    catena_process:register(temp_reg, Pid),
    timer:sleep(10),  %% Let it finish
    %% Send to dead registered process
    ?assertError({no_process, _}, catena_process:send(temp_reg, test)),
    catena_process:unregister(temp_reg).

%%%=============================================================================
%%% Link and Unlink Tests
%%%=============================================================================

link_test() ->
    Parent = self(),
    Child = catena_process:spawn(fun() ->
        process_flag(trap_exit, true),
        Parent ! child_ready,
        receive
            die -> ok
        end
    end),
    ?assertEqual(ok, catena_process:link(Child)),
    ?assertEqual(child_ready, receive_result()),
    %% Check link is bidirectional
    ?assert(lists:keymember(Child, 2, element(2, erlang:process_info(Child, links)))),
    ?assert(lists:keymember(self(), 2, element(2, erlang:process_info(self(), links)))),
    exit(Child, normal),
    ?assertEqual(normal, receive
        {'EXIT', Child, _} -> normal
    after 1000 -> timeout
    end).

unlink_test() ->
    Parent = self(),
    Child = catena_process:spawn(fun() ->
        process_flag(trap_exit, true),
        Parent ! child_ready
    end),
    catena_process:link(Child),
    ?assertEqual(child_ready, receive_result()),
    ?assertEqual(ok, catena_process:unlink(Child)),
    exit(Child, normal),
    %% Should not receive EXIT since unlinked
    receive
        {'EXIT', Child, _} -> ?assert(false, should_not_receive_exit)
    after 500 -> ok
    end.

%%%=============================================================================
%%% Monitor Tests
%%%=============================================================================

monitor_with_demonitor_test() ->
    Pid = catena_process:spawn(fun() -> timer:sleep(100) end),
    Ref = catena_process:monitor(Pid),
    ?assertEqual(ok, catena_process:demonitor(Ref)),
    exit(Pid, normal),
    %% Should not receive DOWN since demonitored
    receive
        {'DOWN', Ref, _, _, _} -> ?assert(false, should_not_receive_down)
    after 500 -> ok
    end.

%%%=============================================================================
%%% Process Information Tests
%%%=============================================================================

self_test() ->
    Pid = catena_process:self(),
    ?assertEqual(Pid, self()),
    ?assert(is_pid(Pid)).

whereis_existing_test() ->
    Pid = catena_process:spawn(fun() -> timer:sleep(100) end),
    catena_process:register(test_proc1, Pid),
    ?assertEqual(Pid, catena_process:whereis(test_proc1)),
    exit(Pid, kill),
    timer:sleep(10),
    ?assertEqual(undefined, catena_process:whereis(test_proc1)).

whereis_nonexistent_test() ->
    ?assertEqual(undefined, catena_process:whereis(nonexistent_proc)).

register_test() ->
    Pid = catena_process:spawn(fun() -> timer:sleep(100) end),
    ?assertEqual(ok, catena_process:register(test_proc2, Pid)),
    ?assertEqual(Pid, catena_process:whereis(test_proc2)),
    %% Duplicate registration should fail
    ?assertError({name_already_registered, test_proc2},
                 catena_process:register(test_proc2, Pid)),
    exit(Pid, kill).

unregister_test() ->
    Pid = catena_process:spawn(fun() -> timer:sleep(100) end),
    catena_process:register(test_proc3, Pid),
    ?assertEqual(ok, catena_process:unregister(test_proc3)),
    ?assertEqual(undefined, catena_process:whereis(test_proc3)).

is_process_alive_test() ->
    Pid = catena_process:spawn(fun() -> timer:sleep(50) end),
    ?assert(catena_process:is_process_alive(Pid)),
    timer:sleep(60),  %% Wait for it to finish
    ?assertNot(catena_process:is_process_alive(Pid)).

%%%=============================================================================
%%% Receive Tests
%%%=============================================================================

recv_basic_test() ->
    Parent = self(),
    Pid = catena_process:spawn(fun() ->
        Parent ! {self(), hello}
    end),
    ?assertEqual(hello, receive_result()),
    ?assertEqual({Pid, hello}, catena_process:recv(100, timeout)).

recv_with_timeout_test() ->
    %% Test timeout returns default
    Result = catena_process:recv(50, timeout),
    ?assertEqual(timeout, Result).

recv_with_predicate_test() ->
    Parent = self(),
    Pid = catena_process:spawn(fun() ->
        Parent ! important,
        Parent ! not_important
    end),
    %% Predicate to receive only 'important' messages
    Pred = fun
        (important) -> true;
        (_) -> false
    end,
    Result = catena_process:recv(Pred, 100, timeout),
    ?assertEqual(important, Result).

%%%=============================================================================
%%% Exit Handling Tests
%%%=============================================================================

trap_exit_true_test() ->
    Parent = self(),
    Child = catena_process:spawn_link(fun() ->
        catena_process:trap_exit(true),
        Parent ! trapping_enabled,
        receive
            {exit, _Reason} -> ok
        end
    end),
    ?assertEqual(trapping_enabled, receive_result()),
    exit(Child, test_reason),
    ?assertEqual(test_reason, receive
        {'EXIT', Child, _} -> test_reason
    after 500 -> timeout
    end).

trap_exit_false_test() ->
    %% When trap_exit is false, exit signals terminate the process
    Parent = self(),
    Child = catena_process:spawn_link(fun() ->
        catena_process:trap_exit(false),
        Parent ! not_trapping
    end),
    ?assertEqual(not_trapping, receive_result()),
    exit(Child, test_reason),
    %% Child should have exited
    ?assertNot(is_process_alive(Child)).

exit_test() ->
    Parent = self(),
    Pid = catena_process:spawn(fun() ->
        catena_process:trap_exit(true),
        receive
            die -> catena_process:exit(normal)
        end
    end),
    Pid ! die,
    ?assertEqual(normal, receive
        {'EXIT', Pid, _} -> normal
    after 500 -> timeout
    end).

exit_process_test() ->
    Parent = self(),
    Child = catena_process:spawn_link(fun() ->
        process_flag(trap_exit, true),
        Parent ! child_ready
    end),
    ?assertEqual(child_ready, receive_result()),
    ?assertEqual(ok, catena_process:exit(Child, test_reason)),
    ?assertEqual(test_reason, receive
        {'EXIT', Child, _} -> test_reason
    after 500 -> timeout
    end).

%%%=============================================================================
%%% Utility Functions
%%%=============================================================================

receive_result() ->
    receive
        Msg -> Msg
    after 500 -> timeout
    end.
