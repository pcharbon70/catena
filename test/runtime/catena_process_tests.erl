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
    clear_registered_names(),
    flush_mailbox(),
    ok.

cleanup(_) ->
    clear_registered_names(),
    flush_mailbox(),
    ok.

isolated(Test) ->
    fun() ->
        setup(),
        try
            Test()
        after
            cleanup(ok)
        end
    end.

process_test_() ->
    [
        {"spawn", isolated(fun spawn_case/0)},
        {"spawn with result", isolated(fun spawn_with_result_case/0)},
        {"spawn link", isolated(fun spawn_link_case/0)},
        {"spawn monitor", isolated(fun spawn_monitor_case/0)},
        {"send", isolated(fun send_case/0)},
        {"send to registered process", isolated(fun send_to_registered_case/0)},
        {"send to dead process", isolated(fun send_to_dead_process_case/0)},
        {"send to named dead process", isolated(fun send_to_named_dead_process_case/0)},
        {"link", isolated(fun link_case/0)},
        {"unlink", isolated(fun unlink_case/0)},
        {"monitor and demonitor", isolated(fun monitor_with_demonitor_case/0)},
        {"self", isolated(fun self_case/0)},
        {"whereis existing", isolated(fun whereis_existing_case/0)},
        {"whereis nonexistent", isolated(fun whereis_nonexistent_case/0)},
        {"register", isolated(fun register_case/0)},
        {"unregister", isolated(fun unregister_case/0)},
        {"is process alive", isolated(fun is_process_alive_case/0)},
        {"receive basic", isolated(fun recv_basic_case/0)},
        {"receive timeout", isolated(fun recv_with_timeout_case/0)},
        {"receive predicate", isolated(fun recv_with_predicate_case/0)},
        {"trap exits", isolated(fun trap_exit_true_case/0)},
        {"do not trap exits", isolated(fun trap_exit_false_case/0)},
        {"exit self", isolated(fun exit_case/0)},
        {"exit process", isolated(fun exit_process_case/0)}
    ].

%%%=============================================================================
%%% Spawn Tests
%%%=============================================================================

spawn_case() ->
    Pid = catena_process:spawn(fun() -> 42 end),
    ?assert(is_process_alive(Pid)),
    ?assert(is_pid(Pid)).

spawn_with_result_case() ->
    Parent = self(),
    Pid = catena_process:spawn(fun() -> Parent ! result end),
    ?assertEqual(result, receive_result(result)).

spawn_link_case() ->
    Parent = self(),
    Child = catena_process:spawn_link(fun() ->
        %% Trap exit to avoid crashing parent
        process_flag(trap_exit, true),
        Parent ! child_started
    end),
    ?assert(is_process_alive(Child)),
    ?assert(lists:keymember(Child, 2, element(2, erlang:process_info(Child, links)))).

spawn_monitor_case() ->
    Parent = self(),
    {Pid, Ref} = catena_process:spawn_monitor(fun() ->
        Parent ! monitored
    end),
    ?assert(is_reference(Ref)),
    ?assertEqual(monitored, receive_result(monitored)),
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

send_case() ->
    Parent = self(),
    Pid = catena_process:spawn(fun() ->
        receive
            hello -> Parent ! got_hello
        end
    end),
    ?assertEqual(ok, catena_process:send(Pid, hello)),
    ?assertEqual(got_hello, receive_result(got_hello)).

send_to_registered_case() ->
    Parent = self(),
    Pid = catena_process:spawn(fun() ->
        receive
            test_msg -> Parent ! got_test_msg
        end
    end),
    catena_process:register(test_reg, Pid),
    ?assertEqual(ok, catena_process:send(test_reg, test_msg)),
    ?assertEqual(got_test_msg, receive_result(got_test_msg)),
    catena_process:unregister(test_reg).

send_to_dead_process_case() ->
    %% Spawn a process that dies immediately
    Pid = catena_process:spawn(fun() -> ok end),
    timer:sleep(10),  %% Let it finish
    %% Send to dead process should fail
    ?assertError({no_process, _}, catena_process:send(Pid, test)).

send_to_named_dead_process_case() ->
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

link_case() ->
    Parent = self(),
    Child = catena_process:spawn(fun() ->
        process_flag(trap_exit, true),
        Parent ! child_ready,
        receive
            die -> ok
        end
    end),
    ?assertEqual(ok, catena_process:link(Child)),
    ?assertEqual(child_ready, receive_result(child_ready)),
    %% Check link is bidirectional
    ?assert(lists:keymember(Child, 2, element(2, erlang:process_info(Child, links)))),
    ?assert(lists:keymember(self(), 2, element(2, erlang:process_info(self(), links)))),
    exit(Child, normal),
    ?assertEqual(normal, receive
        {'EXIT', Child, _} -> normal
    after 1000 -> timeout
    end).

unlink_case() ->
    Parent = self(),
    Child = catena_process:spawn(fun() ->
        process_flag(trap_exit, true),
        Parent ! child_ready
    end),
    catena_process:link(Child),
    ?assertEqual(child_ready, receive_result(child_ready)),
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

monitor_with_demonitor_case() ->
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

self_case() ->
    Pid = catena_process:self(),
    ?assertEqual(Pid, self()),
    ?assert(is_pid(Pid)).

whereis_existing_case() ->
    Pid = catena_process:spawn(fun() -> timer:sleep(100) end),
    catena_process:register(test_proc1, Pid),
    ?assertEqual(Pid, catena_process:whereis(test_proc1)),
    exit(Pid, kill),
    timer:sleep(10),
    ?assertEqual(undefined, catena_process:whereis(test_proc1)).

whereis_nonexistent_case() ->
    ?assertEqual(undefined, catena_process:whereis(nonexistent_proc)).

register_case() ->
    Pid = catena_process:spawn(fun() -> timer:sleep(100) end),
    ?assertEqual(ok, catena_process:register(test_proc2, Pid)),
    ?assertEqual(Pid, catena_process:whereis(test_proc2)),
    %% Duplicate registration should fail
    ?assertError({name_already_registered, test_proc2},
                 catena_process:register(test_proc2, Pid)),
    exit(Pid, kill).

unregister_case() ->
    Pid = catena_process:spawn(fun() -> timer:sleep(100) end),
    catena_process:register(test_proc3, Pid),
    ?assertEqual(ok, catena_process:unregister(test_proc3)),
    ?assertEqual(undefined, catena_process:whereis(test_proc3)).

is_process_alive_case() ->
    Pid = catena_process:spawn(fun() -> timer:sleep(50) end),
    ?assert(catena_process:is_process_alive(Pid)),
    timer:sleep(60),  %% Wait for it to finish
    ?assertNot(catena_process:is_process_alive(Pid)).

%%%=============================================================================
%%% Receive Tests
%%%=============================================================================

recv_basic_case() ->
    Parent = self(),
    Pid = catena_process:spawn(fun() ->
        Parent ! {self(), hello}
    end),
    ?assertEqual(hello, receive_result(hello)),
    ?assertEqual({Pid, hello}, catena_process:recv(100, timeout)).

recv_with_timeout_case() ->
    %% Test timeout returns default
    Result = catena_process:recv(50, timeout),
    ?assertEqual(timeout, Result).

recv_with_predicate_case() ->
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

trap_exit_true_case() ->
    Parent = self(),
    Child = catena_process:spawn_link(fun() ->
        catena_process:trap_exit(true),
        Parent ! trapping_enabled,
        receive
            {exit, _Reason} -> ok
        end
    end),
    ?assertEqual(trapping_enabled, receive_result(trapping_enabled)),
    exit(Child, test_reason),
    ?assertEqual(test_reason, receive
        {'EXIT', Child, _} -> test_reason
    after 500 -> timeout
    end).

trap_exit_false_case() ->
    %% When trap_exit is false, exit signals terminate the process
    Parent = self(),
    Child = catena_process:spawn_link(fun() ->
        catena_process:trap_exit(false),
        Parent ! not_trapping
    end),
    ?assertEqual(not_trapping, receive_result(not_trapping)),
    exit(Child, test_reason),
    %% Child should have exited
    ?assertNot(is_process_alive(Child)).

exit_case() ->
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

exit_process_case() ->
    Parent = self(),
    Child = catena_process:spawn_link(fun() ->
        process_flag(trap_exit, true),
        Parent ! child_ready
    end),
    ?assertEqual(child_ready, receive_result(child_ready)),
    ?assertEqual(ok, catena_process:exit(Child, test_reason)),
    ?assertEqual(test_reason, receive
        {'EXIT', Child, _} -> test_reason
    after 500 -> timeout
    end).

%%%=============================================================================
%%% Utility Functions
%%%=============================================================================

receive_result(Expected) ->
    receive
        Expected -> Expected;
        {_Pid, Expected} -> Expected
    after 500 -> timeout
    end.

clear_registered_names() ->
    lists:foreach(
        fun(Name) ->
            case erlang:whereis(Name) of
                undefined -> ok;
                _Pid -> erlang:unregister(Name)
            end
        end,
        [test_proc1, test_proc2, test_proc3, test_reg, temp_reg]
    ).

flush_mailbox() ->
    receive
        _ -> flush_mailbox()
    after 0 ->
        ok
    end.
