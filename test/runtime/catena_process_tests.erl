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
        {"native OTP call", isolated(fun native_otp_call_case/0)},
        {"native OTP call to dead process", isolated(fun native_otp_call_dead_case/0)},
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
    _Pid = catena_process:spawn(fun() -> Parent ! result end),
    ?assertEqual(result, receive_result(result)).

spawn_link_case() ->
    Parent = self(),
    Child = catena_process:spawn_link(fun() ->
        Parent ! {child_started, self()},
        receive
            stop -> ok
        end
    end),
    ?assertEqual({child_started, Child}, receive_message()),
    ?assert(lists:member(Child, process_links(self()))),
    ?assert(lists:member(self(), process_links(Child))),
    Child ! stop.

spawn_monitor_case() ->
    Parent = self(),
    {Pid, Ref} = catena_process:spawn_monitor(fun() ->
        Parent ! monitored
    end),
    ?assert(is_reference(Ref)),
    ?assertEqual(monitored, receive_result(monitored)),
    ?assertEqual({'DOWN', Ref, process, Pid, normal}, receive
        {'DOWN', Ref, process, Pid, normal} = Down -> Down
    after 1000 -> timeout
    end),
    %% Demonitor remains a safe no-op after DOWN has already removed it.
    ?assertEqual(ok, catena_process:demonitor(Ref)).

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
    {Pid, Ref} = catena_process:spawn_monitor(fun() -> ok end),
    ?assertMatch({'DOWN', Ref, process, Pid, normal}, receive_message()),
    %% Native PID sends are asynchronous and succeed even after the target dies.
    ?assertEqual(ok, catena_process:send(Pid, test)).

send_to_named_dead_process_case() ->
    Parent = self(),
    Pid = catena_process:spawn(fun() ->
        Parent ! registered_process_ready,
        receive
            stop -> ok
        end
    end),
    catena_process:register(temp_reg, Pid),
    ?assertEqual(registered_process_ready, receive_result(registered_process_ready)),
    Ref = catena_process:monitor(Pid),
    Pid ! stop,
    ?assertMatch({'DOWN', Ref, process, Pid, normal}, receive_message()),
    %% BEAM automatically removes the registered name when the process exits.
    ?assertError({no_process, _}, catena_process:send(temp_reg, test)),
    ?assertEqual(ok, catena_process:unregister(temp_reg)).

native_otp_call_case() ->
    {ok, Server} = gen_server:start_link(test_gen_server, [{value, 41}], []),
    try
        ?assertEqual(41, catena_process:call(Server, get_value)),
        ?assertEqual(42, catena_process:call(Server, {add, 1}, 1000))
    after
        gen_server:stop(Server)
    end.

native_otp_call_dead_case() ->
    {Pid, Ref} = catena_process:spawn_monitor(fun() -> ok end),
    ?assertMatch({'DOWN', Ref, process, Pid, normal}, receive_message()),
    ?assertError({no_process, Pid}, catena_process:call(Pid, get_value, 100)).

%%%=============================================================================
%%% Link and Unlink Tests
%%%=============================================================================

link_case() ->
    with_trap_exit(fun() ->
        Parent = self(),
        Child = catena_process:spawn(fun() ->
            Parent ! child_ready,
            receive
                wait -> ok
            end
        end),
        ?assertEqual(ok, catena_process:link(Child)),
        ?assertEqual(child_ready, receive_result(child_ready)),
        ?assert(lists:member(Child, process_links(self()))),
        ?assert(lists:member(self(), process_links(Child))),
        ?assertEqual(ok, catena_process:exit(Child, test_reason)),
        ?assertEqual({'EXIT', Child, test_reason}, receive_message())
    end).

unlink_case() ->
    with_trap_exit(fun() ->
        Parent = self(),
        Child = catena_process:spawn(fun() ->
            Parent ! child_ready,
            receive
                wait -> ok
            end
        end),
        ?assertEqual(ok, catena_process:link(Child)),
        ?assertEqual(child_ready, receive_result(child_ready)),
        ?assertEqual(ok, catena_process:unlink(Child)),
        ?assertEqual(ok, catena_process:exit(Child, test_reason)),
        receive
            {'EXIT', Child, _} -> ?assert(false, should_not_receive_exit)
        after 100 -> ok
        end
    end).

%%%=============================================================================
%%% Monitor Tests
%%%=============================================================================

monitor_with_demonitor_case() ->
    Pid = catena_process:spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Ref = catena_process:monitor(Pid),
    ?assertEqual(ok, catena_process:demonitor(Ref)),
    Pid ! stop,
    %% Should not receive DOWN since demonitored
    receive
        {'DOWN', Ref, _, _, _} -> ?assert(false, should_not_receive_down)
    after 100 -> ok
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
    ?assertEqual({Pid, hello}, catena_process:recv(100, timeout)).

recv_with_timeout_case() ->
    %% Test timeout returns default
    Result = catena_process:recv(50, timeout),
    ?assertEqual(timeout, Result).

recv_with_predicate_case() ->
    Parent = self(),
    _Pid = catena_process:spawn(fun() ->
        Parent ! not_important,
        Parent ! important
    end),
    %% Predicate to receive only 'important' messages
    Pred = fun
        (important) -> true;
        (_) -> false
    end,
    Result = catena_process:recv(Pred, 100, timeout),
    ?assertEqual(important, Result),
    ?assertEqual(not_important, catena_process:recv(100, timeout)).

%%%=============================================================================
%%% Exit Handling Tests
%%%=============================================================================

trap_exit_true_case() ->
    with_trap_exit(fun() ->
        Child = catena_process:spawn_link(fun() ->
            receive
                wait -> ok
            end
        end),
        ?assertEqual(ok, catena_process:exit(Child, test_reason)),
        ?assertEqual({'EXIT', Child, test_reason}, receive_message())
    end).

trap_exit_false_case() ->
    Parent = self(),
    {Child, Ref} = catena_process:spawn_monitor(fun() ->
        Previous = catena_process:trap_exit(false),
        Parent ! {not_trapping, Previous},
        receive
            wait -> ok
        end
    end),
    ?assertEqual({not_trapping, false}, receive_message()),
    ?assertEqual(ok, catena_process:exit(Child, test_reason)),
    ?assertEqual({'DOWN', Ref, process, Child, test_reason}, receive_message()).

exit_case() ->
    {Pid, Ref} = catena_process:spawn_monitor(fun() ->
        receive
            die -> catena_process:exit(normal)
        end
    end),
    Pid ! die,
    ?assertEqual({'DOWN', Ref, process, Pid, normal}, receive_message()).

exit_process_case() ->
    Parent = self(),
    {Child, Ref} = catena_process:spawn_monitor(fun() ->
        Parent ! child_ready,
        receive
            wait -> ok
        end
    end),
    ?assertEqual(child_ready, receive_result(child_ready)),
    ?assertEqual(ok, catena_process:exit(Child, test_reason)),
    ?assertEqual({'DOWN', Ref, process, Child, test_reason}, receive_message()).

%%%=============================================================================
%%% Utility Functions
%%%=============================================================================

receive_result(Expected) ->
    receive
        Expected -> Expected;
        {_Pid, Expected} -> Expected
    after 500 -> timeout
    end.

receive_message() ->
    receive
        Message -> Message
    after 1000 ->
        timeout
    end.

process_links(Pid) ->
    {links, Links} = erlang:process_info(Pid, links),
    Links.

with_trap_exit(Test) ->
    Previous = catena_process:trap_exit(true),
    try
        Test()
    after
        _ = catena_process:trap_exit(Previous)
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
