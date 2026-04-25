%% @doc Unit Tests for Phase 6.1: Process Testing Support
-module(catena_process_tests).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_process.hrl").
-include("../../src/proptest/catena_gen.hrl").

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%====================================================================
%% Section 6.1.1: Test Process Management
%%====================================================================

spawn_test_process_creates_process_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    %% Cleanup
    catena_process:stop_process(Proc),
    ok.

spawn_test_process_with_name_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun, #{name => test_proc_name}),
    ?assertEqual(test_proc_name, Proc#test_process.name),
    ?assertNotEqual(undefined, whereis(test_proc_name)),
    %% Cleanup
    catena_process:stop_process(Proc),
    ?assertEqual(undefined, whereis(test_proc_name)),
    ok.

spawn_test_process_without_link_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun, #{link => false}),
    ?assert(is_pid(Proc#test_process.pid)),
    ?assert(is_process_alive(Proc#test_process.pid)),
    %% Cleanup
    catena_process:stop_process(Proc),
    ok.

with_process_auto_cleanup_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    TestFun = fun(Proc) ->
        Pid = Proc#test_process.pid,
        ?assert(is_process_alive(Pid)),
        ok
    end,
    catena_process:with_process(Fun, TestFun),
    %% Process should be cleaned up automatically
    ok.

stop_process_terminates_gracefully_test() ->
    Fun = fun() ->
        receive
            stop -> ok
        after infinity -> ok
        end
    end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    ?assert(is_process_alive(Pid)),
    catena_process:stop_process(Proc),
    ?assertNot(is_process_alive(Pid)),
    ok.

kill_process_terminates_forcibly_test() ->
    Fun = fun() ->
        receive
            _ -> ok
        after infinity -> ok
        end
    end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    ?assert(is_process_alive(Pid)),
    catena_process:kill_process(Proc),
    ?assertNot(is_process_alive(Pid)),
    ok.

%%====================================================================
%% Section 6.1.2: Process State Inspection
%%====================================================================

process_info_safe_alive_process_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    Info = catena_process:process_info_safe(Pid, [message_queue_len]),
    ?assertMatch(#{message_queue_len := _}, Info),
    catena_process:stop_process(Proc),
    ok.

process_info_safe_dead_process_test() ->
    %% Use a pid that doesn't exist
    FakePid = list_to_pid("<0.9999.0>"),
    Info = catena_process:process_info_safe(FakePid, [memory]),
    ?assertMatch({error, _}, Info),
    ok.

get_state_returns_genserver_state_test() ->
    {ok, Pid} = gen_server:start_link(?MODULE, #{count => 3}, []),
    ?assertEqual(#{count => 3}, catena_process:get_state(Pid)),
    gen_server:stop(Pid),
    ok.

message_queue_empty_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    {ok, QLen} = catena_process:message_queue(Pid),
    ?assertEqual(0, QLen),
    catena_process:stop_process(Proc),
    ok.

message_queue_with_messages_test() ->
    Fun = fun() ->
        receive
            stop -> ok
        after infinity -> ok
        end
    end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    Pid ! msg1,
    Pid ! msg2,
    timer:sleep(100),  %% Give time for messages to be delivered
    {ok, QLen} = catena_process:message_queue(Pid),
    ?assert(QLen >= 2),
    catena_process:stop_process(Proc),
    ok.

process_memory_returns_number_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    {ok, Mem} = catena_process:process_memory(Pid),
    ?assert(is_integer(Mem)),
    ?assert(Mem > 0),
    catena_process:stop_process(Proc),
    ok.

full_state_returns_complete_info_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    State = catena_process:full_state(Pid),
    ?assertMatch(#process_state{pid = Pid}, State),
    ?assert(is_integer(State#process_state.memory)),
    ?assert(is_integer(State#process_state.heap_size)),
    catena_process:stop_process(Proc),
    ok.

%%====================================================================
%% Section 6.1.3: Process Generators
%%====================================================================

gen_pid_returns_pid_test() ->
    Gen = catena_process:gen_pid(),
    Seed = #seed{state = 42},
    {Pid, _Seed} = Gen(Seed),
    ?assert(is_pid(Pid)),
    ok.

gen_ref_returns_unique_reference_test() ->
    Gen = catena_process:gen_ref(),
    Seed1 = #seed{state = 42},
    Seed2 = #seed{state = 43},
    {Ref1, _} = Gen(Seed1),
    {Ref2, _} = Gen(Seed2),
    ?assert(is_reference(Ref1)),
    ?assert(is_reference(Ref2)),
    ?assertNot(Ref1 =:= Ref2),
    ok.

gen_node_returns_node_name_test() ->
    Gen = catena_process:gen_node(),
    Seed = #seed{state = 42},
    {Node, _} = Gen(Seed),
    ?assert(is_atom(Node)),
    ok.

gen_registered_name_returns_atom_test() ->
    Gen = catena_process:gen_registered_name(),
    Seed = #seed{state = 42},
    {Name, _} = Gen(Seed),
    ?assert(is_atom(Name)),
    ok.

gen_timeout_returns_timeout_value_test() ->
    Gen = catena_process:gen_timeout(),
    Seed = #seed{state = 42},
    {Timeout1, _} = Gen(Seed),
    ?assert(is_integer(Timeout1) orelse Timeout1 =:= infinity),
    ok.

gen_exit_reason_returns_term_test() ->
    Gen = catena_process:gen_exit_reason(),
    Seed = #seed{state = 42},
    {Reason, _} = Gen(Seed),
    ?assertNot(Reason =:= undefined),
    ok.

%%====================================================================
%% Section 6.1.4: Process Assertions
%%====================================================================

assert_alive_passes_for_live_process_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    ?assertEqual(ok, catena_process:assert_alive(Proc)),
    catena_process:stop_process(Proc),
    ok.

assert_alive_fails_for_dead_process_test() ->
    FakePid = list_to_pid("<0.9999.0>"),
    Result = catena_process:assert_alive(FakePid),
    ?assertMatch({error, {process_not_alive, _}}, Result),
    ok.

assert_dead_passes_for_dead_process_test() ->
    FakePid = list_to_pid("<0.9999.0>"),
    ?assertEqual(ok, catena_process:assert_dead(FakePid)),
    ok.

assert_dead_fails_for_live_process_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Result = catena_process:assert_dead(Proc),
    ?assertMatch({error, {process_still_alive, _}}, Result),
    catena_process:stop_process(Proc),
    ok.

assert_exit_reason_tracks_shutdown_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    catena_process:stop_process(Proc),
    ?assertEqual(ok, catena_process:assert_exit_reason(Pid, shutdown)),
    ok.

assert_exit_reason_detects_mismatch_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    catena_process:kill_process(Proc),
    Result = catena_process:assert_exit_reason(Pid, shutdown),
    ?assertMatch({error, {unexpected_exit_reason, shutdown, killed}}, Result),
    ok.

assert_no_messages_passes_for_empty_queue_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    ?assertEqual(ok, catena_process:assert_no_messages(Pid)),
    catena_process:stop_process(Proc),
    ok.

assert_no_messages_fails_for_nonempty_queue_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    Pid ! test_message,
    Result = catena_process:assert_no_messages(Pid),
    ?assertMatch({error, {messages_in_queue, _}}, Result),
    catena_process:stop_process(Proc),
    ok.

assert_message_count_passes_for_correct_count_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    Pid ! msg1,
    Pid ! msg2,
    ?assertEqual(ok, catena_process:assert_message_count(Pid, 2)),
    catena_process:stop_process(Proc),
    ok.

assert_message_count_fails_for_wrong_count_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    Result = catena_process:assert_message_count(Pid, 5),
    ?assertMatch({error, {wrong_message_count, 5, _}}, Result),
    catena_process:stop_process(Proc),
    ok.

assert_registered_fails_for_unregistered_name_test() ->
    Result = catena_process:assert_registered(nonexistent_process),
    ?assertMatch({error, {process_not_registered, _}}, Result),
    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

process_lifecycle_full_test() ->
    %% 1. Spawn process
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,

    %% 2. Verify alive
    ?assertEqual(ok, catena_process:assert_alive(Pid)),

    %% 3. Send messages
    Pid ! msg1,
    Pid ! msg2,
    ?assertEqual(ok, catena_process:assert_message_count(Pid, 2)),

    %% 4. Get state info
    {ok, Mem} = catena_process:process_memory(Pid),
    ?assert(Mem > 0),

    %% 5. Stop process
    catena_process:stop_process(Pid),

    %% 6. Verify dead
    ?assertEqual(ok, catena_process:assert_dead(Pid)),
    ok.

cleanup_processes_handles_multiple_processes_test() ->
    %% Spawn multiple processes
    Procs = [catena_process:spawn_test_process(
                fun() -> receive after infinity -> ok end end)
             || _ <- lists:seq(1, 3)],
    Pids = [P#test_process.pid || P <- Procs],

    %% All should be alive
    ?assert(lists:all(fun is_process_alive/1, Pids)),

    %% Cleanup
    ?assertEqual(ok, catena_process:cleanup_processes()),

    %% All should be dead
    ?assertNot(lists:any(fun is_process_alive/1, Pids)),
    ok.

%%====================================================================
%% gen_server callbacks for test support
%%====================================================================

init(State) ->
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
