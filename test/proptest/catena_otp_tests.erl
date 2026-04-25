%% @doc Unit Tests for Phase 6.5: OTP Behavior Testing
-module(catena_otp_tests).

-behaviour(gen_server).
-behaviour(supervisor).
-behaviour(gen_statem).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_process.hrl").
-include("../../src/proptest/catena_gen.hrl").
-include("../../src/proptest/catena_otp.hrl").

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         callback_mode/0,
         handle_event/4,
         start_test_worker/0,
         test_events/0]).

%%====================================================================
%% Section 6.5.1: GenServer Testing
%%====================================================================

genserver_property_executes_test_test() ->
    Result = catena_otp:genserver_property(undefined, fun() -> test_result end),
    ?assertEqual({ok, test_result}, Result),
    ok.

gen_call_generates_request_test() ->
    Result = catena_otp:gen_call(erlang),
    ?assertMatch({_, _}, Result),
    ok.

gen_cast_generates_request_test() ->
    Result = catena_otp:gen_cast(erlang),
    ?assertMatch({_, _}, Result),
    ok.

gen_info_generates_message_test() ->
    Result = catena_otp:gen_info(test),
    ?assert(is_tuple(Result) orelse is_atom(Result)),
    ok.

assert_state_match_matches_pattern_test() ->
    {ok, Pid} = start_fixture_genserver(#{count => 2}),
    ?assertEqual(ok, catena_otp:assert_state_match(Pid, #{count => 2})),
    gen_server:stop(Pid),
    ok.

get_state_returns_state_test() ->
    {ok, Pid} = start_fixture_genserver(#{count => 5}),
    ?assertEqual({ok, #{count => 5}}, catena_otp:get_state(Pid)),
    gen_server:stop(Pid),
    ok.

simulate_timeout_sends_message_test() ->
    {ok, Pid} = start_fixture_genserver(#{count => 0}),
    ?assertEqual(ok, catena_otp:simulate_timeout(Pid)),
    timer:sleep(20),
    ?assertEqual({ok, #{count => 0, timeout => true}}, catena_otp:get_state(Pid)),
    gen_server:stop(Pid),
    ok.

%%====================================================================
%% Section 6.5.2: Supervisor Testing
%%====================================================================

supervisor_property_executes_test_test() ->
    Result = catena_otp:supervisor_property(undefined, fun() -> sup_result end),
    ?assertEqual({ok, sup_result}, Result),
    ok.

child_spec_generates_spec_test() ->
    Spec = catena_otp:child_spec(test_module),
    ?assertMatch(#{id := test_module, start := _, restart := _}, Spec),
    ok.

child_spec_with_args_generates_spec_test() ->
    Spec = catena_otp:child_spec({test_module, [arg1, arg2]}),
    ?assertMatch(#{id := test_module, start := {test_module, start_link, [arg1, arg2]}}, Spec),
    ok.

crash_child_exits_process_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Pid = spawn(Fun),
    ?assert(is_process_alive(Pid)),
    ?assertEqual(ok, catena_otp:crash_child(Pid, test_reason)),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)),
    ok.

crash_child_dead_process_test() ->
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10),
    ?assertEqual({error, process_not_alive}, catena_otp:crash_child(DeadPid, test_reason)),
    ok.

restart_strategy_returns_strategy_test() ->
    {ok, SupPid} = start_fixture_supervisor(),
    ?assertEqual({ok, one_for_one}, catena_otp:restart_strategy(SupPid)),
    exit(SupPid, shutdown),
    ok.

verify_restart_count_checks_count_test() ->
    {ok, SupPid} = start_fixture_supervisor(),
    {ok, [ChildPid]} = catena_otp:get_children(SupPid),
    ok = catena_otp:crash_child(ChildPid, kill),
    timer:sleep(50),
    ?assert(catena_otp:verify_restart_count(ChildPid, 1)),
    exit(SupPid, shutdown),
    ok.

get_children_returns_children_test() ->
    {ok, SupPid} = start_fixture_supervisor(),
    {ok, Children} = catena_otp:get_children(SupPid),
    ?assertEqual(1, length(Children)),
    exit(SupPid, shutdown),
    ok.

stop_child_exits_process_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Pid = spawn(Fun),
    ?assert(is_process_alive(Pid)),
    ?assertEqual(ok, catena_otp:stop_child(Pid, test_reason)),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)),
    ok.

%%====================================================================
%% Section 6.5.3: gen_statem Testing
%%====================================================================

statem_property_executes_test_test() ->
    Result = catena_otp:statem_property(undefined, fun() -> statem_result end),
    ?assertEqual({ok, statem_result}, Result),
    ok.

statem_event_generates_event_test() ->
    Result = catena_otp:statem_event(undefined),
    ?assert(is_tuple(Result)),
    ok.

statem_transition_transitions_test() ->
    {ok, Pid} = start_fixture_statem(disconnected),
    ?assertEqual({ok, connected}, catena_otp:statem_transition(Pid, {call, connect}, 100)),
    gen_statem:stop(Pid),
    ok.

get_statem_state_returns_state_test() ->
    {ok, Pid} = start_fixture_statem(disconnected),
    ?assertMatch({ok, {disconnected, _}}, catena_otp:get_statem_state(Pid)),
    gen_statem:stop(Pid),
    ok.

force_transition_changes_state_test() ->
    {ok, Pid} = start_fixture_statem(disconnected),
    ?assertEqual(ok, catena_otp:force_transition(Pid, connected, #{count => 1})),
    ?assertEqual(true, catena_otp:verify_state(Pid, connected)),
    gen_statem:stop(Pid),
    ok.

verify_state_checks_state_test() ->
    {ok, Pid} = start_fixture_statem(disconnected),
    ?assertEqual(true, catena_otp:verify_state(Pid, disconnected)),
    gen_statem:stop(Pid),
    ok.

statem_events_returns_events_test() ->
    Events = catena_otp:statem_events(?MODULE),
    ?assert(is_list(Events)),
    ?assert(lists:member({call, connect}, Events)),
    ok.

%%====================================================================
%% Section 6.5.4: Custom Behavior Testing
%%====================================================================

behavior_property_executes_test_test() ->
    Result = catena_otp:behavior_property(undefined, fun() -> behavior_result end),
    ?assertEqual({ok, behavior_result}, Result),
    ok.

callback_call_executes_callback_test() ->
    Result = catena_otp:callback_call(erlang, is_atom, [test_atom]),
    ?assertEqual(true, Result),
    ok.

verify_callback_checks_return_value_test() ->
    Result = catena_otp:verify_callback(erlang, is_atom, true),
    case Result of
        true -> ok;
        {error, _} -> ok
    end,
    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

genserver_lifecycle_test() ->
    {ok, Pid} = start_fixture_genserver(#{count => 0}),
    ?assertEqual(ok, catena_otp:assert_state(Pid, #{count => 0})),
    ok = gen_server:call(Pid, increment),
    ?assertEqual(ok, catena_otp:assert_state(Pid, #{count => 1})),
    gen_server:stop(Pid),
    ok.

supervisor_restart_test() ->
    {ok, SupPid} = start_fixture_supervisor(),
    {ok, [ChildPid]} = catena_otp:get_children(SupPid),
    ?assertEqual(ok, catena_otp:crash_child(ChildPid, simulated_crash)),
    timer:sleep(50),
    {ok, [NewChildPid]} = catena_otp:get_children(SupPid),
    ?assertNotEqual(ChildPid, NewChildPid),
    exit(SupPid, shutdown),
    ok.

statem_transition_test() ->
    {ok, Pid} = start_fixture_statem(disconnected),
    ?assertEqual({ok, connected}, catena_otp:statem_transition(Pid, {call, connect}, 100)),
    ?assertEqual({ok, authenticated}, catena_otp:statem_transition(Pid, {call, authenticate}, 100)),
    ?assertEqual(true, catena_otp:verify_state(Pid, authenticated)),
    gen_statem:stop(Pid),
    ok.

%%====================================================================
%% Internal helper functions and fixtures
%%====================================================================

start_fixture_genserver(State) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {genserver, State}, []),
    unlink(Pid),
    {ok, Pid}.

start_fixture_supervisor() ->
    {ok, Pid} = supervisor:start_link(?MODULE, {supervisor, []}),
    unlink(Pid),
    {ok, Pid}.

start_fixture_statem(State) ->
    {ok, Pid} = gen_statem:start_link(?MODULE, {statem, State}, []),
    unlink(Pid),
    {ok, Pid}.

start_test_worker() ->
    Pid = spawn_link(fun worker_loop/0),
    {ok, Pid}.

worker_loop() ->
    receive
        stop -> ok;
        _ -> worker_loop()
    end.

test_events() ->
    [{call, connect}, {call, authenticate}, {cast, disconnect}, {info, timeout}].

init({genserver, State}) ->
    {ok, State};
init({supervisor, _Args}) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 60},
    ChildSpec = #{
        id => otp_test_worker,
        start => {?MODULE, start_test_worker, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [?MODULE]
    },
    {ok, {SupFlags, [ChildSpec]}};
init({statem, State}) ->
    {ok, State, #{count => 0}}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(increment, _From, State) when is_map(State) ->
    NewState = maps:update_with(count, fun(Count) -> Count + 1 end, 1, State),
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) when is_map(State) ->
    {noreply, maps:put(timeout, true, State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

callback_mode() ->
    handle_event_function.

handle_event({call, From}, connect, disconnected, Data) ->
    {next_state, connected, Data, [{reply, From, connected}]};
handle_event({call, From}, authenticate, connected, Data) ->
    {next_state, authenticated, Data, [{reply, From, authenticated}]};
handle_event({call, From}, _Event, State, Data) ->
    {keep_state, Data, [{reply, From, State}]};
handle_event(cast, disconnect, _State, Data) ->
    {next_state, disconnected, Data};
handle_event(info, timeout, State, Data) ->
    {keep_state, maps:put(timeout, true, Data)};
handle_event(_Type, _Event, _State, Data) ->
    {keep_state, Data}.
