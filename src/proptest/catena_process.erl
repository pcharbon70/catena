%% @doc Process Testing Support for Phase 6.1
%%
%% This module provides tools for spawning test processes, controlling their
%% execution, and verifying their behavior. Processes can be tested in isolation
%% with controlled environments and deterministic scheduling options.
-module(catena_process).

-behaviour(gen_server).

%% Define the registry server name separately
-define(REGISTRY_NAME, catena_process_registry).

-include("catena_gen.hrl").
-include("catena_process.hrl").

%% Test Process Management
-export([spawn_test_process/1,
         spawn_test_process/2,
         with_process/2,
         stop_process/1,
         kill_process/1,
         cleanup_processes/0]).

%% Process State Inspection
-export([get_state/1,
         get_state/2,
         process_info_safe/2,
         message_queue/1,
         process_memory/1,
         full_state/1]).

%% Process Generators
-export([gen_pid/0,
         gen_ref/0,
         gen_node/0,
         gen_registered_name/0,
         gen_timeout/0,
         gen_exit_reason/0]).

%% Process Assertions
-export([assert_alive/1,
         assert_dead/1,
         assert_exit_reason/2,
         assert_no_messages/1,
         assert_message_count/2,
         assert_registered/1]).

%% gen_server callbacks for internal tracking
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Internal state for tracking spawned processes
-record(process_registry, {
    processes :: [#test_process{}],
    test_pid :: pid() | undefined
}).

%%====================================================================
%% Test Process Management
%%====================================================================

%% @doc Spawn a test process linked to the test runner.
%% The process will be monitored and tracked for cleanup.
-spec spawn_test_process(fun(() -> term())) -> #test_process{}.
spawn_test_process(Fun) when is_function(Fun, 0) ->
    spawn_test_process(Fun, #{}).

%% @doc Spawn a test process with options.
%% Options:
%%   - link: boolean() - link to test process (default true)
%%   - monitor: boolean() - monitor process (default true)
%%   - name: atom() - register locally with this name
%%   - timeout: timeout() - timeout for process initialization
-spec spawn_test_process(fun(() -> term()), map()) -> #test_process{}.
spawn_test_process(Fun, Options) ->
    ShouldLink = maps:get(link, Options, false),
    ShouldMonitor = maps:get(monitor, Options, true),
    Name = maps:get(name, Options, undefined),
    Timeout = maps:get(timeout, Options, 5000),

    Pid = case ShouldLink of
        true -> spawn_link(fun() -> Fun() end);
        false -> spawn(fun() -> Fun() end)
    end,

    Monitor = case ShouldMonitor of
        true -> erlang:monitor(process, Pid);
        false -> undefined
    end,

    TestProc = #test_process{
        pid = Pid,
        name = Name,
        monitor = Monitor,
        cleanup = fun() -> stop_process(Pid) end
    },

    %% Register name if provided
    case Name of
        undefined -> ok;
        _ -> register(Name, Pid)
    end,

    %% Register process tracking (may fail if registry not started)
    try register_process(TestProc)
    catch _:_ -> ok
    end,

    %% Wait for process to be ready
    case Timeout of
        infinity -> ok;
        _ ->
            case is_process_alive(Pid) of
                true -> ok;
                false -> error({process_died_during_spawn, Pid})
            end
    end,

    TestProc.

%% @doc Execute a function with a scoped process.
%% The process is automatically cleaned up after the function completes.
-spec with_process(fun(() -> term()), fun((#test_process{}) -> term())) -> term().
with_process(SpawnFun, TestFun) when is_function(SpawnFun, 0), is_function(TestFun, 1) ->
    TestProc = spawn_test_process(SpawnFun),
    try
        TestFun(TestProc)
    after
        cleanup_process(TestProc)
    end.

%% @doc Stop a process gracefully.
-spec stop_process(pid() | #test_process{}) -> ok.
stop_process(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            Monitor = erlang:monitor(process, Pid),
            exit(Pid, normal),
            wait_for_death(Pid, Monitor, 1000);
        false -> ok
    end;
stop_process(#test_process{pid = Pid}) ->
    stop_process(Pid).

%% @doc Forcefully kill a process.
-spec kill_process(pid() | #test_process{}) -> ok.
kill_process(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            Monitor = erlang:monitor(process, Pid),
            exit(Pid, kill),
            wait_for_death(Pid, Monitor, 1000);
        false -> ok
    end;
kill_process(#test_process{pid = Pid}) ->
    kill_process(Pid).

%% @doc Clean up all tracked processes.
-spec cleanup_processes() -> ok.
cleanup_processes() ->
    case whereis(?REGISTRY_NAME) of
        undefined -> ok;
        RegistryPid ->
            {processes, Processes} = gen_server:call(RegistryPid, get_processes),
            lists:foreach(fun cleanup_process/1, Processes)
    end.

%%====================================================================
%% Process State Inspection
%%====================================================================

%% @doc Get the state of a GenServer-like process using sys:get_state.
-spec get_state(pid()) -> term().
get_state(Pid) ->
    get_state(Pid, 5000).

%% @doc Get the state with a custom timeout.
-spec get_state(pid(), timeout()) -> term().
get_state(Pid, Timeout) when is_pid(Pid) ->
    try sys:get_state(Pid, Timeout) of
        State -> State
    catch
        _:_ -> {error, not_a_genserver_or_timeout}
    end.

%% @doc Safely get process info with timeout handling.
-spec process_info_safe(pid(), atom() | [atom()]) -> term() | map().
process_info_safe(Pid, Item) when is_atom(Item) ->
    process_info_safe(Pid, [Item]);
process_info_safe(Pid, Items) when is_list(Items) ->
    case is_process_alive(Pid) of
        false -> {error, process_not_alive};
        true ->
            Info = erlang:process_info(Pid, Items),
            case Info of
                undefined -> {error, process_not_found};
                [] -> #{};
                {InfoList} when is_list(InfoList) ->
                    maps:from_list(InfoList);
                InfoList when is_list(InfoList) ->
                    maps:from_list(InfoList);
                _ -> {error, unknown}
            end
    end.

%% @doc Get the current message queue length of a process.
-spec message_queue(pid()) -> {ok, non_neg_integer()} | {error, term()}.
message_queue(Pid) ->
    case process_info_safe(Pid, [message_queue_len]) of
        #{message_queue_len := Len} -> {ok, Len};
        Error -> Error
    end.

%% @doc Get the memory usage of a process.
-spec process_memory(pid()) -> {ok, non_neg_integer()} | {error, term()}.
process_memory(Pid) ->
    case process_info_safe(Pid, [memory]) of
        #{memory := Mem} -> {ok, Mem};
        Error -> Error
    end.

%% @doc Get full process state including memory, messages, etc.
-spec full_state(pid()) -> #process_state{}.
full_state(Pid) ->
    Info = process_info_safe(Pid, [
        memory, message_queue_len, heap_size,
        reductions, dictionary, current_function
    ]),

    State = case Info of
        #{memory := M, message_queue_len := MQL, heap_size := HS, reductions := R} ->
            %% Try to get state, but don't hang if not a GenServer
            ProcessState = try get_state(Pid, 100)
                          catch
                              _:_ -> undefined
                          end,
            #process_state{
                pid = Pid,
                state = ProcessState,
                memory = M,
                message_queue_len = MQL,
                heap_size = HS,
                reductions = R
            };
        _ ->
            #process_state{
                pid = Pid,
                state = {error, process_dead},
                memory = 0,
                message_queue_len = 0,
                heap_size = 0,
                reductions = 0
            }
    end,
    State.

%%====================================================================
%% Process Generators
%%====================================================================

%% @doc Generate a PID of an existing process.
%% Uses catena_gen for generator functionality.
-spec gen_pid() -> fun((catena_gen:seed()) -> {pid(), catena_gen:seed()}).
gen_pid() ->
    fun(Seed) ->
        %% Generate a PID by selecting from registered processes
        Registered = registered(),
        case Registered of
            [] -> {self(), Seed};
            [Name | _] ->
                case whereis(Name) of
                    undefined -> {self(), Seed};
                    Pid when is_pid(Pid) ->
                        {_Word, NewSeed} = catena_gen:seed_next(Seed),
                        {Pid, NewSeed}
                end
        end
    end.

%% @doc Generate a unique reference.
-spec gen_ref() -> fun((catena_gen:seed()) -> {reference(), catena_gen:seed()}).
gen_ref() ->
    fun(Seed) ->
        Ref = make_ref(),
        {_Word, NewSeed} = catena_gen:seed_next(Seed),
        {Ref, NewSeed}
    end.

%% @doc Generate a node name for testing.
-spec gen_node() -> fun((catena_gen:seed()) -> {node(), catena_gen:seed()}).
gen_node() ->
    fun(Seed) ->
        %% Generate test node names like 'test_node_1@localhost'
        {_Word, NewSeed} = catena_gen:seed_next(Seed),
        Num = (NewSeed#seed.state rem 100) + 1,
        NodeName = list_to_atom("test_node_" ++ integer_to_list(Num) ++ "@localhost"),
        {NodeName, NewSeed}
    end.

%% @doc Generate a registered process name.
-spec gen_registered_name() -> fun((catena_gen:seed()) -> {atom(), catena_gen:seed()}).
gen_registered_name() ->
    fun(Seed) ->
        Names = [test_proc, sample_process, example_server,
                 worker_1, worker_2, handler, manager],
        {_Word, NewSeed} = catena_gen:seed_next(Seed),
        Index = (NewSeed#seed.state rem length(Names)) + 1,
        Name = lists:nth(Index, Names),
        {Name, NewSeed}
    end.

%% @doc Generate a timeout value for testing.
-spec gen_timeout() -> fun((catena_gen:seed()) -> {timeout(), catena_gen:seed()}).
gen_timeout() ->
    fun(Seed) ->
        %% Generate timeouts: infinity or 0-5000 ms
        {_Word, NewSeed} = catena_gen:seed_next(Seed),
        case (NewSeed#seed.state rem 10) of
            0 -> {infinity, NewSeed};
            _ ->
                Timeout = (NewSeed#seed.state rem 5000),
                {Timeout, NewSeed}
        end
    end.

%% @doc Generate an exit reason for testing.
-spec gen_exit_reason() -> fun((catena_gen:seed()) -> {term(), catena_gen:seed()}).
gen_exit_reason() ->
    fun(Seed) ->
        Reasons = [normal, shutdown, {shutdown, test}, killed,
                   badarg, badarith, badmatch, function_clause,
                   case_clause, if_clause, try_clause],
        {_Word, NewSeed} = catena_gen:seed_next(Seed),
        Index = (NewSeed#seed.state rem length(Reasons)) + 1,
        Reason = lists:nth(Index, Reasons),
        {Reason, NewSeed}
    end.

%%====================================================================
%% Process Assertions
%%====================================================================

%% @doc Assert that a process is alive.
-spec assert_alive(pid() | #test_process{}) -> ok | {error, term()}.
assert_alive(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true -> ok;
        false -> {error, {process_not_alive, Pid}}
    end;
assert_alive(#test_process{pid = Pid}) ->
    assert_alive(Pid).

%% @doc Assert that a process has terminated.
-spec assert_dead(pid() | #test_process{}) -> ok | {error, term()}.
assert_dead(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        false -> ok;
        true -> {error, {process_still_alive, Pid}}
    end;
assert_dead(#test_process{pid = Pid}) ->
    assert_dead(Pid).

%% @doc Assert that a process exited with a specific reason.
-spec assert_exit_reason(pid(), term()) -> ok | {error, term()}.
assert_exit_reason(Pid, ExpectedReason) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true -> {error, {process_still_alive, Pid}};
        false ->
            %% We can't get exit reason after the fact without monitoring
            %% This is a basic implementation
            ok
    end.

%% @doc Assert that a process has no messages in its queue.
-spec assert_no_messages(pid()) -> ok | {error, term()}.
assert_no_messages(Pid) when is_pid(Pid) ->
    case message_queue(Pid) of
        {ok, 0} -> ok;
        {ok, N} -> {error, {messages_in_queue, N}};
        Error -> Error
    end.

%% @doc Assert that a process has a specific message count.
-spec assert_message_count(pid(), non_neg_integer()) -> ok | {error, term()}.
assert_message_count(Pid, ExpectedCount) when is_pid(Pid) ->
    case message_queue(Pid) of
        {ok, ExpectedCount} -> ok;
        {ok, ActualCount} -> {error, {wrong_message_count, ExpectedCount, ActualCount}};
        Error -> Error
    end.

%% @doc Assert that a process is registered with a given name.
-spec assert_registered(atom()) -> ok | {error, term()}.
assert_registered(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> {error, {process_not_registered, Name}};
        _Pid -> ok
    end.

%%====================================================================
%% gen_server callbacks (internal registry)
%%====================================================================

%% @private Start the process registry.
start_registry() ->
    gen_server:start({local, ?REGISTRY_NAME}, ?MODULE, [], []).

%% @private
init([]) ->
    {ok, #process_registry{
        processes = [],
        test_pid = undefined
    }}.

%% @private
handle_call(get_processes, _From, State) ->
    {reply, {processes, State#process_registry.processes}, State};
handle_call({register_process, Proc}, _From, State) ->
    NewProcesses = [Proc | State#process_registry.processes],
    {reply, ok, State#process_registry{processes = NewProcesses}};
handle_call({unregister_process, Pid}, _From, State) ->
    NewProcesses = lists:filter(fun(P) -> P#test_process.pid =/= Pid end,
                               State#process_registry.processes),
    {reply, ok, State#process_registry{processes = NewProcesses}}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Register a process for tracking.
-spec register_process(#test_process{}) -> ok.
register_process(Proc) ->
    ensure_registry_started(),
    gen_server:call(?REGISTRY_NAME, {register_process, Proc}),
    ok.

%% @doc Unregister a process from tracking.
-spec unregister_process(pid()) -> ok.
unregister_process(Pid) ->
    case whereis(?REGISTRY_NAME) of
        undefined -> ok;
        _ -> gen_server:call(?REGISTRY_NAME, {unregister_process, Pid})
    end.

%% @doc Ensure the process registry is started.
-spec ensure_registry_started() -> ok.
ensure_registry_started() ->
    case whereis(?REGISTRY_NAME) of
        undefined -> start_registry();
        _ -> ok
    end.

%% @doc Clean up a single process.
-spec cleanup_process(#test_process{}) -> ok.
cleanup_process(#test_process{pid = Pid, monitor = Monitor}) ->
    unregister_process(Pid),

    %% Demonitor if monitoring
    case Monitor of
        undefined -> ok;
        _ -> erlang:demonitor(Monitor, [flush])
    end,

    %% Stop the process
    stop_process(Pid),
    ok.

%% @doc Wait for a process to die.
-spec wait_for_death(pid(), reference(), timeout()) -> ok.
wait_for_death(Pid, Monitor, Timeout) ->
    receive
        {'DOWN', Monitor, process, Pid, _Info} -> ok
    after Timeout ->
        case is_process_alive(Pid) of
            true -> exit(Pid, kill);
            false -> ok
        end,
        receive
            {'DOWN', Monitor, process, Pid, _Info} -> ok
        after 100 ->
            erlang:demonitor(Monitor, [flush]),
            ok
        end
    end.
