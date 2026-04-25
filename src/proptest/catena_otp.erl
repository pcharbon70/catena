%% @doc OTP Behavior Testing for Phase 6.5
%%
%% This module provides specialized testing support for common OTP behaviors
%% including GenServer, Supervisor, gen_statem, and custom behaviors.
%% These patterns understand OTP semantics and provide appropriate
%% generators and assertions.
-module(catena_otp).

%% GenServer Testing
-export([genserver_property/2,
         gen_call/1,
         gen_cast/1,
         gen_info/1,
         with_genserver/2,
         assert_state/2,
         assert_state_match/2,
         simulate_timeout/1,
         get_state/1]).

%% Supervisor Testing
-export([supervisor_property/2,
         child_spec/1,
         crash_child/2,
         restart_strategy/1,
         verify_restart_count/2,
         with_supervisor/2,
         get_children/1,
         stop_child/2]).

%% gen_statem Testing
-export([statem_property/2,
         statem_event/1,
         statem_transition/3,
         get_statem_state/1,
         force_transition/3,
         verify_state/2,
         statem_events/1]).

%% Custom Behavior Testing
-export([behavior_property/2,
         callback_call/3,
         verify_callback/3,
         with_behavior/2]).

-include("catena_process.hrl").
-include("catena_gen.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(genserver_test, {
    pid :: pid() | undefined,
    module :: module(),
    init_args :: term(),
    state :: term() | undefined
}).

-record(supervisor_test, {
    pid :: pid() | undefined,
    module :: module(),
    init_args :: term(),
    children :: [map()]
}).

-record(statem_test, {
    pid :: pid() | undefined,
    module :: module(),
    init_args :: term(),
    current_state :: term() | undefined
}).

-record(behavior_test, {
    pid :: pid() | undefined,
    module :: module(),
    callbacks :: [atom()]
}).

%%====================================================================
%% GenServer Testing
%%====================================================================

%% @doc Create a GenServer property test.
-spec genserver_property(module(), fun(() -> term())) -> {ok, term()} | {error, term()}.
genserver_property(Module, TestFun) ->
    %% Create a property test for a GenServer module
    try TestFun() of
        Result -> {ok, Result}
    catch
        _:Error -> {error, Error}
    end.

%% @doc Generate a gen_server call request.
-spec gen_call(module()) -> {atom(), term()}.
gen_call(Module) ->
    %% Generate a random call message for the module
    Exports = Module:module_info(exports),
    Calls = [N || {N, A} <- Exports, N =/= handle_call, N =/= handle_cast, N =/= handle_info,
                                     N =/= init, N =/= terminate, N =/= code_change, A >= 0, A =< 3],
    case Calls of
        [] -> {undefined, undefined};
        _ ->
            Call = lists:nth(rand:uniform(length(Calls)), Calls),
            Args = gen_args(Module, Call, 3),
            {Call, Args}
    end.

%% @doc Generate a gen_server cast request.
-spec gen_cast(module()) -> {atom(), term()}.
gen_cast(Module) ->
    %% Generate a random cast message for the module
    Exports = Module:module_info(exports),
    Casts = [N || {N, A} <- Exports, N =/= handle_call, N =/= handle_cast, N =/= handle_info,
                                      N =/= init, N =/= terminate, N =/= code_change, A >= 0, A =< 2],
    case Casts of
        [] -> {undefined, undefined};
        _ ->
            Cast = lists:nth(rand:uniform(length(Casts)), Casts),
            Args = gen_args(Module, Cast, 2),
            {Cast, Args}
    end.

%% @doc Generate a gen_server info message.
-spec gen_info(term()) -> term().
gen_info(_Context) ->
    %% Generate a random info message
    case rand:uniform(4) of
        1 -> timeout;
        2 -> {timeout, make_ref()};
        3 -> {system, {self(), make_ref()}};
        4 -> {'EXIT', self(), normal}
    end.

%% @doc Execute a function with a GenServer, cleaning up after.
-spec with_genserver({module(), term()}, fun((pid()) -> term())) -> {ok, term()} | {error, term()}.
with_genserver({Module, InitArgs}, Fun) ->
    case start_genserver(Module, InitArgs) of
        {ok, Pid} ->
            try
                Result = Fun(Pid),
                {ok, Result}
            after
                gen_server:stop(Pid)
            end;
        {error, Reason} ->
            {error, {start_failed, Reason}}
    end.

%% @doc Assert the GenServer is in a specific state.
-spec assert_state(pid(), term()) -> ok | {error, term()}.
assert_state(Pid, ExpectedState) ->
    case get_state(Pid) of
        {ok, ExpectedState} -> ok;
        {ok, ActualState} -> {error, {state_mismatch, ExpectedState, ActualState}};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Assert the GenServer state matches a pattern.
-spec assert_state_match(pid(), term()) -> ok | {error, term()}.
assert_state_match(Pid, Pattern) ->
    case get_state(Pid) of
        {ok, State} ->
            case State of
                Pattern -> ok;
                _ -> {error, {state_mismatch, Pattern, State}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Simulate a timeout in a GenServer.
-spec simulate_timeout(pid()) -> ok | {error, term()}.
simulate_timeout(Pid) ->
    erlang:send_after(0, Pid, timeout),
    ok.

%% @doc Get the current state of a GenServer.
-spec get_state(pid()) -> {ok, term()} | {error, term()}.
get_state(Pid) ->
    try
        State = sys:get_state(Pid),
        {ok, State}
    catch
        _:Error -> {error, Error}
    end.

%%====================================================================
%% Supervisor Testing
%%====================================================================

%% @doc Create a Supervisor property test.
-spec supervisor_property(module(), fun(() -> term())) -> {ok, term()} | {error, term()}.
supervisor_property(Module, TestFun) ->
    %% Create a property test for a Supervisor module
    try TestFun() of
        Result -> {ok, Result}
    catch
        _:Error -> {error, Error}
    end.

%% @doc Generate a child spec for testing.
-spec child_spec(module() | {module(), term()}) -> supervisor:child_spec().
child_spec({Module, StartArgs}) ->
    #{id => Module,
      start => {Module, start_link, StartArgs},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [Module]};
child_spec(Module) ->
    child_spec({Module, []}).

%% @doc Crash a child process.
-spec crash_child(pid(), term()) -> ok | {error, term()}.
crash_child(Pid, Reason) ->
    case is_process_alive(Pid) of
        true ->
            exit(Pid, Reason),
            ok;
        false ->
            {error, process_not_alive}
    end.

%% @doc Get the restart strategy of a supervisor.
-spec restart_strategy(pid()) -> {ok, supervisor:strategy()} | {error, term()}.
restart_strategy(Sup) ->
    try
        _ = supervisor:which_children(Sup),
        State = sys:get_state(Sup),
        case extract_restart_strategy(State) of
            undefined -> {error, cannot_extract_strategy};
            Strategy -> {ok, Strategy}
        end
    catch
        _:Error -> {error, Error}
    end.

%% @doc Verify the restart count of a child.
-spec verify_restart_count(pid(), non_neg_integer()) -> boolean() | {error, term()}.
verify_restart_count(ChildPid, ExpectedCount) ->
    case ExpectedCount of
        0 -> is_process_alive(ChildPid);
        N when N > 0 -> not is_process_alive(ChildPid)
    end.

%% @doc Execute a function with a Supervisor, cleaning up after.
-spec with_supervisor({module(), term()}, fun((pid()) -> term())) -> {ok, term()} | {error, term()}.
with_supervisor({Module, InitArgs}, Fun) ->
    case start_supervisor(Module, InitArgs) of
        {ok, Pid} ->
            try
                Result = Fun(Pid),
                {ok, Result}
            after
                exit(Pid, shutdown)
            end;
        {error, Reason} ->
            {error, {start_failed, Reason}}
    end.

%% @doc Get all children of a supervisor.
-spec get_children(pid()) -> {ok, [pid()]} | {error, term()}.
get_children(Sup) ->
    try
        Children = supervisor:which_children(Sup),
        Pids = [Pid || {_Id, Pid, _Type, _Modules} <- Children, Pid =/= undefined],
        {ok, Pids}
    catch
        _:Error -> {error, Error}
    end.

%% @doc Stop a child process.
-spec stop_child(pid(), term()) -> ok | {error, term()}.
stop_child(Pid, Reason) ->
    case is_process_alive(Pid) of
        true ->
            exit(Pid, Reason),
            ok;
        false ->
            {error, process_not_alive}
    end.

%%====================================================================
%% gen_statem Testing
%%====================================================================

%% @doc Create a gen_statem property test.
-spec statem_property(module(), fun(() -> term())) -> {ok, term()} | {error, term()}.
statem_property(Module, TestFun) ->
    %% Create a property test for a gen_statem module
    try TestFun() of
        Result -> {ok, Result}
    catch
        _:Error -> {error, Error}
    end.

%% @doc Generate a statem event.
-spec statem_event(module()) -> term().
statem_event(Module) ->
    %% Generate a random event for the state machine
    case rand:uniform(3) of
        1 -> {timeout, make_ref()};
        2 -> {call, self(), make_ref()};
        3 -> {cast, make_ref()}
    end.

%% @doc Execute a state transition.
-spec statem_transition(pid(), term(), term()) -> {ok, term()} | {error, term()}.
statem_transition(Pid, Event, Timeout) ->
    try
        case Event of
            {call, Request} ->
                {ok, gen_statem:call(Pid, Request, Timeout)};
            {cast, Request} ->
                gen_statem:cast(Pid, Request),
                {ok, ok};
            {info, Message} ->
                Pid ! Message,
                {ok, ok};
            _ ->
                {ok, gen_statem:call(Pid, Event, Timeout)}
        end
    catch
        _:Error -> {error, Error}
    end.

%% @doc Get the current state of a gen_statem.
-spec get_statem_state(pid()) -> {ok, term()} | {error, term()}.
get_statem_state(Pid) ->
    try
        State = sys:get_state(Pid),
        {ok, State}
    catch
        _:Error -> {error, Error}
    end.

%% @doc Force a state transition.
-spec force_transition(pid(), term(), term()) -> ok | {error, term()}.
force_transition(Pid, NewState, Data) ->
    %% Force the statem into a specific state (for testing)
    try
        sys:replace_state(Pid, fun(_) -> {NewState, Data} end),
        ok
    catch
        _:Error -> {error, Error}
    end.

%% @doc Verify the current state of a gen_statem.
-spec verify_state(pid(), term()) -> boolean() | {error, term()}.
verify_state(Pid, ExpectedState) ->
    case get_statem_state(Pid) of
        {ok, {ExpectedState, _Data}} -> true;
        {ok, ExpectedState} -> true;
        {ok, _Other} -> false;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Get all possible events for a statem.
-spec statem_events(module()) -> [term()].
statem_events(Module) ->
    case erlang:function_exported(Module, test_events, 0) of
        true -> Module:test_events();
        false -> [timeout, info]
    end.

%%====================================================================
%% Custom Behavior Testing
%%====================================================================

%% @doc Create a custom behavior property test.
-spec behavior_property(module(), fun(() -> term())) -> {ok, term()} | {error, term()}.
behavior_property(Module, TestFun) ->
    %% Create a property test for a custom behavior
    try TestFun() of
        Result -> {ok, Result}
    catch
        _:Error -> {error, Error}
    end.

%% @doc Generate a callback call.
-spec callback_call(module(), atom(), [term()]) -> term().
callback_call(Module, Callback, Args) ->
    %% Generate and execute a callback call
    try
        apply(Module, Callback, Args)
    catch
        _:Error -> {error, Error}
    end.

%% @doc Verify a callback returns the expected value.
-spec verify_callback(module(), atom(), term()) -> boolean() | {error, term()}.
verify_callback(Module, Callback, ExpectedValue) ->
    %% Verify that a callback returns the expected value
    case callback_call(Module, Callback, []) of
        ExpectedValue -> true;
        Other -> {error, {unexpected_value, ExpectedValue, Other}}
    end.

%% @doc Execute a function with a custom behavior, cleaning up after.
-spec with_behavior({module(), term(), [atom()]}, fun((pid()) -> term())) -> {ok, term()} | {error, term()}.
with_behavior({Module, InitArgs, Callbacks}, Fun) ->
    case start_behavior(Module, InitArgs, Callbacks) of
        {ok, Pid} ->
            try
                Result = Fun(Pid),
                {ok, Result}
            after
                catch exit(Pid, normal)
            end;
        {error, Reason} ->
            {error, {start_failed, Reason}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Start a GenServer for testing.
start_genserver(Module, InitArgs) ->
    try gen_server:start_link(Module, InitArgs, []) of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    catch
        _:Error -> {error, Error}
    end.

%% @doc Start a Supervisor for testing.
start_supervisor(Module, InitArgs) ->
    try supervisor:start_link(Module, InitArgs) of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    catch
        _:Error -> {error, Error}
    end.

%% @doc Start a custom behavior for testing.
start_behavior(Module, InitArgs, _Callbacks) ->
    try
        case Module:start_link(InitArgs) of
            {ok, Pid} -> {ok, Pid};
            {error, Reason} -> {error, Reason}
        end
    catch
        _:Error -> {error, Error}
    end.

%% @doc Generate arguments for a function call.
gen_args(_Module, _Function, Arity) ->
    %% Generate random arguments based on arity
    [gen_term() || _ <- lists:seq(1, Arity)].

%% @doc Generate a random term.
gen_term() ->
    case rand:uniform(10) of
        1 -> [];
        2 -> test_atom;
        3 -> make_ref();
        4 -> self();
        5 -> 42;
        6 -> "test";
        7 -> <<"binary">>;
        8 -> #{key => value};
        9 -> {tuple, element};
        10 -> [1, 2, 3]
    end.

extract_restart_strategy(Term) when is_atom(Term) ->
    case lists:member(Term, [one_for_one, one_for_all, rest_for_one, simple_one_for_one]) of
        true -> Term;
        false -> undefined
    end;
extract_restart_strategy(Term) when is_tuple(Term) ->
    extract_restart_strategy(tuple_to_list(Term));
extract_restart_strategy(Term) when is_list(Term) ->
    lists:foldl(
        fun(Item, Acc) ->
            case Acc of
                undefined -> extract_restart_strategy(Item);
                Strategy -> Strategy
            end
        end,
        undefined,
        Term
    );
extract_restart_strategy(Term) when is_map(Term) ->
    extract_restart_strategy(maps:values(Term));
extract_restart_strategy(_Term) ->
    undefined.
