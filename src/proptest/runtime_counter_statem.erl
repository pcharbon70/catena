%% @doc Runtime-backed counter state machine for concrete execution tests.
-module(runtime_counter_statem).

-include("catena_statem.hrl").

-export([
    initial_state/0,
    command/1,
    precondition/2,
    next_state/3,
    postcondition/3,
    setup/0,
    cleanup/1,
    increment/1,
    decrement/1,
    reset/1,
    value/1
]).

initial_state() ->
    #{count => 0}.

command(State) ->
    [
        catena_statem:command(increment, catena_gen:constant([]), #{
            weight => 3
        }),
        catena_statem:command(decrement, catena_gen:constant([]), #{
            precondition => fun(S) -> maps:get(count, S, 0) > 0 end,
            weight => 1
        }),
        catena_statem:command(reset, catena_gen:constant([]), #{
            precondition => fun(S) -> maps:get(count, S, 0) > 0 end,
            weight => 1
        })
    ].

precondition(State, {call, _, decrement, []}) ->
    maps:get(count, State, 0) > 0;
precondition(State, {call, _, reset, []}) ->
    maps:get(count, State, 0) > 0;
precondition(_State, _Command) ->
    true.

next_state(State, _Vars, {call, _, increment, []}) ->
    State#{count => maps:get(count, State, 0) + 1};
next_state(State, _Vars, {call, _, decrement, []}) ->
    State#{count => maps:get(count, State, 0) - 1};
next_state(State, _Vars, {call, _, reset, []}) ->
    State#{count => 0};
next_state(State, _Vars, _Command) ->
    State.

postcondition(_State, _Command, Result) ->
    Result =:= ok.

setup() ->
    Parent = self(),
    Pid = spawn_link(fun() -> counter_loop(Parent, 0) end),
    {ok, Pid}.

cleanup(Pid) when is_pid(Pid) ->
    Pid ! stop,
    ok;
cleanup(_System) ->
    ok.

increment(Pid) ->
    counter_call(Pid, increment).

decrement(Pid) ->
    counter_call(Pid, decrement).

reset(Pid) ->
    counter_call(Pid, reset).

value(Pid) ->
    counter_call(Pid, value).

counter_call(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
        {Ref, Reply} -> Reply
    after 1000 ->
        timeout
    end.

counter_loop(_Parent, Count) ->
    receive
        {From, Ref, increment} ->
            From ! {Ref, ok},
            counter_loop(self(), Count + 1);
        {From, Ref, decrement} when Count > 0 ->
            From ! {Ref, ok},
            counter_loop(self(), Count - 1);
        {From, Ref, decrement} ->
            From ! {Ref, {error, empty}},
            counter_loop(self(), Count);
        {From, Ref, reset} ->
            From ! {Ref, ok},
            counter_loop(self(), 0);
        {From, Ref, value} ->
            From ! {Ref, Count},
            counter_loop(self(), Count);
        stop ->
            ok
    end.
