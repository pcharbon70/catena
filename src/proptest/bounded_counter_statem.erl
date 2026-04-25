%% @doc Stateful test helper with state-dependent preconditions and frequencies.
-module(bounded_counter_statem).

-include("catena_statem.hrl").

-export([
    initial_state/0,
    command/1,
    precondition/2,
    next_state/3,
    postcondition/3
]).

initial_state() ->
    #{count => 0}.

command(State) ->
    [
        catena_statem:command(add, catena_gen:constant([2]), #{
            frequency => fun(S) ->
                case maps:get(count, S, 0) of
                    0 -> 20;
                    _ -> 2
                end
            end
        }),
        catena_statem:command(remove, catena_gen:constant([1]), #{
            precondition => fun(S) -> maps:get(count, S, 0) > 0 end,
            frequency => fun(S) ->
                max(maps:get(count, S, 0), 1)
            end
        })
    ].

precondition(State, {call, _, add, [_Amount]}) ->
    maps:get(count, State, 0) >= 0;
precondition(State, {call, _, remove, [Amount]}) ->
    maps:get(count, State, 0) >= Amount;
precondition(_State, _Command) ->
    true.

next_state(State, _Vars, {call, _, add, [Amount]}) ->
    State#{count => maps:get(count, State, 0) + Amount};
next_state(State, _Vars, {call, _, remove, [Amount]}) ->
    State#{count => maps:get(count, State, 0) - Amount};
next_state(State, _Vars, _Command) ->
    State.

postcondition(State, {call, _, remove, [Amount]}, _Result) ->
    maps:get(count, State, 0) >= Amount;
postcondition(_State, _Command, _Result) ->
    true.
