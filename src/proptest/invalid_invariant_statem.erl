%% @doc Invalid invariant state machine used for validation tests.
-module(invalid_invariant_statem).

-include("catena_statem.hrl").

-export([
    initial_state/0,
    command/1,
    precondition/2,
    next_state/3,
    postcondition/3,
    invariants/0
]).

initial_state() ->
    #{count => 0}.

command(_State) ->
    [
        #command_gen{name = increment, args_gen = catena_gen:constant([]), weight = 1}
    ].

precondition(_State, _Command) ->
    true.

next_state(State, _Vars, _Command) ->
    State.

postcondition(_State, _Command, _Result) ->
    true.

invariants() ->
    [
        #invariant{name = <<"bad">>, check = undefined, when_fun = false}
    ].
