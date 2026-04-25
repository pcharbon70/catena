%% @doc Invalid command state machine used for validation tests.
-module(invalid_command_statem).

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

command(_State) ->
    [
        #command_gen{name = <<"not-an-atom">>, args_gen = undefined, weight = 0}
    ].

precondition(_State, _Command) ->
    true.

next_state(State, _Vars, _Command) ->
    State.

postcondition(_State, _Command, _Result) ->
    true.
