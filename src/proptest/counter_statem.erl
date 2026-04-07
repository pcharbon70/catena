%% @doc Counter State Machine for Testing
%%
%% A simple counter state machine used for testing the stateful property
%% testing framework.
-module(counter_statem).

-include("catena_statem.hrl").

%%====================================================================
%% Callbacks
%%====================================================================

initial_state() -> #{count => 0}.

command(_State) ->
    [
        #command_gen{name = increment, args_gen = catena_gen:constant([]), weight = 5},
        #command_gen{name = decrement, args_gen = catena_gen:constant([]), weight = 3},
        #command_gen{name = reset, args_gen = catena_gen:constant([]), weight = 1}
    ].

precondition(_State, {call, _, reset, []}) ->
    false;  %% Reset not allowed in this simple test
precondition(_State, _Command) ->
    true.

next_state(State, _Vars, {call, _, increment, []}) ->
    State#{count => maps:get(count, State) + 1};
next_state(State, _Vars, {call, _, decrement, []}) ->
    State#{count => maps:get(count, State) - 1};
next_state(State, _Vars, {call, _, reset, []}) ->
    State#{count => 0}.

postcondition(State, {call, _, increment, []}, _Result) ->
    maps:get(count, State) > 0;
postcondition(State, {call, _, decrement, []}, _Result) ->
    maps:get(count, State) >= 0;
postcondition(_State, _Command, _Result) ->
    true.
