%% @doc Unit Tests for Phase 5.3: Symbolic Execution
-module(catena_statem_symbolic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_statem.hrl").

%%====================================================================
%% Section 5.3.1: Symbolic Variables
%%====================================================================

new_var_creates_var_test() ->
    Var = catena_statem:new_var(1),
    ?assertEqual({var, 1}, Var),
    ok.

new_var_different_indices_test() ->
    Var1 = catena_statem:new_var(1),
    Var2 = catena_statem:new_var(2),
    ?assertNotEqual(Var1, Var2),
    ok.

%%====================================================================
%% Section 5.3.2: Variable Bindings
%%====================================================================

bind_var_adds_binding_test() ->
    Var = {var, 1},
    Bindings = catena_statem:bind_var(Var, 42, []),
    ?assertEqual([{Var, 42}], Bindings),
    ok.

bind_var_prepends_test() ->
    Var1 = {var, 1},
    Var2 = {var, 2},
    Bindings1 = catena_statem:bind_var(Var1, 10, []),
    Bindings2 = catena_statem:bind_var(Var2, 20, Bindings1),
    ?assertEqual([{Var2, 20}, {Var1, 10}], Bindings2),
    ok.

lookup_var_found_test() ->
    Var = {var, 1},
    Bindings = [{Var, 42}],
    Result = catena_statem:lookup_var(Var, Bindings),
    ?assertEqual({ok, 42}, Result),
    ok.

lookup_var_not_found_test() ->
    Var = {var, 1},
    Bindings = [],
    Result = catena_statem:lookup_var(Var, Bindings),
    ?assertEqual(error, Result),
    ok.

lookup_var_multiple_bindings_test() ->
    Var1 = {var, 1},
    Var2 = {var, 2},
    Bindings = [{Var1, 10}, {Var2, 20}],
    ?assertEqual({ok, 10}, catena_statem:lookup_var(Var1, Bindings)),
    ?assertEqual({ok, 20}, catena_statem:lookup_var(Var2, Bindings)),
    ok.

%%====================================================================
%% Section 5.3.3: Variable Substitution
%%====================================================================

substitute_vars_empty_test() ->
    Result = catena_statem:substitute_vars([], []),
    ?assertEqual([], Result),
    ok.

substitute_vars_literals_test() ->
    Args = [1, "hello", {foo, bar}],
    Result = catena_statem:substitute_vars(Args, []),
    ?assertEqual(Args, Result),
    ok.

substitute_vars_single_var_test() ->
    Var = {var, 1},
    Args = [Var],
    Bindings = [{Var, 42}],
    Result = catena_statem:substitute_vars(Args, Bindings),
    ?assertEqual([42], Result),
    ok.

substitute_vars_mixed_test() ->
    Var1 = {var, 1},
    Var2 = {var, 2},
    Args = [1, Var1, "test", Var2],
    Bindings = [{Var1, 42}, {Var2, 99}],
    Result = catena_statem:substitute_vars(Args, Bindings),
    ?assertEqual([1, 42, "test", 99], Result),
    ok.

substitute_vars_unbound_var_test() ->
    Var = {var, 1},
    Args = [Var],
    Result = catena_statem:substitute_vars(Args, []),
    %% Unbound variables are kept as-is
    ?assertEqual([Var], Result),
    ok.

substitute_vars_nested_var_test() ->
    %% Variable substitution only applies at the top level of argument lists
    %% Nested variables are kept as-is (would require recursive traversal)
    Var = {var, 1},
    Args = [{tuple, [1, Var, 3]}],
    Bindings = [{Var, 42}],
    Result = catena_statem:substitute_vars(Args, Bindings),
    %% The tuple itself contains a var, which isn't substituted at top level
    ?assertEqual([{tuple, [1, Var, 3]}], Result),
    ok.

%%====================================================================
%% Section 5.3.4: Symbolic State Update
%%====================================================================

symbolic_next_state_counter_test() ->
    State = #{count => 0},
    Cmd = {call, counter_statem, increment, []},
    NewState = catena_statem:symbolic_next_state(State, [], Cmd),
    ?assertEqual(#{count => 1}, NewState),
    ok.

symbolic_next_state_multiple_commands_test() ->
    State1 = #{count => 0},
    Cmd1 = {call, counter_statem, increment, []},
    State2 = catena_statem:symbolic_next_state(State1, [], Cmd1),

    Cmd2 = {call, counter_statem, increment, []},
    State3 = catena_statem:symbolic_next_state(State2, [], Cmd2),

    Cmd3 = {call, counter_statem, decrement, []},
    State4 = catena_statem:symbolic_next_state(State3, [], Cmd3),

    %% 0 + 1 + 1 - 1 = 1
    ?assertEqual(#{count => 1}, State4),
    ok.

%%====================================================================
%% Section 5.3.5: Postcondition Collection
%%====================================================================

collect_postconditions_empty_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Result = catena_statem:collect_postconditions(SM, #{count => 0}, []),
    ?assertEqual([], Result),
    ok.

collect_postconditions_single_command_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Cmds = [{call, counter_statem, increment, []}],
    Result = catena_statem:collect_postconditions(SM, #{count => 0}, Cmds),
    ?assertEqual(1, length(Result)),
    [{Index, _Fun}] = Result,
    ?assertEqual(1, Index),
    ok.

collect_postconditions_multiple_commands_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Cmds = [
        {call, counter_statem, increment, []},
        {call, counter_statem, increment, []},
        {call, counter_statem, decrement, []}
    ],
    Result = catena_statem:collect_postconditions(SM, #{count => 0}, Cmds),
    ?assertEqual(3, length(Result)),
    %% Check indices are sequential
    Indices = [I || {I, _F} <- Result],
    ?assert(lists:member(1, Indices)),
    ?assert(lists:member(2, Indices)),
    ?assert(lists:member(3, Indices)),
    ok.

collect_postconditions_skip_failed_precondition_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Cmds = [
        {call, counter_statem, increment, []},
        {call, counter_statem, reset, []},  %% precondition returns false
        {call, counter_statem, increment, []}
    ],
    Result = catena_statem:collect_postconditions(SM, #{count => 0}, Cmds),
    %% reset should be skipped due to failed precondition
    ?assertEqual(2, length(Result)),
    [{Index1, _}, {Index2, _}] = Result,
    ?assertEqual(1, Index1),
    ?assertEqual(2, Index2),
    ok.

collect_postcondition_returns_function_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    %% Start with count = 1 so increment postcondition will pass
    Cmds = [{call, counter_statem, increment, []}],
    Result = catena_statem:collect_postconditions(SM, #{count => 1}, Cmds),
    [{_Index, Fun}] = Result,
    ?assert(is_function(Fun, 1)),
    %% Postcondition checks that count > 0 in pre-state (1 > 0 = true)
    ?assert(Fun(some_result)),
    ok.
