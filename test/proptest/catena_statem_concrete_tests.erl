%% @doc Unit Tests for Phase 5.4: Concrete Execution
-module(catena_statem_concrete_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_statem.hrl").

%%====================================================================
%% Section 5.4.1: Single Command Execution
%%====================================================================

execute_command_creates_binding_test() ->
    %% Use erlang:'+' which adds two numbers
    Cmd = {call, erlang, '+', [10, 20]},
    {ok, Result, Bindings} = catena_statem:execute_command(none, [], Cmd),
    ?assertEqual(30, Result),
    ?assertEqual(1, length(Bindings)),
    [{{var, 1}, 30}] = Bindings,
    ok.

execute_command_with_var_substitution_test() ->
    %% First command creates a binding (10 + 20 = 30)
    Cmd1 = {call, erlang, '+', [10, 20]},
    {ok, 30, Bindings1} = catena_statem:execute_command(none, [], Cmd1),

    %% Second command uses symbolic variable (30 + 5 = 35)
    Var = {var, 1},
    Cmd2 = {call, erlang, '+', [Var, 5]},
    {ok, Result2, Bindings2} = catena_statem:execute_command(none, Bindings1, Cmd2),
    ?assertEqual(35, Result2),
    ?assertEqual(2, length(Bindings2)),
    ok.

execute_command_preserves_old_bindings_test() ->
    Cmd1 = {call, erlang, '+', [10, 20]},
    {ok, _, Bindings1} = catena_statem:execute_command(none, [], Cmd1),

    Cmd2 = {call, erlang, '+', [5, 3]},
    {ok, _, Bindings2} = catena_statem:execute_command(none, Bindings1, Cmd2),

    %% First binding should still be present
    ?assertEqual(2, length(Bindings2)),
    ?assert(lists:keymember({var, 1}, 1, Bindings2)),
    ok.

%%====================================================================
%% Section 5.4.2: Command Sequence Execution
%%====================================================================

execute_commands_empty_test() ->
    Result = catena_statem:execute_commands(none, #{count => 0}, [], counter_statem),
    ?assertMatch({ok, [], _}, Result),
    ok.

execute_commands_single_with_counter_statem_test() ->
    %% Use counter_statem which has all required callbacks
    Cmds = [{call, counter_statem, increment, []}],
    %% The command will fail because counter_statem:increment doesn't exist
    %% but we're testing the execution flow
    Result = catena_statem:execute_commands(none, #{count => 0}, Cmds, counter_statem),
    ?assertMatch({error, {command_failed, _, _}, _}, Result),
    ok.

%%====================================================================
%% Section 5.4.3: Parallel Track Execution
%%====================================================================

run_parallel_tracks_empty_test() ->
    Result = catena_statem:run_parallel_tracks(none, #{state => 1}, []),
    ?assertEqual(none, Result),
    ok.

run_parallel_tracks_first_fails_no_alternative_test() ->
    %% All tracks fail because counter_statem:increment doesn't exist as callable
    Track1 = [{call, counter_statem, increment, []}],
    Tracks = [Track1],
    Result = catena_statem:run_parallel_tracks(none, #{state => 1}, Tracks),
    ?assertEqual(none, Result),
    ok.

run_parallel_tracks_with_counter_statem_test() ->
    %% Test that parallel tracks work even when commands fail
    Track1 = [{call, counter_statem, increment, []}],
    Track2 = [{call, counter_statem, decrement, []}],
    Tracks = [Track1, Track2],
    Result = catena_statem:run_parallel_tracks(none, #{state => 1}, Tracks),
    %% Both fail because counter_statem doesn't have callable functions
    ?assertEqual(none, Result),
    ok.

%%====================================================================
%% Section 5.4.4: Postcondition Checking
%%====================================================================

check_postconditions_empty_test() ->
    Result = catena_statem:check_postconditions([], [], #{state => 1}),
    ?assertEqual(ok, Result),
    ok.

check_postconditions_all_pass_test() ->
    PostConds = [
        {1, fun(_) -> true end},
        {2, fun(_) -> true end}
    ],
    Results = [ok, ok],
    Result = catena_statem:check_postconditions(PostConds, Results, #{state => 1}),
    ?assertEqual(ok, Result),
    ok.

check_postconditions_one_fails_test() ->
    PostConds = [
        {1, fun(_) -> true end},
        {2, fun(_) -> false end}
    ],
    Results = [ok, ok],
    Result = catena_statem:check_postconditions(PostConds, Results, #{state => 1}),
    ?assertMatch({error, {postcondition_failed, 2, _}}, Result),
    ok.

check_postconditions_mismatched_indices_test() ->
    PostConds = [
        {1, fun(_) -> true end},
        {3, fun(_) -> true end}  %% Index 2 skipped
    ],
    Results = [ok, ok, ok],
    Result = catena_statem:check_postconditions(PostConds, Results, #{state => 1}),
    ?assertEqual(ok, Result),
    ok.

check_postconditions_state_included_in_error_test() ->
    PostConds = [
        {1, fun(_) -> false end}
    ],
    Results = [ok],
    State = #{count => 5},
    Result = catena_statem:check_postconditions(PostConds, Results, State),
    ?assertMatch({error, {postcondition_failed, 1, State}}, Result),
    ok.

%%====================================================================
%% Variable Binding Tests (already in Section 5.3 but good to verify)
%%====================================================================

new_var_test() ->
    Var = catena_statem:new_var(1),
    ?assertEqual({var, 1}, Var),
    ok.

bind_var_test() ->
    Var = {var, 1},
    Bindings = catena_statem:bind_var(Var, 42, []),
    ?assertEqual([{Var, 42}], Bindings),
    ok.

lookup_var_found_test() ->
    Var = {var, 1},
    Bindings = [{Var, 42}],
    Result = catena_statem:lookup_var(Var, Bindings),
    ?assertEqual({ok, 42}, Result),
    ok.

lookup_var_not_found_test() ->
    Var = {var, 1},
    Result = catena_statem:lookup_var(Var, []),
    ?assertEqual(error, Result),
    ok.

substitute_vars_test() ->
    Var = {var, 1},
    Args = [1, Var, 3],
    Bindings = [{Var, 42}],
    Result = catena_statem:substitute_vars(Args, Bindings),
    ?assertEqual([1, 42, 3], Result),
    ok.

substitute_vars_unbound_test() ->
    Var = {var, 1},
    Args = [Var],
    Result = catena_statem:substitute_vars(Args, []),
    %% Unbound variables kept as-is
    ?assertEqual([Var], Result),
    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

execute_command_sequence_with_bindings_test() ->
    %% Create a sequence where later commands use earlier results
    Cmd1 = {call, erlang, '*', [7, 4]},  %% Returns 28
    {ok, 28, Bindings1} = catena_statem:execute_command(none, [], Cmd1),

    Var1 = {var, 1},
    Cmd2 = {call, erlang, '+', [Var1, 2]},  %% 28 + 2 = 30
    {ok, 30, Bindings2} = catena_statem:execute_command(none, Bindings1, Cmd2),

    ?assertEqual(2, length(Bindings2)),
    ?assertEqual({ok, 28}, catena_statem:lookup_var(Var1, Bindings2)),
    %% Var 2 should have 30
    Var2 = {var, 2},
    ?assertEqual({ok, 30}, catena_statem:lookup_var(Var2, Bindings2)),
    ok.
