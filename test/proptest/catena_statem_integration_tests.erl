%% @doc Integration Tests for Phase 5.6: Stateful Property Testing
-module(catena_statem_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_statem.hrl").

%%====================================================================
%% Section 5.6.1: Full Workflow Integration
%%====================================================================

full_workflow_generation_to_execution_test() ->
    %% 1. Create state machine
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),

    %% 2. Generate command sequence
    {Commands, _Seed} = catena_statem:generate_commands(SM, 5, 42),

    %% 3. Verify commands were generated
    ?assert(is_list(Commands)),
    ?assert(length(Commands) =< 5),

    %% 4. Commands should be valid call tuples
    ?assert(lists:all(fun(C) -> is_tuple(C) andalso element(1, C) =:= call end, Commands)),
    ok.

full_workflow_symbolic_to_concrete_test() ->
    %% 1. Create state machine
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),

    %% 2. Generate command sequence
    {Commands, _Seed} = catena_statem:generate_commands(SM, 3, 42),

    %% 3. Simulate state through commands
    FinalState = catena_statem:simulate_state(SM, Commands),

    %% 4. State should be valid
    ?assert(is_map(FinalState)),
    ?assert(maps:is_key(count, FinalState)),
    ok.

full_workflow_with_shrinking_test() ->
    %% 1. Create state machine
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),

    %% 2. Generate command sequence
    Commands = [
        {call, counter_statem, increment, []},
        {call, counter_statem, increment, []},
        {call, counter_statem, decrement, []}
    ],

    %% 3. Generate shrunk sequences
    Shrunk = catena_statem:shrink_command_seq(Commands, SM),

    %% 4. Should have shrunk alternatives
    ?assert(length(Shrunk) > 0),

    %% 5. All shrunk sequences should be lists
    ?assert(lists:all(fun is_list/1, Shrunk)),
    ok.

runtime_counter_symbolic_to_concrete_workflow_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"runtime">>, runtime_counter_statem, Options),
    Commands = [
        {call, runtime_counter_statem, increment, []},
        {call, runtime_counter_statem, increment, []},
        {call, runtime_counter_statem, decrement, []}
    ],
    {ok, Trace, SymbolicFinalState, _Bindings} = catena_statem:run_symbolic(SM, Commands),
    {ok, #{results := [ok, ok, ok], final_state := ConcreteFinalState}} = catena_statem:run_concrete(SM, Commands),
    ?assertEqual(3, length(Trace)),
    ?assertEqual(SymbolicFinalState, ConcreteFinalState),
    ?assertEqual(#{count => 1}, ConcreteFinalState),
    ok.

parallel_runtime_counter_workflow_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"runtime">>, runtime_counter_statem, Options),
    {Prefix, Tracks, _Seed} = catena_statem:gen_parallel_commands(SM, 2, 2, 42),
    ?assertEqual([], Prefix),
    ?assertEqual(2, length(Tracks)),
    Results = catena_statem:parallel_property_test(SM, 2, 2, 42),
    ?assert(lists:all(fun({_SeedValue, Status}) -> Status =:= ok end, Results)),
    ok.

%%====================================================================
%% Section 5.6.2: State Machine Validation Integration
%%====================================================================

validate_state_machine_with_callbacks_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),

    %% Validate should pass for counter_statem
    ?assertEqual(ok, catena_statem:validate(SM)),
    ok.

validate_state_machine_with_commands_test() ->
    %% Check that commands can be extracted
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),

    %% Extract commands from initial state
    InitialState = catena_statem:get_initial_state(SM),
    Commands = catena_statem:extract_commands(counter_statem, InitialState),

    %% Should have commands defined
    ?assert(is_list(Commands)),
    ?assert(length(Commands) > 0),
    ok.

%%====================================================================
%% Section 5.6.3: Invariant Checking Integration
%%====================================================================

invariants_checked_during_execution_test() ->
    %% Define invariants
    Invariants = [
        #invariant{name = count_non_negative, check = fun(S) -> maps:get(count, S) >= 0 end}
    ],

    %% Check with valid state
    ValidState = #{count => 5},
    ?assertEqual(ok, catena_statem:check_invariants(Invariants, ValidState)),

    %% Check with invalid state
    InvalidState = #{count => -1},
    ?assertMatch({error, {invariant_failed, _}}, catena_statem:check_invariants(Invariants, InvalidState)),
    ok.

conditional_invariants_test() ->
    %% Invariant that only checks in certain mode
    Invariants = [
        #invariant{
            name = limited_count,
            check = fun(S) -> maps:get(count, S) < 100 end,
            when_fun = fun(S) -> maps:get(mode, S) =:= limited end
        }
    ],

    %% When mode is unlimited, invariant shouldn't fail
    State1 = #{count => 200, mode => unlimited},
    ?assertEqual(ok, catena_statem:check_invariants(Invariants, State1)),

    %% When mode is limited and count is high, should fail
    State2 = #{count => 200, mode => limited},
    ?assertMatch({error, {invariant_failed, _}}, catena_statem:check_invariants(Invariants, State2)),
    ok.

%%====================================================================
%% Section 5.6.4: Command Generation Integration
%%====================================================================

command_generation_with_weights_test() ->
    %% Test that weighted command selection works
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    State = #{count => 0},

    %% Generate multiple commands and check distribution
    Commands = [catena_statem:generate_command(SM, State) || _ <- lists:seq(1, 50)],
    Names = [N || {call, _, N, _} <- Commands],

    %% increment has weight 5, decrement has weight 3, reset has weight 1
    %% So increment should be most common
    IncrementCount = lists:foldl(fun(N, Acc) -> case N of increment -> Acc + 1; _ -> Acc end end, 0, Names),
    ?assert(IncrementCount > 20),  %% Should be majority
    ok.

%%====================================================================
%% Section 5.6.5: Variable Binding Integration
%%====================================================================

variable_substitution_integration_test() ->
    %% Create bindings through command execution
    Var1 = catena_statem:new_var(1),
    Var2 = catena_statem:new_var(2),

    Bindings1 = catena_statem:bind_var(Var1, 42, []),
    Bindings2 = catena_statem:bind_var(Var2, 99, Bindings1),

    %% Look up both variables
    ?assertEqual({ok, 42}, catena_statem:lookup_var(Var1, Bindings2)),
    ?assertEqual({ok, 99}, catena_statem:lookup_var(Var2, Bindings2)),
    ok.

substitute_vars_with_bindings_test() ->
    Args = [1, {var, 1}, 3, {var, 2}],
    Bindings = [
        {{var, 1}, 42},
        {{var, 2}, 99}
    ],
    Result = catena_statem:substitute_vars(Args, Bindings),
    ?assertEqual([1, 42, 3, 99], Result),
    ok.

%%====================================================================
%% Section 5.6.6: Postcondition Integration
%%====================================================================

collect_and_check_postconditions_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    State = #{count => 1},  %% Start with count=1 so increment postcondition passes

    Commands = [
        {call, counter_statem, increment, []},
        {call, counter_statem, increment, []}
    ],

    %% Collect postconditions
    PostConds = catena_statem:collect_postconditions(SM, State, Commands),
    ?assertEqual(2, length(PostConds)),

    %% Check postconditions against results
    Results = [ok, ok],
    ?assertEqual(ok, catena_statem:check_postconditions(PostConds, Results, State)),
    ok.

linearizability_witness_integration_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"runtime">>, runtime_counter_statem, Options),
    Observed = [
        #{track => 1, commands => [{call, runtime_counter_statem, decrement, []}], result => {ok, [ok], []}}
    ],
    ?assertMatch({error, {non_linearizable, _}}, catena_statem:check_linearizable(SM, #{count => 0}, Observed)),
    ok.

performance_smoke_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"bounded">>, bounded_counter_statem, Options),
    Results = [
        begin
            {Commands, _Seed} = catena_statem:generate_commands(SM, 5, Seed),
            is_list(Commands)
        end
     || Seed <- lists:seq(1, 200)
    ],
    ?assert(lists:all(fun(Result) -> Result =:= true end, Results)),
    ok.
