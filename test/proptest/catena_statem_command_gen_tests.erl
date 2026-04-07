%% @doc Unit Tests for Phase 5.2: Command Generation
-module(catena_statem_command_gen_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_statem.hrl").

%%====================================================================
%% Section 5.2.1: Command Selection
%%====================================================================

weight_select_single_command_test() ->
    Cmds = [#command_gen{name = test, args_gen = catena_gen:constant([]), weight = 1}],
    Selected = catena_statem:weight_select(Cmds),
    ?assertEqual(test, Selected#command_gen.name),
    ok.

weight_select_returns_command_spec_test() ->
    Cmds = [
        #command_gen{name = cmd1, args_gen = catena_gen:constant([]), weight = 1},
        #command_gen{name = cmd2, args_gen = catena_gen:constant([]), weight = 2}
    ],
    Selected = catena_statem:weight_select(Cmds),
    ?assert(is_record(Selected, command_gen)),
    Name = Selected#command_gen.name,
    ?assert(lists:member(Name, [cmd1, cmd2])),
    ok.

weight_select_empty_raises_error_test() ->
    ?assertError(no_commands_available, catena_statem:weight_select([])),
    ok.

weight_select_respects_weights_test() ->
    %% Run many times to check distribution
    Cmds = [
        #command_gen{name = heavy, args_gen = catena_gen:constant([]), weight = 9},
        #command_gen{name = light, args_gen = catena_gen:constant([]), weight = 1}
    ],
    Selected = [catena_statem:weight_select(Cmds) || _ <- lists:seq(1, 100)],
    Results = [S#command_gen.name || S <- Selected],
    HeavyCount = lists:foldl(fun(N, Acc) -> case N of heavy -> Acc + 1; _ -> Acc end end, 0, Results),
    %% heavy should be selected significantly more often
    ?assert(HeavyCount > 50),
    ok.

%%====================================================================
%% Section 5.2.2: Command Filtering
%%====================================================================

filter_valid_commands_empty_test() ->
    Result = catena_statem:filter_valid_commands([], #{state => 1}),
    ?assertEqual([], Result),
    ok.

filter_valid_commands_all_pass_test() ->
    Cmds = [
        #command_gen{name = a, args_gen = catena_gen:constant([]), precondition = fun(_) -> true end},
        #command_gen{name = b, args_gen = catena_gen:constant([]), precondition = fun(_) -> true end}
    ],
    Result = catena_statem:filter_valid_commands(Cmds, #{state => 1}),
    ?assertEqual(2, length(Result)),
    ok.

filter_valid_commands_some_fail_test() ->
    Cmds = [
        #command_gen{name = a, args_gen = catena_gen:constant([]), precondition = fun(_) -> true end},
        #command_gen{name = b, args_gen = catena_gen:constant([]), precondition = fun(_) -> false end},
        #command_gen{name = c, args_gen = catena_gen:constant([]), precondition = fun(_) -> true end}
    ],
    Result = catena_statem:filter_valid_commands(Cmds, #{state => 1}),
    ?assertEqual(2, length(Result)),
    Names = [C#command_gen.name || C <- Result],
    ?assert(lists:member(a, Names)),
    ?assert(lists:member(c, Names)),
    ?assertNot(lists:member(b, Names)),
    ok.

filter_valid_commands_no_precondition_test() ->
    Cmds = [
        #command_gen{name = a, args_gen = catena_gen:constant([])}
    ],
    Result = catena_statem:filter_valid_commands(Cmds, #{state => 1}),
    ?assertEqual(1, length(Result)),
    ok.

filter_valid_commands_state_dependent_test() ->
    Cmds = [
        #command_gen{name = need_count, args_gen = catena_gen:constant([]), precondition = fun(S) -> maps:is_key(count, S) end},
        #command_gen{name = need_flag, args_gen = catena_gen:constant([]), precondition = fun(S) -> maps:is_key(flag, S) end}
    ],
    State1 = #{count => 5},
    Result1 = catena_statem:filter_valid_commands(Cmds, State1),
    ?assertEqual(1, length(Result1)),
    ?assertEqual(need_count, (lists:nth(1, Result1))#command_gen.name),

    State2 = #{flag => true},
    Result2 = catena_statem:filter_valid_commands(Cmds, State2),
    ?assertEqual(1, length(Result2)),
    ?assertEqual(need_flag, (lists:nth(1, Result2))#command_gen.name),
    ok.

%%====================================================================
%% Section 5.2.3: Command Generation
%%====================================================================

generate_command_returns_call_tuple_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Command = catena_statem:generate_command(SM, #{count => 0}),
    ?assertMatch({call, _, _, _}, Command),
    ok.

generate_command_uses_module_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Command = catena_statem:generate_command(SM, #{count => 0}),
    {call, Module, _Name, _Args} = Command,
    ?assertEqual(counter_statem, Module),
    ok.

%%====================================================================
%% Section 5.2.4: Command Sequence Generation
%%====================================================================

generate_commands_zero_length_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    {Commands, _Seed} = catena_statem:generate_commands(SM, 0, 42),
    ?assertEqual([], Commands),
    ok.

generate_commands_returns_list_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    {Commands, _Seed} = catena_statem:generate_commands(SM, 5, 42),
    ?assert(is_list(Commands)),
    ?assert(length(Commands) =< 5),  %% May stop if no valid commands
    ok.

generate_commands_all_valid_calls_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    {Commands, _Seed} = catena_statem:generate_commands(SM, 10, 42),
    ?assert(lists:all(fun(Cmd) -> is_tuple(Cmd) andalso element(1, Cmd) =:= call end, Commands)),
    ok.

generate_commands_uses_different_seeds_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    {Commands1, Seed1} = catena_statem:generate_commands(SM, 5, 42),
    {Commands2, Seed2} = catena_statem:generate_commands(SM, 5, Seed1),
    %% Seeds should be different
    ?assertNot(Seed1 =:= Seed2),
    %% Commands might be same or different (random)
    ?assert(is_list(Commands2)),
    ok.

%%====================================================================
%% Section 5.2.5: State Simulation
%%====================================================================

simulate_state_empty_commands_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    State = catena_statem:simulate_state(SM, []),
    ?assertEqual(#{count => 0}, State),
    ok.

simulate_state_single_command_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Commands = [{call, counter_statem, increment, []}],
    State = catena_statem:simulate_state(SM, Commands),
    ?assertEqual(#{count => 1}, State),
    ok.

simulate_state_multiple_commands_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Commands = [
        {call, counter_statem, increment, []},
        {call, counter_statem, increment, []},
        {call, counter_statem, decrement, []}
    ],
    State = catena_statem:simulate_state(SM, Commands),
    %% 0 + 1 + 1 - 1 = 1
    ?assertEqual(#{count => 1}, State),
    ok.

%%====================================================================
%% Section 5.2.6: Command Shrinking
%%====================================================================

shrink_command_seq_empty_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Result = catena_statem:shrink_command_seq([], SM),
    ?assertEqual([], Result),
    ok.

shrink_command_seq_single_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Commands = [{call, counter_statem, increment, []}],
    Result = catena_statem:shrink_command_seq(Commands, SM),
    %% Shrinking a single command should give the empty sequence option
    ?assertEqual([[]], Result),
    ok.

shrink_command_seq_multiple_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Commands = [
        {call, counter_statem, increment, []},
        {call, counter_statem, increment, []},
        {call, counter_statem, decrement, []}
    ],
    Result = catena_statem:shrink_command_seq(Commands, SM),
    ?assert(length(Result) > 0),
    %% All shrunk sequences should be shorter or equal
    ?assert(lists:all(fun(Seq) -> length(Seq) < length(Commands) end, Result)),
    ok.

shrink_command_seq_removes_from_end_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Commands = [
        {call, counter_statem, increment, []},
        {call, counter_statem, increment, []}
    ],
    Result = catena_statem:shrink_command_seq(Commands, SM),
    %% Should include empty list and single prefix
    ?assert(lists:member([], Result)),
    ok.

shrink_command_seq_removes_middle_test() ->
    Options = catena_statem:default_options(),
    SM = catena_statem:state_machine(<<"test">>, counter_statem, Options),
    Commands = [
        {call, counter_statem, increment, []},
        {call, counter_statem, decrement, []},
        {call, counter_statem, increment, []}
    ],
    Result = catena_statem:shrink_command_seq(Commands, SM),
    %% Should have sequences with middle removed
    HasTwoElement = lists:any(fun(Seq) -> length(Seq) =:= 2 end, Result),
    ?assert(HasTwoElement),
    ok.
