%% @doc Unit Tests for Phase 5.1: State Machine DSL
-module(catena_statem_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_statem.hrl").

%%====================================================================
%% Unit Tests - Section 5.1
%%====================================================================

%% ---- Section 5.1.1: State Machine Type Definition ----

state_machine_type_test() ->
    %% Just test that the record type works
    SM = #state_machine{
        name = <<"test">>,
        module = fake_module,
        initial_state = #{count => 0},
        commands = [],
        invariants = [],
        options = catena_statem:default_options()
    },
    ?assertEqual(<<"test">>, SM#state_machine.name),
    ?assertEqual(#{count => 0}, SM#state_machine.initial_state),
    ok.

%% ---- Section 5.1.2: Command Specification ----

command_gen_type_test() ->
    CmdGen = #command_gen{
        name = test,
        args_gen = catena_gen:constant([1, 2, 3]),
        precondition = undefined,
        weight = 5
    },
    ?assertEqual(test, CmdGen#command_gen.name),
    ?assertEqual(5, CmdGen#command_gen.weight),
    ?assertEqual(undefined, CmdGen#command_gen.precondition),
    ok.

command_with_precondition_test() ->
    Precond = fun(_) -> true end,
    CmdGen = #command_gen{
        name = test,
        args_gen = catena_gen:constant([]),
        precondition = Precond,
        weight = 1
    },
    ?assertEqual(Precond, CmdGen#command_gen.precondition),
    ok.

command_has_name_test() ->
    Gen = #command_gen{name = increment, args_gen = catena_gen:constant([])},
    ?assertEqual(increment, Gen#command_gen.name),
    ok.

command_has_args_gen_test() ->
    ArgsGen = catena_gen:constant([1, 2, 3]),
    Gen = #command_gen{name = test, args_gen = ArgsGen},
    ?assertEqual(ArgsGen, Gen#command_gen.args_gen),
    ok.

command_has_weight_test() ->
    Gen = #command_gen{name = test, args_gen = catena_gen:constant([]), weight = 5},
    ?assertEqual(5, Gen#command_gen.weight),
    ok.

command_without_precondition_test() ->
    Gen = #command_gen{name = test, args_gen = catena_gen:constant([])},
    ?assertEqual(undefined, Gen#command_gen.precondition),
    ok.

%% ---- Section 5.1.3: State Model ----

state_get_map_test() ->
    State = #{count => 5, name => <<"test">>},
    ?assertEqual(5, catena_statem:state_get(count, State)),
    ?assertEqual(<<"test">>, catena_statem:state_get(name, State)),
    ok.

state_put_map_test() ->
    State = #{count => 5},
    NewState = catena_statem:state_put(count, 10, State),
    ?assertEqual(10, maps:get(count, NewState)),
    ok.

state_update_map_test() ->
    State = #{count => 5},
    NewState = catena_statem:state_update(count, fun(X) -> X * 2 end, State),
    ?assertEqual(10, maps:get(count, NewState)),
    ok.

state_get_list_test() ->
    State = [{count, 5}, {name, <<"test">>}],
    ?assertEqual(5, catena_statem:state_get(count, State)),
    ?assertEqual(<<"test">>, catena_statem:state_get(name, State)),
    ok.

state_put_list_test() ->
    State = [{count, 5}],
    NewState = catena_statem:state_put(count, 10, State),
    ?assertEqual(10, proplists:get_value(count, NewState)),
    ok.

state_update_list_test() ->
    State = [{count, 5}],
    NewState = catena_statem:state_update(count, fun(X) -> X * 2 end, State),
    ?assertEqual(10, proplists:get_value(count, NewState)),
    ok.

%% ---- Section 5.1.4: Invariants ----

invariant_type_test() ->
    Inv = #invariant{
        name = test,
        check = fun(_) -> true end,
        when_fun = undefined
    },
    ?assertEqual(test, Inv#invariant.name),
    ?assertEqual(undefined, Inv#invariant.when_fun),
    ok.

check_invariants_empty_test() ->
    ?assertEqual(ok, catena_statem:check_invariants([], #{state => 1})),
    ok.

check_invariants_passing_test() ->
    Invariants = [
        #invariant{name = positive_count, check = fun(S) -> maps:get(count, S) >= 0 end}
    ],
    State = #{count => 5},
    ?assertEqual(ok, catena_statem:check_invariants(Invariants, State)),
    ok.

check_invariants_failing_test() ->
    Invariants = [
        #invariant{name = positive_count, check = fun(S) -> maps:get(count, S) >= 0 end}
    ],
    State = #{count => -1},
    Result = catena_statem:check_invariants(Invariants, State),
    ?assertMatch({error, {invariant_failed, _}}, Result),
    ok.

check_invariants_conditional_test() ->
    Invariants = [
        #invariant{
            name = conditional,
            check = fun(S) -> maps:get(count, S) < 100 end,
            when_fun = fun(S) -> maps:get(mode, S) =:= limited end
        }
    ],
    %% When mode is not 'limited', invariant shouldn't be checked
    State = #{count => 150, mode => unlimited},
    ?assertEqual(ok, catena_statem:check_invariants(Invariants, State)),
    %% When mode is 'limited', invariant should fail
    State2 = #{count => 150, mode => limited},
    Result = catena_statem:check_invariants(Invariants, State2),
    ?assertMatch({error, {invariant_failed, _}}, Result),
    ok.

multiple_invariants_all_pass_test() ->
    Invariants = [
        #invariant{name = inv1, check = fun(_) -> true end},
        #invariant{name = inv2, check = fun(_) -> true end},
        #invariant{name = inv3, check = fun(_) -> true end}
    ],
    ?assertEqual(ok, catena_statem:check_invariants(Invariants, #{state => 1})),
    ok.

multiple_invariants_one_fails_test() ->
    Invariants = [
        #invariant{name = inv1, check = fun(_) -> true end},
        #invariant{name = inv2, check = fun(_) -> false end},
        #invariant{name = inv3, check = fun(_) -> true end}
    ],
    Result = catena_statem:check_invariants(Invariants, #{state => 1}),
    ?assertMatch({error, {invariant_failed, _}}, Result),
    ok.

format_invariant_failure_test() ->
    Name = positive_count,
    State = #{count => -1},
    Formatted = catena_statem:format_invariant_failure({Name, State}),
    ?assert(is_list(Formatted)),
    ok.

%% ---- Default Options ----

default_options_test() ->
    Options = catena_statem:default_options(),
    ?assertEqual(100, Options#options.num_commands),
    ?assertEqual(1000, Options#options.max_shrinks),
    ?assertEqual(5000, Options#options.timeout),
    ?assertEqual(2, Options#options.parallel_tracks),
    ok.
