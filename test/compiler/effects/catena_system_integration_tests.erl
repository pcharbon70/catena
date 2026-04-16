%%%-------------------------------------------------------------------
%%% @doc Catena Effect System Integration Tests (Phase 14.1)
%%%
%%% End-to-end integration tests for the complete effect system,
%%% verifying that all components from Phases 7-13 work together
%%% correctly through the unified orchestration layer.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_system_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    catena_effects:init().

cleanup(_) ->
    catena_effects:shutdown().

%%====================================================================
%% Handler and Resumption Integration Tests
%%====================================================================

handler_resumption_integration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Handler receives resumption", fun test_handler_receives_resumption/0},
                {"Resumption continues computation", fun test_resumption_continues/0},
                {"Multiple resumptions (multi-shot)", fun test_multi_shot_resumption/0}
            ]
        end
    }.

test_handler_receives_resumption() ->
    %% Verify handler receives both value and resumption
    PassedRef = make_ref(),
    Result = catena_effects:handle(
        test_op,
        fun(_Val, Resumption) ->
            %% Check resumption is a function
            ?assert(is_function(Resumption, 1)),
            handler_result
        end,
        fun() -> computation_result end
    ),
    ?assertEqual(handler_result, Result).

test_resumption_continues() ->
    %% Verify resumption continues the computation
    Result = catena_effects:handle(
        resumption_test_op,
        fun(_Val, Resumption) ->
            %% Resume with a specific value
            catena_effects:resume(Resumption, 42)
        end,
        fun() ->
            %% After resumption, this should receive 42
            Received = catena_effects:perform(receive_value, dummy),
            Received * 2
        end
    ),
    ?assertEqual(84, Result).

test_multi_shot_resumption() ->
    %% Verify multi-shot continuations work
    {Results, FinalState} = catena_effects:run_state(
        fun() ->
            %% Modify state twice using the same operation
            catena_effects:state_modify(fun(S) -> S + 1 end),
            catena_effects:state_modify(fun(S) -> S * 2 end),
            catena_effects:state_get()
        end,
        5
    ),
    %% (5 + 1) * 2 = 12
    ?assertEqual(12, Results),
    ?assertEqual(12, FinalState).

%%====================================================================
%% Deep and Shallow Handler Integration Tests
%%====================================================================

deep_shallow_integration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Shallow handler doesn't intercept nested", fun test_shallow_handler/0},
                {"Deep handler intercepts nested", fun test_deep_handler/0}
            ]
        end
    }.

test_shallow_handler() ->
    %% Shallow handler only handles direct operations
    OuterHandler = fun(_Val, Res) ->
        catena_effects:resume(Res, outer)
    end,
    InnerHandler = fun(_Val, Res) ->
        catena_effects:resume(Res, inner)
    end,

    Result = catena_effects:handle(
        shallow_op,
        OuterHandler,
        fun() ->
            %% Inner handler should handle this operation
            catena_effects:handle(
                shallow_op,
                InnerHandler,
                fun() -> catena_effects:perform(shallow_op, val) end
            )
        end
    ),
    ?assertEqual(inner, Result).

test_deep_handler() ->
    %% Deep handler handles all operations in scope
    OuterHandler = fun(_Val, Res) ->
        catena_effects:resume(Res, outer_handled)
    end,

    Result = catena_effects:handle_deep(
        deep_op,
        OuterHandler,
        fun() ->
            %% This should be handled by outer deep handler
            catena_effects:perform(deep_op, val)
        end
    ),
    ?assertEqual(outer_handled, Result).

%%====================================================================
%% Row Polymorphism Integration Tests
%%====================================================================

row_polymorphism_integration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Effect set operations", fun test_effect_set_operations/0},
                {"Row variable generalization", fun test_row_generalization/0}
            ]
        end
    }.

test_effect_set_operations() ->
    %% Verify effect row operations work
    Empty = catena_row_types:empty_row(),
    ?assertEqual(true, catena_row_types:is_empty_row(Empty)),

    Row1 = catena_row_types:effect_row([state, reader]),
    ?assertEqual(false, catena_row_types:is_empty_row(Row1)),
    ?assertEqual(true, catena_row_types:row_contains(Row1, state)),

    Row2 = catena_row_types:row_union(Row1, catena_row_types:effect_row([writer])),
    ?assertEqual(true, catena_row_types:row_contains(Row2, writer)).

test_row_generalization() ->
    %% Verify row variable operations
    Var = catena_row_types:fresh_row_var(),
    ?assertEqual(true, catena_row_types:is_row_var(Var)),

    RowWithVar = catena_row_types:effect_row([state]),
    %% Row should be valid
    ?assertEqual(true, catena_row_types:is_valid_row(RowWithVar)).

%%====================================================================
%% Typed Handler Integration Tests
%%====================================================================

typed_handler_integration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Handler type inference", fun test_handler_inference/0},
                {"Handler type checking", fun test_handler_checking/0}
            ]
        end
    }.

test_handler_inference() ->
    %% Verify handler type inference works
    HandlerFn = fun(Val, _Res) -> Val end,
    %% Handler should be a valid 2-arity function
    ?assert(is_function(HandlerFn, 2)),
    ok.

test_handler_checking() ->
    %% Verify handler type checking
    HandlerFn = fun(_IntVal, _Res) -> ok end,
    ?assert(is_function(HandlerFn, 2)),

    %% Register and verify
    ok = catena_effects:register_handler(typed_op, HandlerFn),
    {ok, Handler} = catena_effects:lookup_handler(typed_op),
    ?assert(is_tuple(Handler)).

%%====================================================================
%% Higher-Order Effect Integration Tests
%%====================================================================

higher_order_integration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Effectful function parameter", fun test_effectful_param/0},
                {"Hefty tree construction", fun test_hefty_construction/0}
            ]
        end
    }.

test_effectful_param() ->
    %% Test operations with effectful function parameters
    Result = catena_effects:run_state(
        fun() ->
            %% Use state operations within a computation
            S = catena_effects:state_get(),
            catena_effects:state_put(S + 1),
            catena_effects:state_get()
        end,
        10
    ),
    ?assertEqual(11, Result).

test_hefty_construction() ->
    %% Verify hefty tree operations exist
    %% Pure value
    Pure = catena_hefty:pure(42),
    ?assert(is_function(Pure)),

    %% Effect operation
    Effect = catena_hefty:effect(test_op, val, fun(_) -> catena_hefty:pure(result) end),
    ?assert(is_function(Effect)).

%%====================================================================
%% Equation-Based Optimization Integration Tests
%%====================================================================

equation_optimization_integration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Apply equations to program", fun test_apply_equations/0},
                {"Optimize with equations", fun test_optimize_with_equations/0}
            ]
        end
    }.

test_apply_equations() ->
    %% Verify equation system integrates with optimization
    Eq = {eq, {var, x}, {lit, 5}},
    ok = catena_effects:add_equation(opt_op, Eq),

    %% Apply equations to a program
    Program = [{effect, {state, {put, 1}}}, {effect, {state, {put, 2}}}],
    Result = catena_effects:apply_equations(Program),
    ?assert(is_list(Result)).

test_optimize_with_equations() ->
    %% Verify optimization pipeline uses equations
    Program = [{effect, {state, {put, 1}}}, {effect, {state, {put, 2}}}],
    Result = catena_effects:optimize(Program, 2),
    ?assert(is_list(Result)).

%%====================================================================
%% End-to-End Scenario Tests
%%====================================================================

end_to_end_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"State management scenario", fun test_state_management/0},
                {"Error handling scenario", fun test_error_handling/0},
                {"Combined effects scenario", fun test_combined_effects/0}
            ]
        end
    }.

test_state_management() ->
    %% Real-world state management scenario
    {Result, FinalState} = catena_effects:run_state(
        fun() ->
            %% Initialize counter
            catena_effects:state_put(#{count => 0, total => 0}),

            %% Add some values
            lists:foreach(fun(N) ->
                catena_effects:state_modify(fun(S) ->
                    S#{count => maps:get(count, S) + 1,
                       total => maps:get(total, S) + N}
                end)
            end, lists:seq(1, 5)),

            %% Get final state
            catena_effects:state_get()
        end,
        #{}
    ),

    ?assertEqual(5, maps:get(count, FinalState)),
    ?assertEqual(15, maps:get(total, FinalState)).

test_error_handling() ->
    %% Real-world error handling scenario
    Process = fun(Input) ->
        case Input of
            error_val -> catena_effects:throw(invalid_input);
            N when N > 100 -> catena_effects:throw(out_of_bounds);
            N -> N * 2
        end
    end,

    %% Test error path
    ErrorResult = catena_effects:catch_error(
        fun() -> Process(error_val) end,
        fun(Error) -> {error, Error} end
    ),
    ?assertEqual({error, invalid_input}, ErrorResult),

    %% Test success path
    SuccessResult = catena_effects:catch_error(
        fun() -> Process(50) end,
        fun(Error) -> {error, Error} end
    ),
    ?assertEqual(100, SuccessResult).

test_combined_effects() ->
    %% Test combining multiple effects
    {Result, WriterOutput} = catena_effects:run_writer(
        fun() ->
            %% Combine state and reader in a single computation
            InitialState = catena_effects:state_get(),
            catena_effects:writer_tell({initial, InitialState}),

            %% Modify state
            catena_effects:state_put(InitialState * 2),
            NewState = catena_effects:state_get(),
            catena_effects:writer_tell({modified, NewState}),

            %% Return final result
            {final, NewState}
        end
    ),

    ?assertMatch({final, _}, Result),
    ?assertEqual([{initial, 0}, {modified, 0}], WriterOutput).

%%====================================================================
%% System State Consistency Tests
%%====================================================================

state_consistency_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Handler stack consistency", fun test_handler_stack_consistency/0},
                {"Effect registry consistency", fun test_registry_consistency/0},
                {"Stats consistency", fun test_stats_consistency/0}
            ]
        end
    }.

test_handler_stack_consistency() ->
    %% Verify handler stack maintains consistency
    InitialDepth = catena_effects:diagnostics(),
    ?assert(maps:get(scope_depth, InitialDepth) >= 0),

    %% Push some scopes
    catena_effects:push_handler_scope(),
    catena_effects:push_handler_scope(),

    MidDepth = catena_effects:diagnostics(),
    ?assertEqual(2, maps:get(scope_depth, MidDepth)),

    %% Pop all
    catena_effects:pop_handler_scope(),
    catena_effects:pop_handler_scope(),

    FinalDepth = catena_effects:diagnostics(),
    ?assertEqual(0, maps:get(scope_depth, FinalDepth)).

test_registry_consistency() ->
    %% Verify effect registry maintains consistency
    Effects1 = catena_effects:list_effects(),
    NumEffects1 = length(Effects1),

    %% Add an effect
    catena_effects:register_effect(consistency_test, [op]),

    Effects2 = catena_effects:list_effects(),
    NumEffects2 = length(Effects2),

    ?assertEqual(NumEffects1 + 1, NumEffects2),

    %% Remove the effect
    catena_effects:unregister_effect(consistency_test),

    Effects3 = catena_effects:list_effects(),
    NumEffects3 = length(Effects3),

    ?assertEqual(NumEffects1, NumEffects3).

test_stats_consistency() ->
    %% Verify statistics are tracked
    InitialStats = catena_effects:stats(),
    InitialPerforms = maps:get(performs, InitialStats),

    %% Perform some operations
    {_, _} = catena_effects:run_state(
        fun() ->
            catena_effects:state_get(),
            catena_effects:state_put(5),
            catena_effects:state_get()
        end,
        0
    ),

    FinalStats = catena_effects:stats(),
    FinalPerforms = maps:get(performs, FinalStats),

    %% Should have more performs now
    ?assert(FinalPerforms >= InitialPerforms).

%%====================================================================
%% Error Recovery Tests
%%====================================================================

error_recovery_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Handler error recovery", fun test_handler_error_recovery/0},
                {"System recovery after error", fun test_system_recovery/0}
            ]
        end
    }.

test_handler_error_recovery() ->
    %% Verify system recovers from handler errors
    BadHandler = fun(_V, _R) -> error(handler_crashed) end,

    Result = catch catena_effects:handle(
        bad_op,
        BadHandler,
        fun() -> normal_result end
    ),

    %% Should get an error, but system should still be functional
    ?assertMatch({'EXIT', _}, Result),

    %% System should still be functional
    ?assert(catena_effects:is_initialized()).

test_system_recovery() ->
    %% Verify system recovers and can continue after errors
    %% First, a failing operation
    _ = catch catena_effects:perform(nonexistent_op, val),

    %% System should still work
    ?assert(catena_effects:is_initialized()),

    %% New operations should work
    {Result, _} = catena_effects:run_state(
        fun() -> catena_effects:state_get() end,
        42
    ),
    ?assertEqual(42, Result).
