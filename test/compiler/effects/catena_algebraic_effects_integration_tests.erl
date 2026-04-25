%%%-------------------------------------------------------------------
%%% @doc Phase 14.5 integration tests for the algebraic effects track.
%%%
%%% These tests focus on the integration gaps that were still missing:
%%% - end-to-end validation across the new phase surfaces
%%% - real-world runtime scenarios
%%% - equation-based optimization over algebraic laws
%%% @end
%%%-------------------------------------------------------------------
-module(catena_algebraic_effects_integration_tests).

-include_lib("eunit/include/eunit.hrl").

algebraic_effects_integration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Full validation pipeline passes", fun test_validation_pipeline/0},
                {"State management scenario", fun test_state_management_scenario/0},
                {"Logging scenario", fun test_logging_scenario/0},
                {"Resource management scenario", fun test_resource_management_scenario/0},
                {"Equation rewrite optimization", fun test_equation_rewrite_optimization/0}
            ]
        end
    }.

setup() ->
    catch catena_effect_system:shutdown(),
    ok.

cleanup(_) ->
    catch catena_effect_system:shutdown(),
    ok.

test_validation_pipeline() ->
    Report = catena_effect_validation:validate(),
    ?assert(maps:get(passed, Report)).

test_state_management_scenario() ->
    Result = catena_effect_system:with_runtime(fun(Ctx) ->
        catena_effect_runtime:with_handlers(
            Ctx,
            [{'State', [
                {get, fun() ->
                    case erlang:get(counter) of
                        undefined -> 0;
                        Value -> Value
                    end
                end},
                {put, fun(Value) ->
                    erlang:put(counter, Value),
                    ok
                end}
            ]}],
            fun(ChildCtx) ->
                Initial = catena_effect_runtime:perform(ChildCtx, 'State', get, []),
                ok = catena_effect_runtime:perform(ChildCtx, 'State', put, [41]),
                Updated = catena_effect_runtime:perform(ChildCtx, 'State', get, []),
                {Initial, Updated}
            end
        )
    end),
    ?assertEqual({0, 41}, Result).

test_logging_scenario() ->
    Result = catena_effect_system:with_runtime(fun(Ctx) ->
        catena_effect_runtime:with_handlers(
            Ctx,
            [{'Log', [
                {emit, fun(Message) ->
                    Existing = case erlang:get(log_messages) of
                        undefined -> [];
                        Messages -> Messages
                    end,
                    erlang:put(log_messages, [Message | Existing]),
                    ok
                end},
                {flush, fun() ->
                    lists:reverse(case erlang:get(log_messages) of
                        undefined -> [];
                        Messages -> Messages
                    end)
                end}
            ]}],
            fun(ChildCtx) ->
                ok = catena_effect_runtime:perform(ChildCtx, 'Log', emit, [<<"start">>]),
                ok = catena_effect_runtime:perform(ChildCtx, 'Log', emit, [<<"finish">>]),
                catena_effect_runtime:perform(ChildCtx, 'Log', flush, [])
            end
        )
    end),
    ?assertEqual([<<"start">>, <<"finish">>], Result).

test_resource_management_scenario() ->
    Result = catena_effect_system:with_runtime(fun(Ctx) ->
        catena_effect_runtime:with_handlers(
            Ctx,
            [{'Resource', [
                {open, fun(Name) ->
                    erlang:put(resource_state, {open, Name}),
                    {opened, Name}
                end},
                {close, fun() ->
                    erlang:put(resource_state, closed),
                    ok
                end},
                {status, fun() ->
                    case erlang:get(resource_state) of
                        undefined -> closed;
                        Value -> Value
                    end
                end}
            ]}],
            fun(ChildCtx) ->
                Before = catena_effect_runtime:perform(ChildCtx, 'Resource', status, []),
                Opened = catena_effect_runtime:perform(ChildCtx, 'Resource', open, [file_handle]),
                During = catena_effect_runtime:perform(ChildCtx, 'Resource', status, []),
                ok = catena_effect_runtime:perform(ChildCtx, 'Resource', close, []),
                After = catena_effect_runtime:perform(ChildCtx, 'Resource', status, []),
                {Before, Opened, During, After}
            end
        )
    end),
    ?assertEqual({closed, {opened, file_handle}, {open, file_handle}, closed}, Result).

test_equation_rewrite_optimization() ->
    ?assertEqual(ok, catena_equation_spec:validate_set(catena_algebraic_laws:state_laws())),

    Eq = catena_equations:new(
        catena_equations:op(inc, 1, catena_equations:var(x)),
        catena_equations:var(x)
    ),
    Set = catena_equation_spec:add_equation(
        catena_equation_spec:new_set(optimize),
        simplify_inc,
        Eq
    ),
    Expr = catena_equations:op(
        inc,
        1,
        catena_equations:op(inc, 1, catena_equations:lit(42))
    ),
    {ok, Optimized, _} = catena_equation_rewrite:optimize(Expr, Set),
    ?assertMatch({lit, 42}, Optimized).
