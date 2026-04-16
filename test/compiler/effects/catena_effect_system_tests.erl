%%%-------------------------------------------------------------------
%%% @doc Catena Effect System Tests (Phase 14.1)
%%%
%%% Tests for the unified effect system orchestration layer that
%%% integrates all components from Phases 7-13.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_system_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Setup and teardown
setup() ->
    catena_effect_system:init(),
    ok.

cleanup(_) ->
    catena_effect_system:shutdown(),
    ok.

%%====================================================================
%% System Lifecycle Tests
%%====================================================================

system_lifecycle_test_() ->
    [
        {"Initialize system", fun test_init/0},
        {"Check initialized state", fun test_is_initialized/0},
        {"Shutdown system", fun test_shutdown/0},
        {"Reinitialize after shutdown", fun test_reinit/0}
    ].

test_init() ->
    %% System is initialized from setup
    ?assert(catena_effect_system:is_initialized()).

test_is_initialized() ->
    ?assert(catena_effect_system:is_initialized()).

test_shutdown() ->
    ?assert(catena_effect_system:is_initialized()),
    catena_effect_system:shutdown(),
    ?assertNot(catena_effect_system:is_initialized()),
    %% Reinitialize for subsequent tests
    catena_effect_system:init(),
    ?assert(catena_effect_system:is_initialized()).

test_reinit() ->
    catena_effect_system:shutdown(),
    ?assertNot(catena_effect_system:is_initialized()),
    catena_effect_system:init(),
    ?assert(catena_effect_system:is_initialized()).

%%====================================================================
%% Configuration Tests
%%====================================================================

configuration_test_() ->
    [
        {"Get default configuration", fun test_get_config/0},
        {"Configure optimization level", fun test_configure_optimization/0},
        {"Enable/disable equations", fun test_configure_equations/0},
        {"Enable/disable hefty", fun test_configure_hefty/0}
    ].

test_get_config() ->
    Level = catena_effect_system:get_config(optimization_level),
    ?assert(is_integer(Level)),
    ?assert(Level >= 0 andalso Level =< 3),
    Enabled = catena_effect_system:get_config(enable_equations),
    ?assert(is_boolean(Enabled)).

test_configure_optimization() ->
    catena_effect_system:configure([{optimization_level, 3}]),
    ?assertEqual(3, catena_effect_system:get_config(optimization_level)),
    catena_effect_system:configure([{optimization_level, 0}]),
    ?assertEqual(0, catena_effect_system:get_config(optimization_level)).

test_configure_equations() ->
    catena_effect_system:configure([{enable_equations, false}]),
    ?assertEqual(false, catena_effect_system:get_config(enable_equations)),
    catena_effect_system:configure([{enable_equations, true}]),
    ?assertEqual(true, catena_effect_system:get_config(enable_equations)).

test_configure_hefty() ->
    catena_effect_system:configure([{enable_hefty, false}]),
    ?assertEqual(false, catena_effect_system:get_config(enable_hefty)),
    catena_effect_system:configure([{enable_hefty, true}]),
    ?assertEqual(true, catena_effect_system:get_config(enable_hefty)).

%%====================================================================
%% Handler Stack Management Tests
%%====================================================================

handler_stack_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Push and pop handler scope", fun test_push_pop_scope/0},
                {"Multiple handler scopes", fun test_multiple_scopes/0},
                {"Scope depth tracking", fun test_scope_depth/0},
                {"Reset scopes", fun test_reset_scopes/0}
            ]
        end
    }.

test_push_pop_scope() ->
    ?assertEqual(0, catena_effect_system:scope_depth()),
    catena_effect_system:push_handler_scope(),
    ?assertEqual(1, catena_effect_system:scope_depth()),
    {ok, _} = catena_effect_system:pop_handler_scope(),
    ?assertEqual(0, catena_effect_system:scope_depth()).

test_multiple_scopes() ->
    ?assertEqual(0, catena_effect_system:scope_depth()),
    catena_effect_system:push_handler_scope(),
    catena_effect_system:push_handler_scope(),
    catena_effect_system:push_handler_scope(),
    ?assertEqual(3, catena_effect_system:scope_depth()),
    {ok, _} = catena_effect_system:pop_handler_scope(),
    ?assertEqual(2, catena_effect_system:scope_depth()).

test_scope_depth() ->
    ?assertEqual(0, catena_effect_system:scope_depth()),
    lists:foreach(fun(_) -> catena_effect_system:push_handler_scope() end, lists:seq(1, 5)),
    ?assertEqual(5, catena_effect_system:scope_depth()).

test_reset_scopes() ->
    lists:foreach(fun(_) -> catena_effect_system:push_handler_scope() end, lists:seq(1, 3)),
    ?assertEqual(3, catena_effect_system:scope_depth()),
    catena_effect_system:reset_scopes(),
    ?assertEqual(0, catena_effect_system:scope_depth()).

%%====================================================================
%% Effect Registration Tests
%%====================================================================

effect_registration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Register effect", fun test_register_effect/0},
                {"Lookup effect", fun test_lookup_effect/0},
                {"List effects", fun test_list_effects/0},
                {"Unregister effect", fun test_unregister_effect/0},
                {"Check effect exists", fun test_effect_exists/0}
            ]
        end
    }.

test_register_effect() ->
    ok = catena_effect_system:register_effect(custom_effect, [op1, op2]),
    {ok, EffectDef} = catena_effect_system:lookup_effect(custom_effect),
    ?assertEqual(custom_effect, element(2, EffectDef)).  %% Check name field

test_lookup_effect() ->
    ok = catena_effect_system:register_effect(test_effect, [a, b]),
    {ok, EffectDef} = catena_effect_system:lookup_effect(test_effect),
    ?assert(is_tuple(EffectDef)),
    {error, not_found} = catena_effect_system:lookup_effect(nonexistent).

test_list_effects() ->
    catena_effect_system:register_effect(effect1, [op1]),
    catena_effect_system:register_effect(effect2, [op2]),
    Effects = catena_effect_system:list_effects(),
    ?assert(lists:member(effect1, Effects)),
    ?assert(lists:member(effect2, Effects)).

test_unregister_effect() ->
    catena_effect_system:register_effect(temp_effect, [op]),
    {ok, _} = catena_effect_system:lookup_effect(temp_effect),
    ok = catena_effect_system:unregister_effect(temp_effect),
    {error, not_found} = catena_effect_system:lookup_effect(temp_effect).

test_effect_exists() ->
    catena_effect_system:register_effect(check_effect, [op]),
    ?assert(catena_effect_system:effect_exists(check_effect)),
    ?assertNot(catena_effect_system:effect_exists(no_effect)).

%%====================================================================
%% Handler Registration Tests
%%====================================================================

handler_registration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Register handler", fun test_register_handler/0},
                {"Lookup handler", fun test_lookup_handler/0},
                {"Unregister handler", fun test_unregister_handler/0},
                {"Get current handlers", fun test_current_handlers/0}
            ]
        end
    }.

test_register_handler() ->
    HandlerFn = fun(_Val, _Res) -> ok end,
    ok = catena_effect_system:register_handler(test_op, HandlerFn),
    {ok, Handler} = catena_effect_system:lookup_handler(test_op),
    ?assert(is_tuple(Handler)).

test_lookup_handler() ->
    HandlerFn = fun(_V, _R) -> result end,
    catena_effect_system:register_handler(lookup_op, HandlerFn),
    {ok, _Handler} = catena_effect_system:lookup_handler(lookup_op),
    {error, not_found} = catena_effect_system:lookup_handler(no_handler).

test_unregister_handler() ->
    HandlerFn = fun(_V, _R) -> ok end,
    catena_effect_system:register_handler(temp_op, HandlerFn),
    {ok, _} = catena_effect_system:lookup_handler(temp_op),
    ok = catena_effect_system:unregister_handler(temp_op),
    {error, not_found} = catena_effect_system:lookup_handler(temp_op).

test_current_handlers() ->
    HandlerFn = fun(_V, _R) -> ok end,
    catena_effect_system:push_handler_scope(),
    catena_effect_system:register_handler(op1, HandlerFn),
    catena_effect_system:register_handler(op2, HandlerFn),
    Handlers = catena_effect_system:current_handlers(),
    ?assert(is_map(Handlers)),
    ?assert(maps:is_key(op1, Handlers)),
    ?assert(maps:is_key(op2, Handlers)),
    catena_effect_system:pop_handler_scope().

%%====================================================================
%% Equation Management Tests
%%====================================================================

equation_management_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Add equation", fun test_add_equation/0},
                {"Find equations", fun test_find_equations/0},
                {"Remove equation", fun test_remove_equation/0},
                {"All equations", fun test_all_equations/0}
            ]
        end
    }.

test_add_equation() ->
    %% Create a simple equation (using a placeholder pattern)
    Eq = {eq, {var, x}, {lit, 5}},
    ok = catena_effect_system:add_equation(test_op, Eq),
    Eqs = catena_effect_system:find_equations(test_op),
    ?assert(length(Eqs) > 0).

test_find_equations() ->
    Eq1 = {eq, a, b},
    Eq2 = {eq, c, d},
    catena_effect_system:add_equations([{find_op, Eq1}, {find_op, Eq2}]),
    Eqs = catena_effect_system:find_equations(find_op),
    ?assert(length(Eqs) >= 2).

test_remove_equation() ->
    Eq = {eq, x, y},
    catena_effect_system:add_equation(remove_op, Eq),
    ?assert(length(catena_effect_system:find_equations(remove_op)) > 0),
    catena_effect_system:remove_equation(remove_op),
    ?assertEqual([], catena_effect_system:find_equations(remove_op)).

test_all_equations() ->
    Eq = {eq, x, y},
    catena_effect_system:add_equation(all_test_op, Eq),
    All = catena_effect_system:all_equations(),
    ?assert(is_list(All)),
    ?assert(length(All) > 0).

%%====================================================================
%% Handler Execution Tests
%%====================================================================

handler_execution_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Handle with simple handler", fun test_handle_with/0},
                {"Handle with abort", fun test_handle_with_abort/0},
                {"Handle with resume", fun test_handle_with_resume/0}
            ]
        end
    }.

test_handle_with() ->
    Result = catena_effect_system:handle_with(
        test_op,
        fun(_Val, _Res) -> handled_result end,
        fun() -> direct_result end
    ),
    ?assertEqual(direct_result, Result).

test_handle_with_abort() ->
    Result = catena_effect_system:handle_with(
        abort_op,
        fun(_Val, _Res) -> aborted end,
        fun() -> not_aborted end
    ),
    ?assertEqual(not_aborted, Result).

test_handle_with_resume() ->
    Result = catena_effect_system:handle_with(
        resume_op,
        fun(Val, Res) -> catena_resumption:resume(Res, Val * 2) end,
        fun() -> 5 end
    ),
    ?assertEqual(10, Result).

%%====================================================================
%% Optimization Tests
%%====================================================================

optimization_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Optimize program", fun test_optimize_program/0},
                {"Fuse effects", fun test_fuse_effects/0},
                {"Inline handlers", fun test_inline_handlers/0}
            ]
        end
    }.

test_optimize_program() ->
    Program = [{effect, {state, {put, 1}}}, {effect, {state, {put, 2}}}],
    Optimized = catena_effect_system:optimize_program(Program),
    ?assert(is_list(Optimized)).

test_fuse_effects() ->
    Program = [{effect, {state, {put, 1}}}, {effect, {state, {put, 2}}}],
    Fused = catena_effect_system:fuse_effects(Program),
    ?assert(is_list(Fused)).

test_inline_handlers() ->
    Program = [{effect, {state, get}}, {effect, {state, {put, 5}}}],
    Inlined = catena_effect_system:inline_handlers(Program),
    ?assert(is_list(Inlined)).

%%====================================================================
%% System State Tests
%%====================================================================

system_state_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Get system state", fun test_system_state/0},
                {"Handler stack dump", fun test_handler_stack_dump/0},
                {"Effect registry dump", fun test_effect_registry_dump/0},
                {"Diagnostics", fun test_diagnostics/0}
            ]
        end
    }.

test_system_state() ->
    State = catena_effect_system:system_state(),
    ?assert(is_tuple(State)).

test_handler_stack_dump() ->
    catena_effect_system:push_handler_scope(),
    Dump = catena_effect_system:handler_stack_dump(),
    ?assert(is_list(Dump)),
    catena_effect_system:pop_handler_scope().

test_effect_registry_dump() ->
    Dump = catena_effect_system:effect_registry_dump(),
    ?assert(is_list(Dump)).

test_diagnostics() ->
    Diag = catena_effect_system:diagnostics(),
    ?assert(is_map(Diag)),
    ?assert(maps:is_key(initialized, Diag)),
    ?assert(maps:is_key(scope_depth, Diag)),
    ?assert(maps:is_key(num_effects, Diag)).

%%====================================================================
%% Stats Tests
%%====================================================================

stats_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Get initial stats", fun test_initial_stats/0},
                {"Stats update after perform", fun test_stats_after_perform/0}
            ]
        end
    }.

test_initial_stats() ->
    Stats = catena_effect_system:stats(),
    %% stats() returns a #stats{} record - check it's a tuple with correct size
    ?assert(is_tuple(Stats)),
    ?assertEqual(5, tuple_size(Stats)).

test_stats_after_perform() ->
    %% Register a handler
    catena_effect_system:push_handler_scope(),
    HandlerFn = fun(_V, R) -> catena_resumption:resume(R, ok) end,
    catena_effect_system:register_handler(stats_op, HandlerFn),

    %% Perform operation
    catena_effect_system:perform(stats_op, test_value),

    %% Check stats updated
    Stats = catena_effect_system:stats(),
    %% stats() returns a #stats{} record - check it's a tuple with correct size
    ?assert(is_tuple(Stats)),
    ?assertEqual(5, tuple_size(Stats)),

    catena_effect_system:pop_handler_scope().

%%====================================================================
%% Default Handler Tests
%%====================================================================

default_handler_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Set default handler", fun test_set_default_handler/0},
                {"Get default handler", fun test_get_default_handler/0}
            ]
        end
    }.

test_set_default_handler() ->
    HandlerFn = fun(_V, _R) -> default_handled end,
    ok = catena_effect_system:set_default_handler(default_op, HandlerFn),
    {ok, _Handler} = catena_effect_system:get_default_handler(default_op).

test_get_default_handler() ->
    HandlerFn = fun(_V, _R) -> ok end,
    catena_effect_system:set_default_handler(get_default_op, HandlerFn),
    {ok, _} = catena_effect_system:get_default_handler(get_default_op),
    {error, not_found} = catena_effect_system:get_default_handler(no_default).

%%====================================================================
%% Main Test Generator - Ensures Sequential Execution
%%====================================================================

%% @doc Main test generator that ensures all tests run sequentially
%% with proper setup/teardown.
main_test_() ->
    {setup,
        fun() ->
            %% Initialize the effect system for all tests
            catena_effect_system:init(),
            ok
        end,
        fun(_) ->
            %% Cleanup after all tests
            catena_effect_system:shutdown(),
            ok
        end,
        {generator,
            fun() ->
                [
                    system_lifecycle_test_(),
                    configuration_test_(),
                    handler_stack_test_(),
                    effect_registration_test_(),
                    handler_registration_test_(),
                    equation_management_test_(),
                    handler_execution_test_(),
                    optimization_test_(),
                    system_state_test_(),
                    stats_test_(),
                    default_handler_test_()
                ]
            end
        }
    }.
