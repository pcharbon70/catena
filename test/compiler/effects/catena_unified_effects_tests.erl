%%%-------------------------------------------------------------------
%%% @doc Catena Unified Effects API Tests (Phase 14.1)
%%%
%%% Tests for the unified effect API provided by catena_effects
%%% that integrates all effect system components.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_unified_effects_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    catena_effects:init().

cleanup(_) ->
    catena_effects:shutdown().

%%====================================================================
%% System Lifecycle Tests
%%====================================================================

lifecycle_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Initialize system", fun test_init/0},
                {"Check initialized state", fun test_is_initialized/0},
                {"Configure system", fun test_configure/0},
                {"Shutdown system", fun test_shutdown/0}
            ]
        end
    }.

test_init() ->
    ?assert(catena_effects:is_initialized()).

test_is_initialized() ->
    ?assert(catena_effects:is_initialized()),
    catena_effects:shutdown(),
    ?assertNot(catena_effects:is_initialized()),
    catena_effects:init(),
    ?assert(catena_effects:is_initialized()).

test_configure() ->
    ok = catena_effects:configure(#{
        optimization_level => 2,
        enable_equations => true,
        trace => false
    }),
    ?assert(catena_effects:is_initialized()).

test_shutdown() ->
    ?assert(catena_effects:is_initialized()),
    catena_effects:shutdown(),
    ?assertNot(catena_effects:is_initialized()),
    catena_effects:init().  %% Reinitialize for cleanup

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
                {"Register effect with metadata", fun test_register_effect_with_metadata/0},
                {"List effects", fun test_list_effects/0},
                {"Unregister effect", fun test_unregister_effect/0}
            ]
        end
    }.

test_register_effect() ->
    ok = catena_effects:register_effect(test_effect, [op1, op2]),
    Effects = catena_effects:list_effects(),
    ?assert(lists:member(test_effect, Effects)).

test_register_effect_with_metadata() ->
    ok = catena_effects:register_effect(meta_effect, [op1], #{version => 1}),
    Effects = catena_effects:list_effects(),
    ?assert(lists:member(meta_effect, Effects)).

test_list_effects() ->
    Effects = catena_effects:list_effects(),
    ?assert(is_list(Effects)),
    ?assert(lists:member(state, Effects)),
    ?assert(lists:member(reader, Effects)),
    ?assert(lists:member(writer, Effects)).

test_unregister_effect() ->
    ok = catena_effects:register_effect(temp_effect, [op]),
    ?assert(lists:member(temp_effect, catena_effects:list_effects())),
    ok = catena_effects:unregister_effect(temp_effect),
    ?assertNot(lists:member(temp_effect, catena_effects:list_effects())).

%%====================================================================
%% Handler Management Tests
%%====================================================================

handler_management_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Register handler", fun test_register_handler/0},
                {"Lookup handler", fun test_lookup_handler/0},
                {"Unregister handler", fun test_unregister_handler/0},
                {"Current handlers", fun test_current_handlers/0}
            ]
        end
    }.

test_register_handler() ->
    HandlerFn = fun(_V, _R) -> ok end,
    ok = catena_effects:register_handler(test_op, HandlerFn),
    {ok, _} = catena_effects:lookup_handler(test_op).

test_lookup_handler() ->
    HandlerFn = fun(_V, _R) -> result end,
    catena_effects:register_handler(lookup_test_op, HandlerFn),
    {ok, _} = catena_effects:lookup_handler(lookup_test_op),
    {error, not_found} = catena_effects:lookup_handler(nonexistent_op).

test_unregister_handler() ->
    HandlerFn = fun(_V, _R) -> ok end,
    catena_effects:register_handler(unreg_op, HandlerFn),
    {ok, _} = catena_effects:lookup_handler(unreg_op),
    ok = catena_effects:unregister_handler(unreg_op),
    {error, not_found} = catena_effects:lookup_handler(unreg_op).

test_current_handlers() ->
    HandlerFn = fun(_V, _R) -> ok end,
    catena_effects:push_handler_scope(),
    catena_effects:register_handler(op1, HandlerFn),
    catena_effects:register_handler(op2, HandlerFn),
    Handlers = catena_effects:current_handlers(),
    ?assert(is_map(Handlers)),
    ?assert(maps:is_key(op1, Handlers)),
    ?assert(maps:is_key(op2, Handlers)),
    catena_effects:pop_handler_scope().

%%====================================================================
%% State Effect Tests
%%====================================================================

state_effect_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Run state computation", fun test_run_state/0},
                {"State get and put", fun test_state_get_put/0},
                {"State modify", fun test_state_modify/0},
                {"Eval state", fun test_eval_state/0}
            ]
        end
    }.

test_run_state() ->
    {Result, FinalState} = catena_effects:run_state(
        fun() ->
            S = catena_effects:state_get(),
            catena_effects:state_put(S * 2),
            catena_effects:state_get()
        end,
        5
    ),
    ?assertEqual(10, Result),
    ?assertEqual(10, FinalState).

test_state_get_put() ->
    {Result, FinalState} = catena_effects:run_state(
        fun() ->
            catena_effects:state_put(42),
            catena_effects:state_get()
        end,
        0
    ),
    ?assertEqual(42, Result),
    ?assertEqual(42, FinalState).

test_state_modify() ->
    {Result, FinalState} = catena_effects:run_state(
        fun() ->
            catena_effects:state_modify(fun(S) -> S + 10 end)
        end,
        5
    ),
    ?assertEqual(15, Result),
    ?assertEqual(15, FinalState).

test_eval_state() ->
    Result = catena_effects:eval_state(
        fun() ->
            S = catena_effects:state_get(),
            catena_effects:state_put(S * 3),
            S * 2
        end,
        7
    ),
    ?assertEqual(14, Result).

%%====================================================================
%% Reader Effect Tests
%%====================================================================

reader_effect_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Run reader computation", fun test_run_reader/0},
                {"Reader ask", fun test_reader_ask/0},
                {"Reader local", fun test_reader_local/0}
            ]
        end
    }.

test_run_reader() ->
    Result = catena_effects:run_reader(
        fun() ->
            Env = catena_effects:reader_ask(),
            Env * 2
        end,
        21
    ),
    ?assertEqual(42, Result).

test_reader_ask() ->
    Result = catena_effects:run_reader(
        fun() ->
            catena_effects:reader_ask()
        end,
        test_env
    ),
    ?assertEqual(test_env, Result).

test_reader_local() ->
    Result = catena_effects:run_reader(
        fun() ->
            Outer = catena_effects:reader_ask(),
            InnerResult = catena_effects:reader_local(fun() ->
                Inner = catena_effects:reader_ask(),
                {outer, Outer, inner, Inner}
            end),
            InnerResult
        end,
        outer_value
    ),
    ?assertEqual({outer, outer_value, inner, outer_value}, Result).

%%====================================================================
%% Writer Effect Tests
%%====================================================================

writer_effect_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Run writer computation", fun test_run_writer/0},
                {"Writer tell", fun test_writer_tell/0},
                {"Writer collect output", fun test_writer_collect/0}
            ]
        end
    }.

test_run_writer() ->
    {Result, Output} = catena_effects:run_writer(
        fun() ->
            catena_effects:writer_tell("hello"),
            catena_effects:writer_tell(" "),
            catena_effects:writer_tell("world"),
            result_value
        end
    ),
    ?assertEqual(result_value, Result),
    ?assertEqual(["hello", " ", "world"], Output).

test_writer_tell() ->
    {_, Output} = catena_effects:run_writer(
        fun() ->
            catena_effects:tell(1),
            catena_effects:tell(2),
            catena_effects:tell(3)
        end
    ),
    ?assertEqual([1, 2, 3], Output).

test_writer_collect() ->
    Outputs = lists:map(fun(N) ->
        {_, Out} = catena_effects:run_writer(
            fun() ->
                lists:foreach(fun(I) -> catena_effects:tell(I) end, lists:seq(1, N))
            end
        ),
        Out
    end, [1, 2, 3]),
    ?assertEqual([[1], [1, 2], [1, 2, 3]], Outputs).

%%====================================================================
%% Error Effect Tests
%%====================================================================

error_effect_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Run error computation - success path", fun test_run_error_success/0},
                {"Run error computation - error path", fun test_run_error_error/0},
                {"Error throw and catch", fun test_error_throw_catch/0}
            ]
        end
    }.

test_run_error_success() ->
    Result = catena_effects:run_error(
        fun() ->
            success_value
        end,
        fun(Error) -> {error, Error} end
    ),
    ?assertEqual(success_value, Result).

test_run_error_error() ->
    Result = catena_effects:run_error(
        fun() ->
            catena_effects:error_throw(test_error)
        end,
        fun(Error) -> {caught, Error} end
    ),
    ?assertEqual({caught, test_error}, Result).

test_error_throw_catch() ->
    Result = catena_effects:catch_error(
        fun() ->
            catena_effects:throw(error_value)
        end,
        fun(E) -> {error, E} end
    ),
    ?assertEqual({error, error_value}, Result).

%%====================================================================
%% Effect Operation Tests
%%====================================================================

effect_operations_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Perform operation", fun test_perform/0},
                {"Try perform - success", fun test_try_perform_success/0},
                {"Try perform - failure", fun test_try_perform_failure/0},
                {"Handle computation", fun test_handle/0}
            ]
        end
    }.

test_perform() ->
    catena_effects:push_handler_scope(),
    HandlerFn = fun(Val, Res) -> catena_effects:resume(Res, Val * 2) end,
    catena_effects:register_handler(perform_op, HandlerFn),
    Result = catena_effects:perform(perform_op, 5),
    ?assertEqual(10, Result),
    catena_effects:pop_handler_scope().

test_try_perform_success() ->
    catena_effects:push_handler_scope(),
    HandlerFn = fun(Val, Res) -> catena_effects:resume(Res, Val) end,
    catena_effects:register_handler(try_op, HandlerFn),
    {ok, Result} = catena_effects:try_perform(try_op, success_value),
    ?assertEqual(success_value, Result),
    catena_effects:pop_handler_scope().

test_try_perform_failure() ->
    Result = catena_effects:try_perform(no_handler, value),
    ?assertEqual({error, {unhandled_effect, no_handler}}, Result).

test_handle() ->
    Result = catena_effects:handle(
        handle_op,
        fun(Val, Res) -> catena_effects:resume(Res, {handled, Val}) end,
        fun() -> original_value end
    ),
    ?assertEqual(original_value, Result).

%%====================================================================
%% Optimization Tests
%%====================================================================

optimization_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Optimize program", fun test_optimize/0},
                {"Optimize with level", fun test_optimize_with_level/0},
                {"Add equation", fun test_add_equation/0},
                {"Apply equations", fun test_apply_equations/0}
            ]
        end
    }.

test_optimize() ->
    Program = [{effect, {state, {put, 1}}}, {effect, {state, {put, 2}}}],
    Optimized = catena_effects:optimize(Program),
    ?assert(is_list(Optimized)).

test_optimize_with_level() ->
    Program = [{effect, {state, {put, 1}}}, {effect, {state, {put, 2}}}],
    Optimized0 = catena_effects:optimize(Program, 0),
    Optimized3 = catena_effects:optimize(Program, 3),
    ?assert(is_list(Optimized0)),
    ?assert(is_list(Optimized3)).

test_add_equation() ->
    Eq = {eq, x, y},
    ok = catena_effects:add_equation(test_eq_op, Eq),
    ?assertEqual(ok, ok).  %% Just check it doesn't crash

test_apply_equations() ->
    Expr = {some, expression},
    Result = catena_effects:apply_equations(Expr),
    ?assert(is_tuple(Result)).

%%====================================================================
%% Diagnostics Tests
%%====================================================================

diagnostics_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Get diagnostics", fun test_diagnostics/0},
                {"Get stats", fun test_stats/0},
                {"Handler stack", fun test_handler_stack/0}
            ]
        end
    }.

test_diagnostics() ->
    Diag = catena_effects:diagnostics(),
    ?assert(is_map(Diag)),
    ?assert(maps:is_key(initialized, Diag)),
    ?assert(maps:is_key(scope_depth, Diag)),
    ?assert(maps:is_key(num_effects, Diag)).

test_stats() ->
    Stats = catena_effects:stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(performs, Stats)),
    ?assert(maps:is_key(handler_executions, Stats)).

test_handler_stack() ->
    catena_effects:push_handler_scope(),
    Stack = catena_effects:handler_stack(),
    ?assert(is_list(Stack)),
    ?assert(length(Stack) > 0),
    catena_effects:pop_handler_scope().

%%====================================================================
%% Continuation Tests
%%====================================================================

continuation_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Capture resumption", fun test_capture_resumption/0},
                {"Resume computation", fun test_resume/0},
                {"Continuation kind checks", fun test_continuation_kind/0}
            ]
        end
    }.

test_capture_resumption() ->
    Resumption = catena_effects:capture_resumption(),
    ?assert(is_function(Resumption)).

test_resume() ->
    catena_effects:push_handler_scope(),
    HandlerFn = fun(_V, Res) ->
        catena_effects:resume(Res, resumed_value)
    end,
    catena_effects:register_handler(resume_test_op, HandlerFn),
    Result = catena_effects:perform(resume_test_op, test_value),
    ?assertEqual(resumed_value, Result),
    catena_effects:pop_handler_scope().

test_continuation_kind() ->
    %% Default is multi-shot
    ?assert(catena_effects:is_multi_shot()),
    %% One-shot depends on handler configuration
    ?assert(is_boolean(catena_effects:is_one_shot())).

%%====================================================================
%% Type System Integration Tests
%%====================================================================

type_system_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_) ->
            [
                {"Infer effect type", fun test_infer_effect_type/0},
                {"Check effect type", fun test_check_effect_type/0},
                {"Generalize effects", fun test_generalize_effects/0}
            ]
        end
    }.

test_infer_effect_type() ->
    Result = catena_effects:infer_effect_type({some, expression}),
    ?assertMatch({ok, _}, Result).

test_check_effect_type() ->
    Env = #{},
    Result = catena_effects:check_effect_type({expr}, Env),
    case Result of
        {ok, _} -> ok;
        {error, _} -> ok
    end.

test_generalize_effects() ->
    Result = catena_effects:generalize_effects({type, expr}),
    ?assert(is_tuple(Result)).
