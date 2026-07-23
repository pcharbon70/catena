%%%-------------------------------------------------------------------
%%% @doc Public execution-contract tests for catena_effects.
%%%
%%% The unified effect API performs operations through installed handlers.
%%% Raw `{effect, ...}` descriptors belong to the optimizer and distribution
%%% components and are intentionally not constructed by this module.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effects_tests).

-include_lib("eunit/include/eunit.hrl").

effect_library_test_() ->
    [
        {"builtin effects are registered", isolated(fun builtin_effects_registered/0)},
        {"state get returns current state", isolated(fun state_get/0)},
        {"state put updates state", isolated(fun state_put/0)},
        {"state modify updates state", isolated(fun state_modify/0)},
        {"state get-and-put returns prior state", isolated(fun state_get_and_put/0)},
        {"eval state returns computation value", isolated(fun eval_state/0)},
        {"reader ask returns environment", isolated(fun reader_ask/0)},
        {"reader ask-local maps environment", isolated(fun reader_ask_local/0)},
        {"reader local executes in scope", isolated(fun reader_local/0)},
        {"writer tell accumulates output", isolated(fun writer_tell/0)},
        {"writer listen isolates inner output", isolated(fun writer_listen/0)},
        {"writer pass returns its value", isolated(fun writer_pass/0)},
        {"error runner preserves success", isolated(fun error_success/0)},
        {"error runner short-circuits", isolated(fun error_short_circuit/0)},
        {"error aliases use the runner", isolated(fun error_aliases/0)},
        {"custom handler executes", isolated(fun custom_handler/0)},
        {"try-perform returns handled value", isolated(fun try_perform_success/0)},
        {"try-perform reports unhandled effect", isolated(fun try_perform_failure/0)},
        {"deep handler executes", isolated(fun deep_handler/0)},
        {"shallow handler executes", isolated(fun shallow_handler/0)},
        {"async spawn delegates to handler", isolated(fun async_spawn/0)},
        {"async await delegates to handler", isolated(fun async_await/0)},
        {"async yield delegates to handler", isolated(fun async_yield/0)},
        {"builtin runners compose", isolated(fun builtin_runners_compose/0)},
        {"diagnostics include execution stats", isolated(fun diagnostics/0)},
        {"perform fails loudly without handler", isolated(fun unhandled_perform/0)}
    ].

isolated(Test) ->
    fun() ->
        catch catena_effects:shutdown(),
        ok = catena_effects:init(),
        try
            Test()
        after
            catena_effects:shutdown()
        end
    end.

builtin_effects_registered() ->
    Effects = catena_effects:list_effects(),
    ?assertEqual(
        [async, error, reader, state, writer],
        lists:sort(Effects)
    ).

state_get() ->
    ?assertEqual({5, 5}, catena_effects:run_state(
        fun catena_effects:state_get/0,
        5
    )).

state_put() ->
    ?assertEqual({42, 42}, catena_effects:run_state(
        fun() -> catena_effects:state_put(42) end,
        0
    )).

state_modify() ->
    ?assertEqual({6, 6}, catena_effects:run_state(
        fun() -> catena_effects:state_modify(fun(Value) -> Value + 1 end) end,
        5
    )).

state_get_and_put() ->
    ?assertEqual({5, 10}, catena_effects:run_state(
        fun() -> catena_effects:state_get_and_put(fun(Value) -> Value * 2 end) end,
        5
    )).

eval_state() ->
    ?assertEqual(done, catena_effects:eval_state(
        fun() ->
            catena_effects:state_put(9),
            done
        end,
        0
    )).

reader_ask() ->
    ?assertEqual(environment, catena_effects:run_reader(
        fun catena_effects:reader_ask/0,
        environment
    )).

reader_ask_local() ->
    ?assertEqual(42, catena_effects:run_reader(
        fun() -> catena_effects:reader_ask_local(fun(Value) -> Value * 2 end) end,
        21
    )).

reader_local() ->
    ?assertEqual(environment, catena_effects:run_reader(
        fun() ->
            catena_effects:reader_local(fun catena_effects:reader_ask/0)
        end,
        environment
    )).

writer_tell() ->
    ?assertEqual({ok, [first, second]}, catena_effects:run_writer(
        fun() ->
            catena_effects:writer_tell(first),
            catena_effects:writer_tell(second)
        end
    )).

writer_listen() ->
    ?assertEqual({{inner, [message]}, []}, catena_effects:run_writer(
        fun() ->
            catena_effects:writer_listen(fun() ->
                catena_effects:writer_tell(message),
                inner
            end)
        end
    )).

writer_pass() ->
    ?assertEqual({value, []}, catena_effects:run_writer(
        fun() -> catena_effects:writer_pass(value) end
    )).

error_success() ->
    ?assertEqual(success, catena_effects:run_error(
        fun() -> success end,
        fun(Error) -> {caught, Error} end
    )).

error_short_circuit() ->
    ?assertEqual({caught, failed}, catena_effects:run_error(
        fun() ->
            catena_effects:error_throw(failed),
            unreachable
        end,
        fun(Error) -> {caught, Error} end
    )).

error_aliases() ->
    ?assertEqual({error, failed}, catena_effects:catch_error(
        fun() -> catena_effects:throw(failed) end,
        fun(Error) -> {error, Error} end
    )).

custom_handler() ->
    ?assertEqual({handled, value}, catena_effects:handle(
        custom,
        fun(Value, _Resumption) -> {handled, Value} end,
        fun() -> catena_effects:perform(custom, value) end
    )).

try_perform_success() ->
    ?assertEqual({ok, value}, catena_effects:handle(
        custom,
        fun(Value, _Resumption) -> Value end,
        fun() -> catena_effects:try_perform(custom, value) end
    )).

try_perform_failure() ->
    ?assertEqual(
        {error, {unhandled_effect, missing}},
        catena_effects:try_perform(missing, value)
    ).

deep_handler() ->
    ?assertEqual(deep, catena_effects:handle_deep(
        custom,
        fun(_Value, _Resumption) -> deep end,
        fun() -> catena_effects:perform(custom, value) end
    )).

shallow_handler() ->
    ?assertEqual(shallow, catena_effects:handle_shallow(
        custom,
        fun(_Value, _Resumption) -> shallow end,
        fun() -> catena_effects:perform(custom, value) end
    )).

async_spawn() ->
    ?assertEqual(completed, catena_effects:handle(
        async,
        fun({spawn, Computation}, _Resumption) -> Computation() end,
        fun() -> catena_effects:async_spawn(fun() -> completed end) end
    )).

async_await() ->
    ?assertEqual({awaited, future}, catena_effects:handle(
        async,
        fun({await, Future}, _Resumption) -> {awaited, Future} end,
        fun() -> catena_effects:async_await(future) end
    )).

async_yield() ->
    ?assertEqual({yielded, value}, catena_effects:handle(
        async,
        fun({yield, Value}, _Resumption) -> {yielded, Value} end,
        fun() -> catena_effects:async_yield(value) end
    )).

builtin_runners_compose() ->
    ?assertEqual({{6, 6}, [updated]}, catena_effects:run_writer(
        fun() ->
            catena_effects:run_state(
                fun() ->
                    New = catena_effects:state_modify(fun(Value) -> Value + 1 end),
                    catena_effects:writer_tell(updated),
                    New
                end,
                5
            )
        end
    )).

diagnostics() ->
    _ = catena_effects:run_state(fun catena_effects:state_get/0, 0),
    Diagnostics = catena_effects:diagnostics(),
    Stats = catena_effects:stats(),
    ?assert(maps:get(initialized, Diagnostics)),
    ?assert(maps:get(performs, Stats) >= 1),
    ?assert(maps:get(handler_executions, Stats) >= 1).

unhandled_perform() ->
    ?assertError(
        {unhandled_effect, missing},
        catena_effects:perform(missing, value)
    ).
