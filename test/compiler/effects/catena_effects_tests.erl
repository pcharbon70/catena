%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_effects (Phase 6.2)
%%%
%%% Tests for the expanded effect library including:
%%% - State effect (get/put/modify)
%%% - Reader effect (ask/local)
%%% - Writer effect (tell/listen/pass)
%%% - Async effect (spawn/await/yield)
%%% - Error effect (throw/catch)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effects_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% State Effect Tests
%%%=============================================================================

state_get_test() ->
    Effect = catena_effects:state_get(),
    ?assertMatch({effect, {state, get}}, Effect).

state_put_test() ->
    Effect = catena_effects:state_put(42),
    ?assertMatch({effect, {state, {put, 42}}}, Effect).

state_modify_test() ->
    Fun = fun(X) -> X + 1 end,
    Effect = catena_effects:state_modify(Fun),
    ?assertMatch({effect, {state, {modify, _}}}, Effect).

state_get_and_put_test() ->
    Fun = fun(X) -> {X, X * 2} end,
    Effect = catena_effects:state_get_and_put(Fun),
    ?assertMatch({effect, {state, {get_and_put, _}}}, Effect).

state_type_test() ->
    Effect = catena_effects:state(my_state),
    ?assertEqual({effect, {state, my_state}}, Effect).

%%%=============================================================================
%%% Reader Effect Tests
%%%=============================================================================

reader_ask_test() ->
    Effect = catena_effects:reader_ask(),
    ?assertMatch({effect, {reader, ask}}, Effect).

reader_local_test() ->
    Fun = fun() -> ok end,
    Effect = catena_effects:reader_local(Fun),
    ?assertMatch({effect, {reader, {local, _}}}, Effect).

reader_ask_local_test() ->
    Fun = fun(Env) -> Env end,
    Effect = catena_effects:reader_ask_local(Fun),
    ?assertMatch({effect, {reader, {ask_local, _}}}, Effect).

reader_type_test() ->
    Effect = catena_effects:reader(config),
    ?assertEqual({effect, {reader, config}}, Effect).

%%%=============================================================================
%%% Writer Effect Tests
%%%=============================================================================

writer_tell_test() ->
    Effect = catena_effects:writer_tell(log_message),
    ?assertMatch({effect, {writer, {tell, log_message}}}, Effect).

writer_listen_test() ->
    Fun = fun(Output) -> Output end,
    Effect = catena_effects:writer_listen(Fun),
    ?assertMatch({effect, {writer, {listen, _}}}, Effect).

writer_pass_test() ->
    Effect = catena_effects:writer_pass(value),
    ?assertMatch({effect, {writer, {pass, value}}}, Effect).

writer_type_test() ->
    Effect = catena_effects:writer(log),
    ?assertEqual({effect, {writer, log}}, Effect).

%%%=============================================================================
%%% Async Effect Tests
%%%=============================================================================

async_spawn_test() ->
    Fun = fun() -> result end,
    Effect = catena_effects:async_spawn(Fun),
    ?assertMatch({effect, {async, {spawn, _}}}, Effect).

async_await_test() ->
    Effect = catena_effects:async_await(future_ref),
    ?assertMatch({effect, {async, {await, future_ref}}}, Effect).

async_yield_test() ->
    Effect = catena_effects:async_yield(intermediate_value),
    ?assertMatch({effect, {async, {yield, intermediate_value}}}, Effect).

async_type_test() ->
    Effect = catena_effects:async(),
    ?assertEqual({effect, {async}}, Effect).

%%%=============================================================================
%%% Error Effect Tests
%%%=============================================================================

error_throw_test() ->
    Effect = catena_effects:error_throw(my_error),
    ?assertMatch({effect, {error, {throw, my_error}}}, Effect).

error_catch_test() ->
    ErrorFun = fun() -> ok end,
    HandlerFun = fun() -> recovered end,
    Effect = catena_effects:error_catch(ErrorFun, HandlerFun),
    ?assertMatch({effect, {error, {'catch', _, _}}}, Effect).

error_type_test() ->
    Effect = catena_effects:error(validation),
    ?assertEqual({effect, {error, validation}}, Effect).

%%%=============================================================================
%%% Effect Combinator Tests
%%%=============================================================================

combine_effects_test() ->
    StateEff = catena_effects:state(my_state),
    ReaderEff = catena_effects:reader(config),
    Combined = catena_effects:combine_effects([StateEff, ReaderEff]),
    ?assertMatch({effect, {combined, [_, _]}}, Combined).

run_with_test() ->
    Effect = catena_effects:state(my_state),
    Computation = fun() -> result end,
    RunEffect = catena_effects:run_with(Effect, Computation),
    ?assertMatch({effect, {run_with, _, _}}, RunEffect).

handle_pure_test() ->
    Computation = fun() -> pure_result end,
    Effect = catena_effects:handle_pure(Computation),
    ?assertMatch({effect, {handle_pure, _}}, Effect).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

state_reader_integration_test() ->
    % Test combining State and Reader effects
    StateEff = catena_effects:state(counter),
    ReaderEff = catena_effects:reader(initial_value),

    Combined = catena_effects:combine_effects([StateEff, ReaderEff]),
    ?assertMatch({effect, {combined, [_, _]}}, Combined),

    % Verify we can extract individual effects from the combined list
    EffectsList = element(2, element(2, Combined)),
    ?assertMatch({effect, {state, counter}}, lists:nth(1, EffectsList)),
    ?assertMatch({effect, {reader, initial_value}}, lists:nth(2, EffectsList)).

writer_async_integration_test() ->
    % Test Writer effect for logging async operations
    WriterEff = catena_effects:writer(log),
    AsyncEff = catena_effects:async(),

    Combined = catena_effects:combine_effects([WriterEff, AsyncEff]),
    ?assertMatch({effect, {combined, [_, _]}}, Combined),

    % Verify both effects are present from the combined list
    EffectsList = element(2, element(2, Combined)),
    ?assertMatch({effect, {writer, log}}, lists:nth(1, EffectsList)),
    ?assertMatch({effect, {async}}, lists:nth(2, EffectsList)).

error_state_integration_test() ->
    % Test Error effect with State effect for rollback
    ErrorEff = catena_effects:error(validation),
    StateEff = catena_effects:state(current_state),

    Combined = catena_effects:combine_effects([ErrorEff, StateEff]),
    ?assertMatch({effect, {combined, [_, _]}}, Combined),

    % Verify both effects are present from the combined list
    EffectsList = element(2, element(2, Combined)),
    ?assertMatch({effect, {error, validation}}, lists:nth(1, EffectsList)),
    ?assertMatch({effect, {state, current_state}}, lists:nth(2, EffectsList)).
