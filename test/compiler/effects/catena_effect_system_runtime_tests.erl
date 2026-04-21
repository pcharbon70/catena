%%%-------------------------------------------------------------------
%%% @doc Focused runtime lifecycle tests for Phase 14.3 integration.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_system_runtime_tests).

-include_lib("eunit/include/eunit.hrl").

runtime_lifecycle_test_() ->
    {setup,
        fun() ->
            catch catena_effect_system:shutdown(),
            ok
        end,
        fun(_) ->
            catch catena_effect_system:shutdown(),
            ok
        end,
        fun(_) ->
            [
                ?_test(test_start_runtime_initializes_orchestration()),
                ?_test(test_stop_runtime_shuts_orchestration_down()),
                ?_test(test_with_runtime_provides_context_and_cleans_up())
            ]
        end
    }.

test_start_runtime_initializes_orchestration() ->
    ?assertNot(catena_effect_system:is_initialized()),
    Ctx = catena_effect_system:start_runtime(),
    ?assert(catena_effect_system:is_initialized()),
    ?assert(is_map(Ctx)),
    ?assertEqual(#{}, maps:get(handlers, Ctx)),
    ?assertEqual(undefined, maps:get(parent, Ctx)).

test_stop_runtime_shuts_orchestration_down() ->
    _Ctx = catena_effect_system:start_runtime(),
    ?assert(catena_effect_system:is_initialized()),
    ok = catena_effect_system:stop_runtime(),
    ?assertNot(catena_effect_system:is_initialized()).

test_with_runtime_provides_context_and_cleans_up() ->
    ?assertNot(catena_effect_system:is_initialized()),
    Result = catena_effect_system:with_runtime(
        [{optimization_level, 3}],
        fun(Ctx) ->
            ?assert(catena_effect_system:is_initialized()),
            ?assertEqual(3, catena_effect_system:get_config(optimization_level)),
            ?assert(is_map(Ctx)),
            maps:is_key(handlers, Ctx) andalso maps:is_key(parent, Ctx)
        end
    ),
    ?assert(Result),
    ?assertNot(catena_effect_system:is_initialized()).
