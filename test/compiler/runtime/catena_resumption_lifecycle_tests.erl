-module(catena_resumption_lifecycle_tests).
-include_lib("eunit/include/eunit.hrl").

catena_resumption_lifecycle_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun retained_frame_lease_survives_scope_exit/0,
            fun discard_is_idempotent_and_releases_lease/0,
            fun control_abort_discards_unreturned_authority/0,
            fun owner_death_revokes_and_releases/0,
            fun provider_death_revokes_and_releases/0,
            fun expired_delimiter_fails_before_invocation/0,
            fun timeout_consumes_and_releases/0,
            fun handler_exception_is_normalized/0,
            fun provider_timeout_is_normalized/0,
            fun cleanup_while_running_is_structured/0
        ]}.

setup() ->
    catena_resumption_runtime:reset_for_test().

cleanup(_State) ->
    catena_resumption_runtime:reset_for_test().

retained_frame_lease_survives_scope_exit() ->
    Resumption = retained_resumption(
        fun(Value, _Ctx) -> {resumed, Value, self()} end
    ),
    ?assertEqual(
        {ok, active},
        catena_resumption_runtime:lease_status(Resumption)
    ),
    ?assertEqual(
        {resumed, value, self()},
        catena_effect_runtime:resume(Resumption, value)
    ),
    ?assertEqual(
        {ok, released},
        catena_resumption_runtime:lease_status(Resumption)
    ).

discard_is_idempotent_and_releases_lease() ->
    {ok, Resumption} = capture(
        fun(Value, _Ctx) -> Value end,
        #{}
    ),
    ?assertEqual(ok, catena_resumption_runtime:discard(Resumption)),
    ?assertEqual(ok, catena_resumption_runtime:discard(Resumption)),
    ?assertEqual(
        {ok, released},
        catena_resumption_runtime:lease_status(Resumption)
    ),
    ?assertMatch(
        {error, #{category := resumption_already_consumed}},
        catena_resumption_runtime:resume(Resumption, value)
    ).

control_abort_discards_unreturned_authority() ->
    TestProcess = self(),
    Case = catena_effect_runtime:control_case(
        stop,
        0,
        fun([], Resumption, _HandlerCtx) ->
            TestProcess ! {aborted_resumption, Resumption},
            aborted
        end
    ),
    ?assertEqual(
        aborted,
        catena_effect_runtime:with_resumable_handler(
            catena_effect_runtime:empty_context(),
            handler('Abort', [Case]),
            fun(Ctx) ->
                catena_effect_runtime:perform_cps(
                    Ctx,
                    'Abort',
                    stop,
                    [],
                    fun(Value, _RestoredCtx) -> Value end
                )
            end
        )
    ),
    receive
        {aborted_resumption, Resumption} ->
            ?assertEqual(
                {ok, consumed},
                catena_resumption_runtime:status(Resumption)
            ),
            ?assertEqual(
                {ok, released},
                catena_resumption_runtime:lease_status(Resumption)
            )
    after 1000 ->
        ?assert(false)
    end.

owner_death_revokes_and_releases() ->
    Parent = self(),
    {Owner, OwnerMonitor} = spawn_monitor(fun() ->
        {ok, Resumption} = capture(
            fun(Value, _Ctx) -> Value end,
            #{}
        ),
        Parent ! {owner_resumption, Resumption}
    end),
    Resumption = receive
        {owner_resumption, Handle} -> Handle
    after 1000 ->
        error(owner_capture_timeout)
    end,
    receive
        {'DOWN', OwnerMonitor, process, Owner, normal} -> ok
    after 1000 ->
        error(owner_exit_timeout)
    end,
    ?assertMatch(
        {error, #{category := expired_resumption_owner}},
        catena_resumption_runtime:resume(Resumption, value)
    ),
    ?assertEqual(
        {ok, released},
        catena_resumption_runtime:lease_status(Resumption)
    ).

provider_death_revokes_and_releases() ->
    Provider = spawn(fun provider_loop/0),
    ProviderMonitor = erlang:monitor(process, Provider),
    {ok, Resumption} = capture(
        fun(Value, _Ctx) -> Value end,
        #{providers => [Provider]}
    ),
    exit(Provider, kill),
    receive
        {'DOWN', ProviderMonitor, process, Provider, killed} -> ok
    after 1000 ->
        error(provider_exit_timeout)
    end,
    ?assertMatch(
        {error, #{
            category := handler_failure,
            details := #{reason := provider_unavailable}
        }},
        catena_resumption_runtime:resume(Resumption, value)
    ),
    ?assertEqual(
        {ok, released},
        catena_resumption_runtime:lease_status(Resumption)
    ).

expired_delimiter_fails_before_invocation() ->
    {ok, Resumption} = capture(
        fun(_Value, _Ctx) -> erlang:error(must_not_run) end,
        #{}
    ),
    ?assertEqual(
        ok,
        catena_resumption_runtime:expire_delimiter(Resumption)
    ),
    ?assertMatch(
        {error, #{
            category := stale_resumption_delimiter,
            details := #{reason := expired_frame}
        }},
        catena_resumption_runtime:resume(Resumption, value)
    ),
    ?assertEqual(
        {ok, released},
        catena_resumption_runtime:lease_status(Resumption)
    ).

timeout_consumes_and_releases() ->
    Owner = self(),
    {ok, Resumption} = capture(
        fun(_Value, _Ctx) ->
            timer:sleep(10),
            self()
        end,
        #{}
    ),
    ?assertMatch(
        {error, #{
            category := handler_failure,
            details := #{reason := timeout}
        }},
        catena_resumption_runtime:resume(Resumption, value, 1)
    ),
    ?assertEqual(Owner, self()),
    ?assertEqual(
        {ok, consumed},
        catena_resumption_runtime:status(Resumption)
    ),
    ?assertEqual(
        {ok, released},
        catena_resumption_runtime:lease_status(Resumption)
    ).

handler_exception_is_normalized() ->
    Case = catena_effect_runtime:value_case(
        fail,
        0,
        fun([], _HandlerCtx) ->
            erlang:error({private, make_ref(), fun() -> secret end})
        end
    ),
    Result = catena_effect_runtime:with_resumable_handler(
        catena_effect_runtime:empty_context(),
        handler('Failure', [Case]),
        fun(Ctx) ->
            catena_effect_runtime:perform_cps(
                Ctx,
                'Failure',
                fail,
                [],
                fun(Value, _RestoredCtx) -> Value end
            )
        end
    ),
    ?assertMatch(
        {error, #{
            category := handler_failure,
            origin := {runtime_value_case, fail},
            details := #{class := error, reason := handler_failed}
        }},
        Result
    ).

provider_timeout_is_normalized() ->
    Context = catena_effect_runtime:new_context(#{timeout => 1}),
    Result = catena_effect_runtime:with_handlers(
        Context,
        [{'Slow', [{wait, fun() -> timer:sleep(20), done end}]}],
        fun(Ctx) ->
            catena_effect_runtime:perform_cps(
                Ctx,
                'Slow',
                wait,
                [],
                fun(Value, _RestoredCtx) -> Value end
            )
        end
    ),
    ?assertMatch(
        {error, #{
            category := handler_failure,
            details := #{
                reason := timeout,
                effect := 'Slow',
                operation := wait
            }
        }},
        Result
    ).

cleanup_while_running_is_structured() ->
    Continuation = fun(Resumption, _Ctx) ->
        catena_resumption_runtime:discard(Resumption)
    end,
    {ok, Resumption} = capture(Continuation, #{}),
    ?assertMatch(
        {ok, {error, #{
            category := handler_failure,
            details := #{reason := cleanup_while_running}
        }}},
        catena_resumption_runtime:resume(Resumption, Resumption)
    ),
    ?assertEqual(
        {ok, consumed},
        catena_resumption_runtime:status(Resumption)
    ),
    ?assertEqual(
        {ok, released},
        catena_resumption_runtime:lease_status(Resumption)
    ).

retained_resumption(Continuation) ->
    Case = catena_effect_runtime:control_case(
        retain,
        0,
        fun([], Resumption, _HandlerCtx) -> Resumption end
    ),
    catena_effect_runtime:with_resumable_handler(
        catena_effect_runtime:empty_context(),
        handler('Retain', [Case]),
        fun(Ctx) ->
            catena_effect_runtime:perform_cps(
                Ctx,
                'Retain',
                retain,
                [],
                Continuation
            )
        end
    ).

capture(Continuation, ExtraSpec) ->
    Base = #{
        context => catena_effect_runtime:empty_context(),
        delimiter => make_ref(),
        depth => deep,
        kind => one_shot,
        origin => {test, ?MODULE},
        metadata => #{fixture => lifecycle},
        type_identity => dynamic
    },
    catena_resumption_runtime:capture(
        Continuation,
        maps:merge(Base, ExtraSpec)
    ).

handler(Effect, Cases) ->
    #{
        effect => Effect,
        cases => Cases,
        origin => {test_handler, Effect}
    }.

provider_loop() ->
    receive
        stop -> ok
    end.
