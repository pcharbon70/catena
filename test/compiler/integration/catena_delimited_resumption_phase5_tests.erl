-module(catena_delimited_resumption_phase5_tests).
-include_lib("eunit/include/eunit.hrl").

catena_delimited_resumption_phase5_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun explicit_resume_returns_and_transforms_delimiter_result/0,
            fun value_cases_auto_resume_sequential_operations/0,
            fun nested_frames_shadow_and_fall_back_deterministically/0,
            fun returned_resumption_restores_deep_context_later/0,
            fun resume_preserves_mailbox_links_monitors_and_owner/0,
            fun process_provider_computes_only_operation_result/0,
            fun wrong_owner_does_not_consume_owner_authority/0,
            fun double_and_reentrant_resume_are_deterministic/0,
            fun malformed_version_and_expired_frame_fail_closed/0,
            fun provider_death_revokes_retained_context/0,
            fun owner_death_revokes_retained_context/0,
            fun failures_timeouts_and_cleanup_release_authority/0
        ]}.

setup() ->
    catena_resumption_runtime:reset_for_test().

cleanup(_State) ->
    catena_resumption_runtime:reset_for_test().

explicit_resume_returns_and_transforms_delimiter_result() ->
    Case = catena_effect_runtime:control_case(
        choose,
        0,
        fun([], Resumption, _HandlerCtx) ->
            DelimiterResult = catena_effect_runtime:resume(Resumption, 21),
            {handler_transformed, DelimiterResult + 1}
        end
    ),
    Result = with_handler(
        catena_effect_runtime:empty_context(),
        'Choice',
        [Case],
        fun(Ctx) ->
            catena_effect_runtime:perform_cps(
                Ctx,
                'Choice',
                choose,
                [],
                fun(Value, _RestoredCtx) -> Value * 2 end
            )
        end
    ),
    ?assertEqual({handler_transformed, 43}, Result).

value_cases_auto_resume_sequential_operations() ->
    Case = catena_effect_runtime:value_case(
        next,
        1,
        fun([Value], _HandlerCtx) -> Value + 1 end
    ),
    Result = with_handler(
        catena_effect_runtime:empty_context(),
        'Counter',
        [Case],
        fun(Ctx) ->
            catena_effect_runtime:perform_cps(
                Ctx,
                'Counter',
                next,
                [0],
                fun(First, FirstCtx) ->
                    catena_effect_runtime:perform_cps(
                        FirstCtx,
                        'Counter',
                        next,
                        [First],
                        fun(Second, _SecondCtx) -> {First, Second} end
                    )
                end
            )
        end
    ),
    ?assertEqual({1, 2}, Result).

nested_frames_shadow_and_fall_back_deterministically() ->
    OuterRead = catena_effect_runtime:value_case(
        read,
        0,
        fun([], _Ctx) -> outer_read end
    ),
    OuterWrite = catena_effect_runtime:value_case(
        write,
        1,
        fun([Value], _Ctx) -> {outer_write, Value} end
    ),
    InnerRead = catena_effect_runtime:value_case(
        read,
        0,
        fun([], _Ctx) -> inner_read end
    ),
    Result = with_handler(
        catena_effect_runtime:empty_context(),
        'State',
        [OuterRead, OuterWrite],
        fun(OuterCtx) ->
            with_handler(OuterCtx, 'State', [InnerRead], fun(InnerCtx) ->
                catena_effect_runtime:perform_cps(
                    InnerCtx,
                    'State',
                    read,
                    [],
                    fun(Read, ReadCtx) ->
                        catena_effect_runtime:perform_cps(
                            ReadCtx,
                            'State',
                            write,
                            [Read],
                            fun(Write, _WriteCtx) -> {Read, Write} end
                        )
                    end
                )
            end)
        end
    ),
    ?assertEqual(
        {inner_read, {outer_write, inner_read}},
        Result
    ).

returned_resumption_restores_deep_context_later() ->
    Case = catena_effect_runtime:control_case(
        step,
        1,
        fun
            ([retain], Resumption, _HandlerCtx) ->
                Resumption;
            ([Value], Resumption, _HandlerCtx) ->
                catena_effect_runtime:resume(Resumption, Value + 1)
        end
    ),
    Retained = with_handler(
        catena_effect_runtime:empty_context(),
        'Deep',
        [Case],
        fun(Ctx) ->
            catena_effect_runtime:perform_cps(
                Ctx,
                'Deep',
                step,
                [retain],
                fun(First, RestoredCtx) ->
                    catena_effect_runtime:perform_cps(
                        RestoredCtx,
                        'Deep',
                        step,
                        [First],
                        fun(Second, _Ctx) -> {First, Second} end
                    )
                end
            )
        end
    ),
    ?assertEqual(
        {ok, active},
        catena_resumption_runtime:lease_status(Retained)
    ),
    ?assertEqual(
        {10, 11},
        catena_effect_runtime:resume(Retained, 10)
    ),
    ?assertEqual(
        {ok, released},
        catena_resumption_runtime:lease_status(Retained)
    ).

resume_preserves_mailbox_links_monitors_and_owner() ->
    Owner = self(),
    Helper = spawn_link(fun helper_loop/0),
    Monitor = erlang:monitor(process, Helper),
    Continuation = fun(Value, _RestoredCtx) ->
        MailboxResult = receive
            phase5_mailbox_marker -> received
        after 100 ->
            missing
        end,
        {links, Links} = process_info(self(), links),
        {monitors, Monitors} = process_info(self(), monitors),
        {
            Value,
            self(),
            MailboxResult,
            lists:member(Helper, Links),
            lists:member({process, Helper}, Monitors)
        }
    end,
    Retained = retained(
        catena_effect_runtime:empty_context(),
        'Identity',
        Continuation
    ),
    self() ! phase5_mailbox_marker,
    try
        ?assertEqual(
            {value, Owner, received, true, true},
            catena_effect_runtime:resume(Retained, value)
        )
    after
        erlang:demonitor(Monitor, [flush]),
        unlink(Helper),
        exit(Helper, kill)
    end.

process_provider_computes_only_operation_result() ->
    Owner = self(),
    Result = catena_effect_runtime:with_handlers(
        catena_effect_runtime:empty_context(),
        [{'Remote', [{owner, fun() -> self() end}]}],
        fun(Ctx) ->
            catena_effect_runtime:perform_cps(
                Ctx,
                'Remote',
                owner,
                [],
                fun(ProviderOwner, _RestoredCtx) ->
                    {ProviderOwner, self()}
                end
            )
        end
    ),
    {ProviderOwner, ContinuationOwner} = Result,
    ?assert(ProviderOwner =/= Owner),
    ?assertEqual(Owner, ContinuationOwner).

wrong_owner_does_not_consume_owner_authority() ->
    Owner = self(),
    Retained = retained(
        catena_effect_runtime:empty_context(),
        'Owner',
        fun(Value, _Ctx) -> {self(), Value} end
    ),
    Parent = self(),
    spawn(fun() ->
        Parent ! {foreign_result,
            catena_effect_runtime:resume(Retained, foreign)}
    end),
    receive
        {foreign_result, ForeignResult} ->
            ?assertMatch(
                {error, #{category := wrong_resumption_owner}},
                ForeignResult
            )
    after 1000 ->
        ?assert(false)
    end,
    ?assertEqual(
        {Owner, local},
        catena_effect_runtime:resume(Retained, local)
    ).

double_and_reentrant_resume_are_deterministic() ->
    Continuation = fun(Handle, _Ctx) ->
        catena_effect_runtime:resume(Handle, nested)
    end,
    Retained = retained(
        catena_effect_runtime:empty_context(),
        'OneShot',
        Continuation
    ),
    ?assertMatch(
        {error, #{category := resumption_reentrant}},
        catena_effect_runtime:resume(Retained, Retained)
    ),
    ?assertMatch(
        {error, #{category := resumption_already_consumed}},
        catena_effect_runtime:resume(Retained, second)
    ).

malformed_version_and_expired_frame_fail_closed() ->
    ?assertMatch(
        {error, #{category := invalid_resumption}},
        catena_effect_runtime:resume(not_a_resumption, value)
    ),
    ?assertMatch(
        {error, #{category := invalid_resumption_version}},
        catena_effect_runtime:resume(
            {catena_resumption, 999, make_ref()},
            value
        )
    ),
    Retained = retained(
        catena_effect_runtime:empty_context(),
        'Expired',
        fun(Value, _Ctx) -> Value end
    ),
    ok = catena_resumption_runtime:expire_delimiter(Retained),
    ?assertMatch(
        {error, #{category := stale_resumption_delimiter}},
        catena_effect_runtime:resume(Retained, value)
    ).

provider_death_revokes_retained_context() ->
    Retained = catena_effect_runtime:with_handlers(
        catena_effect_runtime:empty_context(),
        [{'Remote', [{read, fun() -> remote_value end}]}],
        fun(ProviderCtx) ->
            retained(
                ProviderCtx,
                'ProviderLease',
                fun(_Value, RestoredCtx) ->
                    catena_effect_runtime:perform_cps(
                        RestoredCtx,
                        'Remote',
                        read,
                        [],
                        fun(Result, _Ctx) -> Result end
                    )
                end
            )
        end
    ),
    ?assertMatch(
        {error, #{
            category := handler_failure,
            details := #{reason := provider_unavailable}
        }},
        catena_effect_runtime:resume(Retained, ignored)
    ),
    ?assertEqual(
        {ok, released},
        catena_resumption_runtime:lease_status(Retained)
    ).

owner_death_revokes_retained_context() ->
    Parent = self(),
    {Owner, Monitor} = spawn_monitor(fun() ->
        Retained = retained(
            catena_effect_runtime:empty_context(),
            'DeadOwner',
            fun(Value, _Ctx) -> Value end
        ),
        Parent ! {dead_owner_resumption, Retained}
    end),
    Retained = receive
        {dead_owner_resumption, Handle} -> Handle
    after 1000 ->
        error(owner_capture_timeout)
    end,
    receive
        {'DOWN', Monitor, process, Owner, normal} -> ok
    after 1000 ->
        error(owner_exit_timeout)
    end,
    ?assertMatch(
        {error, #{category := expired_resumption_owner}},
        catena_effect_runtime:resume(Retained, value)
    ),
    ?assertEqual(
        {ok, released},
        catena_resumption_runtime:lease_status(Retained)
    ).

failures_timeouts_and_cleanup_release_authority() ->
    FailureCase = catena_effect_runtime:value_case(
        fail,
        0,
        fun([], _Ctx) -> erlang:error(private_handler_failure) end
    ),
    HandlerFailure = with_handler(
        catena_effect_runtime:empty_context(),
        'Failure',
        [FailureCase],
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
        {error, #{category := handler_failure}},
        HandlerFailure
    ),
    TimeoutHandle = retained(
        catena_effect_runtime:empty_context(),
        'Timeout',
        fun(Value, _Ctx) -> timer:sleep(10), Value end
    ),
    ?assertMatch(
        {error, #{
            category := handler_failure,
            details := #{reason := timeout}
        }},
        catena_effect_runtime:resume(TimeoutHandle, value, 1)
    ),
    ?assertEqual(
        {ok, released},
        catena_resumption_runtime:lease_status(TimeoutHandle)
    ),
    CleanupHandle = retained(
        catena_effect_runtime:empty_context(),
        'Cleanup',
        fun(Value, _Ctx) -> Value end
    ),
    ?assertEqual(ok, catena_effect_runtime:discard(CleanupHandle)),
    ?assertEqual(ok, catena_effect_runtime:discard(CleanupHandle)),
    ?assertEqual(
        {ok, released},
        catena_resumption_runtime:lease_status(CleanupHandle)
    ).

retained(Context, Effect, Continuation) ->
    Case = catena_effect_runtime:control_case(
        retain,
        0,
        fun([], Resumption, _HandlerCtx) -> Resumption end
    ),
    with_handler(Context, Effect, [Case], fun(Ctx) ->
        catena_effect_runtime:perform_cps(
            Ctx,
            Effect,
            retain,
            [],
            Continuation
        )
    end).

with_handler(Context, Effect, Cases, Body) ->
    catena_effect_runtime:with_resumable_handler(
        Context,
        #{
            effect => Effect,
            cases => Cases,
            origin => {phase5_handler, Effect}
        },
        Body
    ).

helper_loop() ->
    receive
        stop -> ok
    end.
