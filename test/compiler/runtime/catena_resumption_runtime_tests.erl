-module(catena_resumption_runtime_tests).
-include_lib("eunit/include/eunit.hrl").

catena_resumption_runtime_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun opaque_handle_hides_private_state/0,
            fun construction_contract_is_fail_closed/0,
            fun first_invocation_runs_on_owner/0,
            fun exceptional_exit_consumes/0,
            fun double_resume_is_rejected/0,
            fun reentrant_resume_is_rejected/0,
            fun cross_process_resume_is_rejected_without_consuming/0,
            fun malformed_stale_and_unregistered_handles_are_rejected/0,
            fun unsupported_modes_are_rejected/0
        ]}.

setup() ->
    catena_resumption_runtime:reset_for_test().

cleanup(_State) ->
    catena_resumption_runtime:reset_for_test().

opaque_handle_hides_private_state() ->
    SecretContext = #{secret => context},
    SecretContinuation = fun(Value, _Context) -> Value end,
    {ok, Handle} = capture(SecretContinuation, SecretContext),
    ?assertMatch(
        {catena_resumption, 1, Opaque} when is_reference(Opaque),
        Handle
    ),
    ?assert(catena_resumption_runtime:is_resumption(Handle)),
    ?assertNot(
        lists:member(SecretContext, tuple_to_list(Handle))
    ),
    ?assertNot(
        lists:member(SecretContinuation, tuple_to_list(Handle))
    ),
    ?assertEqual({ok, fresh}, catena_resumption_runtime:status(Handle)).

construction_contract_is_fail_closed() ->
    ValidSpec = capture_spec(#{}),
    ?assertMatch(
        {error, #{category := invalid_resumption}},
        catena_resumption_runtime:capture(not_a_continuation, ValidSpec)
    ),
    ?assertMatch(
        {error, #{category := invalid_resumption}},
        catena_resumption_runtime:capture(
            fun(_Value, _Context) -> ok end,
            maps:remove(delimiter, ValidSpec)
        )
    ),
    ?assertMatch(
        {error, #{category := invalid_resumption}},
        catena_resumption_runtime:capture(
            fun(_Value, _Context) -> ok end,
            ValidSpec#{context := private_context_term}
        )
    ).

first_invocation_runs_on_owner() ->
    Owner = self(),
    Context = #{marker => restored},
    Continuation = fun(Value, RestoredContext) ->
        {self(), Value, RestoredContext}
    end,
    {ok, Handle} = capture(Continuation, Context),
    ?assertEqual(
        {ok, {Owner, supplied, Context}},
        catena_resumption_runtime:resume(Handle, supplied)
    ),
    ?assertEqual(
        {ok, consumed},
        catena_resumption_runtime:status(Handle)
    ).

exceptional_exit_consumes() ->
    Continuation = fun(_Value, _Context) ->
        erlang:error(private_exception_payload)
    end,
    {ok, Handle} = capture(Continuation, #{}),
    ?assertMatch(
        {error, #{
            category := handler_failure,
            details := #{class := error, reason := private_exception_payload}
        }},
        catena_resumption_runtime:resume(Handle, ignored)
    ),
    ?assertEqual(
        {ok, consumed},
        catena_resumption_runtime:status(Handle)
    ).

double_resume_is_rejected() ->
    {ok, Handle} = capture(fun(Value, _Context) -> Value end, #{}),
    ?assertEqual(
        {ok, first},
        catena_resumption_runtime:resume(Handle, first)
    ),
    ?assertMatch(
        {error, #{category := resumption_already_consumed}},
        catena_resumption_runtime:resume(Handle, second)
    ).

reentrant_resume_is_rejected() ->
    Owner = self(),
    Continuation = fun(Handle, _Context) ->
        {self(), catena_resumption_runtime:resume(Handle, nested)}
    end,
    {ok, Handle} = capture(Continuation, #{}),
    ?assertMatch(
        {ok, {Owner, {error, #{category := resumption_reentrant}}}},
        catena_resumption_runtime:resume(Handle, Handle)
    ),
    ?assertEqual(
        {ok, consumed},
        catena_resumption_runtime:status(Handle)
    ).

cross_process_resume_is_rejected_without_consuming() ->
    Parent = self(),
    Continuation = fun(Value, _Context) -> {self(), Value} end,
    {ok, Handle} = capture(Continuation, #{}),
    spawn(fun() ->
        Parent ! {foreign_resume,
            catena_resumption_runtime:resume(Handle, foreign)}
    end),
    receive
        {foreign_resume, ForeignResult} ->
            ?assertMatch(
                {error, #{category := wrong_resumption_owner}},
                ForeignResult
            )
    after 1000 ->
        ?assert(false)
    end,
    ?assertEqual({ok, fresh}, catena_resumption_runtime:status(Handle)),
    ?assertEqual(
        {ok, {Parent, owner}},
        catena_resumption_runtime:resume(Handle, owner)
    ).

malformed_stale_and_unregistered_handles_are_rejected() ->
    ?assertMatch(
        {error, #{category := invalid_resumption}},
        catena_resumption_runtime:resume(not_a_handle, value)
    ),
    ?assertMatch(
        {error, #{category := invalid_resumption_version}},
        catena_resumption_runtime:resume(
            {catena_resumption, 999, make_ref()},
            value
        )
    ),
    ?assertMatch(
        {error, #{category := invalid_resumption}},
        catena_resumption_runtime:resume(
            {catena_resumption, catena_resumption_runtime:version(), make_ref()},
            value
        )
    ).

unsupported_modes_are_rejected() ->
    Continuation = fun(Value, _Context) -> Value end,
    ?assertMatch(
        {error, #{category := unsupported_semantic_mode}},
        catena_resumption_runtime:capture(
            Continuation,
            (capture_spec(#{}))#{kind := multi_shot}
        )
    ),
    ?assertMatch(
        {error, #{category := unsupported_semantic_mode}},
        catena_resumption_runtime:capture(
            Continuation,
            (capture_spec(#{}))#{depth := shallow}
        )
    ).

capture(Continuation, Context) ->
    catena_resumption_runtime:capture(
        Continuation,
        capture_spec(Context)
    ).

capture_spec(Context) ->
    #{
        context => Context,
        delimiter => make_ref(),
        depth => deep,
        kind => one_shot,
        origin => {test, ?MODULE},
        metadata => #{fixture => compiler_generated},
        type_identity => dynamic
    }.
