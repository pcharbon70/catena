%%%-------------------------------------------------------------------
%%% @doc Unit fixtures for the executable delimited-resumption oracle.
%%%-------------------------------------------------------------------
-module(catena_resumption_oracle_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ORACLE, catena_resumption_oracle).

simple_resume_executes_remainder_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(choice, choose, []),
            fun(Answer) ->
                ?ORACLE:pure({remainder, Answer})
            end
        ),
        [
            ?ORACLE:control_case(choice, choose, fun([], K) ->
                ?ORACLE:resume(K, true)
            end)
        ]
    ),
    {ok, {remainder, true}, State} = ?ORACLE:run(Computation),
    ?assertEqual(
        [
            {delimiter_enter, 1},
            {perform, choice, choose, []},
            {capture, 1, 1, oracle_owner, deep, one_shot},
            {handler_select, 1, choice, choose, control},
            {resume_begin, 1, true},
            {delimiter_return, 1, {remainder, true}, resumed},
            {consume, 1, completed},
            {resume_return, 1, {remainder, true}}
        ],
        ?ORACLE:trace(State)
    ).

resume_result_can_be_transformed_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(choice, choose, []),
            fun(Number) -> ?ORACLE:pure(Number * 2) end
        ),
        [
            ?ORACLE:control_case(choice, choose, fun([], K) ->
                ?ORACLE:bind(
                    ?ORACLE:resume(K, 4),
                    fun(Result) -> ?ORACLE:pure({handled, Result + 1}) end
                )
            end)
        ]
    ),
    {ok, {handled, 9}, State} = ?ORACLE:run(Computation),
    ?assert(lists:member({resume_return, 1, 8}, ?ORACLE:trace(State))).

return_without_resume_aborts_remainder_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(choice, choose, []),
            fun(_Answer) -> ?ORACLE:fail(remainder_ran, unexpected) end
        ),
        [
            ?ORACLE:control_case(choice, choose, fun([], _K) ->
                ?ORACLE:pure(fallback)
            end)
        ]
    ),
    {ok, fallback, State} = ?ORACLE:run(Computation),
    ?assertEqual(
        [
            {delimiter_enter, 1},
            {perform, choice, choose, []},
            {capture, 1, 1, oracle_owner, deep, one_shot},
            {handler_select, 1, choice, choose, control},
            {abort, 1, 1, fallback},
            {consume, 1, aborted}
        ],
        ?ORACLE:trace(State)
    ).

sequential_performs_receive_distinct_resumptions_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(counter, next, []),
            fun(First) ->
                ?ORACLE:bind(
                    ?ORACLE:perform(counter, next, []),
                    fun(Second) -> ?ORACLE:pure({First, Second}) end
                )
            end
        ),
        [
            ?ORACLE:value_case(counter, next, fun([]) ->
                ?ORACLE:pure(7)
            end)
        ]
    ),
    {ok, {7, 7}, State} = ?ORACLE:run(Computation),
    Captures = [
        Event
     || {capture, _Id, _Delimiter, _Owner, _Depth, _Kind} = Event <-
            ?ORACLE:trace(State)
    ],
    ?assertEqual(
        [
            {capture, 1, 1, oracle_owner, deep, one_shot},
            {capture, 2, 1, oracle_owner, deep, one_shot}
        ],
        Captures
    ).

nested_handler_propagates_and_restores_inner_context_test() ->
    Inner = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(outer_effect, ask, []),
            fun(Value) ->
                ?ORACLE:perform(inner_effect, check, [Value])
            end
        ),
        [
            ?ORACLE:value_case(inner_effect, check, fun([Value]) ->
                ?ORACLE:pure({inner, Value})
            end)
        ]
    ),
    Computation = ?ORACLE:handle(
        Inner,
        [
            ?ORACLE:value_case(outer_effect, ask, fun([]) ->
                ?ORACLE:pure(answer)
            end)
        ]
    ),
    {ok, {inner, answer}, State} = ?ORACLE:run(Computation),
    Trace = ?ORACLE:trace(State),
    ?assert(lists:member({propagate, 2, outer_effect, ask}, Trace)),
    ?assert(lists:member(
        {handler_select, 2, inner_effect, check, value},
        Trace
    )).

value_handler_auto_resumes_exactly_once_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(file_io, read, [path]),
            fun(Bytes) -> ?ORACLE:pure({decoded, Bytes}) end
        ),
        [
            ?ORACLE:value_case(file_io, read, fun([path]) ->
                ?ORACLE:pure(<<"data">>)
            end)
        ]
    ),
    {ok, {decoded, <<"data">>}, State} = ?ORACLE:run(Computation),
    AutoResumes = [
        Event
     || {auto_resume, _Id, _Value} = Event <- ?ORACLE:trace(State)
    ],
    ?assertEqual([{auto_resume, 1, <<"data">>}], AutoResumes).

retained_resumption_can_run_in_later_same_owner_evaluation_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(choice, choose, []),
            fun(Value) -> ?ORACLE:pure({later, Value}) end
        ),
        [
            ?ORACLE:control_case(choice, choose, fun([], K) ->
                ?ORACLE:pure(K)
            end)
        ]
    ),
    {ok, Resumption, State1} = ?ORACLE:run(Computation),
    {ok, {later, selected}, State2} = ?ORACLE:run(
        ?ORACLE:resume(Resumption, selected),
        State1
    ),
    ?assert(lists:member({retain, 1, 1}, ?ORACLE:trace(State2))).

second_one_shot_invocation_fails_deterministically_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(choice, choose, []),
            fun(Value) -> ?ORACLE:pure(Value) end
        ),
        [
            ?ORACLE:control_case(choice, choose, fun([], K) ->
                ?ORACLE:pure(K)
            end)
        ]
    ),
    {ok, Resumption, State1} = ?ORACLE:run(Computation),
    {ok, first, State2} = ?ORACLE:run(
        ?ORACLE:resume(Resumption, first),
        State1
    ),
    {error, resumption_already_consumed, #{id := 1}, State3} =
        ?ORACLE:run(?ORACLE:resume(Resumption, second), State2),
    ?assertEqual(
        {failure, resumption_already_consumed, #{id => 1}},
        lists:last(?ORACLE:trace(State3))
    ).

pure_failure_and_unhandled_request_are_distinct_test() ->
    {ok, value, _PureState} = ?ORACLE:run(?ORACLE:pure(value)),
    {error, explicit_failure, details, FailedState} =
        ?ORACLE:run(?ORACLE:fail(explicit_failure, details)),
    ?assertEqual(
        [{failure, explicit_failure, details}],
        ?ORACLE:trace(FailedState)
    ),
    UnhandledDetails = #{
        effect => missing,
        operation => operation,
        arguments => [argument]
    },
    {error, unhandled_effect, UnhandledDetails, UnhandledState} =
        ?ORACLE:run(?ORACLE:perform(missing, operation, [argument])),
    ?assertEqual(
        [
            {perform, missing, operation, [argument]},
            {failure, unhandled_effect, UnhandledDetails}
        ],
        ?ORACLE:trace(UnhandledState)
    ).

invalid_computation_and_callback_failures_are_stable_test() ->
    {error, invalid_oracle_computation, #{term := invalid}, _} =
        ?ORACLE:run(invalid),
    ContinuationFailure = ?ORACLE:bind(
        ?ORACLE:pure(value),
        fun(_Value) -> error(continuation_failed) end
    ),
    {error, oracle_callback_failure, ContinuationDetails, _} =
        ?ORACLE:run(ContinuationFailure),
    ?assertEqual(continuation, maps:get(phase, ContinuationDetails)),
    ?assertEqual(error, maps:get(class, ContinuationDetails)),
    ?assertEqual(
        continuation_failed,
        maps:get(reason, ContinuationDetails)
    ),
    HandlerFailure = ?ORACLE:handle(
        ?ORACLE:perform(effect, operation, []),
        [
            ?ORACLE:control_case(effect, operation, fun([], _K) ->
                error(handler_failed)
            end)
        ]
    ),
    {error, oracle_callback_failure, HandlerDetails, HandlerState} =
        ?ORACLE:run(HandlerFailure),
    ?assertEqual(handler, maps:get(phase, HandlerDetails)),
    ?assert(lists:member(
        {consume, 1, handler_failed},
        ?ORACLE:trace(HandlerState)
    )).

value_handler_failure_does_not_auto_resume_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:perform(effect, operation, []),
        [
            ?ORACLE:value_case(effect, operation, fun([]) ->
                error(value_handler_failed)
            end)
        ]
    ),
    {error, oracle_callback_failure, #{phase := handler}, State} =
        ?ORACLE:run(Computation),
    ?assertEqual([], [
        Event
     || {auto_resume, _Id, _Value} = Event <- ?ORACLE:trace(State)
    ]).

value_handler_body_can_suspend_outward_before_auto_resume_test() ->
    Inner = ?ORACLE:handle(
        ?ORACLE:perform(inner, operation, []),
        [
            ?ORACLE:value_case(inner, operation, fun([]) ->
                ?ORACLE:perform(outer, provide, [])
            end)
        ]
    ),
    Computation = ?ORACLE:handle(
        Inner,
        [
            ?ORACLE:value_case(outer, provide, fun([]) ->
                ?ORACLE:pure(provided)
            end)
        ]
    ),
    {ok, provided, State} = ?ORACLE:run(Computation),
    ?assert(lists:member({auto_resume, 1, provided}, ?ORACLE:trace(State))).

retained_resume_can_suspend_into_an_unhandled_request_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(capture, operation, []),
            fun(_Value) -> ?ORACLE:perform(missing, operation, []) end
        ),
        [
            ?ORACLE:control_case(capture, operation, fun([], K) ->
                ?ORACLE:pure(K)
            end)
        ]
    ),
    {ok, Resumption, State1} = ?ORACLE:run(Computation),
    {error, unhandled_effect, _Details, State2} =
        ?ORACLE:run(?ORACLE:resume(Resumption, supplied), State1),
    ?assert(lists:member(
        {propagate, 1, missing, operation},
        ?ORACLE:trace(State2)
    )).

invalid_resumption_shapes_have_stable_categories_test() ->
    {error, invalid_resumption_version, #{version := 99}, _} =
        ?ORACLE:run(
            ?ORACLE:resume(
                {catena_oracle_resumption, 99, 1},
                value
            )
        ),
    {error, invalid_resumption, #{id := 99}, _} =
        ?ORACLE:run(
            ?ORACLE:resume(
                {catena_oracle_resumption, 1, 99},
                value
            )
        ),
    {error, invalid_resumption, #{term := not_a_resumption}, _} =
        ?ORACLE:run(?ORACLE:resume(not_a_resumption, value)).

missing_delimiter_and_registered_mode_fail_closed_test() ->
    {Resumption, State1} = retained_resumption_state(),
    Delimiters = maps:get(delimiters, State1),
    MissingDelimiterState = State1#{
        delimiters := maps:remove(1, Delimiters)
    },
    {error, stale_resumption_delimiter, #{id := 1, delimiter := 1}, _} =
        ?ORACLE:run(
            ?ORACLE:resume(Resumption, value),
            MissingDelimiterState
        ),
    Resumptions = maps:get(resumptions, State1),
    Entry = maps:get(1, Resumptions),
    UnsupportedState = State1#{
        resumptions := Resumptions#{
            1 => Entry#{kind => multi_shot}
        }
    },
    {error, unsupported_semantic_mode, #{id := 1, kind := multi_shot}, _} =
        ?ORACLE:run(?ORACLE:resume(Resumption, value), UnsupportedState).

resumption_retention_detects_nested_data_test() ->
    lists:foreach(
        fun(Wrap) ->
            Computation = ?ORACLE:handle(
                ?ORACLE:perform(capture, operation, []),
                [
                    ?ORACLE:control_case(
                        capture,
                        operation,
                        fun([], K) -> ?ORACLE:pure(Wrap(K)) end
                    )
                ]
            ),
            {ok, _Value, State} = ?ORACLE:run(Computation),
            ?assert(lists:member({retain, 1, 1}, ?ORACLE:trace(State)))
        end,
        [
            fun(K) -> [K] end,
            fun(K) -> {saved, K} end,
            fun(K) -> #{saved => K} end
        ]
    ).

expire_helpers_ignore_unknown_handles_test() ->
    State = ?ORACLE:new(),
    ?assertEqual(State, ?ORACLE:expire_delimiter(unknown, State)),
    ?assertEqual(
        State,
        ?ORACLE:expire_delimiter(
            {catena_oracle_resumption, 1, 99},
            State
        )
    ).

retained_resumption_state() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(capture, operation, []),
            fun(Value) -> ?ORACLE:pure(Value) end
        ),
        [
            ?ORACLE:control_case(capture, operation, fun([], K) ->
                ?ORACLE:pure(K)
            end)
        ]
    ),
    {ok, Resumption, State} = ?ORACLE:run(Computation),
    {Resumption, State}.
