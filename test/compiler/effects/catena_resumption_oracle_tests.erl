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
