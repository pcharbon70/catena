%%%-------------------------------------------------------------------
%%% @doc Phase 1 integration contract for delimited-resumption semantics.
%%%
%%% These tests compare the normative model with the independent executable
%%% oracle.  They do not claim that the production source-to-BEAM path already
%%% implements `with`, `resume`, or first-class delimited continuations.
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_phase1_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ORACLE, catena_resumption_oracle).

%%%===================================================================
%%% Positive semantic agreement
%%%===================================================================

explicit_resume_trace_matches_normative_derivation_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(choice, choose, []),
            fun(Answer) ->
                ?ORACLE:pure(
                    case Answer of
                        true -> selected;
                        false -> rejected
                    end
                )
            end
        ),
        [
            ?ORACLE:control_case(choice, choose, fun([], K) ->
                ?ORACLE:resume(K, true)
            end)
        ]
    ),
    {ok, selected, State} = ?ORACLE:run(Computation),
    ?assertEqual(
        [
            {delimiter_enter, 1},
            {perform, choice, choose, []},
            {capture, 1, 1, oracle_owner, deep, one_shot},
            {handler_select, 1, choice, choose, control},
            {resume_begin, 1, true},
            {delimiter_return, 1, selected, resumed},
            {consume, 1, completed},
            {resume_return, 1, selected}
        ],
        ?ORACLE:trace(State)
    ).

value_handler_auto_resume_trace_matches_translation_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(file_io, read, ["/tmp/input"]),
            fun(Content) -> ?ORACLE:pure({processed, Content}) end
        ),
        [
            ?ORACLE:value_case(file_io, read, fun(["/tmp/input"]) ->
                ?ORACLE:pure(<<"contents">>)
            end)
        ]
    ),
    {ok, {processed, <<"contents">>}, State} = ?ORACLE:run(Computation),
    ?assertEqual(
        [
            {delimiter_enter, 1},
            {perform, file_io, read, ["/tmp/input"]},
            {capture, 1, 1, oracle_owner, deep, one_shot},
            {handler_select, 1, file_io, read, value},
            {auto_resume, 1, <<"contents">>},
            {resume_begin, 1, <<"contents">>},
            {delimiter_return, 1, {processed, <<"contents">>}, resumed},
            {consume, 1, completed},
            {resume_return, 1, {processed, <<"contents">>}}
        ],
        ?ORACLE:trace(State)
    ).

control_handler_abort_discards_remainder_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(choice, choose, []),
            fun(_Answer) -> ?ORACLE:fail(remainder_executed, invalid) end
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

handler_can_transform_result_of_resume_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(number, get, []),
            fun(Value) -> ?ORACLE:pure(Value * 3) end
        ),
        [
            ?ORACLE:control_case(number, get, fun([], K) ->
                ?ORACLE:bind(
                    ?ORACLE:resume(K, 4),
                    fun(ResumedResult) ->
                        ?ORACLE:pure({transformed, ResumedResult + 2})
                    end
                )
            end)
        ]
    ),
    {ok, {transformed, 14}, State} = ?ORACLE:run(Computation),
    ?assert(lists:member({resume_return, 1, 12}, ?ORACLE:trace(State))).

nested_deep_handler_trace_preserves_inner_frame_test() ->
    Inner = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(environment, ask, []),
            fun(Value) ->
                ?ORACLE:perform(validation, validate, [Value])
            end
        ),
        [
            ?ORACLE:value_case(validation, validate, fun([Value]) ->
                ?ORACLE:pure({validated, Value})
            end)
        ]
    ),
    Computation = ?ORACLE:handle(
        Inner,
        [
            ?ORACLE:value_case(environment, ask, fun([]) ->
                ?ORACLE:pure(configured)
            end)
        ]
    ),
    {ok, {validated, configured}, State} = ?ORACLE:run(Computation),
    assert_subsequence(
        [
            {perform, environment, ask, []},
            {propagate, 2, environment, ask},
            {handler_select, 1, environment, ask, value},
            {resume_begin, 1, configured},
            {perform, validation, validate, [configured]},
            {handler_select, 2, validation, validate, value},
            {resume_begin, 2, {validated, configured}}
        ],
        ?ORACLE:trace(State)
    ).

sequential_performs_capture_distinct_one_shot_authorities_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(sequence, next, [first]),
            fun(First) ->
                ?ORACLE:bind(
                    ?ORACLE:perform(sequence, next, [second]),
                    fun(Second) -> ?ORACLE:pure([First, Second]) end
                )
            end
        ),
        [
            ?ORACLE:value_case(sequence, next, fun([Position]) ->
                ?ORACLE:pure({value, Position})
            end)
        ]
    ),
    {ok, [{value, first}, {value, second}], State} =
        ?ORACLE:run(Computation),
    Trace = ?ORACLE:trace(State),
    ?assertEqual(
        [
            {capture, 1, 1, oracle_owner, deep, one_shot},
            {capture, 2, 1, oracle_owner, deep, one_shot}
        ],
        events_named(capture, Trace)
    ),
    ?assertEqual(
        [
            {auto_resume, 1, {value, first}},
            {auto_resume, 2, {value, second}}
        ],
        events_named(auto_resume, Trace)
    ).

retained_resumption_runs_later_on_same_owner_test() ->
    {Resumption, State1} = retained_resumption(),
    ?assertEqual({retain, 1, 1}, lists:last(?ORACLE:trace(State1))),
    {ok, {continued, later}, State2} = ?ORACLE:run(
        ?ORACLE:resume(Resumption, later),
        State1
    ),
    assert_subsequence(
        [
            {retain, 1, 1},
            {resume_begin, 1, later},
            {delimiter_return, 1, {continued, later}, resumed},
            {consume, 1, completed},
            {resume_return, 1, {continued, later}}
        ],
        ?ORACLE:trace(State2)
    ).

%%%===================================================================
%%% Negative semantic agreement
%%%===================================================================

second_one_shot_resume_has_stable_category_test() ->
    {Resumption, State1} = retained_resumption(),
    {ok, {continued, first}, State2} = ?ORACLE:run(
        ?ORACLE:resume(Resumption, first),
        State1
    ),
    {error, resumption_already_consumed, #{id := 1}, State3} =
        ?ORACLE:run(?ORACLE:resume(Resumption, second), State2),
    ?assertEqual(
        {failure, resumption_already_consumed, #{id => 1}},
        lists:last(?ORACLE:trace(State3))
    ).

reentrant_one_shot_resume_has_stable_category_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(reentrant, operation, []),
            fun(SameResumption) ->
                ?ORACLE:resume(SameResumption, second)
            end
        ),
        [
            ?ORACLE:control_case(reentrant, operation, fun([], K) ->
                ?ORACLE:resume(K, K)
            end)
        ]
    ),
    {error, resumption_reentrant, #{id := 1}, State} =
        ?ORACLE:run(Computation),
    assert_subsequence(
        [
            {resume_begin, 1,
                {catena_oracle_resumption, 1, 1}},
            {failure, resumption_reentrant, #{id => 1}},
            {consume, 1, failed}
        ],
        ?ORACLE:trace(State)
    ).

cross_owner_resume_has_stable_category_test() ->
    {Resumption, State1} = retained_resumption(owner_a),
    {error, wrong_resumption_owner, Details, State2} = ?ORACLE:run(
        ?ORACLE:resume_as(Resumption, value, owner_b),
        State1
    ),
    ?assertEqual(
        #{id => 1, expected => owner_a, actual => owner_b},
        Details
    ),
    ?assertEqual(
        {failure, wrong_resumption_owner, Details},
        lists:last(?ORACLE:trace(State2))
    ).

stale_delimiter_resume_has_stable_category_test() ->
    {Resumption, State1} = retained_resumption(),
    State2 = ?ORACLE:expire_delimiter(Resumption, State1),
    {error, stale_resumption_delimiter, Details, State3} =
        ?ORACLE:run(?ORACLE:resume(Resumption, value), State2),
    ?assertEqual(
        #{id => 1, delimiter => 1, status => expired},
        Details
    ),
    ?assertEqual(
        {failure, stale_resumption_delimiter, Details},
        lists:last(?ORACLE:trace(State3))
    ).

expired_owner_resume_has_stable_category_test() ->
    {Resumption, State1} = retained_resumption(owner_a),
    State2 = ?ORACLE:expire_owner(owner_a, State1),
    {error, expired_resumption_owner, #{id := 1}, State3} =
        ?ORACLE:run(
            ?ORACLE:resume_as(Resumption, value, owner_a),
            State2
        ),
    ?assertEqual(
        {failure, expired_resumption_owner, #{id => 1}},
        lists:last(?ORACLE:trace(State3))
    ).

multi_shot_mode_fails_instead_of_falling_back_test() ->
    Computation = ?ORACLE:handle(
        ?ORACLE:pure(unreachable),
        [],
        deep,
        multi_shot
    ),
    Details = #{depth => deep, kind => multi_shot},
    {error, unsupported_semantic_mode, Details, State} =
        ?ORACLE:run(Computation),
    ?assertEqual(
        [{failure, unsupported_semantic_mode, Details}],
        ?ORACLE:trace(State)
    ).

%%%===================================================================
%%% Honest current implementation boundary
%%%===================================================================

legacy_capture_remains_an_explicit_marker_test() ->
    MarkerResumption = catena_resumption:capture_continuation(),
    ?assertEqual(
        {resumed, supplied_value},
        catena_resumption:resume(MarkerResumption, supplied_value)
    ),
    ?assertNotEqual(
        {continued, supplied_value},
        catena_resumption:resume(MarkerResumption, supplied_value)
    ).

with_and_resume_are_not_yet_promoted_lexer_words_test() ->
    ?assertEqual(
        {ok,
            [
                {lower_ident, 1, "with"},
                {lower_ident, 1, "resume"}
            ],
            1},
        catena_lexer:string("with resume")
    ).

%%%===================================================================
%%% Helpers
%%%===================================================================

retained_resumption() ->
    retained_resumption(oracle_owner).

retained_resumption(Owner) ->
    Computation = ?ORACLE:handle(
        ?ORACLE:bind(
            ?ORACLE:perform(choice, choose, []),
            fun(Value) -> ?ORACLE:pure({continued, Value}) end
        ),
        [
            ?ORACLE:control_case(choice, choose, fun([], K) ->
                ?ORACLE:pure(K)
            end)
        ]
    ),
    {ok, Resumption, State} = ?ORACLE:run(
        Computation,
        ?ORACLE:new(Owner)
    ),
    {Resumption, State}.

events_named(Name, Events) ->
    [
        Event
     || Event <- Events,
        element(1, Event) =:= Name
    ].

assert_subsequence(Expected, Actual) ->
    ?assertEqual([], missing_subsequence(Expected, Actual)).

missing_subsequence([], _Actual) ->
    [];
missing_subsequence(Expected, []) ->
    Expected;
missing_subsequence([Event | Expected], [Event | Actual]) ->
    missing_subsequence(Expected, Actual);
missing_subsequence(Expected, [_Other | Actual]) ->
    missing_subsequence(Expected, Actual).
