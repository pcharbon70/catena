%%%-------------------------------------------------------------------
%%% @doc Phase 8.1 compiler-backed REPL and safe control-tooling evidence.
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_phase8_tooling_tests).

-include_lib("eunit/include/eunit.hrl").

compiler_backed_session_recompiles_multiline_handlers_test() ->
    catena_resumption_runtime:reset_for_test(),
    {ok, Session0} = catena_repl:new_session(#{module => 'PhaseEightRepl'}),
    try
        {ok, Definition, Session1} = catena_repl:session_define(
            handler_declaration(),
            Session0
        ),
        ?assertEqual(1, maps:get(generation, Definition)),
        ?assert(lists:member(
            {transform, chooseOne},
            maps:get(definitions, Definition)
        )),
        {ok, Evaluation, Session2} = catena_repl:session_eval(
            "chooseOne(0)",
            Session1
        ),
        ?assertEqual(42, maps:get(value, maps:get(value, Evaluation))),
        ?assertEqual({tcon, int}, maps:get(type, Evaluation)),
        ?assertEqual(2, maps:get(generation, Evaluation)),
        AllowedFrames = [transform, perform, handler, binder, resume, delimiter],
        ?assert(lists:all(
            fun(Frame) ->
                lists:member(maps:get(construct, Frame), AllowedFrames)
            end,
            maps:get(frames, Evaluation)
        )),
        {ok, SessionDescription} = catena_repl_session:describe(Session2),
        ?assertEqual(2, maps:get(generation, SessionDescription)),
        ?assertEqual([it], maps:get(runtime_bindings, SessionDescription))
    after
        ok = catena_repl:close_session(Session0),
        catena_resumption_runtime:reset_for_test()
    end.

typed_resumption_binding_is_preserved_and_owner_affine_test() ->
    catena_resumption_runtime:reset_for_test(),
    {ok, Session0} = catena_repl:new_session(#{module => 'PhaseEightBinding'}),
    {ok, Handle} = capture(
        fun(Value, _Context) -> {resumed, Value} end,
        one_shot,
        deep,
        #{line => 12, column => 8}
    ),
    PublicType = {tresumption, one_shot, int, int, []},
    {ok, Session1} = catena_repl:session_bind(
        pending,
        Handle,
        PublicType,
        Session0
    ),
    {ok, Inspection} = catena_repl:session_inspect(pending, Session1),
    ?assertEqual(PublicType, maps:get(type, Inspection)),
    #{kind := resumption, description := Description} =
        maps:get(value, Inspection),
    ?assertEqual(one_shot, maps:get(kind, Description)),
    ?assertEqual(deep, maps:get(depth, Description)),
    ?assertEqual(current_process, maps:get(owner_relationship, Description)),
    ?assertEqual(fresh, maps:get(state, Description)),
    ?assertNot(contains_private_runtime_term(Description)),
    {ok, ResumeResult, _Session2} = catena_repl:session_resume(
        pending,
        41,
        Session1
    ),
    ?assertEqual(
        {resumed, 41},
        maps:get(value, maps:get(value, ResumeResult))
    ),
    ok = catena_repl:close_session(Session0),
    catena_resumption_runtime:reset_for_test().

structured_trace_is_bounded_redacted_and_complete_test() ->
    catena_resumption_runtime:reset_for_test(),
    ok = catena_resumption_runtime:configure_trace(#{max_events => 64}),
    {ok, Completed} = capture(
        fun(Value, _Context) -> Value end,
        one_shot,
        deep,
        #{line => 4, column => 2}
    ),
    ?assertEqual({ok, done}, catena_resumption_runtime:resume(Completed, done)),
    {ok, Aborted} = capture(
        fun(Value, _Context) -> Value end,
        one_shot,
        shallow,
        #{line => 9, column => 3}
    ),
    ok = catena_resumption_runtime:discard(Aborted),
    {ok, TimedOut} = capture(
        fun(_Value, _Context) -> timer:sleep(5), late end,
        one_shot,
        deep,
        #{line => 14, column => 1}
    ),
    ?assertMatch(
        {error, #{details := #{reason := timeout}}},
        catena_resumption_runtime:resume(TimedOut, ignored, 0)
    ),
    {ok, Multi} = capture(
        fun(Value, _Context) -> Value end,
        multi_shot,
        deep,
        #{line => 20, column => 1}
    ),
    ?assertEqual({ok, branch_value},
        catena_resumption_runtime:resume(Multi, branch_value)),
    {ok, Events} = catena_resumption_runtime:trace(),
    Kinds = [maps:get(event, Event) || Event <- Events],
    lists:foreach(
        fun(Kind) -> ?assert(lists:member(Kind, Kinds)) end,
        [
            capture,
            handler_selection,
            resume,
            abort,
            branch,
            consumption,
            timeout,
            cleanup
        ]
    ),
    ?assertEqual(
        lists:seq(1, length(Events)),
        [maps:get(sequence, Event) || Event <- Events]
    ),
    ?assertNot(contains_private_runtime_term(Events)),
    ok = catena_resumption_runtime:configure_trace(#{
        max_events => 2,
        events => [capture]
    }),
    ok = catena_resumption_runtime:clear_trace(),
    {ok, _} = capture(fun(V, _C) -> V end, one_shot, deep, #{line => 30}),
    {ok, _} = capture(fun(V, _C) -> V end, one_shot, deep, #{line => 31}),
    {ok, _} = capture(fun(V, _C) -> V end, one_shot, deep, #{line => 32}),
    {ok, Bounded} = catena_resumption_runtime:trace(),
    ?assertEqual(2, length(Bounded)),
    ?assert(lists:all(
        fun(Event) -> maps:get(event, Event) =:= capture end,
        Bounded
    )),
    catena_resumption_runtime:reset_for_test().

artifact_frames_and_runtime_trace_form_source_view_test() ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(
        "module PhaseEightFrames\n" ++ handler_declaration()
    ),
    Frames = catena_control_diagnostics:source_frames(Artifact),
    Constructs = lists:usort([
        maps:get(construct, Frame)
        || Frame <- Frames
    ]),
    ?assert(lists:member(transform, Constructs)),
    ?assert(lists:member(perform, Constructs)),
    ?assert(lists:member(handler, Constructs)),
    ?assert(lists:member(resume, Constructs)),
    ?assert(lists:member(delimiter, Constructs)),
    Event = #{event => capture, sequence => 1, resumption_id => 7},
    [View] = catena_control_diagnostics:trace_view([Event], Artifact),
    ?assert(lists:all(
        fun(Frame) -> maps:get(construct, Frame) =:= perform end,
        maps:get(frames, View)
    )).

legacy_repl_commands_never_print_resumption_authority_test() ->
    catena_resumption_runtime:reset_for_test(),
    Env = catena_type_env:empty(),
    State0 = {repl_state, Env, #{}, #{}, [], "catena> ", ""},
    {ok, Handle} = capture(
        fun(Value, _Context) -> Value end,
        one_shot,
        deep,
        #{line => 40}
    ),
    State1 = catena_repl:bind_runtime_value(
        pending,
        Handle,
        {tresumption, one_shot, int, int, []},
        State0
    ),
    {ok, {resumption, Description}, _} =
        catena_repl:eval(":resumption pending", State1),
    ?assertNot(contains_private_runtime_term(Description)),
    {ok, {trace_configured, on}, State2} = catena_repl:eval(":trace on", State1),
    {ok, {control_trace, _}, _} = catena_repl:eval(":trace show", State2),
    catena_resumption_runtime:reset_for_test().

handler_declaration() ->
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform chooseOne ignored = handle "
        "(let selected = perform Choice.choose() in selected + 1) then {\n"
    "  Choice { choose() with k -> resume(k, 41) }\n"
    "}\n".

capture(Continuation, Kind, Depth, Location) ->
    Context = catena_effect_runtime:empty_context(),
    catena_resumption_runtime:capture(Continuation, #{
        context => Context,
        parent_context => Context,
        delimiter => make_ref(),
        depth => Depth,
        kind => Kind,
        origin => #{
            perform => #{location => Location, construct => perform},
            handler_case => #{location => Location, construct => handler},
            delimiter => #{location => Location, construct => delimiter}
        },
        metadata => #{
            effect => 'Choice',
            operation => choose,
            private => fun() -> secret end
        },
        type_identity => {tresumption, Kind, int, int, []}
    }).

contains_private_runtime_term(Term) when is_function(Term) -> true;
contains_private_runtime_term(Term) when is_pid(Term) -> true;
contains_private_runtime_term(Term) when is_reference(Term) -> true;
contains_private_runtime_term({catena_resumption, _, _}) -> true;
contains_private_runtime_term(Term) when is_tuple(Term) ->
    contains_private_runtime_term(tuple_to_list(Term));
contains_private_runtime_term(Term) when is_list(Term) ->
    lists:any(fun contains_private_runtime_term/1, Term);
contains_private_runtime_term(Term) when is_map(Term) ->
    contains_private_runtime_term(maps:to_list(Term));
contains_private_runtime_term(_Term) -> false.
