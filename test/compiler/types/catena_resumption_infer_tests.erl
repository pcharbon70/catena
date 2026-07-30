%%%-------------------------------------------------------------------
%%% Phase 3.2 tests for handler-binder and resume inference.
%%%-------------------------------------------------------------------
-module(catena_resumption_infer_tests).

-include_lib("eunit/include/eunit.hrl").

explicit_handler_infers_one_shot_resumption_test() ->
    Source =
        "module ExplicitResumeTyping\n"
        "effect State\n"
        "operation put : Int -> Int\n"
        "end\n"
        "transform run ignored = handle perform State.put(1) then {\n"
        "  State { put(value) with k -> resume(k, value + 1) }\n"
        "}\n",
    Evidence = resumption_evidence(Source),
    [Binder, Resume] = Evidence,
    Expected = catena_types:tresumption(
        catena_types:one_shot(),
        catena_types:tcon(int),
        catena_types:tcon(int),
        catena_types:teffectrow([])
    ),
    ?assertMatch(
        #{
            kind := resumption_binder,
            binder := k,
            mode := one_shot,
            effect := 'State',
            operation := put
        },
        Binder
    ),
    ?assert(catena_types:type_equal(Expected, maps:get(type, Binder))),
    ?assertMatch(#{kind := resume, binder := k}, Resume),
    ?assert(catena_types:type_equal(Expected, maps:get(type, Resume))).

operation_and_delimiter_results_are_derived_independently_test() ->
    Source =
        "module IndependentResumeResults\n"
        "effect Prompt\n"
        "operation ask : Int -> String\n"
        "end\n"
        "transform run ignored = handle "
        "(let answer = perform Prompt.ask(1) in true) then {\n"
        "  Prompt { ask(question) with k -> resume(k, \"accepted\") }\n"
        "}\n",
    [Binder | _] = resumption_evidence(Source),
    ?assertEqual(
        catena_types:tresumption(
            catena_types:one_shot(),
            catena_types:tcon(string),
            catena_types:tcon(bool),
            catena_types:teffectrow([])
        ),
        maps:get(type, Binder)
    ).

handled_effect_is_removed_but_residual_effect_is_retained_test() ->
    Source =
        "module ResidualResumeEffects\n"
        "effect State\n"
        "operation put : Int -> Int\n"
        "end\n"
        "effect Log\n"
        "operation write : Int -> Int\n"
        "end\n"
        "transform run ignored = handle "
        "(let logged = perform Log.write(0) in "
        "perform State.put(logged)) then {\n"
        "  State { put(value) with k -> resume(k, value) }\n"
        "}\n",
    {Type, Evidence} = typed_transform_type_and_evidence(Source),
    ?assertMatch(
        {tfun, _, {tcon, int}, {effect_set, ['Log']}},
        Type
    ),
    ?assertEqual(
        [
            {teffectrow, ['Log'], closed},
            {teffectrow, ['Log'], closed}
        ],
        [maps:get(residual_effects, Entry) || Entry <- Evidence]
    ).

open_residual_row_flows_through_resume_test() ->
    ResumptionType = catena_types:tresumption(
        catena_types:one_shot(),
        catena_types:tcon(int),
        catena_types:tcon(bool),
        catena_types:teffectrow(['Log'], 42)
    ),
    Env = catena_type_env:singleton(
        k,
        catena_type_scheme:mono(ResumptionType)
    ),
    Expr = {
        resume_expr,
        {var, k, location(1, 8)},
        {lit, {int, 1}},
        location(1, 1)
    },
    {ok, {tcon, bool}, Details} =
        catena_infer:infer_expr_detailed(Expr, Env),
    ?assertEqual(
        {effect_set, ['Log']},
        maps:get(effects, Details)
    ),
    [Resume] = maps:get(resumptions, Details),
    ?assertEqual(ResumptionType, maps:get(type, Resume)).

automatic_handler_receives_synthetic_typed_authority_test() ->
    Source =
        "module AutomaticResumeTyping\n"
        "effect Reader\n"
        "operation read : Int\n"
        "end\n"
        "transform run ignored = handle perform Reader.read() then {\n"
        "  Reader { read() -> 42 }\n"
        "}\n",
    [Binder, Resume] = resumption_evidence(Source),
    ?assertMatch(
        #{
            kind := resumption_binder,
            binder_origin := {
                synthetic,
                value_handler_auto_resume,
                _
            }
        },
        Binder
    ),
    ?assertMatch(
        #{
            kind := resume,
            resume_location := {
                synthetic,
                value_handler_auto_resume,
                _
            }
        },
        Resume
    ).

wrong_resume_value_reports_all_relevant_origins_test() ->
    Source =
        "module WrongResumeValue\n"
        "effect State\n"
        "operation put : Int -> Int\n"
        "end\n"
        "transform run ignored = handle perform State.put(1) then {\n"
        "  State { put(value) with k -> resume(k, true) }\n"
        "}\n",
    Errors = transform_type_errors(Source),
    Context = error_context(resume_value_type_mismatch, Errors),
    ?assertMatch(
        #{
            expected_type := {tcon, int},
            actual_type := {tcon, bool},
            operation_declaration := #{
                effect := 'State',
                operation := put,
                location := _
            },
            binder_origin := _,
            delimiter_location := _,
            resume_location := _
        },
        Context
    ),
    Message = catena_type_error:format_error(
        {resume_value_type_mismatch, Context}
    ),
    ?assert(string:find(Message, "Operation declaration") =/= nomatch),
    ?assert(string:find(Message, "Binder origin") =/= nomatch),
    ?assert(string:find(Message, "Delimiter") =/= nomatch),
    ?assert(string:find(Message, "Resume expression") =/= nomatch).

wrong_handler_result_reports_delimiter_mismatch_test() ->
    Source =
        "module WrongDelimiterResult\n"
        "effect State\n"
        "operation put : Int -> Int\n"
        "end\n"
        "transform run ignored = handle perform State.put(1) then {\n"
        "  State { put(value) with k -> true }\n"
        "}\n",
    Errors = transform_type_errors(Source),
    ?assertMatch(
        #{
            expected_type := {tcon, int},
            actual_type := {tcon, bool},
            operation_declaration := #{location := _},
            binder_origin := _,
            delimiter_location := _
        },
        error_context(resume_delimiter_type_mismatch, Errors)
    ).

non_resumption_target_has_dedicated_error_test() ->
    Env = catena_type_env:singleton(
        value,
        catena_type_scheme:mono(catena_types:tcon(int))
    ),
    Expr = {
        resume_expr,
        {var, value, location(3, 8)},
        {lit, {int, 1}},
        location(3, 1)
    },
    {error, Errors} = catena_infer:infer_expr(Expr, Env),
    ?assertMatch(
        #{
            target := value,
            expected_type := {tresumption, _, _, _, _},
            actual_type := {tcon, int},
            target_origin := {location, 3, 8},
            resume_location := {location, 3, 1}
        },
        error_context(invalid_resume_target, Errors)
    ).

typed_transform_type_and_evidence(Source) ->
    {ok, {typed_module, _Name, Declarations, _Exports}} =
        catena_compile:compile_string(Source),
    {
        typed_transform,
        run,
        Type,
        _Clauses,
        #{resumptions := Evidence},
        _Location
    } = lists:keyfind(typed_transform, 1, Declarations),
    {Type, Evidence}.

resumption_evidence(Source) ->
    {_Type, Evidence} = typed_transform_type_and_evidence(Source),
    Evidence.

transform_type_errors(Source) ->
    {error, {type_error, run, Errors}} =
        catena_compile:compile_string(Source),
    Errors.

error_context(Tag, Errors) ->
    {Tag, Context} = lists:keyfind(Tag, 1, Errors),
    Context.

location(Line, Column) ->
    {location, Line, Column}.
