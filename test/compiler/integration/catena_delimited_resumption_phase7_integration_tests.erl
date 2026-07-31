%%%-------------------------------------------------------------------
%%% @doc Phase 7 mixed-mode and fail-closed source-to-BEAM evidence.
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_phase7_integration_tests).

-include_lib("eunit/include/eunit.hrl").

depth_ordering_shadowing_fallback_abort_and_owner_test() ->
    Owner = self(),
    with_loaded(depth_matrix_source(), 'PhaseSevenDepthMatrix', fun(Module) ->
        ?assertEqual(20, Module:deep_inside_shallow(ignored)),
        ?assertEqual(11, Module:shallow_inside_deep(ignored)),
        ?assertEqual(42, Module:parent_fallback(ignored)),
        ?assertEqual(7, Module:abort(ignored)),
        ?assertEqual(Owner, Module:owner(ignored))
    end).

retained_shallow_exception_and_process_identity_test() ->
    catena_resumption_runtime:reset_for_test(),
    Owner = self(),
    RetainCase = catena_effect_runtime:control_case(
        choose,
        0,
        fun([], Resumption, _HandlerContext) -> Resumption end
    ),
    Retained = catena_effect_runtime:with_resumable_handler(
        catena_effect_runtime:empty_context(),
        handler('Choice', [RetainCase], shallow, one_shot, #{}),
        fun(Context) ->
            catena_effect_runtime:perform_cps(
                Context,
                'Choice',
                choose,
                [],
                fun(Value, _RestoredContext) -> {Value, self()} end
            )
        end
    ),
    ?assertEqual(
        {retained_value, Owner},
        catena_effect_runtime:resume(Retained, retained_value)
    ),
    ?assertMatch(
        {error, #{category := resumption_already_consumed}},
        catena_effect_runtime:resume(Retained, repeated)
    ),
    Parent = self(),
    CrashCase = catena_effect_runtime:control_case(
        choose,
        0,
        fun([], Resumption, _HandlerContext) ->
            Parent ! {exception_handle, Resumption},
            catena_effect_runtime:resume(Resumption, crash)
        end
    ),
    Result = catena_effect_runtime:with_resumable_handler(
        catena_effect_runtime:empty_context(),
        handler('Choice', [CrashCase], shallow, one_shot, #{}),
        fun(Context) ->
            catena_effect_runtime:perform_cps(
                Context,
                'Choice',
                choose,
                [],
                fun(_Value, _RestoredContext) ->
                    erlang:error(resumed_branch_failed)
                end
            )
        end
    ),
    ?assertMatch(
        {error, #{
            category := handler_failure,
            details := #{reason := resumed_branch_failed}
        }},
        Result
    ),
    receive
        {exception_handle, ExceptionHandle} ->
            ?assertEqual(
                {ok, consumed},
                catena_resumption_runtime:status(ExceptionHandle)
            )
    after 1000 ->
        ?assert(false)
    end,
    catena_resumption_runtime:reset_for_test().

many_distinct_multishot_branches_execute_from_source_test() ->
    with_loaded(many_branches_source(), 'PhaseSevenManyBranches', fun(Module) ->
        ?assertEqual([10, 20, 30, 40], Module:run(ignored))
    end).

nondeterministic_search_enumerates_nested_branches_deterministically_test() ->
    with_loaded(
        nondeterministic_source(),
        'PhaseSevenNondeterministicSearch',
        fun(Module) ->
            Expected = [0, 1, 10, 11],
            ?assertEqual(Expected, Module:run(ignored)),
            ?assertEqual(Expected, Module:run(ignored))
        end
    ).

branch_failure_budget_and_cross_process_paths_are_isolated_test() ->
    catena_resumption_runtime:reset_for_test(),
    {ok, Handle} = capture_multi(
        fun
            (fail, _Context) -> erlang:error(rejected_branch);
            (Value, _Context) -> {self(), Value}
        end,
        #{max_invocations => 2}
    ),
    ?assertMatch(
        {error, #{category := handler_failure}},
        catena_resumption_runtime:resume(Handle, fail)
    ),
    Parent = self(),
    spawn(fun() ->
        Parent ! {foreign_multi,
            catena_resumption_runtime:resume(Handle, foreign)}
    end),
    receive
        {foreign_multi, ForeignResult} ->
            ?assertMatch(
                {error, #{category := wrong_resumption_owner}},
                ForeignResult
            )
    after 1000 ->
        ?assert(false)
    end,
    ?assertEqual(
        {ok, {self(), recovered}},
        catena_resumption_runtime:resume(Handle, recovered)
    ),
    ?assertMatch(
        {error, #{
            category := resumption_budget_exceeded,
            details := #{resource := invocations, limit := 2}
        }},
        catena_resumption_runtime:resume(Handle, exhausted)
    ),
    {ok, Stats} = catena_resumption_runtime:branch_stats(Handle),
    ?assertEqual(2, maps:get(completed_branches, Stats)),
    ?assertEqual(1, maps:get(failed_branches, Stats)),
    catena_resumption_runtime:reset_for_test().

syntax_effect_rows_and_unselected_default_fail_closed_test() ->
    ?assertMatch(
        {error, _},
        catena_compile:compile_string(
            "transform bad = handle multi shot 1 then { "
            "Choice { choose() with k -> resume(k, 1) } }"
        )
    ),
    {error, {type_error, run, ShallowErrors}} =
        catena_compile:compile_string_to_unit(shallow_declared_pure_source()),
    ?assertMatch(
        #{handler_depth := shallow},
        error_context(resume_effect_mismatch, ShallowErrors)
    ),
    {error, {type_error, run, ResidualErrors}} =
        catena_compile:compile_string(inadmissible_residual_source()),
    ?assertMatch(
        #{
            requested_mode := multi_shot,
            inadmissible_effects := ['Log']
        },
        error_context(inadmissible_multi_shot_effects, ResidualErrors)
    ),
    OpenType = catena_types:tresumption(
        catena_types:multi_shot(),
        catena_types:tcon(int),
        catena_types:tcon(int),
        catena_types:teffectrow([], 701)
    ),
    ?assertMatch(
        {error, {inadmissible_multi_shot_effects,
            #{reason := open_effect_row}}},
        catena_resumption_flow:validate_supported_mode(OpenType, #{})
    ),
    ?assertMatch(
        {error, {type_error, run, _}},
        catena_compile:compile_string(default_double_resume_source())
    ).

artifact_runtime_version_and_kind_disagreement_fail_before_load_test() ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(
        many_branches_source()
    ),
    Contract = maps:get(runtime_contract, Artifact),
    ?assertEqual(
        [#{depth => deep, kind => multi_shot}],
        maps:get(handler_modes, Contract)
    ),
    Availability = runtime_availability(
        maps:get(runtime_dependencies, Artifact)
    ),
    StaleAvailability = Availability#{catena_resumption_runtime := #{
        version => catena_resumption_runtime:version() - 1,
        features => catena_resumption_runtime:features()
    }},
    {error, StaleDiagnostic} = catena_beam_artifact:validate(
        Artifact,
        #{available_runtime_modules => StaleAvailability}
    ),
    ?assertEqual(
        incompatible_runtime_version,
        maps:get(reason, catena_backend_error:details(StaleDiagnostic))
    ),
    KindMismatch = Artifact#{runtime_contract := Contract#{
        handler_modes := [#{depth => deep, kind => one_shot}]
    }},
    {error, KindDiagnostic} = catena_beam_artifact:validate(KindMismatch),
    ?assertEqual(
        artifact_validation_failed,
        catena_backend_error:category(KindDiagnostic)
    ),
    ?assertEqual(non_existing, code:which('PhaseSevenManyBranches')).

deep_one_shot_default_remains_exact_test() ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(
        default_one_shot_source()
    ),
    Contract = maps:get(runtime_contract, Artifact),
    ?assertEqual(
        [#{depth => deep, kind => one_shot}],
        maps:get(handler_modes, Contract)
    ),
    with_artifact(Artifact, 'PhaseSevenDefaultOneShot', fun(Module) ->
        ?assertEqual(42, Module:run(ignored))
    end).

handler(Effect, Cases, Depth, Kind, Budget) ->
    #{
        effect => Effect,
        cases => Cases,
        depth => Depth,
        resumption_kind => Kind,
        resumption_budget => Budget,
        origin => {phase7_integration_handler, Effect, Depth, Kind}
    }.

capture_multi(Continuation, Budget) ->
    catena_resumption_runtime:capture(Continuation, #{
        context => #{},
        parent_context => #{},
        delimiter => make_ref(),
        depth => deep,
        kind => multi_shot,
        origin => {phase7_integration_multishot, ?FUNCTION_NAME},
        metadata => #{fixture => phase7_integration},
        type_identity => dynamic,
        budget => Budget
    }).

depth_matrix_source() ->
    "module PhaseSevenDepthMatrix\n"
    "export transform deep_inside_shallow\n"
    "export transform shallow_inside_deep\n"
    "export transform parent_fallback\n"
    "export transform abort\n"
    "export transform owner\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "effect Outer\n"
    "operation read : Int\n"
    "end\n"
    "effect Inner\n"
    "operation touch : Int\n"
    "end\n"
    "effect Process\n"
    "operation self : Int\n"
    "end\n"
    "transform deep_inside_shallow ignored = handle shallow "
        "(handle (let first = perform Choice.choose() in "
            "let second = perform Choice.choose() in first + second) "
        "then { Choice { choose() with inner_k -> "
            "resume(inner_k, 10) } }) "
    "then { Choice { choose() with outer_k -> "
        "resume(outer_k, 1) } }\n"
    "transform shallow_inside_deep ignored = handle "
        "(handle shallow (let first = perform Choice.choose() in "
            "let second = perform Choice.choose() in first + second) "
        "then { Choice { choose() with inner_k -> "
            "resume(inner_k, 10) } }) "
    "then { Choice { choose() with outer_k -> "
        "resume(outer_k, 1) } }\n"
    "transform parent_fallback ignored = handle "
        "(handle perform Outer.read() then { "
            "Inner { touch() with inner_k -> resume(inner_k, 0) } }) "
    "then { Outer { read() with outer_k -> resume(outer_k, 42) } }\n"
    "transform abort ignored = handle "
        "(let value = perform Choice.choose() in value + 100) "
    "then { Choice { choose() with k -> 7 } }\n"
    "transform owner ignored = perform Process.self()\n".

many_branches_source() ->
    "module PhaseSevenManyBranches\n"
    "export transform run\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run ignored = handle multi_shot "
        "(let selected = perform Choice.choose() in [selected * 10]) "
    "then {\n"
    "  Choice { choose() with k -> "
        "let first = resume(k, 1) in "
        "let second = resume(k, 2) in "
        "let third = resume(k, 3) in "
        "let fourth = resume(k, 4) in "
        "first ++ second ++ third ++ fourth }\n"
    "}\n".

nondeterministic_source() ->
    "module PhaseSevenNondeterministicSearch\n"
    "export transform run\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run ignored = handle multi_shot "
        "(let left = perform Choice.choose() in "
        "let right = perform Choice.choose() in [left * 10 + right]) "
    "then {\n"
    "  Choice { choose() with k -> "
        "let zero = resume(k, 0) in "
        "let one = resume(k, 1) in zero ++ one }\n"
    "}\n".

shallow_declared_pure_source() ->
    "module PhaseSevenShallowPureRejected\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run : Int / {}\n"
    "transform run = handle shallow perform Choice.choose() then {\n"
    "  Choice { choose() with k -> resume(k, 1) }\n"
    "}\n".

inadmissible_residual_source() ->
    "module PhaseSevenResidualRejected\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "effect Log\n"
    "operation write : Int -> Int\n"
    "end\n"
    "transform run ignored = handle multi_shot "
        "(let logged = perform Log.write(0) in "
        "perform Choice.choose()) then {\n"
    "  Choice { choose() with k -> resume(k, 1) }\n"
    "}\n".

default_double_resume_source() ->
    "module PhaseSevenDefaultReuseRejected\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run ignored = handle perform Choice.choose() then {\n"
    "  Choice { choose() with k -> "
        "let first = resume(k, 1) in resume(k, first) }\n"
    "}\n".

default_one_shot_source() ->
    "module PhaseSevenDefaultOneShot\n"
    "export transform run\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run ignored = handle "
        "(let selected = perform Choice.choose() in selected + 1) "
    "then { Choice { choose() with k -> resume(k, 41) } }\n".

error_context(Reason, Errors) ->
    Matches = [Context || {Candidate, Context} <- Errors, Candidate =:= Reason],
    case Matches of
        [Context | _] -> Context;
        [] -> undefined
    end.

runtime_availability(Dependencies) ->
    maps:from_list([
        {maps:get(module, Dependency), #{
            version => maps:get(version, Dependency),
            features => maps:get(features, Dependency, [])
        }}
        || Dependency <- Dependencies
    ]).

with_loaded(Source, Module, Assertion) ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(Source),
    with_artifact(Artifact, Module, Assertion).

with_artifact(Artifact, Module, Assertion) ->
    {ok, Artifact} = catena_beam_artifact:validate(Artifact),
    unload(Module),
    try
        {module, Module} = catena_beam_artifact:load(Artifact),
        Assertion(Module)
    after
        unload(Module),
        catena_resumption_runtime:reset_for_test()
    end.

unload(Module) ->
    code:purge(Module),
    code:delete(Module).
