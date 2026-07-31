%%%-------------------------------------------------------------------
%%% @doc Phase 3 source-to-typed-frontend integration contract.
%%%
%%% Phase 3 promotes first-class static Resumption values, not executable
%%% selective CPS. Explicit control must therefore reach validated units with
%%% complete evidence and still fail closed at the backend boundary.
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_phase3_tests).

-include_lib("eunit/include/eunit.hrl").

polymorphic_operation_instantiates_resumption_input_test() ->
    Source =
        "module PhaseThreePolymorphic\n"
        "effect Echo\n"
        "operation echo : a -> a\n"
        "end\n"
        "transform run ignored = handle "
        "perform Echo.echo(\"hello\") then {\n"
        "  Echo { echo(value) with k -> resume(k, value) }\n"
        "}\n",
    {Type, [Binder, Resume]} = run_type_and_evidence(Source),
    ?assertMatch(
        {tfun, _, {tcon, string}, {effect_set, []}},
        Type
    ),
    Expected = {
        tresumption,
        {tcon, 'OneShot'},
        {tcon, string},
        {tcon, string},
        {teffectrow, [], closed}
    },
    ?assertEqual(Expected, maps:get(type, Binder)),
    ?assertEqual(Expected, maps:get(type, Resume)).

nested_delimiters_retain_distinct_typed_authorities_test() ->
    Source =
        "module PhaseThreeNested\n"
        "effect Outer\n"
        "operation outer : Int\n"
        "end\n"
        "effect Inner\n"
        "operation inner : Int\n"
        "end\n"
        "transform run ignored = handle "
        "(handle perform Inner.inner() then {\n"
        "  Inner { inner() with inner_k -> resume(inner_k, 3) }\n"
        "}) then {\n"
        "  Outer { outer() with outer_k -> resume(outer_k, 4) }\n"
        "}\n",
    {_Type, Evidence} = run_type_and_evidence(Source),
    Binders = [
        maps:get(binder, Entry)
        || Entry <- Evidence,
           maps:get(kind, Entry) =:= resumption_binder
    ],
    ?assertEqual([inner_k, outer_k], Binders),
    ?assertEqual(
        4,
        length(Evidence)
    ),
    lists:foreach(
        fun(Entry) ->
            ?assertMatch(
                {
                    tresumption,
                    {tcon, 'OneShot'},
                    {tcon, int},
                    {tcon, int},
                    {teffectrow, [], closed}
                },
                maps:get(type, Entry)
            )
        end,
        Evidence
    ).

algebraic_storage_return_and_higher_order_flow_test() ->
    Source =
        "module PhaseThreeFirstClass\n"
        "type Box a = Box a\n"
        "effect State\n"
        "operation put : Int -> Int\n"
        "end\n"
        "transform unbox Box(value) = value\n"
        "transform preserve value = value\n"
        "transform run ignored = handle perform State.put(1) then {\n"
        "  State { put(value) with k -> "
        "let stored = Box (preserve k) in "
        "resume(unbox stored, value) }\n"
        "}\n",
    {Type, Evidence} = run_type_and_evidence(Source),
    ?assertMatch({tfun, _, {tcon, int}, {effect_set, []}}, Type),
    ?assertEqual(2, length(Evidence)),
    ?assertMatch(
        #{kind := resume, target_origin := _},
        lists:nth(2, Evidence)
    ).

open_rows_and_resume_result_transformation_reach_typed_frontend_test() ->
    OpenSource =
        "transform advance : "
        "Resumption k Int Int e -> Int -> Int\n",
    {ok, {typed_module, _, [OpenTransform], _}} =
        catena_compile:compile_string(OpenSource),
    {
        typed_transform,
        advance,
        {
            tfun,
            {
                tresumption,
                {tkvar, resumption_kind, _},
                {tcon, int},
                {tcon, int},
                {teffectrow, [], Row}
            },
            _,
            _
        },
        [],
        _
    } = OpenTransform,
    ?assert(is_integer(Row)),
    TransformSource =
        "module PhaseThreeResumeResult\n"
        "effect State\n"
        "operation put : Int -> Int\n"
        "end\n"
        "transform run ignored = handle "
        "(let result = perform State.put(1) in result + 1) then {\n"
        "  State { put(value) with k -> "
        "let completed = resume(k, value) in completed + 10 }\n"
        "}\n",
    {Type, [_, Resume]} = run_type_and_evidence(TransformSource),
    ?assertMatch({tfun, _, {tcon, int}, _}, Type),
    ?assertEqual({tcon, int}, maps:get(supplied_type, Resume)).

automatic_value_handler_remains_executable_test() ->
    Source =
        "module PhaseThreeAutomaticCompatibility\n"
        "export transform run\n"
        "effect Reader\n"
        "operation read : Int\n"
        "end\n"
        "transform run ignored = handle perform Reader.read() then {\n"
        "  Reader { read() -> 42 }\n"
        "}\n",
    {_Type, [Binder, Resume]} = run_type_and_evidence(Source),
    ?assertMatch(
        #{
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
            resume_location := {
                synthetic,
                value_handler_auto_resume,
                _
            }
        },
        Resume
    ),
    with_loaded_module(Source, fun() ->
        ?assertEqual(42, 'PhaseThreeAutomaticCompatibility':run(0))
    end).

typed_unit_retains_origins_and_reaches_control_backend_test() ->
    Source = explicit_state_source("resume(k, value)"),
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    ?assertMatch(
        {ok, #{construct := operation_case, mode := explicit_control}},
        catena_resumption_normalize:first_resumption(
            catena_compilation_unit:normalized_ast(Unit)
        )
    ),
    {typed_module, _, Declarations, _} =
        catena_compilation_unit:typed_module(Unit),
    {
        typed_transform,
        run,
        _,
        _,
        #{resumptions := [Binder, Resume]},
        _
    } = lists:keyfind(run, 2, Declarations),
    lists:foreach(
        fun(Entry) ->
            ?assertMatch(
                #{
                    operation_declaration := #{location := _},
                    binder_origin := _,
                    delimiter_location := _
                },
                Entry
            )
        end,
        [Binder, Resume]
    ),
    ?assertMatch({ok, _}, catena_compile:compile_string_to_core(Source)).

negative_type_and_effect_diagnostics_are_stable_test() ->
    InvalidTarget =
        "transform bad ignored = resume(1, 1)\n",
    ?assert(has_transform_error(
        bad,
        invalid_resume_target,
        catena_compile:compile_string(InvalidTarget)
    )),
    WrongValue = explicit_state_source("resume(k, true)"),
    ?assert(has_transform_error(
        run,
        resume_value_type_mismatch,
        catena_compile:compile_string(WrongValue)
    )),
    WrongDelimiter = explicit_state_source("true"),
    ?assert(has_transform_error(
        run,
        resume_delimiter_type_mismatch,
        catena_compile:compile_string(WrongDelimiter)
    )),
    ResidualMismatch =
        "module PhaseThreeResidualMismatch\n"
        "effect State\n"
        "operation put : Int -> Int\n"
        "end\n"
        "effect Log\n"
        "operation write : Int -> Int\n"
        "end\n"
        "transform run : Int -> Int\n"
        "transform run ignored = handle "
        "(let logged = perform Log.write(0) in "
        "perform State.put(logged)) then {\n"
        "  State { put(value) with k -> resume(k, value) }\n"
        "}\n",
    Result = catena_compile:compile_string(ResidualMismatch),
    ?assert(has_transform_error(run, resume_effect_mismatch, Result)),
    {error, {type_error, run, Errors}} = Result,
    ?assertMatch(
        #{
            declared_effects := {effect_set, []},
            inferred_effects := {effect_set, ['Log']},
            residual_effects := {teffectrow, ['Log'], closed}
        },
        error_context(resume_effect_mismatch, Errors)
    ).

negative_consumption_mode_and_opacity_diagnostics_are_stable_test() ->
    Duplicate = explicit_state_source(
        "let first = resume(k, value) in resume(k, value)"
    ),
    ?assert(has_transform_error(
        run,
        obvious_one_shot_reuse,
        catena_compile:compile_string(Duplicate)
    )),
    ?assertMatch(
        {error, {invalid_resumption_representation, #{
            reason := reserved_type_name
        }}},
        catena_compile:compile_string("type Resumption = Fake\n")
    ),
    MultiShot = catena_types:tresumption(
        catena_types:multi_shot(),
        catena_types:tcon(int),
        catena_types:tcon(int),
        catena_types:teffectrow(['State'])
    ),
    Env = catena_type_env:singleton(
        k,
        catena_type_scheme:mono(MultiShot)
    ),
    {error, ModeErrors} = catena_infer:infer_expr(
        {
            resume_expr,
            {var, k, {location, 1, 8}},
            {lit, {int, 1}},
            {location, 1, 1}
        },
        Env
    ),
    ?assertMatch(
        #{
            requested_mode := multi_shot,
            reason := external_or_stateful_effects_not_duplicable
        },
        error_context(inadmissible_multi_shot_effects, ModeErrors)
    ).

run_type_and_evidence(Source) ->
    {ok, {typed_module, _Name, Declarations, _Env}} =
        catena_compile:compile_string(Source),
    {
        typed_transform,
        run,
        Type,
        _Clauses,
        #{resumptions := Evidence},
        _Location
    } = lists:keyfind(run, 2, Declarations),
    {Type, Evidence}.

explicit_state_source(CaseBody) ->
    "module PhaseThreeExplicitState\n"
    "effect State\n"
    "operation put : Int -> Int\n"
    "end\n"
    "transform run ignored = handle perform State.put(1) then {\n"
    "  State { put(value) with k -> " ++ CaseBody ++ " }\n"
    "}\n".

has_transform_error(Name, Tag, {error, {type_error, Name, Errors}}) ->
    lists:keymember(Tag, 1, Errors);
has_transform_error(_Name, _Tag, _Other) ->
    false.

error_context(Tag, Errors) ->
    {Tag, Context} = lists:keyfind(Tag, 1, Errors),
    Context.

with_loaded_module(Source, Assertion) ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(Source),
    Module = maps:get(runtime_module, Artifact),
    unload(Module),
    try
        {module, Module} = code:load_binary(
            Module,
            "delimited-resumption-phase3-memory",
            maps:get(beam, Artifact)
        ),
        Assertion()
    after
        unload(Module),
        case catena_effect_system:is_initialized() of
            true -> catena_effect_system:stop_runtime();
            false -> ok
        end
    end.

unload(Module) ->
    _ = code:purge(Module),
    _ = code:delete(Module),
    ok.
