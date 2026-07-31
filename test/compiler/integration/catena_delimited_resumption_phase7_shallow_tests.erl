%%%-------------------------------------------------------------------
%%% @doc Section 7.2 shallow-handler runtime and artifact semantics.
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_phase7_shallow_tests).

-include_lib("eunit/include/eunit.hrl").

catena_delimited_resumption_phase7_shallow_test_() ->
    {foreach,
        fun setup/0,
        fun cleanup/1,
        [
            fun shallow_resume_removes_only_selected_frame/0,
            fun deep_resume_reinstalls_selected_frame/0,
            fun retained_shallow_resume_is_one_shot_and_process_affine/0,
            fun source_to_loaded_beam_preserves_depth_and_contract/0
        ]}.

setup() ->
    catena_resumption_runtime:reset_for_test().

cleanup(_State) ->
    catena_resumption_runtime:reset_for_test(),
    unload('PhaseSevenShallowRuntime').

shallow_resume_removes_only_selected_frame() ->
    Owner = self(),
    ?assertEqual(
        {inner, marker, outer, Owner},
        nested_result(shallow)
    ).

deep_resume_reinstalls_selected_frame() ->
    Owner = self(),
    ?assertEqual(
        {inner, marker, inner, Owner},
        nested_result(deep)
    ).

retained_shallow_resume_is_one_shot_and_process_affine() ->
    Owner = self(),
    OuterCase = resume_case(outer),
    RetainCase = catena_effect_runtime:control_case(
        choose,
        0,
        fun([], Resumption, _HandlerContext) -> Resumption end
    ),
    Retained = catena_effect_runtime:with_resumable_handler(
        catena_effect_runtime:empty_context(),
        handler('Choice', [OuterCase], deep),
        fun(OuterContext) ->
            catena_effect_runtime:with_resumable_handler(
                OuterContext,
                handler('Choice', [RetainCase], shallow),
                fun(InnerContext) ->
                    catena_effect_runtime:perform_cps(
                        InnerContext,
                        'Choice',
                        choose,
                        [],
                        fun(First, RestoredContext) ->
                            catena_effect_runtime:perform_cps(
                                RestoredContext,
                                'Choice',
                                choose,
                                [],
                                fun(Second, _FinalContext) ->
                                    {First, Second, self()}
                                end
                            )
                        end
                    )
                end
            )
        end
    ),
    ?assert(catena_resumption_runtime:is_resumption(Retained)),
    ?assertEqual(
        {supplied, outer, Owner},
        catena_effect_runtime:resume(Retained, supplied)
    ),
    ?assertMatch(
        {error, #{category := resumption_already_consumed}},
        catena_effect_runtime:resume(Retained, again)
    ).

source_to_loaded_beam_preserves_depth_and_contract() ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(source()),
    Contract = maps:get(runtime_contract, Artifact),
    ?assertEqual(3, maps:get(artifact_format_version, Contract)),
    ?assertEqual(2, maps:get(control_abi_version, Contract)),
    ?assertEqual(2, maps:get(resumption_runtime_version, Contract)),
    ?assertEqual(
        [
            #{depth => deep, kind => one_shot},
            #{depth => shallow, kind => one_shot}
        ],
        maps:get(handler_modes, Contract)
    ),
    {ok, Artifact} = catena_beam_artifact:validate(Artifact),
    {module, 'PhaseSevenShallowRuntime'} =
        catena_beam_artifact:load(Artifact),
    ?assertEqual(11, 'PhaseSevenShallowRuntime':run(ignored)).

nested_result(Depth) ->
    OuterCase = resume_case(outer),
    InnerCase = resume_case(inner),
    MarkerCase = {mark, fun() -> marker end},
    catena_effect_runtime:with_resumable_handler(
        catena_effect_runtime:empty_context(),
        handler('Choice', [OuterCase], deep),
        fun(OuterContext) ->
            catena_effect_runtime:with_value_provider(
                OuterContext,
                {'Marker', [MarkerCase]},
                fun(MarkerContext) ->
                    catena_effect_runtime:with_resumable_handler(
                        MarkerContext,
                        handler('Choice', [InnerCase], Depth),
                        fun(InnerContext) ->
                            two_operations(InnerContext)
                        end
                    )
                end
            )
        end
    ).

two_operations(Context) ->
    catena_effect_runtime:perform_cps(
        Context,
        'Choice',
        choose,
        [],
        fun(First, RestoredContext) ->
            catena_effect_runtime:perform_cps(
                RestoredContext,
                'Marker',
                mark,
                [],
                fun(Marker, MarkerContext) ->
                    catena_effect_runtime:perform_cps(
                        MarkerContext,
                        'Choice',
                        choose,
                        [],
                        fun(Second, _FinalContext) ->
                            {First, Marker, Second, self()}
                        end
                    )
                end
            )
        end
    ).

resume_case(Value) ->
    catena_effect_runtime:control_case(
        choose,
        0,
        fun([], Resumption, _HandlerContext) ->
            catena_effect_runtime:resume(Resumption, Value)
        end
    ).

handler(Effect, Cases, Depth) ->
    #{
        effect => Effect,
        cases => Cases,
        depth => Depth,
        resumption_kind => one_shot,
        origin => {phase7_shallow_handler, Effect, Depth}
    }.

source() ->
    "module PhaseSevenShallowRuntime\n"
    "export transform run\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run ignored = handle "
        "(handle shallow "
            "(let first = perform Choice.choose() in "
            "let second = perform Choice.choose() in first + second) "
        "then { Choice { choose() with inner_k -> "
            "resume(inner_k, 10) } }) "
    "then { Choice { choose() with outer_k -> "
        "resume(outer_k, 1) } }\n".

unload(Module) ->
    code:purge(Module),
    code:delete(Module).
