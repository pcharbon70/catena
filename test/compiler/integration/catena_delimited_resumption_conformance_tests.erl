%%%-------------------------------------------------------------------
%%% @doc Stable SCN-012 delimited-resumption conformance evidence.
%%%
%%% The scenario deliberately crosses source compilation, optimized and
%%% unoptimized control IR, BEAM artifact validation/loading, and opaque
%%% runtime authority. Deferred or inconsistent behavior fails closed.
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_conformance_tests).

-include_lib("eunit/include/eunit.hrl").

promoted_modes_execute_from_optimized_and_unoptimized_beam_test() ->
    Scenarios = catena_resumption_benchmark:scenarios(),
    Cases = [
        {deep_one_shot, 'PhaseEightPerfDeep', 42},
        {shallow_handling, 'PhaseEightPerfShallow', 42},
        {multi_shot_branching, 'PhaseEightPerfMulti', 82}
    ],
    lists:foreach(
        fun({Name, Module, Expected}) ->
            Source = maps:get(Name, Scenarios),
            ?assertEqual(Expected, execute(Source, Module, #{})),
            ?assertEqual(Expected, execute(Source, Module, no_optimization()))
        end,
        Cases
    ).

artifact_mode_disagreement_fails_before_load_test() ->
    Source = maps:get(
        shallow_handling,
        catena_resumption_benchmark:scenarios()
    ),
    {ok, Artifact} = catena_compile:compile_string_to_beam(Source),
    Module = maps:get(runtime_module, Artifact),
    unload(Module),
    Contract = maps:get(runtime_contract, Artifact),
    Tampered = Artifact#{runtime_contract := Contract#{
        handler_modes := [#{depth => deep, kind => one_shot}]
    }},
    {error, Diagnostic} = catena_beam_artifact:load(Tampered),
    ?assertEqual(
        artifact_validation_failed,
        catena_backend_error:category(Diagnostic)
    ),
    ?assertEqual(non_existing, code:which(Module)).

ownership_version_consumption_and_budget_fail_closed_test() ->
    catena_resumption_runtime:reset_for_test(),
    Owner = self(),
    {ok, OneShot} = capture(
        fun(Value, _Context) -> {self(), Value} end,
        one_shot,
        #{}
    ),
    spawn(fun() ->
        Owner ! {foreign_resume,
            catena_resumption_runtime:resume(OneShot, foreign)}
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
    ?assertEqual(
        {ok, {Owner, local}},
        catena_resumption_runtime:resume(OneShot, local)
    ),
    ?assertMatch(
        {error, #{category := resumption_already_consumed}},
        catena_resumption_runtime:resume(OneShot, repeated)
    ),
    ?assertMatch(
        {error, #{category := invalid_resumption_version}},
        catena_resumption_runtime:resume(
            {catena_resumption, catena_resumption_runtime:version() - 1,
                make_ref()},
            invalid
        )
    ),
    {ok, MultiShot} = capture(
        fun(Value, _Context) -> Value end,
        multi_shot,
        #{max_invocations => 1}
    ),
    ?assertEqual(
        {ok, first},
        catena_resumption_runtime:resume(MultiShot, first)
    ),
    ?assertMatch(
        {error, #{
            category := resumption_budget_exceeded,
            details := #{resource := invocations, limit := 1}
        }},
        catena_resumption_runtime:resume(MultiShot, exhausted)
    ),
    catena_resumption_runtime:reset_for_test().

deferred_stateful_multishot_capture_fails_closed_test() ->
    catena_resumption_runtime:reset_for_test(),
    Owner = self(),
    ?assertMatch(
        {error, #{
            category := inadmissible_multishot_context,
            details := #{reason := lexical_capability}
        }},
        capture(
            fun(Value, _Context) -> {Owner, Value} end,
            multi_shot,
            #{}
        )
    ),
    catena_resumption_runtime:reset_for_test().

capture(Continuation, Kind, Budget) ->
    Context = catena_effect_runtime:empty_context(),
    catena_resumption_runtime:capture(Continuation, #{
        context => Context,
        parent_context => Context,
        delimiter => make_ref(),
        depth => deep,
        kind => Kind,
        origin => #{construct => conformance, scenario => 'SCN-012'},
        metadata => #{scenario => 'SCN-012'},
        type_identity => dynamic,
        budget => Budget
    }).

no_optimization() ->
    #{codegen_opts => #{optimize_control => false}}.

execute(Source, Module, Options) ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(Source, Options),
    unload(Module),
    try
        {module, Module} = catena_beam_artifact:load(Artifact),
        Module:run(ignored)
    after
        unload(Module),
        catena_resumption_runtime:reset_for_test()
    end.

unload(Module) ->
    _ = code:soft_purge(Module),
    _ = code:delete(Module),
    _ = code:purge(Module),
    ok.
