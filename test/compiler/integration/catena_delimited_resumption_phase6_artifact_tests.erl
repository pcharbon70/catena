%%%-------------------------------------------------------------------
%%% @doc Section 6.3 artifact contracts and source-oriented diagnostics.
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_phase6_artifact_tests).

-include_lib("eunit/include/eunit.hrl").

resumable_artifact_declares_and_loads_exact_contract_test() ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(resumable_source()),
    ?assertEqual(3, catena_beam_artifact:format_version()),
    ?assertEqual(3, maps:get(format_version, Artifact)),
    Contract = maps:get(runtime_contract, Artifact),
    ?assertEqual(2, maps:get(control_abi_version, Contract)),
    ?assertEqual(
        catena_resumption_runtime:version(),
        maps:get(resumption_runtime_version, Contract)
    ),
    ?assertEqual(
        [#{depth => deep, kind => one_shot}],
        maps:get(handler_modes, Contract)
    ),
    ?assertEqual(
        catena_resumption_runtime:features(),
        maps:get(required_handler_frame_features, Contract)
    ),
    ?assertEqual('PhaseSixArtifact', maps:get(source_module, Contract)),
    ?assert(lists:any(
        fun(Dependency) ->
            maps:get(module, Dependency) =:= catena_resumption_runtime andalso
                maps:get(features, Dependency) =:=
                    catena_resumption_runtime:features()
        end,
        maps:get(runtime_dependencies, Artifact)
    )),
    Attributes = core_attributes(maps:get(core, Artifact)),
    ?assertEqual(2, maps:get(catena_control_abi_version, Attributes)),
    ?assertEqual(
        catena_resumption_runtime:version(),
        maps:get(catena_resumption_runtime_version, Attributes)
    ),
    ?assertEqual(
        catena_resumption_runtime:features(),
        maps:get(catena_handler_frame_features, Attributes)
    ),
    {ok, Artifact} = catena_beam_artifact:validate(Artifact),
    unload('PhaseSixArtifact'),
    try
        {module, 'PhaseSixArtifact'} = catena_beam_artifact:load(Artifact),
        ?assertEqual(42, 'PhaseSixArtifact':run(0))
    after
        unload('PhaseSixArtifact')
    end.

stale_and_feature_incompatible_runtimes_fail_before_load_test() ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(resumable_source()),
    Availability = runtime_availability(
        maps:get(runtime_dependencies, Artifact)
    ),
    Stale = Availability#{catena_resumption_runtime := #{
        version => 0,
        features => catena_resumption_runtime:features()
    }},
    {error, StaleDiagnostic} = catena_beam_artifact:validate(
        Artifact,
        #{available_runtime_modules => Stale}
    ),
    ?assertEqual(
        runtime_dependency_unavailable,
        catena_backend_error:category(StaleDiagnostic)
    ),
    ?assertEqual(
        incompatible_runtime_version,
        maps:get(reason, catena_backend_error:details(StaleDiagnostic))
    ),
    MissingFeature = Availability#{catena_resumption_runtime := #{
        version => catena_resumption_runtime:version(),
        features => [deep_handlers]
    }},
    {error, FeatureDiagnostic} = catena_beam_artifact:validate(
        Artifact,
        #{available_runtime_modules => MissingFeature}
    ),
    ?assertEqual(
        missing_runtime_features,
        maps:get(reason, catena_backend_error:details(FeatureDiagnostic))
    ),
    ?assertEqual(non_existing, code:which('PhaseSixArtifact')).

artifact_versions_identities_and_interface_integrity_fail_closed_test() ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(resumable_source()),
    InvalidArtifacts = [
        Artifact#{format_version := 1},
        Artifact#{runtime_module := 'WrongRuntimeModule'},
        Artifact#{metadata := (maps:get(metadata, Artifact))#{
            interface_checksum := <<"stale">>
        }},
        Artifact#{runtime_contract := (maps:get(runtime_contract, Artifact))#{
            control_abi_version := 99
        }}
    ],
    lists:foreach(
        fun(Invalid) ->
            {error, Diagnostic} = catena_beam_artifact:validate(Invalid),
            ?assertEqual(
                artifact_validation_failed,
                catena_backend_error:category(Diagnostic)
            )
        end,
        InvalidArtifacts
    ),
    ?assertEqual(non_existing, code:which('PhaseSixArtifact')).

module_dependencies_are_bound_to_interface_checksums_test() ->
    Sources = #{
        'PhaseSixChecksumProvider' =>
            "module PhaseSixChecksumProvider\n"
            "export transform identity\n"
            "transform identity : Int -> Int\n"
            "transform identity value = value\n",
        'PhaseSixChecksumConsumer' =>
            "module PhaseSixChecksumConsumer\n"
            "export transform run\n"
            "import PhaseSixChecksumProvider\n"
            "transform run : Int -> Int\n"
            "transform run value = identity value\n"
    },
    {ok, Result} = catena_compile:compile_source_set_to_beam(Sources),
    Artifacts = maps:get(artifacts, Result),
    Provider = maps:get('PhaseSixChecksumProvider', Artifacts),
    Consumer = maps:get('PhaseSixChecksumConsumer', Artifacts),
    [Dependency] = [
        Item
        || Item <- maps:get(artifact_dependencies, Consumer),
           maps:get(kind, Item, runtime) =:= catena_module
    ],
    ?assertEqual(3, maps:get(interface_version, Dependency)),
    ?assertEqual(
        catena_module_interface:checksum(maps:get(interface, Provider)),
        maps:get(interface_checksum, Dependency)
    ),
    {ok, Consumer} = catena_beam_artifact:validate(
        Consumer,
        #{available_artifacts => #{
            'PhaseSixChecksumProvider' => Provider
        }}
    ),
    StaleProvider = Provider#{interface := (maps:get(interface, Provider))#{
        source_identity := stale_provider
    }},
    {error, Diagnostic} = catena_beam_artifact:validate(
        Consumer,
        #{available_artifacts => #{
            'PhaseSixChecksumProvider' => StaleProvider
        }}
    ),
    ?assertEqual(
        artifact_validation_failed,
        catena_backend_error:category(Diagnostic)
    ),
    ?assertMatch(
        {dependency_checksum_mismatch, 'PhaseSixChecksumProvider', _, _},
        maps:get(reason, catena_backend_error:details(Diagnostic))
    ).

control_origin_inventory_and_runtime_failures_hide_closures_test() ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(resumable_source()),
    Origins = maps:get(origins, maps:get(metadata, Artifact)),
    ControlOrigins = maps:get(control, Origins),
    Operations = [maps:get(operation, Origin) || Origin <- ControlOrigins],
    ?assert(lists:member(perform, Operations)),
    ?assert(lists:member(resume, Operations)),
    PerformOrigin = hd([
        Origin
        || Origin <- ControlOrigins,
           maps:get(operation, Origin) =:= perform
    ]),
    ?assertEqual(
        [perform, generated_continuation, runtime_capture],
        maps:get(synthetic_chain, PerformOrigin)
    ),
    RuntimeOrigin = #{source => {location, 9, 20}, construct => closure},
    Closure = catena_effect_runtime:control_closure(
        resumable,
        fun(_Arguments, _Context, _Continuation) ->
            erlang:error({private_closure, fun() -> secret end})
        end,
        RuntimeOrigin
    ),
    {error, Failure} = catena_effect_runtime:apply_control(
        Closure,
        [],
        catena_effect_runtime:empty_context(),
        fun(Value, _Context) -> Value end
    ),
    ?assertEqual(RuntimeOrigin, maps:get(origin, Failure)),
    ?assertEqual(
        handler_failed,
        maps:get(reason, maps:get(details, Failure))
    ),
    ?assertNot(contains_function(Failure)).

resumable_source() ->
    "module PhaseSixArtifact\n"
    "export transform run\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run ignored = handle "
        "(let selected = perform Choice.choose() in selected + 1) then {\n"
    "  Choice { choose() with k -> resume(k, 41) }\n"
    "}\n".

runtime_availability(Dependencies) ->
    maps:from_list([
        {maps:get(module, Dependency), #{
            version => maps:get(version, Dependency),
            features => maps:get(features, Dependency, [])
        }}
        || Dependency <- Dependencies
    ]).

core_attributes(Core) ->
    maps:from_list([
        {cerl:atom_val(Name), cerl:concrete(Value)}
        || {Name, Value} <- cerl:module_attrs(Core)
    ]).

contains_function(Term) when is_function(Term) -> true;
contains_function(Term) when is_tuple(Term) ->
    contains_function(tuple_to_list(Term));
contains_function(Term) when is_list(Term) ->
    lists:any(fun contains_function/1, Term);
contains_function(Term) when is_map(Term) ->
    contains_function(maps:to_list(Term));
contains_function(_Term) -> false.

unload(Module) ->
    code:purge(Module),
    code:delete(Module).
