%%%-------------------------------------------------------------------
%%% @doc Dedicated executable evidence for SCN-011.
%%%
%%% This suite is intentionally organized by the promoted backend feature
%%% matrix.  It exercises the maintained public artifact boundary wherever a
%%% single source module is sufficient and the closed-set linker for imports.
%%%-------------------------------------------------------------------
-module(catena_backend_conformance_tests).

-include_lib("eunit/include/eunit.hrl").

pure_higher_order_operator_and_collection_matrix_test() ->
    Source =
        "module BackendPureConformance\n"
        "export transform run\n"
        "transform add : Int -> Int -> Int\n"
        "transform add left right = left + right\n"
        "type AddBoundary = AddBoundary\n"
        "transform run = "
            "let increment = fn value -> value + 1 in "
            "(increment 41, 40 |> add 2, "
            "6 * 7, 8 / 2, 2 < 3 && 3 >= 3, "
            "[1, 2] ++ (3 :: [4]), "
            "{answer: 42}.answer)\n",
    with_public_module(Source, fun(Artifact) ->
        ?assertEqual(
            {42, 42, 42, 4.0, true, [1, 2, 3, 4], 42},
            'BackendPureConformance':run()
        ),
        ?assertEqual(
            control_dependencies([]),
            maps:get(runtime_dependencies, Artifact)
        )
    end).

recursive_pattern_and_data_representation_matrix_test() ->
    Source =
        "module BackendDataConformance\n"
        "export transform copy\n"
        "export transform decode\n"
        "type Nat = Zero | Succ Nat\n"
        "type Payload = Payload Int Int\n"
        "transform copy : Nat -> Nat\n"
        "transform copy Zero = Zero\n"
        "transform copy Succ(rest) = Succ (copy rest)\n"
        "type CopyBoundary = CopyBoundary\n"
        "transform decode "
            "(Payload(left right), [head tail], {answer: answer}) "
            "when answer > 0 = "
            "(left, right, head, tail, answer)\n",
    with_public_module(Source, fun(_Artifact) ->
        Value = {'Succ', {'Succ', {'Zero'}}},
        ?assertEqual(Value, 'BackendDataConformance':copy(Value)),
        ?assertEqual(
            {1, 2, 3, 4, 5},
            'BackendDataConformance':decode(
                {{'Payload', 1, 2}, [3, 4], #{answer => 5}}
            )
        )
    end).

imported_and_higher_order_call_matrix_test() ->
    Provider =
        "module BackendProvider\n"
        "export transform increment\n"
        "transform increment : Int -> Int\n"
        "transform increment value = value + 1\n",
    Consumer =
        "module BackendConsumer\n"
        "export transform run\n"
        "import BackendProvider\n"
        "transform invoke function value = function value\n"
        "transform run value = invoke increment value\n",
    Sources = #{
        'BackendProvider' => Provider,
        'BackendConsumer' => Consumer
    },
    with_source_set(Sources, fun(Result) ->
        ?assertEqual(
            ['BackendProvider', 'BackendConsumer'],
            maps:get(order, Result)
        ),
        ?assertEqual(42, 'BackendConsumer':run(41)),
        ConsumerArtifact = maps:get(
            'BackendConsumer',
            maps:get(artifacts, Result)
        ),
        ?assertMatch(
            [#{
                kind := catena_module,
                source_module := 'BackendProvider'
            }],
            [
                Dependency
                || Dependency <- maps:get(
                    artifact_dependencies,
                    ConsumerArtifact
                ),
                   maps:get(kind, Dependency, runtime) =:= catena_module
            ]
        )
    end).

effect_runtime_matrix_test() ->
    Source =
        "module BackendEffectConformance\n"
        "export transform run\n"
        "effect Answer\n"
        "operation get : Int\n"
        "end\n"
        "transform helper ignored = perform Answer.get()\n"
        "type HelperBoundary = HelperBoundary\n"
        "transform run ignored = handle helper ignored then {\n"
        "  Answer { get -> 42 }\n"
        "}\n",
    with_public_module(Source, fun(Artifact) ->
        ?assertEqual(42, 'BackendEffectConformance':run(0)),
        ?assertEqual(
            control_dependencies([
                #{
                    module => catena_effect_runtime,
                    version => catena_effect_runtime:version(),
                    features => catena_effect_runtime:features()
                },
                #{module => catena_effect_system, version => 1}
            ]),
            maps:get(runtime_dependencies, Artifact)
        ),
        ?assertNot(catena_effect_system:is_initialized())
    end).

trait_dictionary_matrix_test() ->
    Source =
        "module BackendTraitConformance\n"
        "export transform run\n"
        "type Flag = On | Off\n"
        "trait Comparable a where\n"
        "  equals : a -> a -> Bool,\n"
        "  notEquals : a -> a -> Bool,\n"
        "  notEquals left right = false\n"
        "end\n"
        "instance Comparable Flag where\n"
        "  transform equals left right = true\n"
        "end\n"
        "transform run left right = "
            "(equals left right, notEquals left right)\n",
    with_public_module(Source, fun(Artifact) ->
        ?assertEqual(
            {true, false},
            'BackendTraitConformance':run({'On'}, {'Off'})
        ),
        Interface = maps:get(interface, Artifact),
        ?assertEqual(1, length(maps:get(dictionaries, Interface))),
        ?assertEqual(
            control_dependencies([
                #{module => catena_trait_runtime, version => 1}
            ]),
            maps:get(runtime_dependencies, Artifact)
        )
    end).

control_dependencies(Additional) ->
    lists:usort(Additional ++ [
        #{
            module => catena_effect_runtime,
            version => catena_effect_runtime:version(),
            features => catena_effect_runtime:features()
        },
        #{
            module => catena_resumption_runtime,
            version => catena_resumption_runtime:version(),
            features => catena_resumption_runtime:features()
        }
    ]).

artifact_and_diagnostic_contract_matrix_test() ->
    Source =
        "module BackendArtifactConformance\n"
        "export transform identity\n"
        "transform identity value = value\n",
    {ok, Artifact} = catena_compile:compile_string_to_beam(
        Source,
        #{
            process_imports => false,
            search_paths => ["conformance"],
            codegen_opts => #{file => "backend-conformance.cat"}
        }
    ),
    ?assertEqual(catena_beam_artifact, maps:get(format, Artifact)),
    ?assert(is_binary(maps:get(beam, Artifact))),
    ?assertEqual([], maps:get(warnings, Artifact)),
    ?assert(maps:is_key(origins, maps:get(metadata, Artifact))),
    InvalidCore = cerl:c_module(
        cerl:c_atom('BackendInvalidCore'),
        [cerl:c_fname(run, 0)],
        [],
        [{
            cerl:c_fname(run, 0),
            cerl:c_fun([], cerl:c_var(unbound_name))
        }]
    ),
    {error, Diagnostic} = catena_beam_artifact:validate_core(
        InvalidCore,
        #{
            module => 'BackendInvalidCore',
            source_identity => #{
                kind => file,
                path => "backend-invalid.cat"
            }
        }
    ),
    ?assertEqual(
        core_validation_failed,
        catena_backend_error:category(Diagnostic)
    ),
    [Normalized | _] = maps:get(
        errors,
        catena_backend_error:details(Diagnostic)
    ),
    ?assertEqual(catena_artifact_diagnostic, maps:get(kind, Normalized)),
    ?assert(maps:is_key(otp_detail, Normalized)).

deferred_surfaces_fail_before_artifact_emission_test() ->
    Deferred = [
        {
            'DeferredConformanceTest',
            "module DeferredConformanceTest\n"
            "test \"not emitted\" = 1\n"
        },
        {
            'DeferredConformanceProperty',
            "module DeferredConformanceProperty\n"
            "property \"not emitted\" = forall x : Int . x\n"
        },
        {
            'DeferredConformanceActor',
            "module DeferredConformanceActor\n"
            "actor worker\n"
        },
        {
            'DeferredConformanceProcess',
            "module DeferredConformanceProcess\n"
            "process worker\n"
        }
    ],
    lists:foreach(
        fun({Module, Source}) ->
            ?assertMatch(
                {error, _},
                catena_compile:compile_string_to_beam(Source)
            ),
            ?assertEqual(non_existing, code:which(Module))
        end,
        Deferred
    ).

with_public_module(Source, Assertion) ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(Source),
    Module = maps:get(runtime_module, Artifact),
    unload(Module),
    try
        {module, Module} = code:load_binary(
            Module,
            "backend-conformance-memory",
            maps:get(beam, Artifact)
        ),
        Assertion(Artifact)
    after
        unload(Module)
    end.

with_source_set(Sources, Assertion) ->
    {ok, Result} = catena_module_compile:compile_source_set(Sources, #{}),
    Modules = maps:get(order, Result),
    Artifacts = maps:get(artifacts, Result),
    lists:foreach(
        fun(Module) ->
            unload(Module),
            Artifact = maps:get(Module, Artifacts),
            {module, Module} = code:load_binary(
                Module,
                "backend-conformance-set-memory",
                maps:get(beam, Artifact)
            )
        end,
        Modules
    ),
    try
        Assertion(Result)
    after
        lists:foreach(fun unload/1, lists:reverse(Modules))
    end.

unload(Module) ->
    code:purge(Module),
    code:delete(Module).
