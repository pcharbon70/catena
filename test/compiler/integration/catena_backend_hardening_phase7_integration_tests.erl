-module(catena_backend_hardening_phase7_integration_tests).

-include_lib("eunit/include/eunit.hrl").

public_single_module_story_executes_and_cleans_up_test() ->
    Source =
        "module PhaseSevenSingle\n"
        "export transform run\n"
        "type Nat = Zero | Succ Nat\n"
        "transform copy : Nat -> Nat\n"
        "transform copy Zero = Zero\n"
        "transform copy Succ(rest) = Succ (copy rest)\n"
        "type CopyBoundary = CopyBoundary\n"
        "transform apply function value = function value\n"
        "type ApplyBoundary = ApplyBoundary\n"
        "transform run value = "
            "let increment = fn item -> item + 1 in "
            "(copy (Succ (Succ Zero)), apply increment value)\n",
    {ok, Artifact} =
        catena_compile:compile_string_to_beam(Source),
    with_artifacts(
        ['PhaseSevenSingle'],
        #{'PhaseSevenSingle' => Artifact},
        fun() ->
            ?assertEqual(
                {
                    {'Succ', {'Succ', {'Zero'}}},
                    42
                },
                'PhaseSevenSingle':run(41)
            )
        end
    ),
    ?assertEqual(non_existing, code:which('PhaseSevenSingle')).

public_multi_module_trait_story_executes_in_order_test() ->
    Provider =
        "module PhaseSevenProvider\n"
        "export trait Comparable\n"
        "export type Flag\n"
        "type Flag = On | Off\n"
        "trait Comparable a where\n"
        "  equals : a -> a -> Bool\n"
        "end\n"
        "instance Comparable Flag where\n"
        "  transform equals left right = true\n"
        "end\n",
    Consumer =
        "module PhaseSevenConsumer\n"
        "export transform run\n"
        "import PhaseSevenProvider\n"
        "transform run left right = equals left right\n",
    {ok, Result} = catena_compile:compile_source_set_to_beam(
        #{
            'PhaseSevenProvider' => Provider,
            'PhaseSevenConsumer' => Consumer
        }
    ),
    Order = maps:get(order, Result),
    Artifacts = maps:get(artifacts, Result),
    ?assertEqual(
        ['PhaseSevenProvider', 'PhaseSevenConsumer'],
        Order
    ),
    ProviderArtifact = maps:get('PhaseSevenProvider', Artifacts),
    ?assertNot(maps:is_key(unit, ProviderArtifact)),
    with_artifacts(Order, Artifacts, fun() ->
        ?assertEqual(
            true,
            'PhaseSevenConsumer':run({'On'}, {'Off'})
        )
    end),
    lists:foreach(
        fun(Module) ->
            ?assertEqual(non_existing, code:which(Module))
        end,
        Order
    ).

public_effect_story_cleans_runtime_state_test() ->
    Source =
        "module PhaseSevenEffect\n"
        "export transform run\n"
        "effect Answer\n"
        "operation get : Int\n"
        "end\n"
        "transform run ignored = "
            "handle perform Answer.get() then {\n"
        "  Answer { get -> 42 }\n"
        "}\n",
    {ok, Artifact} =
        catena_compile:compile_string_to_beam(Source),
    with_artifacts(
        ['PhaseSevenEffect'],
        #{'PhaseSevenEffect' => Artifact},
        fun() ->
            ?assertEqual(42, 'PhaseSevenEffect':run(0)),
            ?assertNot(catena_effect_system:is_initialized())
        end
    ),
    ?assertNot(catena_effect_system:is_initialized()).

negative_public_boundaries_leave_no_partial_state_test() ->
    InvalidSources = [
        {
            'PhaseSevenUnresolved',
            "module PhaseSevenUnresolved\n"
            "export transform run\n"
            "transform run value = missing value\n"
        },
        {
            'PhaseSevenDeferred',
            "module PhaseSevenDeferred\n"
            "test \"not emitted\" = 1\n"
        },
        {
            'PhaseSevenBadConstructor',
            "module PhaseSevenBadConstructor\n"
            "export transform run\n"
            "type Pair = Pair Int Int\n"
            "transform run = Pair 1\n"
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
        InvalidSources
    ),
    FailingSet = #{
        'PhaseSevenPartialProvider' =>
            "module PhaseSevenPartialProvider\n"
            "export transform value\n"
            "transform value = 42\n",
        'PhaseSevenPartialConsumer' =>
            "module PhaseSevenPartialConsumer\n"
            "import PhaseSevenPartialProvider\n"
            "transform broken value = missing value\n"
    },
    ?assertMatch(
        {error, _},
        catena_compile:compile_source_set_to_beam(FailingSet)
    ),
    ?assertEqual(
        non_existing,
        code:which('PhaseSevenPartialProvider')
    ),
    ?assertEqual(
        non_existing,
        code:which('PhaseSevenPartialConsumer')
    ),
    ?assertNot(catena_effect_system:is_initialized()).

with_artifacts(Order, Artifacts, Assertion) ->
    lists:foreach(
        fun(Module) ->
            unload(Module),
            Artifact = maps:get(Module, Artifacts),
            {module, Module} = code:load_binary(
                Module,
                "phase7-integration-memory",
                maps:get(beam, Artifact)
            )
        end,
        Order
    ),
    try
        Assertion()
    after
        lists:foreach(fun unload/1, lists:reverse(Order))
    end.

unload(Module) ->
    code:purge(Module),
    code:delete(Module).
