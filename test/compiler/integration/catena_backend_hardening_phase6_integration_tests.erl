-module(catena_backend_hardening_phase6_integration_tests).

-include_lib("eunit/include/eunit.hrl").

multi_module_trait_and_import_workflow_test() ->
    Sources = #{
        'TraitLibrary' => trait_library_source(),
        'TraitApplication' => trait_application_source()
    },
    with_compiled_set(Sources, fun(Result) ->
        ?assertEqual(
            ['TraitLibrary', 'TraitApplication'],
            maps:get(order, Result)
        ),
        ?assertEqual(
            {'Box', 42},
            'TraitApplication':mapped({'Box', 41})
        ),
        ?assertEqual(
            {'Box', 42},
            'TraitApplication':applied({'Box', 41})
        ),
        ?assertEqual(
            {'Box', 42},
            'TraitApplication':chained({'Box', 41})
        ),
        ?assertEqual(
            {'Box', 42},
            'TraitApplication':doChained({'Box', 41})
        ),
        ?assertEqual(
            {'Box', 41},
            'TraitApplication':joined({'Box', {'Box', 41}})
        ),
        ?assertEqual(
            true,
            'TraitApplication':flagEquals({'On'}, {'Off'})
        ),
        ?assertEqual(
            false,
            'TraitApplication':tokenEquals({'Red'}, {'Red'})
        ),
        ?assertEqual(
            true,
            'TraitApplication':tokenNotEquals({'Red'}, {'Red'})
        ),
        ?assertEqual(
            {'Stage', 0},
            'TraitApplication':composed(
                {'Stage', 2},
                {'Stage', 1}
            )
        ),
        ?assertEqual(
            {'Stage', 0},
            'TraitApplication':composedBefore(
                {'Stage', 2},
                {'Stage', 1}
            )
        ),
        ?assertEqual(
            {'Stage', 0},
            'TraitApplication':parallelStages(
                {'Stage', 2},
                {'Stage', 1}
            )
        ),
        ?assertEqual(
            {'Stage', 0},
            'TraitApplication':splitStages(
                {'Stage', 2},
                {'Stage', 1}
            )
        ),
        ?assertEqual(
            {'Stage', 0},
            'TraitApplication':firstStage({'Stage', 1})
        ),
        LibraryInterface = maps:get(
            interface,
            maps:get(
                'TraitLibrary',
                maps:get(artifacts, Result)
            )
        ),
        ?assertEqual(
            8,
            length(maps:get(dictionaries, LibraryInterface))
        )
    end).

dependency_failures_are_source_oriented_test() ->
    Missing = #{
        'MissingConsumer' =>
            "module MissingConsumer\n"
            "import NotPresent\n"
            "transform run value = value\n"
    },
    ?assertMatch(
        {error, #{
            reason := missing_dependency,
            module := 'MissingConsumer',
            dependency := 'NotPresent',
            location := {location, 2, _}
        }},
        catena_module_compile:compile_source_set(Missing, #{})
    ),
    Cycle = #{
        'CycleA' =>
            "module CycleA\n"
            "import CycleB\n"
            "transform run value = value\n",
        'CycleB' =>
            "module CycleB\n"
            "import CycleA\n"
            "transform run value = value\n"
    },
    ?assertMatch(
        {error, #{
            reason := dependency_cycle,
            cycle := [First, _Second, First],
            locations := [{location, 2, _}, {location, 2, _}]
        }},
        catena_module_compile:compile_source_set(Cycle, #{})
    ).

trait_library_source() ->
    "module TraitLibrary\n"
    "type Box a = Box a\n"
    "type Flag = On | Off\n"
    "type Token = Red | Blue\n"
    "type Stage a b = Stage Int\n"
    "trait Comparable a where\n"
    "  equals : a -> a -> Bool\n"
    "end\n"
    "trait Mapper f where\n"
    "  map : (a -> b) -> f a -> f b\n"
    "end\n"
    "trait Applicator f extend Mapper f where\n"
    "  pure : a -> f a,\n"
    "  apply : f (a -> b) -> f a -> f b\n"
    "end\n"
    "trait Chainable m extend Mapper m where\n"
    "  chain : (a -> m b) -> m a -> m b\n"
    "end\n"
    "trait Pipeline m extend Applicator m, Chainable m where\n"
    "  join : m (m a) -> m a\n"
    "end\n"
    "trait System arr where\n"
    "  identity : arr a a,\n"
    "  compose : arr b c -> arr a b -> arr a c\n"
    "end\n"
    "trait Flow arr extend System arr where\n"
    "  lift : (a -> b) -> arr a b,\n"
    "  first : arr a b -> arr (a, c) (b, c),\n"
    "  parallel : arr a b -> arr c d -> arr (a, c) (b, d),\n"
    "  split : arr a b -> arr a c -> arr a (b, c)\n"
    "end\n"
    "instance Comparable Flag where\n"
    "  transform equals left right = true\n"
    "end\n"
    "instance Comparable Token where\n"
    "  transform equals left right = false\n"
    "end\n"
    "instance Mapper Box where\n"
    "  transform map f value = match value of\n"
    "    | Box x -> Box (f x)\n"
    "  end\n"
    "end\n"
    "instance Applicator Box where\n"
    "  transform pure value = Box value,\n"
    "  transform apply wrappedFunction wrappedValue = "
        "match wrappedFunction of\n"
    "    | Box f -> map f wrappedValue\n"
    "  end\n"
    "end\n"
    "instance Chainable Box where\n"
    "  transform chain f value = match value of\n"
    "    | Box x -> f x\n"
    "  end\n"
    "end\n"
    "instance Pipeline Box where\n"
    "  transform join value = match value of\n"
    "    | Box inner -> inner\n"
    "  end\n"
    "end\n"
    "instance System Stage where\n"
    "  transform identity = Stage 0,\n"
    "  transform compose right left = Stage 0\n"
    "end\n"
    "instance Flow Stage where\n"
    "  transform lift f = Stage 0,\n"
    "  transform first flow = Stage 0,\n"
    "  transform parallel left right = Stage 0,\n"
    "  transform split left right = Stage 0\n"
    "end\n".

trait_application_source() ->
    "module TraitApplication\n"
    "export transform mapped\n"
    "export transform applied\n"
    "export transform chained\n"
    "export transform doChained\n"
    "export transform joined\n"
    "export transform flagEquals\n"
    "export transform tokenEquals\n"
    "export transform tokenNotEquals\n"
    "export transform composed\n"
    "export transform composedBefore\n"
    "export transform parallelStages\n"
    "export transform splitStages\n"
    "export transform firstStage\n"
    "import TraitLibrary\n"
    "transform increment value = value + 1\n"
    "transform boxedIncrement value = Box (increment value)\n"
    "transform mapped value = increment <$> value\n"
    "transform applied value = (Box increment) <*> value\n"
    "transform chained value = value >>= boxedIncrement\n"
    "transform doChained value = do { x <- value; Box (increment x) }\n"
    "transform joined value = join value\n"
    "transform flagEquals left right = left === right\n"
    "transform tokenEquals left right = left === right\n"
    "transform tokenNotEquals left right = left !== right\n"
    "transform composed right left = left >>> right\n"
    "transform composedBefore right left = right <<< left\n"
    "transform parallelStages left right = left *** right\n"
    "transform splitStages left right = left &&& right\n"
    "transform firstStage value = first value\n".

with_compiled_set(Sources, Assertion) ->
    {ok, Result} = catena_module_compile:compile_source_set(Sources, #{}),
    Artifacts = maps:get(artifacts, Result),
    Modules = maps:get(order, Result),
    lists:foreach(
        fun(Module) ->
            Artifact = maps:get(Module, Artifacts),
            {module, Module} = code:load_binary(
                Module,
                "phase6-integration-memory",
                maps:get(beam, Artifact)
            )
        end,
        Modules
    ),
    try
        Assertion(Result)
    after
        lists:foreach(
            fun(Module) ->
                code:purge(Module),
                code:delete(Module)
            end,
            lists:reverse(Modules)
        )
    end.
