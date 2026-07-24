-module(catena_backend_hardening_phase6_import_tests).

-include_lib("eunit/include/eunit.hrl").

unqualified_import_executes_test() ->
    Sources = #{
        'Provider' => provider_source('Provider'),
        'Consumer' => consumer_source(
            "import Provider\n",
            "inc value"
        )
    },
    with_compiled_set(Sources, fun() ->
        ?assertEqual(42, 'Consumer':run(41))
    end).

qualified_alias_import_executes_test() ->
    Sources = #{
        'Provider' => provider_source('Provider'),
        'Consumer' => consumer_source(
            "import qualified Provider as P\n",
            "P.inc value"
        )
    },
    with_compiled_set(Sources, fun() ->
        ?assertEqual(42, 'Consumer':run(41))
    end).

selective_lowercase_import_executes_test() ->
    Sources = #{
        'Provider' => provider_source('Provider'),
        'Consumer' => consumer_source(
            "import Provider (inc)\n",
            "inc value"
        )
    },
    with_compiled_set(Sources, fun() ->
        ?assertEqual(42, 'Consumer':run(41))
    end).

dotted_import_executes_test() ->
    Sources = #{
        'Math.Basic' => provider_source('Math.Basic'),
        'Consumer' => consumer_source(
            "import Math.Basic\n",
            "inc value"
        )
    },
    with_compiled_set(Sources, fun() ->
        ?assertEqual(42, 'Consumer':run(41))
    end).

imported_transform_as_higher_order_value_test() ->
    Sources = #{
        'Provider' => provider_source('Provider'),
        'Consumer' =>
            "module Consumer\n"
            "export transform run\n"
            "import Provider\n"
            "transform invoke f value = f value\n"
            "transform run value = invoke inc value\n"
    },
    with_compiled_set(Sources, fun() ->
        ?assertEqual(42, 'Consumer':run(41))
    end).

local_transform_shadows_import_test() ->
    Sources = #{
        'Provider' => provider_source('Provider'),
        'Consumer' =>
            "module Consumer\n"
            "export transform run\n"
            "import Provider\n"
            "transform inc value = value + 10\n"
            "transform run value = inc value\n"
    },
    with_compiled_set(Sources, fun() ->
        ?assertEqual(51, 'Consumer':run(41))
    end).

private_and_ambiguous_imports_fail_test() ->
    PrivateProvider =
        "module PrivateProvider\n"
        "export transform visible\n"
        "transform visible value = value\n"
        "transform hidden value = value\n",
    PrivateSources = #{
        'PrivateProvider' => PrivateProvider,
        'Consumer' => consumer_source(
            "import PrivateProvider (hidden)\n",
            "hidden value"
        )
    },
    ?assertMatch(
        {error, #{reason := symbol_not_exported, symbols := [hidden]}},
        catena_module_compile:compile_source_set(PrivateSources, #{})
    ),
    AmbiguousSources = #{
        'First' => named_provider_source('First', 1),
        'Second' => named_provider_source('Second', 2),
        'Consumer' => consumer_source(
            "import First\nimport Second\n",
            "adjust value"
        )
    },
    ?assertMatch(
        {error, {backend_error, ambiguous_call, _}},
        catena_module_compile:compile_source_set(AmbiguousSources, #{})
    ).

wrong_imported_arity_fails_test() ->
    Sources = #{
        'Provider' => provider_source('Provider'),
        'Consumer' => consumer_source(
            "import Provider\n",
            "inc value value"
        )
    },
    ?assertMatch(
        {error, _},
        catena_module_compile:compile_source_set(Sources, #{})
    ).

provider_source(Module) ->
    lists:flatten(io_lib:format(
        "module ~s\n"
        "export transform inc\n"
        "transform inc : Int -> Int\n"
        "transform inc value = value + 1\n",
        [atom_to_list(Module)]
    )).

named_provider_source(Module, Amount) ->
    lists:flatten(io_lib:format(
        "module ~s\n"
        "export transform adjust\n"
        "transform adjust : Int -> Int\n"
        "transform adjust value = value + ~p\n",
        [atom_to_list(Module), Amount]
    )).

consumer_source(Import, Expression) ->
    "module Consumer\n"
    "export transform run\n" ++
    Import ++
    "transform run : Int -> Int\n"
    "transform run value = " ++ Expression ++ "\n".

with_compiled_set(Sources, Assertion) ->
    {ok, Result} = catena_module_compile:compile_source_set(Sources, #{}),
    Artifacts = maps:get(artifacts, Result),
    Modules = maps:get(order, Result),
    lists:foreach(
        fun(Module) ->
            Artifact = maps:get(Module, Artifacts),
            {module, Module} = code:load_binary(
                Module,
                "phase6-memory",
                maps:get(beam, Artifact)
            )
        end,
        Modules
    ),
    try
        Assertion()
    after
        lists:foreach(
            fun(Module) ->
                code:purge(Module),
                code:delete(Module)
            end,
            lists:reverse(Modules)
        )
    end.
