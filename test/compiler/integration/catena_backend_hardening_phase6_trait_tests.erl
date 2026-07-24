-module(catena_backend_hardening_phase6_trait_tests).

-include_lib("eunit/include/eunit.hrl").

local_dictionary_and_default_method_execute_test() ->
    Source =
        "module LocalTraits\n"
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
        "transform run left right = notEquals left right\n",
    with_compiled_set(#{'LocalTraits' => Source}, fun(Result) ->
        ?assertEqual(
            false,
            'LocalTraits':run({'On'}, {'Off'})
        ),
        Interface = artifact_interface('LocalTraits', Result),
        [Dictionary] = maps:get(dictionaries, Interface),
        ?assertEqual('Comparable', maps:get(trait, Dictionary)),
        ?assertEqual(
            {'LocalTraits', '$catena_dictionary', 'Comparable',
                [{type_con, 'Flag'}]},
            maps:get(identity, Dictionary)
        )
    end).

inherited_dictionary_executes_parent_method_test() ->
    Source =
        "module InheritedTraits\n"
        "export transform run\n"
        "type Flag = On | Off\n"
        "trait Parent a where\n"
        "  base : a -> Bool\n"
        "end\n"
        "trait Child a extend Parent a where\n"
        "  child : a -> Bool\n"
        "end\n"
        "instance Parent Flag where\n"
        "  transform base value = true\n"
        "end\n"
        "instance Child Flag where\n"
        "  transform child value = false\n"
        "end\n"
        "transform run value = base value\n",
    with_compiled_set(#{'InheritedTraits' => Source}, fun(Result) ->
        ?assertEqual(true, 'InheritedTraits':run({'On'})),
        Interface = artifact_interface('InheritedTraits', Result),
        Child = hd([
            Dictionary
            || Dictionary <- maps:get(dictionaries, Interface),
               maps:get(trait, Dictionary) =:= 'Child'
        ]),
        ?assertEqual(1, length(maps:get(parents, Child)))
    end).

imported_dictionary_executes_test() ->
    Provider =
        "module TraitProvider\n"
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
        "module TraitConsumer\n"
        "export transform run\n"
        "import TraitProvider\n"
        "transform run left right = equals left right\n",
    with_compiled_set(
        #{
            'TraitProvider' => Provider,
            'TraitConsumer' => Consumer
        },
        fun(_Result) ->
            ?assertEqual(
                true,
                'TraitConsumer':run({'On'}, {'Off'})
            )
        end
    ).

desugared_comparable_operator_uses_dictionary_test() ->
    Source =
        "module ComparableOperator\n"
        "export transform run\n"
        "type Flag = On | Off\n"
        "trait Comparable a where\n"
        "  equals : a -> a -> Bool\n"
        "end\n"
        "instance Comparable Flag where\n"
        "  transform equals left right = false\n"
        "end\n"
        "transform run left right = left === right\n",
    with_compiled_set(#{'ComparableOperator' => Source}, fun(_Result) ->
        ?assertEqual(
            false,
            'ComparableOperator':run({'On'}, {'On'})
        )
    end).

missing_duplicate_overlapping_and_orphan_instances_fail_test() ->
    Missing =
        "module MissingMethod\n"
        "type Flag = On | Off\n"
        "trait Comparable a where\n"
        "  equals : a -> a -> Bool\n"
        "end\n"
        "instance Comparable Flag where\n"
        "end\n",
    ?assertMatch(
        {error, {trait_validation_error,
            {missing_instance_method, 'Comparable', equals, _}}},
        catena_module_compile:compile_source_set(
            #{'MissingMethod' => Missing},
            #{}
        )
    ),
    WrongType =
        "module WrongMethodType\n"
        "type Flag = On | Off\n"
        "trait Comparable a where\n"
        "  equals : a -> a -> Bool\n"
        "end\n"
        "instance Comparable Flag where\n"
        "  transform equals left right = 42\n"
        "end\n",
    ?assertMatch(
        {error, {trait_validation_error,
            {instance_method_type_mismatch, 'Comparable', equals,
                _, _, _, _}}},
        catena_module_compile:compile_source_set(
            #{'WrongMethodType' => WrongType},
            #{}
        )
    ),
    Overlap =
        "module Overlap\n"
        "type Flag = On | Off\n"
        "trait Comparable a where\n"
        "  equals : a -> a -> Bool\n"
        "end\n"
        "instance Comparable Flag where\n"
        "  transform equals left right = true\n"
        "end\n"
        "instance Comparable Flag where\n"
        "  transform equals left right = false\n"
        "end\n",
    ?assertMatch(
        {error, {incoherent_instances, 'Comparable', _, _}},
        catena_module_compile:compile_source_set(
            #{'Overlap' => Overlap},
            #{}
        )
    ),
    TraitOwner =
        "module TraitOwner\n"
        "export trait Comparable\n"
        "trait Comparable a where\n"
        "  equals : a -> a -> Bool\n"
        "end\n",
    TypeOwner =
        "module TypeOwner\n"
        "export type Flag\n"
        "type Flag = On | Off\n",
    Orphan =
        "module Orphan\n"
        "import TraitOwner\n"
        "import TypeOwner\n"
        "instance Comparable Flag where\n"
        "  transform equals left right = true\n"
        "end\n",
    ?assertMatch(
        {error, {trait_validation_error,
            {orphan_instance, 'Orphan', 'Comparable', _, _}}},
        catena_module_compile:compile_source_set(
            #{
                'TraitOwner' => TraitOwner,
                'TypeOwner' => TypeOwner,
                'Orphan' => Orphan
            },
            #{}
        )
    ).

artifact_interface(Module, Result) ->
    Artifact = maps:get(Module, maps:get(artifacts, Result)),
    maps:get(interface, Artifact).

with_compiled_set(Sources, Assertion) ->
    {ok, Result} = catena_module_compile:compile_source_set(Sources, #{}),
    Artifacts = maps:get(artifacts, Result),
    Modules = maps:get(order, Result),
    lists:foreach(
        fun(Module) ->
            Artifact = maps:get(Module, Artifacts),
            {module, Module} = code:load_binary(
                Module,
                "phase6-trait-memory",
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
