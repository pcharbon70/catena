-module(catena_effect_context_codegen_tests).

-include_lib("eunit/include/eunit.hrl").

effectful_callers_share_one_explicit_context_test() ->
    Source =
        "module EffectContextFlow\n"
        "export transform run\n"
        "effect Reader\n"
        "operation read : Int\n"
        "end\n"
        "transform helper ignored = perform Reader.read()\n"
        "type HelperBoundary = HelperBoundary\n"
        "transform run ignored = handle helper 0 then {\n"
        "  Reader { read -> 42 }\n"
        "}\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    ?assertEqual(
        #{helper => 1, run => 1},
        catena_compilation_unit:effectful_transforms(Unit)
    ),
    with_executable_module(
        Source,
        'EffectContextFlow',
        fun() ->
            ?assertEqual(42, 'EffectContextFlow':run(0))
        end
    ).

effect_context_variables_are_hygienic_test() ->
    Source =
        "module HygienicEffectContext\n"
        "export transform run\n"
        "effect Reader\n"
        "operation read : Int\n"
        "end\n"
        "transform run context = handle "
            "perform Reader.read() then {\n"
        "  Reader { read -> context }\n"
        "}\n",
    with_executable_module(
        Source,
        'HygienicEffectContext',
        fun() ->
            ?assertEqual(73, 'HygienicEffectContext':run(73))
        end
    ).

runtime_lifecycle_cleans_up_after_unhandled_operation_test() ->
    Source =
        "module EffectRuntimeCleanup\n"
        "export transform run\n"
        "effect Missing\n"
        "operation read : Int\n"
        "end\n"
        "transform run ignored = perform Missing.read()\n",
    with_executable_module(
        Source,
        'EffectRuntimeCleanup',
        fun() ->
            ?assertError(
                {no_handler_for_effect, 'Missing', read},
                'EffectRuntimeCleanup':run(0)
            ),
            ?assertNot(catena_effect_system:is_initialized())
        end
    ).

generated_effect_entries_are_context_aware_and_private_test() ->
    Source =
        "module EffectEntryShape\n"
        "export transform run\n"
        "effect Reader\n"
        "operation read : Int\n"
        "end\n"
        "transform run ignored = perform Reader.read()\n",
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    Definitions = cerl:module_defs(CoreModule),
    ?assert(lists:keymember({run, 1}, 1, named_definitions(Definitions))),
    EntryName = catena_codegen_utils:effect_entry_name(run),
    ?assert(
        lists:keymember(
            {EntryName, 2},
            1,
            named_definitions(Definitions)
        )
    ),
    Exports = [
        {cerl:fname_id(Export), cerl:fname_arity(Export)}
        || Export <- cerl:module_exports(CoreModule)
    ],
    ?assert(lists:member({run, 1}, Exports)),
    ?assertNot(lists:member({EntryName, 2}, Exports)).

named_definitions(Definitions) ->
    [
        {{cerl:fname_id(Name), cerl:fname_arity(Name)}, Definition}
        || {Name, Definition} <- Definitions
    ].

with_executable_module(Source, Module, Assertion) ->
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    {ok, Module, Binary, _Warnings} = compile:forms(
        CoreModule,
        [from_core, binary, return_errors, return_warnings]
    ),
    unload(Module),
    try
        {module, Module} = code:load_binary(
            Module,
            atom_to_list(Module) ++ ".core",
            Binary
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
    code:purge(Module),
    code:delete(Module).
