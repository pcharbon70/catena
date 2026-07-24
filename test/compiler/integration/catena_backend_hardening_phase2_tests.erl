-module(catena_backend_hardening_phase2_tests).

-include_lib("eunit/include/eunit.hrl").

validated_unit_carries_complete_backend_authority_test() ->
    Source =
        "module PhaseTwoInventory\n"
        "export transform identity\n"
        "import Prelude\n"
        "type LocalChoice a = LocalNone | LocalSome a\n"
        "transform identity value = value\n",
    Opts = #{
        codegen_opts => #{file => "phase_two_inventory.cat"},
        source_identity => #{
            kind => string,
            name => "phase-two-integration"
        }
    },
    {ok, Unit} = catena_compile:compile_string_to_unit(Source, Opts),
    ?assert(catena_compilation_unit:is_compilation_unit(Unit)),
    ?assertEqual('PhaseTwoInventory',
        catena_compilation_unit:module_name(Unit)),
    ?assertMatch(
        {module, 'PhaseTwoInventory', _, _, _, _},
        catena_compilation_unit:normalized_ast(Unit)
    ),
    ?assertMatch(
        {typed_module, 'PhaseTwoInventory', _, _},
        catena_compilation_unit:typed_module(Unit)
    ),
    ?assertEqual(Opts, catena_compilation_unit:options(Unit)),
    ?assertEqual(
        maps:get(source_identity, Opts),
        catena_compilation_unit:source_identity(Unit)
    ),
    ?assertMatch(
        [{import, 'Prelude', all, false, undefined, _}],
        catena_compilation_unit:imports(Unit)
    ),
    ?assertEqual(
        [{export_transform, identity}],
        catena_compilation_unit:exports(Unit)
    ),
    assert_symbol(Unit, transform, identity, 1),
    assert_symbol(Unit, constructor, 'LocalNone', 0),
    assert_symbol(Unit, constructor, 'LocalSome', 1),
    assert_symbol(Unit, import, 'Prelude', undefined),
    assert_disposition(Unit, declaration, transform, lowered),
    assert_disposition(Unit, declaration, type, erased_static),
    assert_disposition(Unit, import, import, unsupported),
    assert_disposition(Unit, export, export_transform, lowered),
    Locations = catena_compilation_unit:locations(Unit),
    ?assertNotEqual(undefined, maps:get(module, Locations)),
    ?assertNotEqual([], maps:get(imports, Locations)),
    ?assertNotEqual([], maps:get(declarations, Locations)),
    ?assertNotEqual([], maps:get(clauses, Locations)),
    ?assertNotEqual([], maps:get(patterns, Locations)),
    ?assertNotEqual([], maps:get(expressions, Locations)).

typed_and_core_apis_share_one_frontend_result_test() ->
    Source =
        "module SharedFrontend\n"
        "export transform identity\n"
        "type Choice a = Empty | Value a\n"
        "transform identity value = value\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    ?assertEqual(
        {ok, catena_compilation_unit:typed_module(Unit)},
        catena_compile:compile_string(Source)
    ),
    ?assertEqual(
        catena_codegen_module:generate_validated_module(Unit),
        catena_compile:compile_string_to_core(Source)
    ).

frontend_failures_cannot_produce_validated_units_test_() ->
    [
        {"lexical validation",
            ?_assertMatch(
                {error, {lex_error, _}},
                catena_compile:compile_string_to_unit(
                    "module Broken\ntransform broken = \"unterminated\n"
                )
            )},
        {"syntax validation",
            ?_assertMatch(
                {error, _},
                catena_compile:compile_string_to_unit(
                    "module Broken\ntransform broken = + +\n"
                )
            )},
        {"semantic validation",
            ?_assertMatch(
                {error, {impure_guard, guarded, _, _}},
                catena_compile:compile_string_to_unit(
                    "module Guarded\n"
                    "transform guarded x when "
                    "perform IO.read() = x\n"
                )
            )},
        {"import validation",
            ?_assertMatch(
                {error, {module_not_found, _, _}},
                catena_compile:compile_string_to_unit(
                    "module Imports\n"
                    "import DefinitelyMissingPhaseTwoModule\n"
                    "transform ok = 1\n"
                )
            )},
        {"kind and trait validation",
            ?_assertMatch(
                {error, {kind_errors, [_ | _]}},
                catena_compile:compile_string_to_unit(
                    "module BadKinds\n"
                    "trait BadMapper f where\n"
                    "badmap : (a -> b) -> f a -> f b\n"
                    "end\n"
                    "instance BadMapper Int where\n"
                    "end\n"
                    "transform ok = 1\n"
                )
            )},
        {"type validation",
            ?_assertMatch(
                {error, {type_error, broken, _}},
                catena_compile:compile_string_to_unit(
                    "module Broken\n"
                    "transform broken value = missing\n"
                )
            )},
        {"effect validation",
            ?_assertMatch(
                {error, {effect_mismatch, bad, _, _}},
                catena_compile:compile_string_to_unit(
                    "module Effects\n"
                    "effect IO\n"
                    "operation read : Int\n"
                    "end\n"
                    "transform bad : Int / {}\n"
                    "transform bad = perform IO.read()\n"
                )
            )}
    ].

unclassified_unit_cannot_enter_codegen_test() ->
    Location = {location, 4, 2},
    Declaration =
        {transform_decl,
            answer,
            undefined,
            [
                {transform_clause,
                    [],
                    undefined,
                    {literal, integer, 42, Location},
                    Location}
            ],
            Location},
    AST =
        {module,
            unclassified,
            [{export_transform, answer}],
            [],
            [Declaration],
            Location},
    Typed = {typed_module, unclassified, [Declaration], #{}},
    ValidationState = maps:from_list([
        {Stage, passed}
        || Stage <- catena_compilation_unit:validated_stages()
    ]),
    {ok, Unit} = catena_compilation_unit:new(
        AST,
        Typed,
        #{
            validation_state => ValidationState,
            options => #{},
            source_identity => #{kind => integration_test}
        }
    ),
    Result = catena_codegen_module:generate_validated_module(Unit),
    ?assertMatch(
        {error,
            {backend_error, invalid_declaration_disposition, #{}}},
        Result
    ).

static_metadata_is_retained_before_core_emission_test() ->
    Source =
        "module StaticMetadata\n"
        "export transform run\n"
        "type Maybe a = None | Some a\n"
        "effect Console\n"
        "operation print : String -> Unit\n"
        "end\n"
        "transform run = 42\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    TypeDisposition = find_disposition(Unit, declaration, type),
    EffectDisposition = find_disposition(Unit, declaration, effect),
    ?assert(maps:is_key(representation, TypeDisposition)),
    ?assert(maps:is_key(representation, EffectDisposition)),
    {ok, CoreModule} = catena_codegen_module:generate_validated_module(Unit),
    ?assertEqual(1, length(cerl:module_defs(CoreModule))).

deferred_runtime_declarations_fail_with_source_context_test_() ->
    [
        {"test declaration",
            ?_test(assert_deferred_declaration_rejected(
                "module DeferredTest\n"
                "test \"not emitted\" = 1\n",
                application_test_artifact_deferred
            ))},
        {"property declaration",
            ?_test(assert_deferred_declaration_rejected(
                "module DeferredProperty\n"
                "property \"not emitted\" = "
                "forall x : Int . x\n",
                application_property_artifact_deferred
            ))}
    ].

exported_signature_requires_runtime_implementation_test() ->
    Source =
        "module RequiredImplementation\n"
        "export transform required\n"
        "transform required : Int\n",
    Result = catena_compile:compile_string_to_core(Source),
    ?assertMatch(
        {error,
            {backend_error, missing_transform_implementation, #{}}},
        Result
    ),
    {error, Diagnostic} = Result,
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(required, maps:get(source_identity, Details)),
    ?assertMatch({location, 3, _}, maps:get(location, Details)).

phase_one_executable_slice_survives_validated_unit_test() ->
    Source =
        "module PhaseTwoExecutable\n"
        "export transform value_or_zero\n"
        "type Maybe a = None | Some a\n"
        "transform value_or_zero None = 0\n"
        "transform value_or_zero Some(value) = value\n",
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    {ok, 'PhaseTwoExecutable', Binary, _Warnings} =
        compile_core(CoreModule),
    unload('PhaseTwoExecutable'),
    try
        {module, 'PhaseTwoExecutable'} = code:load_binary(
            'PhaseTwoExecutable',
            "PhaseTwoExecutable.core",
            Binary
        ),
        ?assertEqual(
            0,
            'PhaseTwoExecutable':value_or_zero({'None'})
        ),
        ?assertEqual(
            9,
            'PhaseTwoExecutable':value_or_zero({'Some', 9})
        )
    after
        unload('PhaseTwoExecutable')
    end.

assert_deferred_declaration_rejected(Source, Reason) ->
    Result = catena_compile:compile_string_to_core(Source),
    ?assertMatch(
        {error,
            {backend_error, invalid_declaration_disposition, #{}}},
        Result
    ),
    {error, Diagnostic} = Result,
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(Reason, maps:get(reason, Details)),
    ?assertNotEqual(undefined, maps:get(location, Details)).

assert_symbol(Unit, Kind, Name, Arity) ->
    ?assert(lists:any(
        fun(Symbol) ->
            maps:get(kind, Symbol) =:= Kind andalso
                maps:get(name, Symbol) =:= Name andalso
                maps:get(arity, Symbol) =:= Arity
        end,
        catena_compilation_unit:symbols(Unit)
    )).

assert_disposition(Unit, Subject, Kind, Class) ->
    Disposition = find_disposition(Unit, Subject, Kind),
    ?assertEqual(Class, maps:get(disposition, Disposition)).

find_disposition(Unit, Subject, Kind) ->
    hd([
        Disposition
        || Disposition <- catena_compilation_unit:dispositions(Unit),
           maps:get(subject, Disposition) =:= Subject,
           maps:get(kind, Disposition) =:= Kind
    ]).

compile_core(CoreModule) ->
    compile:forms(
        CoreModule,
        [from_core, binary, return_errors, return_warnings]
    ).

unload(Module) ->
    code:purge(Module),
    code:delete(Module).
