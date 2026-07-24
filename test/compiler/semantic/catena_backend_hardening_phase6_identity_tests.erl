-module(catena_backend_hardening_phase6_identity_tests).

-include_lib("eunit/include/eunit.hrl").

simple_and_dotted_identity_test() ->
    {ok, Simple} = catena_module_identity:normalize('Prelude'),
    {ok, Dotted} = catena_module_identity:normalize('Effect.IO'),
    ?assertEqual('Prelude', maps:get(runtime_module, Simple)),
    ?assertEqual(
        [<<"Effect">>, <<"IO">>],
        maps:get(components, Dotted)
    ),
    ?assertNotEqual(
        maps:get(runtime_module, Dotted),
        'Effect_IO'
    ).

identity_validation_rejects_duplicates_and_empty_components_test() ->
    ?assertEqual(
        {error, {duplicate_source_module, 'A'}},
        catena_module_identity:validate_unique(['A', 'A'])
    ),
    ?assertEqual(
        {error, {invalid_module_identity, 'Effect..IO'}},
        catena_module_identity:normalize('Effect..IO')
    ).

dependency_plan_is_deterministic_test() ->
    Loc = {1, 1},
    Modules = #{
        'C' => module_ast('C', []),
        'A' => module_ast('A', []),
        'B' => module_ast(
            'B',
            [{import, 'A', all, false, undefined, Loc}]
        )
    },
    {ok, Plan} = catena_module_linkage:plan(Modules),
    ?assertEqual(['A', 'B', 'C'], maps:get(order, Plan)).

dependency_plan_reports_missing_module_location_test() ->
    Loc = {7, 4},
    Modules = #{
        'Consumer' => module_ast(
            'Consumer',
            [{import, 'Missing', all, false, undefined, Loc}]
        )
    },
    ?assertMatch(
        {error, #{
            reason := missing_dependency,
            module := 'Consumer',
            dependency := 'Missing',
            location := Loc
        }},
        catena_module_linkage:plan(Modules)
    ).

dependency_plan_reports_cycle_locations_test() ->
    LocAB = {2, 1},
    LocBA = {3, 1},
    Modules = #{
        'A' => module_ast(
            'A',
            [{import, 'B', all, false, undefined, LocAB}]
        ),
        'B' => module_ast(
            'B',
            [{import, 'A', all, false, undefined, LocBA}]
        )
    },
    {error, Diagnostic} = catena_module_linkage:plan(Modules),
    ?assertEqual(dependency_cycle, maps:get(reason, Diagnostic)),
    ?assertEqual(['A', 'B', 'A'], maps:get(cycle, Diagnostic)),
    ?assertEqual([LocAB, LocBA], maps:get(locations, Diagnostic)).

validated_unit_publishes_executable_interface_test() ->
    Source =
        "module IdentityFixture\n"
        "export transform visible\n"
        "export type Box\n"
        "type Box = Wrapped Int\n"
        "transform visible value = value\n"
        "transform hidden value = value\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(
        Source,
        #{process_imports => false}
    ),
    Interface = catena_compilation_unit:interface(Unit),
    ?assert(catena_module_interface:is_interface(Interface)),
    ?assertEqual(
        'IdentityFixture',
        catena_compilation_unit:runtime_module(Unit)
    ),
    {ok, Visible} = catena_module_interface:find_export(
        transform,
        visible,
        Interface
    ),
    ?assertEqual(1, maps:get(arity, Visible)),
    {ok, Box} = catena_module_interface:find_export(
        constructor,
        'Wrapped',
        Interface
    ),
    ?assertEqual(1, maps:get(arity, Box)),
    ?assertMatch(
        {error, {symbol_not_exported, 'IdentityFixture', transform, hidden}},
        catena_module_interface:find_export(transform, hidden, Interface)
    ).

module_ast(Name, Imports) ->
    {module, Name, [], Imports, [], {1, 1}}.
