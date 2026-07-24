-module(catena_compilation_unit_tests).

-include_lib("eunit/include/eunit.hrl").

validated_unit_retains_frontend_authority_test() ->
    {AST, TypedModule} = validated_artifacts(),
    {ok, Unit} = catena_compilation_unit:new(
        AST,
        TypedModule,
        metadata()
    ),
    ?assert(catena_compilation_unit:is_compilation_unit(Unit)),
    ?assertEqual('Inventory', catena_compilation_unit:module_name(Unit)),
    ?assertEqual(AST, catena_compilation_unit:normalized_ast(Unit)),
    ?assertEqual(TypedModule, catena_compilation_unit:typed_module(Unit)),
    ?assertEqual(#{source => test}, catena_compilation_unit:options(Unit)),
    ?assertEqual(
        #{kind => string, name => "inventory test"},
        catena_compilation_unit:source_identity(Unit)
    ),
    ?assertEqual(validation_state(),
        catena_compilation_unit:validation_state(Unit)),
    ?assertEqual(3, length(catena_call_resolution:callables(
        catena_compilation_unit:callables(Unit)
    ))),
    ?assertEqual(3, length(catena_compilation_unit:dispositions(Unit))),
    ?assert(lists:all(
        fun(Disposition) ->
            maps:get(disposition, Disposition) =:= unclassified
        end,
        catena_compilation_unit:dispositions(Unit)
    )).

unit_collects_symbols_and_locations_test() ->
    {AST, TypedModule} = validated_artifacts(),
    {ok, Unit} = catena_compilation_unit:new(AST, TypedModule, metadata()),
    Symbols = catena_compilation_unit:symbols(Unit),
    ?assert(has_symbol(Symbols, transform, run, 1, location(3, 1))),
    ?assert(has_symbol(Symbols, type, 'Maybe', undefined, location(5, 1))),
    ?assert(has_symbol(Symbols, constructor, 'Some', 1, location(5, 21))),
    ?assert(has_symbol(Symbols, effect, 'Console', undefined, location(6, 1))),
    ?assert(has_symbol(
        Symbols,
        effect_operation,
        print,
        0,
        location(7, 3)
    )),
    ?assert(has_symbol(
        Symbols,
        import,
        'Prelude',
        undefined,
        location(2, 1)
    )),
    Callables = catena_call_resolution:callables(
        catena_compilation_unit:callables(Unit)
    ),
    ?assert(has_callable(Callables, transform, run, 1, undefined)),
    ?assert(has_callable(Callables, constructor, 'None', 0, 'Maybe')),
    ?assert(has_callable(Callables, constructor, 'Some', 1, 'Maybe')),
    Locations = catena_compilation_unit:locations(Unit),
    ?assertEqual(location(1, 1), maps:get(module, Locations)),
    ?assertEqual([location(2, 1)], maps:get(imports, Locations)),
    ?assert(lists:member(location(4, 1), maps:get(clauses, Locations))),
    ?assert(lists:member(location(4, 15), maps:get(patterns, Locations))),
    ?assert(lists:member(
        location(4, 19),
        maps:get(expressions, Locations)
    )).

missing_validation_prevents_unit_construction_test() ->
    {AST, TypedModule} = validated_artifacts(),
    InvalidState = maps:remove(effects, validation_state()),
    ?assertEqual(
        {error,
            {invalid_compilation_unit,
                {missing_validations, [effects]}}},
        catena_compilation_unit:new(
            AST,
            TypedModule,
            (metadata())#{validation_state := InvalidState}
        )
    ).

raw_ast_cannot_construct_a_unit_test() ->
    {AST, _TypedModule} = validated_artifacts(),
    ?assertEqual(
        {error, {invalid_compilation_unit, invalid_frontend_artifacts}},
        catena_compilation_unit:new(AST, AST, metadata())
    ).

mismatched_typed_module_is_rejected_test() ->
    {AST, {typed_module, _, TypedDeclarations, Env}} =
        validated_artifacts(),
    ?assertEqual(
        {error,
            {invalid_compilation_unit,
                {module_identity_mismatch, 'Inventory', 'Other'}}},
        catena_compilation_unit:new(
            AST,
            {typed_module, 'Other', TypedDeclarations, Env},
            metadata()
        )
    ).

validated_artifacts() ->
    ModuleLocation = location(1, 1),
    Import = {import, 'Prelude', all, false, undefined, location(2, 1)},
    TransformLocation = location(3, 1),
    ClauseLocation = location(4, 1),
    PatternLocation = location(4, 15),
    BodyLocation = location(4, 19),
    Transform =
        {transform_decl,
            run,
            undefined,
            [
                {transform_clause,
                    [{pat_var, value, PatternLocation}],
                    undefined,
                    {var, value, BodyLocation},
                    ClauseLocation}
            ],
            TransformLocation},
    TypeDeclaration =
        {type_decl,
            'Maybe',
            [a],
            [
                {constructor, 'None', [], location(5, 16)},
                {constructor, 'Some', [{type_var, a}], location(5, 21)}
            ],
            [],
            location(5, 1)},
    EffectDeclaration =
        {effect_decl,
            'Console',
            [
                {effect_operation,
                    print,
                    {type_con, 'Unit'},
                    location(7, 3)}
            ],
            location(6, 1)},
    Declarations = [Transform, TypeDeclaration, EffectDeclaration],
    AST =
        {module,
            'Inventory',
            [{export_transform, run}],
            [Import],
            Declarations,
            ModuleLocation},
    TypedDeclarations = [
        {typed_transform, run, {tfun, value, value, pure},
            element(4, Transform), TransformLocation},
        TypeDeclaration,
        EffectDeclaration
    ],
    {AST, {typed_module, 'Inventory', TypedDeclarations, #{run => typed}}}.

metadata() ->
    #{
        validation_state => validation_state(),
        options => #{source => test},
        source_identity => #{kind => string, name => "inventory test"}
    }.

validation_state() ->
    maps:from_list([
        {Stage, passed}
        || Stage <- catena_compilation_unit:validated_stages()
    ]).

has_symbol(Symbols, Kind, Name, Arity, Location) ->
    lists:any(
        fun(Symbol) ->
            maps:get(kind, Symbol) =:= Kind andalso
                maps:get(name, Symbol) =:= Name andalso
                maps:get(arity, Symbol) =:= Arity andalso
                maps:get(location, Symbol) =:= Location
        end,
        Symbols
    ).

has_callable(Callables, Kind, Name, Arity, Owner) ->
    lists:any(
        fun(Callable) ->
            maps:get(kind, Callable) =:= Kind andalso
                maps:get(name, Callable) =:= Name andalso
                maps:get(arity, Callable) =:= Arity andalso
                maps:get(owner, Callable, undefined) =:= Owner
        end,
        Callables
    ).

location(Line, Column) ->
    {location, Line, Column}.
