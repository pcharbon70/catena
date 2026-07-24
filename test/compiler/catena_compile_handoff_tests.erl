-module(catena_compile_handoff_tests).

-include_lib("eunit/include/eunit.hrl").

typed_api_adapts_validated_unit_test() ->
    Source =
        "module StableTyped\n"
        "transform identity value = value\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    ?assertEqual(
        {ok, catena_compilation_unit:typed_module(Unit)},
        catena_compile:compile_string(Source)
    ).

validated_unit_contains_canonical_frontend_results_test() ->
    Source =
        "module UnitInventory\n"
        "export transform identity\n"
        "type Maybe a = None | Some a\n"
        "transform identity value = value\n",
    Opts = #{
        process_imports => false,
        codegen_opts => #{version => "phase-2"}
    },
    {ok, Unit} = catena_compile:compile_string_to_unit(Source, Opts),
    ?assert(catena_compilation_unit:is_compilation_unit(Unit)),
    ?assertEqual('UnitInventory',
        catena_compilation_unit:module_name(Unit)),
    ?assertEqual(Opts, catena_compilation_unit:options(Unit)),
    ?assertEqual(
        #{kind => string, name => "nofile"},
        catena_compilation_unit:source_identity(Unit)
    ),
    ?assertEqual(
        maps:from_list([
            {Stage, passed}
            || Stage <- catena_compilation_unit:validated_stages()
        ]),
        catena_compilation_unit:validation_state(Unit)
    ),
    ?assertEqual(
        [{export_transform, identity}],
        catena_compilation_unit:exports(Unit)
    ),
    ?assert(has_symbol(
        catena_compilation_unit:symbols(Unit),
        transform,
        identity,
        1
    )),
    ?assert(has_symbol(
        catena_compilation_unit:symbols(Unit),
        constructor,
        'Some',
        1
    )).

public_core_api_uses_validated_handoff_test() ->
    Source =
        "module ValidatedCore\n"
        "export transform answer\n"
        "transform answer = 42\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    ?assertEqual(
        catena_codegen_module:generate_validated_module(Unit),
        catena_compile:compile_string_to_core(Source)
    ),
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    ?assertEqual(
        'ValidatedCore',
        cerl:atom_val(cerl:module_name(CoreModule))
    ).

unchecked_ast_is_rejected_by_validated_entry_test() ->
    RawAST =
        {module, raw, [], [], [], {location, 1, 1}},
    ?assertEqual(
        {error, {invalid_compilation_unit, unchecked_backend_input}},
        catena_codegen_module:generate_validated_module(RawAST)
    ).

file_compilation_preserves_source_identity_and_codegen_file_test() ->
    Source =
        "module FileIdentity\n"
        "export transform identity\n"
        "transform identity value = value\n",
    Path = filename:join(
        "/tmp",
        "catena_phase2_handoff_" ++
            integer_to_list(erlang:unique_integer([positive])) ++
            ".cat"
    ),
    try
        ok = file:write_file(Path, Source),
        {ok, CoreModule} = catena_compile:compile_file_to_core(Path),
        FileAttributes = [
            cerl:concrete(Value)
            || {Key, Value} <- cerl:module_attrs(CoreModule),
               cerl:atom_val(Key) =:= file
        ],
        ?assertEqual([Path], FileAttributes)
    after
        file:delete(Path)
    end.

has_symbol(Symbols, Kind, Name, Arity) ->
    lists:any(
        fun(Symbol) ->
            maps:get(kind, Symbol) =:= Kind andalso
                maps:get(name, Symbol) =:= Name andalso
                maps:get(arity, Symbol) =:= Arity
        end,
        Symbols
    ).
