-module(catena_effect_resolution_tests).

-include_lib("eunit/include/eunit.hrl").

validated_unit_retains_resolved_operation_metadata_test() ->
    Source =
        "module EffectInventory\n"
        "effect Console\n"
        "operation emit : String -> Int\n"
        "end\n"
        "transform run : String -> Int / {Console}\n"
        "transform run message = perform Console.emit(message)\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    Operations = catena_compilation_unit:effect_operations(Unit),
    Operation = maps:get({'Console', emit}, Operations),
    ?assertEqual({'Console', emit, 1}, maps:get(identity, Operation)),
    ?assertEqual(1, maps:get(arity, Operation)),
    ?assertMatch(
        [{type_con, 'String', _}],
        maps:get(parameter_types, Operation)
    ),
    ?assertMatch(
        {type_con, 'Int', _},
        maps:get(result_type, Operation)
    ),
    [Use] = catena_compilation_unit:effect_uses(Unit),
    ?assertEqual({'Console', emit, 1}, maps:get(identity, Use)),
    ?assertMatch({location, 6, _}, maps:get(location, Use)).

operation_signature_drives_argument_and_result_inference_test() ->
    ValidSource =
        "module EffectTypes\n"
        "effect Counter\n"
        "operation increment : Int -> Int\n"
        "end\n"
        "transform run value = "
            "perform Counter.increment(value) + 1\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(ValidSource),
    TypedDeclarations = catena_compilation_unit:typed_declarations(Unit),
    ?assertMatch(
        [
            {effect_decl, 'Counter', _, _},
            {typed_transform, run, {tfun, _, {tcon, int}, _}, _, _}
        ],
        TypedDeclarations
    ),
    InvalidSource =
        "module BadEffectTypes\n"
        "effect Counter\n"
        "operation increment : Int -> Int\n"
        "end\n"
        "transform run = perform Counter.increment(\"wrong\")\n",
    ?assertMatch(
        {error, {type_error, run, [_ | _]}},
        catena_compile:compile_string_to_unit(InvalidSource)
    ).

performed_operations_must_resolve_exactly_test_() ->
    [
        {"unknown effect",
            ?_test(assert_resolution_error(
                "module UnknownEffect\n"
                "transform run = perform Missing.read()\n",
                unknown_effect
            ))},
        {"unknown operation",
            ?_test(assert_resolution_error(
                "module UnknownOperation\n"
                "effect Console\n"
                "operation read : Int\n"
                "end\n"
                "transform run = perform Console.write()\n",
                unknown_operation
            ))},
        {"operation arity",
            ?_test(assert_resolution_error(
                "module BadOperationArity\n"
                "effect Console\n"
                "operation emit : String -> Int\n"
                "end\n"
                "transform run = perform Console.emit()\n",
                operation_arity_mismatch
            ))},
        {"missing operation signature",
            ?_test(assert_resolution_error(
                "module MissingSignature\n"
                "effect Console\n"
                "operation emit\n"
                "end\n"
                "transform run = 1\n",
                missing_operation_signature
            ))},
        {"duplicate operation",
            ?_test(assert_resolution_error(
                "module DuplicateOperation\n"
                "effect Console\n"
                "operation read : Int\n"
                "operation read : Int\n"
                "end\n"
                "transform run = 1\n",
                duplicate_operation
            ))}
    ].

declared_effect_obligations_remain_enforced_test() ->
    Source =
        "module EffectObligation\n"
        "effect Console\n"
        "operation read : Int\n"
        "end\n"
        "transform run : Int / {}\n"
        "transform run = perform Console.read()\n",
    ?assertMatch(
        {error, {effect_mismatch, run, _, _}},
        catena_compile:compile_string_to_unit(Source)
    ).

assert_resolution_error(Source, ExpectedReason) ->
    Result = catena_compile:compile_string_to_unit(Source),
    ?assertMatch(
        {error, {effect_resolution_error, ExpectedReason, #{}}},
        Result
    ),
    {error, {effect_resolution_error, ExpectedReason, Details}} = Result,
    ?assertEqual(effect_resolution, maps:get(stage, Details)),
    ?assertNotEqual(undefined, maps:get(location, Details)).
