-module(catena_backend_hardening_phase1_tests).

-include_lib("eunit/include/eunit.hrl").

arithmetic_source_reaches_executable_beam_test() ->
    Source =
        "module HardenedArithmetic\n"
        "export transform add_one\n"
        "transform add_one x = x + 1\n",
    with_executable_module(
        Source,
        'HardenedArithmetic',
        fun() ->
            ?assertEqual(42, 'HardenedArithmetic':add_one(41))
        end
    ).

constructor_patterns_reach_executable_beam_test() ->
    Source =
        "module HardenedMaybe\n"
        "export transform value_or_zero\n"
        "type Maybe a = None | Some a\n"
        "transform value_or_zero None = 0\n"
        "transform value_or_zero Some(x) = x\n",
    with_executable_module(
        Source,
        'HardenedMaybe',
        fun() ->
            ?assertEqual(0, 'HardenedMaybe':value_or_zero({'None'})),
            ?assertEqual(7, 'HardenedMaybe':value_or_zero({'Some', 7}))
        end
    ).

lexer_failure_is_preserved_test() ->
    ?assertMatch(
        {error, {lex_error, _}},
        catena_compile:compile_string_to_core(
            "module Broken\ntransform broken = \"unterminated\n"
        )
    ).

parser_failure_is_preserved_test() ->
    ?assertMatch(
        {error, {_, catena_parser, _}},
        catena_compile:compile_string_to_core(
            "module Broken\ntransform broken = + +\n"
        )
    ).

semantic_failure_is_preserved_test() ->
    Source =
        "module Guarded\n"
        "export transform guarded\n"
        "transform guarded x when perform IO.read() = x\n",
    ?assertMatch(
        {error, {impure_guard, guarded, {effect_set, ['IO']}, _}},
        catena_compile:compile_string_to_core(Source)
    ).

kind_failure_is_preserved_test() ->
    Source =
        "module BadKinds\n"
        "trait BadMapper f where\n"
        "badmap : (a -> b) -> f a -> f b\n"
        "end\n"
        "instance BadMapper Int where\n"
        "end\n"
        "transform ok = 1\n",
    ?assertMatch(
        {error, {kind_errors, [_ | _]}},
        catena_compile:compile_string_to_core(Source)
    ).

import_failure_is_preserved_test() ->
    Source =
        "module Imports\n"
        "import DefinitelyMissingBackendModule\n"
        "transform ok = 1\n",
    ?assertMatch(
        {error,
            {module_not_found, 'DefinitelyMissingBackendModule', _}},
        catena_compile:compile_string_to_core(Source)
    ).

type_failure_is_preserved_test() ->
    Source =
        "module Broken\n"
        "export transform broken\n"
        "transform broken x = missing\n",
    ?assertMatch(
        {error, {type_error, broken, [{unbound_variable, missing}]}},
        catena_compile:compile_string_to_core(Source)
    ).

effect_failure_is_preserved_test() ->
    Source =
        "module Effects\n"
        "effect IO\n"
        "operation read : Int\n"
        "end\n"
        "transform bad : Int / {}\n"
        "transform bad = perform IO.read()\n",
    ?assertMatch(
        {error, {effect_mismatch, bad, _, "Effects not satisfied"}},
        catena_compile:compile_string_to_core(Source)
    ).

test_declaration_is_rejected_by_public_core_api_test() ->
    Source =
        "module DeferredTest\n"
        "test \"not an application artifact\" = 1\n",
    assert_backend_rejection(
        catena_compile:compile_string_to_core(Source),
        invalid_declaration_disposition,
        declaration
    ).

property_declaration_is_rejected_by_public_core_api_test() ->
    Source =
        "module DeferredProperty\n"
        "property \"not an application artifact\" = forall x : Int . x\n",
    assert_backend_rejection(
        catena_compile:compile_string_to_core(Source),
        invalid_declaration_disposition,
        declaration
    ).

unknown_expression_is_rejected_before_core_success_test() ->
    assert_backend_rejection(
        catena_codegen_module:generate_module(
            backend_module({unknown_expression, location()})
        ),
        unsupported_backend_construct,
        expression
    ).

unknown_operator_is_rejected_before_core_success_test() ->
    Body =
        {binary_op,
            unknown_operator,
            {literal, integer, 1, location()},
            {literal, integer, 2, location()},
            location()},
    assert_backend_rejection(
        catena_codegen_module:generate_module(backend_module(Body)),
        unsupported_backend_construct,
        operator
    ).

unknown_pattern_is_rejected_before_core_success_test() ->
    Body =
        {match_expr,
            {literal, integer, 1, location()},
            [
                {clause,
                    [{unknown_pattern, location()}],
                    [],
                    {literal, integer, 2, location()}}
            ],
            location()},
    assert_backend_rejection(
        catena_codegen_module:generate_module(backend_module(Body)),
        unsupported_backend_construct,
        pattern
    ).

complex_let_pattern_is_rejected_before_core_success_test() ->
    Pattern =
        {pat_constructor,
            'Some',
            [{pat_var, value, location()}],
            location()},
    Body =
        {let_expr,
            [
                {Pattern,
                    {constructor,
                        'Some',
                        [{literal, integer, 1, location()}],
                        location()}}
            ],
            {literal, integer, 2, location()},
            location()},
    assert_backend_rejection(
        catena_codegen_module:generate_module(backend_module(Body)),
        unsupported_backend_construct,
        binding_pattern
    ).

with_executable_module(Source, Module, Assertion) ->
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    {ok, Module, Binary, _Warnings} = compile_core(CoreModule),
    unload(Module),
    try
        {module, Module} =
            code:load_binary(Module, atom_to_list(Module) ++ ".core", Binary),
        Assertion()
    after
        unload(Module)
    end.

compile_core(CoreModule) ->
    compile:forms(
        CoreModule,
        [from_core, binary, return_errors, return_warnings]
    ).

unload(Module) ->
    code:purge(Module),
    code:delete(Module).

assert_backend_rejection(Result, Category, Construct) ->
    ?assertMatch({error, {backend_error, _, #{}}}, Result),
    {error, Diagnostic} = Result,
    ?assertEqual(Category, catena_backend_error:category(Diagnostic)),
    ?assertEqual(
        Construct,
        maps:get(construct, catena_backend_error:details(Diagnostic))
    ).

backend_module(Body) ->
    {module,
        'RejectedBackendInput',
        [],
        [],
        [{transform, rejected, [], Body, location()}],
        location()}.

location() ->
    {location, 11, 7}.
