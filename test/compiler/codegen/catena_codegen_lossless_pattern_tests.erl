-module(catena_codegen_lossless_pattern_tests).

-include_lib("eunit/include/eunit.hrl").

structural_transform_parameters_execute_test() ->
    Source =
        "module StructuralPatterns\n"
        "export transform unpack\n"
        "type Maybe a = None | Some a\n"
        "transform unpack (Some value, [first second], {answer: answer}) = "
            "(value, first, second, answer)\n",
    with_source_module(
        Source,
        'StructuralPatterns',
        fun() ->
            ?assertEqual(
                {7, 10, 20, 42},
                'StructuralPatterns':unpack(
                    {{'Some', 7}, [10, 20], #{answer => 42}}
                )
            )
        end
    ).

literal_list_cons_and_clause_order_execute_test() ->
    Source =
        "module OrderedPatterns\n"
        "export transform classify\n"
        "transform classify [1 2] = 12\n"
        "transform classify head :: _ = head\n"
        "transform classify [] = 0\n",
    with_source_module(
        Source,
        'OrderedPatterns',
        fun() ->
            ?assertEqual(12, 'OrderedPatterns':classify([1, 2])),
            ?assertEqual(9, 'OrderedPatterns':classify([9, 8])),
            ?assertEqual(0, 'OrderedPatterns':classify([]))
        end
    ).

as_pattern_preserves_alias_and_inner_bindings_test() ->
    Source =
        "module AliasPattern\n"
        "export transform inspect\n"
        "type Maybe a = None | Some a\n"
        "transform inspect Some(value) as whole = (whole, value)\n",
    with_source_module(
        Source,
        'AliasPattern',
        fun() ->
            ?assertEqual(
                {{'Some', 42}, 42},
                'AliasPattern':inspect({'Some', 42})
            )
        end
    ).

or_patterns_guards_and_fallthrough_execute_test() ->
    Source =
        "module GuardedAlternatives\n"
        "export transform unwrap\n"
        "type Either a = Left a | Right a\n"
        "transform unwrap value = match value of\n"
        "  | Left(x) | Right(x) when x > 0, 10 / x > 1 -> x\n"
        "  | Left(_) | Right(_) -> 0\n"
        "end\n",
    with_source_module(
        Source,
        'GuardedAlternatives',
        fun() ->
            ?assertEqual(5, 'GuardedAlternatives':unwrap({'Left', 5})),
            ?assertEqual(2, 'GuardedAlternatives':unwrap({'Right', 2})),
            ?assertEqual(0, 'GuardedAlternatives':unwrap({'Left', 0})),
            ?assertEqual(0, 'GuardedAlternatives':unwrap({'Right', 20}))
        end
    ).

nested_and_multi_position_or_patterns_execute_test() ->
    Location = location(),
    Module =
        {module,
            lossless_or_patterns,
            [{unwrap, 1}, {pair_choice, 2}],
            [],
            [
                type_decl('Either', [
                    constructor('Left', 1),
                    constructor('Right', 1)
                ]),
                type_decl('Boxed', [constructor('Box', 1)]),
                type_decl('FirstChoice', [
                    constructor('A', 0),
                    constructor('B', 0)
                ]),
                type_decl('SecondChoice', [
                    constructor('C', 0),
                    constructor('D', 0)
                ]),
                {transform_decl,
                    unwrap,
                    undefined,
                    [
                        {transform_clause,
                            [
                                {pat_constructor,
                                    'Box',
                                    [
                                        {pat_or,
                                            [
                                                constructor_pattern(
                                                    'Left',
                                                    [{pat_var, value, Location}]
                                                ),
                                                constructor_pattern(
                                                    'Right',
                                                    [{pat_var, value, Location}]
                                                )
                                            ],
                                            Location}
                                    ],
                                    Location}
                            ],
                            undefined,
                            {var, value, Location},
                            Location}
                    ],
                    Location},
                {transform_decl,
                    pair_choice,
                    undefined,
                    [
                        {transform_clause,
                            [
                                or_pattern(['A', 'B']),
                                or_pattern(['C', 'D'])
                            ],
                            undefined,
                            {literal, integer, 42, Location},
                            Location}
                    ],
                    Location}
            ],
            Location},
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    with_core_module(
        CoreModule,
        lossless_or_patterns,
        fun() ->
            ?assertEqual(
                7,
                lossless_or_patterns:unwrap({'Box', {'Left', 7}})
            ),
            ?assertEqual(
                8,
                lossless_or_patterns:unwrap({'Box', {'Right', 8}})
            ),
            lists:foreach(
                fun({First, Second}) ->
                    ?assertEqual(
                        42,
                        lossless_or_patterns:pair_choice(
                            {First},
                            {Second}
                        )
                    )
                end,
                [{'A', 'C'}, {'A', 'D'}, {'B', 'C'}, {'B', 'D'}]
            )
        end
    ).

constructor_pattern_arity_is_validated_test() ->
    Location = location(),
    Module =
        {module,
            invalid_pattern_arity,
            [{unwrap, 1}],
            [],
            [
                type_decl('PairValue', [constructor('Pair', 2)]),
                {transform_decl,
                    unwrap,
                    undefined,
                    [
                        {transform_clause,
                            [constructor_pattern(
                                'Pair',
                                [{pat_var, value, Location}]
                            )],
                            undefined,
                            {var, value, Location},
                            Location}
                    ],
                    Location}
            ],
            Location},
    ?assertMatch(
        {error, {backend_error, arity_mismatch, #{}}},
        catena_codegen_module:generate_module(Module)
    ).

invalid_normalized_patterns_fail_closed_test() ->
    Location = location(),
    UnknownResult = capture_backend_throw(
        fun() ->
            catena_codegen_lower:lower_pattern(
                {unknown_normalized_pattern, Location}
            )
        end
    ),
    assert_backend_error(
        UnknownResult,
        unsupported_backend_construct,
        pattern_lowering,
        pattern
    ),
    MismatchedOr =
        {pat_or,
            [
                constructor_pattern('Left', [{pat_var, value, Location}]),
                constructor_pattern('Right', [])
            ],
            Location},
    MismatchResult = capture_backend_throw(
        fun() ->
            catena_codegen_pattern:compile_clauses(
                [
                    {clause,
                        [MismatchedOr],
                        [],
                        {literal, integer, 0, Location}}
                ],
                catena_codegen_utils:new_state(),
                #{}
            )
        end
    ),
    assert_backend_error(
        MismatchResult,
        unsupported_backend_construct,
        pattern_compilation,
        or_pattern_bindings
    ).

type_decl(Name, Constructors) ->
    {type_decl, Name, [], Constructors, [], location()}.

constructor(Name, Arity) ->
    {constructor,
        Name,
        lists:duplicate(Arity, {type_con, 'Int'}),
        location()}.

constructor_pattern(Name, Arguments) ->
    {pat_constructor, Name, Arguments, location()}.

or_pattern(Names) ->
    {pat_or, [constructor_pattern(Name, []) || Name <- Names], location()}.

with_source_module(Source, Module, Assertion) ->
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    with_core_module(CoreModule, Module, Assertion).

with_core_module(CoreModule, Module, Assertion) ->
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
        unload(Module)
    end.

unload(Module) ->
    code:purge(Module),
    code:delete(Module).

capture_backend_throw(Fun) ->
    try
        Fun(),
        error(expected_backend_diagnostic)
    catch
        throw:{backend_error, _, _} = Diagnostic ->
            Diagnostic
    end.

assert_backend_error(Diagnostic, Category, Stage, Construct) ->
    ?assertEqual(Category, catena_backend_error:category(Diagnostic)),
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(Stage, maps:get(stage, Details)),
    ?assertEqual(Construct, maps:get(construct, Details)).

location() ->
    {location, 1, 1}.
