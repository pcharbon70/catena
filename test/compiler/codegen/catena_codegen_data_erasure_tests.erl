-module(catena_codegen_data_erasure_tests).

-include_lib("eunit/include/eunit.hrl").

emitted_values_match_their_runtime_patterns_test() ->
    Source =
        "module DataRoundTrip\n"
        "export transform run\n"
        "export transform read_answer\n"
        "type PairValue a = Pair a a\n"
        "transform consume "
            "(Pair(left right), [head tail], (first, second), "
            "{answer: answer}) = "
            "(left, right, head, tail, first, second, answer)\n"
        "type RunBoundary = RunBoundary\n"
        "transform run = "
            "consume (Pair 1 2, [3, 4], (5, 6), {answer: 7})\n"
        "type RecordBoundary = RecordBoundary\n"
        "transform read_answer {answer: value} = value\n"
        "transform read_answer _ = 0\n",
    with_source_module(
        Source,
        'DataRoundTrip',
        fun() ->
            ?assertEqual(
                {1, 2, 3, 4, 5, 6, 7},
                'DataRoundTrip':run()
            ),
            ?assertEqual(
                42,
                'DataRoundTrip':read_answer(
                    #{answer => 42, extra => preserved}
                )
            ),
            ?assertEqual(0, 'DataRoundTrip':read_answer(#{}))
        end
    ).

nested_static_annotations_are_erased_exhaustively_test() ->
    Location = location(),
    TypedExpression =
        {typed_expr,
            {lambda,
                [
                    {pat_typed_var,
                        value,
                        {tcon, 'Int'},
                        Location}
                ],
                {tuple_expr,
                    [
                        {typed_expr,
                            {var, value, Location},
                            {tcon, 'Int'},
                            Location},
                        {list_expr,
                            [
                                {type_ascription,
                                    {literal, integer, 1, Location},
                                    {tcon, 'Int'},
                                    Location}
                            ],
                            Location}
                    ],
                    Location},
                Location},
            {type_fun, {tcon, 'Int'}, {tcon, 'Result'}},
            Location},
    ?assertEqual(
        {lambda,
            [{pat_var, value, Location}],
            {tuple_expr,
                [
                    {var, value, Location},
                    {list_expr,
                        [{literal, integer, 1, Location}],
                        Location}
                ],
                Location},
            Location},
        catena_codegen_erase:erase_expr(TypedExpression)
    ).

classified_static_declarations_have_explicit_erasure_rules_test() ->
    Location = location(),
    ?assertEqual(
        erased,
        catena_codegen_erase:erase_decl(
            {type_decl, 'Maybe', [], [], [], Location}
        )
    ),
    ?assertEqual(
        erased,
        catena_codegen_erase:erase_decl(
            {effect_decl, 'IO', [], Location}
        )
    ),
    ?assertEqual(erased, catena_codegen_erase:erase_decl(erased)).

handler_erasure_preserves_runtime_shape_test() ->
    Location = location(),
    Expression =
        {handle_expr,
            {typed_expr,
                {literal, integer, 1, Location},
                {tcon, 'Int'},
                Location},
            [
                {handler_clause,
                    'IO',
                    [
                        {operation_case,
                            read,
                            [
                                {pat_typed_var,
                                    path,
                                    {tcon, 'Text'},
                                    Location}
                            ],
                            {typed_expr,
                                {var, path, Location},
                                {tcon, 'Text'},
                                Location},
                            Location}
                    ],
                    Location}
            ],
            Location},
    ?assertEqual(
        {handle_expr,
            {literal, integer, 1, Location},
            [
                {handler_clause,
                    'IO',
                    [
                        {operation_case,
                            read,
                            [{pat_var, path, Location}],
                            {var, path, Location},
                            Location}
                    ],
                    Location}
            ],
            Location},
        catena_codegen_erase:erase_expr(Expression)
    ).

unknown_erasure_forms_fail_closed_test() ->
    Location = location(),
    assert_erasure_error(
        fun() ->
            catena_codegen_erase:erase_decl(
                {unknown_declaration, Location}
            )
        end,
        invalid_declaration_disposition,
        declaration
    ),
    assert_erasure_error(
        fun() ->
            catena_codegen_erase:erase_expr(
                {unknown_expression, Location}
            )
        end,
        unsupported_backend_construct,
        expression
    ),
    assert_erasure_error(
        fun() ->
            catena_codegen_erase:erase_pattern(
                {unknown_pattern, Location}
            )
        end,
        unsupported_backend_construct,
        pattern
    ),
    assert_erasure_error(
        fun() ->
            catena_codegen_erase:erase_expr(
                {match_expr,
                    {literal, integer, 1, Location},
                    [{unknown_clause, Location}],
                    Location}
            )
        end,
        unsupported_backend_construct,
        clause
    ),
    assert_erasure_error(
        fun() ->
            catena_codegen_erase:erase_expr(
                {record_expr,
                    [{answer, {literal, integer, 42, Location}}, malformed],
                    Location}
            )
        end,
        unsupported_backend_construct,
        record_fields
    ).

deferred_dispatch_declarations_cannot_bypass_disposition_test() ->
    Location = location(),
    Trait =
        {trait_decl,
            'Show',
            [a],
            [],
            [],
            Location},
    Module =
        {module,
            rejected_erasure_dispatch,
            [],
            [],
            [Trait],
            Location},
    ?assertMatch(
        {error,
            {backend_error,
                invalid_declaration_disposition,
                #{
                    module := rejected_erasure_dispatch,
                    stage := type_erasure,
                    declaration := Trait
                }}},
        catena_codegen_module:generate_module(Module)
    ).

with_source_module(Source, Module, Assertion) ->
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
        unload(Module)
    end.

unload(Module) ->
    code:purge(Module),
    code:delete(Module).

assert_erasure_error(Fun, Category, Construct) ->
    Diagnostic = capture_backend_throw(Fun),
    ?assertEqual(Category, catena_backend_error:category(Diagnostic)),
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(type_erasure, maps:get(stage, Details)),
    ?assertEqual(Construct, maps:get(construct, Details)).

capture_backend_throw(Fun) ->
    try
        Fun(),
        error(expected_backend_diagnostic)
    catch
        throw:{backend_error, _, _} = Diagnostic ->
            Diagnostic
    end.

location() ->
    {location, 1, 1}.
