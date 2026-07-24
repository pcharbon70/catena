-module(catena_codegen_pure_expr_tests).

-include_lib("eunit/include/eunit.hrl").

literals_collections_and_records_execute_test() ->
    Source =
        "module PureValues\n"
        "export transform run\n"
        "transform run = "
            "({answer: 42}.answer, [1, 2], (3.5, \"ok\"))\n",
    with_executable_module(
        Source,
        'PureValues',
        fun() ->
            ?assertEqual(
                {42, [1, 2], {3.5, "ok"}},
                'PureValues':run()
            )
        end
    ).

primitive_operators_execute_test() ->
    Source =
        "module PrimitiveOperators\n"
        "export transform run\n"
        "transform run = "
            "(8 + 2, 8 - 2, 3 * 4, 8 / 2, "
            "2 < 3, 3 > 2, 2 <= 2, 3 >= 2, "
            "2 == 2, 2 /= 3, true && false, true || false)\n",
    with_executable_module(
        Source,
        'PrimitiveOperators',
        fun() ->
            ?assertEqual(
                {
                    10,
                    6,
                    12,
                    4.0,
                    true,
                    true,
                    true,
                    true,
                    true,
                    true,
                    false,
                    true
                },
                'PrimitiveOperators':run()
            )
        end
    ).

list_append_and_cons_execute_test() ->
    Source =
        "module ListOperators\n"
        "export transform run\n"
        "transform run = [1, 2] ++ (3 :: [4])\n",
    with_executable_module(
        Source,
        'ListOperators',
        fun() ->
            ?assertEqual([1, 2, 3, 4], 'ListOperators':run())
        end
    ).

pipe_to_named_multi_argument_transform_executes_test() ->
    Source =
        "module PipeOperator\n"
        "export transform run\n"
        "transform add : Int -> Int -> Int\n"
        "transform add left right = left + right\n"
        "type PipeBoundary = PipeBoundary\n"
        "transform run = 40 |> add 2\n",
    with_executable_module(
        Source,
        'PipeOperator',
        fun() ->
            ?assertEqual(42, 'PipeOperator':run())
        end
    ).

record_access_preserves_missing_field_behavior_test() ->
    Source =
        "module RecordAccess\n"
        "export transform read_answer\n"
        "transform read_answer record = record.answer\n",
    with_executable_module(
        Source,
        'RecordAccess',
        fun() ->
            ?assertEqual(42, 'RecordAccess':read_answer(#{answer => 42})),
            ?assertError(
                {badkey, answer},
                'RecordAccess':read_answer(#{})
            )
        end
    ).

desugared_library_operator_reaches_local_call_resolution_test() ->
    Source =
        "module DesugaredCombine\n"
        "export transform run\n"
        "transform combine : Int -> Int -> Int\n"
        "transform combine left right = left + right\n"
        "type CombineBoundary = CombineBoundary\n"
        "transform run = 20 <> 22\n",
    with_executable_module(
        Source,
        'DesugaredCombine',
        fun() ->
            ?assertEqual(42, 'DesugaredCombine':run())
        end
    ).

do_notation_reaches_local_chain_resolution_test() ->
    Source =
        "module DesugaredDo\n"
        "export transform run\n"
        "transform chain : Int -> Int -> Int\n"
        "transform chain function value = value + 1\n"
        "type ChainBoundary = ChainBoundary\n"
        "transform run = do { value <- 41; value + 1 }\n",
    with_executable_module(
        Source,
        'DesugaredDo',
        fun() ->
            ?assertEqual(42, 'DesugaredDo':run())
        end
    ).

unknown_pure_forms_fail_with_backend_diagnostics_test() ->
    Location = {location, 9, 4},
    ExpressionDiagnostic = capture_backend_throw(
        fun() ->
            catena_codegen_lower:lower_expr(
                {unknown_pure_expression, Location}
            )
        end
    ),
    assert_unsupported(
        ExpressionDiagnostic,
        expression_lowering,
        expression,
        Location
    ),
    OperatorDiagnostic = capture_backend_throw(
        fun() ->
            catena_codegen_lower:lower_operator(
                unknown_pure_operator
            )
        end
    ),
    assert_unsupported(
        OperatorDiagnostic,
        operator_lowering,
        operator,
        undefined
    ),
    LiteralDiagnostic = capture_backend_throw(
        fun() ->
            catena_codegen_expr:translate_expr(
                {literal, unsupported, value, Location},
                catena_codegen_utils:new_state()
            )
        end
    ),
    assert_unsupported(
        LiteralDiagnostic,
        expression_translation,
        literal,
        Location
    ).

with_executable_module(Source, Module, Assertion) ->
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    {ok, Module, Binary, _Warnings} = compile_core(CoreModule),
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

compile_core(CoreModule) ->
    compile:forms(
        CoreModule,
        [from_core, binary, return_errors, return_warnings]
    ).

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

assert_unsupported(Diagnostic, Stage, Construct, Location) ->
    ?assertEqual(
        unsupported_backend_construct,
        catena_backend_error:category(Diagnostic)
    ),
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(Stage, maps:get(stage, Details)),
    ?assertEqual(Construct, maps:get(construct, Details)),
    ?assertEqual(Location, maps:get(location, Details)).
