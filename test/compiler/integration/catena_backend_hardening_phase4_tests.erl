-module(catena_backend_hardening_phase4_tests).

-include_lib("eunit/include/eunit.hrl").

pure_expression_and_operator_matrix_executes_test() ->
    Source =
        "module PhaseFourExpressions\n"
        "export transform run\n"
        "transform add : Int -> Int -> Int\n"
        "transform add left right = left + right\n"
        "type AddBoundary = AddBoundary\n"
        "transform combine : Int -> Int -> Int\n"
        "transform combine left right = left + right\n"
        "type CombineBoundary = CombineBoundary\n"
        "transform combined ignored = 20 <> 22\n"
        "type CombinedBoundary = CombinedBoundary\n"
        "transform chain : (Int -> Int) -> Int -> Int\n"
        "transform chain function value = value + 1\n"
        "type ChainBoundary = ChainBoundary\n"
        "transform chained ignored = do { value <- 41; value + 1 }\n"
        "type ChainedBoundary = ChainedBoundary\n"
        "transform run = "
            "let increment = fn value -> value + 1 in "
            "(6 * 7, 8 / 2, 2 < 3 && 3 >= 3, "
            "[1, 2] ++ (3 :: [4]), "
            "({answer: 42}.answer, (1, 2)), "
            "increment 41, 40 |> add 2, combined 0, "
            "chained 0)\n",
    with_executable_module(
        Source,
        'PhaseFourExpressions',
        fun() ->
            ?assertEqual(
                {
                    42,
                    4.0,
                    true,
                    [1, 2, 3, 4],
                    {42, {1, 2}},
                    42,
                    42,
                    42,
                    42
                },
                'PhaseFourExpressions':run()
            )
        end
    ).

parser_native_pattern_matrix_executes_test() ->
    Source =
        "module PhaseFourPatterns\n"
        "export transform classify\n"
        "export transform unwrap\n"
        "export transform inspect\n"
        "export transform float_match\n"
        "export transform string_match\n"
        "export transform bool_match\n"
        "export transform empty_record\n"
        "type Either a = Left a | Right a\n"
        "type Maybe a = None | Some a\n"
        "transform classify [1 2] = 12\n"
        "transform classify head :: _ = head\n"
        "transform classify [] = 0\n"
        "type ClassifyBoundary = ClassifyBoundary\n"
        "transform unwrap value = match value of\n"
        "  | Left(x) | Right(x) when x > 0, 10 / x > 1 -> x\n"
        "  | Left(_) | Right(_) -> 0\n"
        "end\n"
        "type UnwrapBoundary = UnwrapBoundary\n"
        "transform inspect "
            "(Some value as whole, {answer: answer}, (first, _)) "
            "when answer > 0 = (whole, value, answer, first)\n"
        "type InspectBoundary = InspectBoundary\n"
        "transform float_match 3.5 = 1\n"
        "transform float_match _ = 0\n"
        "type FloatBoundary = FloatBoundary\n"
        "transform string_match \"ok\" = 1\n"
        "transform string_match _ = 0\n"
        "type StringBoundary = StringBoundary\n"
        "transform bool_match true = 1\n"
        "transform bool_match false = 0\n"
        "type BoolBoundary = BoolBoundary\n"
        "transform empty_record {} = 1\n",
    with_executable_module(
        Source,
        'PhaseFourPatterns',
        fun() ->
            ?assertEqual(12, 'PhaseFourPatterns':classify([1, 2])),
            ?assertEqual(9, 'PhaseFourPatterns':classify([9, 8])),
            ?assertEqual(0, 'PhaseFourPatterns':classify([])),
            ?assertEqual(5, 'PhaseFourPatterns':unwrap({'Left', 5})),
            ?assertEqual(2, 'PhaseFourPatterns':unwrap({'Right', 2})),
            ?assertEqual(0, 'PhaseFourPatterns':unwrap({'Left', 0})),
            ?assertEqual(0, 'PhaseFourPatterns':unwrap({'Right', 20})),
            ?assertEqual(
                {{'Some', 7}, 7, 42, 10},
                'PhaseFourPatterns':inspect(
                    {{'Some', 7}, #{answer => 42}, {10, ignored}}
                )
            ),
            ?assertEqual(1, 'PhaseFourPatterns':float_match(3.5)),
            ?assertEqual(0, 'PhaseFourPatterns':float_match(2.5)),
            ?assertEqual(1, 'PhaseFourPatterns':string_match("ok")),
            ?assertEqual(0, 'PhaseFourPatterns':string_match("other")),
            ?assertEqual(1, 'PhaseFourPatterns':bool_match(true)),
            ?assertEqual(0, 'PhaseFourPatterns':bool_match(false)),
            ?assertEqual(1, 'PhaseFourPatterns':empty_record(#{}))
        end
    ).

data_representation_round_trip_executes_test() ->
    Source =
        "module PhaseFourRoundTrip\n"
        "export transform run\n"
        "type Payload = Payload Int Int\n"
        "transform decode "
            "(Payload(left right), [head tail], {answer: answer}) = "
            "(left, right, head, tail, answer)\n"
        "type DecodeBoundary = DecodeBoundary\n"
        "transform run = "
            "decode (Payload 1 2, [3, 4], {answer: 5})\n",
    with_executable_module(
        Source,
        'PhaseFourRoundTrip',
        fun() ->
            ?assertEqual(
                {1, 2, 3, 4, 5},
                'PhaseFourRoundTrip':run()
            )
        end
    ).

record_access_and_pattern_failure_semantics_execute_test() ->
    Source =
        "module PhaseFourRecords\n"
        "export transform read\n"
        "export transform project\n"
        "transform read {answer: value} = value\n"
        "transform read _ = 0\n"
        "type RecordBoundary = RecordBoundary\n"
        "transform project record = record.answer\n",
    with_executable_module(
        Source,
        'PhaseFourRecords',
        fun() ->
            ?assertEqual(
                42,
                'PhaseFourRecords':read(
                    #{answer => 42, extra => retained}
                )
            ),
            ?assertEqual(0, 'PhaseFourRecords':read(#{})),
            ?assertEqual(
                42,
                'PhaseFourRecords':project(#{answer => 42})
            ),
            ?assertError(
                {badkey, answer},
                'PhaseFourRecords':project(#{})
            )
        end
    ).

unsupported_pure_forms_fail_before_artifact_success_test() ->
    Location = {location, 9, 4},
    assert_backend_rejection(
        catena_codegen_module:generate_module(
            backend_module(
                {unknown_phase_four_expression, Location}
            )
        ),
        unsupported_backend_construct,
        expression
    ),
    assert_backend_rejection(
        catena_codegen_module:generate_module(
            backend_module(
                {binary_op,
                    unknown_phase_four_operator,
                    {literal, integer, 1, Location},
                    {literal, integer, 2, Location},
                    Location}
            )
        ),
        unsupported_backend_construct,
        operator
    ),
    assert_backend_rejection(
        catena_codegen_module:generate_module(
            {module,
                phase_four_unknown_pattern,
                [{run, 1}],
                [],
                [
                    {transform,
                        run,
                        [{unknown_phase_four_pattern, Location}],
                        {literal, integer, 0, Location},
                        Location}
                ],
                Location}
        ),
        unsupported_backend_construct,
        pattern
    ),
    assert_backend_rejection(
        catena_codegen_module:generate_module(
            {module,
                phase_four_unknown_declaration,
                [],
                [],
                [{unknown_phase_four_declaration, Location}],
                Location}
        ),
        invalid_declaration_disposition,
        declaration
    ),
    AritySource =
        "module PhaseFourBadArity\n"
        "export transform unwrap\n"
        "type Pair = Pair Int Int\n"
        "transform unwrap Pair(value) = value\n",
    ?assertMatch(
        {error, {backend_error, arity_mismatch, #{}}},
        catena_compile:compile_string_to_core(AritySource)
    ).

backend_module(Body) ->
    Location = {location, 9, 4},
    {module,
        phase_four_invalid,
        [{run, 0}],
        [],
        [{transform, run, [], Body, Location}],
        Location}.

assert_backend_rejection(Result, Category, Construct) ->
    ?assertMatch({error, {backend_error, Category, _}}, Result),
    {error, Diagnostic} = Result,
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(Construct, maps:get(construct, Details)).

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
        unload(Module)
    end.

unload(Module) ->
    code:purge(Module),
    code:delete(Module).
