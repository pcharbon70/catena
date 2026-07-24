-module(catena_backend_error_tests).

-include_lib("eunit/include/eunit.hrl").

stable_categories_test() ->
    ?assertEqual(
        [
            unsupported_backend_construct,
            unresolved_call,
            ambiguous_call,
            arity_mismatch,
            missing_transform_implementation,
            invalid_declaration_disposition,
            runtime_dependency_unavailable,
            core_validation_failed,
            beam_compilation_failed
        ],
        catena_backend_error:categories()
    ).

diagnostic_context_preserves_source_and_generated_identity_test() ->
    SourceTerm = {binary_op, mystery, left, right, {location, 12, 8}},
    Context =
        catena_backend_error:context(
            expression_translation,
            operator,
            SourceTerm,
            #{
                module => 'Example',
                transform => calculate,
                generated_identity => {calculate, 1}
            }
        ),
    Diagnostic =
        catena_backend_error:unsupported_backend_construct(
            operator,
            Context
        ),
    ?assert(catena_backend_error:is_diagnostic(Diagnostic)),
    ?assertEqual(
        unsupported_backend_construct,
        catena_backend_error:category(Diagnostic)
    ),
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(expression_translation, maps:get(stage, Details)),
    ?assertEqual(operator, maps:get(construct, Details)),
    ?assertEqual({location, 12, 8}, maps:get(location, Details)),
    ?assertEqual('Example', maps:get(module, Details)),
    ?assertEqual(calculate, maps:get(transform, Details)),
    ?assertEqual({calculate, 1}, maps:get(generated_identity, Details)).

constructor_payloads_are_stable_test_() ->
    Context = catena_backend_error:context(call_resolution, call, undefined),
    [
        ?_assertMatch(
            {backend_error, unresolved_call, #{source_identity := {run, 1}}},
            catena_backend_error:unresolved_call(run, 1, Context)
        ),
        ?_assertMatch(
            {backend_error, ambiguous_call, #{candidates := [local, imported]}},
            catena_backend_error:ambiguous_call(
                run,
                1,
                [local, imported],
                Context
            )
        ),
        ?_assertMatch(
            {backend_error, arity_mismatch,
                #{expected_arity := 2, actual_arity := 1}},
            catena_backend_error:arity_mismatch(run, 2, 1, Context)
        ),
        ?_assertMatch(
            {backend_error, missing_transform_implementation,
                #{source_identity := run}},
            catena_backend_error:missing_transform_implementation(
                run,
                Context
            )
        ),
        ?_assertMatch(
            {backend_error, invalid_declaration_disposition,
                #{declaration := {test_decl, "x"}}},
            catena_backend_error:invalid_declaration_disposition(
                {test_decl, "x"},
                Context
            )
        ),
        ?_assertMatch(
            {backend_error, core_validation_failed, #{errors := [invalid]}},
            catena_backend_error:core_validation_failed(
                [invalid],
                [],
                Context
            )
        ),
        ?_assertMatch(
            {backend_error, beam_compilation_failed, #{warnings := [unused]}},
            catena_backend_error:beam_compilation_failed(
                [],
                [unused],
                Context
            )
        )
    ].

format_prefers_catena_source_context_test() ->
    Diagnostic =
        catena_backend_error:unsupported_backend_construct(
            pattern,
            #{
                stage => pattern_compilation,
                module => 'MaybeValue',
                transform => value_or_zero,
                location => {location, 7, 19},
                generated_identity => {'value_or_zero$backend', 1}
            }
        ),
    Message = catena_backend_error:format(Diagnostic),
    ?assertNotEqual(nomatch, string:find(Message, "pattern construct")),
    ?assertNotEqual(nomatch, string:find(Message, "module 'MaybeValue'")),
    ?assertNotEqual(nomatch, string:find(Message, "transform value_or_zero")),
    ?assertNotEqual(nomatch, string:find(Message, "line 7, column 19")),
    ?assertEqual(nomatch, string:find(Message, "value_or_zero$backend")).

invalid_term_is_not_a_diagnostic_test() ->
    ?assertNot(catena_backend_error:is_diagnostic({codegen_error, bad_ast})).
