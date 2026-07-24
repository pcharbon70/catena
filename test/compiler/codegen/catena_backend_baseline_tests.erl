-module(catena_backend_baseline_tests).

-include_lib("eunit/include/eunit.hrl").

named_top_level_call_resolves_as_core_function_test() ->
    Source =
        "module Calls\n"
        "export transform caller\n"
        "transform callee x = x + 1\n"
        "transform caller x = callee(x)\n",
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    ?assertMatch(
        {ok, 'Calls', _Binary, _Warnings},
        compile_core(CoreModule)
    ).

unknown_expression_is_rejected_test() ->
    Unknown = {mystery_expr, location()},
    Diagnostic = capture_backend_throw(
        fun() -> catena_codegen_expr:translate_expr(Unknown, new_state()) end
    ),
    assert_unsupported(Diagnostic, expression, location()).

unknown_pattern_is_rejected_test() ->
    Unknown = {mystery_pattern, location()},
    Diagnostic = capture_backend_throw(
        fun() ->
            catena_codegen_pattern:compile_pattern(Unknown, new_state())
        end
    ),
    assert_unsupported(Diagnostic, pattern, location()).

complex_let_binding_is_rejected_test() ->
    Pattern = {pat_constructor, 'Some', [{pat_var, x, location()}], location()},
    Expr =
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
    Diagnostic = capture_backend_throw(
        fun() -> catena_codegen_expr:translate_expr(Expr, new_state()) end
    ),
    assert_unsupported(Diagnostic, binding_pattern, location()).

unknown_operator_is_rejected_test() ->
    Expr =
        {binary_op,
            mystery_operator,
            {literal, integer, 1, location()},
            {literal, integer, 2, location()},
            location()},
    Diagnostic = capture_backend_throw(
        fun() -> catena_codegen_expr:translate_expr(Expr, new_state()) end
    ),
    assert_unsupported(Diagnostic, operator, location()),
    ?assertEqual(
        mystery_operator,
        maps:get(operator, catena_backend_error:details(Diagnostic))
    ).

unknown_unary_operator_is_rejected_test() ->
    Expr =
        {unary_op,
            mystery_unary,
            {literal, integer, 1, location()},
            location()},
    Diagnostic = capture_backend_throw(
        fun() -> catena_codegen_expr:translate_expr(Expr, new_state()) end
    ),
    assert_unsupported(Diagnostic, unary_operator, location()),
    ?assertEqual(
        mystery_unary,
        maps:get(operator, catena_backend_error:details(Diagnostic))
    ).

unclassified_declaration_is_rejected_test() ->
    Declaration =
        {test_decl,
            "not emitted",
            {literal, bool, true, location()},
            location()},
    Module =
        {module,
            'Silent',
            [],
            [],
            [Declaration],
            location()},
    ?assertMatch(
        {error,
            {backend_error, invalid_declaration_disposition,
                #{
                    module := 'Silent',
                    declaration := Declaration,
                    location := {location, 9, 4}
                }}},
        catena_codegen_module:generate_module(Module)
    ).

misplaced_or_pattern_is_rejected_test() ->
    Pattern =
        {pat_or,
            [
                {pat_literal, 1, integer, location()},
                {pat_literal, 2, integer, location()}
            ],
            location()},
    Diagnostic = capture_backend_throw(
        fun() ->
            catena_codegen_pattern:compile_pattern(Pattern, new_state())
        end
    ),
    assert_unsupported(Diagnostic, or_pattern, location()).

unknown_expression_propagates_through_module_boundary_test() ->
    Module =
        {module,
            'UnknownBody',
            [],
            [],
            [
                {transform,
                    broken,
                    [],
                    {mystery_expr, location()},
                    location()}
            ],
            location()},
    ?assertMatch(
        {error,
            {backend_error, unsupported_backend_construct,
                #{
                    stage := expression_lowering,
                    construct := expression,
                    location := {location, 9, 4}
                }}},
        catena_codegen_module:generate_module(Module)
    ).

new_state() ->
    catena_codegen_utils:new_state().

location() ->
    {location, 9, 4}.

compile_core(CoreModule) ->
    compile:forms(
        CoreModule,
        [from_core, binary, return_errors, return_warnings]
    ).

capture_backend_throw(Fun) ->
    try
        Fun(),
        error(expected_backend_diagnostic)
    catch
        throw:{backend_error, _, _} = Diagnostic ->
            Diagnostic
    end.

assert_unsupported(Diagnostic, Construct, Location) ->
    ?assertMatch(
        {backend_error, unsupported_backend_construct, #{}},
        Diagnostic
    ),
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(Construct, maps:get(construct, Details)),
    ?assertEqual(Location, maps:get(location, Details)).
