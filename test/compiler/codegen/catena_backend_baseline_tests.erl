-module(catena_backend_baseline_tests).

-include_lib("eunit/include/eunit.hrl").

named_top_level_call_reaches_core_lint_as_unbound_variable_test() ->
    Source =
        "module Calls\n"
        "export transform caller\n"
        "transform callee x = x + 1\n"
        "transform caller x = callee(x)\n",
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    ?assertMatch(
        {error, [{_, [{none, core_lint, {unbound_var, callee, {caller, 1}}}]}], []},
        compile_core(CoreModule)
    ).

unknown_expression_placeholder_path_crashes_test() ->
    Unknown = {mystery_expr, location()},
    ?assertException(
        error,
        undef,
        catena_codegen_expr:translate_expr(Unknown, new_state())
    ).

unknown_pattern_becomes_wildcard_test() ->
    {CorePattern, _State} =
        catena_codegen_pattern:compile_pattern(
            {mystery_pattern, location()},
            new_state()
        ),
    ?assertEqual('_', cerl:var_name(CorePattern)).

complex_let_binding_becomes_wildcard_test() ->
    Pattern = {pat_constructor, 'Some', [{pat_var, x, location()}], location()},
    Expr =
        {let_expr,
            [{Pattern, {constructor, 'Some', [{literal, integer, 1, location()}], location()}}],
            {literal, integer, 2, location()},
            location()},
    {CoreLet, _State} = catena_codegen_expr:translate_expr(Expr, new_state()),
    [CoreVar] = cerl:let_vars(CoreLet),
    ?assertEqual('_', cerl:var_name(CoreVar)).

unknown_operator_becomes_arbitrary_erlang_call_test() ->
    Expr =
        {binary_op,
            mystery_operator,
            {literal, integer, 1, location()},
            {literal, integer, 2, location()},
            location()},
    {CoreCall, _State} = catena_codegen_expr:translate_expr(Expr, new_state()),
    ?assertEqual(erlang, cerl:atom_val(cerl:call_module(CoreCall))),
    ?assertEqual(mystery_operator, cerl:atom_val(cerl:call_name(CoreCall))).

unclassified_declaration_is_silently_omitted_test() ->
    Module =
        {module,
            'Silent',
            [],
            [],
            [{test_decl, "not emitted", {literal, bool, true, location()}, location()}],
            location()},
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    ?assertEqual([], cerl:module_defs(CoreModule)).

misplaced_or_pattern_becomes_wildcard_test() ->
    Pattern =
        {pat_or,
            [
                {pat_literal, 1, integer, location()},
                {pat_literal, 2, integer, location()}
            ],
            location()},
    {CorePattern, _State} =
        catena_codegen_pattern:compile_pattern(Pattern, new_state()),
    ?assertEqual('_', cerl:var_name(CorePattern)).

new_state() ->
    catena_codegen_utils:new_state().

location() ->
    {location, 9, 4}.

compile_core(CoreModule) ->
    compile:forms(
        CoreModule,
        [from_core, binary, return_errors, return_warnings]
    ).
