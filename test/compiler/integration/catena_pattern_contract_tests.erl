-module(catena_pattern_contract_tests).

-include_lib("eunit/include/eunit.hrl").

parser_native_effectful_guard_is_rejected_test() ->
    Source =
        "transform guarded x when perform IO.read() = x\n",
    Parsed = parse(Source),
    ?assertMatch(
        {error, {impure_guard, guarded, {effect_set, ['IO']}, _}},
        catena_semantic:analyze(Parsed)
    ).

inconsistent_transform_clause_arity_is_rejected_test() ->
    Source =
        "transform choose x = x\n"
        "transform choose x y = x\n",
    Parsed = parse(Source),
    ?assertMatch(
        {error, {inconsistent_clause_arity, choose, 1, 2, _}},
        catena_semantic:analyze(Parsed)
    ).

or_pattern_binding_mismatch_is_rejected_test() ->
    Source =
        "transform choose value = match value of\n"
        "  | Some(x) | None -> x\n"
        "end\n",
    Parsed = parse(Source),
    ?assertMatch(
        {error, {or_pattern_binding_mismatch, [x], [], _}},
        catena_semantic:analyze(Parsed)
    ).

consistent_or_pattern_reaches_valid_core_test() ->
    Source =
        "module EitherValue\n"
        "export transform unwrap\n"
        "transform unwrap value = match value of\n"
        "  | Left(x) | Right(x) -> x\n"
        "end\n",
    ?assertEqual(ok, validate_source_core(Source)).

advanced_clause_patterns_reach_valid_core_test() ->
    Source =
        "module PatternForms\n"
        "export transform first\n"
        "transform first [x] = x\n"
        "transform first h :: _ = h\n",
    ?assertEqual(ok, validate_source_core(Source)).

as_pattern_reaches_valid_core_test() ->
    Source =
        "module AsPattern\n"
        "export transform unwrap\n"
        "transform unwrap Some(x) as whole = x\n",
    ?assertEqual(ok, validate_source_core(Source)).

tuple_pattern_with_guard_reaches_valid_core_test() ->
    Source =
        "module GuardedTuple\n"
        "export transform first_positive\n"
        "transform first_positive (x, _) when x > 0 = x\n",
    ?assertEqual(ok, validate_source_core(Source)).

parse(Source) ->
    {ok, Tokens, _EndLocation} = catena_lexer:string(Source),
    {ok, ParsedModule} = catena_parser:parse(Tokens),
    ParsedModule.

validate_source_core(Source) ->
    ParsedModule = parse(Source),
    {ok, AnalyzedModule} = catena_semantic:analyze(ParsedModule),
    {ok, CoreModule} = catena_codegen_module:generate_module(AnalyzedModule),
    case compile:forms(
        CoreModule,
        [from_core, binary, return_errors, return_warnings]
    ) of
        {ok, _Module, _Binary, _Warnings} ->
            ok;
        {error, Errors, Warnings} ->
            {error, Errors, Warnings}
    end.
