-module(catena_codegen_lower_tests).

-include_lib("eunit/include/eunit.hrl").

parser_native_arithmetic_module_test() ->
    Source =
        "module Math\n"
        "export transform add_one\n"
        "transform add_one x = x + 1\n",
    CoreModule = generate_from_source(Source),
    ?assertEqual(
        [{add_one, 1}],
        export_pairs(CoreModule)
    ),
    ?assertEqual(ok, validate_core_module(CoreModule)).

parser_native_multiclause_module_test() ->
    Source =
        "module MaybeValue\n"
        "export transform value_or_zero\n"
        "type Maybe a = None | Some a\n"
        "transform value_or_zero None = 0\n"
        "transform value_or_zero Some(x) = x\n",
    CoreModule = generate_from_source(Source),
    ?assertEqual(
        [{value_or_zero, 1}],
        export_pairs(CoreModule)
    ),
    ?assertEqual(ok, validate_core_module(CoreModule)).

parser_native_exports_are_honored_test() ->
    Source =
        "module Visibility\n"
        "export transform public\n"
        "transform public x = x\n"
        "transform hidden x = x\n",
    CoreModule = generate_from_source(Source),
    ?assertEqual(
        [{public, 1}],
        export_pairs(CoreModule)
    ).

canonical_literal_and_operator_lowering_test() ->
    Location = {location, 1, 1},
    ParserExpr = {binary_op, plus,
        {literal, 1, integer, Location},
        {literal, 2, integer, Location},
        Location},
    ?assertEqual(
        {binary_op, '+',
            {literal, integer, 1, Location},
            {literal, integer, 2, Location},
            Location},
        catena_codegen_lower:lower_expr(ParserExpr)
    ).

generate_from_source(Source) ->
    {ok, Tokens, _EndLocation} = catena_lexer:string(Source),
    {ok, ParsedModule} = catena_parser:parse(Tokens),
    {ok, AnalyzedModule} = catena_semantic:analyze(ParsedModule),
    {ok, CoreModule} = catena_codegen_module:generate_module(AnalyzedModule),
    CoreModule.

export_pairs(CoreModule) ->
    lists:sort([
        {cerl:fname_id(Export), cerl:fname_arity(Export)}
        || Export <- cerl:module_exports(CoreModule)
    ]).

validate_core_module(CoreModule) ->
    case compile:forms(
        CoreModule,
        [from_core, binary, return_errors, return_warnings]
    ) of
        {ok, _Module, _Binary, _Warnings} ->
            ok;
        {error, Errors, Warnings} ->
            {error, Errors, Warnings}
    end.
