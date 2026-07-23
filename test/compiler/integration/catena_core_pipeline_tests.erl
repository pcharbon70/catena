-module(catena_core_pipeline_tests).

-include_lib("eunit/include/eunit.hrl").

source_to_executable_core_test() ->
    Source =
        "module Arithmetic\n"
        "export transform add_one\n"
        "transform add_one x = x + 1\n",
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    {ok, 'Arithmetic', Binary, _Warnings} = compile_core(CoreModule),
    code:purge('Arithmetic'),
    code:delete('Arithmetic'),
    try
        {module, 'Arithmetic'} =
            code:load_binary('Arithmetic', "Arithmetic.core", Binary),
        ?assertEqual(42, 'Arithmetic':add_one(41))
    after
        code:purge('Arithmetic'),
        code:delete('Arithmetic')
    end.

multi_clause_constructor_patterns_reach_executable_core_test() ->
    Source =
        "module MaybeValue\n"
        "export transform value_or_zero\n"
        "type Maybe a = None | Some a\n"
        "transform value_or_zero None = 0\n"
        "transform value_or_zero Some(x) = x\n",
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    {ok, 'MaybeValue', Binary, _Warnings} = compile_core(CoreModule),
    code:purge('MaybeValue'),
    code:delete('MaybeValue'),
    try
        {module, 'MaybeValue'} =
            code:load_binary('MaybeValue', "MaybeValue.core", Binary),
        ?assertEqual(0, 'MaybeValue':value_or_zero({'None'})),
        ?assertEqual(7, 'MaybeValue':value_or_zero({'Some', 7}))
    after
        code:purge('MaybeValue'),
        code:delete('MaybeValue')
    end.

type_error_prevents_codegen_test() ->
    Source =
        "module Broken\n"
        "export transform broken\n"
        "transform broken x = missing\n",
    ?assertMatch(
        {error, {type_error, broken, _}},
        catena_compile:compile_string_to_core(Source)
    ).

semantic_error_is_preserved_test() ->
    Source =
        "module Guarded\n"
        "export transform guarded\n"
        "transform guarded x when perform IO.read() = x\n",
    ?assertMatch(
        {error, {impure_guard, guarded, _, _}},
        catena_compile:compile_string_to_core(Source)
    ).

lexer_error_is_preserved_test() ->
    ?assertMatch(
        {error, {lex_error, _}},
        catena_compile:compile_string_to_core(
            "module Broken\ntransform broken = \"unterminated\n"
        )
    ).

parser_error_is_preserved_test() ->
    ?assertMatch(
        {error, _},
        catena_compile:compile_string_to_core(
            "module Broken\ntransform broken = + +\n"
        )
    ).

file_to_core_test() ->
    Source =
        "module FileCompiled\n"
        "export transform identity\n"
        "transform identity x = x\n",
    Path = filename:join(
        "/tmp",
        "catena_core_pipeline_" ++
            integer_to_list(erlang:unique_integer([positive])) ++
            ".cat"
    ),
    try
        ok = file:write_file(Path, Source),
        {ok, CoreModule} = catena_compile:compile_file_to_core(Path),
        ?assertEqual(
            'FileCompiled',
            cerl:atom_val(cerl:module_name(CoreModule))
        )
    after
        file:delete(Path)
    end.

invalid_core_file_extension_test() ->
    ?assertEqual(
        {error, {invalid_file_extension, "invalid.txt", ".txt"}},
        catena_compile:compile_file_to_core("invalid.txt")
    ).

compile_core(CoreModule) ->
    compile:forms(
        CoreModule,
        [from_core, binary, return_errors, return_warnings]
    ).
