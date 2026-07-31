-module(catena_backend_hardening_phase7_api_tests).

-include_lib("eunit/include/eunit.hrl").

string_api_returns_validated_artifact_test() ->
    Source =
        "module PublicBeamApi\n"
        "export transform increment\n"
        "transform increment value = value + 1\n",
    Options = #{
        process_imports => false,
        search_paths => ["fixtures"],
        codegen_opts => #{
            file => "public-beam-api.cat",
            version => "7.1"
        }
    },
    {ok, Artifact} =
        catena_compile:compile_string_to_beam(Source, Options),
    ?assertEqual(catena_beam_artifact, maps:get(format, Artifact)),
    ?assertEqual(2, maps:get(format_version, Artifact)),
    ?assertEqual('PublicBeamApi', maps:get(source_module, Artifact)),
    ?assertEqual('PublicBeamApi', maps:get(runtime_module, Artifact)),
    ?assert(is_binary(maps:get(beam, Artifact))),
    ?assertEqual(
        #{kind => string, name => "public-beam-api.cat"},
        maps:get(source_identity, Artifact)
    ),
    ?assertEqual([], maps:get(runtime_dependencies, Artifact)),
    ?assertEqual([], maps:get(warnings, Artifact)),
    Metadata = maps:get(metadata, Artifact),
    ?assertEqual(
        Options,
        maps:get(compiler_options, Metadata)
    ),
    ?assertEqual(
        passed,
        maps:get(
            otp_from_core,
            maps:get(validation, Metadata)
        )
    ),
    with_loaded_artifact(Artifact, fun() ->
        ?assertEqual(42, 'PublicBeamApi':increment(41))
    end).

file_api_preserves_source_identity_and_filename_test() ->
    Path = filename:join(
        "/tmp",
        "catena-phase7-public-file-api.cat"
    ),
    Source =
        "module PublicBeamFileApi\n"
        "export transform identity\n"
        "transform identity value = value\n",
    ok = file:write_file(Path, Source),
    try
        {ok, Artifact} =
            catena_compile:compile_file_to_beam(
                Path,
                #{process_imports => false}
            ),
        ?assertEqual(
            #{kind => file, path => Path},
            maps:get(source_identity, Artifact)
        ),
        CompilerOptions = maps:get(
            compiler_options,
            maps:get(metadata, Artifact)
        ),
        ?assertEqual(
            Path,
            maps:get(
                file,
                maps:get(codegen_opts, CompilerOptions)
            )
        )
    after
        file:delete(Path)
    end.

core_lint_rejects_invalid_core_without_artifact_test() ->
    InvalidCore = cerl:c_module(
        cerl:c_atom('InvalidCoreFixture'),
        [cerl:c_fname(run, 0)],
        [],
        [{
            cerl:c_fname(run, 0),
            cerl:c_fun([], cerl:c_var(unbound_source_name))
        }]
    ),
    {error, Diagnostic} = catena_beam_artifact:validate_core(
        InvalidCore,
        #{
            module => 'InvalidCoreFixture',
            source_identity => #{kind => string, name => "invalid.cat"}
        }
    ),
    ?assert(catena_backend_error:is_diagnostic(Diagnostic)),
    ?assertEqual(
        core_validation_failed,
        catena_backend_error:category(Diagnostic)
    ),
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(core_validation, maps:get(stage, Details)),
    ?assertNotEqual([], maps:get(errors, Details)).

frontend_failure_returns_no_partial_artifact_test() ->
    ?assertMatch(
        {error, _},
        catena_compile:compile_string_to_beam(
            "module BrokenBeamApi\n"
            "transform broken value = missing value\n"
        )
    ),
    ?assertEqual(non_existing, code:which('BrokenBeamApi')).

invalid_unit_and_source_set_inputs_fail_closed_test() ->
    ?assertEqual(
        {error, {invalid_compilation_unit, unchecked_backend_input}},
        catena_beam_artifact:from_unit(#{})
    ),
    ?assertMatch(
        {error, {invalid_source_set, _, _}},
        catena_compile:compile_source_set_to_beam(
            not_a_source_set,
            #{}
        )
    ).

with_loaded_artifact(Artifact, Assertion) ->
    Module = maps:get(runtime_module, Artifact),
    {module, Module} = code:load_binary(
        Module,
        "phase7-public-api-memory",
        maps:get(beam, Artifact)
    ),
    try
        Assertion()
    after
        code:purge(Module),
        code:delete(Module)
    end.
