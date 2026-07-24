-module(catena_backend_hardening_phase7_diagnostic_tests).

-include_lib("eunit/include/eunit.hrl").

generated_core_carries_user_and_synthetic_origins_test() ->
    Source =
        "module OriginFixture\n"
        "export transform unwrap\n"
        "type Box = Box Int\n"
        "transform unwrap value = match value of\n"
        "  | Box item -> item\n"
        "end\n",
    {ok, Artifact} = catena_compile:compile_string_to_beam(
        Source,
        #{
            process_imports => false,
            codegen_opts => #{file => "origin-fixture.cat"}
        }
    ),
    Core = maps:get(core, Artifact),
    {ok, ModuleOrigin} = catena_core_origin:annotation(Core),
    ?assertEqual(user, maps:get(origin, ModuleOrigin)),
    ?assertEqual(module, maps:get(construct, ModuleOrigin)),
    ?assertEqual("origin-fixture.cat", maps:get(file, ModuleOrigin)),
    Origins = collect_origins(Core),
    ?assert(
        lists:any(
            fun(Origin) ->
                maps:get(construct, Origin) =:= transform andalso
                    maps:get(transform, Origin) =:= unwrap
            end,
            Origins
        )
    ),
    ?assert(
        lists:any(
            fun(Origin) ->
                maps:get(construct, Origin) =:= clause
            end,
            Origins
        )
    ),
    ?assert(
        lists:any(
            fun(Origin) ->
                maps:get(construct, Origin) =:= pat_constructor
            end,
            Origins
        )
    ),
    ?assert(
        lists:any(
            fun(Origin) ->
                maps:get(origin, Origin) =:= synthetic andalso
                    maps:get(construct, Origin) =:=
                        generated_parameter
            end,
            Origins
        )
    ),
    MetadataOrigins = maps:get(
        origins,
        maps:get(metadata, Artifact)
    ),
    ?assertMatch(
        [#{
            origin := user,
            construct := transform,
            transform := unwrap,
            generated_identity := {unwrap, 1}
        }],
        maps:get(generated, MetadataOrigins)
    ).

effect_entry_is_indexed_as_synthetic_origin_test() ->
    Source =
        "module EffectOriginFixture\n"
        "export transform run\n"
        "effect Answer\n"
        "operation get : Int\n"
        "end\n"
        "transform run = handle perform Answer.get() then {\n"
        "  Answer { get -> 42 }\n"
        "}\n",
    {ok, Artifact} =
        catena_compile:compile_string_to_beam(Source),
    Generated = maps:get(
        generated,
        maps:get(origins, maps:get(metadata, Artifact))
    ),
    ?assert(
        lists:any(
            fun(Origin) ->
                maps:get(origin, Origin) =:= synthetic andalso
                    maps:get(construct, Origin) =:=
                        effect_runtime_entry andalso
                    maps:get(transform, Origin) =:= run
            end,
            Generated
        )
    ).

core_lint_diagnostic_maps_to_catena_source_test() ->
    Identity = {responsible, 0},
    InvalidCore = cerl:c_module(
        cerl:c_atom('DiagnosticFixture'),
        [cerl:c_fname(responsible, 0)],
        [],
        [{
            cerl:c_fname(responsible, 0),
            cerl:c_fun([], cerl:c_var(unbound_name))
        }]
    ),
    Context = #{
        module => 'DiagnosticFixture',
        source_identity => #{
            kind => file,
            path => "diagnostic-fixture.cat"
        },
        origins => #{
            generated => [#{
                origin => user,
                construct => transform,
                module => 'DiagnosticFixture',
                transform => responsible,
                generated_identity => Identity,
                location => {location, 9, 3}
            }]
        }
    },
    {error, Diagnostic} =
        catena_beam_artifact:validate_core(InvalidCore, Context),
    Details = catena_backend_error:details(Diagnostic),
    [Error | _] = maps:get(errors, Details),
    ?assertEqual(catena_artifact_diagnostic, maps:get(kind, Error)),
    ?assertEqual(error, maps:get(severity, Error)),
    ?assertEqual(core_validation, maps:get(stage, Error)),
    ?assertEqual(responsible, maps:get(transform, Error)),
    ?assertEqual(Identity, maps:get(generated_identity, Error)),
    ?assertEqual({location, 9, 3}, maps:get(location, Error)),
    ?assert(maps:is_key(otp_detail, Error)),
    ?assert(maps:is_key(otp_errors, Details)),
    ?assertEqual(
        "Catena backend generated Core Erlang that OTP rejected in "
            "module 'DiagnosticFixture', transform responsible at line 9, "
            "column 3",
        catena_backend_error:format(Diagnostic)
    ).

beam_compiler_diagnostic_retains_original_detail_test() ->
    InvalidCore = cerl:c_module(
        cerl:c_atom('BeamDiagnosticFixture'),
        [cerl:c_fname(run, 0)],
        [],
        [{
            cerl:c_fname(run, 0),
            cerl:c_fun([], cerl:c_var(unbound_name))
        }]
    ),
    {error, Diagnostic} = catena_beam_artifact:compile_core(
        InvalidCore,
        #{
            module => 'BeamDiagnosticFixture',
            source_identity => #{kind => string, name => "beam-invalid.cat"}
        }
    ),
    ?assertEqual(
        beam_compilation_failed,
        catena_backend_error:category(Diagnostic)
    ),
    Details = catena_backend_error:details(Diagnostic),
    [Error | _] = maps:get(errors, Details),
    ?assertEqual(beam_compilation, maps:get(stage, Error)),
    ?assertEqual(beam_compilation_failed, maps:get(category, Error)),
    ?assert(maps:is_key(otp_detail, Error)),
    ?assert(maps:is_key(otp_errors, Details)).

collect_origins(Core) ->
    lists:reverse(
        cerl_trees:fold(
            fun(Node, Acc) ->
                case catena_core_origin:annotation(Node) of
                    {ok, Origin} -> [Origin | Acc];
                    error -> Acc
                end
            end,
            [],
            Core
        )
    ).
