%%%-------------------------------------------------------------------
%%% @doc Validated in-memory BEAM artifact boundary.
%%%
%%% This module is the only maintained path from a validated Catena
%%% compilation unit to a BEAM binary.  It lints generated Core Erlang before
%%% invoking the OTP compiler and never returns an artifact when either
%%% boundary rejects the generated module.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_beam_artifact).

-export([
    format_version/0,
    from_unit/1,
    validate/1,
    validate/2,
    load/1,
    load/2,
    validate_core/2,
    compile_core/2
]).

-define(ARTIFACT_VERSION, 2).

-type artifact() :: #{
    format := catena_beam_artifact,
    format_version := pos_integer(),
    module_identity := map(),
    source_module := atom(),
    runtime_module := atom(),
    beam := binary(),
    core := cerl:cerl(),
    source_identity := term(),
    runtime_dependencies := [map()],
    artifact_dependencies := [map()],
    warnings := [term()],
    interface := catena_module_interface:interface(),
    runtime_contract := map(),
    metadata := map()
}.

-export_type([artifact/0]).

-spec format_version() -> pos_integer().
format_version() -> ?ARTIFACT_VERSION.

%% @doc Generate, validate, and compile an artifact from a validated unit.
-spec from_unit(catena_compilation_unit:t()) ->
    {ok, artifact()} | {error, term()}.
from_unit(Unit) ->
    case catena_compilation_unit:is_compilation_unit(Unit) of
        true ->
            case catena_codegen_module:generate_validated_module(Unit) of
                {ok, CoreModule} ->
                    Origins = catena_core_origin:inventory(
                        Unit,
                        CoreModule
                    ),
                    Context = (artifact_context(Unit))#{
                        origins => Origins
                    },
                    case validate_core(CoreModule, Context) of
                        {ok, CoreWarnings} ->
                            case compile_core(CoreModule, Context) of
                                {ok, RuntimeModule, Binary, BeamWarnings} ->
                                    expected_runtime_module(
                                        Unit,
                                        RuntimeModule,
                                        Binary,
                                        CoreModule,
                                        CoreWarnings ++ BeamWarnings,
                                        Origins
                                    );
                                {error, _} = Error ->
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, {invalid_compilation_unit, unchecked_backend_input}}
    end.

%% @doc Validate an artifact and its exact runtime contract before loading.
-spec validate(term()) -> {ok, artifact()} | {error, term()}.
validate(Artifact) ->
    validate(Artifact, #{}).

-spec validate(term(), map()) -> {ok, artifact()} | {error, term()}.
validate(Artifact, Options) when is_map(Options) ->
    case validate_envelope(Artifact) of
        ok ->
            Context = artifact_validation_context(Artifact),
            Available = maps:get(available_runtime_modules, Options, auto),
            case catena_runtime_contract:validate(
                maps:get(runtime_dependencies, Artifact),
                Available,
                Context#{stage => artifact_load}
            ) of
                ok ->
                    validate_artifact_dependencies(Artifact, Options, Context);
                {error, _} = Error ->
                    Error
            end;
        {error, Reason} ->
            {error, catena_backend_error:artifact_validation_failed(
                Reason,
                envelope_context(Artifact)
            )}
    end;
validate(Artifact, Options) ->
    {error, catena_backend_error:artifact_validation_failed(
        invalid_validation_options,
        #{stage => artifact_load, source_term => {Artifact, Options}}
    )}.

%% @doc Load only after the version, identity, runtime, and checksum gates pass.
-spec load(term()) -> {module, atom()} | {error, term()}.
load(Artifact) ->
    load(Artifact, #{}).

-spec load(term(), map()) -> {module, atom()} | {error, term()}.
load(Artifact, Options) ->
    case validate(Artifact, Options) of
        {ok, Validated} ->
            Module = maps:get(runtime_module, Validated),
            Filename = maps:get(filename, Options, "catena-artifact-memory"),
            case code:load_binary(Module, Filename, maps:get(beam, Validated)) of
                {module, Module} = Loaded ->
                    Loaded;
                {error, Reason} ->
                    {error, catena_backend_error:artifact_validation_failed(
                        {beam_load_failed, Reason},
                        (artifact_validation_context(Validated))#{
                            stage => artifact_load
                        }
                    )}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Run OTP Core lint as an explicit artifact acceptance boundary.
-spec validate_core(cerl:cerl(), map()) ->
    {ok, [term()]} | {error, catena_backend_error:diagnostic()}.
validate_core(CoreModule, Context) when is_map(Context) ->
    case core_lint:module(CoreModule) of
        {ok, Warnings} ->
            {ok, catena_artifact_diagnostic:normalize(
                core_validation,
                warning,
                Warnings,
                Context
            )};
        {error, Errors, Warnings} ->
            NormalizedErrors =
                catena_artifact_diagnostic:normalize(
                    core_validation,
                    error,
                    Errors,
                    Context
                ),
            NormalizedWarnings =
                catena_artifact_diagnostic:normalize(
                    core_validation,
                    warning,
                    Warnings,
                    Context
                ),
            {error,
                catena_backend_error:core_validation_failed(
                    NormalizedErrors,
                    NormalizedWarnings,
                    failure_context(
                        Context,
                        NormalizedErrors,
                        core_validation,
                        Errors,
                        Warnings
                    )
                )};
        Other ->
            {error,
                catena_backend_error:core_validation_failed(
                    [{unexpected_core_lint_result, Other}],
                    [],
                    Context#{stage => core_validation}
                )}
    end.

%% @doc Compile an already linted Core module to a binary with OTP.
%%
%% Callers which expose an artifact must invoke validate_core/2 first.
-spec compile_core(cerl:cerl(), map()) ->
    {ok, atom(), binary(), [term()]} |
    {error, catena_backend_error:diagnostic()}.
compile_core(CoreModule, Context) when is_map(Context) ->
    case compile:forms(
        CoreModule,
        [from_core, binary, return_errors, return_warnings]
    ) of
        {ok, Module, Binary} ->
            {ok, Module, Binary, []};
        {ok, Module, Binary, Warnings} ->
            {ok,
                Module,
                Binary,
                catena_artifact_diagnostic:normalize(
                    beam_compilation,
                    warning,
                    Warnings,
                    Context
                )};
        {error, Errors, Warnings} ->
            NormalizedErrors =
                catena_artifact_diagnostic:normalize(
                    beam_compilation,
                    error,
                    Errors,
                    Context
                ),
            NormalizedWarnings =
                catena_artifact_diagnostic:normalize(
                    beam_compilation,
                    warning,
                    Warnings,
                    Context
                ),
            {error,
                catena_backend_error:beam_compilation_failed(
                    NormalizedErrors,
                    NormalizedWarnings,
                    failure_context(
                        Context,
                        NormalizedErrors,
                        beam_compilation,
                        Errors,
                        Warnings
                    )
                )};
        Other ->
            {error,
                catena_backend_error:beam_compilation_failed(
                    [{unexpected_core_compiler_result, Other}],
                    [],
                    Context#{stage => beam_compilation}
                )}
    end.

expected_runtime_module(
    Unit,
    RuntimeModule,
    Binary,
    CoreModule,
    Warnings,
    Origins
) ->
    ExpectedRuntimeModule = catena_compilation_unit:runtime_module(Unit),
    case RuntimeModule =:= ExpectedRuntimeModule of
        true ->
            {ok,
                build_artifact(
                    Unit,
                    RuntimeModule,
                    Binary,
                    CoreModule,
                    Warnings,
                    Origins
                )};
        false ->
            Context = artifact_context(Unit),
            {error,
                catena_backend_error:beam_compilation_failed(
                    [{
                        runtime_module_mismatch,
                        ExpectedRuntimeModule,
                        RuntimeModule
                    }],
                    Warnings,
                    Context#{stage => beam_compilation}
                )}
    end.

build_artifact(
    Unit,
    RuntimeModule,
    Binary,
    CoreModule,
    Warnings,
    Origins
) ->
    SourceModule = catena_compilation_unit:module_name(Unit),
    #{
        format => catena_beam_artifact,
        format_version => ?ARTIFACT_VERSION,
        module_identity => #{
            source_module => SourceModule,
            runtime_module => RuntimeModule
        },
        source_module => SourceModule,
        runtime_module => RuntimeModule,
        beam => Binary,
        core => CoreModule,
        source_identity => catena_compilation_unit:source_identity(Unit),
        runtime_dependencies =>
            catena_compilation_unit:runtime_dependencies(Unit),
        artifact_dependencies =>
            catena_compilation_unit:artifact_dependencies(Unit),
        warnings => Warnings,
        interface => catena_compilation_unit:interface(Unit),
        runtime_contract => runtime_contract(Unit),
        metadata => #{
            compiler_options => catena_compilation_unit:options(Unit),
            validation => #{
                frontend =>
                    catena_compilation_unit:validation_state(Unit),
                core_lint => passed,
                otp_from_core => passed
            },
            origins => Origins,
            interface_checksum => catena_module_interface:checksum(
                catena_compilation_unit:interface(Unit)
            )
        }
    }.

runtime_contract(Unit) ->
    SourceModule = catena_compilation_unit:module_name(Unit),
    RuntimeModule = catena_compilation_unit:runtime_module(Unit),
    Resumable = lists:any(
        fun(Entry) -> maps:get(mode, Entry) =:= resumable end,
        catena_control_mode:entries(
            catena_compilation_unit:control_modes(Unit)
        )
    ),
    #{
        artifact_format_version => ?ARTIFACT_VERSION,
        control_abi_version => case Resumable of
            true -> catena_control_codegen:control_abi_version();
            false -> none
        end,
        resumption_runtime_version => case Resumable of
            true -> catena_resumption_runtime:version();
            false -> none
        end,
        required_handler_frame_features => case Resumable of
            true -> catena_resumption_runtime:features();
            false -> []
        end,
        source_module => SourceModule,
        runtime_module => RuntimeModule,
        dependency_checksums => [
            #{
                source_module => maps:get(source_module, Dependency),
                interface_version => maps:get(interface_version, Dependency),
                interface_checksum => maps:get(interface_checksum, Dependency)
            }
            || Dependency <- catena_compilation_unit:artifact_dependencies(Unit),
               maps:get(kind, Dependency, runtime) =:= catena_module,
               maps:is_key(interface_checksum, Dependency)
        ]
    }.

validate_envelope(#{
    format := catena_beam_artifact,
    format_version := ?ARTIFACT_VERSION,
    module_identity := #{
        source_module := SourceModule,
        runtime_module := RuntimeModule
    },
    source_module := SourceModule,
    runtime_module := RuntimeModule,
    beam := Beam,
    runtime_dependencies := RuntimeDependencies,
    artifact_dependencies := ArtifactDependencies,
    interface := Interface,
    runtime_contract := Contract = #{
        artifact_format_version := ?ARTIFACT_VERSION,
        source_module := SourceModule,
        runtime_module := RuntimeModule
    },
    metadata := Metadata
}) when
    is_atom(SourceModule),
    is_atom(RuntimeModule),
    is_binary(Beam),
    is_list(RuntimeDependencies),
    is_list(ArtifactDependencies)
->
    case catena_module_interface:is_interface(Interface) of
        false -> {error, invalid_module_interface};
        true ->
            case validate_interface_contract(
                Interface,
                SourceModule,
                RuntimeModule,
                ArtifactDependencies,
                Contract,
                Metadata
            ) of
                ok ->
                    case validate_control_contract(
                        Contract,
                        RuntimeDependencies
                    ) of
                        ok -> validate_beam_identity(Beam, RuntimeModule);
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error -> Error
            end
    end;
validate_envelope(#{format_version := Version}) when
    Version =/= ?ARTIFACT_VERSION
->
    {error, {artifact_version_mismatch, ?ARTIFACT_VERSION, Version}};
validate_envelope(_Artifact) ->
    {error, invalid_artifact_envelope}.

validate_control_contract(#{
    control_abi_version := none,
    resumption_runtime_version := none,
    required_handler_frame_features := []
}, _RuntimeDependencies) ->
    ok;
validate_control_contract(#{
    control_abi_version := ControlVersion,
    resumption_runtime_version := ResumptionVersion,
    required_handler_frame_features := Features
}, RuntimeDependencies) ->
    ExpectedControl = catena_control_codegen:control_abi_version(),
    ExpectedFeatures = catena_resumption_runtime:features(),
    MatchingRuntime = lists:any(
        fun(Dependency) ->
            maps:get(module, Dependency, undefined) =:=
                catena_resumption_runtime andalso
                maps:get(version, Dependency, undefined) =:=
                    ResumptionVersion andalso
                maps:get(features, Dependency, []) =:= Features
        end,
        RuntimeDependencies
    ),
    case {
        ControlVersion =:= ExpectedControl,
        ResumptionVersion =:= catena_resumption_runtime:version(),
        Features =:= ExpectedFeatures,
        MatchingRuntime
    } of
        {true, true, true, true} -> ok;
        {false, _, _, _} ->
            {error, {control_abi_version_mismatch,
                ExpectedControl, ControlVersion}};
        {_, false, _, _} ->
            {error, {resumption_runtime_version_mismatch,
                catena_resumption_runtime:version(), ResumptionVersion}};
        {_, _, false, _} ->
            {error, {handler_frame_feature_mismatch,
                ExpectedFeatures, Features}};
        {_, _, _, false} ->
            {error, inconsistent_resumption_runtime_dependency}
    end;
validate_control_contract(_Contract, _RuntimeDependencies) ->
    {error, invalid_control_runtime_contract}.

validate_interface_contract(
    Interface,
    SourceModule,
    RuntimeModule,
    ArtifactDependencies,
    Contract,
    Metadata
) ->
    InterfaceChecksum = catena_module_interface:checksum(Interface),
    ExpectedDependencies = dependency_checksum_manifest(
        ArtifactDependencies
    ),
    case {
        catena_module_interface:source_module(Interface) =:= SourceModule,
        catena_module_interface:runtime_module(Interface) =:= RuntimeModule,
        maps:get(interface_checksum, Metadata, missing) =:= InterfaceChecksum,
        maps:get(dependency_checksums, Contract, missing) =:=
            ExpectedDependencies
    } of
        {true, true, true, true} -> ok;
        {false, _, _, _} -> {error, interface_source_identity_mismatch};
        {_, false, _, _} -> {error, interface_runtime_identity_mismatch};
        {_, _, false, _} -> {error, artifact_interface_checksum_mismatch};
        {_, _, _, false} -> {error, dependency_checksum_manifest_mismatch}
    end.

dependency_checksum_manifest(ArtifactDependencies) ->
    [
        #{
            source_module => maps:get(source_module, Dependency),
            interface_version => maps:get(interface_version, Dependency),
            interface_checksum => maps:get(interface_checksum, Dependency)
        }
        || Dependency <- ArtifactDependencies,
           maps:get(kind, Dependency, runtime) =:= catena_module,
           maps:is_key(interface_checksum, Dependency)
    ].

validate_beam_identity(Beam, ExpectedModule) ->
    case beam_lib:chunks(Beam, [attributes]) of
        {ok, {ExpectedModule, _Chunks}} -> ok;
        {ok, {ActualModule, _Chunks}} ->
            {error, {beam_module_identity_mismatch, ExpectedModule, ActualModule}};
        {error, Reason} ->
            {error, {invalid_beam_binary, Reason}}
    end.

validate_artifact_dependencies(Artifact, Options, Context) ->
    AvailableArtifacts = maps:get(available_artifacts, Options, #{}),
    Dependencies = maps:get(artifact_dependencies, Artifact),
    case validate_dependency_checksums(Dependencies, AvailableArtifacts) of
        ok -> {ok, Artifact};
        {error, Reason} ->
            {error, catena_backend_error:artifact_validation_failed(
                Reason,
                Context#{stage => artifact_load}
            )}
    end.

validate_dependency_checksums(_Dependencies, Available) when map_size(Available) =:= 0 ->
    ok;
validate_dependency_checksums([], _Available) ->
    ok;
validate_dependency_checksums([#{
    kind := catena_module,
    source_module := Module,
    interface_checksum := Expected
} | Rest], Available) ->
    case maps:find(Module, Available) of
        {ok, #{interface := Interface}} ->
            Actual = catena_module_interface:checksum(Interface),
            case Actual =:= Expected of
                true -> validate_dependency_checksums(Rest, Available);
                false -> {error, {dependency_checksum_mismatch,
                    Module, Expected, Actual}}
            end;
        {ok, Interface} ->
            Actual = catena_module_interface:checksum(Interface),
            case Actual =:= Expected of
                true -> validate_dependency_checksums(Rest, Available);
                false -> {error, {dependency_checksum_mismatch,
                    Module, Expected, Actual}}
            end;
        error ->
            {error, {missing_artifact_dependency, Module}}
    end;
validate_dependency_checksums([_Dependency | Rest], Available) ->
    validate_dependency_checksums(Rest, Available).

artifact_validation_context(Artifact) ->
    #{
        module => maps:get(source_module, Artifact),
        runtime_module => maps:get(runtime_module, Artifact),
        source_identity => maps:get(source_identity, Artifact, undefined),
        construct => beam_artifact
    }.

envelope_context(Artifact) when is_map(Artifact) ->
    #{
        stage => artifact_load,
        module => maps:get(source_module, Artifact, undefined),
        runtime_module => maps:get(runtime_module, Artifact, undefined),
        source_identity => maps:get(source_identity, Artifact, undefined)
    };
envelope_context(_Artifact) ->
    #{stage => artifact_load}.

artifact_context(Unit) ->
    #{
        module => catena_compilation_unit:module_name(Unit),
        runtime_module => catena_compilation_unit:runtime_module(Unit),
        source_identity => catena_compilation_unit:source_identity(Unit),
        locations => catena_compilation_unit:locations(Unit)
    }.

failure_context(Context, NormalizedErrors, Stage, Errors, Warnings) ->
    Closest = case NormalizedErrors of
        [First | _] ->
            maps:with(
                [
                    module,
                    transform,
                    generated_identity,
                    location,
                    origin
                ],
                First
            );
        [] ->
            #{}
    end,
    (maps:merge(Context, Closest))#{
        stage => Stage,
        otp_errors => Errors,
        otp_warnings => Warnings
    }.
