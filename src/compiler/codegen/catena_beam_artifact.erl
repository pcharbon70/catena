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
    from_unit/1,
    validate_core/2,
    compile_core/2
]).

-define(ARTIFACT_VERSION, 1).

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
    metadata := map()
}.

-export_type([artifact/0]).

%% @doc Generate, validate, and compile an artifact from a validated unit.
-spec from_unit(catena_compilation_unit:t()) ->
    {ok, artifact()} | {error, term()}.
from_unit(Unit) ->
    case catena_compilation_unit:is_compilation_unit(Unit) of
        true ->
            case catena_codegen_module:generate_validated_module(Unit) of
                {ok, CoreModule} ->
                    Context = artifact_context(Unit),
                    case validate_core(CoreModule, Context) of
                        {ok, CoreWarnings} ->
                            case compile_core(CoreModule, Context) of
                                {ok, RuntimeModule, Binary, BeamWarnings} ->
                                    expected_runtime_module(
                                        Unit,
                                        RuntimeModule,
                                        Binary,
                                        CoreModule,
                                        CoreWarnings ++ BeamWarnings
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

%% @doc Run OTP Core lint as an explicit artifact acceptance boundary.
-spec validate_core(cerl:cerl(), map()) ->
    {ok, [term()]} | {error, catena_backend_error:diagnostic()}.
validate_core(CoreModule, Context) when is_map(Context) ->
    case core_lint:module(CoreModule) of
        {ok, Warnings} ->
            {ok, nonempty_diagnostics(Warnings)};
        {error, Errors, Warnings} ->
            {error,
                catena_backend_error:core_validation_failed(
                    Errors,
                    nonempty_diagnostics(Warnings),
                    Context#{stage => core_validation}
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
            {ok, Module, Binary, nonempty_diagnostics(Warnings)};
        {error, Errors, Warnings} ->
            {error,
                catena_backend_error:beam_compilation_failed(
                    Errors,
                    nonempty_diagnostics(Warnings),
                    Context#{stage => beam_compilation}
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
    Warnings
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
                    Warnings
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

build_artifact(Unit, RuntimeModule, Binary, CoreModule, Warnings) ->
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
        metadata => #{
            compiler_options => catena_compilation_unit:options(Unit),
            validation => #{
                frontend =>
                    catena_compilation_unit:validation_state(Unit),
                core_lint => passed,
                otp_from_core => passed
            }
        }
    }.

artifact_context(Unit) ->
    #{
        module => catena_compilation_unit:module_name(Unit),
        runtime_module => catena_compilation_unit:runtime_module(Unit),
        source_identity => catena_compilation_unit:source_identity(Unit),
        locations => catena_compilation_unit:locations(Unit)
    }.

nonempty_diagnostics(Diagnostics) when is_list(Diagnostics) ->
    [
        Diagnostic
        || Diagnostic <- Diagnostics,
           not empty_diagnostic(Diagnostic)
    ];
nonempty_diagnostics(Diagnostic) ->
    [Diagnostic].

empty_diagnostic({_Source, []}) ->
    true;
empty_diagnostic(_) ->
    false.
