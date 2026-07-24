%%%-------------------------------------------------------------------
%%% @doc Normalize OTP Core/BEAM diagnostics into Catena vocabulary.
%%%
%%% The original OTP detail is retained under `otp_detail`; callers can rely
%%% on the surrounding stable fields without depending on OTP's nested tuple
%%% representation.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_artifact_diagnostic).

-export([normalize/4]).

-type severity() :: error | warning.
-type stage() :: core_validation | beam_compilation.
-type diagnostic() :: #{
    kind := catena_artifact_diagnostic,
    severity := severity(),
    stage := stage(),
    category := catena_backend_error:category(),
    module => atom() | undefined,
    transform => atom() | undefined,
    generated_identity => term(),
    source_identity => term(),
    origin => user | synthetic,
    construct => atom(),
    location => term(),
    otp_detail := term()
}.

-export_type([diagnostic/0]).

-spec normalize(stage(), severity(), term(), map()) -> [diagnostic()].
normalize(Stage, Severity, Diagnostics, Context) ->
    [
        normalize_one(Stage, Severity, Detail, Context)
        || Detail <- diagnostic_groups(Diagnostics),
           not empty_group(Detail)
    ].

normalize_one(Stage, Severity, Detail, Context) ->
    Origin = catena_core_origin:nearest(Detail, Context),
    #{
        kind => catena_artifact_diagnostic,
        severity => Severity,
        stage => Stage,
        category => category(Stage),
        module => maps:get(module, Origin, maps:get(module, Context, undefined)),
        transform => maps:get(transform, Origin, undefined),
        generated_identity =>
            maps:get(generated_identity, Origin, undefined),
        source_identity => maps:get(source_identity, Context, undefined),
        origin => maps:get(origin, Origin, user),
        construct => maps:get(construct, Origin, module),
        location => maps:get(location, Origin, undefined),
        otp_detail => Detail
    }.

category(core_validation) -> core_validation_failed;
category(beam_compilation) -> beam_compilation_failed.

diagnostic_groups(Diagnostics) when is_list(Diagnostics) ->
    Diagnostics;
diagnostic_groups(Diagnostic) ->
    [Diagnostic].

empty_group({_Source, []}) -> true;
empty_group([]) -> true;
empty_group(_) -> false.
