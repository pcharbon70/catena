%%%-------------------------------------------------------------------
%%% @doc Version and feature validation for generated Catena artifacts.
%%%
%%% Runtime dependencies are exact ABI contracts.  Feature requirements are
%%% checked as a subset so a compatible runtime may advertise additional
%%% capabilities without invalidating older artifacts.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_runtime_contract).

-export([validate/1, validate/2, validate/3]).

-type availability() ::
    auto |
    all |
    [atom()] |
    #{atom() => pos_integer() | map()}.

-spec validate([map()]) -> ok | {error, catena_backend_error:diagnostic()}.
validate(Dependencies) ->
    validate(Dependencies, auto, #{}).

-spec validate([map()], availability()) ->
    ok | {error, catena_backend_error:diagnostic()}.
validate(Dependencies, Available) ->
    validate(Dependencies, Available, #{}).

-spec validate([map()], availability(), map()) ->
    ok | {error, catena_backend_error:diagnostic()}.
validate(Dependencies, Available, Context)
        when is_list(Dependencies), is_map(Context) ->
    validate_dependencies(Dependencies, Available, Context).

validate_dependencies([], _Available, _Context) ->
    ok;
validate_dependencies([Dependency | Rest], Available, Context) ->
    case validate_dependency(Dependency, Available) of
        ok ->
            validate_dependencies(Rest, Available, Context);
        {error, Reason, Actual} ->
            Module = maps:get(module, Dependency),
            Version = maps:get(version, Dependency),
            {error, catena_backend_error:runtime_dependency_unavailable(
                Module,
                Version,
                Context#{
                    reason => Reason,
                    actual_runtime_contract => Actual,
                    required_features => maps:get(features, Dependency, [])
                }
            )}
    end.

validate_dependency(_Dependency, all) ->
    ok;
validate_dependency(Dependency, auto) ->
    Module = maps:get(module, Dependency),
    case code:ensure_loaded(Module) of
        {module, Module} ->
            validate_loaded_dependency(Dependency);
        {error, Reason} ->
            {error, missing_runtime, Reason}
    end;
validate_dependency(Dependency, Available) when is_list(Available) ->
    Module = maps:get(module, Dependency),
    case lists:member(Module, Available) of
        true -> ok;
        false -> {error, missing_runtime, Available}
    end;
validate_dependency(Dependency, Available) when is_map(Available) ->
    Module = maps:get(module, Dependency),
    case maps:find(Module, Available) of
        {ok, Version} when is_integer(Version) ->
            validate_contract(Dependency, Version, []);
        {ok, Contract} when is_map(Contract) ->
            validate_contract(
                Dependency,
                maps:get(version, Contract, missing),
                maps:get(features, Contract, [])
            );
        error ->
            {error, missing_runtime, unavailable};
        {ok, Other} ->
            {error, invalid_runtime_contract, Other}
    end;
validate_dependency(_Dependency, Available) ->
    {error, invalid_runtime_availability, Available}.

validate_loaded_dependency(Dependency) ->
    Module = maps:get(module, Dependency),
    case erlang:function_exported(Module, version, 0) of
        false ->
            {error, missing_runtime_version, Module};
        true ->
            Version = Module:version(),
            Features = case erlang:function_exported(Module, features, 0) of
                true -> Module:features();
                false -> []
            end,
            validate_contract(Dependency, Version, Features)
    end.

validate_contract(Dependency, ActualVersion, ActualFeatures) ->
    RequiredVersion = maps:get(version, Dependency),
    RequiredFeatures = maps:get(features, Dependency, []),
    case ActualVersion =:= RequiredVersion of
        false ->
            {error, incompatible_runtime_version, #{
                version => ActualVersion,
                features => ActualFeatures
            }};
        true ->
            Missing = RequiredFeatures -- ActualFeatures,
            case Missing of
                [] -> ok;
                _ ->
                    {error, missing_runtime_features, #{
                        version => ActualVersion,
                        features => ActualFeatures,
                        missing_features => Missing
                    }}
            end
    end.
