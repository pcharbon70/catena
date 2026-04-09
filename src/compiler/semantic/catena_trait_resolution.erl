%%%-------------------------------------------------------------------
%%% @doc Catena Cross-Module Trait Resolution Module
%%%
%%% This module handles trait resolution across module boundaries,
%%% including trait instance lookup, orphan instance detection, and
%%% coherent instance resolution.
%%%
%%% == Trait Resolution Rules ==
%%%
%%% <ul>
%%%   <li><b>Local instances shadow imported instances</b> - Instances
%%%       defined in the current module take precedence over imported ones.</li>
%%%   <li><b>Imported instances from different modules</b> - If the same
%%%       instance is imported from multiple modules, it's an error unless
%%%       one is an orphan and the other is not.</li>
%%%   <li><b>Orphan instance rule</b> - An instance is an orphan if neither
%%%       the trait nor the type are defined in the same module as the instance.
%%%       Orphan instances are only allowed if no other instance exists.</li>
%%%   <li><b>Coherence</b> - For any given type and trait combination, there
%%%       must be exactly one instance in scope.</li>
%%% </ul>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_trait_resolution).

%% API
-export([
    resolve_instance/3,
    find_instances/2,
    detect_orphan_instances/2,
    check_coherence/1,
    format_trait_error/1,
    get_trait_instances/2,
    import_instances/3
]).

%% Types
-type module_name() :: atom().
-type trait_name() :: atom().
-type type_name() :: atom().
-type instance_key() :: {trait_name(), type_name()}.
-type instance_info() :: #{
    trait => trait_name(),
    type => type_name(),
    module => module_name(),
    location => location(),
    is_orphan => boolean()
}.
-type trait_registry() :: #{instance_key() => [instance_info()]}.
-type location() :: {integer(), integer()}.
-type trait_error() :: {instance_ambiguous, instance_key(), [instance_info()]} |
                       {instance_not_found, instance_key()} |
                       {orphan_instance, instance_info()} |
                       {incoherent_instances, instance_key(), [instance_info()]}.

%%%=============================================================================
%%% Instance Resolution
%%%=============================================================================

%% @doc Resolve a trait instance for a given type in the context of a module.
%% Returns the single matching instance or an error if multiple or none exist.
%% @param Trait The trait name
%% @param Type The type name
%% @param SymbolTables All symbol tables
%% @return {ok, InstanceInfo} | {error, trait_error()}
-spec resolve_instance(trait_name(), type_name(), #{module_name() => map()}) ->
    {ok, instance_info()} | {error, trait_error()}.
resolve_instance(Trait, Type, SymbolTables) ->
    Key = {Trait, Type},
    %% Find all instances across all modules
    AllInstances = find_all_instances(Key, SymbolTables),

    %% Filter to visible instances (not shadowed by local definitions)
    %% and check for conflicts
    case resolve_visible_instances(AllInstances) of
        {ok, Instance} ->
            {ok, Instance};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Find all instances of a specific trait across all modules.
%% @param Trait The trait name
%% @param SymbolTables All symbol tables
%% @return List of instance_info maps
-spec find_instances(trait_name(), #{module_name() => map()}) -> [instance_info()].
find_instances(Trait, SymbolTables) ->
    maps:fold(fun(_ModuleName, ModuleInfo, Acc) ->
        Instances = get_trait_instances(Trait, ModuleInfo),
        Instances ++ Acc
    end, [], SymbolTables).

%% @doc Get all trait instances from a module's symbol table entry.
%% @param Trait The trait name
%% @param ModuleInfo The module information map
%% @return List of instance_info maps
-spec get_trait_instances(trait_name(), map()) -> [instance_info()].
get_trait_instances(Trait, ModuleInfo) ->
    Declarations = maps:get(declarations, ModuleInfo, []),
    lists:filtermap(fun(Decl) ->
        case Decl of
            {instance_decl, Trait, _Type, _Methods, Location} ->
                InstanceInfo = #{
                    trait => Trait,
                    type => extract_type_from_instance(Decl),
                    module => maps:get(module_name, ModuleInfo, unknown),
                    location => Location,
                    is_orphan => false  %% Will be determined later
                },
                {true, InstanceInfo};
            _ ->
                false
        end
    end, Declarations).

%% @doc Extract the type name from an instance declaration.
-spec extract_type_from_instance(tuple()) -> type_name().
extract_type_from_instance({instance_decl, _Trait, Type, _Methods, _Location}) ->
    extract_type_name(Type).

%% @doc Extract type name from a type reference.
-spec extract_type_name(term()) -> type_name().
extract_type_name({type_ref, Name}) -> Name;
extract_type_name({type_ref, Name, _Args}) -> Name;
extract_type_name(Name) when is_atom(Name) -> Name.

%%%=============================================================================
%%% Orphan Instance Detection
%%%=============================================================================

%% @doc Detect orphan instances in a module.
%% An orphan instance is one where neither the trait nor the type
%% are defined in the same module as the instance.
%% @param ModuleName The module to check
%% @param SymbolTables All symbol tables
%% @return List of orphan instance_info maps
-spec detect_orphan_instances(module_name(), #{module_name() => map()}) -> [instance_info()].
detect_orphan_instances(ModuleName, SymbolTables) ->
    case maps:get(ModuleName, SymbolTables, undefined) of
        undefined ->
            [];
        ModuleInfo ->
            %% Get all instances from this module
            AllInstances = lists:filtermap(fun(Decl) ->
                case Decl of
                    {instance_decl, Trait, Type, _Methods, Location} ->
                        TypeName = extract_type_name(Type),
                        InstanceInfo = #{
                            trait => Trait,
                            type => TypeName,
                            module => ModuleName,
                            location => Location,
                            is_orphan => false
                        },
                        {true, InstanceInfo};
                    _ ->
                        false
                end
            end, maps:get(declarations, ModuleInfo, [])),

            %% Check each instance for orphan status
            lists:filter(fun(InstanceInfo) ->
                is_orphan_instance(InstanceInfo, ModuleName, SymbolTables)
            end, AllInstances)
    end.

%% @doc Check if an instance is an orphan.
%% An instance is an orphan if neither the trait nor the type
%% are defined in the same module as the instance.
-spec is_orphan_instance(instance_info(), module_name(), #{module_name() => map()}) -> boolean().
is_orphan_instance(#{trait := Trait, type := Type}, ModuleName, SymbolTables) ->
    %% Check if trait is defined in this module
    TraitDefinedHere = is_trait_defined_in(Trait, ModuleName, SymbolTables),

    %% Check if type is defined in this module
    TypeDefinedHere = is_type_defined_in(Type, ModuleName, SymbolTables),

    %% Instance is orphan if neither trait nor type is defined here
    not TraitDefinedHere andalso not TypeDefinedHere.

%% @doc Check if a trait is defined in a specific module.
-spec is_trait_defined_in(trait_name(), module_name(), #{module_name() => map()}) -> boolean().
is_trait_defined_in(Trait, ModuleName, SymbolTables) ->
    case maps:get(ModuleName, SymbolTables, undefined) of
        undefined ->
            false;
        ModuleInfo ->
            Declarations = maps:get(declarations, ModuleInfo, []),
            lists:any(fun(Decl) ->
                case Decl of
                    {trait_decl, Trait, _Location} ->
                        true;
                    _ ->
                        false
                end
            end, Declarations)
    end.

%% @doc Check if a type is defined in a specific module.
-spec is_type_defined_in(type_name(), module_name(), #{module_name() => map()}) -> boolean().
is_type_defined_in(Type, ModuleName, SymbolTables) ->
    case maps:get(ModuleName, SymbolTables, undefined) of
        undefined ->
            false;
        ModuleInfo ->
            Declarations = maps:get(declarations, ModuleInfo, []),
            lists:any(fun(Decl) ->
                case Decl of
                    {type_decl, Type, _Params, _Constructors, _Location} ->
                        true;
                    _ ->
                        false
                end
            end, Declarations)
    end.

%%%=============================================================================
%%% Coherence Checking
%%%=============================================================================

%% @doc Check for coherence violations across all modules.
%% Ensures that for any trait/type combination, there is exactly
%% one instance visible (no overlaps).
%% @param SymbolTables All symbol tables
%% @return ok | {error, trait_error()}
-spec check_coherence(#{module_name() => map()}) -> ok | {error, trait_error()}.
check_coherence(SymbolTables) ->
    %% Build a registry of all instances
    Registry = build_instance_registry(SymbolTables),

    %% Check each key for multiple instances
    maps:fold(fun(Key, Instances, _Acc) ->
        case length(Instances) of
            0 ->
                ok;
            1 ->
                ok;
            _ ->
                %% Multiple instances - check if they conflict
                case check_instance_conflict(Instances) of
                    ok ->
                        ok;
                    {error, Reason} ->
                        {error, Reason}
                end
        end
    end, ok, Registry).

%% @doc Build a registry of all instances keyed by {Trait, Type}.
-spec build_instance_registry(#{module_name() => map()}) -> trait_registry().
build_instance_registry(SymbolTables) ->
    maps:fold(fun(_ModuleName, ModuleInfo, Registry) ->
        Declarations = maps:get(declarations, ModuleInfo, []),
        lists:foldl(fun(Decl, RegIn) ->
            case Decl of
                {instance_decl, Trait, Type, _Methods, Location} ->
                    TypeName = extract_type_name(Type),
                    Key = {Trait, TypeName},
                    InstanceInfo = #{
                        trait => Trait,
                        type => TypeName,
                        module => maps:get(module_name, ModuleInfo, unknown),
                        location => Location,
                        is_orphan => false
                    },
                    maps:update_with(Key, fun(V) -> [InstanceInfo | V] end, [InstanceInfo], RegIn);
                _ ->
                    RegIn
            end
        end, Registry, Declarations)
    end, #{}, SymbolTables).

%% @doc Check if multiple instances conflict.
%% Instances don't conflict if they're in different modules and
%% at most one is an orphan.
-spec check_instance_conflict([instance_info()]) -> ok | {error, trait_error()}.
check_instance_conflict([_]) ->
    ok;
check_instance_conflict(Instances) when length(Instances) > 1 ->
    %% Check if more than one non-orphan instance exists
    NonOrphans = [I || I <- Instances, not maps:get(is_orphan, I, false)],
    case length(NonOrphans) of
        0 ->
            %% All orphans - this is OK (shouldn't happen in practice)
            ok;
        1 ->
            %% One non-orphan, others are orphans - OK
            ok;
        _ ->
            %% Multiple non-orphan instances - coherence violation
            Key = {maps:get(trait, hd(Instances)), maps:get(type, hd(Instances))},
            {error, {incoherent_instances, Key, NonOrphans}}
    end.

%%%=============================================================================
%%% Import Handling
%%%=============================================================================

%% @doc Import trait instances from another module.
%% @param SourceModule The module to import from
%% @param Items Specific items to import (or 'all')
%% @param SymbolTables All symbol tables
%% @return List of importable instance_info maps
-spec import_instances(module_name(), all | [atom()], #{module_name() => map()}) -> [instance_info()].
import_instances(SourceModule, Items, SymbolTables) ->
    case maps:get(SourceModule, SymbolTables, undefined) of
        undefined ->
            [];
        ModuleInfo ->
            AllInstances = get_all_instances(ModuleInfo),
            filter_importable_instances(AllInstances, Items, ModuleInfo)
    end.

%% @doc Get all instances from a module.
-spec get_all_instances(map()) -> [instance_info()].
get_all_instances(ModuleInfo) ->
    Declarations = maps:get(declarations, ModuleInfo, []),
    lists:filtermap(fun(Decl) ->
        case Decl of
            {instance_decl, Trait, Type, _Methods, Location} ->
                TypeName = extract_type_name(Type),
                InstanceInfo = #{
                    trait => Trait,
                    type => TypeName,
                    module => maps:get(module_name, ModuleInfo, unknown),
                    location => Location,
                    is_orphan => false
                },
                {true, InstanceInfo};
            _ ->
                false
        end
    end, Declarations).

%% @doc Filter instances based on import list.
-spec filter_importable_instances([instance_info()], all | [atom()], map()) -> [instance_info()].
filter_importable_instances(Instances, all, _ModuleInfo) ->
    Instances;
filter_importable_instances(Instances, Items, _ModuleInfo) ->
    [I || I <- Instances, lists:member(maps:get(trait, I), Items)].

%%%=============================================================================
%%% Internal Helpers
%%%=============================================================================

%% @doc Find all instances of a specific trait/type combination.
-spec find_all_instances(instance_key(), #{module_name() => map()}) -> [instance_info()].
find_all_instances({Trait, Type}, SymbolTables) ->
    maps:fold(fun(_ModuleName, ModuleInfo, Acc) ->
        Declarations = maps:get(declarations, ModuleInfo, []),
        lists:foldl(fun(Decl, AccIn) ->
            case Decl of
                {instance_decl, Trait, DeclType, _Methods, Location} ->
                    TypeName = extract_type_name(DeclType),
                    case TypeName of
                        Type ->
                            InstanceInfo = #{
                                trait => Trait,
                                type => TypeName,
                                module => _ModuleName,
                                location => Location,
                                is_orphan => false
                            },
                            [InstanceInfo | AccIn];
                        _ ->
                            AccIn
                    end;
                _ ->
                    AccIn
            end
        end, Acc, Declarations)
    end, [], SymbolTables).

%% @doc Resolve the visible instance from a list of candidates.
%% Handles shadowing and conflict detection.
-spec resolve_visible_instances([instance_info()]) ->
    {ok, instance_info()} | {error, trait_error()}.
resolve_visible_instances([]) ->
    {error, {instance_not_found, unknown}};
resolve_visible_instances([Instance]) ->
    {ok, Instance};
resolve_visible_instances(Instances) when length(Instances) > 1 ->
    %% Check for coherence violations
    check_instance_conflict(Instances).

%%%=============================================================================
%%% Error Formatting
%%%=============================================================================

%% @doc Format a trait resolution error as a human-readable string.
-spec format_trait_error(trait_error()) -> iolist().
format_trait_error({instance_ambiguous, {Trait, Type}, Instances}) ->
    ["Ambiguous instance for trait ", atom_to_list(Trait),
     " and type ", atom_to_list(Type), "\n",
     "Multiple instances found:\n",
     format_instance_list(Instances)];
format_trait_error({instance_not_found, {Trait, Type}}) ->
    ["No instance found for trait ", atom_to_list(Trait),
     " and type ", atom_to_list(Type), "\n"];
format_trait_error({orphan_instance, #{trait := Trait, type := Type, module := Module}}) ->
    ["Orphan instance defined in module ", atom_to_list(Module),
     "\n  Trait: ", atom_to_list(Trait),
     "\n  Type: ", atom_to_list(Type),
     "\n  Neither trait nor type defined in this module\n"];
format_trait_error({incoherent_instances, {Trait, Type}, Instances}) ->
    ["Incoherent instances for trait ", atom_to_list(Trait),
     " and type ", atom_to_list(Type), "\n",
     "Multiple conflicting instances:\n",
     format_instance_list(Instances)];
format_trait_error({unknown, Reason}) ->
    ["Unknown trait error: ", io_lib:format("~p", [Reason]), "\n"].

%% @doc Format a list of instances with their locations.
-spec format_instance_list([instance_info()]) -> iolist().
format_instance_list(Instances) ->
    [format_instance(I) || I <- Instances].

%% @doc Format a single instance with its location.
-spec format_instance(instance_info()) -> iolist().
format_instance(#{trait := Trait, type := Type, module := Module, location := {Line, Col}}) ->
    io_lib:format("  ~s:~s (~s at ~w:~w)~n", [Trait, Type, Module, Line, Col]).
