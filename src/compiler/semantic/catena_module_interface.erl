%%%-------------------------------------------------------------------
%%% @doc Versioned executable interface metadata for a Catena module.
%%%
%%% Interfaces contain only names that downstream compilation is allowed to
%%% resolve.  They retain declarations needed for type checking and trait
%%% validation, while callable entries include the exact runtime module and
%%% arity required by Core Erlang generation.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_module_interface).

-export([
    build/7,
    is_interface/1,
    source_module/1,
    runtime_module/1,
    version/0,
    checksum/1,
    exported_symbols/1,
    find_export/3,
    artifact_dependencies/1,
    with_control_modes/2,
    with_artifact_dependencies/2
]).

-define(INTERFACE_VERSION, 2).

-type interface() :: map().
-export_type([interface/0]).

-spec version() -> pos_integer().
version() -> ?INTERFACE_VERSION.

%% @doc Stable digest used to bind consumers to the exact interface compiled.
-spec checksum(interface()) -> binary().
checksum(Interface) ->
    true = is_interface(Interface),
    crypto:hash(sha256, term_to_binary(Interface, [deterministic])).

-spec build(atom(), [term()], [term()], [map()], [map()], term(),
    catena_trait_dictionary:inventory()) ->
    {ok, interface()} | {error, term()}.
build(
    Module,
    Exports,
    Declarations,
    Symbols,
    ArtifactDependencies,
    SourceIdentity,
    TraitInventory
) ->
    case catena_module_identity:normalize(Module) of
        {ok, Identity} ->
            ExportPolicy = export_policy(Exports),
            PublishedSymbols = publish_symbols(
                Symbols,
                Declarations,
                ExportPolicy,
                Identity
            ),
            {ok, #{
                '$catena_module_interface' => ?INTERFACE_VERSION,
                source_module => Module,
                runtime_module => maps:get(runtime_module, Identity),
                identity => Identity,
                source_identity => SourceIdentity,
                exports => PublishedSymbols,
                transforms => by_kind(transform, PublishedSymbols),
                constructors => by_kind(constructor, PublishedSymbols),
                effects => declaration_metadata(
                    effect_decl,
                    Declarations,
                    ExportPolicy
                ),
                traits => declaration_metadata(
                    trait_decl,
                    Declarations,
                    ExportPolicy
                ),
                instances => instance_metadata(Declarations, Module),
                dictionaries =>
                    catena_trait_dictionary:public_dictionaries(
                        TraitInventory
                    ),
                artifact_dependencies =>
                    normalize_dependencies(ArtifactDependencies)
            }};
        {error, _} = Error ->
            Error
    end.

-spec is_interface(term()) -> boolean().
is_interface(#{
    '$catena_module_interface' := ?INTERFACE_VERSION,
    source_module := Source,
    runtime_module := Runtime,
    exports := Exports,
    artifact_dependencies := Dependencies
}) ->
    is_atom(Source) andalso
        is_atom(Runtime) andalso
        is_list(Exports) andalso
        is_list(Dependencies);
is_interface(_) ->
    false.

-spec source_module(interface()) -> atom().
source_module(Interface) ->
    maps:get(source_module, Interface).

-spec runtime_module(interface()) -> atom().
runtime_module(Interface) ->
    maps:get(runtime_module, Interface).

-spec exported_symbols(interface()) -> [map()].
exported_symbols(Interface) ->
    maps:get(exports, Interface).

-spec find_export(atom(), atom(), interface()) ->
    {ok, map()} | {error, term()}.
find_export(Kind, Name, Interface) ->
    Matches = [
        Entry
        || Entry <- exported_symbols(Interface),
           maps:get(kind, Entry) =:= Kind,
           maps:get(name, Entry) =:= Name
    ],
    case Matches of
        [Entry] ->
            {ok, Entry};
        [] ->
            {error, {symbol_not_exported, source_module(Interface), Kind, Name}};
        _ ->
            {error, {ambiguous_export, source_module(Interface), Kind, Name}}
    end.

-spec artifact_dependencies(interface()) -> [map()].
artifact_dependencies(Interface) ->
    maps:get(artifact_dependencies, Interface).

-spec with_artifact_dependencies(interface(), [map()]) ->
    {ok, interface()} | {error, term()}.
with_artifact_dependencies(Interface, Dependencies) when is_list(Dependencies) ->
    Candidate = Interface#{
        artifact_dependencies := normalize_dependencies(Dependencies)
    },
    case is_interface(Candidate) of
        true -> {ok, Candidate};
        false -> {error, {invalid_module_interface_dependencies, Dependencies}}
    end;
with_artifact_dependencies(_Interface, Dependencies) ->
    {error, {invalid_module_interface_dependencies, Dependencies}}.

-spec with_control_modes(interface(), catena_control_mode:inventory()) ->
    {ok, interface()} | {error, term()}.
with_control_modes(Interface, Modes) ->
    case {
        is_interface(Interface),
        catena_control_mode:is_inventory(Modes)
    } of
        {true, true} ->
            Exports = [
                attach_control_mode(Export, Modes)
                || Export <- maps:get(exports, Interface)
            ],
            Dictionaries = [
                attach_dictionary_control_modes(Dictionary)
                || Dictionary <- maps:get(dictionaries, Interface, [])
            ],
            {ok, Interface#{
                exports := Exports,
                transforms := by_kind(transform, Exports),
                dictionaries := Dictionaries
            }};
        _ ->
            {error, {invalid_module_interface_control_modes,
                Interface, Modes}}
    end.

attach_control_mode(#{kind := transform, name := Name} = Export, Modes) ->
    case catena_control_mode:mode(Name, Modes) of
        {ok, Mode} -> Export#{control_mode => Mode};
        none -> Export#{control_mode => resumable}
    end;
attach_control_mode(#{kind := trait_method} = Export, _Modes) ->
    Export#{control_mode => resumable};
attach_control_mode(Export, _Modes) ->
    Export.

attach_dictionary_control_modes(Dictionary) ->
    Methods = [
        Method#{control_mode => resumable}
        || Method <- maps:get(methods, Dictionary, [])
    ],
    Dictionary#{methods := Methods}.

export_policy([]) ->
    all;
export_policy(Exports) ->
    sets:from_list([
        {export_kind(Export), export_name(Export)}
        || Export <- Exports
    ]).

export_kind({export_transform, _}) -> transform;
export_kind({export_type, _}) -> type;
export_kind({export_trait, _}) -> trait;
export_kind({export_effect, _}) -> effect;
export_kind(_) -> unknown.

export_name(Export) when is_tuple(Export), tuple_size(Export) >= 2 ->
    element(2, Export);
export_name(_) ->
    undefined.

publish_symbols(Symbols, Declarations, Policy, Identity) ->
    RuntimeModule = maps:get(runtime_module, Identity),
    lists:sort(
        fun symbol_before/2,
        [
            publish_symbol(Symbol, RuntimeModule)
            || Symbol <- Symbols,
               publishable_symbol(Symbol, Declarations, Policy)
        ]
    ).

publishable_symbol(#{kind := transform, name := Name}, _Declarations, Policy) ->
    exported(transform, Name, Policy);
publishable_symbol(
    #{kind := constructor, owner := TypeName},
    _Declarations,
    Policy
) ->
    exported(type, TypeName, Policy);
publishable_symbol(#{kind := trait, name := Name}, _Declarations, Policy) ->
    exported(trait, Name, Policy);
publishable_symbol(
    #{kind := trait_method, owner := Trait},
    _Declarations,
    Policy
) ->
    exported(trait, Trait, Policy);
publishable_symbol(_, _, _) ->
    false.

publish_symbol(Symbol, RuntimeModule) ->
    (maps:with(
        [kind, name, arity, owner, location],
        Symbol
    ))#{
        source_module => maps:get(module, Symbol),
        runtime_module => RuntimeModule,
        visibility => public
    }.

symbol_before(Left, Right) ->
    {
        maps:get(kind, Left),
        maps:get(name, Left),
        maps:get(arity, Left, undefined)
    } =<
    {
        maps:get(kind, Right),
        maps:get(name, Right),
        maps:get(arity, Right, undefined)
    }.

by_kind(Kind, Symbols) ->
    [Symbol || Symbol <- Symbols, maps:get(kind, Symbol) =:= Kind].

declaration_metadata(Tag, Declarations, Policy) ->
    [
        declaration_entry(Declaration)
        || Declaration <- Declarations,
           element(1, Declaration) =:= Tag,
           exported(declaration_kind(Tag), element(2, Declaration), Policy)
    ].

declaration_kind(effect_decl) -> effect;
declaration_kind(trait_decl) -> trait.

declaration_entry(
    {effect_decl, Name, Operations, Location}
) ->
    #{kind => effect, name => Name, operations => Operations, location => Location};
declaration_entry(
    {trait_decl, Name, Params, Extends, Members, Location}
) ->
    #{
        kind => trait,
        name => Name,
        params => Params,
        extends => Extends,
        members => Members,
        location => Location
    }.

instance_metadata(Declarations, Module) ->
    [
        #{
            trait => Trait,
            type_arguments => TypeArguments,
            constraints => Constraints,
            methods => Methods,
            source_module => Module,
            location => Location
        }
        || {instance_decl, Trait, TypeArguments, Constraints, Methods, Location}
            <- Declarations
    ].

exported(_Kind, _Name, all) ->
    true;
exported(Kind, Name, Policy) ->
    sets:is_element({Kind, Name}, Policy).

normalize_dependencies(Dependencies) ->
    lists:usort(Dependencies).
