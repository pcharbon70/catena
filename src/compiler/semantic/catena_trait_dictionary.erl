%%%-------------------------------------------------------------------
%%% @doc Validated trait definitions and executable instance dictionaries.
%%%
%%% This module is the maintained boundary between source traits/instances
%%% and runtime dispatch.  It validates the visible trait universe, produces
%%% stable dictionary identities, and compiles local dictionaries into one
%%% exported lookup function per BEAM module.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_trait_dictionary).

-export([
    empty/1,
    build/4,
    build/5,
    is_inventory/1,
    dictionaries/1,
    public_dictionaries/1,
    resolve_method/4,
    resolve_method_value/3,
    runtime_required/1,
    runtime_dependency/0,
    compile_dictionaries/2,
    dictionary_export/0
]).

-define(INVENTORY_VERSION, 1).
-define(DICTIONARY_FUNCTION, '$catena_dictionary').

-type inventory() :: map().
-export_type([inventory/0]).

-spec empty(atom()) -> inventory().
empty(Module) ->
    #{
        '$catena_trait_inventory' => ?INVENTORY_VERSION,
        module => Module,
        traits => #{},
        dictionaries => [],
        local_dictionaries => [],
        methods => #{}
    }.

-spec build(atom(), [term()], #{atom() => map()},
    catena_import_resolution:resolution()) ->
    {ok, inventory()} | {error, term()}.
build(Module, Declarations, Interfaces, ImportResolution) ->
    build(Module, Declarations, [], Interfaces, ImportResolution).

-spec build(atom(), [term()], [term()], #{atom() => map()},
    catena_import_resolution:resolution()) ->
    {ok, inventory()} | {error, term()}.
build(
    Module,
    Declarations,
    TypedDeclarations,
    Interfaces,
    ImportResolution
) ->
    try
        do_build(
            Module,
            Declarations,
            TypedDeclarations,
            Interfaces,
            ImportResolution
        )
    catch
        throw:{trait_validation_error, Reason} ->
            {error, {trait_validation_error, Reason}};
        error:Reason:Stack ->
            {error, {trait_validation_internal_error, Reason, Stack}}
    end.

do_build(
    Module,
    Declarations,
    TypedDeclarations,
    Interfaces,
    ImportResolution
) ->
    LocalTraits = collect_local_traits(Module, Declarations),
    ImportedTraits = collect_imported_traits(Interfaces, ImportResolution),
    case merge_traits(LocalTraits, ImportedTraits) of
        {ok, Traits} ->
            case validate_trait_hierarchy(Traits) of
                ok ->
                    LocalTypes = collect_local_types(Declarations),
                    TypeDescriptors = collect_type_descriptors(
                        Declarations,
                        Interfaces
                    ),
                    ImportedDictionaries =
                        collect_imported_dictionaries(
                            Interfaces,
                            ImportResolution
                        ),
                    case build_local_dictionaries(
                        Module,
                        Declarations,
                        TypedDeclarations,
                        Traits,
                        LocalTraits,
                        LocalTypes,
                        TypeDescriptors
                    ) of
                        {ok, LocalDictionaries0} ->
                            All0 =
                                ImportedDictionaries ++
                                    LocalDictionaries0,
                            case validate_coherence(All0) of
                                ok ->
                                    case attach_parents(All0, Traits) of
                                        {ok, All} ->
                                            Local = [
                                                Dictionary
                                                || Dictionary <- All,
                                                   maps:get(
                                                       source_module,
                                                       Dictionary
                                                   ) =:= Module
                                            ],
                                            {ok, #{
                                                '$catena_trait_inventory' =>
                                                    ?INVENTORY_VERSION,
                                                module => Module,
                                                traits => Traits,
                                                dictionaries => All,
                                                local_dictionaries => Local,
                                                methods =>
                                                    index_methods(All)
                                            }};
                                        {error, _} = Error ->
                                            Error
                                    end;
                                {error, _} = Error ->
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec is_inventory(term()) -> boolean().
is_inventory(#{
    '$catena_trait_inventory' := ?INVENTORY_VERSION,
    module := Module,
    traits := Traits,
    dictionaries := Dictionaries,
    local_dictionaries := Local,
    methods := Methods
}) ->
    is_atom(Module) andalso
        is_map(Traits) andalso
        is_list(Dictionaries) andalso
        is_list(Local) andalso
        is_map(Methods);
is_inventory(_) ->
    false.

-spec dictionaries(inventory()) -> [map()].
dictionaries(Inventory) ->
    maps:get(dictionaries, Inventory).

-spec public_dictionaries(inventory()) -> [map()].
public_dictionaries(Inventory) ->
    [public_dictionary(Dictionary) ||
        Dictionary <- maps:get(local_dictionaries, Inventory)].

-spec resolve_method(atom(), non_neg_integer(), inventory(), map()) ->
    {ok, [map()]} | {error, term()}.
resolve_method(Name, Arity, Inventory, Context) ->
    Candidates0 = maps:get(Name, maps:get(methods, Inventory), []),
    Candidates = [
        Candidate
        || Candidate <- Candidates0,
           maps:get(arity, Candidate) =:= Arity
    ],
    case {Candidates0, Candidates} of
        {[], _} ->
            {error, catena_backend_error:unresolved_call(
                Name,
                Arity,
                Context#{callable_kind => trait_method}
            )};
        {_, []} ->
            ExpectedArities = lists:usort([
                maps:get(arity, Candidate)
                || Candidate <- Candidates0
            ]),
            {error, {
                trait_method_arity_mismatch,
                Name,
                ExpectedArities,
                Arity,
                Context
            }};
        {_, _} ->
            {ok, [runtime_candidate(Candidate) ||
                Candidate <- Candidates]}
    end.

-spec resolve_method_value(atom(), inventory(), map()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
resolve_method_value(Name, Inventory, Context) ->
    Candidates = maps:get(Name, maps:get(methods, Inventory), []),
    Arities = lists:usort([
        maps:get(arity, Candidate)
        || Candidate <- Candidates
    ]),
    case {Candidates, Arities} of
        {[], _} ->
            {error, catena_backend_error:unresolved_call(
                Name,
                0,
                Context#{callable_kind => trait_method}
            )};
        {_, [Arity]} ->
            {ok, Arity, [runtime_candidate(Candidate) ||
                Candidate <- Candidates]};
        {_, _} ->
            {error, {ambiguous_trait_method_value, Name, Arities, Context}}
    end.

-spec runtime_required(inventory()) -> boolean().
runtime_required(Inventory) ->
    map_size(maps:get(methods, Inventory)) > 0.

-spec runtime_dependency() -> map().
runtime_dependency() ->
    #{module => catena_trait_runtime, version => 1}.

-spec dictionary_export() -> {atom(), non_neg_integer()}.
dictionary_export() ->
    {?DICTIONARY_FUNCTION, 2}.

-spec compile_dictionaries(inventory(),
    catena_codegen_utils:codegen_state()) ->
    {[{cerl:cerl(), cerl:cerl()}], catena_codegen_utils:codegen_state(),
        [cerl:cerl()]}.
compile_dictionaries(Inventory, State) ->
    case maps:get(local_dictionaries, Inventory) of
        [] ->
            {[], State, []};
        LocalDictionaries ->
            {Clauses, State1} = lists:mapfoldl(
                fun compile_dictionary_clause/2,
                State,
                LocalDictionaries
            ),
            TraitVar = cerl:c_var('$catena_trait'),
            HeadVar = cerl:c_var('$catena_instance_head'),
            Key = cerl:c_tuple([TraitVar, HeadVar]),
            Body = cerl:c_case(Key, Clauses),
            Definition = {
                cerl:c_fname(?DICTIONARY_FUNCTION, 2),
                cerl:c_fun([TraitVar, HeadVar], Body)
            },
            {
                [Definition],
                State1,
                [cerl:c_fname(?DICTIONARY_FUNCTION, 2)]
            }
    end.

collect_local_traits(Module, Declarations) ->
    [
        normalize_trait(Module, Declaration)
        || {trait_decl, _, _, _, _, _} = Declaration <- Declarations
    ].

collect_imported_traits(Interfaces, ImportResolution) ->
    VisibleTraitNames = sets:from_list([
        maps:get(owner, Entry, maps:get(name, Entry))
        || Entry <- catena_import_resolution:entries(ImportResolution),
           lists:member(
               maps:get(kind, Entry),
               [trait, trait_method]
           )
    ]),
    lists:append([
        [
            normalize_interface_trait(
                catena_module_interface:source_module(Interface),
                Trait
            )
            || Trait <- maps:get(traits, Interface, []),
               sets:is_element(
                   maps:get(name, Trait),
                   VisibleTraitNames
               )
        ]
        || Interface <- maps:values(Interfaces),
           catena_module_interface:is_interface(Interface)
    ]).

normalize_trait(
    Module,
    {trait_decl, Name, Params, Extends, Members, Location}
) ->
    #{
        name => Name,
        params => Params,
        extends => [constraint_name(Constraint) ||
            Constraint <- optional_list(Extends)],
        methods => normalize_trait_members(Name, Members),
        source_module => Module,
        location => Location
    }.

normalize_interface_trait(Module, Trait) ->
    normalize_trait(
        Module,
        {
            trait_decl,
            maps:get(name, Trait),
            maps:get(params, Trait),
            maps:get(extends, Trait),
            maps:get(members, Trait),
            maps:get(location, Trait)
        }
    ).

normalize_trait_members(Trait, Members) ->
    normalize_trait_members(Trait, Members, #{}, #{}).

normalize_trait_members(_Trait, [], Signatures, Defaults) ->
    ExtraDefaults = [
        Name
        || Name <- maps:keys(Defaults),
           not maps:is_key(Name, Signatures)
    ],
    case ExtraDefaults of
        [] ->
            ok;
        _ ->
            throw({trait_validation_error, {
                defaults_without_signatures,
                ExtraDefaults
            }})
    end,
    maps:map(
        fun(Name, Signature) ->
            case maps:get(Name, Defaults, undefined) of
                undefined -> Signature;
                Default -> Signature#{default => Default}
            end
        end,
        Signatures
    );
normalize_trait_members(
    Trait,
    [{trait_sig, Name, Type, Location} | Rest],
    Signatures,
    Defaults
) ->
    case maps:is_key(Name, Signatures) of
        true ->
            throw({trait_validation_error, {
                duplicate_trait_method_signature,
                Trait,
                Name,
                Location
            }});
        false ->
            normalize_trait_members(
                Trait,
                Rest,
                Signatures#{Name => #{
                    name => Name,
                    arity => type_arity(Type),
                    type => Type,
                    location => Location
                }},
                Defaults
            )
    end;
normalize_trait_members(
    Trait,
    [{trait_default, Name, Params, Body, Location} | Rest],
    Signatures,
    Defaults
) ->
    case maps:is_key(Name, Defaults) of
        true ->
            throw({trait_validation_error, {
                duplicate_trait_default,
                Trait,
                Name,
                Location
            }});
        false ->
            normalize_trait_members(
                Trait,
                Rest,
                Signatures,
                Defaults#{Name => #{
                    lambda => {lambda, Params, Body, Location},
                    arity => length(Params),
                    location => Location
                }}
            )
    end;
normalize_trait_members(Trait, [Other | _], _Signatures, _Defaults) ->
    throw({trait_validation_error, {
        invalid_trait_member,
        Trait,
        Other
    }}).

merge_traits(LocalTraits, ImportedTraits) ->
    try
        Traits = lists:foldl(
            fun(Trait, Acc) ->
                Name = maps:get(name, Trait),
                case maps:get(Name, Acc, undefined) of
                    undefined ->
                        validate_defaults(Trait),
                        Acc#{Name => Trait};
                    Existing ->
                        throw({duplicate_visible_trait, Name, [
                            maps:get(source_module, Existing),
                            maps:get(source_module, Trait)
                        ]})
                end
            end,
            #{},
            LocalTraits ++ ImportedTraits
        ),
        {ok, Traits}
    catch
        throw:{trait_validation_error, Reason} ->
            {error, {trait_validation_error, Reason}};
        throw:Reason ->
            {error, {trait_validation_error, Reason}}
    end.

validate_defaults(Trait) ->
    maps:foreach(
        fun(Name, Method) ->
            case maps:get(default, Method, undefined) of
                undefined ->
                    ok;
                Default ->
                    case maps:get(arity, Default) =:=
                        maps:get(arity, Method)
                    of
                        true ->
                            ok;
                        false ->
                            throw({trait_validation_error, {
                                default_method_arity_mismatch,
                                maps:get(name, Trait),
                                Name,
                                maps:get(arity, Method),
                                maps:get(arity, Default)
                            }})
                    end
            end
        end,
        maps:get(methods, Trait)
    ).

validate_trait_hierarchy(Traits) ->
    Definitions = maps:map(
        fun(_Name, Trait) ->
            {
                maps:get(name, Trait),
                maps:get(extends, Trait),
                maps:get(location, Trait)
            }
        end,
        Traits
    ),
    case catena_trait_hierarchy:check_hierarchy(Definitions) of
        {ok, valid} ->
            ok;
        {error, Errors} ->
            {error, {trait_hierarchy_errors, Errors}}
    end.

collect_local_types(Declarations) ->
    sets:from_list([
        Name
        || {type_decl, Name, _, _, _, _} <- Declarations
    ]).

collect_type_descriptors(Declarations, Interfaces) ->
    Local = maps:from_list([
        {Name, {constructors, [
            Constructor
            || {constructor, Constructor, _, _} <- Constructors
        ]}}
        || {type_decl, Name, _, Constructors, _, _} <- Declarations
    ]),
    lists:foldl(
        fun(Interface, Acc) ->
            lists:foldl(
                fun(Entry, Inner) ->
                    Owner = maps:get(owner, Entry),
                    Constructor = maps:get(name, Entry),
                    maps:update_with(
                        Owner,
                        fun({constructors, Existing}) ->
                            {constructors,
                                lists:usort([Constructor | Existing])}
                        end,
                        {constructors, [Constructor]},
                        Inner
                    )
                end,
                Acc,
                maps:get(constructors, Interface, [])
            )
        end,
        Local,
        maps:values(Interfaces)
    ).

collect_imported_dictionaries(Interfaces, ImportResolution) ->
    ImportedModules = sets:from_list([
        maps:get(source_module, Entry)
        || Entry <- catena_import_resolution:entries(ImportResolution)
    ]),
    lists:append([
        [
            inflate_public_dictionary(Dictionary)
            || Dictionary <- maps:get(dictionaries, Interface, [])
        ]
        || Interface <- maps:values(Interfaces),
           catena_module_interface:is_interface(Interface),
           sets:is_element(
               catena_module_interface:source_module(Interface),
               ImportedModules
           )
    ]).

inflate_public_dictionary(Dictionary) ->
    Methods = maps:from_list([
        {
            maps:get(name, Method),
            Method
        }
        || Method <- maps:get(methods, Dictionary, [])
    ]),
    Dictionary#{methods := Methods}.

build_local_dictionaries(
    Module,
    Declarations,
    TypedDeclarations,
    Traits,
    LocalTraits,
    LocalTypes,
    TypeDescriptors
) ->
    LocalTraitNames = sets:from_list([
        maps:get(name, Trait)
        || Trait <- LocalTraits
    ]),
    try
        RawInstances = [
            Instance
            || {instance_decl, _, _, _, _, _} = Instance <-
                Declarations
        ],
        TypedInstances = [
            Instance
            || {typed_instance, _, _, _, _, _} = Instance <-
                TypedDeclarations
        ],
        Paired = pair_instances(RawInstances, TypedInstances),
        Dictionaries = [
            build_local_dictionary(
                Module,
                RawInstance,
                TypedInstance,
                Traits,
                LocalTraitNames,
                LocalTypes,
                TypeDescriptors
            )
            || {RawInstance, TypedInstance} <- Paired
        ],
        {ok, Dictionaries}
    catch
        throw:{trait_validation_error, Reason} ->
            {error, {trait_validation_error, Reason}}
    end.

build_local_dictionary(
    Module,
    {
        instance_decl,
        TraitName,
        TypeArguments,
        Constraints,
        Methods,
        Location
    },
    TypedInstance,
    Traits,
    LocalTraitNames,
    LocalTypes,
    TypeDescriptors
) ->
    Trait = case maps:get(TraitName, Traits, undefined) of
        undefined ->
            throw({trait_validation_error, {
                unknown_instance_trait,
                TraitName,
                Location
            }});
        Found ->
            Found
    end,
    Head = canonical_types(TypeArguments),
    Root = head_root(TypeArguments),
    case sets:is_element(TraitName, LocalTraitNames) orelse
        sets:is_element(Root, LocalTypes)
    of
        true ->
            ok;
        false ->
            throw({trait_validation_error, {
                orphan_instance,
                Module,
                TraitName,
                Head,
                Location
            }})
    end,
    Implementations = normalize_instance_methods(
        TraitName,
        Methods
    ),
    ResolvedMethods = resolve_instance_methods(
        Trait,
        Implementations,
        Location
    ),
    ok = validate_method_signatures(
        Trait,
        TypeArguments,
        Implementations,
        ResolvedMethods,
        TypedInstance,
        Location
    ),
    #{
        trait => TraitName,
        head => Head,
        constraints => Constraints,
        methods => ResolvedMethods,
        parents => [],
        source_module => Module,
        runtime_module => Module,
        dictionary_function => ?DICTIONARY_FUNCTION,
        match => match_descriptor(Root, TypeDescriptors),
        location => Location,
        identity => {
            Module,
            ?DICTIONARY_FUNCTION,
            TraitName,
            Head
        }
    }.

pair_instances(RawInstances, []) ->
    [{Instance, undefined} || Instance <- RawInstances];
pair_instances(RawInstances, TypedInstances)
  when length(RawInstances) =:= length(TypedInstances) ->
    lists:zip(RawInstances, TypedInstances);
pair_instances(RawInstances, TypedInstances) ->
    throw({trait_validation_error, {
        instance_typing_evidence_mismatch,
        length(RawInstances),
        length(TypedInstances)
    }}).

validate_method_signatures(
    _Trait,
    _TypeArguments,
    _Implementations,
    _ResolvedMethods,
    undefined,
    _Location
) ->
    ok;
validate_method_signatures(
    Trait,
    TypeArguments,
    Implementations,
    ResolvedMethods,
    {typed_instance, _TraitName, _TypedHead, _Constraints,
        TypedMethods, _TypedLocation},
    Location
) ->
    TypedByName = maps:from_list([
        {Name, InferredType}
        || {Name, InferredType, _Lambda} <- TypedMethods
    ]),
    maps:foreach(
        fun(Name, _Implementation) ->
            ExpectedAST = maps:get(
                declared_type,
                maps:get(Name, ResolvedMethods)
            ),
            SpecializedAST = specialize_type(
                ExpectedAST,
                maps:get(params, Trait),
                TypeArguments
            ),
            Expected = internal_type(SpecializedAST),
            Actual = maps:get(Name, TypedByName),
            case catena_infer_unify:unify_types(Actual, Expected) of
                {ok, _Substitution} ->
                    ok;
                {error, Reason} ->
                    throw({trait_validation_error, {
                        instance_method_type_mismatch,
                        maps:get(name, Trait),
                        Name,
                        Expected,
                        Actual,
                        Reason,
                        Location
                    }})
            end
        end,
        Implementations
    ),
    ok.

normalize_instance_methods(Trait, Methods) ->
    lists:foldl(
        fun({Name, {lambda, Params, _Body, Location} = Lambda}, Acc) ->
            case maps:is_key(Name, Acc) of
                true ->
                    throw({trait_validation_error, {
                        duplicate_instance_method,
                        Trait,
                        Name,
                        Location
                    }});
                false ->
                    Acc#{Name => #{
                        name => Name,
                        arity => length(Params),
                        lambda => Lambda,
                        location => Location
                    }}
            end
        end,
        #{},
        Methods
    ).

resolve_instance_methods(Trait, Implementations, InstanceLocation) ->
    Declared = maps:get(methods, Trait),
    Extra = [
        Name
        || Name <- maps:keys(Implementations),
           not maps:is_key(Name, Declared)
    ],
    case Extra of
        [] -> ok;
        _ ->
            throw({trait_validation_error, {
                extra_instance_methods,
                maps:get(name, Trait),
                Extra,
                InstanceLocation
            }})
    end,
    maps:fold(
        fun(Name, Method, Acc) ->
            Implementation = maps:get(
                Name,
                Implementations,
                maps:get(default, Method, undefined)
            ),
            case Implementation of
                undefined ->
                    throw({trait_validation_error, {
                        missing_instance_method,
                        maps:get(name, Trait),
                        Name,
                        InstanceLocation
                    }});
                _ ->
                    Expected = maps:get(arity, Method),
                    Actual = maps:get(arity, Implementation),
                    case Expected =:= Actual of
                        true ->
                            Acc#{Name => Implementation#{
                                declared_type => maps:get(type, Method)
                            }};
                        false ->
                            throw({trait_validation_error, {
                                instance_method_arity_mismatch,
                                maps:get(name, Trait),
                                Name,
                                Expected,
                                Actual,
                                maps:get(location, Implementation)
                            }})
                    end
            end
        end,
        #{},
        Declared
    ).

validate_coherence(Dictionaries) ->
    validate_coherence(Dictionaries, []).

validate_coherence([], _Seen) ->
    ok;
validate_coherence([Dictionary | Rest], Seen) ->
    Overlaps = [
        Existing
        || Existing <- Seen,
           maps:get(trait, Existing) =:= maps:get(trait, Dictionary),
           heads_overlap(
               maps:get(head, Existing),
               maps:get(head, Dictionary)
           )
    ],
    case Overlaps of
        [] ->
            validate_coherence(Rest, [Dictionary | Seen]);
        _ ->
            {error, {
                incoherent_instances,
                maps:get(trait, Dictionary),
                maps:get(head, Dictionary),
                [maps:get(identity, Existing) ||
                    Existing <- Overlaps] ++
                    [maps:get(identity, Dictionary)]
            }}
    end.

attach_parents(Dictionaries, Traits) ->
    try
        {ok, [
            attach_dictionary_parents(Dictionary, Dictionaries, Traits)
            || Dictionary <- Dictionaries
        ]}
    catch
        throw:{trait_validation_error, Reason} ->
            {error, {trait_validation_error, Reason}}
    end.

attach_dictionary_parents(Dictionary, Dictionaries, Traits) ->
    Trait = maps:get(maps:get(trait, Dictionary), Traits),
    ParentTraits = maps:get(extends, Trait),
    Parents = [
        find_parent_dictionary(ParentTrait, Dictionary, Dictionaries)
        || ParentTrait <- ParentTraits
    ],
    Dictionary#{parents := [maps:get(identity, Parent) ||
        Parent <- Parents]}.

find_parent_dictionary(ParentTrait, Dictionary, Dictionaries) ->
    Head = maps:get(head, Dictionary),
    Matches = [
        Candidate
        || Candidate <- Dictionaries,
           maps:get(trait, Candidate) =:= ParentTrait,
           maps:get(head, Candidate) =:= Head
    ],
    case Matches of
        [Parent] ->
            Parent;
        [] ->
            throw({trait_validation_error, {
                missing_inherited_instance,
                maps:get(trait, Dictionary),
                ParentTrait,
                Head,
                maps:get(location, Dictionary)
            }});
        _ ->
            throw({trait_validation_error, {
                ambiguous_inherited_instance,
                maps:get(trait, Dictionary),
                ParentTrait,
                Head
            }})
    end.

index_methods(Dictionaries) ->
    lists:foldl(
        fun(Dictionary, Index0) ->
            maps:fold(
                fun(Name, Method, Index) ->
                    Candidate = Method#{
                        dictionary => public_dictionary(Dictionary)
                    },
                    maps:update_with(
                        Name,
                        fun(Existing) -> Existing ++ [Candidate] end,
                        [Candidate],
                        Index
                    )
                end,
                Index0,
                maps:get(methods, Dictionary)
            )
        end,
        #{},
        Dictionaries
    ).

public_dictionary(Dictionary) ->
    (maps:with(
        [
            trait,
            head,
            parents,
            source_module,
            runtime_module,
            dictionary_function,
            match,
            location,
            identity
        ],
        Dictionary
    ))#{
        methods => [
            #{
                name => Name,
                arity => maps:get(arity, Method)
            }
            || {Name, Method} <- maps:to_list(
                maps:get(methods, Dictionary)
            )
        ]
    }.

runtime_candidate(#{dictionary := Dictionary}) ->
    maps:with(
        [
            trait,
            head,
            source_module,
            runtime_module,
            dictionary_function,
            match,
            identity
        ],
        Dictionary
    ).

compile_dictionary_clause(Dictionary, State) ->
    {Pairs, State1} = maps:fold(
        fun(Name, Method, {Acc, CurrentState}) ->
            Lambda0 = maps:get(lambda, Method),
            Lambda1 = catena_codegen_lower:lower_expr(Lambda0),
            Lambda = catena_codegen_erase:erase_expr(Lambda1),
            {CoreLambda, NextState} =
                catena_codegen_expr:translate_expr(
                    Lambda,
                    CurrentState
                ),
            {
                [cerl:c_map_pair(cerl:c_atom(Name), CoreLambda) | Acc],
                NextState
            }
        end,
        {[], State},
        maps:get(methods, Dictionary)
    ),
    ParentPair = cerl:c_map_pair(
        cerl:c_atom('$catena_parents'),
        cerl:abstract(maps:get(parents, Dictionary))
    ),
    Body = cerl:c_map([ParentPair | lists:reverse(Pairs)]),
    Pattern = cerl:c_tuple([
        cerl:c_atom(maps:get(trait, Dictionary)),
        cerl:abstract(maps:get(head, Dictionary))
    ]),
    {cerl:c_clause([Pattern], Body), State1}.

optional_list(undefined) -> [];
optional_list(List) when is_list(List) -> List.

constraint_name({trait_constraint, Name, _Args}) -> Name;
constraint_name({trait_constraint, Name, _Args, _Location}) -> Name;
constraint_name({Name, _Args}) when is_atom(Name) -> Name;
constraint_name(Name) when is_atom(Name) -> Name;
constraint_name(Other) ->
    throw({trait_validation_error, {invalid_trait_constraint, Other}}).

type_arity({type_fun, _From, To, _Location}) ->
    1 + type_arity(To);
type_arity({type_effect, Type, _Effects, _Location}) ->
    type_arity(Type);
type_arity({constrained_type, _Constraints, Type, _Location}) ->
    type_arity(Type);
type_arity(_) ->
    0.

specialize_type(Type, Params, Arguments) ->
    Substitution = maps:from_list(lists:zip(Params, Arguments)),
    specialize_type(Type, Substitution).

specialize_type({type_var, Name, _Location} = Type, Substitution) ->
    maps:get(Name, Substitution, Type);
specialize_type(
    {type_fun, From, To, Location},
    Substitution
) ->
    {type_fun,
        specialize_type(From, Substitution),
        specialize_type(To, Substitution),
        Location};
specialize_type(
    {type_effect, Type, Effects, Location},
    Substitution
) ->
    {type_effect,
        specialize_type(Type, Substitution),
        Effects,
        Location};
specialize_type(
    {constrained_type, Constraints, Type, Location},
    Substitution
) ->
    {constrained_type,
        Constraints,
        specialize_type(Type, Substitution),
        Location};
specialize_type(
    {type_app, Constructor, Arguments, Location},
    Substitution
) ->
    {type_app,
        specialize_type(Constructor, Substitution),
        [
            specialize_type(Argument, Substitution)
            || Argument <- Arguments
        ],
        Location};
specialize_type(
    {type_tuple, Elements, Location},
    Substitution
) ->
    {type_tuple,
        [specialize_type(Element, Substitution) ||
            Element <- Elements],
        Location};
specialize_type(Type, _Substitution) ->
    Type.

internal_type({type_var, Name, _Location}) ->
    {tvar, trait_type_variable(Name)};
internal_type({type_con, Name, _Location}) ->
    {tcon, internal_type_name(Name)};
internal_type({type_app, Constructor, Arguments, _Location}) ->
    InternalConstructor = internal_type(Constructor),
    InternalArguments = [
        internal_type(Argument)
        || Argument <- Arguments
    ],
    case InternalConstructor of
        {tapp, RootConstructor, ExistingArguments} ->
            {tapp,
                RootConstructor,
                ExistingArguments ++ InternalArguments};
        _ ->
            {tapp, InternalConstructor, InternalArguments}
    end;
internal_type({type_fun, From, To, _Location}) ->
    {tfun,
        internal_type(From),
        internal_type(To),
        catena_types:empty_effects()};
internal_type(
    {type_effect, Type, Effects, _Location}
) ->
    replace_terminal_effects(
        internal_type(Type),
        catena_types:effect_set(Effects)
    );
internal_type({constrained_type, _Constraints, Type, _Location}) ->
    internal_type(Type);
internal_type({type_tuple, Elements, _Location}) ->
    {ttuple, [internal_type(Element) || Element <- Elements]};
internal_type(Other) ->
    Other.

replace_terminal_effects({tfun, From, To, _}, Effects) ->
    case To of
        {tfun, _, _, _} ->
            {tfun, From, replace_terminal_effects(To, Effects),
                catena_types:empty_effects()};
        _ ->
            {tfun, From, To, Effects}
    end;
replace_terminal_effects(Type, _Effects) ->
    Type.

internal_type_name('Bool') -> bool;
internal_type_name('Int') -> int;
internal_type_name('Float') -> float;
internal_type_name('String') -> string;
internal_type_name('Unit') -> unit;
internal_type_name('List') -> list;
internal_type_name(Name) -> Name.

trait_type_variable(Name) ->
    erlang:phash2({catena_trait_method_type, Name}, 100000000) +
        1000000.

canonical_types(Types) ->
    [canonical_type(Type) || Type <- Types].

canonical_type({type_con, Name, _Location}) ->
    {type_con, Name};
canonical_type({type_var, Name, _Location}) ->
    {type_var, Name};
canonical_type({type_app, Constructor, Arguments, _Location}) ->
    {type_app,
        canonical_type(Constructor),
        [canonical_type(Argument) || Argument <- Arguments]};
canonical_type({type_tuple, Elements, _Location}) ->
    {type_tuple, [canonical_type(Element) || Element <- Elements]};
canonical_type(Type) when is_tuple(Type) ->
    list_to_tuple([
        canonical_type(Element)
        || Element <- tuple_to_list(Type)
    ]);
canonical_type(Type) when is_list(Type) ->
    [canonical_type(Element) || Element <- Type];
canonical_type(Type) ->
    Type.

head_root([{type_con, Name, _Location} | _]) ->
    Name;
head_root([{type_app, Constructor, _Arguments, _Location} | _]) ->
    head_root([Constructor]);
head_root([Other | _]) ->
    canonical_type(Other);
head_root([]) ->
    undefined.

match_descriptor('Int', _Descriptors) -> {builtin, integer};
match_descriptor('Float', _Descriptors) -> {builtin, float};
match_descriptor('Bool', _Descriptors) -> {builtin, boolean};
match_descriptor('String', _Descriptors) -> {builtin, string};
match_descriptor('List', _Descriptors) -> {builtin, list};
match_descriptor(Root, Descriptors) ->
    maps:get(Root, Descriptors, {opaque, Root}).

heads_overlap(Left, Right) ->
    canonical_heads_overlap(Left, Right).

canonical_heads_overlap(Left, Right) when Left =:= Right ->
    true;
canonical_heads_overlap([{type_var, _} | _], _Right) ->
    true;
canonical_heads_overlap(_Left, [{type_var, _} | _]) ->
    true;
canonical_heads_overlap(
    [{type_app, LeftCon, LeftArgs} | LeftRest],
    [{type_app, RightCon, RightArgs} | RightRest]
) ->
    canonical_heads_overlap([LeftCon], [RightCon]) andalso
        canonical_heads_overlap(LeftArgs, RightArgs) andalso
        canonical_heads_overlap(LeftRest, RightRest);
canonical_heads_overlap([Left | LeftRest], [Right | RightRest]) ->
    canonical_heads_overlap_term(Left, Right) andalso
        canonical_heads_overlap(LeftRest, RightRest);
canonical_heads_overlap([], []) ->
    true;
canonical_heads_overlap(_, _) ->
    false.

canonical_heads_overlap_term({type_var, _}, _Right) -> true;
canonical_heads_overlap_term(_Left, {type_var, _}) -> true;
canonical_heads_overlap_term(Left, Right) -> Left =:= Right.
