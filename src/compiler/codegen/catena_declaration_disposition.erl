%%%-------------------------------------------------------------------
%%% @doc Declaration classification before Core Erlang erasure.
%%%
%%% The pass assigns one of the backend contract's four dispositions to every
%%% declaration and also records import/export handling.  Representation and
%%% linkage metadata are selected here, while the normalized declarations are
%%% still intact.  Only prepare_for_codegen/1 may filter static declarations.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_declaration_disposition).

-export([
    classes/0,
    classify/1,
    classify_declaration/4,
    prepare_for_codegen/1
]).

-type disposition_class() ::
    lowered |
    erased_static |
    runtime_lowered |
    unsupported.
-type disposition() :: #{
    subject := declaration | import | export,
    index := pos_integer(),
    disposition := disposition_class(),
    kind := atom(),
    location := term()
}.

-export_type([disposition_class/0, disposition/0]).

%% @doc The exhaustive disposition classes accepted by the backend contract.
-spec classes() -> [disposition_class()].
classes() ->
    [lowered, erased_static, runtime_lowered, unsupported].

%% @doc Classify every declaration, import, and export in a validated unit.
-spec classify(catena_compilation_unit:t()) ->
    {ok, catena_compilation_unit:t()} | {error, term()}.
classify(Unit) ->
    case catena_compilation_unit:is_compilation_unit(Unit) of
        true ->
            Module = catena_compilation_unit:module_name(Unit),
            Exports = catena_compilation_unit:exports(Unit),
            Imports = catena_compilation_unit:imports(Unit),
            ImportResolution =
                catena_compilation_unit:import_resolution(Unit),
            {module, _, _, _, Declarations, _} =
                catena_compilation_unit:normalized_ast(Unit),
            ImportDispositions = [
                classify_import(
                    Module,
                    Import,
                    Index,
                    ImportResolution
                )
                || {Index, Import} <- indexed(Imports)
            ],
            DeclarationDispositions = [
                classify_declaration(Module, Exports, Declaration, Index)
                || {Index, Declaration} <- indexed(Declarations)
            ],
            ExportDispositions = [
                classify_export(Module, Export, Index)
                || {Index, Export} <- indexed(Exports)
            ],
            catena_compilation_unit:with_dispositions(
                Unit,
                ImportDispositions ++
                    DeclarationDispositions ++
                    ExportDispositions
            );
        false ->
            {error, {invalid_compilation_unit, unchecked_backend_input}}
    end.

%% @doc Classify one normalized declaration.
-spec classify_declaration(atom(), [term()], term(), pos_integer()) ->
    disposition().
classify_declaration(
    Module,
    Exports,
    {transform_decl, Name, Type, [], Location} = Declaration,
    Index
) ->
    Base = declaration_disposition(
        Index,
        transform,
        Declaration,
        Location
    ),
    case transform_requires_runtime_export(Name, Exports) of
        true ->
            Base#{
                disposition => unsupported,
                reason => missing_transform_implementation,
                representation => #{
                    name => Name,
                    arity => type_arity(Type),
                    module => Module
                }
            };
        false ->
            Base#{
                disposition => erased_static,
                reason => unused_signature,
                representation => #{
                    kind => type_signature,
                    name => Name,
                    arity => type_arity(Type)
                }
            }
    end;
classify_declaration(
    Module,
    _Exports,
    {transform_decl, Name, _Type, Clauses, Location} = Declaration,
    Index
) ->
    (declaration_disposition(
        Index,
        transform,
        Declaration,
        Location
    ))#{
        disposition => lowered,
        representation => #{
            kind => core_function,
            module => Module,
            name => Name,
            arity => transform_arity(Clauses)
        }
    };
classify_declaration(
    _Module,
    _Exports,
    {type_decl, Name, Params, Constructors, Derives, Location} = Declaration,
    Index
) ->
    (declaration_disposition(Index, type, Declaration, Location))#{
        disposition => erased_static,
        representation => #{
            kind => algebraic_data_type,
            name => Name,
            parameters => Params,
            constructors => [
                #{
                    name => ConstructorName,
                    arity => length(Fields),
                    fields => Fields,
                    location => ConstructorLocation
                }
                || {constructor, ConstructorName, Fields, ConstructorLocation}
                    <- Constructors
            ],
            derives => Derives
        }
    };
classify_declaration(
    _Module,
    _Exports,
    {effect_decl, Name, Operations, Location} = Declaration,
    Index
) ->
    (declaration_disposition(Index, effect, Declaration, Location))#{
        disposition => erased_static,
        representation => #{
            kind => effect_operations,
            name => Name,
            operations => [
                #{
                    name => OperationName,
                    type => Type,
                    location => OperationLocation
                }
                || {effect_operation, OperationName, Type, OperationLocation}
                    <- Operations
            ]
        }
    };
classify_declaration(
    _Module,
    _Exports,
    {trait_decl, Name, Params, Extends, Members, Location} = Declaration,
    Index
) ->
    (declaration_disposition(Index, trait, Declaration, Location))#{
        disposition => erased_static,
        reason => trait_dispatch_validated,
        representation => #{
            kind => trait_dispatch,
            name => Name,
            parameters => Params,
            extends => Extends,
            methods => trait_methods(Members)
        }
    };
classify_declaration(
    _Module,
    _Exports,
    {instance_decl, Trait, Types, Constraints, Methods, Location} =
        Declaration,
    Index
) ->
    (declaration_disposition(Index, instance, Declaration, Location))#{
        disposition => runtime_lowered,
        reason => instance_dictionary_emitted,
        representation => #{
            kind => instance_dictionary,
            trait => Trait,
            types => Types,
            constraints => Constraints,
            methods => Methods
        }
    };
classify_declaration(
    _Module,
    _Exports,
    {test_decl, Name, _Body, Location} = Declaration,
    Index
) ->
    (declaration_disposition(Index, test, Declaration, Location))#{
        disposition => unsupported,
        reason => application_test_artifact_deferred,
        representation => #{kind => test_artifact, name => Name}
    };
classify_declaration(
    _Module,
    _Exports,
    {property_decl, Name, Body, Location} = Declaration,
    Index
) ->
    (declaration_disposition(Index, property, Declaration, Location))#{
        disposition => unsupported,
        reason => application_property_artifact_deferred,
        representation => property_representation(Name, Body)
    };
classify_declaration(
    _Module,
    _Exports,
    Declaration,
    Index
) ->
    (declaration_disposition(
        Index,
        unknown,
        Declaration,
        source_location(Declaration)
    ))#{
        disposition => unsupported,
        reason => unknown_declaration
    }.

%% @doc Enforce disposition completeness and return only declarations that may
%% enter lowering.  Static declarations are filtered only after their
%% representation metadata has been retained in the unit.
-spec prepare_for_codegen(catena_compilation_unit:t()) ->
    {ok, term()} | {error, term()}.
prepare_for_codegen(Unit) ->
    case catena_compilation_unit:is_compilation_unit(Unit) of
        true ->
            case first_invalid_disposition(
                catena_compilation_unit:dispositions(Unit)
            ) of
                none ->
                    {module, Name, Exports, Imports, Declarations, Location} =
                        catena_compilation_unit:normalized_ast(Unit),
                    DeclarationDispositions = [
                        Disposition
                        || Disposition <-
                            catena_compilation_unit:dispositions(Unit),
                           maps:get(subject, Disposition) =:= declaration
                    ],
                    ActiveDeclarations = [
                        Declaration
                        || {Declaration, Disposition} <-
                            lists:zip(
                                Declarations,
                                DeclarationDispositions
                            ),
                           emits_runtime_declaration(Disposition)
                    ],
                    {ok,
                        {module,
                            Name,
                            Exports,
                            Imports,
                            ActiveDeclarations,
                            Location}};
                {invalid, Disposition} ->
                    disposition_error(Unit, Disposition)
            end;
        false ->
            {error, {invalid_compilation_unit, unchecked_backend_input}}
    end.

classify_import(
    Module,
    {import, ImportedModule, Items, Qualified, Alias, Location} = Import,
    Index,
    ImportResolution
) ->
    Resolved = [
        Entry
        || Entry <- catena_import_resolution:entries(ImportResolution),
           maps:get(source_module, Entry) =:= ImportedModule,
           maps:get(import_location, Entry) =:= Location
    ],
    HasExecutableResolution =
        catena_import_resolution:is_resolution(ImportResolution) andalso
        Resolved =/= [],
    #{
        subject => import,
        index => Index,
        disposition => case HasExecutableResolution of
            true -> erased_static;
            false -> unsupported
        end,
        kind => import,
        declaration => Import,
        location => Location,
        reason => case HasExecutableResolution of
            true -> executable_import_linkage_resolved;
            false -> executable_import_linkage_deferred
        end,
        representation => #{
            kind => import_linkage,
            source_module => Module,
            imported_module => ImportedModule,
            items => Items,
            qualified => Qualified,
            alias => Alias,
            resolved_symbols => Resolved
        }
    }.

classify_export(Module, Export, Index) ->
    #{
        subject => export,
        index => Index,
        disposition => lowered,
        kind => export_kind(Export),
        declaration => Export,
        location => undefined,
        representation => #{
            kind => core_export_metadata,
            module => Module,
            source_export => Export
        }
    }.

declaration_disposition(Index, Kind, Declaration, Location) ->
    #{
        subject => declaration,
        index => Index,
        disposition => unclassified,
        kind => Kind,
        declaration => Declaration,
        location => Location
    }.

first_invalid_disposition([]) ->
    none;
first_invalid_disposition([Disposition | Rest]) ->
    Class = maps:get(disposition, Disposition, unclassified),
    case lists:member(Class, classes()) andalso Class =/= unsupported of
        true -> first_invalid_disposition(Rest);
        false -> {invalid, Disposition}
    end.

disposition_error(Unit, Disposition) ->
    Declaration = maps:get(declaration, Disposition, undefined),
    Context = catena_backend_error:context(
        declaration_disposition,
        maps:get(kind, Disposition, declaration),
        Declaration,
        #{
            module => catena_compilation_unit:module_name(Unit),
            source_identity =>
                catena_compilation_unit:source_identity(Unit),
            disposition => maps:get(
                disposition,
                Disposition,
                unclassified
            ),
            reason => maps:get(reason, Disposition, unclassified)
        }
    ),
    Diagnostic = case maps:get(
        reason,
        Disposition,
        unclassified
    ) of
        missing_transform_implementation ->
            {transform_decl, Name, _, _, _} = Declaration,
            catena_backend_error:missing_transform_implementation(
                Name,
                Context
            );
        _ ->
            catena_backend_error:invalid_declaration_disposition(
                Declaration,
                Context
            )
    end,
    {error, Diagnostic}.

emits_runtime_declaration(Disposition) ->
    case {
        maps:get(disposition, Disposition),
        maps:get(kind, Disposition, unknown)
    } of
        {lowered, _} -> true;
        %% Instance declarations are emitted by the dictionary generator,
        %% not by the ordinary declaration lowering path.
        {runtime_lowered, instance} -> false;
        {runtime_lowered, _} -> true;
        _ -> false
    end.

transform_requires_runtime_export(_Name, []) ->
    true;
transform_requires_runtime_export(Name, Exports) ->
    lists:member({export_transform, Name}, Exports) orelse
        lists:any(
            fun
                ({Name0, Arity}) when Name0 =:= Name, is_integer(Arity) ->
                    true;
                (_) ->
                    false
            end,
            Exports
        ).

transform_arity([{transform_clause, Patterns, _, _, _} | _]) ->
    length(Patterns).

type_arity(undefined) ->
    undefined;
type_arity({type_fun, _From, To, _Location}) ->
    case type_arity(To) of
        undefined -> 1;
        Rest -> Rest + 1
    end;
type_arity({type_effect, Type, _Effects, _Location}) ->
    type_arity(Type);
type_arity(_) ->
    0.

trait_methods(Members) ->
    [
        trait_method(Member)
        || Member <- Members
    ].

trait_method({trait_sig, Name, Type, Location}) ->
    #{
        name => Name,
        arity => type_arity(Type),
        type => Type,
        default => false,
        location => Location
    };
trait_method({trait_default, Name, Params, Body, Location}) ->
    #{
        name => Name,
        arity => length(Params),
        body => Body,
        default => true,
        location => Location
    };
trait_method(Other) ->
    #{name => undefined, source => Other, location => source_location(Other)}.

property_representation(
    Name,
    {property_forall, Bindings, _Body, _Location}
) ->
    #{kind => property_artifact, name => Name, bindings => Bindings};
property_representation(Name, Body) ->
    #{kind => property_artifact, name => Name, source => Body}.

export_kind({export_transform, _}) -> export_transform;
export_kind({export_type, _}) -> export_type;
export_kind({export_trait, _}) -> export_trait;
export_kind({export_effect, _}) -> export_effect;
export_kind(_) -> export.

indexed(Items) ->
    lists:zip(lists:seq(1, length(Items)), Items).

source_location(Term) ->
    catena_backend_error:source_location(Term).
