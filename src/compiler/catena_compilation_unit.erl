%%%-------------------------------------------------------------------
%%% @doc Validated input shared by Catena backend stages.
%%%
%%% A compilation unit can only be constructed from a normalized module and
%%% the matching typed-module result, together with evidence for every
%%% canonical frontend validation stage.  Backend code should use this module
%%% rather than carrying independent AST, type, option, and source terms.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_compilation_unit).

-export([
    new/3,
    is_compilation_unit/1,
    validated_stages/0,
    module_name/1,
    runtime_module/1,
    normalized_ast/1,
    typed_module/1,
    typed_declarations/1,
    type_env/1,
    imports/1,
    exports/1,
    options/1,
    source_identity/1,
    validation_state/1,
    symbols/1,
    callables/1,
    import_resolution/1,
    trait_inventory/1,
    effect_inventory/1,
    effect_operations/1,
    effect_uses/1,
    effect_handlers/1,
    effectful_transforms/1,
    runtime_dependencies/1,
    artifact_dependencies/1,
    interface/1,
    locations/1,
    control_modes/1,
    control_ir/1,
    control_validation/1,
    dispositions/1,
    with_dispositions/2,
    with_control_modes/2,
    with_control_ir/2,
    with_control_validation/2
]).

-define(UNIT_VERSION, 10).

-opaque t() :: #{
    '$catena_compilation_unit' := pos_integer(),
    module_name := atom(),
    runtime_module := atom(),
    normalized_ast := term(),
    typed_module := term(),
    typed_declarations := [term()],
    type_env := term(),
    imports := [term()],
    exports := [term()],
    options := map(),
    source_identity := term(),
    validation_state := validation_state(),
    symbols := [symbol()],
    callables := catena_call_resolution:inventory(),
    import_resolution := catena_import_resolution:resolution(),
    trait_inventory := catena_trait_dictionary:inventory(),
    effect_inventory := catena_effect_resolution:inventory(),
    effectful_transforms := #{atom() => non_neg_integer()},
    runtime_dependencies := [map()],
    artifact_dependencies := [map()],
    interface := catena_module_interface:interface(),
    locations := location_index(),
    control_modes := catena_control_mode:inventory(),
    control_ir := catena_control_ir:ir() | pending,
    control_validation := catena_control_validate:report() | pending,
    dispositions := [map()]
}.

-type validation_stage() ::
    lexical |
    syntax |
    semantic |
    imports |
    kinds |
    types |
    traits |
    effects.
-type validation_state() :: #{validation_stage() => passed}.
-type symbol() :: #{
    kind := atom(),
    name := term(),
    arity := non_neg_integer() | undefined,
    module := atom(),
    location := term()
}.
-type location_index() :: #{
    module := term(),
    imports := [term()],
    declarations := [term()],
    clauses := [term()],
    patterns := [term()],
    expressions := [term()]
}.

-export_type([t/0, validation_stage/0, validation_state/0, symbol/0]).

%% @doc Construct a validated unit from matching normalized and typed modules.
%%
%% Metadata must contain `validation_state`, `options`, and `source_identity`.
%% Every stage returned by validated_stages/0 must be marked `passed`.
-spec new(term(), term(), map()) -> {ok, t()} | {error, term()}.
new(
    {module, Name, Exports, Imports, Declarations, ModuleLocation} =
        NormalizedAST,
    {typed_module, Name, TypedDeclarations, TypeEnv} = TypedModule,
    #{
        validation_state := ValidationState,
        options := Options,
        source_identity := SourceIdentity
    }
) when
    is_atom(Name),
    is_list(Exports),
    is_list(Imports),
    is_list(Declarations),
    is_list(TypedDeclarations),
    is_map(Options),
    is_map(ValidationState)
->
    case validated_compatibility_declarations(
        ValidationState,
        Declarations
    ) of
        {ok, CompatibilityDeclarations} ->
            case catena_effect_resolution:build(
                CompatibilityDeclarations
            ) of
                {ok, EffectInventory} ->
                    case catena_call_resolution:build(
                        Name,
                        Exports,
                        CompatibilityDeclarations
                    ) of
                        {ok, Callables} ->
                            Symbols = collect_symbols(
                                Name,
                                Exports,
                                Imports,
                                CompatibilityDeclarations,
                                ModuleLocation
                            ),
                            Locations = collect_locations(
                                ModuleLocation,
                                Imports,
                                CompatibilityDeclarations
                            ),
                            EffectfulTransforms =
                                catena_effect_resolution:
                                    effectful_transforms(
                                        CompatibilityDeclarations
                                    ),
                            RuntimeDependencies =
                                effect_runtime_dependencies(
                                    EffectfulTransforms
                                ),
                            ImportResolution = maps:get(
                                import_resolution,
                                Options,
                                catena_import_resolution:empty(Name)
                            ),
                            Interfaces = maps:get(
                                module_interfaces,
                                Options,
                                #{}
                            ),
                            case catena_trait_dictionary:build(
                                Name,
                                CompatibilityDeclarations,
                                TypedDeclarations,
                                Interfaces,
                                ImportResolution
                            ) of
                                {ok, TraitInventory} ->
                            TraitRuntimeDependencies =
                                case catena_trait_dictionary:
                                    runtime_required(TraitInventory)
                                of
                                    true ->
                                        [
                                            catena_trait_dictionary:
                                                runtime_dependency()
                                        ];
                                    false ->
                                        []
                                end,
                            AllRuntimeDependencies = lists:usort(
                                RuntimeDependencies ++
                                    TraitRuntimeDependencies
                            ),
                            InitialArtifactDependencies =
                                catena_module_linkage:
                                    artifact_dependencies(
                                        Imports,
                                        AllRuntimeDependencies,
                                        Interfaces
                                    ),
                            case catena_module_interface:build(
                                Name,
                                Exports,
                                CompatibilityDeclarations,
                                Symbols,
                                InitialArtifactDependencies,
                                SourceIdentity,
                                TraitInventory
                            ) of
                                {ok, Interface} ->
                            Unit0 = #{
                                '$catena_compilation_unit' => ?UNIT_VERSION,
                                module_name => Name,
                                runtime_module =>
                                    catena_module_interface:runtime_module(
                                        Interface
                                    ),
                                normalized_ast => NormalizedAST,
                                typed_module => TypedModule,
                                typed_declarations => TypedDeclarations,
                                type_env => TypeEnv,
                                imports => Imports,
                                exports => Exports,
                                options => Options,
                                source_identity => SourceIdentity,
                                validation_state => ValidationState,
                                symbols => Symbols,
                                callables => Callables,
                                import_resolution => ImportResolution,
                                trait_inventory => TraitInventory,
                                effect_inventory => EffectInventory,
                                effectful_transforms => EffectfulTransforms,
                                runtime_dependencies =>
                                    AllRuntimeDependencies,
                                artifact_dependencies =>
                                    InitialArtifactDependencies,
                                interface => Interface,
                                locations => Locations,
                                dispositions =>
                                    unclassified_dispositions(Declarations)
                            },
                            case catena_control_mode:analyze(
                                Name,
                                Declarations,
                                TypedDeclarations,
                                Callables,
                                Options
                            ) of
                                {ok, ControlModes} ->
                                    ControlRuntimeDependencies =
                                        control_runtime_dependencies(
                                            ControlModes
                                        ),
                                    FinalRuntimeDependencies =
                                        lists:usort(
                                            AllRuntimeDependencies ++
                                                ControlRuntimeDependencies
                                        ),
                                    FinalArtifactDependencies =
                                        catena_module_linkage:
                                            artifact_dependencies(
                                                Imports,
                                                FinalRuntimeDependencies,
                                                Interfaces
                                            ),
                                    case update_control_interface(
                                        Interface,
                                        ControlModes,
                                        FinalArtifactDependencies
                                    )
                                    of
                                        {ok, ControlInterface} ->
                                            Unit1 = Unit0#{
                                                interface =>
                                                    ControlInterface,
                                                runtime_dependencies =>
                                                    FinalRuntimeDependencies,
                                                artifact_dependencies =>
                                                    FinalArtifactDependencies,
                                                control_modes =>
                                                    ControlModes,
                                                control_ir => pending,
                                                control_validation => pending
                                            },
                                            case catena_selective_cps:
                                                lower(Unit1)
                                            of
                                                {ok, ControlIR} ->
                                                    Unit2 = Unit1#{
                                                        control_ir =>
                                                            ControlIR
                                                    },
                                                    case
                                                        catena_control_validate:
                                                            validate(Unit2)
                                                    of
                                                        {ok,
                                                            ValidationReport}
                                                        ->
                                                            {ok, Unit2#{
                                                                control_validation =>
                                                                    ValidationReport
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
    end;
new(
    {module, NormalizedName, _, _, _, _},
    {typed_module, TypedName, _, _},
    _Metadata
) when NormalizedName =/= TypedName ->
    {error,
        {invalid_compilation_unit,
            {module_identity_mismatch, NormalizedName, TypedName}}};
new(_NormalizedAST, _TypedModule, Metadata) when not is_map(Metadata) ->
    {error, {invalid_compilation_unit, invalid_metadata}};
new(_NormalizedAST, _TypedModule, Metadata) ->
    Missing = [
        Key
        || Key <- [validation_state, options, source_identity],
           not maps:is_key(Key, Metadata)
    ],
    case Missing of
        [] ->
            {error, {invalid_compilation_unit, invalid_frontend_artifacts}};
        _ ->
            {error, {invalid_compilation_unit, {missing_metadata, Missing}}}
    end.

validated_compatibility_declarations(ValidationState, Declarations) ->
    case validate_evidence(ValidationState) of
        ok ->
            %% Phase 3 type checking validates normalized Resumption nodes.
            %% Phase 4 retains them through control-mode analysis and the
            %% validated selective-CPS graph. The backend compatibility
            %% projection still occurs only in prepare_for_codegen/1 and
            %% remains fail-closed until runtime and Core lowering exist.
            {ok, Declarations};
        {error, _} = Error ->
            Error
    end.

%% @doc Return whether a term satisfies the maintained unit envelope.
-spec is_compilation_unit(term()) -> boolean().
is_compilation_unit(#{
    '$catena_compilation_unit' := ?UNIT_VERSION,
    module_name := Name,
    runtime_module := RuntimeModule,
    normalized_ast := {module, Name, _, _, _, _},
    typed_module := {typed_module, Name, _, _},
    typed_declarations := TypedDeclarations,
    options := Options,
    validation_state := ValidationState,
    symbols := Symbols,
    callables := Callables,
    import_resolution := ImportResolution,
    trait_inventory := TraitInventory,
    effect_inventory := EffectInventory,
    effectful_transforms := EffectfulTransforms,
    runtime_dependencies := RuntimeDependencies,
    artifact_dependencies := ArtifactDependencies,
    interface := Interface,
    locations := Locations,
    control_modes := ControlModes,
    control_ir := ControlIR,
    control_validation := ControlValidation,
    dispositions := Dispositions
}) ->
    is_atom(Name) andalso
        is_atom(RuntimeModule) andalso
        is_list(TypedDeclarations) andalso
        is_map(Options) andalso
        is_list(Symbols) andalso
        catena_call_resolution:is_inventory(Callables) andalso
        catena_import_resolution:is_resolution(ImportResolution) andalso
        catena_trait_dictionary:is_inventory(TraitInventory) andalso
        catena_effect_resolution:is_inventory(EffectInventory) andalso
        is_map(EffectfulTransforms) andalso
        is_list(RuntimeDependencies) andalso
        is_list(ArtifactDependencies) andalso
        catena_module_interface:is_interface(Interface) andalso
        is_map(Locations) andalso
        catena_control_mode:is_inventory(ControlModes) andalso
        (
            ControlIR =:= pending orelse
                catena_control_ir:is_ir(ControlIR)
        ) andalso
        (
            ControlValidation =:= pending orelse
                catena_control_validate:is_report(ControlValidation)
        ) andalso
        is_list(Dispositions) andalso
        validate_evidence(ValidationState) =:= ok;
is_compilation_unit(_) ->
    false.

%% @doc Canonical validations required before a backend unit may exist.
-spec validated_stages() -> [validation_stage()].
validated_stages() ->
    [lexical, syntax, semantic, imports, kinds, types, traits, effects].

-spec module_name(t()) -> atom().
module_name(Unit) -> maps:get(module_name, Unit).

-spec runtime_module(t()) -> atom().
runtime_module(Unit) -> maps:get(runtime_module, Unit).

-spec normalized_ast(t()) -> term().
normalized_ast(Unit) -> maps:get(normalized_ast, Unit).

-spec typed_module(t()) -> term().
typed_module(Unit) -> maps:get(typed_module, Unit).

-spec typed_declarations(t()) -> [term()].
typed_declarations(Unit) -> maps:get(typed_declarations, Unit).

-spec type_env(t()) -> term().
type_env(Unit) -> maps:get(type_env, Unit).

-spec imports(t()) -> [term()].
imports(Unit) -> maps:get(imports, Unit).

-spec exports(t()) -> [term()].
exports(Unit) -> maps:get(exports, Unit).

-spec options(t()) -> map().
options(Unit) -> maps:get(options, Unit).

-spec source_identity(t()) -> term().
source_identity(Unit) -> maps:get(source_identity, Unit).

-spec validation_state(t()) -> validation_state().
validation_state(Unit) -> maps:get(validation_state, Unit).

-spec symbols(t()) -> [symbol()].
symbols(Unit) -> maps:get(symbols, Unit).

-spec callables(t()) -> catena_call_resolution:inventory().
callables(Unit) -> maps:get(callables, Unit).

-spec import_resolution(t()) -> catena_import_resolution:resolution().
import_resolution(Unit) -> maps:get(import_resolution, Unit).

-spec trait_inventory(t()) -> catena_trait_dictionary:inventory().
trait_inventory(Unit) -> maps:get(trait_inventory, Unit).

-spec effect_inventory(t()) -> catena_effect_resolution:inventory().
effect_inventory(Unit) -> maps:get(effect_inventory, Unit).

-spec effect_operations(t()) -> #{{atom(), atom()} => map()}.
effect_operations(Unit) ->
    catena_effect_resolution:operations(effect_inventory(Unit)).

-spec effect_uses(t()) -> [map()].
effect_uses(Unit) ->
    catena_effect_resolution:uses(effect_inventory(Unit)).

-spec effect_handlers(t()) -> [map()].
effect_handlers(Unit) ->
    catena_effect_resolution:handlers(effect_inventory(Unit)).

-spec effectful_transforms(t()) -> #{atom() => non_neg_integer()}.
effectful_transforms(Unit) ->
    maps:get(effectful_transforms, Unit).

-spec runtime_dependencies(t()) -> [map()].
runtime_dependencies(Unit) ->
    maps:get(runtime_dependencies, Unit).

-spec artifact_dependencies(t()) -> [map()].
artifact_dependencies(Unit) ->
    maps:get(artifact_dependencies, Unit).

-spec interface(t()) -> catena_module_interface:interface().
interface(Unit) ->
    maps:get(interface, Unit).

-spec locations(t()) -> location_index().
locations(Unit) -> maps:get(locations, Unit).

-spec control_modes(t()) -> catena_control_mode:inventory().
control_modes(Unit) -> maps:get(control_modes, Unit).

-spec control_ir(t()) -> catena_control_ir:ir() | pending.
control_ir(Unit) -> maps:get(control_ir, Unit).

-spec control_validation(t()) ->
    catena_control_validate:report() | pending.
control_validation(Unit) -> maps:get(control_validation, Unit).

-spec dispositions(t()) -> [map()].
dispositions(Unit) -> maps:get(dispositions, Unit).

%% @doc Attach the complete declaration classification produced by the
%% disposition pass.  The pass must return one entry for every declaration.
-spec with_dispositions(t(), [map()]) -> {ok, t()} | {error, term()}.
with_dispositions(Unit, Dispositions) when
    is_list(Dispositions)
->
    {module, _, _, _, Declarations, _} = normalized_ast(Unit),
    case valid_disposition_indexes(
        Dispositions,
        length(Declarations)
    ) of
        true ->
            {ok, Unit#{dispositions := Dispositions}};
        false ->
            DeclarationDispositionCount = length([
                Disposition
                || Disposition <- Dispositions,
                   maps:get(
                       subject,
                       Disposition,
                       declaration
                   ) =:= declaration
            ]),
            {error,
                {invalid_compilation_unit,
                    {disposition_count_mismatch,
                        length(Declarations),
                        DeclarationDispositionCount}}}
    end.

%% @doc Replace control-mode evidence after a validated re-analysis.
-spec with_control_modes(t(), catena_control_mode:inventory()) ->
    {ok, t()} | {error, term()}.
with_control_modes(Unit, ControlModes) ->
    Candidate = Unit#{control_modes => ControlModes},
    case catena_control_mode:is_inventory(ControlModes) andalso
        is_compilation_unit(Candidate)
    of
        true -> {ok, Candidate};
        false ->
            {error, {invalid_compilation_unit, invalid_control_modes}}
    end.

%% @doc Attach a canonical selective-CPS graph.
-spec with_control_ir(t(), catena_control_ir:ir()) ->
    {ok, t()} | {error, term()}.
with_control_ir(Unit, ControlIR) ->
    Candidate = Unit#{control_ir => ControlIR},
    case catena_control_ir:is_ir(ControlIR) andalso
        is_compilation_unit(Candidate)
    of
        true -> {ok, Candidate};
        false ->
            {error, {invalid_compilation_unit, invalid_control_ir}}
    end.

%% @doc Attach a successful fail-closed control-graph validation report.
-spec with_control_validation(t(), catena_control_validate:report()) ->
    {ok, t()} | {error, term()}.
with_control_validation(Unit, Report) ->
    Candidate = Unit#{control_validation => Report},
    case catena_control_validate:is_report(Report) andalso
        is_compilation_unit(Candidate)
    of
        true -> {ok, Candidate};
        false ->
            {error,
                {invalid_compilation_unit,
                    invalid_control_validation}}
    end.

validate_evidence(ValidationState) when is_map(ValidationState) ->
    Missing = [
        Stage
        || Stage <- validated_stages(),
           maps:get(Stage, ValidationState, missing) =/= passed
    ],
    case Missing of
        [] -> ok;
        _ -> {error, {invalid_compilation_unit, {missing_validations, Missing}}}
    end;
validate_evidence(_) ->
    {error, {invalid_compilation_unit, invalid_validation_state}}.

valid_disposition_indexes(Dispositions, Count) ->
    Indexes = [
        maps:get(index, Disposition, invalid)
        || Disposition <- Dispositions,
           maps:get(subject, Disposition, declaration) =:= declaration
    ],
    Indexes =:= lists:seq(1, Count).

unclassified_dispositions(Declarations) ->
    [
        #{
            subject => declaration,
            index => Index,
            disposition => unclassified,
            declaration => Declaration,
            location => source_location(Declaration)
        }
        || {Index, Declaration} <-
            lists:zip(lists:seq(1, length(Declarations)), Declarations)
    ].

collect_symbols(Module, Exports, Imports, Declarations, ModuleLocation) ->
    ModuleSymbol = symbol(module, Module, undefined, Module, ModuleLocation),
    ImportSymbols = [
        import_symbol(Module, Import)
        || Import <- Imports
    ],
    DeclarationSymbols = lists:append([
        declaration_symbols(Module, Declaration)
        || Declaration <- Declarations
    ]),
    ExportSymbols = [
        export_symbol(Module, Export, ModuleLocation)
        || Export <- Exports
    ],
    [ModuleSymbol | ImportSymbols ++ DeclarationSymbols ++ ExportSymbols].

import_symbol(
    Module,
    {import, ImportedModule, Items, Qualified, Alias, Location}
) ->
    (symbol(import, ImportedModule, undefined, Module, Location))#{
        items => Items,
        qualified => Qualified,
        alias => Alias
    }.

export_symbol(Module, {export_transform, Name}, Location) ->
    symbol(export_transform, Name, undefined, Module, Location);
export_symbol(Module, {export_type, Name}, Location) ->
    symbol(export_type, Name, undefined, Module, Location);
export_symbol(Module, {export_trait, Name}, Location) ->
    symbol(export_trait, Name, undefined, Module, Location);
export_symbol(Module, {export_effect, Name}, Location) ->
    symbol(export_effect, Name, undefined, Module, Location);
export_symbol(Module, Other, Location) ->
    symbol(export, Other, undefined, Module, Location).

declaration_symbols(
    Module,
    {transform_decl, Name, Type, Clauses, Location}
) ->
    [symbol(
        transform,
        Name,
        transform_arity(Type, Clauses),
        Module,
        Location
    )];
declaration_symbols(
    Module,
    {type_decl, Name, _Params, Constructors, _Derives, Location}
) ->
    [symbol(type, Name, undefined, Module, Location) |
        [
            (symbol(
                constructor,
                ConstructorName,
                length(Fields),
                Module,
                ConstructorLocation
            ))#{owner => Name}
            || {constructor, ConstructorName, Fields, ConstructorLocation} <-
                Constructors
        ]];
declaration_symbols(Module, {effect_decl, Name, Operations, Location}) ->
    [symbol(effect, Name, undefined, Module, Location) |
        [
            (symbol(
                effect_operation,
                OperationName,
                type_arity(Type),
                Module,
                OperationLocation
            ))#{owner => Name}
            || {effect_operation, OperationName, Type, OperationLocation} <-
                Operations
        ]];
declaration_symbols(
    Module,
    {trait_decl, Name, _Params, _Extends, Members, Location}
) ->
    [symbol(trait, Name, undefined, Module, Location) |
        [
            trait_member_symbol(Module, Name, Member)
            || Member <- Members
        ]];
declaration_symbols(
    Module,
    {instance_decl, Trait, TypeArguments, _Constraints, _Methods, Location}
) ->
    [symbol(
        instance,
        {Trait, TypeArguments},
        length(TypeArguments),
        Module,
        Location
    )];
declaration_symbols(Module, {test_decl, Name, _Body, Location}) ->
    [symbol(test, Name, 0, Module, Location)];
declaration_symbols(Module, {property_decl, Name, _Body, Location}) ->
    [symbol(property, Name, 0, Module, Location)];
declaration_symbols(Module, Declaration) ->
    [symbol(
        declaration,
        element_or_undefined(2, Declaration),
        undefined,
        Module,
        source_location(Declaration)
    )].

trait_member_symbol(
    Module,
    Trait,
    {trait_sig, Name, Type, Location}
) ->
    (symbol(trait_method, Name, type_arity(Type), Module, Location))#{
        owner => Trait
    };
trait_member_symbol(
    Module,
    Trait,
    {trait_default, Name, Params, _Body, Location}
) ->
    (symbol(trait_method, Name, length(Params), Module, Location))#{
        owner => Trait,
        default => true
    };
trait_member_symbol(Module, Trait, Member) ->
    (symbol(
        trait_member,
        element_or_undefined(2, Member),
        undefined,
        Module,
        source_location(Member)
    ))#{owner => Trait}.

symbol(Kind, Name, Arity, Module, Location) ->
    #{
        kind => Kind,
        name => Name,
        arity => Arity,
        module => Module,
        location => Location
    }.

transform_arity(_Type, [{transform_clause, Patterns, _, _, _} | _]) ->
    length(Patterns);
transform_arity(Type, []) ->
    type_arity(Type).

type_arity(undefined) ->
    undefined;
type_arity({type_fun, _From, To, _Location}) ->
    case type_arity(To) of
        undefined -> 1;
        Rest -> 1 + Rest
    end;
type_arity({type_effect, Type, _Effects, _Location}) ->
    type_arity(Type);
type_arity(_) ->
    0.

collect_locations(ModuleLocation, Imports, Declarations) ->
    Initial = #{
        module => ModuleLocation,
        imports => [source_location(Import) || Import <- Imports],
        declarations => [],
        clauses => [],
        patterns => [],
        expressions => []
    },
    lists:foldl(fun collect_term_location/2, Initial, Declarations).

collect_term_location(Term, Acc) when is_tuple(Term) ->
    Tag = element(1, Term),
    Acc1 = add_tag_location(Tag, source_location(Term), Acc),
    lists:foldl(
        fun collect_term_location/2,
        Acc1,
        tl(tuple_to_list(Term))
    );
collect_term_location(Terms, Acc) when is_list(Terms) ->
    lists:foldl(fun collect_term_location/2, Acc, Terms);
collect_term_location(_Term, Acc) ->
    Acc.

add_tag_location(Tag, Location, Acc) ->
    case {location_bucket(Tag), Location} of
        {undefined, _} -> Acc;
        {_, undefined} -> Acc;
        {Bucket, _} -> Acc#{Bucket := maps:get(Bucket, Acc) ++ [Location]}
    end.

location_bucket(Tag) when
    Tag =:= type_decl;
    Tag =:= transform_decl;
    Tag =:= effect_decl;
    Tag =:= trait_decl;
    Tag =:= instance_decl;
    Tag =:= test_decl;
    Tag =:= property_decl
->
    declarations;
location_bucket(transform_clause) -> clauses;
location_bucket(match_clause) -> clauses;
location_bucket(Tag) ->
    Name = atom_to_list(Tag),
    case lists:prefix("pat_", Name) of
        true ->
            patterns;
        false ->
            case is_expression_tag(Tag) of
                true -> expressions;
                false -> undefined
            end
    end.

is_expression_tag(Tag) ->
    lists:member(
        Tag,
        [
            var,
            literal,
            lambda,
            app,
            if_expr,
            let_expr,
            match_expr,
            tuple_expr,
            list_expr,
            cons_expr,
            record_expr,
            field_access,
            binary_op,
            unary_op,
            constructor,
            perform_expr,
            handle_expr,
            do_expr
        ]
    ).

source_location(Term) ->
    catena_backend_error:source_location(Term).

element_or_undefined(Index, Term) when
    is_tuple(Term),
    tuple_size(Term) >= Index
->
    element(Index, Term);
element_or_undefined(_Index, _Term) ->
    undefined.

effect_runtime_dependencies(EffectfulTransforms)
  when map_size(EffectfulTransforms) =:= 0 ->
    [];
effect_runtime_dependencies(_EffectfulTransforms) ->
    [
        #{module => catena_effect_runtime, version => 1},
        #{module => catena_effect_system, version => 1}
    ].

control_runtime_dependencies(ControlModes) ->
    case lists:any(
        fun(Entry) -> maps:get(mode, Entry) =:= resumable end,
        catena_control_mode:entries(ControlModes)
    ) of
        true ->
            [
                #{module => catena_effect_runtime, version => 1},
                #{
                    module => catena_resumption_runtime,
                    version => 1,
                    features => catena_resumption_runtime:features()
                }
            ];
        false ->
            []
    end.

update_control_interface(Interface, ControlModes, ArtifactDependencies) ->
    case catena_module_interface:with_artifact_dependencies(
        Interface,
        ArtifactDependencies
    ) of
        {ok, DependencyInterface} ->
            catena_module_interface:with_control_modes(
                DependencyInterface,
                ControlModes
            );
        {error, _} = Error ->
            Error
    end.
