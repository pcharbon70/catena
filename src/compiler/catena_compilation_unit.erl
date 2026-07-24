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
    locations/1,
    dispositions/1,
    with_dispositions/2
]).

-define(UNIT_VERSION, 1).

-opaque t() :: #{
    '$catena_compilation_unit' := pos_integer(),
    module_name := atom(),
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
    locations := location_index(),
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
    case validate_evidence(ValidationState) of
        ok ->
            Symbols = collect_symbols(
                Name,
                Exports,
                Imports,
                Declarations,
                ModuleLocation
            ),
            Locations = collect_locations(
                ModuleLocation,
                Imports,
                Declarations
            ),
            {ok, #{
                '$catena_compilation_unit' => ?UNIT_VERSION,
                module_name => Name,
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
                locations => Locations,
                dispositions => unclassified_dispositions(Declarations)
            }};
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

%% @doc Return whether a term satisfies the maintained unit envelope.
-spec is_compilation_unit(term()) -> boolean().
is_compilation_unit(#{
    '$catena_compilation_unit' := ?UNIT_VERSION,
    module_name := Name,
    normalized_ast := {module, Name, _, _, _, _},
    typed_module := {typed_module, Name, _, _},
    typed_declarations := TypedDeclarations,
    options := Options,
    validation_state := ValidationState,
    symbols := Symbols,
    locations := Locations,
    dispositions := Dispositions
}) ->
    is_atom(Name) andalso
        is_list(TypedDeclarations) andalso
        is_map(Options) andalso
        is_list(Symbols) andalso
        is_map(Locations) andalso
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

-spec locations(t()) -> location_index().
locations(Unit) -> maps:get(locations, Unit).

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
