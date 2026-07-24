%% @doc Catena compilation pipeline.
%%
%% This module orchestrates the complete compilation process:
%% 1. Lexical analysis (catena_lexer)
%% 2. Parsing (catena_parser)
%% 3. Semantic analysis (catena_semantic)
%% 4. Type checking (catena_infer)
%%
%% @end
-module(catena_compile).

-export([
    compile_file/1,
    compile_file_to_core/1,
    compile_file_to_core/2,
    compile_string/1,
    compile_string/2,
    compile_string_to_core/1,
    compile_string_to_core/2,
    build_type_env/1,
    build_module_exports_env/1,
    process_imports/1,
    type_check_module/2
]).

%% @doc Compile a Catena source file.
-spec compile_file(string()) -> {ok, term()} | {error, term()}.
compile_file(Path) ->
    case read_source_file(Path) of
        {ok, Source} ->
            compile_string(Source);
        {error, _} = Error ->
            Error
    end.

%% @doc Compile a Catena source file to a Core Erlang module.
-spec compile_file_to_core(string()) -> {ok, cerl:cerl()} | {error, term()}.
compile_file_to_core(Path) ->
    compile_file_to_core(Path, #{}).

%% @doc Compile a Catena source file to Core Erlang with options.
-spec compile_file_to_core(string(), map()) ->
    {ok, cerl:cerl()} | {error, term()}.
compile_file_to_core(Path, Opts) ->
    case read_source_file(Path) of
        {ok, Source} ->
            compile_string_to_core(Source, Opts);
        {error, _} = Error ->
            Error
    end.

read_source_file(Path) ->
    case filename:extension(Path) of
        ".cat" ->
            case file:read_file(Path) of
                {ok, Binary} ->
                    {ok, binary_to_list(Binary)};
                {error, Reason} ->
                    {error, {file_error, Path, Reason}}
            end;
        Extension ->
            {error, {invalid_file_extension, Path, Extension}}
    end.

%% @doc Compile a Catena source string.
-spec compile_string(string()) -> {ok, term()} | {error, term()}.
compile_string(Source) ->
    compile_string(Source, #{}).

%% @doc Compile a Catena source string with options.
%% Options:
%%   - process_imports: boolean() - whether to load and process imports (default: true)
%%   - import_env: env() - pre-built environment from imports (skips import processing)
-spec compile_string(string(), map()) -> {ok, term()} | {error, term()}.
compile_string(Source, Opts) ->
    case analyze_string(Source) of
        {ok, AnalyzedAST} ->
            type_check_with_imports(AnalyzedAST, Opts);
        {error, _} = Error ->
            Error
    end.

%% @doc Compile a Catena source string to a Core Erlang module.
%%
%% The backend runs only after the canonical frontend and type-checking stages
%% have succeeded. `compile_string/1,2` remains the typed-module API.
-spec compile_string_to_core(string()) ->
    {ok, cerl:cerl()} | {error, term()}.
compile_string_to_core(Source) ->
    compile_string_to_core(Source, #{}).

%% @doc Compile a Catena source string to Core Erlang with options.
%% Existing compiler options (`process_imports` and `import_env`) are accepted.
%% Backend options are supplied under the `codegen_opts` map key.
-spec compile_string_to_core(string(), map()) ->
    {ok, cerl:cerl()} | {error, term()}.
compile_string_to_core(Source, Opts) ->
    case analyze_string(Source) of
        {ok, AnalyzedAST} ->
            case type_check_with_imports(AnalyzedAST, Opts) of
                {ok, {typed_module, _, _, _}} ->
                    CodegenOpts = maps:get(codegen_opts, Opts, #{}),
                    catena_codegen_module:generate_module(
                        AnalyzedAST,
                        CodegenOpts
                    );
                {error, _} = TypeError ->
                    TypeError
            end;
        {error, _} = Error ->
            Error
    end.

analyze_string(Source) ->
    case catena_lexer:string(Source) of
        {ok, Tokens, _EndLoc} ->
            case catena_parser:parse(Tokens) of
                {ok, AST} ->
                    case catena_semantic:analyze(AST) of
                        {ok, AnalyzedAST} ->
                            {ok, AnalyzedAST};
                        {error, _} = SemanticError ->
                            SemanticError
                    end;
                {error, _} = ParseError ->
                    ParseError
            end;
        {error, LexError, _} ->
            {error, {lex_error, LexError}}
    end.

%% @doc Type check with import processing.
type_check_with_imports({module, Name, Exports, Imports, Declarations, Location} = AST, Opts) ->
    %% Get imported environment
    ImportedEnvResult = case maps:get(import_env, Opts, undefined) of
        undefined ->
            %% Process imports unless disabled
            case maps:get(process_imports, Opts, true) of
                true -> process_imports(Imports);
                false -> {ok, catena_type_env:empty()}
            end;
        PrebuiltEnv ->
            {ok, PrebuiltEnv}
    end,
    case ImportedEnvResult of
        {ok, ImportedEnv} ->
            type_check_with_env(AST, ImportedEnv);
        {error, _} = Error ->
            Error
    end.

%% @doc Type check with a pre-built imported environment.
type_check_with_env({module, Name, _Exports, _Imports, Declarations, _Location}, ImportedEnv) ->
    %% Build kind environment and validate HKT usage
    KindEnv = catena_kind:build_kind_env(Declarations),
    case catena_kind:validate_hkt(Declarations, KindEnv) of
        {ok, _} ->
            %% Build initial type environment from local declarations
            case build_type_env(Declarations) of
                {ok, LocalEnv} ->
                    %% Merge imported env with local env
                    %% Local definitions shadow imports
                    Env = catena_type_env:merge(ImportedEnv, LocalEnv),
                    %% Type check all declarations
                    case type_check_declarations(Declarations, Env) of
                        {ok, TypedDecls} ->
                            {ok, {typed_module, Name, TypedDecls, Env}};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, KindErrors} ->
            {error, {kind_errors, KindErrors}}
    end.

%% @doc Type check a module AST.
-spec type_check(term()) -> {ok, term()} | {error, term()}.
type_check({module, Name, _Exports, _Imports, Declarations, _Location}) ->
    %% Build kind environment and validate HKT usage
    KindEnv = catena_kind:build_kind_env(Declarations),
    case catena_kind:validate_hkt(Declarations, KindEnv) of
        {ok, _} ->
            %% Build initial type environment from declarations
            case build_type_env(Declarations) of
                {ok, Env} ->
                    %% Type check all declarations
                    case type_check_declarations(Declarations, Env) of
                        {ok, TypedDecls} ->
                            {ok, {typed_module, Name, TypedDecls, Env}};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, KindErrors} ->
            {error, {kind_errors, KindErrors}}
    end.

%% @doc Build type environment from declarations.
%% Registers type constructors and transform signatures.
-spec build_type_env([term()]) -> {ok, term()} | {error, term()}.
build_type_env(Declarations) ->
    Env0 = catena_type_env:empty(),
    %% Add built-in operators
    Env = add_builtins(Env0),
    build_type_env(Declarations, Env).

%% @doc Add built-in operators and functions to environment.
add_builtins(Env) ->
    State = catena_infer_state:new(),

    %% List concat: ++ : List a -> List a -> List a
    {{tvar, A1}, State1} = catena_types:fresh_var(State),
    ListA1 = catena_types:tapp(catena_types:tcon(list), [catena_types:tvar(A1)]),
    PlusPlusType = catena_types:tfun(
        ListA1,
        catena_types:tfun(ListA1, ListA1, catena_types:empty_effects()),
        catena_types:empty_effects()),
    PlusPlusScheme = catena_type_scheme:generalize(PlusPlusType, sets:new()),
    Env1 = catena_type_env:extend(Env, plus_plus, PlusPlusScheme),

    %% Boolean and: && : Bool -> Bool -> Bool
    BoolType = catena_types:tcon(bool),
    AndType = catena_types:tfun(
        BoolType,
        catena_types:tfun(BoolType, BoolType, catena_types:empty_effects()),
        catena_types:empty_effects()),
    AndScheme = catena_type_scheme:mono(AndType),
    Env2 = catena_type_env:extend(Env1, and_op, AndScheme),

    %% Boolean or: || : Bool -> Bool -> Bool
    OrType = catena_types:tfun(
        BoolType,
        catena_types:tfun(BoolType, BoolType, catena_types:empty_effects()),
        catena_types:empty_effects()),
    OrScheme = catena_type_scheme:mono(OrType),
    Env3 = catena_type_env:extend(Env2, or_op, OrScheme),

    %% Cons: :: : a -> List a -> List a
    {{tvar, A2}, _State2} = catena_types:fresh_var(State1),
    ListA2 = catena_types:tapp(catena_types:tcon(list), [catena_types:tvar(A2)]),
    ConsType = catena_types:tfun(
        catena_types:tvar(A2),
        catena_types:tfun(ListA2, ListA2, catena_types:empty_effects()),
        catena_types:empty_effects()),
    ConsScheme = catena_type_scheme:generalize(ConsType, sets:new()),
    Env4 = catena_type_env:extend(Env3, '::', ConsScheme),

    %% Empty list: [] : List a
    {{tvar, A3}, _State3} = catena_types:fresh_var(State1),
    EmptyListType = catena_types:tapp(catena_types:tcon(list), [catena_types:tvar(A3)]),
    EmptyListScheme = catena_type_scheme:generalize(EmptyListType, sets:new()),
    catena_type_env:extend(Env4, '[]', EmptyListScheme).

build_type_env([], Env) ->
    {ok, Env};
build_type_env([Decl | Rest], Env) ->
    case add_decl_to_env(Decl, Env) of
        {ok, NewEnv} ->
            build_type_env(Rest, NewEnv);
        {error, _} = Error ->
            Error
    end.

%% @doc Build type environment from a module's exported declarations only.
%% Used when importing a module - only exported symbols are visible.
-spec build_module_exports_env(term()) -> {ok, term()} | {error, term()}.
build_module_exports_env({module, _Name, Exports, _Imports, Declarations, _Location}) ->
    %% Get the set of exported names
    ExportedNames = get_exported_names(Exports),
    %% Filter declarations to only exported ones
    ExportedDecls = filter_exported_declarations(Declarations, ExportedNames),
    %% Build environment from exported declarations
    build_type_env(ExportedDecls);
build_module_exports_env(_) ->
    {error, invalid_module_ast}.

%% @doc Get the set of exported names from export declarations.
get_exported_names(Exports) ->
    sets:from_list([get_export_name(E) || E <- Exports]).

get_export_name({export_transform, Name}) -> Name;
get_export_name({export_type, Name}) -> Name;
get_export_name({export_trait, Name}) -> Name;
get_export_name({export_effect, Name}) -> Name;
get_export_name(_) -> undefined.

%% @doc Filter declarations to only those that are exported.
filter_exported_declarations(Declarations, ExportedNames) ->
    case sets:is_empty(ExportedNames) of
        true ->
            %% If no exports specified, export all (like Haskell default)
            Declarations;
        false ->
            lists:filter(
                fun(Decl) -> is_exported(Decl, ExportedNames) end,
                Declarations
            )
    end.

is_exported({type_decl, Name, _, _, _, _}, ExportedNames) ->
    sets:is_element(Name, ExportedNames);
is_exported({transform_decl, Name, _, _, _}, ExportedNames) ->
    sets:is_element(Name, ExportedNames);
is_exported({trait_decl, Name, _, _, _, _}, ExportedNames) ->
    sets:is_element(Name, ExportedNames);
is_exported({effect_decl, Name, _, _}, ExportedNames) ->
    sets:is_element(Name, ExportedNames);
is_exported(_, _ExportedNames) ->
    false.

%% @doc Process import declarations and build combined type environment.
%% Returns merged environment from all imported modules.
-spec process_imports([term()]) -> {ok, term()} | {error, term()}.
process_imports(Imports) ->
    SearchPaths = catena_module_loader:get_default_search_paths(),
    process_imports(Imports, SearchPaths, catena_type_env:empty()).

process_imports([], _SearchPaths, Env) ->
    {ok, Env};
process_imports([{import, ModuleName, Items, Qualified, Alias, _Loc} | Rest],
                SearchPaths, Env) ->
    case catena_module_loader:load_module(ModuleName, SearchPaths) of
        {ok, ModuleAST} ->
            case build_module_exports_env(ModuleAST) of
                {ok, ModuleEnv} ->
                    ImportEnv = prepare_import_env(
                        ModuleName,
                        Items,
                        Qualified,
                        Alias,
                        ModuleEnv
                    ),
                    %% Later imports shadow earlier imports.
                    MergedEnv = catena_type_env:merge(Env, ImportEnv),
                    process_imports(Rest, SearchPaths, MergedEnv);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
process_imports([{import, ModuleName, Loc} | Rest], SearchPaths, Env) ->
    %% Legacy compatibility for manually constructed ASTs.
    process_imports(
        [{import, ModuleName, all, false, undefined, Loc} | Rest],
        SearchPaths,
        Env
    );
process_imports([_Other | Rest], SearchPaths, Env) ->
    %% Skip invalid import entries
    process_imports(Rest, SearchPaths, Env).

prepare_import_env(ModuleName, Items, Qualified, Alias, ModuleEnv) ->
    SelectedEnv = case Items of
        all ->
            ModuleEnv;
        ItemList when is_list(ItemList) ->
            maps:with(ItemList, ModuleEnv)
    end,
    case Qualified of
        false ->
            SelectedEnv;
        true ->
            Prefix = case Alias of
                undefined -> ModuleName;
                _ -> Alias
            end,
            maps:from_list([
                {qualify_import_name(Prefix, Name), Scheme}
             || {Name, Scheme} <- maps:to_list(SelectedEnv)
            ])
    end.

qualify_import_name(Prefix, Name) ->
    list_to_atom(atom_to_list(Prefix) ++ "." ++ atom_to_list(Name)).

%% @doc Add a declaration to the type environment.
add_decl_to_env({type_decl, Name, Params, Constructors, _Derives, _Location}, Env) ->
    %% Add constructor types to environment
    add_constructors_to_env(Name, Params, Constructors, Env);

add_decl_to_env({transform_decl, Name, Type, _Clauses, _Location}, Env) ->
    %% If transform has a type signature, add it to environment
    case Type of
        undefined ->
            %% No signature - will be inferred later
            {ok, Env};
        TypeSig ->
            %% Convert type signature to internal type representation
            case convert_type_sig(TypeSig) of
                {ok, InternalType} ->
                    Scheme = catena_type_scheme:mono(InternalType),
                    {ok, catena_type_env:extend(Env, Name, Scheme)};
                {error, _} = Error ->
                    Error
            end
    end;

add_decl_to_env({trait_decl, _Name, _Params, _Extends, Members, _Location}, Env) ->
    add_trait_members_to_env(Members, Env);

add_decl_to_env({instance_decl, _Trait, _Type, _Constraints, _Methods, _Location}, Env) ->
    %% TODO: Validate instance methods
    {ok, Env};

add_decl_to_env(_Other, Env) ->
    %% Skip unknown declarations
    {ok, Env}.

%% @doc Register trait method signatures so defaults and instances can call
%% methods from the same trait or an inherited trait.
add_trait_members_to_env([], Env) ->
    {ok, Env};
add_trait_members_to_env([{trait_sig, Name, Type, _Loc} | Rest], Env) ->
    case convert_type_sig(Type) of
        {ok, InternalType} ->
            Scheme = generalize_type(InternalType),
            add_trait_members_to_env(
                Rest,
                catena_type_env:extend(Env, Name, Scheme)
            );
        {error, _} = Error ->
            Error
    end;
add_trait_members_to_env([_Other | Rest], Env) ->
    add_trait_members_to_env(Rest, Env).

%% @doc Add type constructors to environment.
%% For type Maybe a = None | Some a, adds:
%%   None : Maybe a
%%   Some : a -> Maybe a
add_constructors_to_env(TypeName, TypeParams, Constructors, Env) ->
    ResultType = build_result_type(TypeName, TypeParams),
    add_constructors(Constructors, TypeParams, ResultType, Env).

add_constructors([], _TypeParams, _ResultType, Env) ->
    {ok, Env};
add_constructors([Constructor | Rest], TypeParams, ResultType, Env) ->
    case add_constructor(Constructor, TypeParams, ResultType, Env) of
        {ok, NewEnv} ->
            add_constructors(Rest, TypeParams, ResultType, NewEnv);
        {error, _} = Error ->
            Error
    end.

add_constructor({constructor, Name, [], _Loc}, _TypeParams, ResultType, Env) ->
    %% Nullary constructor: None : Maybe a
    Scheme = generalize_type(ResultType),
    {ok, catena_type_env:extend(Env, Name, Scheme)};

add_constructor({constructor, Name, ArgTypes, _Loc}, TypeParams, ResultType, Env) ->
    %% Constructor with arguments: Some : a -> Maybe a
    ConType = build_constructor_type(ArgTypes, TypeParams, ResultType),
    Scheme = generalize_type(ConType),
    {ok, catena_type_env:extend(Env, Name, Scheme)};

add_constructor(_Other, _TypeParams, _ResultType, Env) ->
    %% Skip malformed constructors
    {ok, Env}.

%% @doc Build result type for a type declaration.
%% For type Maybe a, returns Maybe<a>
build_result_type(TypeName, []) ->
    catena_types:tcon(TypeName);
build_result_type(TypeName, TypeParams) ->
    ParamTypes = [catena_types:tvar(param_to_var(P)) || P <- TypeParams],
    catena_types:tapp(catena_types:tcon(TypeName), ParamTypes).

%% @doc Build constructor function type.
%% For Some with arg type a, returns a -> Maybe a
build_constructor_type([], _TypeParams, ResultType) ->
    ResultType;
build_constructor_type([ArgType | Rest], TypeParams, ResultType) ->
    InternalArg = convert_constructor_arg(ArgType, TypeParams),
    RestType = build_constructor_type(Rest, TypeParams, ResultType),
    catena_types:tfun(InternalArg, RestType, catena_types:empty_effects()).

%% @doc Convert a constructor argument type to internal representation.
convert_constructor_arg({type_var, Name}, _TypeParams) ->
    catena_types:tvar(param_to_var(Name));
convert_constructor_arg({type_var, Name, _Loc}, _TypeParams) ->
    catena_types:tvar(param_to_var(Name));
convert_constructor_arg({type_con, Name}, _TypeParams) ->
    catena_types:tcon(internal_type_name(Name));
convert_constructor_arg({type_con, Name, _Loc}, _TypeParams) ->
    catena_types:tcon(internal_type_name(Name));
convert_constructor_arg({type_app, Con, Args}, TypeParams) ->
    InternalCon = convert_constructor_arg(Con, TypeParams),
    InternalArgs = [convert_constructor_arg(A, TypeParams) || A <- Args],
    case InternalCon of
        {tcon, ConName} ->
            catena_types:tapp(catena_types:tcon(ConName), InternalArgs);
        _ ->
            %% Fallback for complex cases
            catena_types:tapp(InternalCon, InternalArgs)
    end;
convert_constructor_arg({type_app, Con, Args, _Loc}, TypeParams) ->
    convert_constructor_arg({type_app, Con, Args}, TypeParams);
convert_constructor_arg(_Other, _TypeParams) ->
    %% Fallback for unknown types
    catena_types:tcon(unknown).

%% @doc Convert type parameter name to type variable ID.
%% Uses atom_to_list to create a consistent mapping.
param_to_var(Name) when is_atom(Name) ->
    erlang:phash2(Name, 1000000).

%% @doc Generalize a type into a type scheme.
generalize_type(Type) ->
    catena_type_scheme:generalize(Type, sets:new()).

%% @doc Convert a type signature from parser AST to internal type.
convert_type_sig({tfun, From, To, Effects}) ->
    case {convert_type_sig(From), convert_type_sig(To)} of
        {{ok, FromType}, {ok, ToType}} ->
            EffectSet = convert_effects(Effects),
            {ok, catena_types:tfun(FromType, ToType, EffectSet)};
        {{error, _} = E, _} -> E;
        {_, {error, _} = E} -> E
    end;
convert_type_sig({type_fun, From, {type_effect, To, Effects, _InnerLoc}, _OuterLoc}) ->
    case {convert_type_sig(From), convert_type_sig(To)} of
        {{ok, FromType}, {ok, ToType}} ->
            {ok, catena_types:tfun(FromType, ToType, convert_effects(Effects))};
        {{error, _} = E, _} -> E;
        {_, {error, _} = E} -> E
    end;
convert_type_sig({type_fun, From, To, _Loc}) ->
    case {convert_type_sig(From), convert_type_sig(To)} of
        {{ok, FromType}, {ok, ToType}} ->
            {ok, catena_types:tfun(FromType, ToType, catena_types:empty_effects())};
        {{error, _} = E, _} -> E;
        {_, {error, _} = E} -> E
    end;
convert_type_sig({type_effect, Type, _Effects, _Loc}) ->
    convert_type_sig(Type);
convert_type_sig({constrained_type, _Constraints, Type, _Loc}) ->
    convert_type_sig(Type);
convert_type_sig({type_forall, _Variables, Type, _Loc}) ->
    convert_type_sig(Type);
convert_type_sig({type_var, Name, _Loc}) ->
    {ok, catena_types:tvar(param_to_var(Name))};
convert_type_sig({type_var, Name}) ->
    {ok, catena_types:tvar(param_to_var(Name))};
convert_type_sig({type_con, Name, _Loc}) ->
    {ok, catena_types:tcon(internal_type_name(Name))};
convert_type_sig({type_con, Name}) ->
    {ok, catena_types:tcon(internal_type_name(Name))};
convert_type_sig({type_app, Con, Args, _Loc}) ->
    convert_type_sig({type_app, Con, Args});
convert_type_sig({type_app, Con, Args}) ->
    case convert_type_sig(Con) of
        {ok, InternalCon} ->
            case convert_type_args(Args) of
                {ok, ArgTypes} ->
                    {ok, catena_types:tapp(InternalCon, ArgTypes)};
                Error -> Error
            end;
        Error -> Error
    end;
convert_type_sig({type_tuple, Elements, _Loc}) ->
    case convert_type_args(Elements) of
        {ok, ElementTypes} ->
            {ok, catena_types:ttuple(ElementTypes)};
        Error ->
            Error
    end;
convert_type_sig({type_record, Fields, _Row, _Loc}) ->
    convert_record_type(Fields, []);
convert_type_sig(Other) ->
    {error, {unknown_type, Other}}.

convert_type_args([]) ->
    {ok, []};
convert_type_args([Arg | Rest]) ->
    case convert_type_sig(Arg) of
        {ok, ArgType} ->
            case convert_type_args(Rest) of
                {ok, RestTypes} ->
                    {ok, [ArgType | RestTypes]};
                Error -> Error
            end;
        Error -> Error
    end.

convert_record_type([], Acc) ->
    {ok, catena_types:trecord(lists:reverse(Acc), closed)};
convert_record_type([{Name, Type} | Rest], Acc) ->
    case convert_type_sig(Type) of
        {ok, InternalType} ->
            convert_record_type(Rest, [{Name, InternalType} | Acc]);
        Error ->
            Error
    end.

internal_type_name('Bool') -> bool;
internal_type_name('Int') -> int;
internal_type_name('Float') -> float;
internal_type_name('String') -> string;
internal_type_name('Unit') -> unit;
internal_type_name('List') -> list;
internal_type_name(Name) -> Name.

convert_effects(undefined) ->
    catena_types:empty_effects();
convert_effects({effect_set, Effects}) ->
    catena_types:effect_set(Effects);
convert_effects(Effects) when is_list(Effects) ->
    catena_types:effect_set(Effects);
convert_effects(_) ->
    catena_types:empty_effects().

%% @doc Type check all declarations in a module.
-spec type_check_declarations([term()], term()) -> {ok, [term()]} | {error, term()}.
type_check_declarations(Declarations, Env) ->
    type_check_declarations(Declarations, Env, []).

type_check_declarations([], _Env, Acc) ->
    {ok, lists:reverse(Acc)};
type_check_declarations([Decl | Rest], Env, Acc) ->
    case type_check_declaration(Decl, Env) of
        {ok, TypedDecl, NewEnv} ->
            type_check_declarations(Rest, NewEnv, [TypedDecl | Acc]);
        {error, _} = Error ->
            Error
    end.

%% @doc Type check a single declaration.
type_check_declaration({type_decl, Name, Params, Constructors, Derives, Location}, Env) ->
    %% Type declarations are already processed in build_type_env
    {ok, {type_decl, Name, Params, Constructors, Derives, Location}, Env};

type_check_declaration({transform_decl, Name, Type, Clauses, Location}, Env) ->
    %% Type check transform body
    case type_check_transform(Name, Type, Clauses, Env) of
        {ok, InferredType} ->
            TypedDecl = {typed_transform, Name, InferredType, Clauses, Location},
            %% Add inferred type to environment for subsequent declarations
            Scheme = generalize_type(InferredType),
            NewEnv = catena_type_env:extend(Env, Name, Scheme),
            {ok, TypedDecl, NewEnv};
        {error, _} = Error ->
            Error
    end;

type_check_declaration({trait_decl, Name, Params, Extends, Members, Location}, Env) ->
    %% TODO: Validate trait members
    {ok, {trait_decl, Name, Params, Extends, Members, Location}, Env};

type_check_declaration({instance_decl, Trait, Type, Constraints, Methods, Location}, Env) ->
    %% Type check instance methods
    case type_check_instance_methods(Methods, Env) of
        {ok, TypedMethods} ->
            TypedDecl = {typed_instance, Trait, Type, Constraints, TypedMethods, Location},
            {ok, TypedDecl, Env};
        {error, _} = Error ->
            Error
    end;

type_check_declaration(Other, Env) ->
    {ok, Other, Env}.

%% @doc Type check a transform declaration.
type_check_transform(Name, DeclaredType, Clauses, Env) ->
    case Clauses of
        [] ->
            %% No implementation - signature only
            case DeclaredType of
                undefined ->
                    {error, {no_implementation, Name}};
                _ ->
                    convert_type_sig(DeclaredType)
            end;
        [{transform_clause, Patterns, _Guards, _Body, _Loc} | _] ->
            %% Infer every clause through one synthetic match. This keeps
            %% pattern bindings scoped to their clause and unifies argument
            %% and result types across the complete transform.
            Arity = length(Patterns),
            Expr = transform_clauses_to_lambda(Clauses, Arity),
            EffectExpr = transform_clauses_to_effect_expr(Clauses, Arity),
            %% Infer type
            case catena_infer:infer_expr(Expr, Env) of
                {ok, InferredType} ->
                    State0 = catena_infer_state:new(),
                    {SynthesizedEffects, State1} =
                        catena_effect_synthesis:synthesize(EffectExpr, State0),
                    {GeneratedConstraints, State2} =
                        catena_effect_constraints:generate_constraints(EffectExpr, State1),
                    {PropagatedConstraints, State3} =
                        catena_effect_constraints:propagate_constraints(GeneratedConstraints, State2),
                    case catena_effect_constraints:solve_constraints(PropagatedConstraints, State3) of
                        {ok, State4} ->
                            InferredType1 =
                                apply_effects_to_inferred_type(InferredType, SynthesizedEffects),
                            validate_declared_effects(
                                Name,
                                DeclaredType,
                                InferredType1,
                                SynthesizedEffects,
                                State4
                            );
                        {error, Constraints, Message} ->
                            {error, {effect_constraint_error, Name, Constraints, Message}}
                    end;
                {error, Errors} ->
                    {error, {type_error, Name, Errors}}
            end
    end.

%% @doc Type check instance methods.
type_check_instance_methods(Methods, Env) ->
    type_check_instance_methods(Methods, Env, []).

type_check_instance_methods([], _Env, Acc) ->
    {ok, lists:reverse(Acc)};
type_check_instance_methods([{Name, Lambda} | Rest], Env, Acc) ->
    %% Convert lambda to expression AST
    Expr = convert_expr(Lambda),
    %% Infer type
    case catena_infer:infer_expr(Expr, Env) of
        {ok, InferredType} ->
            TypedMethod = {Name, InferredType, Lambda},
            type_check_instance_methods(Rest, Env, [TypedMethod | Acc]);
        {error, Errors} ->
            {error, {method_type_error, Name, Errors}}
    end;
type_check_instance_methods([Other | _Rest], _Env, _Acc) ->
    {error, {invalid_method, Other}}.

%% @doc Convert all transform clauses to a lambda containing one match.
transform_clauses_to_lambda(Clauses, Arity) ->
    ParamNames = inference_param_names(Arity),
    Scrutinee = inference_scrutinee(ParamNames),
    InferClauses = [
        transform_clause_to_infer_clause(Clause, Arity)
        || Clause <- Clauses
    ],
    Match = {match, Scrutinee, InferClauses},
    lists:foldr(
        fun(ParamName, Body) -> {lam, ParamName, Body} end,
        Match,
        ParamNames
    ).

inference_param_names(Arity) ->
    [
        list_to_atom("$catena_infer_arg_" ++ integer_to_list(Index))
        || Index <- lists:seq(1, Arity)
    ].

inference_scrutinee([]) ->
    {lit, {bool, true}};
inference_scrutinee([ParamName]) ->
    {var, ParamName};
inference_scrutinee(ParamNames) ->
    {tuple, [{var, ParamName} || ParamName <- ParamNames]}.

transform_clause_to_infer_clause(
    {transform_clause, Patterns, Guards, Body, _Loc},
    Arity
) ->
    Pattern = combine_infer_patterns(Patterns, Arity),
    InferBody = convert_expr(Body),
    case convert_guards(Guards) of
        undefined ->
            {Pattern, InferBody};
        InferGuard ->
            {Pattern, InferGuard, InferBody}
    end.

combine_infer_patterns([], 0) ->
    {plit, {bool, true}};
combine_infer_patterns([Pattern], 1) ->
    convert_pattern(Pattern);
combine_infer_patterns(Patterns, _Arity) ->
    {ptuple, [convert_pattern(Pattern) || Pattern <- Patterns]}.

%% @doc Build the parser-shaped match used by effect synthesis and solving.
transform_clauses_to_effect_expr(Clauses, Arity) ->
    Location = transform_clauses_location(Clauses),
    ParamNames = inference_param_names(Arity),
    Scrutinee = source_scrutinee(ParamNames, Location),
    Cases = [
        transform_clause_to_effect_case(Clause, Arity)
        || Clause <- Clauses
    ],
    {match_expr, Scrutinee, Cases, Location}.

transform_clauses_location(
    [{transform_clause, _Patterns, _Guards, _Body, Location} | _]
) ->
    Location;
transform_clauses_location([]) ->
    undefined.

source_scrutinee([], Location) ->
    {literal, bool, true, Location};
source_scrutinee([ParamName], Location) ->
    {var, ParamName, Location};
source_scrutinee(ParamNames, Location) ->
    {tuple_expr,
        [{var, ParamName, Location} || ParamName <- ParamNames],
        Location}.

transform_clause_to_effect_case(
    {transform_clause, Patterns, Guards, Body, Location},
    Arity
) ->
    Pattern = combine_source_patterns(Patterns, Arity, Location),
    {match_clause, Pattern, combine_source_guards(Guards), Body, Location}.

combine_source_patterns([], 0, Location) ->
    {pat_literal, true, bool, Location};
combine_source_patterns([Pattern], 1, _Location) ->
    Pattern;
combine_source_patterns(Patterns, _Arity, Location) ->
    {pat_tuple, Patterns, Location}.

combine_source_guards(undefined) ->
    undefined;
combine_source_guards([]) ->
    undefined;
combine_source_guards([Guard]) ->
    Guard;
combine_source_guards([Guard | Rest]) ->
    {binary_op, 'and', Guard, combine_source_guards(Rest), undefined};
combine_source_guards(Guard) ->
    Guard.

%% @doc Convert parser expression to type inference expression.
convert_expr({var, true, _Loc}) ->
    {lit, {bool, true}};
convert_expr({var, false, _Loc}) ->
    {lit, {bool, false}};
convert_expr({var, Name, _Loc}) ->
    {var, Name};
convert_expr({literal, Type, Value, Loc}) ->
    {literal, Type, Value, Loc};
convert_expr({literal, {int, Value}, _Loc}) ->
    {lit, {int, Value}};
convert_expr({literal, {float, Value}, _Loc}) ->
    {lit, {float, Value}};
convert_expr({literal, {string, Value}, _Loc}) ->
    {lit, {string, Value}};
convert_expr({literal, {bool, Value}, _Loc}) ->
    {lit, {bool, Value}};
convert_expr({lambda, Params, Body, _Loc}) ->
    convert_lambda(Params, Body);
convert_expr({app, Func, Args, _Loc}) ->
    convert_application(Func, Args);
convert_expr({if_expr, Cond, Then, Else, _Loc}) ->
    {'if', convert_expr(Cond), convert_expr(Then), convert_expr(Else)};
convert_expr({let_expr, [{pat_var, Name, _}, Value], Body, _Loc}) ->
    {'let', Name, convert_expr(Value), convert_expr(Body)};
convert_expr({let_expr, [Pattern, Value], Body, _Loc}) ->
    %% Complex pattern - extract name or generate fresh
    Name = extract_pattern_name(Pattern),
    {'let', Name, convert_expr(Value), convert_expr(Body)};
convert_expr({tuple_expr, Elements, _Loc}) ->
    {tuple, [convert_expr(E) || E <- Elements]};
convert_expr({tuple, Elements, _Loc}) ->
    {tuple, [convert_expr(E) || E <- Elements]};
convert_expr({list_expr, Elements, Loc}) ->
    {list, [convert_expr(E) || E <- Elements], Loc};
convert_expr({list, Elements, _Loc}) ->
    {list, [convert_expr(E) || E <- Elements], undefined};
convert_expr({match_expr, Scrutinee, Cases, _Loc}) ->
    {match, convert_expr(Scrutinee), [convert_match_clause(C) || C <- Cases]};
convert_expr({perform_expr, Effect, Operation, Args, Loc}) ->
    {perform_expr, Effect, Operation, [convert_expr(Arg) || Arg <- Args], Loc};
convert_expr({handle_expr, Body, Handlers, Loc}) ->
    {handle_expr, convert_expr(Body), Handlers, Loc};
convert_expr({record_expr, Fields, _Base, _Loc}) ->
    {record, [{Name, convert_expr(Expr)} || {Name, Expr} <- Fields]};
convert_expr({field_access, Expr, FieldName, _Loc}) ->
    {field, convert_expr(Expr), FieldName};
convert_expr({binary_op, Op, Left, Right, Loc}) ->
    {binary_op, Op, convert_expr(Left), convert_expr(Right), Loc};
convert_expr({cons_expr, Head, Tail, Loc}) ->
    {cons, convert_expr(Head), convert_expr(Tail), Loc};
convert_expr(_Other) ->
    %% Fallback for unhandled expressions
    {lit, {int, 0}}.

convert_lambda([], Body) ->
    convert_expr(Body);
convert_lambda([{pat_var, Name, _Loc} | Rest], Body) ->
    {lam, Name, convert_lambda(Rest, Body)};
convert_lambda([_Pattern | Rest], Body) ->
    %% Complex pattern - generate fresh name
    FreshName = list_to_atom("_lam_" ++ integer_to_list(erlang:unique_integer([positive]))),
    {lam, FreshName, convert_lambda(Rest, Body)}.

%% @doc Extract name from a pattern, or generate a fresh name.
extract_pattern_name({pat_var, Name, _}) -> Name;
extract_pattern_name(_) ->
    list_to_atom("_pat_" ++ integer_to_list(erlang:unique_integer([positive]))).

convert_application(Func, []) ->
    convert_expr(Func);
convert_application(Func, Args) ->
    lists:foldl(
        fun(Arg, Acc) ->
            {app, Acc, convert_expr(Arg)}
        end,
        convert_expr(Func),
        Args
    ).

convert_match_clause({match_clause, Pattern, undefined, Body, _Loc}) ->
    {convert_pattern(Pattern), convert_expr(Body)};
convert_match_clause({match_clause, Pattern, Guards, Body, _Loc}) ->
    {convert_pattern(Pattern), convert_guards(Guards), convert_expr(Body)};
convert_match_clause(Other) ->
    Other.

convert_guards(undefined) ->
    undefined;
convert_guards([]) ->
    undefined;
convert_guards([Guard]) ->
    convert_expr(Guard);
convert_guards([Guard | Rest]) ->
    {binary_op, 'and', convert_expr(Guard), convert_guards(Rest), undefined};
convert_guards(Guard) ->
    convert_expr(Guard).

%% @doc Convert parser patterns to the representation consumed by inference.
convert_pattern({pat_var, true, _Loc}) ->
    {plit, {bool, true}};
convert_pattern({pat_var, false, _Loc}) ->
    {plit, {bool, false}};
convert_pattern({pat_var, Name, _Loc}) ->
    {pvar, Name};
convert_pattern({pat_wildcard, _Loc}) ->
    {pwild};
convert_pattern({pat_constructor, Name, Args, _Loc}) ->
    {pvariant, Name, [convert_pattern(Arg) || Arg <- Args]};
convert_pattern({pat_literal, Value, integer, _Loc}) ->
    {plit, {int, Value}};
convert_pattern({pat_literal, Value, float, _Loc}) ->
    {plit, {float, Value}};
convert_pattern({pat_literal, Value, string, _Loc}) ->
    {plit, {string, Value}};
convert_pattern({pat_literal, Value, bool, _Loc}) ->
    {plit, {bool, Value}};
convert_pattern({pat_tuple, Patterns, _Loc}) ->
    {ptuple, [convert_pattern(Pattern) || Pattern <- Patterns]};
convert_pattern({pat_record, Fields, _Loc}) ->
    {precord, [{Name, convert_pattern(Pattern)} || {Name, Pattern} <- Fields]};
convert_pattern({pat_list, Patterns, _Loc}) ->
    convert_list_pattern(Patterns);
convert_pattern({pat_cons, Head, Tail, _Loc}) ->
    {pvariant, '::', [convert_pattern(Head), convert_pattern(Tail)]};
convert_pattern({pat_as, Name, Pattern, _Loc}) ->
    {pas, Name, convert_pattern(Pattern)};
convert_pattern({pat_or, Patterns, _Loc}) ->
    {por, [convert_pattern(Pattern) || Pattern <- Patterns]};
convert_pattern(Pattern) ->
    Pattern.

convert_list_pattern([]) ->
    {pvariant, '[]', []};
convert_list_pattern([Pattern | Rest]) ->
    {pvariant, '::', [convert_pattern(Pattern), convert_list_pattern(Rest)]}.

extract_declared_effects(undefined) ->
    undefined;
extract_declared_effects({type_fun, _From, To, _Loc}) ->
    extract_declared_effects(To);
extract_declared_effects({type_effect, _Type, Effects, _Loc}) ->
    catena_types:effect_set(Effects);
extract_declared_effects(_) ->
    catena_types:empty_effects().

apply_effects_to_inferred_type({tfun, From, To, _ExistingEffects}, Effects) ->
    case catena_types:is_function_type(To) of
        true ->
            {tfun, From, apply_effects_to_inferred_type(To, Effects), catena_types:empty_effects()};
        false ->
            {tfun, From, To, Effects}
    end;
apply_effects_to_inferred_type(Type, _Effects) ->
    Type.

validate_declared_effects(_Name, undefined, InferredType, _SynthesizedEffects, _State) ->
    {ok, InferredType};
validate_declared_effects(Name, DeclaredType, InferredType, SynthesizedEffects, State) ->
    case convert_type_sig(DeclaredType) of
        {ok, _ExpectedType} ->
            DeclaredEffects = extract_declared_effects(DeclaredType),
            case DeclaredEffects of
                undefined ->
                    {ok, InferredType};
                _ ->
                    case catena_effect_constraints:solve_effect_constraint(
                        {effects_subset, declared_type, SynthesizedEffects, DeclaredEffects},
                        catena_types:empty_effects(),
                        State
                    ) of
                        {ok, _} ->
                            {ok, InferredType};
                        {error, Constraints, Message} ->
                            {error, {effect_mismatch, Name, Constraints, Message}}
                    end
            end;
        Error ->
            Error
    end.

%% @doc Type check a module with a given environment.
%% Exposed for testing.
-spec type_check_module(term(), term()) -> {ok, term()} | {error, term()}.
type_check_module(Module, Env) ->
    case Module of
        {module, Name, Exports, Imports, Declarations, Location} ->
            case type_check_declarations(Declarations, Env) of
                {ok, TypedDecls} ->
                    {ok, {typed_module, Name, TypedDecls, Env}};
                Error ->
                    Error
            end;
        _ ->
            {error, {invalid_module, Module}}
    end.
