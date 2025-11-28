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
    compile_string/1,
    compile_string/2,
    build_type_env/1,
    build_module_exports_env/1,
    process_imports/1,
    type_check_module/2
]).

%% @doc Compile a Catena source file.
-spec compile_file(string()) -> {ok, term()} | {error, term()}.
compile_file(Path) ->
    case file:read_file(Path) of
        {ok, Binary} ->
            compile_string(binary_to_list(Binary));
        {error, Reason} ->
            {error, {file_error, Path, Reason}}
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
    case catena_lexer:string(Source) of
        {ok, Tokens, _EndLoc} ->
            case catena_parser:parse(Tokens) of
                {ok, AST} ->
                    case catena_semantic:analyze(AST) of
                        {ok, AnalyzedAST} ->
                            type_check_with_imports(AnalyzedAST, Opts);
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
    ListA1 = catena_types:tapp(catena_types:tcon('List'), [catena_types:tvar(A1)]),
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
    ListA2 = catena_types:tapp(catena_types:tcon('List'), [catena_types:tvar(A2)]),
    ConsType = catena_types:tfun(
        catena_types:tvar(A2),
        catena_types:tfun(ListA2, ListA2, catena_types:empty_effects()),
        catena_types:empty_effects()),
    ConsScheme = catena_type_scheme:generalize(ConsType, sets:new()),
    Env4 = catena_type_env:extend(Env3, '::', ConsScheme),

    %% Empty list: [] : List a
    {{tvar, A3}, _State3} = catena_types:fresh_var(State1),
    EmptyListType = catena_types:tapp(catena_types:tcon('List'), [catena_types:tvar(A3)]),
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
process_imports([{import, ModuleName, _Loc} | Rest], SearchPaths, Env) ->
    case catena_module_loader:load_module(ModuleName, SearchPaths) of
        {ok, ModuleAST} ->
            case build_module_exports_env(ModuleAST) of
                {ok, ModuleEnv} ->
                    %% Merge module env into accumulated env
                    MergedEnv = catena_type_env:merge(Env, ModuleEnv),
                    process_imports(Rest, SearchPaths, MergedEnv);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
process_imports([_Other | Rest], SearchPaths, Env) ->
    %% Skip invalid import entries
    process_imports(Rest, SearchPaths, Env).

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

add_decl_to_env({trait_decl, _Name, _Params, _Extends, _Members, _Location}, Env) ->
    %% TODO: Register trait methods
    {ok, Env};

add_decl_to_env({instance_decl, _Trait, _Type, _Constraints, _Methods, _Location}, Env) ->
    %% TODO: Validate instance methods
    {ok, Env};

add_decl_to_env(_Other, Env) ->
    %% Skip unknown declarations
    {ok, Env}.

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
convert_constructor_arg({type_con, Name}, _TypeParams) ->
    catena_types:tcon(Name);
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
convert_type_sig({type_var, Name}) ->
    {ok, catena_types:tvar(param_to_var(Name))};
convert_type_sig({type_con, Name}) ->
    {ok, catena_types:tcon(Name)};
convert_type_sig({type_app, Con, Args}) ->
    case convert_type_sig(Con) of
        {ok, {tcon, ConName}} ->
            case convert_type_args(Args) of
                {ok, ArgTypes} ->
                    {ok, catena_types:tapp(catena_types:tcon(ConName), ArgTypes)};
                Error -> Error
            end;
        {ok, _Other} ->
            {error, {invalid_type_application, Con}};
        Error -> Error
    end;
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

convert_effects(undefined) ->
    catena_types:empty_effects();
convert_effects({effect_set, Effects}) ->
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
        [{transform_clause, Patterns, _Guards, Body, _Loc} | _] ->
            %% Convert transform to lambda expression
            Expr = patterns_to_lambda(Patterns, Body),
            %% Infer type
            case catena_infer:infer_expr(Expr, Env) of
                {ok, InferredType} ->
                    %% If there's a declared type, check it matches
                    case DeclaredType of
                        undefined ->
                            {ok, InferredType};
                        _ ->
                            case convert_type_sig(DeclaredType) of
                                {ok, ExpectedType} ->
                                    %% TODO: Check types are compatible
                                    {ok, InferredType};
                                Error ->
                                    Error
                            end
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

%% @doc Convert pattern list to nested lambda expression.
%% [x, y] and body becomes {lam, x, {lam, y, body}}
patterns_to_lambda([], Body) ->
    convert_expr(Body);
patterns_to_lambda([Pattern | Rest], Body) ->
    case Pattern of
        {pat_var, Name, _Loc} ->
            {lam, Name, patterns_to_lambda(Rest, Body)};
        {pat_wildcard, _Loc} ->
            %% Generate fresh name for wildcard
            FreshName = list_to_atom("_wild_" ++ integer_to_list(erlang:unique_integer([positive]))),
            {lam, FreshName, patterns_to_lambda(Rest, Body)};
        _ ->
            %% TODO: Handle complex patterns (constructors, literals)
            FreshName = list_to_atom("_pat_" ++ integer_to_list(erlang:unique_integer([positive]))),
            {lam, FreshName, patterns_to_lambda(Rest, Body)}
    end.

%% @doc Convert parser expression to type inference expression.
convert_expr({var, Name, _Loc}) ->
    {var, Name};
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
convert_expr({tuple, Elements, _Loc}) ->
    {tuple, [convert_expr(E) || E <- Elements]};
convert_expr({list, Elements, _Loc}) ->
    %% Convert list to nested cons
    convert_list(Elements);
convert_expr({match_expr, Scrutinee, Cases, _Loc}) ->
    %% TODO: Convert match to case expression
    %% For now, return a placeholder
    convert_expr(Scrutinee);
convert_expr({binary_op, Op, Left, Right, _Loc}) ->
    %% Convert binary op to function application
    {app, {app, {var, Op}, convert_expr(Left)}, convert_expr(Right)};
convert_expr(Other) ->
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
convert_application(Func, [Arg]) ->
    {app, convert_expr(Func), convert_expr(Arg)};
convert_application(Func, [Arg | Rest]) ->
    convert_application({app, convert_expr(Func), convert_expr(Arg)}, Rest).

convert_list([]) ->
    {var, '[]'};
convert_list([H | T]) ->
    {app, {app, {var, '::'}, convert_expr(H)}, convert_list(T)}.

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
