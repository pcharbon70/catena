%%%-------------------------------------------------------------------
%%% @doc Module Generation for Core Erlang (Task 1.3.4)
%%%
%%% Generates complete Core Erlang modules from Catena AST.
%%% This module handles:
%%% - Module structure generation with name, exports, attributes
%%% - Function compilation to Core Erlang definitions
%%% - Export list generation for public functions
%%% - Core Erlang file output (.core files)
%%% @end
%%%-------------------------------------------------------------------
-module(catena_codegen_module).

-export([
    %% Main generation
    generate_validated_module/1,
    generate_module/1,
    generate_module/2,

    %% Module structure (1.3.4.1)
    build_module_info/1,
    generate_attributes/1,

    %% Function compilation (1.3.4.2)
    compile_function/2,
    compile_functions/2,

    %% Export generation (1.3.4.3)
    generate_exports/1,
    filter_public/1,

    %% File output (1.3.4.4)
    write_core_file/2,
    module_to_core_string/1,
    compile_to_file/2,
    compile_to_string/1,

    %% Utilities
    format_core/1
]).

%%====================================================================
%% Main Generation
%%====================================================================

%% @doc Generate Core Erlang from the validated compiler handoff.
%%
%% Production source compilation enters the backend here.  The unit contract
%% guarantees that normalized source, typed results, symbols, disposition
%% slots, source locations, and compiler options remain available together.
-spec generate_validated_module(catena_compilation_unit:t()) ->
    {ok, cerl:cerl()} | {error, term()}.
generate_validated_module(Unit) ->
    case catena_compilation_unit:is_compilation_unit(Unit) of
        true ->
            case catena_declaration_disposition:prepare_for_codegen(Unit) of
                {ok, BackendAST} ->
                    CompilerOpts = catena_compilation_unit:options(Unit),
                    CodegenOpts = maps:get(codegen_opts, CompilerOpts, #{}),
                    generate_module_with_inventory(
                        BackendAST,
                        CodegenOpts,
                        catena_compilation_unit:callables(Unit),
                        catena_compilation_unit:import_resolution(Unit),
                        catena_compilation_unit:trait_inventory(Unit),
                        catena_compilation_unit:effectful_transforms(Unit),
                        catena_compilation_unit:runtime_dependencies(Unit),
                        catena_compilation_unit:artifact_dependencies(Unit)
                    );
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, {invalid_compilation_unit, unchecked_backend_input}}
    end.

%% @doc Generate a Core Erlang module directly from backend-shaped Catena AST.
%%
%% This low-level compatibility helper is for code-generation unit tests and
%% internal lowering work.  It is not the safe production compilation boundary;
%% source callers must use catena_compile, which enters through
%% generate_validated_module/1.
-spec generate_module(module_ast()) -> {ok, cerl:cerl()} | {error, term()}.
generate_module(ModuleAST) ->
    generate_module(ModuleAST, #{}).

%% @doc Low-level raw-AST generation with backend options.
-spec generate_module(module_ast(), gen_opts()) -> {ok, cerl:cerl()} | {error, term()}.
generate_module(ModuleAST, Opts) ->
    try
        {module, Name, Exports, _Imports, SourceDecls, _Loc} = ModuleAST,
        case catena_call_resolution:build(Name, Exports, SourceDecls) of
            {ok, Inventory} ->
                EffectfulTransforms =
                    catena_effect_resolution:effectful_transforms(
                        SourceDecls
                    ),
                do_generate_module(
                    ModuleAST,
                    Opts,
                    Inventory,
                    catena_import_resolution:empty(Name),
                    catena_trait_dictionary:empty(Name),
                    EffectfulTransforms,
                    runtime_dependencies(EffectfulTransforms),
                    runtime_dependencies(EffectfulTransforms)
                );
            {error, ResolutionDiagnostic} ->
                throw(ResolutionDiagnostic)
        end
    catch
        error:{backend_error, _, _} = Diagnostic:_Stack ->
            {error, Diagnostic};
        error:Reason:_Stack ->
            {error, {codegen_error, Reason}};
        throw:{backend_error, _, _} = Diagnostic ->
            {error, Diagnostic};
        throw:Reason ->
            {error, {codegen_error, Reason}}
    end.

generate_module_with_inventory(
    ModuleAST,
    Opts,
    Inventory,
    ImportResolution,
    TraitInventory,
    EffectfulTransforms,
    RuntimeDependencies,
    ArtifactDependencies
) ->
    try
        do_generate_module(
            ModuleAST,
            Opts,
            Inventory,
            ImportResolution,
            TraitInventory,
            EffectfulTransforms,
            RuntimeDependencies,
            ArtifactDependencies
        )
    catch
        error:{backend_error, _, _} = Diagnostic:_Stack ->
            {error, Diagnostic};
        error:Reason:_Stack ->
            {error, {codegen_error, Reason}};
        throw:{backend_error, _, _} = Diagnostic ->
            {error, Diagnostic};
        throw:Reason ->
            {error, {codegen_error, Reason}}
    end.

do_generate_module(
    ModuleAST,
    Opts,
    Inventory,
    ImportResolution,
    TraitInventory,
    EffectfulTransforms,
    RuntimeDependencies,
    ArtifactDependencies
) ->
        {module, Name, Exports, _Imports, Decls, _Loc} =
            catena_codegen_lower:lower_module(ModuleAST),
        State = catena_codegen_utils:new_state(#{
            module_name => Name,
            callables => Inventory,
            import_resolution => ImportResolution,
            trait_inventory => TraitInventory,
            effectful_transforms => EffectfulTransforms
        }),

        ok = validate_runtime_dependencies(
            RuntimeDependencies,
            Opts,
            Name
        ),

        %% The raw-AST compatibility path must still classify every erasure
        %% input before static declarations can disappear.
        ok = validate_erasure_inputs(Decls, Name),

        %% Erase types from declarations
        ErasedDecls = erase_types(Decls),

        %% Every declaration must have an explicit emission disposition.
        ok = validate_declaration_dispositions(ErasedDecls, Name),

        %% Filter out erased declarations
        ActiveDecls = [D || D <- ErasedDecls, D =/= erased],

        %% Compile functions
        {CoreFunctions, State1} = compile_functions(ActiveDecls, State),

        {DictionaryFunctions, _State2, DictionaryExports} =
            catena_trait_dictionary:compile_dictionaries(
                TraitInventory,
                State1
            ),

        %% Generate exports
        CoreExports =
            generate_module_exports(ActiveDecls, Exports) ++
                DictionaryExports,

        %% Build module attributes
        Attrs = generate_attributes(
            Opts#{
                runtime_dependencies => RuntimeDependencies,
                artifact_dependencies => ArtifactDependencies
            }
        ),

        %% Create Core Erlang module
        CoreModule = cerl:c_module(
            cerl:c_atom(Name),
            CoreExports,
            Attrs,
            CoreFunctions ++ DictionaryFunctions
        ),

        {ok, CoreModule}.

%% Erase types from declarations
erase_types(Decls) ->
    [catena_codegen_erase:erase_decl(D) || D <- Decls].

validate_erasure_inputs(Decls, ModuleName) ->
    lists:foreach(
        fun
            (erased) ->
                ok;
            ({transform, _, _, _, _}) ->
                ok;
            ({transform_typed, _, _, _, _, _}) ->
                ok;
            ({type_decl, _, _, _, _, _}) ->
                ok;
            ({type_decl, _, _, _, _}) ->
                ok;
            ({effect_decl, _, _, _}) ->
                ok;
            (Declaration) ->
                Context =
                    catena_backend_error:context(
                        type_erasure,
                        declaration,
                        Declaration,
                        #{module => ModuleName}
                    ),
                throw(
                    catena_backend_error:invalid_declaration_disposition(
                        Declaration,
                        Context
                    )
                )
        end,
        Decls
    ).

validate_declaration_dispositions(Decls, ModuleName) ->
    lists:foreach(
        fun
            (erased) ->
                ok;
            ({transform, _, _, _, _}) ->
                ok;
            ({transform_typed, _, _, _, _, _}) ->
                ok;
            (Declaration) ->
                Context =
                    catena_backend_error:context(
                        declaration_disposition,
                        declaration,
                        Declaration,
                        #{module => ModuleName}
                    ),
                throw(
                    catena_backend_error:invalid_declaration_disposition(
                        Declaration,
                        Context
                    )
                )
        end,
        Decls
    ).

%%====================================================================
%% Module Structure Generation (1.3.4.1)
%%====================================================================

%% @doc Build module info from AST
-spec build_module_info(module_ast()) -> module_info().
build_module_info({module, Name, Exports, _Imports, Decls, Loc}) ->
    {module, Name, Exports, _, LoweredDecls, Loc} =
        catena_codegen_lower:lower_module(
            {module, Name, Exports, _Imports, Decls, Loc}
        ),
    #{
        name => Name,
        exports => Exports,
        declarations => LoweredDecls,
        location => Loc,
        function_count => count_functions(LoweredDecls),
        public_count => length(generate_module_exports(LoweredDecls, Exports))
    }.

%% Count function declarations
count_functions(Decls) ->
    length([D || D <- Decls, is_function_decl(D)]).

is_function_decl({transform, _, _, _, _}) -> true;
is_function_decl({transform_typed, _, _, _, _, _}) -> true;
is_function_decl(_) -> false.

%% @doc Generate module attributes
-spec generate_attributes(gen_opts()) -> [{cerl:cerl(), cerl:cerl()}].
generate_attributes(Opts) ->
    BaseAttrs = [
        {cerl:c_atom(file), cerl:c_string(maps:get(file, Opts, "nofile"))}
    ],

    %% Add optional attributes
    VersionAttr = case maps:get(version, Opts, undefined) of
        undefined -> [];
        Version -> [{cerl:c_atom(vsn), cerl:c_string(Version)}]
    end,

    AuthorAttr = case maps:get(author, Opts, undefined) of
        undefined -> [];
        Author -> [{cerl:c_atom(author), cerl:c_string(Author)}]
    end,

    RuntimeDependencyAttr =
        case maps:get(runtime_dependencies, Opts, []) of
            [] ->
                [];
            Dependencies ->
                DependencyTerms = [
                    {
                        maps:get(module, Dependency),
                        maps:get(version, Dependency)
                    }
                    || Dependency <- Dependencies
                ],
                [
                    {
                        cerl:c_atom(catena_runtime_dependencies),
                        cerl:abstract(DependencyTerms)
                    }
                ]
        end,

    ArtifactDependencyAttr =
        case maps:get(artifact_dependencies, Opts, []) of
            [] ->
                [];
            ArtifactDependencies ->
                [
                    {
                        cerl:c_atom(catena_artifact_dependencies),
                        cerl:abstract(ArtifactDependencies)
                    }
                ]
        end,

    BaseAttrs ++ VersionAttr ++ AuthorAttr ++ RuntimeDependencyAttr ++
        ArtifactDependencyAttr.

%%====================================================================
%% Function Compilation (1.3.4.2)
%%====================================================================

%% @doc Compile multiple function declarations
-spec compile_functions([decl()], catena_codegen_utils:codegen_state()) ->
    {[{cerl:cerl(), cerl:cerl()}], catena_codegen_utils:codegen_state()}.
compile_functions(Decls, State) ->
    {Definitions, FinalState} = lists:mapfoldl(
        fun(Decl, CurrentState) ->
            compile_function_definitions(Decl, CurrentState)
        end,
        State,
        [D || D <- Decls, is_function_decl(D)]
    ),
    {lists:append(Definitions), FinalState}.

compile_function_definitions(
    {transform, Name, _Params, _Body, _Location} = Declaration,
    State
) ->
    case catena_codegen_utils:is_effectful_transform(Name, State) of
        true ->
            compile_effectful_function(Declaration, State);
        false ->
            {Definition, State1} = compile_function(Declaration, State),
            {[Definition], State1}
    end;
compile_function_definitions(
    {transform_typed, Name, _Type, Params, Body, Location},
    State
) ->
    compile_function_definitions(
        {transform, Name, Params, Body, Location},
        State
    ).

%% @doc Compile a single function declaration to Core Erlang
-spec compile_function(decl(), catena_codegen_utils:codegen_state()) ->
    {{cerl:cerl(), cerl:cerl()}, catena_codegen_utils:codegen_state()}.
compile_function({transform, Name, Params, Body, _Loc}, State) ->
    Arity = length(Params),

    %% Create function name
    FName = cerl:c_fname(Name, Arity),

    %% Compile parameters to variables
    {ParamVars, State1} = compile_params(Params, State),

    ParamNames = [cerl:var_name(ParamVar) || ParamVar <- ParamVars],
    {CoreBody, State2} = case requires_effect_runtime(Body) of
        true ->
            {ContextVar, ContextState} =
                catena_codegen_utils:fresh_var(State1),
            {TranslatedBody, BodyState} =
                compile_function_body(
                    Name,
                    ParamNames,
                    Body,
                    ContextVar,
                    ContextState
                ),
            {
                catena_effect_codegen:with_runtime_call(
                    ContextVar,
                    TranslatedBody
                ),
                BodyState
            };
        false ->
            catena_codegen_utils:with_function_scope(
                Name,
                ParamNames,
                fun(ScopedState) ->
                    catena_codegen_expr:translate_expr(
                        Body,
                        ScopedState
                    )
                end,
                State1
            )
    end,

    %% Create function definition
    FunDef = cerl:c_fun(ParamVars, CoreBody),

    {{FName, FunDef}, State2};

compile_function({transform_typed, Name, _TypeSig, Params, Body, Loc}, State) ->
    %% Treat same as untyped (types already erased)
    compile_function({transform, Name, Params, Body, Loc}, State).

compile_effectful_function(
    {transform, Name, Params, Body, _Location},
    State
) ->
    Arity = length(Params),
    {ParamVars, State1} = compile_params(Params, State),
    ParamNames = [cerl:var_name(ParamVar) || ParamVar <- ParamVars],
    {ContextVar, State2} = catena_codegen_utils:fresh_var(State1),
    {CoreBody, State3} = compile_function_body(
        Name,
        ParamNames,
        Body,
        ContextVar,
        State2
    ),
    EntryName = catena_codegen_utils:effect_entry_name(Name),
    EntryFName = cerl:c_fname(EntryName, Arity + 1),
    EntryDef = cerl:c_fun([ContextVar | ParamVars], CoreBody),
    WrapperFName = cerl:c_fname(Name, Arity),
    EntryCall = cerl:c_apply(
        EntryFName,
        [ContextVar | ParamVars]
    ),
    WrapperDef = cerl:c_fun(
        ParamVars,
        catena_effect_codegen:with_runtime_call(
            ContextVar,
            EntryCall
        )
    ),
    {[{WrapperFName, WrapperDef}, {EntryFName, EntryDef}], State3}.

compile_function_body(
    Name,
    ParamNames,
    Body,
    ContextVar,
    State
) ->
    catena_codegen_utils:with_function_scope(
        Name,
        ParamNames,
        fun(FunctionState) ->
            catena_codegen_utils:with_runtime_context(
                ContextVar,
                fun(ScopedState) ->
                    catena_codegen_expr:translate_expr(
                        Body,
                        ScopedState
                    )
                end,
                FunctionState
            )
        end,
        State
    ).

%% Compile parameters to Core Erlang variables
compile_params(Params, State) ->
    lists:mapfoldl(
        fun(Param, St) ->
            compile_param(Param, St)
        end,
        State,
        Params
    ).

compile_param({pat_var, Name, _Loc}, State) ->
    {cerl:c_var(Name), State};
compile_param({pat_wildcard, _Loc}, State) ->
    %% Generate fresh variable for wildcard
    catena_codegen_utils:fresh_var(State);
compile_param({pat_typed_var, Name, _Type, _Loc}, State) ->
    {cerl:c_var(Name), State};
compile_param(Other, _State) ->
    Context =
        catena_backend_error:context(
            function_compilation,
            parameter_pattern,
            Other
        ),
    throw(
        catena_backend_error:unsupported_backend_construct(
            parameter_pattern,
            Context
        )
    ).

requires_effect_runtime({perform_expr, _, _, _, _}) ->
    true;
requires_effect_runtime({handle_expr, _, _, _}) ->
    true;
requires_effect_runtime({try_with_expr, _, _, _}) ->
    true;
requires_effect_runtime(Term) when is_tuple(Term) ->
    requires_effect_runtime(tuple_to_list(Term));
requires_effect_runtime(Terms) when is_list(Terms) ->
    lists:any(fun requires_effect_runtime/1, Terms);
requires_effect_runtime(_) ->
    false.

%%====================================================================
%% Export List Generation (1.3.4.3)
%%====================================================================

%% @doc Generate export list for module
-spec generate_exports([decl()]) -> [cerl:cerl()].
generate_exports(Decls) ->
    %% Get all public function declarations
    PublicDecls = filter_public(Decls),

    %% Convert to Core Erlang fname nodes
    [cerl:c_fname(Name, get_arity(Decl))
     || {Name, Decl} <- PublicDecls].

generate_module_exports(Decls, []) ->
    generate_exports(Decls);
generate_module_exports(Decls, Exports) ->
    ExportedTransforms = [
        Name
        || {export_transform, Name} <- Exports
    ],
    ExplicitExports = [
        cerl:c_fname(Name, Arity)
        || {Name, Arity} <- Exports,
           is_atom(Name),
           is_integer(Arity)
    ],
    ParserExports = [
        cerl:c_fname(Name, get_arity(Decl))
        || Decl <- Decls,
           is_function_decl(Decl),
           Name <- [get_name(Decl)],
           lists:member(Name, ExportedTransforms)
    ],
    ExplicitExports ++ ParserExports.

%% @doc Filter declarations to only public functions
-spec filter_public([decl()]) -> [{atom(), decl()}].
filter_public(Decls) ->
    %% For PoC, all transforms are public
    %% Full implementation would check visibility annotations
    [{get_name(D), D} || D <- Decls, is_function_decl(D)].

%% Get function name from declaration
get_name({transform, Name, _, _, _}) -> Name;
get_name({transform_typed, Name, _, _, _, _}) -> Name.

%% Get function arity from declaration
get_arity({transform, _, Params, _, _}) -> length(Params);
get_arity({transform_typed, _, _, Params, _, _}) -> length(Params).

%%====================================================================
%% Core Erlang File Output (1.3.4.4)
%%====================================================================

%% @doc Write Core Erlang module to file
-spec write_core_file(cerl:cerl(), string()) -> ok | {error, term()}.
write_core_file(CoreModule, FilePath) ->
    %% Validate path for security
    case catena_error:validate_source_path(FilePath) of
        {ok, ValidPath} ->
            %% Format module to Core Erlang string
            CoreString = module_to_core_string(CoreModule),

            %% Write to file
            case file:write_file(ValidPath, CoreString) of
                ok -> ok;
                {error, Reason} -> {error, {write_failed, Reason}}
            end;
        {error, path_traversal_attack} ->
            {error, {path_traversal_attack, FilePath}}
    end.

%% @doc Convert Core Erlang module to string representation
-spec module_to_core_string(cerl:cerl()) -> string().
module_to_core_string(CoreModule) ->
    %% Use Core Erlang pretty printer
    format_core(CoreModule).

%% @doc Format Core Erlang AST to string
-spec format_core(cerl:cerl()) -> string().
format_core(Core) ->
    %% Use cerl_prettypr for formatting
    Doc = cerl_prettypr:format(Core),
    lists:flatten(Doc).

%%====================================================================
%% High-Level API
%%====================================================================

%% @doc Compile Catena module AST to Core Erlang and write to file
-spec compile_to_file(module_ast(), string()) -> ok | {error, term()}.
compile_to_file(ModuleAST, OutputPath) ->
    case generate_module(ModuleAST) of
        {ok, CoreModule} ->
            write_core_file(CoreModule, OutputPath);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Compile Catena module AST to Core Erlang string
-spec compile_to_string(module_ast()) -> {ok, string()} | {error, term()}.
compile_to_string(ModuleAST) ->
    case generate_module(ModuleAST) of
        {ok, CoreModule} ->
            {ok, module_to_core_string(CoreModule)};
        {error, Reason} ->
            {error, Reason}
    end.

runtime_dependencies(EffectfulTransforms)
  when map_size(EffectfulTransforms) =:= 0 ->
    [];
runtime_dependencies(_EffectfulTransforms) ->
    [
        #{module => catena_effect_runtime, version => 1},
        #{module => catena_effect_system, version => 1}
    ].

validate_runtime_dependencies(Dependencies, Opts, ModuleName) ->
    Available = maps:get(
        available_runtime_modules,
        Opts,
        auto
    ),
    lists:foreach(
        fun(Dependency) ->
            RuntimeModule = maps:get(module, Dependency),
            Version = maps:get(version, Dependency),
            case dependency_available(
                RuntimeModule,
                Version,
                Available
            ) of
                true ->
                    ok;
                false ->
                    Context = catena_backend_error:context(
                        artifact_preparation,
                        runtime_dependency,
                        maps:get(location, Opts, undefined),
                        #{
                            module => ModuleName,
                            available_runtime_modules => Available
                        }
                    ),
                    throw(
                        catena_backend_error:
                            runtime_dependency_unavailable(
                                RuntimeModule,
                                Version,
                                Context
                            )
                    )
            end
        end,
        Dependencies
    ),
    ok.

dependency_available(_Module, _Version, all) ->
    true;
dependency_available(Module, _Version, auto) ->
    code:which(Module) =/= non_existing;
dependency_available(Module, _Version, Available)
  when is_list(Available) ->
    lists:member(Module, Available);
dependency_available(Module, Version, Available)
  when is_map(Available) ->
    case maps:get(Module, Available, unavailable) of
        AvailableVersion when is_integer(AvailableVersion) ->
            AvailableVersion >= Version;
        _ ->
            false
    end;
dependency_available(_Module, _Version, _Available) ->
    false.

%%====================================================================
%% Type Definitions
%%====================================================================

-type module_ast() :: {module, atom(), [export()], [import()], [decl()], term()}.
-type export() :: {atom(), integer()}.
-type import() :: term().
-type decl() :: term().
-type gen_opts() :: #{
    file => string(),
    version => string(),
    author => string(),
    optimize => boolean(),
    available_runtime_modules =>
        auto | all | [atom()] | #{atom() => pos_integer()}
}.
-type module_info() :: #{
    name => atom(),
    exports => [export()],
    declarations => [decl()],
    location => term(),
    function_count => non_neg_integer(),
    public_count => non_neg_integer()
}.
