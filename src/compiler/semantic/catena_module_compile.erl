%%%-------------------------------------------------------------------
%%% @doc Catena Module Compilation and Linking Module
%%%
%%% This module handles compilation of multiple modules with proper
%%% dependency ordering, interface file generation, and module linking.
%%%
%%% == Compilation Process ==
%%%
%%% <ol>
%%%   <li>Parse all source files to extract module information</li>
%%%   <li>Build dependency graph and check for circular dependencies</li>
%%%   <li>Topological sort to determine compilation order</li>
%%%   <li>Compile each module with type checking and code generation</li>
%%%   <li>Generate interface files (.cati) for each module</li>
%%%   <li>Link modules into final BEAM bytecode</li>
%%% </ol>
%%%
%%% == Interface File Format ==
%%%
%%% Interface files (.cati) contain:
%%% <ul>
%%%   <li>Module name and version</li>
%%%   <li>Exported types and their signatures</li>
%%%   <li>Exported transforms and their type signatures</li>
%%%   <li>Declared effects and their operations</li>
%%%   <li>Trait declarations and method signatures</li>
%%%   <li>Required and provided trait instances</li>
%%% </ul>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_module_compile).

%% API
-export([
    compile_modules/2,
    compile_single_module/2,
    generate_interface/2,
    load_interface/1,
    link_modules/1,
    format_compile_error/1,
    extract_module_name/1,
    build_dependency_graph/1,
    extract_exports/1,
    extract_types/1,
    extract_transforms/1,
    extract_effects/1,
    extract_traits/1,
    extract_instances/1
]).

%% Types
-type module_name() :: atom().
-type source_file() :: file:filename().
-type interface_file() :: file:filename().
-type compile_result() :: {ok, module_name(), binary()} | {error, compile_error()}.
-type interface_info() :: #{
    module => module_name(),
    version => binary(),
    exports => [export_info()],
    types => [type_info()],
    transforms => [transform_info()],
    effects => [effect_info()],
    traits => [trait_info()],
    instances => [instance_info()]
}.
-type export_info() :: {type, atom()} | {transform, atom()} | {trait, atom()} | {effect, atom()}.
-type type_info() :: #{
    name => atom(),
    params => [atom()],
    constructors => [constructor_info()]
}.
-type transform_info() :: #{
    name => atom(),
    type => term(),
    effects => [atom()]
}.
-type effect_info() :: #{
    name => atom(),
    operations => [operation_info()]
}.
-type trait_info() :: #{
    name => atom(),
    methods => [method_info()]
}.
-type instance_info() :: #{
    trait => atom(),
    type => atom()
}.
-type constructor_info() :: #{name => atom(), fields => [atom()]}.
-type operation_info() :: #{name => atom(), type => term()}.
-type method_info() :: #{name => atom(), type => term()}.
-type compile_error() :: {circular_dependency, [module_name()]} |
                         {missing_dependency, module_name()} |
                         {type_error, term()} |
                         {parse_error, term()}.
-type compilation_order() :: [module_name()].

%%%=============================================================================
%%% Multi-Module Compilation
%%%=============================================================================

%% @doc Compile multiple modules with proper dependency ordering.
%% @param SourceFiles List of source file paths
%% @param Options Compilation options
%% @return {ok, CompiledModules} | {error, compile_error()}
-spec compile_modules([source_file()], map()) ->
    {ok, [{module_name(), binary()}]} | {error, compile_error()}.
compile_modules(SourceFiles, Options) ->
    %% Step 1: Parse all source files to extract module info
    case parse_all_modules(SourceFiles, Options) of
        {error, Reason} ->
            {error, {parse_error, Reason}};
        {ok, ModuleASTs} ->
            %% Step 2: Build dependency graph
            DepGraph = build_dependency_graph(ModuleASTs),

            %% Step 3: Check for circular dependencies
            case catena_circular_deps:detect_cycles(DepGraph) of
                [] ->
                    %% Step 4: Topological sort for compilation order
                    case catena_circular_deps:topological_sort(DepGraph) of
                        {ok, Order} ->
                            %% Step 5: Compile in dependency order
                            compile_in_order(Order, ModuleASTs, Options, []);
                        {error, {cycle, Cycle}} ->
                            {error, {circular_dependency, Cycle}}
                    end;
                Cycles ->
                    {error, {circular_dependency, hd(Cycles)}}
            end
    end.

%% @doc Parse all source files and extract module ASTs.
-spec parse_all_modules([source_file()], map()) ->
    {ok, #{module_name() => tuple()}} | {error, term()}.
parse_all_modules(SourceFiles, Options) ->
    parse_all_modules(SourceFiles, Options, #{}).

parse_all_modules([], _Options, Acc) ->
    {ok, Acc};
parse_all_modules([File | Rest], Options, Acc) ->
    case parse_single_file(File, Options) of
        {ok, ModuleAST} ->
            ModuleName = extract_module_name(ModuleAST),
            parse_all_modules(Rest, Options, Acc#{ModuleName => ModuleAST});
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Parse a single source file.
-spec parse_single_file(source_file(), map()) ->
    {ok, tuple()} | {error, term()}.
parse_single_file(File, _Options) ->
    case file:read_file(File) of
        {ok, Binary} ->
            Source = binary_to_list(Binary),
            case catena_lexer:string(Source) of
                {ok, Tokens, _} ->
                    case catena_parser:parse(Tokens) of
                        {ok, AST} ->
                            {ok, AST};
                        {error, ParseError} ->
                            {error, ParseError}
                    end;
                {error, LexerError, _} ->
                    {error, LexerError}
            end;
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%% @doc Compile modules in the specified order.
-spec compile_in_order(compilation_order(), #{module_name() => tuple()}, map(), [{module_name(), binary()}]) ->
    {ok, [{module_name(), binary()}]} | {error, compile_error()}.
compile_in_order([], _ModuleASTs, _Options, Acc) ->
    {ok, lists:reverse(Acc)};
compile_in_order([Module | Rest], ModuleASTs, Options, Acc) ->
    ModuleAST = maps:get(Module, ModuleASTs),
    case compile_single_module(ModuleAST, Options) of
        {ok, Module, Binary} ->
            compile_in_order(Rest, ModuleASTs, Options, [{Module, Binary} | Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Compile a single module to BEAM bytecode.
%% @param ModuleAST The module AST
%% @param Options Compilation options
%% @return {ok, ModuleName, Binary} | {error, compile_error()}
-spec compile_single_module(tuple(), map()) ->
    {ok, module_name(), binary()} | {error, compile_error()}.
compile_single_module(ModuleAST, Options) ->
    ModuleName = extract_module_name(ModuleAST),

    %% Type checking
    case catena_infer:infer_module(ModuleAST) of
        {ok, TypedAST, _TypeEnv} ->
            %% Code generation (placeholder - would call catena_codegen)
            case generate_code(ModuleName, TypedAST, Options) of
                {ok, Binary} ->
                    {ok, ModuleName, Binary};
                {error, Reason} ->
                    {error, {codegen_error, Reason}}
            end;
        {error, TypeErrors} ->
            {error, {type_error, TypeErrors}}
    end.

%% @doc Extract module name from AST.
-spec extract_module_name(tuple()) -> module_name().
extract_module_name({module, Name, _Exports, _Imports, _Decls, _Loc}) ->
    Name.

%%%=============================================================================
%%% Dependency Graph
%%%=============================================================================

%% @doc Build dependency graph from module ASTs.
-spec build_dependency_graph(#{module_name() => tuple()}) -> catena_circular_deps:dependency_graph().
build_dependency_graph(ModuleASTs) ->
    maps:map(fun(_ModuleName, AST) ->
        extract_dependencies(AST, ModuleASTs)
    end, ModuleASTs).

%% @doc Extract dependencies from a module AST.
-spec extract_dependencies(tuple(), #{module_name() => tuple()}) -> [module_name()].
extract_dependencies({module, _Name, _Exports, Imports, _Decls, _Loc}, _ModuleASTs) ->
    lists:filtermap(fun(Import) ->
        case Import of
            {import, ModuleName, _Items, _Qualified, _Alias, _Location} ->
                {true, ModuleName};
            _ ->
                false
        end
    end, Imports).

%%%=============================================================================
%%% Interface File Generation
%%%=============================================================================

%% @doc Generate an interface file from a compiled module.
%% @param ModuleName The module name
%% @param CompiledInfo Compiled module information
%% @return {ok, Binary} | {error, term()}
-spec generate_interface(module_name(), map()) -> {ok, binary()} | {error, term()}.
generate_interface(ModuleName, CompiledInfo) ->
    Interface = #{
        module => ModuleName,
        version => <<"1.0.0">>,
        exports => extract_exports(CompiledInfo),
        types => extract_types(CompiledInfo),
        transforms => extract_transforms(CompiledInfo),
        effects => extract_effects(CompiledInfo),
        traits => extract_traits(CompiledInfo),
        instances => extract_instances(CompiledInfo)
    },

    %% Serialize to binary (using term_to_binary for now)
    try
        Binary = term_to_binary(Interface),
        {ok, Binary}
    catch
        Error:_Reason ->
            {error, Error}
    end.

%% @doc Load an interface file.
%% @param File The interface file path
%% @return {ok, interface_info()} | {error, term()}
-spec load_interface(interface_file()) -> {ok, interface_info()} | {error, term()}.
load_interface(File) ->
    case file:read_file(File) of
        {ok, Binary} ->
            try binary_to_term(Binary) of
                Interface = #{module := _} ->
                    {ok, Interface};
                _ ->
                    {error, invalid_interface_format}
            catch
                _:_ ->
                    {error, invalid_interface_binary}
            end;
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%%%=============================================================================
%%% Module Linking
%%%=============================================================================

%% @doc Link multiple compiled modules into a single BEAM file.
%% @param CompiledModules List of {ModuleName, Binary} tuples
%% @return {ok, Binary} | {error, term()}
-spec link_modules([{module_name(), binary()}]) -> {ok, binary()} | {error, term()}.
link_modules(CompiledModules) ->
    %% For now, return a simple concatenated binary
    %% In a real implementation, this would use beam_lib or similar
    try
        Combined = combine_modules(CompiledModules),
        {ok, Combined}
    catch
        Error:_Reason ->
            {error, Error}
    end.

%% @doc Combine multiple module binaries.
-spec combine_modules([{module_name(), binary()}]) -> binary().
combine_modules(Modules) ->
    %% Simple concatenation - real implementation would use BEAM linking
    Binaries = [B || {_M, B} <- Modules],
    iolist_to_binary(Binaries).

%%%=============================================================================
%%% Extractors for Interface Generation
%%%=============================================================================

%% @doc Extract export information.
-spec extract_exports(map()) -> [export_info()].
extract_exports(CompiledInfo) ->
    maps:get(exports, CompiledInfo, []).

%% @doc Extract type information.
-spec extract_types(map()) -> [type_info()].
extract_types(CompiledInfo) ->
    maps:get(types, CompiledInfo, []).

%% @doc Extract transform information.
-spec extract_transforms(map()) -> [transform_info()].
extract_transforms(CompiledInfo) ->
    maps:get(transforms, CompiledInfo, []).

%% @doc Extract effect information.
-spec extract_effects(map()) -> [effect_info()].
extract_effects(CompiledInfo) ->
    maps:get(effects, CompiledInfo, []).

%% @doc Extract trait information.
-spec extract_traits(map()) -> [trait_info()].
extract_traits(CompiledInfo) ->
    maps:get(traits, CompiledInfo, []).

%% @doc Extract instance information.
-spec extract_instances(map()) -> [instance_info()].
extract_instances(CompiledInfo) ->
    maps:get(instances, CompiledInfo, []).

%%%=============================================================================
%%% Code Generation Placeholder
%%%=============================================================================

%% @doc Generate BEAM bytecode from typed AST.
%% @param ModuleName The module name
%% @param TypedAST The typed AST
%% @param Options Compilation options
%% @return {ok, binary()} | {error, term()}
-spec generate_code(module_name(), tuple(), map()) -> {ok, binary()} | {error, term()}.
generate_code(ModuleName, TypedAST, Options) ->
    %% Placeholder for actual code generation
    %% Would call catena_codegen module here
    case maps:get(backend, Options, core_erlang) of
        core_erlang ->
            %% Generate Core Erlang then compile to BEAM
            generate_core_erlang(ModuleName, TypedAST, Options);
        beam ->
            %% Direct BEAM generation
            {error, not_implemented}
    end.

%% @doc Generate Core Erlang from typed AST.
-spec generate_core_erlang(module_name(), tuple(), map()) -> {ok, binary()} | {error, term()}.
generate_core_erlang(_ModuleName, _TypedAST, _Options) ->
    %% Placeholder - would use catena_codegen module
    {ok, <<>>}.

%%%=============================================================================
%%% Error Formatting
%%%=============================================================================

%% @doc Format a compilation error as a human-readable string.
-spec format_compile_error(compile_error()) -> iolist().
format_compile_error({circular_dependency, Cycle}) ->
    ["Circular dependency detected: ", format_cycle(Cycle), "\n"];
format_compile_error({missing_dependency, Module}) ->
    ["Missing dependency: ", atom_to_list(Module), "\n"];
format_compile_error({type_error, Errors}) ->
    ["Type error:\n", format_type_errors(Errors)];
format_compile_error({parse_error, Error}) ->
    ["Parse error: ", format_parse_error(Error), "\n"];
format_compile_error({codegen_error, Reason}) ->
    ["Code generation error: ", io_lib:format("~p", [Reason]), "\n"];
format_compile_error(Unknown) ->
    ["Unknown compilation error: ", io_lib:format("~p", [Unknown]), "\n"].

%% @doc Format a cycle path.
-spec format_cycle([module_name()]) -> iolist().
format_cycle([]) ->
    [];
format_cycle([Module]) ->
    atom_to_list(Module);
format_cycle(Modules) ->
    StringList = [atom_to_list(M) || M <- Modules],
    lists:join(" -> ", StringList).

%% @doc Format type errors.
-spec format_type_errors(term()) -> iolist().
format_type_errors(Errors) when is_list(Errors) ->
    [io_lib:format("  ~p~n", [E]) || E <- Errors];
format_type_errors(Error) ->
    io_lib:format("  ~p~n", [Error]).

%% @doc Format parse errors.
-spec format_parse_error(term()) -> iolist().
format_parse_error({Line, Module, Message}) when is_integer(Line), is_atom(Module), is_list(Message) ->
    io_lib:format("~w:~w: ~s", [Module, Line, Message]);
format_parse_error(Error) ->
    io_lib:format("~p", [Error]).
