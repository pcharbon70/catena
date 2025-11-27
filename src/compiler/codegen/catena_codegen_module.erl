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

%% @doc Generate a Core Erlang module from Catena AST
-spec generate_module(module_ast()) -> {ok, cerl:cerl()} | {error, term()}.
generate_module(ModuleAST) ->
    generate_module(ModuleAST, #{}).

-spec generate_module(module_ast(), gen_opts()) -> {ok, cerl:cerl()} | {error, term()}.
generate_module({module, Name, _Exports, _Imports, Decls, _Loc}, Opts) ->
    try
        State = catena_codegen_utils:new_state(),

        %% Erase types from declarations
        ErasedDecls = erase_types(Decls),

        %% Filter out erased declarations
        ActiveDecls = [D || D <- ErasedDecls, D =/= erased],

        %% Compile functions
        {CoreFunctions, _State1} = compile_functions(ActiveDecls, State),

        %% Generate exports
        Exports = generate_exports(ActiveDecls),

        %% Build module attributes
        Attrs = generate_attributes(Opts),

        %% Create Core Erlang module
        CoreModule = cerl:c_module(
            cerl:c_atom(Name),
            Exports,
            Attrs,
            CoreFunctions
        ),

        {ok, CoreModule}
    catch
        error:Reason:_Stack ->
            {error, {codegen_error, Reason}};
        throw:Reason ->
            {error, {codegen_error, Reason}}
    end.

%% Erase types from declarations
erase_types(Decls) ->
    [catena_codegen_erase:erase_decl(D) || D <- Decls].

%%====================================================================
%% Module Structure Generation (1.3.4.1)
%%====================================================================

%% @doc Build module info from AST
-spec build_module_info(module_ast()) -> module_info().
build_module_info({module, Name, Exports, _Imports, Decls, Loc}) ->
    #{
        name => Name,
        exports => Exports,
        declarations => Decls,
        location => Loc,
        function_count => count_functions(Decls),
        public_count => length(Exports)
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

    BaseAttrs ++ VersionAttr ++ AuthorAttr.

%%====================================================================
%% Function Compilation (1.3.4.2)
%%====================================================================

%% @doc Compile multiple function declarations
-spec compile_functions([decl()], catena_codegen_utils:codegen_state()) ->
    {[{cerl:cerl(), cerl:cerl()}], catena_codegen_utils:codegen_state()}.
compile_functions(Decls, State) ->
    lists:mapfoldl(
        fun(Decl, St) ->
            compile_function(Decl, St)
        end,
        State,
        [D || D <- Decls, is_function_decl(D)]
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

    %% Compile body
    {CoreBody, State2} = catena_codegen_expr:translate_expr(Body, State1),

    %% Create function definition
    FunDef = cerl:c_fun(ParamVars, CoreBody),

    {{FName, FunDef}, State2};

compile_function({transform_typed, Name, _TypeSig, Params, Body, Loc}, State) ->
    %% Treat same as untyped (types already erased)
    compile_function({transform, Name, Params, Body, Loc}, State).

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
compile_param(Other, State) ->
    %% For complex patterns, generate fresh var
    %% (full pattern matching handled elsewhere)
    logger:warning("Complex parameter pattern in code generation: ~p", [Other]),
    catena_codegen_utils:fresh_var(State).

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

%%====================================================================
%% Type Definitions
%%====================================================================

-type module_ast() :: {module, atom(), [export()], [decl()], term()}.
-type export() :: {atom(), integer()}.
-type decl() :: term().
-type gen_opts() :: #{
    file => string(),
    version => string(),
    author => string(),
    optimize => boolean()
}.
-type module_info() :: #{
    name => atom(),
    exports => [export()],
    declarations => [decl()],
    location => term(),
    function_count => non_neg_integer(),
    public_count => non_neg_integer()
}.

