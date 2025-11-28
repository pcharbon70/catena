%%%-------------------------------------------------------------------
%%% @doc Module Loader for Catena Compiler
%%%
%%% Handles loading and parsing of Catena module files.
%%% Supports the standard library and project modules.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_module_loader).

-export([
    load_module/2,
    find_module/2,
    get_stdlib_path/0,
    get_default_search_paths/0,
    module_name_to_path/1
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Load and parse a module by name
%% Returns {ok, AST} on success, {error, Reason} on failure
-spec load_module(atom(), [string()]) -> {ok, term()} | {error, term()}.
load_module(ModuleName, SearchPaths) ->
    case find_module(ModuleName, SearchPaths) of
        {ok, FilePath} ->
            case file:read_file(FilePath) of
                {ok, Binary} ->
                    Source = binary_to_list(Binary),
                    parse_source(Source, FilePath);
                {error, Reason} ->
                    {error, {file_read_error, FilePath, Reason}}
            end;
        {error, not_found} ->
            {error, {module_not_found, ModuleName, SearchPaths}}
    end.

%% @doc Find the file path for a module name
%% Searches through the given paths in order
-spec find_module(atom(), [string()]) -> {ok, string()} | {error, not_found}.
find_module(ModuleName, SearchPaths) ->
    RelativePath = module_name_to_path(ModuleName),
    find_in_paths(RelativePath, SearchPaths).

%% @doc Get the path to the standard library
-spec get_stdlib_path() -> string().
get_stdlib_path() ->
    %% First try relative to current directory (most common case)
    case filelib:is_dir("lib/catena/stdlib") of
        true ->
            "lib/catena/stdlib";
        false ->
            %% Try to find it via code path
            case code:lib_dir(catena) of
                {error, _} ->
                    find_stdlib_path();
                LibDir ->
                    StdlibPath = filename:join([LibDir, "lib", "catena", "stdlib"]),
                    case filelib:is_dir(StdlibPath) of
                        true -> StdlibPath;
                        false -> find_stdlib_path()
                    end
            end
    end.

%% @doc Get default search paths (stdlib + current directory)
-spec get_default_search_paths() -> [string()].
get_default_search_paths() ->
    [get_stdlib_path(), "."].

%% @doc Convert module name to relative file path
%% Prelude -> prelude.cat
%% Effect.IO -> effect/io.cat
-spec module_name_to_path(atom()) -> string().
module_name_to_path(ModuleName) ->
    NameStr = atom_to_list(ModuleName),
    %% Split on dots and convert to path
    Parts = string:split(NameStr, ".", all),
    LowerParts = [string:lowercase(P) || P <- Parts],
    filename:join(LowerParts) ++ ".cat".

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Find a file in the given search paths
find_in_paths(_RelativePath, []) ->
    {error, not_found};
find_in_paths(RelativePath, [Path | Rest]) ->
    FullPath = filename:join(Path, RelativePath),
    case filelib:is_regular(FullPath) of
        true -> {ok, FullPath};
        false -> find_in_paths(RelativePath, Rest)
    end.

%% @private Parse source code into AST
parse_source(Source, FilePath) ->
    case catena_lexer:string(Source) of
        {ok, Tokens, _EndLine} ->
            case catena_parser:parse(Tokens) of
                {ok, AST} ->
                    {ok, AST};
                {error, {Line, _Module, Message}} ->
                    {error, {parse_error, FilePath, Line, Message}}
            end;
        {error, {Line, _Module, Message}, _} ->
            {error, {lex_error, FilePath, Line, Message}}
    end.

%% @private Try to find stdlib path by searching common locations
find_stdlib_path() ->
    %% Try various locations relative to cwd
    Candidates = [
        "lib/catena/stdlib",
        "../lib/catena/stdlib",
        "../../lib/catena/stdlib"
    ],
    case lists:dropwhile(fun(P) -> not filelib:is_dir(P) end, Candidates) of
        [Found | _] -> Found;
        [] -> "lib/catena/stdlib"  %% Default fallback
    end.
