%% @doc Common Test Helpers for Catena Integration Tests
%%
%% Provides reusable helper functions to reduce code duplication
%% across test modules.

-module(catena_test_helpers).

-include_lib("eunit/include/eunit.hrl").

-export([
    %% Location helpers
    loc/0,

    %% Parsing helpers
    parse_source/1,
    parse_and_match/2,
    tokenize_source/1,

    %% Stdlib helpers
    stdlib_dir/0,
    load_stdlib_file/1,
    parse_stdlib_file/1,

    %% Prelude helpers
    load_prelude_decls/0,
    load_prelude_instances/0,
    build_prelude_kind_env/0
]).

%% =============================================================================
%% Location Helpers
%% =============================================================================

%% @doc Return a standard test location
%%
%% Used for constructing AST nodes in tests. Returns {location, 1, 1}
%% which represents line 1, column 1. This is a common helper needed
%% across many test modules.
-spec loc() -> {location, integer(), integer()}.
loc() ->
    {location, 1, 1}.

%% =============================================================================
%% Parsing Helpers
%% =============================================================================

%% @doc Tokenize source code
-spec tokenize_source(string()) -> {ok, list()} | {error, term()}.
tokenize_source(Source) ->
    case catena_lexer:string(Source) of
        {ok, Tokens, _} -> {ok, Tokens};
        {error, Reason, _} -> {error, Reason}
    end.

%% @doc Parse source code and return AST
-spec parse_source(string()) -> {ok, term()} | {error, term()}.
parse_source(Source) ->
    case tokenize_source(Source) of
        {ok, Tokens} -> catena_parser:parse(Tokens);
        Error -> Error
    end.

%% @doc Parse source code and assert it matches a pattern
%%
%% Example:
%%   parse_and_match("type X = Y", {module, _, _, _, [{type_decl, _, _, _, _, _}], _})
-spec parse_and_match(string(), term()) -> ok.
parse_and_match(Source, Pattern) ->
    case parse_source(Source) of
        {ok, AST} ->
            ?assertMatch(Pattern, AST),
            ok;
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% =============================================================================
%% Stdlib Helpers
%% =============================================================================

%% @doc Get the stdlib directory path
-spec stdlib_dir() -> string().
stdlib_dir() ->
    case os:getenv("CATENA_ROOT") of
        false ->
            "/home/ducky/code/catena/lib/catena/stdlib";
        Root ->
            filename:join([Root, "lib", "catena", "stdlib"])
    end.

%% @doc Load a stdlib file as a string
-spec load_stdlib_file(string()) -> {ok, string()} | {error, term()}.
load_stdlib_file(RelPath) ->
    Path = filename:join(stdlib_dir(), RelPath),
    case file:read_file(Path) of
        {ok, Content} -> {ok, binary_to_list(Content)};
        Error -> Error
    end.

%% @doc Parse a stdlib file and return AST
-spec parse_stdlib_file(string()) -> {ok, term()} | {error, term()}.
parse_stdlib_file(RelPath) ->
    case load_stdlib_file(RelPath) of
        {ok, Source} -> parse_source(Source);
        Error -> Error
    end.

%% =============================================================================
%% Prelude Helpers
%% =============================================================================

%% @doc Load and analyze prelude declarations
-spec load_prelude_decls() -> list().
load_prelude_decls() ->
    {ok, Source} = load_stdlib_file("prelude.cat"),
    {ok, Tokens} = tokenize_source(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {ok, {module, _, _, _, Decls, _}} = catena_semantic:analyze(AST),
    Decls.

%% @doc Load prelude and build instance database
-spec load_prelude_instances() -> term().
load_prelude_instances() ->
    Decls = load_prelude_decls(),
    catena_instance:build_instance_db(Decls).

%% @doc Load prelude and build kind environment
-spec build_prelude_kind_env() -> term().
build_prelude_kind_env() ->
    Decls = load_prelude_decls(),
    catena_kind:build_kind_env(Decls).
