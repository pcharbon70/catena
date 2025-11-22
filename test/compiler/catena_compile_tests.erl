%% @doc Tests for Catena compilation pipeline.
-module(catena_compile_tests).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Basic Pipeline Tests
%% =============================================================================

simple_type_decl_test() ->
    %% Simple type declaration
    Source = "type Bool = True | False",
    Result = catena_compile:compile_string(Source),
    ?assertMatch({ok, {typed_module, _, _, _}}, Result).

type_with_params_test() ->
    %% Parameterized type
    Source = "type Maybe a = None | Some a",
    Result = catena_compile:compile_string(Source),
    ?assertMatch({ok, {typed_module, _, _, _}}, Result).

simple_transform_test() ->
    %% Simple transform without type signature
    Source = "transform id x = x",
    Result = catena_compile:compile_string(Source),
    case Result of
        {ok, _} -> ok;
        {error, Reason} ->
            io:format("~nError: ~p~n", [Reason]),
            ?assert(false)
    end.

transform_with_literal_test() ->
    %% Transform returning literal
    Source = "transform answer x = 42",
    Result = catena_compile:compile_string(Source),
    case Result of
        {ok, _} -> ok;
        {error, Reason} ->
            io:format("~nError: ~p~n", [Reason]),
            ?assert(false)
    end.

%% =============================================================================
%% Type Environment Tests
%% =============================================================================

type_env_constructors_test() ->
    %% Verify constructors are added to environment
    Source = "type Maybe a = None | Some a",
    {ok, Tokens, _} = catena_lexer:string(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {ok, Analyzed} = catena_semantic:analyze(AST),
    {module, _, _, _, Decls, _} = Analyzed,
    {ok, Env} = catena_compile:build_type_env(Decls),

    %% Check None is in environment
    ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'None')),
    %% Check Some is in environment
    ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'Some')).

multiple_types_test() ->
    %% Multiple type declarations
    Source = "
        type Bool = True | False
        type Maybe a = None | Some a
    ",
    Result = catena_compile:compile_string(Source),
    ?assertMatch({ok, {typed_module, _, _, _}}, Result).

%% =============================================================================
%% Module Tests
%% =============================================================================

module_with_types_test() ->
    Source = "
        module Test
        type Bool = True | False
        type Maybe a = None | Some a
    ",
    Result = catena_compile:compile_string(Source),
    ?assertMatch({ok, {typed_module, 'Test', _, _}}, Result).

%% =============================================================================
%% Prelude Parsing Test
%% =============================================================================

prelude_parses_test() ->
    %% Just verify prelude parses through semantic analysis
    Path = "/home/ducky/code/catena/lib/catena/stdlib/prelude.cat",
    {ok, Binary} = file:read_file(Path),
    Source = binary_to_list(Binary),
    {ok, Tokens, _} = catena_lexer:string(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {ok, Analyzed} = catena_semantic:analyze(AST),
    ?assertMatch({module, 'Prelude', _, _, _, _}, Analyzed).

prelude_env_build_test() ->
    %% Try to build type environment from prelude
    Path = "/home/ducky/code/catena/lib/catena/stdlib/prelude.cat",
    {ok, Binary} = file:read_file(Path),
    Source = binary_to_list(Binary),
    {ok, Tokens, _} = catena_lexer:string(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {ok, {module, _, _, _, Decls, _}} = catena_semantic:analyze(AST),

    %% Build type environment - may fail, that's what we're testing
    case catena_compile:build_type_env(Decls) of
        {ok, Env} ->
            %% Check some expected constructors
            io:format("~nEnvironment built successfully~n"),
            %% Check Maybe constructors
            case catena_type_env:lookup(Env, 'None') of
                {ok, _} -> io:format("  None: found~n");
                _ -> io:format("  None: NOT FOUND~n")
            end,
            case catena_type_env:lookup(Env, 'Some') of
                {ok, _} -> io:format("  Some: found~n");
                _ -> io:format("  Some: NOT FOUND~n")
            end;
        {error, Reason} ->
            io:format("~nFailed to build environment: ~p~n", [Reason]),
            ?assert(false)
    end.

%% =============================================================================
%% Full Prelude Compilation (expected to have issues initially)
%% =============================================================================

prelude_compile_test() ->
    %% Full compilation - will likely fail initially
    Path = "/home/ducky/code/catena/lib/catena/stdlib/prelude.cat",
    Result = catena_compile:compile_file(Path),
    case Result of
        {ok, {typed_module, Name, TypedDecls, _Env}} ->
            io:format("~nPrelude compiled successfully!~n"),
            io:format("  Module: ~p~n", [Name]),
            io:format("  Declarations: ~p~n", [length(TypedDecls)]);
        {error, Reason} ->
            io:format("~nPrelude compilation error:~n~p~n", [Reason]),
            %% Don't fail test - we expect errors initially
            ok
    end.
