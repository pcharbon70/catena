%%%-------------------------------------------------------------------
%%% @doc Integration tests for Module Import System
%%%
%%% Tests the basic module import resolution functionality.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_import_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Module Loader Tests
%%====================================================================

module_loader_test_() ->
    {"Module loader tests",
     [
      {"get_stdlib_path returns valid path",
       fun() ->
           Path = catena_module_loader:get_stdlib_path(),
           ?assert(filelib:is_dir(Path))
       end},

      {"module_name_to_path converts simple name",
       fun() ->
           ?assertEqual("prelude.cat",
                        catena_module_loader:module_name_to_path('Prelude'))
       end},

      {"module_name_to_path converts dotted name",
       fun() ->
           ?assertEqual("effect/io.cat",
                        catena_module_loader:module_name_to_path('Effect.IO'))
       end},

      {"find_module locates Prelude",
       fun() ->
           Paths = catena_module_loader:get_default_search_paths(),
           {ok, FilePath} = catena_module_loader:find_module('Prelude', Paths),
           ?assert(filelib:is_regular(FilePath))
       end},

      {"find_module returns error for non-existent module",
       fun() ->
           Paths = catena_module_loader:get_default_search_paths(),
           ?assertEqual({error, not_found},
                        catena_module_loader:find_module('NonExistent', Paths))
       end},

      {"load_module parses Prelude successfully",
       fun() ->
           Paths = catena_module_loader:get_default_search_paths(),
           {ok, {module, Name, _Exports, _Imports, _Decls, _Loc}} =
               catena_module_loader:load_module('Prelude', Paths),
           ?assertEqual('Prelude', Name)
       end},

      {"load_module parses Effect.IO successfully",
       fun() ->
           Paths = catena_module_loader:get_default_search_paths(),
           {ok, {module, Name, _, _, _, _}} =
               catena_module_loader:load_module('Effect.IO', Paths),
           ?assertEqual('Effect.IO', Name)
       end}
     ]}.

%%====================================================================
%% Import Parser Tests
%%====================================================================

import_parser_test_() ->
    {"Import parser tests",
     [
      {"parse simple import",
       fun() ->
           Source = "module Test\nimport Prelude\ntransform id x = x",
           {ok, Tokens, _} = catena_lexer:string(Source),
           {ok, {module, _, _, Imports, _, _}} = catena_parser:parse(Tokens),
           ?assertEqual([{import, 'Prelude', {location, 2, 0}}], Imports)
       end},

      {"parse dotted import",
       fun() ->
           Source = "module Test\nimport Effect.IO\ntransform id x = x",
           {ok, Tokens, _} = catena_lexer:string(Source),
           {ok, {module, _, _, Imports, _, _}} = catena_parser:parse(Tokens),
           ?assertEqual([{import, 'Effect.IO', {location, 2, 0}}], Imports)
       end},

      {"parse multiple imports",
       fun() ->
           Source = "module Test\nimport Prelude\nimport Effect.IO\ntransform id x = x",
           {ok, Tokens, _} = catena_lexer:string(Source),
           {ok, {module, _, _, Imports, _, _}} = catena_parser:parse(Tokens),
           ?assertEqual(2, length(Imports)),
           ?assertMatch([{import, 'Prelude', _}, {import, 'Effect.IO', _}], Imports)
       end},

      {"imports separated from declarations",
       fun() ->
           Source = "module Test\nimport Prelude\ntransform foo x = x\ntransform bar y = y",
           {ok, Tokens, _} = catena_lexer:string(Source),
           {ok, {module, _, _, Imports, Decls, _}} = catena_parser:parse(Tokens),
           ?assertEqual(1, length(Imports)),
           ?assertEqual(2, length(Decls))
       end}
     ]}.

%%====================================================================
%% Import Processing Tests
%%====================================================================

import_processing_test_() ->
    {"Import processing tests",
     [
      {"process_imports creates environment with constructors",
       fun() ->
           Imports = [{import, 'Prelude', {location, 1, 0}}],
           {ok, Env} = catena_compile:process_imports(Imports),
           %% Should have Maybe constructors
           ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'Some')),
           ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'None'))
       end},

      {"process_imports includes Either constructors",
       fun() ->
           Imports = [{import, 'Prelude', {location, 1, 0}}],
           {ok, Env} = catena_compile:process_imports(Imports),
           ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'Left')),
           ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'Right'))
       end},

      {"process_imports includes Result constructors",
       fun() ->
           Imports = [{import, 'Prelude', {location, 1, 0}}],
           {ok, Env} = catena_compile:process_imports(Imports),
           ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'Ok')),
           ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'Err'))
       end},

      {"process_imports includes Ordering constructors",
       fun() ->
           Imports = [{import, 'Prelude', {location, 1, 0}}],
           {ok, Env} = catena_compile:process_imports(Imports),
           ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'LT')),
           ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'EQ')),
           ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'GT'))
       end},

      {"process_imports error for non-existent module",
       fun() ->
           Imports = [{import, 'NonExistent', {location, 1, 0}}],
           ?assertMatch({error, {module_not_found, 'NonExistent', _}},
                        catena_compile:process_imports(Imports))
       end},

      {"process_imports handles empty import list",
       fun() ->
           {ok, Env} = catena_compile:process_imports([]),
           ?assertEqual(catena_type_env:empty(), Env)
       end}
     ]}.

%%====================================================================
%% Build Module Exports Env Tests
%%====================================================================

module_exports_test_() ->
    {"Module exports environment tests",
     [
      {"build_module_exports_env extracts exported types",
       fun() ->
           Paths = catena_module_loader:get_default_search_paths(),
           {ok, AST} = catena_module_loader:load_module('Prelude', Paths),
           {ok, Env} = catena_compile:build_module_exports_env(AST),
           %% Should have constructors from exported types
           ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'Some'))
       end},

      {"non-exported declarations not in environment",
       fun() ->
           %% Create a simple module with unexported decl
           Source = "module Test\nexport type Foo\ntype Foo = A | B\ntype Bar = C | D",
           {ok, Tokens, _} = catena_lexer:string(Source),
           {ok, AST} = catena_parser:parse(Tokens),
           {ok, Env} = catena_compile:build_module_exports_env(AST),
           %% Foo constructors should be present
           ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'A')),
           ?assertMatch({ok, _}, catena_type_env:lookup(Env, 'B')),
           %% Bar constructors should NOT be present (lookup returns 'none' for missing)
           ?assertEqual(none, catena_type_env:lookup(Env, 'C')),
           ?assertEqual(none, catena_type_env:lookup(Env, 'D'))
       end}
     ]}.

%%====================================================================
%% Compilation with Imports Tests
%%====================================================================

compile_with_imports_test_() ->
    {"Compilation with imports tests",
     [
      {"compile_string processes imports by default",
       fun() ->
           Source = "module Test\nimport Prelude\ntransform wrap x = Some x",
           %% This should compile - Some comes from Prelude
           Result = catena_compile:compile_string(Source),
           ?assertMatch({ok, _}, Result)
       end},

      {"compile_string can disable import processing",
       fun() ->
           Source = "module Test\nimport Prelude\ntransform id x = x",
           %% Should still compile with imports disabled
           Result = catena_compile:compile_string(Source, #{process_imports => false}),
           ?assertMatch({ok, _}, Result)
       end}
     ]}.
