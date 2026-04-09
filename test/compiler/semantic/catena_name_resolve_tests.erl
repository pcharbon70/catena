%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_name_resolve
%%% @end
%%%-------------------------------------------------------------------
-module(catena_name_resolve_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Data
%%%=============================================================================

%% Simple module AST for testing
simple_module_ast() ->
    {module, test_module,
     {export_decl, {export_list, []}},
     [],
     [{transform_decl, foo, {type_ref, int}, [], {1, 0}}],
     {1, 0}}.

%% Module with exports AST
module_with_exports_ast() ->
    {module, test_module,
     {export_decl, {export_list, [{export_transform, foo}, {export_transform, bar}]}},
     [],
     [{transform_decl, foo, {type_ref, int}, [], {1, 0}},
      {transform_decl, bar, {type_ref, int}, [], {2, 0}},
      {transform_decl, baz, {type_ref, int}, [], {3, 0}}],
     {1, 0}}.

%% Module with imports AST
module_with_imports_ast() ->
    {module, test_module,
     {export_decl, {export_list, []}},
     [{import, prelude, all, false, undefined, {1, 0}},
      {import, data_list, all, true, list, {2, 0}},
      {import, effect_io, [read_file, write_file], false, undefined, {3, 0}}],
     [{transform_decl, foo, {type_ref, int}, [], {4, 0}}],
     {1, 0}}.

%%%=============================================================================
%%% build_symbol_table/1 Tests
%%%=============================================================================

build_symbol_table_empty_module_test() ->
    AST = {module, test_module,
           {export_decl, {export_list, []}},
           [],
           [],
           {1, 0}},
    SymbolTable = catena_name_resolve:build_symbol_table(AST),
    ?assertEqual(1, map_size(SymbolTable)),
    ?assert(maps:is_key(test_module, SymbolTable)),
    ModuleInfo = maps:get(test_module, SymbolTable),
    ?assertEqual([], maps:get(exports, ModuleInfo)),
    ?assertEqual([], maps:get(imports, ModuleInfo)),
    ?assertEqual(0, map_size(maps:get(definitions, ModuleInfo))).

build_symbol_table_with_exports_test() ->
    AST = module_with_exports_ast(),
    SymbolTable = catena_name_resolve:build_symbol_table(AST),
    ModuleInfo = maps:get(test_module, SymbolTable),
    Exports = maps:get(exports, ModuleInfo),
    ?assert(lists:member(foo, Exports)),
    ?assert(lists:member(bar, Exports)),
    ?assertNot(lists:member(baz, Exports)).

build_symbol_table_with_imports_test() ->
    AST = module_with_imports_ast(),
    SymbolTable = catena_name_resolve:build_symbol_table(AST),
    ModuleInfo = maps:get(test_module, SymbolTable),
    Imports = maps:get(imports, ModuleInfo),
    ?assertEqual(3, length(Imports)),
    ?assertMatch({import, prelude, all, false, undefined, {1, 0}}, lists:nth(1, Imports)),
    ?assertMatch({import, data_list, all, true, list, {2, 0}}, lists:nth(2, Imports)),
    ?assertMatch({import, effect_io, [read_file, write_file], false, undefined, {3, 0}}, lists:nth(3, Imports)).

build_symbol_table_with_definitions_test() ->
    AST = module_with_exports_ast(),
    SymbolTable = catena_name_resolve:build_symbol_table(AST),
    ModuleInfo = maps:get(test_module, SymbolTable),
    Definitions = maps:get(definitions, ModuleInfo),
    ?assert(maps:is_key(foo, Definitions)),
    ?assert(maps:is_key(bar, Definitions)),
    ?assert(maps:is_key(baz, Definitions)),
    FooDef = maps:get(foo, Definitions),
    ?assertEqual(exported, maps:get(type, FooDef)),
    ?assertEqual(test_module, maps:get(module, FooDef)).

%%%=============================================================================
%%% resolve_module/2 Tests
%%%=============================================================================

resolve_module_existing_test() ->
    SymbolTable = #{
        test_module => #{
            exports => [foo, bar],
            imports => [],
            definitions => #{foo => #{type => exported, module => test_module, location => {1, 0}}}
        }
    },
    Result = catena_name_resolve:resolve_module(test_module, SymbolTable),
    ?assertMatch({ok, _}, Result),
    {ok, Resolved} = Result,
    ?assert(maps:is_key(test_module, Resolved)).

resolve_module_not_found_test() ->
    SymbolTable = #{},
    Result = catena_name_resolve:resolve_module(nonexistent, SymbolTable),
    ?assertMatch({error, {module_not_found, nonexistent}}, Result).

%%%=============================================================================
%%% resolve_name/3 Tests
%%%=============================================================================

resolve_name_local_test() ->
    SymbolTable = #{
        test_module => #{
            exports => [foo],
            imports => [],
            definitions => #{foo => #{type => exported, module => test_module, location => {1, 0}}}
        }
    },
    Result = catena_name_resolve:resolve_name(foo, test_module, SymbolTable),
    ?assertMatch({ok, _}, Result),
    {ok, Def} = Result,
    ?assertEqual(test_module, maps:get(module, Def)).

resolve_name_not_found_test() ->
    SymbolTable = #{
        test_module => #{
            exports => [],
            imports => [],
            definitions => #{}
        }
    },
    Result = catena_name_resolve:resolve_name(nonexistent, test_module, SymbolTable),
    ?assertMatch({error, {name_not_found, _}}, Result).

resolve_name_module_not_loaded_test() ->
    SymbolTable = #{},
    Result = catena_name_resolve:resolve_name(foo, nonexistent, SymbolTable),
    ?assertMatch({error, {module_not_loaded, _}}, Result).

%%%=============================================================================
%%% qualify_name/2 Tests
%%%=============================================================================

qualify_name_atoms_test() ->
    Result = catena_name_resolve:qualify_name(data_list, map),
    ?assertEqual('data_list.map', Result).

qualify_name_mixed_test() ->
    Result = catena_name_resolve:qualify_name(data_list, "map"),
    ?assertEqual('data_list.map', Result).

%%%=============================================================================
%%% is_qualified/1 Tests
%%%=============================================================================

is_qualified_atom_true_test() ->
    ?assert(catena_name_resolve:is_qualified('data_list.map')).

is_qualified_atom_false_test() ->
    ?assertNot(catena_name_resolve:is_qualified(map)).

is_qualified_string_true_test() ->
    ?assert(catena_name_resolve:is_qualified("data_list.map")).

is_qualified_string_false_test() ->
    ?assertNot(catena_name_resolve:is_qualified("map")).

%%%=============================================================================
%%% split_qualified/1 Tests
%%%=============================================================================

split_qualified_atom_test() ->
    Result = catena_name_resolve:split_qualified('data_list.map'),
    ?assertEqual({data_list, map}, Result).

split_qualified_string_test() ->
    Result = catena_name_resolve:split_qualified("data_list.map"),
    ?assertEqual({data_list, map}, Result).

split_qualified_multi_part_test() ->
    Result = catena_name_resolve:split_qualified('data.structure.list.map'),
    ?assertEqual({'data.structure.list', map}, Result).

split_qualified_unqualified_test() ->
    Result = catena_name_resolve:split_qualified('map'),
    ?assertEqual({undefined, map}, Result).

%%%=============================================================================
%%% format_name_error/2 Tests
%%%=============================================================================

format_name_error_not_found_test() ->
    Error = {error, {name_not_found, foo}},
    Result = catena_name_resolve:format_name_error(Error, []),
    ?assert(is_list(Result)).

format_name_error_module_not_loaded_test() ->
    Error = {error, {module_not_loaded, test_module}},
    Result = catena_name_resolve:format_name_error(Error, []),
    ?assert(is_list(Result)).

format_name_error_module_not_found_test() ->
    Error = {error, {module_not_found, test_module}},
    Result = catena_name_resolve:format_name_error(Error, []),
    ?assert(is_list(Result)).
