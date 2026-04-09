%%%-------------------------------------------------------------------
%%% @doc Integration Tests for Catena Module System (Phase 4)
%%%
%%% These tests verify the complete module system functionality including:
%%% - Module parsing with imports and exports
%%% - Qualified and selective imports
%%% - Name resolution with shadowing
%%% - Circular dependency detection
%%% - Cross-module trait resolution
%%% - Multi-module compilation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_module_system_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% End-to-End Module Parsing Tests
%%%=============================================================================

parse_simple_module_test() ->
    ModuleCode = "
module SimpleTest

export transform foo

transform foo = 42
",
    {ok, Tokens, _} = catena_lexer:string(ModuleCode),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _Name, _Exports, _Imports, Decls, _Loc} = AST,
    ?assert(length(Decls) > 0).

parse_module_with_exports_test() ->
    ModuleCode = "
module ExportTest

export transform foo
export transform bar

transform foo = 42
transform bar = 43
transform baz = 44
",
    {ok, Tokens, _} = catena_lexer:string(ModuleCode),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _Name, Exports, _Imports, _Decls, _Loc} = AST,
    ?assertEqual(2, length(Exports)),
    ?assert(lists:member({export_transform, foo}, Exports)),
    ?assert(lists:member({export_transform, bar}, Exports)),
    ?assertNot(lists:member({export_transform, baz}, Exports)).

parse_module_with_imports_test() ->
    ModuleCode = "
module ImportTest

import Prelude
import qualified Data.List as L

transform foo = 42
",
    {ok, Tokens, _} = catena_lexer:string(ModuleCode),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, Imports, _, _} = AST,
    ?assertEqual(2, length(Imports)),
    %% Verify each import type - module names preserve case
    {import, 'Prelude', all, false, undefined, _} = lists:keyfind('Prelude', 2, Imports),
    {import, 'Data.List', all, true, 'L', _} = lists:keyfind('Data.List', 2, Imports).

%%%=============================================================================
%%% Name Resolution Integration Tests
%%%=============================================================================

name_resolution_local_def_test() ->
    ModuleCode = "
module NameTest

export transform foo

transform foo = 42
",
    {ok, Tokens, _} = catena_lexer:string(ModuleCode),
    {ok, AST} = catena_parser:parse(Tokens),
    SymbolTable = catena_name_resolve:build_symbol_table(AST),
    %% Parser preserves case of module names
    ModuleInfo = maps:get('NameTest', SymbolTable),
    Definitions = maps:get(definitions, ModuleInfo),
    ?assert(maps:is_key(foo, Definitions)),
    #{module := 'NameTest'} = maps:get(foo, Definitions).

name_resolution_exports_test() ->
    ModuleCode = "
module ExportNameTest

export transform foo
export transform bar

transform foo = 42
transform bar = 43
",
    {ok, Tokens, _} = catena_lexer:string(ModuleCode),
    {ok, AST} = catena_parser:parse(Tokens),
    SymbolTable = catena_name_resolve:build_symbol_table(AST),
    %% Parser preserves case of module names
    ModuleInfo = maps:get('ExportNameTest', SymbolTable),
    Exports = maps:get(exports, ModuleInfo),
    ?assert(lists:member(foo, Exports)),
    ?assert(lists:member(bar, Exports)).

name_resolution_imports_in_symbol_table_test() ->
    ModuleCode = "
module ImportSymbolTest

import Prelude
import Data.List

transform foo = 42
",
    {ok, Tokens, _} = catena_lexer:string(ModuleCode),
    {ok, AST} = catena_parser:parse(Tokens),
    SymbolTable = catena_name_resolve:build_symbol_table(AST),
    %% Parser preserves case of module names
    ModuleInfo = maps:get('ImportSymbolTest', SymbolTable),
    Imports = maps:get(imports, ModuleInfo),
    ?assertEqual(2, length(Imports)).

%%%=============================================================================
%%% Circular Dependency Detection Tests
%%%=============================================================================

detect_no_cycle_linear_test() ->
    %% Create a simple linear dependency
    Modules = #{
        module_a => #{
            imports => [{import, module_b, all, false, undefined, {1, 0}}],
            definitions => #{}
        },
        module_b => #{
            imports => [{import, module_c, all, false, undefined, {1, 0}}],
            definitions => #{}
        },
        module_c => #{
            imports => [],
            definitions => #{}
        }
    },
    Graph = build_simple_graph(Modules),
    Cycles = catena_circular_deps:detect_cycles(Graph),
    ?assertEqual([], Cycles).

detect_cycle_two_nodes_test() ->
    %% Create a simple cycle between two modules
    Modules = #{
        module_a => #{
            imports => [{import, module_b, all, false, undefined, {1, 0}}],
            definitions => #{}
        },
        module_b => #{
            imports => [{import, module_a, all, false, undefined, {1, 0}}],
            definitions => #{}
        }
    },
    Graph = build_simple_graph(Modules),
    Cycles = catena_circular_deps:detect_cycles(Graph),
    ?assert(length(Cycles) > 0),
    [Cycle | _] = Cycles,
    ?assert(lists:member(module_a, Cycle)),
    ?assert(lists:member(module_b, Cycle)).

detect_cycle_self_test() ->
    %% Create a self-cycle
    Modules = #{
        module_a => #{
            imports => [{import, module_a, all, false, undefined, {1, 0}}],
            definitions => #{}
        }
    },
    Graph = build_simple_graph(Modules),
    Cycles = catena_circular_deps:detect_cycles(Graph),
    ?assert(length(Cycles) > 0).

topological_sort_linear_test() ->
    %% Topological sort of linear dependencies
    Modules = #{
        module_a => #{
            imports => [{import, module_b, all, false, undefined, {1, 0}}],
            definitions => #{}
        },
        module_b => #{
            imports => [{import, module_c, all, false, undefined, {1, 0}}],
            definitions => #{}
        },
        module_c => #{
            imports => [],
            definitions => #{}
        }
    },
    Graph = build_simple_graph(Modules),
    {ok, Sorted} = catena_circular_deps:topological_sort(Graph),
    %% Dependencies should come before dependents
    PosA = index_of(module_a, Sorted),
    PosB = index_of(module_b, Sorted),
    PosC = index_of(module_c, Sorted),
    ?assert(PosA > PosB),  %% A depends on B
    ?assert(PosB > PosC).  %% B depends on C

topological_sort_cycle_error_test() ->
    %% Topological sort with cycle should error
    Modules = #{
        module_a => #{
            imports => [{import, module_b, all, false, undefined, {1, 0}}],
            definitions => #{}
        },
        module_b => #{
            imports => [{import, module_a, all, false, undefined, {1, 0}}],
            definitions => #{}
        }
    },
    Graph = build_simple_graph(Modules),
    Result = catena_circular_deps:topological_sort(Graph),
    ?assertMatch({error, {cycle, _}}, Result).

%%%=============================================================================
%%% Cross-Module Trait Resolution Tests
%%%=============================================================================

trait_resolution_simple_test() ->
    %% Simple trait resolution test
    SymbolTables = #{
        module_a => #{
            declarations => [
                {trait_decl, functor, {1, 1}},
                {type_decl, my_list, [], [], {2, 1}},
                {instance_decl, functor, {type_ref, my_list}, [], {3, 1}}
            ],
            definitions => #{}
        }
    },
    Instances = catena_trait_resolution:find_instances(functor, SymbolTables),
    ?assertEqual(1, length(Instances)).

trait_orphan_detection_test() ->
    %% Test orphan instance detection
    SymbolTables = #{
        module_a => #{
            declarations => [
                {trait_decl, my_trait, {1, 1}}
            ],
            definitions => #{}
        },
        module_b => #{
            declarations => [
                {type_decl, my_type, [], [], {1, 1}}
            ],
            definitions => #{}
        },
        module_c => #{
            declarations => [
                {instance_decl, my_trait, {type_ref, my_type}, [], {1, 1}}
            ],
            definitions => #{}
        }
    },
    Orphans = catena_trait_resolution:detect_orphan_instances(module_c, SymbolTables),
    ?assertEqual(1, length(Orphans)),
    [Orphan | _] = Orphans,
    ?assertEqual(my_trait, maps:get(trait, Orphan)),
    ?assertEqual(my_type, maps:get(type, Orphan)).

%%%=============================================================================
%%% Module Compilation Tests
%%%=============================================================================

module_compile_dependency_graph_test() ->
    %% Test building dependency graph from ASTs
    ASTs = #{
        module_a => {module, module_a, {export_decl, {export_list, []}},
                     [{import, module_b, all, false, undefined, {1, 0}}],
                     [], {1, 0}},
        module_b => {module, module_b, {export_decl, {export_list, []}},
                     [], [], {1, 0}}
    },
    Graph = catena_module_compile:build_dependency_graph(ASTs),
    ?assertEqual([module_b], maps:get(module_a, Graph)),
    ?assertEqual([], maps:get(module_b, Graph)).

module_compile_extract_name_test() ->
    AST = {module, test_module, {export_decl, {export_list, []}},
           [], [], {1, 0}},
    Name = catena_module_compile:extract_module_name(AST),
    ?assertEqual(test_module, Name).

module_compile_interface_test() ->
    CompiledInfo = #{
        exports => [{transform, foo}, {type, bar}],
        types => [],
        transforms => [],
        effects => [],
        traits => [],
        instances => []
    },
    {ok, Binary} = catena_module_compile:generate_interface(test_module, CompiledInfo),
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) > 0).

%%%=============================================================================
%%% Error Formatting Tests
%%%=============================================================================

error_format_circular_dependency_test() ->
    Error = {cycle, [module_a, module_b, module_c]},
    Formatted = catena_circular_deps:format_cycle_error(Error),
    FormattedString = lists:flatten(Formatted),
    ?assert(string:str(FormattedString, "Circular dependency") > 0).

error_format_missing_module_test() ->
    Error = {error, {module_not_found, unknown_module}},
    Formatted = catena_name_resolve:format_name_error(Error, []),
    FormattedString = lists:flatten(Formatted),
    ?assert(string:str(FormattedString, "not found") > 0).

error_format_incoherent_instances_test() ->
    Instance1 = #{trait => my_trait, type => my_type, module => module_a,
                  location => {1, 1}, is_orphan => false},
    Instance2 = #{trait => my_trait, type => my_type, module => module_b,
                  location => {1, 1}, is_orphan => false},
    Error = {incoherent_instances, {my_trait, my_type}, [Instance1, Instance2]},
    Formatted = catena_trait_resolution:format_trait_error(Error),
    FormattedString = lists:flatten(Formatted),
    ?assert(string:str(FormattedString, "Incoherent instances") > 0).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% @doc Build a simple dependency graph from module info
build_simple_graph(Modules) ->
    maps:map(fun(Name, Info) ->
        [Module || {import, Module, _, _, _, _} <- maps:get(imports, Info, [])]
    end, Modules).

%% @doc Get index of element in list
index_of(Element, List) ->
    index_of(Element, List, 1).

index_of(Element, [Element | _], Index) ->
    Index;
index_of(Element, [_ | Rest], Index) ->
    index_of(Element, Rest, Index + 1);
index_of(_, _, _) ->
    not_found.
