%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_trait_resolution
%%% @end
%%%-------------------------------------------------------------------
-module(catena_trait_resolution_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Data
%%%=============================================================================

%% Simple symbol table with trait and instance in same module
same_module_symbol_table() ->
    #{
        my_module => #{
            module_name => my_module,
            exports => [],
            imports => [],
            declarations => [
                {trait_decl, my_trait, {1, 1}},
                {type_decl, my_type, [], [], {2, 1}},
                {instance_decl, my_trait, {type_ref, my_type}, [], {3, 1}}
            ]
        }
    }.

%% Symbol table with orphan instance (trait and type in different modules)
orphan_instance_symbol_table() ->
    #{
        module_a => #{
            module_name => module_a,
            exports => [],
            imports => [],
            declarations => [
                {trait_decl, my_trait, {1, 1}}
            ]
        },
        module_b => #{
            module_name => module_b,
            exports => [],
            imports => [],
            declarations => [
                {type_decl, my_type, [], [], {1, 1}}
            ]
        },
        module_c => #{
            module_name => module_c,
            exports => [],
            imports => [],
            declarations => [
                {instance_decl, my_trait, {type_ref, my_type}, [], {1, 1}}
            ]
        }
    }.

%% Symbol table with conflicting instances
conflict_symbol_table() ->
    #{
        module_a => #{
            module_name => module_a,
            exports => [],
            imports => [],
            declarations => [
                {trait_decl, my_trait, {1, 1}},
                {type_decl, my_type, [], [], {2, 1}},
                {instance_decl, my_trait, {type_ref, my_type}, [], {3, 1}}
            ]
        },
        module_b => #{
            module_name => module_b,
            exports => [],
            imports => [],
            declarations => [
                {trait_decl, my_trait, {1, 1}},
                {type_decl, my_type, [], [], {2, 1}},
                {instance_decl, my_trait, {type_ref, my_type}, [], {3, 1}}
            ]
        }
    }.

%%%=============================================================================
%%% get_trait_instances/2 Tests
%%%=============================================================================

get_trait_instances_empty_test() ->
    ModuleInfo = #{
        declarations => []
    },
    Result = catena_trait_resolution:get_trait_instances(my_trait, ModuleInfo),
    ?assertEqual([], Result).

get_trait_instances_found_test() ->
    ModuleInfo = #{
        module_name => my_module,
        declarations => [
            {trait_decl, my_trait, {1, 1}},
            {instance_decl, my_trait, {type_ref, my_type}, [], {2, 1}}
        ]
    },
    Result = catena_trait_resolution:get_trait_instances(my_trait, ModuleInfo),
    ?assertEqual(1, length(Result)),
    [Instance | _] = Result,
    ?assertEqual(my_trait, maps:get(trait, Instance)).

get_trait_instances_not_found_test() ->
    ModuleInfo = #{
        module_name => my_module,
        declarations => [
            {trait_decl, other_trait, {1, 1}}
        ]
    },
    Result = catena_trait_resolution:get_trait_instances(my_trait, ModuleInfo),
    ?assertEqual([], Result).

%%%=============================================================================
%%% detect_orphan_instances/2 Tests
%%%=============================================================================

detect_orphan_instances_none_test() ->
    SymbolTables = same_module_symbol_table(),
    Result = catena_trait_resolution:detect_orphan_instances(my_module, SymbolTables),
    ?assertEqual([], Result).

detect_orphan_instances_found_test() ->
    SymbolTables = orphan_instance_symbol_table(),
    Result = catena_trait_resolution:detect_orphan_instances(module_c, SymbolTables),
    ?assertEqual(1, length(Result)),
    [Orphan | _] = Result,
    ?assertEqual(my_trait, maps:get(trait, Orphan)),
    ?assertEqual(my_type, maps:get(type, Orphan)).

detect_orphan_instances_trait_in_module_test() ->
    SymbolTables = #{
        my_module => #{
            module_name => my_module,
            exports => [],
            imports => [],
            declarations => [
                {trait_decl, my_trait, {1, 1}},
                {instance_decl, my_trait, {type_ref, other_type}, [], {2, 1}}
            ]
        }
    },
    Result = catena_trait_resolution:detect_orphan_instances(my_module, SymbolTables),
    ?assertEqual([], Result).  %% Not orphan because trait is in same module

%%%=============================================================================
%%% check_coherence/1 Tests
%%%=============================================================================

check_coherence_ok_test() ->
    SymbolTables = same_module_symbol_table(),
    Result = catena_trait_resolution:check_coherence(SymbolTables),
    ?assertEqual(ok, Result).

check_coherence_conflict_test() ->
    SymbolTables = conflict_symbol_table(),
    Result = catena_trait_resolution:check_coherence(SymbolTables),
    ?assertMatch({error, {incoherent_instances, _, _}}, Result).

check_coherence_empty_test() ->
    SymbolTables = #{},
    Result = catena_trait_resolution:check_coherence(SymbolTables),
    ?assertEqual(ok, Result).

%%%=============================================================================
%%% resolve_instance/3 Tests
%%%=============================================================================

resolve_instance_found_test() ->
    SymbolTables = same_module_symbol_table(),
    Result = catena_trait_resolution:resolve_instance(my_trait, my_type, SymbolTables),
    ?assertMatch({ok, _}, Result),
    {ok, Instance} = Result,
    ?assertEqual(my_trait, maps:get(trait, Instance)),
    ?assertEqual(my_type, maps:get(type, Instance)).

resolve_instance_not_found_test() ->
    SymbolTables = same_module_symbol_table(),
    Result = catena_trait_resolution:resolve_instance(unknown_trait, unknown_type, SymbolTables),
    ?assertMatch({error, {instance_not_found, _}}, Result).

%%%=============================================================================
%%% import_instances/3 Tests
%%%=============================================================================

import_instances_all_test() ->
    SymbolTables = same_module_symbol_table(),
    Result = catena_trait_resolution:import_instances(my_module, all, SymbolTables),
    ?assertEqual(1, length(Result)).

import_instances_specific_test() ->
    SymbolTables = #{
        my_module => #{
            module_name => my_module,
            exports => [],
            imports => [],
            declarations => [
                {trait_decl, my_trait, {1, 1}},
                {trait_decl, other_trait, {1, 2}},
                {instance_decl, my_trait, {type_ref, my_type}, [], {2, 1}},
                {instance_decl, other_trait, {type_ref, other_type}, [], {2, 2}}
            ]
        }
    },
    Result = catena_trait_resolution:import_instances(my_module, [my_trait], SymbolTables),
    ?assertEqual(1, length(Result)),
    [Instance | _] = Result,
    ?assertEqual(my_trait, maps:get(trait, Instance)).

import_instances_nonexistent_module_test() ->
    SymbolTables = #{},
    Result = catena_trait_resolution:import_instances(nonexistent, all, SymbolTables),
    ?assertEqual([], Result).

%%%=============================================================================
%%% format_trait_error/1 Tests
%%%=============================================================================

format_trait_error_not_found_test() ->
    Error = {instance_not_found, {my_trait, my_type}},
    Result = catena_trait_resolution:format_trait_error(Error),
    ?assert(is_list(Result)),
    ResultString = lists:flatten(Result),
    ?assert(string:str(ResultString, "No instance found") > 0).

format_trait_error_incoherent_test() ->
    Instance1 = #{trait => my_trait, type => my_type, module => module_a, location => {1, 1}, is_orphan => false},
    Instance2 = #{trait => my_trait, type => my_type, module => module_b, location => {1, 1}, is_orphan => false},
    Error = {incoherent_instances, {my_trait, my_type}, [Instance1, Instance2]},
    Result = catena_trait_resolution:format_trait_error(Error),
    ?assert(is_list(Result)),
    ResultString = lists:flatten(Result),
    ?assert(string:str(ResultString, "Incoherent instances") > 0).

format_trait_error_orphan_test() ->
    Instance = #{trait => my_trait, type => my_type, module => module_c, location => {1, 1}, is_orphan => true},
    Error = {orphan_instance, Instance},
    Result = catena_trait_resolution:format_trait_error(Error),
    ?assert(is_list(Result)),
    ResultString = lists:flatten(Result),
    ?assert(string:str(ResultString, "Orphan instance") > 0).
