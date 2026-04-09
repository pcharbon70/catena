%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_module_compile
%%% @end
%%%-------------------------------------------------------------------
-module(catena_module_compile_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Data
%%%=============================================================================

%% Simple module AST
simple_module_ast() ->
    {module, test_module,
     {export_decl, {export_list, []}},
     [],
     [{transform_decl, foo, {type_ref, int}, [], {1, 0}}],
     {1, 0}}.

%% Module with imports AST
module_with_imports_ast() ->
    {module, test_module,
     {export_decl, {export_list, []}},
     [{import, dependency, all, false, undefined, {1, 0}}],
     [{transform_decl, foo, {type_ref, int}, [], {2, 0}}],
     {1, 0}}.

%% Dependency module AST
dependency_module_ast() ->
    {module, dependency,
     {export_decl, {export_list, []}},
     [],
     [{transform_decl, bar, {type_ref, int}, [], {1, 0}}],
     {1, 0}}.

%%%=============================================================================
%%% extract_module_name/1 Tests
%%%=============================================================================

extract_module_name_simple_test() ->
    AST = simple_module_ast(),
    Name = catena_module_compile:extract_module_name(AST),
    ?assertEqual(test_module, Name).

extract_module_name_with_imports_test() ->
    AST = module_with_imports_ast(),
    Name = catena_module_compile:extract_module_name(AST),
    ?assertEqual(test_module, Name).

%%%=============================================================================
%%% build_dependency_graph/1 Tests
%%%=============================================================================

build_dependency_graph_empty_test() ->
    ModuleASTs = #{},
    Graph = catena_module_compile:build_dependency_graph(ModuleASTs),
    ?assertEqual(0, map_size(Graph)).

build_dependency_graph_no_imports_test() ->
    ModuleASTs = #{
        test_module => simple_module_ast()
    },
    Graph = catena_module_compile:build_dependency_graph(ModuleASTs),
    ?assertEqual([test_module], maps:keys(Graph)),
    ?assertEqual([], maps:get(test_module, Graph)).

build_dependency_graph_with_imports_test() ->
    ModuleASTs = #{
        test_module => module_with_imports_ast(),
        dependency => dependency_module_ast()
    },
    Graph = catena_module_compile:build_dependency_graph(ModuleASTs),
    ?assertEqual([dependency], maps:get(test_module, Graph)),
    ?assertEqual([], maps:get(dependency, Graph)).

%%%=============================================================================
%%% Interface Generation Tests
%%%=============================================================================

generate_interface_simple_test() ->
    CompiledInfo = #{
        exports => [{transform, foo}],
        types => [],
        transforms => [#{name => foo, type => {type_ref, int}, effects => []}],
        effects => [],
        traits => [],
        instances => []
    },
    Result = catena_module_compile:generate_interface(test_module, CompiledInfo),
    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    ?assert(is_binary(Binary)).

generate_interface_and_load_test() ->
    CompiledInfo = #{
        exports => [{transform, foo}],
        types => [],
        transforms => [#{name => foo, type => {type_ref, int}, effects => []}],
        effects => [],
        traits => [],
        instances => []
    },
    {ok, Binary} = catena_module_compile:generate_interface(test_module, CompiledInfo),
    %% Write and reload
    TempFile = "/tmp/test_interface.catena_i",
    ok = file:write_file(TempFile, Binary),
    Result = catena_module_compile:load_interface(TempFile),
    ?assertMatch({ok, _}, Result),
    {ok, Interface} = Result,
    ?assertEqual(test_module, maps:get(module, Interface)),
    %% Cleanup
    ok = file:delete(TempFile).

load_interface_nonexistent_test() ->
    Result = catena_module_compile:load_interface("/tmp/nonexistent_file.catena_i"),
    ?assertMatch({error, _}, Result).

%%%=============================================================================
%%% Module Linking Tests
%%%=============================================================================

link_modules_empty_test() ->
    Result = catena_module_compile:link_modules([]),
    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    ?assert(is_binary(Binary)).

link_modules_single_test() ->
    Modules = [{test_module, <<"<<module_code>>">>}],
    Result = catena_module_compile:link_modules(Modules),
    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    ?assert(is_binary(Binary)).

link_modules_multiple_test() ->
    Modules = [
        {module_a, <<"<code_a>">>},
        {module_b, <<"<code_b>">>}
    ],
    Result = catena_module_compile:link_modules(Modules),
    ?assertMatch({ok, _Binary}, Result),
    {ok, Binary} = Result,
    ?assert(is_binary(Binary)).

%%%=============================================================================
%%% Error Formatting Tests
%%%=============================================================================

format_compile_error_circular_test() ->
    Error = {circular_dependency, [module_a, module_b, module_c]},
    Result = catena_module_compile:format_compile_error(Error),
    ?assert(is_list(Result)),
    ResultString = lists:flatten(Result),
    ?assert(string:str(ResultString, "Circular dependency") > 0).

format_compile_error_missing_test() ->
    Error = {missing_dependency, unknown_module},
    Result = catena_module_compile:format_compile_error(Error),
    ?assert(is_list(Result)),
    ResultString = lists:flatten(Result),
    ?assert(string:str(ResultString, "Missing dependency") > 0).

format_compile_error_type_test() ->
    Error = {type_error, ["type mismatch", "unification failed"]},
    Result = catena_module_compile:format_compile_error(Error),
    ?assert(is_list(Result)),
    ResultString = lists:flatten(Result),
    ?assert(string:str(ResultString, "Type error") > 0).

format_compile_error_parse_test() ->
    Error = {parse_error, {1, catena_parser, ["syntax error before: foo"]}},
    Result = catena_module_compile:format_compile_error(Error),
    ?assert(is_list(Result)),
    ResultString = lists:flatten(Result),
    ?assert(string:str(ResultString, "Parse error") > 0).

%%%=============================================================================
%%% Extractors Tests
%%%=============================================================================

extract_exports_test() ->
    CompiledInfo = #{exports => [{transform, foo}, {type, bar}]},
    Result = catena_module_compile:extract_exports(CompiledInfo),
    ?assertEqual(2, length(Result)),
    ?assert(lists:member({transform, foo}, Result)),
    ?assert(lists:member({type, bar}, Result)).

extract_types_empty_test() ->
    CompiledInfo = #{types => []},
    Result = catena_module_compile:extract_types(CompiledInfo),
    ?assertEqual([], Result).

extract_transforms_test() ->
    CompiledInfo = #{transforms => [#{name => foo, type => int, effects => []}]},
    Result = catena_module_compile:extract_transforms(CompiledInfo),
    ?assertEqual(1, length(Result)),
    [#{name := Name}] = Result,
    ?assertEqual(foo, Name).

extract_effects_test() ->
    CompiledInfo = #{
        effects => [
            #{name => io, operations => [#{name => read, type => string}]},
            #{name => state, operations => [#{name => get, type => unit}, #{name => put, type => function}]}
        ]
    },
    Result = catena_module_compile:extract_effects(CompiledInfo),
    ?assertEqual(2, length(Result)).

extract_traits_test() ->
    CompiledInfo = #{
        traits => [
            #{name => functor, methods => [#{name => map, type => function}]}
        ]
    },
    Result = catena_module_compile:extract_traits(CompiledInfo),
    ?assertEqual(1, length(Result)),
    [#{name := Name}] = Result,
    ?assertEqual(functor, Name).

extract_instances_test() ->
    CompiledInfo = #{
        instances => [
            #{trait => functor, type => list},
            #{trait => monad, type => 'maybe'}
        ]
    },
    Result = catena_module_compile:extract_instances(CompiledInfo),
    ?assertEqual(2, length(Result)).
