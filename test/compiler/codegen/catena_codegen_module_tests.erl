%%%-------------------------------------------------------------------
%%% @doc Tests for Module Generation (Task 1.3.4)
%%%
%%% Tests generation of Core Erlang modules from Catena AST.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_codegen_module_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

loc() ->
    {location, 1, 1}.

simple_module() ->
    {module, test_module, [{foo, 1}], [
        {transform, foo, [{pat_var, x, loc()}], {var, x, loc()}, loc()}
    ], loc()}.

multi_function_module() ->
    {module, math_module, [{add, 2}, {double, 1}], [
        {transform, add, [
            {pat_var, x, loc()},
            {pat_var, y, loc()}
        ], {binary_op, '+', {var, x, loc()}, {var, y, loc()}, loc()}, loc()},
        {transform, double, [
            {pat_var, x, loc()}
        ], {binary_op, '*', {var, x, loc()}, {literal, integer, 2, loc()}, loc()}, loc()}
    ], loc()}.

typed_module() ->
    {module, typed_mod, [{inc, 1}], [
        {transform_typed, inc, {fun_type, int, int, pure},
            [{pat_typed_var, x, {tcon, int}, loc()}],
            {binary_op, '+', {var, x, loc()}, {literal, integer, 1, loc()}, loc()},
            loc()}
    ], loc()}.

%%====================================================================
%% Module Structure Tests (1.3.4.1)
%%====================================================================

module_structure_test_() ->
    [
        ?_test(test_build_module_info()),
        ?_test(test_generate_attributes()),
        ?_test(test_generate_simple_module()),
        ?_test(test_module_name())
    ].

test_build_module_info() ->
    Module = simple_module(),
    Info = catena_codegen_module:build_module_info(Module),
    ?assertEqual(test_module, maps:get(name, Info)),
    ?assertEqual(1, maps:get(function_count, Info)),
    ?assertEqual(1, maps:get(public_count, Info)).

test_generate_attributes() ->
    Opts = #{file => "test.cat", version => "1.0.0"},
    Attrs = catena_codegen_module:generate_attributes(Opts),
    ?assert(length(Attrs) >= 1),
    %% Check file attribute present
    FileAttrs = [V || {K, V} <- Attrs, cerl:atom_val(K) =:= file],
    ?assertEqual(1, length(FileAttrs)).

test_generate_simple_module() ->
    Module = simple_module(),
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    ?assertEqual(module, cerl:type(CoreModule)),
    ?assertEqual(test_module, cerl:atom_val(cerl:module_name(CoreModule))).

test_module_name() ->
    Module = {module, my_mod, [], [], loc()},
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    ?assertEqual(my_mod, cerl:atom_val(cerl:module_name(CoreModule))).

%%====================================================================
%% Function Compilation Tests (1.3.4.2)
%%====================================================================

function_compilation_test_() ->
    [
        ?_test(test_compile_simple_function()),
        ?_test(test_compile_binary_function()),
        ?_test(test_compile_multiple_functions()),
        ?_test(test_function_arity())
    ].

test_compile_simple_function() ->
    State = catena_codegen_utils:new_state(),
    Decl = {transform, identity, [{pat_var, x, loc()}], {var, x, loc()}, loc()},
    {{FName, FunDef}, _State1} = catena_codegen_module:compile_function(Decl, State),
    %% Check function name
    ?assertEqual(identity, cerl:fname_id(FName)),
    ?assertEqual(1, cerl:fname_arity(FName)),
    %% Check function definition
    ?assertEqual('fun', cerl:type(FunDef)),
    ?assertEqual(1, cerl:fun_arity(FunDef)).

test_compile_binary_function() ->
    State = catena_codegen_utils:new_state(),
    Decl = {transform, add, [
        {pat_var, x, loc()},
        {pat_var, y, loc()}
    ], {binary_op, '+', {var, x, loc()}, {var, y, loc()}, loc()}, loc()},
    {{FName, FunDef}, _State1} = catena_codegen_module:compile_function(Decl, State),
    ?assertEqual(add, cerl:fname_id(FName)),
    ?assertEqual(2, cerl:fname_arity(FName)),
    ?assertEqual(2, cerl:fun_arity(FunDef)).

test_compile_multiple_functions() ->
    Module = multi_function_module(),
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    Defs = cerl:module_defs(CoreModule),
    ?assertEqual(2, length(Defs)).

test_function_arity() ->
    Module = multi_function_module(),
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    Exports = cerl:module_exports(CoreModule),
    %% Find add/2 and double/1
    AddExport = lists:keyfind(add, 1, [{cerl:fname_id(E), cerl:fname_arity(E)} || E <- Exports]),
    DoubleExport = lists:keyfind(double, 1, [{cerl:fname_id(E), cerl:fname_arity(E)} || E <- Exports]),
    ?assertEqual({add, 2}, AddExport),
    ?assertEqual({double, 1}, DoubleExport).

%%====================================================================
%% Export Generation Tests (1.3.4.3)
%%====================================================================

export_generation_test_() ->
    [
        ?_test(test_generate_exports()),
        ?_test(test_filter_public()),
        ?_test(test_no_exports_empty_module()),
        ?_test(test_exports_in_core_module())
    ].

test_generate_exports() ->
    Decls = [
        {transform, foo, [{pat_var, x, loc()}], {var, x, loc()}, loc()},
        {transform, bar, [], {literal, integer, 42, loc()}, loc()}
    ],
    Exports = catena_codegen_module:generate_exports(Decls),
    ?assertEqual(2, length(Exports)),
    Names = [cerl:fname_id(E) || E <- Exports],
    ?assert(lists:member(foo, Names)),
    ?assert(lists:member(bar, Names)).

test_filter_public() ->
    Decls = [
        {transform, pub_func, [], {literal, integer, 1, loc()}, loc()},
        {type_decl, 'MyType', [], [], loc()}  % Not a function
    ],
    Public = catena_codegen_module:filter_public(Decls),
    ?assertEqual(1, length(Public)),
    [{pub_func, _}] = Public.

test_no_exports_empty_module() ->
    Module = {module, empty_mod, [], [], loc()},
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    Exports = cerl:module_exports(CoreModule),
    ?assertEqual([], Exports).

test_exports_in_core_module() ->
    Module = simple_module(),
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    Exports = cerl:module_exports(CoreModule),
    ?assertEqual(1, length(Exports)),
    [Export] = Exports,
    ?assertEqual(foo, cerl:fname_id(Export)),
    ?assertEqual(1, cerl:fname_arity(Export)).

%%====================================================================
%% Core Erlang Output Tests (1.3.4.4)
%%====================================================================

core_output_test_() ->
    [
        ?_test(test_module_to_core_string()),
        ?_test(test_format_core()),
        ?_test(test_compile_to_string())
    ].

test_module_to_core_string() ->
    Module = simple_module(),
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    CoreString = catena_codegen_module:module_to_core_string(CoreModule),
    ?assert(is_list(CoreString)),
    ?assert(length(CoreString) > 0),
    %% Should contain module name
    ?assert(string:find(CoreString, "test_module") =/= nomatch).

test_format_core() ->
    %% Simple Core Erlang expression
    Expr = cerl:c_int(42),
    Formatted = catena_codegen_module:format_core(Expr),
    ?assertEqual("42", Formatted).

test_compile_to_string() ->
    Module = simple_module(),
    {ok, CoreString} = catena_codegen_module:compile_to_string(Module),
    ?assert(is_list(CoreString)),
    %% Should contain function name
    ?assert(string:find(CoreString, "'foo'") =/= nomatch orelse
            string:find(CoreString, "foo") =/= nomatch).

%%====================================================================
%% Typed Function Tests
%%====================================================================

typed_function_test_() ->
    [
        ?_test(test_compile_typed_function()),
        ?_test(test_typed_module_generation())
    ].

test_compile_typed_function() ->
    State = catena_codegen_utils:new_state(),
    Decl = {transform_typed, inc, {fun_type, int, int, pure},
        [{pat_typed_var, x, {tcon, int}, loc()}],
        {binary_op, '+', {var, x, loc()}, {literal, integer, 1, loc()}, loc()},
        loc()},
    {{FName, FunDef}, _State1} = catena_codegen_module:compile_function(Decl, State),
    ?assertEqual(inc, cerl:fname_id(FName)),
    ?assertEqual(1, cerl:fname_arity(FName)),
    ?assertEqual('fun', cerl:type(FunDef)).

test_typed_module_generation() ->
    Module = typed_module(),
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    ?assertEqual(typed_mod, cerl:atom_val(cerl:module_name(CoreModule))),
    Exports = cerl:module_exports(CoreModule),
    ?assertEqual(1, length(Exports)).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
        ?_test(test_full_module_compilation()),
        ?_test(test_module_with_type_decls()),
        ?_test(test_complex_function_bodies())
    ].

test_full_module_compilation() ->
    Module = multi_function_module(),
    {ok, CoreModule} = catena_codegen_module:generate_module(Module, #{
        file => "math.cat",
        version => "1.0.0"
    }),
    %% Verify module structure
    ?assertEqual(math_module, cerl:atom_val(cerl:module_name(CoreModule))),
    ?assertEqual(2, length(cerl:module_exports(CoreModule))),
    ?assertEqual(2, length(cerl:module_defs(CoreModule))),
    %% Verify attributes
    Attrs = cerl:module_attrs(CoreModule),
    ?assert(length(Attrs) >= 1).

test_module_with_type_decls() ->
    %% Type declarations should be erased
    Module = {module, with_types, [{foo, 0}], [
        {type_decl, 'Maybe', [a], [{'None', []}, {'Some', [a]}], loc()},
        {transform, foo, [], {literal, integer, 42, loc()}, loc()}
    ], loc()},
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    %% Only function should be in defs
    Defs = cerl:module_defs(CoreModule),
    ?assertEqual(1, length(Defs)).

test_complex_function_bodies() ->
    Module = {module, complex_mod, [{factorial, 1}], [
        {transform, factorial, [{pat_var, n, loc()}],
            {if_expr,
                {binary_op, '=:=', {var, n, loc()}, {literal, integer, 0, loc()}, loc()},
                {literal, integer, 1, loc()},
                {binary_op, '*',
                    {var, n, loc()},
                    {app, {var, factorial, loc()}, [
                        {binary_op, '-', {var, n, loc()}, {literal, integer, 1, loc()}, loc()}
                    ], loc()},
                    loc()},
                loc()},
            loc()}
    ], loc()},
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    Defs = cerl:module_defs(CoreModule),
    ?assertEqual(1, length(Defs)),
    [{_FName, FunDef}] = Defs,
    %% Body should be a case (if compiles to case)
    Body = cerl:fun_body(FunDef),
    ?assertEqual('case', cerl:type(Body)).

%%====================================================================
%% File I/O Tests
%%====================================================================

file_io_test_() ->
    [
        ?_test(test_write_core_file())
    ].

test_write_core_file() ->
    Module = simple_module(),
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    %% Use workspace-relative path (write_core_file validates paths)
    TempFile = "_build/test_module_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".core",
    try
        Result = catena_codegen_module:write_core_file(CoreModule, TempFile),
        ?assertEqual(ok, Result),
        %% Verify file was written
        {ok, Content} = file:read_file(TempFile),
        ?assert(byte_size(Content) > 0)
    after
        %% Cleanup
        file:delete(TempFile)
    end.

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    [
        ?_test(test_invalid_file_path())
    ].

test_invalid_file_path() ->
    Module = simple_module(),
    {ok, CoreModule} = catena_codegen_module:generate_module(Module),
    %% Path outside workspace should be rejected by path validation
    Result = catena_codegen_module:write_core_file(CoreModule, "/nonexistent/path/file.core"),
    ?assertMatch({error, {path_traversal_attack, _}}, Result).
