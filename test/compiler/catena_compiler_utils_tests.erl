-module(catena_compiler_utils_tests).
-include_lib("eunit/include/eunit.hrl").

%%============================================================================
%% Test Suite for Compiler Utilities
%%
%% Comprehensive tests for catena_compiler_utils module covering:
%% - Token/AST extraction functions
%% - Configuration management
%% - Error formatting
%% - AST traversal and analysis
%% - Validation utilities
%%============================================================================

%%----------------------------------------------------------------------------
%% Section 1: Token/AST Extraction
%%----------------------------------------------------------------------------

%% Test 1.1: extract_atom from different token formats
extract_atom_from_atom_token_test() ->
    Token = {lower_ident, 1, foo},
    ?assertEqual(foo, catena_compiler_utils:extract_atom(Token)).

extract_atom_from_string_token_test() ->
    Token = {lower_ident, 1, "bar"},
    ?assertEqual(bar, catena_compiler_utils:extract_atom(Token)).

extract_atom_from_tag_only_token_test() ->
    Token = {equals, 1},
    ?assertEqual(equals, catena_compiler_utils:extract_atom(Token)).

%% Test 1.2: extract_value from tokens
extract_value_integer_test() ->
    Token = {integer, 1, 42},
    ?assertEqual(42, catena_compiler_utils:extract_value(Token)).

extract_value_float_test() ->
    Token = {float, 1, 3.14},
    ?assertEqual(3.14, catena_compiler_utils:extract_value(Token)).

extract_value_string_test() ->
    Token = {string, 1, "hello"},
    ?assertEqual("hello", catena_compiler_utils:extract_value(Token)).

%% Test 1.3: extract_location from tokens
%% NOTE: These tests depend on catena_location module (not yet implemented)
%% extract_location_simple_token_test() ->
%%     Token = {transform, 5},
%%     Loc = catena_compiler_utils:extract_location(Token),
%%     ?assertMatch({location, 5, 0}, Loc).
%%
%% extract_location_token_with_value_test() ->
%%     Token = {integer, 10, 42},
%%     Loc = catena_compiler_utils:extract_location(Token),
%%     ?assertMatch({location, 10, 0}, Loc).

%% Test 1.4: extract_location from AST nodes
extract_location_from_var_test() ->
    Var = {var, x, {location, 5, 10}},
    Loc = catena_compiler_utils:extract_location(Var),
    ?assertEqual({location, 5, 10}, Loc).

extract_location_from_literal_test() ->
    Literal = {literal, 42, integer, {location, 3, 5}},
    Loc = catena_compiler_utils:extract_location(Literal),
    ?assertEqual({location, 3, 5}, Loc).

extract_location_from_binary_op_test() ->
    BinOp = {binary_op, plus, {var, x, {location, 1, 1}}, {var, y, {location, 1, 5}}, {location, 1, 3}},
    Loc = catena_compiler_utils:extract_location(BinOp),
    ?assertEqual({location, 1, 3}, Loc).

extract_location_from_transform_decl_test() ->
    FlowDecl = {transform_decl, test, undefined, [], {location, 7, 0}},
    Loc = catena_compiler_utils:extract_location(FlowDecl),
    ?assertEqual({location, 7, 0}, Loc).

%% Test 1.5: extract_name
extract_name_5_tuple_test() ->
    % 5-tuple pattern: {tag, Name, _, _, _}
    Node = {constructor, 'Some', [{x, {type_var, a, {location, 1, 10}}}], {location, 1, 0}, extra},
    ?assertEqual('Some', catena_compiler_utils:extract_name(Node)).

extract_name_4_tuple_test() ->
    % 4-tuple pattern: {tag, Name, _, _}
    Node = {pat_constructor, 'Some', [], {location, 1, 0}},
    ?assertEqual('Some', catena_compiler_utils:extract_name(Node)).

extract_name_3_tuple_test() ->
    Node = {var, x, {location, 1, 0}},
    ?assertEqual(x, catena_compiler_utils:extract_name(Node)).

extract_name_undefined_test() ->
    Node = {literal, 42, integer, {location, 1, 0}},
    ?assertEqual(undefined, catena_compiler_utils:extract_name(Node)).

%% Test 1.6: extract_transform_name and extract_transform_type
extract_transform_name_test() ->
    FlowSig = {transform_sig, my_function, undefined, {location, 1, 0}},
    ?assertEqual(my_function, catena_compiler_utils:extract_transform_name(FlowSig)).

extract_transform_type_test() ->
    Type = {type_var, a, {location, 1, 10}},
    FlowSig = {transform_sig, my_function, Type, {location, 1, 0}},
    ?assertEqual(Type, catena_compiler_utils:extract_transform_type(FlowSig)).

%%----------------------------------------------------------------------------
%% Section 2: Configuration Management
%%----------------------------------------------------------------------------

%% Test 2.1: get_config with defaults
get_config_undefined_test() ->
    Result = catena_compiler_utils:get_config(nonexistent_app, nonexistent_key),
    ?assertEqual(undefined, Result).

get_config_with_default_test() ->
    Result = catena_compiler_utils:get_config(nonexistent_app, nonexistent_key, 12345),
    ?assertEqual(12345, Result).

%% Test 2.2: Lexer configuration getters
get_max_input_size_default_test() ->
    Size = catena_compiler_utils:get_max_input_size(),
    ?assert(is_integer(Size)),
    ?assert(Size > 0).

get_max_nesting_depth_default_test() ->
    Depth = catena_compiler_utils:get_max_nesting_depth(),
    ?assert(is_integer(Depth)),
    ?assert(Depth > 0).

get_max_identifier_length_default_test() ->
    Length = catena_compiler_utils:get_max_identifier_length(),
    ?assert(is_integer(Length)),
    ?assert(Length > 0).

%% Test 2.3: Parser configuration getters
get_max_token_count_default_test() ->
    Count = catena_compiler_utils:get_max_token_count(),
    ?assert(is_integer(Count)),
    ?assert(Count > 0).

get_max_parse_time_default_test() ->
    Time = catena_compiler_utils:get_max_parse_time(),
    ?assert(is_integer(Time)),
    ?assert(Time > 0).

get_max_ast_depth_default_test() ->
    Depth = catena_compiler_utils:get_max_ast_depth(),
    ?assert(is_integer(Depth)),
    ?assert(Depth > 0).

get_max_ast_nodes_default_test() ->
    Nodes = catena_compiler_utils:get_max_ast_nodes(),
    ?assert(is_integer(Nodes)),
    ?assert(Nodes > 0).

get_max_pattern_depth_default_test() ->
    Depth = catena_compiler_utils:get_max_pattern_depth(),
    ?assert(is_integer(Depth)),
    ?assert(Depth > 0).

get_max_type_depth_default_test() ->
    Depth = catena_compiler_utils:get_max_type_depth(),
    ?assert(is_integer(Depth)),
    ?assert(Depth > 0).

get_max_substitution_size_default_test() ->
    Size = catena_compiler_utils:get_max_substitution_size(),
    ?assert(is_integer(Size)),
    ?assert(Size > 0).

get_max_environment_size_default_test() ->
    Size = catena_compiler_utils:get_max_environment_size(),
    ?assert(is_integer(Size)),
    ?assert(Size > 0).

%% Test 2.4: Configuration overrides
override_max_token_count_test() ->
    OldMax = application:get_env(catena, max_token_count, undefined),
    try
        application:set_env(catena, max_token_count, 999),
        Count = catena_compiler_utils:get_max_token_count(),
        ?assertEqual(999, Count)
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_token_count);
            Value -> application:set_env(catena, max_token_count, Value)
        end
    end.

%%----------------------------------------------------------------------------
%% Section 3: Error Formatting
%%----------------------------------------------------------------------------

%% Test 3.1: format_location
format_location_integer_test() ->
    Result = catena_compiler_utils:format_location(42),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0).

format_location_line_tuple_test() ->
    Result = catena_compiler_utils:format_location({line, 10}),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0).

%% format_location_enhanced_test() ->
%%     % NOTE: This test depends on catena_location module (not yet implemented)
%%     Result = catena_compiler_utils:format_location({location, 5, 10}),
%%     ?assert(is_list(Result)),
%%     ?assert(length(Result) > 0).

%% Test 3.2: format_file_error
format_file_error_enoent_test() ->
    Result = catena_compiler_utils:format_file_error(enoent),
    ?assertEqual("File not found", Result).

format_file_error_eacces_test() ->
    Result = catena_compiler_utils:format_file_error(eacces),
    ?assertEqual("Permission denied", Result).

format_file_error_unknown_test() ->
    Result = catena_compiler_utils:format_file_error(some_error),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0).

%% Test 3.3: format_size_error
format_size_error_test() ->
    Result = catena_compiler_utils:format_size_error("Input", 1000, 500),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0).

%% Test 3.4: format_depth_error
format_depth_error_test() ->
    Result = catena_compiler_utils:format_depth_error("AST", 600, 500),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0).

%% Test 3.5: format_timeout_error
format_timeout_error_test() ->
    Result = catena_compiler_utils:format_timeout_error("Parse", 5000, 3000),
    ?assert(is_list(Result)),
    ?assert(length(Result) > 0).

%%----------------------------------------------------------------------------
%% Section 4: AST Utilities
%%----------------------------------------------------------------------------

%% Test 4.1: ast_depth calculation
ast_depth_literal_test() ->
    AST = {literal, 42, integer, {location, 1, 0}},
    Depth = catena_compiler_utils:ast_depth(AST),
    ?assertEqual(1, Depth).

ast_depth_binary_op_test() ->
    Left = {literal, 1, integer, {location, 1, 0}},
    Right = {literal, 2, integer, {location, 1, 4}},
    AST = {binary_op, plus, Left, Right, {location, 1, 2}},
    Depth = catena_compiler_utils:ast_depth(AST),
    ?assertEqual(2, Depth).

ast_depth_nested_binary_op_test() ->
    L1 = {literal, 1, integer, {location, 1, 0}},
    L2 = {literal, 2, integer, {location, 1, 4}},
    L3 = {literal, 3, integer, {location, 1, 8}},
    Inner = {binary_op, plus, L1, L2, {location, 1, 2}},
    AST = {binary_op, plus, Inner, L3, {location, 1, 6}},
    Depth = catena_compiler_utils:ast_depth(AST),
    ?assertEqual(3, Depth).

ast_depth_transform_decl_test() ->
    Body = {literal, 42, integer, {location, 1, 15}},
    Clause = {transform_clause, [], undefined, Body, {location, 1, 0}},
    AST = {transform_decl, test, undefined, [Clause], {location, 1, 0}},
    Depth = catena_compiler_utils:ast_depth(AST),
    ?assert(Depth >= 2).

%% Test 4.2: ast_node_count
ast_node_count_literal_test() ->
    AST = {literal, 42, integer, {location, 1, 0}},
    Count = catena_compiler_utils:ast_node_count(AST),
    ?assertEqual(1, Count).

ast_node_count_binary_op_test() ->
    Left = {literal, 1, integer, {location, 1, 0}},
    Right = {literal, 2, integer, {location, 1, 4}},
    AST = {binary_op, plus, Left, Right, {location, 1, 2}},
    Count = catena_compiler_utils:ast_node_count(AST),
    ?assertEqual(3, Count).

ast_node_count_nested_test() ->
    L1 = {literal, 1, integer, {location, 1, 0}},
    L2 = {literal, 2, integer, {location, 1, 4}},
    L3 = {literal, 3, integer, {location, 1, 8}},
    Inner = {binary_op, plus, L1, L2, {location, 1, 2}},
    AST = {binary_op, plus, Inner, L3, {location, 1, 6}},
    Count = catena_compiler_utils:ast_node_count(AST),
    ?assertEqual(5, Count).

%% Test 4.3: pattern_depth calculation
pattern_depth_simple_var_test() ->
    Pattern = {pat_var, x, {location, 1, 0}},
    Depth = catena_compiler_utils:pattern_depth(Pattern),
    ?assertEqual(0, Depth).

pattern_depth_constructor_test() ->
    Inner = {pat_var, x, {location, 1, 5}},
    Pattern = {pat_constructor, 'Some', [Inner], {location, 1, 0}},
    Depth = catena_compiler_utils:pattern_depth(Pattern),
    ?assertEqual(1, Depth).

pattern_depth_nested_constructor_test() ->
    P1 = {pat_var, x, {location, 1, 10}},
    P2 = {pat_constructor, 'Some', [P1], {location, 1, 5}},
    Pattern = {pat_constructor, 'Some', [P2], {location, 1, 0}},
    Depth = catena_compiler_utils:pattern_depth(Pattern),
    ?assertEqual(2, Depth).

pattern_depth_list_test() ->
    P1 = {pat_var, x, {location, 1, 1}},
    P2 = {pat_var, y, {location, 1, 3}},
    Pattern = {pat_list, [P1, P2], {location, 1, 0}},
    Depth = catena_compiler_utils:pattern_depth(Pattern),
    ?assertEqual(1, Depth).

pattern_depth_nested_list_test() ->
    P1 = {pat_var, x, {location, 1, 2}},
    Inner = {pat_list, [P1], {location, 1, 1}},
    Pattern = {pat_list, [Inner], {location, 1, 0}},
    Depth = catena_compiler_utils:pattern_depth(Pattern),
    ?assertEqual(2, Depth).

%% Test 4.4: type_depth calculation
type_depth_simple_var_test() ->
    Type = {type_var, a, {location, 1, 0}},
    Depth = catena_compiler_utils:type_depth(Type),
    ?assertEqual(0, Depth).

type_depth_function_type_test() ->
    From = {type_var, a, {location, 1, 0}},
    To = {type_var, b, {location, 1, 5}},
    Type = {type_fun, From, To, {location, 1, 2}},
    Depth = catena_compiler_utils:type_depth(Type),
    ?assertEqual(1, Depth).

type_depth_nested_function_test() ->
    T1 = {type_var, a, {location, 1, 0}},
    T2 = {type_var, b, {location, 1, 5}},
    T3 = {type_var, c, {location, 1, 10}},
    Inner = {type_fun, T1, T2, {location, 1, 2}},
    Type = {type_fun, Inner, T3, {location, 1, 7}},
    Depth = catena_compiler_utils:type_depth(Type),
    ?assertEqual(2, Depth).

type_depth_type_app_test() ->
    Con = {type_con, 'Maybe', {location, 1, 0}},
    Arg = {type_var, a, {location, 1, 6}},
    Type = {type_app, Con, [Arg], {location, 1, 0}},
    Depth = catena_compiler_utils:type_depth(Type),
    ?assertEqual(1, Depth).

type_depth_nested_type_app_test() ->
    Con1 = {type_con, 'Maybe', {location, 1, 0}},
    Con2 = {type_con, 'Maybe', {location, 1, 6}},
    Arg = {type_var, a, {location, 1, 12}},
    Inner = {type_app, Con2, [Arg], {location, 1, 6}},
    Type = {type_app, Con1, [Inner], {location, 1, 0}},
    Depth = catena_compiler_utils:type_depth(Type),
    ?assertEqual(2, Depth).

type_depth_forall_test() ->
    Inner = {type_var, a, {location, 1, 15}},
    Type = {type_forall, [a], Inner, {location, 1, 0}},
    Depth = catena_compiler_utils:type_depth(Type),
    ?assertEqual(1, Depth).

%%----------------------------------------------------------------------------
%% Section 4.5: AST Traversal (ast_map and ast_fold)
%%----------------------------------------------------------------------------

%% Test 4.5: ast_map transformation
ast_map_simple_transformation_test() ->
    %% Transform all literals to zero
    ZeroLiterals = fun
        ({literal, _, Type, Loc}) -> {literal, 0, Type, Loc};
        (Node) -> Node
    end,

    Original = {literal, 42, integer, {location, 1, 0}},
    Result = catena_compiler_utils:ast_map(ZeroLiterals, Original),
    ?assertEqual({literal, 0, integer, {location, 1, 0}}, Result).

ast_map_nested_transformation_test() ->
    %% Transform all binary_op nodes
    IncrementOp = fun
        ({binary_op, Op, L, R, Loc}) -> {binary_op, Op, L, R, {incremented, Loc}};
        (Node) -> Node
    end,

    Left = {literal, 1, integer, {location, 1, 0}},
    Right = {literal, 2, integer, {location, 1, 4}},
    Original = {binary_op, plus, Left, Right, {location, 1, 2}},
    Result = catena_compiler_utils:ast_map(IncrementOp, Original),
    ?assertMatch({binary_op, plus, _, _, {incremented, _}}, Result).

ast_map_module_test() ->
    %% Test mapping over a module structure
    Body = {literal, 42, integer, {location, 1, 15}},
    Clause = {transform_clause, [], undefined, Body, {location, 1, 0}},
    FlowDecl = {transform_decl, test, undefined, [Clause], {location, 1, 0}},
    Module = {module, undefined, [], [], [FlowDecl], {location, 1, 0}},

    %% Count how many times the function is called
    Identity = fun(Node) -> Node end,
    Result = catena_compiler_utils:ast_map(Identity, Module),
    ?assertMatch({module, _, _, _, _, _}, Result).

%% Test 4.6: ast_fold accumulation
ast_fold_count_nodes_test() ->
    %% Count all nodes
    Counter = fun(_, Acc) -> Acc + 1 end,

    Left = {literal, 1, integer, {location, 1, 0}},
    Right = {literal, 2, integer, {location, 1, 4}},
    AST = {binary_op, plus, Left, Right, {location, 1, 2}},

    Count = catena_compiler_utils:ast_fold(Counter, 0, AST),
    ?assertEqual(3, Count).  % binary_op + 2 literals

ast_fold_collect_names_test() ->
    %% Collect all variable names
    CollectVars = fun
        ({var, Name, _}, Acc) -> [Name | Acc];
        (_, Acc) -> Acc
    end,

    Var1 = {var, x, {location, 1, 0}},
    Var2 = {var, y, {location, 1, 4}},
    AST = {binary_op, plus, Var1, Var2, {location, 1, 2}},

    Names = catena_compiler_utils:ast_fold(CollectVars, [], AST),
    ?assertEqual([y, x], Names).

ast_fold_module_test() ->
    %% Test folding over a module structure
    Body = {literal, 42, integer, {location, 1, 15}},
    Clause = {transform_clause, [], undefined, Body, {location, 1, 0}},
    FlowDecl = {transform_decl, test, undefined, [Clause], {location, 1, 0}},
    Module = {module, undefined, [], [], [FlowDecl], {location, 1, 0}},

    Counter = fun(_, Acc) -> Acc + 1 end,
    Count = catena_compiler_utils:ast_fold(Counter, 0, Module),
    ?assert(Count >= 4).  % module + transform_decl + clause + body

ast_fold_pattern_test() ->
    %% Test folding over patterns
    P1 = {pat_var, x, {location, 1, 10}},
    P2 = {pat_var, y, {location, 1, 15}},
    Pattern = {pat_constructor, 'Some', [P1, P2], {location, 1, 0}},

    Counter = fun(_, Acc) -> Acc + 1 end,
    Count = catena_compiler_utils:ast_fold(Counter, 0, Pattern),
    ?assertEqual(3, Count).  % constructor + 2 vars

ast_fold_type_test() ->
    %% Test folding over types
    From = {type_var, a, {location, 1, 0}},
    To = {type_var, b, {location, 1, 5}},
    Type = {type_fun, From, To, {location, 1, 2}},

    Counter = fun(_, Acc) -> Acc + 1 end,
    Count = catena_compiler_utils:ast_fold(Counter, 0, Type),
    ?assertEqual(3, Count).  % type_fun + 2 type_vars

%%----------------------------------------------------------------------------
%% Section 5: Validation Utilities
%%----------------------------------------------------------------------------

%% Test 5.1: validate_size
validate_size_within_limit_test() ->
    Result = catena_compiler_utils:validate_size("Input", 100, 200),
    ?assertEqual(ok, Result).

validate_size_at_limit_test() ->
    Result = catena_compiler_utils:validate_size("Input", 200, 200),
    ?assertEqual(ok, Result).

validate_size_exceeds_limit_test() ->
    Result = catena_compiler_utils:validate_size("Input", 300, 200),
    ?assertMatch({error, {size_exceeded, "Input", 300, 200}}, Result).

%% Test 5.2: validate_depth
validate_depth_within_limit_test() ->
    Result = catena_compiler_utils:validate_depth("AST", 50, 100),
    ?assertEqual(ok, Result).

validate_depth_at_limit_test() ->
    Result = catena_compiler_utils:validate_depth("AST", 100, 100),
    ?assertEqual(ok, Result).

validate_depth_exceeds_limit_test() ->
    Result = catena_compiler_utils:validate_depth("AST", 150, 100),
    ?assertMatch({error, {depth_exceeded, "AST", 150, 100}}, Result).

%% Test 5.3: validate_timeout
validate_timeout_within_limit_test() ->
    Result = catena_compiler_utils:validate_timeout("Parse", 1000, 5000),
    ?assertEqual(ok, Result).

validate_timeout_at_limit_test() ->
    Result = catena_compiler_utils:validate_timeout("Parse", 5000, 5000),
    ?assertEqual(ok, Result).

validate_timeout_exceeds_limit_test() ->
    Result = catena_compiler_utils:validate_timeout("Parse", 6000, 5000),
    ?assertMatch({error, {timeout, "Parse", 6000, 5000}}, Result).

%%----------------------------------------------------------------------------
%% Section 6: Integration Tests
%%----------------------------------------------------------------------------

%% Test 6.1: Real-world AST depth calculation
integration_ast_depth_module_test() ->
    %% Create a simple module AST
    Body = {literal, 42, integer, {location, 1, 15}},
    Clause = {transform_clause, [], undefined, Body, {location, 1, 0}},
    FlowDecl = {transform_decl, test, undefined, [Clause], {location, 1, 0}},
    Module = {module, undefined, [], [], [FlowDecl], {location, 1, 0}},

    Depth = catena_compiler_utils:ast_depth(Module),
    ?assert(Depth >= 3).

%% Test 6.2: Real-world AST node count
integration_ast_node_count_module_test() ->
    %% Create a simple module AST
    Body = {literal, 42, integer, {location, 1, 15}},
    Clause = {transform_clause, [], undefined, Body, {location, 1, 0}},
    FlowDecl = {transform_decl, test, undefined, [Clause], {location, 1, 0}},
    Module = {module, undefined, [], [], [FlowDecl], {location, 1, 0}},

    Count = catena_compiler_utils:ast_node_count(Module),
    ?assert(Count >= 4).

%% Test 6.3: Complex pattern depth
integration_complex_pattern_depth_test() ->
    %% Create nested pattern: Some(Some(Some(x)))
    P1 = {pat_var, x, {location, 1, 15}},
    P2 = {pat_constructor, 'Some', [P1], {location, 1, 10}},
    P3 = {pat_constructor, 'Some', [P2], {location, 1, 5}},
    Pattern = {pat_constructor, 'Some', [P3], {location, 1, 0}},

    Depth = catena_compiler_utils:pattern_depth(Pattern),
    ?assertEqual(3, Depth).

%% Test 6.4: Complex type depth
integration_complex_type_depth_test() ->
    %% Create nested type: Maybe (Maybe (Maybe Int))
    T1 = {type_con, 'Int', {location, 1, 18}},
    T2 = {type_app, {type_con, 'Maybe', {location, 1, 12}}, [T1], {location, 1, 12}},
    T3 = {type_app, {type_con, 'Maybe', {location, 1, 6}}, [T2], {location, 1, 6}},
    Type = {type_app, {type_con, 'Maybe', {location, 1, 0}}, [T3], {location, 1, 0}},

    Depth = catena_compiler_utils:type_depth(Type),
    ?assert(Depth >= 3).

%% Test 6.5: All configuration getters work
integration_all_config_getters_test() ->
    %% Ensure all configuration getters return valid values
    ?assert(is_integer(catena_compiler_utils:get_max_input_size())),
    ?assert(is_integer(catena_compiler_utils:get_max_nesting_depth())),
    ?assert(is_integer(catena_compiler_utils:get_max_identifier_length())),
    ?assert(is_integer(catena_compiler_utils:get_max_token_count())),
    ?assert(is_integer(catena_compiler_utils:get_max_parse_time())),
    ?assert(is_integer(catena_compiler_utils:get_max_ast_depth())),
    ?assert(is_integer(catena_compiler_utils:get_max_ast_nodes())),
    ?assert(is_integer(catena_compiler_utils:get_max_pattern_depth())),
    ?assert(is_integer(catena_compiler_utils:get_max_type_depth())),
    ?assert(is_integer(catena_compiler_utils:get_max_substitution_size())),
    ?assert(is_integer(catena_compiler_utils:get_max_environment_size())).
