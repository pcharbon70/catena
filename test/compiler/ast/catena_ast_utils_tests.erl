-module(catena_ast_utils_tests).
-include_lib("eunit/include/eunit.hrl").
-include("src/compiler/parser/catena_ast.hrl").

%%====================================================================
%% Traversal Tests
%%====================================================================

map_expr_literal_test() ->
    %% Test that map_expr preserves literals
    Expr = {literal, 42, integer, {line, 1}},
    Result = catena_ast_utils:map_expr(fun(E) -> E end, Expr),
    ?assertEqual(Expr, Result).

map_expr_binary_op_test() ->
    %% Test mapping over binary operations
    Left = {literal, 1, integer, {line, 1}},
    Right = {literal, 2, integer, {line, 1}},
    Expr = {binary_op, plus, Left, Right, {line, 1}},

    %% Map function that increments all literals
    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val + 10, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {binary_op, plus,
        {literal, 11, integer, {line, 1}},
        {literal, 12, integer, {line, 1}},
        {line, 1}},
    ?assertEqual(Expected, Result).

map_expr_nested_test() ->
    %% Test mapping over nested expressions
    Expr = {binary_op, plus,
        {binary_op, star,
            {literal, 2, integer, {line, 1}},
            {literal, 3, integer, {line, 1}},
            {line, 1}},
        {literal, 5, integer, {line, 1}},
        {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val * 2, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {binary_op, plus,
        {binary_op, star,
            {literal, 4, integer, {line, 1}},
            {literal, 6, integer, {line, 1}},
            {line, 1}},
        {literal, 10, integer, {line, 1}},
        {line, 1}},
    ?assertEqual(Expected, Result).

map_expr_list_test() ->
    %% Test mapping over list expressions
    Expr = {list_expr, [
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {literal, 3, integer, {line, 1}}
    ], {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val + 1, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {list_expr, [
        {literal, 2, integer, {line, 1}},
        {literal, 3, integer, {line, 1}},
        {literal, 4, integer, {line, 1}}
    ], {line, 1}},
    ?assertEqual(Expected, Result).

map_expr_perform_test() ->
    %% Test mapping over perform expressions
    Expr = {perform_expr, 'FileIO', read, [
        {literal, 42, integer, {line, 1}}
    ], {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val * 2, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {perform_expr, 'FileIO', read, [
        {literal, 84, integer, {line, 1}}
    ], {line, 1}},
    ?assertEqual(Expected, Result).

fold_expr_count_literals_test() ->
    %% Test folding to count literals
    Expr = {binary_op, plus,
        {binary_op, star,
            {literal, 2, integer, {line, 1}},
            {literal, 3, integer, {line, 1}},
            {line, 1}},
        {literal, 5, integer, {line, 1}},
        {line, 1}},

    Fun = fun({literal, _, _, _}, Acc) -> Acc + 1;
             (_, Acc) -> Acc
          end,

    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(3, Result).

fold_expr_sum_literals_test() ->
    %% Test folding to sum literal values
    Expr = {list_expr, [
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {literal, 3, integer, {line, 1}}
    ], {line, 1}},

    Fun = fun({literal, Val, _, _}, Acc) -> Acc + Val;
             (_, Acc) -> Acc
          end,

    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(6, Result).

walk_expr_side_effects_test() ->
    %% Test walking with side effects
    Expr = {binary_op, plus,
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {line, 1}},

    %% Use ETS table to collect visited nodes
    ets:new(visited, [named_table, public]),
    ets:insert(visited, {count, 0}),

    Fun = fun(_) ->
        [{count, C}] = ets:lookup(visited, count),
        ets:insert(visited, {count, C + 1})
    end,

    catena_ast_utils:walk_expr(Fun, Expr),
    [{count, FinalCount}] = ets:lookup(visited, count),
    ets:delete(visited),

    %% Should visit: binary_op, literal, literal = 3 nodes
    ?assertEqual(3, FinalCount).

%%====================================================================
%% Additional Traversal Tests - Missing Expression Types
%%====================================================================

%% Unary Operation Tests
map_expr_unary_op_test() ->
    %% Test mapping over unary operations
    Expr = {unary_op, negate, {literal, 5, integer, {line, 1}}, {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val * 2, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {unary_op, negate, {literal, 10, integer, {line, 1}}, {line, 1}},
    ?assertEqual(Expected, Result).

fold_expr_unary_op_test() ->
    %% Test folding over unary operations
    Expr = {unary_op, 'not', {literal, 1, integer, {line, 1}}, {line, 1}},

    Fun = fun({literal, _, _, _}, Acc) -> Acc + 1;
             (_, Acc) -> Acc
          end,

    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(1, Result).

%% Function Application Tests
map_expr_app_test() ->
    %% Test mapping over function application
    Expr = {app,
        {var, foo, {line, 1}},
        [{literal, 1, integer, {line, 1}}, {literal, 2, integer, {line, 1}}],
        {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val + 10, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {app,
        {var, foo, {line, 1}},
        [{literal, 11, integer, {line, 1}}, {literal, 12, integer, {line, 1}}],
        {line, 1}},
    ?assertEqual(Expected, Result).

fold_expr_app_test() ->
    %% Test folding over function application
    Expr = {app,
        {var, foo, {line, 1}},
        [{literal, 1, integer, {line, 1}}, {literal, 2, integer, {line, 1}}],
        {line, 1}},

    Fun = fun({literal, _, _, _}, Acc) -> Acc + 1;
             (_, Acc) -> Acc
          end,

    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(2, Result).

%% Lambda Expression Tests
map_expr_lambda_test() ->
    %% Test mapping over lambda expressions
    Expr = {lambda,
        [{pat_var, x, {line, 1}}],
        {binary_op, plus, {var, x, {line, 1}}, {literal, 1, integer, {line, 1}}, {line, 1}},
        {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val * 2, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {lambda,
        [{pat_var, x, {line, 1}}],
        {binary_op, plus, {var, x, {line, 1}}, {literal, 2, integer, {line, 1}}, {line, 1}},
        {line, 1}},
    ?assertEqual(Expected, Result).

fold_expr_lambda_test() ->
    %% Test folding over lambda expressions
    Expr = {lambda,
        [{pat_var, x, {line, 1}}],
        {literal, 42, integer, {line, 1}},
        {line, 1}},

    Fun = fun({literal, _, _, _}, Acc) -> Acc + 1;
             (_, Acc) -> Acc
          end,

    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(1, Result).

%% Let Expression Tests
map_expr_let_test() ->
    %% Test mapping over let expressions
    %% Bindings is a list of {Pattern, Expression} tuples
    Expr = {let_expr,
        [{{pat_var, x, {line, 1}}, {literal, 5, integer, {line, 1}}}],
        {var, x, {line, 1}},
        {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val + 1, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {let_expr,
        [{{pat_var, x, {line, 1}}, {literal, 6, integer, {line, 1}}}],
        {var, x, {line, 1}},
        {line, 1}},
    ?assertEqual(Expected, Result).

fold_expr_let_test() ->
    %% Test folding over let expressions
    %% Bindings is a list of {Pattern, Expression} tuples
    Expr = {let_expr,
        [{{pat_var, x, {line, 1}}, {literal, 10, integer, {line, 1}}}],
        {literal, 20, integer, {line, 1}},
        {line, 1}},

    Fun = fun({literal, Val, _, _}, Acc) -> Acc + Val;
             (_, Acc) -> Acc
          end,

    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(30, Result).

%% If Expression Tests
map_expr_if_test() ->
    %% Test mapping over if expressions
    Expr = {if_expr,
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {literal, 3, integer, {line, 1}},
        {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val * 10, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {if_expr,
        {literal, 10, integer, {line, 1}},
        {literal, 20, integer, {line, 1}},
        {literal, 30, integer, {line, 1}},
        {line, 1}},
    ?assertEqual(Expected, Result).

fold_expr_if_test() ->
    %% Test folding over if expressions
    Expr = {if_expr,
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {literal, 3, integer, {line, 1}},
        {line, 1}},

    Fun = fun({literal, _, _, _}, Acc) -> Acc + 1;
             (_, Acc) -> Acc
          end,

    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(3, Result).

%% Match Expression Tests
map_expr_match_test() ->
    %% Test mapping over match expressions
    Expr = {match_expr, [
        {match_clause,
            {pat_literal, 1, integer, {line, 1}},
            undefined,
            {literal, 10, integer, {line, 1}},
            {line, 1}}
    ], {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val + 5, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {match_expr, [
        {match_clause,
            {pat_literal, 1, integer, {line, 1}},
            undefined,
            {literal, 15, integer, {line, 1}},
            {line, 1}}
    ], {line, 1}},
    ?assertEqual(Expected, Result).

fold_expr_match_test() ->
    %% Test folding over match expressions with guards
    Expr = {match_expr, [
        {match_clause,
            {pat_var, x, {line, 1}},
            [{literal, 1, integer, {line, 1}}],
            {literal, 2, integer, {line, 1}},
            {line, 1}}
    ], {line, 1}},

    Fun = fun({literal, _, _, _}, Acc) -> Acc + 1;
             (_, Acc) -> Acc
          end,

    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(2, Result).

%% Tuple Expression Tests
map_expr_tuple_test() ->
    %% Test mapping over tuple expressions
    Expr = {tuple_expr, [
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}}
    ], {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val * 3, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {tuple_expr, [
        {literal, 3, integer, {line, 1}},
        {literal, 6, integer, {line, 1}}
    ], {line, 1}},
    ?assertEqual(Expected, Result).

fold_expr_tuple_test() ->
    %% Test folding over tuple expressions
    Expr = {tuple_expr, [
        {literal, 5, integer, {line, 1}},
        {literal, 10, integer, {line, 1}}
    ], {line, 1}},

    Fun = fun({literal, Val, _, _}, Acc) -> Acc + Val;
             (_, Acc) -> Acc
          end,

    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(15, Result).

%% Record Expression Tests
map_expr_record_test() ->
    %% Test mapping over record expressions
    %% Structure: {record_expr, Fields, Base, Loc}
    %% Fields is a list of {FieldName, Expression} tuples
    Expr = {record_expr, [
        {x, {literal, 1, integer, {line, 1}}},
        {y, {literal, 2, integer, {line, 1}}}
    ], undefined, {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val + 100, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {record_expr, [
        {x, {literal, 101, integer, {line, 1}}},
        {y, {literal, 102, integer, {line, 1}}}
    ], undefined, {line, 1}},
    ?assertEqual(Expected, Result).

fold_expr_record_test() ->
    %% Test folding over record expressions
    %% Structure: {record_expr, Fields, Base, Loc}
    %% Fields is a list of {FieldName, Expression} tuples
    Expr = {record_expr, [
        {x, {literal, 3, integer, {line, 1}}},
        {y, {literal, 7, integer, {line, 1}}}
    ], undefined, {line, 1}},

    Fun = fun({literal, Val, _, _}, Acc) -> Acc + Val;
             (_, Acc) -> Acc
          end,

    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(10, Result).

%% Record Access Tests
map_expr_record_access_test() ->
    %% Test mapping over record access
    Expr = {record_access,
        {var, point, {line, 1}},
        x,
        {line, 1}},

    Fun = fun(E) -> E end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    ?assertEqual(Expr, Result).

fold_expr_record_access_test() ->
    %% Test folding over record access
    Expr = {record_access,
        {literal, 42, integer, {line, 1}},
        field,
        {line, 1}},

    Fun = fun({literal, _, _, _}, Acc) -> Acc + 1;
             (_, Acc) -> Acc
          end,

    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(1, Result).

%% Try-With Expression Tests
map_expr_try_with_test() ->
    %% Test mapping over try-with expressions (effect handlers)
    Expr = {try_with_expr,
        {literal, 1, integer, {line, 1}},
        [{handler_clause, 'Effect', [], {line, 1}}],
        {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val + 50, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {try_with_expr,
        {literal, 51, integer, {line, 1}},
        [{handler_clause, 'Effect', [], {line, 1}}],
        {line, 1}},
    ?assertEqual(Expected, Result).

fold_expr_try_with_test() ->
    %% Test folding over try-with expressions
    Expr = {try_with_expr,
        {literal, 100, integer, {line, 1}},
        [{handler_clause, 'Effect', [
            {operation_case, op, [], {literal, 200, integer, {line, 1}}, {line, 1}}
        ], {line, 1}}],
        {line, 1}},

    Fun = fun({literal, Val, _, _}, Acc) -> Acc + Val;
             (_, Acc) -> Acc
          end,

    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(300, Result).

walk_expr_lambda_test() ->
    %% Test walking over lambda expressions
    Expr = {lambda,
        [{pat_var, x, {line, 1}}],
        {literal, 10, integer, {line, 1}},
        {line, 1}},

    ets:new(walk_test, [named_table, public]),
    ets:insert(walk_test, {count, 0}),

    Fun = fun(_) ->
        [{count, C}] = ets:lookup(walk_test, count),
        ets:insert(walk_test, {count, C + 1})
    end,

    catena_ast_utils:walk_expr(Fun, Expr),
    [{count, FinalCount}] = ets:lookup(walk_test, count),
    ets:delete(walk_test),

    %% Should visit: lambda, literal = 2 nodes
    ?assertEqual(2, FinalCount).

%%====================================================================
%% Edge Case Tests
%%====================================================================

map_expr_empty_list_test() ->
    %% Test mapping over empty list
    Expr = {list_expr, [], {line, 1}},
    Fun = fun(E) -> E end,
    Result = catena_ast_utils:map_expr(Fun, Expr),
    ?assertEqual(Expr, Result).

map_expr_empty_tuple_test() ->
    %% Test mapping over empty tuple
    Expr = {tuple_expr, [], {line, 1}},
    Fun = fun(E) -> E end,
    Result = catena_ast_utils:map_expr(Fun, Expr),
    ?assertEqual(Expr, Result).

map_expr_deeply_nested_test() ->
    %% Test mapping over deeply nested expressions (10 levels)
    BuildNested = fun BuildNested(0) ->
            {literal, 1, integer, {line, 1}};
        BuildNested(N) ->
            {binary_op, plus,
                BuildNested(N-1),
                {literal, 1, integer, {line, 1}},
                {line, 1}}
    end,

    Expr = BuildNested(10),

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val + 1, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),

    %% Verify it transformed (all literals should be incremented)
    CountLiterals = fun
        ({literal, _, _, _}, Acc) -> Acc + 1;
        (_, Acc) -> Acc
    end,

    OriginalCount = catena_ast_utils:fold_expr(CountLiterals, 0, Expr),
    ResultCount = catena_ast_utils:fold_expr(CountLiterals, 0, Result),

    ?assertEqual(OriginalCount, ResultCount),
    ?assertEqual(11, ResultCount).

fold_expr_deeply_nested_test() ->
    %% Test folding over deeply nested expressions (20 levels)
    BuildNested = fun BuildNested(0) ->
            {literal, 1, integer, {line, 1}};
        BuildNested(N) ->
            {unary_op, negate, BuildNested(N-1), {line, 1}}
    end,

    Expr = BuildNested(20),

    Fun = fun(_, Acc) -> Acc + 1 end,
    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),

    %% Should count: 20 unary_ops + 1 literal = 21 nodes
    ?assertEqual(21, Result).

map_expr_undefined_guards_test() ->
    %% Test mapping over match clause with undefined guards
    Expr = {match_expr, [
        {match_clause,
            {pat_var, x, {line, 1}},
            undefined,
            {literal, 5, integer, {line, 1}},
            {line, 1}}
    ], {line, 1}},

    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val * 2, Type, Loc};
             (E) -> E
          end,

    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {match_expr, [
        {match_clause,
            {pat_var, x, {line, 1}},
            undefined,
            {literal, 10, integer, {line, 1}},
            {line, 1}}
    ], {line, 1}},
    ?assertEqual(Expected, Result).

fold_expr_empty_list_test() ->
    %% Test folding over empty list
    Expr = {list_expr, [], {line, 1}},
    Fun = fun(_, Acc) -> Acc + 1 end,
    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    %% Should count: 1 (the list_expr node itself)
    ?assertEqual(1, Result).

walk_expr_deeply_nested_test() ->
    %% Test walking over deeply nested expressions
    BuildNested = fun BuildNested(0) ->
            {literal, 1, integer, {line, 1}};
        BuildNested(N) ->
            {binary_op, plus, BuildNested(N-1), BuildNested(N-1), {line, 1}}
    end,

    Expr = BuildNested(4),

    ets:new(deep_walk, [named_table, public]),
    ets:insert(deep_walk, {count, 0}),

    Fun = fun(_) ->
        [{count, C}] = ets:lookup(deep_walk, count),
        ets:insert(deep_walk, {count, C + 1})
    end,

    catena_ast_utils:walk_expr(Fun, Expr),
    [{count, FinalCount}] = ets:lookup(deep_walk, count),
    ets:delete(deep_walk),

    %% Should visit many nodes (binary tree structure)
    ?assert(FinalCount > 10).

%%====================================================================
%% Error Handling Tests
%%====================================================================

map_expr_unknown_node_type_test() ->
    %% Test mapping over unknown node type - should pass through unchanged
    UnknownExpr = {unknown_node_type, data, {line, 1}},
    Fun = fun(E) -> E end,
    Result = catena_ast_utils:map_expr(Fun, UnknownExpr),
    ?assertEqual(UnknownExpr, Result).

fold_expr_unknown_node_type_test() ->
    %% Test folding over unknown node type - should still count the node
    UnknownExpr = {unknown_node_type, data, {line, 1}},
    Fun = fun(_, Acc) -> Acc + 1 end,
    Result = catena_ast_utils:fold_expr(Fun, 0, UnknownExpr),
    ?assertEqual(1, Result).

format_expr_unknown_node_returns_placeholder_test() ->
    %% Test formatting unknown node type returns placeholder
    UnknownExpr = {completely_unknown, foo, bar, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(UnknownExpr)),
    ?assertEqual("<??>", Result).

map_expr_malformed_tuple_test() ->
    %% Test mapping with malformed tuple (too short) - should pass through
    MalformedExpr = {binary_op, plus},  % Missing operands and location
    Fun = fun(E) -> E end,
    %% Should pass through unchanged (fallback case)
    Result = catena_ast_utils:map_expr(Fun, MalformedExpr),
    ?assertEqual(MalformedExpr, Result).

fold_expr_with_non_list_children_test() ->
    %% Test folding handles nodes with non-list children gracefully
    %% Using var which has no children to fold over
    Expr = {var, x, {line, 1}},
    Fun = fun(_, Acc) -> Acc + 1 end,
    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    ?assertEqual(1, Result).

get_location_from_non_tuple_test() ->
    %% Test get_location with non-tuple returns default
    Result = catena_ast_utils:get_location(atom),
    ?assertEqual({line, 1}, Result).

get_location_from_empty_tuple_test() ->
    %% Test get_location with empty tuple (invalid AST) causes badarg
    %% Empty tuples are not valid AST nodes, so this should fail
    ?assertError(badarg, catena_ast_utils:get_location({})).

validate_ast_non_module_test() ->
    %% Test validation with non-module node (should handle gracefully)
    NotAModule = {literal, 42, integer, {line, 1}},
    Result = catena_ast_utils:validate_ast(NotAModule),
    %% Should return ok for valid non-module expression
    ?assertEqual(ok, Result).

map_expr_with_invalid_location_test() ->
    %% Test mapping expression with invalid location format
    Expr = {literal, 42, integer, invalid_location},
    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val + 1, Type, Loc};
             (E) -> E
          end,
    Result = catena_ast_utils:map_expr(Fun, Expr),
    Expected = {literal, 43, integer, invalid_location},
    ?assertEqual(Expected, Result).

fold_expr_large_accumulator_test() ->
    %% Test folding with large accumulator (memory safety check)
    %% Build expression with 100 literals
    BuildLargeExpr = fun BuildLargeExpr(0) ->
            {literal, 1, integer, {line, 1}};
        BuildLargeExpr(N) ->
            {list_expr, [BuildLargeExpr(N-1)], {line, 1}}
    end,

    Expr = BuildLargeExpr(100),
    Fun = fun(_, Acc) -> Acc + 1 end,
    Result = catena_ast_utils:fold_expr(Fun, 0, Expr),
    %% Should count all nodes without crashing
    ?assert(Result > 100).

%%====================================================================
%% Additional Format Coverage Tests
%%====================================================================

format_pattern_list_test() ->
    %% Test formatting list pattern
    Pattern = {pat_list, [
        {pat_var, x, {line, 1}},
        {pat_var, y, {line, 1}}
    ], {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_pattern(Pattern)),
    ?assertEqual("[x, y]", Result).

format_pattern_tuple_test() ->
    %% Test formatting tuple pattern
    Pattern = {pat_tuple, [
        {pat_literal, 1, integer, {line, 1}},
        {pat_var, x, {line, 1}}
    ], {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_pattern(Pattern)),
    ?assertEqual("(1, x)", Result).

format_pattern_record_test() ->
    %% Test formatting record pattern (not yet implemented - returns placeholder)
    Pattern = {pat_record, [
        {x, {pat_var, x, {line, 1}}},
        {y, {pat_var, y, {line, 1}}}
    ], {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_pattern(Pattern)),
    ?assertEqual("_", Result).  % Fallback for unimplemented pattern

format_pattern_as_test() ->
    %% Test formatting as-pattern (not yet implemented - returns placeholder)
    Pattern = {pat_as, name,
        {pat_constructor, 'Some', [{pat_var, x, {line, 1}}], {line, 1}},
        {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_pattern(Pattern)),
    ?assertEqual("_", Result).  % Fallback for unimplemented pattern

format_type_app_test() ->
    %% Test formatting type application
    Type = {type_app,
        {type_con, 'List', {line, 1}},
        [{type_con, 'Int', {line, 1}}],
        {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_type(Type)),
    ?assertEqual("List Int", Result).

format_type_tuple_test() ->
    %% Test formatting tuple type (not yet implemented - returns placeholder)
    Type = {type_tuple, [
        {type_con, 'Int', {line, 1}},
        {type_con, 'String', {line, 1}}
    ], {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_type(Type)),
    ?assertEqual("?", Result).  % Fallback for unimplemented type

format_type_record_test() ->
    %% Test formatting record type (not yet implemented - returns placeholder)
    Type = {type_record, [
        {x, {type_con, 'Int', {line, 1}}},
        {y, {type_con, 'Float', {line, 1}}}
    ], {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_type(Type)),
    ?assertEqual("?", Result).  % Fallback for unimplemented type

format_type_forall_test() ->
    %% Test formatting forall type (not yet implemented - returns placeholder)
    Type = {type_forall, [a, b],
        {type_fun,
            {type_var, a, {line, 1}},
            {type_var, b, {line, 1}},
            {line, 1}},
        {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_type(Type)),
    ?assertEqual("?", Result).  % Fallback for unimplemented type

format_decl_trait_test() ->
    %% Test formatting trait declaration (not yet implemented - returns placeholder)
    Decl = {trait_decl, 'Eq', [], [], {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_decl(Decl)),
    ?assertEqual("?", Result).  % Fallback for unimplemented declaration

format_pattern_unknown_test() ->
    %% Test formatting unknown pattern returns placeholder
    Pattern = {unknown_pattern, data, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_pattern(Pattern)),
    ?assertEqual("_", Result).

format_type_unknown_test() ->
    %% Test formatting unknown type returns placeholder
    Type = {unknown_type, data, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_type(Type)),
    ?assertEqual("?", Result).

format_decl_unknown_test() ->
    %% Test formatting unknown declaration returns placeholder
    Decl = {unknown_decl, data, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_decl(Decl)),
    ?assertEqual("?", Result).

%%====================================================================
%% Extended Validation Tests
%%====================================================================

validate_ast_with_all_decl_types_test() ->
    %% Test validation with all declaration types (no duplicates)
    AST = #module{name = undefined, exports = [], imports = [], declarations = [
        #shape_decl{name = 'Maybe', type_params = [], constructors = [], derives = [], location = {line, 1}},
        #flow_decl{name = map, type_sig = undefined, clauses = [], location = {line, 2}},
        #effect_decl{name = 'IO', operations = [], supereffects = [], type_params = [], constraints = [], location = {line, 3}},
        #trait_decl{name = 'Show', type_params = [], extends = undefined, methods = [], default_methods = undefined, location = {line, 4}}
    ], location = {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

validate_ast_duplicate_across_types_test() ->
    %% Test validation detects duplicates across different declaration types
    AST = #module{name = undefined, exports = [], imports = [], declarations = [
        #shape_decl{name = foo, type_params = [], constructors = [], derives = [], location = {line, 1}},
        #flow_decl{name = foo, type_sig = undefined, clauses = [], location = {line, 2}}
    ], location = {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {duplicate_declaration, foo}}, Result).

validate_ast_multiple_duplicates_test() ->
    %% Test validation with multiple duplicate names (should report first)
    AST = #module{name = undefined, exports = [], imports = [], declarations = [
        #flow_decl{name = foo, type_sig = undefined, clauses = [], location = {line, 1}},
        #flow_decl{name = foo, type_sig = undefined, clauses = [], location = {line, 2}},
        #flow_decl{name = bar, type_sig = undefined, clauses = [], location = {line, 3}},
        #flow_decl{name = bar, type_sig = undefined, clauses = [], location = {line, 4}}
    ], location = {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    %% Should report the first duplicate found
    ?assertMatch({error, {duplicate_declaration, _}}, Result).

check_duplicate_names_with_undefined_name_test() ->
    %% Test duplicate checking handles declarations with undefined names
    AST = #module{name = undefined, exports = [], imports = [], declarations = [
        #flow_decl{name = foo, type_sig = undefined, clauses = [], location = {line, 1}},
        {unknown_decl, data, {line, 2}},  % Will return undefined name
        #flow_decl{name = bar, type_sig = undefined, clauses = [], location = {line, 3}}
    ], location = {line, 1}},
    %% Should not crash on undefined name
    Result = catena_ast_utils:validate_ast(AST),
    ?assertEqual(ok, Result).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_map_then_format_test() ->
    %% Test mapping transformation followed by formatting
    Expr = {binary_op, plus,
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {line, 1}},

    %% Transform: increment all literals
    IncrementFun = fun({literal, Val, Type, Loc}) -> {literal, Val + 10, Type, Loc};
                      (E) -> E
                   end,

    Transformed = catena_ast_utils:map_expr(IncrementFun, Expr),

    %% Format the result
    Formatted = lists:flatten(catena_ast_utils:format_expr(Transformed)),

    ?assertEqual("(11 plus 12)", Formatted).

integration_fold_then_validate_test() ->
    %% Test folding to collect info, then using it for validation
    AST = #module{name = undefined, exports = [], imports = [], declarations = [
        #flow_decl{name = foo, type_sig = undefined, clauses = [], location = {line, 1}},
        #flow_decl{name = bar, type_sig = undefined, clauses = [], location = {line, 2}}
    ], location = {line, 1}},

    %% Count all nodes using fold (module node itself only, declarations aren't recursed)
    CountNodes = fun(_, Acc) -> Acc + 1 end,
    Count = catena_ast_utils:fold_expr(CountNodes, 0, AST),

    %% fold_expr only counts the module node (doesn't descend into declaration list)
    ?assertEqual(1, Count),

    %% Then validate (should pass)
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

integration_nested_map_operations_test() ->
    %% Test multiple map operations composed
    Expr = {list_expr, [
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {literal, 3, integer, {line, 1}}
    ], {line, 1}},

    %% First transformation: double all values
    DoubleFun = fun({literal, Val, Type, Loc}) -> {literal, Val * 2, Type, Loc};
                   (E) -> E
                end,

    %% Second transformation: add 1 to all values
    AddOneFun = fun({literal, Val, Type, Loc}) -> {literal, Val + 1, Type, Loc};
                   (E) -> E
                end,

    %% Compose transformations
    Step1 = catena_ast_utils:map_expr(DoubleFun, Expr),
    Step2 = catena_ast_utils:map_expr(AddOneFun, Step1),

    %% Verify: 1*2+1=3, 2*2+1=5, 3*2+1=7
    Expected = {list_expr, [
        {literal, 3, integer, {line, 1}},
        {literal, 5, integer, {line, 1}},
        {literal, 7, integer, {line, 1}}
    ], {line, 1}},

    ?assertEqual(Expected, Step2).

integration_map_walk_combined_test() ->
    %% Test using map and walk together
    Expr = {lambda,
        [{pat_var, x, {line, 1}}],
        {binary_op, plus,
            {var, x, {line, 1}},
            {literal, 5, integer, {line, 1}},
            {line, 1}},
        {line, 1}},

    %% Transform: increment literal
    Transformed = catena_ast_utils:map_expr(
        fun({literal, Val, Type, Loc}) -> {literal, Val + 1, Type, Loc};
           (E) -> E
        end,
        Expr),

    %% Walk to count nodes
    ets:new(integration_count, [named_table, public]),
    ets:insert(integration_count, {count, 0}),

    catena_ast_utils:walk_expr(
        fun(_) ->
            [{count, C}] = ets:lookup(integration_count, count),
            ets:insert(integration_count, {count, C + 1})
        end,
        Transformed),

    [{count, FinalCount}] = ets:lookup(integration_count, count),
    ets:delete(integration_count),

    %% Should have visited: lambda, binary_op, var, literal = 4 nodes
    ?assertEqual(4, FinalCount).

integration_complex_nested_transformation_test() ->
    %% Test complex nested structure with multiple expression types
    Expr = {match_expr, [
        {match_clause,
            {pat_constructor, 'Some', [{pat_var, x, {line, 1}}], {line, 1}},
            [{binary_op, gt, {var, x, {line, 1}}, {literal, 0, integer, {line, 1}}, {line, 1}}],
            {binary_op, star, {var, x, {line, 1}}, {literal, 2, integer, {line, 1}}, {line, 1}},
            {line, 1}},
        {match_clause,
            {pat_wildcard, {line, 1}},
            undefined,
            {literal, 0, integer, {line, 1}},
            {line, 1}}
    ], {line, 1}},

    %% Transform: multiply all literals by 10
    Transformed = catena_ast_utils:map_expr(
        fun({literal, Val, Type, Loc}) -> {literal, Val * 10, Type, Loc};
           (E) -> E
        end,
        Expr),

    %% Fold to sum all literal values
    SumLiterals = fun({literal, Val, _, _}, Acc) -> Acc + Val;
                     (_, Acc) -> Acc
                  end,

    Sum = catena_ast_utils:fold_expr(SumLiterals, 0, Transformed),

    %% Original literals: 0, 2, 0 -> After *10: 0, 20, 0 -> Sum: 20
    ?assertEqual(20, Sum).

integration_location_tracking_test() ->
    %% Test that locations are preserved through transformations
    Expr = {binary_op, plus,
        {literal, 1, integer, {line, 10}},
        {literal, 2, integer, {line, 20}},
        {line, 5}},

    %% Transform while preserving locations
    Transformed = catena_ast_utils:map_expr(
        fun({literal, Val, Type, Loc}) -> {literal, Val * 2, Type, Loc};
           (E) -> E
        end,
        Expr),

    %% Check locations are preserved
    ?assertEqual({line, 5}, catena_ast_utils:get_location(Transformed)),

    {binary_op, _, Left, Right, _} = Transformed,
    ?assertEqual({line, 10}, catena_ast_utils:get_location(Left)),
    ?assertEqual({line, 20}, catena_ast_utils:get_location(Right)).

%%====================================================================
%% Validation Tests
%%====================================================================

validate_empty_ast_test() ->
    %% Test validation of empty module
    AST = {module, undefined, [], [], [], {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_duplicate_names_no_duplicates_test() ->
    %% Test no duplicates
    AST = {module, undefined, [], [], [
        {flow_decl, foo, undefined, [], {line, 1}},
        {flow_decl, bar, undefined, [], {line, 2}},
        {shape_decl, baz, [], [], [], {line, 3}}
    ], {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_duplicate_names_with_duplicates_test() ->
    %% Test duplicate detection
    AST = {module, undefined, [], [], [
        {flow_decl, foo, undefined, [], {line, 1}},
        {flow_decl, foo, undefined, [], {line, 2}}
    ], {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {duplicate_declaration, foo}}, Result).

%%====================================================================
%% Location Format Validation Tests
%%====================================================================

check_location_formats_valid_line_test() ->
    %% Test valid line location format
    AST = {module, undefined, [], [], [
        {flow_decl, foo, undefined, [], {line, 1}}
    ], {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_location_formats_valid_location_test() ->
    %% Test valid location with line and column
    AST = {binary_op, plus,
        {literal, 1, integer, {location, 5, 10}},
        {literal, 2, integer, {location, 5, 15}},
        {location, 5, 10}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_location_formats_invalid_line_zero_test() ->
    %% Test invalid location with line 0
    AST = {literal, 42, integer, {line, 0}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_location, {line, 0}}}, Result).

check_location_formats_invalid_line_negative_test() ->
    %% Test invalid location with negative line
    AST = {literal, 42, integer, {line, -5}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_location, {line, -5}}}, Result).

check_location_formats_invalid_structure_test() ->
    %% Test invalid location structure
    AST = {literal, 42, integer, {invalid, foo}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_location, {invalid, foo}}}, Result).

check_location_formats_invalid_column_negative_test() ->
    %% Test invalid location with negative column
    AST = {literal, 42, integer, {location, 5, -1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_location, {location, 5, -1}}}, Result).

check_location_formats_nested_expr_test() ->
    %% Test location validation in nested expressions
    AST = {binary_op, plus,
        {literal, 1, integer, {line, 1}},
        {binary_op, times,
            {literal, 2, integer, {invalid, bad}},
            {literal, 3, integer, {line, 3}},
            {line, 2}},
        {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_location, {invalid, bad}}}, Result).

%%====================================================================
%% Literal Type Validation Tests
%%====================================================================

check_literal_types_valid_integer_test() ->
    %% Test valid integer literal
    AST = {literal, 42, integer, {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_literal_types_valid_float_test() ->
    %% Test valid float literal
    AST = {literal, 3.14, float, {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_literal_types_valid_string_test() ->
    %% Test valid string literal
    AST = {literal, "hello", string, {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_literal_types_valid_atom_test() ->
    %% Test valid atom literal
    AST = {literal, foo, atom, {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_literal_types_valid_boolean_test() ->
    %% Test valid boolean literal
    AST = {literal, true, boolean, {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_literal_types_invalid_type_test() ->
    %% Test invalid literal type
    AST = {literal, 42, invalid_type, {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_literal_type, invalid_type}}, Result).

check_literal_types_pattern_literal_valid_test() ->
    %% Test valid pattern literal
    AST = {pat_literal, 42, integer, {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_literal_types_pattern_literal_invalid_test() ->
    %% Test invalid pattern literal type
    AST = {pat_literal, 42, bad_type, {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_literal_type, bad_type}}, Result).

check_literal_types_nested_expr_test() ->
    %% Test literal type validation in nested expressions
    AST = {list_expr, [
        {literal, 1, integer, {line, 1}},
        {literal, 2, bad_type, {line, 2}},
        {literal, 3, integer, {line, 3}}
    ], {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_literal_type, bad_type}}, Result).

%%====================================================================
%% Name Validation Tests
%%====================================================================

check_valid_names_var_valid_test() ->
    %% Test valid variable name
    AST = {var, foo, {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_valid_names_var_undefined_test() ->
    %% Test undefined variable name
    AST = {var, undefined, {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_name, undefined}}, Result).

check_valid_names_var_empty_test() ->
    %% Test empty atom variable name
    AST = {var, '', {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_name, ''}}, Result).

check_valid_names_pat_var_valid_test() ->
    %% Test valid pattern variable name
    AST = {pat_var, x, {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_valid_names_pat_var_undefined_test() ->
    %% Test undefined pattern variable name
    AST = {pat_var, undefined, {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_name, undefined}}, Result).

check_valid_names_flow_decl_valid_test() ->
    %% Test valid flow declaration name
    AST = {flow_decl, my_function, undefined, [], {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_valid_names_flow_decl_undefined_test() ->
    %% Test undefined flow declaration name
    AST = {module, undefined, [], [], [
        {flow_decl, undefined, undefined, [], {line, 1}}
    ], {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_name, undefined}}, Result).

check_valid_names_shape_decl_valid_test() ->
    %% Test valid shape declaration name
    AST = {module, undefined, [], [], [
        {shape_decl, 'MyShape', [], [], [], {line, 1}}
    ], {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_valid_names_shape_decl_undefined_test() ->
    %% Test undefined shape declaration name
    AST = {module, undefined, [], [], [
        {shape_decl, undefined, [], [], [], {line, 1}}
    ], {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_name, undefined}}, Result).

check_valid_names_effect_decl_valid_test() ->
    %% Test valid effect declaration name
    AST = {module, undefined, [], [], [
        {effect_decl, 'MyEffect', [], {line, 1}}
    ], {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_valid_names_trait_decl_valid_test() ->
    %% Test valid trait declaration name
    AST = {module, undefined, [], [], [
        {trait_decl, 'MyTrait', [], [], {line, 1}}
    ], {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

check_valid_names_nested_lambda_test() ->
    %% Test name validation in nested lambda expression
    AST = {lambda,
        [{pat_var, undefined, {line, 1}}],
        {var, x, {line, 1}},
        {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_name, undefined}}, Result).

%%====================================================================
%% Multi-Validation Integration Tests
%%====================================================================

multi_validation_all_valid_test() ->
    %% Test AST that passes all validation checks
    AST = {module, undefined, [], [], [
        {flow_decl, foo,
            {type_signature, [], {type_ref, integer, {line, 1}}, {line, 1}},
            [{clause, [],
                {binary_op, plus,
                    {literal, 1, integer, {line, 2}},
                    {literal, 2, integer, {line, 2}},
                    {line, 2}},
                {line, 2}}],
            {line, 1}},
        {shape_decl, 'Point', [],
            [{constructor, 'Point', [
                {type_ref, float, {line, 3}},
                {type_ref, float, {line, 3}}
            ], {line, 3}}],
            [],
            {line, 3}}
    ], {line, 1}},
    ?assertEqual(ok, catena_ast_utils:validate_ast(AST)).

multi_validation_duplicate_and_invalid_location_test() ->
    %% Test AST with both duplicate names and invalid location
    %% Should catch the first error encountered
    AST = {module, undefined, [], [], [
        {flow_decl, foo, undefined, [], {line, 0}},
        {flow_decl, foo, undefined, [], {line, 2}}
    ], {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    %% Should catch invalid location first (before duplicate check)
    ?assertMatch({error, {invalid_location, {line, 0}}}, Result).

multi_validation_invalid_literal_in_complex_expr_test() ->
    %% Test complex expression with invalid literal type
    AST = {if_expr,
        {binary_op, equals,
            {literal, 1, integer, {line, 1}},
            {literal, 2, integer, {line, 1}},
            {line, 1}},
        {literal, true, boolean, {line, 2}},
        {literal, false, bad_type, {line, 3}},
        {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    ?assertMatch({error, {invalid_literal_type, bad_type}}, Result).

multi_validation_multiple_errors_stops_at_first_test() ->
    %% Test that validation stops at the first error
    AST = {list_expr, [
        {var, undefined, {line, 1}},  %% First error: invalid name
        {literal, 42, bad_type, {line, 0}}  %% Also has invalid type and location
    ], {line, 1}},
    Result = catena_ast_utils:validate_ast(AST),
    %% Should stop at first error encountered during traversal
    ?assertMatch({error, _}, Result).

%%====================================================================
%% Pretty-printing Tests
%%====================================================================

format_expr_literal_integer_test() ->
    Expr = {literal, 42, integer, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("42", Result).

format_expr_literal_float_test() ->
    Expr = {literal, 3.14, float, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("3.14000000000000012434e+00", Result).  % Erlang float formatting

format_expr_literal_string_test() ->
    Expr = {literal, "hello", string, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("\"hello\"", Result).

format_expr_variable_test() ->
    Expr = {var, x, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("x", Result).

format_expr_binary_op_test() ->
    Expr = {binary_op, plus,
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("(1 plus 2)", Result).

format_expr_list_test() ->
    Expr = {list_expr, [
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {literal, 3, integer, {line, 1}}
    ], {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("[1, 2, 3]", Result).

format_expr_tuple_test() ->
    Expr = {tuple_expr, [
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}}
    ], {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("(1, 2)", Result).

format_expr_perform_test() ->
    Expr = {perform_expr, 'FileIO', read, [
        {literal, 42, integer, {line, 1}}
    ], {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("perform FileIO.read(42)", Result).

format_expr_unary_op_test() ->
    Expr = {unary_op, negate, {literal, 5, integer, {line, 1}}, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("(negate 5)", Result).

format_expr_app_test() ->
    Expr = {app,
        {var, func, {line, 1}},
        [{literal, 1, integer, {line, 1}}, {literal, 2, integer, {line, 1}}],
        {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("(func 1 2)", Result).

format_expr_lambda_test() ->
    Expr = {lambda,
        [{pat_var, x, {line, 1}}],
        {binary_op, plus, {var, x, {line, 1}}, {literal, 1, integer, {line, 1}}, {line, 1}},
        {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("(\\x -> (x plus 1))", Result).

format_expr_let_test() ->
    Expr = {let_expr,
        [{{pat_var, x, {line, 1}}, {literal, 5, integer, {line, 1}}}],
        {var, x, {line, 1}},
        {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("let x = 5 in x", Result).

format_expr_if_test() ->
    Expr = {if_expr,
        {literal, true, boolean, {line, 1}},
        {literal, 1, integer, {line, 1}},
        {literal, 2, integer, {line, 1}},
        {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("if true then 1 else 2", Result).

format_expr_atom_literal_test() ->
    Expr = {literal, foo, atom, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("foo", Result).

format_expr_boolean_literal_test() ->
    Expr = {literal, true, boolean, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_expr(Expr)),
    ?assertEqual("true", Result).

format_pattern_var_test() ->
    Pattern = {pat_var, x, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_pattern(Pattern)),
    ?assertEqual("x", Result).

format_pattern_wildcard_test() ->
    Pattern = {pat_wildcard, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_pattern(Pattern)),
    ?assertEqual("_", Result).

format_pattern_literal_test() ->
    Pattern = {pat_literal, 42, integer, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_pattern(Pattern)),
    ?assertEqual("42", Result).

format_pattern_constructor_test() ->
    Pattern = {pat_constructor, 'Some', [
        {pat_var, x, {line, 1}}
    ], {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_pattern(Pattern)),
    ?assertEqual("Some x", Result).

format_type_var_test() ->
    Type = {type_var, a, {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_type(Type)),
    ?assertEqual("a", Result).

format_type_con_test() ->
    Type = {type_con, 'Int', {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_type(Type)),
    ?assertEqual("Int", Result).

format_type_fun_test() ->
    Type = {type_fun,
        {type_con, 'Int', {line, 1}},
        {type_con, 'String', {line, 1}},
        {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_type(Type)),
    ?assertEqual("Int -> String", Result).

format_type_effect_test() ->
    Type = {type_effect,
        {type_con, 'String', {line, 1}},
        ['FileIO', 'Network'],
        {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_type(Type)),
    ?assertEqual("String / {FileIO, Network}", Result).

format_decl_shape_test() ->
    Decl = #shape_decl{name = 'Maybe', type_params = [], constructors = [],
                       derives = [], location = {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_decl(Decl)),
    ?assertEqual("shape Maybe", Result).

format_decl_flow_test() ->
    Decl = #flow_decl{name = foo, type_sig = undefined, clauses = [],
                      location = {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_decl(Decl)),
    ?assertEqual("flow foo", Result).

format_decl_effect_test() ->
    Decl = #effect_decl{name = 'FileIO', operations = [], supereffects = [],
                        type_params = [], constraints = [], location = {line, 1}},
    Result = lists:flatten(catena_ast_utils:format_decl(Decl)),
    ?assertEqual("effect FileIO", Result).

%%====================================================================
%% Location Utility Tests
%%====================================================================

get_location_from_node_test() ->
    Node = {var, x, {line, 42}},
    ?assertEqual({line, 42}, catena_ast_utils:get_location(Node)).

get_location_default_test() ->
    ?assertEqual({line, 1}, catena_ast_utils:get_location(not_a_node)).

default_location_test() ->
    ?assertEqual({line, 1}, catena_ast_utils:default_location()).

%%====================================================================
%% Depth Limit Tests
%%====================================================================

%% Helper to build deeply nested binary operations
build_deep_expr(0) ->
    {literal, 1, integer, {line, 1}};
build_deep_expr(N) ->
    {binary_op, plus, build_deep_expr(N - 1), {literal, 1, integer, {line, 1}}, {line, 1}}.

map_expr_depth_limit_test() ->
    %% Build expression with depth > 1000 (exceeds MAX_DEPTH)
    DeepExpr = build_deep_expr(1001),
    ?assertThrow({error, {max_depth_exceeded, 1000}},
                 catena_ast_utils:map_expr(fun(E) -> E end, DeepExpr)).

fold_expr_depth_limit_test() ->
    %% Build expression with depth > 1000 (exceeds MAX_DEPTH)
    DeepExpr = build_deep_expr(1001),
    ?assertThrow({error, {max_depth_exceeded, 1000}},
                 catena_ast_utils:fold_expr(fun(_, Acc) -> Acc end, 0, DeepExpr)).

walk_expr_depth_limit_test() ->
    %% Build expression with depth > 1000 (exceeds MAX_DEPTH)
    DeepExpr = build_deep_expr(1001),
    ?assertThrow({error, {max_depth_exceeded, 1000}},
                 catena_ast_utils:walk_expr(fun(_) -> ok end, DeepExpr)).

map_expr_within_depth_limit_test() ->
    %% Build expression with depth = 100 (well within MAX_DEPTH)
    DeepExpr = build_deep_expr(100),
    Fun = fun({literal, Val, Type, Loc}) -> {literal, Val + 1, Type, Loc};
             (E) -> E
          end,
    %% Should succeed without throwing
    Result = catena_ast_utils:map_expr(Fun, DeepExpr),
    %% Verify it's a binary_op structure (not checking exact values)
    ?assertMatch({binary_op, plus, _, _, {line, 1}}, Result).

fold_expr_within_depth_limit_test() ->
    %% Build expression with depth = 100 (well within MAX_DEPTH)
    DeepExpr = build_deep_expr(100),
    %% Count all nodes
    Result = catena_ast_utils:fold_expr(fun(_, Acc) -> Acc + 1 end, 0, DeepExpr),
    %% Should succeed and count nodes (100 binary_ops + 101 literals = 201 nodes)
    ?assertEqual(201, Result).

walk_expr_within_depth_limit_test() ->
    %% Build expression with depth = 100 (well within MAX_DEPTH)
    DeepExpr = build_deep_expr(100),
    %% Count nodes via side effect using process dictionary
    put(walk_count, 0),
    catena_ast_utils:walk_expr(
        fun(_) ->
            put(walk_count, get(walk_count) + 1)
        end,
        DeepExpr
    ),
    Count = get(walk_count),
    erase(walk_count),
    %% Should count all 201 nodes
    ?assertEqual(201, Count).
