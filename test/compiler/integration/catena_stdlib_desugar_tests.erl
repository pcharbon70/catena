%% @doc Do-Notation Desugaring Tests
%% Tests for Section 1.5.5 of Phase 1.
%% Validates do-notation parsing and desugaring to bind chains.

-module(catena_stdlib_desugar_tests).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Section 1.5.5 - Do-Notation Desugaring
%% =============================================================================

%% 1.5.5.1 Test parsing do-block syntax
parse_do_block_test() ->
    Source = "transform check x = do { y <- x; pure y }",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, _, _, _, Decls, _}} ->
            [{transform_decl, check, _, Clauses, _}] = Decls,
            [{transform_clause, _, _, Body, _}] = Clauses,
            %% Body should be a do_expr
            ?assertMatch({do_expr, _, _}, Body);
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% 1.5.5.2 Test parsing bind in do-block
parse_do_bind_test() ->
    Source = "transform check x = do { a <- x; b <- a; pure b }",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, _, _, _, Decls, _}} ->
            [{transform_decl, check, _, Clauses, _}] = Decls,
            [{transform_clause, _, _, {do_expr, Stmts, _}, _}] = Clauses,
            %% Should have 3 statements: bind, bind, return
            ?assertEqual(3, length(Stmts)),
            [{do_bind, a, _, _}, {do_bind, b, _, _}, {do_return, _, _}] = Stmts;
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% 1.5.5.3 Test parsing let in do-block
parse_do_let_test() ->
    Source = "transform check x = do { let y = 42; pure y }",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, _, _, _, Decls, _}} ->
            [{transform_decl, check, _, Clauses, _}] = Decls,
            [{transform_clause, _, _, {do_expr, Stmts, _}, _}] = Clauses,
            %% Should have 2 statements: let, return
            ?assertEqual(2, length(Stmts)),
            [{do_let, y, _, _}, {do_return, _, _}] = Stmts;
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% 1.5.5.4 Test parsing action in do-block (sequence without binding)
parse_do_action_test() ->
    Source = "transform check x = do { print x; pure 42 }",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, _, _, _, Decls, _}} ->
            [{transform_decl, check, _, Clauses, _}] = Decls,
            [{transform_clause, _, _, {do_expr, Stmts, _}, _}] = Clauses,
            %% Should have 2 statements: action, return
            ?assertEqual(2, length(Stmts)),
            [{do_action, _, _}, {do_return, _, _}] = Stmts;
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% 1.5.5.5 Test desugaring bind to chain
desugar_bind_test() ->
    Source = "transform check x = do { y <- x; pure y }",
    {ok, AST} = catena_test_helpers:parse_source(Source),
    %% Desugar
    Desugared = catena_desugar:desugar(AST),
    %% Extract the desugared body
    {module, _, _, _, [{transform_decl, check, _, Clauses, _}], _} = Desugared,
    [{transform_clause, _, _, Body, _}] = Clauses,
    %% Should be: chain (fn y -> pure y) x
    ?assertMatch({app, {var, chain, _}, [_, _], _}, Body).

%% 1.5.5.6 Test desugaring let to let_expr
desugar_let_test() ->
    Source = "transform check u = do { let x = 42; pure x }",
    {ok, AST} = catena_test_helpers:parse_source(Source),
    Desugared = catena_desugar:desugar(AST),
    {module, _, _, _, [{transform_decl, check, _, Clauses, _}], _} = Desugared,
    [{transform_clause, _, _, Body, _}] = Clauses,
    %% Should be: let x = 42 in pure x
    ?assertMatch({let_expr, _, _, _}, Body).

%% 1.5.5.7 Test desugaring action (sequence)
desugar_action_test() ->
    Source = "transform check x = do { print x; pure 42 }",
    {ok, AST} = catena_test_helpers:parse_source(Source),
    Desugared = catena_desugar:desugar(AST),
    {module, _, _, _, [{transform_decl, check, _, Clauses, _}], _} = Desugared,
    [{transform_clause, _, _, Body, _}] = Clauses,
    %% Should be: chain (fn _ -> pure 42) (print x)
    ?assertMatch({app, {var, chain, _}, [{lambda, [{pat_wildcard, _}], _, _}, _], _}, Body).

%% 1.5.5.8 Test complex do-block with multiple binds
desugar_complex_do_test() ->
    Source = "transform check x = do { a <- x; b <- f a; c <- g b; pure (a, b, c) }",
    {ok, AST} = catena_test_helpers:parse_source(Source),
    Desugared = catena_desugar:desugar(AST),
    {module, _, _, _, [{transform_decl, check, _, Clauses, _}], _} = Desugared,
    [{transform_clause, _, _, Body, _}] = Clauses,
    %% Should be nested chain calls
    ?assertMatch({app, {var, chain, _}, _, _}, Body).

%% =============================================================================
%% Error Path Tests
%% =============================================================================

%% Test invalid do-statement: bind as final statement (no return)
error_do_bind_as_final_test() ->
    %% A do-block must end with a return expression, not a bind
    %% This is a function_clause error since desugar expects do_return as final
    Stmts = [
        {do_bind, x, {var, ma, {location, 1, 1}}, {location, 1, 1}}
        %% Missing final return statement
    ],
    DoExpr = {do_expr, Stmts, {location, 1, 1}},
    %% Desugaring will crash with function_clause - this documents expected behavior
    Result = (catch catena_desugar:desugar_do_expr(DoExpr)),
    %% Expect a function_clause error for unhandled case
    ?assertMatch({'EXIT', {function_clause, _}}, Result).

%% Test invalid do-statement: action as final statement (no return)
error_do_action_as_final_test() ->
    %% A do-block should end with a return, not an action
    Stmts = [
        {do_action, {var, action, {location, 1, 1}}, {location, 1, 1}}
        %% Missing final return
    ],
    DoExpr = {do_expr, Stmts, {location, 1, 1}},
    %% Desugaring will crash with function_clause
    Result = (catch catena_desugar:desugar_do_expr(DoExpr)),
    ?assertMatch({'EXIT', {function_clause, _}}, Result).

%% Test invalid do-statement: empty do-block
error_do_empty_block_test() ->
    %% Empty do-block is invalid
    Stmts = [],
    DoExpr = {do_expr, Stmts, {location, 1, 1}},
    Result = (catch catena_desugar:desugar_do_expr(DoExpr)),
    %% Empty list causes function_clause error
    ?assertMatch({'EXIT', {function_clause, _}}, Result).
