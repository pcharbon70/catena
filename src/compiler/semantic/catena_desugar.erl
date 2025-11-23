%%%-------------------------------------------------------------------
%%% @doc Do-Notation Desugaring (Section 1.5.5)
%%%
%%% This module implements desugaring transformations for Catena's
%%% do-notation, converting do-blocks into explicit bind chains.
%%%
%%% Transformations:
%%% - `x <- ma; rest` becomes `ma >>= (fn x -> rest)`
%%% - `ma; rest` becomes `ma >> rest` (sequence)
%%% - `let x = e; rest` becomes `let x = e in rest`
%%% - Final expr remains as-is (return value)
%%%
%%% The desugared code uses Pipeline.chain for >>= operations.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_desugar).

-export([
    desugar/1,
    desugar_expr/1,
    desugar_do_expr/1
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Desugar an entire AST (module or declarations)
-spec desugar(term()) -> term().
desugar({module, Name, Exports, Imports, Decls, Loc}) ->
    {module, Name, Exports, Imports, [desugar_decl(D) || D <- Decls], Loc};
desugar(Decls) when is_list(Decls) ->
    [desugar_decl(D) || D <- Decls];
desugar(Expr) ->
    desugar_expr(Expr).

%% @doc Desugar a single declaration
desugar_decl({transform_decl, Name, Type, Clauses, Loc}) ->
    {transform_decl, Name, Type, [desugar_clause(C) || C <- Clauses], Loc};
desugar_decl({instance_decl, TraitName, TypeArgs, Constraints, Methods, Loc}) ->
    DesugaredMethods = [{N, desugar_method(M)} || {N, M} <- Methods],
    {instance_decl, TraitName, TypeArgs, Constraints, DesugaredMethods, Loc};
desugar_decl(Other) ->
    Other.

%% @doc Desugar a transform clause
desugar_clause({transform_clause, Patterns, Guards, Body, Loc}) ->
    {transform_clause, Patterns, Guards, desugar_expr(Body), Loc}.

%% @doc Desugar an instance method
desugar_method({lambda, Params, Body, Loc}) ->
    {lambda, Params, desugar_expr(Body), Loc};
desugar_method(Other) ->
    Other.

%% @doc Desugar an expression, recursively processing sub-expressions
-spec desugar_expr(term()) -> term().

%% Do-expression: desugar to bind chain
desugar_expr({do_expr, Stmts, Loc}) ->
    desugar_do_expr({do_expr, Stmts, Loc});

%% Recursive cases
desugar_expr({let_expr, [{Pat, Val}], Body, Loc}) ->
    {let_expr, [{Pat, desugar_expr(Val)}], desugar_expr(Body), Loc};
desugar_expr({lambda, Params, Body, Loc}) ->
    {lambda, Params, desugar_expr(Body), Loc};
desugar_expr({app, Fun, Args, Loc}) ->
    {app, desugar_expr(Fun), [desugar_expr(A) || A <- Args], Loc};
desugar_expr({binary_op, Op, Left, Right, Loc}) ->
    {binary_op, Op, desugar_expr(Left), desugar_expr(Right), Loc};
desugar_expr({cons_expr, Head, Tail, Loc}) ->
    {cons_expr, desugar_expr(Head), desugar_expr(Tail), Loc};
desugar_expr({match_expr, Scrutinee, Clauses, Loc}) ->
    DesugaredScrutinee = case Scrutinee of
        undefined -> undefined;
        _ -> desugar_expr(Scrutinee)
    end,
    DesugaredClauses = [desugar_match_clause(C) || C <- Clauses],
    {match_expr, DesugaredScrutinee, DesugaredClauses, Loc};
desugar_expr({tuple_expr, Elems, Loc}) ->
    {tuple_expr, [desugar_expr(E) || E <- Elems], Loc};
desugar_expr({list_expr, Elems, Loc}) ->
    {list_expr, [desugar_expr(E) || E <- Elems], Loc};
desugar_expr({record_expr, Fields, Base, Loc}) ->
    DesugaredFields = [{N, desugar_expr(V)} || {N, V} <- Fields],
    {record_expr, DesugaredFields, Base, Loc};
desugar_expr({record_access, Expr, Field, Loc}) ->
    {record_access, desugar_expr(Expr), Field, Loc};
desugar_expr({handle_expr, Body, Handlers, Loc}) ->
    {handle_expr, desugar_expr(Body), Handlers, Loc};

%% Base cases (no sub-expressions to desugar)
desugar_expr({var, _, _} = Expr) -> Expr;
desugar_expr({literal, _, _, _} = Expr) -> Expr;
desugar_expr({perform_expr, _, _, _, _} = Expr) -> Expr;
desugar_expr(Other) -> Other.

%% @doc Desugar a match clause
desugar_match_clause({match_clause, Pattern, Guards, Body, Loc}) ->
    {match_clause, Pattern, Guards, desugar_expr(Body), Loc}.

%% @doc Desugar a do-expression into explicit bind chains
%%
%% do { x <- ma; y <- mb; pure (x + y) }
%% becomes:
%% chain (fn x -> chain (fn y -> pure (x + y)) mb) ma
%%
-spec desugar_do_expr(term()) -> term().
desugar_do_expr({do_expr, Stmts, Loc}) ->
    desugar_stmts(Stmts, Loc, 0).

%% Maximum nesting depth to prevent DoS via deeply nested do-blocks
-define(MAX_DO_DEPTH, 1000).

%% @doc Desugar a list of do statements with depth tracking
desugar_stmts(_, Loc, Depth) when Depth > ?MAX_DO_DEPTH ->
    %% Return error marker for excessive nesting
    {error, {do_nesting_exceeded, Depth, Loc}};

desugar_stmts([{do_return, Expr, _}], _Loc, _Depth) ->
    %% Final expression - just return it (possibly desugared)
    desugar_expr(Expr);

desugar_stmts([{do_bind, Var, MonadExpr, StmtLoc} | Rest], Loc, Depth) ->
    %% x <- ma; rest
    %% becomes: chain (fn x -> rest) ma
    RestDesugared = desugar_stmts(Rest, Loc, Depth + 1),
    Lambda = {lambda,
        [{pat_var, Var, StmtLoc}],
        RestDesugared,
        StmtLoc},
    {app,
        {var, chain, StmtLoc},
        [Lambda, desugar_expr(MonadExpr)],
        StmtLoc};

desugar_stmts([{do_action, Expr, StmtLoc} | Rest], Loc, Depth) ->
    %% ma; rest
    %% becomes: chain (fn _ -> rest) ma
    RestDesugared = desugar_stmts(Rest, Loc, Depth + 1),
    Lambda = {lambda,
        [{pat_wildcard, StmtLoc}],
        RestDesugared,
        StmtLoc},
    {app,
        {var, chain, StmtLoc},
        [Lambda, desugar_expr(Expr)],
        StmtLoc};

desugar_stmts([{do_let, Var, Expr, StmtLoc} | Rest], Loc, Depth) ->
    %% let x = e; rest
    %% becomes: let x = e in rest
    RestDesugared = desugar_stmts(Rest, Loc, Depth + 1),
    {let_expr,
        [{pat_var, Var, StmtLoc}, desugar_expr(Expr)],
        RestDesugared,
        StmtLoc}.
