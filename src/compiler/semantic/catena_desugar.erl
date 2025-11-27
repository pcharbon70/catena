%%%-------------------------------------------------------------------
%%% @doc Desugaring Transformations (Sections 1.5.5 and 1.5.7)
%%%
%%% This module implements desugaring transformations for Catena's
%%% syntactic sugar, converting high-level constructs to core forms.
%%%
%%% == Do-Notation Desugaring (Section 1.5.5) ==
%%%
%%% Transformations:
%%% - `x <- ma; rest` becomes `chain (fn x -> rest) ma`
%%% - `ma; rest` becomes `chain (fn _ -> rest) ma`
%%% - `let x = e; rest` becomes `let x = e in rest`
%%% - Final expr remains as-is (return value)
%%%
%%% == Operator Desugaring (Section 1.5.7) ==
%%%
%%% Category theory operators are desugared to trait method calls:
%%% - `f <$> x` becomes `map f x` (Mapper trait)
%%% - `f <*> x` becomes `apply f x` (Applicator trait)
%%% - `m >>= f` becomes `chain f m` (Chainable trait)
%%% - `a <> b` becomes `combine a b` (Combiner trait)
%%% - `a === b` becomes `equals a b` (Comparable trait)
%%% - `a !== b` becomes `not (equals a b)` (Comparable trait)
%%%
%%% The desugared code uses trait methods (chain, map, etc.).
%%%
%%% == Location Tracking ==
%%%
%%% Synthesized AST nodes (lambdas, applications) use the location of
%%% their originating statement. This means all nodes generated from
%%% `x <- ma` will have the same location as that statement.
%%%
%%% Implications for error messages:
%%% - Errors point to the original do-notation statement
%%% - Multiple nodes may share the same location
%%% - This is intentional: users wrote the do-statement, not the bind
%%%
%%% Future enhancement: Could use synthesized/generated location markers
%%% to distinguish user code from generated code in error messages.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_desugar).

-export([
    desugar/1,
    desugar_expr/1,
    desugar_do_expr/1,
    %% Operator desugaring (Section 1.5.7)
    desugar_operator/4,
    operator_to_method/1
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

%% Binary operators that should be desugared to trait methods (Section 1.5.7)
desugar_expr({binary_op, Op, Left, Right, Loc}) ->
    DesugaredLeft = desugar_expr(Left),
    DesugaredRight = desugar_expr(Right),
    case operator_to_method(Op) of
        {ok, Method} ->
            desugar_operator(Method, DesugaredLeft, DesugaredRight, Loc);
        not_desugared ->
            {binary_op, Op, DesugaredLeft, DesugaredRight, Loc}
    end;

%% Recursive cases
desugar_expr({let_expr, [{Pat, Val}], Body, Loc}) ->
    {let_expr, [{Pat, desugar_expr(Val)}], desugar_expr(Body), Loc};
desugar_expr({lambda, Params, Body, Loc}) ->
    {lambda, Params, desugar_expr(Body), Loc};
desugar_expr({app, Fun, Args, Loc}) ->
    {app, desugar_expr(Fun), [desugar_expr(A) || A <- Args], Loc};
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

%%%===================================================================
%%% Operator Desugaring (Section 1.5.7)
%%%===================================================================

%% @doc Map operators to their trait method names
%% Returns {ok, Method} for operators that should be desugared,
%% or not_desugared for operators that remain as binary ops.
-spec operator_to_method(atom()) -> {ok, atom()} | not_desugared.

%% Functor map: f <$> x => map f x
operator_to_method(fmap) -> {ok, map};

%% Applicative apply: f <*> x => apply f x
operator_to_method(ap) -> {ok, apply};

%% Monad bind: m >>= f => chain f m
operator_to_method(bind) -> {ok, chain};

%% Semigroup combine: a <> b => combine a b
operator_to_method(mappend) -> {ok, combine};

%% Setoid equality: a === b => equals a b
operator_to_method(setoid_eq) -> {ok, equals};

%% Setoid inequality: a !== b => not (equals a b)
%% Note: This requires special handling - see desugar_operator
operator_to_method(setoid_neq) -> {ok, not_equals};

%% Kleisli composition: f >=> g => kleisli f g
%% (a -> m b) >=> (b -> m c) : a -> m c
operator_to_method(kleisli) -> {ok, kleisli};

%% All other operators remain as binary ops
operator_to_method(_) -> not_desugared.

%% @doc Desugar an operator to a function application
%% Different operators have different argument orders:
%% - map f x (function first, value second)
%% - chain f m (function first, monad second)
%% - combine a b (left to right)
%% - equals a b (left to right)
-spec desugar_operator(atom(), term(), term(), term()) -> term().

%% map: f <$> x => map f x
desugar_operator(map, Func, Value, Loc) ->
    {app, {var, map, Loc}, [Func, Value], Loc};

%% apply: f <*> x => apply f x
desugar_operator(apply, Func, Value, Loc) ->
    {app, {var, apply, Loc}, [Func, Value], Loc};

%% chain: m >>= f => chain f m (note: operand order reverses!)
%% In Haskell: m >>= f means "bind m to f"
%% Our chain: chain f m means "chain function f over monad m"
desugar_operator(chain, Monad, Func, Loc) ->
    {app, {var, chain, Loc}, [Func, Monad], Loc};

%% combine: a <> b => combine a b
desugar_operator(combine, Left, Right, Loc) ->
    {app, {var, combine, Loc}, [Left, Right], Loc};

%% equals: a === b => equals a b
desugar_operator(equals, Left, Right, Loc) ->
    {app, {var, equals, Loc}, [Left, Right], Loc};

%% not_equals: a !== b => not (equals a b)
desugar_operator(not_equals, Left, Right, Loc) ->
    EqualsApp = {app, {var, equals, Loc}, [Left, Right], Loc},
    {app, {var, 'not', Loc}, [EqualsApp], Loc};

%% kleisli: f >=> g => kleisli f g
%% Kleisli composition: (a -> m b) >=> (b -> m c) : a -> m c
desugar_operator(kleisli, Left, Right, Loc) ->
    {app, {var, kleisli, Loc}, [Left, Right], Loc}.
