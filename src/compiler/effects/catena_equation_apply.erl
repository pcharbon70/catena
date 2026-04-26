%%%-------------------------------------------------------------------
%%% @doc Catena Equation Application (Phase 8.3)
%%%
%%% This module implements equation application for rewriting expressions
%%% using algebraic equations. This is the core mechanism that enables:
%%%
%%% - Expression rewriting using equations
%%% - Optimization through algebraic simplification
%%% - Normalization to canonical forms
%%% - Equational reasoning about effectful code
%%%
%%% == Rewriting Direction ==
%%%
%%% Equations can be applied in two directions:
%%% - Forward (LHS -> RHS): Simplify or normalize expressions
%%% - Backward (RHS -> LHS): Introduce structure or abstractions
%%%
%%% == Rewrite Strategy ==
%%%
%%% Multiple rewrite strategies are supported:
%%% - innermost: Evaluate subexpressions first (call-by-value)
%%% - outermost: Evaluate outer expressions first (call-by-name)
%%% - once: Apply first matching equation only
%%% - normal: Continue until no more equations apply
%%%
%%% == Application Context ==
%%%
%%% The context maintains:
%%% - Available equations for lookup
%%% - Rewrite limits to prevent non-termination
%%% - Trace of applied rewrites for debugging
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_equation_apply).

%% Equation application API
-export([
    apply_equation/3,
    apply_equations/3,
    rewrite/2,
    rewrite/3,
    rewrite_with_strategy/3,
    rewrite_with_strategy/4
]).

%% Rewrite strategies
-export([
    innermost/2,
    outermost/2,
    once/2
]).

%% Rewrite context and limits
-export([
    new_context/1,
    new_context/2,
    add_equations/2,
    set_limit/2,
    get_trace/1
]).

%%====================================================================
%% Types
%%====================================================================

-type expr() :: catena_equations:pattern().
-type equation() :: catena_equations:equation().
-type equation_set() :: catena_equation_spec:equation_set().
-type direction() :: forward | backward.

-type strategy() :: innermost | outermost | once | normal.

-type rewrite_context() :: #{
    equations => equation_set(),
    limit => non_neg_integer() | infinity,
    trace => [rewrite_step()],
    max_depth => non_neg_integer()
}.

-type rewrite_step() :: #{
    from := expr(),
    to := expr(),
    equation => atom(),
    depth => non_neg_integer()
}.

-type rewrite_result() :: {expr, rewrite_context()} | {no_match, rewrite_context()}.

-export_type([
    strategy/0,
    direction/0,
    rewrite_context/0,
    rewrite_result/0,
    rewrite_step/0
]).

%%====================================================================
%% Equation Application API
%%====================================================================

%% @doc Apply a single equation to an expression.
-spec apply_equation(expr(), equation(), direction()) -> rewrite_result().
apply_equation(Expr, Equation, Direction) ->
    LHS = catena_equations:lhs(Equation),
    RHS = catena_equations:rhs(Equation),
    Condition = catena_equations:condition(Equation),

    Pattern = case Direction of
        forward -> LHS;
        backward -> RHS
    end,
    Target = case Direction of
        forward -> RHS;
        backward -> LHS
    end,

    case catena_equations:match_pattern(Pattern, Expr) of
        {ok, Subst} ->
            case catena_equations:evaluate_guard(Condition, Subst) of
                true ->
                    % Normalize substitution: extract values from pattern wrappers
                    NormSubst = normalize_substitution(Subst),
                    NewExpr = substitute_pattern(Target, NormSubst),
                    case NewExpr =:= Expr of
                        true ->
                            {no_match, empty_context()};
                        false ->
                            Context = #{
                                equations => catena_equation_spec:new_set(temp),
                                limit => infinity,
                                trace => [#{
                                    from => Expr,
                                    to => NewExpr,
                                    equation => unknown,
                                    depth => 0
                                }],
                                max_depth => 100,
                                rewrite_count => 0
                            },
                            {expr, NewExpr, Context}
                    end;
                false ->
                    {no_match, empty_context()}
            end;
        {error, nomatch} ->
            {no_match, empty_context()}
    end.

%% @doc Apply equations from a set to an expression.
-spec apply_equations(expr(), equation_set(), direction()) -> rewrite_result().
apply_equations(Expr, EqSet, Direction) ->
    apply_equations_loop(Expr, EqSet, Direction, #{
        equations => EqSet,
        limit => infinity,
        trace => [],
        max_depth => 100
    }, 0).

%% @private Loop through equations until one matches.
apply_equations_loop(Expr, EqSet, Direction, Context, _Depth) ->
    EqNames = catena_equation_spec:list_equations(EqSet),

    case try_equations(Expr, EqNames, EqSet, Direction, Context) of
        {ok, NewExpr, Step} ->
            {expr, NewExpr, Context#{trace => [Step | maps:get(trace, Context)]}};
        nomatch ->
            {no_match, Context}
    end.

%% @private Try each equation in sequence.
try_equations(_Expr, [], _EqSet, _Direction, _Context) -> nomatch;
try_equations(Expr, [Name | Rest], EqSet, Direction, Context) ->
    {ok, Eq} = catena_equation_spec:get_equation(EqSet, Name),
    case apply_equation(Expr, Eq, Direction) of
        {expr, NewExpr, _} ->
            Step = #{
                from => Expr,
                to => NewExpr,
                equation => Name,
                depth => 0
            },
            {ok, NewExpr, Step};
        {no_match, _} ->
            try_equations(Expr, Rest, EqSet, Direction, Context)
    end.

%%====================================================================
%% Rewrite API
%%====================================================================

%% @doc Rewrite an expression using default (innermost) strategy.
-spec rewrite(expr(), equation_set()) -> {expr, rewrite_context()} | {no_match, rewrite_context()}.
rewrite(Expr, EqSet) ->
    rewrite(Expr, EqSet, innermost).

%% @doc Rewrite an expression using a specific strategy.
-spec rewrite(expr(), equation_set(), strategy()) -> {expr, rewrite_context()} | {no_match, rewrite_context()}.
rewrite(Expr, EqSet, Strategy) ->
    Context = new_context(EqSet),
    rewrite_with_strategy(Expr, EqSet, Strategy, Context).

%% @doc Rewrite an expression with a specific strategy (creates context).
-spec rewrite_with_strategy(expr(), equation_set(), strategy()) -> {expr, rewrite_context()} | {no_match, rewrite_context()}.
rewrite_with_strategy(Expr, EqSet, Strategy) ->
    Context = new_context(EqSet),
    rewrite_with_strategy(Expr, EqSet, Strategy, Context).

%% @doc Rewrite an expression with a specific strategy and context.
-spec rewrite_with_strategy(expr(), equation_set(), strategy(), rewrite_context()) ->
    {expr, rewrite_context()} | {no_match, rewrite_context()}.
rewrite_with_strategy(Expr, EqSet, Strategy, Context) ->
    case Strategy of
        innermost -> innermost(Expr, Context#{equations => EqSet});
        outermost -> outermost(Expr, Context#{equations => EqSet});
        once -> once(Expr, Context#{equations => EqSet});
        normal -> normal_rewrite(Expr, Context#{equations => EqSet})
    end.

%%====================================================================
%% Rewrite Strategies
%%====================================================================

%% @doc Innermost rewriting: rewrite subexpressions first.
-spec innermost(expr(), rewrite_context()) -> {expr, rewrite_context()} | {no_match, rewrite_context()}.
innermost(Expr, Context) ->
    innermost_with_depth(Expr, Context, 0).

%% @private Innermost with depth tracking for limit checking.
innermost_with_depth(Expr, Context, Depth) ->
    Limit = maps:get(limit, Context, infinity),
    MaxDepth = maps:get(max_depth, Context, 100),
    RewriteCount = length(maps:get(trace, Context, [])),
    case {Limit, RewriteCount} of
        {infinity, _} when Depth >= MaxDepth ->
            {no_match, Context};
        {Limit, Count} when Limit =/= infinity, Count >= Limit ->
            {no_match, Context};
        _ ->
            case rewrite_children_innermost(Expr, Context, Depth) of
                {changed, NewExpr, NewContext} ->
                    % Try to rewrite the new expression
                    innermost_with_depth(NewExpr, NewContext, Depth + 1);
                {unchanged, _, _} ->
                    % Try to rewrite at current level
                    case apply_equations(Expr, maps:get(equations, Context), forward) of
                        {expr, NewExpr, ApplyContext} ->
                            MergedTrace = merge_traces(maps:get(trace, Context), maps:get(trace, ApplyContext)),
                            {expr, NewExpr, Context#{trace => MergedTrace}};
                        {no_match, _} ->
                            {no_match, Context}
                    end
            end
    end.

%% @private Rewrite children using innermost strategy (recursively applies innermost).
-spec rewrite_children_innermost(expr(), rewrite_context(), non_neg_integer()) ->
    {changed, expr(), rewrite_context()} | {unchanged, expr(), rewrite_context()}.
rewrite_children_innermost({op, Op, Value, Arg}, Context, Depth) ->
    case innermost_with_depth(Arg, Context, Depth + 1) of
        {expr, NewArg, NewContext} ->
            {changed, {op, Op, Value, NewArg}, NewContext};
        {no_match, _} ->
            {unchanged, {op, Op, Value, Arg}, Context}
    end;
rewrite_children_innermost({seq, Patterns}, Context, Depth) ->
    case rewrite_seq_innermost(Patterns, Context, Depth + 1, []) of
        {changed, NewPatterns, NewContext} ->
            {changed, {seq, NewPatterns}, NewContext};
        {unchanged, _, _} ->
            {unchanged, {seq, Patterns}, Context}
    end;
rewrite_children_innermost({bind, Name, Pattern}, Context, Depth) ->
    case innermost_with_depth(Pattern, Context, Depth + 1) of
        {expr, NewPattern, NewContext} ->
            {changed, {bind, Name, NewPattern}, NewContext};
        {no_match, _} ->
            {unchanged, {bind, Name, Pattern}, Context}
    end;
rewrite_children_innermost(Other, Context, _Depth) ->
    {unchanged, Other, Context}.

%% @private Rewrite a sequence list using innermost strategy.
rewrite_seq_innermost([], Context, _Depth, Acc) ->
    case lists:reverse(Acc) of
        [] -> {unchanged, [], Context};
        List -> {unchanged, List, Context}
    end;
rewrite_seq_innermost([P | Ps], Context, Depth, Acc) ->
    case innermost_with_depth(P, Context, Depth) of
        {expr, NewP, NewContext} ->
            NewList = lists:reverse(Acc) ++ [NewP | Ps],
            {changed, NewList, NewContext};
        {no_match, _} ->
            rewrite_seq_innermost(Ps, Context, Depth, [P | Acc])
    end.

%% @doc Outermost rewriting: rewrite outer expressions first.
-spec outermost(expr(), rewrite_context()) -> {expr, rewrite_context()} | {no_match, rewrite_context()}.
outermost(Expr, Context) ->
    outermost_with_limit(Expr, Context, 0).

%% @private Outermost with rewrite limit tracking.
outermost_with_limit(Expr, Context, Depth) ->
    Limit = maps:get(limit, Context, infinity),
    RewriteCount = length(maps:get(trace, Context, [])),
    MaxDepth = maps:get(max_depth, Context, 100),
    case {Limit, RewriteCount} of
        {infinity, _} when Depth >= MaxDepth ->
            {no_match, Context};
        {Limit, Count} when Limit =/= infinity, Count >= Limit ->
            {no_match, Context};
        _ ->
            % Try to rewrite at current level first
            case apply_equations(Expr, maps:get(equations, Context), forward) of
                {expr, NewExpr, ApplyContext} ->
                    MergedTrace = merge_traces(maps:get(trace, Context), maps:get(trace, ApplyContext)),
                    % Continue rewriting the new expression
                    outermost_with_limit(NewExpr, Context#{trace => MergedTrace}, Depth + 1);
                {no_match, _} ->
                    % Try to rewrite children
                    case rewrite_children_outermost(Expr, Context) of
                        {changed, NewExpr, NewContext} ->
                            outermost_with_limit(NewExpr, NewContext, Depth + 1);
                        {unchanged, _, _} ->
                            {no_match, Context}
                    end
            end
    end.

%% @private Rewrite children using outermost strategy.
-spec rewrite_children_outermost(expr(), rewrite_context()) ->
    {changed, expr(), rewrite_context()} | {unchanged, expr(), rewrite_context()}.
rewrite_children_outermost({op, Op, Value, Arg}, Context) ->
    case outermost(Arg, Context) of
        {expr, NewArg, NewContext} ->
            {changed, {op, Op, Value, NewArg}, NewContext};
        {no_match, _} ->
            {unchanged, {op, Op, Value, Arg}, Context}
    end;
rewrite_children_outermost({seq, Patterns}, Context) ->
    case rewrite_seq_outermost(Patterns, Context, []) of
        {changed, NewPatterns, NewContext} ->
            {changed, {seq, NewPatterns}, NewContext};
        {unchanged, _, _} ->
            {unchanged, {seq, Patterns}, Context}
    end;
rewrite_children_outermost({bind, Name, Pattern}, Context) ->
    case outermost(Pattern, Context) of
        {expr, NewPattern, NewContext} ->
            {changed, {bind, Name, NewPattern}, NewContext};
        {no_match, _} ->
            {unchanged, {bind, Name, Pattern}, Context}
    end;
rewrite_children_outermost(Other, Context) ->
    {unchanged, Other, Context}.

%% @private Rewrite a sequence list using outermost strategy.
rewrite_seq_outermost([], Context, Acc) ->
    case lists:reverse(Acc) of
        [] -> {unchanged, [], Context};
        List -> {unchanged, List, Context}
    end;
rewrite_seq_outermost([P | Ps], Context, Acc) ->
    case outermost(P, Context) of
        {expr, NewP, NewContext} ->
            NewList = lists:reverse(Acc) ++ [NewP | Ps],
            {changed, NewList, NewContext};
        {no_match, _} ->
            rewrite_seq_outermost(Ps, Context, [P | Acc])
    end.

%% @doc Apply first matching equation once, don't recurse.
-spec once(expr(), rewrite_context()) -> {expr, rewrite_context()} | {no_match, rewrite_context()}.
once(Expr, Context) ->
    case apply_equations(Expr, maps:get(equations, Context), forward) of
        {expr, NewExpr, ApplyContext} ->
            MergedTrace = merge_traces(maps:get(trace, Context), maps:get(trace, ApplyContext)),
            {expr, NewExpr, Context#{trace => MergedTrace}};
        {no_match, _} ->
            {no_match, Context}
    end.

%% @doc Normal rewriting: continue until normal form (no more equations apply).
-spec normal_rewrite(expr(), rewrite_context()) -> {expr, rewrite_context()} | {no_match, rewrite_context()}.
normal_rewrite(Expr, Context) ->
    Limit = maps:get(limit, Context, infinity),
    normal_rewrite_loop(Expr, Context, 0, Limit).

%% @private Loop for normal rewrite with limit checking.
normal_rewrite_loop(Expr, Context, Count, Limit) when Limit =/= infinity, Count >= Limit ->
    {expr, Expr, Context};
normal_rewrite_loop(Expr, Context, Count, Limit) ->
    case innermost(Expr, Context) of
        {expr, NewExpr, NewContext} ->
            normal_rewrite_loop(NewExpr, NewContext#{trace => maps:get(trace, NewContext)}, Count + 1, Limit);
        {no_match, FinalContext} ->
            {expr, Expr, FinalContext}
    end.

%%====================================================================
%% Rewrite Context and Limits
%%====================================================================

%% @doc Create a new rewrite context from an equation set.
-spec new_context(equation_set()) -> rewrite_context().
new_context(EqSet) ->
    new_context(EqSet, infinity).

%% @doc Create a new rewrite context with a rewrite limit.
-spec new_context(equation_set(), non_neg_integer() | infinity) -> rewrite_context().
new_context(EqSet, Limit) ->
    #{
        equations => EqSet,
        limit => Limit,
        trace => [],
        max_depth => 100,
        rewrite_count => 0
    }.

%% @doc Add equations to a rewrite context.
-spec add_equations(rewrite_context(), equation_set()) -> rewrite_context().
add_equations(Context, EqSet) ->
    Context#{equations => EqSet}.

%% @doc Set the rewrite limit.
-spec set_limit(rewrite_context(), non_neg_integer() | infinity) -> rewrite_context().
set_limit(Context, Limit) ->
    Context#{limit => Limit}.

%% @doc Get the trace of applied rewrites.
-spec get_trace(rewrite_context()) -> [rewrite_step()].
get_trace(Context) ->
    lists:reverse(maps:get(trace, Context, [])).

%%====================================================================
%% Private Functions
%%====================================================================

%% @private Rewrite children of an expression.
-spec rewrite_children(expr(), rewrite_context()) ->
    {changed, expr(), rewrite_context()} | {unchanged, expr(), rewrite_context()}.
rewrite_children({op, Op, Value, Arg}, Context) ->
    case rewrite_children(Arg, Context) of
        {changed, NewArg, NewContext} ->
            {changed, {op, Op, Value, NewArg}, NewContext};
        {unchanged, _, _} ->
            {unchanged, {op, Op, Value, Arg}, Context}
    end;
rewrite_children({seq, Patterns}, Context) ->
    case rewrite_seq_list(Patterns, Context, []) of
        {changed, NewPatterns, NewContext} ->
            {changed, {seq, NewPatterns}, NewContext};
        {unchanged, _, _} ->
            {unchanged, {seq, Patterns}, Context}
    end;
rewrite_children({bind, Name, Pattern}, Context) ->
    case rewrite_children(Pattern, Context) of
        {changed, NewPattern, NewContext} ->
            {changed, {bind, Name, NewPattern}, NewContext};
        {unchanged, _, _} ->
            {unchanged, {bind, Name, Pattern}, Context}
    end;
rewrite_children(Other, Context) ->
    {unchanged, Other, Context}.

%% @private Rewrite a list of patterns.
-spec rewrite_seq_list([catena_equations:pattern()], rewrite_context(), [catena_equations:pattern()]) ->
    {changed, [catena_equations:pattern()], rewrite_context()} |
    {unchanged, [catena_equations:pattern()], rewrite_context()}.
rewrite_seq_list([], Context, Acc) ->
    case lists:reverse(Acc) of
        [] -> {unchanged, [], Context};
        List -> {unchanged, List, Context}
    end;
rewrite_seq_list([P | Ps], Context, Acc) ->
    case rewrite_children(P, Context) of
        {changed, NewP, NewContext} ->
            NewList = lists:reverse(Acc) ++ [NewP | Ps],
            {changed, NewList, NewContext};
        {unchanged, _, _} ->
            rewrite_seq_list(Ps, Context, [P | Acc])
    end.

%% @private Merge two trace lists.
merge_traces(Trace1, Trace2) ->
    lists:append(lists:reverse(Trace2), Trace1).

%% @private Normalize substitution by extracting values from pattern wrappers.
%% When matching stores patterns like {lit, 42}, we extract the value 42.
normalize_substitution(Subst) ->
    maps:map(fun(_Key, Value) -> extract_value_from_pattern(Value) end, Subst).

%% @private Extract the actual value from a pattern for substitution.
%% For literal patterns, extract the value. For complex patterns, keep as-is.
extract_value_from_pattern({lit, V}) -> V;
extract_value_from_pattern({var, _} = V) -> V;
extract_value_from_pattern({op, _, _, _} = V) -> V;
extract_value_from_pattern({seq, _} = V) -> V;
extract_value_from_pattern({wildcard}) -> {wildcard};
extract_value_from_pattern({bind, _, _} = V) -> V;
extract_value_from_pattern(Other) -> Other.

%% @private Substitute variables in a pattern with values from substitution.
%% This reconstructs patterns with substituted values.
substitute_pattern({var, Name}, Subst) ->
    case maps:find(Name, Subst) of
        {ok, Value} -> reconstruct_pattern(Value);
        error -> {var, Name}
    end;
substitute_pattern({lit, Value}, _Subst) ->
    {lit, Value};
substitute_pattern({op, Op, Value, Arg}, Subst) ->
    {op, Op, Value, substitute_pattern(Arg, Subst)};
substitute_pattern({seq, Patterns}, Subst) ->
    {seq, [substitute_pattern(P, Subst) || P <- Patterns]};
substitute_pattern({wildcard}, _Subst) ->
    {wildcard};
substitute_pattern({bind, Name, Pattern}, Subst) ->
    {bind, Name, substitute_pattern(Pattern, Subst)}.

%% @private Reconstruct a pattern from a substituted value.
%% If the value is a simple term, wrap in {lit, ...}.
%% If it's already a pattern, return as-is.
reconstruct_pattern({var, _} = V) -> V;
reconstruct_pattern({lit, _} = V) -> V;
reconstruct_pattern({op, _, _, _} = V) -> V;
reconstruct_pattern({seq, _} = V) -> V;
reconstruct_pattern({wildcard}) -> {wildcard};
reconstruct_pattern({bind, _, _} = V) -> V;
reconstruct_pattern(Value) -> {lit, Value}.

empty_context() ->
    #{
        equations => catena_equation_spec:new_set(temp),
        limit => infinity,
        trace => [],
        max_depth => 100,
        rewrite_count => 0
    }.
