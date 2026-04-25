%%%-------------------------------------------------------------------
%%% @doc Catena Equation Rewrite Surface (Phase 8.4)
%%%
%%% Thin optimization-facing wrapper around the lower-level equation
%%% application engine.
%%%-------------------------------------------------------------------
-module(catena_equation_rewrite).

-export([
    rewrite/2,
    rewrite/3,
    normalize/2,
    normalize/3,
    optimize/2,
    optimize/3,
    termination_check/3
]).

-type rewrite_status() :: terminates | limit_reached.

-export_type([rewrite_status/0]).

-spec rewrite(catena_equations:pattern(), catena_equation_spec:equation_set()) ->
    {ok, catena_equations:pattern(), map()}.
rewrite(Expr, EqSet) ->
    rewrite(Expr, EqSet, normal).

-spec rewrite(
    catena_equations:pattern(),
    catena_equation_spec:equation_set(),
    catena_equation_apply:strategy()
) -> {ok, catena_equations:pattern(), map()}.
rewrite(Expr, EqSet, Strategy) ->
    unwrap_result(Expr, catena_equation_apply:rewrite(Expr, EqSet, Strategy)).

-spec normalize(catena_equations:pattern(), catena_equation_spec:equation_set()) ->
    {ok, catena_equations:pattern(), map()}.
normalize(Expr, EqSet) ->
    normalize(Expr, EqSet, #{}).

-spec normalize(catena_equations:pattern(), catena_equation_spec:equation_set(), map()) ->
    {ok, catena_equations:pattern(), map()}.
normalize(Expr, EqSet, Opts) ->
    Limit = maps:get(limit, Opts, 50),
    Context0 = catena_equation_apply:new_context(EqSet, Limit),
    Context = case maps:find(max_depth, Opts) of
        {ok, MaxDepth} -> Context0#{max_depth => MaxDepth};
        error -> Context0
    end,
    unwrap_result(Expr, catena_equation_apply:rewrite_with_strategy(Expr, EqSet, normal, Context)).

-spec optimize(catena_equations:pattern(), catena_equation_spec:equation_set()) ->
    {ok, catena_equations:pattern(), map()}.
optimize(Expr, EqSet) ->
    normalize(Expr, EqSet).

-spec optimize(catena_equations:pattern(), catena_equation_spec:equation_set(), map()) ->
    {ok, catena_equations:pattern(), map()}.
optimize(Expr, EqSet, Opts) ->
    normalize(Expr, EqSet, Opts).

-spec termination_check(
    catena_equations:pattern(),
    catena_equation_spec:equation_set(),
    map()
) -> {rewrite_status(), catena_equations:pattern(), map()}.
termination_check(Expr, EqSet, Opts) ->
    Limit = maps:get(limit, Opts, 50),
    {ok, Result, Context} = normalize(Expr, EqSet, Opts#{limit => Limit}),
    Trace = catena_equation_apply:get_trace(Context),
    case Limit =/= infinity andalso length(Trace) >= Limit of
        true -> {limit_reached, Result, Context};
        false -> {terminates, Result, Context}
    end.

unwrap_result(_Original, {expr, Result, Context}) ->
    {ok, Result, Context};
unwrap_result(Original, {no_match, Context}) ->
    {ok, Original, Context}.
