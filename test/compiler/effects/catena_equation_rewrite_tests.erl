%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_equation_rewrite (Phase 8.4)
%%%-------------------------------------------------------------------
-module(catena_equation_rewrite_tests).
-include_lib("eunit/include/eunit.hrl").

rewrite_set() ->
    Eq = catena_equations:new(
        catena_equations:op(inc, 1, catena_equations:var(x)),
        catena_equations:var(x)
    ),
    catena_equation_spec:add_equation(
        catena_equation_spec:new_set(rewrite),
        simplify,
        Eq
    ).

expanding_set() ->
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:op(inc, 1, catena_equations:var(x))
    ),
    catena_equation_spec:add_equation(
        catena_equation_spec:new_set(expanding),
        expand,
        Eq
    ).

normalize_wrapper_test() ->
    Set = rewrite_set(),
    Expr = catena_equations:op(inc, 1, catena_equations:lit(3)),
    {ok, Result, _Context} = catena_equation_rewrite:normalize(Expr, Set),
    ?assertEqual(catena_equations:lit(3), Result).

rewrite_once_wrapper_test() ->
    Set = rewrite_set(),
    Expr = catena_equations:op(
        inc, 1, catena_equations:op(inc, 1, catena_equations:lit(3))
    ),
    {ok, Result, _Context} = catena_equation_rewrite:rewrite(Expr, Set, once),
    ?assertMatch({op, inc, 1, {lit, 3}}, Result).

termination_check_limit_reached_test() ->
    Set = expanding_set(),
    Expr = catena_equations:lit(1),
    {limit_reached, Result, _Context} = catena_equation_rewrite:termination_check(
        Expr,
        Set,
        #{limit => 3}
    ),
    ?assertMatch({op, inc, 1, _}, Result).
