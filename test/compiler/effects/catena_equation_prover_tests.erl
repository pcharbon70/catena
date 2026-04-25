%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_equation_prover (Phase 8.3)
%%%-------------------------------------------------------------------
-module(catena_equation_prover_tests).
-include_lib("eunit/include/eunit.hrl").

simple_rewrite_set() ->
    Eq = catena_equations:new(
        catena_equations:op(inc, 1, catena_equations:var(x)),
        catena_equations:var(x)
    ),
    catena_equation_spec:add_equation(
        catena_equation_spec:new_set(rewrite),
        simplify,
        Eq
    ).

handler_equation_set() ->
    Eq = catena_equations:new(
        catena_equations:op(get, 0, catena_equations:var(state)),
        catena_equations:var(state)
    ),
    catena_equation_spec:add_handler_equation(
        catena_equation_spec:new_set(handler),
        state_handler,
        state_get,
        Eq
    ).

normalize_rewrites_to_normal_form_test() ->
    Set = simple_rewrite_set(),
    Expr = catena_equations:op(inc, 1, catena_equations:lit(7)),
    {ok, Normal, _Context} = catena_equation_prover:normalize(Expr, Set),
    ?assertEqual(catena_equations:lit(7), Normal).

prove_equivalence_via_normalization_test() ->
    Set = simple_rewrite_set(),
    Left = catena_equations:op(inc, 1, catena_equations:lit(9)),
    Right = catena_equations:lit(9),
    {ok, Report} = catena_equation_prover:prove_equivalence(Left, Right, Set),
    ?assertEqual(normalized, maps:get(strategy, Report)),
    ?assertEqual(Right, maps:get(normal_form, Report)).

prove_equivalence_reports_missing_proof_test() ->
    EmptySet = catena_equation_spec:new_set(empty),
    Left = catena_equations:op(inc, 1, catena_equations:lit(9)),
    Right = catena_equations:lit(9),
    ?assertMatch(
        {error, #{reason := no_proof}},
        catena_equation_prover:prove_equivalence(Left, Right, EmptySet)
    ).

prove_equation_test() ->
    Set = simple_rewrite_set(),
    {ok, Eq} = catena_equation_spec:get_equation(Set, simplify),
    ?assertMatch({ok, _}, catena_equation_prover:prove_equation(Eq, Set)).

verify_handler_success_test() ->
    Set = handler_equation_set(),
    IdentityHandler = fun(Pattern) -> Pattern end,
    {ok, Report} = catena_equation_prover:verify_handler(
        state_handler,
        IdentityHandler,
        Set,
        [get]
    ),
    ?assertEqual([state_get], maps:get(verified, Report)),
    ?assertEqual([], maps:get(failed, Report)).

verify_handler_failure_test() ->
    Set = handler_equation_set(),
    BrokenHandler = fun
        ({op, get, _, _}) -> catena_equations:lit(0);
        ({var, state}) -> catena_equations:lit(1);
        (Other) -> Other
    end,
    {error, Report} = catena_equation_prover:verify_handler(
        state_handler,
        BrokenHandler,
        Set,
        [get]
    ),
    ?assertEqual([], maps:get(verified, Report)),
    ?assert(length(maps:get(failed, Report)) > 0).

equation_property_passes_test() ->
    Set = simple_rewrite_set(),
    {ok, Eq} = catena_equation_spec:get_equation(Set, simplify),
    ?assertMatch(
        {passed, _},
        catena_equation_prover:run_equation_property(Eq, Set, #{num_tests => 10})
    ).

equation_property_fails_without_equations_test() ->
    Set = simple_rewrite_set(),
    {ok, Eq} = catena_equation_spec:get_equation(Set, simplify),
    EmptySet = catena_equation_spec:new_set(empty),
    ?assertMatch(
        {failed, _},
        catena_equation_prover:run_equation_property(Eq, EmptySet, #{num_tests => 5})
    ).
