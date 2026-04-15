%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_equation_proof (Phase 8.4)
%%%
%%% Tests for equation proof and derivation:
%%% - Proof rules (refl, symm, trans, congr, subst, eq)
%%% - Proof step validation
%%% - Proof chain construction and validation
%%% - Proof derivation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_equation_proof_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Proof Rules Tests
%%%=============================================================================

rule_refl_test() ->
    Step = catena_equation_proof:rule_refl(catena_equations:lit(42)),
    ?assertEqual(catena_equations:lit(42), maps:get(to, Step)),
    ?assertMatch({refl, _}, maps:get(rule, Step)).

rule_symm_test() ->
    Step = catena_equation_proof:rule_symm(
        catena_equations:lit(42),
        catena_equations:lit(42),
        catena_equation_proof:rule_refl(catena_equations:lit(42))
    ),
    ?assertMatch({symm, _}, maps:get(rule, Step)).

rule_trans_test() ->
    Step1 = catena_equation_proof:rule_refl(catena_equations:lit(1)),
    Step2 = catena_equation_proof:rule_refl(catena_equations:lit(1)),
    Step3 = catena_equation_proof:rule_trans(
        catena_equations:lit(1),
        catena_equations:lit(1),
        catena_equations:lit(1),
        Step1,
        Step2
    ),
    ?assertMatch({trans, _}, maps:get(rule, Step3)).

rule_congr_test() ->
    AB = catena_equation_proof:rule_refl(catena_equations:var(x)),
    Step = catena_equation_proof:rule_congr(inc, catena_equations:var(x), catena_equations:var(x), AB),
    ?assertMatch({congr, {inc, _}}, maps:get(rule, Step)).

rule_eq_test() ->
    Eq = catena_equations:new(
        catena_equations:op(inc, 1, catena_equations:var(x)),
        catena_equations:var(x)
    ),
    Step = catena_equation_proof:rule_eq(
        catena_equations:op(inc, 1, catena_equations:lit(5)),
        catena_equations:lit(5),
        inc_inverse,
        Eq
    ),
    ?assertMatch({eq, inc_inverse}, maps:get(rule, Step)).

%%%=============================================================================
%%% Proof Steps Tests
%%%=============================================================================

new_step_test() ->
    From = [catena_equations:lit(42)],
    To = catena_equations:lit(42),
    Rule = {refl, catena_equations:lit(42)},
    Step = catena_equation_proof:new_step(From, To, Rule),
    ?assertEqual(From, maps:get(from, Step)),
    ?assertEqual(To, maps:get(to, Step)).

is_valid_step_test() ->
    ValidStep = catena_equation_proof:rule_refl(catena_equations:lit(42)),
    ?assert(catena_equation_proof:is_valid_step(ValidStep)).

validate_step_valid_refl_test() ->
    Step = catena_equation_proof:rule_refl(catena_equations:lit(42)),
    EqSet = catena_equation_spec:new_set(test),
    ?assertMatch({valid, _}, catena_equation_proof:validate_step(Step, EqSet)).

validate_step_valid_eq_test() ->
    % Test equation validation with proper LHS matching
    Eq = catena_equations:new(
        catena_equations:op(inc, 1, catena_equations:var(x)),
        catena_equations:var(x)
    ),
    EqSet = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(EqSet, inc_inverse, Eq),

    % Step that matches the equation pattern
    Step = catena_equation_proof:rule_eq(
        catena_equations:op(inc, 1, catena_equations:var(x)),
        catena_equations:var(x),
        inc_inverse,
        Eq
    ),
    ?assertMatch({valid, _}, catena_equation_proof:validate_step(Step, SetWithEq)).

validate_step_invalid_eq_test() ->
    EqSet = catena_equation_spec:new_set(test),
    Step = catena_equation_proof:rule_eq(
        catena_equations:op(unknown, 1, catena_equations:lit(5)),
        catena_equations:lit(5),
        unknown,
        catena_equations:new(catena_equations:var(x), catena_equations:var(x))
    ),
    ?assertMatch({invalid, _}, catena_equation_proof:validate_step(Step, EqSet)).

step_to_string_test() ->
    Step = catena_equation_proof:rule_refl(catena_equations:lit(42)),
    String = catena_equation_proof:step_to_string(Step),
    ?assert(is_list(String)),
    ?assert(string:str(String, "42") > 0).

%%%=============================================================================
%%% Proof Chains Tests
%%%=============================================================================

new_chain_test() ->
    Chain = catena_equation_proof:new_chain(),
    ?assertEqual([], Chain).

add_step_test() ->
    Chain0 = catena_equation_proof:new_chain(),
    Step = catena_equation_proof:rule_refl(catena_equations:lit(42)),
    Chain1 = catena_equation_proof:add_step(Chain0, Step),
    ?assertEqual(1, length(Chain1)),
    ?assertEqual(Step, lists:last(Chain1)).

validate_chain_valid_test() ->
    EqSet = catena_equation_spec:new_set(test),

    % Chain of just reflexivity steps (always valid)
    Step1 = catena_equation_proof:rule_refl(catena_equations:lit(42)),
    Step2 = catena_equation_proof:rule_refl(catena_equations:var(x)),
    Chain = [Step1, Step2],
    ?assertMatch({valid, _}, catena_equation_proof:validate_chain(Chain, EqSet)).

validate_chain_invalid_test() ->
    EqSet = catena_equation_spec:new_set(test),

    % Invalid step with unknown rule
    Step = catena_equation_proof:new_step(
        [catena_equations:lit(42)],
        catena_equations:lit(42),
        {unknown_rule, test}
    ),
    Chain = [Step],
    ?assertMatch({invalid, _}, catena_equation_proof:validate_chain(Chain, EqSet)).

chain_to_string_test() ->
    Chain = [
        catena_equation_proof:rule_refl(catena_equations:lit(42)),
        catena_equation_proof:rule_refl(catena_equations:lit(42))
    ],
    String = catena_equation_proof:chain_to_string(Chain),
    ?assert(is_list(String)),
    ?assert(string:str(String, "42") > 0).

simplify_chain_test() ->
    Chain = [
        catena_equation_proof:rule_refl(catena_equations:lit(42)),
        catena_equation_proof:rule_refl(catena_equations:lit(42))
    ],
    Simplified = catena_equation_proof:simplify_chain(Chain),
    ?assertEqual([], Simplified).

%%%=============================================================================
%%% Proof Derivation Tests
%%%=============================================================================

derive_simple_test() ->
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:lit(42)
    ),
    EqSet = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(EqSet, simplify, Eq),

    {ok, Chain} = catena_equation_proof:derive(
        catena_equations:var(x),
        catena_equations:lit(42),
        SetWithEq
    ),
    ?assert(length(Chain) > 0).

derive_identity_test() ->
    EqSet = catena_equation_spec:new_set(test),
    {ok, Chain} = catena_equation_proof:derive(
        catena_equations:lit(42),
        catena_equations:lit(42),
        EqSet
    ),
    ?assertEqual(1, length(Chain)).

prove_direct_test() ->
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:lit(42)
    ),
    EqSet = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(EqSet, simplify, Eq),

    Result = catena_equation_proof:prove(
        [catena_equations:var(x)],
        catena_equations:lit(42),
        SetWithEq,
        10
    ),
    ?assertMatch({valid, _}, Result).

find_proof_refl_test() ->
    EqSet = catena_equation_spec:new_set(test),
    {ok, Chain} = catena_equation_proof:find_proof(
        catena_equations:lit(42),
        catena_equations:lit(42),
        EqSet,
        10
    ),
    ?assertEqual(1, length(Chain)).

find_proof_no_proof_test() ->
    EqSet = catena_equation_spec:new_set(test),
    ?assertEqual({error, no_proof}, catena_equation_proof:find_proof(
        catena_equations:var(x),
        catena_equations:var(y),
        EqSet,
        10
    )).

%%%=============================================================================
%%% Complex Proof Tests
%%%=============================================================================

prove_chain_test() ->
    Eq1 = catena_equations:new(
        catena_equations:var(x),
        catena_equations:lit(0)
    ),
    Eq2 = catena_equations:new(
        catena_equations:op(add, 0, catena_equations:var(x)),
        catena_equations:var(x)
    ),
    EqSet = catena_equation_spec:new_set(test),
    SetWithEqs = catena_equation_spec:add_equations(EqSet, [
        {zero_def, Eq1},
        {add_zero, Eq2}
    ]),

    % Prove: add(0, 0) ≡ 0
    Result = catena_equation_proof:prove(
        [catena_equations:op(add, 0, catena_equations:lit(0))],
        catena_equations:lit(0),
        SetWithEqs,
        10
    ),
    ?assertMatch({valid, _}, Result).

prove_nested_test() ->
    % Test nested proof derivation
    Eq1 = catena_equations:new(
        catena_equations:op(inc, 1, catena_equations:var(x)),
        catena_equations:var(x)
    ),
    Eq2 = catena_equations:new(
        catena_equations:var(z),
        catena_equations:lit(0)
    ),
    EqSet = catena_equation_spec:new_set(test),
    SetWithEqs = catena_equation_spec:add_equations(EqSet, [
        {inc_inv, Eq1},
        {zero_def, Eq2}
    ]),

    % Prove: inc(1, z) ≡ 0 through variable chain
    Result = catena_equation_proof:prove(
        [catena_equations:op(inc, 1, catena_equations:var(z))],
        catena_equations:lit(0),
        SetWithEqs,
        10
    ),
    ?assertMatch({valid, _}, Result).
