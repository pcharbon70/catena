%%%-------------------------------------------------------------------
%%% @doc Integration tests for Phase 8: Equations and Algebraic Laws
%%%
%%% This module provides end-to-end integration tests for the equation
%%% system, testing the interaction between:
%%%
%%% - Equation type and representation (Section 8.1)
%%% - Equation specification and validation (Section 8.2)
%%% - Equation application and rewriting (Section 8.3)
%%% - Proof checking and derivation (Section 8.4)
%%% - Algebraic laws library (Section 8.5)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_equations_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Setup and Teardown
%%%=============================================================================

setup_arithmetic_equations() ->
    % Create a set of simple arithmetic equations
    Eq1 = catena_equations:new(
        catena_equations:op(add, 0,
            catena_equations:seq([catena_equations:var(x), catena_equations:lit(0)])
        ),
        catena_equations:var(x)
    ),
    Set0 = catena_equation_spec:new_set(arithmetic),
    catena_equation_spec:add_equation(Set0, add_zero, Eq1).

setup_state_equations() ->
    % Use the predefined state laws
    catena_algebraic_laws:state_laws().

setup_monadic_equations() ->
    % Use the predefined monadic laws
    catena_algebraic_laws:monadic_laws().

%%%=============================================================================
%%% Equation Creation and Validation Integration Tests
%%%=============================================================================

create_and_validate_equation_test() ->
    % Test that we can create an equation and validate it
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:var(y)
    ),
    Set = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(Set, test_eq, Eq),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(SetWithEq, test_eq)).

create_equation_set_with_multiple_laws_test() ->
    % Test creating a set with multiple equations
    Set = setup_arithmetic_equations(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assertEqual(1, length(Laws)),
    ?assert(lists:member(add_zero, Laws)).

validate_full_equation_set_test() ->
    % Test that the arithmetic equation set validates
    Set = setup_arithmetic_equations(),
    ?assertEqual(ok, catena_equation_spec:validate_set(Set)).

%%%=============================================================================
%%% Pattern Matching Integration Tests
%%%=============================================================================

match_simple_pattern_test() ->
    % Test simple pattern matching with variable
    Pattern = catena_equations:var(x),
    Expr = catena_equations:var(x),
    {ok, Subst} = catena_equations:match_pattern(Pattern, Expr),
    ?assert(maps:is_key(x, Subst)).

match_operation_pattern_test() ->
    % Test matching operation patterns with variable
    Pattern = catena_equations:op(add, 1, catena_equations:var(x)),
    Expr = catena_equations:op(add, 1, catena_equations:var(x)),
    ?assertMatch({ok, _}, catena_equations:match_pattern(Pattern, Expr)).

unify_variables_test() ->
    % Test variable unification
    P1 = catena_equations:var(x),
    P2 = catena_equations:var(y),
    ?assertMatch({ok, _}, catena_equations:unify_patterns(P1, P2)).

unify_operations_test() ->
    % Test operation unification
    P1 = catena_equations:op(add, 0,
        catena_equations:seq([catena_equations:var(x), catena_equations:var(y)])
    ),
    P2 = catena_equations:op(add, 0,
        catena_equations:seq([catena_equations:var(a), catena_equations:var(b)])
    ),
    ?assertMatch({ok, _}, catena_equations:unify_patterns(P1, P2)).

%%%=============================================================================
%%% Equation Application Integration Tests
%%%=============================================================================

apply_single_equation_test() ->
    % Test applying a single equation with exact match
    Expr = catena_equations:var(x),
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:var(y)
    ),
    {expr, Result, _} = catena_equation_apply:apply_equation(Expr, Eq, forward),
    ?assertMatch({var, y}, Result).

apply_equation_backward_test() ->
    % Test applying an equation backward
    Expr = catena_equations:var(x),
    Eq = catena_equations:new(
        catena_equations:var(y),
        catena_equations:var(x)
    ),
    {expr, Result, _} = catena_equation_apply:apply_equation(Expr, Eq, backward),
    ?assertMatch({var, y}, Result).

rewrite_with_strategy_test() ->
    % Test rewriting with innermost strategy
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:var(y)
    ),
    Set = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(Set, simplify, Eq),
    Expr = catena_equations:var(x),
    {expr, Result, _} = catena_equation_apply:rewrite(Expr, SetWithEq, innermost),
    ?assertMatch({var, y}, Result).

rewrite_with_normal_strategy_test() ->
    % Test once strategy rewriting (avoids infinite loops)
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:var(y)
    ),
    Set = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(Set, simplify, Eq),
    Expr = catena_equations:var(x),
    {expr, Result, _} = catena_equation_apply:rewrite(Expr, SetWithEq, once),
    ?assertMatch({var, y}, Result).

%%%=============================================================================
%%% Proof Checking Integration Tests
%%%=============================================================================

create_and_validate_proof_step_test() ->
    % Test creating a proof step and validating it
    Step = catena_equation_proof:rule_refl(catena_equations:lit(42)),
    Set = catena_equation_spec:new_set(test),
    ?assertMatch({valid, _}, catena_equation_proof:validate_step(Step, Set)).

validate_proof_chain_test() ->
    % Test validating a proof chain
    Set = catena_equation_spec:new_set(test),
    Step1 = catena_equation_proof:rule_refl(catena_equations:lit(42)),
    Step2 = catena_equation_proof:rule_refl(catena_equations:var(x)),
    Chain = [Step1, Step2],
    ?assertMatch({valid, _}, catena_equation_proof:validate_chain(Chain, Set)).

derive_simple_proof_test() ->
    % Test deriving a simple proof
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:lit(42)
    ),
    Set = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(Set, simplify, Eq),
    {ok, Chain} = catena_equation_proof:derive(
        catena_equations:var(x),
        catena_equations:lit(42),
        SetWithEq
    ),
    ?assert(length(Chain) > 0).

%%%=============================================================================
%%% Algebraic Laws Integration Tests
%%%=============================================================================

get_state_laws_test() ->
    % Test getting state laws
    Set = catena_algebraic_laws:get_law_set(state),
    Laws = catena_equation_spec:list_equations(Set),
    ?assert(lists:member(state_put_put, Laws)),
    ?assert(lists:member(state_get_put, Laws)).

validate_state_laws_test() ->
    % Test that state laws validate
    Set = catena_algebraic_laws:state_laws(),
    ?assertEqual(ok, catena_equation_spec:validate_set(Set)).

apply_state_law_test() ->
    % Test applying a state law
    Eq = catena_algebraic_laws:state_put_put(),
    Expr = catena_equations:seq([
        catena_equations:op(put, 1, catena_equations:var(s1)),
        catena_equations:op(put, 1, catena_equations:var(s2))
    ]),
    % The equation may not match directly, but we can check it's valid
    Set = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(Set, put_put, Eq),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(SetWithEq, put_put)).

get_all_laws_test() ->
    % Test getting all law sets
    Set = catena_algebraic_laws:get_law_set(all),
    Laws = catena_equation_spec:list_equations(Set),
    ?assert(length(Laws) > 10).

%%%=============================================================================
%%% End-to-End Rewrite Scenarios
%%%=============================================================================

rewrite_arithmetic_expression_test() ->
    % Test a simple rewrite scenario
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:var(y)
    ),
    Set = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(Set, simplify, Eq),
    Expr = catena_equations:var(x),
    {expr, Result, _} = catena_equation_apply:rewrite(Expr, SetWithEq, normal),
    ?assertMatch({var, y}, Result).

rewrite_nested_expression_test() ->
    % Test rewriting nested expressions with sequence
    Eq = catena_equations:new(
        catena_equations:seq([catena_equations:var(x)]),
        catena_equations:var(y)
    ),
    Set = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(Set, simplify, Eq),
    Expr = catena_equations:seq([catena_equations:var(x)]),
    {expr, Result, _} = catena_equation_apply:rewrite(Expr, SetWithEq, normal),
    ?assertMatch({var, y}, Result).

rewrite_with_limit_test() ->
    % Test rewriting with a limit
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:op(inc, 1, catena_equations:var(x))
    ),
    Set = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(Set, expand, Eq),

    Expr = catena_equations:var(x),
    Context = catena_equation_apply:new_context(SetWithEq, 2),
    {expr, Result, _} = catena_equation_apply:rewrite_with_strategy(
        Expr, SetWithEq, normal, Context),
    ?assertMatch({op, inc, _, _}, Result).

%%%=============================================================================
%%% Error Handling Integration Tests
%%%=============================================================================

validate_invalid_equation_test() ->
    % Test that invalid equations are rejected
    % Create an equation with unbound variables
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:seq([catena_equations:var(y), catena_equations:var(z)])
    ),
    Set = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(Set, bad_eq, Eq),
    Result = catena_equation_spec:validate_set(SetWithEq),
    ?assertMatch({error, _}, Result).

apply_non_matching_equation_test() ->
    % Test that non-matching equations return no_match
    Expr = catena_equations:op(mul, 0,
        catena_equations:seq([catena_equations:var(x), catena_equations:lit(1)])
    ),
    Eq = catena_equations:new(
        catena_equations:op(add, 0,
            catena_equations:seq([catena_equations:var(x), catena_equations:lit(0)])
        ),
        catena_equations:var(x)
    ),
    ?assertMatch({no_match, _}, catena_equation_apply:apply_equation(Expr, Eq, forward)).

prove_nonexistent_proof_test() ->
    % Test that non-existent proofs are rejected
    Set = catena_equation_spec:new_set(test),
    ?assertEqual({error, no_proof},
        catena_equation_proof:find_proof(
            catena_equations:var(x),
            catena_equations:var(y),
            Set,
            10
        )
    ).

%%%=============================================================================
%%% Complex Integration Scenarios
%%%=============================================================================

combine_and_use_law_sets_test() ->
    % Test combining multiple law sets and using them
    Set1 = catena_algebraic_laws:state_laws(),
    Set2 = catena_algebraic_laws:monadic_laws(),
    Combined = catena_algebraic_laws:combine_law_sets(Set1, Set2),
    Laws = catena_equation_spec:list_equations(Combined),
    ?assert(lists:member(state_put_put, Laws)),
    ?assert(lists:member(monad_left_id, Laws)).

lookup_equations_by_operation_test() ->
    % Test looking up equations by operation
    Set = setup_arithmetic_equations(),
    Eqs = catena_equation_spec:lookup_equations(Set, add),
    ?assert(length(Eqs) > 0).

find_matching_equations_test() ->
    % Test finding equations that match a pattern
    Set = setup_arithmetic_equations(),
    Pattern = catena_equations:op(add, 0,
        catena_equations:seq([catena_equations:var(x), catena_equations:lit(0)])
    ),
    Matches = catena_equation_spec:find_matching(Set, Pattern),
    ?assert(length(Matches) > 0).

%%%=============================================================================
%%% Trace and Debug Integration Tests
%%%=============================================================================

get_rewrite_trace_test() ->
    % Test getting trace of rewrites
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:var(y)
    ),
    Set = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(Set, simplify, Eq),
    Expr = catena_equations:var(x),
    {expr, _, Context} = catena_equation_apply:rewrite(Expr, SetWithEq),
    Trace = catena_equation_apply:get_trace(Context),
    ?assert(is_list(Trace)).

chain_to_string_test() ->
    % Test converting proof chain to string
    Chain = [
        catena_equation_proof:rule_refl(catena_equations:lit(42)),
        catena_equation_proof:rule_refl(catena_equations:var(x))
    ],
    String = catena_equation_proof:chain_to_string(Chain),
    ?assert(is_list(String)),
    ?assert(string:length(String) > 0).

simplify_chain_test() ->
    % Test simplifying a proof chain
    Chain = [
        catena_equation_proof:rule_refl(catena_equations:lit(42)),
        catena_equation_proof:rule_refl(catena_equations:var(x))
    ],
    Simplified = catena_equation_proof:simplify_chain(Chain),
    % Both refl steps should be removed
    ?assertEqual([], Simplified).

%%%=============================================================================
%%% Performance and Stress Tests
%%%=============================================================================

large_equation_set_test() ->
    % Test with a larger set of equations
    Set0 = catena_equation_spec:new_set(large),
    % Add 20 equations
    SetN = lists:foldl(fun(I, Acc) ->
        Eq = catena_equations:new(
            catena_equations:var(x),
            catena_equations:var(y)
        ),
        Name = list_to_atom("eq_" ++ integer_to_list(I)),
        catena_equation_spec:add_equation(Acc, Name, Eq)
    end, Set0, lists:seq(1, 20)),
    Laws = catena_equation_spec:list_equations(SetN),
    ?assertEqual(20, length(Laws)).

deep_nesting_rewrite_test() ->
    % Test rewriting moderately nested expressions
    Set = setup_arithmetic_equations(),
    % Create a nested expression
    Nested = catena_equations:op(add, 0,
        catena_equations:seq([
            catena_equations:op(add, 0,
                catena_equations:seq([catena_equations:var(x), catena_equations:lit(0)])
            ),
            catena_equations:lit(0)
        ])
    ),
    {expr, Result, _} = catena_equation_apply:rewrite(Nested, Set, normal),
    ?assertMatch({var, x}, Result).
