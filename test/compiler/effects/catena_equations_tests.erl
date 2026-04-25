%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_equations (Phase 8.1)
%%%
%%% Tests for equation type and representation:
%%% - Equation type definition and constructors
%%% - Pattern matching and unification
%%% - Guard evaluation
%%% - Variable extraction and substitution
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_equations_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% No special setup needed for this module

%%%=============================================================================
%%% Equation Type and Constructors Tests
%%%=============================================================================

equation_new_test() ->
    LHS = catena_equations:var(x),
    RHS = catena_equations:lit(42),
    Eq = catena_equations:new(LHS, RHS),
    ?assert(catena_equations:is_equation(Eq)),
    ?assertEqual(LHS, catena_equations:lhs(Eq)),
    ?assertEqual(RHS, catena_equations:rhs(Eq)).

equation_new_with_condition_test() ->
    LHS = catena_equations:var(x),
    RHS = catena_equations:lit(42),
    Condition = {is_type, integer},
    Eq = catena_equations:new(LHS, RHS, Condition),
    ?assert(catena_equations:has_condition(Eq)),
    ?assertEqual(Condition, catena_equations:condition(Eq)).

equation_new_unconditional_test() ->
    LHS = catena_equations:var(x),
    RHS = catena_equations:lit(42),
    Eq = catena_equations:new(LHS, RHS),
    ?assertNot(catena_equations:has_condition(Eq)),
    ?assertEqual(undefined, catena_equations:condition(Eq)).

equation_from_patterns_test() ->
    Eq = catena_equations:from_patterns({var, x}, {lit, 42}),
    ?assert(catena_equations:is_equation(Eq)),
    ?assertMatch({var, x}, catena_equations:lhs(Eq)),
    ?assertMatch({lit, 42}, catena_equations:rhs(Eq)).

equation_flip_test() ->
    LHS = catena_equations:var(x),
    RHS = catena_equations:lit(42),
    Eq = catena_equations:new(LHS, RHS),
    Flipped = catena_equations:flip(Eq),
    ?assertEqual(RHS, catena_equations:lhs(Flipped)),
    ?assertEqual(LHS, catena_equations:rhs(Flipped)).

%%%=============================================================================
%%% Pattern Constructors Tests
%%%=============================================================================

pattern_var_test() ->
    Pattern = catena_equations:var(x),
    ?assertMatch({var, x}, Pattern).

pattern_lit_test() ->
    Pattern = catena_equations:lit(42),
    ?assertMatch({lit, 42}, Pattern).

pattern_op_test() ->
    Arg = catena_equations:var(x),
    Pattern = catena_equations:op(inc, 1, Arg),
    ?assertMatch({op, inc, 1, {var, x}}, Pattern).

pattern_seq_test() ->
    Patterns = [catena_equations:var(x), catena_equations:var(y)],
    Pattern = catena_equations:seq(Patterns),
    ?assertMatch({seq, [{var, x}, {var, y}]}, Pattern).

pattern_wildcard_test() ->
    Pattern = catena_equations:wildcard(),
    ?assertMatch({wildcard}, Pattern).

pattern_bind_test() ->
    Inner = catena_equations:op(inc, 1, catena_equations:wildcard()),
    Pattern = catena_equations:bind(result, Inner),
    ?assertMatch({bind, result, {op, inc, 1, {wildcard}}}, Pattern).

%%%=============================================================================
%%% Pattern Matching Tests
%%%=============================================================================

match_var_test() ->
    Pattern = catena_equations:var(x),
    ?assertEqual({ok, #{x => 42}}, catena_equations:match_pattern(Pattern, 42)),
    ?assertEqual({ok, #{x => "hello"}}, catena_equations:match_pattern(Pattern, "hello")).

match_lit_success_test() ->
    Pattern = catena_equations:lit(42),
    ?assertEqual({ok, #{}}, catena_equations:match_pattern(Pattern, 42)).

match_lit_failure_test() ->
    Pattern = catena_equations:lit(42),
    ?assertEqual({error, nomatch}, catena_equations:match_pattern(Pattern, 41)).

match_wildcard_test() ->
    Pattern = catena_equations:wildcard(),
    ?assertEqual({ok, #{}}, catena_equations:match_pattern(Pattern, anything)).

match_op_success_test() ->
    ArgPattern = catena_equations:var(x),
    Pattern = catena_equations:op(inc, 1, ArgPattern),
    Value = {op, inc, 1, 5},
    ?assertEqual({ok, #{x => 5}}, catena_equations:match_pattern(Pattern, Value)).

match_op_failure_test() ->
    ArgPattern = catena_equations:var(x),
    Pattern = catena_equations:op(inc, 1, ArgPattern),
    Value = {op, dec, 1, 5},
    ?assertEqual({error, nomatch}, catena_equations:match_pattern(Pattern, Value)).

match_seq_success_test() ->
    Patterns = [catena_equations:var(x), catena_equations:var(y), catena_equations:var(z)],
    Pattern = catena_equations:seq(Patterns),
    Values = [1, 2, 3],
    ?assertEqual({ok, #{x => 1, y => 2, z => 3}}, catena_equations:match_pattern(Pattern, Values)).

match_seq_length_mismatch_test() ->
    Patterns = [catena_equations:var(x), catena_equations:var(y)],
    Pattern = catena_equations:seq(Patterns),
    Values = [1],
    ?assertEqual({error, nomatch}, catena_equations:match_pattern(Pattern, Values)).

match_bind_test() ->
    Inner = catena_equations:op(inc, 1, catena_equations:wildcard()),
    Pattern = catena_equations:bind(result, Inner),
    Value = {op, inc, 1, 5},
    ?assertEqual({ok, #{result => {op, inc, 1, 5}}}, catena_equations:match_pattern(Pattern, Value)).

match_with_existing_subst_test() ->
    Pattern = catena_equations:var(x),
    Subst = #{y => 10},
    ?assertEqual({ok, #{y => 10, x => 42}}, catena_equations:match_pattern(Pattern, 42, Subst)).

match_consistent_var_test() ->
    Pattern1 = catena_equations:var(x),
    Pattern2 = catena_equations:var(x),
    {ok, Subst1} = catena_equations:match_pattern(Pattern1, 42),
    ?assertEqual({ok, Subst1}, catena_equations:match_pattern(Pattern2, 42, Subst1)).

match_inconsistent_var_test() ->
    Pattern1 = catena_equations:var(x),
    Pattern2 = catena_equations:var(x),
    {ok, Subst1} = catena_equations:match_pattern(Pattern1, 42),
    ?assertEqual({error, nomatch}, catena_equations:match_pattern(Pattern2, 41, Subst1)).

%%%=============================================================================
%%% Pattern Unification Tests
%%%=============================================================================

unify_same_var_test() ->
    P1 = catena_equations:var(x),
    P2 = catena_equations:var(x),
    ?assertEqual({ok, #{}}, catena_equations:unify_patterns(P1, P2)).

unify_var_to_pattern_test() ->
    P1 = catena_equations:var(x),
    P2 = catena_equations:lit(42),
    ?assertEqual({ok, #{x => {lit, 42}}}, catena_equations:unify_patterns(P1, P2)).

unify_lit_same_test() ->
    P1 = catena_equations:lit(42),
    P2 = catena_equations:lit(42),
    ?assertEqual({ok, #{}}, catena_equations:unify_patterns(P1, P2)).

unify_lit_different_test() ->
    P1 = catena_equations:lit(42),
    P2 = catena_equations:lit(41),
    ?assertEqual({error, nomatch}, catena_equations:unify_patterns(P1, P2)).

unify_op_success_test() ->
    P1 = catena_equations:op(inc, 1, catena_equations:var(x)),
    P2 = catena_equations:op(inc, 1, catena_equations:lit(5)),
    ?assertMatch({ok, #{x := {lit, 5}}}, catena_equations:unify_patterns(P1, P2)).

unify_op_failure_test() ->
    P1 = catena_equations:op(inc, 1, catena_equations:wildcard()),
    P2 = catena_equations:op(dec, 1, catena_equations:wildcard()),
    ?assertEqual({error, nomatch}, catena_equations:unify_patterns(P1, P2)).

%%%=============================================================================
%%% Variable Extraction Tests
%%%=============================================================================

extract_vars_var_test() ->
    Pattern = catena_equations:var(x),
    ?assertEqual([x], catena_equations:extract_variables(Pattern)).

extract_vars_nested_test() ->
    Pattern = catena_equations:op(inc, 1, catena_equations:var(x)),
    ?assertEqual([x], catena_equations:extract_variables(Pattern)).

extract_vars_seq_test() ->
    Patterns = [catena_equations:var(x), catena_equations:var(y), catena_equations:var(z)],
    Pattern = catena_equations:seq(Patterns),
    Vars = lists:sort(catena_equations:extract_variables(Pattern)),
    ?assertEqual([x, y, z], Vars).

extract_vars_duplicate_test() ->
    Pattern = catena_equations:seq([
        catena_equations:var(x),
        catena_equations:var(y),
        catena_equations:var(x)
    ]),
    Vars = lists:sort(catena_equations:extract_variables(Pattern)),
    ?assertEqual([x, y], Vars).

extract_vars_bind_test() ->
    Inner = catena_equations:op(inc, 1, catena_equations:var(x)),
    Pattern = catena_equations:bind(result, Inner),
    Vars = lists:sort(catena_equations:extract_variables(Pattern)),
    ?assertEqual([result, x], Vars).

%%%=============================================================================
%%% Variable Substitution Tests
%%%=============================================================================

substitute_var_test() ->
    Pattern = catena_equations:var(x),
    Subst = #{x => 42},
    ?assertMatch({lit, 42}, catena_equations:substitute_variables(Pattern, Subst)).

substitute_nested_test() ->
    Pattern = catena_equations:op(inc, 1, catena_equations:var(x)),
    Subst = #{x => 5},
    ?assertMatch({op, inc, 1, {lit, 5}}, catena_equations:substitute_variables(Pattern, Subst)).

substitute_seq_test() ->
    Patterns = [catena_equations:var(x), catena_equations:var(y)],
    Pattern = catena_equations:seq(Patterns),
    Subst = #{x => 1, y => 2},
    ?assertMatch({seq, [{lit, 1}, {lit, 2}]}, catena_equations:substitute_variables(Pattern, Subst)).

substitute_unbound_var_test() ->
    Pattern = catena_equations:var(z),
    Subst = #{x => 1, y => 2},
    ?assertMatch({var, z}, catena_equations:substitute_variables(Pattern, Subst)).

%%%=============================================================================
%%% Guard Evaluation Tests
%%%=============================================================================

guard_undefined_test() ->
    ?assert(catena_equations:evaluate_guard(undefined, #{})).

guard_is_type_test() ->
    Guard = {is_type, integer},
    ?assert(catena_equations:evaluate_guard(Guard, #{})).

guard_compare_test() ->
    Guard = {compare, '>', [5, 3]},
    ?assert(catena_equations:evaluate_guard(Guard, #{})).

guard_andalso_test() ->
    Guard = {logic_op, 'andalso', [
        {is_type, integer},
        {compare, '>', [x, 0]}
    ]},
    ?assert(catena_equations:evaluate_guard(Guard, #{})).

guard_orelse_test() ->
    Guard = {logic_op, 'orelse', [
        {compare, '>', [x, 100]},
        {compare, '<<', [x, 10]}
    ]},
    ?assert(catena_equations:evaluate_guard(Guard, #{})).

guard_vars_test() ->
    Guard = {logic_op, 'andalso', [
        {compare, '>', [x, 0]},
        {compare, '=:=', [y, 100]}
    ]},
    Vars = lists:sort(catena_equations:guard_vars(Guard)),
    ?assertEqual([x, y], Vars).

guard_combine_guards_test() ->
    G1 = {is_type, integer},
    G2 = {compare, '>', [x, 0]},
    Combined = catena_equations:combine_guards(G1, G2),
    ?assertMatch({logic_op, 'andalso', [G1, G2]}, Combined).

combine_guards_with_undefined_test() ->
    G1 = {is_type, integer},
    Combined = catena_equations:combine_guards(undefined, G1),
    ?assertEqual(G1, Combined).

guard_compare_with_substitution_test() ->
    Guard = {compare, '>', [x, 3]},
    ?assert(catena_equations:evaluate_guard(Guard, #{x => 5})),
    ?assertNot(catena_equations:evaluate_guard(Guard, #{x => 2})).

guard_is_type_for_named_variable_test() ->
    Guard = {is_type, {value, integer}},
    ?assert(catena_equations:evaluate_guard(Guard, #{value => 5})),
    ?assertNot(catena_equations:evaluate_guard(Guard, #{value => <<"nope">>})).

substitute_pattern_value_test() ->
    Pattern = catena_equations:var(x),
    Subst = #{x => {var, y}},
    ?assertMatch({var, y}, catena_equations:substitute_variables(Pattern, Subst)).

match_bind_respects_existing_binding_test() ->
    Pattern = catena_equations:bind(result, catena_equations:wildcard()),
    ?assertEqual(
        {error, nomatch},
        catena_equations:match_pattern(Pattern, 5, #{result => 3})
    ).

%%%=============================================================================
%%% Equation String Conversion Tests
%%%=============================================================================

equation_to_string_simple_test() ->
    LHS = catena_equations:var(x),
    RHS = catena_equations:lit(42),
    Eq = catena_equations:new(LHS, RHS),
    String = catena_equations:equation_to_string(Eq),
    ?assertMatch("x ≡ 42" ++ _, String).

equation_to_string_with_condition_test() ->
    LHS = catena_equations:var(x),
    RHS = catena_equations:lit(42),
    Condition = {is_type, integer},
    Eq = catena_equations:new(LHS, RHS, Condition),
    String = catena_equations:equation_to_string(Eq),
    ?assert(is_list(String)),
    ?assert(string:str(String, "when") > 0).

%%%=============================================================================
%%% Complex Pattern Tests
%%%=============================================================================

complex_nested_op_test() ->
    % Use literal values instead of functions for comparison
    Inner = catena_equations:op(inc, 1, catena_equations:var(y)),
    Pattern = catena_equations:op(context, state, Inner),
    Value = {op, context, state, {op, inc, 1, 5}},
    ?assertEqual({ok, #{y => 5}}, catena_equations:match_pattern(Pattern, Value)).

complex_bind_chain_test() ->
    % Test that binding works correctly
    P1 = catena_equations:bind(x, catena_equations:op(get, s, catena_equations:wildcard())),
    Pattern = catena_equations:seq([P1]),
    Value1 = {op, get, s, 5},
    ?assertEqual({ok, #{x => Value1}}, catena_equations:match_pattern(Pattern, [Value1])).

complex_multi_var_unification_test() ->
    % Different lengths should fail
    P1 = catena_equations:seq([catena_equations:var(x)]),
    P2 = catena_equations:seq([catena_equations:lit(1), catena_equations:lit(2)]),
    ?assertEqual({error, nomatch}, catena_equations:unify_patterns(P1, P2)),

    % Same length should work
    P3 = catena_equations:seq([catena_equations:var(x), catena_equations:var(y)]),
    P4 = catena_equations:seq([catena_equations:lit(1), catena_equations:lit(2)]),
    ?assertMatch({ok, #{x := {lit, 1}, y := {lit, 2}}}, catena_equations:unify_patterns(P3, P4)).
