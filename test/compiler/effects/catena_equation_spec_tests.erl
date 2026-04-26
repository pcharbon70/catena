%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_equation_spec (Phase 8.2)
%%%
%%% Tests for equation specification system:
%%% - Equation set creation and management
%%% - Equation addition, removal, and lookup
%%% - Equation validation
%%% - Equation query and search
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_equation_spec_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Setup and Teardown
%%%=============================================================================

setup_test_set() ->
    Eq1 = catena_equations:new(
        catena_equations:var(x),
        catena_equations:lit(42)
    ),
    Eq2 = catena_equations:new(
        catena_equations:op(inc, 1, catena_equations:var(x)),
        catena_equations:var(x)
    ),
    Eq3 = catena_equations:new(
        catena_equations:seq([catena_equations:var(x), catena_equations:var(y)]),
        catena_equations:seq([catena_equations:var(y), catena_equations:var(x)])
    ),
    Set0 = catena_equation_spec:new_set(test_effects),
    Set1 = catena_equation_spec:add_equations(Set0, [
        {identity, Eq1},
        {increment, Eq2}
    ]),
    catena_equation_spec:add_equation(Set1, swap, Eq3).

%%%=============================================================================
%%% Equation Set Creation Tests
%%%=============================================================================

new_set_creates_empty_set_test() ->
    Set = catena_equation_spec:new_set(my_effects),
    ?assertEqual(my_effects, maps:get(name, Set)),
    ?assertEqual(#{}, maps:get(equations, Set)),
    ?assertEqual(#{}, maps:get(operations, Set)).

new_set_with_metadata_test() ->
    Metadata = #{description => "Test equations", version => 1},
    Set = catena_equation_spec:new_set(my_effects, Metadata),
    ?assertEqual(my_effects, maps:get(name, Set)),
    ?assertEqual(Metadata, maps:get(metadata, Set)).

%%%=============================================================================
%%% Equation Addition Tests
%%%=============================================================================

add_equation_test() ->
    Set = catena_equation_spec:new_set(test_set),
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:lit(42)
    ),
    NewSet = catena_equation_spec:add_equation(Set, test_eq, Eq),

    ?assertEqual({ok, Eq}, catena_equation_spec:get_equation(NewSet, test_eq)),
    ?assert(lists:member(test_eq, catena_equation_spec:list_equations(NewSet))).

add_equation_indexes_operations_test() ->
    Set = catena_equation_spec:new_set(test_set),
    Eq = catena_equations:new(
        catena_equations:op(inc, 1, catena_equations:var(x)),
        catena_equations:var(x)
    ),
    NewSet = catena_equation_spec:add_equation(Set, inc_eq, Eq),

    % Should be able to lookup by operation
    Eqs = catena_equation_spec:lookup_equations(NewSet, inc),
    ?assertEqual(1, length(Eqs)),
    ?assertEqual(Eq, hd(Eqs)).

add_operation_equation_tracks_explicit_operation_test() ->
    Set = catena_equation_spec:new_set(test_set),
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:var(y)
    ),
    NewSet = catena_equation_spec:add_operation_equation(Set, send, send_identity, Eq),

    Eqs = catena_equation_spec:lookup_equations(NewSet, send),
    ?assertEqual([Eq], Eqs).

add_handler_equation_tracks_handler_index_test() ->
    Set = catena_equation_spec:new_set(test_set),
    Eq = catena_equations:new(
        catena_equations:op(get, 0, catena_equations:wildcard()),
        catena_equations:var(state)
    ),
    NewSet = catena_equation_spec:add_handler_equation(Set, state_handler, get_rule, Eq),

    Eqs = catena_equation_spec:lookup_handler_equations(NewSet, state_handler),
    ?assertEqual([Eq], Eqs).

add_multiple_equations_test() ->
    Set = catena_equation_spec:new_set(test_set),
    Eq1 = catena_equations:new(catena_equations:var(x), catena_equations:lit(1)),
    Eq2 = catena_equations:new(catena_equations:var(y), catena_equations:lit(2)),
    Eq3 = catena_equations:new(catena_equations:var(z), catena_equations:lit(3)),

    NewSet = catena_equation_spec:add_equations(Set, [
        {eq1, Eq1}, {eq2, Eq2}, {eq3, Eq3}
    ]),

    ?assertEqual(3, length(catena_equation_spec:list_equations(NewSet))),
    ?assertEqual({ok, Eq1}, catena_equation_spec:get_equation(NewSet, eq1)),
    ?assertEqual({ok, Eq2}, catena_equation_spec:get_equation(NewSet, eq2)),
    ?assertEqual({ok, Eq3}, catena_equation_spec:get_equation(NewSet, eq3)).

add_equation_same_operation_multiple_times_test() ->
    Set = catena_equation_spec:new_set(test_set),
    Eq1 = catena_equations:new(
        catena_equations:op(inc, 1, catena_equations:var(x)),
        catena_equations:var(x)
    ),
    Eq2 = catena_equations:new(
        catena_equations:op(inc, 1, catena_equations:lit(0)),
        catena_equations:var(x)
    ),

    NewSet = catena_equation_spec:add_equations(Set, [
        {inc_eq1, Eq1},
        {inc_eq2, Eq2}
    ]),

    % Both equations should be indexed under 'inc'
    Eqs = catena_equation_spec:lookup_equations(NewSet, inc),
    ?assertEqual(2, length(Eqs)).

%%%=============================================================================
%%% Equation Removal Tests
%%%=============================================================================

remove_equation_test() ->
    Set = setup_test_set(),
    NewSet = catena_equation_spec:remove_equation(Set, identity),

    ?assertEqual(error, catena_equation_spec:get_equation(NewSet, identity)),
    ?assertNot(lists:member(identity, catena_equation_spec:list_equations(NewSet))).

remove_equation_updates_operation_index_test() ->
    Set = setup_test_set(),
    ?assertEqual(1, length(catena_equation_spec:lookup_equations(Set, inc))),

    NewSet = catena_equation_spec:remove_equation(Set, increment),
    ?assertEqual(0, length(catena_equation_spec:lookup_equations(NewSet, inc))).

remove_nonexistent_equation_test() ->
    Set = setup_test_set(),
    OriginalEquations = catena_equation_spec:list_equations(Set),
    NewSet = catena_equation_spec:remove_equation(Set, nonexistent),

    ?assertEqual(OriginalEquations, catena_equation_spec:list_equations(NewSet)).

%%%=============================================================================
%%% Equation Lookup Tests
%%%=============================================================================

get_equation_found_test() ->
    Set = setup_test_set(),
    {ok, Eq} = catena_equation_spec:get_equation(Set, identity),
    ?assert(catena_equations:is_equation(Eq)).

get_equation_not_found_test() ->
    Set = setup_test_set(),
    ?assertEqual(error, catena_equation_spec:get_equation(Set, nonexistent)).

list_equations_test() ->
    Set = setup_test_set(),
    Names = catena_equation_spec:list_equations(Set),
    ?assertEqual(3, length(Names)),
    ?assert(lists:member(identity, Names)),
    ?assert(lists:member(increment, Names)),
    ?assert(lists:member(swap, Names)).

lookup_equations_by_operation_test() ->
    Set = setup_test_set(),
    IncEqs = catena_equation_spec:lookup_equations(Set, inc),
    ?assertEqual(1, length(IncEqs)),
    ?assertMatch({op, inc, 1, _}, catena_equations:lhs(hd(IncEqs))).

lookup_equations_nonexistent_operation_test() ->
    Set = setup_test_set(),
    Eqs = catena_equation_spec:lookup_equations(Set, nonexistent_op),
    ?assertEqual(0, length(Eqs)).

%%%=============================================================================
%%% Equation Validation Tests
%%%=============================================================================

validate_simple_equation_test() ->
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:lit(42)
    ),
    ?assertEqual(ok, catena_equation_spec:validate_equation(Eq)).

validate_equation_with_unbound_rhs_variable_test() ->
    LHS = catena_equations:var(x),
    RHS = catena_equations:var(y),  % y not bound on LHS
    Eq = catena_equations:new(LHS, RHS),
    Result = catena_equation_spec:validate_equation(Eq),
    ?assertMatch({error, [#{type := unbound_variable}]}, Result).

validate_equation_with_guard_binding_test() ->
    % Guard binds the variable
    LHS = catena_equations:var(x),
    RHS = catena_equations:var(y),
    Condition = {compare, '>', [y, 0]},  % y appears in guard
    Eq = catena_equations:new(LHS, RHS, Condition),
    Result = catena_equation_spec:validate_equation(Eq),
    % y is bound by the guard, so this should be ok
    ?assertEqual(ok, Result).

validate_equation_with_well_formed_patterns_test() ->
    LHS = catena_equations:op(inc, 1, catena_equations:var(x)),
    RHS = catena_equations:var(x),
    Eq = catena_equations:new(LHS, RHS),
    ?assertEqual(ok, catena_equation_spec:validate_equation(Eq)).

validate_set_all_valid_test() ->
    Set = setup_test_set(),
    ?assertEqual(ok, catena_equation_spec:validate_set(Set)).

validate_equation_with_invalid_guard_test() ->
    LHS = catena_equations:var(x),
    RHS = catena_equations:var(x),
    % Use an invalid logic operator
    Condition = {logic_op, invalid_op, []},
    Eq = catena_equations:new(LHS, RHS, Condition),
    Result = catena_equation_spec:validate_equation(Eq),
    ?assertMatch({error, [#{type := invalid_guard}]}, Result).

validate_equation_rejects_invalid_nested_sequence_item_test() ->
    Eq = catena_equations:new(
        {seq, [catena_equations:var(x), invalid_pattern]},
        catena_equations:var(x)
    ),
    Result = catena_equation_spec:validate_equation(Eq),
    ?assertMatch({error, [#{type := ill_formed}]}, Result).

validate_equation_rejects_invalid_nested_guard_test() ->
    Eq = catena_equations:new(
        catena_equations:var(x),
        catena_equations:var(x),
        {logic_op, 'andalso', [{is_type, integer}, invalid_guard]}
    ),
    Result = catena_equation_spec:validate_equation(Eq),
    ?assertMatch({error, [#{type := invalid_guard}]}, Result).

validate_handler_equations_complete_coverage_test() ->
    Eq = catena_equations:new(
        catena_equations:op(get, 0, catena_equations:var(state)),
        catena_equations:var(state)
    ),
    Set = catena_equation_spec:add_handler_equation(
        catena_equation_spec:new_set(test),
        state_handler,
        get_rule,
        Eq
    ),
    ?assertEqual(
        ok,
        catena_equation_spec:validate_handler_equations(Set, state_handler, [get])
    ).

validate_handler_equations_reports_missing_coverage_test() ->
    Eq = catena_equations:new(
        catena_equations:op(get, 0, catena_equations:var(state)),
        catena_equations:var(state)
    ),
    Set = catena_equation_spec:add_handler_equation(
        catena_equation_spec:new_set(test),
        state_handler,
        get_rule,
        Eq
    ),
    Result = catena_equation_spec:validate_handler_equations(Set, state_handler, [get, put]),
    ?assertMatch(
        {error, [#{type := coverage, details := #{missing := [put]}}]},
        Result
    ).

find_conflicts_detects_same_lhs_with_different_rhs_test() ->
    Eq1 = catena_equations:new(
        catena_equations:op(get, 0, catena_equations:wildcard()),
        catena_equations:var(x)
    ),
    Eq2 = catena_equations:new(
        catena_equations:op(get, 0, catena_equations:wildcard()),
        catena_equations:lit(undefined)
    ),
    Set0 = catena_equation_spec:new_set(test),
    Set1 = catena_equation_spec:add_equation(Set0, eq1, Eq1),
    Set2 = catena_equation_spec:add_equation(Set1, eq2, Eq2),

    Conflicts = catena_equation_spec:find_conflicts(Set2),
    ?assertMatch([#{operation := get, equations := _}], Conflicts).

check_well_formed_valid_patterns_test() ->
    ?assertEqual(ok, catena_equation_spec:check_well_formed(catena_equations:var(x))),
    ?assertEqual(ok, catena_equation_spec:check_well_formed(catena_equations:lit(42))),
    ?assertEqual(ok, catena_equation_spec:check_well_formed(catena_equations:wildcard())).

check_well_formed_nested_pattern_test() ->
    Pattern = catena_equations:op(inc, 1, catena_equations:var(x)),
    ?assertEqual(ok, catena_equation_spec:check_well_formed(Pattern)).

check_well_formed_sequence_test() ->
    Pattern = catena_equations:seq([
        catena_equations:var(x),
        catena_equations:var(y)
    ]),
    ?assertEqual(ok, catena_equation_spec:check_well_formed(Pattern)).

check_well_formed_bind_test() ->
    Pattern = catena_equations:bind(x, catena_equations:op(inc, 1, catena_equations:wildcard())),
    ?assertEqual(ok, catena_equation_spec:check_well_formed(Pattern)).

%%%=============================================================================
%%% Equation Query Tests
%%%=============================================================================

find_matching_unification_test() ->
    Set = setup_test_set(),
    Pattern = catena_equations:var(x),
    Matches = catena_equation_spec:find_matching(Set, Pattern),

    % Should find the identity equation: x ≡ 42
    ?assertEqual(1, length(Matches)),
    {Name, _Eq} = hd(Matches),
    ?assertEqual(identity, Name).

find_matching_with_op_pattern_test() ->
    Set = setup_test_set(),
    Pattern = catena_equations:op(inc, 1, catena_equations:wildcard()),
    Matches = catena_equation_spec:find_matching(Set, Pattern),

    % Should find the increment equation
    ?assertEqual(1, length(Matches)),
    {increment, _Eq} = hd(Matches).

find_matching_no_match_test() ->
    Set = setup_test_set(),
    Pattern = catena_equations:op(dec, 1, catena_equations:wildcard()),
    Matches = catena_equation_spec:find_matching(Set, Pattern),
    ?assertEqual(0, length(Matches)).

find_by_operation_test() ->
    Set = setup_test_set(),
    Results = catena_equation_spec:find_by_operation(Set, inc),
    ?assertEqual(1, length(Results)),
    {increment, _Eq} = hd(Results).

find_by_operation_nonexistent_test() ->
    Set = setup_test_set(),
    Results = catena_equation_spec:find_by_operation(Set, nonexistent),
    ?assertEqual(0, length(Results)).

equations_for_pattern_test() ->
    Set = setup_test_set(),
    Pattern = catena_equations:op(inc, 1, catena_equations:var(x)),
    Eqs = catena_equation_spec:equations_for_pattern(Set, Pattern),
    ?assertEqual(1, length(Eqs)).

equations_for_pattern_no_ops_test() ->
    Set = setup_test_set(),
    Pattern = catena_equations:var(x),
    Eqs = catena_equation_spec:equations_for_pattern(Set, Pattern),
    % No operations in the pattern, so no equations found
    ?assertEqual(0, length(Eqs)).

%%%=============================================================================
%%% Complex Equation Tests
%%%=============================================================================

complex_equation_with_sequence_test() ->
    LHS = catena_equations:seq([
        catena_equations:op(get, s, catena_equations:wildcard()),
        catena_equations:op(put, s, catena_equations:var(x))
    ]),
    RHS = catena_equations:var(x),
    Eq = catena_equations:new(LHS, RHS),
    ?assertEqual(ok, catena_equation_spec:validate_equation(Eq)).

complex_equation_with_multiple_operations_test() ->
    Set = catena_equation_spec:new_set(complex),
    Eq1 = catena_equations:new(
        catena_equations:op(op1, 1, catena_equations:var(x)),
        catena_equations:var(x)
    ),
    Eq2 = catena_equations:new(
        catena_equations:op(op2, 2, catena_equations:var(y)),
        catena_equations:var(y)
    ),

    NewSet = catena_equation_spec:add_equations(Set, [
        {eq1, Eq1},
        {eq2, Eq2}
    ]),

    ?assertEqual(2, length(catena_equation_spec:list_equations(NewSet))),
    ?assertEqual(1, length(catena_equation_spec:lookup_equations(NewSet, op1))),
    ?assertEqual(1, length(catena_equation_spec:lookup_equations(NewSet, op2))).

complex_nested_pattern_validation_test() ->
    LHS = catena_equations:bind(result,
        catena_equations:op(compute, 1,
            catena_equations:seq([
                catena_equations:var(x),
                catena_equations:var(y)
            ])
        )
    ),
    RHS = catena_equations:var(result),
    Eq = catena_equations:new(LHS, RHS),
    ?assertEqual(ok, catena_equation_spec:validate_equation(Eq)).
