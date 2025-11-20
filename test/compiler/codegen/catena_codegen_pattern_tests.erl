%%%-------------------------------------------------------------------
%%% @doc Tests for Pattern Compilation (Task 1.3.2)
%%%
%%% Tests compilation of Catena patterns to Core Erlang.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_codegen_pattern_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

new_state() ->
    catena_codegen_utils:new_state().

loc() ->
    {location, 1, 1}.

%%====================================================================
%% Basic Pattern Compilation Tests (1.3.2.1)
%%====================================================================

basic_pattern_test_() ->
    [
        ?_test(test_compile_variable_pattern()),
        ?_test(test_compile_wildcard_pattern()),
        ?_test(test_compile_integer_literal()),
        ?_test(test_compile_float_literal()),
        ?_test(test_compile_string_literal()),
        ?_test(test_compile_atom_literal()),
        ?_test(test_compile_bool_literal())
    ].

test_compile_variable_pattern() ->
    State = new_state(),
    Pattern = {pat_var, x, loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assertEqual(x, cerl:var_name(Core)).

test_compile_wildcard_pattern() ->
    State = new_state(),
    Pattern = {pat_wildcard, loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assertEqual('_', cerl:var_name(Core)).

test_compile_integer_literal() ->
    State = new_state(),
    Pattern = {pat_literal, 42, integer, loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assertEqual(42, cerl:int_val(Core)).

test_compile_float_literal() ->
    State = new_state(),
    Pattern = {pat_literal, 3.14, float, loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assertEqual(3.14, cerl:float_val(Core)).

test_compile_string_literal() ->
    State = new_state(),
    Pattern = {pat_literal, "hello", string, loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assertEqual("hello", cerl:string_val(Core)).

test_compile_atom_literal() ->
    State = new_state(),
    Pattern = {pat_literal, ok, atom, loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assertEqual(ok, cerl:atom_val(Core)).

test_compile_bool_literal() ->
    State = new_state(),
    TruePattern = {pat_literal, true, bool, loc()},
    FalsePattern = {pat_literal, false, bool, loc()},
    {CoreTrue, _} = catena_codegen_pattern:compile_pattern(TruePattern, State),
    {CoreFalse, _} = catena_codegen_pattern:compile_pattern(FalsePattern, State),
    ?assertEqual(true, cerl:atom_val(CoreTrue)),
    ?assertEqual(false, cerl:atom_val(CoreFalse)).

%%====================================================================
%% Constructor Pattern Tests
%%====================================================================

constructor_pattern_test_() ->
    [
        ?_test(test_compile_nullary_constructor()),
        ?_test(test_compile_unary_constructor()),
        ?_test(test_compile_binary_constructor()),
        ?_test(test_compile_nested_constructor())
    ].

test_compile_nullary_constructor() ->
    State = new_state(),
    Pattern = {pat_constructor, 'None', [], loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assert(cerl:is_c_tuple(Core)),
    [Tag] = cerl:tuple_es(Core),
    ?assertEqual('None', cerl:atom_val(Tag)).

test_compile_unary_constructor() ->
    State = new_state(),
    Pattern = {pat_constructor, 'Some', [{pat_var, x, loc()}], loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assert(cerl:is_c_tuple(Core)),
    [Tag, Arg] = cerl:tuple_es(Core),
    ?assertEqual('Some', cerl:atom_val(Tag)),
    ?assertEqual(x, cerl:var_name(Arg)).

test_compile_binary_constructor() ->
    State = new_state(),
    Pattern = {pat_constructor, 'Point', [
        {pat_var, x, loc()},
        {pat_var, y, loc()}
    ], loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assert(cerl:is_c_tuple(Core)),
    [Tag, X, Y] = cerl:tuple_es(Core),
    ?assertEqual('Point', cerl:atom_val(Tag)),
    ?assertEqual(x, cerl:var_name(X)),
    ?assertEqual(y, cerl:var_name(Y)).

test_compile_nested_constructor() ->
    State = new_state(),
    %% Some(Some(x))
    Pattern = {pat_constructor, 'Some', [
        {pat_constructor, 'Some', [{pat_var, x, loc()}], loc()}
    ], loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assert(cerl:is_c_tuple(Core)),
    [Tag, Inner] = cerl:tuple_es(Core),
    ?assertEqual('Some', cerl:atom_val(Tag)),
    ?assert(cerl:is_c_tuple(Inner)).

%%====================================================================
%% List Pattern Tests
%%====================================================================

list_pattern_test_() ->
    [
        ?_test(test_compile_empty_list()),
        ?_test(test_compile_single_element_list()),
        ?_test(test_compile_multi_element_list()),
        ?_test(test_compile_cons_pattern())
    ].

test_compile_empty_list() ->
    State = new_state(),
    Pattern = {pat_list, [], loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assert(cerl:is_c_nil(Core)).

test_compile_single_element_list() ->
    State = new_state(),
    Pattern = {pat_list, [{pat_var, x, loc()}], loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assert(cerl:is_c_list(Core)).

test_compile_multi_element_list() ->
    State = new_state(),
    Pattern = {pat_list, [
        {pat_var, x, loc()},
        {pat_var, y, loc()},
        {pat_var, z, loc()}
    ], loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assert(cerl:is_c_list(Core)).

test_compile_cons_pattern() ->
    State = new_state(),
    %% [h | t]
    Pattern = {pat_cons, {pat_var, h, loc()}, {pat_var, t, loc()}, loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assert(cerl:is_c_cons(Core)),
    Head = cerl:cons_hd(Core),
    Tail = cerl:cons_tl(Core),
    ?assertEqual(h, cerl:var_name(Head)),
    ?assertEqual(t, cerl:var_name(Tail)).

%%====================================================================
%% Tuple and As-Pattern Tests
%%====================================================================

tuple_as_pattern_test_() ->
    [
        ?_test(test_compile_tuple_pattern()),
        ?_test(test_compile_as_pattern())
    ].

test_compile_tuple_pattern() ->
    State = new_state(),
    Pattern = {pat_tuple, [
        {pat_var, x, loc()},
        {pat_var, y, loc()}
    ], loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assert(cerl:is_c_tuple(Core)),
    ?assertEqual(2, cerl:tuple_arity(Core)).

test_compile_as_pattern() ->
    State = new_state(),
    %% x as Some(y)
    Pattern = {pat_as, x, {pat_constructor, 'Some', [{pat_var, y, loc()}], loc()}, loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assertEqual(alias, cerl:type(Core)).

%%====================================================================
%% Guard Compilation Tests (1.3.2.2)
%%====================================================================

guard_compilation_test_() ->
    [
        ?_test(test_compile_empty_guard()),
        ?_test(test_compile_simple_guard()),
        ?_test(test_compile_comparison_guard()),
        ?_test(test_compile_multiple_guards())
    ].

test_compile_empty_guard() ->
    State = new_state(),
    {Core, _State1} = catena_codegen_pattern:compile_guard([], State),
    ?assertEqual(true, cerl:atom_val(Core)).

test_compile_simple_guard() ->
    State = new_state(),
    %% Guard: x > 0
    Guard = {binary_op, '>', {var, x, loc()}, {literal, integer, 0, loc()}, loc()},
    {Core, _State1} = catena_codegen_pattern:compile_guard(Guard, State),
    ?assertEqual(call, cerl:type(Core)).

test_compile_comparison_guard() ->
    State = new_state(),
    %% Guard: x === y
    Guard = {binary_op, '===', {var, x, loc()}, {var, y, loc()}, loc()},
    {Core, _State1} = catena_codegen_pattern:compile_guard(Guard, State),
    ?assertEqual(call, cerl:type(Core)).

test_compile_multiple_guards() ->
    State = new_state(),
    %% Guards: x > 0, y < 10
    Guards = [
        {binary_op, '>', {var, x, loc()}, {literal, integer, 0, loc()}, loc()},
        {binary_op, '<', {var, y, loc()}, {literal, integer, 10, loc()}, loc()}
    ],
    {Core, _State1} = catena_codegen_pattern:compile_guard(Guards, State),
    %% Should be combined with 'andalso'
    ?assertEqual(call, cerl:type(Core)).

%%====================================================================
%% Clause Compilation Tests
%%====================================================================

clause_compilation_test_() ->
    [
        ?_test(test_compile_simple_clause()),
        ?_test(test_compile_clause_with_guard()),
        ?_test(test_compile_multiple_clauses())
    ].

test_compile_simple_clause() ->
    State = new_state(),
    %% {[x], x}
    Clause = {[{pat_var, x, loc()}], {var, x, loc()}},
    {Core, _State1} = catena_codegen_pattern:compile_clauses([Clause], State, #{}),
    ?assertEqual(1, length(Core)),
    [C] = Core,
    ?assertEqual(clause, cerl:type(C)).

test_compile_clause_with_guard() ->
    State = new_state(),
    %% {clause, [x], [x > 0], x}
    Clause = {clause, [{pat_var, x, loc()}], [
        {binary_op, '>', {var, x, loc()}, {literal, integer, 0, loc()}, loc()}
    ], {var, x, loc()}},
    {Core, _State1} = catena_codegen_pattern:compile_clauses([Clause], State, #{}),
    ?assertEqual(1, length(Core)),
    [C] = Core,
    ?assertEqual(clause, cerl:type(C)).

test_compile_multiple_clauses() ->
    State = new_state(),
    Clauses = [
        {[{pat_literal, 0, integer, loc()}], {literal, atom, zero, loc()}},
        {[{pat_var, n, loc()}], {var, n, loc()}}
    ],
    {Core, _State1} = catena_codegen_pattern:compile_clauses(Clauses, State, #{}),
    ?assertEqual(2, length(Core)).

%%====================================================================
%% Match Expression Tests
%%====================================================================

match_expression_test_() ->
    [
        ?_test(test_compile_simple_match()),
        ?_test(test_compile_constructor_match())
    ].

test_compile_simple_match() ->
    State = new_state(),
    Scrutinee = cerl:c_var(x),
    Clauses = [
        {[{pat_literal, 0, integer, loc()}], {literal, atom, zero, loc()}},
        {[{pat_wildcard, loc()}], {literal, atom, other, loc()}}
    ],
    {Core, _State1} = catena_codegen_pattern:compile_match(Scrutinee, Clauses, State, #{}),
    ?assertEqual('case', cerl:type(Core)).

test_compile_constructor_match() ->
    State = new_state(),
    Scrutinee = cerl:c_var(maybe_val),
    Clauses = [
        {[{pat_constructor, 'None', [], loc()}], {literal, integer, 0, loc()}},
        {[{pat_constructor, 'Some', [{pat_var, x, loc()}], loc()}], {var, x, loc()}}
    ],
    {Core, _State1} = catena_codegen_pattern:compile_match(Scrutinee, Clauses, State, #{}),
    ?assertEqual('case', cerl:type(Core)),
    ?assertEqual(2, length(cerl:case_clauses(Core))).

%%====================================================================
%% Exhaustiveness Checking Tests (1.3.2.4)
%%====================================================================

exhaustiveness_test_() ->
    [
        ?_test(test_exhaustive_bool_patterns()),
        ?_test(test_non_exhaustive_bool_patterns()),
        ?_test(test_wildcard_makes_exhaustive()),
        ?_test(test_exhaustive_list_patterns()),
        ?_test(test_non_exhaustive_list_patterns())
    ].

test_exhaustive_bool_patterns() ->
    Patterns = [
        {pat_literal, true, bool, loc()},
        {pat_literal, false, bool, loc()}
    ],
    Result = catena_codegen_pattern:check_exhaustiveness(Patterns, {bool}),
    ?assertEqual(ok, Result).

test_non_exhaustive_bool_patterns() ->
    Patterns = [
        {pat_literal, true, bool, loc()}
    ],
    Result = catena_codegen_pattern:check_exhaustiveness(Patterns, {bool}),
    ?assertMatch({warning, [{missing_literal, false}]}, Result).

test_wildcard_makes_exhaustive() ->
    Patterns = [
        {pat_wildcard, loc()}
    ],
    Result = catena_codegen_pattern:check_exhaustiveness(Patterns, {bool}),
    ?assertEqual(ok, Result).

test_exhaustive_list_patterns() ->
    Patterns = [
        {pat_list, [], loc()},
        {pat_cons, {pat_var, h, loc()}, {pat_var, t, loc()}, loc()}
    ],
    Result = catena_codegen_pattern:check_exhaustiveness(Patterns, {list, unknown}),
    ?assertEqual(ok, Result).

test_non_exhaustive_list_patterns() ->
    Patterns = [
        {pat_list, [], loc()}
    ],
    Result = catena_codegen_pattern:check_exhaustiveness(Patterns, {list, unknown}),
    ?assertMatch({warning, [{missing_pattern, non_empty_list}]}, Result).

%%====================================================================
%% Decision Tree Tests (1.3.2.3)
%%====================================================================

decision_tree_test_() ->
    [
        ?_test(test_generate_simple_tree()),
        ?_test(test_analyze_patterns())
    ].

test_generate_simple_tree() ->
    State = new_state(),
    Clauses = [
        {clause, [{pat_literal, 0, integer, loc()}], [], {literal, atom, zero, loc()}}
    ],
    {Tree, _State1} = catena_codegen_pattern:generate_decision_tree(Clauses, State),
    ?assertMatch({leaf, _}, Tree).

test_analyze_patterns() ->
    Clauses = [
        {clause, [{pat_var, x, loc()}, {pat_var, y, loc()}], [], {var, x, loc()}}
    ],
    {Tree, _} = catena_codegen_pattern:generate_decision_tree(Clauses, new_state()),
    ?assertMatch({leaf, _}, Tree).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
        ?_test(test_full_match_compilation()),
        ?_test(test_complex_pattern_compilation())
    ].

test_full_match_compilation() ->
    State = new_state(),
    %% Match on Maybe type
    Scrutinee = cerl:c_var(input),
    Clauses = [
        {clause,
            [{pat_constructor, 'None', [], loc()}],
            [],
            {literal, integer, 0, loc()}},
        {clause,
            [{pat_constructor, 'Some', [{pat_var, x, loc()}], loc()}],
            [{binary_op, '>', {var, x, loc()}, {literal, integer, 0, loc()}, loc()}],
            {var, x, loc()}},
        {clause,
            [{pat_wildcard, loc()}],
            [],
            {literal, integer, -1, loc()}}
    ],
    {Core, _State1} = catena_codegen_pattern:compile_match(Scrutinee, Clauses, State, #{}),
    ?assertEqual('case', cerl:type(Core)),
    ?assertEqual(3, length(cerl:case_clauses(Core))).

test_complex_pattern_compilation() ->
    State = new_state(),
    %% Nested pattern: Pair(Some(x), None)
    Pattern = {pat_constructor, 'Pair', [
        {pat_constructor, 'Some', [{pat_var, x, loc()}], loc()},
        {pat_constructor, 'None', [], loc()}
    ], loc()},
    {Core, _State1} = catena_codegen_pattern:compile_pattern(Pattern, State),
    ?assert(cerl:is_c_tuple(Core)),
    [Tag, First, Second] = cerl:tuple_es(Core),
    ?assertEqual('Pair', cerl:atom_val(Tag)),
    ?assert(cerl:is_c_tuple(First)),
    ?assert(cerl:is_c_tuple(Second)).
