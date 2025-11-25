%%%-------------------------------------------------------------------
%%% @doc Tests for Decision Tree Pattern Compilation (Phase 3.2)
%%%
%%% Tests for the catena_pattern_decision_tree module which implements
%%% efficient pattern matching via decision trees.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_pattern_decision_tree_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

loc() ->
    {location, 1, 1}.

%% Helper to create a simple state for testing
mock_state() ->
    #{
        var_counter => 0,
        bindings => #{}
    }.

%%====================================================================
%% Pattern Matrix Construction Tests (3.2.1.1)
%%====================================================================

pattern_matrix_test_() ->
    [
        {"single pattern clause",
         ?_test(test_single_pattern_matrix())},
        {"multiple pattern clauses",
         ?_test(test_multiple_pattern_matrix())},
        {"clause with guard",
         ?_test(test_matrix_with_guards())}
    ].

test_single_pattern_matrix() ->
    Clause = {{pat_var, x, loc()}, {var, x, loc()}},
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses([Clause]),
    ?assertEqual(1, length(Matrix)).

test_multiple_pattern_matrix() ->
    Clauses = [
        {{pat_literal, 1, integer, loc()}, {literal, integer, 1, loc()}},
        {{pat_literal, 2, integer, loc()}, {literal, integer, 2, loc()}},
        {{pat_var, x, loc()}, {var, x, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    ?assertEqual(3, length(Matrix)).

test_matrix_with_guards() ->
    Clauses = [
        {clause, [{pat_var, x, loc()}], [{binary_op, '>', {var, x, loc()}, {literal, integer, 0, loc()}, loc()}], {var, x, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    ?assertEqual(1, length(Matrix)).

%%====================================================================
%% Decision Tree Construction Tests (3.2.1.2)
%%====================================================================

tree_construction_test_() ->
    [
        {"empty matrix produces failure",
         ?_test(test_empty_matrix_failure())},
        {"single wildcard produces leaf",
         ?_test(test_single_wildcard_leaf())},
        {"literal patterns produce switch",
         ?_test(test_literal_switch())},
        {"constructor patterns produce switch",
         ?_test(test_constructor_switch())},
        {"nested patterns work",
         ?_test(test_nested_patterns())}
    ].

test_empty_matrix_failure() ->
    Tree = catena_pattern_decision_tree:build_tree([], #{}),
    ?assertMatch({fail, _}, Tree).

test_single_wildcard_leaf() ->
    Clauses = [
        {{pat_var, x, loc()}, {var, x, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    ?assertMatch({leaf, 0, [], _}, Tree).

test_literal_switch() ->
    Clauses = [
        {{pat_literal, 1, integer, loc()}, {literal, integer, 1, loc()}},
        {{pat_literal, 2, integer, loc()}, {literal, integer, 2, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    ?assertMatch({switch, _, _, _}, Tree).

test_constructor_switch() ->
    Clauses = [
        {{pat_constructor, 'None', [], loc()}, {literal, atom, none, loc()}},
        {{pat_constructor, 'Some', [{pat_var, x, loc()}], loc()}, {var, x, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    ?assertMatch({switch, _, _, _}, Tree).

test_nested_patterns() ->
    %% Match (Some (Some x)) vs (Some None) vs None
    Clauses = [
        {{pat_constructor, 'Some', [{pat_constructor, 'Some', [{pat_var, x, loc()}], loc()}], loc()},
         {var, x, loc()}},
        {{pat_constructor, 'Some', [{pat_constructor, 'None', [], loc()}], loc()},
         {literal, atom, inner_none, loc()}},
        {{pat_constructor, 'None', [], loc()},
         {literal, atom, none, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    ?assertMatch({switch, _, _, _}, Tree).

%%====================================================================
%% Column Selection Heuristics Tests (3.2.3)
%%====================================================================

column_selection_test_() ->
    [
        {"single column returns 0",
         ?_test(test_single_column_selection())},
        {"selects non-wildcard column",
         ?_test(test_non_wildcard_column())},
        {"cost calculation",
         ?_test(test_cost_calculation())},
        {"benefit calculation",
         ?_test(test_benefit_calculation())}
    ].

test_single_column_selection() ->
    Clauses = [
        {{pat_var, x, loc()}, {var, x, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Col = catena_pattern_decision_tree:select_column(Matrix, #{}),
    ?assertEqual(0, Col).

test_non_wildcard_column() ->
    %% Two columns: first is wildcard, second is literal
    Clauses = [
        {[{pat_var, x, loc()}, {pat_literal, 1, integer, loc()}], {var, x, loc()}},
        {[{pat_var, y, loc()}, {pat_literal, 2, integer, loc()}], {var, y, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Col = catena_pattern_decision_tree:select_column(Matrix, #{}),
    %% Should select column 1 (literals) over column 0 (wildcards)
    ?assertEqual(1, Col).

test_cost_calculation() ->
    Clauses = [
        {{pat_literal, 1, integer, loc()}, {var, x, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Cost = catena_pattern_decision_tree:column_cost(Matrix, 0),
    %% Literal has cost 1.0
    ?assertEqual(1.0, Cost).

test_benefit_calculation() ->
    Clauses = [
        {{pat_literal, 1, integer, loc()}, {var, x, loc()}},
        {{pat_var, x, loc()}, {var, x, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Benefit = catena_pattern_decision_tree:column_benefit(Matrix, 0),
    %% 1 non-wildcard out of 2 rows = 0.5
    ?assertEqual(0.5, Benefit).

%%====================================================================
%% Matrix Specialization Tests
%%====================================================================

matrix_specialization_test_() ->
    [
        {"specialize for literal",
         ?_test(test_specialize_literal())},
        {"specialize for constructor",
         ?_test(test_specialize_constructor())},
        {"default matrix",
         ?_test(test_default_matrix())}
    ].

test_specialize_literal() ->
    Clauses = [
        {{pat_literal, 1, integer, loc()}, {literal, integer, 1, loc()}},
        {{pat_literal, 2, integer, loc()}, {literal, integer, 2, loc()}},
        {{pat_var, x, loc()}, {var, x, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),

    %% Specialize for literal 1
    Specialized = catena_pattern_decision_tree:specialize_matrix(Matrix, 0, {literal, {1, integer}}),
    %% Should keep rows 0 (literal 1) and 2 (wildcard)
    ?assertEqual(2, length(Specialized)).

test_specialize_constructor() ->
    Clauses = [
        {{pat_constructor, 'Some', [{pat_var, x, loc()}], loc()}, {var, x, loc()}},
        {{pat_constructor, 'None', [], loc()}, {literal, atom, none, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),

    %% Specialize for Some constructor
    Specialized = catena_pattern_decision_tree:specialize_matrix(Matrix, 0, {ctor, 'Some', 1}),
    %% Should keep only the Some row
    ?assertEqual(1, length(Specialized)).

test_default_matrix() ->
    Clauses = [
        {{pat_literal, 1, integer, loc()}, {literal, integer, 1, loc()}},
        {{pat_var, x, loc()}, {var, x, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),

    %% Default matrix keeps only wildcard rows
    Default = catena_pattern_decision_tree:default_matrix(Matrix, 0),
    ?assertEqual(1, length(Default)).

%%====================================================================
%% Complete Signature Tests
%%====================================================================

complete_signature_test_() ->
    [
        {"boolean is complete with true and false",
         ?_test(test_bool_complete())},
        {"boolean is incomplete with just true",
         ?_test(test_bool_incomplete())},
        {"list is complete with nil and cons",
         ?_test(test_list_complete())}
    ].

test_bool_complete() ->
    Ctors = [{literal, {true, bool}}, {literal, {false, bool}}],
    ?assertEqual(true, catena_pattern_decision_tree:is_complete_signature(Ctors, {bool})).

test_bool_incomplete() ->
    Ctors = [{literal, {true, bool}}],
    ?assertEqual(false, catena_pattern_decision_tree:is_complete_signature(Ctors, {bool})).

test_list_complete() ->
    Ctors = [nil, cons],
    ?assertEqual(true, catena_pattern_decision_tree:is_complete_signature(Ctors, {list, any})).

%%====================================================================
%% Guard Handling Tests
%%====================================================================

guard_handling_test_() ->
    [
        {"guarded clause produces guarded leaf",
         ?_test(test_guarded_clause())},
        {"multiple guarded clauses chain",
         ?_test(test_multiple_guards())}
    ].

test_guarded_clause() ->
    Guard = {binary_op, '>', {var, x, loc()}, {literal, integer, 0, loc()}, loc()},
    Clauses = [
        {clause, [{pat_var, x, loc()}], [Guard], {var, x, loc()}},
        {{pat_var, y, loc()}, {literal, integer, 0, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    %% First clause has guard, second doesn't - should produce guarded_leaf
    ?assertMatch({guarded_leaf, 0, [_], _, _}, Tree).

test_multiple_guards() ->
    Guard1 = {binary_op, '>', {var, x, loc()}, {literal, integer, 0, loc()}, loc()},
    Guard2 = {binary_op, '<', {var, x, loc()}, {literal, integer, 10, loc()}, loc()},
    Clauses = [
        {clause, [{pat_var, x, loc()}], [Guard1], {literal, atom, positive, loc()}},
        {clause, [{pat_var, x, loc()}], [Guard2], {literal, atom, small, loc()}},
        {{pat_var, x, loc()}, {literal, atom, other, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    %% Should chain guarded_leaf nodes
    ?assertMatch({guarded_leaf, 0, _, _, {guarded_leaf, 1, _, _, {leaf, 2, [], _}}}, Tree).

%%====================================================================
%% Pattern Analysis Tests
%%====================================================================

pattern_analysis_test_() ->
    [
        {"analyze empty patterns",
         ?_test(test_analyze_empty())},
        {"analyze patterns with sharing",
         ?_test(test_analyze_sharing())}
    ].

test_analyze_empty() ->
    Analysis = catena_pattern_decision_tree:analyze_patterns([]),
    ?assertEqual(0, maps:get(num_rows, Analysis)),
    ?assertEqual(0, maps:get(num_columns, Analysis)).

test_analyze_sharing() ->
    %% Two clauses with same action
    SharedAction = {literal, integer, 42, loc()},
    Clauses = [
        {{pat_literal, 1, integer, loc()}, SharedAction},
        {{pat_literal, 2, integer, loc()}, SharedAction},
        {{pat_literal, 3, integer, loc()}, {literal, integer, 3, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Analysis = catena_pattern_decision_tree:analyze_patterns(Matrix),

    Sharing = maps:get(sharing_opportunities, Analysis),
    %% Should detect rows 0 and 1 have same action
    ?assertEqual(1, length(Sharing)).

%%====================================================================
%% List Pattern Tests
%%====================================================================

list_pattern_test_() ->
    [
        {"empty list pattern",
         ?_test(test_empty_list_pattern())},
        {"cons pattern",
         ?_test(test_cons_pattern())},
        {"list with elements",
         ?_test(test_list_with_elements())}
    ].

test_empty_list_pattern() ->
    Clauses = [
        {{pat_list, [], loc()}, {literal, atom, empty, loc()}},
        {{pat_var, x, loc()}, {literal, atom, nonempty, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    ?assertMatch({switch, _, [{nil, _} | _], _}, Tree).

test_cons_pattern() ->
    Clauses = [
        {{pat_cons, {pat_var, h, loc()}, {pat_var, t, loc()}, loc()}, {var, h, loc()}},
        {{pat_list, [], loc()}, {literal, atom, empty, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    ?assertMatch({switch, _, _, _}, Tree).

test_list_with_elements() ->
    Clauses = [
        {{pat_list, [{pat_var, a, loc()}, {pat_var, b, loc()}], loc()}, {var, a, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    ?assertMatch({switch, _, [{cons, _} | _], _}, Tree).

%%====================================================================
%% Tuple Pattern Tests
%%====================================================================

tuple_pattern_test_() ->
    [
        {"tuple pattern produces switch",
         ?_test(test_tuple_pattern())},
        {"different arity tuples",
         ?_test(test_tuple_arity())}
    ].

test_tuple_pattern() ->
    Clauses = [
        {{pat_tuple, [{pat_var, x, loc()}, {pat_var, y, loc()}], loc()},
         {binary_op, '+', {var, x, loc()}, {var, y, loc()}, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    ?assertMatch({switch, _, [{{tuple, 2}, _}], _}, Tree).

test_tuple_arity() ->
    Clauses = [
        {{pat_tuple, [{pat_var, x, loc()}], loc()}, {literal, integer, 1, loc()}},
        {{pat_tuple, [{pat_var, x, loc()}, {pat_var, y, loc()}], loc()}, {literal, integer, 2, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    {switch, _, Branches, _} = Tree,
    %% Should have branches for both tuple arities
    ?assertEqual(2, length(Branches)).

%%====================================================================
%% Or-Pattern Tests
%%====================================================================

or_pattern_test_() ->
    [
        {"or pattern matches first alternative",
         ?_test(test_or_pattern())}
    ].

test_or_pattern() ->
    %% Match 1 | 2 | 3
    OrPattern = {pat_or, [
        {pat_literal, 1, integer, loc()},
        {pat_literal, 2, integer, loc()},
        {pat_literal, 3, integer, loc()}
    ], loc()},
    Clauses = [
        {OrPattern, {literal, atom, matched, loc()}},
        {{pat_var, x, loc()}, {literal, atom, other, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    %% Or patterns should be handled (using first alternative)
    ?assertMatch({switch, _, _, _}, Tree).

%%====================================================================
%% As-Pattern Tests
%%====================================================================

as_pattern_test_() ->
    [
        {"as pattern binds and matches",
         ?_test(test_as_pattern())}
    ].

test_as_pattern() ->
    %% Match x@(Some y)
    AsPattern = {pat_as, x, {pat_constructor, 'Some', [{pat_var, y, loc()}], loc()}, loc()},
    Clauses = [
        {AsPattern, {var, x, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    ?assertMatch({switch, _, [{{ctor, 'Some', 1}, _}], _}, Tree).

%%====================================================================
%% Failure Compilation Tests (3.2.4)
%%====================================================================

failure_compilation_test_() ->
    [
        {"lenient mode returns default",
         ?_test(test_lenient_mode())},
        {"strict mode produces failure",
         ?_test(test_strict_mode())}
    ].

test_lenient_mode() ->
    Opts = #{lenient => true, default_value => {literal, atom, default, loc()}},
    Tree = catena_pattern_decision_tree:build_tree([], Opts),
    ?assertMatch({leaf, -1, [], _}, Tree).

test_strict_mode() ->
    Tree = catena_pattern_decision_tree:build_tree([], #{}),
    ?assertMatch({fail, _}, Tree).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
        {"factorial-like pattern matching",
         ?_test(test_factorial_patterns())},
        {"option type pattern matching",
         ?_test(test_option_patterns())},
        {"mixed literals and constructors",
         ?_test(test_mixed_patterns())}
    ].

test_factorial_patterns() ->
    %% factorial 0 = 1
    %% factorial n = n * factorial (n - 1)
    Clauses = [
        {{pat_literal, 0, integer, loc()}, {literal, integer, 1, loc()}},
        {{pat_var, n, loc()}, {binary_op, '*', {var, n, loc()}, {var, rec, loc()}, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),

    %% Should produce switch on 0 with default
    ?assertMatch({switch, _, [{_, {leaf, 0, [], _}}], {leaf, 1, [], _}}, Tree).

test_option_patterns() ->
    %% getOrElse (Some x) _ = x
    %% getOrElse None default = default
    Clauses = [
        {[{pat_constructor, 'Some', [{pat_var, x, loc()}], loc()}, {pat_var, '_', loc()}],
         {var, x, loc()}},
        {[{pat_constructor, 'None', [], loc()}, {pat_var, default, loc()}],
         {var, default, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),

    %% Should switch on first column (constructor)
    ?assertMatch({switch, #{index := 0}, _, _}, Tree).

test_mixed_patterns() ->
    %% Match integers 0, 1, 2 and constructor Some/None
    Clauses = [
        {{pat_literal, 0, integer, loc()}, {literal, atom, zero, loc()}},
        {{pat_literal, 1, integer, loc()}, {literal, atom, one, loc()}},
        {{pat_var, x, loc()}, {var, x, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),

    %% Should produce switch with two literal branches and default
    {switch, _, Branches, Default} = Tree,
    ?assertEqual(2, length(Branches)),
    ?assertMatch({leaf, 2, [], _}, Default).

%%====================================================================
%% Edge Case Tests
%%====================================================================

edge_case_test_() ->
    [
        {"deeply nested constructors",
         ?_test(test_deep_nesting())},
        {"many alternatives",
         ?_test(test_many_alternatives())}
    ].

test_deep_nesting() ->
    %% Triple nested: Some (Some (Some x))
    DeepPattern = {pat_constructor, 'Some', [
        {pat_constructor, 'Some', [
            {pat_constructor, 'Some', [{pat_var, x, loc()}], loc()}
        ], loc()}
    ], loc()},
    Clauses = [
        {DeepPattern, {var, x, loc()}},
        {{pat_var, '_', loc()}, {literal, atom, shallow, loc()}}
    ],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),
    ?assertMatch({switch, _, _, _}, Tree).

test_many_alternatives() ->
    %% 10 literal patterns
    Clauses = [{
        {pat_literal, N, integer, loc()},
        {literal, integer, N, loc()}
    } || N <- lists:seq(1, 10)],
    Matrix = catena_pattern_decision_tree:pattern_matrix_from_clauses(Clauses),
    Tree = catena_pattern_decision_tree:build_tree(Matrix, #{}),

    {switch, _, Branches, _} = Tree,
    ?assertEqual(10, length(Branches)).
