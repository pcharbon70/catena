%%%-------------------------------------------------------------------
%%% @doc Tests for Pattern Exhaustiveness and Redundancy Checking (Phase 3.3)
%%%
%%% Tests the catena_pattern_check module which implements static analysis
%%% for pattern matching safety.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_pattern_check_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

loc() ->
    {location, 1, 1}.

%% Helper to create common patterns
wildcard() -> {pat_wildcard, loc()}.
var(Name) -> {pat_var, Name, loc()}.
int(V) -> {pat_literal, V, integer, loc()}.
bool(V) -> {pat_literal, V, bool, loc()}.
ctor(Name) -> {pat_constructor, Name, [], loc()}.
ctor(Name, Args) -> {pat_constructor, Name, Args, loc()}.
tuple(Elems) -> {pat_tuple, Elems, loc()}.
list(Elems) -> {pat_list, Elems, loc()}.
cons(H, T) -> {pat_cons, H, T, loc()}.
pat_or(Alts) -> {pat_or, Alts, loc()}.

%% Type info helpers
maybe_type() ->
    {adt, 'Maybe', [{'None', 0}, {'Some', 1}]}.

either_type() ->
    {adt, 'Either', [{'Left', 1}, {'Right', 1}]}.

color_type() ->
    {adt, 'Color', [{'Red', 0}, {'Green', 0}, {'Blue', 0}]}.

%%====================================================================
%% Exhaustiveness Tests (3.3.1)
%%====================================================================

exhaustiveness_test_() ->
    [
        {"wildcard is exhaustive",
         ?_test(test_wildcard_exhaustive())},
        {"variable is exhaustive",
         ?_test(test_variable_exhaustive())},
        {"complete bool is exhaustive",
         ?_test(test_bool_exhaustive())},
        {"incomplete bool is non-exhaustive",
         ?_test(test_bool_non_exhaustive())},
        {"complete ADT is exhaustive",
         ?_test(test_adt_exhaustive())},
        {"incomplete ADT is non-exhaustive",
         ?_test(test_adt_non_exhaustive())},
        {"complete list is exhaustive",
         ?_test(test_list_exhaustive())},
        {"incomplete list is non-exhaustive",
         ?_test(test_list_non_exhaustive())}
    ].

test_wildcard_exhaustive() ->
    Patterns = [wildcard()],
    Result = catena_pattern_check:check_match(Patterns, {any}),
    ?assertEqual(true, maps:get(exhaustive, Result)),
    ?assertEqual([], maps:get(missing, Result)).

test_variable_exhaustive() ->
    Patterns = [var(x)],
    Result = catena_pattern_check:check_match(Patterns, {any}),
    ?assertEqual(true, maps:get(exhaustive, Result)).

test_bool_exhaustive() ->
    Patterns = [bool(true), bool(false)],
    Result = catena_pattern_check:check_match(Patterns, {bool}),
    ?assertEqual(true, maps:get(exhaustive, Result)),
    ?assertEqual([], maps:get(missing, Result)).

test_bool_non_exhaustive() ->
    Patterns = [bool(true)],
    Result = catena_pattern_check:check_match(Patterns, {bool}),
    ?assertEqual(false, maps:get(exhaustive, Result)),
    Missing = maps:get(missing, Result),
    ?assertEqual(1, length(Missing)).

test_adt_exhaustive() ->
    %% Maybe type with None and Some x
    Patterns = [ctor('None'), ctor('Some', [wildcard()])],
    Result = catena_pattern_check:check_match(Patterns, maybe_type()),
    ?assertEqual(true, maps:get(exhaustive, Result)).

test_adt_non_exhaustive() ->
    %% Only None, missing Some
    Patterns = [ctor('None')],
    Result = catena_pattern_check:check_match(Patterns, maybe_type()),
    ?assertEqual(false, maps:get(exhaustive, Result)),
    Missing = maps:get(missing, Result),
    ?assert(length(Missing) > 0).

test_list_exhaustive() ->
    %% Empty list and cons pattern
    Patterns = [list([]), cons(wildcard(), wildcard())],
    Result = catena_pattern_check:check_match(Patterns, {list, {any}}),
    ?assertEqual(true, maps:get(exhaustive, Result)).

test_list_non_exhaustive() ->
    %% Only empty list
    Patterns = [list([])],
    Result = catena_pattern_check:check_match(Patterns, {list, {any}}),
    ?assertEqual(false, maps:get(exhaustive, Result)).

%%====================================================================
%% Redundancy Tests (3.3.2)
%%====================================================================

redundancy_test_() ->
    [
        {"no redundancy in simple patterns",
         ?_test(test_no_redundancy())},
        {"wildcard after specific is redundant",
         ?_test(test_wildcard_redundant())},
        {"duplicate pattern is redundant",
         ?_test(test_duplicate_redundant())},
        {"subsumed pattern is redundant",
         ?_test(test_subsumed_redundant())},
        {"unreachable after wildcard",
         ?_test(test_unreachable_after_wildcard())}
    ].

test_no_redundancy() ->
    Patterns = [bool(true), bool(false)],
    Result = catena_pattern_check:check_match(Patterns, {bool}),
    ?assertEqual([], maps:get(redundant, Result)).

test_wildcard_redundant() ->
    %% true, false, _ - wildcard is unreachable
    Patterns = [bool(true), bool(false), wildcard()],
    Result = catena_pattern_check:check_match(Patterns, {bool}),
    Redundant = maps:get(redundant, Result),
    ?assertEqual(1, length(Redundant)),
    [{2, _}] = Redundant.  % Index 2 (third pattern) is redundant

test_duplicate_redundant() ->
    %% true, true - second true is redundant
    Patterns = [bool(true), bool(true)],
    Result = catena_pattern_check:check_match(Patterns, {bool}),
    Redundant = maps:get(redundant, Result),
    ?assertEqual(1, length(Redundant)),
    [{1, _}] = Redundant.

test_subsumed_redundant() ->
    %% wildcard subsumes everything after it
    Patterns = [wildcard(), bool(true)],
    Result = catena_pattern_check:check_match(Patterns, {bool}),
    Redundant = maps:get(redundant, Result),
    ?assertEqual(1, length(Redundant)).

test_unreachable_after_wildcard() ->
    %% Multiple patterns after wildcard are all redundant
    Patterns = [wildcard(), bool(true), bool(false), int(42)],
    Result = catena_pattern_check:check_match(Patterns, {any}),
    Redundant = maps:get(redundant, Result),
    ?assertEqual(3, length(Redundant)).

%%====================================================================
%% Subsumption Tests
%%====================================================================

subsumption_test_() ->
    [
        {"wildcard subsumes everything",
         ?_test(test_wildcard_subsumes())},
        {"variable subsumes everything",
         ?_test(test_variable_subsumes())},
        {"literal does not subsume different literal",
         ?_test(test_literal_no_subsume())},
        {"constructor with wildcard subsumes specific",
         ?_test(test_ctor_subsumes())}
    ].

test_wildcard_subsumes() ->
    ?assertEqual(true, catena_pattern_check:pattern_subsumes(wildcard(), bool(true))),
    ?assertEqual(true, catena_pattern_check:pattern_subsumes(wildcard(), int(42))),
    ?assertEqual(true, catena_pattern_check:pattern_subsumes(wildcard(), ctor('None'))).

test_variable_subsumes() ->
    ?assertEqual(true, catena_pattern_check:pattern_subsumes(var(x), bool(true))),
    ?assertEqual(true, catena_pattern_check:pattern_subsumes(var(x), ctor('Some', [int(1)]))).

test_literal_no_subsume() ->
    ?assertEqual(false, catena_pattern_check:pattern_subsumes(bool(true), bool(false))),
    ?assertEqual(false, catena_pattern_check:pattern_subsumes(int(1), int(2))).

test_ctor_subsumes() ->
    %% Some(_) subsumes Some(42)
    General = ctor('Some', [wildcard()]),
    Specific = ctor('Some', [int(42)]),
    ?assertEqual(true, catena_pattern_check:pattern_subsumes(General, Specific)),
    ?assertEqual(false, catena_pattern_check:pattern_subsumes(Specific, General)).

%%====================================================================
%% Usefulness Tests
%%====================================================================

usefulness_test_() ->
    [
        {"empty matrix makes any pattern useful",
         ?_test(test_useful_empty_matrix())},
        {"covered pattern is not useful",
         ?_test(test_not_useful_covered())},
        {"uncovered pattern is useful",
         ?_test(test_useful_uncovered())}
    ].

test_useful_empty_matrix() ->
    ?assertEqual(true, catena_pattern_check:is_useful([], [wildcard()])),
    ?assertEqual(true, catena_pattern_check:is_useful([], [bool(true)])).

test_not_useful_covered() ->
    Matrix = [[wildcard()]],
    ?assertEqual(false, catena_pattern_check:is_useful(Matrix, [bool(true)])),
    ?assertEqual(false, catena_pattern_check:is_useful(Matrix, [int(42)])).

test_useful_uncovered() ->
    Matrix = [[bool(true)]],
    ?assertEqual(true, catena_pattern_check:is_useful(Matrix, [bool(false)])).

%%====================================================================
%% ADT Pattern Tests
%%====================================================================

adt_pattern_test_() ->
    [
        {"complete color enum is exhaustive",
         ?_test(test_color_exhaustive())},
        {"partial color enum is non-exhaustive",
         ?_test(test_color_non_exhaustive())},
        {"either type with both constructors",
         ?_test(test_either_exhaustive())},
        {"nested ADT patterns",
         ?_test(test_nested_adt())}
    ].

test_color_exhaustive() ->
    Patterns = [ctor('Red'), ctor('Green'), ctor('Blue')],
    Result = catena_pattern_check:check_match(Patterns, color_type()),
    ?assertEqual(true, maps:get(exhaustive, Result)).

test_color_non_exhaustive() ->
    Patterns = [ctor('Red'), ctor('Green')],  % Missing Blue
    Result = catena_pattern_check:check_match(Patterns, color_type()),
    ?assertEqual(false, maps:get(exhaustive, Result)).

test_either_exhaustive() ->
    Patterns = [ctor('Left', [wildcard()]), ctor('Right', [wildcard()])],
    Result = catena_pattern_check:check_match(Patterns, either_type()),
    ?assertEqual(true, maps:get(exhaustive, Result)).

test_nested_adt() ->
    %% Maybe (Maybe a) - need None, Some None, Some (Some _)
    Patterns = [
        ctor('None'),
        ctor('Some', [ctor('None')]),
        ctor('Some', [ctor('Some', [wildcard()])])
    ],
    %% The inner type is also Maybe
    InnerMaybe = maybe_type(),
    Result = catena_pattern_check:check_match(Patterns, maybe_type()),
    ?assertEqual(true, maps:get(exhaustive, Result)).

%%====================================================================
%% Tuple Pattern Tests
%%====================================================================

tuple_pattern_test_() ->
    [
        {"tuple with wildcards is exhaustive",
         ?_test(test_tuple_wildcard())},
        {"tuple must match arity",
         ?_test(test_tuple_arity())}
    ].

test_tuple_wildcard() ->
    Patterns = [tuple([wildcard(), wildcard()])],
    Result = catena_pattern_check:check_match(Patterns, {tuple, [{any}, {any}]}),
    ?assertEqual(true, maps:get(exhaustive, Result)).

test_tuple_arity() ->
    %% Tuple with two elements
    Patterns = [tuple([wildcard(), wildcard()])],
    Result = catena_pattern_check:check_match(Patterns, {tuple, [{any}, {any}]}),
    ?assertEqual(true, maps:get(exhaustive, Result)),
    ?assertEqual([], maps:get(redundant, Result)).

%%====================================================================
%% Or-Pattern Tests
%%====================================================================

or_pattern_test_() ->
    [
        {"or pattern covers multiple cases",
         ?_test(test_or_pattern_coverage())},
        {"or pattern with all alternatives is exhaustive",
         ?_test(test_or_pattern_exhaustive())}
    ].

test_or_pattern_coverage() ->
    %% Red | Green covers two constructors
    Patterns = [pat_or([ctor('Red'), ctor('Green')]), ctor('Blue')],
    Result = catena_pattern_check:check_match(Patterns, color_type()),
    ?assertEqual(true, maps:get(exhaustive, Result)).

test_or_pattern_exhaustive() ->
    %% Red | Green | Blue covers all
    Patterns = [pat_or([ctor('Red'), ctor('Green'), ctor('Blue')])],
    Result = catena_pattern_check:check_match(Patterns, color_type()),
    ?assertEqual(true, maps:get(exhaustive, Result)).

%%====================================================================
%% Warning Generation Tests (3.3.3)
%%====================================================================

warning_test_() ->
    [
        {"non-exhaustive generates warning",
         ?_test(test_non_exhaustive_warning())},
        {"redundant generates warning",
         ?_test(test_redundant_warning())},
        {"warnings can be suppressed",
         ?_test(test_warning_suppression())}
    ].

test_non_exhaustive_warning() ->
    Patterns = [bool(true)],
    Result = catena_pattern_check:check_match(Patterns, {bool}),
    Warnings = maps:get(warnings, Result),
    NonExhaustive = [W || {warning, non_exhaustive, _, _} = W <- Warnings],
    ?assert(length(NonExhaustive) > 0).

test_redundant_warning() ->
    Patterns = [wildcard(), bool(true)],
    Result = catena_pattern_check:check_match(Patterns, {bool}),
    Warnings = maps:get(warnings, Result),
    Redundant = [W || {warning, redundant_pattern, _, _} = W <- Warnings],
    ?assertEqual(1, length(Redundant)).

test_warning_suppression() ->
    Patterns = [bool(true)],
    Opts = #{warnings => []},  % No warnings enabled
    Result = catena_pattern_check:check_match(Patterns, {bool}, Opts),
    Warnings = maps:get(warnings, Result),
    ?assertEqual([], Warnings).

%%====================================================================
%% Warning Formatting Tests
%%====================================================================

warning_format_test_() ->
    [
        {"format missing warning",
         ?_test(test_format_missing())},
        {"format redundancy warning",
         ?_test(test_format_redundancy())}
    ].

test_format_missing() ->
    Missing = [ctor('None')],
    Msg = catena_pattern_check:format_missing_warning(Missing, loc()),
    ?assert(is_binary(Msg)),
    ?assert(binary:match(Msg, <<"Non-exhaustive">>) =/= nomatch).

test_format_redundancy() ->
    Msg = catena_pattern_check:format_redundancy_warning(2, bool(true), loc()),
    ?assert(is_binary(Msg)),
    ?assert(binary:match(Msg, <<"Redundant">>) =/= nomatch),
    ?assert(binary:match(Msg, <<"3">>) =/= nomatch).  % Index 2 = clause 3

%%====================================================================
%% Matrix Operations Tests
%%====================================================================

matrix_ops_test_() ->
    [
        {"specialize matrix for constructor",
         ?_test(test_specialize_matrix())},
        {"default matrix extracts wildcards",
         ?_test(test_default_matrix())}
    ].

test_specialize_matrix() ->
    Matrix = [
        [ctor('Some', [var(x)])],
        [ctor('None')],
        [wildcard()]
    ],
    Specialized = catena_pattern_check:specialize(Matrix, {ctor, 'Some', 1}),
    %% Should keep Some row (with arg exposed) and wildcard row
    ?assertEqual(2, length(Specialized)).

test_default_matrix() ->
    Matrix = [
        [ctor('Some', [var(x)])],
        [ctor('None')],
        [wildcard()]
    ],
    Default = catena_pattern_check:default_matrix(Matrix),
    %% Should only keep wildcard row
    ?assertEqual(1, length(Default)).

%%====================================================================
%% SMT Integration Tests (3.3.4)
%%====================================================================

smt_test_() ->
    [
        {"guard exhaustiveness with pattern coverage",
         ?_test(test_guard_pattern_exhaustive())},
        {"guard exhaustiveness returns unknown without SMT",
         ?_test(test_guard_unknown())}
    ].

test_guard_pattern_exhaustive() ->
    %% If patterns are exhaustive, guards don't matter
    PatternGuards = [
        {bool(true), {literal, bool, true, loc()}},
        {bool(false), {literal, bool, true, loc()}}
    ],
    Result = catena_pattern_check:check_guard_exhaustiveness(PatternGuards, {bool}),
    ?assertEqual(exhaustive, Result).

test_guard_unknown() ->
    %% When patterns aren't exhaustive, return unknown (need SMT)
    PatternGuards = [
        {bool(true), {literal, bool, true, loc()}}
    ],
    Result = catena_pattern_check:check_guard_exhaustiveness(PatternGuards, {bool}),
    ?assertEqual(unknown, Result).

%%====================================================================
%% Edge Case Tests
%%====================================================================

edge_case_test_() ->
    [
        {"empty pattern list",
         ?_test(test_empty_patterns())},
        {"single exhaustive pattern",
         ?_test(test_single_exhaustive())},
        {"deeply nested patterns",
         ?_test(test_deep_nesting())}
    ].

test_empty_patterns() ->
    %% Empty pattern list is never exhaustive
    Result = catena_pattern_check:check_match([], {bool}),
    ?assertEqual(false, maps:get(exhaustive, Result)).

test_single_exhaustive() ->
    %% Single wildcard should be exhaustive
    Patterns = [wildcard()],
    Result = catena_pattern_check:check_match(Patterns, {bool}),
    ?assertEqual(true, maps:get(exhaustive, Result)).

test_deep_nesting() ->
    %% Nested constructors: Some(Some(Some(_)))
    Deep = ctor('Some', [ctor('Some', [ctor('Some', [wildcard()])])]),
    Patterns = [ctor('None'), ctor('Some', [ctor('None')]),
                ctor('Some', [ctor('Some', [ctor('None')])]), Deep],
    Result = catena_pattern_check:check_match(Patterns, maybe_type()),
    ?assertEqual(true, maps:get(exhaustive, Result)).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
        {"realistic option handling",
         ?_test(test_option_handling())},
        {"realistic result handling",
         ?_test(test_result_handling())}
    ].

test_option_handling() ->
    %% Common pattern: Some x -> use x, None -> default
    Patterns = [ctor('Some', [var(x)]), ctor('None')],
    Result = catena_pattern_check:check_match(Patterns, maybe_type()),
    ?assertEqual(true, maps:get(exhaustive, Result)),
    ?assertEqual([], maps:get(redundant, Result)).

test_result_handling() ->
    %% Either for error handling: Left err -> handle, Right val -> use
    Patterns = [ctor('Left', [var(err)]), ctor('Right', [var(val)])],
    Result = catena_pattern_check:check_match(Patterns, either_type()),
    ?assertEqual(true, maps:get(exhaustive, Result)),
    ?assertEqual([], maps:get(redundant, Result)).
