%% @doc Advanced Pattern Type Inference Tests (Phase 3.1)
%%
%% Tests for or-pattern type inference, as-pattern type inference,
%% and guard purity checking.
-module(catena_pattern_advanced_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Or-Pattern Type Inference Tests
%%====================================================================

%% Test: Or-pattern with matching literal types
or_pattern_same_literal_types_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    %% {pat_or, [{pat_literal, 1, integer}, {pat_literal, 2, integer}]}
    Pattern = {por, [
        {plit, {int, 1}},
        {plit, {int, 2}}
    ]},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    %% All literals are int, so result should be int
    ?assertEqual({tcon, int}, Type),
    %% No variables bound
    ?assertEqual(#{}, Bindings).

%% Test: Or-pattern with wildcard alternatives
or_pattern_wildcards_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    Pattern = {por, [
        {pwild},
        {pwild}
    ]},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    %% Type should be a fresh type variable
    ?assertMatch({tvar, _}, Type),
    ?assertEqual(#{}, Bindings).

%% Test: Single-alternative or-pattern (degenerates to simple pattern)
single_or_pattern_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    Pattern = {por, [{pvar, x}]},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    ?assertMatch({tvar, _}, Type),
    ?assert(maps:is_key(x, Bindings)).

%%====================================================================
%% As-Pattern Type Inference Tests
%%====================================================================

%% Test: As-pattern binds both name and inner pattern
as_pattern_binds_both_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    %% x as y
    Pattern = {pas, y, {pvar, x}},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    %% Both x and y should be bound
    ?assert(maps:is_key(x, Bindings)),
    ?assert(maps:is_key(y, Bindings)),

    %% Both should have the same type
    XScheme = maps:get(x, Bindings),
    YScheme = maps:get(y, Bindings),
    ?assertEqual(XScheme, YScheme).

%% Test: As-pattern with literal
as_pattern_with_literal_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    %% 42 as n
    Pattern = {pas, n, {plit, {int, 42}}},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    ?assertEqual({tcon, int}, Type),
    ?assert(maps:is_key(n, Bindings)).

%% Test: As-pattern with wildcard
as_pattern_with_wildcard_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    %% _ as x
    Pattern = {pas, x, {pwild}},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    %% Type should be fresh variable
    ?assertMatch({tvar, _}, Type),
    %% Only x should be bound (wildcard binds nothing)
    ?assert(maps:is_key(x, Bindings)),
    ?assertEqual(1, maps:size(Bindings)).

%%====================================================================
%% Guard Purity Tests
%%====================================================================

%% Test: Simple pure guard (comparison)
pure_comparison_guard_test() ->
    Guard = {binary_op, gt, {var, x, loc}, {literal, 0, integer, loc}, loc},
    ?assertEqual(ok, catena_infer_effect:check_guard_purity(Guard)).

%% Test: Pure guard with arithmetic
pure_arithmetic_guard_test() ->
    Guard = {binary_op, gt,
             {binary_op, plus, {var, x, loc}, {literal, 1, integer, loc}, loc},
             {literal, 10, integer, loc},
             loc},
    ?assertEqual(ok, catena_infer_effect:check_guard_purity(Guard)).

%% Test: Pure guard with variable reference
pure_variable_guard_test() ->
    Guard = {var, x, loc},
    ?assertEqual(ok, catena_infer_effect:check_guard_purity(Guard)).

%% Test: Pure guard with literal
pure_literal_guard_test() ->
    Guard = {literal, true, bool, loc},
    ?assertEqual(ok, catena_infer_effect:check_guard_purity(Guard)).

%% Test: Impure guard with perform (IO effect)
impure_perform_guard_test() ->
    Guard = {perform, 'IO', print, loc},
    Result = catena_infer_effect:check_guard_purity(Guard),
    ?assertMatch({error, {impure_guard, _, _}}, Result).

%% Test: Impure guard with do expression
impure_do_guard_test() ->
    Guard = {do_expr, [], loc},
    Result = catena_infer_effect:check_guard_purity(Guard),
    ?assertMatch({error, {impure_guard, _, _}}, Result).

%% Test: Complex pure guard
complex_pure_guard_test() ->
    %% (x > 0) and (y < 10)
    Guard = {binary_op, 'and',
             {binary_op, gt, {var, x, loc}, {literal, 0, integer, loc}, loc},
             {binary_op, lt, {var, y, loc}, {literal, 10, integer, loc}, loc},
             loc},
    ?assertEqual(ok, catena_infer_effect:check_guard_purity(Guard)).

%%====================================================================
%% Effect Set Tests
%%====================================================================

%% Test: Pure effect set
is_pure_test() ->
    ?assert(catena_infer_effect:is_pure(catena_infer_effect:pure())),
    ?assertNot(catena_infer_effect:is_pure(catena_infer_effect:from_list(['IO']))).

%% Test: Effect union
effect_union_test() ->
    E1 = catena_infer_effect:from_list(['IO']),
    E2 = catena_infer_effect:from_list(['State']),
    Union = catena_infer_effect:union(E1, E2),
    ?assertEqual({effect_set, ['IO', 'State']}, Union).

%% Test: Effect normalization (dedup and sort)
effect_normalize_test() ->
    E = catena_infer_effect:from_list(['State', 'IO', 'IO', 'State']),
    ?assertEqual({effect_set, ['IO', 'State']}, E).
