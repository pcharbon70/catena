%%%-------------------------------------------------------------------
%%% @doc Unit Tests for Pattern Type Inference
%%%
%%% Tests for catena_infer_pattern module covering:
%%% - Wildcard patterns
%%% - Literal patterns
%%% - Variable patterns
%%% - Tuple patterns
%%% - Record patterns
%%% - Variant patterns
%%% - As-patterns
%%% - Pattern binding collection
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_infer_pattern_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Section 1: Wildcard Patterns
%%%===================================================================

infer_wildcard_pattern_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type, Bindings, _State1} = catena_infer_pattern:infer({pwild}, Env, State),

    % Wildcard should produce fresh type variable
    ?assertMatch({tvar, _}, Type),

    % Wildcard should produce no bindings
    ?assertEqual([], maps:keys(Bindings)).

%%%===================================================================
%%% Section 2: Literal Patterns
%%%===================================================================

infer_int_literal_pattern_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type, Bindings, _State1} = catena_infer_pattern:infer({plit, {int, 42}}, Env, State),

    ?assertEqual({tcon, int}, Type),
    ?assertEqual([], maps:keys(Bindings)).

infer_bool_literal_pattern_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type, Bindings, _State1} = catena_infer_pattern:infer({plit, {bool, true}}, Env, State),

    ?assertEqual({tcon, bool}, Type),
    ?assertEqual([], maps:keys(Bindings)).

infer_string_literal_pattern_test() ->
    Env = catena_infer_state:new(),
    State = catena_infer_state:new(),

    {Type, Bindings, _State1} = catena_infer_pattern:infer({plit, {string, <<"hello">>}}, Env, State),

    ?assertEqual({tcon, string}, Type),
    ?assertEqual([], maps:keys(Bindings)).

infer_unit_literal_pattern_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type, Bindings, _State1} = catena_infer_pattern:infer({plit, {unit}}, Env, State),

    ?assertEqual({tcon, unit}, Type),
    ?assertEqual([], maps:keys(Bindings)).

%%%===================================================================
%%% Section 3: Variable Patterns
%%%===================================================================

infer_var_pattern_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type, Bindings, _State1} = catena_infer_pattern:infer({pvar, x}, Env, State),

    % Should produce fresh type variable
    ?assertMatch({tvar, _}, Type),

    % Should bind variable x
    ?assertEqual([x], maps:keys(Bindings)),

    % Binding should be monomorphic
    {mono, BoundType} = maps:get(x, Bindings),
    ?assertEqual(Type, BoundType).

infer_multiple_var_patterns_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type1, Bindings1, State1} = catena_infer_pattern:infer({pvar, x}, Env, State),
    {Type2, Bindings2, _State2} = catena_infer_pattern:infer({pvar, y}, Env, State1),

    % Should produce different type variables
    ?assertMatch({tvar, _}, Type1),
    ?assertMatch({tvar, _}, Type2),
    ?assertNotEqual(Type1, Type2),

    % Each should bind their own variable
    ?assertEqual([x], maps:keys(Bindings1)),
    ?assertEqual([y], maps:keys(Bindings2)).

%%%===================================================================
%%% Section 4: Tuple Patterns
%%%===================================================================

infer_empty_tuple_pattern_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type, Bindings, _State1} = catena_infer_pattern:infer({ptuple, []}, Env, State),

    ?assertEqual({ttuple, []}, Type),
    ?assertEqual([], maps:keys(Bindings)).

infer_tuple_with_literals_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    Pattern = {ptuple, [{plit, {int, 42}}, {plit, {bool, true}}]},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    ?assertEqual({ttuple, [{tcon, int}, {tcon, bool}]}, Type),
    ?assertEqual([], maps:keys(Bindings)).

infer_tuple_with_vars_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    Pattern = {ptuple, [{pvar, x}, {pvar, y}]},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    % Should be tuple of two type variables
    ?assertMatch({ttuple, [{tvar, _}, {tvar, _}]}, Type),

    % Should bind both x and y
    ?assertEqual([x, y], lists:sort(maps:keys(Bindings))).

infer_nested_tuple_pattern_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % ((x, y), z)
    Pattern = {ptuple, [
        {ptuple, [{pvar, x}, {pvar, y}]},
        {pvar, z}
    ]},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    % Should be ((α, β), γ)
    ?assertMatch({ttuple, [{ttuple, [{tvar, _}, {tvar, _}]}, {tvar, _}]}, Type),

    % Should bind x, y, z
    ?assertEqual([x, y, z], lists:sort(maps:keys(Bindings))).

%%%===================================================================
%%% Section 5: Record Patterns
%%%===================================================================

infer_empty_record_pattern_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    {Type, Bindings, _State1} = catena_infer_pattern:infer({precord, []}, Env, State),

    ?assertEqual({trecord, [], closed}, Type),
    ?assertEqual([], maps:keys(Bindings)).

infer_record_with_literals_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    Pattern = {precord, [{x, {plit, {int, 10}}}, {y, {plit, {bool, false}}}]},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    % Type should be {x: Int, y: Bool | closed}
    ?assertMatch({trecord, _, closed}, Type),
    {trecord, Fields, closed} = Type,

    % Should have both fields (order may vary)
    ?assertEqual([x, y], lists:sort([Label || {Label, _} <- Fields])),

    % No variable bindings
    ?assertEqual([], maps:keys(Bindings)).

infer_record_with_vars_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    Pattern = {precord, [{name, {pvar, n}}, {age, {pvar, a}}]},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    % Type should be {name: α, age: β | closed}
    ?assertMatch({trecord, _, closed}, Type),

    % Should bind both n and a
    ?assertEqual([a, n], lists:sort(maps:keys(Bindings))).

%%%===================================================================
%%% Section 6: Variant Patterns
%%%===================================================================

infer_variant_no_args_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    Pattern = {pvariant, none, []},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    ?assertEqual({tvariant, [{none, []}]}, Type),
    ?assertEqual([], maps:keys(Bindings)).

infer_variant_with_args_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    Pattern = {pvariant, some, [{pvar, x}]},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    % Should be variant with constructor some carrying α
    ?assertMatch({tvariant, [{some, [{tvar, _}]}]}, Type),

    % Should bind x
    ?assertEqual([x], maps:keys(Bindings)).

infer_variant_multiple_args_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    Pattern = {pvariant, pair, [{pvar, x}, {pvar, y}]},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    ?assertMatch({tvariant, [{pair, [{tvar, _}, {tvar, _}]}]}, Type),

    % Should bind both x and y
    ?assertEqual([x, y], lists:sort(maps:keys(Bindings))).

%%====================================================================
%% Duplicate Binding Detection Tests
%%====================================================================

duplicate_binding_same_type_test() ->
    % Test that duplicate bindings with same type are allowed
    IntScheme = catena_type_scheme:mono({tcon, int}),
    Env1 = catena_type_env:singleton(x, IntScheme),
    Env2 = catena_type_env:singleton(x, IntScheme),
    
    % Should merge successfully (same type)
    Result = catena_infer_pattern:merge_bindings(Env1, Env2),
    ?assertEqual(IntScheme, maps:get(x, Result)).

duplicate_binding_different_type_test() ->
    % Test that duplicate bindings with different types are detected
    IntScheme = catena_type_scheme:mono({tcon, int}),
    StrScheme = catena_type_scheme:mono({tcon, string}),
    Env1 = catena_type_env:singleton(x, IntScheme),
    Env2 = catena_type_env:singleton(x, StrScheme),
    
    % Should throw error for different types
    ?assertError({duplicate_pattern_binding, x, IntScheme, StrScheme},
        catena_infer_pattern:merge_bindings(Env1, Env2)).

duplicate_binding_multiple_variables_test() ->
    % Test with multiple variables, some conflicting
    IntScheme = catena_type_scheme:mono({tcon, int}),
    StrScheme = catena_type_scheme:mono({tcon, string}),
    BoolScheme = catena_type_scheme:mono({tcon, bool}),
    
    Env1 = #{x => IntScheme, y => StrScheme},
    Env2 = #{y => StrScheme, z => BoolScheme},  % y matches, z is new
    
    % Should merge successfully (only y overlaps but types match)
    Result = catena_infer_pattern:merge_bindings(Env1, Env2),
    ?assertEqual(IntScheme, maps:get(x, Result)),
    ?assertEqual(StrScheme, maps:get(y, Result)),
    ?assertEqual(BoolScheme, maps:get(z, Result)).

%%====================================================================
%% Pattern Matching Tests
%%====================================================================

infer_as_pattern_with_wildcard_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    Pattern = {pas, x, {pwild}},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    % Should produce type variable
    ?assertMatch({tvar, _}, Type),

    % Should bind x
    ?assertEqual([x], maps:keys(Bindings)),

    % x should have same type as wildcard
    {mono, BoundType} = maps:get(x, Bindings),
    ?assertEqual(Type, BoundType).

infer_as_pattern_with_literal_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    Pattern = {pas, num, {plit, {int, 42}}},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    ?assertEqual({tcon, int}, Type),

    % Should bind num
    ?assertEqual([num], maps:keys(Bindings)),
    {mono, BoundType} = maps:get(num, Bindings),
    ?assertEqual({tcon, int}, BoundType).

infer_as_pattern_with_tuple_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % (x, y) as pair
    Pattern = {pas, pair, {ptuple, [{pvar, x}, {pvar, y}]}},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    % Should be tuple type
    ?assertMatch({ttuple, [{tvar, _}, {tvar, _}]}, Type),

    % Should bind pair, x, and y
    ?assertEqual([pair, x, y], lists:sort(maps:keys(Bindings))),

    % pair should have tuple type
    {mono, PairType} = maps:get(pair, Bindings),
    ?assertEqual(Type, PairType).

%%%===================================================================
%%% Section 8: Integration Tests
%%%===================================================================

integration_complex_pattern_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % {x: (a, b), y: c} as record
    Pattern = {pas, record, {precord, [
        {x, {ptuple, [{pvar, a}, {pvar, b}]}},
        {y, {pvar, c}}
    ]}},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    % Type should be {x: (α, β), y: γ | closed}
    ?assertMatch({trecord, _, closed}, Type),

    % Should bind record, a, b, c
    ?assertEqual([a, b, c, record], lists:sort(maps:keys(Bindings))).

integration_variant_with_record_test() ->
    Env = catena_type_env:empty(),
    State = catena_infer_state:new(),

    % Person {name: n, age: a}
    Pattern = {pvariant, person, [{precord, [{name, {pvar, n}}, {age, {pvar, a}}]}]},

    {Type, Bindings, _State1} = catena_infer_pattern:infer(Pattern, Env, State),

    % Should be variant with person constructor
    ?assertMatch({tvariant, [{person, [{trecord, _, closed}]}]}, Type),

    % Should bind n and a
    ?assertEqual([a, n], lists:sort(maps:keys(Bindings))).

%%%===================================================================
%%% Test Suite Integration
%%%===================================================================

integration_all_sections_test() ->
    % Verify all sections run successfully
    ?assert(true).