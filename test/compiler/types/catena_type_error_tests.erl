%%%-------------------------------------------------------------------
%%% @doc Error Handling and Edge Case Tests for Type System
%%%
%%% Tests defensive programming, error scenarios, and edge cases
%%% that should fail gracefully or be handled properly.
%%%
%%% ## Performance/Stress Tests
%%%
%%% Some tests that check deep nesting limits and large collections
%%% are behind a compile-time flag to avoid slowing down regular
%%% test runs. To enable these tests, compile with:
%%%
%%%   erlc -DCATENA_ENABLE_STRESS_TESTS -o _build/test/ ...
%%%
%%% Or run the full suite with:
%%%
%%%   make test-stress
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_type_error_tests).

-include_lib("eunit/include/eunit.hrl").


%%====================================================================
%% Invalid Input Tests - Type Construction
%%====================================================================

invalid_construction_test_() ->
    [
     ?_test(test_invalid_type_variable_id()),
     ?_test(test_invalid_constructor_name()),
     ?_test(test_duplicate_record_fields()),
     ?_test(test_duplicate_variant_constructors())
    ].

test_invalid_type_variable_id() ->
    % Negative ID should fail
    ?assertError(function_clause, catena_types:tvar(-1)),

    % Zero ID should fail
    ?assertError(function_clause, catena_types:tvar(0)),

    % Non-integer should fail
    ?assertError(function_clause, catena_types:tvar(foo)),
    ?assertError(function_clause, catena_types:tvar("1")).

test_invalid_constructor_name() ->
    % Non-atom should fail
    ?assertError(function_clause, catena_types:tcon("Integer")),
    ?assertError(function_clause, catena_types:tcon(123)).

test_duplicate_record_fields() ->
    % Duplicate field names should be rejected
    % API returns error tuple instead of throwing
    ?assertEqual({error, {duplicate_record_fields, [x]}},
                 catena_types:trecord([
                     {x, catena_types:tcon(integer)},
                     {x, catena_types:tcon(string)}  % Duplicate field
                 ], closed)),

    % Multiple duplicates
    ?assertError({duplicate_record_fields, _},
                 catena_types:trecord([
                     {x, catena_types:tcon(integer)},
                     {y, catena_types:tcon(string)},
                     {x, catena_types:tcon(float)},   % x duplicated
                     {y, catena_types:tcon(boolean)}  % y duplicated
                 ], closed)),

    % No duplicates should work fine
    ValidRecord = catena_types:trecord([
        {x, catena_types:tcon(integer)},
        {y, catena_types:tcon(string)}
    ], closed),
    ?assertMatch({trecord, _, closed}, ValidRecord).

test_duplicate_variant_constructors() ->
    % Duplicate constructor names should be rejected
    ?assertError({duplicate_variant_constructors, ['Some']},
                 catena_types:tvariant([
                     {'Some', [catena_types:tcon(integer)]},
                     {'Some', [catena_types:tcon(string)]}  % Duplicate constructor
                 ])),

    % Multiple duplicates
    ?assertError({duplicate_variant_constructors, _},
                 catena_types:tvariant([
                     {'Red', []},
                     {'Green', []},
                     {'Red', []},    % Red duplicated
                     {'Green', []}   % Green duplicated
                 ])),

    % No duplicates should work fine
    ValidVariant = catena_types:tvariant([
        {'None', []},
        {'Some', [catena_types:tvar(1)]}
    ]),
    ?assertMatch({tvariant, _}, ValidVariant).

%%====================================================================
%% Circular Substitution Tests
%%====================================================================

circular_substitution_test_() ->
    [
      ?_test(test_simple_cycle()),
      ?_test(test_three_way_cycle()),
      ?_test(test_self_reference()),
      ?_test(test_occurs_check_explicit()),
      ?_test(test_depth_limit_exceeded()),
      ?_test(test_cycle_in_record_type()),
      ?_test(test_cycle_in_variant_type()),
      ?_test(test_cycle_in_tuple_type()),
      ?_test(test_cycle_in_function_type()),
      ?_test(test_indirect_cycle_through_record()),
      ?_test(test_indirect_cycle_through_variant()),
      ?_test(test_cycle_with_row_variable()),
      ?_test(test_composition_induced_cycle())
    ].

test_simple_cycle() ->
    % S = {1 ↦ α₂, 2 ↦ α₁}
    % Applying this creates infinite loop: α₁ → α₂ → α₁ → ...

    S = #{
        1 => catena_types:tvar(2),
        2 => catena_types:tvar(1)
    },

    % Occurs check should detect this and raise error
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(1))),

    % Should also fail when starting from variable 2
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(2))).

test_three_way_cycle() ->
    % S = {1 ↦ α₂, 2 ↦ α₃, 3 ↦ α₁}
    % Creates cycle: α₁ → α₂ → α₃ → α₁

    S = #{
        1 => catena_types:tvar(2),
        2 => catena_types:tvar(3),
        3 => catena_types:tvar(1)
    },

    % Should be detected by occurs check at any entry point
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(1))),
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(2))),
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(3))).

test_self_reference() ->
    % S = {1 ↦ List α₁}
    % This is the classic occurs check case

    S = #{
        1 => catena_types:tapp(
            catena_types:tcon('List'),
            [catena_types:tvar(1)]  % Self-reference
        )
    },

    % Should fail occurs check
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(1))),

    % Test the explicit occurs_check function
    ?assert(catena_type_subst:occurs_check(1, catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(1)]
    ))),

    % Variable that doesn't occur should return false
    ?assertNot(catena_type_subst:occurs_check(2, catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(1)]
    ))).

test_occurs_check_explicit() ->
    % Test the explicit occurs_check/2 function

    % Variable occurs in itself (wrapped)
    ?assert(catena_type_subst:occurs_check(1, catena_types:tvar(1))),

    % Variable occurs in function type
    FunType = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tvar(2),
        catena_types:empty_effects()
    ),
    ?assert(catena_type_subst:occurs_check(1, FunType)),
    ?assert(catena_type_subst:occurs_check(2, FunType)),
    ?assertNot(catena_type_subst:occurs_check(3, FunType)),

    % Variable occurs in nested type application
    NestedType = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tapp(
            catena_types:tcon('Maybe'),
            [catena_types:tvar(5)]
        )]
    ),
    ?assert(catena_type_subst:occurs_check(5, NestedType)),
    ?assertNot(catena_type_subst:occurs_check(1, NestedType)).

test_depth_limit_exceeded() ->
    % Create a very deep substitution chain that exceeds the depth limit
    % Build chain: α₁ → α₂ → α₃ → ... → α₆₀₀

    % Create substitution with 600 chained variables (exceeds 500 limit)
    DeepSubst = lists:foldl(
        fun(I, Acc) ->
            catena_type_subst:extend(Acc, I, catena_types:tvar(I + 1))
        end,
        catena_type_subst:empty(),
        lists:seq(1, 600)
    ),

    % Applying this should hit depth limit
    ?assertError({substitution_depth_exceeded, _, _},
                 catena_type_subst:apply(DeepSubst, catena_types:tvar(1))).

test_cycle_in_record_type() ->
    % S = {1 ↦ {x: α₁}}
    % Record contains reference to itself
    S = #{
        1 => catena_types:trecord([{x, catena_types:tvar(1)}], closed)
    },

    % Should detect cycle
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(1))),

    % Also test with occurs_check
    ?assert(catena_type_subst:occurs_check(1,
        catena_types:trecord([{x, catena_types:tvar(1)}], closed))).

test_cycle_in_variant_type() ->
    % S = {1 ↦ 'Some'(α₁)}
    % Variant constructor contains reference to itself
    S = #{
        1 => catena_types:tvariant([{'Some', [catena_types:tvar(1)]}, {'None', []}])
    },

    % Should detect cycle
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(1))),

    % Test with occurs_check
    ?assert(catena_type_subst:occurs_check(1,
        catena_types:tvariant([{'Some', [catena_types:tvar(1)]}]))).

test_cycle_in_tuple_type() ->
    % S = {1 ↦ (α₁, Int)}
    % Tuple contains reference to itself
    S = #{
        1 => catena_types:ttuple([catena_types:tvar(1), catena_types:tcon(integer)])
    },

    % Should detect cycle
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(1))),

    % Test with occurs_check
    ?assert(catena_type_subst:occurs_check(1,
        catena_types:ttuple([catena_types:tvar(1), catena_types:tcon(integer)]))).

test_cycle_in_function_type() ->
    % S = {1 ↦ (α₁ -> Int)}
    % Function input type contains reference to itself
    S1 = #{
        1 => catena_types:tfun(
            catena_types:tvar(1),
            catena_types:tcon(integer),
            catena_types:empty_effects()
        )
    },

    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S1, catena_types:tvar(1))),

    % S = {2 ↦ (Int -> α₂)}
    % Function output type contains reference to itself
    S2 = #{
        2 => catena_types:tfun(
            catena_types:tcon(integer),
            catena_types:tvar(2),
            catena_types:empty_effects()
        )
    },

    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S2, catena_types:tvar(2))).

test_indirect_cycle_through_record() ->
    % S = {1 ↦ α₂, 2 ↦ {x: α₁}}
    % α₁ → α₂ → {x: α₁} creates indirect cycle
    S = #{
        1 => catena_types:tvar(2),
        2 => catena_types:trecord([{x, catena_types:tvar(1)}], closed)
    },

    % Should detect indirect cycle
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(1))),
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(2))).

test_indirect_cycle_through_variant() ->
    % S = {1 ↦ α₂, 2 ↦ 'Node'(α₁, α₁)}
    % α₁ → α₂ → 'Node'(α₁, α₁) creates indirect cycle
    S = #{
        1 => catena_types:tvar(2),
        2 => catena_types:tvariant([
            {'Node', [catena_types:tvar(1), catena_types:tvar(1)]},
            {'Leaf', []}
        ])
    },

    % Should detect indirect cycle
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(1))),
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(2))).

test_cycle_with_row_variable() ->
    % Test cycle involving row variables in records
    % S = {1 ↦ α₂, 2 ↦ {x: α₁ | ρ₃}}
    % This creates a cycle: α₁ → α₂ → {x: α₁ | ρ₃}
    S = #{
        1 => catena_types:tvar(2),
        2 => catena_types:trecord([{x, catena_types:tvar(1)}], 3)
    },

    % Should detect cycle through the field type
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S, catena_types:tvar(1))),

    % Create another case: α₁ → α₂, α₂ → {y: String | α₁}
    % This creates cycle through the row variable itself
    S2 = #{
        1 => catena_types:tvar(2),
        2 => catena_types:trecord([{y, catena_types:tcon(string)}], 1)
    },

    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S2, catena_types:tvar(1))).

test_composition_induced_cycle() ->
    % S1 = {1 ↦ α₂}
    % S2 = {2 ↦ List α₁}
    % compose(S2, S1) = {1 ↦ List α₁, 2 ↦ List α₁}
    % This creates a cycle when applied

    S1 = catena_type_subst:singleton(1, catena_types:tvar(2)),
    S2 = catena_type_subst:singleton(2, catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(1)]
    )),

    % Compose the substitutions
    S_composed = catena_type_subst:compose(S2, S1),

    % Applying to α₁ should detect cycle
    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S_composed, catena_types:tvar(1))),

    % Test another composition scenario
    % S3 = {3 ↦ α₄}, S4 = {4 ↦ (α₃ -> Int)}
    S3 = catena_type_subst:singleton(3, catena_types:tvar(4)),
    S4 = catena_type_subst:singleton(4, catena_types:tfun(
        catena_types:tvar(3),
        catena_types:tcon(integer),
        catena_types:empty_effects()
    )),

    S_composed2 = catena_type_subst:compose(S4, S3),

    ?assertError({circular_substitution, _},
                 catena_type_subst:apply(S_composed2, catena_types:tvar(3))).

%%====================================================================
%% Deep Nesting Tests
%%====================================================================

deep_nesting_test_() ->
    [
      ?_test(test_moderately_deep_type()),
      ?_test(test_deep_type_variables())
     ] ++ deep_nesting_stress_tests().

test_moderately_deep_type() ->
    % Build type with 50 levels of nesting
    % List<List<List<...<Int>...>>>

    DeepType = lists:foldl(
        fun(_, Acc) ->
            catena_types:tapp(catena_types:tcon('List'), [Acc])
        end,
        catena_types:tcon(integer),
        lists:seq(1, 50)
    ),

    % Should be able to extract variables (none in this case)
    Vars = catena_types:type_vars(DeepType),
    ?assertEqual([], sets:to_list(Vars)),

    % Should be able to pretty-print (though output is long)
    PP = catena_type_pp:pp_type(DeepType),
    ?assert(is_list(PP)),
    ?assert(length(PP) > 100).  % Should be long

test_deep_type_variables() ->
    % Build function type with 20 nested functions
    % α₁ -> (α₂ -> (α₃ -> ... -> α₂₀))

    DeepFun = lists:foldl(
        fun(I, Acc) ->
            catena_types:tfun(
                catena_types:tvar(I),
                Acc,
                catena_types:empty_effects()
            )
        end,
        catena_types:tvar(20),
        lists:seq(19, 1, -1)
    ),

    % Should have 20 unique type variables
    Vars = catena_types:type_vars(DeepFun),
    VarList = lists:sort(sets:to_list(Vars)),
    ?assertEqual(lists:seq(1, 20), VarList).

%% Stress tests are conditionally included based on compile flag
-ifdef(CATENA_ENABLE_STRESS_TESTS).
deep_nesting_stress_tests() ->
    [
     ?_test(test_very_deep_type_nesting()),
     ?_test(test_extremely_deep_substitution_chain())
    ].
-else.
deep_nesting_stress_tests() ->
    [].
-endif.

-ifdef(CATENA_ENABLE_STRESS_TESTS).
test_very_deep_type_nesting() ->
    % Build type with 400 levels of nesting
    % This tests the limits of type traversal operations
    % (stays under MAX_SUBST_DEPTH of 500)
    % List<List<List<...<Int>...>>> (400 levels)

    DeepType = lists:foldl(
        fun(_, Acc) ->
            catena_types:tapp(catena_types:tcon('List'), [Acc])
        end,
        catena_types:tcon(integer),
        lists:seq(1, 400)
    ),

    % Should be able to extract variables
    Vars = catena_types:type_vars(DeepType),
    ?assertEqual([], sets:to_list(Vars)),

    % Should be able to apply empty substitution
    Result = catena_type_subst:apply(catena_type_subst:empty(), DeepType),
    ?assertEqual(DeepType, Result),

    % Should be able to pretty-print (though output is very long)
    PP = catena_type_pp:pp_type(DeepType),
    ?assert(is_list(PP)),
    ?assert(length(PP) > 2000).

test_extremely_deep_substitution_chain() ->
    % Create a substitution chain with 400 variables
    % (stays under MAX_SUBST_DEPTH of 500)
    % α₁ → α₂ → α₃ → ... → α₄₀₀ → Int
    % This tests performance of deep substitution application

    DeepSubst = lists:foldl(
        fun(I, Acc) ->
            catena_type_subst:extend(Acc, I, catena_types:tvar(I + 1))
        end,
        catena_type_subst:singleton(400, catena_types:tcon(integer)),
        lists:seq(1, 399)
    ),

    % Applying to α₁ should eventually resolve to Int
    % This will traverse 400 substitution steps
    Result = catena_type_subst:apply(DeepSubst, catena_types:tvar(1)),
    ?assertEqual(catena_types:tcon(integer), Result),

    % Verify domain size
    Domain = catena_type_subst:domain(DeepSubst),
    ?assertEqual(400, length(Domain)).
-endif.

%%====================================================================
%% Large Collection Tests
%%====================================================================

large_collection_test_() ->
    [
      ?_test(test_large_substitution()),
      ?_test(test_substitution_size_limit()),
      ?_test(test_large_environment()),
      ?_test(test_many_effects()),
      ?_test(test_effect_set_operations()),
      ?_test(test_effect_deduplication()),
      ?_test(test_effects_in_function_types())
     ] ++ large_collection_stress_tests().

test_large_substitution() ->
    % Create substitution with 1000 mappings
    LargeSubst = lists:foldl(
        fun(I, Acc) ->
            catena_type_subst:extend(Acc, I, catena_types:tcon(integer))
        end,
        catena_type_subst:empty(),
        lists:seq(1, 1000)
    ),

    % Should have 1000 entries in domain
    Domain = catena_type_subst:domain(LargeSubst),
    ?assertEqual(1000, length(Domain)),

    % Should be able to look up any entry
    ?assertEqual({ok, catena_types:tcon(integer)},
                 catena_type_subst:lookup(LargeSubst, 500)).

test_substitution_size_limit() ->
    % Test that exceeding the MAX_SUBSTITUTION_SIZE (10,000) raises an error
    % We build 10,000 mappings first (should succeed)
    Base = lists:foldl(
        fun(I, Acc) ->
            catena_type_subst:extend(Acc, I, catena_types:tcon(integer))
        end,
        catena_type_subst:empty(),
        lists:seq(1, 10000)
    ),

    % Should have 10,000 entries
    ?assertEqual(10000, maps:size(Base)),

    % Attempting to add one more should fail with substitution_too_large
    ?assertError({substitution_too_large, 10001, 10000},
                 catena_type_subst:extend(Base, 10001, catena_types:tcon(integer))).

test_large_environment() ->
    % Create environment with 1000 bindings
    LargeEnv = lists:foldl(
        fun(I, Acc) ->
            VarName = list_to_atom("var_" ++ integer_to_list(I)),
            Scheme = catena_type_scheme:mono(catena_types:tcon(integer)),
            catena_type_env:extend(Acc, VarName, Scheme)
        end,
        catena_type_env:empty(),
        lists:seq(1, 1000)
    ),

    % Should be able to look up any variable
    ?assertMatch({ok, _}, catena_type_env:lookup(LargeEnv, var_500)),

    % FTV should complete (though may be slow)
    Ftv = catena_type_env:ftv_env(LargeEnv),
    ?assert(is_tuple(Ftv)).  % Returns a set

test_many_effects() ->
    % Create effect set with 100 effects
    ManyEffects = catena_types:normalize_effects(
        [list_to_atom("effect_" ++ integer_to_list(I)) || I <- lists:seq(1, 100)]
    ),

    % Should be normalized (sorted, deduplicated)
    {effect_set, EffList} = ManyEffects,
    ?assertEqual(100, length(EffList)),

    % Should be sorted
    ?assertEqual(EffList, lists:sort(EffList)).

test_effect_set_operations() ->
    % Test union of large effect sets
    Effects1 = catena_types:normalize_effects(
        [list_to_atom("effect_a_" ++ integer_to_list(I)) || I <- lists:seq(1, 50)]
    ),
    Effects2 = catena_types:normalize_effects(
        [list_to_atom("effect_b_" ++ integer_to_list(I)) || I <- lists:seq(1, 50)]
    ),

    % Union should combine both sets
    Union = catena_types:union_effects(Effects1, Effects2),
    {effect_set, UnionList} = Union,
    ?assertEqual(100, length(UnionList)),

    % Should still be sorted and normalized
    ?assertEqual(UnionList, lists:sort(UnionList)),

    % Test union with overlapping effects
    Effects3 = catena_types:normalize_effects(
        [list_to_atom("effect_a_" ++ integer_to_list(I)) || I <- lists:seq(1, 25)]
    ),
    UnionOverlap = catena_types:union_effects(Effects1, Effects3),
    {effect_set, OverlapList} = UnionOverlap,
    ?assertEqual(50, length(OverlapList)),  % No duplicates

    % Empty effect set operations
    Empty = catena_types:empty_effects(),
    UnionWithEmpty = catena_types:union_effects(Effects1, Empty),
    ?assertEqual(Effects1, UnionWithEmpty),

    % Test union of two empty sets
    UnionEmptyEmpty = catena_types:union_effects(Empty, Empty),
    ?assertEqual(Empty, UnionEmptyEmpty),

    % Test is_pure with large sets
    ?assertNot(catena_types:is_pure(Effects1)),
    ?assert(catena_types:is_pure(Empty)).

test_effect_deduplication() ->
    % Create effect list with many duplicates
    EffectsWithDups = [io, io, io, network, network, file, file, io, network],

    % Normalize should remove duplicates and sort
    Normalized = catena_types:normalize_effects(EffectsWithDups),
    {effect_set, EffList} = Normalized,

    ?assertEqual(3, length(EffList)),  % Only io, network, file
    ?assertEqual([file, io, network], EffList),  % Sorted

    % Test with 200 effects with many duplicates
    ManyDuplicates = lists:flatten([
        [list_to_atom("effect_" ++ integer_to_list(I rem 50)) || I <- lists:seq(1, 200)]
    ]),
    NormalizedMany = catena_types:normalize_effects(ManyDuplicates),
    {effect_set, ManyList} = NormalizedMany,

    % Should have 50 unique effects
    ?assertEqual(50, length(ManyList)),
    ?assertEqual(ManyList, lists:usort(ManyList)).

test_effects_in_function_types() ->
    % Create function types with large effect sets
    LargeEffects = catena_types:normalize_effects(
        [list_to_atom("effect_" ++ integer_to_list(I)) || I <- lists:seq(1, 100)]
    ),

    FunType = catena_types:tfun(
        catena_types:tcon(integer),
        catena_types:tcon(string),
        LargeEffects
    ),

    % Should be able to extract effects
    {ok, ExtractedEffects} = catena_types:extract_function_effects(FunType),
    ?assertEqual(LargeEffects, ExtractedEffects),

    % Should be able to apply substitution
    Subst = catena_type_subst:empty(),
    ResultType = catena_type_subst:apply(Subst, FunType),
    ?assertEqual(FunType, ResultType),

    % Should be able to pretty-print
    PP = catena_type_pp:pp_type(FunType),
    ?assert(is_list(PP)),
    ?assert(length(PP) > 100),  % Should include all effects

    % Create nested function with effects
    NestedFun = catena_types:tfun(
        catena_types:tcon(float),
        FunType,
        catena_types:singleton_effect(async)
    ),

    % Should maintain separate effect sets
    ?assert(catena_types:is_function_type(NestedFun)),
    {ok, OuterEffects} = catena_types:extract_function_effects(NestedFun),
    ?assertNot(catena_types:effects_equal(OuterEffects, LargeEffects)).

-ifdef(CATENA_ENABLE_STRESS_TESTS).
large_collection_stress_tests() ->
    [
     ?_test(test_massive_substitution()),
     ?_test(test_massive_environment()),
     ?_test(test_massive_record_type()),
     ?_test(test_massive_effect_sets()),
     ?_test(test_effect_set_performance())
    ].
-else.
large_collection_stress_tests() ->
    [].
-endif.

-ifdef(CATENA_ENABLE_STRESS_TESTS).
test_massive_substitution() ->
    % Create substitution with 10,000 mappings
    % This tests memory usage and lookup performance
    MassiveSubst = lists:foldl(
        fun(I, Acc) ->
            catena_type_subst:extend(Acc, I, catena_types:tcon(integer))
        end,
        catena_type_subst:empty(),
        lists:seq(1, 10000)
    ),

    % Should be able to look up any mapping
    ?assertMatch({ok, _}, catena_type_subst:lookup(MassiveSubst, 5000)),
    ?assertMatch({ok, _}, catena_type_subst:lookup(MassiveSubst, 9999)),

    % Domain should have 10,000 entries
    Domain = catena_type_subst:domain(MassiveSubst),
    ?assertEqual(10000, length(Domain)),

    % Should be able to apply to a type
    Result = catena_type_subst:apply(MassiveSubst, catena_types:tvar(100)),
    ?assertEqual(catena_types:tcon(integer), Result).

test_massive_environment() ->
    % Create environment with 10,000 bindings
    % This tests memory usage and lookup performance
    MassiveEnv = lists:foldl(
        fun(I, Acc) ->
            VarName = list_to_atom("var_" ++ integer_to_list(I)),
            Scheme = catena_type_scheme:mono(catena_types:tcon(integer)),
            catena_type_env:extend(Acc, VarName, Scheme)
        end,
        catena_type_env:empty(),
        lists:seq(1, 10000)
    ),

    % Should be able to look up variables
    ?assertMatch({ok, _}, catena_type_env:lookup(MassiveEnv, var_5000)),
    ?assertMatch({ok, _}, catena_type_env:lookup(MassiveEnv, var_9999)),

    % FTV computation should complete
    Ftv = catena_type_env:ftv_env(MassiveEnv),
    ?assert(is_tuple(Ftv)).

test_massive_record_type() ->
    % Create record type with 500 fields
    % This tests record construction and field lookup performance
    Fields = [{list_to_atom("field_" ++ integer_to_list(I)), catena_types:tcon(integer)}
              || I <- lists:seq(1, 500)],

    MassiveRecord = catena_types:trecord(Fields, closed),

    % Should construct successfully
    ?assertMatch({trecord, _, closed}, MassiveRecord),

    % Should be able to extract type variables
    Vars = catena_types:type_vars(MassiveRecord),
    ?assertEqual([], sets:to_list(Vars)),

    % Should be able to pretty-print
    PP = catena_type_pp:pp_type(MassiveRecord),
    ?assert(is_list(PP)),
    ?assert(length(PP) > 5000).

test_massive_effect_sets() ->
    % Create effect set with 1,000 effects
    % This tests normalization and storage of very large effect sets
    MassiveEffects = catena_types:normalize_effects(
        [list_to_atom("effect_" ++ integer_to_list(I)) || I <- lists:seq(1, 1000)]
    ),

    % Should be normalized
    {effect_set, EffList} = MassiveEffects,
    ?assertEqual(1000, length(EffList)),
    ?assertEqual(EffList, lists:usort(EffList)),

    % Should support union operations
    MoreEffects = catena_types:normalize_effects(
        [list_to_atom("effect_" ++ integer_to_list(I)) || I <- lists:seq(500, 1500)]
    ),
    Union = catena_types:union_effects(MassiveEffects, MoreEffects),
    {effect_set, UnionList} = Union,
    ?assertEqual(1500, length(UnionList)),

    % Should support equality checks
    ?assert(catena_types:effects_equal(MassiveEffects, MassiveEffects)),
    ?assertNot(catena_types:effects_equal(MassiveEffects, MoreEffects)),

    % Should be usable in function types
    FunWithMassiveEffects = catena_types:tfun(
        catena_types:tcon(integer),
        catena_types:tcon(string),
        MassiveEffects
    ),

    % Should be able to extract effects
    {ok, Extracted} = catena_types:extract_function_effects(FunWithMassiveEffects),
    ?assertEqual(MassiveEffects, Extracted),

    % Should be able to apply substitutions
    Result = catena_type_subst:apply(catena_type_subst:empty(), FunWithMassiveEffects),
    ?assertEqual(FunWithMassiveEffects, Result).

test_effect_set_performance() ->
    % Test performance of effect set operations with many duplicates
    % Create 5,000 effects with only 100 unique values
    ManyDuplicates = lists:flatten([
        [list_to_atom("effect_" ++ integer_to_list(I rem 100)) || I <- lists:seq(1, 5000)]
    ]),

    % Normalization should handle this efficiently
    Normalized = catena_types:normalize_effects(ManyDuplicates),
    {effect_set, EffList} = Normalized,
    ?assertEqual(100, length(EffList)),

    % Test union of multiple large sets
    Set1 = catena_types:normalize_effects(
        [list_to_atom("set1_" ++ integer_to_list(I)) || I <- lists:seq(1, 200)]
    ),
    Set2 = catena_types:normalize_effects(
        [list_to_atom("set2_" ++ integer_to_list(I)) || I <- lists:seq(1, 200)]
    ),
    Set3 = catena_types:normalize_effects(
        [list_to_atom("set3_" ++ integer_to_list(I)) || I <- lists:seq(1, 200)]
    ),

    % Chain multiple unions
    Union1 = catena_types:union_effects(Set1, Set2),
    Union2 = catena_types:union_effects(Union1, Set3),
    {effect_set, FinalList} = Union2,
    ?assertEqual(600, length(FinalList)),
    ?assertEqual(FinalList, lists:usort(FinalList)),

    % Test with overlapping sets
    Set4 = catena_types:normalize_effects(
        [list_to_atom("set1_" ++ integer_to_list(I)) || I <- lists:seq(1, 100)]
    ),
    UnionOverlap = catena_types:union_effects(Set1, Set4),
    {effect_set, OverlapList} = UnionOverlap,
    ?assertEqual(200, length(OverlapList)),  % No duplicates from overlap

    % Test is_pure predicate
    ?assertNot(catena_types:is_pure(Union2)),
    ?assertNot(catena_types:is_pure(Normalized)).
-endif.

%%====================================================================
%% Substitution Edge Cases
%%====================================================================

substitution_edge_cases_test_() ->
    [
      ?_test(test_empty_substitution_application()),
      ?_test(test_identity_substitution()),
      ?_test(test_substitution_on_substitution()),
      ?_test(test_row_variable_edge_cases())
    ].

%%====================================================================
%% Row Variable Substitution Tests
%%====================================================================

row_variable_substitution_test_() ->
    [
      ?_test(test_row_var_to_row_var()),
      ?_test(test_row_var_to_closed()),
      ?_test(test_row_var_in_nested_records()),
      ?_test(test_multiple_row_vars()),
      ?_test(test_row_var_composition()),
      ?_test(test_row_var_type_vars()),
      ?_test(test_row_var_pretty_printing()),
      ?_test(test_row_var_field_merging())
    ].

test_empty_substitution_application() ->
    Empty = catena_type_subst:empty(),

    % Applying empty substitution should be identity
    Type = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tvar(2),
        catena_types:empty_effects()
    ),

    ?assertEqual(Type, catena_type_subst:apply(Empty, Type)).

test_identity_substitution() ->
    % S = {1 ↦ α₁} (maps variable to itself)
    S = catena_type_subst:singleton(1, catena_types:tvar(1)),

    Type = catena_types:tvar(1),

    % Should return same variable
    ?assertEqual(Type, catena_type_subst:apply(S, Type)).

test_substitution_on_substitution() ->
    % Apply substitution to the range of another substitution
    S1 = catena_type_subst:singleton(1, catena_types:tvar(2)),
    S2 = catena_type_subst:singleton(2, catena_types:tcon(integer)),

    % Compose: should map 1 → Int (via 2)
    Composed = catena_type_subst:compose(S2, S1),

    Result = catena_type_subst:apply(Composed, catena_types:tvar(1)),
    ?assertEqual(catena_types:tcon(integer), Result).

test_row_variable_edge_cases() ->
    % Row variable substituted with closed record
    S = catena_type_subst:singleton(
        1,
        catena_types:trecord([{x, catena_types:tcon(integer)}], closed)
    ),

    OpenRecord = catena_types:trecord(
        [{y, catena_types:tcon(string)}],
        1  % Row variable
    ),

    % After substitution, row variable should be closed
    Result = catena_type_subst:apply(S, OpenRecord),
    ?assertMatch({trecord, _, closed}, Result).

test_row_var_to_row_var() ->
    % Substituting one row variable with another
    % S = {ρ₁ ↦ ρ₂}
    S = catena_type_subst:singleton(1, catena_types:tvar(2)),

    % Record with ρ₁: {x: Int | ρ₁}
    Rec1 = catena_types:trecord([{x, catena_types:tcon(integer)}], 1),

    % After substitution: {x: Int | ρ₂}
    Result = catena_type_subst:apply(S, Rec1),
    ?assertMatch({trecord, [{x, _}], 2}, Result),

    % Test chaining: First apply S1={ρ₁→ρ₂}, then S2={ρ₂→ρ₃}
    % Row variable substitution is shallow, so we need two applications
    % or composition to get full chain resolution
    S1 = catena_type_subst:singleton(1, catena_types:tvar(2)),
    S2 = catena_type_subst:singleton(2, catena_types:tvar(3)),

    % Apply S1 first: {x: Int | ρ₁} -> {x: Int | ρ₂}
    Result2a = catena_type_subst:apply(S1, Rec1),
    ?assertMatch({trecord, [{x, _}], 2}, Result2a),

    % Then apply S2: {x: Int | ρ₂} -> {x: Int | ρ₃}
    Result2b = catena_type_subst:apply(S2, Result2a),
    ?assertMatch({trecord, [{x, _}], 3}, Result2b),

    % Identity: S = {ρ₁ ↦ ρ₁}
    SIdentity = catena_type_subst:singleton(1, catena_types:tvar(1)),
    ResultIdentity = catena_type_subst:apply(SIdentity, Rec1),
    ?assertEqual(Rec1, ResultIdentity).

test_row_var_to_closed() ->
    % Substituting row variable with 'closed'
    % This happens when we know the record has no more fields

    % S = {ρ₁ ↦ {}}  (empty closed record)
    S1 = catena_type_subst:singleton(
        1,
        catena_types:trecord([], closed)
    ),

    OpenRec = catena_types:trecord([{x, catena_types:tcon(integer)}], 1),
    Result1 = catena_type_subst:apply(S1, OpenRec),
    ?assertMatch({trecord, [{x, _}], closed}, Result1),

    % S = {ρ₁ ↦ {y: String}}  (closed record with field)
    S2 = catena_type_subst:singleton(
        1,
        catena_types:trecord([{y, catena_types:tcon(string)}], closed)
    ),

    Result2 = catena_type_subst:apply(S2, OpenRec),
    ?assertMatch({trecord, [{x, _}], closed}, Result2),

    % Test that closed stays closed
    ClosedRec = catena_types:trecord([{a, catena_types:tcon(atom)}], closed),
    ResultClosed = catena_type_subst:apply(S1, ClosedRec),
    ?assertEqual(ClosedRec, ResultClosed).

test_row_var_in_nested_records() ->
    % Test row variables in nested record types
    % {outer: {inner: Int | ρ₁} | ρ₂}

    InnerRec = catena_types:trecord([{inner, catena_types:tcon(integer)}], 1),
    OuterRec = catena_types:trecord([{outer, InnerRec}], 2),

    % Substitute inner row variable
    S1 = catena_type_subst:singleton(1, catena_types:tvar(3)),
    Result1 = catena_type_subst:apply(S1, OuterRec),
    ?assertMatch({trecord, [{outer, {trecord, [{inner, _}], 3}}], 2}, Result1),

    % Substitute outer row variable
    S2 = catena_type_subst:singleton(2, catena_types:tvar(4)),
    Result2 = catena_type_subst:apply(S2, OuterRec),
    ?assertMatch({trecord, [{outer, {trecord, _, 1}}], 4}, Result2),

    % Substitute both
    S3 = catena_type_subst:extend(S1, 2, catena_types:tvar(4)),
    Result3 = catena_type_subst:apply(S3, OuterRec),
    ?assertMatch({trecord, [{outer, {trecord, _, 3}}], 4}, Result3),

    % Close inner
    S4 = catena_type_subst:singleton(1, catena_types:trecord([], closed)),
    Result4 = catena_type_subst:apply(S4, OuterRec),
    ?assertMatch({trecord, [{outer, {trecord, _, closed}}], 2}, Result4).

test_multiple_row_vars() ->
    % Test multiple independent row variables in the same type
    % For example in a tuple of records: ({a: Int | ρ₁}, {b: String | ρ₂})

    Rec1 = catena_types:trecord([{a, catena_types:tcon(integer)}], 1),
    Rec2 = catena_types:trecord([{b, catena_types:tcon(string)}], 2),
    TupleType = catena_types:ttuple([Rec1, Rec2]),

    % Substitute only ρ₁
    S1 = catena_type_subst:singleton(1, catena_types:tvar(3)),
    Result1 = catena_type_subst:apply(S1, TupleType),
    ?assertMatch({ttuple, [
        {trecord, [{a, _}], 3},
        {trecord, [{b, _}], 2}
    ]}, Result1),

    % Substitute only ρ₂
    S2 = catena_type_subst:singleton(2, catena_types:tvar(4)),
    Result2 = catena_type_subst:apply(S2, TupleType),
    ?assertMatch({ttuple, [
        {trecord, [{a, _}], 1},
        {trecord, [{b, _}], 4}
    ]}, Result2),

    % Substitute both
    S3 = catena_type_subst:extend(S1, 2, catena_types:tvar(4)),
    Result3 = catena_type_subst:apply(S3, TupleType),
    ?assertMatch({ttuple, [
        {trecord, [{a, _}], 3},
        {trecord, [{b, _}], 4}
    ]}, Result3),

    % Close both
    S4 = #{
        1 => catena_types:trecord([], closed),
        2 => catena_types:trecord([], closed)
    },
    Result4 = catena_type_subst:apply(S4, TupleType),
    ?assertMatch({ttuple, [
        {trecord, [{a, _}], closed},
        {trecord, [{b, _}], closed}
    ]}, Result4).

test_row_var_composition() ->
    % Test substitution composition with row variables
    % S1 = {ρ₁ ↦ ρ₂}, S2 = {ρ₂ ↦ ρ₃}
    % compose(S2, S1) should give {ρ₁ ↦ ρ₃, ρ₂ ↦ ρ₃}

    S1 = catena_type_subst:singleton(1, catena_types:tvar(2)),
    S2 = catena_type_subst:singleton(2, catena_types:tvar(3)),

    SComposed = catena_type_subst:compose(S2, S1),

    Rec = catena_types:trecord([{x, catena_types:tcon(integer)}], 1),
    Result = catena_type_subst:apply(SComposed, Rec),
    ?assertMatch({trecord, [{x, _}], 3}, Result),

    % S1 = {ρ₁ ↦ ρ₂}, S2 = {ρ₂ ↦ closed}
    S3 = catena_type_subst:singleton(1, catena_types:tvar(2)),
    S4 = catena_type_subst:singleton(2, catena_types:trecord([], closed)),

    SComposed2 = catena_type_subst:compose(S4, S3),
    Result2 = catena_type_subst:apply(SComposed2, Rec),
    ?assertMatch({trecord, [{x, _}], closed}, Result2).

test_row_var_type_vars() ->
    % Test that type_vars correctly extracts row variables

    % Open record with row variable
    OpenRec = catena_types:trecord([{x, catena_types:tcon(integer)}], 1),
    Vars1 = catena_types:type_vars(OpenRec),
    ?assertEqual([1], lists:sort(sets:to_list(Vars1))),

    % Closed record has no row variable
    ClosedRec = catena_types:trecord([{x, catena_types:tcon(integer)}], closed),
    Vars2 = catena_types:type_vars(ClosedRec),
    ?assertEqual([], sets:to_list(Vars2)),

    % Record with type variable in field and row variable
    RecWithTVar = catena_types:trecord([{x, catena_types:tvar(2)}], 1),
    Vars3 = catena_types:type_vars(RecWithTVar),
    ?assertEqual([1, 2], lists:sort(sets:to_list(Vars3))),

    % Nested records with multiple row variables
    InnerRec = catena_types:trecord([{inner, catena_types:tvar(3)}], 1),
    OuterRec = catena_types:trecord([{outer, InnerRec}], 2),
    Vars4 = catena_types:type_vars(OuterRec),
    ?assertEqual([1, 2, 3], lists:sort(sets:to_list(Vars4))).

test_row_var_pretty_printing() ->
    % Test pretty-printing of records with row variables

    % Open record
    OpenRec = catena_types:trecord([{x, catena_types:tcon(integer)}], 1),
    PP1 = catena_type_pp:pp_type(OpenRec),
    ?assert(is_list(PP1)),
    ?assert(length(PP1) > 0),

    % Closed record
    ClosedRec = catena_types:trecord([{x, catena_types:tcon(integer)}], closed),
    PP2 = catena_type_pp:pp_type(ClosedRec),
    ?assert(is_list(PP2)),
    ?assert(length(PP2) > 0),

    % Record with multiple fields and row variable
    MultiFieldRec = catena_types:trecord([
        {x, catena_types:tcon(integer)},
        {y, catena_types:tcon(string)},
        {z, catena_types:tcon(float)}
    ], 5),
    PP3 = catena_type_pp:pp_type(MultiFieldRec),
    ?assert(is_list(PP3)),
    ?assert(length(PP3) > 10),

    % Nested record with row variables
    InnerRec = catena_types:trecord([{inner, catena_types:tcon(atom)}], 2),
    OuterRec = catena_types:trecord([{outer, InnerRec}], 3),
    PP4 = catena_type_pp:pp_type(OuterRec),
    ?assert(is_list(PP4)),
    ?assert(length(PP4) > 5).

test_row_var_field_merging() ->
    % Test scenarios that might involve field merging (conceptually)
    % Even though we don't actually merge, test the substitution behavior

    % S = {ρ₁ ↦ {y: String}}
    % Applied to {x: Int | ρ₁} conceptually gives {x: Int, y: String}
    S = catena_type_subst:singleton(
        1,
        catena_types:trecord([{y, catena_types:tcon(string)}], closed)
    ),

    OpenRec = catena_types:trecord([{x, catena_types:tcon(integer)}], 1),
    Result = catena_type_subst:apply(S, OpenRec),

    % After substitution, row variable becomes closed
    ?assertMatch({trecord, [{x, _}], closed}, Result),

    % With multiple fields on both sides
    S2 = catena_type_subst:singleton(
        1,
        catena_types:trecord([
            {y, catena_types:tcon(string)},
            {z, catena_types:tcon(float)}
        ], closed)
    ),

    OpenRec2 = catena_types:trecord([
        {a, catena_types:tcon(atom)},
        {b, catena_types:tcon(boolean)}
    ], 1),

    Result2 = catena_type_subst:apply(S2, OpenRec2),
    ?assertMatch({trecord, [{a, _}, {b, _}], closed}, Result2),

    % Empty record extension: {| ρ₁} with S = {ρ₁ ↦ {x: Int}}
    EmptyRec = catena_types:trecord([], 1),
    Result3 = catena_type_subst:apply(S, EmptyRec),
    ?assertMatch({trecord, [], closed}, Result3).

%%====================================================================
%% Fresh Variable Generation Edge Cases
%%====================================================================

fresh_var_edge_cases_test_() ->
    [
      ?_test(test_counter_wraparound()),
      ?_test(test_many_fresh_variables())
    ].

test_counter_wraparound() ->
    % Test that counter doesn't overflow (Erlang handles big integers)
    % Generate many variables and check uniqueness

    State0 = catena_infer_state:new(),

    % Generate 10,000 variables
    {Vars, _StateFinal} = lists:foldl(
        fun(_, {AccVars, AccState}) ->
            {Var, NewState} = catena_types:fresh_var(AccState),
            {[Var | AccVars], NewState}
        end,
        {[], State0},
        lists:seq(1, 10000)
    ),

    % All should be unique
    UniqueVars = lists:usort(Vars),
    ?assertEqual(10000, length(UniqueVars)),

    % IDs should be sequential (reversed due to cons)
    {tvar, FirstId} = lists:last(Vars),  % First generated
    ?assertEqual(1, FirstId),
    {tvar, LastId} = hd(Vars),  % Last generated
    ?assertEqual(10000, LastId).

test_many_fresh_variables() ->
    % Stress test: generate 50,000 variables
    State0 = catena_infer_state:new(),

    % This should not crash or slow down significantly
    Count = 50000,
    {_Vars, StateFinal} = lists:foldl(
        fun(_, {AccVars, AccState}) ->
            {Var, NewState} = catena_types:fresh_var(AccState),
            {[Var | AccVars], NewState}
        end,
        {[], State0},
        lists:seq(1, Count)
    ),

    % Counter should be at expected value (at least Count, implementation may vary)
    {_NextVar, StateNext} = catena_types:fresh_var(StateFinal),
    Counter = catena_infer_state:get_counter(StateNext),
    ?assert(Counter >= Count, io_lib:format("Counter ~p should be >= ~p", [Counter, Count])).

%%====================================================================
%% Pretty-Printing Edge Cases
%%====================================================================

pretty_printing_edge_cases_test_() ->
    [
      ?_test(test_empty_structures()),
      ?_test(test_special_atom_names()),
      ?_test(test_very_long_type())
    ].

test_empty_structures() ->
    % Empty tuple
    EmptyTuple = catena_types:ttuple([]),
    ?assertEqual("()", catena_type_pp:pp_type(EmptyTuple)),

    % Empty variant (edge case, may not be semantically valid)
    EmptyVariant = catena_types:tvariant([]),
    PP = catena_type_pp:pp_type(EmptyVariant),
    ?assert(is_list(PP)),

    % Closed record with no fields
    EmptyRecord = catena_types:trecord([], closed),
    ?assertEqual("{}", catena_type_pp:pp_type(EmptyRecord)).

test_special_atom_names() ->
    % Test atoms with special characters
    SpecialCon = catena_types:tcon('Foo-Bar'),
    PP = catena_type_pp:pp_type(SpecialCon),
    ?assertEqual("Foo-Bar", PP),

    % Atom with spaces (quoted)
    QuotedCon = catena_types:tcon('My Type'),
    PP2 = catena_type_pp:pp_type(QuotedCon),
    ?assertEqual("My Type", PP2).

test_very_long_type() ->
    % Create type with many fields/constructors
    ManyFields = [{list_to_atom("field_" ++ integer_to_list(I)),
                   catena_types:tcon(integer)} || I <- lists:seq(1, 50)],

    LargeRecord = catena_types:trecord(ManyFields, closed),

    % Should produce very long string
    PP = catena_type_pp:pp_type(LargeRecord),
    ?assert(length(PP) > 500),

    % Should contain all field names
    ?assert(string:find(PP, "field_1") =/= nomatch),
    ?assert(string:find(PP, "field_50") =/= nomatch).

%%====================================================================
%% Type Scheme Edge Cases
%%====================================================================

type_scheme_edge_cases_test_() ->
    [
      ?_test(test_generalize_with_no_free_vars()),
      ?_test(test_instantiate_empty_quantifiers()),
      ?_test(test_nested_quantification())
    ].

test_generalize_with_no_free_vars() ->
    % Generalize a concrete type (no variables)
    ConcreteType = catena_types:tfun(
        catena_types:tcon(integer),
        catena_types:tcon(string),
        catena_types:empty_effects()
    ),

    EmptyEnv = catena_type_env:empty(),
    Scheme = catena_type_scheme:generalize(ConcreteType, catena_type_env:ftv_env(EmptyEnv)),

    % Should be monomorphic (no quantified variables)
    ?assertMatch({mono, _}, Scheme).

test_instantiate_empty_quantifiers() ->
    % Create polymorphic scheme with empty quantifier list
    % (should behave like monomorphic)

    State = catena_infer_state:new(),
    Type = catena_types:tcon(integer),
    Scheme = catena_type_scheme:poly([], Type),

    % Instantiate should return original type
    {Result, _Constraints, _NewState} = catena_type_scheme:instantiate(Scheme, State),
    ?assertEqual(Type, Result).

test_nested_quantification() ->
    % Generalize twice (should only quantify once)
    State0 = catena_infer_state:new(),

    {{tvar, Var1}, _State1} = catena_types:fresh_var(State0),
    Type = catena_types:tvar(Var1),

    EmptyEnv = catena_type_env:empty(),
    EnvFtv = catena_type_env:ftv_env(EmptyEnv),

    % First generalization
    Scheme1 = catena_type_scheme:generalize(Type, EnvFtv),
    ?assertMatch({poly, [_], _}, Scheme1),

    % Can't generalize a scheme (only types)
    % This is a type system invariant
    ok.

%%====================================================================
%% Environment Edge Cases
%%====================================================================

environment_edge_cases_test_() ->
    [
      ?_test(test_remove_nonexistent()),
      ?_test(test_shadow_and_remove()),
      ?_test(test_empty_environment_ftv())
    ].

test_remove_nonexistent() ->
    Env = catena_type_env:singleton(x, catena_type_scheme:mono(catena_types:tcon(integer))),

    % Remove variable that doesn't exist (should be no-op)
    Env2 = catena_type_env:remove(Env, nonexistent),

    % Should still be able to look up original
    ?assertMatch({ok, _}, catena_type_env:lookup(Env2, x)),
    ?assertEqual(none, catena_type_env:lookup(Env2, nonexistent)).

test_shadow_and_remove() ->
    IntScheme = catena_type_scheme:mono(catena_types:tcon(integer)),
    StringScheme = catena_type_scheme:mono(catena_types:tcon(string)),

    Env1 = catena_type_env:singleton(x, IntScheme),
    Env2 = catena_type_env:extend(Env1, x, StringScheme),  % Shadow

    % Should get string scheme
    ?assertEqual({ok, StringScheme}, catena_type_env:lookup(Env2, x)),

    % Remove x (removes shadow, doesn't restore original)
    Env3 = catena_type_env:remove(Env2, x),
    ?assertEqual(none, catena_type_env:lookup(Env3, x)).

test_empty_environment_ftv() ->
    Empty = catena_type_env:empty(),
    Ftv = catena_type_env:ftv_env(Empty),

    % Should be empty set
    ?assertEqual([], sets:to_list(Ftv)).

%%====================================================================
%% Integration: Error Scenarios
%%====================================================================

integration_error_test_() ->
    [
      ?_test(test_complex_substitution_chain()),
      ?_test(test_generalize_after_large_substitution())
    ].

test_complex_substitution_chain() ->
    % Build chain: α₁ → α₂ → α₃ → Int
    S1 = catena_type_subst:singleton(1, catena_types:tvar(2)),
    S2 = catena_type_subst:singleton(2, catena_types:tvar(3)),
    S3 = catena_type_subst:singleton(3, catena_types:tcon(integer)),

    % Compose all
    S12 = catena_type_subst:compose(S2, S1),
    S123 = catena_type_subst:compose(S3, S12),

    % Apply to α₁
    Result = catena_type_subst:apply(S123, catena_types:tvar(1)),

    % Should resolve to Int
    ?assertEqual(catena_types:tcon(integer), Result).

test_generalize_after_large_substitution() ->
    % Create large substitution
    LargeSubst = lists:foldl(
        fun(I, Acc) ->
            catena_type_subst:extend(Acc, I, catena_types:tcon(integer))
        end,
        catena_type_subst:empty(),
        lists:seq(1, 100)
    ),

    % Create type with many variables
    ManyVarType = catena_types:ttuple([
        catena_types:tvar(I) || I <- lists:seq(1, 100)
    ]),

    % Apply substitution (all vars become Int)
    SubstitutedType = catena_type_subst:apply(LargeSubst, ManyVarType),

    % Generalize (should be monomorphic, no free vars)
    EmptyEnv = catena_type_env:empty(),
    Scheme = catena_type_scheme:generalize(SubstitutedType, catena_type_env:ftv_env(EmptyEnv)),

    ?assertMatch({mono, _}, Scheme).

%%====================================================================
%% Error Constructor Tests
%%====================================================================

error_constructor_test_() ->
    [
      ?_test(test_error_constructors())
    ].

test_error_constructors() ->
    % Test that all error constructors return correctly structured errors
    ?assertMatch({circular_substitution, 42},
                 catena_type_error:circular_substitution(42)),

    ?assertMatch({substitution_depth_exceeded, 150, 100},
                 catena_type_error:substitution_depth_exceeded(150, 100)),

    ?assertMatch({substitution_too_large, 10001, 10000},
                 catena_type_error:substitution_too_large(10001, 10000)),

    ?assertMatch({duplicate_record_fields, [x, y]},
                 catena_type_error:duplicate_record_fields([x, y])),

    ?assertMatch({duplicate_variant_constructors, ['Some', 'None']},
                 catena_type_error:duplicate_variant_constructors(['Some', 'None'])),

    IntType = catena_types:tcon(integer),
    StringType = catena_types:tcon(string),
    ?assertMatch({unification_error, _, _},
                 catena_type_error:unification_error(IntType, StringType)),

    ?assertMatch({occurs_check, 1, _},
                 catena_type_error:occurs_check(1, IntType)),

    ?assertMatch({type_depth_exceeded, 200, 100},
                 catena_type_error:type_depth_exceeded(200, 100)),

    ?assertMatch({unbound_variable, foo},
                 catena_type_error:unbound_variable(foo)),

    ?assertMatch({environment_too_large, 5001, 5000},
                 catena_type_error:environment_too_large(5001, 5000)),

    ?assertMatch({arity_mismatch, 'List', 1, 2},
                 catena_type_error:arity_mismatch('List', 1, 2)),

    Args = [IntType, StringType],
    ?assertMatch({invalid_type_application, _, _},
                 catena_type_error:invalid_type_application(IntType, Args)),

    Pure = catena_types:empty_effects(),
    Impure = catena_types:singleton_effect(io),
    ?assertMatch({effect_mismatch, _, _},
                 catena_type_error:effect_mismatch(Pure, Impure)),

    RecordType = catena_types:trecord([{x, IntType}], closed),
    ?assertMatch({missing_field, y, _},
                 catena_type_error:missing_field(y, RecordType)).

%%====================================================================
%% Error Formatting Tests
%%====================================================================

error_formatting_test_() ->
    [
      ?_test(test_format_circular_substitution()),
      ?_test(test_format_substitution_depth_exceeded()),
      ?_test(test_format_substitution_too_large()),
      ?_test(test_format_duplicate_record_fields()),
      ?_test(test_format_duplicate_variant_constructors()),
      ?_test(test_format_unification_error()),
      ?_test(test_format_occurs_check()),
      ?_test(test_format_type_depth_exceeded()),
      ?_test(test_format_unbound_variable()),
      ?_test(test_format_environment_too_large()),
      ?_test(test_format_arity_mismatch()),
      ?_test(test_format_invalid_type_application()),
      ?_test(test_format_effect_mismatch_pure_to_impure()),
      ?_test(test_format_effect_mismatch_impure_to_pure()),
      ?_test(test_format_effect_mismatch_different_effects()),
      ?_test(test_format_missing_field()),
      ?_test(test_format_unknown_error())
      % test_format_with_location() excluded - requires catena_location module
    ].

test_format_circular_substitution() ->
    Error = catena_type_error:circular_substitution(42),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "Circular") > 0),
    ?assert(string:str(Msg, "α42") > 0).

test_format_substitution_depth_exceeded() ->
    Error = catena_type_error:substitution_depth_exceeded(150, 100),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "substitution depth") > 0),
    ?assert(string:str(Msg, "150") > 0),
    ?assert(string:str(Msg, "100") > 0).

test_format_unification_error() ->
    Type1 = catena_types:tcon(integer),
    Type2 = catena_types:tcon(string),
    Error = catena_type_error:unification_error(Type1, Type2),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "unification") > 0),
    ?assert(string:str(Msg, "integer") > 0),
    ?assert(string:str(Msg, "string") > 0).

test_format_occurs_check() ->
    Type = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tcon(integer),
        catena_types:empty_effects()
    ),
    Error = catena_type_error:occurs_check(1, Type),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "Occurs check") > 0),
    ?assert(string:str(Msg, "α1") > 0),
    ?assert(string:str(Msg, "infinite") > 0).

test_format_type_depth_exceeded() ->
    Error = catena_type_error:type_depth_exceeded(200, 100),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "depth") > 0),
    ?assert(string:str(Msg, "200") > 0),
    ?assert(string:str(Msg, "100") > 0).

test_format_unbound_variable() ->
    Error = catena_type_error:unbound_variable(foo),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "Unbound") > 0),
    ?assert(string:str(Msg, "foo") > 0).

test_format_arity_mismatch() ->
    Error = catena_type_error:arity_mismatch('List', 1, 2),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "Arity mismatch") > 0),
    ?assert(string:str(Msg, "List") > 0).

test_format_substitution_too_large() ->
    Error = catena_type_error:substitution_too_large(10001, 10000),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "substitution too large") > 0),
    ?assert(string:str(Msg, "10001") > 0),
    ?assert(string:str(Msg, "10000") > 0).

test_format_duplicate_record_fields() ->
    Error = catena_type_error:duplicate_record_fields([x, y, z]),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "Duplicate field") > 0),
    ?assert(string:str(Msg, "x") > 0),
    ?assert(string:str(Msg, "y") > 0),
    ?assert(string:str(Msg, "z") > 0).

test_format_duplicate_variant_constructors() ->
    Error = catena_type_error:duplicate_variant_constructors(['Some', 'None', 'Other']),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "Duplicate constructor") > 0),
    ?assert(string:str(Msg, "Some") > 0),
    ?assert(string:str(Msg, "None") > 0),
    ?assert(string:str(Msg, "Other") > 0).

test_format_environment_too_large() ->
    Error = catena_type_error:environment_too_large(5001, 5000),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "environment too large") > 0),
    ?assert(string:str(Msg, "5001") > 0),
    ?assert(string:str(Msg, "5000") > 0).

test_format_invalid_type_application() ->
    Constructor = catena_types:tcon('Maybe'),
    Args = [catena_types:tcon(integer), catena_types:tcon(string)],
    Error = catena_type_error:invalid_type_application(Constructor, Args),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "Invalid type application") > 0),
    ?assert(string:str(Msg, "Maybe") > 0),
    ?assert(string:str(Msg, "2") > 0).  % 2 arguments

test_format_effect_mismatch_pure_to_impure() ->
    Pure = catena_types:empty_effects(),
    Impure = catena_types:singleton_effect(io),
    Error = catena_type_error:effect_mismatch(Pure, Impure),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "Effect mismatch") > 0),
    ?assert(string:str(Msg, "pure") > 0),
    ?assert(string:str(Msg, "{io}") > 0).

test_format_effect_mismatch_impure_to_pure() ->
    Pure = catena_types:empty_effects(),
    Impure = catena_types:singleton_effect(network),
    Error = catena_type_error:effect_mismatch(Impure, Pure),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "Effect mismatch") > 0),
    ?assert(string:str(Msg, "pure") > 0),
    ?assert(string:str(Msg, "{network}") > 0).

test_format_effect_mismatch_different_effects() ->
    Effects1 = catena_types:union_effects(
        catena_types:singleton_effect(io),
        catena_types:singleton_effect(file)
    ),
    Effects2 = catena_types:union_effects(
        catena_types:singleton_effect(network),
        catena_types:singleton_effect(database)
    ),
    Error = catena_type_error:effect_mismatch(Effects1, Effects2),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "Effect mismatch") > 0),
    ?assert(string:str(Msg, "file") > 0 orelse string:str(Msg, "io") > 0),
    ?assert(string:str(Msg, "network") > 0 orelse string:str(Msg, "database") > 0).

test_format_unknown_error() ->
    % Test the catch-all case for unknown errors
    UnknownError = {unknown_error_type, some, data},
    Msg = catena_type_error:format_error(UnknownError),
    ?assert(string:str(Msg, "Unknown type error") > 0),
    ?assert(string:str(Msg, "unknown_error_type") > 0).

test_format_missing_field() ->
    RecordType = catena_types:trecord([
        {x, catena_types:tcon(integer)}
    ], closed),
    Error = catena_type_error:missing_field(y, RecordType),
    Msg = catena_type_error:format_error(Error),
    ?assert(string:str(Msg, "Missing field") > 0),
    ?assert(string:str(Msg, "y") > 0).

test_format_with_location() ->
    Loc = catena_location:new(10, 5),
    Error = catena_type_error:unbound_variable(foo),
    Msg = catena_type_error:format_error_with_location(Loc, Error),
    ?assert(string:str(Msg, "10:5") > 0),
    ?assert(string:str(Msg, "foo") > 0).
