%%%-------------------------------------------------------------------
%%% @doc Unit Tests for catena_type_subst module
%%% @end
%%%-------------------------------------------------------------------
-module(catena_type_subst_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%%====================================================================
%% Substitution Construction Tests
%%====================================================================

construction_test_() ->
    [
      ?_test(test_empty()),
      ?_test(test_singleton()),
      ?_test(test_lookup()),
      ?_test(test_extend())
    ].

test_empty() ->
    Empty = catena_type_subst:empty(),
    ?assertEqual(#{}, Empty),
    ?assertEqual([], catena_type_subst:domain(Empty)),
    ?assertEqual([], catena_type_subst:range(Empty)).

test_singleton() ->
    S = catena_type_subst:singleton(1, catena_types:tcon(integer)),
    ?assertEqual(#{1 => {tcon, integer}}, S),
    ?assertEqual([1], catena_type_subst:domain(S)),
    ?assertEqual([{tcon, integer}], catena_type_subst:range(S)).

test_lookup() ->
    S = catena_type_subst:singleton(1, catena_types:tcon(integer)),

    % Variable in substitution
    ?assertEqual({ok, catena_types:tcon(integer)},
                 catena_type_subst:lookup(S, 1)),

    % Variable not in substitution
    ?assertEqual(none, catena_type_subst:lookup(S, 2)).

test_extend() ->
    S1 = catena_type_subst:empty(),
    S2 = catena_type_subst:extend(S1, 1, catena_types:tcon(integer)),
    S3 = catena_type_subst:extend(S2, 2, catena_types:tcon(string)),

    ?assertEqual({ok, catena_types:tcon(integer)},
                 catena_type_subst:lookup(S3, 1)),
    ?assertEqual({ok, catena_types:tcon(string)},
                 catena_type_subst:lookup(S3, 2)),

    % Extend can override
    S4 = catena_type_subst:extend(S3, 1, catena_types:tcon(boolean)),
    ?assertEqual({ok, catena_types:tcon(boolean)},
                 catena_type_subst:lookup(S4, 1)).

%%====================================================================
%% Substitution Application Tests
%%====================================================================

application_test_() ->
    [
      ?_test(test_apply_to_var()),
      ?_test(test_apply_to_con()),
      ?_test(test_apply_to_fun()),
      ?_test(test_apply_to_app()),
      ?_test(test_apply_to_tuple()),
      ?_test(test_apply_to_record())
    ].

test_apply_to_var() ->
    S = catena_type_subst:singleton(1, catena_types:tcon(integer)),

    % Variable in substitution
    Var1 = catena_types:tvar(1),
    ?assertEqual(catena_types:tcon(integer),
                 catena_type_subst:apply(S, Var1)),

    % Variable not in substitution
    Var2 = catena_types:tvar(2),
    ?assertEqual(Var2, catena_type_subst:apply(S, Var2)).

test_apply_to_con() ->
    S = catena_type_subst:singleton(1, catena_types:tcon(integer)),

    % Constants are unchanged
    Con = catena_types:tcon(string),
    ?assertEqual(Con, catena_type_subst:apply(S, Con)).

test_apply_to_fun() ->
    S = catena_type_subst:singleton(1, catena_types:tcon(integer)),

    % α -> β becomes Int -> β
    Fun = catena_types:tfun(
        catena_types:tvar(1),  % α
        catena_types:tvar(2),  % β
        catena_types:empty_effects()
    ),

    Expected = catena_types:tfun(
        catena_types:tcon(integer),  % α substituted with Int
        catena_types:tvar(2),        % β unchanged
        catena_types:empty_effects()
    ),

    ?assertEqual(Expected, catena_type_subst:apply(S, Fun)).

test_apply_to_app() ->
    S = catena_type_subst:singleton(1, catena_types:tcon(integer)),

    % List α becomes List Int
    ListAlpha = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(1)]
    ),

    Expected = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tcon(integer)]
    ),

    ?assertEqual(Expected, catena_type_subst:apply(S, ListAlpha)).

test_apply_to_tuple() ->
    S = catena_type_subst:singleton(1, catena_types:tcon(integer)),

    % (α, String) becomes (Int, String)
    Tuple = catena_types:ttuple([
        catena_types:tvar(1),
        catena_types:tcon(string)
    ]),

    Expected = catena_types:ttuple([
        catena_types:tcon(integer),
        catena_types:tcon(string)
    ]),

    ?assertEqual(Expected, catena_type_subst:apply(S, Tuple)).

test_apply_to_record() ->
    S = catena_type_subst:singleton(1, catena_types:tcon(integer)),

    % {x: α, y: String | ρ} becomes {x: Int, y: String | ρ}
    Record = catena_types:trecord(
        [{x, catena_types:tvar(1)}, {y, catena_types:tcon(string)}],
        2  % Row variable
    ),

    Expected = catena_types:trecord(
        [{x, catena_types:tcon(integer)}, {y, catena_types:tcon(string)}],
        2  % Row variable unchanged
    ),

    ?assertEqual(Expected, catena_type_subst:apply(S, Record)).

%%====================================================================
%% Substitution Composition Tests
%%====================================================================

composition_test_() ->
    [
      ?_test(test_compose_empty()),
      ?_test(test_compose_disjoint()),
      ?_test(test_compose_chain()),
      ?_test(test_compose_override())
    ].

test_compose_empty() ->
    S1 = catena_type_subst:singleton(1, catena_types:tcon(integer)),
    Empty = catena_type_subst:empty(),

    % S1 ∘ empty = S1
    Result1 = catena_type_subst:compose(S1, Empty),
    ?assertEqual(S1, Result1),

    % empty ∘ S1 = S1
    Result2 = catena_type_subst:compose(Empty, S1),
    ?assertEqual(S1, Result2).

test_compose_disjoint() ->
    % S1 = {1 ↦ Int}
    S1 = catena_type_subst:singleton(1, catena_types:tcon(integer)),
    % S2 = {2 ↦ String}
    S2 = catena_type_subst:singleton(2, catena_types:tcon(string)),

    % S2 ∘ S1 = {1 ↦ Int, 2 ↦ String}
    Result = catena_type_subst:compose(S2, S1),

    ?assertEqual({ok, catena_types:tcon(integer)},
                 catena_type_subst:lookup(Result, 1)),
    ?assertEqual({ok, catena_types:tcon(string)},
                 catena_type_subst:lookup(Result, 2)).

test_compose_chain() ->
    % S1 = {1 ↦ α₂}
    S1 = catena_type_subst:singleton(1, catena_types:tvar(2)),
    % S2 = {2 ↦ Int}
    S2 = catena_type_subst:singleton(2, catena_types:tcon(integer)),

    % S2 ∘ S1: Apply S1 first, then S2
    % {1 ↦ α₂} then {2 ↦ Int}
    % Result: {1 ↦ Int, 2 ↦ Int}
    Result = catena_type_subst:compose(S2, S1),

    % α₁ should map to Int (via α₂)
    ?assertEqual({ok, catena_types:tcon(integer)},
                 catena_type_subst:lookup(Result, 1)),
    % α₂ should map to Int directly
    ?assertEqual({ok, catena_types:tcon(integer)},
                 catena_type_subst:lookup(Result, 2)).

test_compose_override() ->
    % S1 = {1 ↦ α₂}
    S1 = catena_type_subst:singleton(1, catena_types:tvar(2)),
    % S2 = {1 ↦ Int} (overrides S1's binding for 1)
    S2 = catena_type_subst:singleton(1, catena_types:tcon(integer)),

    % S2 ∘ S1: S2's binding takes precedence
    Result = catena_type_subst:compose(S2, S1),

    % α₁ should map to Int (S2's binding wins)
    ?assertEqual({ok, catena_types:tcon(integer)},
                 catena_type_subst:lookup(Result, 1)).

%%====================================================================
%% Substitution Laws (Properties) Tests
%%====================================================================

laws_test_() ->
    [
      ?_test(test_identity_law()),
      ?_test(test_composition_apply()),
      ?_test(test_idempotent_application())
    ].

test_identity_law() ->
    % Applying empty substitution is identity
    Empty = catena_type_subst:empty(),

    Type1 = catena_types:tvar(1),
    ?assertEqual(Type1, catena_type_subst:apply(Empty, Type1)),

    Type2 = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tcon(integer),
        catena_types:empty_effects()
    ),
    ?assertEqual(Type2, catena_type_subst:apply(Empty, Type2)).

test_composition_apply() ->
    % Apply (S2 ∘ S1) = Apply S2 (Apply S1 type)

    S1 = catena_type_subst:singleton(1, catena_types:tvar(2)),
    S2 = catena_type_subst:singleton(2, catena_types:tcon(integer)),

    Type = catena_types:tvar(1),

    % Method 1: Compose then apply
    Composed = catena_type_subst:compose(S2, S1),
    Result1 = catena_type_subst:apply(Composed, Type),

    % Method 2: Apply S1, then apply S2
    Intermediate = catena_type_subst:apply(S1, Type),
    Result2 = catena_type_subst:apply(S2, Intermediate),

    ?assertEqual(Result1, Result2),
    ?assertEqual(catena_types:tcon(integer), Result1).

test_idempotent_application() ->
    % Applying the same substitution twice should be idempotent
    S = catena_type_subst:singleton(1, catena_types:tcon(integer)),

    Type = catena_types:tvar(1),
    Applied1 = catena_type_subst:apply(S, Type),
    Applied2 = catena_type_subst:apply(S, Applied1),

    ?assertEqual(Applied1, Applied2),
    ?assertEqual(catena_types:tcon(integer), Applied2).

%%====================================================================
%% Complex Scenario Tests
%%====================================================================

complex_test_() ->
    [
      ?_test(test_nested_substitution()),
      ?_test(test_function_type_substitution())
    ].

test_nested_substitution() ->
    % Build complex substitution chain
    % S1 = {1 ↦ List α₂}
    S1 = catena_type_subst:singleton(1,
        catena_types:tapp(catena_types:tcon('List'), [catena_types:tvar(2)])),

    % S2 = {2 ↦ Int}
    S2 = catena_type_subst:singleton(2, catena_types:tcon(integer)),

    % Compose: S2 ∘ S1
    S = catena_type_subst:compose(S2, S1),

    % Apply to α₁
    Type = catena_types:tvar(1),
    Result = catena_type_subst:apply(S, Type),

    % Should get List Int
    Expected = catena_types:tapp(catena_types:tcon('List'),
                                [catena_types:tcon(integer)]),
    ?assertEqual(Expected, Result).

test_function_type_substitution() ->
    % Type: (α₁ -> α₂) -> List α₁ -> List α₂
    InnerFun = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tvar(2),
        catena_types:empty_effects()
    ),

    ListAlpha1 = catena_types:tapp(catena_types:tcon('List'), [catena_types:tvar(1)]),
    ListAlpha2 = catena_types:tapp(catena_types:tcon('List'), [catena_types:tvar(2)]),

    MapType = catena_types:tfun(
        InnerFun,
        catena_types:tfun(ListAlpha1, ListAlpha2, catena_types:empty_effects()),
        catena_types:empty_effects()
    ),

    % Substitution: {1 ↦ Int, 2 ↦ String}
    S1 = catena_type_subst:singleton(1, catena_types:tcon(integer)),
    S2 = catena_type_subst:singleton(2, catena_types:tcon(string)),
    S = catena_type_subst:compose(S2, S1),

    % Apply substitution
    Result = catena_type_subst:apply(S, MapType),

    % Expected: (Int -> String) -> List Int -> List String
    ExpectedInner = catena_types:tfun(
        catena_types:tcon(integer),
        catena_types:tcon(string),
        catena_types:empty_effects()
    ),
    ExpectedListInt = catena_types:tapp(catena_types:tcon('List'),
                                       [catena_types:tcon(integer)]),
    ExpectedListString = catena_types:tapp(catena_types:tcon('List'),
                                          [catena_types:tcon(string)]),
    Expected = catena_types:tfun(
        ExpectedInner,
        catena_types:tfun(ExpectedListInt, ExpectedListString,
                        catena_types:empty_effects()),
        catena_types:empty_effects()
    ),

    ?assertEqual(Expected, Result).

%%====================================================================
%% Size Limit Tests
%%====================================================================

size_limit_test_() ->
    [
      ?_test(test_extend_within_limit()),
      ?_test(test_compose_within_limit())
    ].

test_extend_within_limit() ->
    % Build a substitution up to a reasonable size (well under the limit)
    % Default limit is 10,000, so let's test with 100
    MaxSize = 100,
    Subst = lists:foldl(
        fun(I, Acc) ->
            catena_type_subst:extend(Acc, I, catena_types:tcon(integer))
        end,
        catena_type_subst:empty(),
        lists:seq(1, MaxSize)
    ),

    % Should succeed - verify we can look up a value
    ?assertEqual({ok, catena_types:tcon(integer)},
                 catena_type_subst:lookup(Subst, 50)).

test_compose_within_limit() ->
    % Create two substitutions that together are under the limit
    % Each with 50 mappings, total 100 (well under 10,000 limit)
    S1 = lists:foldl(
        fun(I, Acc) ->
            catena_type_subst:extend(Acc, I, catena_types:tcon(integer))
        end,
        catena_type_subst:empty(),
        lists:seq(1, 50)
    ),

    S2 = lists:foldl(
        fun(I, Acc) ->
            catena_type_subst:extend(Acc, I + 1000, catena_types:tcon(string))
        end,
        catena_type_subst:empty(),
        lists:seq(1, 50)
    ),

    % Compose should succeed
    Composed = catena_type_subst:compose(S2, S1),

    % Result should have mappings from both
    ?assertEqual({ok, catena_types:tcon(integer)},
                 catena_type_subst:lookup(Composed, 25)),
    ?assertEqual({ok, catena_types:tcon(string)},
                 catena_type_subst:lookup(Composed, 1025)).
