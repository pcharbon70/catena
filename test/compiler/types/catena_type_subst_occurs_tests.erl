%%%-------------------------------------------------------------------
%%% @doc Tests for Substitution Occurs Check Security
%%%
%%% Tests the occurs check functionality. The current design separates
%%% occurs checking from substitution construction - occurs_check/2
%%% detects infinite types, while extend/3 just builds substitutions.
%%% The unification layer is responsible for calling occurs_check.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_type_subst_occurs_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Occurs Check Functionality Tests
%%====================================================================

occurs_check_security_test_() ->
    [
      ?_test(test_occurs_check_detects_simple_infinite()),
      ?_test(test_occurs_check_detects_nested_infinite()),
      ?_test(test_occurs_check_allows_safe_types()),
      ?_test(test_extend_creates_substitution()),
      ?_test(test_singleton_creates_substitution()),
      ?_test(test_unification_prevents_infinite_types())
    ].

%% @doc Test that occurs_check detects α ≡ α
test_occurs_check_detects_simple_infinite() ->
    VarId = 123,
    TVar = {tvar, VarId},

    % Occurs check should detect α occurring in α
    ?assert(catena_type_subst:occurs_check(VarId, TVar)).

%% @doc Test that occurs_check detects α in (α -> α)
test_occurs_check_detects_nested_infinite() ->
    VarId = 456,
    TVar = {tvar, VarId},

    % Create α -> α (function type)
    InfiniteFunType = catena_types:tfun(TVar, TVar, catena_types:empty_effects()),

    % Occurs check should detect α occurring in the function type
    ?assert(catena_type_subst:occurs_check(VarId, InfiniteFunType)),

    % Also test List<α>
    ListType = {tapp, {tcon, list}, TVar},
    ?assert(catena_type_subst:occurs_check(VarId, ListType)).

%% @doc Test that occurs_check allows safe types
test_occurs_check_allows_safe_types() ->
    VarId = 111,

    % α doesn't occur in Int
    ?assertNot(catena_type_subst:occurs_check(VarId, {tcon, int})),

    % α doesn't occur in β (different variable)
    ?assertNot(catena_type_subst:occurs_check(VarId, {tvar, 222})),

    % α doesn't occur in β -> Int
    FunType = catena_types:tfun({tvar, 222}, {tcon, int}, catena_types:empty_effects()),
    ?assertNot(catena_type_subst:occurs_check(VarId, FunType)).

%% @doc Test that extend creates substitutions (doesn't do occurs check itself)
test_extend_creates_substitution() ->
    Subst = catena_type_subst:empty(),
    VarId = 111,

    % extend returns the map directly
    NewSubst = catena_type_subst:extend(Subst, VarId, {tcon, int}),
    ?assertMatch(#{111 := {tcon, int}}, NewSubst),

    % Can extend with another mapping
    Subst2 = catena_type_subst:extend(NewSubst, 222, {tvar, 333}),
    ?assertMatch(#{111 := {tcon, int}, 222 := {tvar, 333}}, Subst2).

%% @doc Test that singleton creates substitutions
test_singleton_creates_substitution() ->
    VarId = 333,

    % Singleton returns map directly
    Subst = catena_type_subst:singleton(VarId, {tcon, int}),
    ?assertMatch(#{333 := {tcon, int}}, Subst),

    % Lookup works
    ?assertEqual({ok, {tcon, int}}, catena_type_subst:lookup(Subst, VarId)).

%% @doc Test that unification layer prevents infinite types using occurs check
test_unification_prevents_infinite_types() ->
    State0 = catena_infer_state:new(),

    % Try to unify α with α -> Int (should fail through unification)
    {{tvar, VarId}, State1} = catena_types:fresh_var(State0),
    TVar = {tvar, VarId},
    FunType = catena_types:tfun(TVar, {tcon, int}, catena_types:empty_effects()),

    % Unification should detect occurs check and fail
    case catena_infer_unify:unify(TVar, FunType, State1) of
        {ok, _Subst, _State} ->
            ?assert(false, "Unification should prevent infinite type α ≡ α -> Int");
        {error, Error, _State} ->
            % Should be an occurs check error
            ?assertMatch({occurs_check, _, _}, Error)
    end.

%%====================================================================
%% Integration Security Tests
%%====================================================================

integration_security_test_() ->
    [
      ?_test(test_complex_occurs_check_scenarios())
    ].

%% @doc Test occurs check with complex nested types
test_complex_occurs_check_scenarios() ->
    VarId = 555,

    % α ≡ List<α> - α occurs in the type argument
    ListWithTVar = {tapp, {tcon, list}, {tvar, VarId}},
    ?assert(catena_type_subst:occurs_check(VarId, ListWithTVar)),

    % α ≡ (α -> Int) -> Bool - α occurs in the inner function
    InnerFun = catena_types:tfun({tvar, VarId}, {tcon, int}, catena_types:empty_effects()),
    OuterFun = catena_types:tfun(InnerFun, {tcon, bool}, catena_types:empty_effects()),
    ?assert(catena_type_subst:occurs_check(VarId, OuterFun)),

    % α ≡ {x: α, y: Int} - α occurs in record field
    RecordWithTVar = {trecord, [{x, {tvar, VarId}}, {y, {tcon, int}}], closed},
    ?assert(catena_type_subst:occurs_check(VarId, RecordWithTVar)),

    % β doesn't occur in any of these
    OtherVarId = 666,
    ?assertNot(catena_type_subst:occurs_check(OtherVarId, ListWithTVar)),
    ?assertNot(catena_type_subst:occurs_check(OtherVarId, OuterFun)),
    ?assertNot(catena_type_subst:occurs_check(OtherVarId, RecordWithTVar)).

%%====================================================================
%% Performance Tests (Optional - commented out by default)
%%====================================================================

%% Uncomment to run performance tests
%% performance_tests() ->
%%     [
%%       ?_test(test_occurs_check_performance())
%%     ].

%% test_occurs_check_performance() ->
%%     % Test that occurs check is efficient even on deeply nested types
%%     VarId = 1,
%%
%%     % Build a deeply nested type: Int -> (Int -> (Int -> ... ))
%%     DeepType = lists:foldl(
%%         fun(_, Acc) ->
%%             catena_types:tfun({tcon, int}, Acc, catena_types:empty_effects())
%%         end,
%%         {tcon, int},
%%         lists:seq(1, 1000)
%%     ),
%%
%%     % Should complete quickly (var doesn't occur)
%%     ?assertNot(catena_type_subst:occurs_check(VarId, DeepType)).
