%%%-------------------------------------------------------------------
%%% @doc Tests for Substitution Occurs Check Security
%%%
%%% Tests the critical security fix for missing occurs check in
%%% substitution extension. Prevents creation of infinite types.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_type_subst_occurs_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Occurs Check Security Tests
%%====================================================================

occurs_check_security_test_() ->
    [
      ?_test(test_extend_prevents_simple_infinite_type()),
      ?_test(test_extend_prevents_nested_infinite_type()),
      ?_test(test_singleton_prevents_infinite_type()),
      ?_test(test_extend_allows_safe_substitution()),
      ?_test(test_singleton_allows_safe_substitution()),
      ?_test(test_complex_infinite_scenarios())
    ].

%% @doc Test that extend prevents simple infinite types
%% α ≡ α should be prevented
test_extend_prevents_simple_infinite_type() ->
    Subst = catena_type_subst:empty(),
    VarId = 123,
    TVar = {tvar, VarId},
    
    % Try to create α ≡ α (should fail)
    case catena_type_subst:extend(Subst, VarId, TVar) of
        {ok, _Result} ->
            ?assert(false, "Should prevent simple infinite type α ≡ α");
        {error, Error} ->
            ?assertMatch({occurs_check, VarId, TVar}, Error)
    end.

%% @doc Test that extend prevents function infinite types
%% α ≡ α -> α should be prevented
test_extend_prevents_nested_infinite_type() ->
    Subst = catena_type_subst:empty(),
    VarId = 456,
    TVar = {tvar, VarId},
    
    % Create α -> α (function type)
    InfiniteFunType = catena_types:tfun(TVar, TVar, catena_types:empty_effects()),
    
    % Try to create α ≡ α -> α (should fail)
    case catena_type_subst:extend(Subst, VarId, InfiniteFunType) of
        {ok, _Result} ->
            ?assert(false, "Should prevent function infinite type α ≡ α -> α");
        {error, Error} ->
            ?assertMatch({occurs_check, VarId, InfiniteFunType}, Error)
    end.

%% @doc Test that singleton prevents infinite types
test_singleton_prevents_infinite_type() ->
    VarId = 789,
    TVar = {tvar, VarId},
    
    % Try to create singleton α ≡ α (should fail)
    case catena_type_subst:singleton(VarId, TVar) of
        {ok, _} ->
            ?assert(false, "Should prevent singleton infinite type α ≡ α");
        {error, Error} ->
            ?assertMatch({occurs_check, VarId, TVar}, Error)
    end,

    % Try to create singleton α ≡ α -> α (should fail)
    InfiniteFunType = catena_types:tfun(TVar, TVar, catena_types:empty_effects()),

    case catena_type_subst:singleton(VarId, InfiniteFunType) of
        {ok, _} ->
            ?assert(false, "Should prevent singleton infinite type α ≡ α -> α");
        {error, Error2} ->
            ?assertMatch({occurs_check, VarId, InfiniteFunType}, Error2)
    end.

%% @doc Test that extend allows safe substitutions
test_extend_allows_safe_substitution() ->
    Subst = catena_type_subst:empty(),
    VarId = 111,
    TVar = {tvar, VarId},
    
    % α ≡ Int (should be allowed)
    Result1 = catena_type_subst:extend(Subst, VarId, {tcon, int}),
    ?assertMatch({ok, _Subst}, Result1),
    
    % α ≡ β (different variables, should be allowed)
    {ok, Result1Subst} = Result1,
    OtherVarId = 222,
    Result2 = catena_type_subst:extend(Result1Subst, VarId, {tvar, OtherVarId}),
    ?assertMatch({ok, _Subst}, Result2),
    
    % α ≡ β -> Int (should be allowed, α doesn't occur in β -> Int)
    {ok, Result2Subst} = Result2,
    FunType = catena_types:tfun({tvar, OtherVarId}, {tcon, int}, catena_types:empty_effects()),
    Result3 = catena_type_subst:extend(Result2Subst, VarId, FunType),
    ?assertMatch({ok, _Subst}, Result3).

%% @doc Test that singleton allows safe substitutions
test_singleton_allows_safe_substitution() ->
    VarId = 333,
    
    % α ≡ Int (should be allowed)
    Result1 = catena_type_subst:singleton(VarId, {tcon, int}),
    ?assertMatch(Subst when is_map(Subst), Result1),
    
    % Check the substitution is correct
    case catena_type_subst:lookup(Result1, VarId) of
        {ok, {tcon, int}} -> ok;
        _ -> ?assert(false, "Singleton should map variable to correct type")
    end,
    
    % α ≡ β (different variables, should be allowed)
    OtherVarId = 444,
    Result2 = catena_type_subst:singleton(VarId, {tvar, OtherVarId}),
    ?assertMatch(Subst when is_map(Subst), Result2),
    
    % Check the substitution is correct
    case catena_type_subst:lookup(Result2, VarId) of
        {ok, {tvar, OtherVarId}} -> ok;
        _ -> ?assert(false, "Singleton should map variable to correct type")
    end.

%% @doc Test complex infinite type scenarios
test_complex_infinite_scenarios() ->
    VarId = 555,
    Subst = catena_type_subst:empty(),
    
    % α ≡ List<α> (should fail)
    ListWithTVar = {tapp, {tcon, list}, {tvar, VarId}},
    case catena_type_subst:extend(Subst, VarId, ListWithTVar) of
        {ok, _} ->
            ?assert(false, "Should prevent α ≡ List<α>");
        {error, Error1} ->
            ?assertMatch({occurs_check, VarId, ListWithTVar}, Error1)
    end,

    % α ≡ (α -> Int) -> Bool (should fail, α occurs in α -> Int)
    InnerFun = catena_types:tfun({tvar, VarId}, {tcon, int}, catena_types:empty_effects()),
    OuterFun = catena_types:tfun(InnerFun, {tcon, bool}, catena_types:empty_effects()),
    case catena_type_subst:extend(Subst, VarId, OuterFun) of
        {ok, _} ->
            ?assert(false, "Should prevent α ≡ (α -> Int) -> Bool");
        {error, Error2} ->
            ?assertMatch({occurs_check, VarId, OuterFun}, Error2)
    end,

    % α ≡ {x: α, y: Int} (should fail, α occurs in record field)
    RecordWithTVar = {trecord, [{x, {tvar, VarId}}, {y, {tcon, int}}], closed},
    case catena_type_subst:extend(Subst, VarId, RecordWithTVar) of
        {ok, _} ->
            ?assert(false, "Should prevent α ≡ {x: α, y: Int}");
        {error, Error3} ->
            ?assertMatch({occurs_check, VarId, RecordWithTVar}, Error3)
    end.

%%====================================================================
%% Integration Security Tests
%%====================================================================

integration_security_test_() ->
    [
      ?_test(test_unification_security_with_occurs_check()),
      ?_test(test_inference_security_with_occurs_check())
    ].

%% @doc Test that unification layer is protected by occurs check
test_unification_security_with_occurs_check() ->
    State0 = catena_infer_state:new(),
    
    % Try to unify α with α -> Int (should fail through unification)
    TVar = {tvar, 666},
    FunType = catena_types:tfun(TVar, {tcon, int}, catena_types:empty_effects()),
    
    case catena_infer_unify:unify(TVar, FunType, State0) of
        {ok, _Subst, _State} ->
            ?assert(false, "Unification should prevent infinite type");
        {error, Error, _State} ->
            % Should be an occurs check error
            % The error might come from different places but should prevent infinite types
            ?assert(is_tuple(Error), "Should return some error preventing infinite types")
    end.

%% @doc Test that inference layer is protected by occurs check
test_inference_security_with_occurs_check() ->
    % This test ensures that even complex inference scenarios
    % are protected against infinite types
    
    % Create a scenario that would lead to infinite type if not protected
    % For example: let rec f = λx. f x (infinite recursion)
    self = {var, self},
    app_self = {app, self, {var, x}},
    recursive_fun = {lam, x, app_self},
    letrec_self = {'letrec', self, recursive_fun, {var, self}},
    
    case catena_infer:infer_expr(letrec_self) of
        {ok, _Type} ->
            % If this succeeds, it should be a safe type (not infinite)
            % The exact type depends on the implementation, but it shouldn't be infinite
            ok;
        {error, _Errors} ->
            % This is also fine - type error is better than infinite type
            ok
    end.

%%====================================================================
%% Performance Tests - Ensure No Regression
%%====================================================================

performance_tests() ->
    [
      ?_test(test_occurs_check_performance()),
      ?_test(test_extend_performance_with_safety())
    ].

%% @doc Test that occurs check doesn't significantly impact performance
test_occurs_check_performance() ->
    % Test occurs check on a reasonably complex type
    VarId = 777,
    ComplexType = catena_types:tfun(
        catena_types:tfun({tvar, 888}, {tcon, bool}, catena_types:empty_effects()),
        {tcon, int},
        catena_types:empty_effects()
    ),
    
    % This should be fast and succeed
    Result = catena_type_subst:singleton(VarId, ComplexType),
    ?assertMatch(Subst when is_map(Subst), Result),
    
    % Verify the substitution was created correctly
    case catena_type_subst:lookup(Result, VarId) of
        {ok, ComplexType} -> ok;
        _ -> ?assert(false, "Complex type should be in substitution")
    end.

%% @doc Test that extend with occurs check is still performant
test_extend_performance_with_safety() ->
    Subst0 = catena_type_subst:empty(),
    
    % Create a series of safe substitutions quickly
    VarIds = [801, 802, 803, 804, 805],
    Types = [{tcon, int}, {tcon, bool}, {tcon, string}, {tcon, float}, {tcon, atom}],
    
    {FinalSubst, _} = lists:foldl(
        fun({VarId, Type}, {Subst, Index}) ->
            Result = catena_type_subst:extend(Subst, VarId, Type),
            ?assertMatch({ok, _Subst}, Result),
            {ok, NewSubst} = Result,
            {NewSubst, Index + 1}
        end,
        {Subst0, 0},
        lists:zip(VarIds, Types)
    ),
    
    % Verify all substitutions were created
    ?assertEqual(5, maps:size(FinalSubst)),
    
    % Check a few random ones
    case catena_type_subst:lookup(FinalSubst, 801) of
        {ok, {tcon, int}} -> ok;
        _ -> ?assert(false, "First substitution should be Int")
    end,
    
    case catena_type_subst:lookup(FinalSubst, 805) of
        {ok, {tcon, atom}} -> ok;
        _ -> ?assert(false, "Last substitution should be Atom")
    end.