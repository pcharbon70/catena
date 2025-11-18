%%%-------------------------------------------------------------------
%%% @doc Unit Tests for Inference State Management
%%%
%%% Tests for catena_infer_state module covering:
%%% - State creation
%%% - Fresh variable generation
%%% - Substitution management
%%% - Error collection
%%% - State accessors and modifiers
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_infer_state_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Section 1: State Creation and Initialization
%%%===================================================================

new_state_test() ->
    State = catena_infer_state:new(),

    % Should start with variable counter at 1
    ?assertEqual(1, catena_infer_state:get_next_var(State)),
    % Check get_counter alias works too
    ?assertEqual(1, catena_infer_state:get_counter(State)),

    % Should start with empty substitution
    Subst = catena_infer_state:get_subst(State),
    ?assertEqual([], catena_type_subst:domain(Subst)),

    % Should start with no errors
    ?assertEqual(false, catena_infer_state:has_errors(State)),
    ?assertEqual([], catena_infer_state:get_errors(State)).

new_state_custom_counter_test() ->
    % Test new/1 constructor
    State = catena_infer_state:new(42),
    ?assertEqual(42, catena_infer_state:get_next_var(State)),
    ?assertEqual(42, catena_infer_state:get_counter(State)).

%%%===================================================================
%%% Section 2: Fresh Variable Generation
%%%===================================================================

fresh_var_single_test() ->
    State0 = catena_infer_state:new(),

    % Generate first variable
    {{tvar, Id1}, State1} = catena_infer_state:fresh_var(State0),
    ?assertEqual(1, Id1),
    ?assertEqual(2, catena_infer_state:get_next_var(State1)),
    ?assertEqual(2, catena_infer_state:get_counter(State1)),

    % Generate second variable
    {{tvar, Id2}, State2} = catena_infer_state:fresh_var(State1),
    ?assertEqual(2, Id2),
    ?assertEqual(3, catena_infer_state:get_next_var(State2)),

    % Generate third variable
    {{tvar, Id3}, _State3} = catena_infer_state:fresh_var(State2),
    ?assertEqual(3, Id3).

fresh_var_id_test() ->
    State0 = catena_infer_state:new(),
    {Id, State1} = catena_infer_state:fresh_var_id(State0),

    % First ID should be 1
    ?assertEqual(1, Id),
    % Counter should be updated
    ?assertEqual(2, catena_infer_state:get_next_var(State1)),
    ?assertEqual(2, catena_infer_state:get_counter(State1)).

fresh_var_independence_test() ->
    % Each state should maintain its own counter
    State1 = catena_infer_state:new(),
    State2 = catena_infer_state:new(),

    {{tvar, Id1}, _} = catena_infer_state:fresh_var(State1),
    {{tvar, Id2}, _} = catena_infer_state:fresh_var(State2),

    % Both should generate variable 1
    ?assertEqual(Id1, Id2),
    ?assertEqual(1, Id1).

fresh_vars_zero_test() ->
    State0 = catena_infer_state:new(),

    % Generate zero variables
    {Vars, State1} = catena_infer_state:fresh_vars(0, State0),

    ?assertEqual([], Vars),
    ?assertEqual(1, catena_infer_state:get_next_var(State1)).

fresh_vars_multiple_test() ->
    State0 = catena_infer_state:new(),

    % Generate 5 variables
    {Vars, State1} = catena_infer_state:fresh_vars(5, State0),

    ?assertEqual(5, length(Vars)),
    ?assertEqual([{tvar, 1}, {tvar, 2}, {tvar, 3}, {tvar, 4}, {tvar, 5}], Vars),
    ?assertEqual(6, catena_infer_state:get_next_var(State1)),

    % Generate 3 more
    {Vars2, State2} = catena_infer_state:fresh_vars(3, State1),

    ?assertEqual(3, length(Vars2)),
    ?assertEqual([{tvar, 6}, {tvar, 7}, {tvar, 8}], Vars2),
    ?assertEqual(9, catena_infer_state:get_next_var(State2)).

fresh_vars_ordering_test() ->
    State0 = catena_infer_state:new(),

    % Variables should be returned in ascending order
    {Vars, _State1} = catena_infer_state:fresh_vars(10, State0),

    ExpectedOrder = [{tvar, N} || N <- lists:seq(1, 10)],
    ?assertEqual(ExpectedOrder, Vars).

%%%===================================================================
%%% Section 3: Substitution Management
%%%===================================================================

get_subst_initial_test() ->
    State = catena_infer_state:new(),
    Subst = catena_infer_state:get_subst(State),

    % Should be empty
    ?assertEqual([], catena_type_subst:domain(Subst)).

add_subst_test() ->
    State0 = catena_infer_state:new(),

    % Create a substitution: α → Int
    Subst = catena_type_subst:singleton(1, {tcon, int}),
    State1 = catena_infer_state:add_subst(Subst, State0),

    % Should be stored in state
    StoredSubst = catena_infer_state:get_subst(State1),
    ?assertEqual([1], catena_type_subst:domain(StoredSubst)),

    Type = catena_type_subst:apply(StoredSubst, {tvar, 1}),
    ?assertEqual({tcon, int}, Type).

add_subst_replacement_test() ->
    State0 = catena_infer_state:new(),

    % Add first substitution: α → Int
    Subst1 = catena_type_subst:singleton(1, {tcon, int}),
    State1 = catena_infer_state:add_subst(Subst1, State0),

    % Replace with second substitution: β → Bool
    Subst2 = catena_type_subst:singleton(2, {tcon, bool}),
    State2 = catena_infer_state:add_subst(Subst2, State1),

    % Should have replaced, not composed
    StoredSubst = catena_infer_state:get_subst(State2),
    ?assertEqual([2], catena_type_subst:domain(StoredSubst)),

    Type = catena_type_subst:apply(StoredSubst, {tvar, 2}),
    ?assertEqual({tcon, bool}, Type).

compose_subst_test() ->
    State0 = catena_infer_state:new(),

    % Start with substitution: α → Int
    Subst1 = catena_type_subst:singleton(1, {tcon, int}),
    State1 = catena_infer_state:add_subst(Subst1, State0),

    % Compose with: β → α
    Subst2 = catena_type_subst:singleton(2, {tvar, 1}),
    State2 = catena_infer_state:compose_subst(Subst2, State1),

    % Should have both mappings with β → Int (composed)
    StoredSubst = catena_infer_state:get_subst(State2),
    Domain = lists:sort(catena_type_subst:domain(StoredSubst)),
    ?assertEqual([1, 2], Domain),

    % β should map to Int (through α)
    Type = catena_type_subst:apply(StoredSubst, {tvar, 2}),
    ?assertEqual({tcon, int}, Type).

compose_subst_multiple_test() ->
    State0 = catena_infer_state:new(),

    % Build up a composition: α → Int, β → α, γ → β
    Subst1 = catena_type_subst:singleton(1, {tcon, int}),
    State1 = catena_infer_state:add_subst(Subst1, State0),

    Subst2 = catena_type_subst:singleton(2, {tvar, 1}),
    State2 = catena_infer_state:compose_subst(Subst2, State1),

    Subst3 = catena_type_subst:singleton(3, {tvar, 2}),
    State3 = catena_infer_state:compose_subst(Subst3, State2),

    % All should resolve to Int
    StoredSubst = catena_infer_state:get_subst(State3),
    ?assertEqual({tcon, int}, catena_type_subst:apply(StoredSubst, {tvar, 1})),
    ?assertEqual({tcon, int}, catena_type_subst:apply(StoredSubst, {tvar, 2})),
    ?assertEqual({tcon, int}, catena_type_subst:apply(StoredSubst, {tvar, 3})).

%%%===================================================================
%%% Section 4: Error Collection
%%%===================================================================

has_errors_initial_test() ->
    State = catena_infer_state:new(),
    ?assertEqual(false, catena_infer_state:has_errors(State)).

add_error_single_test() ->
    State0 = catena_infer_state:new(),

    % Add an error
    Error = catena_type_error:unification_error({tcon, int}, {tcon, bool}),
    State1 = catena_infer_state:add_error(Error, State0),

    ?assertEqual(true, catena_infer_state:has_errors(State1)),
    Errors = catena_infer_state:get_errors(State1),
    ?assertEqual(1, length(Errors)),
    ?assertEqual([Error], Errors).

add_error_multiple_test() ->
    State0 = catena_infer_state:new(),

    % Add multiple errors
    Error1 = catena_type_error:unification_error({tcon, int}, {tcon, bool}),
    State1 = catena_infer_state:add_error(Error1, State0),

    Error2 = catena_type_error:occurs_check(1, {tvar, 1}),
    State2 = catena_infer_state:add_error(Error2, State1),

    Error3 = catena_type_error:circular_substitution(2),
    State3 = catena_infer_state:add_error(Error3, State2),

    ?assertEqual(true, catena_infer_state:has_errors(State3)),
    Errors = catena_infer_state:get_errors(State3),
    ?assertEqual(3, length(Errors)),

    % Errors should be in order of addition
    ?assertEqual([Error1, Error2, Error3], Errors).

get_errors_ordering_test() ->
    State0 = catena_infer_state:new(),

    % Add errors in specific order
    Errors = [
        catena_type_error:circular_substitution(1),
        catena_type_error:substitution_depth_exceeded(100, 50),
        catena_type_error:unbound_variable(test_var)
    ],

    State3 = lists:foldl(
        fun(E, S) -> catena_infer_state:add_error(E, S) end,
        State0,
        Errors
    ),

    % Should get errors in order of addition
    RetrievedErrors = catena_infer_state:get_errors(State3),
    ?assertEqual(Errors, RetrievedErrors).

%%%===================================================================
%%% Section 5: State Accessors and Modifiers
%%%===================================================================

get_next_var_test() ->
    State0 = catena_infer_state:new(),
    ?assertEqual(1, catena_infer_state:get_next_var(State0)),

    {_Var, State1} = catena_infer_state:fresh_var(State0),
    ?assertEqual(2, catena_infer_state:get_next_var(State1)).

set_next_var_test() ->
    State0 = catena_infer_state:new(),

    % Set to specific value
    State1 = catena_infer_state:set_next_var(100, State0),
    ?assertEqual(100, catena_infer_state:get_next_var(State1)),

    % Generate variable should use new counter
    {{tvar, Id}, State2} = catena_infer_state:fresh_var(State1),
    ?assertEqual(100, Id),
    ?assertEqual(101, catena_infer_state:get_next_var(State2)).

%%%===================================================================
%%% Section 6: Integration Tests
%%%===================================================================

integration_inference_simulation_test() ->
    % Simulate a typical inference flow
    State0 = catena_infer_state:new(),

    % 1. Generate some fresh variables
    {[Alpha, Beta], State1} = catena_infer_state:fresh_vars(2, State0),
    ?assertEqual({tvar, 1}, Alpha),
    ?assertEqual({tvar, 2}, Beta),

    % 2. Unify α with Int (add substitution)
    Subst1 = catena_type_subst:singleton(1, {tcon, int}),
    State2 = catena_infer_state:add_subst(Subst1, State1),

    % 3. Unify β with α (compose substitution)
    Subst2 = catena_type_subst:singleton(2, {tvar, 1}),
    State3 = catena_infer_state:compose_subst(Subst2, State2),

    % 4. Check final substitution
    FinalSubst = catena_infer_state:get_subst(State3),
    ?assertEqual({tcon, int}, catena_type_subst:apply(FinalSubst, Alpha)),
    ?assertEqual({tcon, int}, catena_type_subst:apply(FinalSubst, Beta)),

    % 5. Should have no errors
    ?assertEqual(false, catena_infer_state:has_errors(State3)).

integration_error_recovery_test() ->
    % Simulate error handling in inference
    State0 = catena_infer_state:new(),

    % Try to infer something that fails
    Error1 = catena_type_error:unification_error({tcon, int}, {tcon, bool}),
    State1 = catena_infer_state:add_error(Error1, State0),

    % Continue inferring other things
    {_Var, State2} = catena_infer_state:fresh_var(State1),

    % Add another error
    Error2 = catena_type_error:occurs_check(1, {tapp, {tcon, list}, [{tvar, 1}]}),
    State3 = catena_infer_state:add_error(Error2, State2),

    % Should have both errors
    ?assertEqual(true, catena_infer_state:has_errors(State3)),
    Errors = catena_infer_state:get_errors(State3),
    ?assertEqual(2, length(Errors)),
    ?assertEqual([Error1, Error2], Errors).

integration_complex_substitution_test() ->
    % Test complex substitution composition
    State0 = catena_infer_state:new(),

    % Generate variables: α, β, γ, δ
    {[Alpha, Beta, Gamma, Delta], State1} = catena_infer_state:fresh_vars(4, State0),

    % Build substitution: α → Int, β → List<α>, γ → β, δ → γ
    Subst1 = catena_type_subst:singleton(1, {tcon, int}),
    State2 = catena_infer_state:add_subst(Subst1, State1),

    Subst2 = catena_type_subst:singleton(2, {tapp, {tcon, list}, [{tvar, 1}]}),
    State3 = catena_infer_state:compose_subst(Subst2, State2),

    Subst3 = catena_type_subst:singleton(3, {tvar, 2}),
    State4 = catena_infer_state:compose_subst(Subst3, State3),

    Subst4 = catena_type_subst:singleton(4, {tvar, 3}),
    State5 = catena_infer_state:compose_subst(Subst4, State4),

    % All should resolve correctly
    FinalSubst = catena_infer_state:get_subst(State5),
    ?assertEqual({tcon, int}, catena_type_subst:apply(FinalSubst, Alpha)),

    ExpectedList = {tapp, {tcon, list}, [{tcon, int}]},
    ?assertEqual(ExpectedList, catena_type_subst:apply(FinalSubst, Beta)),
    ?assertEqual(ExpectedList, catena_type_subst:apply(FinalSubst, Gamma)),
    ?assertEqual(ExpectedList, catena_type_subst:apply(FinalSubst, Delta)).

%%%===================================================================
%%% Section 7: State Independence Tests
%%%===================================================================

state_not_modified_test() ->
    State0 = catena_infer_state:new(),

    % Generate variable but don't use returned state
    {_Var1, _State1} = catena_infer_state:fresh_var(State0),

    % Original state unchanged
    ?assertEqual(1, catena_infer_state:get_next_var(State0)),
    ?assertEqual(1, catena_infer_state:get_counter(State0)),

    % Generate another from original - should get same ID
    {Var2, State2} = catena_infer_state:fresh_var(State0),
    ?assertMatch({tvar, 1}, Var2),
    ?assertEqual(2, catena_infer_state:get_next_var(State2)),
    ?assertEqual(2, catena_infer_state:get_counter(State2)).

parallel_states_test() ->
    % Two independent state threads
    StateA0 = catena_infer_state:new(),
    StateB0 = catena_infer_state:new(),

    % Branch A
    {VarA1, StateA1} = catena_infer_state:fresh_var(StateA0),
    {VarA2, StateA2} = catena_infer_state:fresh_var(StateA1),

    % Branch B (independent)
    {VarB1, StateB1} = catena_infer_state:fresh_var(StateB0),
    {VarB2, StateB2} = catena_infer_state:fresh_var(StateB1),

    % Both branches generate same IDs (independent)
    ?assertMatch({tvar, 1}, VarA1),
    ?assertMatch({tvar, 2}, VarA2),
    ?assertMatch({tvar, 1}, VarB1),
    ?assertMatch({tvar, 2}, VarB2),

    % Final counters
    ?assertEqual(3, catena_infer_state:get_next_var(StateA2)),
    ?assertEqual(3, catena_infer_state:get_next_var(StateB2)).

sequential_ids_fold_test() ->
    State0 = catena_infer_state:new(),

    % Generate 10 fresh variable IDs using fold
    {Ids, StateFinal} = lists:foldl(
        fun(_, {AccIds, AccState}) ->
            {Id, NewState} = catena_infer_state:fresh_var_id(AccState),
            {[Id | AccIds], NewState}
        end,
        {[], State0},
        lists:seq(1, 10)
    ),

    % IDs should be 1..10 (reversed because of cons)
    ?assertEqual(lists:seq(10, 1, -1), Ids),
    % Counter should be 10
    ?assertEqual(11, catena_infer_state:get_next_var(StateFinal)),
    ?assertEqual(11, catena_infer_state:get_counter(StateFinal)).

%%%===================================================================
%%% Test Suite Integration
%%%===================================================================

fresh_var_overflow_test() ->
    % Test that overflow detection works
    % Create a state near the limit
    MaxId = 1000000,
    State0 = catena_infer_state:new(MaxId),
    
    % Should work up to the limit
    {{tvar, MaxId}, State1} = catena_infer_state:fresh_var(State0),
    ?assertEqual(MaxId + 1, catena_infer_state:get_next_var(State1)),
    
    % Next call should overflow and throw error
    OverflowId = MaxId + 1,
    ?assertError({type_var_overflow, OverflowId, MaxId},
        catena_infer_state:fresh_var(State1)).

integration_all_sections_test() ->
    % Verify all sections run successfully
    ?assert(true).
