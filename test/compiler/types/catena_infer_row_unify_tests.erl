%%%-------------------------------------------------------------------
%%% @doc Tests for Record Row Polymorphism Unification
%%%
%%% Tests the fixes for row variable binding bugs in record unification.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_infer_row_unify_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Row Variable vs Closed Record Tests
%%====================================================================

row_variable_closed_record_test_() ->
    [
      ?_test(test_row_variable_unifies_with_closed()),
      ?_test(test_row_variable_binding_persists()),
      ?_test(test_mismatched_fields_error())
    ].

test_row_variable_unifies_with_closed() ->
    % Test: {x: Int | α} ≡ {x: Int | closed}
    % Should bind α to {} (empty record)
    
    State0 = catena_infer_state:new(),
    
    % Create a record type with row variable
    RecWithRow = {trecord, [{x, {tcon, int}}], 123}, % 123 is a row variable ID
    ClosedRec = {trecord, [{x, {tcon, int}}], closed},
    
    % Test unification
    {ok, Subst} = catena_infer_unify:unify_types(RecWithRow, ClosedRec),
    
    % Check that row variable 123 is bound to empty record
    case catena_type_subst:lookup(Subst, 123) of
        {ok, BoundType} ->
            ?assertEqual({trecord, [], closed}, BoundType);
        none ->
            ?assert(false, "Row variable should be bound to empty record")
    end.

test_row_variable_binding_persists() ->
    % Test that the row variable binding actually works when applied
    
    State0 = catena_infer_state:new(),
    
    % Create a complex type with row variable
    RecWithRow = {trecord, [{x, {tcon, int}}], 456},
    TVarUse = {tvar, 456}, % Use the same row variable elsewhere
    
    % First unify with closed record to bind the row variable
    ClosedRec = {trecord, [{x, {tcon, int}}], closed},
    case catena_infer_unify:unify_types(RecWithRow, ClosedRec) of
        {ok, Subst} ->
            % Apply substitution to the standalone row variable
            ResultType = catena_type_subst:apply(Subst, TVarUse),
            % Should become empty record
            ?assertEqual({trecord, [], closed}, ResultType);
        {error, _} ->
            ?assert(false, "Should successfully unify")
    end.

test_mismatched_fields_error() ->
    % Test: {x: Int | α} ≡ {x: Bool | closed}
    % Should fail due to field type mismatch, not row variable issue
    
    RecWithRow = {trecord, [{x, {tcon, int}}], 789},
    ClosedRec = {trecord, [{x, {tcon, bool}}], closed},
    
    % Should fail due to field unification error
    case catena_infer_unify:unify_types(RecWithRow, ClosedRec) of
        {ok, _Subst} ->
            ?assert(false, "Should fail due to field type mismatch");
        {error, Error} ->
            % Check that it's a unification error, not row variable error
            ?assertMatch({unification_error, {tcon, int}, {tcon, bool}}, Error)
    end.

%%====================================================================
%% Different Row Variables Tests
%%====================================================================

different_row_variables_test_() ->
    [
      ?_test(test_different_row_variables_unify()),
      ?_test(test_row_variable_substitution_composition()),
      ?_test(test_nested_types_with_row_variables())
    ].

test_different_row_variables_unify() ->
    % Test: {x: Int | α} ≡ {x: Int | β}
    % Should create substitution α ≡ β
    
    Rec1 = {trecord, [{x, {tcon, int}}], 111},
    Rec2 = {trecord, [{x, {tcon, int}}], 222},
    
    case catena_infer_unify:unify_types(Rec1, Rec2) of
        {ok, Subst} ->
            % Should bind 222 to 111 (or vice versa)
            % Check that one of the variables is bound to the other
            case {catena_type_subst:lookup(Subst, 111), catena_type_subst:lookup(Subst, 222)} of
                {{ok, {tvar, _Other}}, none} -> ok; % One bound to other
                {none, {ok, {tvar, _Other}}} -> ok; % Other way around
                {{ok, {tvar, 111}}, {ok, {tvar, 222}}} -> 
                    ?assert(false, "Should bind row variables together");
                _ -> 
                    ?assert(false, "Row variables should be unified")
            end;
        {error, _} ->
            ?assert(false, "Should successfully unify")
    end.

test_row_variable_substitution_composition() ->
    % Test that field substitutions and row variable substitutions compose correctly
    
    % Create types with different field types that need unification
    % and different row variables that need unification
    Rec1 = {trecord, [{x, {tvar, 333}}], 111}, % x has a type variable
    Rec2 = {trecord, [{x, {tcon, int}}], 222}, % x has concrete type
    
    case catena_infer_unify:unify_types(Rec1, Rec2) of
        {ok, Subst} ->
            % Should have both:
            % 1. Variable 333 bound to Int (from field unification)
            % 2. Row variable 111 bound to 222 or vice versa
            
            % Check field variable binding
            case catena_type_subst:lookup(Subst, 333) of
                {ok, {tcon, int}} -> ok;
                _ -> ?assert(false, "Field variable should be bound to Int")
            end,
            
            % Check row variable binding
            case {catena_type_subst:lookup(Subst, 111), catena_type_subst:lookup(Subst, 222)} of
                {{ok, {tvar, 222}}, none} -> ok;
                {none, {ok, {tvar, 111}}} -> ok;
                {{ok, {tvar, 222}}, {ok, {tvar, 111}}} -> ?assert(false, "Only one row variable should be bound");
                _ -> ?assert(false, "Row variables should be unified")
            end;
        {error, _} ->
            ?assert(false, "Should successfully unify")
    end.

test_nested_types_with_row_variables() ->
    % Test row variables in nested types (e.g., inside functions)
    
    % Function types with records containing row variables:
    % (Record1 -> Int) ≡ (Record2 -> Int)
    % where Record1 and Record2 have different row variables
    
    Record1 = {trecord, [{x, {tcon, int}}], 444},
    Record2 = {trecord, [{x, {tcon, int}}], 555},
    
    FunType1 = catena_types:tfun(Record1, {tcon, int}, catena_types:empty_effects()),
    FunType2 = catena_types:tfun(Record2, {tcon, int}, catena_types:empty_effects()),
    
    case catena_infer_unify:unify_types(FunType1, FunType2) of
        {ok, Subst} ->
            % Should bind row variables together
            case {catena_type_subst:lookup(Subst, 444), catena_type_subst:lookup(Subst, 555)} of
                {{ok, {tvar, 555}}, none} -> ok;
                {none, {ok, {tvar, 444}}} -> ok;
                _ -> ?assert(false, "Row variables should be unified")
            end;
        {error, _} ->
            ?assert(false, "Should successfully unify")
    end.

%%====================================================================
%% Integration with Orchestration Tests
%%====================================================================

integration_test_() ->
    [
      ?_test(test_record_inference_with_row_variables()),
      ?_test(test_polymorphic_record_functions())
    ].

test_record_inference_with_row_variables() ->
    % Test record inference through the orchestration layer
    
    % Create a record expression that should get a polymorphic type
    % For simplicity, we'll test unification directly through the
    % orchestration API
    
    Env = catena_type_env:empty(),
    State0 = catena_infer_state:new(),
    
    % Test that record types with row variables can be inferred
    % This would involve creating AST nodes and using catena_infer_expr
    % For now, test the unification directly
    
    Rec1 = {trecord, [{hello, {tcon, string}}], 666},
    Rec2 = {trecord, [{hello, {tcon, string}}], closed},
    
    case catena_infer_unify:unify(Rec1, Rec2, State0) of
        {ok, Subst, State1} ->
            % Should successfully unify and bind row variable
            case catena_infer_state:get_subst(State1) of
                FinalSubst ->
                    case catena_type_subst:lookup(FinalSubst, 666) of
                        {ok, {trecord, [], closed}} -> ok;
                        _ -> ?assert(false, "Row variable should be bound to empty record")
                    end
            end;
        {error, _, _} ->
            ?assert(false, "Should successfully unify")
    end.

test_polymorphic_record_functions() ->
    % Test functions that operate on polymorphic records
    
    % Function type: {x: Int | α} -> {x: Int | α}
    % This should be generalizable as polymorphic over α
    
    RecordType = {trecord, [{x, {tcon, int}}], 777},
    FunType = catena_types:tfun(RecordType, RecordType, catena_types:empty_effects()),
    
    % Test that we can create and work with such types
    % The actual polymorphism would be tested in the generalization/instantiation
    ?assertMatch({tfun, {trecord, [{x, {tcon, int}}], 777}, {trecord, [{x, {tcon, int}}], 777}, {effect_set, []}}, FunType).

%%====================================================================
%% Error Cases
%%====================================================================

error_cases_test_() ->
    [
      ?_test(test_occurs_check_in_row_variables()),
      ?_test(test_incompatible_field_types())
    ].

test_occurs_check_in_row_variables() ->
    % Test occurs check with row variables
    % α ≡ {x: T | α} should fail (infinite record type)
    
    % Create a row variable that appears in its own binding
    RowVarId = 888,
    RecWithSelf = {trecord, [{x, {tcon, int}}], RowVarId},
    
    % Try to unify the row variable to a record that contains it
    % First, we need to create a type where the row variable appears in the binding
    % This is a bit tricky to construct directly, so let's test a simpler case
    
    % Try to unify a row variable with a record containing the same row variable
    case catena_infer_unify:unify_types({tvar, RowVarId}, RecWithSelf) of
        {ok, _Subst} ->
            ?assert(false, "Should fail due to occurs check");
        {error, Error} ->
            ?assertMatch({occurs_check, RowVarId, _}, Error)
    end.

test_incompatible_field_types() ->
    % Test that incompatible field types still work with row variable binding
    
    Rec1 = {trecord, [{x, {tcon, int}}], 999},
    Rec2 = {trecord, [{x, {tcon, bool}}], closed},
    
    case catena_infer_unify:unify(Rec1, Rec2, catena_infer_state:new()) of
        {error, UnificationError, _State} ->
            % Should fail due to field unification, not row variable issue
            ?assertMatch({unification_error, {tcon, int}, {tcon, bool}}, UnificationError);
        {ok, _Subst, _State} ->
            ?assert(false, "Should fail due to incompatible field types")
    end.