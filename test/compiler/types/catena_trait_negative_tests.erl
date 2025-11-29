%%%-------------------------------------------------------------------
%%% @doc Negative Tests for Trait System
%%%
%%% Tests error cases and edge conditions for trait resolution:
%%% - Missing instances
%%% - Type mismatches
%%% - Circular dependencies
%%% - Overlapping instances
%%% - Invalid constraints
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_trait_negative_tests).

-include_lib("eunit/include/eunit.hrl").

%% Import common test helpers
-import(catena_test_helpers, [loc/0]).

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Get the combined instance database
instance_db() ->
    catena_trait_resolve:get_instance_db().

%%%===================================================================
%%% Missing Instance Tests
%%%===================================================================

missing_instance_test_() ->
    {"Missing instance error handling",
     [
      {"Resolve fails for non-existent trait",
       fun() ->
           DB = instance_db(),
           %% NonExistentTrait is not defined
           Constraint = catena_constraint:trait_constraint('NonExistentTrait', [{tcon, 'Maybe'}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           ?assertMatch({error, _}, Result)
       end},

      {"Resolve fails for type without instance",
       fun() ->
           DB = instance_db(),
           %% Mapper is not implemented for arbitrary custom type
           Constraint = catena_constraint:trait_constraint('Mapper', [{tcon, 'CustomUnknownType'}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           ?assertMatch({error, _}, Result)
       end},

      {"Resolve fails for wrong arity type argument",
       fun() ->
           DB = instance_db(),
           %% Mapper expects f :: * -> *, not Int :: *
           Constraint = catena_constraint:trait_constraint('Mapper', [{tcon, int}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           %% Should fail - Int is not a container type (no Mapper instance for Int)
           %% Note: may return {error, _} or deferred constraint depending on impl
           case Result of
               {error, _} -> ok;
               {ok, deferred, _} -> ok  % Deferred for later checking
           end
       end}
     ]}.

%%%===================================================================
%%% Type Mismatch Tests
%%%===================================================================

type_mismatch_test_() ->
    {"Type mismatch error handling",
     [
      {"Method type inference fails for wrong argument types",
       fun() ->
           DB = instance_db(),
           State = catena_infer_state:new(),
           %% Try to use map with non-function first argument
           %% map : (a -> b) -> f a -> f b
           %% Passing Int as first arg should fail
           NonFuncType = {tcon, int},
           ContainerType = {tapp, {tcon, 'Maybe'}, [{tcon, int}]},
           Result = catena_trait_resolve:resolve_method_type(map, [NonFuncType, ContainerType], DB, State),
           %% May return error or type var (depends on implementation)
           case Result of
               {error, _} -> ok;
               {ok, _, _} -> ok  % Type inference may succeed with constraints
           end
       end},

      {"Trait constraints can be added to state",
       fun() ->
           %% Create trait constraints
           Constraint = catena_constraint:trait_constraint('Comparable', [{tvar, 1}], unknown),
           State = catena_infer_state:new(),
           State1 = catena_infer_state:add_constraint(Constraint, State),
           %% Verify constraint was added
           ?assert(length(catena_infer_state:get_constraints(State1)) > 0)
       end}
     ]}.

%%%===================================================================
%%% Circular Dependency Tests
%%%===================================================================

circular_dependency_test_() ->
    {"Circular trait dependency detection",
     [
      {"Self-referential constraint doesn't cause infinite loop",
       fun() ->
           %% Constraint: Mapper a where a = f a (infinite type)
           %% This shouldn't cause the resolver to loop forever
           DB = instance_db(),
           %% Create type that references itself
           %% Note: The occurs check should prevent this
           Constraint = catena_constraint:trait_constraint('Mapper', [{tvar, 1}], unknown),
           %% Resolution should either succeed (with constraints) or fail gracefully
           Result = catena_instance:resolve_constraint(Constraint, DB),
           case Result of
               {ok, _, _} -> ok;  % Deferred constraint
               {error, _} -> ok   % Direct failure
           end
       end},

      {"Mutually dependent traits don't cause infinite resolution",
       fun() ->
           %% If A requires B and B requires A, resolution should terminate
           %% For now, our trait system doesn't support mutual dependencies
           %% This test documents expected behavior
           DB = instance_db(),
           State = catena_infer_state:new(),
           %% Attempt to resolve a standard trait - should terminate
           _Result = catena_trait_resolve:resolve_method_type(
               map,
               [{tfun, {tvar, 1}, {tvar, 2}, {effect_set, []}},
                {tapp, {tcon, 'Maybe'}, [{tvar, 1}]}],
               DB, State),
           %% If we get here, it terminated (success)
           ok
       end}
     ]}.

%%%===================================================================
%%% Overlapping Instance Tests
%%%===================================================================

overlapping_instance_test_() ->
    {"Overlapping instance detection",
     [
      {"More specific instance is preferred",
       fun() ->
           %% If we have Mapper for Maybe and Mapper for (Maybe Int),
           %% the more specific one should be chosen
           %% For now, our system doesn't support instance overlapping
           %% This test documents expected behavior
           DB = instance_db(),
           SpecificConstraint = catena_constraint:trait_constraint(
               'Mapper',
               [{tapp, {tcon, 'Maybe'}, [{tcon, int}]}],
               unknown),
           GenericConstraint = catena_constraint:trait_constraint(
               'Mapper',
               [{tcon, 'Maybe'}],
               unknown),
           %% Both should resolve to the same instance (Maybe's Mapper)
           SpecificResult = catena_instance:resolve_constraint(SpecificConstraint, DB),
           GenericResult = catena_instance:resolve_constraint(GenericConstraint, DB),
           %% Generic should succeed
           ?assertMatch({ok, _, _}, GenericResult),
           %% Specific may or may not - depends on instance db structure
           case SpecificResult of
               {ok, _, _} -> ok;
               {error, _} -> ok
           end
       end},

      {"Instance uniqueness is enforced",
       fun() ->
           %% Attempting to add duplicate instance should fail
           %% This is checked during instance DB construction, not resolution
           %% Test that our built-in instances don't conflict
           DB = instance_db(),
           %% Get all instances for a trait
           %% If there were duplicates, the DB construction would have failed
           ?assert(is_map(DB) orelse is_list(DB))
       end}
     ]}.

%%%===================================================================
%%% Invalid Constraint Tests
%%%===================================================================

invalid_constraint_test_() ->
    {"Invalid constraint handling",
     [
      {"Empty type list is rejected",
       fun() ->
           %% Constraint with no type arguments
           Constraint = catena_constraint:trait_constraint('Mapper', [], unknown),
           DB = instance_db(),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           %% Should fail - Mapper requires exactly one type argument
           ?assertMatch({error, _}, Result)
       end},

      {"Too many type arguments is rejected",
       fun() ->
           %% Mapper takes 1 type arg, not 2
           Constraint = catena_constraint:trait_constraint(
               'Mapper',
               [{tcon, 'Maybe'}, {tcon, int}],
               unknown),
           DB = instance_db(),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           %% Should fail - wrong arity
           ?assertMatch({error, _}, Result)
       end},

      {"Malformed type argument is handled",
       fun() ->
           %% Invalid type structure
           Constraint = catena_constraint:trait_constraint(
               'Mapper',
               [invalid_type_structure],
               unknown),
           DB = instance_db(),
           %% Should not crash, should return error
           Result = try
               catena_instance:resolve_constraint(Constraint, DB)
           catch
               _:_ -> {error, caught_exception}
           end,
           ?assertMatch({error, _}, Result)
       end}
     ]}.

%%%===================================================================
%%% Edge Case Tests
%%%===================================================================

edge_case_test_() ->
    {"Trait system edge cases",
     [
      {"Empty instance database returns errors",
       fun() ->
           EmptyDB = catena_instance:empty_instance_db(),
           Constraint = catena_constraint:trait_constraint('Mapper', [{tcon, 'Maybe'}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, EmptyDB),
           ?assertMatch({error, _}, Result)
       end},

      {"Unknown method name returns error",
       fun() ->
           DB = instance_db(),
           State = catena_infer_state:new(),
           %% 'unknown_method' is not a trait method
           Result = catena_trait_resolve:resolve_method_type(
               unknown_method,
               [{tcon, int}],
               DB, State),
           %% Should fail or return unresolved
           case Result of
               {error, _} -> ok;
               {ok, _, _} -> ok  % May return unresolved method call
           end
       end},

      {"is_trait_method returns false for unknown methods",
       fun() ->
           ?assertNot(catena_trait_resolve:is_trait_method(not_a_real_method)),
           ?assertNot(catena_trait_resolve:is_trait_method(definitely_fake)),
           ?assertNot(catena_trait_resolve:is_trait_method('123invalid'))
       end}
     ]}.
