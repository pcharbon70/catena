%%%-------------------------------------------------------------------
%%% @doc Tests for Trait Instance Resolution (Section 1.5.2)
%%%
%%% These tests validate that trait instances resolve correctly during
%%% type checking for the standard library traits.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_trait_resolve_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Get the combined instance database
instance_db() ->
    catena_trait_resolve:get_instance_db().

%%%===================================================================
%%% 1.5.2.1 - Resolve Mapper Instance for Maybe
%%%===================================================================

mapper_maybe_test_() ->
    {"1.5.2.1 - Resolve Mapper instance for Maybe",
     [
      {"Mapper instance exists for Maybe",
       fun() ->
           DB = instance_db(),
           Constraint = catena_constraint:trait_constraint('Mapper', [{tcon, 'Maybe'}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           ?assertMatch({ok, _, _}, Result)
       end},

      {"resolve_method_type works for map on Maybe",
       fun() ->
           DB = instance_db(),
           State = catena_infer_state:new(),
           %% map : (a -> b) -> Maybe a -> Maybe b
           %% Simulate: map f (Some 42)
           FuncType = {tfun, {tcon, int}, {tcon, string}, {effect_set, []}},
           ContainerType = {tapp, {tcon, 'Maybe'}, [{tcon, int}]},
           Result = catena_trait_resolve:resolve_method_type(map, [FuncType, ContainerType], DB, State),
           ?assertMatch({ok, {tapp, {tcon, 'Maybe'}, [_]}, _}, Result)
       end},

      {"is_trait_method recognizes map",
       fun() ->
           ?assert(catena_trait_resolve:is_trait_method(map))
       end},

      {"method_to_trait maps map to Mapper",
       fun() ->
           ?assertEqual('Mapper', catena_trait_resolve:method_to_trait(map))
       end}
     ]}.

%%%===================================================================
%%% 1.5.2.2 - Resolve Mapper Instance for List
%%%===================================================================

mapper_list_test_() ->
    {"1.5.2.2 - Resolve Mapper instance for List",
     [
      {"Mapper instance exists for List",
       fun() ->
           DB = instance_db(),
           Constraint = catena_constraint:trait_constraint('Mapper', [{tcon, list}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           ?assertMatch({ok, _, _}, Result)
       end},

      {"resolve_method_type works for map on List",
       fun() ->
           DB = instance_db(),
           State = catena_infer_state:new(),
           %% map : (a -> b) -> List a -> List b
           FuncType = {tfun, {tcon, int}, {tcon, string}, {effect_set, []}},
           ContainerType = {tapp, {tcon, list}, [{tcon, int}]},
           Result = catena_trait_resolve:resolve_method_type(map, [FuncType, ContainerType], DB, State),
           ?assertMatch({ok, {tapp, {tcon, list}, [_]}, _}, Result)
       end}
     ]}.

%%%===================================================================
%%% 1.5.2.3 - Resolve Constrained Instances (Nested Resolution)
%%%===================================================================

constrained_instances_test_() ->
    {"1.5.2.3 - Resolve constrained instances with nested resolution",
     [
      {"Comparable instance exists for Maybe",
       fun() ->
           DB = instance_db(),
           Constraint = catena_constraint:trait_constraint('Comparable', [{tcon, 'Maybe'}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           ?assertMatch({ok, _, _}, Result)
       end},

      {"Comparable instance exists for List",
       fun() ->
           DB = instance_db(),
           Constraint = catena_constraint:trait_constraint('Comparable', [{tcon, list}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           ?assertMatch({ok, _, _}, Result)
       end},

      {"Comparable instance exists for Int",
       fun() ->
           DB = instance_db(),
           Constraint = catena_constraint:trait_constraint('Comparable', [{tcon, int}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           ?assertMatch({ok, _, _}, Result)
       end},

      {"resolve_method_type works for equals on Int",
       fun() ->
           DB = instance_db(),
           State = catena_infer_state:new(),
           IntType = {tcon, int},
           Result = catena_trait_resolve:resolve_method_type(equals, [IntType, IntType], DB, State),
           ?assertMatch({ok, {tcon, bool}, _}, Result)
       end}
     ]}.

%%%===================================================================
%%% 1.5.2.4 - Detect and Report Missing Instances
%%%===================================================================

missing_instances_test_() ->
    {"1.5.2.4 - Detect and report missing instances",
     [
      {"Missing instance returns error",
       fun() ->
           DB = instance_db(),
           %% NonExistentType doesn't have a Mapper instance
           Constraint = catena_constraint:trait_constraint('Mapper', [{tcon, 'NonExistentType'}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           ?assertMatch({error, no_instance}, Result)
       end},

      {"resolve_method_type returns error for missing instance",
       fun() ->
           DB = instance_db(),
           State = catena_infer_state:new(),
           FuncType = {tfun, {tcon, int}, {tcon, string}, {effect_set, []}},
           %% UnknownContainer doesn't have Mapper instance
           ContainerType = {tapp, {tcon, 'UnknownContainer'}, [{tcon, int}]},
           Result = catena_trait_resolve:resolve_method_type(map, [FuncType, ContainerType], DB, State),
           ?assertMatch({error, {no_instance, 'Mapper', _}}, Result)
       end},

      {"Not a trait method returns error",
       fun() ->
           DB = instance_db(),
           State = catena_infer_state:new(),
           Result = catena_trait_resolve:resolve_method_type(unknown_method, [], DB, State),
           ?assertMatch({error, {not_trait_method, unknown_method}}, Result)
       end}
     ]}.

%%%===================================================================
%%% 1.5.2.5 - Verify Trait Hierarchy Resolution
%%%===================================================================

trait_hierarchy_test_() ->
    {"1.5.2.5 - Verify trait hierarchy resolution",
     [
      {"Pipeline instance exists for Maybe (extends Applicator + Chainable)",
       fun() ->
           DB = instance_db(),
           Constraint = catena_constraint:trait_constraint('Pipeline', [{tcon, 'Maybe'}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           ?assertMatch({ok, _, _}, Result)
       end},

      {"Applicator instance exists for Maybe (extends Mapper)",
       fun() ->
           DB = instance_db(),
           Constraint = catena_constraint:trait_constraint('Applicator', [{tcon, 'Maybe'}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           ?assertMatch({ok, _, _}, Result)
       end},

      {"Chainable instance exists for Maybe (extends Mapper)",
       fun() ->
           DB = instance_db(),
           Constraint = catena_constraint:trait_constraint('Chainable', [{tcon, 'Maybe'}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           ?assertMatch({ok, _, _}, Result)
       end},

      {"Orderable instance exists for Int (extends Comparable)",
       fun() ->
           DB = instance_db(),
           Constraint = catena_constraint:trait_constraint('Orderable', [{tcon, int}], unknown),
           Result = catena_instance:resolve_constraint(Constraint, DB),
           ?assertMatch({ok, _, _}, Result)
       end},

      {"All hierarchy levels work for List",
       fun() ->
           DB = instance_db(),
           %% List should have instances at all levels: Mapper -> Applicator -> Pipeline
           MapperC = catena_constraint:trait_constraint('Mapper', [{tcon, list}], unknown),
           ApplicatorC = catena_constraint:trait_constraint('Applicator', [{tcon, list}], unknown),
           ChainableC = catena_constraint:trait_constraint('Chainable', [{tcon, list}], unknown),
           PipelineC = catena_constraint:trait_constraint('Pipeline', [{tcon, list}], unknown),

           ?assertMatch({ok, _, _}, catena_instance:resolve_constraint(MapperC, DB)),
           ?assertMatch({ok, _, _}, catena_instance:resolve_constraint(ApplicatorC, DB)),
           ?assertMatch({ok, _, _}, catena_instance:resolve_constraint(ChainableC, DB)),
           ?assertMatch({ok, _, _}, catena_instance:resolve_constraint(PipelineC, DB))
       end}
     ]}.

%%%===================================================================
%%% Additional Trait Method Tests
%%%===================================================================

trait_methods_test_() ->
    {"Additional trait method resolution tests",
     [
      {"chain method resolves for Maybe",
       fun() ->
           DB = instance_db(),
           State = catena_infer_state:new(),
           %% chain : (a -> m b) -> m a -> m b
           FuncType = {tfun, {tcon, int}, {tapp, {tcon, 'Maybe'}, [{tcon, string}]}, {effect_set, []}},
           ContainerType = {tapp, {tcon, 'Maybe'}, [{tcon, int}]},
           Result = catena_trait_resolve:resolve_method_type(chain, [FuncType, ContainerType], DB, State),
           ?assertMatch({ok, {tapp, {tcon, 'Maybe'}, [_]}, _}, Result)
       end},

      {"combine method resolves for String",
       fun() ->
           DB = instance_db(),
           State = catena_infer_state:new(),
           StringType = {tcon, string},
           Result = catena_trait_resolve:resolve_method_type(combine, [StringType, StringType], DB, State),
           ?assertMatch({ok, {tcon, string}, _}, Result)
       end},

      {"apply method resolves for List",
       fun() ->
           DB = instance_db(),
           State = catena_infer_state:new(),
           %% apply : f (a -> b) -> f a -> f b
           FuncContainerType = {tapp, {tcon, list}, [{tfun, {tcon, int}, {tcon, string}, {effect_set, []}}]},
           ArgContainerType = {tapp, {tcon, list}, [{tcon, int}]},
           Result = catena_trait_resolve:resolve_method_type(apply, [FuncContainerType, ArgContainerType], DB, State),
           ?assertMatch({ok, {tapp, {tcon, list}, [_]}, _}, Result)
       end}
     ]}.

%%%===================================================================
%%% Instance Database Tests
%%%===================================================================

instance_db_test_() ->
    {"Instance database tests",
     [
      {"stdlib_instance_db creates non-empty database",
       fun() ->
           DB = catena_stdlib_instances:stdlib_instance_db(),
           ?assert(map_size(DB) > 0)
       end},

      {"get_instance_db combines builtin and stdlib",
       fun() ->
           DB = catena_trait_resolve:get_instance_db(),
           %% Should have both Eq (builtin) and Mapper (stdlib)
           EqInstances = catena_instance:get_instances('Eq', DB),
           MapperInstances = catena_instance:get_instances('Mapper', DB),
           ?assert(length(EqInstances) > 0),
           ?assert(length(MapperInstances) > 0)
       end}
     ]}.
