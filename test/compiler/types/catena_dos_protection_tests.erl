%%%-------------------------------------------------------------------
%%% @doc DoS Protection Tests
%%%
%%% Tests security limits that protect against denial-of-service attacks
%%% via unbounded resource usage.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_dos_protection_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    ok.

teardown(_) ->
    ok.

%%====================================================================
%% Configuration Tests
%%====================================================================

configuration_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_configuration_constants()),
      ?_test(test_validation_functions())
     ]}.

test_configuration_constants() ->
    % Test that all configuration constants return positive integers
    ?assert(catena_type_config:max_constraint_set_size() > 0),
    ?assert(catena_type_config:max_instances_per_trait() > 0),
    ?assert(catena_type_config:max_resolution_depth() > 0),
    ?assert(catena_type_config:max_type_depth() > 0),

    % Reasonable limits
    ?assert(catena_type_config:max_constraint_set_size() >= 100),
    ?assert(catena_type_config:max_instances_per_trait() >= 100),
    ?assert(catena_type_config:max_resolution_depth() >= 10).

test_validation_functions() ->
    % Test constraint set size validation
    ?assertEqual(ok, catena_type_config:check_constraint_set_size(0)),
    ?assertEqual(ok, catena_type_config:check_constraint_set_size(100)),
    ?assertEqual(ok, catena_type_config:check_constraint_set_size(1000)),
    ?assertMatch(
        {error, {constraint_set_too_large, 1001, 1000}},
        catena_type_config:check_constraint_set_size(1001)
    ),

    % Test instance count validation
    ?assertEqual(ok, catena_type_config:check_instance_count('Eq', 0)),
    ?assertEqual(ok, catena_type_config:check_instance_count('Eq', 5000)),
    ?assertEqual(ok, catena_type_config:check_instance_count('Eq', 10000)),
    ?assertMatch(
        {error, {too_many_instances, 'Eq', 10001, 10000}},
        catena_type_config:check_instance_count('Eq', 10001)
    ),

    % Test resolution depth validation
    ?assertEqual(ok, catena_type_config:check_resolution_depth(0)),
    ?assertEqual(ok, catena_type_config:check_resolution_depth(50)),
    ?assertEqual(ok, catena_type_config:check_resolution_depth(100)),
    ?assertMatch(
        {error, {resolution_depth_exceeded, 101, 100}},
        catena_type_config:check_resolution_depth(101)
    ).

%%====================================================================
%% Constraint Set Size Limit Tests
%%====================================================================

constraint_size_limits_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_add_constraint_within_limit()),
      ?_test(test_add_constraint_exceeds_limit()),
      ?_test(test_add_constraints_within_limit()),
      ?_test(test_add_constraints_exceeds_limit()),
      ?_test(test_simplify_within_limit()),
      ?_test(test_simplify_exceeds_limit())
     ]}.

test_add_constraint_within_limit() ->
    % Should succeed for small constraint sets
    Constraint = catena_constraint:trait_constraint('Eq', [catena_types:tcon(int)]),
    Set = catena_constraint:empty_constraint_set(),

    % Add one constraint
    Set1 = catena_constraint:add_constraint(Constraint, Set),
    ?assertEqual(1, length(Set1)),

    % Add multiple constraints (under limit)
    Set100 = lists:foldl(
        fun(_, Acc) ->
            catena_constraint:add_constraint(Constraint, Acc)
        end,
        Set,
        lists:seq(1, 100)
    ),
    ?assertEqual(100, length(Set100)).

test_add_constraint_exceeds_limit() ->
    % Build a constraint set at the limit
    Constraint = catena_constraint:trait_constraint('Eq', [catena_types:tcon(int)]),
    Set = catena_constraint:empty_constraint_set(),

    % Build constraint list up to limit (1000)
    LargeSet = lists:foldl(
        fun(_, Acc) -> [Constraint | Acc] end,
        [],
        lists:seq(1, 1000)
    ),

    % Adding one more should fail
    ?assertError(
        {constraint_set_too_large, 1001, 1000},
        catena_constraint:add_constraint(Constraint, LargeSet)
    ).

test_add_constraints_within_limit() ->
    Constraint = catena_constraint:trait_constraint('Eq', [catena_types:tcon(int)]),

    % Create two small sets
    Set1 = [Constraint, Constraint],
    Set2 = [Constraint, Constraint, Constraint],

    % Combine them (should succeed)
    Combined = catena_constraint:add_constraints(Set1, Set2),
    ?assertEqual(5, length(Combined)).

test_add_constraints_exceeds_limit() ->
    Constraint = catena_constraint:trait_constraint('Eq', [catena_types:tcon(int)]),

    % Create two sets that together exceed limit
    Set1 = lists:duplicate(600, Constraint),
    Set2 = lists:duplicate(600, Constraint),

    % Combining should fail (1200 > 1000)
    ?assertError(
        {constraint_set_too_large, 1200, 1000},
        catena_constraint:add_constraints(Set1, Set2)
    ).

test_simplify_within_limit() ->
    % Simplify should work for reasonable constraint sets
    Constraint1 = catena_constraint:trait_constraint('Eq', [catena_types:tcon(int)]),
    Constraint2 = catena_constraint:trait_constraint('Ord', [catena_types:tcon(int)]),

    Set = [Constraint1, Constraint2, Constraint1],  % Has duplicate

    Simplified = catena_constraint:simplify(Set),
    ?assert(length(Simplified) =< 3).

test_simplify_exceeds_limit() ->
    % Create a large constraint set exceeding limit
    Constraint = catena_constraint:trait_constraint('Eq', [catena_types:tcon(int)]),
    LargeSet = lists:duplicate(1500, Constraint),

    % Simplify should fail due to size check
    ?assertError(
        {constraint_set_too_large, 1500, 1000},
        catena_constraint:simplify(LargeSet)
    ).

%%====================================================================
%% Instance Database Size Limit Tests
%%====================================================================

instance_size_limits_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_add_instance_within_limit()),
      ?_test(test_add_instance_exceeds_limit())
     ]}.

test_add_instance_within_limit() ->
    % Should succeed for reasonable instance counts
    Instance1 = catena_instance:make_instance('Eq', [catena_types:tcon(int)]),
    Instance2 = catena_instance:make_instance('Eq', [catena_types:tcon(float)]),

    DB0 = catena_instance:empty_instance_db(),
    DB1 = catena_instance:add_instance(Instance1, DB0),
    DB2 = catena_instance:add_instance(Instance2, DB1),

    Instances = catena_instance:get_instances('Eq', DB2),
    ?assertEqual(2, length(Instances)).

test_add_instance_exceeds_limit() ->
    % Build instance database at limit (10000 instances for 'Eq')
    Instance = catena_instance:make_instance('Eq', [catena_types:tcon(int)]),

    % Build up to limit
    DB = lists:foldl(
        fun(N, AccDB) ->
            % Use different type vars to avoid duplicates
            {TVar, _} = catena_types:fresh_var(catena_infer_state:new()),
            Inst = catena_instance:make_instance('Eq', [TVar]),
            catena_instance:add_instance(Inst, AccDB)
        end,
        catena_instance:empty_instance_db(),
        lists:seq(1, 9999)
    ),

    % Adding 10000th instance should succeed
    DB10k = catena_instance:add_instance(Instance, DB),
    ?assertEqual(10000, length(catena_instance:get_instances('Eq', DB10k))),

    % Adding 10001st should fail
    ?assertError(
        {too_many_instances, 'Eq', 10001, 10000},
        catena_instance:add_instance(Instance, DB10k)
    ).

%%====================================================================
%% Resolution Depth Limit Tests
%%====================================================================

resolution_depth_limits_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_resolve_within_depth_limit()),
      ?_test(test_resolve_exceeds_depth_limit())
     ]}.

test_resolve_within_depth_limit() ->
    % Normal constraint resolution should work (depth = 0)
    DB = catena_builtin_instances:builtin_instance_db(),
    Constraint = catena_constraint:trait_constraint('Eq', [catena_types:tcon(int)]),

    Result = catena_instance:resolve_constraint(Constraint, DB),
    ?assertMatch({ok, _, _}, Result).

test_resolve_exceeds_depth_limit() ->
    % Test internal depth-tracking function directly
    DB = catena_builtin_instances:builtin_instance_db(),
    Constraint = catena_constraint:trait_constraint('Eq', [catena_types:tcon(int)]),

    % Depth 100 should work
    ?assertMatch(
        {ok, _, _},
        catena_instance:resolve_constraint_depth(Constraint, DB, 100)
    ),

    % Depth 101 should fail
    ?assertError(
        {resolution_depth_exceeded, 101, 100},
        catena_instance:resolve_constraint_depth(Constraint, DB, 101)
    ).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_realistic_constraint_sizes()),
      ?_test(test_realistic_instance_counts())
     ]}.

test_realistic_constraint_sizes() ->
    % Realistic constraint sets should all work
    % Test 10 constraints (typical for complex types)
    Constraints = [
        catena_constraint:trait_constraint('Eq', [catena_types:tcon(int)]),
        catena_constraint:trait_constraint('Ord', [catena_types:tcon(int)]),
        catena_constraint:trait_constraint('Show', [catena_types:tcon(int)]),
        catena_constraint:trait_constraint('Eq', [catena_types:tcon(float)]),
        catena_constraint:trait_constraint('Ord', [catena_types:tcon(float)]),
        catena_constraint:trait_constraint('Show', [catena_types:tcon(float)]),
        catena_constraint:trait_constraint('Functor', [catena_types:tcon(list)]),
        catena_constraint:trait_constraint('Monad', [catena_types:tcon(list)]),
        catena_constraint:trait_constraint('Functor', [catena_types:tcon('Maybe')]),
        catena_constraint:trait_constraint('Monad', [catena_types:tcon('Maybe')])
    ],

    % Should be able to add all
    Set = lists:foldl(
        fun(C, Acc) -> catena_constraint:add_constraint(C, Acc) end,
        catena_constraint:empty_constraint_set(),
        Constraints
    ),

    ?assertEqual(10, length(Set)),

    % Should be able to simplify
    Simplified = catena_constraint:simplify(Set),
    ?assert(length(Simplified) =< 10).

test_realistic_instance_counts() ->
    % Realistic instance counts should work
    % Typical trait might have 50-100 instances
    DB = lists:foldl(
        fun(N, AccDB) ->
            % Create instance for different numeric types
            Type = case N rem 5 of
                0 -> catena_types:tcon(int);
                1 -> catena_types:tcon(float);
                2 -> catena_types:tcon(bool);
                3 -> catena_types:tcon(string);
                4 -> catena_types:tcon(atom)
            end,
            Instance = catena_instance:make_instance('Show', [Type]),
            catena_instance:add_instance(Instance, AccDB)
        end,
        catena_instance:empty_instance_db(),
        lists:seq(1, 100)
    ),

    Instances = catena_instance:get_instances('Show', DB),
    ?assertEqual(100, length(Instances)).
