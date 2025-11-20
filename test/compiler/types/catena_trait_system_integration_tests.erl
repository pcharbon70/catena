%%%-------------------------------------------------------------------
%%% @doc Integration Tests for Trait Constraint System (Task 1.2.6)
%%%
%%% End-to-end tests that verify the complete trait system works together:
%%% - Trait hierarchy checking
%%% - Instance resolution with superclass constraints
%%% - Method type checking
%%% - Coherence validation
%%% @end
%%%-------------------------------------------------------------------
-module(catena_trait_system_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Trait definitions for Ord extends Eq scenario
ord_eq_trait_defs() ->
    #{
        'Eq' => {'Eq', [], {location, 1, 1}},
        'Ord' => {'Ord', ['Eq'], {location, 5, 1}}
    }.

%% Trait definitions with diamond hierarchy
%% D extends B, C; B extends A; C extends A
diamond_trait_defs() ->
    #{
        'A' => {'A', [], {location, 1, 1}},
        'B' => {'B', ['A'], {location, 5, 1}},
        'C' => {'C', ['A'], {location, 10, 1}},
        'D' => {'D', ['B', 'C'], {location, 15, 1}}
    }.

%% Instance database with Eq and Ord for Int
eq_ord_instance_db() ->
    DB1 = catena_instance:add_instance(
        catena_instance:make_instance('Eq', [{tcon, int}], {location, 20, 1}),
        catena_instance:empty_instance_db()
    ),
    catena_instance:add_instance(
        catena_instance:make_instance('Ord', [{tcon, int}], {location, 25, 1}),
        DB1
    ).

%%====================================================================
%% Trait Hierarchy Integration Tests
%%====================================================================

trait_hierarchy_integration_test_() ->
    [
        ?_test(test_ord_eq_hierarchy_valid()),
        ?_test(test_diamond_hierarchy_valid()),
        ?_test(test_supertraits_resolution())
    ].

test_ord_eq_hierarchy_valid() ->
    TraitDefs = ord_eq_trait_defs(),
    Result = catena_trait_hierarchy:check_hierarchy(TraitDefs),
    ?assertEqual({ok, valid}, Result).

test_diamond_hierarchy_valid() ->
    TraitDefs = diamond_trait_defs(),
    Result = catena_trait_hierarchy:check_hierarchy(TraitDefs),
    ?assertEqual({ok, valid}, Result).

test_supertraits_resolution() ->
    TraitDefs = ord_eq_trait_defs(),
    %% Get all supertraits of Ord should include Eq
    {ok, Supers} = catena_trait_hierarchy:get_all_supertraits('Ord', TraitDefs),
    ?assertEqual(['Eq'], Supers).

%%====================================================================
%% Instance Resolution Integration Tests
%%====================================================================

instance_resolution_integration_test_() ->
    [
        ?_test(test_basic_instance_resolution()),
        ?_test(test_resolution_with_superclass_constraints()),
        ?_test(test_missing_instance_error())
    ].

test_basic_instance_resolution() ->
    DB = eq_ord_instance_db(),
    Constraint = catena_constraint:trait_constraint('Eq', [{tcon, int}], {location, 30, 1}),
    Result = catena_instance:resolve_constraint(Constraint, DB),
    ?assertMatch({ok, _, _}, Result).

test_resolution_with_superclass_constraints() ->
    DB = eq_ord_instance_db(),
    TraitDefs = ord_eq_trait_defs(),
    Constraint = catena_constraint:trait_constraint('Ord', [{tcon, int}], {location, 30, 1}),

    %% Resolve Ord Int and get superclass constraints
    Result = catena_instance:resolve_with_superclasses(Constraint, DB, TraitDefs),
    ?assertMatch({ok, _, _, _}, Result),
    {ok, _Instance, _Subst, SuperConstraints} = Result,

    %% Should generate Eq Int constraint
    ?assertEqual(1, length(SuperConstraints)),
    [{trait, 'Eq', [{tcon, int}], _}] = SuperConstraints.

test_missing_instance_error() ->
    DB = catena_instance:empty_instance_db(),
    Constraint = catena_constraint:trait_constraint('Eq', [{tcon, int}], {location, 30, 1}),
    Result = catena_instance:resolve_constraint(Constraint, DB),
    ?assertEqual({error, no_instance}, Result).

%%====================================================================
%% Coherence Integration Tests
%%====================================================================

coherence_integration_test_() ->
    [
        ?_test(test_coherent_database()),
        ?_test(test_overlapping_instances_detected()),
        ?_test(test_new_instance_coherence_check())
    ].

test_coherent_database() ->
    %% Database with non-overlapping instances
    DB = eq_ord_instance_db(),
    Result = catena_coherence:check_instance_db(DB),
    ?assertEqual({ok, coherent}, Result).

test_overlapping_instances_detected() ->
    %% Add overlapping instances: Eq Int and Eq α (would overlap)
    DB1 = catena_instance:add_instance(
        catena_instance:make_instance('Eq', [{tcon, int}], loc),
        catena_instance:empty_instance_db()
    ),
    DB2 = catena_instance:add_instance(
        catena_instance:make_instance('Eq', [{tvar, 1}], loc),  % Eq α overlaps with Eq Int
        DB1
    ),
    Result = catena_coherence:check_instance_db(DB2),
    %% Returns {error, [overlap_errors]}
    ?assertMatch({error, [{overlap, _, _, _} | _]}, Result).

test_new_instance_coherence_check() ->
    DB = catena_instance:add_instance(
        catena_instance:make_instance('Eq', [{tcon, int}], loc),
        catena_instance:empty_instance_db()
    ),
    %% Check a non-overlapping instance
    NewInstance = catena_instance:make_instance('Eq', [{tcon, bool}], loc),
    Result = catena_coherence:check_new_instance(NewInstance, DB),
    ?assertEqual({ok, no_overlap}, Result).

%%====================================================================
%% Full Workflow Integration Tests
%%====================================================================

full_workflow_integration_test_() ->
    [
        ?_test(test_full_trait_constraint_workflow()),
        ?_test(test_hierarchy_and_resolution_combined())
    ].

test_full_trait_constraint_workflow() ->
    %% Complete workflow:
    %% 1. Validate trait hierarchy
    %% 2. Build instance database
    %% 3. Resolve constraints with superclasses
    %% 4. Check coherence

    %% Step 1: Validate hierarchy
    TraitDefs = ord_eq_trait_defs(),
    ?assertEqual({ok, valid}, catena_trait_hierarchy:check_hierarchy(TraitDefs)),

    %% Step 2: Build instance database
    DB = eq_ord_instance_db(),

    %% Step 3: Check coherence
    ?assertEqual({ok, coherent}, catena_coherence:check_instance_db(DB)),

    %% Step 4: Resolve constraint with superclasses
    Constraint = catena_constraint:trait_constraint('Ord', [{tcon, int}], loc),
    {ok, _Inst, _Subst, SuperConstraints} = catena_instance:resolve_with_superclasses(
        Constraint, DB, TraitDefs
    ),

    %% Step 5: Resolve all superclass constraints
    Results = lists:map(
        fun(SC) -> catena_instance:resolve_constraint(SC, DB) end,
        SuperConstraints
    ),
    %% All should succeed
    lists:foreach(
        fun(R) -> ?assertMatch({ok, _, _}, R) end,
        Results
    ).

test_hierarchy_and_resolution_combined() ->
    %% Test with diamond hierarchy
    TraitDefs = diamond_trait_defs(),
    ?assertEqual({ok, valid}, catena_trait_hierarchy:check_hierarchy(TraitDefs)),

    %% Get topological order
    {ok, Sorted} = catena_trait_hierarchy:topological_sort(TraitDefs),
    %% A should be first
    ?assertEqual('A', hd(Sorted)),
    %% D should be last
    ?assertEqual('D', lists:last(Sorted)).

%%====================================================================
%% Error Propagation Tests
%%====================================================================

error_propagation_test_() ->
    [
        ?_test(test_cycle_error_format()),
        ?_test(test_unknown_supertrait_error_format())
    ].

test_cycle_error_format() ->
    CycleDefs = #{
        'A' => {'A', ['B'], loc},
        'B' => {'B', ['A'], loc}
    },
    Result = catena_trait_hierarchy:check_hierarchy(CycleDefs),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    %% Format the error
    [FirstError | _] = Errors,
    Msg = catena_trait_hierarchy:format_error(FirstError),
    ?assert(is_list(Msg)),
    ?assert(length(Msg) > 0).

test_unknown_supertrait_error_format() ->
    UnknownDefs = #{
        'A' => {'A', ['Unknown'], loc}
    },
    Result = catena_trait_hierarchy:check_hierarchy(UnknownDefs),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    [{unknown_supertrait, 'A', 'Unknown'}] = Errors,
    Msg = catena_trait_hierarchy:format_error(hd(Errors)),
    ?assert(string:find(Msg, "Unknown") =/= nomatch).
