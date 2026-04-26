-module(catena_row_polymorphism_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Integration Tests for Row Polymorphism
%%%---------------------------------------------------------------------

%% These integration tests verify the interaction between:
%% - Row type definition and operations
%% - Row unification and substitution
%% - Row polymorphic type inference
%% - Effect set operations with row variables

catena_row_polymorphism_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"row variable creation", fun test_row_variable_creation/0},
        {"row union and intersection", fun test_row_union_intersection/0},
        {"row difference operations", fun test_row_difference/0},
        {"row unification success", fun test_row_unification_success/0},
        {"row unification failure", fun test_row_unification_failure/0},
        {"row occurs check prevents cycles", fun test_row_occurs_check/0},
        {"effect union with row variables", fun test_effect_union_rows/0},
        {"effect difference with rows", fun test_effect_difference_rows/0},
        {"effect subsumption", fun test_effect_subsumption/0},
        {"row variable generalization", fun test_generalization/0},
        {"row variable instantiation", fun test_instantiation/0},
        {"row poly function inference", fun test_function_inference/0},
        {"row poly operation inference", fun test_operation_inference/0},
        {"row poly handler inference", fun test_handler_inference/0},
        {"row poly handler inference with body effects", fun test_handler_inference_with_body_effects/0},
        {"handled effect removal preserves remaining effects", fun test_handled_effect_removal/0},
        {"effect subset failure is reported", fun test_effect_subset_failure/0},
        {"full row polymorphism workflow", fun test_workflow/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

loc() ->
    {location, 1, 1}.

%%%---------------------------------------------------------------------
%%% Row Type Tests
%%%---------------------------------------------------------------------

test_row_variable_creation() ->
    %% Create fresh row variables
    Var1 = catena_row_types:fresh_row_var(),
    Var2 = catena_row_types:fresh_row_var(),

    ?assert(catena_row_types:is_row_var(Var1)),
    ?assert(catena_row_types:is_row_var(Var2)),

    %% Variables should have unique IDs
    Id1 = catena_row_types:row_var_id(Var1),
    Id2 = catena_row_types:row_var_id(Var2),
    ?assertNotEqual(Id1, Id2).

test_row_union_intersection() ->
    Row1 = catena_row_types:effect_row([a, b, c]),
    Row2 = catena_row_types:effect_row([b, c, d]),

    %% Union
    Union = catena_row_types:row_union(Row1, Row2),
    UnionList = catena_row_types:row_to_list(Union),
    ?assertEqual([a, b, c, d], lists:sort(UnionList)),

    %% Intersection
    Intersect = catena_row_types:row_intersection(Row1, Row2),
    IntersectList = catena_row_types:row_to_list(Intersect),
    ?assertEqual([b, c], lists:sort(IntersectList)).

test_row_difference() ->
    Row1 = catena_row_types:effect_row([a, b, c, d]),
    Row2 = catena_row_types:effect_row([b, c]),

    Diff = catena_row_types:row_difference(Row1, Row2),
    DiffList = catena_row_types:row_to_list(Diff),
    ?assertEqual([a, d], lists:sort(DiffList)).

%%%---------------------------------------------------------------------
%%% Row Unification Tests
%%%---------------------------------------------------------------------

test_row_unification_success() ->
    Row1 = catena_row_types:effect_row([a, b, c]),
    Row2 = catena_row_types:effect_row([c, b, a]),

    ?assertMatch({ok, _}, catena_row_unify:unify_rows(Row1, Row2)).

test_row_unification_failure() ->
    Row1 = catena_row_types:effect_row([a, b]),
    Row2 = catena_row_types:effect_row([c, d]),

    ?assertMatch({error, _}, catena_row_unify:unify_rows(Row1, Row2)).

test_row_occurs_check() ->
    Var = catena_row_types:row_var(),
    VarId = catena_row_types:row_var_id(Var),

    %% Row with the variable
    RowWithVar = maps:put(row_var, Var, catena_row_types:effect_row([])),

    ?assert(catena_row_unify:row_occurs(VarId, RowWithVar)).

%%%---------------------------------------------------------------------
%%% Effect Set Operations Tests
%%%---------------------------------------------------------------------

test_effect_union_rows() ->
    Row1 = catena_row_types:effect_row([a, b]),
    Row2 = catena_row_types:effect_row([c, d]),

    Union = catena_row_operations:effect_union_rows(Row1, Row2),
    UnionList = catena_row_types:row_to_list(Union),
    ?assertEqual([a, b, c, d], lists:sort(UnionList)).

test_effect_difference_rows() ->
    Row1 = catena_row_types:effect_row([a, b, c, d]),
    Row2 = catena_row_types:effect_row([b, c]),

    Diff = catena_row_operations:effect_difference(Row1, Row2),
    DiffList = catena_row_types:row_to_list(Diff),
    ?assertEqual([a, d], lists:sort(DiffList)).

test_effect_subsumption() ->
    Row1 = catena_row_types:effect_row([a, b, c]),
    Row2 = catena_row_types:effect_row([a, b]),
    Row3 = catena_row_types:effect_row([a, b, d]),

    ?assert(catena_row_operations:effect_subsumes(Row1, Row2)),
    ?assertNot(catena_row_operations:effect_subsumes(Row1, Row3)).

%%%---------------------------------------------------------------------
%%% Row Polymorphic Inference Tests
%%%---------------------------------------------------------------------

test_generalization() ->
    RowVar = catena_row_types:row_var({row_var, 1}),
    Effects = catena_row_types:effect_row([a, b], RowVar),
    Type = #{
        kind => row_poly,
        effects => Effects,
        row_vars => []
    },
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    {Scheme, _} = catena_row_inference:generalize_row_vars(Type, State),
    ?assertEqual(row_scheme, maps:get(kind, Scheme)),
    ?assertEqual([{row_var, 1}], maps:get(row_vars, Scheme)).

test_instantiation() ->
    OldRowVar = catena_row_types:row_var({row_var, 1}),
    Effects = catena_row_types:effect_row([a], OldRowVar),
    Type = #{
        kind => row_poly,
        effects => Effects,
        row_vars => [{row_var, 1}]
    },
    Scheme = #{
        kind => row_scheme,
        type => Type,
        row_vars => [{row_var, 1}]
    },
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    {Instance, NewState} = catena_row_inference:instantiate_row_vars(Scheme, State),
    ?assertEqual(row_poly, maps:get(kind, Instance)),
    ?assertEqual(1, maps:get(row_var_counter, NewState)),
    ?assertEqual([{row_var, 0}], maps:get(row_vars, Instance)).

test_function_inference() ->
    FunInfo = #{effects => [a, b, c]},
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    {Type, _} = catena_row_inference:infer_row_poly_function(FunInfo, State),
    ?assertEqual(row_poly, maps:get(kind, Type)),

    Effects = maps:get(effects, Type),
    EffectList = catena_row_types:row_to_list(Effects),
    ?assertEqual([a, b, c], lists:sort(EffectList)).

test_operation_inference() ->
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    {Type, NewState} = catena_row_inference:infer_row_poly_operation('State', State),
    ?assertEqual(row_poly, maps:get(kind, Type)),
    ?assertEqual(1, maps:get(row_var_counter, NewState)),
    ?assertEqual([{row_var, 0}], maps:get(row_vars, Type)).

test_handler_inference() ->
    HandlerInfo = #{
        handled => [a, b],
        remaining => [c]
    },
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    {Type, _} = catena_row_inference:infer_row_poly_handler(HandlerInfo, State),
    ?assertEqual(row_poly, maps:get(kind, Type)),
    ?assertEqual([c], catena_row_types:row_to_list(maps:get(effects, Type))).

test_handler_inference_with_body_effects() ->
    HandlerInfo = #{
        handled => [a, b],
        remaining => [d],
        body_effects => [a, b, c]
    },
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    {Type, _} = catena_row_inference:infer_row_poly_handler(HandlerInfo, State),
    ?assertEqual([c, d], catena_row_types:row_to_list(maps:get(effects, Type))).

test_handled_effect_removal() ->
    Expr = {handle_expr,
        {perform_expr, 'IO', read, [], loc()},
        [{handler_clause, 'IO', [
            {operation_case, read, [], {perform_expr, 'State', get, [], loc()}, loc()}
        ], loc()}],
        loc()},

    {Effects, _} = catena_effect_synthesis:synthesize(Expr, catena_infer_state:new()),
    ?assertEqual(['State'], lists:sort(catena_effect_synthesis:to_list(Effects))).

test_effect_subset_failure() ->
    Result = catena_effect_constraints:solve_effect_constraint(
        {effects_subset, declared_effects,
            catena_types:effect_set(['IO', 'State']),
            catena_types:effect_set(['IO'])},
        catena_types:empty_effects(),
        catena_infer_state:new()
    ),
    ?assertMatch({error, _, _}, Result).

%%%---------------------------------------------------------------------
%%% Full Workflow Tests
%%%---------------------------------------------------------------------

test_workflow() ->
    %% Create effect rows
    OpenVar = catena_row_types:row_var({row_var, 5}),
    Row1 = catena_row_types:effect_row([a, b], OpenVar),
    Row2 = catena_row_types:effect_row([b, c]),

    %% Perform operations
    Union = catena_row_operations:effect_union_rows(Row1, Row2),
    Diff = catena_row_operations:effect_difference(Row1, Row2),

    ?assert(catena_row_operations:effect_subsumes(Union, Row1)),
    ?assertNot(catena_row_operations:effect_subsumes(Diff, Row2)),

    %% Type inference
    FunInfo = #{effects => Row1},
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    {Type, _} = catena_row_inference:infer_row_poly_function(FunInfo, State),
    ?assertEqual(row_poly, maps:get(kind, Type)),
    ?assertEqual([{row_var, 5}], maps:get(row_vars, Type)).
