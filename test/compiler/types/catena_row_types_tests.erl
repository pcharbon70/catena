-module(catena_row_types_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_row_types_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"row type constructors", fun test_row_constructors/0},
        {"row type predicates", fun test_row_predicates/0},
        {"row union", fun test_row_union/0},
        {"row intersection", fun test_row_intersection/0},
        {"row difference", fun test_row_difference/0},
        {"row contains", fun test_row_contains/0},
        {"row normalize", fun test_row_normalize/0},
        {"row to list and back", fun test_row_conversion/0},
        {"row variable management", fun test_row_variables/0},
        {"row validation", fun test_row_validation/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    erase(row_var_counter),
    ok.

cleanup(_) ->
    erase(row_var_counter),
    ok.

%%%---------------------------------------------------------------------
%%% Row Type Constructors Tests
%%%---------------------------------------------------------------------

test_row_constructors() ->
    Empty = catena_row_types:empty_row(),
    ?assertEqual([], catena_row_types:row_to_list(Empty)),

    Row1 = catena_row_types:effect_row([a, b, c]),
    ?assert(catena_row_types:is_effect_row(Row1)),
    ?assertEqual([a, b, c], lists:sort(catena_row_types:row_to_list(Row1))),

    Row2 = catena_row_types:effect_row([a, b, a, c]),
    ?assertEqual([a, b, c], lists:sort(catena_row_types:row_to_list(Row2))),

    Var = catena_row_types:row_var(),
    ?assert(catena_row_types:is_row_var(Var)).

%%%---------------------------------------------------------------------
%%% Row Type Predicates Tests
%%%---------------------------------------------------------------------

test_row_predicates() ->
    Empty = catena_row_types:empty_row(),
    Row = catena_row_types:effect_row([a, b]),
    Var = catena_row_types:row_var(),

    ?assert(catena_row_types:is_effect_row(Empty)),
    ?assert(catena_row_types:is_effect_row(Row)),
    ?assertNot(catena_row_types:is_effect_row(Var)),

    ?assert(catena_row_types:is_row_var(Var)),
    ?assertNot(catena_row_types:is_row_var(Row)),

    ?assert(catena_row_types:is_empty_row(Empty)),
    ?assertNot(catena_row_types:is_empty_row(Row)).

%%%---------------------------------------------------------------------
%%% Row Union Tests
%%%---------------------------------------------------------------------

test_row_union() ->
    R1 = catena_row_types:effect_row([a, b]),
    R2 = catena_row_types:effect_row([b, c, d]),

    Union = catena_row_types:row_union(R1, R2),
    UnionList = catena_row_types:row_to_list(Union),
    ?assertEqual([a, b, c, d], lists:sort(UnionList)),

    Empty = catena_row_types:empty_row(),
    UnionEmpty = catena_row_types:row_union(R1, Empty),
    ?assertEqual([a, b], lists:sort(catena_row_types:row_to_list(UnionEmpty))).

%%%---------------------------------------------------------------------
%%% Row Intersection Tests
%%%---------------------------------------------------------------------

test_row_intersection() ->
    R1 = catena_row_types:effect_row([a, b, c]),
    R2 = catena_row_types:effect_row([b, c, d]),

    Intersect = catena_row_types:row_intersection(R1, R2),
    IntersectList = catena_row_types:row_to_list(Intersect),
    ?assertEqual([b, c], lists:sort(IntersectList)),

    Disjoint1 = catena_row_types:effect_row([a, b]),
    Disjoint2 = catena_row_types:effect_row([c, d]),
    EmptyIntersect = catena_row_types:row_intersection(Disjoint1, Disjoint2),
    ?assert(catena_row_types:is_empty_row(EmptyIntersect)).

%%%---------------------------------------------------------------------
%%% Row Difference Tests
%%%---------------------------------------------------------------------

test_row_difference() ->
    R1 = catena_row_types:effect_row([a, b, c, d]),
    R2 = catena_row_types:effect_row([b, c]),

    Diff = catena_row_types:row_difference(R1, R2),
    DiffList = catena_row_types:row_to_list(Diff),
    ?assertEqual([a, d], lists:sort(DiffList)),

    Empty = catena_row_types:empty_row(),
    DiffEmpty = catena_row_types:row_difference(R1, Empty),
    ?assertEqual([a, b, c, d], lists:sort(catena_row_types:row_to_list(DiffEmpty))).

%%%---------------------------------------------------------------------
%%% Row Contains Tests
%%%---------------------------------------------------------------------

test_row_contains() ->
    Row = catena_row_types:effect_row([a, b, c]),

    ?assert(catena_row_types:row_contains(Row, a)),
    ?assert(catena_row_types:row_contains(Row, b)),
    ?assert(catena_row_types:row_contains(Row, c)),
    ?assertNot(catena_row_types:row_contains(Row, d)),

    Empty = catena_row_types:empty_row(),
    ?assertNot(catena_row_types:row_contains(Empty, a)).

%%%---------------------------------------------------------------------
%%% Row Normalize Tests
%%%---------------------------------------------------------------------

test_row_normalize() ->
    Row = catena_row_types:effect_row([c, a, b, a]),
    Normalized = catena_row_types:row_normalize(Row),
    NormalizedList = catena_row_types:row_to_list(Normalized),
    ?assertEqual([a, b, c], NormalizedList).

%%%---------------------------------------------------------------------
%%% Row Conversion Tests
%%%---------------------------------------------------------------------

test_row_conversion() ->
    Row = catena_row_types:effect_row([a, b, c]),
    List = catena_row_types:row_to_list(Row),
    ?assertEqual([a, b, c], List),

    Converted = catena_row_types:list_to_row(List),
    ?assertEqual([a, b, c], catena_row_types:row_to_list(Converted)).

%%%---------------------------------------------------------------------
%%% Row Variable Management Tests
%%%---------------------------------------------------------------------

test_row_variables() ->
    Var1 = catena_row_types:fresh_row_var(),
    Var2 = catena_row_types:fresh_row_var(),

    ?assert(catena_row_types:is_row_var(Var1)),
    ?assert(catena_row_types:is_row_var(Var2)),

    Id1 = catena_row_types:row_var_id(Var1),
    Id2 = catena_row_types:row_var_id(Var2),
    ?assertNotEqual(Id1, Id2).

%%%---------------------------------------------------------------------
%%% Row Validation Tests
%%%---------------------------------------------------------------------

test_row_validation() ->
    Row = catena_row_types:effect_row(['State', 'FileIO']),

    ?assert(catena_row_types:is_valid_row(Row)),

    %% Test with actual uppercase atoms (common for effect names)
    ?assert(catena_row_types:is_valid_effect('State')),
    ?assert(catena_row_types:is_valid_effect('FileIO')),
    ?assert(catena_row_types:is_valid_effect('IO')),
    ?assertNot(catena_row_types:is_valid_effect(state)),
    ?assertNot(catena_row_types:is_valid_effect(123)).

row_validation_test() ->
    test_row_validation().
