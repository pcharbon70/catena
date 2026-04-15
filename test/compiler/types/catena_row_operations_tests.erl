-module(catena_row_operations_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_row_operations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"effect union with rows", fun test_effect_union_rows/0},
        {"effect union with options", fun test_effect_union_options/0},
        {"effect difference", fun test_effect_difference/0},
        {"effect subsumes", fun test_effect_subsumes/0},
        {"effect subsumes with options", fun test_effect_subsumes_options/0},
        {"effect normalize", fun test_effect_normalize/0},
        {"effect validate", fun test_effect_validate/0},
        {"extend effect row", fun test_extend_effect_row/0},
        {"restrict effect row", fun test_restrict_effect_row/0},
        {"effect equality", fun test_effect_eq/0},
        {"effect subset", fun test_effect_subset/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Effect Union Tests
%%%---------------------------------------------------------------------

test_effect_union_rows() ->
    Row1 = catena_row_types:effect_row([a, b]),
    Row2 = catena_row_types:effect_row([c, d]),

    Union = catena_row_operations:effect_union_rows(Row1, Row2),
    UnionList = catena_row_types:row_to_list(Union),
    ?assertEqual([a, b, c, d], lists:sort(UnionList)).

test_effect_union_options() ->
    Row1 = catena_row_types:effect_row([c, a, b]),
    Row2 = catena_row_types:effect_row([d, c]),

    %% With normalization
    Normalized = catena_row_operations:effect_union_rows(Row1, Row2, #{normalize => true}),
    ?assertEqual([a, b, c, d], catena_row_types:row_to_list(Normalized)).

%%%---------------------------------------------------------------------
%%% Effect Difference Tests
%%%---------------------------------------------------------------------

test_effect_difference() ->
    Row1 = catena_row_types:effect_row([a, b, c, d]),
    Row2 = catena_row_types:effect_row([b, c]),

    Diff = catena_row_operations:effect_difference(Row1, Row2),
    DiffList = catena_row_types:row_to_list(Diff),
    ?assertEqual([a, d], lists:sort(DiffList)).

%%%---------------------------------------------------------------------
%%% Effect Subsumption Tests
%%%---------------------------------------------------------------------

test_effect_subsumes() ->
    Row1 = catena_row_types:effect_row([a, b, c]),
    Row2 = catena_row_types:effect_row([a, b]),
    Row3 = catena_row_types:effect_row([a, b, d]),

    ?assert(catena_row_operations:effect_subsumes(Row1, Row2)),
    ?assertNot(catena_row_operations:effect_subsumes(Row1, Row3)),
    ?assert(catena_row_operations:effect_subsumes(Row1, Row1)).

test_effect_subsumes_options() ->
    Row1 = catena_row_types:effect_row([a, b]),
    Row2 = catena_row_types:effect_row([a]),

    %% Allow row vars
    ?assert(catena_row_operations:effect_subsumes(Row1, Row2, #{allow_row_vars => true})),

    %% Strict mode
    ?assert(catena_row_operations:effect_subsumes(Row1, Row2, #{strict => true})).

%%%---------------------------------------------------------------------
%%% Effect Normalization Tests
%%%---------------------------------------------------------------------

test_effect_normalize() ->
    Row = catena_row_types:effect_row([c, a, b, a]),
    Normalized = catena_row_operations:effect_normalize(Row),
    ?assertEqual([a, b, c], catena_row_types:row_to_list(Normalized)).

%%%---------------------------------------------------------------------
%%% Effect Validation Tests
%%%---------------------------------------------------------------------

test_effect_validate() ->
    ValidRow = catena_row_types:effect_row(['State', 'IO']),
    ?assert(catena_row_operations:is_valid_effect_set(ValidRow)).

%%%---------------------------------------------------------------------
%%% Extend Effect Row Tests
%%%---------------------------------------------------------------------

test_extend_effect_row() ->
    Row = catena_row_types:effect_row([a, b]),
    Extended = catena_row_operations:extend_effect_row(Row, [c, d]),
    ExtendedList = catena_row_types:row_to_list(Extended),
    ?assertEqual([a, b, c, d], lists:sort(ExtendedList)).

%%%---------------------------------------------------------------------
%%% Restrict Effect Row Tests
%%%---------------------------------------------------------------------

test_restrict_effect_row() ->
    Row = catena_row_types:effect_row([a, b, c, d]),
    Restricted = catena_row_operations:restrict_effect_row(Row, [a, c]),
    ?assertEqual([a, c], lists:sort(catena_row_types:row_to_list(Restricted))).

%%%---------------------------------------------------------------------
%%% Effect Equality Tests
%%%---------------------------------------------------------------------

test_effect_eq() ->
    Row1 = catena_row_types:effect_row([a, b, c]),
    Row2 = catena_row_types:effect_row([c, a, b]),
    Row3 = catena_row_types:effect_row([a, b]),

    ?assert(catena_row_operations:effect_eq(Row1, Row2)),
    ?assertNot(catena_row_operations:effect_eq(Row1, Row3)).

%%%---------------------------------------------------------------------
%%% Effect Subset Tests
%%%---------------------------------------------------------------------

test_effect_subset() ->
    Row1 = catena_row_types:effect_row([a, b]),
    Row2 = catena_row_types:effect_row([a, b, c]),
    Row3 = catena_row_types:effect_row([a, b, d]),

    ?assert(catena_row_operations:effect_subset(Row1, Row2)),
    ?assert(catena_row_operations:effect_subset(Row1, Row1)),
    ?assertNot(catena_row_operations:effect_subset(Row3, Row2)).
