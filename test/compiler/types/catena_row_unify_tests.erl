-module(catena_row_unify_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_row_unify_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"unify identical rows", fun test_unify_identical/0},
        {"unify empty rows", fun test_unify_empty/0},
        {"unify rows with same elements", fun test_unify_same_elements/0},
        {"unify rows with different elements fails", fun test_unify_different_fails/0},
        {"unify rows with row variables", fun test_unify_row_vars/0},
        {"unify closed row with open row binds missing effects", fun test_unify_open_closed/0},
        {"row occurs check", fun test_row_occurs/0},
        {"apply row substitution", fun test_apply_subst/0},
        {"compose row substitutions", fun test_compose_subst/0},
        {"generate row constraints", fun test_generate_constraints/0},
        {"solve row constraints", fun test_solve_constraints/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Unification Tests
%%%---------------------------------------------------------------------

test_unify_identical() ->
    Row = catena_row_types:effect_row([a, b, c]),
    ?assertMatch({ok, _}, catena_row_unify:unify_rows(Row, Row)).

test_unify_empty() ->
    Empty1 = catena_row_types:empty_row(),
    Empty2 = catena_row_types:empty_row(),
    ?assertMatch({ok, _}, catena_row_unify:unify_rows(Empty1, Empty2)).

test_unify_same_elements() ->
    Row1 = catena_row_types:effect_row([a, b, c]),
    Row2 = catena_row_types:effect_row([c, b, a]),
    ?assertMatch({ok, _}, catena_row_unify:unify_rows(Row1, Row2)).

test_unify_different_fails() ->
    Row1 = catena_row_types:effect_row([a, b]),
    Row2 = catena_row_types:effect_row([c, d]),
    ?assertMatch({error, _}, catena_row_unify:unify_rows(Row1, Row2)).

test_unify_row_vars() ->
    Var = catena_row_types:row_var(),
    Row1 = catena_row_types:effect_row([a]),
    Row2 = maps:put(row_var, Var, catena_row_types:effect_row([a])),
    ?assertMatch({ok, _}, catena_row_unify:unify_rows(Row1, Row2)).

test_unify_open_closed() ->
    Var = catena_row_types:row_var(),
    VarId = catena_row_types:row_var_id(Var),
    Closed = catena_row_types:effect_row([a, b, c]),
    Open = catena_row_types:effect_row([a], Var),

    {ok, Subst} = catena_row_unify:unify_rows(Closed, Open),
    ?assertEqual(catena_row_types:effect_row([b, c]), maps:get(VarId, Subst)).

%%%---------------------------------------------------------------------
%%% Row Occurs Check Tests
%%%---------------------------------------------------------------------

test_row_occurs() ->
    Var = catena_row_types:row_var(),
    VarId = catena_row_types:row_var_id(Var),

    RowWithVar = maps:put(row_var, Var, catena_row_types:effect_row([])),
    ?assert(catena_row_unify:row_occurs(VarId, RowWithVar)),

    RowWithoutVar = catena_row_types:effect_row([a, b]),
    ?assertNot(catena_row_unify:row_occurs(VarId, RowWithoutVar)).

%%%---------------------------------------------------------------------
%%% Substitution Tests
%%%---------------------------------------------------------------------

test_apply_subst() ->
    Var = catena_row_types:row_var(),
    VarId = catena_row_types:row_var_id(Var),
    Subst = #{VarId => catena_row_types:effect_row([c])},
    Row = catena_row_types:effect_row([a, b], Var),

    ?assertEqual(
        catena_row_types:effect_row([a, b, c]),
        catena_row_unify:apply_row_subst(Row, Subst)
    ).

test_compose_subst() ->
    Var1 = catena_row_types:row_var(),
    Var2 = catena_row_types:row_var(),
    VarId1 = catena_row_types:row_var_id(Var1),
    VarId2 = catena_row_types:row_var_id(Var2),

    Subst1 = #{VarId1 => catena_row_types:effect_row([a], Var2)},
    Subst2 = #{VarId2 => catena_row_types:effect_row([b])},

    Composed = catena_row_unify:compose_row_subst(Subst1, Subst2),
    ?assertEqual(
        catena_row_types:effect_row([a, b]),
        maps:get(VarId1, Composed)
    ),
    ?assertEqual(catena_row_types:effect_row([b]), maps:get(VarId2, Composed)).

%%%---------------------------------------------------------------------
%%% Constraint Tests
%%%---------------------------------------------------------------------

test_generate_constraints() ->
    Row1 = catena_row_types:effect_row([a]),
    Row2 = catena_row_types:effect_row([b]),

    Constraints = catena_row_unify:generate_row_constraints(Row1, Row2),
    ?assertEqual(1, length(Constraints)),
    ?assertMatch({unify, _, _}, hd(Constraints)).

test_solve_constraints() ->
    Row1 = catena_row_types:effect_row([a, b]),
    Row2 = catena_row_types:effect_row([b, a]),

    Constraints = catena_row_unify:generate_row_constraints(Row1, Row2),
    ?assertMatch({ok, _}, catena_row_unify:solve_row_constraints(Constraints)).

%%%---------------------------------------------------------------------
%%% Utility Tests
%%%---------------------------------------------------------------------

test_row_subst_utilities() ->
    EmptySubst = catena_row_unify:empty_row_subst(),
    ?assertEqual(#{}, EmptySubst),

    Var = catena_row_types:row_var(),
    VarId = catena_row_types:row_var_id(Var),
    Row = catena_row_types:effect_row([a]),

    List = [{VarId, Row}],
    Subst = catena_row_unify:list_to_row_subst(List),
    ?assertEqual(List, catena_row_unify:row_subst_to_list(Subst)).

row_subst_utilities_test() ->
    test_row_subst_utilities().
