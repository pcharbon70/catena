-module(catena_row_constraints_tests).
-include_lib("eunit/include/eunit.hrl").

generate_constraints_test() ->
    Row1 = catena_row_types:effect_row([a]),
    Row2 = catena_row_types:effect_row([a, b]),
    Constraints = catena_row_constraints:generate_constraints(Row1, Row2),
    ?assertEqual([{unify, Row1, Row2}], Constraints).

solve_constraints_test() ->
    Var = catena_row_types:row_var(),
    VarId = catena_row_types:row_var_id(Var),
    Closed = catena_row_types:effect_row([a, b]),
    Open = catena_row_types:effect_row([a], Var),

    {ok, Subst} = catena_row_constraints:solve_constraints(
        catena_row_constraints:generate_constraints(Closed, Open)
    ),
    ?assertEqual(catena_row_types:effect_row([b]), maps:get(VarId, Subst)).

merge_constraints_test() ->
    Row1 = catena_row_types:effect_row([a]),
    Row2 = catena_row_types:effect_row([b]),
    Left = [{unify, Row1, Row1}],
    Right = [{unify, Row1, Row2}],
    ?assertEqual(Left ++ Right, catena_row_constraints:merge_constraints(Left, Right)).
