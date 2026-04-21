%%%-------------------------------------------------------------------
%%% @doc Focused tests for Phase 14.2 effect constraints.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_constraints_tests).

-include_lib("eunit/include/eunit.hrl").

loc() ->
    {location, 1, 1}.

state() ->
    catena_infer_state:new().

generate_constraints_for_perform_and_handle_test() ->
    Expr = {handle_expr,
        {perform_expr, 'IO', read, [], loc()},
        [
            {handler_clause, 'IO', [
                {operation_case, read, [], {literal, integer, 0, loc()}, loc()}
            ], loc()}
        ],
        loc()},
    {Constraints, _State} = catena_effect_constraints:generate_constraints(Expr, state()),
    ?assert(lists:member({has_effect, perform_scope, 'IO'}, Constraints)),
    ?assert(lists:member({remove_effect, handle_scope, 'IO'}, Constraints)).

solve_effect_subset_constraint_test() ->
    Result = catena_effect_constraints:solve_effect_constraint(
        {effects_subset, declared_type, {effect_set, ['IO']}, {effect_set, ['IO', 'State']}},
        {effect_set, []},
        state()
    ),
    ?assertMatch({ok, _}, Result).

solve_effect_subset_constraint_failure_test() ->
    Result = catena_effect_constraints:solve_effect_constraint(
        {effects_subset, declared_type, {effect_set, ['IO', 'State']}, {effect_set, ['IO']}},
        {effect_set, []},
        state()
    ),
    ?assertMatch({error, _, _}, Result).

unify_effect_sets_unions_sets_test() ->
    {ok, Unified, _State} = catena_effect_constraints:unify_effect_sets(
        {effect_set, ['IO']},
        {effect_set, ['State']},
        state()
    ),
    ?assertEqual({effect_set, ['IO', 'State']}, Unified).
