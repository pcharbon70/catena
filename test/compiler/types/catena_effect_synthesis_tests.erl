%%%-------------------------------------------------------------------
%%% @doc Focused tests for Phase 14.2 effect synthesis.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_synthesis_tests).

-include_lib("eunit/include/eunit.hrl").

loc() ->
    {location, 1, 1}.

state() ->
    catena_infer_state:new().

empty_effects_test() ->
    ?assertEqual({effect_set, []}, catena_effect_synthesis:empty()).

perform_expr_adds_effect_test() ->
    Expr = {perform_expr, 'IO', print, [{var, msg, loc()}], loc()},
    {Effects, _State} = catena_effect_synthesis:synthesize(Expr, state()),
    ?assertEqual({effect_set, ['IO']}, Effects).

application_collects_argument_effects_test() ->
    Expr = {app, {var, f}, {perform_expr, 'State', get, [], loc()}},
    {Effects, _State} = catena_effect_synthesis:synthesize(Expr, state()),
    ?assertEqual({effect_set, ['State']}, Effects).

handle_expr_removes_handled_effect_test() ->
    Expr = {handle_expr,
        {perform_expr, 'IO', read, [], loc()},
        [
            {handler_clause, 'IO', [
                {operation_case, read, [], {literal, integer, 0, loc()}, loc()}
            ], loc()}
        ],
        loc()},
    {Effects, _State} = catena_effect_synthesis:synthesize(Expr, state()),
    ?assertEqual({effect_set, []}, Effects).

handle_expr_preserves_handler_body_effects_test() ->
    Expr = {handle_expr,
        {perform_expr, 'IO', read, [], loc()},
        [
            {handler_clause, 'IO', [
                {operation_case, read, [],
                    {perform_expr, 'Log', write, [], loc()},
                    loc()}
            ], loc()}
        ],
        loc()},
    {Effects, _State} = catena_effect_synthesis:synthesize(Expr, state()),
    ?assertEqual({effect_set, ['Log']}, Effects).

let_if_and_match_union_effects_test() ->
    Expr = {let_expr,
        [{pat_var, value, loc()}, {perform_expr, 'State', get, [], loc()}],
        {if_expr,
            {literal, bool, true, loc()},
            {match_expr,
                {var, value, loc()},
                [
                    {match_clause, {pat_var, x, loc()}, undefined,
                        {perform_expr, 'IO', print, [], loc()}, loc()}
                ],
                loc()},
            {literal, integer, 0, loc()},
            loc()},
        loc()},
    {Effects, _State} = catena_effect_synthesis:synthesize(Expr, state()),
    ?assertEqual({effect_set, ['IO', 'State']}, Effects).

tuple_and_list_collect_effects_test() ->
    TupleExpr = {tuple_expr, [
        {perform_expr, 'IO', print, [], loc()},
        {literal, integer, 1, loc()}
    ], loc()},
    ListExpr = {list_expr, [
        {perform_expr, 'State', put, [], loc()},
        {literal, integer, 2, loc()}
    ], loc()},
    {TupleEffects, _State1} = catena_effect_synthesis:synthesize(TupleExpr, state()),
    {ListEffects, _State2} = catena_effect_synthesis:synthesize(ListExpr, state()),
    ?assertEqual({effect_set, ['IO']}, TupleEffects),
    ?assertEqual({effect_set, ['State']}, ListEffects).

effect_set_operations_test() ->
    Effects = catena_effect_synthesis:from_list(['IO', 'State']),
    Removed = catena_effect_synthesis:remove(Effects, ['State']),
    ?assert(catena_effect_synthesis:contains(Effects, 'IO')),
    ?assertEqual({effect_set, ['IO']}, Removed),
    ?assertEqual(['IO'], catena_effect_synthesis:to_list(Removed)).
