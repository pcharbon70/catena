%%%-------------------------------------------------------------------
%%% @doc Catena Effect Type Synthesis Tests (Phase 14.2.1)
%%%
%%% Tests for effect type synthesis for all expressions in Catena.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_synthesis_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    State = catena_infer_state:new(),
    Env = catena_type_env:empty(),
    {State, Env}.

cleanup(_) ->
    ok.

%%====================================================================
%% Empty Effect Tests
%%====================================================================

empty_effects_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun({State, _Env}) ->
            [
                {"Empty effect set", fun test_empty_effects/0},
                {"Empty is pure", fun test_empty_is_pure/0}
            ]
        end
    }.

test_empty_effects() ->
    Empty = catena_effect_synthesis:empty(),
    ?assertEqual({effect_set, []}, Empty).

test_empty_is_pure() ->
    Empty = catena_effect_synthesis:empty(),
    ?assert(catena_effect_synthesis:is_empty(Empty)).

%%====================================================================
%% Literal Synthesis Tests
%%====================================================================

literal_synthesis_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun({State, _Env}) ->
            [
                {"Integer literal", fun test_int_literal/0},
                {"String literal", fun test_string_literal/0},
                {"Boolean literal", fun test_bool_literal/0}
            ]
        end
    }.

test_int_literal() ->
    State = catena_infer_state:new(),
    Literal = {literal, {int, 42}, {1, 1}},
    {Effects, _NewState} = catena_effect_synthesis:synthesize_literal(Literal, State),
    ?assertEqual({effect_set, []}, Effects).

test_string_literal() ->
    State = catena_infer_state:new(),
    Literal = {literal, {string, "hello"}, {1, 1}},
    {Effects, _NewState} = catena_effect_synthesis:synthesize_literal(Literal, State),
    ?assertEqual({effect_set, []}, Effects).

test_bool_literal() ->
    State = catena_infer_state:new(),
    Literal = {literal, {bool, true}, {1, 1}},
    {Effects, _NewState} = catena_effect_synthesis:synthesize_literal(Literal, State),
    ?assertEqual({effect_set, []}, Effects).

%%====================================================================
%% Variable Synthesis Tests
%%====================================================================

variable_synthesis_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun({State, _Env}) ->
            [
                {"Variable reference", fun test_var_synthesis/0}
            ]
        end
    }.

test_var_synthesis() ->
    State = catena_infer_state:new(),
    Var = {var, x, {1, 1}},
    {Effects, _NewState} = catena_effect_synthesis:synthesize_var(x, State),
    ?assertEqual({effect_set, []}, Effects).

%%====================================================================
%% Lambda Synthesis Tests
%%====================================================================

lambda_synthesis_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun({State, _Env}) ->
            [
                {"Lambda expression", fun test_lambda_synthesis/0},
                {"Multi-param lambda", fun test_multi_param_lambda/0}
            ]
        end
    }.

test_lambda_synthesis() ->
    State = catena_infer_state:new(),
    Lambda = {lambda, [{pat_var, x, {1, 1}}], {var, x, {1, 2}}, {1, 1}},
    {Effects, _NewState} = catena_effect_synthesis:synthesize_lambda(Lambda, State),
    ?assertEqual({effect_set, []}, Effects).

test_multi_param_lambda() ->
    State = catena_infer_state:new(),
    Lambda = {lambda,
        [{pat_var, x, {1, 1}}, {pat_var, y, {1, 1}}],
        {var, x, {1, 2}},
        {1, 1}
    },
    {Effects, _NewState} = catena_effect_synthesis:synthesize_lambda(Lambda, State),
    ?assertEqual({effect_set, []}, Effects).

%%====================================================================
%% Perform Synthesis Tests
%%====================================================================

perform_synthesis_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun({State, _Env}) ->
            [
                {"Perform effect", fun test_perform_synthesis/0},
                {"Perform with args", fun test_perform_with_args/0}
            ]
        end
    }.

test_perform_synthesis() ->
    State = catena_infer_state:new(),
    Perform = {perform, {state, get, []}, {1, 1}},
    {Effects, _NewState} = catena_effect_synthesis:synthesize_perform({state, get, []}, State),
    ?assertEqual({effect_set, [state]}, Effects).

test_perform_with_args() ->
    State = catena_infer_state:new(),
    {Effects, _NewState} = catena_effect_synthesis:synthesize_perform({io, write, ["hello"]}, State),
    ?assertEqual({effect_set, [io]}, Effects).

%%====================================================================
%% Application Synthesis Tests
%%====================================================================

application_synthesis_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun({State, _Env}) ->
            [
                {"Pure application", fun test_pure_app/0},
                {"Effectful application", fun test_effectful_app/0}
            ]
        end
    }.

test_pure_app() ->
    State = catena_infer_state:new(),
    App = {app, {var, f, {1, 1}}, [{literal, {int, 1, {1, 1}}}], {1, 1}},
    {Effects, _NewState} = catena_effect_synthesis:synthesize_app({{var, f, {1, 1}}, [{literal, {int, 1, {1, 1}}}]}, State),
    ?assertEqual({effect_set, []}, Effects).

test_effectful_app() ->
    State = catena_infer_state:new(),
    %% Application of a function with state effects
    Func = {var, f, {1, 1}},
    Arg = {literal, {int, 1, {1, 1}}},
    {Effects, _NewState} = catena_effect_synthesis:synthesize_app({Func, [Arg]}, State),
    ?assert(is_list(Effects)).

%%====================================================================
%% Let Synthesis Tests
%%====================================================================

let_synthesis_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun({State, _Env}) ->
            [
                {"Pure let", fun test_pure_let/0},
                {"Effectful let", fun test_effectful_let/0}
            ]
        end
    }.

test_pure_let() ->
    State = catena_infer_state:new(),
    Let = {let_expr,
        [{pat_var, x, {1, 1}},
        {literal, {int, 42, {1, 1}}},
        {var, x, {1, 2}},
        {1, 1}
    },
    {Effects, _NewState} = catena_effect_synthesis:synthesize_let(
        {{literal, {int, 42, {1, 1}}, {var, x, {1, 2}}},
        State
    ),
    ?assertEqual({effect_set, []}, Effects).

test_effectful_let() ->
    State = catena_infer_state:new(),
    %% Let with perform in value
    Perform = {perform, {state, get, []}, {1, 1}},
    Body = {var, x, {1, 2}},
    {Effects, _NewState} = catena_effect_synthesis:synthesize_let({Perform, Body}, State),
    ?assertEqual({effect_set, [state]}, Effects).

%%====================================================================
%% If Synthesis Tests
%%====================================================================

if_synthesis_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun({State, _Env}) ->
            [
                {"Pure if", fun test_pure_if/0},
                {"Effectful if", fun test_effectful_if/0}
            ]
        end
    }.

test_pure_if() ->
    State = catena_infer_state:new(),
    If = {'if',
        {literal, {bool, true, {1, 1}}},
        {literal, {int, 1, {1, 1}}},
        {literal, {int, 2, {1, 1}}},
        {1, 1}
    },
    {Effects, _NewState} = catena_effect_synthesis:synthesize_if(If, State),
    ?assertEqual({effect_set, []}, Effects).

test_effectful_if() ->
    State = catena_infer_state:new(),
    If = {'if',
        {literal, {bool, true, {1, 1}}},
        {perform, {state, get, []}, {1, 2}},
        {literal, {int, 2, {1, 1}}},
        {1, 1}
    },
    {Effects, _NewState} = catena_effect_synthesis:synthesize_if(If, State),
    ?assertEqual({effect_set, [state]}, Effects).

%%====================================================================
%% Effect Set Operations Tests
%%====================================================================

effect_operations_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_StateAndEnv) ->
            [
                {"Union effects", fun test_union_effects/0},
                {"Remove effect", fun test_remove_effect/0},
                {"Contains effect", fun test_contains_effect/0},
                {"To/from list", fun test_to_from_list/0}
            ]
        end
    }.

test_union_effects() ->
    Empty = catena_effect_synthesis:empty(),
    Single = catena_effect_synthesis:from_list([state]),
    Union = catena_effect_synthesis:union(Empty, Single),
    ?assertEqual({effect_set, [state]}, Union),

    Both = catena_effect_synthesis:union(
        catena_effect_synthesis:from_list([state]),
        catena_effect_synthesis:from_list([io])
    ),
    ?assertEqual(2, length(catena_effect_synthesis:to_list(Both))).

test_remove_effect() ->
    Effects = catena_effect_synthesis:from_list([state, io, error]),
    WithoutState = catena_effect_synthesis:remove(Effects, [state]),
    ?assertEqual({effect_set, [error, io]}, WithoutState).

test_contains_effect() ->
    Effects = catena_effect_synthesis:from_list([state, io]),
    ?assert(catena_effect_synthesis:contains(Effects, state)),
    ?assertNot(catena_effect_synthesis:contains(Effects, error)).

test_to_from_list() ->
    List = [state, io, error],
    Effects = catena_effect_synthesis:from_list(List),
    ?assertEqual(List, lists:sort(catena_effect_synthesis:to_list(Effects))).

%%====================================================================
%% Full Synthesis Tests
%%====================================================================

full_synthesis_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun({State, _Env}) ->
            [
                {"Nested expression", fun test_nested_expression/0},
                {"Complex program", fun test_complex_program/0}
            ]
        end
    }.

test_nested_expression() ->
    State = catena_infer_state:new(),
    %% let x = 5 in let y = perform state get in x + y
    Expr = {let_expr,
        [{pat_var, x, {1, 1}},
        {literal, {int, 5, {1, 1}},
        {let_expr,
            [{pat_var, y, {1, 2}}],
            {perform, {state, get, []}, {1, 2}},
            {app, {app, {var, '+', {1, 3}}, [{var, x, {1, 3}}], {1, 3}},
                [{var, y, {1, 3}}],
                {1, 3}
            },
            {1, 2}
        },
        {1, 1}
    },
    {Effects, _NewState} = catena_effect_synthesis:synthesize(Expr, State),
    ?assertEqual({effect_set, [state]}, Effects).

test_complex_program() ->
    State = catena_infer_state:new(),
    %% Multiple effects in sequence
    Expr1 = {perform, {state, get, []}, {1, 1}},
    Expr2 = {perform, {io, write, []}, {1, 2}},
    {Effects1, State1} = catena_effect_synthesis:synthesize(Expr1, State),
    {Effects2, State2} = catena_effect_synthesis:synthesize(Expr2, State1),
    Combined = catena_effect_synthesis:union(Effects1, Effects2),
    ?assertEqual(2, length(catena_effect_synthesis:to_list(Combined))).
