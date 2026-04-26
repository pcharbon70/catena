-module(catena_row_inference_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_row_inference_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"row variable generalization", fun test_generalize_row_vars/0},
        {"row variable generalization with explicit vars", fun test_generalize_explicit/0},
        {"row variable instantiation", fun test_instantiate_row_vars/0},
        {"row variable instantiation with vars", fun test_instantiate_with_vars/0},
        {"infer row poly function", fun test_infer_row_poly_function/0},
        {"infer row poly operation", fun test_infer_row_poly_operation/0},
        {"infer row poly handler", fun test_infer_row_poly_handler/0},
        {"infer row poly handler with body effects", fun test_infer_row_poly_handler_with_body_effects/0},
        {"propagate row constraints", fun test_propagate_constraints/0},
        {"row poly scheme creation", fun test_row_poly_scheme/0},
        {"row poly scheme instantiation", fun test_row_poly_scheme_instantiate/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Generalization Tests
%%%---------------------------------------------------------------------

test_generalize_row_vars() ->
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
    ?assertEqual([{row_var, 1}], maps:get(row_vars, Scheme)),
    ?assertEqual([{row_var, 1}], maps:get(row_vars, maps:get(type, Scheme))).

test_generalize_explicit() ->
    Type = #{
        kind => row_poly,
        effects => catena_row_types:effect_row([a]),
        row_vars => []
    },
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    VarId1 = {row_var, 1},
    VarId2 = {row_var, 2},

    {Scheme, _} = catena_row_inference:generalize_row_vars(Type, [VarId1, VarId2], State),
    ?assertEqual([VarId1, VarId2], maps:get(row_vars, Scheme)).

%%%---------------------------------------------------------------------
%%% Instantiation Tests
%%%---------------------------------------------------------------------

test_instantiate_row_vars() ->
    OldRowVar = catena_row_types:row_var({row_var, 1}),
    Type = #{
        kind => row_poly,
        effects => catena_row_types:effect_row([a], OldRowVar),
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
    InstanceEffects = maps:get(effects, Instance),
    ?assertMatch(#{row_var := #{id := {row_var, 0}}}, InstanceEffects),
    ?assertEqual([{row_var, 0}], maps:get(row_vars, Instance)).

test_instantiate_with_vars() ->
    OldRowVar = catena_row_types:row_var({row_var, 1}),
    Type = #{
        kind => row_poly,
        effects => catena_row_types:effect_row([a], OldRowVar),
        row_vars => [{row_var, 1}]
    },
    Scheme = #{
        kind => row_scheme,
        type => Type,
        row_vars => [{row_var, 1}]
    },
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    Var = catena_row_types:row_var({row_var, 99}),
    {Instance, _} = catena_row_inference:instantiate_row_vars(Scheme, [Var], State),
    ?assertEqual(row_poly, maps:get(kind, Instance)),
    ?assertMatch(#{row_var := #{id := {row_var, 99}}}, maps:get(effects, Instance)),
    ?assertEqual([{row_var, 99}], maps:get(row_vars, Instance)).

%%%---------------------------------------------------------------------
%%% Function Inference Tests
%%%---------------------------------------------------------------------

test_infer_row_poly_function() ->
    FunInfo = #{effects => [a, b, c]},
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    {Type, _} = catena_row_inference:infer_row_poly_function(FunInfo, State),
    ?assertEqual(row_poly, maps:get(kind, Type)),

    Effects = maps:get(effects, Type),
    EffectList = catena_row_types:row_to_list(Effects),
    ?assertEqual([a, b, c], lists:sort(EffectList)).

%%%---------------------------------------------------------------------
%%% Operation Inference Tests
%%%---------------------------------------------------------------------

test_infer_row_poly_operation() ->
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    {Type, NewState} = catena_row_inference:infer_row_poly_operation('State', State),
    ?assertEqual(row_poly, maps:get(kind, Type)),
    ?assertEqual(1, maps:get(row_var_counter, NewState)),
    ?assertEqual([{row_var, 0}], maps:get(row_vars, Type)),
    ?assertMatch(#{row_var := #{id := {row_var, 0}}}, maps:get(effects, Type)).

%%%---------------------------------------------------------------------
%%% Handler Inference Tests
%%%---------------------------------------------------------------------

test_infer_row_poly_handler() ->
    HandlerInfo = #{
        handled => [a, b],
        remaining => [c]
    },
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    {Type, _} = catena_row_inference:infer_row_poly_handler(HandlerInfo, State),
    ?assertEqual(row_poly, maps:get(kind, Type)),
    ?assertEqual([c], catena_row_types:row_to_list(maps:get(effects, Type))).

test_infer_row_poly_handler_with_body_effects() ->
    HandlerInfo = #{
        handled => [a, b],
        remaining => [d],
        body_effects => [a, b, c]
    },
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    {Type, _} = catena_row_inference:infer_row_poly_handler(HandlerInfo, State),
    ?assertEqual([c, d], catena_row_types:row_to_list(maps:get(effects, Type))).

%%%---------------------------------------------------------------------
%%% Constraint Propagation Tests
%%%---------------------------------------------------------------------

test_propagate_constraints() ->
    Effects = catena_row_types:effect_row([a, b]),
    Type = #{
        kind => row_poly,
        effects => Effects,
        row_vars => []
    },
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    ?assertMatch({ok, _}, catena_row_inference:propagate_row_constraints(Type, State)).

%%%---------------------------------------------------------------------
%%% Scheme Tests
%%%---------------------------------------------------------------------

test_row_poly_scheme() ->
    RowVar = catena_row_types:row_var({row_var, 2}),
    Type = #{
        kind => row_poly,
        effects => catena_row_types:effect_row([a], RowVar),
        row_vars => []
    },
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    Scheme = catena_row_inference:row_poly_scheme(Type, State),
    ?assertEqual(row_scheme, maps:get(kind, Scheme)),
    ?assertEqual([{row_var, 2}], maps:get(row_vars, Scheme)).

test_row_poly_scheme_instantiate() ->
    RowVar = catena_row_types:row_var({row_var, 2}),
    Type = #{
        kind => row_poly,
        effects => catena_row_types:effect_row([a], RowVar),
        row_vars => [{row_var, 2}]
    },
    Scheme = #{
        kind => row_scheme,
        type => Type,
        row_vars => [{row_var, 2}]
    },
    State = #{row_var_counter => 0, constraints => [], substitutions => #{}},

    {Instance, NewState} = catena_row_inference:row_poly_scheme_instantiate(Scheme, State),
    ?assertEqual(row_poly, maps:get(kind, Instance)),
    ?assertEqual(1, maps:get(row_var_counter, NewState)),
    ?assertEqual([{row_var, 0}], maps:get(row_vars, Instance)).
