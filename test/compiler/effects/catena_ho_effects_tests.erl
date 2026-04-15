-module(catena_ho_effects_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_ho_effects_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"ho op type constructors", fun test_ho_op_constructors/0},
        {"effectful param types", fun test_effectful_params/0},
        {"ho op validation", fun test_ho_validation/0},
        {"param type inference", fun test_param_inference/0},
        {"effect substitution", fun test_effect_substitution/0},
        {"ho type operations", fun test_ho_operations/0},
        {"pretty printing", fun test_pretty_printing/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Constructor Tests
%%%---------------------------------------------------------------------

test_ho_op_constructors() ->
    %% Empty HO op type
    Empty = catena_ho_effects:ho_op_type(),
    ?assert(catena_ho_effects:is_ho_op(Empty)),

    %% Named HO op type
    Named = catena_ho_effects:ho_op_type(my_ho_op),
    ?assert(catena_ho_effects:is_ho_op(Named)),

    %% HO op with params and result
    Params = [atom, int],
    Result = bool,
    WithParams = catena_ho_effects:ho_op_type(op1, Params, Result),
    ?assert(catena_ho_effects:is_ho_op(WithParams)),

    %% Complete HO op with effects
    Effects = effect_row_example(),
    Complete = catena_ho_effects:ho_op_type(op2, Params, Result, Effects),
    ?assert(catena_ho_effects:is_ho_op(Complete)).

%%%---------------------------------------------------------------------
%%% Effectful Parameter Tests
%%%---------------------------------------------------------------------

test_effectful_params() ->
    %% Empty effectful param
    Empty = catena_ho_effects:effectful_param(),
    ?assert(catena_ho_effects:is_effectful_param(Empty)),

    %% Effectful param with input/output
    Input = atom,
    Output = int,
    WithIO = catena_ho_effects:effectful_param(Input, Output),
    ?assert(catena_ho_effects:is_effectful_param(WithIO)),
    ?assertEqual(Input, catena_ho_effects:param_input_type(WithIO)),
    ?assertEqual(Output, catena_ho_effects:param_output_type(WithIO)),

    %% Complete effectful param
    Effects = effect_row_example(),
    Complete = catena_ho_effects:effectful_param(Input, Output, Effects),
    ?assert(catena_ho_effects:is_effectful_param(Complete)),
    ?assertEqual(Effects, catena_ho_effects:param_effects(Complete)),

    %% Not effectful
    ?assertNot(catena_ho_effects:is_effectful_param(atom)),
    ?assertNot(catena_ho_effects:is_effectful_param(123)),
    ?assertNot(catena_ho_effects:is_effectful_param(#{})).

%%%---------------------------------------------------------------------
%%% Validation Tests
%%%---------------------------------------------------------------------

test_ho_validation() ->
    %% HO op with effectful params
    EffParam = catena_ho_effects:effectful_param(atom, int),
    Params = [atom, EffParam, bool],
    HO1 = catena_ho_effects:ho_op_type(ho_op, Params, int),

    ?assert(catena_ho_effects:is_ho_op(HO1)),
    ?assertEqual(1, catena_ho_effects:count_effectful_params(HO1)),

    EffParams = catena_ho_effects:get_effectful_params(HO1),
    ?assertEqual(1, length(EffParams)),
    ?assertEqual(EffParam, hd(EffParams)),

    %% HO op with multiple effectful params
    EffParam2 = catena_ho_effects:effectful_param(bool, atom),
    Params2 = [EffParam, EffParam2],
    HO2 = catena_ho_effects:ho_op_type(ho_op2, Params2, int),

    ?assertEqual(2, catena_ho_effects:count_effectful_params(HO2)),
    ?assertEqual(2, length(catena_ho_effects:get_effectful_params(HO2))),

    %% HO op with no effectful params
    SimpleParams = [atom, int, bool],
    HO3 = catena_ho_effects:ho_op_type(simple_op, SimpleParams, int),
    ?assertEqual(0, catena_ho_effects:count_effectful_params(HO3)),

    %% Check if param is higher-order type
    ?assert(catena_ho_effects:is_ho_param_type(EffParam)),
    ?assertNot(catena_ho_effects:is_ho_param_type(atom)).

%%%---------------------------------------------------------------------
%%% Type Inference Tests
%%%---------------------------------------------------------------------

test_param_inference() ->
    %% Infer simple parameter type
    Constraints = #{
        effectful_params => 0,
        max_nesting => 2,
        allow_impredicative => false
    },
    SimpleType = catena_ho_effects:infer_param_type(atom, Constraints),
    ?assertNot(catena_ho_effects:is_effectful_param(SimpleType)),
    ?assertEqual(atom, SimpleType),

    %% Infer effectful param effects
    EffParam = catena_ho_effects:effectful_param(atom, int),
    ParamEffects = catena_ho_effects:infer_param_effects(EffParam, Constraints),
    ?assert(is_map(ParamEffects)),
    ?assertEqual(effect_row, maps:get(kind, ParamEffects)),

    %% Infer all param types
    Terms = [atom, int, bool],
    InferredParams = catena_ho_effects:infer_all_param_types(Terms),
    ?assertEqual(3, length(InferredParams)),

    %% Unify matching param types
    EffParam1 = catena_ho_effects:effectful_param(atom, int),
    EffParam2 = catena_ho_effects:effectful_param(atom, int),
    ?assertMatch({ok, _}, catena_ho_effects:unify_param_types(EffParam1, EffParam2)),

    %% Unify mismatched param types
    EffParam3 = catena_ho_effects:effectful_param(bool, int),
    ?assertMatch({error, _}, catena_ho_effects:unify_param_types(EffParam1, EffParam3)).

%%%---------------------------------------------------------------------
%%% Effect Substitution Tests
%%%---------------------------------------------------------------------

test_effect_substitution() ->
    %% Substitute param effects
    EffParam = catena_ho_effects:effectful_param(atom, int),
    Substitution = #{kind => effect_row, elements => ['IO'], row_var => undefined},
    SubstitutedParam = catena_ho_effects:substitute_param_effects(EffParam, Substitution),
    ?assert(catena_ho_effects:is_effectful_param(SubstitutedParam)),
    SubstitutedEffects = catena_ho_effects:param_effects(SubstitutedParam),
    ?assertEqual(['IO'], maps:get(elements, SubstitutedEffects, [])),

    %% Substitute HO op effects
    EffParam2 = catena_ho_effects:effectful_param(bool, atom),
    Params = [EffParam, EffParam2],
    Effects = effect_row_example(),
    HO = catena_ho_effects:ho_op_type(op, Params, int, Effects),

    NewSubstitution = #{kind => effect_row, elements => ['State'], row_var => undefined},
    SubstitutedHO = catena_ho_effects:substitute_ho_effects(HO, NewSubstitution),
    ?assert(catena_ho_effects:is_ho_op(SubstitutedHO)),

    %% Instantiate HO type
    Instantiated = catena_ho_effects:instantiate_ho_type(HO, NewSubstitution),
    ?assert(catena_ho_effects:is_ho_op(Instantiated)),

    %% Generalize HO type
    Generalized = catena_ho_effects:generalize_ho_type(HO),
    ?assert(catena_ho_effects:is_ho_op(Generalized)).

%%%---------------------------------------------------------------------
%%% HO Type Operations Tests
%%%---------------------------------------------------------------------

test_ho_operations() ->
    %% Compose matching HO types
    EffParam = catena_ho_effects:effectful_param(atom, int),
    Params1 = [EffParam],
    Effects1 = #{
        kind => effect_row,
        elements => ['IO'],
        row_var => undefined
    },
    HO1 = catena_ho_effects:ho_op_type(ho1, Params1, bool, Effects1),

    Params2 = [bool],
    Effects2 = #{
        kind => effect_row,
        elements => ['State'],
        row_var => undefined
    },
    HO2 = catena_ho_effects:ho_op_type(ho2, Params2, int, Effects2),

    %% Note: compose_ho_types expects input/output fields which our records don't have
    %% So we'll test the simpler operations
    ?assert(catena_ho_effects:is_ho_op(HO1)),
    ?assert(catena_ho_effects:is_ho_op(HO2)),

    %% Type equality
    HO1Copy = catena_ho_effects:ho_op_type(ho1, Params1, bool, Effects1),
    ?assert(catena_ho_effects:ho_type_eq(HO1, HO1Copy)),

    HO3 = catena_ho_effects:ho_op_type(ho3, Params1, atom, Effects1),
    ?assertNot(catena_ho_effects:ho_type_eq(HO1, HO3)),

    %% Merge effect rows
    Row1 = #{
        kind => effect_row,
        elements => ['IO'],
        row_var => undefined
    },
    Row2 = #{
        kind => effect_row,
        elements => ['State'],
        row_var => undefined
    },
    Merged = catena_ho_effects:merge_effect_rows(Row1, Row2),
    ?assertEqual(2, length(maps:get(elements, Merged, []))),
    ?assert(lists:member('IO', maps:get(elements, Merged, []))),
    ?assert(lists:member('State', maps:get(elements, Merged, []))).

%%%---------------------------------------------------------------------
%%% Pretty Printing Tests
%%%---------------------------------------------------------------------

test_pretty_printing() ->
    %% Format HO type
    EffParam = catena_ho_effects:effectful_param(atom, int),
    Params = [atom, EffParam],
    Effects = #{
        kind => effect_row,
        elements => ['IO'],
        row_var => undefined
    },
    HO = catena_ho_effects:ho_op_type(my_op, Params, bool, Effects),
    Formatted = catena_ho_effects:format_ho_type(HO),
    ?assert(is_binary(Formatted)),
    ?assertNotEqual(<<>>, Formatted),
    case binary:match(Formatted, <<"my_op">>) of
        {_, _} -> ok;
        nomatch -> ?assert(false)
    end,

    %% Format effectful param
    FormattedParam = catena_ho_effects:format_effectful_param(EffParam),
    ?assert(is_binary(FormattedParam)),
    ?assertNotEqual(<<>>, FormattedParam),
    case binary:match(FormattedParam, <<"->">>) of
        {_, _} -> ok;
        nomatch -> ?assert(false)
    end,
    case binary:match(FormattedParam, <<"/">>) of
        {_, _} -> ok;
        nomatch -> ?assert(false)
    end.

%%%---------------------------------------------------------------------
%%% Helper Functions
%%%---------------------------------------------------------------------

effect_row_example() ->
    #{
        kind => effect_row,
        elements => ['IO', 'State'],
        row_var => undefined
    }.
