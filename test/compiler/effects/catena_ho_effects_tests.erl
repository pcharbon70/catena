-module(catena_ho_effects_tests).
-include_lib("eunit/include/eunit.hrl").

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

setup() ->
    ok.

cleanup(_) ->
    ok.

test_ho_op_constructors() ->
    Empty = catena_ho_effects:ho_op_type(),
    ?assert(catena_ho_effects:is_ho_op(Empty)),

    Named = catena_ho_effects:ho_op_type(my_ho_op),
    ?assert(catena_ho_effects:is_ho_op(Named)),

    Params = [atom, int],
    Result = bool,
    WithParams = catena_ho_effects:ho_op_type(op1, Params, Result),
    ?assert(catena_ho_effects:is_ho_op(WithParams)),

    Effects = effect_row_example(),
    Complete = catena_ho_effects:ho_op_type(op2, Params, Result, Effects),
    ?assert(catena_ho_effects:is_ho_op(Complete)).

test_effectful_params() ->
    Empty = catena_ho_effects:effectful_param(),
    ?assert(catena_ho_effects:is_effectful_param(Empty)),

    Input = atom,
    Output = int,
    WithIO = catena_ho_effects:effectful_param(Input, Output),
    ?assert(catena_ho_effects:is_effectful_param(WithIO)),
    ?assertEqual(Input, catena_ho_effects:param_input_type(WithIO)),
    ?assertEqual(Output, catena_ho_effects:param_output_type(WithIO)),

    Effects = effect_row_example(),
    Complete = catena_ho_effects:effectful_param(Input, Output, Effects),
    ?assert(catena_ho_effects:is_effectful_param(Complete)),
    ?assertEqual(Effects, catena_ho_effects:param_effects(Complete)),

    ?assertNot(catena_ho_effects:is_effectful_param(atom)),
    ?assertNot(catena_ho_effects:is_effectful_param(123)),
    ?assertNot(catena_ho_effects:is_effectful_param(#{})).

test_ho_validation() ->
    EffParam = catena_ho_effects:effectful_param(atom, int),
    Params = [atom, EffParam, bool],
    HO1 = catena_ho_effects:ho_op_type(ho_op, Params, int),

    ?assert(catena_ho_effects:is_ho_op(HO1)),
    ?assertEqual(1, catena_ho_effects:count_effectful_params(HO1)),

    EffParams = catena_ho_effects:get_effectful_params(HO1),
    ?assertEqual([EffParam], EffParams),

    EffParam2 = catena_ho_effects:effectful_param(bool, atom),
    HO2 = catena_ho_effects:ho_op_type(ho_op2, [EffParam, EffParam2], int),
    ?assertEqual(2, catena_ho_effects:count_effectful_params(HO2)),
    ?assert(catena_ho_effects:is_ho_param_type(EffParam)),
    ?assertNot(catena_ho_effects:is_ho_param_type(atom)).

test_param_inference() ->
    Constraints = #{
        effectful_params => 0,
        max_nesting => 2,
        allow_impredicative => false
    },
    SimpleType = catena_ho_effects:infer_param_type(atom, Constraints),
    ?assertNot(catena_ho_effects:is_effectful_param(SimpleType)),
    ?assertEqual(atom, SimpleType),

    HOConstraints = Constraints#{allow_impredicative => true},
    Inferred = catena_ho_effects:infer_param_type({effectful_fun, atom, int}, HOConstraints),
    ?assert(catena_ho_effects:is_effectful_param(Inferred)),
    InferredEffects = catena_ho_effects:param_effects(Inferred),
    ?assertNotEqual(undefined, maps:get(row_var, InferredEffects)),

    ParamEffects = catena_ho_effects:infer_param_effects(Inferred, Constraints),
    ?assertEqual(effect_row, maps:get(kind, ParamEffects)),

    Terms = [atom, {effectful_fun, int, bool}, bool],
    InferredParams = catena_ho_effects:infer_all_param_types(Terms),
    ?assertEqual(3, length(InferredParams)),

    EffParam1 = catena_ho_effects:effectful_param(atom, int, effect_row_example()),
    EffParam2 = catena_ho_effects:effectful_param(atom, int, state_row_example()),
    ?assertMatch({ok, _}, catena_ho_effects:unify_param_types(EffParam1, EffParam2)),

    EffParam3 = catena_ho_effects:effectful_param(bool, int),
    ?assertMatch({error, _}, catena_ho_effects:unify_param_types(EffParam1, EffParam3)).

test_effect_substitution() ->
    EffParam = catena_ho_effects:effectful_param(
        atom,
        int,
        #{kind => effect_row, elements => ['IO'], row_var => catena_op_signatures:fresh_row_var(rho)}
    ),
    Substitution = #{kind => effect_row, elements => ['State'], row_var => undefined},
    SubstitutedParam = catena_ho_effects:substitute_param_effects(EffParam, Substitution),
    SubstitutedEffects = catena_ho_effects:param_effects(SubstitutedParam),
    ?assertEqual(['IO', 'State'], maps:get(elements, SubstitutedEffects)),
    ?assertEqual(undefined, maps:get(row_var, SubstitutedEffects)),

    EffParam2 = catena_ho_effects:effectful_param(bool, atom),
    HO = catena_ho_effects:ho_op_type(
        op,
        [EffParam, EffParam2],
        int,
        #{kind => effect_row, elements => ['Reader'], row_var => catena_op_signatures:fresh_row_var(epsilon)}
    ),

    SubstitutedHO = catena_ho_effects:substitute_ho_effects(HO, Substitution),
    ?assert(catena_ho_effects:is_ho_op(SubstitutedHO)),

    Instantiated = catena_ho_effects:instantiate_ho_type(HO, Substitution),
    ?assert(catena_ho_effects:is_ho_op(Instantiated)),

    Generalized = catena_ho_effects:generalize_ho_type(
        catena_ho_effects:ho_op_type(simple, [EffParam2], int, #{kind => effect_row, elements => [], row_var => undefined})
    ),
    ?assert(catena_ho_effects:is_ho_op(Generalized)).

test_ho_operations() ->
    EffParam = catena_ho_effects:effectful_param(atom, int),
    HO1 = catena_ho_effects:ho_op_type(
        ho1,
        [EffParam],
        bool,
        effect_row_example()
    ),

    HO2 = catena_ho_effects:ho_op_type(
        ho2,
        [bool],
        int,
        state_row_example()
    ),

    ?assert(catena_ho_effects:is_ho_op(HO1)),
    ?assert(catena_ho_effects:is_ho_op(HO2)),
    ?assertMatch({ok, _}, catena_ho_effects:compose_ho_types(HO1, HO2)),

    HO1Copy = catena_ho_effects:ho_op_type(ho1, [EffParam], bool, effect_row_example()),
    ?assert(catena_ho_effects:ho_type_eq(HO1, HO1Copy)),

    HO3 = catena_ho_effects:ho_op_type(ho3, [EffParam], atom, effect_row_example()),
    ?assertNot(catena_ho_effects:ho_type_eq(HO1, HO3)),

    Merged = catena_ho_effects:merge_effect_rows(effect_row_example(), state_row_example()),
    ?assertEqual(2, length(maps:get(elements, Merged, []))),
    ?assert(lists:member('IO', maps:get(elements, Merged, []))),
    ?assert(lists:member('State', maps:get(elements, Merged, []))).

test_pretty_printing() ->
    EffParam = catena_ho_effects:effectful_param(atom, int, effect_row_example()),
    HO = catena_ho_effects:ho_op_type(my_op, [atom, EffParam], bool, state_row_example()),
    Formatted = catena_ho_effects:format_ho_type(HO),
    ?assert(is_binary(Formatted)),
    ?assertMatch({_, _}, binary:match(Formatted, <<"my_op">>)),

    FormattedParam = catena_ho_effects:format_effectful_param(EffParam),
    ?assert(is_binary(FormattedParam)),
    ?assertMatch({_, _}, binary:match(FormattedParam, <<"IO">>)).

effect_row_example() ->
    #{
        kind => effect_row,
        elements => ['IO'],
        row_var => undefined
    }.

state_row_example() ->
    #{
        kind => effect_row,
        elements => ['State'],
        row_var => undefined
    }.
