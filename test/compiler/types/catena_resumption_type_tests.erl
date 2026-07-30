%%%-------------------------------------------------------------------
%%% Phase 3.1 tests for first-class Resumption kinds and types.
%%%-------------------------------------------------------------------
-module(catena_resumption_type_tests).

-include_lib("eunit/include/eunit.hrl").

resumption_constructor_kind_test() ->
    Env = catena_kind:build_kind_env([]),
    Expected = catena_kind:arrow(
        catena_kind:resumption_kind(),
        catena_kind:arrow(
            catena_kind:star(),
            catena_kind:arrow(
                catena_kind:star(),
                catena_kind:arrow(
                    catena_kind:effect_row(),
                    catena_kind:star()
                )
            )
        )
    ),
    ?assertEqual(
        {ok, catena_kind:resumption_kind()},
        catena_kind:get_type_kind('OneShot', Env)
    ),
    ?assertEqual(
        {ok, catena_kind:resumption_kind()},
        catena_kind:get_type_kind('MultiShot', Env)
    ),
    ?assertEqual(
        {ok, Expected},
        catena_kind:get_type_kind('Resumption', Env)
    ),
    ?assertEqual(
        "ResumptionKind -> Type -> Type -> EffectRow -> Type",
        catena_kind:format_kind(Expected)
    ).

resumption_kind_validation_test() ->
    Env = catena_kind:build_kind_env([]),
    Type = catena_types:tresumption(
        catena_types:one_shot(),
        catena_types:tcon(int),
        catena_types:tcon(string),
        catena_types:teffectrow(['Console'])
    ),
    ?assertEqual({ok, catena_kind:star()}, catena_kind:infer_type_kind(Type, Env)),
    Invalid = {
        tresumption,
        catena_types:tcon(int),
        catena_types:tcon(int),
        catena_types:tcon(string),
        catena_types:teffectrow([])
    },
    ?assertMatch(
        {error, {kind_mismatch, resumption_kind, star, _}},
        catena_kind:infer_type_kind(Invalid, Env)
    ).

resumption_representation_and_accessors_test() ->
    Effects = catena_types:teffectrow(['State', 'Console', 'State'], 4),
    Type = catena_types:tresumption(
        catena_types:one_shot(),
        catena_types:tvar(1),
        catena_types:tvar(2),
        Effects
    ),
    ?assert(catena_types:is_resumption_type(Type)),
    ?assertEqual({ok, catena_types:one_shot()}, catena_types:resumption_kind(Type)),
    ?assertEqual({ok, catena_types:tvar(1)},
        catena_types:resumption_operation_result(Type)),
    ?assertEqual({ok, catena_types:tvar(2)},
        catena_types:resumption_delimiter_result(Type)),
    ?assertEqual(
        {ok, {teffectrow, ['Console', 'State'], 4}},
        catena_types:resumption_effect_row(Type)
    ),
    ?assertEqual(ok, catena_types:validate_type(Type)),
    ?assertEqual(
        {error, {invalid_resumption_kind, catena_types:tcon(int)}},
        catena_types:validate_type(setelement(2, Type, catena_types:tcon(int)))
    ).

resumption_structural_equality_test() ->
    Left = catena_types:tresumption(
        catena_types:one_shot(),
        catena_types:tvar(1),
        catena_types:tcon(string),
        catena_types:teffectrow(['State', 'Console'], 3)
    ),
    Right = {
        tresumption,
        catena_types:one_shot(),
        catena_types:tvar(1),
        catena_types:tcon(string),
        {teffectrow, ['Console', 'State'], 3}
    },
    ?assert(catena_types:type_equal(Left, Right)),
    ?assertNot(catena_types:type_equal(
        Left,
        setelement(2, Right, catena_types:multi_shot())
    )).

resumption_substitution_traverses_all_variable_roles_test() ->
    Type = catena_types:tresumption(
        catena_types:resumption_kind_var(1),
        catena_types:tvar(2),
        catena_types:tvar(3),
        catena_types:teffectrow(['Console'], 4)
    ),
    Substitution = maps:from_list([
        {1, catena_types:one_shot()},
        {2, catena_types:tcon(int)},
        {3, catena_types:tcon(string)},
        {4, catena_types:teffectrow(['State'])}
    ]),
    ?assertEqual(
        catena_types:tresumption(
            catena_types:one_shot(),
            catena_types:tcon(int),
            catena_types:tcon(string),
            catena_types:teffectrow(['Console', 'State'])
        ),
        catena_type_subst:apply(Substitution, Type)
    ).

resumption_unification_and_row_occurs_test() ->
    Left = catena_types:tresumption(
        catena_types:resumption_kind_var(1),
        catena_types:tvar(2),
        catena_types:tcon(string),
        catena_types:teffectrow(['Console'], 4)
    ),
    Right = catena_types:tresumption(
        catena_types:one_shot(),
        catena_types:tcon(int),
        catena_types:tcon(string),
        catena_types:teffectrow(['Console', 'State'])
    ),
    {ok, Substitution} = catena_infer_unify:unify_types(Left, Right),
    ?assert(catena_types:type_equal(
        Right,
        catena_type_subst:apply(Substitution, Left)
    )),
    ?assert(catena_type_subst:occurs_check(
        4,
        catena_types:teffectrow(['Console'], 4)
    )).

resumption_scheme_generalization_and_instantiation_test() ->
    Type = catena_types:tresumption(
        catena_types:one_shot(),
        catena_types:tvar(100),
        catena_types:tvar(200),
        catena_types:teffectrow(['Console'], 300)
    ),
    Scheme = catena_type_scheme:generalize(Type, sets:new()),
    ?assertMatch({poly, [100, 200, 300], _}, Scheme),
    {Instance, [], _State} = catena_type_scheme:instantiate(
        Scheme,
        catena_infer_state:new()
    ),
    ?assertMatch(
        {
            tresumption,
            {tcon, 'OneShot'},
            {tvar, 1},
            {tvar, 2},
            {teffectrow, ['Console'], 3}
        },
        Instance
    ).

resumption_pretty_print_preserves_row_identity_test() ->
    Type = catena_types:tresumption(
        catena_types:one_shot(),
        catena_types:tvar(1),
        catena_types:tcon(string),
        catena_types:teffectrow(['Console', 'State'], 7)
    ),
    ?assertEqual(
        "Resumption OneShot α1 string {Console, State | ε7}",
        catena_type_pp:pp_type(Type)
    ),
    ?assertEqual(
        "∀α1 α7. Resumption OneShot α1 string {Console, State | ε7}",
        catena_type_pp:pp_scheme(catena_type_scheme:poly([1, 7], Type))
    ).
