-module(catena_op_signatures_tests).
-include_lib("eunit/include/eunit.hrl").

catena_op_signatures_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"operation signature constructors", fun test_op_sig_constructors/0},
        {"row variable management", fun test_row_var_management/0},
        {"row variable scope tracking", fun test_scope_tracking/0},
        {"signature validation", fun test_validation/0},
        {"polymorphism restrictions", fun test_polymorphism_restrictions/0},
        {"signature operations", fun test_signature_operations/0},
        {"pretty printing", fun test_pretty_printing/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

test_op_sig_constructors() ->
    Empty = catena_op_signatures:op_sig(),
    ?assert(catena_op_signatures:is_valid_sig(Empty)),

    Params = [atom, int],
    Result = int,
    Sig1 = catena_op_signatures:op_sig(Params, Result),
    ?assert(catena_op_signatures:is_valid_sig(Sig1)),
    ?assertEqual(Params, catena_op_signatures:sig_params(Sig1)),

    Effects = effect_row_example(),
    Sig2 = catena_op_signatures:op_sig(Params, Result, Effects),
    ?assert(catena_op_signatures:is_valid_sig(Sig2)),
    ?assertEqual(Effects, catena_op_signatures:sig_effects(Sig2)),

    Sig3 = catena_op_signatures:op_sig(get, Params, Result, Effects),
    ?assert(catena_op_signatures:is_valid_sig(Sig3)),
    ?assertEqual(get, catena_op_signatures:sig_name(Sig3)).

test_row_var_management() ->
    Var1 = catena_op_signatures:fresh_row_var(),
    ?assert(catena_op_signatures:is_row_var(Var1)),

    Var2 = catena_op_signatures:fresh_row_var(rho),
    ?assert(catena_op_signatures:is_row_var(Var2)),
    ?assertEqual(rho, maps:get(name, Var2)),

    Id = {row_var, 42},
    Var3 = catena_op_signatures:row_var(Id),
    ?assert(catena_op_signatures:is_row_var(Var3)),
    ?assertEqual(Id, catena_op_signatures:row_var_id(Var3)),

    ?assertNot(catena_op_signatures:is_row_var(#{})),
    ?assertNot(catena_op_signatures:is_row_var(atom)),
    ?assertNot(catena_op_signatures:is_row_var([])).

test_scope_tracking() ->
    Root = catena_op_signatures:new_scope(),
    ?assertEqual(undefined, maps:get(parent, Root)),
    ?assertEqual(0, maps:get(depth, Root)),

    Child = catena_op_signatures:enter_scope(Root),
    ?assertEqual(Root, catena_op_signatures:exit_scope(Child)),
    ?assertEqual(1, maps:get(depth, Child)),

    RootVar = catena_op_signatures:fresh_row_var(root),
    RootWithVar = catena_op_signatures:scope_add_var(Root, RootVar),
    ChildWithVar = catena_op_signatures:enter_scope(RootWithVar),

    ChildVar = catena_op_signatures:fresh_row_var(child),
    ChildScoped = catena_op_signatures:scope_add_var(ChildWithVar, ChildVar),

    ?assert(catena_op_signatures:in_scope(RootVar, ChildScoped)),
    ?assert(catena_op_signatures:in_scope(ChildVar, ChildScoped)),
    ?assertNot(catena_op_signatures:in_scope(ChildVar, RootWithVar)),
    ?assert(catena_op_signatures:scope_contains_var(ChildScoped, RootVar)),

    Vars = catena_op_signatures:scope_get_vars(ChildScoped),
    ?assertEqual(2, length(Vars)).

test_validation() ->
    Params = [atom, int],
    Result = int,
    Effects = effect_row_example(),
    ValidSig = catena_op_signatures:op_sig(op, Params, Result, Effects),
    ?assert(catena_op_signatures:is_valid_sig(ValidSig)),

    ?assert(catena_op_signatures:is_valid_sig(catena_op_signatures:op_sig())),
    ?assert(catena_op_signatures:is_valid_op_name(lowercase)),
    ?assert(catena_op_signatures:is_valid_op_name(with_underscore)),
    ?assert(catena_op_signatures:is_valid_op_name('with-quotes')),

    ?assertNot(catena_op_signatures:is_valid_op_name('Uppercase')),
    ?assertNot(catena_op_signatures:is_valid_op_name(123)),
    ?assertNot(catena_op_signatures:is_valid_op_name([])),

    ?assert(catena_op_signatures:is_valid_params([])),
    ?assert(catena_op_signatures:is_valid_params([atom, int, bool])),
    ?assertNot(catena_op_signatures:is_valid_params(not_a_list)),
    ?assertNot(catena_op_signatures:is_valid_params([atom, 123])),

    ?assert(catena_op_signatures:is_valid_result(atom)),
    ?assert(catena_op_signatures:is_valid_result({pair, atom, int})),
    ?assertNot(catena_op_signatures:is_valid_result(123)),
    ?assertNot(catena_op_signatures:is_valid_result("string")),

    ?assert(catena_op_signatures:is_valid_effect_row(Effects)),
    ?assertNot(catena_op_signatures:is_valid_effect_row(#{})).

test_polymorphism_restrictions() ->
    RowVar = catena_op_signatures:fresh_row_var(rho),
    Sig = catena_op_signatures:op_sig(
        my_op,
        [atom],
        int,
        #{
            kind => effect_row,
            elements => ['IO', 'State'],
            row_var => RowVar
        }
    ),

    Scope = catena_op_signatures:scope_add_var(
        catena_op_signatures:new_scope(),
        RowVar
    ),
    ?assertMatch({ok, _}, catena_op_signatures:check_row_var_escape(Sig, Scope)),
    ?assertMatch({error, _}, catena_op_signatures:check_row_var_escape(Sig, catena_op_signatures:new_scope())),

    Constraints = catena_op_signatures:signature_restrictions(Sig),
    ?assertEqual(['IO', 'State'], maps:get(required, Constraints)),
    ?assertEqual([], maps:get(forbidden, Constraints)),
    ?assertEqual(1, length(maps:get(row_vars, Constraints))),

    ?assert(catena_op_signatures:satisfies_constraints(['IO', 'State'], Sig)),
    ?assert(catena_op_signatures:satisfies_constraints(['IO', 'State', 'Other'], Sig)),
    ?assertNot(catena_op_signatures:satisfies_constraints(['IO'], Sig)).

test_signature_operations() ->
    RowVar = catena_op_signatures:fresh_row_var(),
    Sig = catena_op_signatures:op_sig(
        op,
        [atom],
        int,
        #{
            kind => effect_row,
            elements => ['IO'],
            row_var => RowVar
        }
    ),

    Replacement = #{
        kind => effect_row,
        elements => ['State'],
        row_var => undefined
    },
    Substituted = catena_op_signatures:substitute_row_var(Sig, RowVar, Replacement),
    SubstitutedEffects = catena_op_signatures:sig_effects(Substituted),
    ?assertEqual(['IO', 'State'], maps:get(elements, SubstitutedEffects)),
    ?assertEqual(undefined, maps:get(row_var, SubstitutedEffects)),

    OtherVar = catena_op_signatures:fresh_row_var(),
    NotSubstituted = catena_op_signatures:substitute_row_var(Sig, OtherVar, Replacement),
    ?assertEqual(RowVar, maps:get(row_var, catena_op_signatures:sig_effects(NotSubstituted))),

    SimpleSig = catena_op_signatures:op_sig(
        op2,
        [atom],
        int,
        #{kind => effect_row, elements => [], row_var => undefined}
    ),
    Generalized = catena_op_signatures:generalize_sig(SimpleSig),
    GeneralizedEffects = catena_op_signatures:sig_effects(Generalized),
    ?assertNotEqual(undefined, maps:get(row_var, GeneralizedEffects)),

    Instantiated = catena_op_signatures:instantiate_sig(Generalized, Replacement),
    InstantiatedEffects = catena_op_signatures:sig_effects(Instantiated),
    ?assertEqual(['State'], maps:get(elements, InstantiatedEffects)),
    ?assertEqual(undefined, maps:get(row_var, InstantiatedEffects)),

    Sig1 = catena_op_signatures:op_sig(op1, [atom], int, effect_row_example()),
    Sig2 = catena_op_signatures:op_sig(op1, [atom], int, effect_row_example()),
    Sig3 = catena_op_signatures:op_sig(op1, [bool], int, effect_row_example()),
    ?assert(catena_op_signatures:sig_eq(Sig1, Sig2)),
    ?assertNot(catena_op_signatures:sig_eq(Sig1, Sig3)).

test_pretty_printing() ->
    RowVar = catena_op_signatures:fresh_row_var(epsilon),
    Effects = #{
        kind => effect_row,
        elements => ['IO'],
        row_var => RowVar
    },
    Sig = catena_op_signatures:op_sig(my_op, [atom, int], bool, Effects),
    Formatted = catena_op_signatures:format_sig(Sig),
    ?assert(is_binary(Formatted)),
    ?assertMatch({_, _}, binary:match(Formatted, <<"my_op">>)),
    ?assertMatch({_, _}, binary:match(Formatted, <<"epsilon">>)),

    FormattedVar = catena_op_signatures:format_row_var(RowVar),
    ?assertEqual(<<"epsilon">>, FormattedVar).

effect_row_example() ->
    #{
        kind => effect_row,
        elements => ['IO'],
        row_var => undefined
    }.
