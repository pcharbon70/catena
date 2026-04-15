-module(catena_op_signatures_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

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

test_op_sig_constructors() ->
    %% Empty signature
    Empty = catena_op_signatures:op_sig(),
    ?assert(catena_op_signatures:is_valid_sig(Empty)),

    %% Signature with params and result
    Params = [atom, int],
    Result = int,
    Sig1 = catena_op_signatures:op_sig(Params, Result),
    ?assert(catena_op_signatures:is_valid_sig(Sig1)),
    ?assertEqual(Params, catena_op_signatures:sig_params(Sig1)),

    %% Signature with effects
    Effects = effect_row_example(),
    Sig2 = catena_op_signatures:op_sig(Params, Result, Effects),
    ?assert(catena_op_signatures:is_valid_sig(Sig2)),
    ?assertEqual(Effects, catena_op_signatures:sig_effects(Sig2)),

    %% Complete signature with name
    Sig3 = catena_op_signatures:op_sig(get, Params, Result, Effects),
    ?assert(catena_op_signatures:is_valid_sig(Sig3)),
    ?assertEqual(get, catena_op_signatures:sig_name(Sig3)).

%%%---------------------------------------------------------------------
%%% Row Variable Management Tests
%%%---------------------------------------------------------------------

test_row_var_management() ->
    %% Fresh row variable
    Var1 = catena_op_signatures:fresh_row_var(),
    ?assert(catena_op_signatures:is_row_var(Var1)),

    %% Fresh named row variable
    Var2 = catena_op_signatures:fresh_row_var(rho),
    ?assert(catena_op_signatures:is_row_var(Var2)),
    ?assertEqual(rho, maps:get(name, Var2)),

    %% Row variable with ID
    Id = {row_var, 42},
    Var3 = catena_op_signatures:row_var(Id),
    ?assert(catena_op_signatures:is_row_var(Var3)),
    ?assertEqual(Id, catena_op_signatures:row_var_id(Var3)),

    %% Not a row variable
    ?assertNot(catena_op_signatures:is_row_var(#{})),
    ?assertNot(catena_op_signatures:is_row_var(atom)),
    ?assertNot(catena_op_signatures:is_row_var([])).

%%%---------------------------------------------------------------------
%%% Scope Tracking Tests
%%%---------------------------------------------------------------------

test_scope_tracking() ->
    %% New scope
    Root = catena_op_signatures:new_scope(),
    ?assertEqual(0, maps:get(id, Root)),
    ?assertEqual(undefined, maps:get(parent, Root)),
    ?assertEqual(0, maps:get(depth, Root)),

    %% Enter nested scope
    Child = catena_op_signatures:enter_scope(Root),
    ?assertNotEqual(maps:get(id, Root), maps:get(id, Child)),
    ?assertEqual(maps:get(id, Root), maps:get(parent, Child)),
    ?assertEqual(1, maps:get(depth, Child)),

    %% Add variable to scope
    Var = catena_op_signatures:fresh_row_var(),
    ScopeWithVar = catena_op_signatures:scope_add_var(Child, Var),
    ?assert(catena_op_signatures:scope_contains_var(ScopeWithVar, Var)),

    %% Check if variable is in scope
    ?assert(catena_op_signatures:in_scope(Var, ScopeWithVar)),
    ?assertNot(catena_op_signatures:in_scope(Var, Root)),

    %% Get variables in scope
    Vars = catena_op_signatures:scope_get_vars(ScopeWithVar),
    ?assertEqual(1, length(Vars)),
    ?assertEqual(Var, hd(Vars)),

    %% Exit scope
    Exited = catena_op_signatures:exit_scope(Child),
    ?assertMatch({exited, _}, Exited).

%%%---------------------------------------------------------------------
%%% Validation Tests
%%%---------------------------------------------------------------------

test_validation() ->
    %% Valid signature
    Params = [atom, int],
    Result = int,
    Effects = effect_row_example(),
    ValidSig = catena_op_signatures:op_sig(op, Params, Result, Effects),
    ?assert(catena_op_signatures:is_valid_sig(ValidSig)),

    %% Invalid signature with empty params
    EmptySig = catena_op_signatures:op_sig(),
    ?assert(catena_op_signatures:is_valid_sig(EmptySig)),

    %% Valid operation names
    ?assert(catena_op_signatures:is_valid_op_name(lowercase)),
    ?assert(catena_op_signatures:is_valid_op_name(with_underscore)),
    ?assert(catena_op_signatures:is_valid_op_name('with-quotes')),

    %% Invalid operation names
    ?assertNot(catena_op_signatures:is_valid_op_name('Uppercase')),
    ?assertNot(catena_op_signatures:is_valid_op_name(123)),
    ?assertNot(catena_op_signatures:is_valid_op_name([])),

    %% Valid params
    ?assert(catena_op_signatures:is_valid_params([])),
    ?assert(catena_op_signatures:is_valid_params([atom, int, bool])),

    %% Invalid params
    ?assertNot(catena_op_signatures:is_valid_params(not_a_list)),
    ?assertNot(catena_op_signatures:is_valid_params([atom, 123])),

    %% Valid result
    ?assert(catena_op_signatures:is_valid_result(atom)),
    ?assert(catena_op_signatures:is_valid_result(#{kind => some_type})),

    %% Invalid result
    ?assertNot(catena_op_signatures:is_valid_result(123)),
    ?assertNot(catena_op_signatures:is_valid_result("string")),

    %% Valid effect rows
    ValidEffects1 = effect_row_example(),
    ?assert(catena_op_signatures:is_valid_effect_row(ValidEffects1)),

    ValidEffects2 = #{
        kind => effect_row,
        elements => ['IO', 'State'],
        row_var => undefined
    },
    ?assert(catena_op_signatures:is_valid_effect_row(ValidEffects2)),

    %% Invalid effect rows
    ?assertNot(catena_op_signatures:is_valid_effect_row(#{})),
    ?assertNot(catena_op_signatures:is_valid_effect_row(#{kind => effect_row, elements => not_a_list})).

%%%---------------------------------------------------------------------
%%% Polymorphism Restrictions Tests
%%%---------------------------------------------------------------------

test_polymorphism_restrictions() ->
    %% Signature with required effects
    Effects = #{
        kind => effect_row,
        elements => ['IO', 'State'],
        row_var => undefined
    },
    Sig = catena_op_signatures:op_sig(my_op, [atom], int, Effects),

    %% Get required effects
    Required = catena_op_signatures:required_effects(Sig),
    ?assertEqual(2, length(Required)),
    ?assert(lists:member('IO', Required)),
    ?assert(lists:member('State', Required)),

    %% Get signature restrictions
    Constraints = catena_op_signatures:signature_restrictions(Sig),
    ?assertEqual(['IO', 'State'], maps:get(required, Constraints)),
    ?assertEqual([], maps:get(forbidden, Constraints)),

    %% Check constraint satisfaction
    ?assert(catena_op_signatures:satisfies_constraints(['IO', 'State'], Sig)),
    ?assert(catena_op_signatures:satisfies_constraints(['IO', 'State', 'Other'], Sig)),
    ?assertNot(catena_op_signatures:satisfies_constraints(['IO'], Sig)),
    ?assertNot(catena_op_signatures:satisfies_constraints([], Sig)),

    %% Row variable escape detection
    RowVar = catena_op_signatures:fresh_row_var(),
    EffectsWithVar = #{
        kind => effect_row,
        elements => ['IO'],
        row_var => RowVar
    },
    SigWithVar = catena_op_signatures:op_sig(op2, [atom], int, EffectsWithVar),
    Scope = catena_op_signatures:scope_add_var(
        catena_op_signatures:new_scope(),
        RowVar
    ),
    ?assertMatch({ok, _}, catena_op_signatures:check_row_var_escape(SigWithVar, Scope)),

    %% Row variable not in scope - should error
    EmptyScope = catena_op_signatures:new_scope(),
    ?assertMatch({error, _}, catena_op_signatures:check_row_var_escape(SigWithVar, EmptyScope)).

%%%---------------------------------------------------------------------
%%% Signature Operations Tests
%%%---------------------------------------------------------------------

test_signature_operations() ->
    %% Row variable substitution
    RowVar = catena_op_signatures:fresh_row_var(),
    Effects1 = #{
        kind => effect_row,
        elements => ['IO'],
        row_var => RowVar
    },
    Sig = catena_op_signatures:op_sig(op, [atom], int, Effects1),

    Replacement = #{
        kind => effect_row,
        elements => ['State'],
        row_var => undefined
    },
    Substituted = catena_op_signatures:substitute_row_var(Sig, RowVar, Replacement),
    SubstitutedEffects = catena_op_signatures:sig_effects(Substituted),
    ?assertEqual(undefined, maps:get(row_var, SubstitutedEffects)),

    %% No substitution when row vars don't match
    OtherVar = catena_op_signatures:fresh_row_var(),
    NotSubstituted = catena_op_signatures:substitute_row_var(Sig, OtherVar, Replacement),
    NotSubstitutedEffects = catena_op_signatures:sig_effects(NotSubstituted),
    ?assertEqual(RowVar, maps:get(row_var, NotSubstitutedEffects)),

    %% Generalize signature
    SimpleEffects = #{
        kind => effect_row,
        elements => [],
        row_var => undefined
    },
    SimpleSig = catena_op_signatures:op_sig(op2, [atom], int, SimpleEffects),
    Generalized = catena_op_signatures:generalize_sig(SimpleSig),
    GeneralizedEffects = catena_op_signatures:sig_effects(Generalized),
    ?assertNotEqual(undefined, maps:get(row_var, GeneralizedEffects)),

    %% Instantiate signature
    Instantiated = catena_op_signatures:instantiate_sig(Generalized, Replacement),
    InstantiatedEffects = catena_op_signatures:sig_effects(Instantiated),
    ?assertEqual(undefined, maps:get(row_var, InstantiatedEffects)),

    %% Signature equality
    Sig1 = catena_op_signatures:op_sig(op1, [atom], int, SimpleEffects),
    Sig2 = catena_op_signatures:op_sig(op1, [atom], int, SimpleEffects),
    ?assert(catena_op_signatures:sig_eq(Sig1, Sig2)),

    Sig3 = catena_op_signatures:op_sig(op1, [bool], int, SimpleEffects),
    ?assertNot(catena_op_signatures:sig_eq(Sig1, Sig3)).

%%%---------------------------------------------------------------------
%%% Pretty Printing Tests
%%%---------------------------------------------------------------------

test_pretty_printing() ->
    %% Format signature
    Effects = #{
        kind => effect_row,
        elements => ['IO', 'State'],
        row_var => undefined
    },
    Sig = catena_op_signatures:op_sig(my_op, [atom, int], bool, Effects),
    Formatted = catena_op_signatures:format_sig(Sig),
    ?assert(is_binary(Formatted)),
    ?assertNotEqual(<<>>, Formatted),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"my_op">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"->">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"/">>)),

    %% Format row variable
    Var1 = catena_op_signatures:fresh_row_var(),
    FormattedVar1 = catena_op_signatures:format_row_var(Var1),
    ?assert(is_binary(FormattedVar1)),
    ?assertNotEqual(<<>>, FormattedVar1),

    Var2 = catena_op_signatures:fresh_row_var(epsilon),
    FormattedVar2 = catena_op_signatures:format_row_var(Var2),
    ?assert(is_binary(FormattedVar2)),
    case binary:match(FormattedVar2, <<"epsilon">>) of
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
