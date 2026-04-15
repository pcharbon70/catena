-module(catena_higher_order_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_higher_order_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"signature-based polymorphism integration", fun test_signature_polymorphism/0},
        {"higher-order effect integration", fun test_ho_effects/0},
        {"hefty algebra integration", fun test_hefty_integration/0},
        {"end-to-end higher-order execution", fun test_e2e_execution/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Signature-Based Polymorphism Integration Tests
%%%---------------------------------------------------------------------

test_signature_polymorphism() ->
    %% Test operation signatures with row variables
    RowVar = catena_op_signatures:fresh_row_var(rho),
    Effects = #{
        kind => effect_row,
        elements => ['IO'],
        row_var => RowVar
    },
    Sig = catena_op_signatures:op_sig(my_op, [atom], int, Effects),

    %% Check that row variable is in scope
    Scope = catena_op_signatures:scope_add_var(
        catena_op_signatures:new_scope(),
        RowVar
    ),
    ?assertMatch({ok, _}, catena_op_signatures:check_row_var_escape(Sig, Scope)),

    %% Test signature restrictions
    Constraints = catena_op_signatures:signature_restrictions(Sig),
    ?assertEqual(['IO'], maps:get(required, Constraints)),

    %% Test required effects
    Required = catena_op_signatures:required_effects(Sig),
    ?assertEqual(['IO'], Required),

    %% Test constraint satisfaction
    ?assert(catena_op_signatures:satisfies_constraints(['IO'], Sig)),
    ?assert(catena_op_signatures:satisfies_constraints(['IO', 'State'], Sig)),
    ?assertNot(catena_op_signatures:satisfies_constraints([], Sig)),
    ?assertNot(catena_op_signatures:satisfies_constraints(['State'], Sig)),

    %% Test substitution
    Replacement = #{
        kind => effect_row,
        elements => ['State'],
        row_var => undefined
    },
    Substituted = catena_op_signatures:substitute_row_var(Sig, RowVar, Replacement),
    SubstitutedEffects = catena_op_signatures:sig_effects(Substituted),
    ?assertEqual(undefined, maps:get(row_var, SubstitutedEffects, undefined)),

    %% Test generalization
    SimpleSig = catena_op_signatures:op_sig(simple, [atom], int, #{
        kind => effect_row,
        elements => [],
        row_var => undefined
    }),
    Generalized = catena_op_signatures:generalize_sig(SimpleSig),
    GeneralizedEffects = catena_op_signatures:sig_effects(Generalized),
    ?assertNotEqual(undefined, maps:get(row_var, GeneralizedEffects, undefined)).

%%%---------------------------------------------------------------------
%%% Higher-Order Effect Integration Tests
%%%---------------------------------------------------------------------

test_ho_effects() ->
    %% Create effectful parameter
    EffParam = catena_ho_effects:effectful_param(atom, int),
    ?assert(catena_ho_effects:is_effectful_param(EffParam)),

    %% Create HO operation with effectful parameter
    Params = [atom, EffParam, bool],
    Effects = #{
        kind => effect_row,
        elements => ['State'],
        row_var => undefined
    },
    HO = catena_ho_effects:ho_op_type(iterate, Params, int, Effects),

    %% Validate HO operation
    ?assert(catena_ho_effects:is_ho_op(HO)),
    ?assertEqual(1, catena_ho_effects:count_effectful_params(HO)),

    %% Get effectful params
    EffParams = catena_ho_effects:get_effectful_params(HO),
    ?assertEqual(1, length(EffParams)),
    ?assertEqual(EffParam, hd(EffParams)),

    %% Test param type inference
    Constraints = #{
        effectful_params => 1,
        max_nesting => 2,
        allow_impredicative => false
    },
    InferredParam = catena_ho_effects:infer_param_type(atom, Constraints),
    ?assertNot(catena_ho_effects:is_effectful_param(InferredParam)),

    %% Test effect substitution
    Substitution = #{
        kind => effect_row,
        elements => ['IO'],
        row_var => undefined
    },
    SubstitutedHO = catena_ho_effects:substitute_ho_effects(HO, Substitution),
    ?assert(catena_ho_effects:is_ho_op(SubstitutedHO)),

    %% Test generalization
    GeneralizedHO = catena_ho_effects:generalize_ho_type(HO),
    ?assert(catena_ho_effects:is_ho_op(GeneralizedHO)),

    %% Test type equality
    HOCopy = catena_ho_effects:ho_op_type(iterate, Params, int, Effects),
    ?assert(catena_ho_effects:ho_type_eq(HO, HOCopy)).

%%%---------------------------------------------------------------------
%%% Hefty Algebra Integration Tests
%%%---------------------------------------------------------------------

test_hefty_integration() ->
    %% Create simple hefty tree
    Tree = catena_hefty:pure(42),
    ?assert(catena_hefty:is_hefty_tree(Tree)),
    ?assertEqual(1, catena_hefty:tree_size(Tree)),

    %% Create operation tree
    OpTree = catena_hefty:effect('State', 'get', []),
    ?assert(catena_hefty:is_hefty_tree(OpTree)),

    %% Create bind tree
    Bound = catena_hefty:bind(Tree, fun(X) -> X * 2 end),
    ?assert(catena_hefty:is_hefty_tree(Bound)),

    %% Create sequence
    Seq = catena_hefty:sequence([Tree, OpTree]),
    ?assert(catena_hefty:is_hefty_tree(Seq)),

    %% Create handler
    Ops = #{
        {'State', 'get'} => fun([]) -> 42 end,
        {'State', 'put'} => fun([_]) -> ok end
    },
    Handler = catena_hefty:hefty_handler(state, Ops),

    %% Interpret tree with handler
    Result = catena_hefty:interpret(OpTree, Handler),
    ?assertEqual(42, Result),

    %% Test optimization
    Optimized = catena_hefty:optimize(Seq),
    ?assert(catena_hefty:is_hefty_tree(Optimized)),
    ?assert(catena_hefty:tree_size(Optimized) =< catena_hefty:tree_size(Seq)).

%%%---------------------------------------------------------------------
%%% End-to-End Higher-Order Execution Tests
%%%---------------------------------------------------------------------

test_e2e_execution() ->
    %% Create execution context
    Context = catena_ho_execution:new_context(),

    %% Execute first-order operation
    {ok, _, Context1} = catena_ho_execution:execute_ho_op('IO', 'print', [], Context),
    ?assertEqual(normal, catena_ho_execution:context_error_state(Context1)),

    %% Invoke effectful parameter
    Fun = fun(X) -> X * 2 end,
    {ok, FunResult} = catena_ho_execution:invoke_effectful_param(Fun, [21]),
    ?assertEqual(42, FunResult),

    %% Test context operations
    Effects = #{
        kind => effect_row,
        elements => ['IO', 'State'],
        row_var => undefined
    },
    Context2 = catena_ho_execution:context_add_effects(Context1, Effects),
    ContextEffects = catena_ho_execution:context_effects(Context2),
    ?assertEqual(2, length(maps:get(elements, ContextEffects, []))),

    %% Test context merging
    Context3 = catena_ho_execution:context_with_effects(#{
        kind => effect_row,
        elements => ['IO'],
        row_var => undefined
    }),
    Merged = catena_ho_execution:context_merge(Context2, Context3),
    MergedEffects = catena_ho_execution:context_effects(Merged),
    ?assert(lists:member('IO', maps:get(elements, MergedEffects, []))),
    ?assert(lists:member('State', maps:get(elements, MergedEffects, []))),

    %% Test error handling
    ?assert(catena_ho_execution:is_recoverable(temporary)),
    ?assert(catena_ho_execution:is_recoverable(timeout)),
    ?assertNot(catena_ho_execution:is_recoverable(permanent)),

    %% Test error recovery
    RecoveryFn = fun(temporary) -> {ok, recovered} end,
    ErrorContext = catena_ho_execution:context_with_error(temporary),
    RecoveredContext = catena_ho_execution:recover_from_error(ErrorContext, RecoveryFn),
    ?assertEqual(normal, catena_ho_execution:context_error_state(RecoveredContext)),

    %% Test effectual handler invocation
    HandlerFn = fun() -> 42 end,
    {ok, HandlerResult, _} = catena_ho_execution:invoke_effectual_handler(HandlerFn, [], Context),
    ?assertEqual(42, HandlerResult).

%%%---------------------------------------------------------------------
%%% Combined Integration Tests
%%%---------------------------------------------------------------------

%% Test the full flow from operation signatures to execution
test_full_flow() ->
    %% Define operation signature
    RowVar = catena_op_signatures:fresh_row_var(),
    OpSig = catena_op_signatures:op_sig(
        perform_io,
        [string],
        int,
        #{
            kind => effect_row,
            elements => ['IO'],
            row_var => RowVar
        }
    ),

    %% Create HO type with effectful param
    EffParam = catena_ho_effects:effectful_param(string, int),
    HO = catena_ho_effects:ho_op_type(
        map_io,
        [EffParam],
        int,
        catena_op_signatures:sig_effects(OpSig)
    ),

    %% Verify the HO type
    ?assert(catena_ho_effects:is_ho_op(HO)),
    ?assertEqual(1, catena_ho_effects:count_effectful_params(HO)),

    %% Create hefty tree for execution
    Tree = catena_hefty:effect('IO', 'perform_io', ["test"]),
    ?assert(catena_hefty:is_hefty_tree(Tree)),

    %% Execute with context
    Context = catena_ho_execution:new_context(),
    {ok, _, ExecContext} = catena_ho_execution:execute_ho_op('IO', 'perform_io', ["test"], Context),
    ?assertEqual(normal, catena_ho_execution:context_error_state(ExecContext)).
