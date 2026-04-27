-module(catena_higher_order_integration_tests).
-include_lib("eunit/include/eunit.hrl").

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

setup() ->
    ok.

cleanup(_) ->
    ok.

test_signature_polymorphism() ->
    Root = catena_op_signatures:new_scope(),
    Scope = catena_op_signatures:enter_scope(Root),
    RowVar = catena_op_signatures:fresh_row_var(rho),
    Scoped = catena_op_signatures:scope_add_var(Scope, RowVar),
    Sig = catena_op_signatures:op_sig(
        iterate,
        [atom],
        int,
        #{
            kind => effect_row,
            elements => ['IO'],
            row_var => RowVar
        }
    ),

    ?assertMatch({ok, _}, catena_op_signatures:check_row_var_escape(Sig, Scoped)),
    Constraints = catena_op_signatures:signature_restrictions(Sig),
    ?assertEqual(['IO'], maps:get(required, Constraints)),
    ?assertEqual(['IO'], catena_op_signatures:required_effects(Sig)),
    ?assert(catena_op_signatures:satisfies_constraints(['IO', 'State'], Sig)),
    ?assertNot(catena_op_signatures:satisfies_constraints(['State'], Sig)),

    Replacement = #{
        kind => effect_row,
        elements => ['State'],
        row_var => undefined
    },
    Instantiated = catena_op_signatures:instantiate_sig(Sig, Replacement),
    InstantiatedEffects = catena_op_signatures:sig_effects(Instantiated),
    ?assertEqual(['IO', 'State'], maps:get(elements, InstantiatedEffects)),
    ?assertEqual(undefined, maps:get(row_var, InstantiatedEffects)).

test_ho_effects() ->
    CallbackParam = catena_ho_effects:effectful_param(
        int,
        int,
        #{
            kind => effect_row,
            elements => ['State'],
            row_var => undefined
        }
    ),
    HO = catena_ho_effects:ho_op_type(
        iterate,
        [CallbackParam, int],
        int,
        #{
            kind => effect_row,
            elements => ['IO'],
            row_var => catena_op_signatures:fresh_row_var(epsilon)
        }
    ),

    ?assert(catena_ho_effects:is_ho_op(HO)),
    ?assertEqual(1, catena_ho_effects:count_effectful_params(HO)),

    Context = catena_ho_execution:context_register_signature(
        catena_ho_execution:new_context(),
        'Loop',
        iterate,
        HO
    ),
    Callback = fun(X) -> X * 2 end,
    {ok, Result, Context1} = catena_ho_execution:execute_ho_op('Loop', iterate, [Callback, 21], Context),
    ?assertEqual(42, Result),
    ContextEffects = catena_ho_execution:context_effects(Context1),
    ?assert(lists:member('IO', maps:get(elements, ContextEffects, []))),
    ?assert(lists:member('State', maps:get(elements, ContextEffects, []))).

test_hefty_integration() ->
    Tree = catena_hefty:sequence([
        catena_hefty:effect('State', 'get', []),
        catena_hefty:effect('State', 'tick', fun(Value) -> catena_hefty:pure(Value + 1) end)
    ]),
    ?assert(catena_hefty:is_hefty_tree(Tree)),

    Handler = catena_hefty:hefty_handler(state, #{
        {'State', 'get'} => fun([]) -> 41 end,
        {'State', 'tick'} => fun([], Continuation) -> Continuation(41) end
    }),
    Result = catena_hefty_interpreter:interpret(Tree, Handler),
    ?assertEqual(42, Result),

    Optimized = catena_hefty:optimize(Tree),
    ?assert(catena_hefty:is_hefty_tree(Optimized)),
    ?assert(catena_hefty:tree_size(Optimized) =< catena_hefty:tree_size(Tree)).

test_e2e_execution() ->
    RowVar = catena_op_signatures:fresh_row_var(rho),
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
    CallbackParam = catena_ho_effects:effectful_param(
        string,
        int,
        catena_op_signatures:sig_effects(OpSig)
    ),
    HO = catena_ho_effects:ho_op_type(
        map_io,
        [CallbackParam, string],
        int,
        catena_op_signatures:sig_effects(OpSig)
    ),
    Context0 = catena_ho_execution:context_register_signature(
        catena_ho_execution:new_context(),
        'IO',
        map_io,
        HO
    ),
    Context = catena_ho_execution:context_add_effects(
        Context0,
        #{
            kind => effect_row,
            elements => ['Reader'],
            row_var => undefined
        }
    ),
    Callback = fun(_Text) -> 42 end,
    {ok, Result, ExecContext} = catena_ho_execution:execute_ho_op('IO', map_io, [Callback, "test"], Context),
    ?assertEqual(42, Result),
    ?assertEqual(normal, catena_ho_execution:context_error_state(ExecContext)),

    FinalEffects = catena_ho_execution:context_effects(ExecContext),
    ?assert(lists:member('IO', maps:get(elements, FinalEffects, []))),
    ?assert(lists:member('Reader', maps:get(elements, FinalEffects, []))).
