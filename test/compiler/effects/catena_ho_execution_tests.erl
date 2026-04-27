-module(catena_ho_execution_tests).
-include_lib("eunit/include/eunit.hrl").

catena_ho_execution_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"ho op execution", fun test_ho_op_execution/0},
        {"effectual handler invocation", fun test_effectual_invocation/0},
        {"ho effect optimization", fun test_ho_optimization/0},
        {"execution context management", fun test_context_management/0},
        {"error handling", fun test_error_handling/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

test_ho_op_execution() ->
    Context = catena_ho_execution:new_context(),
    {ok, Result, Context1} = catena_ho_execution:execute_ho_op('IO', 'print', [], Context),
    ?assertEqual(undefined, Result),
    ?assertEqual(normal, catena_ho_execution:context_error_state(Context1)),

    HOType = catena_ho_effects:ho_op_type(
        iterate,
        [
            catena_ho_effects:effectful_param(
                int,
                int,
                #{kind => effect_row, elements => ['State'], row_var => undefined}
            ),
            int
        ],
        int,
        #{kind => effect_row, elements => ['IO'], row_var => undefined}
    ),
    Registered = catena_ho_execution:context_register_signature(Context, 'Loop', iterate, HOType),
    Callback = fun(X) -> X * 2 end,
    {ok, CallbackResult, Context2} = catena_ho_execution:execute_ho_op('Loop', iterate, [Callback, 21], Registered),
    ?assertEqual(42, CallbackResult),
    Effects = catena_ho_execution:context_effects(Context2),
    ?assert(lists:member('IO', maps:get(elements, Effects, []))),
    ?assert(lists:member('State', maps:get(elements, Effects, []))),

    {ok, _, _} = catena_ho_execution:execute_with_params('State', 'get', []),

    Fun = fun(X) -> X * 2 end,
    {ok, FunResult} = catena_ho_execution:invoke_effectful_param(Fun, [21]),
    ?assertEqual(42, FunResult),

    ErrorFun = fun(_) -> error(test_error) end,
    ?assertMatch({error, _}, catena_ho_execution:invoke_effectful_param(ErrorFun, [1])).

test_effectual_invocation() ->
    Context = catena_ho_execution:new_context(),
    HandlerFn = fun() ->
        {with_effects, 42, #{kind => effect_row, elements => ['IO'], row_var => undefined}}
    end,
    {ok, Result, HandlerEffects} = catena_ho_execution:invoke_effectual_handler(HandlerFn, [], Context),
    ?assertEqual(42, Result),
    ?assertEqual(['IO'], maps:get(elements, HandlerEffects, [])),

    Effects = catena_ho_execution:capture_effect_context(Context),
    ?assert(is_map(Effects)),
    RestoredContext = catena_ho_execution:restore_effect_context(Effects, Context),
    ?assertEqual(Effects, catena_ho_execution:context_effects(RestoredContext)),

    NewEffects = #{
        kind => effect_row,
        elements => ['IO'],
        row_var => undefined
    },
    UpdatedContext = catena_ho_execution:propagate_handler_effects(NewEffects, Context),
    UpdatedEffects = catena_ho_execution:context_effects(UpdatedContext),
    ?assertEqual(['IO'], maps:get(elements, UpdatedEffects, [])),

    ErrorHandler = fun() -> error(test_error) end,
    ?assertMatch({error, _}, catena_ho_execution:invoke_effectual_handler(ErrorHandler, [], Context)).

test_ho_optimization() ->
    Handlers = [
        {handler, io, fun() -> ok end},
        {handler, io, fun() -> ok end},
        {handler, state, fun() -> ok end}
    ],
    Fused = catena_ho_execution:fuse_effectual_handlers(Handlers),
    ?assertEqual(2, length(Fused)),

    Inlined = catena_ho_execution:inline_simple_handlers(Handlers),
    ?assertMatch({inlined_handler, io, _}, hd(Inlined)),

    HOType = catena_ho_effects:ho_op_type(
        ho_type,
        [catena_ho_effects:effectful_param(atom, atom)],
        atom,
        #{kind => effect_row, elements => ['Reader'], row_var => catena_op_signatures:fresh_row_var(rho)}
    ),
    KnownEffects = #{kind => effect_row, elements => ['State'], row_var => undefined},
    Specialized = catena_ho_execution:specialize_ho_effects(HOType, KnownEffects),
    ?assert(catena_ho_effects:is_ho_op(Specialized)),

    Result = catena_ho_execution:eliminate_dead_effects([{dead_effect, io}, live_effect]),
    ?assertEqual([live_effect], Result).

test_context_management() ->
    Context = catena_ho_execution:new_context(),
    ?assert(is_map(catena_ho_execution:context_effects(Context))),

    NewEffects = #{
        kind => effect_row,
        elements => ['IO', 'State'],
        row_var => undefined
    },
    UpdatedContext = catena_ho_execution:context_add_effects(Context, NewEffects),
    UpdatedEffects = catena_ho_execution:context_effects(UpdatedContext),
    ?assertEqual(2, length(maps:get(elements, UpdatedEffects, []))),

    Effects1 = #{kind => effect_row, elements => ['IO'], row_var => undefined},
    Effects2 = #{kind => effect_row, elements => ['State'], row_var => undefined},
    Context1 = catena_ho_execution:context_with_effects(Effects1),
    Context2 = catena_ho_execution:context_with_effects(Effects2),
    Context3 = catena_ho_execution:context_register_signature(Context2, 'Loop', iterate, ho_type()),
    MergedContext = catena_ho_execution:context_merge(Context1, Context3),
    MergedEffects = catena_ho_execution:context_effects(MergedContext),
    ?assertEqual(2, length(maps:get(elements, MergedEffects, []))),
    ?assertNotEqual(undefined, catena_ho_execution:context_lookup_signature(MergedContext, 'Loop', iterate)),

    ClonedContext = catena_ho_execution:context_clone(Context),
    ?assertEqual(catena_ho_execution:context_effects(Context), catena_ho_execution:context_effects(ClonedContext)).

test_error_handling() ->
    ?assert(catena_ho_execution:is_recoverable(temporary)),
    ?assert(catena_ho_execution:is_recoverable(timeout)),
    ?assert(catena_ho_execution:is_recoverable(retryable)),
    ?assertNot(catena_ho_execution:is_recoverable(permanent)),
    ?assertNot(catena_ho_execution:is_recoverable(fatal)),

    Context = catena_ho_execution:new_context(),
    RecoveryFn = fun(temporary) -> {ok, recovered} end,
    {ok, recovered, _} = catena_ho_execution:handle_execution_error(
        temporary, Context, RecoveryFn),

    ?assertMatch({error, _, _}, catena_ho_execution:handle_execution_error(
        permanent, Context, RecoveryFn)),

    ErrorContext = catena_ho_execution:context_with_error(temporary),
    RecoveredContext = catena_ho_execution:recover_from_error(ErrorContext, RecoveryFn),
    ?assertEqual(normal, catena_ho_execution:context_error_state(RecoveredContext)),

    PermanentErrorContext = catena_ho_execution:context_with_error(permanent),
    RecoveredContext2 = catena_ho_execution:recover_from_error(PermanentErrorContext, RecoveryFn),
    ?assertEqual({error, permanent}, catena_ho_execution:context_error_state(RecoveredContext2)).

ho_type() ->
    catena_ho_effects:ho_op_type(
        iterate,
        [catena_ho_effects:effectful_param(int, int)],
        int,
        #{kind => effect_row, elements => ['IO'], row_var => undefined}
    ).
