-module(catena_ho_execution_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

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

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Higher-Order Operation Execution Tests
%%%---------------------------------------------------------------------

test_ho_op_execution() ->
    %% Execute first-order operation
    Context = catena_ho_execution:new_context(),
    {ok, Result, _} = catena_ho_execution:execute_ho_op('IO', 'print', [], Context),
    ?assertEqual(undefined, Result),

    %% Execute with params
    {ok, _, _} = catena_ho_execution:execute_with_params('State', 'get', []),

    %% Invoke effectful param
    Fun = fun(X) -> X * 2 end,
    {ok, FunResult} = catena_ho_execution:invoke_effectful_param(Fun, [21]),
    ?assertEqual(42, FunResult),

    %% Invoke with error
    ErrorFun = fun(_) -> error(test_error) end,
    ?assertMatch({error, _}, catena_ho_execution:invoke_effectful_param(ErrorFun, [])).

%%%---------------------------------------------------------------------
%%% Effectual Handler Invocation Tests
%%%---------------------------------------------------------------------

test_effectual_invocation() ->
    %% Invoke effectual handler
    Context = catena_ho_execution:new_context(),
    HandlerFn = fun() -> 42 end,
    {ok, Result, _} = catena_ho_execution:invoke_effectual_handler(HandlerFn, [], Context),
    ?assertEqual(42, Result),

    %% Capture and restore context
    Effects = catena_ho_execution:capture_effect_context(Context),
    ?assert(is_map(Effects)),
    RestoredContext = catena_ho_execution:restore_effect_context(Effects, Context),
    ?assertEqual(Effects, catena_ho_execution:context_effects(RestoredContext)),

    %% Propagate handler effects
    NewEffects = #{
        kind => effect_row,
        elements => ['IO'],
        row_var => undefined
    },
    UpdatedContext = catena_ho_execution:propagate_handler_effects(NewEffects, Context),
    UpdatedEffects = catena_ho_execution:context_effects(UpdatedContext),
    ?assertEqual(['IO'], maps:get(elements, UpdatedEffects, [])),

    %% Handler error case - handler throws an error
    ErrorHandler = fun() -> error(test_error) end,
    ?assertMatch({error, _}, catena_ho_execution:invoke_effectual_handler(ErrorHandler, [], Context)).

%%%---------------------------------------------------------------------
%%% Higher-Order Effect Optimization Tests
%%%---------------------------------------------------------------------

test_ho_optimization() ->
    %% Fuse effectual handlers
    Handlers = [handler1, handler2, handler3],
    Fused = catena_ho_execution:fuse_effectual_handlers(Handlers),
    ?assert(is_list(Fused)),
    ?assertEqual(3, length(Fused)),

    %% Inline simple handlers
    Inlined = catena_ho_execution:inline_simple_handlers(Handlers),
    ?assert(is_list(Inlined)),
    ?assertEqual(3, length(Inlined)),

    %% Specialize HO effects
    HOType = {ho_type, example},
    KnownEffects = #{kind => effect_row, elements => ['State'], row_var => undefined},
    Specialized = catena_ho_execution:specialize_ho_effects(HOType, KnownEffects),
    ?assert(is_tuple(Specialized)),

    %% Eliminate dead effects
    Result = catena_ho_execution:eliminate_dead_effects(HOType),
    ?assert(is_tuple(Result)).

%%%---------------------------------------------------------------------
%%% Execution Context Management Tests
%%%---------------------------------------------------------------------

test_context_management() ->
    %% New context
    Context = catena_ho_execution:new_context(),
    ?assert(is_map(catena_ho_execution:context_effects(Context))),

    %% Add effects to context
    NewEffects = #{
        kind => effect_row,
        elements => ['IO', 'State'],
        row_var => undefined
    },
    UpdatedContext = catena_ho_execution:context_add_effects(Context, NewEffects),
    UpdatedEffects = catena_ho_execution:context_effects(UpdatedContext),
    ?assertEqual(2, length(maps:get(elements, UpdatedEffects, []))),

    %% Merge contexts
    Effects1 = #{kind => effect_row, elements => ['IO'], row_var => undefined},
    Effects2 = #{kind => effect_row, elements => ['State'], row_var => undefined},
    Context1 = catena_ho_execution:context_with_effects(Effects1),
    Context2 = catena_ho_execution:context_with_effects(Effects2),
    MergedContext = catena_ho_execution:context_merge(Context1, Context2),
    MergedEffects = catena_ho_execution:context_effects(MergedContext),
    ?assertEqual(2, length(maps:get(elements, MergedEffects, []))),

    %% Clone context
    ClonedContext = catena_ho_execution:context_clone(Context),
    ContextEffects = catena_ho_execution:context_effects(Context),
    ClonedEffects = catena_ho_execution:context_effects(ClonedContext),
    ?assertEqual(ContextEffects, ClonedEffects).

%%%---------------------------------------------------------------------
%%% Error Handling Tests
%%%---------------------------------------------------------------------

test_error_handling() ->
    %% Recoverable errors
    ?assert(catena_ho_execution:is_recoverable(temporary)),
    ?assert(catena_ho_execution:is_recoverable(timeout)),
    ?assert(catena_ho_execution:is_recoverable(retryable)),
    ?assertNot(catena_ho_execution:is_recoverable(permanent)),
    ?assertNot(catena_ho_execution:is_recoverable(fatal)),

    %% Handle execution error with recovery
    Context = catena_ho_execution:new_context(),
    RecoveryFn = fun(temporary) -> {ok, recovered} end,
    {ok, recovered, _} = catena_ho_execution:handle_execution_error(
        temporary, Context, RecoveryFn),

    %% Non-recoverable error
    ?assertMatch({error, _, _}, catena_ho_execution:handle_execution_error(
        permanent, Context, RecoveryFn)),

    %% Recover from error in context
    ErrorContext = catena_ho_execution:context_with_error(temporary),
    RecoveredContext = catena_ho_execution:recover_from_error(ErrorContext, RecoveryFn),
    ?assertEqual(normal, catena_ho_execution:context_error_state(RecoveredContext)),

    %% Non-recoverable error in context
    PermanentErrorContext = catena_ho_execution:context_with_error(permanent),
    RecoveredContext2 = catena_ho_execution:recover_from_error(PermanentErrorContext, RecoveryFn),
    ?assertEqual({error, permanent}, catena_ho_execution:context_error_state(RecoveredContext2)).
