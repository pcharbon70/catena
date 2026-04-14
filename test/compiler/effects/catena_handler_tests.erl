%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_handler and catena_handler_stack (Phase 7.2)
%%%
%%% Tests for the handler type and interface:
%%% - Handler type definition and constructors
%%% - Handler registration and lookup
%%% - Handler stack management for nested scopes
%%% - Handler execution protocol
%%%
%%% Note: Tests use simple test functions (not generators) to ensure all
%%% operations run in the same process and can access the process dictionary
%%% used by the handler stack.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% @private Setup handler stack for a test.
setup_stack() ->
    erase(catena_handler_stack),
    catena_handler_stack:push().

%% @private Cleanup handler stack after a test.
cleanup_stack() ->
    erase(catena_handler_stack).

%% @private Setup with an empty stack (no scopes).
setup_empty_stack() ->
    erase(catena_handler_stack).

%%%=============================================================================
%%% Handler Type and Constructors Tests
%%%=============================================================================

handler_new_test() ->
    HandlerFn = fun(Value, Resumption) -> {handled, Value, Resumption} end,
    Handler = catena_handler:new(my_operation, HandlerFn),
    ?assert(catena_handler:is_handler(Handler)),
    ?assertEqual(my_operation, catena_handler:operation_of(Handler)).

handler_new_with_metadata_test() ->
    HandlerFn = fun(Value, Resumption) -> {handled, Value, Resumption} end,
    Metadata = #{priority => high},
    Handler = catena_handler:new(my_operation, HandlerFn, Metadata),
    ?assert(catena_handler:is_handler(Handler)),
    ?assertEqual(Metadata, catena_handler:metadata_of(Handler)),
    ?assertEqual(HandlerFn, catena_handler:handler_fn_of(Handler)).

handler_is_handler_test() ->
    Handler = catena_handler:new(op, fun(V, R) -> {V, R} end),
    ?assert(catena_handler:is_handler(Handler)),
    ?assertNot(catena_handler:is_handler(not_a_handler)),
    ?assertNot(catena_handler:is_handler({handler, partial})).

handler_operation_of_test() ->
    Handler = catena_handler:new(test_op, fun(V, R) -> {V, R} end),
    ?assertEqual(test_op, catena_handler:operation_of(Handler)).

handler_handler_fn_of_test() ->
    HandlerFn = fun(Value, Resumption) -> {handled, Value, Resumption} end,
    Handler = catena_handler:new(op, HandlerFn),
    ?assertEqual(HandlerFn, catena_handler:handler_fn_of(Handler)).

%%%=============================================================================
%%% Handler Registration Tests
%%%=============================================================================

handler_register_test() ->
    setup_stack(),
    Handler = catena_handler:new(op, fun(V, R) -> {V, R} end),
    ?assertEqual(ok, catena_handler:register(op, Handler)),
    ?assertEqual({ok, Handler}, catena_handler:lookup(op)),
    cleanup_stack().

handler_unregister_test() ->
    setup_stack(),
    Handler = catena_handler:new(op, fun(V, R) -> {V, R} end),
    catena_handler:register(op, Handler),
    ?assertEqual(ok, catena_handler:unregister(op)),
    ?assertEqual({error, not_found}, catena_handler:lookup(op)),
    cleanup_stack().

handler_lookup_not_found_test() ->
    setup_stack(),
    ?assertEqual({error, not_found}, catena_handler:lookup(nonexistent_op)),
    cleanup_stack().

handler_replace_test() ->
    setup_stack(),
    Handler1 = catena_handler:new(op, fun(V, R) -> {v1, V, R} end),
    Handler2 = catena_handler:new(op, fun(V, R) -> {v2, V, R} end),
    catena_handler:register(op, Handler1),
    catena_handler:register(op, Handler2),
    ?assertEqual({ok, Handler2}, catena_handler:lookup(op)),
    cleanup_stack().

%%%=============================================================================
%%% Handler Execution Tests
%%%=============================================================================

handler_execute_success_test() ->
    setup_stack(),
    HandlerFn = fun(Value, _Resumption) -> {result, Value} end,
    Handler = catena_handler:new(op, HandlerFn),
    Resumption = catena_resumption:new(fun(X) -> X end, 0, 0),
    ?assertEqual({result, 42}, catena_handler:execute(Handler, 42, Resumption)),
    cleanup_stack().

handler_execute_with_resumption_test() ->
    setup_stack(),
    HandlerFn = fun(_Value, Resumption) ->
        catena_resumption:resume(Resumption, 21)
    end,
    Handler = catena_handler:new(op, HandlerFn),
    Resumption = catena_resumption:new(fun(X) -> {resumed, X * 2} end, 0, 0),
    ?assertEqual({resumed, 42}, catena_handler:execute(Handler, ignored, Resumption)),
    cleanup_stack().

handler_execute_error_test() ->
    setup_stack(),
    HandlerFn = fun(_Value, _Resumption) -> error(intentional) end,
    Handler = catena_handler:new(op, HandlerFn),
    Resumption = catena_resumption:new(fun(X) -> X end, 0, 0),
    Result = catena_handler:execute(Handler, test, Resumption),
    ?assertMatch({handler_error, error, intentional, _}, Result),
    cleanup_stack().

%%%=============================================================================
%%% Handler Stack Tests
%%%=============================================================================

handler_stack_new_test() ->
    ?assertEqual([], catena_handler_stack:new()),
    Registry = #{op => dummy_handler},
    ?assertEqual([Registry], catena_handler_stack:new(Registry)).

handler_stack_push_pop_test() ->
    setup_stack(),
    StartDepth = catena_handler_stack:depth(),
    ?assertEqual(1, StartDepth),
    catena_handler_stack:push(),
    ?assertEqual(StartDepth + 1, catena_handler_stack:depth()),
    ?assertMatch({ok, _Registry}, catena_handler_stack:pop()),
    ?assertEqual(StartDepth, catena_handler_stack:depth()),
    cleanup_stack().

handler_stack_push_with_registry_test() ->
    setup_stack(),
    Registry = #{op => catena_handler:new(op, fun(V, R) -> {V, R} end)},
    catena_handler_stack:push(Registry),
    Current = catena_handler_stack:current(),
    ?assertEqual(Registry, Current),
    cleanup_stack().

handler_stack_pop_empty_test() ->
    setup_empty_stack(),
    ?assertEqual({error, empty}, catena_handler_stack:pop()).

handler_stack_pop_with_default_test() ->
    setup_empty_stack(),
    Default = #{},
    ?assertEqual(Default, catena_handler_stack:pop(Default)).

%%%=============================================================================
%%% Handler Stack Inspection Tests
%%%=============================================================================

handler_stack_current_test() ->
    setup_stack(),
    Current = catena_handler_stack:current(),
    ?assert(is_map(Current)),
    Registry = #{op => catena_handler:new(op, fun(V, R) -> {V, R} end)},
    catena_handler_stack:push(Registry),
    ?assertEqual(Registry, catena_handler_stack:current()),
    cleanup_stack().

handler_stack_depth_test() ->
    setup_stack(),
    StartDepth = catena_handler_stack:depth(),
    catena_handler_stack:push(),
    ?assertEqual(StartDepth + 1, catena_handler_stack:depth()),
    catena_handler_stack:push(),
    ?assertEqual(StartDepth + 2, catena_handler_stack:depth()),
    catena_handler_stack:pop(),
    ?assertEqual(StartDepth + 1, catena_handler_stack:depth()),
    cleanup_stack().

handler_stack_is_empty_test() ->
    setup_empty_stack(),
    ?assert(catena_handler_stack:is_empty()).

%%%=============================================================================
%%% Handler Stack Lookup Tests
%%%=============================================================================

handler_stack_lookup_single_scope_test() ->
    setup_stack(),
    Handler = catena_handler:new(op, fun(V, R) -> {V, R} end),
    catena_handler_stack:register_in_current(op, Handler),
    ?assertEqual({ok, Handler}, catena_handler_stack:lookup(op)),
    ?assertEqual({error, not_found}, catena_handler_stack:lookup(other)),
    cleanup_stack().

handler_stack_lookup_nested_scopes_test() ->
    setup_stack(),
    OuterHandler = catena_handler:new(op, fun(V, R) -> {outer, V, R} end),
    InnerHandler = catena_handler:new(op, fun(V, R) -> {inner, V, R} end),

    catena_handler_stack:register_in_current(op, OuterHandler),
    ?assertEqual({ok, OuterHandler}, catena_handler_stack:lookup(op)),

    catena_handler_stack:push(),
    catena_handler_stack:register_in_current(op, InnerHandler),
    ?assertEqual({ok, InnerHandler}, catena_handler_stack:lookup(op)),

    {ok, _} = catena_handler_stack:pop(),
    ?assertEqual({ok, OuterHandler}, catena_handler_stack:lookup(op)),
    cleanup_stack().

handler_stack_lookup_in_current_test() ->
    setup_stack(),
    Handler = catena_handler:new(op, fun(V, R) -> {V, R} end),
    catena_handler_stack:register_in_current(op, Handler),
    ?assertEqual({ok, Handler}, catena_handler_stack:lookup_in_current(op)),

    catena_handler_stack:push(),
    ?assertEqual({error, not_found}, catena_handler_stack:lookup_in_current(op)),
    cleanup_stack().

%%%=============================================================================
%%% Handler Stack Registry Operations Tests
%%%=============================================================================

handler_stack_register_in_current_test() ->
    setup_stack(),
    Handler = catena_handler:new(op, fun(V, R) -> {V, R} end),
    ?assertEqual(ok, catena_handler_stack:register_in_current(op, Handler)),
    ?assertEqual({ok, Handler}, catena_handler_stack:lookup(op)),
    cleanup_stack().

handler_stack_register_in_current_empty_test() ->
    setup_empty_stack(),
    ?assertEqual({error, no_current_scope}, catena_handler_stack:register_in_current(op, dummy)).

handler_stack_unregister_in_current_test() ->
    setup_stack(),
    Handler = catena_handler:new(op, fun(V, R) -> {V, R} end),
    catena_handler_stack:register_in_current(op, Handler),
    ?assertEqual(ok, catena_handler_stack:unregister_in_current(op)),
    ?assertEqual({error, not_found}, catena_handler_stack:lookup(op)),
    cleanup_stack().

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

handler_nested_scope_test() ->
    setup_stack(),
    OuterHandler = catena_handler:new(io, fun(V, R) -> {outer_io, V, R} end),
    InnerHandler = catena_handler:new(state, fun(V, R) -> {inner_state, V, R} end),

    catena_handler:register(io, OuterHandler),
    {ok, FoundOuter} = catena_handler_stack:lookup(io),
    ?assertEqual(io, catena_handler:operation_of(FoundOuter)),

    catena_handler_stack:push(),
    catena_handler:register(state, InnerHandler),
    {ok, FoundInner} = catena_handler_stack:lookup(state),
    ?assertEqual(state, catena_handler:operation_of(FoundInner)),

    {ok, _} = catena_handler_stack:pop(),
    {ok, FoundOuter2} = catena_handler_stack:lookup(io),
    ?assertEqual(io, catena_handler:operation_of(FoundOuter2)),
    cleanup_stack().

handler_execute_with_resume_test() ->
    setup_stack(),
    Resumption = catena_resumption:new(fun(X) -> {resumed, X * 2} end, 0, 0),
    Handler = catena_handler:new(op, fun(_Value, Res) ->
        catena_resumption:resume(Res, 21)
    end),
    Result = catena_handler:execute(Handler, ignored, Resumption),
    ?assertEqual({resumed, 42}, Result),
    cleanup_stack().

handler_shadowing_test() ->
    setup_stack(),
    OuterHandler = catena_handler:new(op, fun(_V, _R) -> outer end),
    InnerHandler = catena_handler:new(op, fun(_V, _R) -> inner end),

    catena_handler:register(op, OuterHandler),
    catena_handler_stack:push(),
    catena_handler:register(op, InnerHandler),

    {ok, FoundHandler} = catena_handler_stack:lookup(op),
    ?assertEqual(op, catena_handler:operation_of(FoundHandler)),

    catena_handler:unregister(op),
    {ok, FoundHandler2} = catena_handler_stack:lookup(op),
    ?assertEqual(op, catena_handler:operation_of(FoundHandler2)),

    {ok, _} = catena_handler_stack:pop(),
    {ok, FoundHandler3} = catena_handler_stack:lookup(op),
    ?assertEqual(op, catena_handler:operation_of(FoundHandler3)),
    cleanup_stack().

handler_multi_scope_test() ->
    setup_stack(),
    Handler1 = catena_handler:new(op, fun(V, R) -> {h1, V, R} end),
    Handler2 = catena_handler:new(op, fun(V, R) -> {h2, V, R} end),
    Handler3 = catena_handler:new(op, fun(V, R) -> {h3, V, R} end),

    catena_handler_stack:register_in_current(op, Handler1),
    ?assertEqual({ok, Handler1}, catena_handler_stack:lookup(op)),

    catena_handler_stack:push(),
    catena_handler_stack:register_in_current(op, Handler2),
    ?assertEqual({ok, Handler2}, catena_handler_stack:lookup(op)),

    catena_handler_stack:push(),
    catena_handler_stack:register_in_current(op, Handler3),
    ?assertEqual({ok, Handler3}, catena_handler_stack:lookup(op)),

    {ok, _} = catena_handler_stack:pop(),
    ?assertEqual({ok, Handler2}, catena_handler_stack:lookup(op)),

    {ok, _} = catena_handler_stack:pop(),
    ?assertEqual({ok, Handler1}, catena_handler_stack:lookup(op)),
    cleanup_stack().

handler_execute_timeout_test() ->
    setup_stack(),
    HandlerFn = fun(_Value, _Resumption) ->
        timer:sleep(200),
        timeout_result
    end,
    Handler = catena_handler:new(op, HandlerFn),
    Resumption = catena_resumption:new(fun(X) -> X end, 0, 0),
    Result = catena_handler:execute_with_timeout(Handler, test, Resumption, 50),
    ?assertMatch({timeout, test}, Result),
    cleanup_stack().
