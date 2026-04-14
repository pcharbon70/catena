%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_handler_stack (Phase 7.2)
%%%
%%% Tests for handler stack management:
%%% - Stack creation and manipulation
%%% - Nested scope handling
%%% - Handler shadowing and precedence
%%% - Scope exit cleanup
%%%
%%% Note: Tests use simple test functions (not generators) to ensure all
%%% operations run in the same process and can access the process dictionary
%%% used by the handler stack.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_handler_stack_tests).
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
%%% Stack Creation Tests
%%%=============================================================================

stack_new_test() ->
    Stack = catena_handler_stack:new(),
    ?assertEqual([], Stack).

stack_new_with_registry_test() ->
    setup_empty_stack(),
    Registry = #{op => dummy_handler},
    Stack = catena_handler_stack:new(Registry),
    ?assertEqual([Registry], Stack),
    catena_handler_stack:push(Registry),
    ?assertEqual(1, catena_handler_stack:depth()),
    cleanup_stack().

%%%=============================================================================
%%% Stack Manipulation Tests
%%%=============================================================================

stack_push_empty_test() ->
    ok = catena_handler_stack:push(),
    ?assertEqual(1, catena_handler_stack:depth()),
    {ok, Registry} = catena_handler_stack:pop(),
    ?assert(is_map(Registry)),
    ?assertEqual(0, catena_handler_stack:depth()),
    cleanup_stack().

stack_push_with_registry_test() ->
    setup_stack(),
    Registry = #{op => dummy_handler},
    ok = catena_handler_stack:push(Registry),
    ?assertEqual(2, catena_handler_stack:depth()),
    Current = catena_handler_stack:current(),
    ?assertEqual(Registry, Current),
    cleanup_stack().

stack_pop_test() ->
    setup_stack(),
    {ok, Registry} = catena_handler_stack:pop(),
    ?assert(is_map(Registry)),
    ?assertEqual(0, catena_handler_stack:depth()).

stack_pop_empty_test() ->
    setup_empty_stack(),
    ?assertEqual({error, empty}, catena_handler_stack:pop()).

stack_pop_with_default_test() ->
    setup_empty_stack(),
    Default = #{default => value},
    ?assertEqual(Default, catena_handler_stack:pop(Default)).

stack_multiple_push_pop_test() ->
    setup_empty_stack(),
    R1 = #{r1 => v1},
    R2 = #{r2 => v2},
    R3 = #{r3 => v3},
    catena_handler_stack:push(R1),
    catena_handler_stack:push(R2),
    catena_handler_stack:push(R3),
    ?assertEqual(3, catena_handler_stack:depth()),
    {ok, Top} = catena_handler_stack:pop(),
    ?assertEqual(R3, Top),
    ?assertEqual(2, catena_handler_stack:depth()),
    {ok, Next} = catena_handler_stack:pop(),
    ?assertEqual(R2, Next),
    ?assertEqual(1, catena_handler_stack:depth()),
    {ok, Last} = catena_handler_stack:pop(),
    ?assertEqual(R1, Last),
    ?assertEqual(0, catena_handler_stack:depth()),
    cleanup_stack().

%%%=============================================================================
%%% Stack Inspection Tests
%%%=============================================================================

stack_current_test() ->
    setup_empty_stack(),
    ?assertEqual(undefined, catena_handler_stack:current()),
    Registry = #{op => value},
    catena_handler_stack:push(Registry),
    ?assertEqual(Registry, catena_handler_stack:current()),
    cleanup_stack().

stack_depth_test() ->
    setup_empty_stack(),
    ?assertEqual(0, catena_handler_stack:depth()),
    catena_handler_stack:push(),
    ?assertEqual(1, catena_handler_stack:depth()),
    catena_handler_stack:push(),
    ?assertEqual(2, catena_handler_stack:depth()),
    catena_handler_stack:pop(),
    ?assertEqual(1, catena_handler_stack:depth()),
    cleanup_stack().

stack_is_empty_test() ->
    setup_empty_stack(),
    ?assert(catena_handler_stack:is_empty()),
    catena_handler_stack:push(),
    ?assertNot(catena_handler_stack:is_empty()),
    catena_handler_stack:pop(),
    ?assert(catena_handler_stack:is_empty()),
    cleanup_stack().

stack_to_list_test() ->
    setup_empty_stack(),
    ?assertEqual([], catena_handler_stack:to_list()),
    R1 = #{r1 => v1},
    R2 = #{r2 => v2},
    catena_handler_stack:push(R1),
    catena_handler_stack:push(R2),
    List = catena_handler_stack:to_list(),
    ?assertEqual([R2, R1], List),
    cleanup_stack().

%%%=============================================================================
%%% Handler Lookup Tests
%%%=============================================================================

stack_lookup_single_scope_test() ->
    setup_stack(),
    Handler = catena_handler:new(op, fun(V, R) -> {V, R} end),
    catena_handler_stack:register_in_current(op, Handler),
    ?assertEqual({ok, Handler}, catena_handler_stack:lookup(op)),
    ?assertEqual({error, not_found}, catena_handler_stack:lookup(other)),
    cleanup_stack().

stack_lookup_nested_scopes_test() ->
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

stack_lookup_in_current_test() ->
    setup_stack(),
    Handler = catena_handler:new(op, fun(V, R) -> {V, R} end),
    catena_handler_stack:register_in_current(op, Handler),
    ?assertEqual({ok, Handler}, catena_handler_stack:lookup_in_current(op)),

    catena_handler_stack:push(),
    ?assertEqual({error, not_found}, catena_handler_stack:lookup_in_current(op)),
    cleanup_stack().

stack_lookup_all_test() ->
    setup_stack(),
    Handler1 = catena_handler:new(op, fun(V, R) -> {h1, V, R} end),
    Handler2 = catena_handler:new(op, fun(V, R) -> {h2, V, R} end),
    Handler3 = catena_handler:new(op, fun(V, R) -> {h3, V, R} end),

    catena_handler_stack:register_in_current(op, Handler1),
    catena_handler_stack:push(),
    catena_handler_stack:register_in_current(op, Handler2),
    catena_handler_stack:push(),
    catena_handler_stack:register_in_current(op, Handler3),

    All = catena_handler_stack:lookup_all(op),
    ?assertEqual(3, length(All)),
    ?assertEqual(Handler3, lists:nth(1, All)),
    ?assertEqual(Handler2, lists:nth(2, All)),
    ?assertEqual(Handler1, lists:nth(3, All)),
    cleanup_stack().

%%%=============================================================================
%%% Registry Operations Tests
%%%=============================================================================

stack_register_in_current_test() ->
    setup_stack(),
    Handler = catena_handler:new(op, fun(V, R) -> {V, R} end),
    ?assertEqual(ok, catena_handler_stack:register_in_current(op, Handler)),
    ?assertEqual({ok, Handler}, catena_handler_stack:lookup(op)),
    cleanup_stack().

stack_register_in_current_empty_test() ->
    setup_empty_stack(),
    ?assertEqual({error, no_current_scope}, catena_handler_stack:register_in_current(op, dummy)).

stack_unregister_in_current_test() ->
    setup_stack(),
    Handler = catena_handler:new(op, fun(V, R) -> {V, R} end),
    catena_handler_stack:register_in_current(op, Handler),
    ?assertEqual(ok, catena_handler_stack:unregister_in_current(op)),
    ?assertEqual({error, not_found}, catena_handler_stack:lookup(op)),
    cleanup_stack().

stack_replace_handler_test() ->
    setup_stack(),
    Handler1 = catena_handler:new(op, fun(V, R) -> {v1, V, R} end),
    Handler2 = catena_handler:new(op, fun(V, R) -> {v2, V, R} end),

    catena_handler_stack:register_in_current(op, Handler1),
    ?assertEqual({ok, Handler1}, catena_handler_stack:lookup(op)),

    catena_handler_stack:register_in_current(op, Handler2),
    ?assertEqual({ok, Handler2}, catena_handler_stack:lookup(op)),
    cleanup_stack().

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

stack_complex_nesting_test() ->
    setup_stack(),
    Handler1 = catena_handler:new(op1, fun(V, R) -> {h1, V, R} end),
    Handler2 = catena_handler:new(op2, fun(V, R) -> {h2, V, R} end),
    Handler3 = catena_handler:new(op1, fun(V, R) -> {h3, V, R} end),

    catena_handler_stack:register_in_current(op1, Handler1),
    ?assertEqual({ok, Handler1}, catena_handler_stack:lookup(op1)),
    ?assertEqual({error, not_found}, catena_handler_stack:lookup(op2)),

    catena_handler_stack:push(),
    catena_handler_stack:register_in_current(op2, Handler2),
    ?assertEqual({ok, Handler2}, catena_handler_stack:lookup(op2)),
    ?assertEqual({error, not_found}, catena_handler_stack:lookup_in_current(op1)),
    ?assertEqual({ok, Handler1}, catena_handler_stack:lookup(op1)),

    catena_handler_stack:push(),
    catena_handler_stack:register_in_current(op1, Handler3),
    ?assertEqual({ok, Handler3}, catena_handler_stack:lookup(op1)),
    ?assertEqual({ok, Handler3}, catena_handler_stack:lookup_in_current(op1)),

    {ok, _Scope3} = catena_handler_stack:pop(),
    ?assertEqual({ok, Handler2}, catena_handler_stack:lookup(op2)),
    ?assertEqual({ok, Handler1}, catena_handler_stack:lookup(op1)),

    {ok, _Scope2} = catena_handler_stack:pop(),
    ?assertEqual({ok, Handler1}, catena_handler_stack:lookup(op1)),
    ?assertEqual({error, not_found}, catena_handler_stack:lookup(op2)),
    cleanup_stack().

stack_scope_isolation_test() ->
    setup_stack(),
    Handler = catena_handler:new(op, fun(V, R) -> {V, R} end),

    catena_handler_stack:register_in_current(op, Handler),
    catena_handler_stack:push(),
    catena_handler_stack:unregister_in_current(op),
    ?assertEqual({error, not_found}, catena_handler_stack:lookup_in_current(op)),

    {ok, _} = catena_handler_stack:pop(),
    ?assertEqual({ok, Handler}, catena_handler_stack:lookup(op)),
    cleanup_stack().

stack_deep_nesting_test() ->
    setup_stack(),
    Handlers = [catena_handler:new(op, fun(V, R) -> {hN, V, R} end) || _N <- lists:seq(1, 10)],

    lists:foreach(fun(H) -> catena_handler_stack:push() end, lists:seq(1, 10)),
    ?assertEqual(11, catena_handler_stack:depth()),

    lists:foldl(fun(H, N) ->
        catena_handler_stack:register_in_current(op, H),
        ?assertEqual({ok, H}, catena_handler_stack:lookup(op)),
        N + 1
    end, 1, Handlers),

    lists:foreach(fun(_) -> {ok, _} = catena_handler_stack:pop() end, lists:seq(1, 10)),
    ?assertEqual(1, catena_handler_stack:depth()),
    cleanup_stack().

stack_lookup_all_empty_test() ->
    setup_stack(),
    All = catena_handler_stack:lookup_all(nonexistent),
    ?assertEqual([], All),
    cleanup_stack().
