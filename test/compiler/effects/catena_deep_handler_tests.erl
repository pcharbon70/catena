%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_deep_handler (Phase 9.3)
%%%
%%% Tests for deep handler implementation:
%%% - Deep handler execution
%%% - Deep handler scoping
%%% - Deep handler traversal
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_deep_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Deep Handler Execution Tests
%%%=============================================================================

scope_effects_deep_test() ->
    % Test basic deep scoping
    Context = #{handlers => [], stack => [], cache => #{}},
    Result = catena_deep_handler:scope_effects_deep(Context, fun() -> 42 end),
    ?assertEqual(42, Result).

scope_effects_deep_restores_previous_context_test() ->
    put(deep_context, #{handlers => [], stack => [], cache => #{}, serial => 1, trace => []}),
    Context = #{handlers => [], stack => [], cache => #{}, serial => 0, trace => []},
    ok = catena_deep_handler:scope_effects_deep(Context, fun() ->
        ?assert(is_map(get(deep_context))),
        ok
    end),
    Restored = get(deep_context),
    ?assertEqual(1, maps:get(serial, Restored)),
    erase(deep_context).

with_deep_handler_test() ->
    % Test with_deep_handler sets up context correctly
    HandlerFun = fun(get, []) -> {ok, test_state} end,
    Result = catena_deep_handler:with_deep_handler(test_effect, HandlerFun, fun() ->
        Context = get(deep_context),
        ?assert(maps:is_key(handlers, Context)),
        ?assertMatch({value, _}, catena_deep_handler:cached_handler_lookup(test_effect, Context)),
        context_ok
    end),
    ?assertEqual(context_ok, Result).

execute_deep_handles_test() ->
    % Test that operations are handled at any depth
    HandlerFun = fun(_Op, _Args) -> handled end,
    Handler = #{effect => test_effect, handler => HandlerFun, scope => make_ref(), depth => 0},
    Context = #{handlers => [Handler], stack => [], cache => #{}},
    % Deep handlers handle regardless of operation depth
    Result = catena_deep_handler:execute_deep({test_effect, get, []}, Context, 5),
    ?assertEqual({handled, handled}, Result).

execute_deep_no_matching_handler_test() ->
    % Test that operations without matching handler are not handled
    HandlerFun = fun(_Op, _Args) -> handled end,
    Handler = #{effect => other_effect, handler => HandlerFun, scope => make_ref(), depth => 0},
    Context = #{handlers => [Handler], stack => [], cache => #{}},
    Result = catena_deep_handler:execute_deep({test_effect, get, []}, Context, 0),
    ?assertEqual({unhandled, {test_effect, get, []}}, Result).

execute_deep_handler_throws_test() ->
    % Test that handler errors result in unhandled operation
    HandlerFun = fun(_Op, _Args) -> error(intentional) end,
    Handler = #{effect => test_effect, handler => HandlerFun, scope => make_ref(), depth => 0},
    Context = #{handlers => [Handler], stack => [], cache => #{}},
    Result = catena_deep_handler:execute_deep({test_effect, get, []}, Context, 0),
    ?assertEqual({unhandled, {test_effect, get, []}}, Result).

execute_deep_respects_handler_depth_test() ->
    HandlerFun = fun(_Op, _Args) -> handled end,
    Handler = #{effect => test_effect, handler => HandlerFun, scope => make_ref(), depth => 2, serial => 1},
    Context = #{handlers => [Handler], stack => [], cache => #{test_effect => Handler}, serial => 1, trace => []},
    Result = catena_deep_handler:execute_deep({test_effect, get, []}, Context, 1),
    ?assertEqual({unhandled, {test_effect, get, []}}, Result).

%%%=============================================================================
%%% Deep Handler Scoping Tests
%%%=============================================================================

deep_scope_boundary_test() ->
    % Test that deep scope boundaries are transparent
    Boundary = catena_deep_handler:deep_scope_boundary(fun() -> isolated end),
    ?assert(is_function(Boundary)),
    Result = Boundary(),
    ?assertEqual(isolated, Result).

deep_scope_boundary_preserves_handlers_test() ->
    HandlerFun = fun(_Op, _Args) -> handled end,
    Result = catena_deep_handler:with_deep_handler(test, HandlerFun, fun() ->
        Boundary = catena_deep_handler:deep_scope_boundary(fun() ->
            Context = get(deep_context),
            length(maps:get(handlers, Context, []))
        end),
        Boundary()
    end),
    ?assertEqual(1, Result).

current_deep_depth_zero_test() ->
    % Test initial depth is 0
    ?assertEqual(0, catena_deep_handler:current_deep_depth()).

nested_current_deep_depth_test() ->
    HandlerFun = fun(_Op, _Args) -> ok end,
    Result = catena_deep_handler:with_deep_handler(outer, HandlerFun, fun() ->
        catena_deep_handler:with_deep_handler(inner, HandlerFun, fun() ->
            catena_deep_handler:current_deep_depth()
        end)
    end),
    ?assertEqual(2, Result).

is_in_deep_scope_true_test() ->
    % Test detection when in deep scope
    Handler = #{effect => test, handler => fun(_Op, _Args) -> ok end, scope => make_ref(), depth => 0},
    Context = #{handlers => [Handler], stack => [], cache => #{}},
    ?assert(catena_deep_handler:is_in_deep_scope(Context)).

is_in_deep_scope_false_test() ->
    % Test detection when not in deep scope
    Context = #{handlers => [], stack => [], cache => #{}},
    ?assertNot(catena_deep_handler:is_in_deep_scope(Context)).

is_in_deep_scope_empty_test() ->
    % Test detection with empty context
    Context = #{},
    ?assertNot(catena_deep_handler:is_in_deep_scope(Context)).

handler_stack_depth_zero_test() ->
    % Test initial stack depth is 0
    ?assertEqual(0, catena_deep_handler:handler_stack_depth()).

%%%=============================================================================
%%% Deep Handler Traversal Tests
%%%=============================================================================

find_handler_exists_test() ->
    % Test finding an existing handler
    HandlerFun = fun(_Op, _Args) -> ok end,
    Handler = #{effect => test, handler => HandlerFun, scope => make_ref(), depth => 0},
    Handlers = [Handler],
    Result = catena_deep_handler:find_handler(test, Handlers),
    ?assertMatch({value, _}, Result).

find_handler_not_exists_test() ->
    % Test finding a non-existent handler
    HandlerFun = fun(_Op, _Args) -> ok end,
    Handler = #{effect => other, handler => HandlerFun, scope => make_ref(), depth => 0},
    Handlers = [Handler],
    Result = catena_deep_handler:find_handler(test, Handlers),
    ?assertEqual(false, Result).

traverse_handlers_first_test() ->
    % Test traversal finds first matching handler
    H1 = #{effect => test, handler => fun(_Op, _Args) -> h1 end, scope => make_ref(), depth => 0},
    H2 = #{effect => test, handler => fun(_Op, _Args) -> h2 end, scope => make_ref(), depth => 0},
    Handlers = [H1, H2],
    {value, Handler} = catena_deep_handler:traverse_handlers(test, Handlers),
    ?assertEqual(h1, apply(maps:get(handler, Handler), [op, []])).

traverse_handlers_order_test() ->
    % Test traversal respects handler order (most recent first)
    H1 = #{effect => test, handler => fun(_Op, _Args) -> h1 end, scope => ref1, depth => 0},
    H2 = #{effect => test, handler => fun(_Op, _Args) -> h2 end, scope => ref2, depth => 1},
    Handlers = [H1, H2],
    {value, Handler} = catena_deep_handler:traverse_handlers(test, Handlers),
    ?assertEqual(h1, apply(maps:get(handler, Handler), [op, []])).

cached_handler_lookup_hit_test() ->
    % Test cached lookup returns handler
    Handler = #{effect => test, handler => fun(_Op, _Args) -> ok end, scope => make_ref(), depth => 0, serial => 1},
    Cache = #{test => Handler},
    Context = #{handlers => [Handler], stack => [], cache => Cache, serial => 1, trace => []},
    Result = catena_deep_handler:cached_handler_lookup(test, Context),
    ?assertMatch({value, _}, Result).

cached_handler_lookup_miss_test() ->
    % Test cached lookup falls back to traversal
    Handler = #{effect => test, handler => fun(_Op, _Args) -> ok end, scope => make_ref(), depth => 0, serial => 1},
    Context = #{handlers => [Handler], stack => [], cache => #{}, serial => 1, trace => []},
    Result = catena_deep_handler:cached_handler_lookup(test, Context),
    ?assertMatch({value, _}, Result).

cached_handler_lookup_stale_test() ->
    % Test cached lookup with stale cache entry
    CachedHandler = #{effect => test, handler => fun(_Op, _Args) -> stale end, scope => old_ref, depth => 0, serial => 1},
    CurrentHandler = #{effect => test, handler => fun(_Op, _Args) -> current end, scope => new_ref, depth => 0, serial => 2},
    Cache = #{test => CachedHandler},
    Context = #{handlers => [CurrentHandler], stack => [], cache => Cache, serial => 2, trace => []},
    {value, Handler} = catena_deep_handler:cached_handler_lookup(test, Context),
    % Should fall back to traversal and return current handler
    ?assertEqual(current, apply(maps:get(handler, Handler), [op, []])).

%%%=============================================================================
%%% Comparison with Shallow Handlers Tests
%%%=============================================================================

deep_vs_shallow_depth_handling_test() ->
    % Demonstrate deep handlers handle at any depth, shallow doesn't
    HandlerFun = fun(_Op, _Args) -> handled end,
    % Deep handler handles at depth 5
    DeepHandler = #{effect => test, handler => HandlerFun, scope => make_ref(), depth => 0},
    DeepContext = #{handlers => [DeepHandler], stack => [], cache => #{}},
    DeepResult = catena_deep_handler:execute_deep({test, get, []}, DeepContext, 5),
    ?assertEqual({handled, handled}, DeepResult),
    % Shallow handler doesn't handle at depth 5
    ShallowHandler = #{effect => test, handler => HandlerFun, scope => make_ref()},
    ShallowContext = #{handlers => [ShallowHandler], depth => 0},
    ShallowResult = catena_shallow_handler:execute_shallow({test, get, []}, ShallowContext, 5),
    ?assertEqual({unhandled, {test, get, []}}, ShallowResult).

%%%=============================================================================
%%% Context Management Tests
%%%=============================================================================

context_preservation_test() ->
    % Test that context is preserved after handler execution
    HandlerFun = fun(_Op, _Args) -> ok end,
    _ = catena_deep_handler:with_deep_handler(test, HandlerFun, fun() ->
        % Context should be set during execution
        ?assert(is_map(get(deep_context)))
    end),
    % After execution, context should be restored
    ok.

multiple_handlers_same_effect_test() ->
    % Test that most recent handler takes precedence
    H1 = #{effect => test, handler => fun(_Op, _Args) -> h1 end, scope => ref1, depth => 0},
    H2 = #{effect => test, handler => fun(_Op, _Args) -> h2 end, scope => ref2, depth => 1},
    Context = #{handlers => [H1, H2], stack => [], cache => #{}},
    Result = catena_deep_handler:execute_deep({test, get, []}, Context, 0),
    ?assertEqual({handled, h1}, Result).
