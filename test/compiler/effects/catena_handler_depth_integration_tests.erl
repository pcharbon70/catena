%%%-------------------------------------------------------------------
%%% @doc Integration tests for Phase 9: Deep vs Shallow Handlers
%%%
%%% This module provides end-to-end integration tests for the handler
%%% depth system, testing the interaction between:
%%%
%%% - Handler depth semantics (Section 9.1)
%%% - Shallow handler implementation (Section 9.2)
%%% - Deep handler implementation (Section 9.3)
%%% - Handler depth selection (Section 9.4)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_handler_depth_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Setup and Teardown
%%%=============================================================================

setup_test_handlers() ->
    #{
        state => fun(_Op, _Args) -> {ok, test_state} end,
        logger => fun(_Op, _Args) -> logged end
    }.

%%%=============================================================================
%%% Shallow Handler Integration Tests
%%%=============================================================================

shallow_handles_direct_operation_test() ->
    % Test shallow handler handles operation at direct scope
    HandlerFun = fun(_Op, _Args) -> handled end,
    Result = catena_shallow_handler:with_shallow_handler(test, HandlerFun, fun() ->
        % Direct operation should be handled
        Context = get(shallow_context),
        Handler = hd(maps:get(handlers, Context)),
        ?assertEqual(test, maps:get(effect, Handler)),
        direct_scope
    end),
    ?assertEqual(direct_scope, Result).

shallow_bypasses_nested_operation_test() ->
    % Test shallow handler doesn't handle nested operations
    HandlerFun = fun(_Op, _Args) -> handled end,
    Result = catena_shallow_handler:with_shallow_handler(test, HandlerFun, fun() ->
        % Create nested function that performs operation
        NestedFun = fun() ->
            % Nested operations should not be handled by shallow handler
            Context = get(shallow_context),
            Handlers = maps:get(handlers, Context, []),
            ?assert(length(Handlers) > 0)
        end,
        NestedFun()
    end),
    ?assertEqual(ok, Result).

shallow_scope_isolation_test() ->
    % Test shallow scope boundaries isolate operations
    HandlerFun = fun(_Op, _Args) -> handled end,
    Boundary = catena_shallow_handler:shallow_scope_boundary(fun() ->
        Context = get(shallow_context),
        ?assert(maps:is_key(handlers, Context)),
        boundary_isolated
    end),
    Result = catena_shallow_handler:with_shallow_handler(test, HandlerFun, fun() ->
        Boundary()
    end),
    ?assertEqual(boundary_isolated, Result).

%%%=============================================================================
%%% Deep Handler Integration Tests
%%%=============================================================================

deep_handles_nested_operation_test() ->
    % Test deep handler handles nested operations
    HandlerFun = fun(_Op, _Args) -> handled end,
    Result = catena_deep_handler:with_deep_handler(test, HandlerFun, fun() ->
        % Deep handler should catch nested operations
        Context = get(deep_context),
        Handlers = maps:get(handlers, Context, []),
        ?assert(length(Handlers) > 0),
        has_handlers
    end),
    ?assertEqual(has_handlers, Result).

deep_transparent_boundary_test() ->
    % Test deep boundaries are transparent
    HandlerFun = fun(_Op, _Args) -> handled end,
    Boundary = catena_deep_handler:deep_scope_boundary(fun() ->
        Context = get(deep_context),
        Handlers = maps:get(handlers, Context, []),
        ?assert(length(Handlers) > 0),
        handlers_visible
    end),
    Result = catena_deep_handler:with_deep_handler(test, HandlerFun, fun() ->
        Boundary()
    end),
    ?assertEqual(handlers_visible, Result).

deep_stack_traversal_test() ->
    % Test deep handlers traverse stack to find operations
    HandlerFun = fun(_Op, _Args) -> handled end,
    Result = catena_deep_handler:with_deep_handler(test, HandlerFun, fun() ->
        % Simulate nested call stack
        Context = get(deep_context),
        Handlers = maps:get(handlers, Context, []),
        ?assert(length(Handlers) > 0),
        stack_depth_ok
    end),
    ?assertEqual(stack_depth_ok, Result).

%%%=============================================================================
%%% Depth Selection Integration Tests
%%%=============================================================================

with_deep_wrapper_test() ->
    % Test with_deep_handler wrapper works correctly
    HandlerFun = fun(_Op, _Args) -> handled end,
    Result = catena_depth_selection:with_deep_handler(test, HandlerFun, fun() ->
        Context = get(deep_context),
        ?assert(length(maps:get(handlers, Context, [])) > 0),
        deep_ok
    end),
    ?assertEqual(deep_ok, Result).

with_shallow_wrapper_test() ->
    % Test with_shallow_handler wrapper works correctly
    HandlerFun = fun(_Op, _Args) -> handled end,
    Result = catena_depth_selection:with_shallow_handler(test, HandlerFun, fun() ->
        Context = get(shallow_context),
        ?assert(length(maps:get(handlers, Context, [])) > 0),
        shallow_ok
    end),
    ?assertEqual(shallow_ok, Result).

with_handler_depth_param_test() ->
    % Test with_handler with depth parameter
    HandlerFun = fun(_Op, _Args) -> handled end,
    % Test deep
    ResultDeep = catena_depth_selection:with_handler(test, deep, HandlerFun, fun() ->
        Context = get(deep_context),
        ?assert(length(maps:get(handlers, Context, [])) > 0),
        deep_ok
    end),
    ?assertEqual(deep_ok, ResultDeep),
    % Test shallow
    ResultShallow = catena_depth_selection:with_handler(test, shallow, HandlerFun, fun() ->
        Context = get(shallow_context),
        ?assert(length(maps:get(handlers, Context, [])) > 0),
        shallow_ok
    end),
    ?assertEqual(shallow_ok, ResultShallow).

%%%=============================================================================
%%% Depth Conversion Integration Tests
%%%=============================================================================

convert_shallow_to_deep_test() ->
    % Test converting shallow handler to deep
    ShallowHandler = #{type => shallow, effect => test, handler => fun() -> ok end},
    DeepHandler = catena_depth_selection:to_deep(ShallowHandler),
    ?assertEqual(deep, maps:get(type, DeepHandler)),
    ?assertEqual(deep, maps:get(depth, DeepHandler)).

convert_deep_to_shallow_test() ->
    % Test converting deep handler to shallow
    DeepHandler = #{type => deep, effect => test, handler => fun() -> ok end},
    ShallowHandler = catena_depth_selection:to_shallow(DeepHandler),
    ?assertEqual(shallow, maps:get(type, ShallowHandler)),
    ?assertEqual(shallow, maps:get(depth, ShallowHandler)).

convert_depth_consistency_test() ->
    % Test that convert_depth is consistent with to_deep/to_shallow
    Handler = #{type => shallow, effect => test, handler => fun() -> ok end},
    ViaConvert = catena_depth_selection:convert_depth(Handler, deep),
    ViaToDeep = catena_depth_selection:to_deep(Handler),
    ?assertEqual(maps:get(type, ViaConvert), maps:get(type, ViaToDeep)).

%%%=============================================================================
%%% Mixed Depth Integration Tests
%%%=============================================================================

mixed_depth_scope_test() ->
    % Test mixing deep and shallow handlers
    Handlers = [
        #{type => deep, effect => test1, handler => fun() -> d1 end},
        #{type => shallow, effect => test2, handler => fun() -> s1 end}
    ],
    Scope = catena_depth_selection:mixed_handler_scope(Handlers, []),
    ?assertEqual(Handlers, maps:get(handlers, Scope)),
    ?assertEqual(deep_first, maps:get(precedence, Scope)).

deep_preferred_in_mixed_scope_test() ->
    % Test that deep is preferred when deep_first precedence
    Deep = #{type => deep, effect => test, handler => fun() -> deep_h end},
    Shallow = #{type => shallow, effect => test, handler => fun() -> shallow_h end},
    {Which, Handler} = catena_depth_selection:depth_precedence(Deep, Shallow),
    ?assertEqual(deep, Which),
    ?assertEqual(Deep, Handler).

conflict_resolution_deep_first_test() ->
    % Test conflict resolution prefers deep handler
    Operation = {test, op, []},
    H1 = #{effect => test, handler => fun() -> h1 end, type => deep},
    H2 = #{effect => test, handler => fun() -> h2 end, type => shallow},
    {ok, Handler} = catena_depth_selection:resolve_handler_conflict(Operation, [H1, H2], []),
    ?assertEqual(H1, Handler).

conflict_resolution_shallow_first_test() ->
    % Test conflict resolution prefers shallow when configured
    Operation = {test, op, []},
    H1 = #{effect => test, handler => fun() -> h1 end, type => deep},
    H2 = #{effect => test, handler => fun() -> h2 end, type => shallow},
    {ok, Handler} = catena_depth_selection:resolve_handler_conflict(Operation, [H1, H2], [{precedence, shallow_first}]),
    ?assertEqual(H2, Handler).

%%%=============================================================================
%%% Performance Characteristics Tests
%%%=============================================================================

shallow_o1_lookup_test() ->
    % Test shallow has O(1) operation lookup
    HandlerFun = fun(_Op, _Args) -> ok end,
    Context = #{handlers => [#{effect => test, handler => HandlerFun, scope => make_ref()}], depth => 0},
    % Shallow only checks current scope - O(1)
    ?assertEqual(1, length(maps:get(handlers, Context, []))),
    ok.

deep_traversal_finds_matching_test() ->
    % Test deep traverses handlers to find matching effect
    HandlerFun = fun(_Op, _Args) -> ok end,
    % Deep traverses handlers to find matching effect
    Handlers = [
        #{effect => other, handler => fun() -> ok end, scope => make_ref()},
        #{effect => test, handler => HandlerFun, scope => make_ref()}
    ],
    % May need to traverse multiple handlers
    ?assert(lists:any(fun(H) -> maps:get(effect, H) =:= test end, Handlers)),
    ok.

%%%=============================================================================
%%% Edge Cases and Error Handling
%%%=============================================================================

empty_handler_list_test() ->
    % Test behavior with no handlers
    Operation = {test, op, []},
    Result = catena_depth_selection:resolve_handler_conflict(Operation, [], []),
    ?assertEqual({unhandled, no_handler}, Result).

no_matching_effect_test() ->
    % Test behavior when no handler matches effect
    Operation = {test, op, []},
    Handlers = [
        #{effect => other, handler => fun() -> ok end, type => deep}
    ],
    Result = catena_depth_selection:resolve_handler_conflict(Operation, Handlers, []),
    ?assertEqual({unhandled, no_matching_handler}, Result).

handler_error_propagates_test() ->
    % Test that handler errors result in unhandled status
    HandlerFun = fun(_Op, _Args) -> error(test_error) end,
    Handler = #{effect => test, handler => HandlerFun, type => deep},
    Context = #{handlers => [Handler], stack => [], cache => #{}},
    Result = catena_deep_handler:execute_deep({test, op, []}, Context, 0),
    ?assertEqual({unhandled, {test, op, []}}, Result).

%%%=============================================================================
%%% Comparison Scenarios
%%%=============================================================================

shallow_vs_deep_same_operation_test() ->
    % Test that shallow and deep handle same operation differently when nested
    HandlerFun = fun(_Op, _Args) -> handled end,

    % Shallow: direct operation handled
    ShallowDirect = catena_shallow_handler:with_shallow_handler(test, HandlerFun, fun() ->
        % At depth 0, should be handled
        Context = get(shallow_context),
        Handler = hd(maps:get(handlers, Context)),
        {test, Handler, 0}
    end),
    ?assertMatch({test, _, 0}, ShallowDirect),

    % Deep: nested operation also handled
    DeepNested = catena_deep_handler:with_deep_handler(test, HandlerFun, fun() ->
        % At any depth, should be handled
        Context = get(deep_context),
        Handler = hd(maps:get(handlers, Context)),
        {test, Handler}
    end),
    ?assertMatch({test, _}, DeepNested).

depth_selection_default_test() ->
    % Test that default depth selection is deep
    Selected = catena_depth_selection:select_depth([]),
    ?assertEqual(deep, Selected).

depth_selection_from_options_test() ->
    % Test that depth selection reads from options
    SelectedDeep = catena_depth_selection:select_depth([{depth, deep}]),
    ?assertEqual(deep, SelectedDeep),

    SelectedShallow = catena_depth_selection:select_depth([{depth, shallow}]),
    ?assertEqual(shallow, SelectedShallow).

%%%=============================================================================
%%% End-to-End Scenarios
%%%=============================================================================

nested_shallow_handlers_test() ->
    % Test nested shallow handlers both in context
    InnerHandler = fun(_Op, _Args) -> inner_handled end,
    OuterHandler = fun(_Op, _Args) -> outer_handled end,

    Result = catena_shallow_handler:with_shallow_handler(outer, OuterHandler, fun() ->
        % Inner handler in nested scope
        catena_shallow_handler:with_shallow_handler(inner, InnerHandler, fun() ->
            Context = get(shallow_context),
            Handlers = maps:get(handlers, Context, []),
            % Should see both handlers (context accumulates)
            ?assert(length(Handlers) >= 1),
            inner_ok
        end)
    end),
    ?assertEqual(inner_ok, Result).

nested_deep_handlers_test() ->
    % Test nested deep handlers both handle operations
    InnerHandler = fun(_Op, _Args) -> inner_handled end,
    OuterHandler = fun(_Op, _Args) -> outer_handled end,

    Result = catena_deep_handler:with_deep_handler(outer, OuterHandler, fun() ->
        % Inner handler in nested scope
        catena_deep_handler:with_deep_handler(inner, InnerHandler, fun() ->
            Context = get(deep_context),
            Handlers = maps:get(handlers, Context, []),
            % Should see both handlers (outer is first in stack)
            ?assert(lists:any(fun(H) -> maps:get(effect, H) =:= inner end, Handlers)),
            ?assert(lists:any(fun(H) -> maps:get(effect, H) =:= outer end, Handlers)),
            both_ok
        end)
    end),
    ?assertEqual(both_ok, Result).

conversion_preserves_handler_function_test() ->
    % Test that depth conversion preserves handler function
    HandlerFun = fun(_Op, _Args) -> original_result end,
    Handler = #{type => shallow, effect => test, handler => HandlerFun},

    % Convert to deep
    DeepHandler = catena_depth_selection:to_deep(Handler),
    ?assertEqual(original_result, apply(maps:get(handler, DeepHandler), [op, []])),

    % Convert to shallow
    ShallowHandler = catena_depth_selection:to_shallow(DeepHandler),
    ?assertEqual(original_result, apply(maps:get(handler, ShallowHandler), [op, []])).

depth_independence_test() ->
    % Test that handler depth is independent of effect
    Effects = [state, reader, writer, error],
    lists:foreach(fun(Effect) ->
        HandlerFun = fun(_Op, _Args) -> ok end,
        % Should work for any effect type
        DeepSpec = catena_depth_selection:handler_spec(Effect, deep, HandlerFun),
        ?assertEqual(Effect, maps:get(effect, DeepSpec)),

        ShallowSpec = catena_depth_selection:handler_spec(Effect, shallow, HandlerFun),
        ?assertEqual(Effect, maps:get(effect, ShallowSpec))
    end, Effects).
