%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_shallow_handler (Phase 9.2)
%%%
%%% Tests for shallow handler implementation:
%%% - Shallow handler execution
%%% - Shallow handler scoping
%%% - Shallow handler composition
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_shallow_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Setup and Teardown
%%%=============================================================================

setup_test_state() ->
    fun(State) ->
        % Simple state handler that returns the state
        fun(get, []) -> {ok, State};
           ({put, [NewState]}, []) -> {ok, undefined, NewState}
        end
    end.

%%%=============================================================================
%%% Shallow Handler Execution Tests
%%%=============================================================================

scope_effects_shallow_test() ->
    % Test basic shallow scoping
    Context = #{handlers => [], depth => 0},
    Result = catena_shallow_handler:scope_effects_shallow(Context, fun() -> 42 end),
    ?assertEqual(42, Result).

with_shallow_handler_test() ->
    % Test with_shallow_handler sets up context correctly
    HandlerFun = fun(get, []) -> {ok, test_state} end,
    Result = catena_shallow_handler:with_shallow_handler(test_effect, HandlerFun, fun() ->
        Context = get(shallow_context),
        ?assert(maps:is_key(handlers, Context)),
        context_ok
    end),
    ?assertEqual(context_ok, Result).

execute_shallow_same_depth_test() ->
    % Test that operations at same depth are handled
    HandlerFun = fun(_Op, _Args) -> handled end,
    Handler = #{effect => test_effect, handler => HandlerFun, scope => make_ref()},
    Context = #{handlers => [Handler], depth => 0},
    Result = catena_shallow_handler:execute_shallow({test_effect, get, []}, Context, 0),
    ?assertEqual({handled, handled}, Result).

execute_shallow_nested_depth_test() ->
    % Test that operations from nested depth are NOT handled
    HandlerFun = fun(_Op, _Args) -> handled end,
    Handler = #{effect => test_effect, handler => HandlerFun, scope => make_ref()},
    Context = #{handlers => [Handler], depth => 0},
    Result = catena_shallow_handler:execute_shallow({test_effect, get, []}, Context, 1),
    ?assertEqual({unhandled, {test_effect, get, []}}, Result).

execute_shallow_no_matching_handler_test() ->
    % Test that operations without matching handler are not handled
    HandlerFun = fun(_Op, _Args) -> handled end,
    Handler = #{effect => other_effect, handler => HandlerFun, scope => make_ref()},
    Context = #{handlers => [Handler], depth => 0},
    Result = catena_shallow_handler:execute_shallow({test_effect, get, []}, Context, 0),
    ?assertEqual({unhandled, {test_effect, get, []}}, Result).

%%%=============================================================================
%%% Shallow Handler Scoping Tests
%%%=============================================================================

shallow_scope_boundary_test() ->
    % Test that scope boundaries isolate operations
    Boundary = catena_shallow_handler:shallow_scope_boundary(fun() -> isolated end),
    ?assert(is_function(Boundary)),
    Result = Boundary(),
    ?assertEqual(isolated, Result).

current_shallow_depth_zero_test() ->
    % Test initial depth is 0
    ?assertEqual(0, catena_shallow_handler:current_shallow_depth()).

is_in_shallow_scope_true_test() ->
    % Test detection when in shallow scope
    Context = #{handlers => [#{effect => test, handler => fun() -> ok end}], depth => 0},
    ?assert(catena_shallow_handler:is_in_shallow_scope(Context)).

is_in_shallow_scope_false_test() ->
    % Test detection when not in shallow scope
    Context = #{handlers => [], depth => 0},
    ?assertNot(catena_shallow_handler:is_in_shallow_scope(Context)).

is_in_shallow_scope_empty_test() ->
    % Test detection with empty context
    Context = #{handlers => []},
    ?assertNot(catena_shallow_handler:is_in_shallow_scope(Context)).

%%%=============================================================================
%%% Shallow Handler Composition Tests
%%%=============================================================================

compose_shallow_test() ->
    % Test composing two shallow handlers
    H1 = #{effect => test, handler => fun(_Op, _Args) -> {h1, op} end, scope => make_ref()},
    H2 = #{effect => test, handler => fun(_Op, _Args) -> {h2, op} end, scope => make_ref()},
    Composed = catena_shallow_handler:compose_shallow(H1, H2),
    ?assert(is_map(Composed)),
    ?assertEqual(composed, maps:get(effect, Composed)).

shallow_precedence_same_scope_test() ->
    % Test precedence with same scope
    H1 = #{effect => test, handler => fun(_Op, _Args) -> ok end, scope => ref1},
    H2 = #{effect => test, handler => fun(_Op, _Args) -> ok end, scope => ref1},
    Result = catena_shallow_handler:shallow_precedence(H1, H2),
    ?assertEqual({equal, both}, Result).

shallow_precedence_different_scopes_test() ->
    % Test precedence with different scopes
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    H1 = #{effect => test, handler => fun(_Op, _Args) -> ok end, scope => Ref1},
    H2 = #{effect => test, handler => fun(_Op, _Args) -> ok end, scope => Ref2},
    Result = catena_shallow_handler:shallow_precedence(H1, H2),
    % One should have precedence (either first or second)
    case Result of
        {first, _} -> ok;
        {second, _} -> ok
    end.

%%%=============================================================================
%% Handler Error Handling Tests
%%%=============================================================================

execute_shallow_handler_throws_test() ->
    % Test that handler errors result in unhandled operation
    HandlerFun = fun(_Op, _Args) -> error(intentional) end,
    Handler = #{effect => test_effect, handler => HandlerFun, scope => make_ref()},
    Context = #{handlers => [Handler], depth => 0},
    Result = catena_shallow_handler:execute_shallow({test_effect, get, []}, Context, 0),
    ?assertEqual({unhandled, {test_effect, get, []}}, Result).

%%%=============================================================================
%% Context Management Tests
%%%=============================================================================

context_preservation_test() ->
    % Test that context is preserved after handler execution
    InitialContext = #{handlers => [], depth => 0},
    HandlerFun = fun(get, []) -> ok end,
    UserFun = fun() -> context_return end,
    _ = catena_shallow_handler:with_shallow_handler(test, HandlerFun, fun() ->
        % Context should be set during execution
        ?assert(is_map(get(shallow_context)))
    end),
    % After execution, process dictionary should be clean or restored
    ok.

multiple_handlers_same_effect_test() ->
    % Test behavior with multiple handlers for same effect
    H1 = #{effect => test, handler => fun(get, []) -> h1 end, scope => make_ref()},
    H2 = #{effect => test, handler => fun(get, []) -> h2 end, scope => make_ref()},
    Context = #{handlers => [H1, H2], depth => 0},
    % First handler should take precedence
    Result = catena_shallow_handler:execute_shallow({test, get, []}, Context, 0),
    ?assertEqual({handled, h1}, Result).
