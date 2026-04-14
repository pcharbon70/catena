%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_perform (Phase 7.3)
%%%
%%% Tests for the perform operation and suspension mechanism:
%%% - Perform operation with handler lookup
%%% - Suspension and resumption
%%% - Handler scoping (with_handler, with_handlers)
%%% - Effect context management
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_perform_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% @private Setup handler stack for a test.
setup_stack() ->
    erase(catena_handler_stack),
    erase(catena_perform_context),
    catena_handler_stack:push().

%% @private Cleanup handler stack after a test.
cleanup_stack() ->
    erase(catena_handler_stack),
    erase(catena_perform_context).

%% @private Setup with an empty stack.
setup_empty() ->
    erase(catena_handler_stack),
    erase(catena_perform_context).

%%%=============================================================================
%%% Perform Operation Tests
%%%=============================================================================

perform_with_handler_test() ->
    setup_stack(),
    % Create a handler that returns a result
    Handler = catena_handler:new(test_op, fun(Value, _Resumption) ->
        {handled, Value}
    end),
    catena_handler:register(test_op, Handler),

    % Perform the operation
    Result = catena_perform:perform(test_op, 42),

    % Should get the handler's result
    ?assertEqual({handled, 42}, Result),
    cleanup_stack().

perform_without_handler_test() ->
    setup_stack(),

    % Perform an operation with no registered handler
    Result = catena_perform:perform(unhandled_op, test_value),

    % Should get unhandled error
    ?assertEqual({unhandled, unhandled_op, test_value}, Result),
    cleanup_stack().

perform_with_resumption_test() ->
    setup_stack(),
    % Create a handler that resumes with a modified value
    Handler = catena_handler:new(test_op, fun(_Value, Resumption) ->
        catena_resumption:resume(Resumption, 42 * 2)
    end),
    catena_handler:register(test_op, Handler),

    % Perform the operation
    Result = catena_perform:perform(test_op, ignored),

    % Should get the resumed value
    ?assertEqual({resumed, 84}, Result),
    cleanup_stack().

perform_with_abort_test() ->
    setup_stack(),
    % Create a handler that aborts (doesn't resume)
    Handler = catena_handler:new(test_op, fun(Value, _Resumption) ->
        {aborted, Value}
    end),
    catena_handler:register(test_op, Handler),

    % Perform the operation
    Result = catena_perform:perform(test_op, 42),

    % Should get the abort result
    ?assertEqual({aborted, 42}, Result),
    cleanup_stack().

perform_nested_handlers_test() ->
    setup_stack(),
    % Create outer and inner handlers
    OuterHandler = catena_handler:new(op, fun(Value, _Resumption) ->
        {outer, Value}
    end),
    InnerHandler = catena_handler:new(op, fun(Value, _Resumption) ->
        {inner, Value}
    end),

    % Register outer handler
    catena_handler:register(op, OuterHandler),

    % Perform from outer scope
    Result1 = catena_perform:perform(op, 1),
    ?assertEqual({outer, 1}, Result1),

    % Push inner scope and register inner handler
    catena_handler_stack:push(),
    catena_handler:register(op, InnerHandler),

    % Perform from inner scope (should find inner handler)
    Result2 = catena_perform:perform(op, 2),
    ?assertEqual({inner, 2}, Result2),

    % Pop back to outer scope
    catena_handler_stack:pop(),

    % Should find outer handler again
    Result3 = catena_perform:perform(op, 3),
    ?assertEqual({outer, 3}, Result3),
    cleanup_stack().

perform_with_custom_context_test() ->
    setup_stack(),
    % Create a handler
    Handler = catena_handler:new(test_op, fun(Value, _Resumption) ->
        {handled, Value}
    end),
    catena_handler:register(test_op, Handler),

    % Create a custom context
    CustomContext = #{
        handlers => catena_handler:current_stack(),
        depth => 5,
        metadata => #{test => true}
    },

    % Perform with custom context
    Result = catena_perform:perform_with_context(test_op, 42, CustomContext),

    % Should get the handler's result
    ?assertEqual({handled, 42}, Result),
    cleanup_stack().

%%%=============================================================================
%%% Suspension and Resumption Tests
%%%=============================================================================

suspend_creates_suspension_test() ->
    setup_stack(),

    % Create a suspension
    Suspension = catena_perform:suspend(test_op, 42),

    % Check the suspension structure
    ?assertEqual(test_op, maps:get(operation, Suspension)),
    ?assertEqual(42, maps:get(value, Suspension)),
    ?assertMatch(#{resumption := _}, Suspension),
    ?assertMatch(#{context := _}, Suspension),
    cleanup_stack().

suspend_resumes_with_value_test() ->
    setup_stack(),

    % Create a suspension
    Suspension = catena_perform:suspend(test_op, original),

    % Resume with a new value
    Result = catena_perform:resume(Suspension, new_value),

    % Should get the resumed value
    ?assertEqual({resumed, new_value}, Result),
    cleanup_stack().

abort_returns_abort_result_test() ->
    setup_stack(),

    % Create a suspension
    Suspension = catena_perform:suspend(test_op, 42),

    % Abort the suspension
    Result = catena_perform:abort(Suspension),

    % Should get abort result
    ?assertEqual({aborted, test_op, 42}, Result),
    cleanup_stack().

%%%=============================================================================
%%% Handler Scoping Tests
%%%=============================================================================

with_handler_installs_temporarily_test() ->
    setup_stack(),

    % Create a handler
    Handler = catena_handler:new(test_op, fun(Value, _Resumption) ->
        {handled, Value}
    end),

    % Perform without handler - should be unhandled
    Result1 = catena_perform:perform(test_op, 1),
    ?assertEqual({unhandled, test_op, 1}, Result1),

    % Run computation with handler
    Result2 = catena_perform:with_handler(test_op, Handler, fun() ->
        catena_perform:perform(test_op, 2)
    end),
    ?assertEqual({handled, 2}, Result2),

    % Perform after with_handler - should be unhandled again
    Result3 = catena_perform:perform(test_op, 3),
    ?assertEqual({unhandled, test_op, 3}, Result3),
    cleanup_stack().

with_handler_cleanup_on_error_test() ->
    setup_stack(),

    % Create a handler
    Handler = catena_handler:new(test_op, fun(Value, _Resumption) ->
        {handled, Value}
    end),

    % Run computation that errors
    try
        catena_perform:with_handler(test_op, Handler, fun() ->
            catena_perform:perform(test_op, 1),
            error = should_not_happen
        end),
        ?assert(false) % Should not reach here
    catch
        error:{badmatch, should_not_happen} -> ok
    end,

    % Handler should still be cleaned up despite error
    Result = catena_perform:perform(test_op, 2),
    ?assertEqual({unhandled, test_op, 2}, Result),
    cleanup_stack().

with_handlers_multiple_test() ->
    setup_stack(),

    % Create multiple handlers
    Handler1 = catena_handler:new(op1, fun(Value, _Resumption) -> {h1, Value} end),
    Handler2 = catena_handler:new(op2, fun(Value, _Resumption) -> {h2, Value} end),

    % Run computation with both handlers
    Result = catena_perform:with_handlers(#{op1 => Handler1, op2 => Handler2}, fun() ->
        R1 = catena_perform:perform(op1, 1),
        R2 = catena_perform:perform(op2, 2),
        {R1, R2}
    end),

    ?assertEqual({{h1, 1}, {h2, 2}}, Result),

    % After with_handlers, operations should be unhandled
    ?assertEqual({unhandled, op1, 3}, catena_perform:perform(op1, 3)),
    ?assertEqual({unhandled, op2, 4}, catena_perform:perform(op2, 4)),
    cleanup_stack().

with_handlers_nested_test() ->
    setup_stack(),

    % Create handlers for different scopes
    OuterHandler = catena_handler:new(op, fun(V, _R) -> {outer, V} end),
    InnerHandler = catena_handler:new(op, fun(V, _R) -> {inner, V} end),

    % Run with outer handler
    Result = catena_perform:with_handler(op, OuterHandler, fun() ->
        % Check outer handler is active
        ?assertEqual({outer, 1}, catena_perform:perform(op, 1)),

        % Run with inner handler
        InnerResult = catena_perform:with_handler(op, InnerHandler, fun() ->
            % Inner handler shadows outer
            catena_perform:perform(op, 2)
        end),

        ?assertEqual({inner, 2}, InnerResult),

        % Back to outer handler
        ?assertEqual({outer, 3}, catena_perform:perform(op, 3)),

        InnerResult
    end),

    ?assertEqual({inner, 2}, Result),
    cleanup_stack().

%%%=============================================================================
%%% Effect Context Tests
%%%=============================================================================

context_push_and_pop_test() ->
    setup_empty(),

    % Push a context
    Context1 = #{depth => 1, metadata => #{a => 1}},
    ?assertEqual(ok, catena_perform:push_context(Context1)),

    % Current context should be the one we pushed
    Current1 = catena_perform:current_context(),
    ?assertEqual(1, maps:get(depth, Current1)),
    ?assertEqual(#{a => 1}, maps:get(metadata, Current1)),

    % Push another context
    Context2 = #{depth => 2, metadata => #{b => 2}},
    ?assertEqual(ok, catena_perform:push_context(Context2)),

    % Current context should be the new one
    Current2 = catena_perform:current_context(),
    ?assertEqual(2, maps:get(depth, Current2)),
    ?assertEqual(#{b => 2}, maps:get(metadata, Current2)),

    % Pop context
    {ok, Popped} = catena_perform:pop_context(),
    ?assertEqual(2, maps:get(depth, Popped)),

    % Back to first context
    Current3 = catena_perform:current_context(),
    ?assertEqual(1, maps:get(depth, Current3)),

    cleanup_stack().

context_pop_empty_test() ->
    setup_empty(),

    % Pop from empty stack
    ?assertEqual({error, empty}, catena_perform:pop_context()),

    cleanup_stack().

context_default_when_empty_test() ->
    setup_empty(),

    % Current context should be default when stack is empty
    Default = catena_perform:current_context(),
    ?assertMatch(#{handlers := _, depth := 0, metadata := _}, Default),

    cleanup_stack().

context_isolation_test() ->
    setup_empty(),

    % Context should be isolated between tests
    Context1 = catena_perform:current_context(),
    Context2 = catena_perform:current_context(),

    % Both should be equivalent defaults
    ?assertEqual(maps:get(depth, Context1), maps:get(depth, Context2)),

    cleanup_stack().

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

perform_with_multi_shot_handler_test() ->
    setup_stack(),

    % Create a multi-shot handler that resumes multiple times
    Handler = catena_handler:new(op, fun(_Value, Resumption) ->
        % Resume the computation multiple times
        R1 = catena_resumption:resume(Resumption, 1),
        R2 = catena_resumption:resume(Resumption, 2),
        R3 = catena_resumption:resume(Resumption, 3),
        {multi_shot, [R1, R2, R3]}
    end),

    catena_handler:register(op, Handler),

    Result = catena_perform:perform(op, ignored),
    ?assertMatch({multi_shot, [{resumed, 1}, {resumed, 2}, {resumed, 3}]}, Result),

    cleanup_stack().

perform_state_handler_test() ->
    setup_stack(),

    % Create a state-like handler
    Handler = catena_handler:new(state, fun(Value, Resumption) ->
        case Value of
            get ->
                catena_resumption:resume(Resumption, current_state);
            put ->
                catena_resumption:resume(Resumption, {ok, new_state})
        end
    end),

    catena_handler:register(state, Handler),

    % Get state
    ?assertEqual({resumed, current_state}, catena_perform:perform(state, get)),

    % Put state
    ?assertEqual({resumed, {ok, new_state}}, catena_perform:perform(state, put)),

    cleanup_stack().

handler_performing_operations_test() ->
    setup_stack(),

    % Create a handler that performs another operation
    Handler2 = catena_handler:new(op2, fun(Value, _Resumption) -> {handled_by_op2, Value} end),
    catena_handler:register(op2, Handler2),

    Handler1 = catena_handler:new(op1, fun(_Value, _Resumption) ->
        % Handler performs op2
        catena_perform:perform(op2, from_handler)
    end),
    catena_handler:register(op1, Handler1),

    % Perform op1, which will perform op2
    Result = catena_perform:perform(op1, test),
    ?assertEqual({handled_by_op2, from_handler}, Result),

    cleanup_stack().
