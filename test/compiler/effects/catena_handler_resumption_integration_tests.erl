%%%-------------------------------------------------------------------
%%% @doc Integration tests for Handler/Resumption Core Model (Phase 7.5)
%%%
%%% Comprehensive integration tests for the handler/resumption model:
%%% - Basic handler/resumption interaction
%%% - Nested handler scopes
%%% - Multi-shot resumption handling
%%% - Real-world effect scenarios
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_handler_resumption_integration_tests).
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
%%% Basic Handler/Resumption Tests
%%%=============================================================================

handler_receives_value_and_resumption_test() ->
    setup_stack(),
    ReceivedRef = make_ref(),

    Handler = catena_handler:new(test_op, fun(Value, Resumption) ->
        % Verify handler receives the value
        ?assertEqual(42, Value),

        % Verify handler receives a resumption
        ?assert(catena_resumption:is_resumption(Resumption)),

        % Verify resumption is callable
        ?assert(is_function(catena_resumption:cont_of(Resumption), 1)),

        ReceivedRef
    end),

    catena_handler:register(test_op, Handler),
    Result = catena_perform:perform(test_op, 42),

    ?assertEqual(ReceivedRef, Result),
    cleanup_stack().

resume_returns_to_computation_test() ->
    setup_stack(),

    Handler = catena_handler:new(test_op, fun(_Value, Resumption) ->
        % Resume with a value
        catena_resumption:resume(Resumption, resumed_value)
    end),

    catena_handler:register(test_op, Handler),
    Result = catena_perform:perform(test_op, ignored),

    % Computation receives the resumed value
    ?assertEqual({resumed, resumed_value}, Result),
    cleanup_stack().

abort_terminates_computation_test() ->
    setup_stack(),

    Handler = catena_handler:new(test_op, fun(Value, _Resumption) ->
        % Abort without resuming
        {aborted, Value}
    end),

    catena_handler:register(test_op, Handler),
    Result = catena_perform:perform(test_op, original),

    % Computation receives abort result
    ?assertEqual({aborted, original}, Result),
    cleanup_stack().

handler_return_value_propagates_test() ->
    setup_stack(),

    Handler = catena_handler:new(test_op, fun(_Value, _Resumption) ->
        {handler_result, computed}
    end),

    catena_handler:register(test_op, Handler),
    Result = catena_perform:perform(test_op, ignored),

    % Handler's return value propagates
    ?assertEqual({handler_result, computed}, Result),
    cleanup_stack().

multiple_operations_sequence_test() ->
    setup_stack(),

    Handler = catena_handler:new(counter, fun(Value, _Resumption) ->
        Value + 1
    end),

    catena_handler:register(counter, Handler),

    % Perform multiple operations in sequence
    R1 = catena_perform:perform(counter, 0),
    R2 = catena_perform:perform(counter, 10),
    R3 = catena_perform:perform(counter, 100),

    ?assertEqual(1, R1),
    ?assertEqual(11, R2),
    ?assertEqual(101, R3),
    cleanup_stack().

%%%=============================================================================
%%% Nested Handler Tests
%%%=============================================================================

inner_handler_handles_operation_test() ->
    setup_stack(),

    OuterHandler = catena_handler:new(op, fun(V, _R) -> {outer, V} end),
    InnerHandler = catena_handler:new(op, fun(V, _R) -> {inner, V} end),

    % Register outer handler
    catena_handler:register(op, OuterHandler),

    % Push inner scope and register inner handler
    catena_handler_stack:push(),
    catena_handler:register(op, InnerHandler),

    % Inner handler should handle the operation
    Result = catena_perform:perform(op, value),
    ?assertEqual({inner, value}, Result),
    cleanup_stack().

outer_handles_when_inner_absent_test() ->
    setup_stack(),

    OuterHandler = catena_handler:new(op, fun(V, _R) -> {outer, V} end),
    InnerHandler = catena_handler:new(op, fun(V, _R) -> {inner, V} end),

    % Register outer handler
    catena_handler:register(op, OuterHandler),

    % Push inner scope and register inner handler
    catena_handler_stack:push(),
    catena_handler:register(op, InnerHandler),

    % Inner handler handles
    ?assertEqual({inner, 1}, catena_perform:perform(op, 1)),

    % Unregister inner handler
    catena_handler:unregister(op),

    % Outer handler should now handle
    ?assertEqual({outer, 2}, catena_perform:perform(op, 2)),
    cleanup_stack().

handler_stack_push_pop_behavior_test() ->
    setup_stack(),

    Handler1 = catena_handler:new(op, fun(V, _R) -> {h1, V} end),
    Handler2 = catena_handler:new(op, fun(V, _R) -> {h2, V} end),
    Handler3 = catena_handler:new(op, fun(V, _R) -> {h3, V} end),

    % Register handler in initial scope
    catena_handler:register(op, Handler1),
    ?assertEqual({h1, 1}, catena_perform:perform(op, 1)),

    % Push new scope
    catena_handler_stack:push(),
    catena_handler:register(op, Handler2),
    ?assertEqual({h2, 2}, catena_perform:perform(op, 2)),

    % Push another scope
    catena_handler_stack:push(),
    catena_handler:register(op, Handler3),
    ?assertEqual({h3, 3}, catena_perform:perform(op, 3)),

    % Pop back to scope 2
    {ok, _} = catena_handler_stack:pop(),
    ?assertEqual({h2, 4}, catena_perform:perform(op, 4)),

    % Pop back to scope 1
    {ok, _} = catena_handler_stack:pop(),
    ?assertEqual({h1, 5}, catena_perform:perform(op, 5)),
    cleanup_stack().

handler_shadowing_precedence_test() ->
    setup_stack(),

    Handler1 = catena_handler:new(op, fun(_V, _R) -> h1 end),
    Handler2 = catena_handler:new(op, fun(_V, _R) -> h2 end),
    Handler3 = catena_handler:new(op, fun(_V, _R) -> h3 end),

    % Register handlers in nested scopes
    catena_handler:register(op, Handler1),
    catena_handler_stack:push(),
    catena_handler:register(op, Handler2),
    catena_handler_stack:push(),
    catena_handler:register(op, Handler3),

    % Innermost (h3) should handle
    ?assertEqual(h3, catena_perform:perform(op, test)),

    % Pop and verify h2 handles
    {ok, _} = catena_handler_stack:pop(),
    ?assertEqual(h2, catena_perform:perform(op, test)),

    % Pop and verify h1 handles
    {ok, _} = catena_handler_stack:pop(),
    ?assertEqual(h1, catena_perform:perform(op, test)),
    cleanup_stack().

handler_restoration_after_scope_exit_test() ->
    setup_stack(),

    Handler1 = catena_handler:new(op, fun(V, _R) -> {scope1, V} end),
    Handler2 = catena_handler:new(op, fun(V, _R) -> {scope2, V} end),

    % Register handler in outer scope
    catena_handler:register(op, Handler1),

    % Enter inner scope
    catena_handler_stack:push(),
    catena_handler:register(op, Handler2),

    % Inner handler is active
    ?assertEqual({scope2, 1}, catena_perform:perform(op, 1)),

    % Exit inner scope
    {ok, _} = catena_handler_stack:pop(),

    % Outer handler is restored
    ?assertEqual({scope1, 2}, catena_perform:perform(op, 2)),
    cleanup_stack().

%%%=============================================================================
%%% Multi-Shot Handler Tests
%%%=============================================================================

handler_calls_resumption_twice_test() ->
    setup_stack(),

    Handler = catena_handler:new(op, fun(_Value, Resumption) ->
        % Call resumption twice
        R1 = catena_resumption:resume(Resumption, first),
        R2 = catena_resumption:resume(Resumption, second),
        [R1, R2]
    end),

    catena_handler:register(op, Handler),
    Result = catena_perform:perform(op, ignored),

    ?assertMatch([{resumed, first}, {resumed, second}], Result),
    cleanup_stack().

resumption_state_across_invocations_test() ->
    setup_stack(),

    Handler = catena_handler:new(counter, fun(Start, Resumption) ->
        % Resume multiple times with incrementing values
        R1 = catena_resumption:resume(Resumption, Start),
        R2 = catena_resumption:resume(Resumption, Start + 1),
        R3 = catena_resumption:resume(Resumption, Start + 2),
        [R1, R2, R3]
    end),

    catena_handler:register(counter, Handler),
    Result = catena_perform:perform(counter, 0),

    ?assertMatch([{resumed, 0}, {resumed, 1}, {resumed, 2}], Result),
    cleanup_stack().

multishot_with_state_mutations_test() ->
    setup_stack(),

    Handler = catena_handler:new(state_op, fun(_Value, Resumption) ->
        % Simulate state mutations across multiple resumes
        Get1 = catena_resumption:resume(Resumption, {get, state1}),
        Put1 = catena_resumption:resume(Resumption, {put, state2}),
        Get2 = catena_resumption:resume(Resumption, {get, ignored}),

        [Get1, Put1, Get2]
    end),

    catena_handler:register(state_op, Handler),
    Result = catena_perform:perform(state_op, ignored),

    ?assertMatch(
        [{resumed, {get, state1}}, {resumed, {put, state2}}, {resumed, {get, _}}],
        Result
    ),
    cleanup_stack().

multishot_error_handling_test() ->
    setup_stack(),

    Handler = catena_handler:new(op, fun(_Value, Resumption) ->
        % First resume succeeds
        R1 = catena_resumption:resume(Resumption, ok),
        % Second resume with different value
        R2 = catena_resumption:resume(Resumption, error_value),
        [R1, R2]
    end),

    catena_handler:register(op, Handler),
    Result = catena_perform:perform(op, ignored),

    ?assertMatch([{resumed, ok}, {resumed, error_value}], Result),
    cleanup_stack().

multishot_resource_handling_test() ->
    setup_stack(),

    % Simulate a handler that manages resources across resumes
    Resources = spawn_resource_tracker(),

    Handler = catena_handler:new(op, fun(Count, Resumption) ->
        % Track each resume
        Results = lists:map(fun(N) ->
            track_resource(Resources, N),
            catena_resumption:resume(Resumption, N)
        end, lists:seq(1, Count)),

        % Verify all resources were tracked
        ?assertEqual(Count, get_tracked_count(Resources)),

        Results
    end),

    catena_handler:register(op, Handler),
    Result = catena_perform:perform(op, 5),

    ?assertMatch([{resumed, 1}, {resumed, 2}, {resumed, 3}, {resumed, 4}, {resumed, 5}], Result),
    cleanup_stack().

%%%=============================================================================
%%% Real-World Effect Scenarios
%%%=============================================================================

state_effect_scenario_test() ->
    setup_stack(),

    % Simulate a State effect
    Handler = catena_handler:new(state, fun(Value, Resumption) ->
        case Value of
            {get, State} ->
                catena_resumption:resume(Resumption, {ok, State});
            {put, NewState} ->
                % Return to computation with new state
                catena_resumption:resume(Resumption, {ok, NewState});
            {modify, Fun} ->
                % Modify state and continue
                NewState = Fun(old_state),
                catena_resumption:resume(Resumption, {ok, NewState})
        end
    end),

    catena_handler:register(state, Handler),

    % Use the state effect
    ?assertEqual({resumed, {ok, initial}}, catena_perform:perform(state, {get, initial})),
    ?assertEqual({resumed, {ok, new}}, catena_perform:perform(state, {put, new})),
    ModifyFun = fun(_) -> modified end,

    ?assertEqual({resumed, {ok, modified}}, catena_perform:perform(state, {modify, ModifyFun})),
    cleanup_stack().

reader_effect_scenario_test() ->
    setup_stack(),

    % Simulate a Reader effect
    Environment = #{database => "db", cache => "cache"},

    Handler = catena_handler:new(reader, fun(Value, Resumption) ->
        case Value of
            {ask, Key} ->
                Env = maps:get(Key, Environment, undefined),
                catena_resumption:resume(Resumption, {ok, Env});
            {local, NewEnv} ->
                catena_resumption:resume(Resumption, {ok, NewEnv})
        end
    end),

    catena_handler:register(reader, Handler),

    % Use the reader effect
    ?assertEqual({resumed, {ok, "db"}}, catena_perform:perform(reader, {ask, database})),
    ?assertEqual({resumed, {ok, "cache"}}, catena_perform:perform(reader, {ask, cache})),

    cleanup_stack().

error_handling_scenario_test() ->
    setup_stack(),

    % Simulate an Error effect
    Handler = catena_handler:new(error, fun(Value, Resumption) ->
        case Value of
            {throw, Reason} ->
                % Abort with error
                {error, Reason};
            {try_catch, ErrorFun, HandlerFun} ->
                % Try the computation, handle errors
                case ErrorFun() of
                    {ok, Result} -> catena_resumption:resume(Resumption, Result);
                    {error, Reason} -> HandlerFun(Reason)
                end
        end
    end),

    catena_handler:register(error, Handler),

    % Throw an error
    ?assertEqual({error, test_reason}, catena_perform:perform(error, {throw, test_reason})),

    cleanup_stack().

logging_effect_scenario_test() ->
    setup_stack(),

    % Accumulate log messages in a process dictionary
    LogKey = make_ref(),
    put(LogKey, []),  % Initialize with empty list

    Handler = catena_handler:new(log, fun(Value, Resumption) ->
        % Store log message
        Logs = get(LogKey),
        put(LogKey, [Value | Logs]),
        catena_resumption:resume(Resumption, ok)
    end),

    catena_handler:register(log, Handler),

    % Perform multiple log operations
    ?assertEqual({resumed, ok}, catena_perform:perform(log, "First message")),
    ?assertEqual({resumed, ok}, catena_perform:perform(log, "Second message")),
    ?assertEqual({resumed, ok}, catena_perform:perform(log, "Third message")),

    % Verify logs were accumulated (in reverse order due to prepending)
    Logs = get(LogKey),
    ?assertEqual(3, length(Logs)),
    ?assert(lists:member("Third message", Logs)),
    ?assert(lists:member("Second message", Logs)),
    ?assert(lists:member("First message", Logs)),

    cleanup_stack().

%%%=============================================================================
%%% Complex Scenarios
%%%=============================================================================

handler_performing_operations_test() ->
    setup_stack(),

    % Create handlers for two operations
    Handler2 = catena_handler:new(op2, fun(Value, _Resumption) ->
        {handled_by_op2, Value}
    end),

    Handler1 = catena_handler:new(op1, fun(_Value, _Resumption) ->
        % Handler1 performs op2
        catena_perform:perform(op2, from_handler)
    end),

    catena_handler:register(op2, Handler2),
    catena_handler:register(op1, Handler1),

    % Perform op1, which will perform op2
    Result = catena_perform:perform(op1, test),

    % op2's handler should have handled the operation
    ?assertEqual({handled_by_op2, from_handler}, Result),

    cleanup_stack().

deeply_nested_handlers_test() ->
    setup_stack(),

    % Create 5 levels of nested handlers
    Handlers = [
        catena_handler:new(op, fun(V, _R) -> {level, N, V} end)
        || N <- lists:seq(1, 5)
    ],

    % Register handlers in nested scopes (push first, then register)
    lists:foreach(fun(H) ->
        catena_handler_stack:push(),
        catena_handler:register(op, H)
    end, Handlers),

    % Innermost handler (level 5) should handle
    ?assertEqual({level, 5, test}, catena_perform:perform(op, test)),

    % Pop scopes and verify each level
    lists:foreach(fun(N) ->
        {ok, _} = catena_handler_stack:pop(),
        ExpectedLevel = 5 - N,
        ?assertEqual({level, ExpectedLevel, test}, catena_perform:perform(op, test))
    end, lists:seq(1, 4)),

    cleanup_stack().

%%%=============================================================================
%%% Helper Functions for Tests
%%%=============================================================================

%% @private Spawn a simple resource tracker for testing.
spawn_resource_tracker() ->
    Self = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        resource_loop(Ref, Self, [])
    end),
    {Pid, Ref}.

%% @private Track a resource usage.
track_resource({Pid, Ref}, Value) ->
    Pid ! {track, Value, Ref}.

%% @private Get the count of tracked resources.
get_tracked_count({Pid, Ref}) ->
    Pid ! {get_count, Ref, self()},
    receive
        {count, Count, Ref} -> Count
    after 100 -> timeout
    end.

%% @private Resource tracker loop.
resource_loop(Ref, Parent, Tracked) ->
    receive
        {track, Value, Ref} ->
            resource_loop(Ref, Parent, [Value | Tracked]);
        {get_count, Ref, From} ->
            From ! {count, length(Tracked), Ref},
            resource_loop(Ref, Parent, Tracked);
        stop ->
            ok
    end.
