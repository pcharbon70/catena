%% @doc Effect System Integration Tests (Phase 2.4.4)
%%
%% These tests validate that the effect system integrates correctly with
%% the REPL and runtime, testing IO effects, Process effects, handler
%% composition, and error handling.
-module(catena_effect_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

%% Create a fresh effect context for testing
setup_context() ->
    catena_effect_runtime:empty_context().

%%====================================================================
%% 2.4.4.1 IO Effect Execution Tests
%%====================================================================

io_print_test() ->
    %% Test that print executes without error
    %% Note: actual output goes to console, we just verify no crash
    Ctx = setup_context(),
    ?assertEqual(ok, catena_effect_runtime:perform(Ctx, 'IO', print, ["test"])).

io_println_test() ->
    Ctx = setup_context(),
    ?assertEqual(ok, catena_effect_runtime:perform(Ctx, 'IO', println, ["test"])).

io_read_write_file_test() ->
    Ctx = setup_context(),
    %% Write a temp file
    TmpFile = "/tmp/catena_effect_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    TestContent = "Hello from Catena effect system!",
    ?assertEqual(ok, catena_effect_runtime:perform(Ctx, 'IO', writeFile, [TmpFile, TestContent])),
    %% Read it back
    ReadContent = catena_effect_runtime:perform(Ctx, 'IO', readFile, [TmpFile]),
    ?assertEqual(list_to_binary(TestContent), ReadContent),
    %% Cleanup
    file:delete(TmpFile).

io_read_nonexistent_file_test() ->
    Ctx = setup_context(),
    %% Reading nonexistent file should error
    ?assertError({io_error, readFile, enoent},
                 catena_effect_runtime:perform(Ctx, 'IO', readFile, ["/tmp/nonexistent_file_12345.txt"])).

io_write_to_restricted_path_test() ->
    Ctx = setup_context(),
    %% Writing to system path should be blocked
    ?assertError({io_error, writeFile, {path_security_error, _}},
                 catena_effect_runtime:perform(Ctx, 'IO', writeFile, ["/etc/passwd", "test"])).

io_path_traversal_blocked_test() ->
    Ctx = setup_context(),
    %% Path traversal should be blocked
    ?assertError({io_error, readFile, {path_security_error, _}},
                 catena_effect_runtime:perform(Ctx, 'IO', readFile, ["/tmp/../etc/passwd"])).

%%====================================================================
%% 2.4.4.2 Effect Type Inspection Tests
%%====================================================================

%% Note: Due to current type inference limitations, direct type inspection
%% of effectful computations in the REPL is limited. These tests verify
%% the effect runtime behavior instead.

io_handler_has_correct_operations_test() ->
    {'IO', Operations} = catena_effect_runtime:io_handler(),
    OpNames = [Name || {Name, _} <- Operations],
    ?assert(lists:member(print, OpNames)),
    ?assert(lists:member(println, OpNames)),
    ?assert(lists:member(readFile, OpNames)),
    ?assert(lists:member(writeFile, OpNames)),
    ?assert(lists:member(getLine, OpNames)).

process_handler_has_correct_operations_test() ->
    {'Process', Operations} = catena_effect_runtime:process_handler(),
    OpNames = [Name || {Name, _} <- Operations],
    ?assert(lists:member(spawn, OpNames)),
    ?assert(lists:member(send, OpNames)),
    ?assert(lists:member(self, OpNames)).

%%====================================================================
%% 2.4.4.3 Effect Error Handling Tests
%%====================================================================

unknown_effect_error_test() ->
    Ctx = setup_context(),
    ?assertError({no_handler_for_effect, 'NonExistent', someOp},
                 catena_effect_runtime:perform(Ctx, 'NonExistent', someOp, [])).

unknown_io_operation_error_test() ->
    Ctx = setup_context(),
    ?assertError({unknown_io_operation, badOp},
                 catena_effect_runtime:perform(Ctx, 'IO', badOp, [])).

unknown_process_operation_error_test() ->
    Ctx = setup_context(),
    ?assertError({unknown_process_operation, badOp},
                 catena_effect_runtime:perform(Ctx, 'Process', badOp, [])).

%%====================================================================
%% 2.4.4.4 Process Effect Tests
%%====================================================================

process_self_test() ->
    Ctx = setup_context(),
    Result = catena_effect_runtime:perform(Ctx, 'Process', self, []),
    ?assertEqual(self(), Result).

process_spawn_test() ->
    Ctx = setup_context(),
    Parent = self(),
    SpawnedPid = catena_effect_runtime:perform(Ctx, 'Process', spawn, [
        fun() ->
            Parent ! {spawned_process, self()}
        end
    ]),
    ?assert(is_pid(SpawnedPid)),
    receive
        {spawned_process, Pid} ->
            ?assertEqual(SpawnedPid, Pid)
    after 1000 ->
        ?assert(false, "Timeout waiting for spawned process")
    end.

process_send_test() ->
    Ctx = setup_context(),
    Self = self(),
    TestMsg = {test_message, 42},
    ?assertEqual(ok, catena_effect_runtime:perform(Ctx, 'Process', send, [Self, TestMsg])),
    receive
        Msg ->
            ?assertEqual(TestMsg, Msg)
    after 1000 ->
        ?assert(false, "Timeout waiting for message")
    end.

process_spawn_send_integration_test() ->
    Ctx = setup_context(),
    Parent = self(),
    %% Spawn a process that sends back a computed result
    SpawnedPid = catena_effect_runtime:perform(Ctx, 'Process', spawn, [
        fun() ->
            %% Perform a computation
            Result = lists:foldl(fun(X, Acc) -> X + Acc end, 0, lists:seq(1, 10)),
            Parent ! {computation_result, Result}
        end
    ]),
    ?assert(is_pid(SpawnedPid)),
    receive
        {computation_result, Result} ->
            ?assertEqual(55, Result)  %% sum of 1..10
    after 1000 ->
        ?assert(false, "Timeout waiting for computation result")
    end.

%%====================================================================
%% Handler Composition Tests
%%====================================================================

with_handlers_basic_test() ->
    Ctx = setup_context(),
    %% Define a custom handler
    CustomHandler = {'Custom', [
        {getValue, fun() -> 42 end},
        {double, fun(X) -> X * 2 end}
    ]},
    Result = catena_effect_runtime:with_handlers(Ctx, [CustomHandler], fun(ChildCtx) ->
        catena_effect_runtime:perform(ChildCtx, 'Custom', getValue, [])
    end),
    ?assertEqual(42, Result).

with_handlers_multiple_effects_test() ->
    Ctx = setup_context(),
    Handler1 = {'Effect1', [{op1, fun() -> 10 end}]},
    Handler2 = {'Effect2', [{op2, fun(X) -> X + 1 end}]},
    Result = catena_effect_runtime:with_handlers(Ctx, [Handler1, Handler2], fun(ChildCtx) ->
        V1 = catena_effect_runtime:perform(ChildCtx, 'Effect1', op1, []),
        catena_effect_runtime:perform(ChildCtx, 'Effect2', op2, [V1])
    end),
    ?assertEqual(11, Result).

with_handlers_nested_test() ->
    Ctx = setup_context(),
    OuterHandler = {'Outer', [{getValue, fun() -> 100 end}]},
    InnerHandler = {'Inner', [{getValue, fun() -> 1 end}]},
    Result = catena_effect_runtime:with_handlers(Ctx, [OuterHandler], fun(OuterCtx) ->
        OuterVal = catena_effect_runtime:perform(OuterCtx, 'Outer', getValue, []),
        catena_effect_runtime:with_handlers(OuterCtx, [InnerHandler], fun(InnerCtx) ->
            InnerVal = catena_effect_runtime:perform(InnerCtx, 'Inner', getValue, []),
            %% Outer handler should still be accessible
            OuterVal2 = catena_effect_runtime:perform(InnerCtx, 'Outer', getValue, []),
            {OuterVal, InnerVal, OuterVal2}
        end)
    end),
    ?assertEqual({100, 1, 100}, Result).

with_handlers_override_test() ->
    Ctx = setup_context(),
    Handler1 = {'Test', [{getValue, fun() -> 1 end}]},
    Handler2 = {'Test', [{getValue, fun() -> 2 end}]},
    Result = catena_effect_runtime:with_handlers(Ctx, [Handler1], fun(Ctx1) ->
        V1 = catena_effect_runtime:perform(Ctx1, 'Test', getValue, []),
        catena_effect_runtime:with_handlers(Ctx1, [Handler2], fun(Ctx2) ->
            V2 = catena_effect_runtime:perform(Ctx2, 'Test', getValue, []),
            {V1, V2}
        end)
    end),
    %% Inner handler overrides outer
    ?assertEqual({1, 2}, Result).

with_handlers_cleanup_test() ->
    Ctx = setup_context(),
    %% Handler that tracks if it was called
    Parent = self(),
    Handler = {'Track', [{mark, fun() -> Parent ! handler_called, ok end}]},
    catena_effect_runtime:with_handlers(Ctx, [Handler], fun(ChildCtx) ->
        catena_effect_runtime:perform(ChildCtx, 'Track', mark, [])
    end),
    receive
        handler_called -> ok
    after 1000 ->
        ?assert(false, "Handler was not called")
    end.

%%====================================================================
%% Effect System with Prelude Integration Tests
%%====================================================================

%% Combine effect operations with prelude functions
effect_with_map_test() ->
    Ctx = setup_context(),
    %% Write multiple files using map
    Files = [
        {"/tmp/catena_test_1.txt", "content1"},
        {"/tmp/catena_test_2.txt", "content2"}
    ],
    %% Write all files
    catena_prelude:map(
        fun({Path, Content}) ->
            catena_effect_runtime:perform(Ctx, 'IO', writeFile, [Path, Content])
        end,
        Files
    ),
    %% Read all files back
    Results = catena_prelude:map(
        fun({Path, _}) ->
            catena_effect_runtime:perform(Ctx, 'IO', readFile, [Path])
        end,
        Files
    ),
    ?assertEqual([<<"content1">>, <<"content2">>], Results),
    %% Cleanup
    lists:foreach(fun({Path, _}) -> file:delete(Path) end, Files).

effect_with_fold_test() ->
    Ctx = setup_context(),
    %% Use fold to accumulate results from effect operations
    CustomHandler = {'Counter', [
        {next, fun(N) -> N + 1 end}
    ]},
    Result = catena_effect_runtime:with_handlers(Ctx, [CustomHandler], fun(ChildCtx) ->
        catena_prelude:fold(
            fun(Acc, _) ->
                catena_effect_runtime:perform(ChildCtx, 'Counter', next, [Acc])
            end,
            0,
            [a, b, c, d, e]
        )
    end),
    ?assertEqual(5, Result).

effect_with_filter_test() ->
    Ctx = setup_context(),
    %% Filter based on effect results
    CustomHandler = {'Validator', [
        {isValid, fun(X) -> X > 5 end}
    ]},
    Result = catena_effect_runtime:with_handlers(Ctx, [CustomHandler], fun(ChildCtx) ->
        catena_prelude:filter(
            fun(X) ->
                catena_effect_runtime:perform(ChildCtx, 'Validator', isValid, [X])
            end,
            [1, 3, 7, 2, 9, 4, 8]
        )
    end),
    ?assertEqual([7, 9, 8], Result).

effect_with_bind_test() ->
    %% Use Maybe bind with effectful operations
    Ctx = setup_context(),
    CustomHandler = {'Database', [
        {lookup, fun(Key) ->
            case Key of
                "exists" -> {some, 42};
                _ -> none
            end
        end}
    ]},
    Result = catena_effect_runtime:with_handlers(Ctx, [CustomHandler], fun(ChildCtx) ->
        catena_prelude:bind(
            catena_effect_runtime:perform(ChildCtx, 'Database', lookup, ["exists"]),
            fun(V) -> {some, V * 2} end
        )
    end),
    ?assertEqual({some, 84}, Result),
    %% Test failure path
    Result2 = catena_effect_runtime:with_handlers(Ctx, [CustomHandler], fun(ChildCtx) ->
        catena_prelude:bind(
            catena_effect_runtime:perform(ChildCtx, 'Database', lookup, ["missing"]),
            fun(V) -> {some, V * 2} end
        )
    end),
    ?assertEqual(none, Result2).

%%====================================================================
%% Stateful Effect Handler Tests
%%====================================================================

stateful_counter_test() ->
    Ctx = setup_context(),
    %% A handler that maintains state via closures
    Counter = fun() ->
        CounterPid = spawn(fun() -> counter_loop(0) end),
        {'StatefulCounter', [
            {get, fun() ->
                CounterPid ! {get, self()},
                receive {count, N} -> N after 1000 -> error(timeout) end
            end},
            {increment, fun() ->
                CounterPid ! {increment, self()},
                receive ok -> ok after 1000 -> error(timeout) end
            end},
            {stop, fun() ->
                CounterPid ! stop,
                ok
            end}
        ]}
    end,
    Handler = Counter(),
    Result = catena_effect_runtime:with_handlers(Ctx, [Handler], fun(ChildCtx) ->
        V1 = catena_effect_runtime:perform(ChildCtx, 'StatefulCounter', get, []),
        catena_effect_runtime:perform(ChildCtx, 'StatefulCounter', increment, []),
        catena_effect_runtime:perform(ChildCtx, 'StatefulCounter', increment, []),
        V2 = catena_effect_runtime:perform(ChildCtx, 'StatefulCounter', get, []),
        catena_effect_runtime:perform(ChildCtx, 'StatefulCounter', stop, []),
        {V1, V2}
    end),
    ?assertEqual({0, 2}, Result).

counter_loop(Count) ->
    receive
        {get, From} ->
            From ! {count, Count},
            counter_loop(Count);
        {increment, From} ->
            From ! ok,
            counter_loop(Count + 1);
        stop ->
            ok
    end.
