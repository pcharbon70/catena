%%%-------------------------------------------------------------------
%%% @doc Tests for Effect Runtime System (Task 1.3.5)
%%%
%%% Tests process-based effect handlers with explicit context passing.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_runtime_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Context Creation Tests
%%====================================================================

context_creation_test_() ->
    [
        ?_test(test_empty_context()),
        ?_test(test_new_context()),
        ?_test(test_context_structure())
    ].

test_empty_context() ->
    Ctx = catena_effect_runtime:empty_context(),
    ?assertEqual(#{}, maps:get(handlers, Ctx)),
    ?assertEqual(undefined, maps:get(parent, Ctx)).

test_new_context() ->
    Ctx = catena_effect_runtime:new_context(),
    ?assertEqual(#{}, maps:get(handlers, Ctx)),
    ?assertEqual(undefined, maps:get(parent, Ctx)).

test_context_structure() ->
    Ctx = catena_effect_runtime:empty_context(),
    ?assert(is_map(Ctx)),
    ?assert(maps:is_key(handlers, Ctx)),
    ?assert(maps:is_key(parent, Ctx)).

%%====================================================================
%% Handler Spawning Tests (1.3.5.1)
%%====================================================================

handler_spawning_test_() ->
    [
        ?_test(test_with_handlers_creates_child_context()),
        ?_test(test_with_handlers_cleanup()),
        ?_test(test_nested_handlers()),
        ?_test(test_handler_shadowing()),
        ?_test(test_parent_chain_lookup())
    ].

test_with_handlers_creates_child_context() ->
    Ctx = catena_effect_runtime:empty_context(),
    Handler = {'TestEffect', [
        {getValue, fun() -> 42 end}
    ]},
    Result = catena_effect_runtime:with_handlers(Ctx, [Handler], fun(ChildCtx) ->
        %% Child context should have handler
        Handlers = maps:get(handlers, ChildCtx),
        ?assert(maps:is_key('TestEffect', Handlers)),
        %% Child should have parent reference
        ?assertEqual(Ctx, maps:get(parent, ChildCtx)),
        ok
    end),
    ?assertEqual(ok, Result).

test_with_handlers_cleanup() ->
    Ctx = catena_effect_runtime:empty_context(),
    Handler = {'CleanupTest', [
        {ping, fun() -> pong end}
    ]},
    HandlerPid = catena_effect_runtime:with_handlers(Ctx, [Handler], fun(ChildCtx) ->
        Handlers = maps:get(handlers, ChildCtx),
        maps:get('CleanupTest', Handlers)
    end),
    %% Give handler process time to terminate
    timer:sleep(10),
    ?assertNot(is_process_alive(HandlerPid)).

test_nested_handlers() ->
    Ctx = catena_effect_runtime:empty_context(),
    OuterHandler = {'Outer', [{getValue, fun() -> outer end}]},
    InnerHandler = {'Inner', [{getValue, fun() -> inner end}]},

    Result = catena_effect_runtime:with_handlers(Ctx, [OuterHandler], fun(Ctx1) ->
        catena_effect_runtime:with_handlers(Ctx1, [InnerHandler], fun(Ctx2) ->
            %% Both handlers should be accessible
            OuterResult = catena_effect_runtime:perform(Ctx2, 'Outer', getValue, []),
            InnerResult = catena_effect_runtime:perform(Ctx2, 'Inner', getValue, []),
            {OuterResult, InnerResult}
        end)
    end),
    ?assertEqual({outer, inner}, Result).

test_handler_shadowing() ->
    Ctx = catena_effect_runtime:empty_context(),
    OuterHandler = {'ShadowEffect', [{getValue, fun() -> outer_value end}]},
    InnerHandler = {'ShadowEffect', [{getValue, fun() -> inner_value end}]},

    Result = catena_effect_runtime:with_handlers(Ctx, [OuterHandler], fun(Ctx1) ->
        OuterResult = catena_effect_runtime:perform(Ctx1, 'ShadowEffect', getValue, []),

        InnerResult = catena_effect_runtime:with_handlers(Ctx1, [InnerHandler], fun(Ctx2) ->
            %% Inner should shadow outer
            catena_effect_runtime:perform(Ctx2, 'ShadowEffect', getValue, [])
        end),

        %% After inner scope, outer should still work
        AfterResult = catena_effect_runtime:perform(Ctx1, 'ShadowEffect', getValue, []),

        {OuterResult, InnerResult, AfterResult}
    end),
    ?assertEqual({outer_value, inner_value, outer_value}, Result).

test_parent_chain_lookup() ->
    Ctx = catena_effect_runtime:empty_context(),
    Handler1 = {'Effect1', [{op, fun() -> effect1 end}]},
    Handler2 = {'Effect2', [{op, fun() -> effect2 end}]},

    Result = catena_effect_runtime:with_handlers(Ctx, [Handler1], fun(Ctx1) ->
        catena_effect_runtime:with_handlers(Ctx1, [Handler2], fun(Ctx2) ->
            %% Effect1 should be found in parent context
            R1 = catena_effect_runtime:perform(Ctx2, 'Effect1', op, []),
            %% Effect2 should be found in current context
            R2 = catena_effect_runtime:perform(Ctx2, 'Effect2', op, []),
            {R1, R2}
        end)
    end),
    ?assertEqual({effect1, effect2}, Result).

%%====================================================================
%% Perform Operation Tests (1.3.5.2)
%%====================================================================

perform_operation_test_() ->
    [
        ?_test(test_perform_with_handler()),
        ?_test(test_perform_with_args()),
        ?_test(test_perform_multiple_operations()),
        ?_test(test_perform_builtin_fallback()),
        ?_test(test_perform_with_empty_context())
    ].

test_perform_with_handler() ->
    Ctx = catena_effect_runtime:empty_context(),
    Handler = {'Counter', [
        {get, fun() -> 100 end}
    ]},
    Result = catena_effect_runtime:with_handlers(Ctx, [Handler], fun(ChildCtx) ->
        catena_effect_runtime:perform(ChildCtx, 'Counter', get, [])
    end),
    ?assertEqual(100, Result).

test_perform_with_args() ->
    Ctx = catena_effect_runtime:empty_context(),
    Handler = {'Math', [
        {add, fun(A, B) -> A + B end},
        {multiply, fun(A, B) -> A * B end}
    ]},
    Result = catena_effect_runtime:with_handlers(Ctx, [Handler], fun(ChildCtx) ->
        Sum = catena_effect_runtime:perform(ChildCtx, 'Math', add, [5, 3]),
        Product = catena_effect_runtime:perform(ChildCtx, 'Math', multiply, [4, 7]),
        {Sum, Product}
    end),
    ?assertEqual({8, 28}, Result).

test_perform_multiple_operations() ->
    Ctx = catena_effect_runtime:empty_context(),
    Handler = {'State', [
        {init, fun() -> [] end},
        {push, fun(X) -> {pushed, X} end},
        {pop, fun() -> popped end}
    ]},
    Result = catena_effect_runtime:with_handlers(Ctx, [Handler], fun(ChildCtx) ->
        R1 = catena_effect_runtime:perform(ChildCtx, 'State', init, []),
        R2 = catena_effect_runtime:perform(ChildCtx, 'State', push, [42]),
        R3 = catena_effect_runtime:perform(ChildCtx, 'State', pop, []),
        {R1, R2, R3}
    end),
    ?assertEqual({[], {pushed, 42}, popped}, Result).

test_perform_builtin_fallback() ->
    %% With empty context, builtin effects should still work
    Ctx = catena_effect_runtime:empty_context(),
    Result = catena_effect_runtime:perform(Ctx, 'Process', self, []),
    ?assertEqual(self(), Result).

test_perform_with_empty_context() ->
    Ctx = catena_effect_runtime:empty_context(),
    %% Should fall through to builtin IO
    Result = catena_effect_runtime:perform(Ctx, 'IO', print, [<<"test">>]),
    ?assertEqual(ok, Result).

%%====================================================================
%% Message Protocol Tests (1.3.5.3)
%%====================================================================

message_protocol_test_() ->
    [
        ?_test(test_effect_result_message()),
        ?_test(test_effect_error_message()),
        ?_test(test_unknown_operation_error())
    ].

test_effect_result_message() ->
    Ctx = catena_effect_runtime:empty_context(),
    Handler = {'Proto', [
        {echo, fun(X) -> {echoed, X} end}
    ]},
    Result = catena_effect_runtime:with_handlers(Ctx, [Handler], fun(ChildCtx) ->
        catena_effect_runtime:perform(ChildCtx, 'Proto', echo, [hello])
    end),
    ?assertEqual({echoed, hello}, Result).

test_effect_error_message() ->
    Ctx = catena_effect_runtime:empty_context(),
    Handler = {'ErrorTest', [
        {fail, fun() -> erlang:error(intentional_error) end}
    ]},
    ?assertError(
        {effect_error, 'ErrorTest', fail, {error, intentional_error}},
        catena_effect_runtime:with_handlers(Ctx, [Handler], fun(ChildCtx) ->
            catena_effect_runtime:perform(ChildCtx, 'ErrorTest', fail, [])
        end)
    ).

test_unknown_operation_error() ->
    Ctx = catena_effect_runtime:empty_context(),
    Handler = {'Limited', [
        {known, fun() -> ok end}
    ]},
    ?assertError(
        {effect_error, 'Limited', unknown_op, {unknown_operation, unknown_op}},
        catena_effect_runtime:with_handlers(Ctx, [Handler], fun(ChildCtx) ->
            catena_effect_runtime:perform(ChildCtx, 'Limited', unknown_op, [])
        end)
    ).

%%====================================================================
%% Builtin IO Tests (1.3.5.4)
%%====================================================================

builtin_io_test_() ->
    [
        ?_test(test_io_print()),
        ?_test(test_io_println()),
        ?_test(test_io_read_write_file()),
        ?_test(test_io_handler_spec()),
        ?_test(test_io_with_context())
    ].

test_io_print() ->
    Ctx = catena_effect_runtime:empty_context(),
    Result = catena_effect_runtime:perform(Ctx, 'IO', print, [<<"Hello">>]),
    ?assertEqual(ok, Result).

test_io_println() ->
    Ctx = catena_effect_runtime:empty_context(),
    Result = catena_effect_runtime:perform(Ctx, 'IO', println, [<<"World">>]),
    ?assertEqual(ok, Result).

test_io_read_write_file() ->
    Ctx = catena_effect_runtime:empty_context(),
    TempFile = "/tmp/catena_test_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".txt",
    TestContent = <<"Test content for effect runtime">>,

    try
        %% Write
        WriteResult = catena_effect_runtime:perform(Ctx, 'IO', writeFile, [TempFile, TestContent]),
        ?assertEqual(ok, WriteResult),

        %% Read
        ReadResult = catena_effect_runtime:perform(Ctx, 'IO', readFile, [TempFile]),
        ?assertEqual(TestContent, ReadResult)
    after
        file:delete(TempFile)
    end.

test_io_handler_spec() ->
    {EffectName, Operations} = catena_effect_runtime:io_handler(),
    ?assertEqual('IO', EffectName),
    ?assertEqual(5, length(Operations)),
    OpNames = [Name || {Name, _Fun} <- Operations],
    ?assert(lists:member(print, OpNames)),
    ?assert(lists:member(println, OpNames)),
    ?assert(lists:member(readFile, OpNames)),
    ?assert(lists:member(writeFile, OpNames)),
    ?assert(lists:member(getLine, OpNames)).

test_io_with_context() ->
    %% IO should work with custom handler that shadows builtin
    Ctx = catena_effect_runtime:empty_context(),
    MockIO = {'IO', [
        {print, fun(_) -> mock_printed end}
    ]},
    Result = catena_effect_runtime:with_handlers(Ctx, [MockIO], fun(ChildCtx) ->
        catena_effect_runtime:perform(ChildCtx, 'IO', print, [<<"test">>])
    end),
    ?assertEqual(mock_printed, Result).

%%====================================================================
%% Builtin Process Tests
%%====================================================================

builtin_process_test_() ->
    [
        ?_test(test_process_self()),
        ?_test(test_process_spawn()),
        ?_test(test_process_send()),
        ?_test(test_process_handler_spec())
    ].

test_process_self() ->
    Ctx = catena_effect_runtime:empty_context(),
    Result = catena_effect_runtime:perform(Ctx, 'Process', self, []),
    ?assertEqual(self(), Result).

test_process_spawn() ->
    Ctx = catena_effect_runtime:empty_context(),
    Parent = self(),
    Pid = catena_effect_runtime:perform(Ctx, 'Process', spawn, [
        fun() -> Parent ! {spawned, self()} end
    ]),
    ?assert(is_pid(Pid)),
    receive
        {spawned, SpawnedPid} ->
            ?assertEqual(Pid, SpawnedPid)
    after 1000 ->
        ?assert(false)
    end.

test_process_send() ->
    Ctx = catena_effect_runtime:empty_context(),
    Result = catena_effect_runtime:perform(Ctx, 'Process', send, [self(), test_message]),
    ?assertEqual(ok, Result),
    receive
        test_message -> ok
    after 1000 ->
        ?assert(false)
    end.

test_process_handler_spec() ->
    {EffectName, Operations} = catena_effect_runtime:process_handler(),
    ?assertEqual('Process', EffectName),
    ?assertEqual(3, length(Operations)),
    OpNames = [Name || {Name, _Fun} <- Operations],
    ?assert(lists:member(spawn, OpNames)),
    ?assert(lists:member(send, OpNames)),
    ?assert(lists:member(self, OpNames)).

%%====================================================================
%% Cross-Process Context Tests
%%====================================================================

cross_process_test_() ->
    [
        ?_test(test_context_passed_to_spawned_process()),
        ?_test(test_multiple_processes_share_context())
    ].

test_context_passed_to_spawned_process() ->
    Ctx = catena_effect_runtime:empty_context(),
    Handler = {'SharedEffect', [
        {getValue, fun() -> shared_value end}
    ]},
    Parent = self(),

    catena_effect_runtime:with_handlers(Ctx, [Handler], fun(ChildCtx) ->
        %% Spawn process and pass context to it
        spawn(fun() ->
            Result = catena_effect_runtime:perform(ChildCtx, 'SharedEffect', getValue, []),
            Parent ! {child_result, Result}
        end),

        receive
            {child_result, Result} ->
                ?assertEqual(shared_value, Result)
        after 1000 ->
            ?assert(false)
        end
    end).

test_multiple_processes_share_context() ->
    Ctx = catena_effect_runtime:empty_context(),
    Handler = {'Counter', [
        {get, fun() -> 42 end}
    ]},
    Parent = self(),

    catena_effect_runtime:with_handlers(Ctx, [Handler], fun(ChildCtx) ->
        %% Spawn multiple processes with same context
        lists:foreach(fun(N) ->
            spawn(fun() ->
                Result = catena_effect_runtime:perform(ChildCtx, 'Counter', get, []),
                Parent ! {result, N, Result}
            end)
        end, [1, 2, 3]),

        %% Collect results
        Results = lists:map(fun(_) ->
            receive
                {result, N, R} -> {N, R}
            after 1000 ->
                error
            end
        end, [1, 2, 3]),

        %% All should get same value
        ?assertEqual(3, length(Results)),
        lists:foreach(fun({_N, R}) ->
            ?assertEqual(42, R)
        end, Results)
    end).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
        ?_test(test_full_effect_workflow()),
        ?_test(test_multiple_effects()),
        ?_test(test_mock_testing_pattern()),
        ?_test(test_effect_composition())
    ].

test_full_effect_workflow() ->
    Ctx = catena_effect_runtime:empty_context(),
    Logger = {'Logger', [
        {log, fun(Msg) -> {logged, Msg} end}
    ]},
    Result = catena_effect_runtime:with_handlers(Ctx, [Logger], fun(ChildCtx) ->
        catena_effect_runtime:perform(ChildCtx, 'Logger', log, [<<"Test message">>])
    end),
    ?assertEqual({logged, <<"Test message">>}, Result).

test_multiple_effects() ->
    Ctx = catena_effect_runtime:empty_context(),
    Logger = {'Logger', [{log, fun(M) -> {logged, M} end}]},
    Counter = {'Counter', [{inc, fun(N) -> N + 1 end}]},

    Result = catena_effect_runtime:with_handlers(Ctx, [Logger, Counter], fun(ChildCtx) ->
        L = catena_effect_runtime:perform(ChildCtx, 'Logger', log, [test]),
        C = catena_effect_runtime:perform(ChildCtx, 'Counter', inc, [5]),
        {L, C}
    end),
    ?assertEqual({{logged, test}, 6}, Result).

test_mock_testing_pattern() ->
    %% Demonstrate easy mocking with explicit context
    Ctx = catena_effect_runtime:empty_context(),

    %% Production handler
    ProductionIO = {'IO', [
        {readFile, fun(Path) -> {ok, <<"real:", Path/binary>>} end}
    ]},

    %% Mock handler for testing
    MockIO = {'IO', [
        {readFile, fun(_Path) -> {ok, <<"mock data">>} end}
    ]},

    %% Test with mock
    MockResult = catena_effect_runtime:with_handlers(Ctx, [MockIO], fun(MockCtx) ->
        catena_effect_runtime:perform(MockCtx, 'IO', readFile, [<<"test.txt">>])
    end),
    ?assertEqual({ok, <<"mock data">>}, MockResult),

    %% Production with real handler
    ProdResult = catena_effect_runtime:with_handlers(Ctx, [ProductionIO], fun(ProdCtx) ->
        catena_effect_runtime:perform(ProdCtx, 'IO', readFile, [<<"config.json">>])
    end),
    ?assertEqual({ok, <<"real:config.json">>}, ProdResult).

test_effect_composition() ->
    Ctx = catena_effect_runtime:empty_context(),

    %% Chain of effects
    Reader = {'Reader', [{ask, fun() -> config_value end}]},
    Writer = {'Writer', [{tell, fun(V) -> {written, V} end}]},

    Result = catena_effect_runtime:with_handlers(Ctx, [Reader, Writer], fun(ChildCtx) ->
        Value = catena_effect_runtime:perform(ChildCtx, 'Reader', ask, []),
        catena_effect_runtime:perform(ChildCtx, 'Writer', tell, [Value])
    end),
    ?assertEqual({written, config_value}, Result).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    [
        ?_test(test_no_handler_error()),
        ?_test(test_handler_exception()),
        ?_test(test_io_file_not_found()),
        ?_test(test_io_path_traversal()),
        ?_test(test_io_system_path())
    ].

test_no_handler_error() ->
    Ctx = catena_effect_runtime:empty_context(),
    ?assertError(
        {no_handler_for_effect, 'NonExistent', someOp},
        catena_effect_runtime:perform(Ctx, 'NonExistent', someOp, [])
    ).

test_handler_exception() ->
    Ctx = catena_effect_runtime:empty_context(),
    Handler = {'Faulty', [
        {crash, fun() -> throw(intentional_crash) end}
    ]},
    ?assertError(
        {effect_error, 'Faulty', crash, {throw, intentional_crash}},
        catena_effect_runtime:with_handlers(Ctx, [Handler], fun(ChildCtx) ->
            catena_effect_runtime:perform(ChildCtx, 'Faulty', crash, [])
        end)
    ).

test_io_file_not_found() ->
    Ctx = catena_effect_runtime:empty_context(),
    ?assertError(
        {io_error, readFile, enoent},
        catena_effect_runtime:perform(Ctx, 'IO', readFile, ["/tmp/nonexistent_file_12345.txt"])
    ).

test_io_path_traversal() ->
    Ctx = catena_effect_runtime:empty_context(),
    %% Test that path traversal attacks are blocked
    ?assertError(
        {io_error, readFile, {path_security_error, _}},
        catena_effect_runtime:perform(Ctx, 'IO', readFile, ["../../../etc/passwd"])
    ),
    ?assertError(
        {io_error, writeFile, {path_security_error, _}},
        catena_effect_runtime:perform(Ctx, 'IO', writeFile, ["../../../tmp/evil", <<"data">>])
    ).

test_io_system_path() ->
    Ctx = catena_effect_runtime:empty_context(),
    %% Test that system paths are blocked
    ?assertError(
        {io_error, readFile, {path_security_error, _}},
        catena_effect_runtime:perform(Ctx, 'IO', readFile, ["/etc/passwd"])
    ),
    ?assertError(
        {io_error, writeFile, {path_security_error, _}},
        catena_effect_runtime:perform(Ctx, 'IO', writeFile, ["/etc/evil", <<"data">>])
    ).

%%====================================================================
%% Utility Tests
%%====================================================================

utility_test_() ->
    [
        ?_test(test_handler_spec_format()),
        ?_test(test_io_string_conversion())
    ].

test_handler_spec_format() ->
    %% Both io_handler and process_handler should return {Atom, List}
    {IOName, IOOps} = catena_effect_runtime:io_handler(),
    ?assert(is_atom(IOName)),
    ?assert(is_list(IOOps)),

    {ProcName, ProcOps} = catena_effect_runtime:process_handler(),
    ?assert(is_atom(ProcName)),
    ?assert(is_list(ProcOps)).

test_io_string_conversion() ->
    Ctx = catena_effect_runtime:empty_context(),
    %% Test that various types can be printed
    ?assertEqual(ok, catena_effect_runtime:perform(Ctx, 'IO', print, [<<"binary">>])),
    ?assertEqual(ok, catena_effect_runtime:perform(Ctx, 'IO', print, ["list"])),
    ?assertEqual(ok, catena_effect_runtime:perform(Ctx, 'IO', print, [atom])),
    ?assertEqual(ok, catena_effect_runtime:perform(Ctx, 'IO', print, [42])),
    ?assertEqual(ok, catena_effect_runtime:perform(Ctx, 'IO', print, [3.14])).
