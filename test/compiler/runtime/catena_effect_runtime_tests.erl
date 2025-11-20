%%%-------------------------------------------------------------------
%%% @doc Tests for Effect Runtime System (Task 1.3.5)
%%%
%%% Tests the process-based effect runtime including handler spawning,
%%% perform operations, message protocol, and builtin IO effects.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_runtime_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Handler Process Spawning Tests (1.3.5.1)
%%====================================================================

handler_spawning_test_() ->
    [
        ?_test(test_spawn_single_handler()),
        ?_test(test_spawn_multiple_handlers()),
        ?_test(test_handler_cleanup()),
        ?_test(test_handler_registration()),
        ?_test(test_nested_handlers())
    ].

test_spawn_single_handler() ->
    %% Create a simple handler that doubles numbers
    Handlers = [
        {'Math', [
            {double, fun(X) -> X * 2 end}
        ]}
    ],
    Result = catena_effect_runtime:with_handlers(Handlers, fun() ->
        catena_effect_runtime:perform('Math', double, [21])
    end),
    ?assertEqual(42, Result).

test_spawn_multiple_handlers() ->
    %% Create multiple handlers
    Handlers = [
        {'Math', [
            {add, fun(X, Y) -> X + Y end}
        ]},
        {'String', [
            {concat, fun(A, B) -> <<A/binary, B/binary>> end}
        ]}
    ],
    Result = catena_effect_runtime:with_handlers(Handlers, fun() ->
        Sum = catena_effect_runtime:perform('Math', add, [10, 20]),
        Str = catena_effect_runtime:perform('String', concat, [<<"hello">>, <<" world">>]),
        {Sum, Str}
    end),
    ?assertEqual({30, <<"hello world">>}, Result).

test_handler_cleanup() ->
    %% Verify handlers are cleaned up after with_handlers returns
    Handlers = [
        {'Test', [
            {noop, fun() -> ok end}
        ]}
    ],
    catena_effect_runtime:with_handlers(Handlers, fun() -> ok end),
    %% Handler should be unregistered
    ?assertEqual(undefined, catena_effect_runtime:get_handler('Test')).

test_handler_registration() ->
    %% Test manual handler registration
    Pid = spawn(fun() ->
        receive stop -> ok end
    end),
    catena_effect_runtime:register_handler('Custom', Pid),
    ?assertEqual(Pid, catena_effect_runtime:get_handler('Custom')),
    catena_effect_runtime:unregister_handler('Custom'),
    ?assertEqual(undefined, catena_effect_runtime:get_handler('Custom')),
    Pid ! stop.

test_nested_handlers() ->
    %% Test nested handler scopes
    OuterHandlers = [
        {'Outer', [
            {get, fun() -> outer end}
        ]}
    ],
    InnerHandlers = [
        {'Inner', [
            {get, fun() -> inner end}
        ]}
    ],
    Result = catena_effect_runtime:with_handlers(OuterHandlers, fun() ->
        OuterVal = catena_effect_runtime:perform('Outer', get, []),
        InnerResult = catena_effect_runtime:with_handlers(InnerHandlers, fun() ->
            InnerVal = catena_effect_runtime:perform('Inner', get, []),
            %% Can still access outer handler
            OuterInner = catena_effect_runtime:perform('Outer', get, []),
            {InnerVal, OuterInner}
        end),
        {OuterVal, InnerResult}
    end),
    ?assertEqual({outer, {inner, outer}}, Result).

%%====================================================================
%% Perform Operation Tests (1.3.5.2)
%%====================================================================

perform_operation_test_() ->
    [
        ?_test(test_perform_simple_operation()),
        ?_test(test_perform_with_multiple_args()),
        ?_test(test_perform_with_no_args()),
        ?_test(test_perform_returns_complex_value()),
        ?_test(test_perform_multiple_operations())
    ].

test_perform_simple_operation() ->
    Handlers = [
        {'Counter', [
            {increment, fun(X) -> X + 1 end}
        ]}
    ],
    Result = catena_effect_runtime:with_handlers(Handlers, fun() ->
        catena_effect_runtime:perform('Counter', increment, [41])
    end),
    ?assertEqual(42, Result).

test_perform_with_multiple_args() ->
    Handlers = [
        {'Math', [
            {sum3, fun(A, B, C) -> A + B + C end}
        ]}
    ],
    Result = catena_effect_runtime:with_handlers(Handlers, fun() ->
        catena_effect_runtime:perform('Math', sum3, [10, 20, 30])
    end),
    ?assertEqual(60, Result).

test_perform_with_no_args() ->
    Handlers = [
        {'Generator', [
            {zero, fun() -> 0 end}
        ]}
    ],
    Result = catena_effect_runtime:with_handlers(Handlers, fun() ->
        catena_effect_runtime:perform('Generator', zero, [])
    end),
    ?assertEqual(0, Result).

test_perform_returns_complex_value() ->
    Handlers = [
        {'Data', [
            {create, fun(Name, Age) -> #{name => Name, age => Age} end}
        ]}
    ],
    Result = catena_effect_runtime:with_handlers(Handlers, fun() ->
        catena_effect_runtime:perform('Data', create, [<<"Alice">>, 30])
    end),
    ?assertEqual(#{name => <<"Alice">>, age => 30}, Result).

test_perform_multiple_operations() ->
    %% Same effect with multiple operations
    Handlers = [
        {'State', [
            {get, fun() -> 42 end},
            {set, fun(_V) -> ok end},
            {modify, fun(F) -> F(42) end}
        ]}
    ],
    Result = catena_effect_runtime:with_handlers(Handlers, fun() ->
        V1 = catena_effect_runtime:perform('State', get, []),
        catena_effect_runtime:perform('State', set, [100]),
        V2 = catena_effect_runtime:perform('State', modify, [fun(X) -> X * 2 end]),
        {V1, V2}
    end),
    ?assertEqual({42, 84}, Result).

%%====================================================================
%% Effect Message Protocol Tests (1.3.5.3)
%%====================================================================

message_protocol_test_() ->
    [
        ?_test(test_message_send_receive()),
        ?_test(test_error_propagation()),
        ?_test(test_unknown_operation_error())
    ].

test_message_send_receive() ->
    %% Test that the message protocol works correctly
    Handlers = [
        {'Echo', [
            {echo, fun(X) -> X end}
        ]}
    ],
    Values = [42, <<"hello">>, [1,2,3], #{a => 1}],
    Results = catena_effect_runtime:with_handlers(Handlers, fun() ->
        [catena_effect_runtime:perform('Echo', echo, [V]) || V <- Values]
    end),
    ?assertEqual(Values, Results).

test_error_propagation() ->
    %% Test that handler errors are propagated
    Handlers = [
        {'Faulty', [
            {crash, fun() -> erlang:error(intentional_crash) end}
        ]}
    ],
    ?assertError(
        {effect_error, 'Faulty', crash, {error, intentional_crash}},
        catena_effect_runtime:with_handlers(Handlers, fun() ->
            catena_effect_runtime:perform('Faulty', crash, [])
        end)
    ).

test_unknown_operation_error() ->
    %% Test error for unknown operation
    Handlers = [
        {'Known', [
            {exists, fun() -> ok end}
        ]}
    ],
    ?assertError(
        {effect_error, 'Known', nonexistent, {unknown_operation, nonexistent}},
        catena_effect_runtime:with_handlers(Handlers, fun() ->
            catena_effect_runtime:perform('Known', nonexistent, [])
        end)
    ).

%%====================================================================
%% Builtin IO Effect Tests (1.3.5.4)
%%====================================================================

builtin_io_test_() ->
    [
        ?_test(test_io_print()),
        ?_test(test_io_println()),
        ?_test(test_io_read_write_file()),
        ?_test(test_io_handler_spec()),
        ?_test(test_io_with_custom_handler())
    ].

test_io_print() ->
    %% Test that print returns ok (we can't easily capture stdout)
    Result = catena_effect_runtime:perform('IO', print, [<<"test">>]),
    ?assertEqual(ok, Result).

test_io_println() ->
    %% Test that println returns ok
    Result = catena_effect_runtime:perform('IO', println, [<<"test line">>]),
    ?assertEqual(ok, Result).

test_io_read_write_file() ->
    %% Test file read/write operations
    TempFile = "/tmp/catena_effect_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    TestContent = <<"Hello, Effects!">>,

    try
        %% Write file
        WriteResult = catena_effect_runtime:perform('IO', writeFile, [TempFile, TestContent]),
        ?assertEqual(ok, WriteResult),

        %% Read file
        ReadResult = catena_effect_runtime:perform('IO', readFile, [TempFile]),
        ?assertEqual(TestContent, ReadResult)
    after
        %% Cleanup
        file:delete(TempFile)
    end.

test_io_handler_spec() ->
    %% Test the handler specification format
    {'IO', Operations} = catena_effect_runtime:io_handler(),
    OpNames = [Op || {Op, _Fun} <- Operations],
    ?assert(lists:member(print, OpNames)),
    ?assert(lists:member(println, OpNames)),
    ?assert(lists:member(readFile, OpNames)),
    ?assert(lists:member(writeFile, OpNames)),
    ?assert(lists:member(getLine, OpNames)).

test_io_with_custom_handler() ->
    %% Test overriding builtin IO with custom handler
    PrintLog = self(),
    CustomIO = [
        {'IO', [
            {print, fun(Text) ->
                PrintLog ! {printed, Text},
                ok
            end}
        ]}
    ],
    catena_effect_runtime:with_handlers(CustomIO, fun() ->
        catena_effect_runtime:perform('IO', print, [<<"custom">>])
    end),
    receive
        {printed, <<"custom">>} -> ok
    after 1000 ->
        ?assert(false)
    end.

%%====================================================================
%% Builtin Process Effect Tests
%%====================================================================

builtin_process_test_() ->
    [
        ?_test(test_process_spawn()),
        ?_test(test_process_send()),
        ?_test(test_process_self()),
        ?_test(test_process_handler_spec())
    ].

test_process_spawn() ->
    %% Test spawning a process
    Parent = self(),
    Pid = catena_effect_runtime:perform('Process', spawn, [fun() ->
        Parent ! {hello, self()}
    end]),
    ?assert(is_pid(Pid)),
    receive
        {hello, Pid} -> ok
    after 1000 ->
        ?assert(false)
    end.

test_process_send() ->
    %% Test sending a message
    Target = self(),
    Result = catena_effect_runtime:perform('Process', send, [Target, {test_msg, 42}]),
    ?assertEqual(ok, Result),
    receive
        {test_msg, 42} -> ok
    after 1000 ->
        ?assert(false)
    end.

test_process_self() ->
    %% Test getting self
    Result = catena_effect_runtime:perform('Process', self, []),
    ?assertEqual(self(), Result).

test_process_handler_spec() ->
    %% Test the handler specification format
    {'Process', Operations} = catena_effect_runtime:process_handler(),
    OpNames = [Op || {Op, _Fun} <- Operations],
    ?assert(lists:member(spawn, OpNames)),
    ?assert(lists:member(send, OpNames)),
    ?assert(lists:member(self, OpNames)).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
        ?_test(test_complex_effect_composition()),
        ?_test(test_effect_with_closures()),
        ?_test(test_sequential_effects()),
        ?_test(test_builtin_effects_in_handler_context())
    ].

test_complex_effect_composition() ->
    %% Test composing multiple effects in a computation
    Handlers = [
        {'State', [
            {get, fun() -> 0 end},
            {put, fun(V) -> V end}  % Just returns the value for simplicity
        ]},
        {'Logger', [
            {log, fun(Msg) -> {logged, Msg} end}
        ]}
    ],
    Result = catena_effect_runtime:with_handlers(Handlers, fun() ->
        V = catena_effect_runtime:perform('State', get, []),
        catena_effect_runtime:perform('Logger', log, [<<"Starting">>]),
        NewV = V + 10,
        catena_effect_runtime:perform('State', put, [NewV]),
        catena_effect_runtime:perform('Logger', log, [<<"Done">>]),
        NewV
    end),
    ?assertEqual(10, Result).

test_effect_with_closures() ->
    %% Test that closures work correctly in effect handlers
    Counter = 42,
    Handlers = [
        {'Closure', [
            {getCounter, fun() -> Counter end},
            {addToCounter, fun(X) -> Counter + X end}
        ]}
    ],
    Result = catena_effect_runtime:with_handlers(Handlers, fun() ->
        V1 = catena_effect_runtime:perform('Closure', getCounter, []),
        V2 = catena_effect_runtime:perform('Closure', addToCounter, [8]),
        {V1, V2}
    end),
    ?assertEqual({42, 50}, Result).

test_sequential_effects() ->
    %% Test sequential effect invocations
    Handlers = [
        {'Sequence', [
            {step, fun(N) -> N end}
        ]}
    ],
    Result = catena_effect_runtime:with_handlers(Handlers, fun() ->
        lists:foldl(fun(N, Acc) ->
            V = catena_effect_runtime:perform('Sequence', step, [N]),
            [V | Acc]
        end, [], [1, 2, 3, 4, 5])
    end),
    ?assertEqual([5, 4, 3, 2, 1], Result).

test_builtin_effects_in_handler_context() ->
    %% Test that builtin effects work alongside custom handlers
    TempFile = "/tmp/catena_builtin_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    CustomHandlers = [
        {'Custom', [
            {transform, fun(X) -> X * 2 end}
        ]}
    ],
    try
        Result = catena_effect_runtime:with_handlers(CustomHandlers, fun() ->
            %% Use custom handler
            V = catena_effect_runtime:perform('Custom', transform, [21]),
            %% Use builtin IO
            catena_effect_runtime:perform('IO', writeFile, [TempFile, integer_to_binary(V)]),
            Content = catena_effect_runtime:perform('IO', readFile, [TempFile]),
            {V, Content}
        end),
        ?assertEqual({42, <<"42">>}, Result)
    after
        file:delete(TempFile)
    end.

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    [
        ?_test(test_no_handler_error()),
        ?_test(test_handler_exception()),
        ?_test(test_io_file_not_found())
    ].

test_no_handler_error() ->
    %% Test error when no handler is registered
    ?assertError(
        {no_handler_for_effect, 'Unregistered', someOp},
        catena_effect_runtime:perform('Unregistered', someOp, [])
    ).

test_handler_exception() ->
    %% Test that handler exceptions are properly wrapped
    Handlers = [
        {'Throws', [
            {throwError, fun() -> throw(my_error) end}
        ]}
    ],
    ?assertError(
        {effect_error, 'Throws', throwError, {throw, my_error}},
        catena_effect_runtime:with_handlers(Handlers, fun() ->
            catena_effect_runtime:perform('Throws', throwError, [])
        end)
    ).

test_io_file_not_found() ->
    %% Test IO error for non-existent file
    ?assertError(
        {io_error, readFile, enoent},
        catena_effect_runtime:perform('IO', readFile, ["/nonexistent/path/file.txt"])
    ).

%%====================================================================
%% Utility Conversion Tests
%%====================================================================

utility_test_() ->
    [
        ?_test(test_io_string_conversion())
    ].

test_io_string_conversion() ->
    %% Test that various types can be printed
    ?assertEqual(ok, catena_effect_runtime:perform('IO', print, [<<"binary">>])),
    ?assertEqual(ok, catena_effect_runtime:perform('IO', print, ["list"])),
    ?assertEqual(ok, catena_effect_runtime:perform('IO', print, [atom])),
    ?assertEqual(ok, catena_effect_runtime:perform('IO', print, [42])),
    ?assertEqual(ok, catena_effect_runtime:perform('IO', print, [3.14])).
