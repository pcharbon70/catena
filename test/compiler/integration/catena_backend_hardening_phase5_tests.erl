-module(catena_backend_hardening_phase5_tests).

-include_lib("eunit/include/eunit.hrl").

declared_performed_and_nested_effect_programs_execute_test() ->
    Source =
        "module PhaseFiveEffects\n"
        "export transform run\n"
        "effect Math\n"
        "operation zero : Int\n"
        "operation add : Int -> Int -> Int\n"
        "end\n"
        "effect Label\n"
        "operation mark : Int -> Int\n"
        "end\n"
        "transform helper ignored = "
            "perform Label.mark("
            "perform Math.add(perform Math.zero(), 22))\n"
        "type HelperBoundary = HelperBoundary\n"
        "transform run ignored = (\n"
        "  handle (handle helper ignored then {\n"
        "    Math {\n"
        "      zero -> 20\n"
        "      add(left, right) -> (left + right)\n"
        "    }\n"
        "  }) then {\n"
        "    Label { mark(value) -> value }\n"
        "  },\n"
        "  handle (handle perform Math.zero() then {\n"
        "    Math {\n"
        "      zero -> 41\n"
        "      add(left, right) -> (left + right)\n"
        "    }\n"
        "  }) then {\n"
        "    Math {\n"
        "      zero -> 0\n"
        "      add(left, right) -> (left + right)\n"
        "    }\n"
        "  }\n"
        ")\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    Operations = catena_compilation_unit:effect_operations(Unit),
    ?assertMatch(
        #{identity := {'Math', zero, 0}},
        maps:get({'Math', zero}, Operations)
    ),
    ?assertMatch(
        #{identity := {'Math', add, 2}},
        maps:get({'Math', add}, Operations)
    ),
    with_executable_module(
        Source,
        'PhaseFiveEffects',
        fun() ->
            ?assertEqual({42, 41}, 'PhaseFiveEffects':run(0)),
            ?assertNot(catena_effect_system:is_initialized())
        end
    ).

generated_value_handlers_execute_on_the_capturing_process_test() ->
    Source =
        "module PhaseFiveNormalCleanup\n"
        "export transform run\n"
        "effect Capture\n"
        "operation answer : Int\n"
        "end\n"
        "effect Process\n"
        "operation self : Int\n"
        "operation send : Int -> Int -> Int\n"
        "end\n"
        "transform run observer = "
            "handle perform Capture.answer() then {\n"
        "  Capture {\n"
        "    answer -> "
            "let ignored = perform Process.send("
            "observer, perform Process.self()) "
            "in 42\n"
        "  }\n"
        "}\n",
    with_executable_module(
        Source,
        'PhaseFiveNormalCleanup',
        fun() ->
            ?assertEqual(42, 'PhaseFiveNormalCleanup':run(self())),
            HandlerPid = receive_handler_pid(),
            ?assertEqual(self(), HandlerPid),
            ?assert(is_process_alive(HandlerPid)),
            ?assertNot(catena_effect_system:is_initialized())
        end
    ).

generated_handler_failures_are_structured_on_the_capturing_process_test() ->
    Source =
        "module PhaseFiveHandlerFailure\n"
        "export transform run\n"
        "effect Fault\n"
        "operation fail : Int\n"
        "end\n"
        "effect Process\n"
        "operation self : Int\n"
        "operation send : Int -> Int -> Int\n"
        "end\n"
        "transform run observer = "
            "handle perform Fault.fail() then {\n"
        "  Fault {\n"
        "    fail -> "
            "let ignored = perform Process.send("
            "observer, perform Process.self()) "
            "in 1 / 0\n"
        "  }\n"
        "}\n",
    with_executable_module(
        Source,
        'PhaseFiveHandlerFailure',
        fun() ->
            ?assertMatch(
                {error, #{
                    category := handler_failure,
                    details := #{class := error, reason := badarith}
                }},
                'PhaseFiveHandlerFailure':run(self())
            ),
            HandlerPid = receive_handler_pid(),
            ?assertEqual(self(), HandlerPid),
            ?assertNot(catena_effect_system:is_initialized())
        end
    ).

generated_unhandled_operations_are_structured_test() ->
    Source =
        "module PhaseFiveUnhandled\n"
        "export transform run\n"
        "effect Present\n"
        "operation answer : Int\n"
        "end\n"
        "effect Missing\n"
        "operation read : Int\n"
        "end\n"
        "effect Process\n"
        "operation self : Int\n"
        "operation send : Int -> Int -> Int\n"
        "end\n"
        "transform run observer = "
            "handle "
            "let answered = perform Present.answer() "
            "in perform Missing.read() "
            "then {\n"
        "  Present {\n"
        "    answer -> "
            "let ignored = perform Process.send("
            "observer, perform Process.self()) "
            "in 42\n"
        "  }\n"
        "}\n",
    with_executable_module(
        Source,
        'PhaseFiveUnhandled',
        fun() ->
            ?assertMatch(
                {error, #{
                    category := unhandled_effect,
                    details := #{effect := 'Missing', operation := read}
                }},
                'PhaseFiveUnhandled':run(self())
            ),
            HandlerPid = receive_handler_pid(),
            ?assertEqual(self(), HandlerPid),
            ?assertNot(catena_effect_system:is_initialized())
        end
    ).

generated_local_handlers_do_not_use_process_provider_timeouts_test() ->
    Source =
        "module PhaseFiveTimeout\n"
        "export transform run\n"
        "effect Slow\n"
        "operation wait : Int\n"
        "end\n"
        "effect Process\n"
        "operation self : Int\n"
        "operation send : Int -> Int -> Int\n"
        "end\n"
        "transform run observer = "
            "handle perform Slow.wait() then {\n"
        "  Slow {\n"
        "    wait -> "
            "let ignored = perform Process.send("
            "observer, perform Process.self()) "
            "in 42\n"
        "  }\n"
        "}\n",
    with_executable_module(
        Source,
        'PhaseFiveTimeout',
        fun() ->
            ok = catena_effect_system:init([{effect_timeout, 25}]),
            ?assertEqual(42, 'PhaseFiveTimeout':run(self())),
            HandlerPid = receive_handler_pid(),
            ?assertEqual(self(), HandlerPid),
            ?assert(catena_effect_system:is_initialized())
        end
    ).

invalid_effect_programs_fail_before_artifact_success_test_() ->
    [
        {"unknown operation",
            ?_assertMatch(
                {error,
                    {effect_resolution_error, unknown_operation, #{}}},
                catena_compile:compile_string_to_core(
                    "module PhaseFiveUnknownOperation\n"
                    "effect Console\n"
                    "operation read : Int\n"
                    "end\n"
                    "transform run = perform Console.write()\n"
                )
            )},
        {"wrong operation arity",
            ?_assertMatch(
                {error,
                    {effect_resolution_error,
                        operation_arity_mismatch, #{}}},
                catena_compile:compile_string_to_core(
                    "module PhaseFiveWrongArity\n"
                    "effect Math\n"
                    "operation add : Int -> Int -> Int\n"
                    "end\n"
                    "transform run = perform Math.add(1)\n"
                )
            )},
        {"invalid argument type",
            ?_assertMatch(
                {error, {type_error, run, [_ | _]}},
                catena_compile:compile_string_to_core(
                    "module PhaseFiveWrongType\n"
                    "effect Counter\n"
                    "operation increment : Int -> Int\n"
                    "end\n"
                    "transform run = "
                        "perform Counter.increment(\"wrong\")\n"
                )
            )},
        {"missing handler coverage",
            ?_assertMatch(
                {error,
                    {effect_resolution_error,
                        missing_handler_operations, #{}}},
                catena_compile:compile_string_to_core(
                    "module PhaseFiveMissingCase\n"
                    "effect State\n"
                    "operation get : Int\n"
                    "operation put : Int -> Int\n"
                    "end\n"
                    "transform run = "
                        "handle perform State.get() then {\n"
                    "  State { get -> 1 }\n"
                    "}\n"
                )
            )},
        {"declared effect mismatch",
            ?_assertMatch(
                {error, {effect_mismatch, run, _, _}},
                catena_compile:compile_string_to_core(
                    "module PhaseFiveEffectMismatch\n"
                    "effect Console\n"
                    "operation read : Int\n"
                    "end\n"
                    "transform run : Int / {}\n"
                    "transform run = perform Console.read()\n"
                )
            )},
        {"unavailable runtime contract",
            ?_assertMatch(
                {error,
                    {backend_error,
                        runtime_dependency_unavailable, #{}}},
                catena_compile:compile_string_to_core(
                    "module PhaseFiveUnavailableRuntime\n"
                    "effect Console\n"
                    "operation read : Int\n"
                    "end\n"
                    "transform run = perform Console.read()\n",
                    #{
                        codegen_opts => #{
                            available_runtime_modules => []
                        }
                    }
                )
            )}
    ].

receive_handler_pid() ->
    receive
        HandlerPid when is_pid(HandlerPid) ->
            HandlerPid
    after 1000 ->
        error(handler_pid_not_received)
    end.

with_executable_module(Source, Module, Assertion) ->
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    {ok, Module, Binary, _Warnings} = compile:forms(
        CoreModule,
        [from_core, binary, return_errors, return_warnings]
    ),
    unload(Module),
    try
        {module, Module} = code:load_binary(
            Module,
            atom_to_list(Module) ++ ".core",
            Binary
        ),
        Assertion()
    after
        unload(Module),
        case catena_effect_system:is_initialized() of
            true -> catena_effect_system:stop_runtime();
            false -> ok
        end
    end.

unload(Module) ->
    code:purge(Module),
    code:delete(Module).
