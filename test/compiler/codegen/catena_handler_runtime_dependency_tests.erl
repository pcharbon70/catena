-module(catena_handler_runtime_dependency_tests).

-include_lib("eunit/include/eunit.hrl").

complex_handler_patterns_and_local_scope_execute_test() ->
    Source =
        "module LosslessHandler\n"
        "export transform run\n"
        "effect Math\n"
        "operation sum : (Int, Int) -> Int\n"
        "end\n"
        "transform add left right = left + right\n"
        "type AddBoundary = AddBoundary\n"
        "transform run ignored = handle "
            "perform Math.sum((20, 22)) then {\n"
        "  Math { sum((left, right)) -> (add left right) }\n"
        "}\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    [Handler] = catena_compilation_unit:effect_handlers(Unit),
    ?assertEqual('Math', maps:get(effect, Handler)),
    ?assertEqual(
        [{'Math', sum, 1}],
        maps:get(operations, Handler)
    ),
    with_executable_module(
        Source,
        'LosslessHandler',
        fun() ->
            ?assertEqual(42, 'LosslessHandler':run(0))
        end
    ).

handlers_must_cover_declared_operations_exactly_test_() ->
    [
        {"missing operation",
            ?_test(assert_handler_error(
                handler_source(
                    "get -> 1"
                ),
                missing_handler_operations
            ))},
        {"duplicate operation",
            ?_test(assert_handler_error(
                handler_source(
                    "get -> 1\n"
                    "get -> 2\n"
                    "put(value) -> value"
                ),
                duplicate_handler_operation
            ))},
        {"unknown operation",
            ?_test(assert_handler_error(
                handler_source(
                    "get -> 1\n"
                    "put(value) -> value\n"
                    "unknown -> 0"
                ),
                unknown_handler_operation
            ))},
        {"wrong operation arity",
            ?_test(assert_handler_error(
                handler_source(
                    "get(value) -> value\n"
                    "put(value) -> value"
                ),
                handler_arity_mismatch
            ))},
        {"unknown handled effect",
            ?_test(assert_handler_error(
                "module UnknownHandledEffect\n"
                "transform run ignored = handle 1 then {\n"
                "  Missing { read -> 1 }\n"
                "}\n",
                unknown_handled_effect
            ))}
    ].

generated_modules_declare_runtime_dependencies_test() ->
    Source = effect_source("RuntimeDependencies"),
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    Dependencies = catena_compilation_unit:runtime_dependencies(Unit),
    ?assertEqual(
        [
            #{module => catena_effect_runtime, version => 1},
            #{module => catena_effect_system, version => 1}
        ],
        Dependencies
    ),
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    Attributes = maps:from_list([
        {cerl:atom_val(Name), cerl:concrete(Value)}
        || {Name, Value} <- cerl:module_attrs(CoreModule)
    ]),
    ?assertEqual(
        [
            {catena_effect_runtime, 1},
            {catena_effect_system, 1}
        ],
        maps:get(catena_runtime_dependencies, Attributes)
    ).

unavailable_runtime_contract_fails_artifact_preparation_test() ->
    Source = effect_source("UnavailableRuntime"),
    Result = catena_compile:compile_string_to_core(
        Source,
        #{
            codegen_opts => #{
                available_runtime_modules => []
            }
        }
    ),
    ?assertMatch(
        {error,
            {backend_error, runtime_dependency_unavailable, #{}}},
        Result
    ),
    {error, Diagnostic} = Result,
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(
        artifact_preparation,
        maps:get(stage, Details)
    ),
    ?assertEqual(
        catena_effect_runtime,
        maps:get(dependency_module, Details)
    ).

handler_source(Cases) ->
    "module HandlerValidation\n"
    "effect State\n"
    "operation get : Int\n"
    "operation put : Int -> Int\n"
    "end\n"
    "transform run ignored = handle perform State.get() then {\n"
    "  State {\n" ++ Cases ++ "\n"
    "  }\n"
    "}\n".

effect_source(ModuleName) ->
    "module " ++ ModuleName ++ "\n"
    "export transform run\n"
    "effect Reader\n"
    "operation read : Int\n"
    "end\n"
    "transform run ignored = perform Reader.read()\n".

assert_handler_error(Source, ExpectedReason) ->
    Result = catena_compile:compile_string_to_unit(Source),
    ?assertMatch(
        {error, {effect_resolution_error, ExpectedReason, #{}}},
        Result
    ),
    {error, {effect_resolution_error, ExpectedReason, Details}} = Result,
    ?assertEqual(effect_resolution, maps:get(stage, Details)),
    ?assertNotEqual(undefined, maps:get(location, Details)).

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
