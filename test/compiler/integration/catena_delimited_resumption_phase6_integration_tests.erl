%%%-------------------------------------------------------------------
%%% @doc Phase 6 public source-to-artifact-to-loaded-BEAM acceptance tests.
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_phase6_integration_tests).

-include_lib("eunit/include/eunit.hrl").

explicit_auto_abort_and_multiple_perform_semantics_test() ->
    Source =
        "module PhaseSixHandlerMatrix\n"
        "export transform explicit\n"
        "export transform automatic\n"
        "export transform abandoned\n"
        "export transform multiple\n"
        "effect Choice\n"
        "operation choose : Int\n"
        "end\n"
        "effect Counter\n"
        "operation next : Int -> Int\n"
        "end\n"
        "transform explicit = handle "
            "(let selected = perform Choice.choose() in selected * 2) "
            "then {\n"
        "  Choice { choose() with k -> "
            "let result = resume(k, 21) in result + 1 }\n"
        "}\n"
        "transform automatic = handle "
            "(let selected = perform Choice.choose() in selected + 1) "
            "then {\n"
        "  Choice { choose() -> 41 }\n"
        "}\n"
        "transform abandoned = handle "
            "(let selected = perform Choice.choose() in selected + 100) "
            "then {\n"
        "  Choice { choose() with k -> 7 }\n"
        "}\n"
        "transform multiple = handle "
            "(let first = perform Counter.next(0) in "
            "let second = perform Counter.next(first) in first + second) "
            "then {\n"
        "  Counter { next(value) -> (value + 1) }\n"
        "}\n",
    with_loaded(Source, 'PhaseSixHandlerMatrix', fun(Module) ->
        ?assertEqual(43, Module:explicit()),
        ?assertEqual(42, Module:automatic()),
        ?assertEqual(7, Module:abandoned()),
        ?assertEqual(3, Module:multiple())
    end).

nested_deep_handlers_resume_across_delimiters_test() ->
    Source =
        "module PhaseSixNestedDeep\n"
        "export transform run\n"
        "effect Inner\n"
        "operation read : Int\n"
        "end\n"
        "effect Outer\n"
        "operation add : Int -> Int\n"
        "end\n"
        "transform run = handle "
            "(handle (let value = perform Inner.read() in "
            "perform Outer.add(value)) then {\n"
        "  Inner { read() with innerK -> resume(innerK, 1) }\n"
        "}) then {\n"
        "  Outer { add(value) with outerK -> resume(outerK, value + 41) }\n"
        "}\n",
    with_loaded(Source, 'PhaseSixNestedDeep', fun(Module) ->
        ?assertEqual(42, Module:run())
    end).

first_class_resumption_crosses_helper_and_resumes_on_owner_test() ->
    Source =
        "module PhaseSixRetained\n"
        "export transform run\n"
        "effect State\n"
        "operation put : Int -> Int\n"
        "end\n"
        "transform identity value = value\n"
        "transform continue k value = resume(k, value)\n"
        "transform run ignored = handle perform State.put(1) then {\n"
        "  State { put(value) with k -> "
            "let result = continue (identity k) value in result }\n"
        "}\n",
    with_loaded(Source, 'PhaseSixRetained', fun(Module) ->
        ?assertEqual(1, Module:run(ignored))
    end).

builtin_provider_continues_on_the_public_owner_process_test() ->
    Source =
        "module PhaseSixBuiltinProvider\n"
        "export transform run\n"
        "effect Process\n"
        "operation self : Int\n"
        "end\n"
        "transform run ignored = "
            "let owner = perform Process.self() in owner\n",
    Owner = self(),
    with_loaded(Source, 'PhaseSixBuiltinProvider', fun(Module) ->
        ?assertEqual(Owner, Module:run(ignored))
    end).

with_loaded(Source, Module, Assertion) ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(Source),
    {ok, Artifact} = catena_beam_artifact:validate(Artifact),
    unload(Module),
    try
        {module, Module} = catena_beam_artifact:load(Artifact),
        Assertion(Module)
    after
        unload(Module),
        catena_resumption_runtime:reset_for_test()
    end.

unload(Module) ->
    code:purge(Module),
    code:delete(Module).
