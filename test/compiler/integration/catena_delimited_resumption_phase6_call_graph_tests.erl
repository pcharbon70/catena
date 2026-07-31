%%%-------------------------------------------------------------------
%%% @doc Section 6.2 loaded-BEAM coverage for complete control call graphs.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_phase6_call_graph_tests).

-include_lib("eunit/include/eunit.hrl").

recursive_mixed_mode_and_data_paths_execute_test() ->
    Source =
        "module PhaseSixRecursiveData\n"
        "export transform run\n"
        "effect Choice\n"
        "operation choose : Int -> Int\n"
        "end\n"
        "type Wrapped = Wrapped Int\n"
        "transform even : Int -> Int / {Choice}\n"
        "transform even 0 = perform Choice.choose(5)\n"
        "transform even value = odd (value - 1)\n"
        "type MutualBoundary = MutualBoundary\n"
        "transform odd : Int -> Int / {Choice}\n"
        "transform odd 0 = perform Choice.choose(5)\n"
        "transform odd value = even (value - 1)\n"
        "type DataBoundary = DataBoundary\n"
        "transform package "
            "(Wrapped(value), [head tail], {answer: answer}) "
            "when answer > 0 = (value, head, tail, answer)\n"
        "transform package _ = (0, 0, 0, 0)\n"
        "type RunBoundary = RunBoundary\n"
        "transform run value = handle "
            "(let chosen = even value in "
            "package (Wrapped chosen, [chosen, chosen + 1], "
            "{answer: chosen})) then {\n"
        "  Choice { choose(offered) with k -> "
            "resume(k, offered + 10) }\n"
        "}\n",
    with_loaded_source(Source, 'PhaseSixRecursiveData', fun() ->
        ?assertEqual(
            {15, 15, 16, 15},
            'PhaseSixRecursiveData':run(2000)
        )
    end).

resumable_higher_order_trait_dispatch_executes_test() ->
    Source =
        "module PhaseSixTraitControl\n"
        "export transform run\n"
        "effect Choice\n"
        "operation choose : Int -> Int\n"
        "end\n"
        "trait ApplyChoice a where\n"
        "  applyChoice : (a -> a) -> a -> a\n"
        "end\n"
        "instance ApplyChoice Int where\n"
        "  transform applyChoice function value = function value\n"
        "end\n"
        "transform run value = "
            "let result = applyChoice "
            "(fn current -> handle "
            "(let selected = perform Choice.choose(current) "
            "in selected + 1) then {\n"
        "  Choice { choose(offered) with k -> "
            "resume(k, offered + 10) }\n"
        "}) value in result + 2\n",
    with_loaded_source(Source, 'PhaseSixTraitControl', fun() ->
        ?assertEqual(54, 'PhaseSixTraitControl':run(41))
    end).

imported_resumable_entries_and_closures_execute_test() ->
    Provider =
        "module PhaseSixControlProvider\n"
        "export transform select\n"
        "effect Choice\n"
        "operation choose : Int -> Int\n"
        "end\n"
        "transform select : Int -> Int\n"
        "transform select value = handle "
            "(let chosen = perform Choice.choose(value) in chosen + 1) "
            "then {\n"
        "  Choice { choose(offered) with k -> "
            "resume(k, offered + 10) }\n"
        "}\n",
    Consumer =
        "module PhaseSixControlConsumer\n"
        "export transform run\n"
        "import PhaseSixControlProvider\n"
        "transform invoke : (Int -> Int) -> Int -> Int\n"
        "transform invoke function value = function value\n"
        "type ConsumerBoundary = ConsumerBoundary\n"
        "transform run : Int -> Int\n"
        "transform run value = "
            "let selected = invoke select value in selected + 1\n",
    Sources = #{
        'PhaseSixControlProvider' => Provider,
        'PhaseSixControlConsumer' => Consumer
    },
    with_loaded_set(Sources, fun(Result) ->
        ?assertEqual(53, 'PhaseSixControlConsumer':run(41)),
        ProviderUnit = artifact_unit('PhaseSixControlProvider', Result),
        ConsumerUnit = artifact_unit('PhaseSixControlConsumer', Result),
        ?assertEqual(
            {ok, resumable},
            catena_control_mode:mode(
                select,
                catena_compilation_unit:control_modes(ProviderUnit)
            )
        ),
        ?assertEqual(
            {ok, resumable},
            catena_control_mode:mode(
                run,
                catena_compilation_unit:control_modes(ConsumerUnit)
            )
        )
    end).

artifact_unit(Module, Result) ->
    maps:get(unit, maps:get(Module, maps:get(artifacts, Result))).

with_loaded_source(Source, Module, Assertion) ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(Source),
    unload(Module),
    try
        {module, Module} = code:load_binary(
            Module,
            "phase6-call-graph-memory",
            maps:get(beam, Artifact)
        ),
        Assertion()
    after
        unload(Module)
    end.

with_loaded_set(Sources, Assertion) ->
    {ok, Result} = catena_module_compile:compile_source_set(Sources, #{}),
    Artifacts = maps:get(artifacts, Result),
    Modules = maps:get(order, Result),
    lists:foreach(
        fun(Module) ->
            unload(Module),
            Artifact = maps:get(Module, Artifacts),
            {module, Module} = code:load_binary(
                Module,
                "phase6-call-graph-set-memory",
                maps:get(beam, Artifact)
            )
        end,
        Modules
    ),
    try
        Assertion(Result)
    after
        lists:foreach(fun unload/1, lists:reverse(Modules))
    end.

unload(Module) ->
    _ = code:purge(Module),
    _ = code:delete(Module),
    ok.
