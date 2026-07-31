%%%-------------------------------------------------------------------
%%% @doc Phase 8.2 measurements and optimization equivalence evidence.
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_phase8_performance_tests).

-include_lib("eunit/include/eunit.hrl").

proven_returns_and_direct_bridges_are_optimized_test() ->
    Source = maps:get(mixed_mode_bridges, catena_resumption_benchmark:scenarios()),
    {ok, Optimized} = catena_compile:compile_string_to_unit(Source),
    OptimizedIR = catena_compilation_unit:control_ir(Optimized),
    OptimizedReport = maps:get(
        optimization,
        catena_compilation_unit:control_validation(Optimized)
    ),
    ?assertEqual(true, maps:get(enabled, OptimizedReport)),
    ?assert(maps:get(return_wrappers_eliminated, OptimizedReport) > 0),
    ?assert(maps:get(direct_bridges_collapsed, OptimizedReport) > 0),
    ?assert(
        maps:get(nodes_after, OptimizedReport) <
            maps:get(nodes_before, OptimizedReport)
    ),
    ?assertEqual(0, operation_count(return, OptimizedIR)),
    ?assertEqual(0, operation_count(bridge, OptimizedIR)),
    {ok, Unoptimized} = catena_compile:compile_string_to_unit(
        Source,
        no_optimization()
    ),
    UnoptimizedIR = catena_compilation_unit:control_ir(Unoptimized),
    UnoptimizedReport = maps:get(
        optimization,
        catena_compilation_unit:control_validation(Unoptimized)
    ),
    ?assertEqual(false, maps:get(enabled, UnoptimizedReport)),
    ?assert(operation_count(return, UnoptimizedIR) > 0),
    ?assert(operation_count(bridge, UnoptimizedIR) > 0).

optimized_and_unoptimized_loaded_beam_are_equivalent_test() ->
    Scenarios = catena_resumption_benchmark:scenarios(),
    Cases = [
        {direct, [41], 42},
        {deep_one_shot, [0], 42},
        {mixed_mode_bridges, [0], 41},
        {shallow_handling, [0], 42},
        {multi_shot_branching, [0], 82}
    ],
    lists:foreach(
        fun({Name, Arguments, Expected}) ->
            Source = maps:get(Name, Scenarios),
            Optimized = execute(Source, #{}, Arguments),
            Unoptimized = execute(Source, no_optimization(), Arguments),
            ?assertEqual(Expected, Optimized),
            ?assertEqual(Optimized, Unoptimized)
        end,
        Cases
    ).

open_and_higher_order_capabilities_remain_conservative_test() ->
    Source =
        "module PhaseEightConservative\n"
        "export transform invoke\n"
        "transform invoke f value = f value\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    Modes = catena_compilation_unit:control_modes(Unit),
    ?assertEqual({ok, resumable}, catena_control_mode:mode(invoke, Modes)),
    Report = maps:get(
        optimization,
        catena_compilation_unit:control_validation(Unit)
    ),
    ?assertEqual(0, maps:get(direct_bridges_collapsed, Report)),
    ?assert(lists:any(
        fun(Node) ->
            maps:get(op, Node) =:= closure orelse
                maps:get(op, Node) =:= cps_call
        end,
        catena_control_ir:nodes(catena_compilation_unit:control_ir(Unit))
    )).

compiler_and_runtime_baselines_cover_every_required_class_test() ->
    {ok, Result} = catena_resumption_benchmark:suite(#{iterations => 3}),
    ?assertEqual(passed, maps:get(status, Result)),
    Required = [
        direct,
        provider_only,
        deep_one_shot,
        mixed_mode_bridges,
        retained_resumptions,
        shallow_handling,
        multi_shot_branching
    ],
    Compiler = maps:get(compiler, Result),
    Runtime = maps:get(runtime, Result),
    lists:foreach(
        fun(Name) ->
            ?assert(maps:is_key(Name, Compiler)),
            ?assert(maps:is_key(Name, Runtime))
        end,
        Required
    ),
    lists:foreach(
        fun(Metrics) ->
            lists:foreach(
                fun(Key) -> ?assert(maps:is_key(Key, Metrics)) end,
                [
                    classification_us,
                    cps_lowering_us,
                    core_size_bytes,
                    compile_us,
                    artifact_size_bytes,
                    generated_function_count,
                    source_map_bytes
                ]
            )
        end,
        maps:values(Compiler)
    ),
    lists:foreach(
        fun(Metrics) ->
            lists:foreach(
                fun(Key) -> ?assert(maps:is_key(Key, Metrics)) end,
                [
                    reductions,
                    allocated_words_estimate,
                    closure_words,
                    latency_us,
                    throughput_per_second,
                    scheduler
                ]
            )
        end,
        maps:values(Runtime)
    ),
    Retention = maps:get(retention_resources,
        maps:get(retained_resumptions, Runtime)),
    ?assert(maps:get(retained_words, Retention) > 0),
    Branches = maps:get(branch_resources,
        maps:get(multi_shot_branching, Runtime)),
    ?assertEqual(3, maps:get(completed_branches, Branches)),
    ?assertEqual(ok, catena_resumption_benchmark:check(Result)).

cached_parent_lookup_and_tail_auto_resume_preserve_results_test() ->
    Parent = catena_effect_runtime:with_value_provider(
        catena_effect_runtime:empty_context(),
        {'Outer', [{read, fun() -> 40 end}]},
        fun(Context) -> Context end
    ),
    Case = catena_effect_runtime:value_case(
        choose,
        0,
        fun([], _Context) -> 2 end
    ),
    Result = catena_effect_runtime:with_resumable_handler(
        Parent,
        #{
            effect => 'Inner',
            cases => [Case],
            depth => shallow,
            resumption_kind => one_shot,
            origin => phase8_cached_handler
        },
        fun(Context) ->
            Outer = catena_effect_runtime:perform_cps(
                Context,
                'Outer',
                read,
                [],
                fun(Value, _Restored) -> Value end
            ),
            Inner = catena_effect_runtime:perform_cps(
                Context,
                'Inner',
                choose,
                [],
                fun(Value, _Restored) -> Value end
            ),
            Outer + Inner
        end
    ),
    ?assertEqual(42, Result).

operation_count(Operation, IR) ->
    length([
        Node
        || Node <- catena_control_ir:nodes(IR),
           maps:get(op, Node) =:= Operation
    ]).

no_optimization() ->
    #{codegen_opts => #{optimize_control => false}}.

execute(Source, Options, Arguments) ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(Source, Options),
    Module = maps:get(runtime_module, Artifact),
    unload(Module),
    try
        {module, Module} = catena_beam_artifact:load(Artifact),
        erlang:apply(Module, run, Arguments)
    after
        unload(Module)
    end.

unload(Module) ->
    _ = code:soft_purge(Module),
    _ = code:delete(Module),
    _ = code:purge(Module),
    ok.
