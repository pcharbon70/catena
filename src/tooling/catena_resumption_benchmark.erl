%%%-------------------------------------------------------------------
%%% @doc Deterministic Phase 8 selective-CPS and resumption benchmarks.
%%%
%%% Results are data maps so CI can retain and compare them. Wall-clock
%%% values are observations, while promotion thresholds are intentionally
%%% broad safety rails; semantic equivalence is asserted separately.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_resumption_benchmark).

-export([
    scenarios/0,
    compiler_baseline/2,
    runtime_baselines/1,
    suite/0,
    suite/1,
    thresholds/0,
    check/1
]).

-define(DEFAULT_ITERATIONS, 100).

-type source_text() :: nonempty_list(1..255).

-spec scenarios() -> #{
    direct := source_text(),
    provider_only := source_text(),
    deep_one_shot := source_text(),
    mixed_mode_bridges := source_text(),
    retained_resumptions := source_text(),
    shallow_handling := source_text(),
    multi_shot_branching := source_text()
}.
scenarios() ->
    #{
        direct => direct_source(),
        provider_only => provider_source(),
        deep_one_shot => deep_source(),
        mixed_mode_bridges => mixed_source(),
        retained_resumptions => retained_source(),
        shallow_handling => shallow_source(),
        multi_shot_branching => multi_source()
    }.

-spec compiler_baseline(string(), map()) -> {ok, map()} | {error, term()}.
compiler_baseline(Source, Options) when is_list(Source), is_map(Options) ->
    {CompileUs, UnitResult} = timer:tc(
        catena_compile,
        compile_string_to_unit,
        [Source, Options]
    ),
    case UnitResult of
        {ok, Unit} ->
            {module, Module, _Exports, _Imports, Declarations, _Origin} =
                catena_compilation_unit:normalized_ast(Unit),
            {ClassificationUs, {ok, Modes}} = timer:tc(
                catena_control_mode,
                analyze,
                [
                    Module,
                    Declarations,
                    catena_compilation_unit:typed_declarations(Unit),
                    catena_compilation_unit:callables(Unit),
                    catena_compilation_unit:options(Unit)
                ]
            ),
            {LoweringUs, {ok, LoweredIR}} = timer:tc(
                catena_selective_cps,
                lower,
                [Unit]
            ),
            {ArtifactUs, ArtifactResult} = timer:tc(
                catena_beam_artifact,
                from_unit,
                [Unit]
            ),
            case ArtifactResult of
                {ok, Artifact} ->
                    Core = maps:get(core, Artifact),
                    Origins = maps:get(origins, maps:get(metadata, Artifact)),
                    OptimizedIR = catena_compilation_unit:control_ir(Unit),
                    Validation = catena_compilation_unit:control_validation(Unit),
                    {ok, #{
                        module => Module,
                        compile_us => CompileUs,
                        classification_us => ClassificationUs,
                        cps_lowering_us => LoweringUs,
                        artifact_us => ArtifactUs,
                        core_size_bytes => external_size(Core),
                        artifact_size_bytes => byte_size(maps:get(beam, Artifact)),
                        generated_function_count => length(cerl:module_defs(Core)),
                        source_map_bytes => external_size(Origins),
                        control_nodes_before => length(
                            catena_control_ir:nodes(LoweredIR)
                        ),
                        control_nodes_after => length(
                            catena_control_ir:nodes(OptimizedIR)
                        ),
                        bridge_count => count_operation(bridge, OptimizedIR),
                        classification => classification_summary(Modes),
                        optimization => maps:get(optimization, Validation),
                        artifact => Artifact
                    }};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec runtime_baselines(pos_integer()) -> {ok, map()} | {error, term()}.
runtime_baselines(Iterations) when is_integer(Iterations), Iterations > 0 ->
    catena_resumption_runtime:reset_for_test(),
    ProviderContext = catena_effect_runtime:with_value_provider(
        catena_effect_runtime:empty_context(),
        {'BenchProvider', [{read, fun() -> 41 end}]},
        fun(Context) -> Context end
    ),
    Direct = measure(fun() -> 42 end, Iterations),
    Provider = measure(fun() ->
        catena_effect_runtime:perform_cps(
            ProviderContext,
            'BenchProvider',
            read,
            [],
            fun(Value, _Context) -> Value + 1 end
        )
    end, Iterations),
    Deep = measure(fun() -> automatic_value(deep) end, Iterations),
    Shallow = measure(fun() -> automatic_value(shallow) end, Iterations),
    Retained = (measure(fun retained_once/0, Iterations))#{
        retention_resources => retention_resources()
    },
    {ok, MixedArtifact} = catena_compile:compile_string_to_beam(
        mixed_source(),
        #{codegen_opts => #{optimize_control => false}}
    ),
    MixedModule = maps:get(runtime_module, MixedArtifact),
    {module, MixedModule} = catena_beam_artifact:load(MixedArtifact),
    Mixed = try
        measure(fun() -> erlang:apply(MixedModule, run, [0]) end, Iterations)
    after
        unload(MixedModule)
    end,
    MultiIterations = erlang:min(Iterations, 32),
    {Multi, BranchResources} = measure_multishot(MultiIterations),
    catena_resumption_runtime:reset_for_test(),
    {ok, #{
        direct => Direct,
        provider_only => Provider,
        deep_one_shot => Deep,
        mixed_mode_bridges => Mixed,
        shallow_handling => Shallow,
        retained_resumptions => Retained,
        multi_shot_branching => Multi#{
            branch_resources => BranchResources
        }
    }};
runtime_baselines(Iterations) ->
    {error, {invalid_benchmark_iterations, Iterations}}.

-spec suite() -> {ok, map()} | {error, term()}.
suite() ->
    suite(#{}).

-spec suite(map()) -> {ok, map()} | {error, term()}.
suite(Options) when is_map(Options) ->
    Iterations = maps:get(iterations, Options, ?DEFAULT_ITERATIONS),
    CompilerOptions = maps:get(compiler_options, Options, #{}),
    case compiler_scenarios(maps:to_list(scenarios()), CompilerOptions, #{}) of
        {ok, Compiler} ->
            case runtime_baselines(Iterations) of
                {ok, Runtime} ->
                    Result = #{
                        iterations => Iterations,
                        compiler => Compiler,
                        runtime => Runtime,
                        thresholds => thresholds()
                    },
                    case check(Result) of
                        ok -> {ok, Result#{status => passed}};
                        {error, Failures} ->
                            {error, {performance_threshold_failed, Failures, Result}}
                    end;
                {error, _} = Error -> Error
            end;
        {error, _} = Error -> Error
    end;
suite(Options) ->
    {error, {invalid_benchmark_options, Options}}.

-spec thresholds() -> #{
    compile_us := 5000000,
    artifact_size_bytes := 5000000,
    source_map_bytes := 2000000,
    runtime_latency_us := 100000
}.
thresholds() ->
    #{
        compile_us => 5000000,
        artifact_size_bytes => 5000000,
        source_map_bytes => 2000000,
        runtime_latency_us => 100000
    }.

-spec check(map()) -> ok | {error, [map()]}.
check(#{compiler := Compiler, runtime := Runtime, thresholds := Limits}) ->
    CompilerFailures = lists:append([
        metric_failures(Name, Metrics, Limits)
        || {Name, Metrics} <- maps:to_list(Compiler)
    ]),
    RuntimeFailures = lists:append([
        runtime_failures(Name, Metrics, Limits)
        || {Name, Metrics} <- maps:to_list(Runtime)
    ]),
    case CompilerFailures ++ RuntimeFailures of
        [] -> ok;
        Failures -> {error, Failures}
    end;
check(Result) ->
    {error, [#{reason => invalid_benchmark_result, result => Result}]}.

compiler_scenarios([], _Options, Acc) ->
    {ok, Acc};
compiler_scenarios([{Name, Source} | Rest], Options, Acc) ->
    case compiler_baseline(Source, Options) of
        {ok, Metrics0} ->
            Metrics = maps:remove(artifact, Metrics0),
            compiler_scenarios(Rest, Options, Acc#{Name => Metrics});
        {error, Reason} ->
            {error, {benchmark_compile_failed, Name, Reason}}
    end.

classification_summary(Modes) ->
    Entries = catena_control_mode:entries(Modes),
    #{
        direct => length([
            Entry || Entry <- Entries, maps:get(mode, Entry) =:= direct
        ]),
        resumable => length([
            Entry || Entry <- Entries, maps:get(mode, Entry) =:= resumable
        ]),
        fixed_point_iterations => maps:get(fixed_point_iterations, Modes)
    }.

count_operation(Operation, IR) ->
    length([
        Node
        || Node <- catena_control_ir:nodes(IR),
           maps:get(op, Node) =:= Operation
    ]).

external_size(Term) ->
    byte_size(term_to_binary(Term, [deterministic])).

measure(Fun, Iterations) ->
    erlang:garbage_collect(),
    Reductions0 = reductions(),
    Memory0 = process_memory(),
    Scheduler0 = scheduler_id(),
    {ElapsedUs, LastValue} = timer:tc(fun() ->
        repeat(Fun, Iterations, undefined)
    end),
    Scheduler1 = scheduler_id(),
    Reductions = reductions() - Reductions0,
    MemoryWords = erlang:max(
        0,
        (process_memory() - Memory0) div erlang:system_info(wordsize)
    ),
    #{
        iterations => Iterations,
        elapsed_us => ElapsedUs,
        latency_us => ElapsedUs / Iterations,
        throughput_per_second => throughput(Iterations, ElapsedUs),
        reductions => Reductions,
        reductions_per_iteration => Reductions / Iterations,
        allocated_words_estimate => MemoryWords,
        closure_words => erts_debug:flat_size(Fun),
        scheduler => #{
            before => Scheduler0,
            'after' => Scheduler1,
            migrated => Scheduler0 =/= Scheduler1
        },
        last_value => LastValue
    }.

repeat(_Fun, 0, LastValue) ->
    LastValue;
repeat(Fun, Remaining, _LastValue) ->
    repeat(Fun, Remaining - 1, Fun()).

automatic_value(Depth) ->
    Case = catena_effect_runtime:value_case(
        choose,
        0,
        fun([], _Context) -> 41 end,
        {benchmark_value_handler, Depth}
    ),
    Handler = #{
        effect => 'BenchChoice',
        cases => [Case],
        depth => Depth,
        resumption_kind => one_shot,
        origin => {benchmark_handler, Depth}
    },
    catena_effect_runtime:with_resumable_handler(
        catena_effect_runtime:empty_context(),
        Handler,
        fun(Context) ->
            catena_effect_runtime:perform_cps(
                Context,
                'BenchChoice',
                choose,
                [],
                fun(Value, _RestoredContext) -> Value + 1 end
            )
        end
    ).

retained_once() ->
    Context = catena_effect_runtime:empty_context(),
    {ok, Handle} = catena_resumption_runtime:capture(
        fun(Value, _RestoredContext) -> Value + 1 end,
        capture_spec(Context, one_shot)
    ),
    {ok, 42} = catena_resumption_runtime:resume(Handle, 41),
    42.

retention_resources() ->
    Context = catena_effect_runtime:empty_context(),
    {ok, Handle} = catena_resumption_runtime:capture(
        fun(Value, _RestoredContext) -> Value end,
        capture_spec(Context, one_shot)
    ),
    {ok, Stats} = catena_resumption_runtime:branch_stats(Handle),
    ok = catena_resumption_runtime:discard(Handle),
    maps:with([retained_words, budget], Stats).

measure_multishot(Iterations) ->
    Context = catena_effect_runtime:empty_context(),
    {ok, Handle} = catena_resumption_runtime:capture(
        fun(Value, _RestoredContext) -> Value + 1 end,
        capture_spec(Context, multi_shot)
    ),
    Metrics = measure(fun() ->
        {ok, Value} = catena_resumption_runtime:resume(Handle, 41),
        Value
    end, Iterations),
    {ok, Stats} = catena_resumption_runtime:branch_stats(Handle),
    ok = catena_resumption_runtime:discard(Handle),
    {Metrics, Stats}.

capture_spec(Context, Kind) ->
    #{
        context => Context,
        parent_context => Context,
        delimiter => make_ref(),
        depth => deep,
        kind => Kind,
        origin => #{construct => benchmark_resumption},
        metadata => #{effect => 'BenchChoice', operation => choose},
        type_identity => {tresumption, Kind, int, int, []}
    }.

reductions() ->
    {reductions, Value} = process_info(self(), reductions),
    Value.

process_memory() ->
    {memory, Value} = process_info(self(), memory),
    Value.

scheduler_id() ->
    try erlang:system_info(scheduler_id) of
        Value when is_integer(Value) -> Value;
        _ -> unknown
    catch
        error:badarg -> unknown
    end.

throughput(_Iterations, 0) -> infinity;
throughput(Iterations, ElapsedUs) -> Iterations * 1000000.0 / ElapsedUs.

unload(Module) ->
    _ = code:soft_purge(Module),
    _ = code:delete(Module),
    _ = code:purge(Module),
    ok.

metric_failures(Name, Metrics, Limits) ->
    compare_metrics(Name, compiler, Metrics, Limits, [
        compile_us,
        artifact_size_bytes,
        source_map_bytes
    ]).

runtime_failures(Name, Metrics, Limits) ->
    Limit = maps:get(runtime_latency_us, Limits),
    case maps:get(latency_us, Metrics) =< Limit of
        true -> [];
        false -> [#{
            scenario => Name,
            kind => runtime,
            metric => latency_us,
            observed => maps:get(latency_us, Metrics),
            limit => Limit
        }]
    end.

compare_metrics(Name, Kind, Metrics, Limits, Keys) ->
    [
        #{
            scenario => Name,
            kind => Kind,
            metric => Key,
            observed => maps:get(Key, Metrics),
            limit => maps:get(Key, Limits)
        }
        || Key <- Keys,
           maps:get(Key, Metrics) > maps:get(Key, Limits)
    ].

direct_source() ->
    "module PhaseEightPerfDirect\n"
    "export transform run\n"
    "transform run value = value + 1\n".

provider_source() ->
    "module PhaseEightPerfProvider\n"
    "export transform run\n"
    "effect Reader\n"
    "operation read : Int\n"
    "end\n"
    "transform run ignored = perform Reader.read()\n".

deep_source() ->
    "module PhaseEightPerfDeep\n" ++ handler_body("handle ").

mixed_source() ->
    "module PhaseEightPerfMixed\n"
    "export transform run\n"
    "transform increment value = value + 1\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run ignored = handle perform Choice.choose() then {\n"
    "  Choice { choose() with k -> resume(k, increment 40) }\n"
    "}\n".

retained_source() ->
    "module PhaseEightPerfRetained\n" ++ handler_body("handle ").

shallow_source() ->
    "module PhaseEightPerfShallow\n" ++ handler_body("handle shallow ").

multi_source() ->
    "module PhaseEightPerfMulti\n"
    "export transform run\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run ignored = handle multi_shot "
        "(let selected = perform Choice.choose() in selected * 2) then {\n"
    "  Choice { choose() with k -> "
        "let first = resume(k, 20) in resume(k, first + 1) }\n"
    "}\n".

handler_body(Prefix) ->
    "export transform run\n"
    "effect Choice\n"
    "operation choose : Int\n"
    "end\n"
    "transform run ignored = " ++ Prefix ++
        "(let selected = perform Choice.choose() in selected + 1) then {\n"
    "  Choice { choose() with k -> resume(k, 41) }\n"
    "}\n".
