%% @doc Unit Tests for Phase 7.5: Performance Helpers
-module(catena_perf_tests).

-include_lib("eunit/include/eunit.hrl").

cached_generator_preserves_values_and_tracks_hits_test() ->
    ok = catena_perf:clear_cache(),
    Generator = catena_perf:cached_generator(answer, catena_gen:constant(42)),
    Seed = catena_gen:seed_from_int(1),
    Tree1 = catena_gen:run(Generator, 5, Seed),
    Tree2 = catena_gen:run(Generator, 5, Seed),
    Stats = catena_perf:cache_stats(),
    ?assertEqual(42, catena_tree:root(Tree1)),
    ?assertEqual(42, catena_tree:root(Tree2)),
    ?assertEqual(1, maps:get(hits, Stats)),
    ?assertEqual(1, maps:get(misses, Stats)),
    ok.

bounded_generator_caps_observed_size_test() ->
    Base = catena_gen:sized(fun(Size) ->
        catena_stdgen:gen_list_of_length(Size, catena_gen:constant(x))
    end),
    Generator = catena_perf:bounded_generator(3, Base),
    Tree = catena_gen:run(Generator, 10, catena_gen:seed_from_int(2)),
    ?assertEqual(3, length(catena_tree:root(Tree))),
    ok.

benchmark_generator_reports_iterations_test() ->
    Report = catena_perf:benchmark_generator(
        "constant_bench",
        catena_gen:constant(ok),
        25
    ),
    ?assertEqual(generator, maps:get(kind, Report)),
    ?assertEqual(25, maps:get(iterations, Report)),
    ?assert(maps:get(elapsed_us, Report) >= 0),
    ok.

benchmark_shrinking_returns_minimal_failure_test() ->
    Tree = catena_tree:unfold(100, fun(N) ->
        {N, [Shrink || Shrink <- catena_shrink:shrink_binary(N, 0), Shrink =/= N]}
    end),
    Report = catena_perf:benchmark_shrinking(
        "ordered_shrink",
        fun(N) -> N > 5 end,
        Tree
    ),
    ?assertEqual(shrinking, maps:get(kind, Report)),
    ?assertMatch({found, 6, _}, maps:get(result, Report)),
    ok.

benchmark_properties_uses_parallel_runner_test() ->
    Prop1 = catena_property:new(
        "prop_a",
        catena_gen:constant(a),
        fun(_) -> true end
    ),
    Prop2 = catena_property:new(
        "prop_b",
        catena_gen:constant(b),
        fun(_) -> true end
    ),
    Report = catena_perf:benchmark_properties(
        "parallel_batch",
        [Prop1, Prop2],
        #{
            num_tests => 10,
            seed => catena_gen:seed_from_int(5),
            parallel => true,
            timeout => 5000
        }
    ),
    Batch = maps:get(batch, Report),
    ?assertEqual(2, maps:get(total, Batch)),
    ?assertEqual(2, maps:get(passed, Batch)),
    ?assertEqual(
        [<<"prop_a">>, <<"prop_b">>],
        [Name || {Name, _} <- maps:get(results, Batch)]
    ),
    ok.
