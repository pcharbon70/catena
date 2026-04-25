%% @doc Integration Tests for Phase 7 Advanced Features
-module(catena_phase7_integration_tests).

-include_lib("eunit/include/eunit.hrl").

derived_generators_coverage_and_laws_integration_test() ->
    catena_derive:clear_registry(),
    TreeSpec = #{
        kind => recursive,
        base => [
            #{tag => leaf, args => [integer]}
        ],
        recursive => [
            #{tag => node, args => [self, self]}
        ]
    },
    Generator = catena_derive:derive_generator(TreeSpec, #{max_depth => 3}),
    Sample = catena_gen:sample(Generator, 16),
    Session0 = lists:foldl(
        fun(Value, Acc) ->
            catena_coverage:record_case(Acc, Value, tree_branches(Value))
        end,
        catena_coverage:new([{tree, 1}, {tree, 2}]),
        Sample
    ),
    GuidedGenerator = catena_coverage:guided_generator(
        Generator,
        Session0,
        fun(Value) -> {guided, {node, Value, Value}} end
    ),
    GuidedSample = catena_gen:sample(GuidedGenerator, 8),
    Session = lists:foldl(
        fun(Value, Acc) ->
            catena_coverage:record_case(Acc, Value, tree_branches(normalize_guided(Value)))
        end,
        Session0,
        GuidedSample
    ),
    Report = catena_coverage:coverage_report(Session),
    ?assertEqual([], maps:get(uncovered_branches, Report)),

    Adapter = #{
        combine => fun(A, B) -> {node, A, B} end,
        equals => fun(A, B) -> A =:= B end
    },
    LawResult = catena_law_tests:run_law_tests(<<"derived_tree">>, #{
        generator => Generator,
        adapter => Adapter,
        traits => [semigroup]
    }),
    ?assert(is_map(LawResult)),
    ?assert(maps:get(total, LawResult) >= 1),
    ok.

metamorphic_compiler_like_normalization_test() ->
    SourceGenerator = catena_gen:gen_map(
        fun(Tokens) -> string:join(Tokens, " ") end,
        catena_stdgen:gen_list_of(
            catena_range:range_constant({1, 4}),
            catena_gen:elements(["alpha", "beta", "gamma"])
        )
    ),
    Property = catena_metamorphic:metamorphic(
        "source_normalization",
        SourceGenerator,
        #{
            subject => fun(Source) -> normalize_source(Source) end,
            relations => [
                catena_metamorphic:identity(fun(Source) -> inject_whitespace(Source) end)
            ]
        }
    ),
    {passed, _} = catena_runner:run_property(
        Property,
        #{num_tests => 30, seed => catena_gen:seed_from_int(47)}
    ),
    ok.

type_directed_roundtrip_and_benchmark_integration_test() ->
    Property = catena_props:roundtrip(
        "codec_roundtrip",
        fun(Value) -> term_to_binary(Value) end,
        fun(Binary) -> binary_to_term(Binary) end,
        catena_stdgen:gen_tuple3(
            catena_gen:gen_int_range(-20, 20),
            catena_gen:gen_bool(),
            catena_stdgen:gen_string(catena_range:range_constant({1, 4}))
        )
    ),
    Report = catena_perf:benchmark_properties(
        "codec_suite",
        [Property],
        #{
            num_tests => 250,
            seed => catena_gen:seed_from_int(53),
            parallel => true,
            timeout => 5000
        }
    ),
    Batch = maps:get(batch, Report),
    ?assertEqual(1, maps:get(total, Batch)),
    ?assertEqual(1, maps:get(passed, Batch)),
    ?assert(maps:get(throughput, Report) > 0),
    ok.

performance_helpers_scale_reasonably_test() ->
    Generator = catena_perf:cached_generator(
        phase7_cached,
        catena_perf:bounded_generator(
            6,
            catena_gen:sized(fun(Size) ->
                catena_stdgen:gen_list_of_length(Size, catena_gen:gen_int_range(-5, 5))
            end)
        )
    ),
    GenReport = catena_perf:benchmark_generator("cached_lists", Generator, 250),
    Tree = catena_tree:unfold(64, fun(N) ->
        {N, [Shrink || Shrink <- catena_shrink:shrink_binary(N, 0), Shrink =/= N]}
    end),
    ShrinkReport = catena_perf:benchmark_shrinking(
        "shrink_binary",
        fun(N) -> N > 3 end,
        Tree
    ),
    {found, MinValue, _} = maps:get(result, ShrinkReport),
    ?assertEqual(generator, maps:get(kind, GenReport)),
    ?assertEqual(shrinking, maps:get(kind, ShrinkReport)),
    ?assert(MinValue > 3),
    ok.

tree_branches({leaf, _}) ->
    [{tree, 1}];
tree_branches({node, _, _}) ->
    [{tree, 2}].

normalize_guided({guided, Value}) ->
    Value;
normalize_guided(Value) ->
    Value.

normalize_source(Source) ->
    Lower = string:lowercase(Source),
    Tokens = [Token || Token <- string:tokens(Lower, " \n\t"), Token =/= []],
    string:join(Tokens, " ").

inject_whitespace(Source) ->
    "  " ++ string:replace(Source, " ", "   \n ", all) ++ "  ".
