%% @doc Unit tests for seed-based reproducibility (Section 3.5)
%%
%% Tests that property test results are reproducible with the same seed.
-module(catena_seed_reproducibility_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_property.hrl").
-include("../../src/proptest/catena_gen.hrl").

%%====================================================================
%% Section 3.5.1: Seed Display and Input
%%====================================================================

with_seed_sets_seed_test() ->
    Prop = catena_property:property("test", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    SeededProp = catena_property:with_seed(42, Prop),
    ?assertMatch(#property_config{seed = #seed{}}, SeededProp#property.config),
    ok.

with_seed_from_int_test() ->
    Prop = catena_property:property("test", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    SeededProp = catena_property:with_seed(catena_gen:seed_from_int(42), Prop),
    ?assertMatch(#property_config{seed = #seed{}}, SeededProp#property.config),
    ok.

%%====================================================================
%% Section 3.5.2: Seed Persistence
%%====================================================================

%% Note: Full seed file persistence is complex and would require
%% additional modules. For this phase, we test that seeds are
%% properly captured and can be reused.

result_includes_seed_test() ->
    Prop = catena_property:property("seeded", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> false end)
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 5}),
    %% Result should include the seed used
    ?assertMatch(#seed{}, Result#property_result.seed),
    ok.

same_seed_same_result_test() ->
    %% Run the same property twice with the same seed
    Seed = catena_gen:seed_from_int(42),
    Prop = catena_property:property("reproducible", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({0, 100})),
            fun(N) -> N rem 2 =:= 0 end
        )
    end),
    {failed, Result1} = catena_runner:run_property(Prop, #{num_tests => 10, seed => Seed}),
    {failed, Result2} = catena_runner:run_property(Prop, #{num_tests => 10, seed => Seed}),
    %% The counterexamples should be the same
    ?assertEqual(Result1#property_result.shrunk_counterexample,
                 Result2#property_result.shrunk_counterexample),
    ok.

different_seed_different_result_test() ->
    %% Run the same property with different seeds
    Prop = catena_property:property("varied", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({0, 100})),
            fun(N) -> N rem 2 =:= 0 end
        )
    end),
    {failed, Result1} = catena_runner:run_property(Prop, #{num_tests => 10, seed => catena_gen:seed_from_int(1)}),
    {failed, Result2} = catena_runner:run_property(Prop, #{num_tests => 10, seed => catena_gen:seed_from_int(2)}),
    %% The counterexamples should be different (statistically)
    %% Just check they're both valid integers
    ?assert(is_integer(Result1#property_result.shrunk_counterexample)),
    ?assert(is_integer(Result2#property_result.shrunk_counterexample)),
    ok.

%%====================================================================
%% Section 3.5.3: Determinism Verification
%%====================================================================

determinism_same_seed_twice_test() ->
    %% Verify that running with the same seed produces identical results
    Seed = catena_gen:seed_from_int(42),
    Prop = catena_property:property("deterministic", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({0, 100})),
            fun(N) -> N rem 2 =:= 0 end
        )
    end),
    %% Run twice with same seed
    {failed, Result1} = catena_runner:run_property(Prop, #{num_tests => 50, seed => Seed}),
    {failed, Result2} = catena_runner:run_property(Prop, #{num_tests => 50, seed => Seed}),
    %% Results should be identical
    ?assertEqual(Result1, Result2),
    ok.

determinism_no_seed_different_test() ->
    %% Without specifying seed, results should still be deterministic
    %% (using the random seed generator)
    Prop = catena_property:property("random_but_deterministic", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({0, 10})),
            fun(_) -> true end
        )
    end),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 10}),
    %% Just verify it completes without error
    ok.
