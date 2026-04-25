%% @doc Unit Tests for Phase 7.2: Coverage-Guided Generation
-module(catena_coverage_tests).

-include_lib("eunit/include/eunit.hrl").

new_and_record_case_tracks_branch_hits_test() ->
    Session0 = catena_coverage:new([{demo, 1}, {demo, 2}, {demo, 3}]),
    Session1 = catena_coverage:record_case(Session0, first_input, [{demo, 1}, {demo, 1}, {demo, 2}]),
    Report = catena_coverage:coverage_report(Session1),
    ?assertEqual(2, maps:get(covered_branches, Report)),
    ?assertEqual([{demo, 3}], maps:get(uncovered_branches, Report)),
    ?assertEqual(1, maps:get({demo, 1}, maps:get(branch_hits, Report))),
    ?assertEqual(1, maps:get({demo, 2}, maps:get(branch_hits, Report))),
    ok.

module_filter_ignores_irrelevant_modules_test() ->
    Session0 = catena_coverage:new(
        [{demo, 10}, {demo, 20}, {other, 10}],
        #{modules => [demo]}
    ),
    Session1 = catena_coverage:record_case(Session0, filtered_input, [{demo, 10}, {other, 10}]),
    Report = catena_coverage:coverage_report(Session1),
    ?assertEqual(2, maps:get(total_branches, Report)),
    ?assertEqual([{demo, 20}], maps:get(uncovered_branches, Report)),
    ?assertEqual([filtered_input], maps:get(prioritized_inputs, Report)),
    ok.

prioritized_inputs_prefers_rare_branch_inputs_test() ->
    Session0 = catena_coverage:new([{demo, 1}, {demo, 2}, {demo, 3}]),
    Session1 = catena_coverage:record_case(Session0, common_a, [{demo, 1}]),
    Session2 = catena_coverage:record_case(Session1, common_b, [{demo, 1}]),
    Session3 = catena_coverage:record_case(Session2, rare, [{demo, 2}]),
    [Top | _] = catena_coverage:prioritized_inputs(Session3),
    ?assertEqual(rare, Top),
    ok.

guided_inputs_mutates_prioritized_candidates_test() ->
    Session0 = catena_coverage:new([{demo, 1}, {demo, 2}]),
    Session1 = catena_coverage:record_case(Session0, rare, [{demo, 2}]),
    Session2 = catena_coverage:record_case(Session1, common, [{demo, 1}]),
    Guided = catena_coverage:guided_inputs(Session2, fun(Input) -> {guided, Input} end, 2),
    ?assertEqual([{guided, rare}, {guided, common}], Guided),
    ok.

guided_generator_biases_toward_mutated_candidates_test() ->
    Session0 = catena_coverage:new([{demo, 1}, {demo, 2}]),
    Session1 = catena_coverage:record_case(Session0, seed_value, [{demo, 2}]),
    Generator = catena_coverage:guided_generator(
        catena_gen:constant(base_value),
        Session1,
        fun(Input) -> {guided, Input} end
    ),
    Sample = catena_gen:sample(Generator, 20),
    ?assert(lists:any(fun(Value) -> Value =:= {guided, seed_value} end, Sample)),
    ok.

coverage_report_flags_distant_uncovered_branches_test() ->
    Session0 = catena_coverage:new([{demo, 10}, {demo, 12}, {demo, 40}]),
    Session1 = catena_coverage:record_case(Session0, nearby, [{demo, 10}, {demo, 12}]),
    Report = catena_coverage:coverage_report(Session1),
    ?assertEqual([{demo, 40}], maps:get(suspected_unreachable_branches, Report)),
    ok.

module_lines_reads_cover_analysis_test() ->
    cover:stop(),
    {ok, _} = cover:start(),
    {ok, catena_coverage_fixture} = compile:file(
        "test/proptest/catena_coverage_fixture.erl",
        [debug_info, {outdir, "_build/test"}]
    ),
    Beam = code:which(catena_coverage_fixture),
    {module, catena_coverage_fixture} = cover:compile_beam(Beam),
    negative = catena_coverage_fixture:classify(-1),
    zero = catena_coverage_fixture:classify(0),
    even = catena_coverage_fixture:classify(2),
    {ok, Lines} = catena_coverage:module_lines(catena_coverage_fixture),
    ?assert(lists:any(fun({_Line, Hits}) -> Hits > 0 end, Lines)),
    cover:stop(),
    ok.
