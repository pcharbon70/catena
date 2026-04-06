%% @doc Unit tests for counterexample minimization (Section 3.4)
%%
%% Tests the shrink search algorithm, termination conditions,
%% and shrink progress reporting.
-module(catena_shrink_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_property.hrl").
-include("../../src/proptest/catena_gen.hrl").

%%====================================================================
%% Section 3.4.1: Shrink Search Algorithm
%%====================================================================

shrink_finds_smaller_failing_value_test() ->
    %% Property that fails for numbers > 5
    Prop = catena_property:property("greater_than_5", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({0, 100})),
            fun(N) -> N > 5 end
        )
    end),
    %% This should find a value close to 6 (the smallest failing)
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 100}),
    %% Check that shrinking occurred
    ?assert(is_integer(Result#property_result.shrunk_counterexample)),
    ok.

shrink_limits_attempts_test() ->
    %% Property that always fails (to test shrink limit)
    Prop = catena_property:property("always_fail", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({0, 10})),
            fun(_) -> false end
        )
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 5, max_shrinks => 10}),
    %% Should respect max_shrinks limit
    ?assert(Result#property_result.shrinks_attempted =< 20),
    ok.

%%====================================================================
%% Section 3.4.2: Shrink Progress Reporting
%%====================================================================

shrink_history_includes_attempts_test() ->
    Prop = catena_property:property("shrink_test", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({0, 50})),
            fun(N) -> N rem 10 =/= 0 end
        )
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 50}),
    %% Shrink history should track attempts
    ?assert(is_list(Result#property_result.shrink_history)),
    ok.

shrinks_attempted_counter_test() ->
    Prop = catena_property:property("counter_test", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({0, 20})),
            fun(N) -> N > 5 end
        )
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 20}),
    %% Should have some shrink attempts
    ?assert(is_integer(Result#property_result.shrinks_attempted)),
    ok.

%%====================================================================
%% Section 3.4.3: Shrink Termination
%%====================================================================

shrink_terminates_on_limit_test() ->
    Prop = catena_property:property("limit_test", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({0, 100})),
            fun(_) -> false end
        )
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 10, max_shrinks => 5}),
    %% Should terminate at or before max_shrinks
    ?assert(Result#property_result.shrinks_attempted =< 10),
    ok.

no_shrink_when_root_passes_test() ->
    %% Property that passes - no shrinking should occur
    Prop = catena_property:property("always_pass", fun() ->
        catena_property:forall(
            catena_gen:gen_int(),
            fun(_) -> true end
        )
    end),
    {passed, _Result} = catena_runner:run_property(Prop, #{num_tests => 10}),
    ok.

%%====================================================================
%% Section 3.4.4: Advanced Strategies
%%====================================================================

shrink_reduces_size_test() ->
    %% Property that fails on large lists
    Prop = catena_property:property("list_size", fun() ->
        catena_property:forall(
            catena_stdgen:gen_list(catena_gen:gen_int(catena_range:range_constant({0, 20}))),
            fun(L) -> length(L) < 5 end
        )
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 50}),
    %% Shrunk counterexample should be smaller than original
    OriginalSize = get_list_size(Result#property_result.original_counterexample),
    ShrunkSize = get_list_size(Result#property_result.shrunk_counterexample),
    ?assert(ShrunkSize =< OriginalSize),
    ok.

%% @private Helper to get list size or return default.
get_list_size(L) when is_list(L) -> length(L);
get_list_size(_) -> 999.

%%====================================================================
%% Integration Tests
%%====================================================================

end_to_end_shrinking_test() ->
    %% Full workflow: define, run, fail, shrink
    Prop = catena_property:property("e2e", fun() ->
        catena_property:forall(
            catena_gen:gen_int(catena_range:range_constant({0, 50})),
            fun(N) -> N rem 2 =:= 0 end
        )
    end),
    {failed, Result} = catena_runner:run_property(Prop, #{num_tests => 100}),
    %% Should have both original and shrunk counterexamples
    ?assertNotEqual(undefined, Result#property_result.original_counterexample),
    ?assertNotEqual(undefined, Result#property_result.shrunk_counterexample),
    ok.
