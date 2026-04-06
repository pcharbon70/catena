%% @doc Unit Tests for first-class range values.
-module(catena_range_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test: Range construction and bounds
%%====================================================================

range_constant_singleton_is_size_invariant_test() ->
    Range = catena_range:range_constant(42),

    ?assert(catena_range:is_range(Range)),
    ?assertEqual(42, catena_range:range_origin(Range)),
    ?assertEqual({42, 42}, catena_range:range_bounds(Range, 0)),
    ?assertEqual({42, 42}, catena_range:range_bounds(Range, 50)),
    ?assertEqual({42, 42}, catena_range:range_bounds(Range, 100)).

range_constant_bounds_are_size_invariant_test() ->
    Range = catena_range:range_constant({-10, 10}),

    ?assertEqual(0, catena_range:range_origin(Range)),
    ?assertEqual({-10, 10}, catena_range:range_bounds(Range, 0)),
    ?assertEqual({-10, 10}, catena_range:range_bounds(Range, 50)),
    ?assertEqual({-10, 10}, catena_range:range_bounds(Range, 100)).

range_linear_scales_proportionally_with_size_test() ->
    Range = catena_range:range_linear(0, 100),

    ?assertEqual({0, 0}, catena_range:range_bounds(Range, 0)),
    ?assertEqual({0, 50}, catena_range:range_bounds(Range, 50)),
    ?assertEqual({0, 100}, catena_range:range_bounds(Range, 100)).

range_linear_from_scales_both_bounds_around_origin_test() ->
    Range = catena_range:range_linear_from(0, -10, 20),

    ?assertEqual(0, catena_range:range_origin(Range)),
    ?assertEqual({0, 0}, catena_range:range_bounds(Range, 0)),
    ?assertEqual({-5, 10}, catena_range:range_bounds(Range, 50)),
    ?assertEqual({-10, 20}, catena_range:range_bounds(Range, 100)).

range_exponential_grows_monotonically_toward_full_bounds_test() ->
    Range = catena_range:range_exponential(1, 512),
    {Low0, High0} = catena_range:range_bounds(Range, 0),
    {Low25, High25} = catena_range:range_bounds(Range, 25),
    {Low50, High50} = catena_range:range_bounds(Range, 50),
    {Low75, High75} = catena_range:range_bounds(Range, 75),
    {Low100, High100} = catena_range:range_bounds(Range, 100),

    ?assertEqual({1, 1}, {Low0, High0}),
    ?assertEqual(1, Low25),
    ?assertEqual(1, Low50),
    ?assertEqual(1, Low75),
    ?assertEqual({1, 512}, {Low100, High100}),
    ?assert(High0 < High25),
    ?assert(High25 < High50),
    ?assert(High50 < High75),
    ?assert(High75 < High100).

origin_is_always_within_computed_bounds_test() ->
    Ranges = [
        catena_range:range_constant({5, 10}),
        catena_range:range_linear_from(7, 0, 20),
        catena_range:range_exponential_from(0, -128, 512)
    ],

    lists:foreach(
        fun(Range) ->
            Origin = catena_range:range_origin(Range),
            lists:foreach(
                fun(Size) ->
                    {Min, Max} = catena_range:range_bounds(Range, Size),
                    ?assert(Min =< Origin),
                    ?assert(Origin =< Max)
                end,
                [0, 1, 25, 50, 75, 100, 250]
            )
        end,
        Ranges
    ).
