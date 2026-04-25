%% @doc Small branchy fixture module for catena_coverage tests.
-module(catena_coverage_fixture).

-export([classify/1]).

classify(N) when N < 0 ->
    negative;
classify(0) ->
    zero;
classify(N) when N rem 2 =:= 0 ->
    even;
classify(_N) ->
    odd.
