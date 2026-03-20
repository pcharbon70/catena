%% @doc First-class range values for Catena property testing.
%%
%% This module implements Property Testing Phase 1, Section 1.5.
%% Ranges separate "what values are allowed" from "how bounds scale with size",
%% while also carrying the origin that generated values shrink toward.
-module(catena_range).

-export_type([range/0]).

-export([
    is_range/1,
    range_bounds/2,
    range_origin/1,
    range_constant/1,
    range_linear/2,
    range_linear_from/3,
    range_exponential/2,
    range_exponential_from/3
]).

-define(DEFAULT_RANGE_MAX_SIZE, 100).

-record(range, {
    origin :: integer(),
    min :: integer(),
    max :: integer(),
    scale_fn :: fun((size(), integer(), integer()) -> integer())
}).

-type size() :: non_neg_integer().
-opaque range() :: #range{
    origin :: integer(),
    min :: integer(),
    max :: integer(),
    scale_fn :: fun((size(), integer(), integer()) -> integer())
}.

%%====================================================================
%% API Functions
%%====================================================================

-spec is_range(term()) -> boolean().
is_range(#range{}) ->
    true;
is_range(_) ->
    false.

%% @doc Get the computed bounds for a range at a given size.
-spec range_bounds(range(), size()) -> {integer(), integer()}.
range_bounds(Range = #range{}, Size) when is_integer(Size), Size >= 0 ->
    {ScaledMin0, ScaledMax0} = scaled_bounds(Range, Size),
    {erlang:min(ScaledMin0, ScaledMax0), erlang:max(ScaledMin0, ScaledMax0)};
range_bounds(#range{}, Size) ->
    erlang:error({badarg, {size, Size}});
range_bounds(Range, _Size) ->
    erlang:error({badarg, {range, Range}}).

%% @doc Get the shrink origin for a range.
-spec range_origin(range()) -> integer().
range_origin(#range{origin = Origin}) ->
    Origin;
range_origin(Range) ->
    erlang:error({badarg, {range, Range}}).

%% @doc Construct a size-invariant range from a single value or fixed bounds.
-spec range_constant(integer() | {integer(), integer()}) -> range().
range_constant(Value) when is_integer(Value) ->
    new_range(Value, Value, Value, fun(_Size, _Origin, Bound) -> Bound end);
range_constant({Min, Max}) when is_integer(Min), is_integer(Max), Min =< Max ->
    Origin = clamp(0, Min, Max),
    new_range(Origin, Min, Max, fun(_Size, _Origin, Bound) -> Bound end);
range_constant(Bounds) ->
    erlang:error({badarg, {range_constant, Bounds}}).

%% @doc Construct a linearly scaled range with the default origin.
-spec range_linear(integer(), integer()) -> range().
range_linear(Min, Max) when is_integer(Min), is_integer(Max), Min =< Max ->
    range_linear_from(clamp(0, Min, Max), Min, Max);
range_linear(Min, Max) ->
    erlang:error({badarg, {range_linear, {Min, Max}}}).

%% @doc Construct a linearly scaled range with an explicit origin.
-spec range_linear_from(integer(), integer(), integer()) -> range().
range_linear_from(Origin, Min, Max)
    when is_integer(Origin), is_integer(Min), is_integer(Max), Min =< Max ->
    new_range(Origin, Min, Max, fun(Size, Origin0, Bound) ->
        scale_linear(Size, Origin0, Bound)
    end);
range_linear_from(Origin, Min, Max) ->
    erlang:error({badarg, {range_linear_from, {Origin, Min, Max}}}).

%% @doc Construct an exponentially scaled range with the default origin.
-spec range_exponential(integer(), integer()) -> range().
range_exponential(Min, Max) when is_integer(Min), is_integer(Max), Min =< Max ->
    range_exponential_from(clamp(0, Min, Max), Min, Max);
range_exponential(Min, Max) ->
    erlang:error({badarg, {range_exponential, {Min, Max}}}).

%% @doc Construct an exponentially scaled range with an explicit origin.
-spec range_exponential_from(integer(), integer(), integer()) -> range().
range_exponential_from(Origin, Min, Max)
    when is_integer(Origin), is_integer(Min), is_integer(Max), Min =< Max ->
    new_range(Origin, Min, Max, fun(Size, Origin0, Bound) ->
        scale_exponential(Size, Origin0, Bound)
    end);
range_exponential_from(Origin, Min, Max) ->
    erlang:error({badarg, {range_exponential_from, {Origin, Min, Max}}}).

%%====================================================================
%% Internal Helpers
%%====================================================================

-spec new_range(
    integer(),
    integer(),
    integer(),
    fun((size(), integer(), integer()) -> integer())
) -> range().
new_range(Origin, Min, Max, ScaleFun)
    when is_integer(Origin),
         is_integer(Min),
         is_integer(Max),
         Min =< Max,
         is_function(ScaleFun, 3) ->
    case Origin >= Min andalso Origin =< Max of
        true ->
            #range{
                origin = Origin,
                min = Min,
                max = Max,
                scale_fn = ScaleFun
            };
        false ->
            erlang:error({badarg, {range_origin, {Origin, Min, Max}}})
    end.

-spec scaled_bounds(range(), size()) -> {integer(), integer()}.
scaled_bounds(#range{origin = Origin, min = Min, max = Max, scale_fn = ScaleFun}, Size) ->
    ScaledMin = clamp(ScaleFun(Size, Origin, Min), Min, Max),
    ScaledMax = clamp(ScaleFun(Size, Origin, Max), Min, Max),
    {ScaledMin, ScaledMax}.

-spec scale_linear(size(), integer(), integer()) -> integer().
scale_linear(Size0, Origin, Bound) ->
    Size = normalize_range_size(Size0),
    Delta = Bound - Origin,
    Origin + trunc((Delta * Size) / ?DEFAULT_RANGE_MAX_SIZE).

-spec scale_exponential(size(), integer(), integer()) -> integer().
scale_exponential(Size0, Origin, Bound) ->
    Size = normalize_range_size(Size0),
    Delta = Bound - Origin,
    case Delta of
        0 ->
            Origin;
        _ ->
            Magnitude = abs(Delta) + 1,
            Growth = math:pow(Magnitude, Size / ?DEFAULT_RANGE_MAX_SIZE) - 1,
            Origin + (sign(Delta) * round(Growth))
    end.

-spec normalize_range_size(size()) -> size().
normalize_range_size(Size) when Size =< ?DEFAULT_RANGE_MAX_SIZE ->
    Size;
normalize_range_size(_Size) ->
    ?DEFAULT_RANGE_MAX_SIZE.

-spec clamp(integer(), integer(), integer()) -> integer().
clamp(Value, Min, _Max) when Value < Min ->
    Min;
clamp(Value, _Min, Max) when Value > Max ->
    Max;
clamp(Value, _Min, _Max) ->
    Value.

-spec sign(integer()) -> -1 | 0 | 1.
sign(N) when N < 0 ->
    -1;
sign(0) ->
    0;
sign(_N) ->
    1.
