%% @doc Generator Type and Seed Management for Catena property testing.
%%
%% This module implements Property Testing Phase 1, Section 1.2. Generators
%% are explicit values wrapping functions of `(Size, Seed) -> Tree`, where:
%%
%% - `Size` controls generation complexity
%% - `Seed` provides deterministic reproducibility
%% - `Tree` carries the generated value and its shrink tree
%%
%% The seed implementation uses a small SplitMix64-style stepping function so
%% that deterministic generation is stable across OTP's internal `rand`
%% implementations.
-module(catena_gen).

%% Type exports
-export_type([generator/1, seed/0, size/0]).

%% API exports - Section 1.2.1: Generator Type Definition
-export([
    new/1,
    run/3
]).

%% API exports - Section 1.2.2: Seed Operations
-export([
    seed_new/0,
    seed_from_int/1,
    seed_split/1,
    seed_next/1
]).

%% API exports - Section 1.2.3: Size Management
-export([
    sized/1,
    resize/2,
    scale/2
]).

-define(MASK64, 16#FFFFFFFFFFFFFFFF).
-define(SPLITMIX_GAMMA, 16#9E3779B97F4A7C15).

%%====================================================================
%% Types
%%====================================================================

-record(seed, {
    state :: non_neg_integer()
}).

-record(generator, {
    run :: fun((size(), seed()) -> catena_tree:tree(term()))
}).

-type size() :: non_neg_integer().
-opaque seed() :: #seed{state :: non_neg_integer()}.
-opaque generator(A) :: #generator{
    run :: fun((size(), seed()) -> catena_tree:tree(A))
}.

%%====================================================================
%% API Functions - Section 1.2.1
%%====================================================================

%% @doc Create a generator from a `(Size, Seed) -> Tree` function.
-spec new(fun((size(), seed()) -> catena_tree:tree(A))) -> generator(A).
new(RunFun) when is_function(RunFun, 2) ->
    #generator{run = RunFun}.

%% @doc Execute a generator with a specific size and seed.
-spec run(generator(A), size(), seed()) -> catena_tree:tree(A).
run(#generator{run = RunFun}, Size, Seed = #seed{}) when is_integer(Size), Size >= 0 ->
    RunFun(Size, Seed);
run(#generator{}, Size, _Seed) when not is_integer(Size); Size < 0 ->
    erlang:error({badarg, {size, Size}});
run(#generator{}, _Size, Seed) ->
    erlang:error({badarg, {seed, Seed}}).

%%====================================================================
%% API Functions - Section 1.2.2
%%====================================================================

%% @doc Create a non-deterministic seed from runtime entropy sources.
-spec seed_new() -> seed().
seed_new() ->
    seed_from_int(runtime_entropy()).

%% @doc Create a deterministic seed from an integer.
-spec seed_from_int(integer()) -> seed().
seed_from_int(Int) when is_integer(Int) ->
    #seed{state = normalize_u64(Int)}.

%% @doc Split a seed into two independent deterministic streams.
-spec seed_split(seed()) -> {seed(), seed()}.
seed_split(Seed = #seed{}) ->
    {Word1, Seed1} = seed_next(Seed),
    {Word2, _Seed2} = seed_next(Seed1),
    {seed_from_int(Word1), seed_from_int(Word2)}.

%% @doc Advance a seed, yielding the next pseudo-random word and seed.
-spec seed_next(seed()) -> {non_neg_integer(), seed()}.
seed_next(#seed{state = State0}) ->
    State1 = next_state(State0),
    {mix64(State1), #seed{state = State1}}.

%%====================================================================
%% API Functions - Section 1.2.3
%%====================================================================

%% @doc Build a generator that can inspect the current size.
-spec sized(fun((size()) -> generator(A))) -> generator(A).
sized(SizeFun) when is_function(SizeFun, 1) ->
    new(fun(Size, Seed) ->
        Generated = SizeFun(Size),
        run(Generated, Size, Seed)
    end).

%% @doc Override the size seen by a generator.
-spec resize(size(), generator(A)) -> generator(A).
resize(NewSize, Generator) when is_integer(NewSize), NewSize >= 0 ->
    new(fun(_CurrentSize, Seed) ->
        run(Generator, NewSize, Seed)
    end);
resize(NewSize, _Generator) ->
    erlang:error({badarg, {size, NewSize}}).

%% @doc Transform the incoming size before executing a generator.
-spec scale(fun((size()) -> size()), generator(A)) -> generator(A).
scale(SizeFun, Generator) when is_function(SizeFun, 1) ->
    new(fun(Size, Seed) ->
        ScaledSize = normalize_size(SizeFun(Size)),
        run(Generator, ScaledSize, Seed)
    end).

%%====================================================================
%% Internal Helpers
%%====================================================================

-spec normalize_size(term()) -> size().
normalize_size(Size) when is_integer(Size), Size >= 0 ->
    Size;
normalize_size(Size) ->
    erlang:error({badarg, {size, Size}}).

-spec normalize_u64(integer()) -> non_neg_integer().
normalize_u64(Int) ->
    Int band ?MASK64.

-spec next_state(non_neg_integer()) -> non_neg_integer().
next_state(State) ->
    normalize_u64(State + ?SPLITMIX_GAMMA).

-spec mix64(non_neg_integer()) -> non_neg_integer().
mix64(Value0) ->
    Value1 = normalize_u64((Value0 bxor (Value0 bsr 30)) * 16#BF58476D1CE4E5B9),
    Value2 = normalize_u64((Value1 bxor (Value1 bsr 27)) * 16#94D049BB133111EB),
    normalize_u64(Value2 bxor (Value2 bsr 31)).

-spec runtime_entropy() -> integer().
runtime_entropy() ->
    Time = normalize_u64(erlang:system_time(nanosecond)),
    Unique = normalize_u64(erlang:unique_integer([monotonic, positive])),
    Hash = normalize_u64(erlang:phash2({node(), self(), make_ref()}, 16#100000000)),
    normalize_u64(Time bxor (Unique bsl 17) bxor (Hash bsl 1)).
