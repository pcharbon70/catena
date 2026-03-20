%% @doc Generator Type and Seed Management for Catena property testing.
%%
%% This module implements Property Testing Phase 1, Sections 1.2 and 1.3.
%% Generators are explicit values wrapping functions of `(Size, Seed) -> Tree`,
%% where:
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

%% API exports - Section 1.3.1: Functor Instance
-export([
    gen_map/2,
    gen_map2/3,
    gen_map3/4,
    gen_map4/5
]).

%% API exports - Section 1.3.2: Applicative Instance
-export([
    gen_pure/1,
    gen_ap/2
]).

%% API exports - Section 1.3.3: Monad Instance
-export([
    gen_bind/2,
    gen_flatten/1
]).

%% API exports - Section 1.3.4: Alternative Instance
-export([
    gen_empty/0,
    gen_alt/2,
    gen_one_of/1,
    gen_frequency/1
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

-type weighted_generator(A) :: {pos_integer(), generator(A)}.

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
%% API Functions - Section 1.3.1
%%====================================================================

%% @doc Map over generated values while preserving the shrink tree.
-spec gen_map(fun((A) -> B), generator(A)) -> generator(B).
gen_map(F, Generator) when is_function(F, 1) ->
    new(fun(Size, Seed) ->
        catena_tree:map(F, run(Generator, Size, Seed))
    end).

%% @doc Applicative-style two-argument mapping.
-spec gen_map2(fun((A, B) -> C), generator(A), generator(B)) -> generator(C).
gen_map2(F, GeneratorA, GeneratorB) when is_function(F, 2) ->
    gen_ap(
        gen_map(fun(A) -> fun(B) -> F(A, B) end end, GeneratorA),
        GeneratorB
    ).

%% @doc Applicative-style three-argument mapping.
-spec gen_map3(fun((A, B, C) -> D), generator(A), generator(B), generator(C)) -> generator(D).
gen_map3(F, GeneratorA, GeneratorB, GeneratorC) when is_function(F, 3) ->
    gen_ap(
        gen_map2(fun(A, B) -> fun(C) -> F(A, B, C) end end, GeneratorA, GeneratorB),
        GeneratorC
    ).

%% @doc Applicative-style four-argument mapping.
-spec gen_map4(
    fun((A, B, C, D) -> E),
    generator(A),
    generator(B),
    generator(C),
    generator(D)
) -> generator(E).
gen_map4(F, GeneratorA, GeneratorB, GeneratorC, GeneratorD) when is_function(F, 4) ->
    gen_ap(
        gen_map3(
            fun(A, B, C) -> fun(D) -> F(A, B, C, D) end end,
            GeneratorA,
            GeneratorB,
            GeneratorC
        ),
        GeneratorD
    ).

%%====================================================================
%% API Functions - Section 1.3.2
%%====================================================================

%% @doc Lift a value into the generator context.
-spec gen_pure(A) -> generator(A).
gen_pure(Value) ->
    new(fun(_Size, _Seed) ->
        catena_tree:pure(Value)
    end).

%% @doc Apply a generator of functions to a generator of values.
%%
%% Applicative combination derives independent branch-local randomness by
%% splitting the input seed before running the function and value generators.
-spec gen_ap(generator(fun((A) -> B)), generator(A)) -> generator(B).
gen_ap(GeneratorF, GeneratorA) ->
    new(fun(Size, Seed) ->
        {FunctionSeed, ValueSeed} = seed_split(Seed),
        FunctionTree = run(GeneratorF, Size, FunctionSeed),
        ValueTree = run(GeneratorA, Size, ValueSeed),
        catena_tree:ap(FunctionTree, ValueTree)
    end).

%%====================================================================
%% API Functions - Section 1.3.3
%%====================================================================

%% @doc Bind a generator to a dependent generator-producing function.
%%
%% The dependent generator is re-run for shrunk outer values through the rose
%% tree monad. This preserves the expected shrinking caveat for dependent
%% generation: shrinking the first value does not automatically discover new
%% shrinks in the second beyond re-running the dependency.
-spec gen_bind(generator(A), fun((A) -> generator(B))) -> generator(B).
gen_bind(Generator, NextFun) when is_function(NextFun, 1) ->
    new(fun(Size, Seed) ->
        catena_tree:bind(
            run(Generator, Size, Seed),
            fun(Value) ->
                run(NextFun(Value), Size, Seed)
            end
        )
    end).

%% @doc Flatten a nested generator by one layer.
-spec gen_flatten(generator(generator(A))) -> generator(A).
gen_flatten(GeneratorOfGenerators) ->
    gen_bind(GeneratorOfGenerators, fun(Generator) -> Generator end).

%%====================================================================
%% API Functions - Section 1.3.4
%%====================================================================

%% @doc Create a generator that always fails.
-spec gen_empty() -> generator(term()).
gen_empty() ->
    new(fun(_Size, _Seed) ->
        throw({generator_failed, empty})
    end).

%% @doc Choose between two generators, falling back if the first choice fails.
-spec gen_alt(generator(A), generator(A)) -> generator(A).
gen_alt(GeneratorA, GeneratorB) ->
    new(fun(Size, Seed) ->
        {ChoiceWord, BranchSeed} = seed_next(Seed),
        {Primary, Secondary} =
            case ChoiceWord band 1 of
                0 -> {GeneratorA, GeneratorB};
                1 -> {GeneratorB, GeneratorA}
            end,
        case try_run(Primary, Size, BranchSeed) of
            {ok, Tree} ->
                Tree;
            {error, _Reason} ->
                run(Secondary, Size, BranchSeed)
        end
    end).

%% @doc Uniformly choose one generator from a non-empty list.
-spec gen_one_of([generator(A)]) -> generator(A).
gen_one_of([]) ->
    gen_empty();
gen_one_of(Generators) when is_list(Generators) ->
    new(fun(Size, Seed) ->
        {Index, BranchSeed} = choose_index(Seed, length(Generators)),
        run(lists:nth(Index, Generators), Size, BranchSeed)
    end).

%% @doc Choose one generator using positive integer weights.
-spec gen_frequency([weighted_generator(A)]) -> generator(A).
gen_frequency([]) ->
    gen_empty();
gen_frequency(WeightedGenerators) when is_list(WeightedGenerators) ->
    new(fun(Size, Seed) ->
        Validated = validate_weighted_generators(WeightedGenerators),
        {ChoiceWord, BranchSeed} = seed_next(Seed),
        TotalWeight = lists:sum([Weight || {Weight, _Generator} <- Validated]),
        Target = ChoiceWord rem TotalWeight,
        Generator = choose_weighted(Target, Validated),
        run(Generator, Size, BranchSeed)
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

-spec choose_index(seed(), pos_integer()) -> {pos_integer(), seed()}.
choose_index(Seed, Count) when Count > 0 ->
    {Word, NextSeed} = seed_next(Seed),
    {1 + (Word rem Count), NextSeed}.

-spec try_run(generator(A), size(), seed()) -> {ok, catena_tree:tree(A)} | {error, term()}.
try_run(Generator, Size, Seed) ->
    try
        {ok, run(Generator, Size, Seed)}
    catch
        throw:{generator_failed, Reason} ->
            {error, Reason}
    end.

-spec validate_weighted_generators([weighted_generator(A)]) -> [weighted_generator(A)].
validate_weighted_generators(WeightedGenerators) ->
    lists:map(
        fun({Weight, Generator}) when is_integer(Weight), Weight > 0 ->
                {Weight, Generator};
           (Invalid) ->
                erlang:error({badarg, {weighted_generator, Invalid}})
        end,
        WeightedGenerators
    ).

-spec choose_weighted(non_neg_integer(), [weighted_generator(A)]) -> generator(A).
choose_weighted(Target, [{Weight, Generator} | _Rest]) when Target < Weight ->
    Generator;
choose_weighted(Target, [{Weight, _Generator} | Rest]) ->
    choose_weighted(Target - Weight, Rest).
