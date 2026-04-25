%% @doc Performance helpers for Phase 7.5.
%%
%% This module provides pragmatic optimization helpers and lightweight
%% benchmarks without changing the property DSL surface.
-module(catena_perf).

-include("catena_property.hrl").

-export([
    cached_generator/2,
    bounded_generator/2,
    clear_cache/0,
    cache_stats/0,
    benchmark_generator/3,
    benchmark_shrinking/3,
    benchmark_properties/3
]).

-define(CACHE_TABLE, catena_perf_cache).
-define(HITS_KEY, {metric, hits}).
-define(MISSES_KEY, {metric, misses}).

-spec cached_generator(term(), catena_gen:generator(A)) -> catena_gen:generator(A).
cached_generator(Name, Generator) ->
    ensure_cache(),
    catena_gen:new(fun(Size, Seed) ->
        Key = {Name, Size, Seed},
        case ets:lookup(?CACHE_TABLE, Key) of
            [{Key, Tree}] ->
                bump(?HITS_KEY),
                Tree;
            [] ->
                Tree = catena_gen:run(Generator, Size, Seed),
                ets:insert(?CACHE_TABLE, {Key, Tree}),
                bump(?MISSES_KEY),
                Tree
        end
    end).

-spec bounded_generator(non_neg_integer(), catena_gen:generator(A)) -> catena_gen:generator(A).
bounded_generator(MaxSize, Generator) when is_integer(MaxSize), MaxSize >= 0 ->
    catena_gen:scale(fun(Size) -> min(Size, MaxSize) end, Generator).

-spec clear_cache() -> ok.
clear_cache() ->
    ensure_cache(),
    ets:delete_all_objects(?CACHE_TABLE),
    ets:insert(?CACHE_TABLE, [{?HITS_KEY, 0}, {?MISSES_KEY, 0}]),
    ok.

-spec cache_stats() -> map().
cache_stats() ->
    ensure_cache(),
    #{
        hits => metric(?HITS_KEY),
        misses => metric(?MISSES_KEY),
        entries => max(ets:info(?CACHE_TABLE, size) - 2, 0)
    }.

-spec benchmark_generator(binary() | string(), catena_gen:generator(term()), pos_integer()) -> map().
benchmark_generator(Name, Generator, Iterations) when is_integer(Iterations), Iterations > 0 ->
    Seed = catena_gen:seed_from_int(1),
    {ElapsedUs, LastValue} = timer:tc(fun() ->
        benchmark_generator_loop(Generator, Iterations, Seed, 0, undefined)
    end),
    #{
        name => normalize_name(Name),
        kind => generator,
        iterations => Iterations,
        elapsed_us => ElapsedUs,
        ops_per_sec => ops_per_sec(Iterations, ElapsedUs),
        last_value => LastValue
    }.

-spec benchmark_shrinking(binary() | string(), fun((term()) -> boolean()), catena_tree:tree(term())) -> map().
benchmark_shrinking(Name, Predicate, Tree) when is_function(Predicate, 1) ->
    {ElapsedUs, Result} = timer:tc(fun() ->
        catena_shrink:find_minimal(Predicate, Tree, #{max_attempts => 1000})
    end),
    #{
        name => normalize_name(Name),
        kind => shrinking,
        elapsed_us => ElapsedUs,
        result => Result
    }.

-spec benchmark_properties(binary() | string(), [catena_property:property()], map()) -> map().
benchmark_properties(Name, Properties, Options) when is_list(Properties), is_map(Options) ->
    {ElapsedUs, Batch} = timer:tc(fun() ->
        catena_runner:run_properties(Properties, Options)
    end),
    Total = max(maps:get(total, Batch, 0), 1),
    #{
        name => normalize_name(Name),
        kind => properties,
        elapsed_us => ElapsedUs,
        properties => maps:get(total, Batch, 0),
        throughput => ops_per_sec(Total, ElapsedUs),
        batch => Batch
    }.

benchmark_generator_loop(_Generator, 0, _Seed, _Size, LastValue) ->
    LastValue;
benchmark_generator_loop(Generator, Remaining, Seed, Size, _LastValue) ->
    {RunSeed, NextSeed} = catena_gen:seed_split(Seed),
    Tree = catena_gen:run(Generator, Size, RunSeed),
    benchmark_generator_loop(Generator, Remaining - 1, NextSeed, Size + 1, catena_tree:root(Tree)).

ops_per_sec(_Count, 0) ->
    infinity;
ops_per_sec(Count, ElapsedUs) ->
    (Count * 1000000.0) / ElapsedUs.

ensure_cache() ->
    case ets:info(?CACHE_TABLE) of
        undefined ->
            ets:new(?CACHE_TABLE, [named_table, public, set]),
            ets:insert(?CACHE_TABLE, [{?HITS_KEY, 0}, {?MISSES_KEY, 0}]),
            ok;
        _ ->
            ok
    end.

bump(Key) ->
    ets:update_counter(?CACHE_TABLE, Key, {2, 1}).

metric(Key) ->
    case ets:lookup(?CACHE_TABLE, Key) of
        [{Key, Value}] -> Value;
        [] -> 0
    end.

normalize_name(Name) when is_binary(Name) ->
    Name;
normalize_name(Name) when is_list(Name) ->
    unicode:characters_to_binary(Name).
