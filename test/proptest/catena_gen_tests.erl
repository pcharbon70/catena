%% @doc Unit Tests for Generator Type and Seed Management (Section 1.2)
-module(catena_gen_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test: Generator type and run/3
%%====================================================================

run_executes_generator_with_size_and_seed_test() ->
    Generator = random_word_generator(),
    Seed = catena_gen:seed_from_int(1234),

    Tree = catena_gen:run(Generator, 7, Seed),

    ?assert(is_integer(catena_tree:root(Tree))),
    ?assertEqual([7], [catena_tree:root(Child) || Child <- catena_tree:children(Tree)]).

same_seed_produces_same_tree_test() ->
    Generator = random_word_generator(),
    Seed = catena_gen:seed_from_int(42),

    Tree1 = catena_gen:run(Generator, 10, Seed),
    Tree2 = catena_gen:run(Generator, 10, Seed),

    ?assertEqual(tree_signature(Tree1), tree_signature(Tree2)).

different_seeds_produce_different_values_test() ->
    Generator = random_word_generator(),

    Tree1 = catena_gen:run(Generator, 10, catena_gen:seed_from_int(1)),
    Tree2 = catena_gen:run(Generator, 10, catena_gen:seed_from_int(2)),

    ?assertNotEqual(catena_tree:root(Tree1), catena_tree:root(Tree2)).

seed_new_produces_usable_seed_test() ->
    {Value, NextSeed} = catena_gen:seed_next(catena_gen:seed_new()),
    {NextValue, _} = catena_gen:seed_next(NextSeed),

    ?assert(is_integer(Value)),
    ?assert(is_integer(NextValue)).

%%====================================================================
%% Test: Seed operations
%%====================================================================

seed_from_int_is_deterministic_test() ->
    Seed1 = catena_gen:seed_from_int(99),
    Seed2 = catena_gen:seed_from_int(99),

    Sequence1 = next_values(Seed1, 4),
    Sequence2 = next_values(Seed2, 4),

    ?assertEqual(Sequence1, Sequence2).

seed_split_is_deterministic_for_same_input_test() ->
    Seed = catena_gen:seed_from_int(777),

    {LeftA, RightA} = catena_gen:seed_split(Seed),
    {LeftB, RightB} = catena_gen:seed_split(Seed),

    ?assertEqual(next_values(LeftA, 3), next_values(LeftB, 3)),
    ?assertEqual(next_values(RightA, 3), next_values(RightB, 3)).

seed_split_produces_distinct_streams_test() ->
    {LeftSeed, RightSeed} = catena_gen:seed_split(catena_gen:seed_from_int(2026)),

    LeftValues = next_values(LeftSeed, 3),
    RightValues = next_values(RightSeed, 3),

    ?assertNotEqual(LeftValues, RightValues).

%%====================================================================
%% Test: Size management
%%====================================================================

sized_receives_current_size_test() ->
    Generator = size_echo_generator(),

    Tree = catena_gen:run(Generator, 13, catena_gen:seed_from_int(1)),

    ?assertEqual(13, catena_tree:root(Tree)).

resize_overrides_current_size_test() ->
    Generator = catena_gen:resize(5, size_echo_generator()),

    Tree = catena_gen:run(Generator, 99, catena_gen:seed_from_int(1)),

    ?assertEqual(5, catena_tree:root(Tree)).

scale_transforms_current_size_test() ->
    Generator = catena_gen:scale(fun(Size) -> Size div 2 end, size_echo_generator()),

    Tree = catena_gen:run(Generator, 9, catena_gen:seed_from_int(1)),

    ?assertEqual(4, catena_tree:root(Tree)).

%%====================================================================
%% Helpers
%%====================================================================

random_word_generator() ->
    catena_gen:new(fun(Size, Seed) ->
        {Value, _NextSeed} = catena_gen:seed_next(Seed),
        catena_tree:tree(Value, fun() ->
            [catena_tree:singleton(Size)]
        end)
    end).

size_echo_generator() ->
    catena_gen:sized(fun(Size) ->
        catena_gen:new(fun(_IgnoredSize, _Seed) ->
            catena_tree:singleton(Size)
        end)
    end).

tree_signature(Tree) ->
    {catena_tree:root(Tree), [catena_tree:root(Child) || Child <- catena_tree:children(Tree)]}.

next_values(_Seed, 0) ->
    [];
next_values(Seed, Count) when Count > 0 ->
    {Value, NextSeed} = catena_gen:seed_next(Seed),
    [Value | next_values(NextSeed, Count - 1)].
