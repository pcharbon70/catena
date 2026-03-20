%% @doc Unit Tests for Generator Type, Seed Management, and Generator Instances
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

    ?assertEqual(flat_tree_signature(Tree1), flat_tree_signature(Tree2)).

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
%% Test: Functor instance
%%====================================================================

gen_map_transforms_values_and_shrinks_test() ->
    Generator = fixed_generator(3, [1, 0]),
    Mapped = catena_gen:gen_map(fun(X) -> X * 2 end, Generator),

    Tree = catena_gen:run(Mapped, 0, catena_gen:seed_from_int(1)),

    ?assertEqual({6, [2, 0]}, flat_tree_signature(Tree)).

gen_map_multi_argument_helpers_test() ->
    GeneratorA = catena_gen:gen_pure(2),
    GeneratorB = catena_gen:gen_pure(3),
    GeneratorC = catena_gen:gen_pure(4),
    GeneratorD = catena_gen:gen_pure(5),

    Tree2 = catena_gen:run(
        catena_gen:gen_map2(fun(A, B) -> A + B end, GeneratorA, GeneratorB),
        0,
        catena_gen:seed_from_int(1)
    ),
    Tree3 = catena_gen:run(
        catena_gen:gen_map3(fun(A, B, C) -> A + B + C end, GeneratorA, GeneratorB, GeneratorC),
        0,
        catena_gen:seed_from_int(1)
    ),
    Tree4 = catena_gen:run(
        catena_gen:gen_map4(
            fun(A, B, C, D) -> A + B + C + D end,
            GeneratorA,
            GeneratorB,
            GeneratorC,
            GeneratorD
        ),
        0,
        catena_gen:seed_from_int(1)
    ),

    ?assertEqual(5, catena_tree:root(Tree2)),
    ?assertEqual(9, catena_tree:root(Tree3)),
    ?assertEqual(14, catena_tree:root(Tree4)).

functor_law_identity_test() ->
    Generator = fixed_generator(7, [3, 1]),
    Seed = catena_gen:seed_from_int(9),

    Left = catena_gen:run(catena_gen:gen_map(fun(X) -> X end, Generator), 0, Seed),
    Right = catena_gen:run(Generator, 0, Seed),

    ?assertEqual(tree_to_term(Right), tree_to_term(Left)).

functor_law_composition_test() ->
    Generator = fixed_generator(7, [3, 1]),
    Seed = catena_gen:seed_from_int(9),
    F = fun(X) -> X + 1 end,
    G = fun(X) -> X * 2 end,

    Left = catena_gen:run(catena_gen:gen_map(fun(X) -> F(G(X)) end, Generator), 0, Seed),
    Right = catena_gen:run(
        catena_gen:gen_map(F, catena_gen:gen_map(G, Generator)),
        0,
        Seed
    ),

    ?assertEqual(tree_to_term(Right), tree_to_term(Left)).

%%====================================================================
%% Test: Applicative instance
%%====================================================================

gen_pure_produces_constant_tree_test() ->
    Tree = catena_gen:run(catena_gen:gen_pure(hello), 10, catena_gen:seed_from_int(5)),

    ?assertEqual(hello, catena_tree:root(Tree)),
    ?assertEqual([], catena_tree:children(Tree)).

gen_ap_matches_split_seed_tree_application_test() ->
    Seed = catena_gen:seed_from_int(33),
    {FunctionSeed, ValueSeed} = catena_gen:seed_split(Seed),
    FunctionGenerator =
        catena_gen:gen_map(
            fun(Offset) ->
                fun(Value) -> {Offset, Value} end
            end,
            seed_word_generator()
        ),
    ValueGenerator = seed_word_generator(),

    Expected = catena_tree:ap(
        catena_gen:run(FunctionGenerator, 4, FunctionSeed),
        catena_gen:run(ValueGenerator, 4, ValueSeed)
    ),
    Actual = catena_gen:run(
        catena_gen:gen_ap(FunctionGenerator, ValueGenerator),
        4,
        Seed
    ),

    ?assertEqual(tree_to_term(Expected), tree_to_term(Actual)).

applicative_law_identity_test() ->
    ValueGenerator = fixed_generator(7, [3, 1]),
    Seed = catena_gen:seed_from_int(10),

    Left = catena_gen:run(
        catena_gen:gen_ap(catena_gen:gen_pure(fun(X) -> X end), ValueGenerator),
        0,
        Seed
    ),
    Right = catena_gen:run(ValueGenerator, 0, Seed),

    ?assertEqual(tree_to_term(Right), tree_to_term(Left)).

applicative_law_homomorphism_test() ->
    Seed = catena_gen:seed_from_int(10),
    F = fun(X) -> X * 3 end,

    Left = catena_gen:run(
        catena_gen:gen_ap(catena_gen:gen_pure(F), catena_gen:gen_pure(4)),
        0,
        Seed
    ),
    Right = catena_gen:run(catena_gen:gen_pure(F(4)), 0, Seed),

    ?assertEqual(tree_to_term(Right), tree_to_term(Left)).

applicative_law_interchange_test() ->
    Seed = catena_gen:seed_from_int(10),
    GeneratorU = catena_gen:gen_pure(fun(X) -> X + 2 end),
    ApplyToThree = fun(F) -> F(3) end,

    Left = catena_gen:run(
        catena_gen:gen_ap(GeneratorU, catena_gen:gen_pure(3)),
        0,
        Seed
    ),
    Right = catena_gen:run(
        catena_gen:gen_ap(catena_gen:gen_pure(ApplyToThree), GeneratorU),
        0,
        Seed
    ),

    ?assertEqual(tree_to_term(Right), tree_to_term(Left)).

applicative_law_composition_test() ->
    Seed = catena_gen:seed_from_int(10),
    Compose =
        fun(F) ->
            fun(G) ->
                fun(X) -> F(G(X)) end
            end
        end,
    GeneratorU = catena_gen:gen_pure(fun(X) -> X + 1 end),
    GeneratorV = catena_gen:gen_pure(fun(X) -> X * 2 end),
    GeneratorW = catena_gen:gen_pure(5),

    Left = catena_gen:run(
        catena_gen:gen_ap(
            catena_gen:gen_ap(
                catena_gen:gen_ap(catena_gen:gen_pure(Compose), GeneratorU),
                GeneratorV
            ),
            GeneratorW
        ),
        0,
        Seed
    ),
    Right = catena_gen:run(
        catena_gen:gen_ap(GeneratorU, catena_gen:gen_ap(GeneratorV, GeneratorW)),
        0,
        Seed
    ),

    ?assertEqual(tree_to_term(Right), tree_to_term(Left)).

%%====================================================================
%% Test: Monad instance
%%====================================================================

gen_bind_threads_dependent_generator_test() ->
    Seed = catena_gen:seed_from_int(44),
    BaseGenerator = fixed_generator(2, [1]),
    NextFun = fun(Value) ->
        catena_gen:gen_map(fun(RandomWord) -> {Value, RandomWord} end, seed_word_generator())
    end,

    Expected = catena_tree:bind(
        catena_gen:run(BaseGenerator, 3, Seed),
        fun(Value) ->
            catena_gen:run(NextFun(Value), 3, Seed)
        end
    ),
    Actual = catena_gen:run(catena_gen:gen_bind(BaseGenerator, NextFun), 3, Seed),

    ?assertEqual(tree_to_term(Expected), tree_to_term(Actual)).

gen_flatten_collapses_nested_generators_test() ->
    Inner = catena_gen:gen_pure(5),
    Outer = catena_gen:gen_pure(Inner),

    Tree = catena_gen:run(catena_gen:gen_flatten(Outer), 0, catena_gen:seed_from_int(1)),

    ?assertEqual(5, catena_tree:root(Tree)).

monad_law_left_identity_test() ->
    Seed = catena_gen:seed_from_int(12),
    NextFun = fun(Value) -> catena_gen:gen_pure(Value * 10) end,

    Left = catena_gen:run(
        catena_gen:gen_bind(catena_gen:gen_pure(3), NextFun),
        0,
        Seed
    ),
    Right = catena_gen:run(NextFun(3), 0, Seed),

    ?assertEqual(tree_to_term(Right), tree_to_term(Left)).

monad_law_right_identity_test() ->
    Seed = catena_gen:seed_from_int(12),
    Generator = fixed_generator(7, [3, 1]),

    Left = catena_gen:run(
        catena_gen:gen_bind(Generator, fun(Value) -> catena_gen:gen_pure(Value) end),
        0,
        Seed
    ),
    Right = catena_gen:run(Generator, 0, Seed),

    ?assertEqual(tree_to_term(Right), tree_to_term(Left)).

monad_law_associativity_test() ->
    Seed = catena_gen:seed_from_int(12),
    Generator = catena_gen:gen_pure(5),
    F = fun(Value) -> catena_gen:gen_pure(Value + 2) end,
    G = fun(Value) -> catena_gen:gen_pure(Value * 3) end,

    Left = catena_gen:run(
        catena_gen:gen_bind(catena_gen:gen_bind(Generator, F), G),
        0,
        Seed
    ),
    Right = catena_gen:run(
        catena_gen:gen_bind(Generator, fun(Value) ->
            catena_gen:gen_bind(F(Value), G)
        end),
        0,
        Seed
    ),

    ?assertEqual(tree_to_term(Right), tree_to_term(Left)).

%%====================================================================
%% Test: Alternative instance
%%====================================================================

gen_empty_throws_failure_test() ->
    ?assertThrow(
        {generator_failed, empty},
        catena_gen:run(catena_gen:gen_empty(), 0, catena_gen:seed_from_int(1))
    ).

gen_alt_falls_back_when_primary_choice_fails_test() ->
    Generator = catena_gen:gen_alt(catena_gen:gen_empty(), catena_gen:gen_pure(ok)),

    Values =
        [catena_tree:root(catena_gen:run(Generator, 0, catena_gen:seed_from_int(N)))
         || N <- lists:seq(1, 20)],

    ?assertEqual([ok], lists:usort(Values)).

gen_one_of_visits_all_choices_test() ->
    Generator =
        catena_gen:gen_one_of([
            catena_gen:gen_pure(alpha),
            catena_gen:gen_pure(beta),
            catena_gen:gen_pure(gamma)
        ]),

    Values =
        [catena_tree:root(catena_gen:run(Generator, 0, catena_gen:seed_from_int(N)))
         || N <- lists:seq(1, 240)],

    ?assertEqual([alpha, beta, gamma], lists:usort(Values)).

gen_frequency_biases_toward_heavier_weights_test() ->
    Generator =
        catena_gen:gen_frequency([
            {1, catena_gen:gen_pure(light)},
            {4, catena_gen:gen_pure(heavy)}
        ]),

    Values =
        [catena_tree:root(catena_gen:run(Generator, 0, catena_gen:seed_from_int(N)))
         || N <- lists:seq(1, 400)],
    LightCount = length([Value || Value <- Values, Value =:= light]),
    HeavyCount = length([Value || Value <- Values, Value =:= heavy]),

    ?assert(LightCount > 0),
    ?assert(HeavyCount > 0),
    ?assert(HeavyCount > LightCount).

%%====================================================================
%% Test: Primitive combinators
%%====================================================================

constant_produces_fixed_value_without_shrinks_test() ->
    Tree = catena_gen:run(catena_gen:constant(hello), 50, catena_gen:seed_from_int(9)),

    ?assertEqual(hello, catena_tree:root(Tree)),
    ?assertEqual([], catena_gen:shrinks(Tree)).

element_produces_values_from_the_source_list_test() ->
    Generator = catena_gen:element([alpha, beta, gamma]),
    Values =
        [catena_tree:root(catena_gen:run(Generator, 0, catena_gen:seed_from_int(N)))
         || N <- lists:seq(1, 240)],

    ?assertEqual([alpha, beta, gamma], lists:usort(Values)).

element_shrinks_toward_earlier_values_test() ->
    Generator = catena_gen:element([alpha, beta, gamma]),
    Tree = first_tree_with_root(Generator, gamma, 500),

    ?assertEqual([alpha, beta], catena_gen:shrinks(Tree)).

elements_is_an_alias_for_element_test() ->
    List = [alpha, beta, gamma],
    ElementValues =
        [catena_tree:root(catena_gen:run(catena_gen:element(List), 0, catena_gen:seed_from_int(N)))
         || N <- lists:seq(1, 30)],
    ElementsValues =
        [catena_tree:root(catena_gen:run(catena_gen:elements(List), 0, catena_gen:seed_from_int(N)))
         || N <- lists:seq(1, 30)],

    ?assertEqual(ElementValues, ElementsValues).

gen_bool_produces_both_boolean_values_test() ->
    Generator = catena_gen:gen_bool(),
    Values =
        [catena_tree:root(catena_gen:run(Generator, 0, catena_gen:seed_from_int(N)))
         || N <- lists:seq(1, 80)],

    ?assertEqual([false, true], lists:usort(Values)).

gen_bool_true_shrinks_to_false_test() ->
    Generator = catena_gen:gen_bool(),
    TrueTree = first_tree_satisfying(Generator, fun(Value) -> Value =:= true end, 200),
    FalseTree = first_tree_satisfying(Generator, fun(Value) -> Value =:= false end, 200),

    ?assertEqual([false], catena_gen:shrinks(TrueTree)),
    ?assertEqual([], catena_gen:shrinks(FalseTree)).

gen_bool_probability_biases_generation_test() ->
    Generator = catena_gen:gen_bool(0.8),
    Values =
        [catena_tree:root(catena_gen:run(Generator, 0, catena_gen:seed_from_int(N)))
         || N <- lists:seq(1, 400)],
    TrueCount = length([Value || Value <- Values, Value =:= true]),
    FalseCount = length([Value || Value <- Values, Value =:= false]),

    ?assert(TrueCount > FalseCount).

gen_int_respects_explicit_bounds_test() ->
    Generator = catena_gen:gen_int({-5, 5}),
    Values =
        [catena_tree:root(catena_gen:run(Generator, 0, catena_gen:seed_from_int(N)))
         || N <- lists:seq(1, 300)],

    ?assert(lists:all(fun(Value) -> Value >= -5 andalso Value =< 5 end, Values)).

gen_int_shrinks_toward_zero_when_available_test() ->
    Generator = catena_gen:gen_int({-10, 10}),
    Tree = first_tree_satisfying(Generator, fun(Value) -> Value =/= 0 end, 300),

    ?assert(lists:member(0, catena_gen:shrinks(Tree))).

gen_int_shrinks_toward_bound_when_zero_is_unavailable_test() ->
    Generator = catena_gen:gen_int({5, 10}),
    Tree = first_tree_satisfying(Generator, fun(Value) -> Value > 5 end, 300),

    ?assert(lists:member(5, catena_gen:shrinks(Tree))).

derived_integer_generators_respect_their_domains_test() ->
    PosValues = catena_gen:sample(catena_gen:gen_pos_int(), 20),
    NegValues = catena_gen:sample(catena_gen:gen_neg_int(), 20),
    NatValues = catena_gen:sample(catena_gen:gen_nat(), 20),

    ?assert(lists:all(fun(Value) -> Value > 0 end, PosValues)),
    ?assert(lists:all(fun(Value) -> Value < 0 end, NegValues)),
    ?assert(lists:all(fun(Value) -> Value >= 0 end, NatValues)).

gen_filter_only_produces_matching_values_test() ->
    Generator =
        catena_gen:gen_filter(
            fun(Value) -> Value rem 2 =:= 0 end,
            catena_gen:gen_int({0, 20})
        ),
    Values = catena_gen:sample(Generator, 20),

    ?assert(lists:all(fun(Value) -> Value rem 2 =:= 0 end, Values)).

gen_filter_promotes_valid_shrinks_test() ->
    BaseGenerator =
        catena_gen:new(fun(_Size, _Seed) ->
            catena_tree:tree(4, fun() ->
                [
                    catena_tree:tree(3, fun() -> [catena_tree:singleton(2)] end),
                    catena_tree:singleton(1),
                    catena_tree:singleton(0)
                ]
            end)
        end),
    FilteredGenerator =
        catena_gen:gen_filter(fun(Value) -> Value rem 2 =:= 0 end, BaseGenerator),
    Tree = catena_gen:run(FilteredGenerator, 0, catena_gen:seed_from_int(1)),

    ?assertEqual(4, catena_tree:root(Tree)),
    ?assertEqual([2, 0], catena_gen:shrinks(Tree)).

gen_such_that_aliases_gen_filter_test() ->
    BaseGenerator = catena_gen:gen_int({0, 10}),
    Predicate = fun(Value) -> Value rem 2 =:= 0 end,
    Seed = catena_gen:seed_from_int(18),
    Filtered = catena_gen:run(catena_gen:gen_filter(Predicate, BaseGenerator), 5, Seed),
    SuchThat = catena_gen:run(catena_gen:gen_such_that(Predicate, BaseGenerator), 5, Seed),

    ?assertEqual(tree_to_term(Filtered), tree_to_term(SuchThat)).

sample_returns_requested_number_of_values_test() ->
    Values = catena_gen:sample(catena_gen:constant(ok), 5),

    ?assertEqual([ok, ok, ok, ok, ok], Values).

sample_uses_increasing_sizes_test() ->
    Values = catena_gen:sample(size_echo_generator(), 4),

    ?assertEqual([0, 1, 2, 3], Values).

shrinks_returns_immediate_shrink_values_test() ->
    Tree =
        catena_tree:tree(root, fun() ->
            [catena_tree:singleton(child_one), catena_tree:singleton(child_two)]
        end),

    ?assertEqual([child_one, child_two], catena_gen:shrinks(Tree)).

print_tree_renders_a_displayable_tree_test() ->
    Tree =
        catena_tree:tree(root, fun() ->
            [catena_tree:tree(child, fun() -> [catena_tree:singleton(leaf)] end)]
        end),

    ?assertEqual("root\n  child\n    leaf\n", catena_gen:print_tree(Tree)).

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

fixed_generator(Value, Shrinks) ->
    catena_gen:new(fun(_Size, _Seed) ->
        catena_tree:tree(Value, fun() ->
            [catena_tree:singleton(Shrink) || Shrink <- Shrinks]
        end)
    end).

seed_word_generator() ->
    catena_gen:new(fun(Size, Seed) ->
        {Word, _NextSeed} = catena_gen:seed_next(Seed),
        catena_tree:tree(Word, fun() ->
            [catena_tree:singleton(Size)]
        end)
    end).

flat_tree_signature(Tree) ->
    {catena_tree:root(Tree), [catena_tree:root(Child) || Child <- catena_tree:children(Tree)]}.

tree_to_term(Tree) ->
    {catena_tree:root(Tree), [tree_to_term(Child) || Child <- catena_tree:children(Tree)]}.

next_values(_Seed, 0) ->
    [];
next_values(Seed, Count) when Count > 0 ->
    {Value, NextSeed} = catena_gen:seed_next(Seed),
    [Value | next_values(NextSeed, Count - 1)].

first_tree_with_root(Generator, RootValue, Limit) ->
    first_tree_satisfying(Generator, fun(Value) -> Value =:= RootValue end, Limit).

first_tree_satisfying(Generator, Predicate, Limit) ->
    Candidates =
        [catena_gen:run(Generator, 20, catena_gen:seed_from_int(Seed))
         || Seed <- lists:seq(1, Limit)],
    case [Tree || Tree <- Candidates, Predicate(catena_tree:root(Tree))] of
        [Tree | _] ->
            Tree;
        [] ->
            erlang:error({test_seed_not_found, Limit})
    end.
