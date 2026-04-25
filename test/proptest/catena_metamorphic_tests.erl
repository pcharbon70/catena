%% @doc Unit Tests for Phase 7.3: Metamorphic Testing
-module(catena_metamorphic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_property.hrl").

relation_metadata_test() ->
    Relation = catena_metamorphic:relation(
        reorder,
        fun(List) -> lists:reverse(List) end,
        fun(Output, TransformedOutput) -> Output =:= TransformedOutput end
    ),
    ?assertEqual(<<"reorder">>, maps:get(name, Relation)),
    ?assert(is_function(maps:get(transform, Relation), 1)),
    ok.

identity_relation_passes_for_sort_test() ->
    Generator = catena_stdgen:gen_list_of(
        catena_range:range_constant({0, 6}),
        catena_gen:gen_int_range(-5, 5)
    ),
    Property = catena_metamorphic:metamorphic(
        "sort_identity",
        Generator,
        #{
            subject => fun(List) -> lists:sort(List) end,
            relations => [
                catena_metamorphic:identity(fun(List) -> lists:reverse(List) end)
            ]
        }
    ),
    {passed, _} = catena_runner:run_property(
        Property,
        #{num_tests => 25, seed => catena_gen:seed_from_int(7)}
    ),
    ok.

scaling_relation_passes_test() ->
    Property = catena_metamorphic:metamorphic(
        "linear_scaling",
        catena_gen:gen_int_range(-20, 20),
        #{
            subject => fun(N) -> N * 2 end,
            relations => [
                catena_metamorphic:scaling(
                    fun(N) -> N * 3 end,
                    fun(Output) -> Output * 3 end
                )
            ]
        }
    ),
    {passed, _} = catena_runner:run_property(
        Property,
        #{num_tests => 25, seed => catena_gen:seed_from_int(11)}
    ),
    ok.

composition_relation_passes_test() ->
    Property = catena_metamorphic:metamorphic(
        "affine_composition",
        catena_gen:gen_int_range(-20, 20),
        #{
            subject => fun(N) -> N + 1 end,
            relations => [
                catena_metamorphic:composition(
                    fun(N) -> N * 2 end,
                    fun(Output) -> (Output * 2) - 1 end
                )
            ]
        }
    ),
    {passed, _} = catena_runner:run_property(
        Property,
        #{num_tests => 25, seed => catena_gen:seed_from_int(13)}
    ),
    ok.

multiple_relations_can_share_a_property_test() ->
    Property = catena_metamorphic:metamorphic(
        "sum_relations",
        catena_stdgen:gen_list_of(
            catena_range:range_constant({0, 6}),
            catena_gen:gen_int_range(-3, 3)
        ),
        #{
            subject => fun(List) -> lists:sum(List) end,
            relations => [
                catena_metamorphic:permutation_invariant(),
                catena_metamorphic:identity(fun(List) -> lists:sort(List) end)
            ]
        }
    ),
    {passed, _} = catena_runner:run_property(
        Property,
        #{num_tests => 30, seed => catena_gen:seed_from_int(17)}
    ),
    ok.

permutation_failure_retains_useful_counterexample_test() ->
    Generator = catena_stdgen:gen_list_of(
        catena_range:range_constant({2, 4}),
        catena_gen:gen_int_range(-5, 5)
    ),
    Property = catena_metamorphic:metamorphic(
        "head_is_not_permutation_invariant",
        Generator,
        #{
            subject => fun([Head | _]) -> Head end,
            relations => [
                catena_metamorphic:permutation_invariant()
            ]
        }
    ),
    {failed, Result} = catena_runner:run_property(
        Property,
        #{num_tests => 40, seed => catena_gen:seed_from_int(3)}
    ),
    Counterexample = Result#property_result.shrunk_counterexample,
    ?assert(is_map(Counterexample)),
    ?assertEqual(<<"permutation_invariance">>, maps:get(relation, Counterexample)),
    ?assert(maps:is_key(input, Counterexample)),
    ?assert(maps:is_key(transformed_input, Counterexample)),
    ?assert(maps:is_key(output, Counterexample)),
    ?assert(maps:is_key(transformed_output, Counterexample)),
    ?assertNot(maps:get(output, Counterexample) =:= maps:get(transformed_output, Counterexample)),
    ok.

format_failure_includes_relation_context_test() ->
    Formatted = catena_metamorphic:format_failure(#{
        relation => <<"scaling">>,
        input => 2,
        transformed_input => 6,
        output => 4,
        transformed_output => 13
    }),
    ?assert(binary:match(Formatted, <<"Relation: scaling">>) =/= nomatch),
    ?assert(binary:match(Formatted, <<"Transformed Output: 13">>) =/= nomatch),
    ok.
