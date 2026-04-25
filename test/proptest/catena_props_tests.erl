%% @doc Unit Tests for Phase 7.4: Type-Directed Property Combinators
-module(catena_props_tests).

-include_lib("eunit/include/eunit.hrl").

recognize_patterns_detects_supported_shapes_test() ->
    Patterns = catena_props:recognize_patterns(#{
        inverse => decode,
        arity => 2,
        commutative => true,
        associative => true
    }),
    ?assertEqual([associative, commutative, roundtrip], Patterns),
    ok.

roundtrip_passes_for_term_binary_encoding_test() ->
    Generator = catena_stdgen:gen_tuple2(
        catena_gen:gen_int_range(-20, 20),
        catena_gen:gen_bool()
    ),
    Property = catena_props:roundtrip(
        "term_binary_roundtrip",
        fun(Value) -> term_to_binary(Value) end,
        fun(Binary) -> binary_to_term(Binary) end,
        Generator
    ),
    {passed, _} = catena_runner:run_property(
        Property,
        #{num_tests => 25, seed => catena_gen:seed_from_int(23)}
    ),
    ok.

roundtrip_handles_partial_decode_results_test() ->
    Property = catena_props:roundtrip(
        "partial_roundtrip",
        fun(N) -> integer_to_binary(N) end,
        fun(Binary) ->
            try {ok, binary_to_integer(Binary)}
            catch
                _:_ -> {error, invalid}
            end
        end,
        catena_gen:gen_int_range(-25, 25),
        #{unwrap_result => true}
    ),
    {passed, _} = catena_runner:run_property(
        Property,
        #{num_tests => 25, seed => catena_gen:seed_from_int(29)}
    ),
    ok.

roundtrip_accepts_custom_equality_test() ->
    Property = catena_props:roundtrip(
        "case_insensitive_roundtrip",
        fun(String) -> string:uppercase(String) end,
        fun(String) -> string:lowercase(String) end,
        catena_stdgen:gen_string(catena_range:range_constant({1, 6})),
        #{
            eq => fun(Expected, Actual) ->
                string:lowercase(Expected) =:= string:lowercase(Actual)
            end,
            unwrap_result => false
        }
    ),
    {passed, _} = catena_runner:run_property(
        Property,
        #{num_tests => 20, seed => catena_gen:seed_from_int(31)}
    ),
    ok.

idempotent_property_passes_for_sort_test() ->
    Property = catena_props:idempotent(
        "sort_idempotent",
        fun(List) -> lists:sort(List) end,
        catena_stdgen:gen_list_of(
            catena_range:range_constant({0, 6}),
            catena_gen:gen_int_range(-3, 3)
        )
    ),
    {passed, _} = catena_runner:run_property(
        Property,
        #{num_tests => 20, seed => catena_gen:seed_from_int(37)}
    ),
    ok.

commutative_property_passes_for_addition_test() ->
    Property = catena_props:commutative(
        "addition_commutative",
        fun(A, B) -> A + B end,
        catena_gen:gen_int_range(-50, 50)
    ),
    {passed, _} = catena_runner:run_property(
        Property,
        #{num_tests => 30, seed => catena_gen:seed_from_int(41)}
    ),
    ok.

associative_property_detects_failure_for_subtraction_test() ->
    Property = catena_props:associative(
        "subtraction_not_associative",
        fun(A, B) -> A - B end,
        catena_gen:gen_int_range(-10, 10)
    ),
    {failed, _} = catena_runner:run_property(
        Property,
        #{num_tests => 30, seed => catena_gen:seed_from_int(43)}
    ),
    ok.
