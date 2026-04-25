%% @doc Unit Tests for Phase 7.1: Generic Generator Derivation
-module(catena_derive_tests).

-include_lib("eunit/include/eunit.hrl").

record_gen_builds_map_from_fields_test() ->
    Generator = catena_derive:record_gen([
        {name, catena_gen:constant(<<"ducky">>)},
        {age, catena_gen:constant(42)}
    ]),
    Tree = catena_gen:run(Generator, 5, catena_gen:seed_from_int(1)),
    ?assertEqual(#{name => <<"ducky">>, age => 42}, catena_tree:root(Tree)),
    ok.

record_gen_uses_construct_function_test() ->
    Generator = catena_derive:record_gen(
        [{left, catena_gen:constant(1)}, {right, catena_gen:constant(2)}],
        #{construct => fun(Map) -> {pair, maps:get(left, Map), maps:get(right, Map)} end}
    ),
    Tree = catena_gen:run(Generator, 5, catena_gen:seed_from_int(2)),
    ?assertEqual({pair, 1, 2}, catena_tree:root(Tree)),
    ok.

derive_generator_for_record_descriptor_test() ->
    Spec = #{
        kind => record,
        construct => fun(Map) -> {user, maps:get(name, Map), maps:get(admin, Map)} end,
        fields => [
            #{name => name, type => string, options => #{range => {3, 3}}},
            #{name => admin, type => bool}
        ]
    },
    Generator = catena_derive:derive_generator(Spec),
    Tree = catena_gen:run(Generator, 4, catena_gen:seed_from_int(3)),
    {user, Name, Admin} = catena_tree:root(Tree),
    ?assertEqual(3, length(Name)),
    ?assert(is_boolean(Admin)),
    ok.

variant_gen_builds_tagged_values_test() ->
    Generator = catena_derive:variant_gen([
        #{tag => none, args => [], weight => 1},
        #{tag => some, args => [catena_gen:constant(7)], weight => 1}
    ]),
    Sample = catena_gen:sample(Generator, 4),
    ?assert(lists:any(fun(Value) -> Value =:= none orelse Value =:= {some, 7} end, Sample)),
    ok.

derive_generator_for_variant_descriptor_test() ->
    Spec = #{
        kind => variant,
        constructors => [
            #{tag => ok, args => [integer]},
            #{tag => error, args => [string], weight => 1}
        ]
    },
    Generator = catena_derive:derive_generator(Spec),
    Sample = catena_gen:sample(Generator, 5),
    ?assert(lists:all(fun(Value) ->
        case Value of
            {ok, N} when is_integer(N) -> true;
            {error, Str} when is_list(Str) -> true;
            _ -> false
        end
    end, Sample)),
    ok.

recursive_derivation_prefers_base_cases_at_small_depth_test() ->
    catena_derive:clear_registry(),
    ok = catena_derive:register(tree, catena_derive:derive_generator(
        #{
            kind => recursive,
            base => [
                #{tag => leaf, args => [integer]}
            ],
            recursive => [
                #{tag => node, args => [self, self]}
            ]
        },
        #{max_depth => 2}
    )),
    Generator = catena_derive:derive_generator_for(tree),
    Sample = catena_gen:sample(Generator, 3),
    ?assert(lists:any(fun(Value) ->
        case Value of
            {leaf, N} when is_integer(N) -> true;
            _ -> false
        end
    end, Sample)),
    ok.

register_and_fetch_custom_generator_test() ->
    catena_derive:clear_registry(),
    Custom = catena_gen:constant(custom_value),
    ?assertEqual(ok, catena_derive:register(my_type, Custom)),
    ?assertMatch({ok, _}, catena_derive:registered(my_type)),
    Tree = catena_gen:run(catena_derive:derive_generator_for(my_type), 1, catena_gen:seed_from_int(4)),
    ?assertEqual(custom_value, catena_tree:root(Tree)),
    ok.

derive_generator_rejects_unknown_types_test() ->
    ?assertError({unsupported_type_descriptor, imaginary},
        catena_derive:derive_generator(#{kind => alias, type => imaginary})),
    ok.
