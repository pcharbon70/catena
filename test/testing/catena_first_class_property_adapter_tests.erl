%% @doc Tests for adapting first-class stdlib property and generator values
%% into the internal property engine.
-module(catena_first_class_property_adapter_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_property.hrl").

from_property_value_builds_runnable_property_test() ->
    PropertyValue = #{
        name => "even or discard",
        body => fun(_Unit) ->
            {for_all,
                {gen_int_range, 0, 10},
                fun(N) ->
                    case N rem 2 of
                        0 -> true;
                        _ -> discard
                    end
                end}
        end,
        config => #{
            iterations => 12,
            seed => {some, 9},
            labels => ["phase2"]
        }
    },
    {ok, Prop} = catena_first_class_property_adapter:from_property_value(PropertyValue),
    ?assertEqual(12, Prop#property.config#property_config.num_tests),
    ?assertNotEqual(undefined, Prop#property.config#property_config.seed),
    ?assertEqual([<<"phase2">>], Prop#property.config#property_config.labels),
    {passed, Result} = catena_runner:run_property(Prop, #{
        num_tests => 12,
        seed => catena_gen:seed_from_int(9)
    }),
    ?assertEqual(success, Result#property_result.kind).

one_of_generator_is_translated_test() ->
    {ok, Generator} = catena_first_class_property_adapter:from_generator_value(
        {gen_one_of, [gen_bool, {gen_constant_bool, true}]}
    ),
    Tree = catena_gen:run(Generator, 10, catena_gen:seed_from_int(5)),
    Value = catena_tree:root(Tree),
    ?assert(lists:member(Value, [true, false])).

flat_map_generator_is_translated_test() ->
    {ok, Generator} = catena_first_class_property_adapter:from_generator_value(
        {gen_flat_map, gen_bool, fun(Bool) ->
            case Bool of
                true -> {gen_constant_int, 1};
                false -> {gen_constant_int, 0}
            end
        end}
    ),
    Tree = catena_gen:run(Generator, 10, catena_gen:seed_from_int(13)),
    Value = catena_tree:root(Tree),
    ?assert(lists:member(Value, [0, 1])).

unsupported_generator_returns_explicit_error_test() ->
    ?assertEqual(
        {error, {unsupported_generator_value, unsupported}},
        catena_first_class_property_adapter:from_generator_value(unsupported)
    ).

top_level_discard_property_spec_is_rejected_test() ->
    ?assertEqual(
        {error, {unsupported_property_spec, discard}},
        catena_first_class_property_adapter:from_property_spec(discard)
    ).
