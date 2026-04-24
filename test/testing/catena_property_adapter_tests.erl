%% @doc Tests for adapting legacy `property_decl` AST into `src/proptest`
%% properties.
-module(catena_property_adapter_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_property.hrl").

from_property_decl_creates_runnable_property_test() ->
    PropDecl = {property_decl, "always true",
        {property_forall,
            [{x, 'Bool'}],
            {literal, true, bool, {line, 1}},
            {line, 1}},
        {line, 1}},
    {ok, Prop} = catena_property_adapter:from_property_decl(PropDecl, #{}),
    ?assertMatch({passed, _}, catena_runner:run_property(Prop, #{
        num_tests => 10,
        seed => catena_gen:seed_from_int(7)
    })).

multiple_bindings_are_combined_into_one_property_generator_test() ->
    PropDecl = {property_decl, "commutative add",
        {property_forall,
            [{x, 'Natural'}, {y, 'Natural'}],
            {binary_op, eq,
                {binary_op, plus, {var, x, {line, 1}}, {var, y, {line, 1}}, {line, 1}},
                {binary_op, plus, {var, y, {line, 1}}, {var, x, {line, 1}}, {line, 1}},
                {line, 1}},
            {line, 1}},
        {line, 1}},
    {ok, Prop} = catena_property_adapter:from_property_decl(PropDecl, #{}),
    {passed, Result} = catena_runner:run_property(Prop, #{
        num_tests => 25,
        seed => catena_gen:seed_from_int(11)
    }),
    ?assertEqual(success, Result#property_result.kind).

unsupported_property_generator_returns_explicit_error_test() ->
    PropDecl = {property_decl, "unsupported",
        {property_forall,
            [{x, 'Widget'}],
            {literal, true, bool, {line, 1}},
            {line, 1}},
        {line, 1}},
    ?assertEqual(
        {error, {unsupported_property_generator, 'Widget'}},
        catena_property_adapter:from_property_decl(PropDecl, #{})
    ).

binding_generator_wraps_values_for_runtime_env_test() ->
    {ok, BindingGen} = catena_property_adapter:binding_generator({x, 'Natural'}),
    Tree = catena_gen:run(BindingGen, 10, catena_gen:seed_from_int(5)),
    Generated = catena_tree:root(Tree),
    ?assertMatch(#{x := {value, _}}, Generated).
