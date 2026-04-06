%% @doc Unit tests for catena_property (Section 3.1)
%%
%% Tests the Property Specification DSL including forall quantification,
%% implication/preconditions, and property combinators.
-module(catena_property_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_property.hrl").

%%====================================================================
%% Section 3.1.1: Property Type Definition
%%====================================================================

property_creation_creates_property_test() ->
    Prop = catena_property:property("test_prop", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(N) -> N >= 0 end)
    end),
    ?assertMatch(#property{name = <<"test_prop">>}, Prop),
    ok.

property_new_creates_property_test() ->
    Gen = catena_gen:gen_int(),
    Pred = fun(N) -> N rem 2 =:= 0 end,
    Prop = catena_property:new("test_prop", Gen, Pred),
    ?assertMatch(#property{generator = Gen, predicate = Pred}, Prop),
    ok.

default_config_test() ->
    Config = catena_property:default_config(),
    ?assertEqual(100, Config#property_config.num_tests),
    ?assertEqual(1000, Config#property_config.max_shrinks),
    ok.

%%====================================================================
%% Section 3.1.2: Forall Syntax
%%====================================================================

forall_single_variable_test() ->
    ForAll = catena_property:forall(catena_gen:gen_int(), fun(N) -> is_integer(N) end),
    ?assertMatch({forall, _, _}, ForAll),
    ok.

forall_with_guard_test() ->
    ForAll = catena_property:forall(catena_gen:gen_int(), fun(N) when N > 0 -> N rem 2 =:= 0 end),
    ?assertMatch({forall, _, _}, ForAll),
    ok.

%%====================================================================
%% Section 3.1.3: Implication and Preconditions
%%====================================================================

implication_true_passes_test() ->
    Result = catena_property:implies(true, fun() -> true end),
    ?assertEqual(true, Result),
    ok.

implication_false_discards_test() ->
    Result = catena_property:implies(false, fun() -> true end),
    ?assertEqual(discard, Result),
    ok.

implies_function_true_test() ->
    Result = catena_property:implies(true, fun() -> success end),
    ?assertEqual(success, Result),
    ok.

implies_function_false_discards_test() ->
    Result = catena_property:implies(false, fun() -> success end),
    ?assertEqual(discard, Result),
    ok.

%%====================================================================
%% Section 3.1.4: Property Combinators
%%====================================================================

with_config_modifies_test_count_test() ->
    Prop = catena_property:property("test", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    Modified = catena_property:with_config([{num_tests, 50}], Prop),
    ?assertEqual(50, Modified#property.config#property_config.num_tests),
    ok.

with_config_modifies_shrinks_test() ->
    Prop = catena_property:property("test", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    Modified = catena_property:with_config([{max_shrinks, 100}], Prop),
    ?assertEqual(100, Modified#property.config#property_config.max_shrinks),
    ok.

with_config_modifies_seed_test() ->
    Seed = catena_gen:seed_from_int(12345),
    Prop = catena_property:property("test", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    Modified = catena_property:with_config([{seed, Seed}], Prop),
    ?assertMatch(#property_config{seed = Seed}, Modified#property.config),
    ok.

with_label_adds_label_test() ->
    Prop = catena_property:property("test", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    Modified = catena_property:with_label(<<"even">>, Prop),
    ?assertEqual([<<"even">>], Modified#property.config#property_config.labels),
    ok.

classify_adds_classification_test() ->
    Prop = catena_property:property("test", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    Modified = catena_property:classify(<<"positive">>, fun(_) -> positive end, Prop),
    ?assertMatch(#property_config{classify_fun = {<<"positive">>, _}}, Modified#property.config),
    ok.

property_group_creates_group_test() ->
    Prop1 = catena_property:property("prop1", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end),
    Prop2 = catena_property:property("prop2", fun() ->
        catena_property:forall(catena_gen:gen_bool(), fun(_) -> true end)
    end),
    Group = catena_property:property_group([Prop1, Prop2]),
    ?assert(is_record(Group, property, 5)),
    ok.
