%% @doc Tests for the Catena Standard Prelude (Phase 2.2)
%%
%% These tests verify that all prelude functions work correctly
%% and have the expected behavior for standard functional programming
%% operations.
-module(catena_prelude_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Core Functions Tests (2.2.5)
%%====================================================================

identity_test_() ->
    [
        ?_assertEqual(42, catena_prelude:identity(42)),
        ?_assertEqual("hello", catena_prelude:identity("hello")),
        ?_assertEqual([1,2,3], catena_prelude:identity([1,2,3])),
        ?_assertEqual({some, 5}, catena_prelude:identity({some, 5}))
    ].

const_test_() ->
    [
        ?_assertEqual(5, catena_prelude:const(5, "ignored")),
        ?_assertEqual("always", catena_prelude:const("always", 999)),
        ?_assertEqual(42, catena_prelude:const(42, undefined))
    ].

compose_test_() ->
    Double = fun(X) -> X * 2 end,
    AddOne = fun(X) -> X + 1 end,
    [
        {"Compose returns function",
         ?_assert(is_function(catena_prelude:compose(Double, AddOne)))},
        {"Compose applies right then left",
         ?_assertEqual(12, (catena_prelude:compose(Double, AddOne))(5))},
        {"Compose/3 applies immediately",
         ?_assertEqual(12, catena_prelude:compose(Double, AddOne, 5))}
    ].

flip_test_() ->
    Subtract = fun(A, B) -> A - B end,
    [
        {"Flip returns function",
         ?_assert(is_function(catena_prelude:flip(Subtract)))},
        {"Flip swaps arguments",
         ?_assertEqual(7, (catena_prelude:flip(Subtract))(3, 10))},
        {"Flip/3 applies immediately",
         ?_assertEqual(7, catena_prelude:flip(Subtract, 3, 10))}
    ].

%%====================================================================
%% List Operations Tests (2.2.6)
%%====================================================================

map_test_() ->
    Double = fun(X) -> X * 2 end,
    [
        ?_assertEqual([2,4,6], catena_prelude:map(Double, [1,2,3])),
        ?_assertEqual([], catena_prelude:map(Double, [])),
        ?_assertEqual([10], catena_prelude:map(Double, [5]))
    ].

filter_test_() ->
    Even = fun(X) -> X rem 2 =:= 0 end,
    Positive = fun(X) -> X > 0 end,
    [
        ?_assertEqual([2,4], catena_prelude:filter(Even, [1,2,3,4,5])),
        ?_assertEqual([], catena_prelude:filter(Even, [1,3,5])),
        ?_assertEqual([1,2,3], catena_prelude:filter(Positive, [-1,0,1,2,3]))
    ].

fold_test_() ->
    Sum = fun(Acc, X) -> Acc + X end,
    Product = fun(Acc, X) -> Acc * X end,
    [
        ?_assertEqual(6, catena_prelude:fold(Sum, 0, [1,2,3])),
        ?_assertEqual(24, catena_prelude:fold(Product, 1, [1,2,3,4])),
        ?_assertEqual(0, catena_prelude:fold(Sum, 0, [])),
        ?_assertEqual(10, catena_prelude:fold(Sum, 10, []))
    ].

foldRight_test_() ->
    Cons = fun(X, Acc) -> [X | Acc] end,
    [
        ?_assertEqual([1,2,3], catena_prelude:foldRight(Cons, [], [1,2,3])),
        ?_assertEqual([], catena_prelude:foldRight(Cons, [], []))
    ].

append_test_() ->
    [
        ?_assertEqual([1,2,3,4], catena_prelude:append([1,2], [3,4])),
        ?_assertEqual([1,2], catena_prelude:append([1,2], [])),
        ?_assertEqual([3,4], catena_prelude:append([], [3,4])),
        ?_assertEqual([], catena_prelude:append([], []))
    ].

%%====================================================================
%% List Helpers Tests (2.2.7)
%%====================================================================

head_test_() ->
    [
        ?_assertEqual({some, 1}, catena_prelude:head([1,2,3])),
        ?_assertEqual({some, 5}, catena_prelude:head([5])),
        ?_assertEqual(none, catena_prelude:head([]))
    ].

tail_test_() ->
    [
        ?_assertEqual({some, [2,3]}, catena_prelude:tail([1,2,3])),
        ?_assertEqual({some, []}, catena_prelude:tail([1])),
        ?_assertEqual(none, catena_prelude:tail([]))
    ].

length_test_() ->
    [
        ?_assertEqual(3, catena_prelude:length([1,2,3])),
        ?_assertEqual(0, catena_prelude:length([])),
        ?_assertEqual(1, catena_prelude:length([42]))
    ].

reverse_test_() ->
    [
        ?_assertEqual([3,2,1], catena_prelude:reverse([1,2,3])),
        ?_assertEqual([], catena_prelude:reverse([])),
        ?_assertEqual([1], catena_prelude:reverse([1]))
    ].

take_test_() ->
    [
        ?_assertEqual([1,2], catena_prelude:take(2, [1,2,3,4])),
        ?_assertEqual([1,2,3], catena_prelude:take(10, [1,2,3])),
        ?_assertEqual([], catena_prelude:take(0, [1,2,3])),
        ?_assertEqual([], catena_prelude:take(5, []))
    ].

drop_test_() ->
    [
        ?_assertEqual([3,4], catena_prelude:drop(2, [1,2,3,4])),
        ?_assertEqual([], catena_prelude:drop(10, [1,2,3])),
        ?_assertEqual([1,2,3], catena_prelude:drop(0, [1,2,3])),
        ?_assertEqual([], catena_prelude:drop(5, []))
    ].

%%====================================================================
%% Maybe Operations Tests
%%====================================================================

fromMaybe_test_() ->
    [
        ?_assertEqual(42, catena_prelude:fromMaybe(0, {some, 42})),
        ?_assertEqual(0, catena_prelude:fromMaybe(0, none)),
        ?_assertEqual("default", catena_prelude:fromMaybe("default", none))
    ].

maybe_test_() ->
    Double = fun(X) -> X * 2 end,
    [
        ?_assertEqual(42, catena_prelude:'maybe'(0, Double, {some, 21})),
        ?_assertEqual(0, catena_prelude:'maybe'(0, Double, none))
    ].

isJust_test_() ->
    [
        ?_assert(catena_prelude:isJust({some, 42})),
        ?_assertNot(catena_prelude:isJust(none))
    ].

isNothing_test_() ->
    [
        ?_assert(catena_prelude:isNothing(none)),
        ?_assertNot(catena_prelude:isNothing({some, 42}))
    ].

%%====================================================================
%% Result Operations Tests
%%====================================================================

fromResult_test_() ->
    [
        ?_assertEqual(42, catena_prelude:fromResult(0, {ok, 42})),
        ?_assertEqual(0, catena_prelude:fromResult(0, {err, "error"}))
    ].

isOk_test_() ->
    [
        ?_assert(catena_prelude:isOk({ok, 42})),
        ?_assertNot(catena_prelude:isOk({err, "error"}))
    ].

isErr_test_() ->
    [
        ?_assert(catena_prelude:isErr({err, "error"})),
        ?_assertNot(catena_prelude:isErr({ok, 42}))
    ].

%%====================================================================
%% Functor Operations Tests
%%====================================================================

fmap_test_() ->
    Double = fun(X) -> X * 2 end,
    [
        {"fmap over list",
         ?_assertEqual([2,4,6], catena_prelude:fmap(Double, [1,2,3]))},
        {"fmap over Some",
         ?_assertEqual({some, 42}, catena_prelude:fmap(Double, {some, 21}))},
        {"fmap over None",
         ?_assertEqual(none, catena_prelude:fmap(Double, none))},
        {"fmap over Ok",
         ?_assertEqual({ok, 42}, catena_prelude:fmap(Double, {ok, 21}))},
        {"fmap over Err",
         ?_assertEqual({err, "oops"}, catena_prelude:fmap(Double, {err, "oops"}))}
    ].

%%====================================================================
%% Applicative Operations Tests
%%====================================================================

pure_test_() ->
    [
        ?_assertEqual({some, 42}, catena_prelude:pure(42)),
        ?_assertEqual({some, "hello"}, catena_prelude:pure("hello"))
    ].

apply_f_test_() ->
    Double = fun(X) -> X * 2 end,
    [
        {"apply with Some function and Some value",
         ?_assertEqual({some, 42}, catena_prelude:apply_f({some, Double}, {some, 21}))},
        {"apply with Some function and None",
         ?_assertEqual(none, catena_prelude:apply_f({some, Double}, none))},
        {"apply with None function",
         ?_assertEqual(none, catena_prelude:apply_f(none, {some, 21}))},
        {"apply over lists",
         ?_assertEqual([2,4,3,6], catena_prelude:apply_f([Double, fun(X) -> X * 3 end], [1,2]))}
    ].

%%====================================================================
%% Monad Operations Tests
%%====================================================================

bind_test_() ->
    SafeDiv = fun(X) ->
        case X of
            0 -> none;
            N -> {some, 10 div N}
        end
    end,
    [
        {"bind with Some",
         ?_assertEqual({some, 5}, catena_prelude:bind({some, 2}, SafeDiv))},
        {"bind with None",
         ?_assertEqual(none, catena_prelude:bind(none, SafeDiv))},
        {"bind producing None",
         ?_assertEqual(none, catena_prelude:bind({some, 0}, SafeDiv))},
        {"bind over list (flatMap)",
         ?_assertEqual([1,10,2,20], catena_prelude:bind([1,2], fun(X) -> [X, X * 10] end))}
    ].

chain_test_() ->
    SafeDiv = fun(X) ->
        case X of
            0 -> none;
            N -> {some, 10 div N}
        end
    end,
    [
        {"chain is bind with flipped args",
         ?_assertEqual({some, 5}, catena_prelude:chain(SafeDiv, {some, 2}))}
    ].

join_test_() ->
    [
        {"join nested Some",
         ?_assertEqual({some, 42}, catena_prelude:join({some, {some, 42}}))},
        {"join Some None",
         ?_assertEqual(none, catena_prelude:join({some, none}))},
        {"join None",
         ?_assertEqual(none, catena_prelude:join(none))},
        {"join nested list",
         ?_assertEqual([1,2,3,4], catena_prelude:join([[1,2],[3,4]]))}
    ].

%%====================================================================
%% Prelude Bindings Tests
%%====================================================================

prelude_bindings_test_() ->
    Bindings = catena_prelude:prelude_bindings(),
    [
        {"bindings is a map",
         ?_assert(is_map(Bindings))},
        {"has identity",
         ?_assert(maps:is_key(identity, Bindings))},
        {"has map",
         ?_assert(maps:is_key(map, Bindings))},
        {"has bind",
         ?_assert(maps:is_key(bind, Bindings))},
        {"binding has correct structure",
         fun() ->
             {Fun, Arity, _Type} = maps:get(identity, Bindings),
             ?assert(is_function(Fun)),
             ?assertEqual(1, Arity)
         end}
    ].

prelude_types_test_() ->
    Types = catena_prelude:prelude_types(),
    [
        {"types is a list",
         ?_assert(is_list(Types))},
        {"has Maybe type",
         ?_assert(lists:any(fun({type_decl, 'Maybe', _, _}) -> true; (_) -> false end, Types))},
        {"has Result type",
         ?_assert(lists:any(fun({type_decl, 'Result', _, _}) -> true; (_) -> false end, Types))}
    ].
