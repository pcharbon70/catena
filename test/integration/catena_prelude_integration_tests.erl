%% @doc Prelude Functionality Integration Tests (Phase 2.4.2)
%%
%% These tests validate that the prelude functions work correctly
%% when composed together in pipelines and chains.
-module(catena_prelude_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 2.4.2.1 List Processing Pipeline Tests
%%====================================================================

%% Test map then filter
map_filter_pipeline_test() ->
    %% Double all numbers, then keep only those > 5
    Result = catena_prelude:filter(
        fun(X) -> X > 5 end,
        catena_prelude:map(fun(X) -> X * 2 end, [1, 2, 3, 4, 5])
    ),
    ?assertEqual([6, 8, 10], Result).

%% Test filter then map
filter_map_pipeline_test() ->
    %% Keep evens, then square them
    Result = catena_prelude:map(
        fun(X) -> X * X end,
        catena_prelude:filter(fun(X) -> X rem 2 == 0 end, [1, 2, 3, 4, 5, 6])
    ),
    ?assertEqual([4, 16, 36], Result).

%% Test map |> filter |> fold
map_filter_fold_pipeline_test() ->
    %% Triple numbers, filter > 6, then sum
    Step1 = catena_prelude:map(fun(X) -> X * 3 end, [1, 2, 3, 4, 5]),
    Step2 = catena_prelude:filter(fun(X) -> X > 6 end, Step1),
    Result = catena_prelude:fold(fun(Acc, X) -> Acc + X end, 0, Step2),
    %% [3, 6, 9, 12, 15] -> [9, 12, 15] -> 36
    ?assertEqual(36, Result).

%% Test with foldRight
foldRight_builds_list_test() ->
    %% foldRight with cons builds the list in order
    Result = catena_prelude:foldRight(
        fun(X, Acc) -> [X * 2 | Acc] end,
        [],
        [1, 2, 3]
    ),
    ?assertEqual([2, 4, 6], Result).

%% Test append in pipeline
append_pipeline_test() ->
    List1 = catena_prelude:map(fun(X) -> X * 2 end, [1, 2, 3]),
    List2 = catena_prelude:filter(fun(X) -> X > 3 end, [1, 2, 3, 4, 5]),
    Result = catena_prelude:append(List1, List2),
    ?assertEqual([2, 4, 6, 4, 5], Result).

%% Test nested map
nested_map_test() ->
    %% Map over list of lists
    Input = [[1, 2], [3, 4], [5, 6]],
    Result = catena_prelude:map(
        fun(Inner) -> catena_prelude:map(fun(X) -> X * 2 end, Inner) end,
        Input
    ),
    ?assertEqual([[2, 4], [6, 8], [10, 12]], Result).

%%====================================================================
%% 2.4.2.2 Monadic Sequencing with Maybe and Result
%%====================================================================

%% Test bind with Maybe - success path
maybe_bind_success_test() ->
    %% Computation that might fail
    SafeDiv = fun(X, Y) ->
        case Y of
            0 -> none;
            _ -> {some, X div Y}
        end
    end,
    %% 20 / 4 = 5, then 5 / 1 = 5
    Result = catena_prelude:bind(
        SafeDiv(20, 4),
        fun(X) -> SafeDiv(X, 1) end
    ),
    ?assertEqual({some, 5}, Result).

%% Test bind with Maybe - failure propagation
maybe_bind_failure_test() ->
    SafeDiv = fun(X, Y) ->
        case Y of
            0 -> none;
            _ -> {some, X div Y}
        end
    end,
    %% 20 / 4 = 5, then 5 / 0 = none
    Result = catena_prelude:bind(
        SafeDiv(20, 4),
        fun(X) -> SafeDiv(X, 0) end
    ),
    ?assertEqual(none, Result).

%% Test bind chain with Maybe
maybe_bind_chain_test() ->
    %% Chain of operations: add 1, multiply by 2, subtract 3
    Add = fun(X) -> {some, X + 1} end,
    Mul = fun(X) -> {some, X * 2} end,
    Sub = fun(X) -> {some, X - 3} end,

    %% Start with 5: (5+1)*2-3 = 9
    Result = catena_prelude:bind(
        catena_prelude:bind(
            catena_prelude:bind({some, 5}, Add),
            Mul),
        Sub),
    ?assertEqual({some, 9}, Result).

%% Test bind with Result - success path
result_bind_success_test() ->
    ParseInt = fun(S) ->
        try
            {ok, list_to_integer(S)}
        catch
            _:_ -> {err, "invalid integer"}
        end
    end,
    Result = catena_prelude:bind(
        ParseInt("42"),
        fun(X) -> {ok, X * 2} end
    ),
    ?assertEqual({ok, 84}, Result).

%% Test bind with Result - error propagation
result_bind_error_test() ->
    ParseInt = fun(S) ->
        try
            {ok, list_to_integer(S)}
        catch
            _:_ -> {err, "invalid integer"}
        end
    end,
    Result = catena_prelude:bind(
        ParseInt("not a number"),
        fun(X) -> {ok, X * 2} end
    ),
    ?assertEqual({err, "invalid integer"}, Result).

%% Test chain (flipped bind)
chain_test() ->
    Double = fun(X) -> {some, X * 2} end,
    Result = catena_prelude:chain(Double, {some, 21}),
    ?assertEqual({some, 42}, Result).

%% Test join for nested Maybe
join_maybe_test() ->
    ?assertEqual({some, 42}, catena_prelude:join({some, {some, 42}})),
    ?assertEqual(none, catena_prelude:join({some, none})),
    ?assertEqual(none, catena_prelude:join(none)).

%% Test join for nested Result
join_result_test() ->
    ?assertEqual({ok, 42}, catena_prelude:join({ok, {ok, 42}})),
    ?assertEqual({err, "inner"}, catena_prelude:join({ok, {err, "inner"}})),
    ?assertEqual({err, "outer"}, catena_prelude:join({err, "outer"})).

%% Test bind with List (flatmap behavior)
list_bind_test() ->
    %% bind on lists does flatmap
    Result = catena_prelude:bind([1, 2, 3], fun(X) -> [X, X * 10] end),
    ?assertEqual([1, 10, 2, 20, 3, 30], Result).

%%====================================================================
%% 2.4.2.3 Function Composition Chains
%%====================================================================

%% Test compose two functions
compose_two_test() ->
    Double = fun(X) -> X * 2 end,
    AddOne = fun(X) -> X + 1 end,
    %% compose(f, g)(x) = f(g(x))
    %% compose(double, addOne)(5) = double(addOne(5)) = double(6) = 12
    Composed = catena_prelude:compose(Double, AddOne),
    ?assertEqual(12, Composed(5)).

%% Test compose with immediate application
compose_applied_test() ->
    Double = fun(X) -> X * 2 end,
    AddOne = fun(X) -> X + 1 end,
    Result = catena_prelude:compose(Double, AddOne, 5),
    ?assertEqual(12, Result).

%% Test compose chain
compose_chain_test() ->
    F1 = fun(X) -> X + 1 end,
    F2 = fun(X) -> X * 2 end,
    F3 = fun(X) -> X - 3 end,
    %% Build: f3 . f2 . f1 = subtract 3 from (double (add 1 to x))
    Composed = catena_prelude:compose(F3, catena_prelude:compose(F2, F1)),
    %% (5+1)*2-3 = 9
    ?assertEqual(9, Composed(5)).

%% Test flip
flip_test() ->
    Subtract = fun(A, B) -> A - B end,
    Flipped = catena_prelude:flip(Subtract),
    %% Flipped(3, 10) = Subtract(10, 3) = 7
    ?assertEqual(7, Flipped(3, 10)).

%% Test flip with immediate application
flip_applied_test() ->
    Subtract = fun(A, B) -> A - B end,
    Result = catena_prelude:flip(Subtract, 3, 10),
    ?assertEqual(7, Result).

%% Test identity in composition
identity_compose_test() ->
    Double = fun(X) -> X * 2 end,
    %% identity composed with f should equal f
    Composed = catena_prelude:compose(fun catena_prelude:identity/1, Double),
    ?assertEqual(10, Composed(5)),
    %% f composed with identity should equal f
    Composed2 = catena_prelude:compose(Double, fun catena_prelude:identity/1),
    ?assertEqual(10, Composed2(5)).

%% Test const
const_test() ->
    Result = catena_prelude:const(42, "ignored"),
    ?assertEqual(42, Result),
    %% Can use const with map to replace all values
    Replaced = catena_prelude:map(
        fun(X) -> catena_prelude:const(0, X) end,
        [1, 2, 3, 4]
    ),
    ?assertEqual([0, 0, 0, 0], Replaced).

%%====================================================================
%% 2.4.2.4 Prelude Compilation and Consistency
%%====================================================================

%% Test all core functions are in bindings
core_functions_in_bindings_test() ->
    Bindings = catena_prelude:prelude_bindings(),
    CoreFunctions = [identity, const, compose, flip],
    lists:foreach(
        fun(Name) ->
            ?assert(maps:is_key(Name, Bindings),
                    lists:flatten(io_lib:format("~p should be in bindings", [Name])))
        end,
        CoreFunctions
    ).

%% Test all list operations are in bindings
list_ops_in_bindings_test() ->
    Bindings = catena_prelude:prelude_bindings(),
    ListOps = [map, filter, fold, foldRight, append, head, tail,
               length, reverse, take, drop],
    lists:foreach(
        fun(Name) ->
            ?assert(maps:is_key(Name, Bindings),
                    lists:flatten(io_lib:format("~p should be in bindings", [Name])))
        end,
        ListOps
    ).

%% Test all maybe operations are in bindings
maybe_ops_in_bindings_test() ->
    Bindings = catena_prelude:prelude_bindings(),
    MaybeOps = [fromMaybe, 'maybe', isJust, isNothing],
    lists:foreach(
        fun(Name) ->
            ?assert(maps:is_key(Name, Bindings),
                    lists:flatten(io_lib:format("~p should be in bindings", [Name])))
        end,
        MaybeOps
    ).

%% Test all result operations are in bindings
result_ops_in_bindings_test() ->
    Bindings = catena_prelude:prelude_bindings(),
    ResultOps = [fromResult, isOk, isErr],
    lists:foreach(
        fun(Name) ->
            ?assert(maps:is_key(Name, Bindings),
                    lists:flatten(io_lib:format("~p should be in bindings", [Name])))
        end,
        ResultOps
    ).

%% Test all monad operations are in bindings
monad_ops_in_bindings_test() ->
    Bindings = catena_prelude:prelude_bindings(),
    MonadOps = [fmap, pure, bind, chain, join],
    lists:foreach(
        fun(Name) ->
            ?assert(maps:is_key(Name, Bindings),
                    lists:flatten(io_lib:format("~p should be in bindings", [Name])))
        end,
        MonadOps
    ).

%% Test bindings have correct arity
bindings_arity_test() ->
    Bindings = catena_prelude:prelude_bindings(),
    ArityChecks = [
        {identity, 1},
        {const, 2},
        {compose, 2},
        {flip, 1},
        {map, 2},
        {filter, 2},
        {fold, 3},
        {head, 1},
        {bind, 2}
    ],
    lists:foreach(
        fun({Name, ExpectedArity}) ->
            {_Fun, Arity, _Type} = maps:get(Name, Bindings),
            ?assertEqual(ExpectedArity, Arity,
                        lists:flatten(io_lib:format("~p should have arity ~p", [Name, ExpectedArity])))
        end,
        ArityChecks
    ).

%% Test prelude types are well-formed
prelude_types_wellformed_test() ->
    Types = catena_prelude:prelude_types(),
    ?assert(is_list(Types)),
    ?assert(length(Types) >= 4),
    %% Check Maybe type exists
    MaybeType = lists:keyfind('Maybe', 2, Types),
    ?assertNotEqual(false, MaybeType),
    %% Check Result type exists
    ResultType = lists:keyfind('Result', 2, Types),
    ?assertNotEqual(false, ResultType).

%%====================================================================
%% List Helper Integration Tests
%%====================================================================

%% Test head and tail together
head_tail_test() ->
    List = [1, 2, 3],
    {some, H} = catena_prelude:head(List),
    {some, T} = catena_prelude:tail(List),
    ?assertEqual(1, H),
    ?assertEqual([2, 3], T).

%% Test take and drop together
take_drop_test() ->
    List = [1, 2, 3, 4, 5],
    Taken = catena_prelude:take(3, List),
    Dropped = catena_prelude:drop(3, List),
    %% take(n) ++ drop(n) should equal the original list
    ?assertEqual(List, catena_prelude:append(Taken, Dropped)).

%% Test reverse roundtrip
reverse_roundtrip_test() ->
    List = [1, 2, 3, 4, 5],
    DoubleReversed = catena_prelude:reverse(catena_prelude:reverse(List)),
    ?assertEqual(List, DoubleReversed).

%% Test length with operations
length_after_ops_test() ->
    List = [1, 2, 3, 4, 5],
    %% filter keeps 3 elements
    Filtered = catena_prelude:filter(fun(X) -> X > 2 end, List),
    ?assertEqual(3, catena_prelude:length(Filtered)),
    %% map preserves length
    Mapped = catena_prelude:map(fun(X) -> X * 2 end, List),
    ?assertEqual(5, catena_prelude:length(Mapped)).

%%====================================================================
%% Maybe/Result Helper Integration Tests
%%====================================================================

%% Test fromMaybe with pipeline result
fromMaybe_pipeline_test() ->
    %% head of empty list returns none, fromMaybe gives default
    Default = catena_prelude:fromMaybe(0, catena_prelude:head([])),
    ?assertEqual(0, Default),
    %% head of non-empty list returns value
    Value = catena_prelude:fromMaybe(0, catena_prelude:head([42])),
    ?assertEqual(42, Value).

%% Test maybe function
maybe_function_test() ->
    Double = fun(X) -> X * 2 end,
    %% Apply double if value exists
    Result1 = catena_prelude:'maybe'(0, Double, {some, 21}),
    ?assertEqual(42, Result1),
    %% Return default if no value
    Result2 = catena_prelude:'maybe'(0, Double, none),
    ?assertEqual(0, Result2).

%% Test fromResult with pipeline result
fromResult_pipeline_test() ->
    SafeDiv = fun(X, Y) ->
        case Y of
            0 -> {err, division_by_zero};
            _ -> {ok, X div Y}
        end
    end,
    %% Success case
    Value = catena_prelude:fromResult(0, SafeDiv(10, 2)),
    ?assertEqual(5, Value),
    %% Error case
    Default = catena_prelude:fromResult(0, SafeDiv(10, 0)),
    ?assertEqual(0, Default).

%%====================================================================
%% Functor fmap Integration Tests
%%====================================================================

%% Test fmap on list
fmap_list_test() ->
    Result = catena_prelude:fmap(fun(X) -> X * 2 end, [1, 2, 3]),
    ?assertEqual([2, 4, 6], Result).

%% Test fmap on Maybe
fmap_maybe_test() ->
    Double = fun(X) -> X * 2 end,
    ?assertEqual({some, 42}, catena_prelude:fmap(Double, {some, 21})),
    ?assertEqual(none, catena_prelude:fmap(Double, none)).

%% Test fmap on Result
fmap_result_test() ->
    Double = fun(X) -> X * 2 end,
    ?assertEqual({ok, 42}, catena_prelude:fmap(Double, {ok, 21})),
    ?assertEqual({err, "oops"}, catena_prelude:fmap(Double, {err, "oops"})).

%%====================================================================
%% Applicative Integration Tests
%%====================================================================

%% Test pure
pure_test() ->
    ?assertEqual({some, 42}, catena_prelude:pure(42)).

%% Test apply_f on Maybe
apply_maybe_test() ->
    Double = fun(X) -> X * 2 end,
    ?assertEqual({some, 42}, catena_prelude:apply_f({some, Double}, {some, 21})),
    ?assertEqual(none, catena_prelude:apply_f({some, Double}, none)),
    ?assertEqual(none, catena_prelude:apply_f(none, {some, 21})).

%% Test apply_f on Result
apply_result_test() ->
    Double = fun(X) -> X * 2 end,
    ?assertEqual({ok, 42}, catena_prelude:apply_f({ok, Double}, {ok, 21})),
    ?assertEqual({err, "err1"}, catena_prelude:apply_f({err, "err1"}, {ok, 21})),
    ?assertEqual({err, "err2"}, catena_prelude:apply_f({ok, Double}, {err, "err2"})).

%% Test apply_f on List (cartesian product)
apply_list_test() ->
    Funs = [fun(X) -> X * 2 end, fun(X) -> X + 10 end],
    Values = [1, 2, 3],
    Result = catena_prelude:apply_f(Funs, Values),
    %% Each function applied to each value
    ?assertEqual([2, 4, 6, 11, 12, 13], Result).
