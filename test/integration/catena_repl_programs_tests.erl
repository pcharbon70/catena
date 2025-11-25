%% @doc End-to-End REPL Program Tests (Phase 2.4.3)
%%
%% These tests implement classic functional programming algorithms using
%% the prelude functions, demonstrating that complex programs can be built
%% from the provided primitives.
%%
%% Note: Due to current type inference limitations with REPL-based Catena
%% syntax, these tests exercise the underlying Erlang prelude implementations
%% directly, which represent the runtime behavior of Catena programs.
-module(catena_repl_programs_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 2.4.3.1 Quicksort Implementation
%%====================================================================

%% Quicksort using filter and append from prelude
quicksort([]) -> [];
quicksort([Pivot | Rest]) ->
    Left = catena_prelude:filter(fun(X) -> X < Pivot end, Rest),
    Right = catena_prelude:filter(fun(X) -> X >= Pivot end, Rest),
    catena_prelude:append(
        quicksort(Left),
        catena_prelude:append([Pivot], quicksort(Right))
    ).

quicksort_empty_list_test() ->
    ?assertEqual([], quicksort([])).

quicksort_single_element_test() ->
    ?assertEqual([42], quicksort([42])).

quicksort_sorted_list_test() ->
    ?assertEqual([1, 2, 3, 4, 5], quicksort([1, 2, 3, 4, 5])).

quicksort_reverse_sorted_test() ->
    ?assertEqual([1, 2, 3, 4, 5], quicksort([5, 4, 3, 2, 1])).

quicksort_unsorted_test() ->
    %% Input has duplicates: [3, 1, 4, 1, 5, 9, 2, 6]
    %% Sorted output preserves all elements including duplicates
    ?assertEqual([1, 1, 2, 3, 4, 5, 6, 9], quicksort([3, 1, 4, 1, 5, 9, 2, 6])).

quicksort_with_duplicates_test() ->
    ?assertEqual([1, 1, 2, 2, 3, 3], quicksort([3, 1, 2, 3, 1, 2])).

quicksort_negative_numbers_test() ->
    ?assertEqual([-5, -2, -1, 0, 3, 7], quicksort([3, -2, 7, -5, 0, -1])).

%%====================================================================
%% 2.4.3.2 Recursive Fibonacci
%%====================================================================

%% Standard recursive fibonacci
fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N - 1) + fib(N - 2).

%% Fibonacci using fold (iterative-style, more efficient)
fib_fold(N) ->
    {Result, _} = catena_prelude:fold(
        fun(Acc, _) ->
            {A, B} = Acc,
            {B, A + B}
        end,
        {0, 1},
        lists:seq(1, N)
    ),
    Result.

fib_base_cases_test() ->
    ?assertEqual(0, fib(0)),
    ?assertEqual(1, fib(1)).

fib_sequence_test() ->
    Expected = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34],
    Result = catena_prelude:map(fun fib/1, lists:seq(0, 9)),
    ?assertEqual(Expected, Result).

fib_fold_base_cases_test() ->
    ?assertEqual(0, fib_fold(0)),
    ?assertEqual(1, fib_fold(1)).

fib_fold_sequence_test() ->
    Expected = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34],
    Result = catena_prelude:map(fun fib_fold/1, lists:seq(0, 9)),
    ?assertEqual(Expected, Result).

fib_fold_larger_numbers_test() ->
    %% fib_fold can compute larger fibonacci numbers efficiently
    ?assertEqual(55, fib_fold(10)),
    ?assertEqual(610, fib_fold(15)),
    ?assertEqual(6765, fib_fold(20)).

%%====================================================================
%% 2.4.3.3 Expression Evaluator with Result
%%====================================================================

%% Simple expression type
%% {num, N} - integer literal
%% {add, E1, E2} - addition
%% {sub, E1, E2} - subtraction
%% {mul, E1, E2} - multiplication
%% {div_expr, E1, E2} - division (can fail)

eval_expr({num, N}) ->
    {ok, N};
eval_expr({add, E1, E2}) ->
    catena_prelude:bind(
        eval_expr(E1),
        fun(V1) ->
            catena_prelude:bind(
                eval_expr(E2),
                fun(V2) -> {ok, V1 + V2} end
            )
        end
    );
eval_expr({sub, E1, E2}) ->
    catena_prelude:bind(
        eval_expr(E1),
        fun(V1) ->
            catena_prelude:bind(
                eval_expr(E2),
                fun(V2) -> {ok, V1 - V2} end
            )
        end
    );
eval_expr({mul, E1, E2}) ->
    catena_prelude:bind(
        eval_expr(E1),
        fun(V1) ->
            catena_prelude:bind(
                eval_expr(E2),
                fun(V2) -> {ok, V1 * V2} end
            )
        end
    );
eval_expr({div_expr, E1, E2}) ->
    catena_prelude:bind(
        eval_expr(E1),
        fun(V1) ->
            catena_prelude:bind(
                eval_expr(E2),
                fun(V2) ->
                    case V2 of
                        0 -> {err, division_by_zero};
                        _ -> {ok, V1 div V2}
                    end
                end
            )
        end
    ).

eval_literal_test() ->
    ?assertEqual({ok, 42}, eval_expr({num, 42})).

eval_addition_test() ->
    Expr = {add, {num, 2}, {num, 3}},
    ?assertEqual({ok, 5}, eval_expr(Expr)).

eval_nested_expression_test() ->
    %% (2 + 3) * (4 - 1) = 5 * 3 = 15
    Expr = {mul, {add, {num, 2}, {num, 3}}, {sub, {num, 4}, {num, 1}}},
    ?assertEqual({ok, 15}, eval_expr(Expr)).

eval_division_success_test() ->
    Expr = {div_expr, {num, 10}, {num, 2}},
    ?assertEqual({ok, 5}, eval_expr(Expr)).

eval_division_by_zero_test() ->
    Expr = {div_expr, {num, 10}, {num, 0}},
    ?assertEqual({err, division_by_zero}, eval_expr(Expr)).

eval_error_propagation_test() ->
    %% Error in nested expression should propagate
    %% (10 / 0) + 5 should fail
    Expr = {add, {div_expr, {num, 10}, {num, 0}}, {num, 5}},
    ?assertEqual({err, division_by_zero}, eval_expr(Expr)).

eval_complex_expression_test() ->
    %% ((10 + 5) / 3) * 2 = (15 / 3) * 2 = 5 * 2 = 10
    Expr = {mul, {div_expr, {add, {num, 10}, {num, 5}}, {num, 3}}, {num, 2}},
    ?assertEqual({ok, 10}, eval_expr(Expr)).

%%====================================================================
%% 2.4.3.4 Tree Traversal with Maybe
%%====================================================================

%% Binary tree type
%% leaf - empty tree
%% {node, Value, Left, Right} - tree node

%% Find a value in the tree
tree_find(_, leaf) ->
    none;
tree_find(Target, {node, Value, _, _}) when Target =:= Value ->
    {some, Value};
tree_find(Target, {node, Value, Left, Right}) when Target < Value ->
    tree_find(Target, Left);
tree_find(Target, {node, _Value, _Left, Right}) ->
    tree_find(Target, Right).

%% Get the minimum value in a tree
tree_min(leaf) ->
    none;
tree_min({node, Value, leaf, _}) ->
    {some, Value};
tree_min({node, _, Left, _}) ->
    tree_min(Left).

%% Get the maximum value in a tree
tree_max(leaf) ->
    none;
tree_max({node, Value, _, leaf}) ->
    {some, Value};
tree_max({node, _, _, Right}) ->
    tree_max(Right).

%% In-order traversal (returns sorted list for BST)
tree_inorder(leaf) ->
    [];
tree_inorder({node, Value, Left, Right}) ->
    catena_prelude:append(
        tree_inorder(Left),
        catena_prelude:append([Value], tree_inorder(Right))
    ).

%% Build a BST from a list
tree_insert(Value, leaf) ->
    {node, Value, leaf, leaf};
tree_insert(Value, {node, V, Left, Right}) when Value < V ->
    {node, V, tree_insert(Value, Left), Right};
tree_insert(Value, {node, V, Left, Right}) ->
    {node, V, Left, tree_insert(Value, Right)}.

tree_from_list(List) ->
    catena_prelude:fold(fun(Tree, V) -> tree_insert(V, Tree) end, leaf, List).

%% Test tree
%% Sample tree:     5
%%                /   \
%%               3     7
%%              / \   / \
%%             1   4 6   9
sample_tree() ->
    tree_from_list([5, 3, 7, 1, 4, 6, 9]).

tree_find_existing_test() ->
    Tree = sample_tree(),
    ?assertEqual({some, 5}, tree_find(5, Tree)),
    ?assertEqual({some, 1}, tree_find(1, Tree)),
    ?assertEqual({some, 9}, tree_find(9, Tree)).

tree_find_nonexisting_test() ->
    Tree = sample_tree(),
    ?assertEqual(none, tree_find(0, Tree)),
    ?assertEqual(none, tree_find(8, Tree)),
    ?assertEqual(none, tree_find(100, Tree)).

tree_find_empty_test() ->
    ?assertEqual(none, tree_find(42, leaf)).

tree_min_test() ->
    Tree = sample_tree(),
    ?assertEqual({some, 1}, tree_min(Tree)).

tree_min_empty_test() ->
    ?assertEqual(none, tree_min(leaf)).

tree_max_test() ->
    Tree = sample_tree(),
    ?assertEqual({some, 9}, tree_max(Tree)).

tree_max_empty_test() ->
    ?assertEqual(none, tree_max(leaf)).

tree_inorder_test() ->
    Tree = sample_tree(),
    ?assertEqual([1, 3, 4, 5, 6, 7, 9], tree_inorder(Tree)).

tree_inorder_empty_test() ->
    ?assertEqual([], tree_inorder(leaf)).

tree_from_list_sorts_test() ->
    Unsorted = [5, 2, 8, 1, 9, 3, 7, 4, 6],
    Tree = tree_from_list(Unsorted),
    Sorted = tree_inorder(Tree),
    ?assertEqual([1, 2, 3, 4, 5, 6, 7, 8, 9], Sorted).

%% Tree traversal with Maybe chaining
tree_find_then_transform_test() ->
    Tree = sample_tree(),
    %% Find a value, then double it if found
    Double = fun(X) -> {some, X * 2} end,
    Result = catena_prelude:bind(tree_find(5, Tree), Double),
    ?assertEqual({some, 10}, Result),
    %% If not found, chain short-circuits
    Result2 = catena_prelude:bind(tree_find(100, Tree), Double),
    ?assertEqual(none, Result2).

tree_min_with_default_test() ->
    Tree = sample_tree(),
    MinVal = catena_prelude:fromMaybe(0, tree_min(Tree)),
    ?assertEqual(1, MinVal),
    EmptyMinVal = catena_prelude:fromMaybe(0, tree_min(leaf)),
    ?assertEqual(0, EmptyMinVal).

%%====================================================================
%% Additional Algorithm Tests
%%====================================================================

%% Merge sort using prelude functions
merge([], Ys) -> Ys;
merge(Xs, []) -> Xs;
merge([X | Xs], [Y | Ys]) when X =< Y ->
    [X | merge(Xs, [Y | Ys])];
merge([X | Xs], [Y | Ys]) ->
    [Y | merge([X | Xs], Ys)].

mergesort([]) -> [];
mergesort([X]) -> [X];
mergesort(List) ->
    Len = catena_prelude:length(List),
    Mid = Len div 2,
    Left = catena_prelude:take(Mid, List),
    Right = catena_prelude:drop(Mid, List),
    merge(mergesort(Left), mergesort(Right)).

mergesort_test() ->
    ?assertEqual([], mergesort([])),
    ?assertEqual([1], mergesort([1])),
    ?assertEqual([1, 2, 3, 4, 5], mergesort([5, 4, 3, 2, 1])),
    %% Input has duplicates: [3, 1, 4, 1, 5]
    ?assertEqual([1, 1, 3, 4, 5], mergesort([3, 1, 4, 1, 5])).

%% Sum of squares using map and fold
sum_of_squares(List) ->
    Squared = catena_prelude:map(fun(X) -> X * X end, List),
    catena_prelude:fold(fun(Acc, X) -> Acc + X end, 0, Squared).

sum_of_squares_test() ->
    %% 1² + 2² + 3² + 4² = 1 + 4 + 9 + 16 = 30
    ?assertEqual(30, sum_of_squares([1, 2, 3, 4])),
    ?assertEqual(0, sum_of_squares([])),
    ?assertEqual(1, sum_of_squares([1])).

%% All elements satisfy predicate (implemented with filter)
all(Pred, List) ->
    catena_prelude:length(catena_prelude:filter(Pred, List)) =:= catena_prelude:length(List).

%% Any element satisfies predicate
any(Pred, List) ->
    catena_prelude:length(catena_prelude:filter(Pred, List)) > 0.

all_any_test() ->
    IsPositive = fun(X) -> X > 0 end,
    ?assert(all(IsPositive, [1, 2, 3, 4])),
    ?assertNot(all(IsPositive, [1, 2, -1, 4])),
    ?assert(any(IsPositive, [-1, -2, 3, -4])),
    ?assertNot(any(IsPositive, [-1, -2, -3])).

%% Flatten nested list using foldRight
flatten([]) -> [];
flatten([H | T]) when is_list(H) ->
    catena_prelude:append(flatten(H), flatten(T));
flatten([H | T]) ->
    [H | flatten(T)].

flatten_test() ->
    ?assertEqual([1, 2, 3, 4, 5, 6], flatten([[1, 2], [3, [4, 5]], 6])),
    ?assertEqual([], flatten([])),
    ?assertEqual([1, 2, 3], flatten([1, 2, 3])).

%% Zip two lists (using fold)
zip(Xs, Ys) ->
    {Result, _} = catena_prelude:fold(
        fun({Acc, []}, _) -> {Acc, []};
           ({Acc, [Y | RestY]}, X) -> {Acc ++ [{X, Y}], RestY}
        end,
        {[], Ys},
        Xs
    ),
    Result.

zip_test() ->
    ?assertEqual([{1, a}, {2, b}, {3, c}], zip([1, 2, 3], [a, b, c])),
    ?assertEqual([{1, a}, {2, b}], zip([1, 2, 3], [a, b])),
    ?assertEqual([{1, a}, {2, b}], zip([1, 2], [a, b, c])),
    ?assertEqual([], zip([], [1, 2, 3])).
