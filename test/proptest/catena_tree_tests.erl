%% @doc Unit Tests for Rose Tree Type Definition (Section 1.1.1)
%%
%% Tests covering:
%% - Tree construction with tree/2
%% - Singleton creation with singleton/1
%% - Tree unfolding with unfold/2
%% - Root extraction with root/1
%% - Children access with children/1
%% - Lazy evaluation of children
-module(catena_tree_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test: tree/2 constructor
%%====================================================================

tree_creates_tree_with_value_test() ->
    Tree = catena_tree:tree(42, fun() -> [] end),
    ?assertEqual(42, catena_tree:root(Tree)).

tree_creates_tree_with_children_thunk_test() ->
    Child1 = catena_tree:singleton(1),
    Child2 = catena_tree:singleton(2),
    Tree = catena_tree:tree(0, fun() -> [Child1, Child2] end),
    Children = catena_tree:children(Tree),
    ?assertEqual(2, length(Children)),
    ?assertEqual(1, catena_tree:root(hd(Children))),
    ?assertEqual(2, catena_tree:root(lists:last(Children))).

tree_with_empty_children_test() ->
    Tree = catena_tree:tree(100, fun() -> [] end),
    ?assertEqual(100, catena_tree:root(Tree)),
    ?assertEqual([], catena_tree:children(Tree)).

tree_with_nested_children_test() ->
    %% Create a tree: 0 -> [1 -> [2], 3]
    Leaf2 = catena_tree:singleton(2),
    Child1 = catena_tree:tree(1, fun() -> [Leaf2] end),
    Child3 = catena_tree:singleton(3),
    Root = catena_tree:tree(0, fun() -> [Child1, Child3] end),

    ?assertEqual(0, catena_tree:root(Root)),
    [C1, C3] = catena_tree:children(Root),
    ?assertEqual(1, catena_tree:root(C1)),
    ?assertEqual(3, catena_tree:root(C3)),
    [C2] = catena_tree:children(C1),
    ?assertEqual(2, catena_tree:root(C2)),
    ?assertEqual([], catena_tree:children(C2)).

%%====================================================================
%% Test: singleton/1
%%====================================================================

singleton_creates_leaf_node_test() ->
    Leaf = catena_tree:singleton(42),
    ?assertEqual(42, catena_tree:root(Leaf)),
    ?assertEqual([], catena_tree:children(Leaf)).

singleton_with_different_types_test() ->
    %% Integer
    IntLeaf = catena_tree:singleton(123),
    ?assertEqual(123, catena_tree:root(IntLeaf)),

    %% Atom
    AtomLeaf = catena_tree:singleton(hello),
    ?assertEqual(hello, catena_tree:root(AtomLeaf)),

    %% Tuple
    TupleLeaf = catena_tree:singleton({ok, value}),
    ?assertEqual({ok, value}, catena_tree:root(TupleLeaf)),

    %% List
    ListLeaf = catena_tree:singleton([1, 2, 3]),
    ?assertEqual([1, 2, 3], catena_tree:root(ListLeaf)),

    %% Map
    MapLeaf = catena_tree:singleton(#{key => value}),
    ?assertEqual(#{key => value}, catena_tree:root(MapLeaf)),

    %% Binary
    BinLeaf = catena_tree:singleton(<<"binary">>),
    ?assertEqual(<<"binary">>, catena_tree:root(BinLeaf)).

singleton_has_no_children_test() ->
    Leaf = catena_tree:singleton(any_value),
    ?assertEqual([], catena_tree:children(Leaf)).

%%====================================================================
%% Test: unfold/2
%%====================================================================

unfold_creates_tree_from_seed_test() ->
    %% Simple unfold: value equals seed, no children
    ExpandFn = fun(Seed) -> {Seed, []} end,
    Tree = catena_tree:unfold(42, ExpandFn),
    ?assertEqual(42, catena_tree:root(Tree)),
    ?assertEqual([], catena_tree:children(Tree)).

unfold_with_children_test() ->
    %% Unfold that creates one level of children
    ExpandFn = fun(Seed) ->
        case Seed of
            0 -> {0, []};
            N -> {N, [0]}  % Single child with seed 0
        end
    end,
    Tree = catena_tree:unfold(5, ExpandFn),
    ?assertEqual(5, catena_tree:root(Tree)),
    [Child] = catena_tree:children(Tree),
    ?assertEqual(0, catena_tree:root(Child)),
    ?assertEqual([], catena_tree:children(Child)).

unfold_integer_shrinking_test() ->
    %% Classic integer shrinking toward 0
    ShrinkInt = fun(N) ->
        Shrinks = if
            N =:= 0 -> [];
            N > 0 -> [N div 2, N - 1];
            N < 0 -> [N div 2, N + 1]
        end,
        {N, Shrinks}
    end,

    Tree = catena_tree:unfold(10, ShrinkInt),
    ?assertEqual(10, catena_tree:root(Tree)),

    %% First level children: 10 div 2 = 5, 10 - 1 = 9
    [Child5, Child9] = catena_tree:children(Tree),
    ?assertEqual(5, catena_tree:root(Child5)),
    ?assertEqual(9, catena_tree:root(Child9)),

    %% Second level from 5: 5 div 2 = 2, 5 - 1 = 4
    [Child2, Child4] = catena_tree:children(Child5),
    ?assertEqual(2, catena_tree:root(Child2)),
    ?assertEqual(4, catena_tree:root(Child4)).

unfold_negative_integer_test() ->
    ShrinkInt = fun(N) ->
        Shrinks = if
            N =:= 0 -> [];
            N > 0 -> [N div 2];
            N < 0 -> [N div 2]
        end,
        {N, Shrinks}
    end,

    Tree = catena_tree:unfold(-8, ShrinkInt),
    ?assertEqual(-8, catena_tree:root(Tree)),

    [Child] = catena_tree:children(Tree),
    ?assertEqual(-4, catena_tree:root(Child)).

unfold_transforms_seed_to_value_test() ->
    %% Expand function can transform seed to different value type
    ExpandFn = fun(N) ->
        Value = integer_to_list(N),
        ChildSeeds = if N > 0 -> [N - 1]; true -> [] end,
        {Value, ChildSeeds}
    end,

    Tree = catena_tree:unfold(3, ExpandFn),
    ?assertEqual("3", catena_tree:root(Tree)),
    [Child2] = catena_tree:children(Tree),
    ?assertEqual("2", catena_tree:root(Child2)).

unfold_binary_tree_test() ->
    %% Create a binary tree structure
    ExpandFn = fun(Depth) ->
        case Depth of
            0 -> {leaf, []};
            N -> {node, [N - 1, N - 1]}  % Two children
        end
    end,

    Tree = catena_tree:unfold(2, ExpandFn),
    ?assertEqual(node, catena_tree:root(Tree)),

    [Left, Right] = catena_tree:children(Tree),
    ?assertEqual(node, catena_tree:root(Left)),
    ?assertEqual(node, catena_tree:root(Right)),

    [LL, LR] = catena_tree:children(Left),
    ?assertEqual(leaf, catena_tree:root(LL)),
    ?assertEqual(leaf, catena_tree:root(LR)).

%%====================================================================
%% Test: root/1 (extract)
%%====================================================================

root_extracts_value_test() ->
    Tree = catena_tree:singleton(hello),
    ?assertEqual(hello, catena_tree:root(Tree)).

root_does_not_evaluate_children_test() ->
    %% The counter process tracks if the thunk was called
    Counter = spawn(fun() -> counter_loop(0) end),

    Tree = catena_tree:tree(42, fun() ->
        Counter ! increment,
        [catena_tree:singleton(0)]
    end),

    %% Extract root should NOT evaluate children
    ?assertEqual(42, catena_tree:root(Tree)),

    Counter ! {get, self()},
    receive
        {count, Count} -> ?assertEqual(0, Count)
    after 100 ->
        ?assert(false)
    end,

    Counter ! stop.

%%====================================================================
%% Test: children/1
%%====================================================================

children_returns_empty_for_singleton_test() ->
    Tree = catena_tree:singleton(42),
    ?assertEqual([], catena_tree:children(Tree)).

children_returns_child_trees_test() ->
    Child1 = catena_tree:singleton(1),
    Child2 = catena_tree:singleton(2),
    Child3 = catena_tree:singleton(3),
    Tree = catena_tree:tree(0, fun() -> [Child1, Child2, Child3] end),

    Children = catena_tree:children(Tree),
    ?assertEqual(3, length(Children)),
    Roots = [catena_tree:root(C) || C <- Children],
    ?assertEqual([1, 2, 3], Roots).

children_evaluates_thunk_test() ->
    %% Verify the thunk is actually called
    Counter = spawn(fun() -> counter_loop(0) end),

    Tree = catena_tree:tree(42, fun() ->
        Counter ! increment,
        [catena_tree:singleton(0)]
    end),

    %% Call children - should evaluate thunk
    _Children = catena_tree:children(Tree),

    Counter ! {get, self()},
    receive
        {count, Count} -> ?assertEqual(1, Count)
    after 100 ->
        ?assert(false)
    end,

    Counter ! stop.

children_evaluates_thunk_each_call_test() ->
    %% Each call to children/1 evaluates the thunk
    Counter = spawn(fun() -> counter_loop(0) end),

    Tree = catena_tree:tree(42, fun() ->
        Counter ! increment,
        [catena_tree:singleton(0)]
    end),

    %% Call children multiple times
    _C1 = catena_tree:children(Tree),
    _C2 = catena_tree:children(Tree),
    _C3 = catena_tree:children(Tree),

    Counter ! {get, self()},
    receive
        {count, Count} -> ?assertEqual(3, Count)
    after 100 ->
        ?assert(false)
    end,

    Counter ! stop.

%%====================================================================
%% Test: Lazy evaluation
%%====================================================================

lazy_children_not_evaluated_until_needed_test() ->
    %% Create a deeply nested tree - children should not be evaluated
    %% until explicitly requested
    CallTracker = spawn(fun() -> counter_loop(0) end),

    %% Create tree where each level increments counter when evaluated
    %% We use a recursive helper via process dictionary to avoid closure issues
    MakeTree = fun MakeTreeRec(D) ->
        catena_tree:tree(D, fun() ->
            CallTracker ! increment,
            case D of
                0 -> [];
                _ -> [MakeTreeRec(D - 1)]
            end
        end)
    end,

    Tree = MakeTree(5),

    %% Just creating the tree should not evaluate any children
    CallTracker ! {get, self()},
    receive
        {count, Count0} -> ?assertEqual(0, Count0)
    after 100 ->
        ?assert(false)
    end,

    %% Accessing root should not evaluate children
    ?assertEqual(5, catena_tree:root(Tree)),
    CallTracker ! {get, self()},
    receive
        {count, Count1} -> ?assertEqual(0, Count1)
    after 100 ->
        ?assert(false)
    end,

    %% Accessing children should evaluate only first level
    [Child] = catena_tree:children(Tree),
    CallTracker ! {get, self()},
    receive
        {count, Count2} -> ?assertEqual(1, Count2)
    after 100 ->
        ?assert(false)
    end,

    %% Accessing grandchildren evaluates second level
    _Grandchildren = catena_tree:children(Child),
    CallTracker ! {get, self()},
    receive
        {count, Count3} -> ?assertEqual(2, Count3)
    after 100 ->
        ?assert(false)
    end,

    CallTracker ! stop.

%%====================================================================
%% Test: Various depths and branching factors
%%====================================================================

tree_depth_one_test() ->
    %% Single level with multiple children
    Tree = catena_tree:tree(root, fun() ->
        [catena_tree:singleton(a),
         catena_tree:singleton(b),
         catena_tree:singleton(c)]
    end),

    ?assertEqual(root, catena_tree:root(Tree)),
    Children = catena_tree:children(Tree),
    ?assertEqual(3, length(Children)),
    ?assertEqual([a, b, c], [catena_tree:root(C) || C <- Children]).

tree_deep_linear_test() ->
    %% Deep linear tree (single child at each level)
    DeepTree = lists:foldl(
        fun(N, Child) ->
            catena_tree:tree(N, fun() -> [Child] end)
        end,
        catena_tree:singleton(0),
        lists:seq(1, 10)
    ),

    ?assertEqual(10, catena_tree:root(DeepTree)),

    %% Traverse down to root
    Current = lists:foldl(
        fun(Expected, Tree) ->
            ?assertEqual(Expected, catena_tree:root(Tree)),
            case catena_tree:children(Tree) of
                [] -> Tree;
                [Child] -> Child
            end
        end,
        DeepTree,
        lists:seq(10, 0, -1)
    ),
    ?assertEqual(0, catena_tree:root(Current)).

tree_wide_branching_test() ->
    %% Wide tree with many children
    NumChildren = 100,
    ChildTrees = [catena_tree:singleton(N) || N <- lists:seq(1, NumChildren)],
    Tree = catena_tree:tree(0, fun() -> ChildTrees end),

    ?assertEqual(0, catena_tree:root(Tree)),
    Children = catena_tree:children(Tree),
    ?assertEqual(NumChildren, length(Children)),
    ?assertEqual(lists:seq(1, NumChildren), [catena_tree:root(C) || C <- Children]).

%%====================================================================
%% Helper functions
%%====================================================================

counter_loop(Count) ->
    receive
        increment ->
            counter_loop(Count + 1);
        {get, Pid} ->
            Pid ! {count, Count},
            counter_loop(Count);
        stop ->
            ok
    end.
