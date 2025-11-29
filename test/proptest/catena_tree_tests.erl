%% @doc Unit Tests for Rose Tree (Sections 1.1.1 - 1.1.3)
%%
%% Tests covering:
%% - Section 1.1.1: Tree construction, singleton, unfold, root, children
%% - Section 1.1.2: Comonadic operations (extract, duplicate, extend)
%% - Section 1.1.3: Functor instance (map)
%% - Comonad and Functor law verification
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
%% Test: extract/1 (Section 1.1.2)
%%====================================================================

extract_returns_root_value_test() ->
    Tree = catena_tree:singleton(42),
    ?assertEqual(42, catena_tree:extract(Tree)).

extract_equivalent_to_root_test() ->
    Tree = catena_tree:tree(hello, fun() ->
        [catena_tree:singleton(world)]
    end),
    ?assertEqual(catena_tree:root(Tree), catena_tree:extract(Tree)).

extract_does_not_evaluate_children_test() ->
    Counter = spawn(fun() -> counter_loop(0) end),

    Tree = catena_tree:tree(42, fun() ->
        Counter ! increment,
        [catena_tree:singleton(0)]
    end),

    ?assertEqual(42, catena_tree:extract(Tree)),

    Counter ! {get, self()},
    receive
        {count, Count} -> ?assertEqual(0, Count)
    after 100 ->
        ?assert(false)
    end,

    Counter ! stop.

%%====================================================================
%% Test: duplicate/1 (Section 1.1.2)
%%====================================================================

duplicate_root_is_original_tree_test() ->
    Tree = catena_tree:singleton(42),
    Dup = catena_tree:duplicate(Tree),
    %% The root of the duplicated tree is the original tree
    ?assertEqual(Tree, catena_tree:extract(Dup)).

duplicate_singleton_test() ->
    Tree = catena_tree:singleton(42),
    Dup = catena_tree:duplicate(Tree),
    %% Singleton has no children, so duplicated also has no children
    ?assertEqual([], catena_tree:children(Dup)).

duplicate_preserves_structure_test() ->
    %% Tree: 1 -> [2, 3]
    Tree = catena_tree:tree(1, fun() ->
        [catena_tree:singleton(2), catena_tree:singleton(3)]
    end),

    Dup = catena_tree:duplicate(Tree),

    %% Root of Dup is the original tree
    RootTree = catena_tree:extract(Dup),
    ?assertEqual(1, catena_tree:extract(RootTree)),

    %% Children of Dup are duplicated subtrees
    [DupChild2, DupChild3] = catena_tree:children(Dup),

    %% Each child's root is the subtree at that position
    Child2Tree = catena_tree:extract(DupChild2),
    ?assertEqual(2, catena_tree:extract(Child2Tree)),

    Child3Tree = catena_tree:extract(DupChild3),
    ?assertEqual(3, catena_tree:extract(Child3Tree)).

duplicate_nested_tree_test() ->
    %% Tree: A -> [B -> [C]]
    TreeC = catena_tree:singleton(c),
    TreeB = catena_tree:tree(b, fun() -> [TreeC] end),
    TreeA = catena_tree:tree(a, fun() -> [TreeB] end),

    Dup = catena_tree:duplicate(TreeA),

    %% Root contains full tree A
    RootTree = catena_tree:extract(Dup),
    ?assertEqual(a, catena_tree:extract(RootTree)),

    %% First child contains tree B
    [DupB] = catena_tree:children(Dup),
    TreeBFromDup = catena_tree:extract(DupB),
    ?assertEqual(b, catena_tree:extract(TreeBFromDup)),

    %% Grandchild contains tree C
    [DupC] = catena_tree:children(DupB),
    TreeCFromDup = catena_tree:extract(DupC),
    ?assertEqual(c, catena_tree:extract(TreeCFromDup)).

%%====================================================================
%% Test: extend/2 (Section 1.1.2)
%%====================================================================

extend_applies_function_to_subtrees_test() ->
    %% Tree: 1 -> [2, 3]
    Tree = catena_tree:tree(1, fun() ->
        [catena_tree:singleton(2), catena_tree:singleton(3)]
    end),

    %% Function that extracts the root value
    ExtractFn = fun catena_tree:extract/1,
    Extended = catena_tree:extend(ExtractFn, Tree),

    %% Should be equivalent to original tree since extract just gets root
    ?assertEqual(1, catena_tree:extract(Extended)),
    [C1, C2] = catena_tree:children(Extended),
    ?assertEqual(2, catena_tree:extract(C1)),
    ?assertEqual(3, catena_tree:extract(C2)).

extend_count_nodes_test() ->
    %% Tree: root -> [child1, child2]
    Tree = catena_tree:tree(root, fun() ->
        [catena_tree:singleton(child1), catena_tree:singleton(child2)]
    end),

    %% Function that counts nodes in subtree
    CountNodes = fun CountNodes(T) ->
        1 + lists:sum([CountNodes(C) || C <- catena_tree:children(T)])
    end,

    Counted = catena_tree:extend(CountNodes, Tree),

    %% Root subtree has 3 nodes (root + 2 children)
    ?assertEqual(3, catena_tree:extract(Counted)),

    %% Each child subtree has 1 node
    [C1, C2] = catena_tree:children(Counted),
    ?assertEqual(1, catena_tree:extract(C1)),
    ?assertEqual(1, catena_tree:extract(C2)).

extend_depth_calculation_test() ->
    %% Tree: a -> [b -> [c], d]
    TreeC = catena_tree:singleton(c),
    TreeB = catena_tree:tree(b, fun() -> [TreeC] end),
    TreeD = catena_tree:singleton(d),
    TreeA = catena_tree:tree(a, fun() -> [TreeB, TreeD] end),

    %% Function that calculates depth of subtree
    Depth = fun Depth(T) ->
        case catena_tree:children(T) of
            [] -> 1;
            Children -> 1 + lists:max([Depth(C) || C <- Children])
        end
    end,

    Depths = catena_tree:extend(Depth, TreeA),

    %% Root depth is 3 (a -> b -> c)
    ?assertEqual(3, catena_tree:extract(Depths)),

    %% First child (b -> c) has depth 2
    [DepthB, DepthD] = catena_tree:children(Depths),
    ?assertEqual(2, catena_tree:extract(DepthB)),

    %% d is a leaf with depth 1
    ?assertEqual(1, catena_tree:extract(DepthD)).

extend_preserves_laziness_test() ->
    Counter = spawn(fun() -> counter_loop(0) end),

    Tree = catena_tree:tree(1, fun() ->
        Counter ! increment,
        [catena_tree:singleton(2)]
    end),

    %% extend should be lazy
    Extended = catena_tree:extend(fun catena_tree:extract/1, Tree),

    %% Just creating extended tree doesn't force children
    Counter ! {get, self()},
    receive
        {count, Count0} -> ?assertEqual(0, Count0)
    after 100 ->
        ?assert(false)
    end,

    %% Accessing root of extended tree doesn't force children
    %% (because the function only sees the tree, doesn't have to evaluate children)
    _ = catena_tree:extract(Extended),
    Counter ! {get, self()},
    receive
        {count, Count1} -> ?assertEqual(0, Count1)
    after 100 ->
        ?assert(false)
    end,

    %% Accessing children of extended tree forces evaluation
    _ = catena_tree:children(Extended),
    Counter ! {get, self()},
    receive
        {count, Count2} -> ?assertEqual(1, Count2)
    after 100 ->
        ?assert(false)
    end,

    Counter ! stop.

%%====================================================================
%% Test: map/2 (Section 1.1.3)
%%====================================================================

map_transforms_root_test() ->
    Tree = catena_tree:singleton(5),
    Mapped = catena_tree:map(fun(X) -> X * 2 end, Tree),
    ?assertEqual(10, catena_tree:extract(Mapped)).

map_transforms_all_values_test() ->
    Tree = catena_tree:tree(1, fun() ->
        [catena_tree:singleton(2), catena_tree:singleton(3)]
    end),

    Mapped = catena_tree:map(fun(X) -> X * 10 end, Tree),

    ?assertEqual(10, catena_tree:extract(Mapped)),
    [C1, C2] = catena_tree:children(Mapped),
    ?assertEqual(20, catena_tree:extract(C1)),
    ?assertEqual(30, catena_tree:extract(C2)).

map_preserves_structure_test() ->
    %% Tree: a -> [b -> [c, d], e]
    TreeC = catena_tree:singleton(c),
    TreeD = catena_tree:singleton(d),
    TreeB = catena_tree:tree(b, fun() -> [TreeC, TreeD] end),
    TreeE = catena_tree:singleton(e),
    TreeA = catena_tree:tree(a, fun() -> [TreeB, TreeE] end),

    %% Map atoms to uppercase strings
    ToUpper = fun(Atom) -> string:uppercase(atom_to_list(Atom)) end,
    Mapped = catena_tree:map(ToUpper, TreeA),

    ?assertEqual("A", catena_tree:extract(Mapped)),
    [MB, ME] = catena_tree:children(Mapped),
    ?assertEqual("B", catena_tree:extract(MB)),
    ?assertEqual("E", catena_tree:extract(ME)),
    [MC, MD] = catena_tree:children(MB),
    ?assertEqual("C", catena_tree:extract(MC)),
    ?assertEqual("D", catena_tree:extract(MD)).

map_is_lazy_test() ->
    Counter = spawn(fun() -> counter_loop(0) end),

    Tree = catena_tree:tree(1, fun() ->
        Counter ! increment,
        [catena_tree:singleton(2)]
    end),

    Mapped = catena_tree:map(fun(X) -> X * 2 end, Tree),

    %% Just mapping doesn't force children
    Counter ! {get, self()},
    receive
        {count, Count0} -> ?assertEqual(0, Count0)
    after 100 ->
        ?assert(false)
    end,

    %% Accessing root doesn't force children
    ?assertEqual(2, catena_tree:extract(Mapped)),
    Counter ! {get, self()},
    receive
        {count, Count1} -> ?assertEqual(0, Count1)
    after 100 ->
        ?assert(false)
    end,

    %% Accessing children forces evaluation
    _ = catena_tree:children(Mapped),
    Counter ! {get, self()},
    receive
        {count, Count2} -> ?assertEqual(1, Count2)
    after 100 ->
        ?assert(false)
    end,

    Counter ! stop.

%%====================================================================
%% Test: Comonad Laws (Section 1.1.2)
%%====================================================================

%% Law 1: extract . duplicate == id
comonad_law_extract_duplicate_test() ->
    Tree = catena_tree:tree(42, fun() ->
        [catena_tree:singleton(21), catena_tree:singleton(0)]
    end),

    %% extract(duplicate(tree)) should equal tree
    Dup = catena_tree:duplicate(Tree),
    Extracted = catena_tree:extract(Dup),

    ?assertEqual(catena_tree:extract(Tree), catena_tree:extract(Extracted)),
    ?assertEqual(
        [catena_tree:extract(C) || C <- catena_tree:children(Tree)],
        [catena_tree:extract(C) || C <- catena_tree:children(Extracted)]
    ).

%% Law 2: map extract . duplicate == id
comonad_law_map_extract_duplicate_test() ->
    Tree = catena_tree:tree(1, fun() ->
        [catena_tree:singleton(2), catena_tree:singleton(3)]
    end),

    %% map(extract, duplicate(tree)) should give back original values
    Dup = catena_tree:duplicate(Tree),
    Mapped = catena_tree:map(fun catena_tree:extract/1, Dup),

    %% Check structure matches
    ?assertEqual(catena_tree:extract(Tree), catena_tree:extract(Mapped)),
    ?assertEqual(
        [catena_tree:extract(C) || C <- catena_tree:children(Tree)],
        [catena_tree:extract(C) || C <- catena_tree:children(Mapped)]
    ).

%% Law 3: duplicate . duplicate == map duplicate . duplicate
comonad_law_duplicate_duplicate_test() ->
    Tree = catena_tree:tree(a, fun() ->
        [catena_tree:singleton(b)]
    end),

    %% duplicate(duplicate(tree)) == map(duplicate, duplicate(tree))
    DupDup = catena_tree:duplicate(catena_tree:duplicate(Tree)),
    MapDupDup = catena_tree:map(fun catena_tree:duplicate/1, catena_tree:duplicate(Tree)),

    %% Compare by extracting nested values
    %% Both should have the same structure
    DDRoot = catena_tree:extract(catena_tree:extract(catena_tree:extract(DupDup))),
    MDRoot = catena_tree:extract(catena_tree:extract(catena_tree:extract(MapDupDup))),
    ?assertEqual(DDRoot, MDRoot).

%% Law: extend extract == id
comonad_law_extend_extract_test() ->
    Tree = catena_tree:tree(10, fun() ->
        [catena_tree:singleton(5), catena_tree:singleton(0)]
    end),

    %% extend(extract, tree) should equal tree (in terms of values)
    Extended = catena_tree:extend(fun catena_tree:extract/1, Tree),

    ?assertEqual(catena_tree:extract(Tree), catena_tree:extract(Extended)),
    ?assertEqual(
        [catena_tree:extract(C) || C <- catena_tree:children(Tree)],
        [catena_tree:extract(C) || C <- catena_tree:children(Extended)]
    ).

%% Law: extract . extend f == f
comonad_law_extract_extend_test() ->
    Tree = catena_tree:tree(1, fun() ->
        [catena_tree:singleton(2), catena_tree:singleton(3)]
    end),

    %% A function that sums all values in a subtree
    SumTree = fun SumTree(T) ->
        catena_tree:extract(T) +
        lists:sum([SumTree(C) || C <- catena_tree:children(T)])
    end,

    %% extract(extend(f, tree)) should equal f(tree)
    Extended = catena_tree:extend(SumTree, Tree),
    ?assertEqual(SumTree(Tree), catena_tree:extract(Extended)).

%%====================================================================
%% Test: Functor Laws (Section 1.1.3)
%%====================================================================

%% Law 1: map id == id
functor_law_identity_test() ->
    Tree = catena_tree:tree(42, fun() ->
        [catena_tree:singleton(21), catena_tree:singleton(0)]
    end),

    %% map(fun(X) -> X end, tree) should give back equivalent tree
    Mapped = catena_tree:map(fun(X) -> X end, Tree),

    ?assertEqual(catena_tree:extract(Tree), catena_tree:extract(Mapped)),
    ?assertEqual(
        [catena_tree:extract(C) || C <- catena_tree:children(Tree)],
        [catena_tree:extract(C) || C <- catena_tree:children(Mapped)]
    ).

%% Law 2: map (f . g) == map f . map g
functor_law_composition_test() ->
    Tree = catena_tree:tree(2, fun() ->
        [catena_tree:singleton(3), catena_tree:singleton(4)]
    end),

    F = fun(X) -> X * 2 end,
    G = fun(X) -> X + 1 end,
    Composed = fun(X) -> F(G(X)) end,

    %% map(f . g, tree)
    MapComposed = catena_tree:map(Composed, Tree),

    %% map(f, map(g, tree))
    MapG = catena_tree:map(G, Tree),
    MapFMapG = catena_tree:map(F, MapG),

    %% Both should produce same values
    ?assertEqual(catena_tree:extract(MapComposed), catena_tree:extract(MapFMapG)),
    ?assertEqual(
        [catena_tree:extract(C) || C <- catena_tree:children(MapComposed)],
        [catena_tree:extract(C) || C <- catena_tree:children(MapFMapG)]
    ).

%%====================================================================
%% Test: extend == map . duplicate equivalence
%%====================================================================

extend_equals_map_duplicate_test() ->
    Tree = catena_tree:tree(10, fun() ->
        [catena_tree:singleton(5), catena_tree:singleton(2)]
    end),

    %% A test function
    SumFn = fun SumFn(T) ->
        catena_tree:extract(T) +
        lists:sum([SumFn(C) || C <- catena_tree:children(T)])
    end,

    %% extend(f, tree)
    Extended = catena_tree:extend(SumFn, Tree),

    %% map(f, duplicate(tree))
    Dup = catena_tree:duplicate(Tree),
    MapDup = catena_tree:map(SumFn, Dup),

    %% Both should produce same result
    ?assertEqual(catena_tree:extract(Extended), catena_tree:extract(MapDup)),
    ?assertEqual(
        [catena_tree:extract(C) || C <- catena_tree:children(Extended)],
        [catena_tree:extract(C) || C <- catena_tree:children(MapDup)]
    ).

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
