%% @doc Rose Tree Data Structure for Integrated Shrinking
%%
%% This module implements the rose tree (multi-way tree) data structure that
%% forms the foundation of Catena's property testing library. Each node contains
%% a value and a lazy list of child trees representing possible shrinks.
%%
%% The tree structure enables integrated shrinking where shrink candidates are
%% generated alongside values, ensuring all shrinks respect the same invariants
%% as the original generation.
%%
%% == Design Philosophy ==
%%
%% Children are represented as a thunk (nullary function) to enable lazy
%% evaluation. This is critical because computing all possible shrinks upfront
%% would be prohibitively expensive for complex types. Children are only
%% evaluated when shrinking actually occurs.
%%
%% == Example ==
%% ```
%% %% Create a tree with value 10 that shrinks to 5, 0
%% Tree = catena_tree:tree(10, fun() ->
%%     [catena_tree:singleton(5), catena_tree:singleton(0)]
%% end),
%%
%% %% Extract the root value
%% 10 = catena_tree:root(Tree),
%%
%% %% Get the shrink candidates (forces evaluation)
%% [Tree5, Tree0] = catena_tree:children(Tree).
%% ```
%%
%% @see catena_gen for the Generator type built on rose trees
-module(catena_tree).

%% Type exports
-export_type([tree/1]).

%% API exports - Section 1.1.1: Rose Tree Type Definition
-export([
    tree/2,
    singleton/1,
    unfold/2,
    root/1,
    children/1
]).

%% API exports - Section 1.1.2: Comonadic Operations
-export([
    extract/1,
    duplicate/1,
    extend/2
]).

%% API exports - Section 1.1.3: Functor Instance (needed for comonad laws)
-export([
    map/2
]).

%%====================================================================
%% Types
%%====================================================================

%% @type tree(A).
%% A rose tree (multi-way tree) containing values of type A.
%%
%% Each node has:
%% - `root`: The value at this node
%% - `children`: A thunk that produces a list of child trees (shrink candidates)
%%
%% The children thunk enables lazy evaluation - shrink candidates are only
%% computed when actually needed during shrinking.
-record(tree, {
    root :: term(),
    children :: fun(() -> [tree(term())])
}).

-opaque tree(A) :: #tree{root :: A, children :: fun(() -> [tree(A)])}.

%%====================================================================
%% API Functions - Section 1.1.1
%%====================================================================

%% @doc Create a rose tree from a value and a thunk producing children.
%%
%% The children thunk is not evaluated until `children/1` is called,
%% enabling lazy computation of shrink candidates.
%%
%% == Example ==
%% ```
%% %% A tree with value 42 and two shrink candidates
%% Tree = catena_tree:tree(42, fun() ->
%%     [catena_tree:singleton(21), catena_tree:singleton(0)]
%% end).
%% ```
-spec tree(A, fun(() -> [tree(A)])) -> tree(A).
tree(Value, ChildrenThunk) when is_function(ChildrenThunk, 0) ->
    #tree{root = Value, children = ChildrenThunk}.

%% @doc Create a singleton tree (leaf node) with no children.
%%
%% A singleton tree represents a value that cannot be shrunk further.
%% This is used as the base case for shrinking and for values that
%% are already minimal.
%%
%% == Example ==
%% ```
%% %% A leaf node with value 0 (cannot shrink)
%% Leaf = catena_tree:singleton(0),
%% 0 = catena_tree:root(Leaf),
%% [] = catena_tree:children(Leaf).
%% ```
-spec singleton(A) -> tree(A).
singleton(Value) ->
    #tree{root = Value, children = fun() -> [] end}.

%% @doc Generate a tree by unfolding from a seed value.
%%
%% This is a powerful combinator for generating trees from a seed and
%% an expansion function. The expansion function takes a seed and returns
%% a tuple of the value at this node and a list of seeds for child nodes.
%%
%% The unfolding is lazy - child seeds are only expanded when the children
%% are actually requested.
%%
%% == Parameters ==
%% - `Seed`: The initial seed value
%% - `ExpandFn`: Function `Seed -> {Value, [Seed]}` that produces the node
%%   value and seeds for child nodes
%%
%% == Example ==
%% ```
%% %% Generate a tree of integers shrinking toward 0
%% %% Each node's children are the halves toward zero
%% ShrinkInt = fun(N) ->
%%     Shrinks = if
%%         N =:= 0 -> [];
%%         N > 0 -> [N div 2, N - 1];
%%         N < 0 -> [N div 2, N + 1]
%%     end,
%%     {N, Shrinks}
%% end,
%% Tree = catena_tree:unfold(100, ShrinkInt).
%% %% Tree has root 100, children are trees rooted at 50 and 99
%% ```
-spec unfold(Seed, fun((Seed) -> {A, [Seed]})) -> tree(A).
unfold(Seed, ExpandFn) when is_function(ExpandFn, 1) ->
    {Value, ChildSeeds} = ExpandFn(Seed),
    #tree{
        root = Value,
        children = fun() ->
            [unfold(ChildSeed, ExpandFn) || ChildSeed <- ChildSeeds]
        end
    }.

%% @doc Extract the root value from a tree.
%%
%% This is the "extract" operation of the comonad interface (implemented
%% in a later section). It returns the value at the root of the tree
%% without evaluating any children.
%%
%% == Example ==
%% ```
%% Tree = catena_tree:singleton(42),
%% 42 = catena_tree:root(Tree).
%% ```
-spec root(tree(A)) -> A.
root(#tree{root = Value}) ->
    Value.

%% @doc Get the children of a tree (forces evaluation of the thunk).
%%
%% This evaluates the children thunk and returns the list of child trees.
%% Each child represents a possible shrink of the parent value.
%%
%% Note: This forces evaluation of the lazy children. The returned list
%% may be empty (for leaf nodes) or contain multiple shrink candidates.
%%
%% == Example ==
%% ```
%% Tree = catena_tree:tree(10, fun() ->
%%     [catena_tree:singleton(5), catena_tree:singleton(0)]
%% end),
%% [Child1, Child2] = catena_tree:children(Tree),
%% 5 = catena_tree:root(Child1),
%% 0 = catena_tree:root(Child2).
%% ```
-spec children(tree(A)) -> [tree(A)].
children(#tree{children = ChildrenThunk}) ->
    ChildrenThunk().

%%====================================================================
%% API Functions - Section 1.1.2: Comonadic Operations
%%====================================================================

%% @doc Extract the root value from a tree (comonad's extract/counit).
%%
%% This is the fundamental comonadic operation that extracts the "focus"
%% value from a comonadic context. For rose trees, this returns the root
%% value without evaluating any children.
%%
%% This is an alias for `root/1` provided for categorical consistency.
%%
%% == Comonad Law ==
%% `extract` is the left and right identity for `extend`:
%% - `extend extract t == t`
%% - `extract (extend f t) == f t`
%%
%% == Example ==
%% ```
%% Tree = catena_tree:singleton(42),
%% 42 = catena_tree:extract(Tree).
%% ```
-spec extract(tree(A)) -> A.
extract(#tree{root = Value}) ->
    Value.

%% @doc Create a tree of subtrees (comonad's duplicate/cojoin).
%%
%% Transforms a tree into a tree of trees, where each node contains
%% the subtree rooted at that position. This is the dual of monadic join.
%%
%% For a tree with structure:
%% ```
%%     A
%%    / \
%%   B   C
%% ```
%%
%% `duplicate` produces:
%% ```
%%        Tree(A,B,C)
%%       /          \
%%   Tree(B)      Tree(C)
%% ```
%%
%% Where each node now contains the entire subtree from that position.
%%
%% == Comonad Laws ==
%% - `extract . duplicate == id`
%% - `map extract . duplicate == id`
%% - `duplicate . duplicate == map duplicate . duplicate`
%%
%% == Example ==
%% ```
%% Tree = catena_tree:tree(1, fun() ->
%%     [catena_tree:singleton(2), catena_tree:singleton(3)]
%% end),
%% Dup = catena_tree:duplicate(Tree),
%% %% Root of Dup is the original tree
%% Tree = catena_tree:extract(Dup),
%% %% Children of Dup are duplicated subtrees
%% [Child2, Child3] = catena_tree:children(Dup),
%% 2 = catena_tree:extract(catena_tree:extract(Child2)).
%% ```
-spec duplicate(tree(A)) -> tree(tree(A)).
duplicate(Tree) ->
    #tree{
        root = Tree,
        children = fun() ->
            [duplicate(Child) || Child <- children(Tree)]
        end
    }.

%% @doc Apply a function to all subtrees (comonad's extend/cobind).
%%
%% Extends a function that consumes a tree to work on all subtrees.
%% The function receives the entire subtree at each position and produces
%% a new value for that position.
%%
%% This is the dual of monadic bind. Where bind (>>=) lets you use a value
%% to produce a new context, extend lets you use a context to produce a
%% new value.
%%
%% `extend f` is equivalent to `map f . duplicate`.
%%
%% == Use Cases ==
%% - Context-aware transformations where each node needs info about its subtree
%% - Computing aggregates at each position (e.g., subtree size, depth)
%% - Shrinking strategies that consider the shrink tree structure
%%
%% == Comonad Laws ==
%% - `extend extract == id`
%% - `extract . extend f == f`
%% - `extend f . extend g == extend (f . extend g)`
%%
%% == Example ==
%% ```
%% %% Count nodes in subtree rooted at each position
%% CountNodes = fun CountNodes(T) ->
%%     1 + lists:sum([CountNodes(C) || C <- catena_tree:children(T)])
%% end,
%% Tree = catena_tree:tree(a, fun() ->
%%     [catena_tree:singleton(b), catena_tree:singleton(c)]
%% end),
%% Counted = catena_tree:extend(CountNodes, Tree),
%% 3 = catena_tree:extract(Counted),  % Total tree has 3 nodes
%% [C1, C2] = catena_tree:children(Counted),
%% 1 = catena_tree:extract(C1).  % Each leaf subtree has 1 node
%% ```
-spec extend(fun((tree(A)) -> B), tree(A)) -> tree(B).
extend(F, Tree) when is_function(F, 1) ->
    #tree{
        root = F(Tree),
        children = fun() ->
            [extend(F, Child) || Child <- children(Tree)]
        end
    }.

%%====================================================================
%% API Functions - Section 1.1.3: Functor Instance
%%====================================================================

%% @doc Apply a function to every value in the tree (functor's fmap).
%%
%% Transforms all values in the tree while preserving the tree structure.
%% The function is applied lazily - children are only transformed when
%% they are actually accessed.
%%
%% == Functor Laws ==
%% - `map id == id` (identity)
%% - `map (f . g) == map f . map g` (composition)
%%
%% == Example ==
%% ```
%% Tree = catena_tree:tree(1, fun() ->
%%     [catena_tree:singleton(2), catena_tree:singleton(3)]
%% end),
%% Doubled = catena_tree:map(fun(X) -> X * 2 end, Tree),
%% 2 = catena_tree:extract(Doubled),
%% [C1, C2] = catena_tree:children(Doubled),
%% 4 = catena_tree:extract(C1),
%% 6 = catena_tree:extract(C2).
%% ```
-spec map(fun((A) -> B), tree(A)) -> tree(B).
map(F, Tree) when is_function(F, 1) ->
    #tree{
        root = F(root(Tree)),
        children = fun() ->
            [map(F, Child) || Child <- children(Tree)]
        end
    }.
