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
