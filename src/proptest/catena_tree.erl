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

%% API exports - Section 1.1.4: Applicative Instance
-export([
    pure/1,
    ap/2,
    liftA2/3
]).

%% API exports - Section 1.1.5: Monad Instance
-export([
    bind/2,
    flatten/1,
    return/1
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

%%====================================================================
%% API Functions - Section 1.1.4: Applicative Instance
%%====================================================================

%% @doc Lift a value into a tree with no shrinks (applicative's pure/return).
%%
%% Creates a singleton tree containing the value with no children.
%% This is the identity element for applicative operations - a value
%% that cannot be shrunk further.
%%
%% Note: This is equivalent to `singleton/1` but named `pure` for
%% consistency with the Applicative interface.
%%
%% == Applicative Laws ==
%% `pure` satisfies:
%% - Identity: `ap(pure(id), v) == v`
%% - Homomorphism: `ap(pure(f), pure(x)) == pure(f(x))`
%% - Interchange: `ap(u, pure(y)) == ap(pure(fun(f) -> f(y) end), u)`
%%
%% == Example ==
%% ```
%% Tree = catena_tree:pure(42),
%% 42 = catena_tree:extract(Tree),
%% [] = catena_tree:children(Tree).
%% ```
-spec pure(A) -> tree(A).
pure(Value) ->
    singleton(Value).

%% @doc Apply a tree of functions to a tree of values (applicative's <*>).
%%
%% Produces a tree where the root is `F(X)` (applying root function to root value),
%% and children represent all possible shrinks. The shrinking strategy is
%% "interleaved" meaning we explore:
%% 1. Shrinking the function tree (left shrinks)
%% 2. Shrinking the value tree (right shrinks)
%%
%% This interleaving ensures thorough exploration of the shrink space for
%% independent values. The order (left then right) prioritizes shrinking
%% the earlier/outer generator first.
%%
%% == Shrinking Strategy ==
%% For `ap(TreeF, TreeX)`:
%% - First, try shrinks from TreeF applied to the current X
%% - Then, try shrinks from TreeX with the current F
%%
%% This produces a tree where children = [shrink F with X] ++ [F with shrink X]
%%
%% == Applicative Laws ==
%% - Identity: `ap(pure(fun(X) -> X end), v) == v`
%% - Composition: `ap(ap(ap(pure(compose), u), v), w) == ap(u, ap(v, w))`
%% - Homomorphism: `ap(pure(f), pure(x)) == pure(f(x))`
%% - Interchange: `ap(u, pure(y)) == ap(pure(fun(f) -> f(y) end), u)`
%%
%% == Example ==
%% ```
%% %% Tree of functions
%% FnTree = catena_tree:tree(fun(X) -> X * 2 end, fun() ->
%%     [catena_tree:singleton(fun(X) -> X end)]
%% end),
%% %% Tree of values
%% ValTree = catena_tree:tree(10, fun() ->
%%     [catena_tree:singleton(5)]
%% end),
%% %% Apply
%% Result = catena_tree:ap(FnTree, ValTree),
%% 20 = catena_tree:extract(Result),  % (*2)(10)
%% %% Children: [10 (shrunk fn), 10 (shrunk val)]
%% [ShrunkFn, ShrunkVal] = catena_tree:children(Result),
%% 10 = catena_tree:extract(ShrunkFn),  % id(10)
%% 10 = catena_tree:extract(ShrunkVal). % (*2)(5)
%% ```
-spec ap(tree(fun((A) -> B)), tree(A)) -> tree(B).
ap(TreeF, TreeX) ->
    F = root(TreeF),
    X = root(TreeX),
    #tree{
        root = F(X),
        children = fun() ->
            %% Interleaved shrinking: left shrinks first, then right shrinks
            %% Left: shrink the function tree, keep X constant
            LeftShrinks = [ap(ShrunkF, TreeX) || ShrunkF <- children(TreeF)],
            %% Right: keep F constant, shrink the value tree
            RightShrinks = [ap(TreeF, ShrunkX) || ShrunkX <- children(TreeX)],
            %% Combine: left shrinks have priority
            LeftShrinks ++ RightShrinks
        end
    }.

%% @doc Lift a binary function over two trees (applicative's liftA2).
%%
%% Combines two trees using a binary function, with interleaved shrinking
%% of both inputs. This is a convenience function equivalent to:
%% `ap(map(F, TreeA), TreeB)`
%%
%% == Use Case ==
%% Useful for combining independent generated values:
%% ```
%% %% Generate pairs with independent shrinking
%% liftA2(fun(A, B) -> {A, B} end, GenA, GenB)
%% ```
%%
%% == Shrinking ==
%% Like `ap`, shrinking is interleaved:
%% 1. First shrink TreeA (with current B)
%% 2. Then shrink TreeB (with current A)
%%
%% == Example ==
%% ```
%% TreeA = catena_tree:tree(10, fun() -> [catena_tree:singleton(5)] end),
%% TreeB = catena_tree:tree(3, fun() -> [catena_tree:singleton(1)] end),
%% Result = catena_tree:liftA2(fun(A, B) -> A + B end, TreeA, TreeB),
%% 13 = catena_tree:extract(Result),
%% [Shrink1, Shrink2] = catena_tree:children(Result),
%% 8 = catena_tree:extract(Shrink1),   % 5 + 3 (shrunk A)
%% 11 = catena_tree:extract(Shrink2).  % 10 + 1 (shrunk B)
%% ```
-spec liftA2(fun((A, B) -> C), tree(A), tree(B)) -> tree(C).
liftA2(F, TreeA, TreeB) when is_function(F, 2) ->
    ap(map(fun(A) -> fun(B) -> F(A, B) end end, TreeA), TreeB).

%%====================================================================
%% API Functions - Section 1.1.5: Monad Instance
%%====================================================================

%% @doc Lift a value into a tree (monad's return).
%%
%% This is an alias for `pure/1` provided for consistency with the
%% Monad interface. Creates a singleton tree containing the value
%% with no children.
%%
%% == Monad Laws ==
%% `return` is the identity for `bind`:
%% - Left identity: `bind(return(a), f) == f(a)`
%% - Right identity: `bind(m, return) == m`
%%
%% == Example ==
%% ```
%% Tree = catena_tree:return(42),
%% 42 = catena_tree:extract(Tree),
%% [] = catena_tree:children(Tree).
%% ```
-spec return(A) -> tree(A).
return(Value) ->
    singleton(Value).

%% @doc Sequence a tree through a tree-producing function (monad's >>=/flatMap).
%%
%% Takes a tree and a function that produces a tree from values, applies
%% the function to the root value, and flattens the result while preserving
%% the shrinking structure.
%%
%% == IMPORTANT: Shrinking Trade-off ==
%% Monadic bind has suboptimal shrinking for dependent values. When the
%% second tree depends on the first value, shrinking the first value does
%% NOT automatically re-shrink the dependent values.
%%
%% Consider using Applicative (`ap`, `liftA2`) when values are independent,
%% as it provides better shrinking behavior. Use `bind` only when you need
%% true dependency between generated values.
%%
%% == Shrinking Strategy ==
%% For `bind(TreeA, F)`:
%% 1. Root: Extract A from TreeA, compute F(A), extract root of F(A)
%% 2. Children from F(A): The inner tree's shrinks are preserved
%% 3. Children from TreeA: Each shrink of A produces F(shrunkA) subtrees
%%
%% The shrinks from the inner tree (F(A)) come first, followed by shrinks
%% that re-run F on shrunk values of A.
%%
%% == Monad Laws ==
%% - Left identity: `bind(return(a), f) == f(a)`
%% - Right identity: `bind(m, return) == m`
%% - Associativity: `bind(bind(m, f), g) == bind(m, fun(x) -> bind(f(x), g) end)`
%%
%% == Example ==
%% ```
%% %% Generate a list of length N where N is generated first
%% TreeN = catena_tree:tree(3, fun() -> [catena_tree:singleton(1)] end),
%% Result = catena_tree:bind(TreeN, fun(N) ->
%%     %% Generate a list of N elements
%%     catena_tree:singleton(lists:duplicate(N, x))
%% end),
%% [x, x, x] = catena_tree:extract(Result).
%% ```
-spec bind(tree(A), fun((A) -> tree(B))) -> tree(B).
bind(TreeA, F) when is_function(F, 1) ->
    A = root(TreeA),
    TreeB = F(A),
    B = root(TreeB),
    #tree{
        root = B,
        children = fun() ->
            %% Inner shrinks: shrinks from F(A) come first
            InnerShrinks = children(TreeB),
            %% Outer shrinks: shrinks of A, re-running F on each
            %% Note: This is the source of suboptimal shrinking -
            %% when A shrinks, we get a whole new tree F(shrunkA),
            %% but we don't coordinate shrinking between the two.
            OuterShrinks = [bind(ShrunkA, F) || ShrunkA <- children(TreeA)],
            InnerShrinks ++ OuterShrinks
        end
    }.

%% @doc Flatten a nested tree into a single tree (monad's join).
%%
%% Collapses a tree of trees into a single tree. The outer tree structure
%% provides one level of shrinking, while the inner trees provide another.
%%
%% This is the dual of `duplicate` from the comonad instance.
%%
%% == Relationship to bind ==
%% `flatten` is related to `bind` by:
%% - `flatten(t) == bind(t, fun(x) -> x end)`
%% - `bind(t, f) == flatten(map(f, t))`
%%
%% == Shrinking Strategy ==
%% For `flatten(TreeOfTrees)`:
%% 1. Root: Extract inner tree from outer root, extract root from that
%% 2. Children from inner tree: Inner tree's shrinks come first
%% 3. Children from outer tree: Flatten each outer shrink
%%
%% == Example ==
%% ```
%% %% A tree of trees
%% Inner = catena_tree:tree(42, fun() -> [catena_tree:singleton(21)] end),
%% Outer = catena_tree:tree(Inner, fun() ->
%%     [catena_tree:singleton(catena_tree:singleton(0))]
%% end),
%% Flat = catena_tree:flatten(Outer),
%% 42 = catena_tree:extract(Flat),
%% [Shrink1, Shrink2] = catena_tree:children(Flat),
%% 21 = catena_tree:extract(Shrink1),  % From inner tree
%% 0 = catena_tree:extract(Shrink2).   % From outer tree (flattened)
%% ```
-spec flatten(tree(tree(A))) -> tree(A).
flatten(TreeOfTrees) ->
    InnerTree = root(TreeOfTrees),
    #tree{
        root = root(InnerTree),
        children = fun() ->
            %% Inner shrinks: from the inner tree
            InnerShrinks = children(InnerTree),
            %% Outer shrinks: flatten each outer child
            OuterShrinks = [flatten(OuterChild) || OuterChild <- children(TreeOfTrees)],
            InnerShrinks ++ OuterShrinks
        end
    }.
