-module(catena_hefty_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_hefty_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"hefty tree constructors", fun test_tree_constructors/0},
        {"hefty handler constructors", fun test_handler_constructors/0},
        {"hefty tree construction", fun test_tree_construction/0},
        {"hefty tree operations", fun test_tree_operations/0},
        {"hefty handler interpretation", fun test_interpretation/0},
        {"hefty tree optimization", fun test_optimization/0},
        {"hefty debugging", fun test_debugging/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Tree Constructor Tests
%%%---------------------------------------------------------------------

test_tree_constructors() ->
    %% Empty hefty tree
    Empty = catena_hefty:hefty_tree(),
    ?assert(catena_hefty:is_hefty_tree(Empty)),

    %% Pure value tree
    Pure = catena_hefty:pure(42),
    ?assert(catena_hefty:is_hefty_tree(Pure)),

    %% Operation node tree
    Op = catena_hefty:op('State', 'get', []),
    ?assert(catena_hefty:is_hefty_tree(Op)),

    %% Bind tree
    Bind = catena_hefty:bind(Pure, fun(X) -> X + 1 end),
    ?assert(catena_hefty:is_hefty_tree(Bind)),

    %% Not a hefty tree
    ?assertNot(catena_hefty:is_hefty_tree(atom)),
    ?assertNot(catena_hefty:is_hefty_tree(123)),
    EmptyMap = #{},
    ?assertNot(catena_hefty:is_hefty_tree(EmptyMap)).

%%%---------------------------------------------------------------------
%%% Handler Constructor Tests
%%%---------------------------------------------------------------------

test_handler_constructors() ->
    %% Empty hefty handler
    Empty = catena_hefty:hefty_handler(),
    ?assert(catena_hefty:is_hefty_handler(Empty)),

    %% Named hefty handler
    Named = catena_hefty:hefty_handler(my_handler),
    ?assert(catena_hefty:is_hefty_handler(Named)),

    %% Hefty handler with operations
    Ops = #{
        {'State', 'get'} => fun([]) -> 42 end,
        {'State', 'put'} => fun([_]) -> ok end
    },
    WithOps = catena_hefty:hefty_handler(state_handler, Ops),
    ?assert(catena_hefty:is_hefty_handler(WithOps)),

    %% Not a hefty handler
    ?assertNot(catena_hefty:is_hefty_handler(atom)),
    ?assertNot(catena_hefty:is_hefty_handler(#{})).

%%%---------------------------------------------------------------------
%%% Tree Construction Tests
%%%---------------------------------------------------------------------

test_tree_construction() ->
    %% Return tree
    Return = catena_hefty:return(42),
    ?assert(catena_hefty:is_hefty_tree(Return)),

    %% Effect tree
    Effect = catena_hefty:effect('State', 'get', []),
    ?assert(catena_hefty:is_hefty_tree(Effect)),

    %% Then tree
    Tree1 = catena_hefty:pure(1),
    Tree2 = catena_hefty:pure(2),
    Then = catena_hefty:then(Tree1, Tree2),
    ?assert(catena_hefty:is_hefty_tree(Then)),

    %% Sequence of trees
    Trees = [catena_hefty:pure(1), catena_hefty:pure(2), catena_hefty:pure(3)],
    Seq = catena_hefty:sequence(Trees),
    ?assert(catena_hefty:is_hefty_tree(Seq)),

    %% Empty sequence
    EmptySeq = catena_hefty:sequence([]),
    ?assert(catena_hefty:is_hefty_tree(EmptySeq)).

%%%---------------------------------------------------------------------
%%% Tree Operation Tests
%%%---------------------------------------------------------------------

test_tree_operations() ->
    %% Map over pure tree
    Pure = catena_hefty:pure(41),
    Mapped = catena_hefty:map_hefty(fun(X) -> X + 1 end, Pure),
    ?assert(catena_hefty:is_hefty_tree(Mapped)),

    %% Apply hefty tree
    FunTree = catena_hefty:pure(fun(X) -> X * 2 end),
    ValTree = catena_hefty:pure(21),
    Applied = catena_hefty:apply_hefty(FunTree, ValTree),
    ?assert(catena_hefty:is_hefty_tree(Applied)),

    %% Join hefty tree
    Inner = catena_hefty:pure(42),
    Nested = catena_hefty:pure(Inner),
    Joined = catena_hefty:join_hefty(Nested),
    ?assert(catena_hefty:is_hefty_tree(Joined)),

    %% Flatten hefty tree
    Flattened = catena_hefty:flatten_hefty(Nested),
    ?assert(catena_hefty:is_hefty_tree(Flattened)).

%%%---------------------------------------------------------------------
%%% Interpretation Tests
%%%---------------------------------------------------------------------

test_interpretation() ->
    %% Interpret pure value
    Pure = catena_hefty:pure(42),
    Handler = catena_hefty:hefty_handler(),
    ?assertEqual(42, catena_hefty:interpret(Pure, Handler)),

    %% Interpret operation with handler
    Ops = #{
        {'State', 'get'} => fun([]) -> 42 end
    },
    OpTree = catena_hefty:effect('State', 'get', []),
    HandlerWithOp = catena_hefty:hefty_handler(state, Ops),
    ?assertEqual(42, catena_hefty:interpret(OpTree, HandlerWithOp)),

    %% Handle specific operation
    HandleResult = catena_hefty:handle('State', 'get', fun([]) -> 99 end, OpTree),
    ?assert(catena_hefty:is_hefty_tree(HandleResult)).

%%%---------------------------------------------------------------------
%%% Optimization Tests
%%%---------------------------------------------------------------------

test_optimization() ->
    %% Fuse operations
    Op1 = catena_hefty:effect('State', 'get', []),
    Op2 = catena_hefty:effect('State', 'put', [42]),
    Seq = catena_hefty:then(Op1, Op2),
    Fused = catena_hefty:fuse_operations(Seq),
    ?assert(catena_hefty:is_hefty_tree(Fused)),

    %% Inline pures
    Bind = catena_hefty:bind(catena_hefty:pure(41), fun(X) -> X + 1 end),
    Inlined = catena_hefty:inline_pures(Bind),
    ?assert(catena_hefty:is_hefty_tree(Inlined)),

    %% Full optimization
    Tree = catena_hefty:sequence([
        catena_hefty:pure(1),
        catena_hefty:pure(2)
    ]),
    Optimized = catena_hefty:optimize(Tree),
    ?assert(catena_hefty:is_hefty_tree(Optimized)).

%%%---------------------------------------------------------------------
%%% Debugging Tests
%%%---------------------------------------------------------------------

test_debugging() ->
    %% Tree size
    Pure = catena_hefty:pure(42),
    ?assertEqual(1, catena_hefty:tree_size(Pure)),

    Seq = catena_hefty:sequence([
        catena_hefty:pure(1),
        catena_hefty:pure(2)
    ]),
    ?assertEqual(3, catena_hefty:tree_size(Seq)),

    %% Tree depth
    ?assertEqual(0, catena_hefty:tree_depth(Pure)),
    ?assertEqual(1, catena_hefty:tree_depth(Seq)),

    %% Tree to list
    OpTree = catena_hefty:effect('State', 'get', []),
    OpList = catena_hefty:tree_to_list(OpTree),
    ?assertEqual(1, length(OpList)),

    PureList = catena_hefty:tree_to_list(Pure),
    ?assertEqual(0, length(PureList)),

    %% Format tree
    Formatted = catena_hefty:format_tree(Pure),
    ?assert(is_binary(Formatted)),
    ?assertNotEqual(<<>>, Formatted),
    case binary:match(Formatted, <<"pure">>) of
        {_, _} -> ok;
        nomatch -> ?assert(false)
    end.
