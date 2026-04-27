-module(catena_hefty_tests).
-include_lib("eunit/include/eunit.hrl").

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

setup() ->
    ok.

cleanup(_) ->
    ok.

test_tree_constructors() ->
    Empty = catena_hefty:hefty_tree(),
    ?assert(catena_hefty:is_hefty_tree(Empty)),

    Pure = catena_hefty:pure(42),
    ?assert(catena_hefty:is_hefty_tree(Pure)),

    Op = catena_hefty:op('State', 'get', []),
    ?assert(catena_hefty:is_hefty_tree(Op)),

    Bind = catena_hefty:bind(Pure, fun(X) -> X + 1 end),
    ?assert(catena_hefty:is_hefty_tree(Bind)),

    ?assertNot(catena_hefty:is_hefty_tree(atom)),
    ?assertNot(catena_hefty:is_hefty_tree(123)),
    ?assertNot(catena_hefty:is_hefty_tree(#{})).

test_handler_constructors() ->
    Empty = catena_hefty:hefty_handler(),
    ?assert(catena_hefty:is_hefty_handler(Empty)),

    Named = catena_hefty:hefty_handler(my_handler),
    ?assert(catena_hefty:is_hefty_handler(Named)),

    Ops = #{
        {'State', 'get'} => fun([]) -> 42 end,
        {'State', 'put'} => fun([_]) -> ok end
    },
    WithOps = catena_hefty:hefty_handler(state_handler, Ops),
    ?assert(catena_hefty:is_hefty_handler(WithOps)),

    ?assertNot(catena_hefty:is_hefty_handler(atom)),
    ?assertNot(catena_hefty:is_hefty_handler(#{})).

test_tree_construction() ->
    Return = catena_hefty:return(42),
    ?assert(catena_hefty:is_hefty_tree(Return)),

    Effect = catena_hefty:effect('State', 'get', []),
    ?assert(catena_hefty:is_hefty_tree(Effect)),

    ContinuationEffect = catena_hefty:effect('State', 'get', fun(Value) -> catena_hefty:pure(Value + 1) end),
    ?assert(catena_hefty:is_hefty_tree(ContinuationEffect)),

    Tree1 = catena_hefty:pure(1),
    Tree2 = catena_hefty:pure(2),
    Then = catena_hefty:then(Tree1, Tree2),
    ?assert(catena_hefty:is_hefty_tree(Then)),

    Trees = [catena_hefty:pure(1), catena_hefty:pure(2), catena_hefty:pure(3)],
    Seq = catena_hefty:sequence(Trees),
    ?assert(catena_hefty:is_hefty_tree(Seq)).

test_tree_operations() ->
    Pure = catena_hefty:pure(41),
    Mapped = catena_hefty:map_hefty(fun(X) -> X + 1 end, Pure),
    ?assert(catena_hefty:is_hefty_tree(Mapped)),
    ?assertEqual(42, catena_hefty:interpret(Mapped, catena_hefty:hefty_handler())),

    FunTree = catena_hefty:pure(fun(X) -> X * 2 end),
    ValTree = catena_hefty:pure(21),
    Applied = catena_hefty:apply_hefty(FunTree, ValTree),
    ?assert(catena_hefty:is_hefty_tree(Applied)),

    Inner = catena_hefty:pure(42),
    Nested = catena_hefty:pure(Inner),
    Joined = catena_hefty:join_hefty(Nested),
    ?assert(catena_hefty:is_hefty_tree(Joined)),

    Flattened = catena_hefty:flatten_hefty(Nested),
    ?assert(catena_hefty:is_hefty_tree(Flattened)).

test_interpretation() ->
    Pure = catena_hefty:pure(42),
    Handler = catena_hefty:hefty_handler(),
    ?assertEqual(42, catena_hefty:interpret(Pure, Handler)),

    Ops = #{
        {'State', 'get'} => fun([]) -> 42 end,
        {'State', 'tick'} => fun([], Continuation) ->
            case Continuation of
                undefined -> 1;
                Fun -> Fun(1)
            end
        end
    },
    OpTree = catena_hefty:effect('State', 'get', []),
    HandlerWithOp = catena_hefty:hefty_handler(state, Ops),
    ?assertEqual(42, catena_hefty:interpret(OpTree, HandlerWithOp)),
    ?assertEqual(42, catena_hefty_interpreter:interpret(OpTree, HandlerWithOp)),

    ContinuationTree = catena_hefty:effect('State', 'tick', fun(Value) -> catena_hefty:pure(Value + 1) end),
    ?assertEqual(2, catena_hefty:interpret(ContinuationTree, HandlerWithOp)),

    HandleResult = catena_hefty:handle('State', 'get', fun([]) -> 99 end, OpTree),
    ?assert(catena_hefty:is_hefty_tree(HandleResult)).

test_optimization() ->
    Op1 = catena_hefty:effect('State', 'get', []),
    Op2 = catena_hefty:effect('State', 'get', []),
    Seq = catena_hefty:then(Op1, Op2),
    Fused = catena_hefty:fuse_operations(Seq),
    ?assert(catena_hefty:is_hefty_tree(Fused)),

    Bind = catena_hefty:bind(catena_hefty:pure(41), fun(X) -> X + 1 end),
    Inlined = catena_hefty:inline_pures(Bind),
    ?assert(catena_hefty:is_hefty_tree(Inlined)),

    ContinuationTree = catena_hefty:bind(
        catena_hefty:effect('State', 'tick', fun(Value) -> catena_hefty:pure(Value) end),
        fun(X) -> X + 1 end
    ),
    InlinedHandlers = catena_hefty:inline_handlers(ContinuationTree),
    ?assert(catena_hefty:is_hefty_tree(InlinedHandlers)),

    Tree = catena_hefty:sequence([catena_hefty:pure(1), catena_hefty:pure(2)]),
    Optimized = catena_hefty:optimize(Tree),
    ?assert(catena_hefty:is_hefty_tree(Optimized)).

test_debugging() ->
    Pure = catena_hefty:pure(42),
    ?assertEqual(1, catena_hefty:tree_size(Pure)),

    Seq = catena_hefty:sequence([catena_hefty:pure(1), catena_hefty:pure(2)]),
    ?assertEqual(3, catena_hefty:tree_size(Seq)),

    ?assertEqual(0, catena_hefty:tree_depth(Pure)),
    ?assertEqual(1, catena_hefty:tree_depth(Seq)),

    OpTree = catena_hefty:effect('State', 'get', []),
    OpList = catena_hefty:tree_to_list(OpTree),
    ?assertEqual(1, length(OpList)),

    PureList = catena_hefty:tree_to_list(Pure),
    ?assertEqual(0, length(PureList)),

    Formatted = catena_hefty:format_tree(Pure),
    ?assert(is_binary(Formatted)),
    ?assertMatch({_, _}, binary:match(Formatted, <<"pure">>)).
