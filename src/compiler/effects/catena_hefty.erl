%%%-------------------------------------------------------------------
%%% @doc Catena Hefty Algebras (Phase 13.3)
%%%
%%% This module implements hefty algebras (trees of effect handlers)
%%% for higher-order effects. Hefty algebras provide a way to represent
%%% effectful computations as trees that can be interpreted by handlers,
%%% enabling higher-order effects where operations can take effectful
%%% functions as arguments.
%%%
%%% == Hefty Trees ==
%%%
%%% A hefty tree represents an effectful computation as a structure
%%% that can be traversed and interpreted. The tree has:
%%% - Pure values (leaves)
%%% - Effect operations (nodes with operations and continuations)
%%% - Nested computations (subtrees)
%%%
%%% == Hefty Handlers ==
%%%
%%% A hefty handler interprets a hefty tree by:
%%% 1. Traversing the tree structure
%%% 2. Handling operations at each node
%%% 3. Invoking continuations with results
%%% 4. Managing the handler stack
%%%
%%% == Interpretation ==
%%%
%%% Hefty tree interpretation combines handler composition with tree
%%% traversal, enabling modular effect handling where handlers can be
%%% composed and applied to different computations.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_hefty).

%% Hefty tree type constructors
-export([
    hefty_tree/0,
    pure/1,
    op/3,
    bind/2,
    is_hefty_tree/1
]).

%% Hefty handler type constructors
-export([
    hefty_handler/0,
    hefty_handler/1,
    hefty_handler/2,
    is_hefty_handler/1
]).

%% Hefty tree construction
-export([
    return/1,
    effect/3,
    then/2,
    lift_pure/1,
    sequence/1
]).

%% Hefty tree operations
-export([
    map_hefty/2,
    apply_hefty/2,
    join_hefty/1,
    flatten_hefty/1
]).

%% Hefty handler interpretation
-export([
    interpret/2,
    interpret_with/2,
    handle/4,
    handle_op/4
]).

%% Hefty tree optimization
-export([
    optimize/1,
    fuse_operations/1,
    inline_pures/1,
    dedupe_ops/1
]).

%% Hefty debugging
-export([
    tree_size/1,
    tree_depth/1,
    tree_to_list/1,
    format_tree/1
]).

-export_type([
    hefty_tree/0,
    hefty_handler/0,
    continuation/0,
    op_node/0
]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

%% @doc Hefty tree - represents effectful computation.
-record(hefty_tree, {
    kind :: pure | op | bind | seq,
    value :: term() | undefined,
    op :: op_node() | undefined,
    continuation :: continuation() | undefined,
    subtrees :: [hefty_tree()] | undefined
}).

-type hefty_tree() :: #hefty_tree{}.

%% @doc Operation node in a hefty tree.
-record(op_node, {
    effect :: atom(),
    operation :: atom(),
    args :: [term()]
}).

-type op_node() :: #op_node{}.

%% @doc Continuation representing rest of computation.
-record(continuation, {
    fn :: function(),
    effects :: map()
}).

-type continuation() :: #continuation{}.

%% @doc Hefty handler - interprets a hefty tree.
-record(hefty_handler, {
    name :: atom() | undefined,
    operations :: #{atom() => function()},
    fallback :: function() | undefined
}).

-type hefty_handler() :: #hefty_handler{}.

%%%---------------------------------------------------------------------
%%% Hefty Tree Type Constructors
%%%---------------------------------------------------------------------

%% @doc Create an empty hefty tree.
-spec hefty_tree() -> hefty_tree().
hefty_tree() ->
    #hefty_tree{
        kind = pure,
        value = undefined,
        op = undefined,
        continuation = undefined,
        subtrees = []
    }.

%% @doc Create a pure value tree (leaf node).
-spec pure(term()) -> hefty_tree().
pure(Value) ->
    #hefty_tree{
        kind = pure,
        value = Value,
        op = undefined,
        continuation = undefined,
        subtrees = []
    }.

%% @doc Create an operation node tree.
-spec op(atom(), atom(), [term()]) -> hefty_tree().
op(Effect, Operation, Args) when is_atom(Effect), is_atom(Operation), is_list(Args) ->
    #hefty_tree{
        kind = op,
        value = undefined,
        op = #op_node{effect = Effect, operation = Operation, args = Args},
        continuation = undefined,
        subtrees = []
    }.

%% @doc Create a bind node (monadic bind).
-spec bind(hefty_tree(), function()) -> hefty_tree().
bind(Tree, Fun) when is_function(Fun, 1) ->
    #hefty_tree{
        kind = bind,
        value = undefined,
        op = undefined,
        continuation = #continuation{fn = Fun, effects = empty_effect_row()},
        subtrees = [Tree]
    }.

%% @doc Check if a term is a hefty tree.
-spec is_hefty_tree(term()) -> boolean().
is_hefty_tree(#hefty_tree{}) -> true;
is_hefty_tree(_) -> false.

%%%---------------------------------------------------------------------
%%% Hefty Handler Type Constructors
%%%---------------------------------------------------------------------

%% @doc Create an empty hefty handler.
-spec hefty_handler() -> hefty_handler().
hefty_handler() ->
    #hefty_handler{
        name = undefined,
        operations = #{},
        fallback = undefined
    }.

%% @doc Create a hefty handler with a name.
-spec hefty_handler(atom()) -> hefty_handler().
hefty_handler(Name) when is_atom(Name) ->
    #hefty_handler{
        name = Name,
        operations = #{},
        fallback = undefined
    }.

%% @doc Create a hefty handler with operations.
-spec hefty_handler(atom(), #{atom() => function()}) -> hefty_handler().
hefty_handler(Name, Ops) when is_atom(Name), is_map(Ops) ->
    #hefty_handler{
        name = Name,
        operations = Ops,
        fallback = undefined
    }.

%% @doc Check if a term is a hefty handler.
-spec is_hefty_handler(term()) -> boolean().
is_hefty_handler(#hefty_handler{}) -> true;
is_hefty_handler(_) -> false.

%%%---------------------------------------------------------------------
%%% Hefty Tree Construction
%%%---------------------------------------------------------------------

%% @doc Create a pure return tree.
-spec return(term()) -> hefty_tree().
return(Value) ->
    pure(Value).

%% @doc Create an effect operation tree.
-spec effect(atom(), atom(), [term()]) -> hefty_tree().
effect(Effect, Operation, Args) ->
    op(Effect, Operation, Args).

%% @doc Create a then (sequence) tree.
-spec then(hefty_tree(), hefty_tree()) -> hefty_tree().
then(Tree1, Tree2) ->
    #hefty_tree{
        kind = seq,
        value = undefined,
        op = undefined,
        continuation = undefined,
        subtrees = [Tree1, Tree2]
    }.

%% @doc Lift a pure value into a hefty tree.
-spec lift_pure(term()) -> hefty_tree().
lift_pure(Value) ->
    pure(Value).

%% @doc Sequence a list of hefty trees.
-spec sequence([hefty_tree()]) -> hefty_tree().
sequence([]) ->
    pure([]);
sequence([Tree]) ->
    Tree;
sequence(Trees) when is_list(Trees) ->
    #hefty_tree{
        kind = seq,
        value = undefined,
        op = undefined,
        continuation = undefined,
        subtrees = Trees
    }.

%%%---------------------------------------------------------------------
%%% Hefty Tree Operations
%%%---------------------------------------------------------------------

%% @doc Map a function over a hefty tree.
-spec map_hefty(function(), hefty_tree()) -> hefty_tree().
map_hefty(Fun, #hefty_tree{kind = pure, value = Value}) ->
    pure(Fun(Value));
map_hefty(Fun, #hefty_tree{kind = bind, subtrees = [Tree], continuation = Cont}) ->
    ContFn = Cont#continuation.fn,
    bind(map_hefty(Fun, Tree), fun(V) -> Fun(ContFn(V)) end);
map_hefty(Fun, #hefty_tree{kind = op} = Tree) ->
    bind(Tree, Fun);
map_hefty(Fun, #hefty_tree{kind = seq, subtrees = Trees}) ->
    lists:foldl(fun(T, Acc) -> then(Acc, map_hefty(Fun, T)) end, pure([]), Trees).

%% @doc Apply a hefty tree of functions to a hefty tree of values.
-spec apply_hefty(hefty_tree(), hefty_tree()) -> hefty_tree().
apply_hefty(FunTree, ValTree) ->
    bind(FunTree, fun(Fun) when is_function(Fun, 1) ->
        map_hefty(Fun, ValTree)
    end).

%% @doc Join a nested hefty tree (monadic join).
-spec join_hefty(hefty_tree()) -> hefty_tree().
join_hefty(#hefty_tree{kind = pure, value = InnerTree}) when is_record(InnerTree, hefty_tree) ->
    InnerTree;
join_hefty(#hefty_tree{kind = bind, subtrees = [Tree], continuation = Cont}) ->
    ContFn = Cont#continuation.fn,
    bind(join_hefty(Tree), fun(V) -> join_hefty(ContFn(V)) end);
join_hefty(Tree) ->
    Tree.

%% @doc Flatten a nested hefty tree structure.
-spec flatten_hefty(hefty_tree()) -> hefty_tree().
flatten_hefty(Tree) ->
    join_hefty(Tree).

%%%---------------------------------------------------------------------
%%% Hefty Handler Interpretation
%%%---------------------------------------------------------------------

%% @doc Interpret a hefty tree with a handler.
-spec interpret(hefty_tree(), hefty_handler()) -> term().
interpret(#hefty_tree{kind = pure, value = Value}, _Handler) ->
    Value;
interpret(#hefty_tree{kind = op, op = OpNode}, #hefty_handler{operations = Ops, fallback = Fallback}) ->
    OpKey = {OpNode#op_node.effect, OpNode#op_node.operation},
    case maps:find(OpKey, Ops) of
        {ok, HandlerFn} ->
            HandlerFn(OpNode#op_node.args);
        error ->
            case Fallback of
                undefined -> {error, {unhandled, OpNode}};
                Fn -> Fn(OpNode)
            end
    end;
interpret(#hefty_tree{kind = bind, subtrees = [Tree], continuation = Cont}, Handler) ->
    Value = interpret(Tree, Handler),
    ContFn = Cont#continuation.fn,
    case ContFn(Value) of
        #hefty_tree{} = NextTree -> interpret(NextTree, Handler);
        DirectResult -> DirectResult
    end;
interpret(#hefty_tree{kind = seq, subtrees = Trees}, Handler) ->
    lists:foldl(fun(T, _) -> interpret(T, Handler) end, undefined, Trees).

%% @doc Interpret a hefty tree with a specific handler.
-spec interpret_with(hefty_tree(), hefty_handler()) -> term().
interpret_with(Tree, Handler) ->
    interpret(Tree, Handler).

%% @doc Handle a specific operation in a hefty tree.
-spec handle(atom(), atom(), function(), hefty_tree()) -> hefty_tree().
handle(Effect, Operation, HandlerFn, Tree) ->
    OpMap = #{{Effect, Operation} => HandlerFn},
    Handler = #hefty_handler{
        name = handle_op,
        operations = OpMap,
        fallback = undefined
    },
    case interpret(Tree, Handler) of
        #hefty_tree{} = ResultTree -> ResultTree;
        Result -> pure(Result)
    end.

%% @doc Handle an operation with continuation support.
-spec handle_op(atom(), atom(), function(), hefty_tree()) -> hefty_tree().
handle_op(Effect, Operation, HandlerFn, Tree) ->
    OpMap = #{{Effect, Operation} => HandlerFn},
    Handler = #hefty_handler{
        name = undefined,
        operations = OpMap,
        fallback = undefined
    },
    case interpret(Tree, Handler) of
        #hefty_tree{} = ResultTree -> ResultTree;
        Result -> pure(Result)
    end.

%%%---------------------------------------------------------------------
%%% Hefty Tree Optimization
%%%---------------------------------------------------------------------

%% @doc Optimize a hefty tree.
-spec optimize(hefty_tree()) -> hefty_tree().
optimize(Tree) ->
    Tree1 = fuse_operations(Tree),
    Tree2 = inline_pures(Tree1),
    Tree3 = dedupe_ops(Tree2),
    Tree3.

%% @doc Fuse consecutive operations of the same effect.
-spec fuse_operations(hefty_tree()) -> hefty_tree().
fuse_operations(#hefty_tree{kind = seq, subtrees = Trees}) ->
    Fused = fuse_list(Trees),
    case Fused of
        [Single] -> Single;
        _ -> #hefty_tree{kind = seq, subtrees = Fused, value = undefined, op = undefined, continuation = undefined}
    end;
fuse_operations(Tree) ->
    Tree.

%% @private
fuse_list([Tree]) ->
    [Tree];
fuse_list([#hefty_tree{kind = op, op = Op1} = T1,
           #hefty_tree{kind = op, op = Op2} = T2 | Rest])
    when Op1#op_node.effect =:= Op2#op_node.effect ->
    %% Fuse same-effect operations
    FusedOp = Op1#op_node{
        args = Op1#op_node.args ++ Op2#op_node.args
    },
    fuse_list([T1#hefty_tree{op = FusedOp} | Rest]);
fuse_list([Tree | Rest]) ->
    [optimize(Tree) | fuse_list(Rest)].

%% @doc Inline pure value nodes.
-spec inline_pures(hefty_tree()) -> hefty_tree().
inline_pures(#hefty_tree{kind = bind, subtrees = [#hefty_tree{kind = pure, value = Value}], continuation = Cont}) ->
    ContFn = Cont#continuation.fn,
    case ContFn(Value) of
        #hefty_tree{} = Result -> Result;
        DirectValue -> pure(DirectValue)
    end;
inline_pures(#hefty_tree{kind = seq, subtrees = Trees}) ->
    #hefty_tree{kind = seq, subtrees = [inline_pures(T) || T <- Trees], value = undefined, op = undefined, continuation = undefined};
inline_pures(Tree) ->
    Tree.

%% @doc Deduplicate consecutive identical operations.
-spec dedupe_ops(hefty_tree()) -> hefty_tree().
dedupe_ops(#hefty_tree{kind = seq, subtrees = Trees}) ->
    Deduped = dedupe_list(Trees),
    case Deduped of
        [Single] -> Single;
        _ -> #hefty_tree{kind = seq, subtrees = Deduped, value = undefined, op = undefined, continuation = undefined}
    end;
dedupe_ops(Tree) ->
    Tree.

%% @private
dedupe_list([]) ->
    [];
dedupe_list([Tree]) ->
    [Tree];
dedupe_list([#hefty_tree{kind = op, op = Op1} = T1,
           #hefty_tree{kind = op, op = Op2} = T2 | Rest])
    when Op1 =:= Op2 ->
    dedupe_list([T1 | Rest]);
dedupe_list([Tree | Rest]) ->
    [Tree | dedupe_list(Rest)].

%%%---------------------------------------------------------------------
%%% Hefty Debugging
%%%---------------------------------------------------------------------

%% @doc Get the size (node count) of a hefty tree.
-spec tree_size(hefty_tree()) -> non_neg_integer().
tree_size(#hefty_tree{kind = pure}) ->
    1;
tree_size(#hefty_tree{kind = op}) ->
    1;
tree_size(#hefty_tree{kind = bind, subtrees = [Tree]}) ->
    1 + tree_size(Tree);
tree_size(#hefty_tree{kind = seq, subtrees = Trees}) ->
    1 + lists:foldl(fun(T, Acc) -> Acc + tree_size(T) end, 0, Trees).

%% @doc Get the depth of a hefty tree.
-spec tree_depth(hefty_tree()) -> non_neg_integer().
tree_depth(#hefty_tree{kind = pure}) ->
    0;
tree_depth(#hefty_tree{kind = op}) ->
    0;
tree_depth(#hefty_tree{kind = bind, subtrees = [Tree]}) ->
    1 + tree_depth(Tree);
tree_depth(#hefty_tree{kind = seq, subtrees = Trees}) ->
    1 + lists:foldl(fun(T, Acc) -> max(Acc, tree_depth(T)) end, 0, Trees).

%% @doc Convert a hefty tree to a list of operations.
-spec tree_to_list(hefty_tree()) -> [op_node()].
tree_to_list(#hefty_tree{kind = pure}) ->
    [];
tree_to_list(#hefty_tree{kind = op, op = Op}) ->
    [Op];
tree_to_list(#hefty_tree{kind = bind, subtrees = [Tree]}) ->
    tree_to_list(Tree);
tree_to_list(#hefty_tree{kind = seq, subtrees = Trees}) ->
    lists:flatmap(fun tree_to_list/1, Trees).

%% @doc Format a hefty tree for display.
-spec format_tree(hefty_tree()) -> binary().
format_tree(#hefty_tree{kind = pure, value = Value}) ->
    BinValue = format_term(Value),
    <<"pure(", BinValue/binary, ")">>;
format_tree(#hefty_tree{kind = op, op = #op_node{effect = Eff, operation = Op}}) ->
    EffBin = list_to_binary(atom_to_list(Eff)),
    OpBin = list_to_binary(atom_to_list(Op)),
    <<OpBin/binary, ".", EffBin/binary, "()">>;
format_tree(#hefty_tree{kind = bind, subtrees = [Tree], continuation = Cont}) ->
    Inner = format_tree(Tree),
    <<"bind(", Inner/binary, ", ", "fun(...) -> end)">>;
format_tree(#hefty_tree{kind = seq, subtrees = Trees}) ->
    Formatted = [format_tree(T) || T <- Trees],
    Joined = lists:join(<<"; ">>, Formatted),
    <<"seq(", Joined/binary, ")">>.

%% @private
format_term(Term) when is_atom(Term) ->
    list_to_binary(atom_to_list(Term));
format_term(Term) when is_integer(Term) ->
    list_to_binary(integer_to_list(Term));
format_term(Term) when is_binary(Term) ->
    Term;
format_term(Term) when is_list(Term) ->
    <<"list()">>;
format_term(_) ->
    <<"?">>.

%% @private
empty_effect_row() ->
    #{
        kind => effect_row,
        elements => [],
        row_var => undefined
    }.
