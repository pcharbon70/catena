%%%-------------------------------------------------------------------
%%% @doc Catena Hefty Algebras (Phase 13.3)
%%%
%%% Hefty trees represent higher-order effectful programs explicitly and
%%% allow handlers to interpret operations, including operations whose
%%% nodes carry resumptions or callback continuations.
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
    inline_handlers/1,
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

-record(hefty_tree, {
    kind :: pure | op | bind | seq,
    value :: term() | undefined,
    op :: op_node() | undefined,
    continuation :: continuation() | undefined,
    subtrees :: [hefty_tree()] | undefined
}).

-type hefty_tree() :: #hefty_tree{}.

-record(op_node, {
    effect :: atom(),
    operation :: atom(),
    args :: [term()],
    continuation :: function() | undefined
}).

-type op_node() :: #op_node{}.

-record(continuation, {
    fn :: function(),
    effects :: catena_row_types:effect_row()
}).

-type continuation() :: #continuation{}.

-record(hefty_handler, {
    name :: atom() | undefined,
    operations :: #{term() => function()},
    fallback :: function() | undefined
}).

-type hefty_handler() :: #hefty_handler{}.

%%%---------------------------------------------------------------------
%%% Hefty Tree Type Constructors
%%%---------------------------------------------------------------------

-spec hefty_tree() -> hefty_tree().
hefty_tree() ->
    pure(undefined).

-spec pure(term()) -> hefty_tree().
pure(Value) ->
    #hefty_tree{
        kind = pure,
        value = Value,
        op = undefined,
        continuation = undefined,
        subtrees = []
    }.

-spec op(atom(), atom(), [term()]) -> hefty_tree().
op(Effect, Operation, Args) when is_atom(Effect), is_atom(Operation), is_list(Args) ->
    #hefty_tree{
        kind = op,
        value = undefined,
        op = #op_node{
            effect = Effect,
            operation = Operation,
            args = Args,
            continuation = undefined
        },
        continuation = undefined,
        subtrees = []
    }.

-spec bind(hefty_tree(), function()) -> hefty_tree().
bind(Tree, Fun) when is_function(Fun, 1) ->
    #hefty_tree{
        kind = bind,
        value = undefined,
        op = undefined,
        continuation = #continuation{
            fn = Fun,
            effects = catena_row_types:empty_row()
        },
        subtrees = [Tree]
    }.

-spec is_hefty_tree(term()) -> boolean().
is_hefty_tree(#hefty_tree{}) ->
    true;
is_hefty_tree(_) ->
    false.

%%%---------------------------------------------------------------------
%%% Hefty Handler Type Constructors
%%%---------------------------------------------------------------------

-spec hefty_handler() -> hefty_handler().
hefty_handler() ->
    #hefty_handler{
        name = undefined,
        operations = #{},
        fallback = undefined
    }.

-spec hefty_handler(atom()) -> hefty_handler().
hefty_handler(Name) when is_atom(Name) ->
    (hefty_handler())#hefty_handler{name = Name}.

-spec hefty_handler(atom(), #{term() => function()}) -> hefty_handler().
hefty_handler(Name, Ops) when is_atom(Name), is_map(Ops) ->
    #hefty_handler{
        name = Name,
        operations = Ops,
        fallback = undefined
    }.

-spec is_hefty_handler(term()) -> boolean().
is_hefty_handler(#hefty_handler{}) ->
    true;
is_hefty_handler(_) ->
    false.

%%%---------------------------------------------------------------------
%%% Hefty Tree Construction
%%%---------------------------------------------------------------------

-spec return(term()) -> hefty_tree().
return(Value) ->
    pure(Value).

-spec effect(atom(), atom(), term()) -> hefty_tree().
effect(Effect, Operation, Args) when is_list(Args) ->
    op(Effect, Operation, Args);
effect(Effect, Operation, Continuation) when is_function(Continuation, 1) ->
    #hefty_tree{
        kind = op,
        value = undefined,
        op = #op_node{
            effect = Effect,
            operation = Operation,
            args = [],
            continuation = Continuation
        },
        continuation = undefined,
        subtrees = []
    };
effect(Effect, Operation, Arg) ->
    op(Effect, Operation, [Arg]).

-spec then(hefty_tree(), hefty_tree()) -> hefty_tree().
then(Tree1, Tree2) ->
    sequence([Tree1, Tree2]).

-spec lift_pure(term()) -> hefty_tree().
lift_pure(Value) ->
    pure(Value).

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

-spec map_hefty(function(), hefty_tree()) -> hefty_tree().
map_hefty(Fun, #hefty_tree{kind = pure, value = Value}) ->
    pure(Fun(Value));
map_hefty(Fun, #hefty_tree{kind = op, op = OpNode}) ->
    bind(#hefty_tree{kind = op, value = undefined, op = OpNode, continuation = undefined, subtrees = []},
         fun(Value) -> pure(Fun(Value)) end);
map_hefty(Fun, #hefty_tree{kind = bind, subtrees = [Tree], continuation = Continuation}) ->
    ContFn = Continuation#continuation.fn,
    bind(map_hefty(Fun, Tree), fun(Value) -> lift_result(Fun(unwrap_hefty(ContFn(Value)))) end);
map_hefty(Fun, #hefty_tree{kind = seq, subtrees = Trees}) ->
    sequence([map_hefty(Fun, Tree) || Tree <- Trees]).

-spec apply_hefty(hefty_tree(), hefty_tree()) -> hefty_tree().
apply_hefty(FunTree, ValueTree) ->
    bind(FunTree, fun(Fun) when is_function(Fun, 1) ->
        map_hefty(Fun, ValueTree)
    end).

-spec join_hefty(hefty_tree()) -> hefty_tree().
join_hefty(#hefty_tree{kind = pure, value = InnerTree}) when is_record(InnerTree, hefty_tree) ->
    InnerTree;
join_hefty(#hefty_tree{kind = bind, subtrees = [Tree], continuation = Continuation}) ->
    ContFn = Continuation#continuation.fn,
    bind(join_hefty(Tree), fun(Value) -> join_hefty(lift_result(ContFn(Value))) end);
join_hefty(Tree) ->
    Tree.

-spec flatten_hefty(hefty_tree()) -> hefty_tree().
flatten_hefty(Tree) ->
    join_hefty(Tree).

%%%---------------------------------------------------------------------
%%% Hefty Handler Interpretation
%%%---------------------------------------------------------------------

-spec interpret(hefty_tree(), hefty_handler()) -> term().
interpret(#hefty_tree{kind = pure, value = Value}, _Handler) ->
    Value;
interpret(#hefty_tree{kind = op, op = OpNode}, Handler) ->
    interpret_operation(OpNode, Handler);
interpret(#hefty_tree{kind = bind, subtrees = [Tree], continuation = Continuation}, Handler) ->
    Value = interpret(Tree, Handler),
    ContFn = Continuation#continuation.fn,
    interpret_continuation(ContFn, Value, Handler);
interpret(#hefty_tree{kind = seq, subtrees = Trees}, Handler) ->
    interpret_sequence(Trees, Handler).

-spec interpret_with(hefty_tree(), hefty_handler()) -> term().
interpret_with(Tree, Handler) ->
    interpret(Tree, Handler).

-spec handle(atom(), atom(), function(), hefty_tree()) -> hefty_tree().
handle(Effect, Operation, HandlerFn, Tree) ->
    handle_op(Effect, Operation, HandlerFn, Tree).

-spec handle_op(atom(), atom(), function(), hefty_tree()) -> hefty_tree().
handle_op(Effect, Operation, HandlerFn, Tree) ->
    Handler = hefty_handler(Operation, #{{Effect, Operation} => HandlerFn}),
    lift_result(interpret(Tree, Handler)).

%%%---------------------------------------------------------------------
%%% Hefty Tree Optimization
%%%---------------------------------------------------------------------

-spec optimize(hefty_tree()) -> hefty_tree().
optimize(Tree) ->
    dedupe_ops(
        inline_handlers(
            inline_pures(
                fuse_operations(Tree)
            )
        )
    ).

-spec fuse_operations(hefty_tree()) -> hefty_tree().
fuse_operations(#hefty_tree{kind = seq, subtrees = Trees} = Tree) ->
    Tree#hefty_tree{subtrees = fuse_list([fuse_operations(Subtree) || Subtree <- Trees])};
fuse_operations(#hefty_tree{kind = bind, subtrees = [Tree], continuation = Continuation} = Bind) ->
    Bind#hefty_tree{subtrees = [fuse_operations(Tree)], continuation = Continuation};
fuse_operations(Tree) ->
    Tree.

-spec inline_pures(hefty_tree()) -> hefty_tree().
inline_pures(#hefty_tree{kind = bind, subtrees = [#hefty_tree{kind = pure, value = Value}], continuation = Continuation}) ->
    ContFn = Continuation#continuation.fn,
    lift_result(ContFn(Value));
inline_pures(#hefty_tree{kind = seq, subtrees = Trees} = Tree) ->
    Normalized = [inline_pures(Subtree) || Subtree <- Trees],
    case [Subtree || Subtree <- Normalized, not is_pure_unit(Subtree)] of
        [] -> pure([]);
        [Single] -> Single;
        Remaining -> Tree#hefty_tree{subtrees = Remaining}
    end;
inline_pures(#hefty_tree{kind = bind, subtrees = [Tree]} = Bind) ->
    Bind#hefty_tree{subtrees = [inline_pures(Tree)]};
inline_pures(Tree) ->
    Tree.

-spec inline_handlers(hefty_tree()) -> hefty_tree().
inline_handlers(#hefty_tree{kind = bind, subtrees = [#hefty_tree{kind = op, op = #op_node{continuation = Continuation}} = OpTree], continuation = BindCont}) when Continuation =/= undefined ->
    ContFn = BindCont#continuation.fn,
    bind(OpTree, fun(Value) -> lift_result(ContFn(Value)) end);
inline_handlers(#hefty_tree{kind = seq, subtrees = Trees} = Tree) ->
    Tree#hefty_tree{subtrees = [inline_handlers(Subtree) || Subtree <- Trees]};
inline_handlers(#hefty_tree{kind = bind, subtrees = [Tree]} = Bind) ->
    Bind#hefty_tree{subtrees = [inline_handlers(Tree)]};
inline_handlers(Tree) ->
    Tree.

-spec dedupe_ops(hefty_tree()) -> hefty_tree().
dedupe_ops(#hefty_tree{kind = seq, subtrees = Trees} = Tree) ->
    Tree#hefty_tree{subtrees = dedupe_list([dedupe_ops(Subtree) || Subtree <- Trees])};
dedupe_ops(#hefty_tree{kind = bind, subtrees = [Tree]} = Bind) ->
    Bind#hefty_tree{subtrees = [dedupe_ops(Tree)]};
dedupe_ops(Tree) ->
    Tree.

%%%---------------------------------------------------------------------
%%% Hefty Debugging
%%%---------------------------------------------------------------------

-spec tree_size(hefty_tree()) -> non_neg_integer().
tree_size(#hefty_tree{kind = pure}) ->
    1;
tree_size(#hefty_tree{kind = op}) ->
    1;
tree_size(#hefty_tree{kind = bind, subtrees = [Tree]}) ->
    1 + tree_size(Tree);
tree_size(#hefty_tree{kind = seq, subtrees = Trees}) ->
    1 + lists:sum([tree_size(Tree) || Tree <- Trees]).

-spec tree_depth(hefty_tree()) -> non_neg_integer().
tree_depth(#hefty_tree{kind = pure}) ->
    0;
tree_depth(#hefty_tree{kind = op}) ->
    0;
tree_depth(#hefty_tree{kind = bind, subtrees = [Tree]}) ->
    1 + tree_depth(Tree);
tree_depth(#hefty_tree{kind = seq, subtrees = Trees}) ->
    1 + lists:max([0 | [tree_depth(Tree) || Tree <- Trees]]).

-spec tree_to_list(hefty_tree()) -> [op_node()].
tree_to_list(#hefty_tree{kind = pure}) ->
    [];
tree_to_list(#hefty_tree{kind = op, op = OpNode}) ->
    [OpNode];
tree_to_list(#hefty_tree{kind = bind, subtrees = [Tree]}) ->
    tree_to_list(Tree);
tree_to_list(#hefty_tree{kind = seq, subtrees = Trees}) ->
    lists:flatmap(fun tree_to_list/1, Trees).

-spec format_tree(hefty_tree()) -> binary().
format_tree(#hefty_tree{kind = pure, value = Value}) ->
    <<"pure(", (format_term(Value))/binary, ")">>;
format_tree(#hefty_tree{kind = op, op = #op_node{effect = Effect, operation = Operation, continuation = undefined}}) ->
    <<(atom_to_binary_local(Effect))/binary, ".", (atom_to_binary_local(Operation))/binary>>;
format_tree(#hefty_tree{kind = op, op = #op_node{effect = Effect, operation = Operation}}) ->
    <<(atom_to_binary_local(Effect))/binary, ".", (atom_to_binary_local(Operation))/binary, "{cont}">>;
format_tree(#hefty_tree{kind = bind, subtrees = [Tree]}) ->
    <<"bind(", (format_tree(Tree))/binary, ", fun)">>;
format_tree(#hefty_tree{kind = seq, subtrees = Trees}) ->
    iolist_to_binary(["seq(", lists:join(<<"; ">>, [format_tree(Tree) || Tree <- Trees]), ")"]).

%%%---------------------------------------------------------------------
%%% Internal Helpers
%%%---------------------------------------------------------------------

-spec interpret_operation(op_node(), hefty_handler()) -> term().
interpret_operation(OpNode, #hefty_handler{operations = Ops, fallback = Fallback}) ->
    KeyCandidates = [
        {OpNode#op_node.effect, OpNode#op_node.operation},
        OpNode#op_node.operation,
        OpNode#op_node.effect
    ],
    case lookup_handler(KeyCandidates, Ops) of
        {ok, HandlerFn} ->
            lift_or_interpret(apply_handler(HandlerFn, OpNode), #hefty_handler{operations = Ops, fallback = Fallback});
        error ->
            case Fallback of
                undefined -> {error, {unhandled, OpNode}};
                HandlerFn -> lift_or_interpret(apply_fallback(HandlerFn, OpNode), #hefty_handler{operations = Ops, fallback = Fallback})
            end
    end.

-spec interpret_sequence([hefty_tree()], hefty_handler()) -> term().
interpret_sequence([], _Handler) ->
    undefined;
interpret_sequence([Tree], Handler) ->
    interpret(Tree, Handler);
interpret_sequence([Tree | Rest], Handler) ->
    _ = interpret(Tree, Handler),
    interpret_sequence(Rest, Handler).

-spec interpret_continuation(function(), term(), hefty_handler()) -> term().
interpret_continuation(ContFn, Value, Handler) ->
    lift_or_interpret(ContFn(Value), Handler).

-spec apply_handler(function(), op_node()) -> term().
apply_handler(HandlerFn, #op_node{args = Args, continuation = Continuation}) ->
    case erlang:fun_info(HandlerFn, arity) of
        {arity, 0} ->
            HandlerFn();
        {arity, 1} ->
            HandlerFn(Args);
        {arity, 2} ->
            HandlerFn(Args, Continuation);
        _ ->
            apply(HandlerFn, Args)
    end.

-spec apply_fallback(function(), op_node()) -> term().
apply_fallback(Fallback, OpNode) ->
    case erlang:fun_info(Fallback, arity) of
        {arity, 1} -> Fallback(OpNode);
        {arity, 2} -> Fallback(OpNode#op_node.args, OpNode);
        _ -> {error, {unsupported_fallback_arity, Fallback}}
    end.

-spec lookup_handler([term()], #{term() => function()}) -> {ok, function()} | error.
lookup_handler([], _Ops) ->
    error;
lookup_handler([Key | Rest], Ops) ->
    case maps:find(Key, Ops) of
        {ok, HandlerFn} -> {ok, HandlerFn};
        error -> lookup_handler(Rest, Ops)
    end.

-spec lift_or_interpret(term(), hefty_handler()) -> term().
lift_or_interpret(#hefty_tree{} = Tree, Handler) ->
    interpret(Tree, Handler);
lift_or_interpret(Value, _Handler) ->
    Value.

-spec lift_result(term()) -> hefty_tree().
lift_result(#hefty_tree{} = Tree) ->
    Tree;
lift_result(Value) ->
    pure(Value).

-spec unwrap_hefty(term()) -> term().
unwrap_hefty(#hefty_tree{kind = pure, value = Value}) ->
    Value;
unwrap_hefty(Value) ->
    Value.

-spec fuse_list([hefty_tree()]) -> [hefty_tree()].
fuse_list([#hefty_tree{kind = op, op = #op_node{effect = Effect, operation = Operation, args = Args1, continuation = undefined}} = Tree1,
           #hefty_tree{kind = op, op = #op_node{effect = Effect, operation = Operation, args = Args2, continuation = undefined}} | Rest]) ->
    Fused = Tree1#hefty_tree{
        op = #op_node{
            effect = Effect,
            operation = Operation,
            args = Args1 ++ Args2,
            continuation = undefined
        }
    },
    fuse_list([Fused | Rest]);
fuse_list([Tree | Rest]) ->
    [Tree | fuse_list(Rest)];
fuse_list([]) ->
    [].

-spec dedupe_list([hefty_tree()]) -> [hefty_tree()].
dedupe_list([Tree1, Tree2 | Rest]) ->
    case same_operation(Tree1, Tree2) of
        true -> dedupe_list([Tree1 | Rest]);
        false -> [Tree1 | dedupe_list([Tree2 | Rest])]
    end;
dedupe_list([Tree]) ->
    [Tree];
dedupe_list([]) ->
    [].

-spec same_operation(hefty_tree(), hefty_tree()) -> boolean().
same_operation(#hefty_tree{kind = op, op = #op_node{continuation = undefined} = Op1},
               #hefty_tree{kind = op, op = #op_node{continuation = undefined} = Op2}) ->
    Op1 =:= Op2;
same_operation(_, _) ->
    false.

-spec is_pure_unit(hefty_tree()) -> boolean().
is_pure_unit(#hefty_tree{kind = pure, value = []}) ->
    true;
is_pure_unit(#hefty_tree{kind = pure, value = undefined}) ->
    true;
is_pure_unit(_) ->
    false.

-spec format_term(term()) -> binary().
format_term(Term) when is_atom(Term) ->
    atom_to_binary_local(Term);
format_term(Term) when is_integer(Term) ->
    integer_to_binary(Term);
format_term(Term) when is_binary(Term) ->
    Term;
format_term(Term) when is_list(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term]));
format_term(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).

-spec atom_to_binary_local(atom()) -> binary().
atom_to_binary_local(Atom) ->
    list_to_binary(erlang:atom_to_list(Atom)).
