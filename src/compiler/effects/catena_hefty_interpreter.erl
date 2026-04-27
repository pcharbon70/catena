%%%-------------------------------------------------------------------
%%% @doc Thin interpreter surface for hefty trees.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_hefty_interpreter).

-export([
    interpret/2,
    interpret_many/2,
    handle_operation/4
]).

-spec interpret(catena_hefty:hefty_tree(), catena_hefty:hefty_handler()) -> term().
interpret(Tree, Handler) ->
    catena_hefty:interpret(Tree, Handler).

-spec interpret_many([catena_hefty:hefty_tree()], catena_hefty:hefty_handler()) -> [term()].
interpret_many(Trees, Handler) ->
    [interpret(Tree, Handler) || Tree <- Trees].

-spec handle_operation(atom(), atom(), function(), catena_hefty:hefty_tree()) ->
    catena_hefty:hefty_tree().
handle_operation(Effect, Operation, HandlerFn, Tree) ->
    catena_hefty:handle_op(Effect, Operation, HandlerFn, Tree).
