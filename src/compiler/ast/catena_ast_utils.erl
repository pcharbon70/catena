-module(catena_ast_utils).

%% @doc AST utility functions for the Catena compiler
%%
%% This module provides utilities for manipulating Abstract Syntax Trees,
%% including traversal, validation, and pretty-printing functions.
%%
%% == Traversal Order ==
%%
%% All traversal functions in this module use BOTTOM-UP (post-order) traversal:
%% 1. First, recursively process all child nodes
%% 2. Then, apply the function to the parent node
%%
%% This order is consistent across all three traversal functions:
%% - `map_expr/2': Transforms the AST bottom-up
%% - `fold_expr/3': Accumulates values bottom-up
%% - `walk_expr/2': Executes side effects bottom-up
%%
%% Bottom-up traversal is useful for:
%% - Computing attributes that depend on children (e.g., tree height, type inference)
%% - Propagating constraints upward from leaves to root
%% - Ensuring child nodes are processed before their parents
%%
%% Example: Counting literals in an expression
%% ```
%% Expr = (1 + 2) * 3
%% fold_expr(fun({literal, _, _, _}, Acc) -> Acc + 1;
%%              (_, Acc) -> Acc
%%           end, 0, Expr)
%% % Returns: 3 (after visiting 1, 2, 3, then their parents)
%% ```

%% Suppress type checking due to forward references in header file
-compile([{no_auto_import, []}, nowarn_export_all]).
-compile([nowarn_unused_type, nowarn_unused_function]).

%% Include AST record definitions
-include("../parser/catena_ast.hrl").

%% Maximum recursion depth to prevent stack overflow attacks
-define(MAX_DEPTH, 1000).

%% AST Traversal
-export([
    map_expr/2,
    fold_expr/3,
    walk_expr/2
]).

%% AST Validation
-export([
    validate_ast/1,
    check_duplicate_names/1
]).

%% AST Pretty-printing
-export([
    format_expr/1,
    format_pattern/1,
    format_type/1,
    format_decl/1
]).

%% Location utilities
-export([
    get_location/1,
    default_location/0
]).

%%====================================================================
%% Traversal Functions
%%====================================================================

%% @doc Map a function over all sub-expressions in an expression (bottom-up)
%% This is the public API that initializes depth tracking
map_expr(Fun, Expr) ->
    map_expr(Fun, Expr, 0).

%% @doc Internal map_expr with depth tracking
map_expr(_Fun, _Expr, Depth) when Depth > ?MAX_DEPTH ->
    throw({error, {max_depth_exceeded, ?MAX_DEPTH}});
map_expr(Fun, Expr, Depth) ->
    NextDepth = Depth + 1,
    %% First recursively map over children
    Mapped = case Expr of
        #binary_op{op=Op, left=Left, right=Right, location=Loc} ->
            #binary_op{op=Op, left=map_expr(Fun, Left, NextDepth), right=map_expr(Fun, Right, NextDepth), location=Loc};
        #unary_op{op=Op, operand=Operand, location=Loc} ->
            #unary_op{op=Op, operand=map_expr(Fun, Operand, NextDepth), location=Loc};
        #app{func=Func, args=Args, location=Loc} ->
            #app{func=map_expr(Fun, Func, NextDepth), args=[map_expr(Fun, Arg, NextDepth) || Arg <- Args], location=Loc};
        #lambda{params=Params, body=Body, location=Loc} ->
            #lambda{params=Params, body=map_expr(Fun, Body, NextDepth), location=Loc};
        #let_expr{bindings=Bindings, body=Body, location=Loc} ->
            NewBindings = [{Pat, map_expr(Fun, E, NextDepth)} || {Pat, E} <- Bindings],
            #let_expr{bindings=NewBindings, body=map_expr(Fun, Body, NextDepth), location=Loc};
        #if_expr{condition=Cond, then_branch=Then, else_branch=Else, location=Loc} ->
            #if_expr{condition=map_expr(Fun, Cond, NextDepth), then_branch=map_expr(Fun, Then, NextDepth), else_branch=map_expr(Fun, Else, NextDepth), location=Loc};
        #match_expr{clauses=Clauses, location=Loc} ->
            NewClauses = [map_match_clause(Fun, C, NextDepth) || C <- Clauses],
            #match_expr{clauses=NewClauses, location=Loc};
        #list_expr{elements=Elements, location=Loc} ->
            #list_expr{elements=[map_expr(Fun, E, NextDepth) || E <- Elements], location=Loc};
        #tuple_expr{elements=Elements, location=Loc} ->
            #tuple_expr{elements=[map_expr(Fun, E, NextDepth) || E <- Elements], location=Loc};
        #record_expr{fields=Fields, base=Base, location=Loc} ->
            NewFields = [{Name, map_expr(Fun, E, NextDepth)} || {Name, E} <- Fields],
            NewBase = case Base of
                undefined -> undefined;
                B -> map_expr(Fun, B, NextDepth)
            end,
            #record_expr{fields=NewFields, base=NewBase, location=Loc};
        #record_access{record=Record, field=Field, location=Loc} ->
            #record_access{record=map_expr(Fun, Record, NextDepth), field=Field, location=Loc};
        #perform_expr{effect=Effect, operation=Operation, args=Args, location=Loc} ->
            #perform_expr{effect=Effect, operation=Operation, args=[map_expr(Fun, Arg, NextDepth) || Arg <- Args], location=Loc};
        #try_with_expr{body=Body, handlers=Handlers, location=Loc} ->
            NewBody = map_expr(Fun, Body, NextDepth),
            NewHandlers = [map_handler(Fun, H, NextDepth) || H <- Handlers],
            #try_with_expr{body=NewBody, handlers=NewHandlers, location=Loc};
        _ ->
            %% Leaf nodes (literals, variables)
            Expr
    end,
    %% Then apply the function to the resulting node
    Fun(Mapped).

map_match_clause(Fun, #match_clause{pattern=Pattern, guards=Guards, body=Body, location=Loc}, Depth) ->
    NewGuards = case Guards of
        undefined -> undefined;
        Gs -> [map_expr(Fun, G, Depth) || G <- Gs]
    end,
    #match_clause{pattern=Pattern, guards=NewGuards, body=map_expr(Fun, Body, Depth), location=Loc}.

map_handler(Fun, #handler_clause{effect=Effect, operations=Operations, location=Loc}, Depth) ->
    NewOps = [map_operation_case(Fun, Op, Depth) || Op <- Operations],
    #handler_clause{effect=Effect, operations=NewOps, location=Loc}.

map_operation_case(Fun, #operation_case{operation=Operation, params=Params, body=Body, location=Loc}, Depth) ->
    #operation_case{operation=Operation, params=Params, body=map_expr(Fun, Body, Depth), location=Loc}.

%% @doc Fold over all sub-expressions in an expression (bottom-up)
%% First recursively processes children, then applies function to parent node
%% This is the public API that initializes depth tracking
fold_expr(Fun, Acc, Expr) ->
    fold_expr(Fun, Acc, Expr, 0).

%% @doc Internal fold_expr with depth tracking
fold_expr(_Fun, _Acc, _Expr, Depth) when Depth > ?MAX_DEPTH ->
    throw({error, {max_depth_exceeded, ?MAX_DEPTH}});
fold_expr(Fun, Acc, Expr, Depth) ->
    NextDepth = Depth + 1,
    %% First recursively fold over children
    ChildAcc = case Expr of
        #binary_op{left=Left, right=Right} ->
            Acc2 = fold_expr(Fun, Acc, Left, NextDepth),
            fold_expr(Fun, Acc2, Right, NextDepth);
        #unary_op{operand=Operand} ->
            fold_expr(Fun, Acc, Operand, NextDepth);
        #app{func=Func, args=Args} ->
            Acc2 = fold_expr(Fun, Acc, Func, NextDepth),
            lists:foldl(fun(Arg, A) -> fold_expr(Fun, A, Arg, NextDepth) end, Acc2, Args);
        #lambda{body=Body} ->
            fold_expr(Fun, Acc, Body, NextDepth);
        #let_expr{bindings=Bindings, body=Body} ->
            Acc2 = lists:foldl(fun({_Pat, E}, A) -> fold_expr(Fun, A, E, NextDepth) end, Acc, Bindings),
            fold_expr(Fun, Acc2, Body, NextDepth);
        #if_expr{condition=Cond, then_branch=Then, else_branch=Else} ->
            Acc2 = fold_expr(Fun, Acc, Cond, NextDepth),
            Acc3 = fold_expr(Fun, Acc2, Then, NextDepth),
            fold_expr(Fun, Acc3, Else, NextDepth);
        #match_expr{clauses=Clauses} ->
            lists:foldl(fun(C, A) -> fold_match_clause(Fun, A, C, NextDepth) end, Acc, Clauses);
        #list_expr{elements=Elements} ->
            lists:foldl(fun(E, A) -> fold_expr(Fun, A, E, NextDepth) end, Acc, Elements);
        #tuple_expr{elements=Elements} ->
            lists:foldl(fun(E, A) -> fold_expr(Fun, A, E, NextDepth) end, Acc, Elements);
        #record_expr{fields=Fields, base=Base} ->
            Acc2 = lists:foldl(fun({_Name, E}, A) -> fold_expr(Fun, A, E, NextDepth) end, Acc, Fields),
            case Base of
                undefined -> Acc2;
                B -> fold_expr(Fun, Acc2, B, NextDepth)
            end;
        #record_access{record=Record} ->
            fold_expr(Fun, Acc, Record, NextDepth);
        #perform_expr{args=Args} ->
            lists:foldl(fun(Arg, A) -> fold_expr(Fun, A, Arg, NextDepth) end, Acc, Args);
        #try_with_expr{body=Body, handlers=Handlers} ->
            Acc2 = fold_expr(Fun, Acc, Body, NextDepth),
            lists:foldl(fun(H, A) -> fold_handler(Fun, A, H, NextDepth) end, Acc2, Handlers);
        _ ->
            %% Leaf nodes
            Acc
    end,
    %% Then apply function to current node
    Fun(Expr, ChildAcc).

fold_match_clause(Fun, Acc, #match_clause{guards=Guards, body=Body}, Depth) ->
    Acc2 = case Guards of
        undefined -> Acc;
        Gs -> lists:foldl(fun(G, A) -> fold_expr(Fun, A, G, Depth) end, Acc, Gs)
    end,
    fold_expr(Fun, Acc2, Body, Depth).

fold_handler(Fun, Acc, #handler_clause{operations=Operations}, Depth) ->
    lists:foldl(fun(Op, A) -> fold_operation_case(Fun, A, Op, Depth) end, Acc, Operations).

fold_operation_case(Fun, Acc, #operation_case{body=Body}, Depth) ->
    fold_expr(Fun, Acc, Body, Depth).

%% @doc Walk the AST, executing a function for each node (bottom-up)
%% First recursively walks children, then executes function on parent node
%% This is the public API that initializes depth tracking
walk_expr(Fun, Expr) ->
    walk_expr(Fun, Expr, 0).

%% @doc Internal walk_expr with depth tracking
walk_expr(_Fun, _Expr, Depth) when Depth > ?MAX_DEPTH ->
    throw({error, {max_depth_exceeded, ?MAX_DEPTH}});
walk_expr(Fun, Expr, Depth) ->
    NextDepth = Depth + 1,
    %% First recursively walk children
    case Expr of
        #binary_op{left=Left, right=Right} ->
            walk_expr(Fun, Left, NextDepth),
            walk_expr(Fun, Right, NextDepth);
        #unary_op{operand=Operand} ->
            walk_expr(Fun, Operand, NextDepth);
        #app{func=Func, args=Args} ->
            walk_expr(Fun, Func, NextDepth),
            [walk_expr(Fun, Arg, NextDepth) || Arg <- Args];
        #lambda{body=Body} ->
            walk_expr(Fun, Body, NextDepth);
        #let_expr{bindings=Bindings, body=Body} ->
            [walk_expr(Fun, E, NextDepth) || {_Pat, E} <- Bindings],
            walk_expr(Fun, Body, NextDepth);
        #if_expr{condition=Cond, then_branch=Then, else_branch=Else} ->
            walk_expr(Fun, Cond, NextDepth),
            walk_expr(Fun, Then, NextDepth),
            walk_expr(Fun, Else, NextDepth);
        #match_expr{clauses=Clauses} ->
            [walk_match_clause(Fun, C, NextDepth) || C <- Clauses];
        #list_expr{elements=Elements} ->
            [walk_expr(Fun, E, NextDepth) || E <- Elements];
        #tuple_expr{elements=Elements} ->
            [walk_expr(Fun, E, NextDepth) || E <- Elements];
        #record_expr{fields=Fields, base=Base} ->
            [walk_expr(Fun, E, NextDepth) || {_Name, E} <- Fields],
            case Base of
                undefined -> ok;
                B -> walk_expr(Fun, B, NextDepth)
            end;
        #record_access{record=Record} ->
            walk_expr(Fun, Record, NextDepth);
        #perform_expr{args=Args} ->
            [walk_expr(Fun, Arg, NextDepth) || Arg <- Args];
        #try_with_expr{body=Body, handlers=Handlers} ->
            walk_expr(Fun, Body, NextDepth),
            [walk_handler(Fun, H, NextDepth) || H <- Handlers];
        _ ->
            %% Leaf nodes
            ok
    end,
    %% Then execute function on current node
    Fun(Expr).

walk_match_clause(Fun, #match_clause{guards=Guards, body=Body}, Depth) ->
    case Guards of
        undefined -> ok;
        Gs -> [walk_expr(Fun, G, Depth) || G <- Gs]
    end,
    walk_expr(Fun, Body, Depth).

walk_handler(Fun, #handler_clause{operations=Operations}, Depth) ->
    [walk_operation_case(Fun, Op, Depth) || Op <- Operations].

walk_operation_case(Fun, #operation_case{body=Body}, Depth) ->
    walk_expr(Fun, Body, Depth).

%%====================================================================
%% Validation Functions
%%====================================================================

%% @doc Validate an AST node with multiple structural checks
validate_ast(AST) ->
    try
        %% Check locations first - structural integrity before semantic checks
        check_location_formats(AST),
        check_literal_types(AST),
        check_valid_names(AST),
        check_duplicate_names(AST),
        ok
    catch
        throw:{error, Reason} -> {error, Reason}
    end.

%% @doc Check for duplicate names in declarations
check_duplicate_names(#module{declarations=Declarations}) ->
    Names = [get_decl_name(D) || D <- Declarations],
    Duplicates = Names -- lists:usort(Names),
    case Duplicates of
        [] -> ok;
        [First | _] -> throw({error, {duplicate_declaration, First}})
    end;
check_duplicate_names(_) ->
    ok.

%% @doc Check that all location metadata is in valid format
check_location_formats(AST) ->
    %% First validate the module node itself and its declarations
    case AST of
        #module{declarations=Declarations, location=Loc} ->
            case is_valid_location(Loc) of
                true -> ok;
                false -> throw({error, {invalid_location, Loc}})
            end,
            %% Validate each declaration using fold_expr
            lists:foreach(
                fun(Decl) ->
                    fold_expr(
                        fun(Node, _Acc) ->
                            NodeLoc = get_location(Node),
                            case is_valid_location(NodeLoc) of
                                true -> ok;
                                false -> throw({error, {invalid_location, NodeLoc}})
                            end,
                            ok
                        end,
                        ok,
                        Decl
                    )
                end,
                Declarations
            );
        _ ->
            %% For non-module nodes, just use fold_expr
            fold_expr(
                fun(Node, _Acc) ->
                    Loc = get_location(Node),
                    case is_valid_location(Loc) of
                        true -> ok;
                        false -> throw({error, {invalid_location, Loc}})
                    end,
                    ok
                end,
                ok,
                AST
            )
    end.

%% @doc Check if a location is in valid format
is_valid_location({line, N}) when is_integer(N), N > 0 -> true;
is_valid_location({location, Line, Col}) when is_integer(Line), is_integer(Col),
                                               Line > 0, Col >= 0 -> true;
is_valid_location(_) -> false.

%% @doc Check that all literals have valid type annotations
check_literal_types(AST) ->
    fold_expr(
        fun(Node, _Acc) ->
            case Node of
                {literal, _Value, Type, _Loc} ->
                    case lists:member(Type, [integer, float, string, atom, boolean]) of
                        true -> ok;
                        false -> throw({error, {invalid_literal_type, Type}})
                    end;
                {pat_literal, _Value, Type, _Loc} ->
                    case lists:member(Type, [integer, float, string, atom, boolean]) of
                        true -> ok;
                        false -> throw({error, {invalid_literal_type, Type}})
                    end;
                _ -> ok
            end,
            ok
        end,
        ok,
        AST
    ).

%% @doc Check that all names (variables, functions, etc.) are valid atoms
check_valid_names(AST) ->
    ValidateFun = fun(Node, _Acc) ->
        case Node of
            #var{name=Name} ->
                check_is_valid_name(Name);
            #pat_var{name=Name} ->
                check_is_valid_name(Name);
            #lambda{params=Params} ->
                %% Validate parameter names in lambda
                lists:foreach(
                    fun(#pat_var{name=ParamName}) -> check_is_valid_name(ParamName);
                       (_) -> ok
                    end,
                    Params
                ),
                ok;
            #flow_decl{name=Name} ->
                check_is_valid_name(Name);
            #shape_decl{name=Name} ->
                check_is_valid_name(Name);
            #effect_decl{name=Name} ->
                check_is_valid_name(Name);
            #trait_decl{name=Name} ->
                check_is_valid_name(Name);
            _ -> ok
        end,
        ok
    end,
    %% Handle module nodes specially to traverse declarations
    case AST of
        #module{declarations=Declarations} ->
            %% Validate each declaration
            lists:foreach(
                fun(Decl) ->
                    fold_expr(ValidateFun, ok, Decl)
                end,
                Declarations
            );
        _ ->
            %% For non-module nodes, just use fold_expr
            fold_expr(ValidateFun, ok, AST)
    end.

%% @doc Check if a name is a valid atom (not undefined, empty, etc.)
check_is_valid_name(Name) when is_atom(Name), Name =/= undefined, Name =/= '' ->
    ok;
check_is_valid_name(Name) ->
    throw({error, {invalid_name, Name}}).

get_decl_name(#shape_decl{name=Name}) -> Name;
get_decl_name(#flow_decl{name=Name}) -> Name;
get_decl_name(#effect_decl{name=Name}) -> Name;
get_decl_name(#trait_decl{name=Name}) -> Name;
get_decl_name(_) -> undefined.

%%====================================================================
%% Pretty-printing Functions
%%====================================================================

%% @doc Format an expression as a string
format_expr(#literal{value=Value, type=Type}) ->
    format_literal(Value, Type);
format_expr(#var{name=Name}) ->
    atom_to_list(Name);
format_expr(#binary_op{op=Op, left=Left, right=Right}) ->
    io_lib:format("(~s ~s ~s)", [format_expr(Left), atom_to_list(Op), format_expr(Right)]);
format_expr(#unary_op{op=Op, operand=Operand}) ->
    io_lib:format("(~s ~s)", [atom_to_list(Op), format_expr(Operand)]);
format_expr(#app{func=Func, args=Args}) ->
    ArgsStr = string:join([format_expr(Arg) || Arg <- Args], " "),
    io_lib:format("(~s ~s)", [format_expr(Func), ArgsStr]);
format_expr(#lambda{params=Params, body=Body}) ->
    ParamsStr = string:join([format_pattern(P) || P <- Params], " "),
    io_lib:format("(\\~s -> ~s)", [ParamsStr, format_expr(Body)]);
format_expr(#let_expr{bindings=Bindings, body=Body}) ->
    BindingsStr = string:join([io_lib:format("~s = ~s", [format_pattern(P), format_expr(E)]) || {P, E} <- Bindings], ", "),
    io_lib:format("let ~s in ~s", [BindingsStr, format_expr(Body)]);
format_expr(#if_expr{condition=Cond, then_branch=Then, else_branch=Else}) ->
    io_lib:format("if ~s then ~s else ~s", [format_expr(Cond), format_expr(Then), format_expr(Else)]);
format_expr(#list_expr{elements=Elements}) ->
    ElementsStr = string:join([format_expr(E) || E <- Elements], ", "),
    io_lib:format("[~s]", [ElementsStr]);
format_expr(#tuple_expr{elements=Elements}) ->
    ElementsStr = string:join([format_expr(E) || E <- Elements], ", "),
    io_lib:format("(~s)", [ElementsStr]);
format_expr(#perform_expr{effect=Effect, operation=Operation, args=Args}) ->
    ArgsStr = string:join([format_expr(Arg) || Arg <- Args], ", "),
    io_lib:format("perform ~s.~s(~s)", [atom_to_list(Effect), atom_to_list(Operation), ArgsStr]);
format_expr(_) ->
    "<??>".

format_literal(Value, integer) -> integer_to_list(Value);
format_literal(Value, float) -> float_to_list(Value);
format_literal(Value, string) -> io_lib:format("\"~s\"", [Value]);
format_literal(Value, atom) -> atom_to_list(Value);
format_literal(Value, boolean) -> atom_to_list(Value).

%% @doc Format a pattern as a string
format_pattern(#pat_var{name=Name}) ->
    atom_to_list(Name);
format_pattern(#pat_wildcard{}) ->
    "_";
format_pattern(#pat_literal{value=Value, type=Type}) ->
    format_literal(Value, Type);
format_pattern(#pat_constructor{name=Name, args=Args}) ->
    ArgsStr = string:join([format_pattern(Arg) || Arg <- Args], " "),
    io_lib:format("~s ~s", [atom_to_list(Name), ArgsStr]);
format_pattern(#pat_list{elements=Elements}) ->
    ElementsStr = string:join([format_pattern(E) || E <- Elements], ", "),
    io_lib:format("[~s]", [ElementsStr]);
format_pattern(#pat_tuple{elements=Elements}) ->
    ElementsStr = string:join([format_pattern(E) || E <- Elements], ", "),
    io_lib:format("(~s)", [ElementsStr]);
format_pattern(_) ->
    "_".

%% @doc Format a type expression as a string
format_type(#type_var{name=Name}) ->
    atom_to_list(Name);
format_type(#type_con{name=Name}) ->
    atom_to_list(Name);
format_type(#type_fun{from=From, to=To}) ->
    io_lib:format("~s -> ~s", [format_type(From), format_type(To)]);
format_type(#type_app{constructor=Constructor, args=Args}) ->
    ArgsStr = string:join([format_type(Arg) || Arg <- Args], " "),
    io_lib:format("~s ~s", [format_type(Constructor), ArgsStr]);
format_type(#type_effect{type=Type, effects=Effects}) ->
    EffectsStr = string:join([atom_to_list(E) || E <- Effects], ", "),
    io_lib:format("~s / {~s}", [format_type(Type), EffectsStr]);
format_type(_) ->
    "?".

%% @doc Format a declaration as a string
format_decl(#shape_decl{name=Name}) ->
    io_lib:format("shape ~s", [atom_to_list(Name)]);
format_decl(#flow_decl{name=Name}) ->
    io_lib:format("flow ~s", [atom_to_list(Name)]);
format_decl(#effect_decl{name=Name}) ->
    io_lib:format("effect ~s", [atom_to_list(Name)]);
format_decl(_) ->
    "?".

%%====================================================================
%% Location Utilities
%%====================================================================

%% @doc Get location from an AST node
get_location(Node) when is_tuple(Node) ->
    element(tuple_size(Node), Node);
get_location(_) ->
    default_location().

%% @doc Create a default location
default_location() ->
    {line, 1}.
