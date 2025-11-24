%%%-------------------------------------------------------------------
%%% @doc AST Pretty-Printing
%%%
%%% Converts Catena AST back to source code strings. Useful for:
%%% - Debugging and inspection
%%% - Roundtrip testing
%%% - Code generation
%%% - REPL output
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_ast_pp).

-export([
    %% Main entry points
    pp_expr/1,
    pp_pattern/1,
    pp_decl/1,
    pp_module/1,

    %% Utilities
    format_expr/1
]).

%%%===================================================================
%%% Expression Pretty-Printing
%%%===================================================================

%% @doc Pretty-print an expression to source code
-spec pp_expr(term()) -> string().
pp_expr(Expr) ->
    lists:flatten(pp_expr_iolist(Expr)).

%% @doc Alias for pp_expr/1
-spec format_expr(term()) -> string().
format_expr(Expr) ->
    pp_expr(Expr).

%% Internal: build iolist for expressions
pp_expr_iolist({var, Name, _Loc}) ->
    atom_to_list(Name);

pp_expr_iolist({literal, Value, integer, _Loc}) ->
    integer_to_list(Value);

pp_expr_iolist({literal, Value, float, _Loc}) ->
    float_to_list(Value);

pp_expr_iolist({literal, Value, string, _Loc}) when is_binary(Value) ->
    ["\"", escape_string(binary_to_list(Value)), "\""];

pp_expr_iolist({literal, Value, string, _Loc}) when is_list(Value) ->
    ["\"", escape_string(Value), "\""];

pp_expr_iolist({app, Fun, Args, _Loc}) ->
    FunStr = pp_expr_with_parens(Fun),
    case Args of
        [] ->
            [FunStr, "()"];
        [Arg] ->
            %% Single argument - use normal call syntax
            [FunStr, "(", pp_arg_expr(Arg), ")"];
        _ ->
            %% Multiple arguments - use curried form f(a)(b) to avoid tuple interpretation
            lists:foldl(fun(Arg, Acc) ->
                [Acc, "(", pp_arg_expr(Arg), ")"]
            end, FunStr, Args)
    end;

pp_expr_iolist({lambda, Params, Body, _Loc}) ->
    ParamStrs = [pp_pattern_iolist(P) || P <- Params],
    ["fn ", join(ParamStrs, " "), " -> ", pp_expr_iolist(Body)];

pp_expr_iolist({let_expr, Bindings, Body, _Loc}) ->
    BindStrs = [pp_binding(B) || B <- Bindings],
    ["let ", join(BindStrs, ", "), " in ", pp_expr_iolist(Body)];

pp_expr_iolist({binary_op, Op, Left, Right, _Loc}) ->
    [pp_expr_with_parens(Left), " ", atom_to_list(Op), " ", pp_expr_with_parens(Right)];

pp_expr_iolist({tuple_expr, Elems, _Loc}) ->
    ElemStrs = [pp_expr_iolist(E) || E <- Elems],
    ["(", join(ElemStrs, ", "), ")"];

pp_expr_iolist({list_expr, Elems, _Loc}) ->
    ElemStrs = [pp_expr_iolist(E) || E <- Elems],
    ["[", join(ElemStrs, ", "), "]"];

pp_expr_iolist({cons_expr, Head, Tail, _Loc}) ->
    [pp_expr_iolist(Head), " :: ", pp_expr_iolist(Tail)];

pp_expr_iolist({record_expr, Fields, undefined, _Loc}) ->
    FieldStrs = [pp_field(F) || F <- Fields],
    ["{", join(FieldStrs, ", "), "}"];

pp_expr_iolist({record_expr, Fields, Base, _Loc}) ->
    FieldStrs = [pp_field(F) || F <- Fields],
    ["{", pp_expr_iolist(Base), " | ", join(FieldStrs, ", "), "}"];

pp_expr_iolist({record_access, Expr, Field, _Loc}) ->
    [pp_expr_with_parens(Expr), ".", atom_to_list(Field)];

pp_expr_iolist({match_expr, undefined, Clauses, _Loc}) ->
    ClauseStrs = [pp_match_clause(C) || C <- Clauses],
    ["match\n", join(ClauseStrs, "\n"), "\nend"];

pp_expr_iolist({match_expr, Scrutinee, Clauses, _Loc}) ->
    ClauseStrs = [pp_match_clause(C) || C <- Clauses],
    ["match ", pp_expr_iolist(Scrutinee), "\n", join(ClauseStrs, "\n"), "\nend"];

pp_expr_iolist({do_expr, Stmts, _Loc}) ->
    StmtStrs = [pp_do_stmt(S) || S <- Stmts],
    ["do { ", join(StmtStrs, "; "), " }"];

pp_expr_iolist({perform_expr, Effect, Op, Args, _Loc}) ->
    ArgStrs = [pp_expr_iolist(A) || A <- Args],
    ["perform ", atom_to_list(Effect), ".", atom_to_list(Op), "(", join(ArgStrs, ", "), ")"];

pp_expr_iolist({handle_expr, Body, Handlers, _Loc}) ->
    HandlerStrs = [pp_handler(H) || H <- Handlers],
    ["handle ", pp_expr_iolist(Body), " then { ", join(HandlerStrs, " "), " }"];

pp_expr_iolist(Other) ->
    %% Fallback for unknown expressions
    io_lib:format("~p", [Other]).

%% Helper for expressions that might need parentheses
pp_expr_with_parens({app, _, _, _} = Expr) ->
    ["(", pp_expr_iolist(Expr), ")"];
pp_expr_with_parens({binary_op, _, _, _, _} = Expr) ->
    ["(", pp_expr_iolist(Expr), ")"];
pp_expr_with_parens({lambda, _, _, _} = Expr) ->
    ["(", pp_expr_iolist(Expr), ")"];
pp_expr_with_parens(Expr) ->
    pp_expr_iolist(Expr).

%% Helper for argument expressions (lambdas need parens to avoid tuple confusion)
pp_arg_expr({lambda, _, _, _} = Expr) ->
    ["(", pp_expr_iolist(Expr), ")"];
pp_arg_expr({binary_op, _, _, _, _} = Expr) ->
    ["(", pp_expr_iolist(Expr), ")"];
pp_arg_expr(Expr) ->
    pp_expr_iolist(Expr).

%% Helper for let bindings
pp_binding({Pat, Expr}) ->
    [pp_pattern_iolist(Pat), " = ", pp_expr_iolist(Expr)];
pp_binding([Pat, Expr]) ->
    [pp_pattern_iolist(Pat), " = ", pp_expr_iolist(Expr)].

%% Helper for record fields
pp_field({Name, Expr}) ->
    [atom_to_list(Name), " = ", pp_expr_iolist(Expr)].

%% Helper for match clauses
pp_match_clause({match_clause, Pattern, undefined, Body, _Loc}) ->
    ["  | ", pp_pattern_iolist(Pattern), " -> ", pp_expr_iolist(Body)];
pp_match_clause({match_clause, Pattern, Guards, Body, _Loc}) ->
    GuardStrs = [pp_expr_iolist(G) || G <- Guards],
    ["  | ", pp_pattern_iolist(Pattern), " when ", join(GuardStrs, ", "), " -> ", pp_expr_iolist(Body)].

%% Helper for do statements
pp_do_stmt({do_bind, Var, Expr, _Loc}) ->
    [atom_to_list(Var), " <- ", pp_expr_iolist(Expr)];
pp_do_stmt({do_action, Expr, _Loc}) ->
    pp_expr_iolist(Expr);
pp_do_stmt({do_let, Var, Expr, _Loc}) ->
    ["let ", atom_to_list(Var), " = ", pp_expr_iolist(Expr)];
pp_do_stmt({do_return, Expr, _Loc}) ->
    pp_expr_iolist(Expr).

%% Helper for effect handlers
pp_handler({handler_clause, Effect, Ops, _Loc}) ->
    OpStrs = [pp_handler_op(O) || O <- Ops],
    [atom_to_list(Effect), " { ", join(OpStrs, ", "), " }"].

pp_handler_op({Op, Params, Body, _Loc}) ->
    ParamStrs = [pp_pattern_iolist(P) || P <- Params],
    [atom_to_list(Op), "(", join(ParamStrs, ", "), ") -> ", pp_expr_iolist(Body)].

%%%===================================================================
%%% Pattern Pretty-Printing
%%%===================================================================

%% @doc Pretty-print a pattern to source code
-spec pp_pattern(term()) -> string().
pp_pattern(Pattern) ->
    lists:flatten(pp_pattern_iolist(Pattern)).

pp_pattern_iolist({pat_var, Name, _Loc}) ->
    atom_to_list(Name);

pp_pattern_iolist({pat_wildcard, _Loc}) ->
    "_";

pp_pattern_iolist({pat_literal, Value, integer, _Loc}) ->
    integer_to_list(Value);

pp_pattern_iolist({pat_literal, Value, float, _Loc}) ->
    float_to_list(Value);

pp_pattern_iolist({pat_literal, Value, string, _Loc}) when is_binary(Value) ->
    ["\"", escape_string(binary_to_list(Value)), "\""];

pp_pattern_iolist({pat_literal, Value, string, _Loc}) when is_list(Value) ->
    ["\"", escape_string(Value), "\""];

pp_pattern_iolist({pat_constructor, Name, [], _Loc}) ->
    atom_to_list(Name);

pp_pattern_iolist({pat_constructor, Name, Args, _Loc}) ->
    ArgStrs = [pp_pattern_iolist(A) || A <- Args],
    [atom_to_list(Name), "(", join(ArgStrs, ", "), ")"];

pp_pattern_iolist({pat_tuple, Elems, _Loc}) ->
    ElemStrs = [pp_pattern_iolist(E) || E <- Elems],
    ["(", join(ElemStrs, ", "), ")"];

pp_pattern_iolist({pat_list, Elems, _Loc}) ->
    ElemStrs = [pp_pattern_iolist(E) || E <- Elems],
    ["[", join(ElemStrs, ", "), "]"];

pp_pattern_iolist({pat_record, Fields, _Loc}) ->
    FieldStrs = [[atom_to_list(N), " = ", pp_pattern_iolist(P)] || {N, P} <- Fields],
    ["{", join(FieldStrs, ", "), "}"];

pp_pattern_iolist({pat_as, Name, Pattern, _Loc}) ->
    [atom_to_list(Name), "@", pp_pattern_iolist(Pattern)];

pp_pattern_iolist(Other) ->
    io_lib:format("~p", [Other]).

%%%===================================================================
%%% Declaration Pretty-Printing
%%%===================================================================

%% @doc Pretty-print a declaration to source code
-spec pp_decl(term()) -> string().
pp_decl(Decl) ->
    lists:flatten(pp_decl_iolist(Decl)).

pp_decl_iolist({transform_decl, Name, undefined, Clauses, _Loc}) ->
    ClauseStrs = [pp_transform_clause(Name, C) || C <- Clauses],
    join(ClauseStrs, "\n");

pp_decl_iolist({transform_decl, Name, TypeSig, Clauses, _Loc}) ->
    SigStr = ["transform ", atom_to_list(Name), " : ", pp_type_expr(TypeSig)],
    ClauseStrs = [pp_transform_clause(Name, C) || C <- Clauses],
    [SigStr, "\n", join(ClauseStrs, "\n")];

pp_decl_iolist({type_decl, Name, TypeVars, Constructors, _Derives, _Loc}) ->
    VarStrs = [atom_to_list(V) || V <- TypeVars],
    CtorStrs = [pp_constructor(C) || C <- Constructors],
    ["type ", atom_to_list(Name), " ", join(VarStrs, " "), " = ", join(CtorStrs, " | ")];

pp_decl_iolist(Other) ->
    io_lib:format("~p", [Other]).

pp_transform_clause(Name, {transform_clause, Patterns, undefined, Body, _Loc}) ->
    PatStrs = [pp_pattern_iolist(P) || P <- Patterns],
    ["transform ", atom_to_list(Name), " ", join(PatStrs, " "), " = ", pp_expr_iolist(Body)];
pp_transform_clause(Name, {transform_clause, Patterns, Guards, Body, _Loc}) ->
    PatStrs = [pp_pattern_iolist(P) || P <- Patterns],
    GuardStrs = [pp_expr_iolist(G) || G <- Guards],
    ["transform ", atom_to_list(Name), " ", join(PatStrs, " "), " when ",
     join(GuardStrs, ", "), " = ", pp_expr_iolist(Body)].

pp_constructor({constructor, Name, [], _Loc}) ->
    atom_to_list(Name);
pp_constructor({constructor, Name, Fields, _Loc}) ->
    FieldStrs = [pp_type_expr(F) || F <- Fields],
    [atom_to_list(Name), " ", join(FieldStrs, " ")].

%% Simple type expression pretty printing (for signatures)
pp_type_expr({type_var, Name, _Loc}) ->
    atom_to_list(Name);
pp_type_expr({type_con, Name, _Loc}) ->
    atom_to_list(Name);
pp_type_expr({type_app, Con, Args, _Loc}) ->
    ArgStrs = [pp_type_expr(A) || A <- Args],
    [pp_type_expr(Con), " ", join(ArgStrs, " ")];
pp_type_expr({type_fun, From, To, _Loc}) ->
    [pp_type_expr_with_parens(From), " -> ", pp_type_expr(To)];
pp_type_expr({type_effect, Type, Effects, _Loc}) ->
    EffStrs = [atom_to_list(E) || E <- Effects],
    [pp_type_expr(Type), " / {", join(EffStrs, ", "), "}"];
pp_type_expr(Other) ->
    io_lib:format("~p", [Other]).

pp_type_expr_with_parens({type_fun, _, _, _} = T) ->
    ["(", pp_type_expr(T), ")"];
pp_type_expr_with_parens(T) ->
    pp_type_expr(T).

%%%===================================================================
%%% Module Pretty-Printing
%%%===================================================================

%% @doc Pretty-print a module to source code
-spec pp_module(term()) -> string().
pp_module({module, Name, _Exports, _Imports, Decls, _Loc}) ->
    DeclStrs = [pp_decl(D) || D <- Decls],
    lists:flatten(["module ", atom_to_list(Name), "\n\n", join(DeclStrs, "\n\n"), "\n"]).

%%%===================================================================
%%% Utility Functions
%%%===================================================================

%% Join a list of iolists with a separator
join([], _Sep) -> [];
join([H], _Sep) -> [H];
join([H|T], Sep) -> [H | [[Sep, X] || X <- T]].

%% Escape special characters in strings
escape_string(Str) ->
    lists:flatmap(fun escape_char/1, Str).

escape_char($\n) -> "\\n";
escape_char($\t) -> "\\t";
escape_char($\r) -> "\\r";
escape_char($\\) -> "\\\\";
escape_char($") -> "\\\"";
escape_char(C) -> [C].
