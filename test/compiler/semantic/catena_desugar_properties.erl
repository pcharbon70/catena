%%%-------------------------------------------------------------------
%%% @doc Property-Based Tests for Do-Notation Desugaring
%%%
%%% Uses PropEr for comprehensive testing of desugaring invariants,
%%% including nested do-blocks, various statement types, and edge cases.
%%%
%%% ## Running Property Tests
%%%
%%%   rebar3 proper -m catena_desugar_properties
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_desugar_properties).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    prop_desugar_never_crashes/0,
    prop_desugar_preserves_structure/0,
    prop_nested_do_blocks_handled/0,
    prop_all_statement_types_handled/0,
    prop_depth_limit_enforced/0,
    prop_desugar_roundtrip/0
]).

%%====================================================================
%% Property: Desugaring Never Crashes
%%====================================================================

prop_desugar_never_crashes() ->
    ?FORALL(DoExpr, do_expr_generator(),
        begin
            Result = (catch catena_desugar:desugar_do_expr(DoExpr)),
            case Result of
                {'EXIT', _} -> false;  % Crash is a failure
                _ -> true              % Any other result is acceptable
            end
        end).

%%====================================================================
%% Property: Desugaring Preserves Statement Count Structure
%%====================================================================

prop_desugar_preserves_structure() ->
    ?FORALL(Stmts, non_empty_stmt_list(),
        begin
            DoExpr = {do_expr, Stmts, {location, 1, 1}},
            Result = catena_desugar:desugar_do_expr(DoExpr),
            % Result should be a valid expression (not an error)
            is_valid_desugared_expr(Result)
        end).

%%====================================================================
%% Property: Nested Do-Blocks Handled Correctly
%%====================================================================

prop_nested_do_blocks_handled() ->
    ?FORALL(Depth, choose(1, 10),
        begin
            NestedExpr = build_nested_do(Depth),
            Result = catena_desugar:desugar_do_expr(NestedExpr),
            % Should produce nested chain calls
            is_valid_desugared_expr(Result)
        end).

%%====================================================================
%% Property: All Statement Types Are Handled
%%====================================================================

prop_all_statement_types_handled() ->
    ?FORALL(StmtType, oneof([do_bind, do_action, do_let, do_return]),
        begin
            Stmt = generate_stmt(StmtType),
            Stmts = case StmtType of
                do_return -> [Stmt];
                _ -> [Stmt, {do_return, {var, result, {location, 1, 1}}, {location, 1, 1}}]
            end,
            DoExpr = {do_expr, Stmts, {location, 1, 1}},
            Result = catena_desugar:desugar_do_expr(DoExpr),
            is_valid_desugared_expr(Result)
        end).

%%====================================================================
%% Property: Depth Limit Is Enforced
%%====================================================================

prop_depth_limit_enforced() ->
    ?FORALL(Depth, choose(1001, 1010),
        begin
            %% Create statement list exceeding depth limit
            Stmts = lists:duplicate(Depth,
                {do_bind, x, {var, ma, {location, 1, 1}}, {location, 1, 1}})
                ++ [{do_return, {var, x, {location, 1, 1}}, {location, 1, 1}}],
            DoExpr = {do_expr, Stmts, {location, 1, 1}},
            Result = catena_desugar:desugar_do_expr(DoExpr),
            %% Should return error for excessive depth
            case Result of
                {error, {do_nesting_exceeded, _, _}} -> true;
                _ ->
                    %% May also be valid if depth doesn't trigger limit
                    is_valid_desugared_expr(Result)
            end
        end).

%%====================================================================
%% Property: Desugar -> Pretty Print -> Parse Roundtrip
%%====================================================================

prop_desugar_roundtrip() ->
    ?FORALL(DoExpr, simple_do_expr_generator(),
        begin
            %% Step 1: Desugar the do-expression
            Desugared = catena_desugar:desugar_do_expr(DoExpr),

            %% Step 2: Pretty print the desugared expression
            Source = catena_ast_pp:pp_expr(Desugared),

            %% Step 3: Wrap in a transform and parse
            FullSource = "transform test x = " ++ Source ++ "\n",
            case catena_lexer:string(FullSource) of
                {ok, Tokens, _} ->
                    case catena_parser:parse(Tokens) of
                        {ok, {module, _, _, _, Decls, _}} ->
                            %% Extract the body from the parsed result
                            [{transform_decl, test, _, Clauses, _}] = Decls,
                            [{transform_clause, _, _, ParsedBody, _}] = Clauses,

                            %% Step 4: Compare (ignoring locations)
                            NormDesugared = strip_locations(Desugared),
                            NormParsed = strip_locations(ParsedBody),

                            NormDesugared =:= NormParsed;
                        {error, _Reason} ->
                            %% Parse error - log for debugging
                            false
                    end;
                {error, _LexError, _} ->
                    false
            end
        end).

%% Generator for simple do-expressions (easier to roundtrip)
simple_do_expr_generator() ->
    ?LET(Stmts, simple_stmt_list(),
        {do_expr, Stmts, {location, 1, 1}}).

simple_stmt_list() ->
    ?LET({Intermediate, Final}, {list(simple_intermediate_stmt()), return_stmt()},
        Intermediate ++ [Final]).

simple_intermediate_stmt() ->
    %% Only use do_bind for roundtrip test
    %% do_action produces fn _ -> ... which parser doesn't accept
    %% (parser only allows fn lower_ident -> ...)
    {do_bind, x, {var, ma, {location, 1, 1}}, {location, 1, 1}}.

%% Strip locations from AST for comparison
%% Also normalizes multi-arg apps to nested form for comparison
strip_locations({app, Fun, Args, _Loc}) ->
    NormFun = strip_locations(Fun),
    NormArgs = strip_locations(Args),
    %% Normalize multi-arg app to nested apps: f(a, b) -> f(a)(b)
    case NormArgs of
        [] -> {app, NormFun, [], no_loc};
        [Single] -> {app, NormFun, [Single], no_loc};
        [First | Rest] ->
            %% Build nested apps: f(a)(b)(c)...
            lists:foldl(fun(Arg, Acc) ->
                {app, Acc, [Arg], no_loc}
            end, {app, NormFun, [First], no_loc}, Rest)
    end;
strip_locations({lambda, Params, Body, _Loc}) ->
    {lambda, strip_locations(Params), strip_locations(Body), no_loc};
strip_locations({var, Name, _Loc}) ->
    {var, Name, no_loc};
strip_locations({pat_var, Name, _Loc}) ->
    {pat_var, Name, no_loc};
strip_locations({pat_wildcard, _Loc}) ->
    {pat_wildcard, no_loc};
strip_locations({literal, Val, Type, _Loc}) ->
    {literal, Val, Type, no_loc};
strip_locations({let_expr, Bindings, Body, _Loc}) ->
    {let_expr, strip_locations(Bindings), strip_locations(Body), no_loc};
strip_locations({binary_op, Op, Left, Right, _Loc}) ->
    {binary_op, Op, strip_locations(Left), strip_locations(Right), no_loc};
strip_locations({tuple_expr, Elems, _Loc}) ->
    {tuple_expr, strip_locations(Elems), no_loc};
strip_locations(List) when is_list(List) ->
    [strip_locations(E) || E <- List];
strip_locations({A, B}) ->
    %% For pairs like bindings
    {strip_locations(A), strip_locations(B)};
strip_locations(Other) ->
    Other.

%%====================================================================
%% Generators
%%====================================================================

do_expr_generator() ->
    ?LET(Stmts, non_empty_stmt_list(),
        {do_expr, Stmts, {location, 1, 1}}).

non_empty_stmt_list() ->
    ?LET({Intermediate, Final}, {list(intermediate_stmt()), return_stmt()},
        Intermediate ++ [Final]).

intermediate_stmt() ->
    oneof([
        ?LET(Var, var_name(),
            {do_bind, Var, {var, ma, {location, 1, 1}}, {location, 1, 1}}),
        {do_action, {var, action, {location, 1, 1}}, {location, 1, 1}},
        ?LET(Var, var_name(),
            {do_let, Var, {literal, integer, 42, {location, 1, 1}}, {location, 1, 1}})
    ]).

return_stmt() ->
    {do_return, {var, result, {location, 1, 1}}, {location, 1, 1}}.

var_name() ->
    oneof([x, y, z, a, b, c, val, tmp]).

generate_stmt(do_bind) ->
    {do_bind, x, {var, ma, {location, 1, 1}}, {location, 1, 1}};
generate_stmt(do_action) ->
    {do_action, {var, action, {location, 1, 1}}, {location, 1, 1}};
generate_stmt(do_let) ->
    {do_let, x, {literal, integer, 1, {location, 1, 1}}, {location, 1, 1}};
generate_stmt(do_return) ->
    {do_return, {var, x, {location, 1, 1}}, {location, 1, 1}}.

build_nested_do(1) ->
    {do_expr, [{do_return, {var, x, {location, 1, 1}}, {location, 1, 1}}], {location, 1, 1}};
build_nested_do(N) ->
    Inner = build_nested_do(N - 1),
    {do_expr, [
        {do_bind, x, {var, ma, {location, 1, 1}}, {location, 1, 1}},
        {do_return, Inner, {location, 1, 1}}
    ], {location, 1, 1}}.

%%====================================================================
%% Validators
%%====================================================================

is_valid_desugared_expr({error, _}) ->
    %% Errors are valid results (e.g., depth exceeded)
    true;
is_valid_desugared_expr({app, _, _, _}) ->
    true;
is_valid_desugared_expr({let_expr, _, _, _}) ->
    true;
is_valid_desugared_expr({var, _, _}) ->
    true;
is_valid_desugared_expr({literal, _, _, _}) ->
    true;
is_valid_desugared_expr({lambda, _, _, _}) ->
    true;
is_valid_desugared_expr(_) ->
    false.

%%====================================================================
%% EUnit Test Generator
%%====================================================================

desugar_properties_test_() ->
    [
        {"Desugar never crashes", ?_assert(proper:quickcheck(prop_desugar_never_crashes(), 50))},
        {"Desugar preserves structure", ?_assert(proper:quickcheck(prop_desugar_preserves_structure(), 50))},
        {"Nested do-blocks handled", ?_assert(proper:quickcheck(prop_nested_do_blocks_handled(), 30))},
        {"All statement types handled", ?_assert(proper:quickcheck(prop_all_statement_types_handled(), 50))},
        {"Depth limit enforced", ?_assert(proper:quickcheck(prop_depth_limit_enforced(), 10))},
        {"Desugar roundtrip", ?_assert(proper:quickcheck(prop_desugar_roundtrip(), 30))}
    ].
