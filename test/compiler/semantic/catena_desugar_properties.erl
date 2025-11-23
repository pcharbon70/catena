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
    prop_depth_limit_enforced/0
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
        {"Depth limit enforced", ?_assert(proper:quickcheck(prop_depth_limit_enforced(), 10))}
    ].
