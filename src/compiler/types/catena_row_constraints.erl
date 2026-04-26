-module(catena_row_constraints).

%% Lightweight row-constraint generation and solving.

-export([
    generate_constraints/2,
    solve_constraints/1,
    merge_constraints/2
]).

-type row_constraint() :: catena_row_unify:row_constraint().

-export_type([row_constraint/0]).

-spec generate_constraints(catena_row_types:effect_row(), catena_row_types:effect_row()) ->
    [row_constraint()].
generate_constraints(Row1, Row2) ->
    catena_row_unify:generate_row_constraints(Row1, Row2).

-spec solve_constraints([row_constraint()]) ->
    {ok, catena_row_unify:row_subst()} | {error, term()}.
solve_constraints(Constraints) ->
    catena_row_unify:solve_row_constraints(Constraints).

-spec merge_constraints([row_constraint()], [row_constraint()]) -> [row_constraint()].
merge_constraints(Left, Right) ->
    Left ++ Right.
