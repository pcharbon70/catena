-module(catena_row_unify).

%% Row unification for effect types with occurs check.

-export([
    unify_rows/2,
    unify_rows/3,
    row_occurs/2,
    apply_row_subst/2,
    compose_row_subst/2,
    generate_row_constraints/2,
    solve_row_constraints/1,
    row_subst_to_list/1,
    list_to_row_subst/1,
    empty_row_subst/0
]).

-export_type([
    row_subst/0,
    row_constraint/0,
    unify_result/0
]).

-type row_subst() :: catena_row_subst:row_subst().

-type row_constraint() ::
    {unify, catena_row_types:effect_row(), catena_row_types:effect_row()}.

-type unify_result() ::
    {ok, row_subst()} |
    {error, term()}.

-spec unify_rows(catena_row_types:effect_row(), catena_row_types:effect_row()) -> unify_result().
unify_rows(Row1, Row2) ->
    unify_rows(Row1, Row2, empty_row_subst()).

-spec unify_rows(catena_row_types:effect_row(), catena_row_types:effect_row(), row_subst()) -> unify_result().
unify_rows(Row1, Row2, Subst) ->
    Normalized1 = apply_row_subst(catena_row_types:row_normalize(Row1), Subst),
    Normalized2 = apply_row_subst(catena_row_types:row_normalize(Row2), Subst),
    unify_normalized_rows(Normalized1, Normalized2, Subst).

-spec row_occurs(catena_row_types:row_var_id(), catena_row_types:effect_row()) -> boolean().
row_occurs(RowVarId, Row) ->
    row_occurs(RowVarId, Row, empty_row_subst()).

-spec apply_row_subst(catena_row_types:effect_row(), row_subst()) -> catena_row_types:effect_row().
apply_row_subst(Row, Subst) ->
    catena_row_subst:apply(Row, Subst).

-spec compose_row_subst(row_subst(), row_subst()) -> row_subst().
compose_row_subst(S1, S2) ->
    catena_row_subst:compose(S1, S2).

-spec generate_row_constraints(catena_row_types:effect_row(), catena_row_types:effect_row()) -> [row_constraint()].
generate_row_constraints(Row1, Row2) ->
    [{unify, Row1, Row2}].

-spec solve_row_constraints([row_constraint()]) -> {ok, row_subst()} | {error, term()}.
solve_row_constraints(Constraints) ->
    lists:foldl(
        fun({unify, Row1, Row2}, {ok, Subst}) ->
            unify_rows(Row1, Row2, Subst);
           (_Constraint, {error, _Reason} = Error) ->
            Error
        end,
        {ok, empty_row_subst()},
        Constraints
    ).

-spec row_subst_to_list(row_subst()) -> [{catena_row_types:row_var_id(), catena_row_types:effect_row()}].
row_subst_to_list(Subst) ->
    catena_row_subst:to_list(Subst).

-spec list_to_row_subst([{catena_row_types:row_var_id(), catena_row_types:effect_row()}]) -> row_subst().
list_to_row_subst(List) ->
    catena_row_subst:from_list(List).

-spec empty_row_subst() -> row_subst().
empty_row_subst() ->
    catena_row_subst:empty().

-spec unify_normalized_rows(catena_row_types:effect_row(), catena_row_types:effect_row(), row_subst()) -> unify_result().
unify_normalized_rows(
    #{elements := Es1, row_var := Rv1},
    #{elements := Es2, row_var := Rv2},
    Subst
) ->
    Only1 = subtract(Es1, Es2),
    Only2 = subtract(Es2, Es1),
    case {Only1, Only2, Rv1, Rv2} of
        {[], [], undefined, undefined} ->
            {ok, Subst};
        {[], [], RowVar1, RowVar2} ->
            unify_row_vars(RowVar1, RowVar2, Subst);
        {_, _, undefined, undefined} ->
            {error, {effect_mismatch, Es1, Es2}};
        {OnlyLeft, [], undefined, RowVar2} ->
            bind_row_var(RowVar2, catena_row_types:effect_row(OnlyLeft), Subst);
        {[], OnlyRight, RowVar1, undefined} ->
            bind_row_var(RowVar1, catena_row_types:effect_row(OnlyRight), Subst);
        {[], [], RowVar1, RowVar2} ->
            unify_row_vars(RowVar1, RowVar2, Subst);
        {OnlyLeft, OnlyRight, RowVar1, RowVar2} ->
            case bind_row_var(RowVar1, catena_row_types:effect_row(OnlyRight, RowVar2), Subst) of
                {ok, Subst1} when OnlyLeft =:= [] ->
                    {ok, Subst1};
                {ok, _Subst1} ->
                    {error, {ambiguous_open_row_unification, OnlyLeft, OnlyRight}};
                {error, _Reason} = Error ->
                    Error
            end
    end.

-spec unify_row_vars(catena_row_types:row_var() | undefined, catena_row_types:row_var() | undefined, row_subst()) ->
    unify_result().
unify_row_vars(undefined, undefined, Subst) ->
    {ok, Subst};
unify_row_vars(RowVar, undefined, Subst) when is_map(RowVar) ->
    bind_row_var(RowVar, catena_row_types:empty_row(), Subst);
unify_row_vars(undefined, RowVar, Subst) when is_map(RowVar) ->
    bind_row_var(RowVar, catena_row_types:empty_row(), Subst);
unify_row_vars(RowVar1, RowVar2, Subst) ->
    case catena_row_types:row_var_id(RowVar1) =:= catena_row_types:row_var_id(RowVar2) of
        true -> {ok, Subst};
        false -> bind_row_var(RowVar1, catena_row_types:effect_row([], RowVar2), Subst)
    end.

-spec bind_row_var(catena_row_types:row_var(), catena_row_types:effect_row(), row_subst()) -> unify_result().
bind_row_var(RowVar, Replacement, Subst) ->
    RowVarId = catena_row_types:row_var_id(RowVar),
    NormalizedReplacement = catena_row_types:row_normalize(Replacement),
    case row_occurs(RowVarId, NormalizedReplacement, Subst) of
        true ->
            {error, {row_occurs_check_failed, RowVarId, NormalizedReplacement}};
        false ->
            case maps:find(RowVarId, Subst) of
                {ok, Existing} ->
                    unify_rows(Existing, NormalizedReplacement, Subst);
                error ->
                    {ok, maps:put(RowVarId, NormalizedReplacement, Subst)}
            end
    end.

-spec row_occurs(catena_row_types:row_var_id(), catena_row_types:effect_row(), row_subst()) -> boolean().
row_occurs(_RowVarId, #{row_var := undefined}, _Subst) ->
    false;
row_occurs(RowVarId, #{row_var := RowVar}, Subst) ->
    NestedId = catena_row_types:row_var_id(RowVar),
    case NestedId =:= RowVarId of
        true ->
            true;
        false ->
            case maps:find(NestedId, Subst) of
                {ok, NestedRow} -> row_occurs(RowVarId, NestedRow, Subst);
                error -> false
            end
    end.

-spec subtract([atom()], [atom()]) -> [atom()].
subtract(Left, Right) ->
    [Element || Element <- Left, not lists:member(Element, Right)].
