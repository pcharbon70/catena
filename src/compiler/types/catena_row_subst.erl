-module(catena_row_subst).

-compile({no_auto_import, [apply/2]}).

%% Row substitutions for effect-row unification.

-export([
    empty/0,
    from_list/1,
    to_list/1,
    apply/2,
    compose/2
]).

-type row_subst() :: #{catena_row_types:row_var_id() => catena_row_types:effect_row()}.

-export_type([row_subst/0]).

-spec empty() -> row_subst().
empty() ->
    #{}.

-spec from_list([{catena_row_types:row_var_id(), catena_row_types:effect_row()}]) -> row_subst().
from_list(List) ->
    maps:from_list(List).

-spec to_list(row_subst()) -> [{catena_row_types:row_var_id(), catena_row_types:effect_row()}].
to_list(Subst) ->
    maps:to_list(Subst).

-spec apply(catena_row_types:effect_row(), row_subst()) -> catena_row_types:effect_row().
apply(#{elements := Elements, row_var := undefined}, _Subst) ->
    catena_row_types:row_normalize(catena_row_types:effect_row(Elements));
apply(#{elements := Elements, row_var := RowVar}, Subst) ->
    RowVarId = catena_row_types:row_var_id(RowVar),
    case maps:find(RowVarId, Subst) of
        {ok, Replacement} ->
            AppliedReplacement = apply(Replacement, Subst),
            catena_row_types:row_normalize(
                catena_row_types:effect_row(
                    Elements ++ maps:get(elements, AppliedReplacement),
                    maps:get(row_var, AppliedReplacement)
                )
            );
        error ->
            catena_row_types:row_normalize(catena_row_types:effect_row(Elements, RowVar))
    end.

-spec compose(row_subst(), row_subst()) -> row_subst().
compose(Left, Right) ->
    AppliedLeft = maps:map(
        fun(_RowVarId, Row) ->
            apply(Row, Right)
        end,
        Left
    ),
    maps:merge(Right, AppliedLeft).
