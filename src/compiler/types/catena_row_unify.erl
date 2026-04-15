-module(catena_row_unify).

%% Row unification for effect types with occurs check.
%% Unification determines if two effect rows can be made equal
%% through substitution of row variables.

-export([
    %% Row unification
    unify_rows/2,
    unify_rows/3,
    %% Row occurs check
    row_occurs/2,
    %% Row substitution
    apply_row_subst/2,
    compose_row_subst/2,
    %% Row constraint operations
    generate_row_constraints/2,
    solve_row_constraints/1,
    %% Utilities
    row_subst_to_list/1,
    list_to_row_subst/1,
    empty_row_subst/0
]).

-export_type([
    row_subst/0,
    row_constraint/0,
    unify_result/0
]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

%% @doc Row substitution mapping row variables to effect rows.
-type row_subst() :: #{catena_row_types:row_var_id() => catena_row_types:effect_row()}.

%% @doc Row constraint for unification.
-type row_constraint() ::
    {unify, catena_row_types:effect_row(), catena_row_types:effect_row()}.

%% @doc Result of row unification.
-type unify_result() ::
    {ok, row_subst()} |
    {error, term()}.

%%%---------------------------------------------------------------------
%%% Row Unification
%%%---------------------------------------------------------------------

%% @doc Unify two effect rows, producing a substitution.
-spec unify_rows(catena_row_types:effect_row(), catena_row_types:effect_row()) -> unify_result().
unify_rows(Row1, Row2) ->
    unify_rows(Row1, Row2, #{}).

%% @doc Unify two effect rows with an existing substitution.
-spec unify_rows(catena_row_types:effect_row(), catena_row_types:effect_row(), row_subst()) -> unify_result().
unify_rows(#{elements := Es1, row_var := Rv1}, #{elements := Es2, row_var := Rv2}, Subst) ->
    case unify_elements(Es1, Es2, Subst) of
        {ok, Subst1} ->
            unify_row_vars(Rv1, Rv2, Subst1);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Unify lists of effect elements.
-spec unify_elements([atom()], [atom()], row_subst()) -> unify_result().
unify_elements(Es1, Es2, Subst) when length(Es1) =:= length(Es2) ->
    Sorted1 = lists:sort(Es1),
    Sorted2 = lists:sort(Es2),
    case Sorted1 =:= Sorted2 of
        true -> {ok, Subst};
        false -> {error, {effect_mismatch, Sorted1, Sorted2}}
    end;
unify_elements(Es1, Es2, _Subst) ->
    {error, {effect_count_mismatch, length(Es1), length(Es2)}}.

%% @doc Unify row variables.
-spec unify_row_vars(catena_row_types:row_var() | undefined, catena_row_types:row_var() | undefined, row_subst()) -> unify_result().
unify_row_vars(undefined, undefined, Subst) ->
    {ok, Subst};
unify_row_vars(Var, undefined, Subst) when is_map(Var) ->
    %% Row variable can match empty row
    {ok, Subst};
unify_row_vars(undefined, Var, Subst) when is_map(Var) ->
    {ok, Subst};
unify_row_vars(Var1, Var2, Subst) when is_map(Var1), is_map(Var2) ->
    %% Both are row variables - unify them
    Id1 = catena_row_types:row_var_id(Var1),
    Id2 = catena_row_types:row_var_id(Var2),
    case Id1 =:= Id2 of
        true -> {ok, Subst};
        false ->
            %% Create unification constraint
            Subst#{Id1 => catena_row_types:effect_row([], Var2)}
    end.

%%%---------------------------------------------------------------------
%%% Row Occurs Check
%%%---------------------------------------------------------------------

%% @doc Check if a row variable occurs in an effect row.
%% Prevents infinite loops in unification.
-spec row_occurs(catena_row_types:row_var_id(), catena_row_types:effect_row()) -> boolean().
row_occurs(VarId, #{row_var := #{id := VarId}}) ->
    true;
row_occurs(VarId, #{row_var := #{id := OtherId}}) when VarId =:= OtherId ->
    true;
row_occurs(VarId, #{row_var := RowVar}) when is_map(RowVar) ->
    %% Recursively check row variable's definition
    case maps:get(VarId, undefined, undefined) of
        undefined -> false;
        Row -> row_occurs(VarId, Row)
    end;
row_occurs(_, _) ->
    false.

%%%---------------------------------------------------------------------
%%% Row Substitution
%%%---------------------------------------------------------------------

%% @doc Apply a substitution to an effect row.
-spec apply_row_subst(catena_row_types:effect_row(), row_subst()) -> catena_row_types:effect_row().
apply_row_subst(#{elements := Es, row_var := undefined}, _Subst) ->
    catena_row_types:effect_row(Es);
apply_row_subst(#{elements := Es, row_var := Var}, Subst) when is_map(Var) ->
    VarId = catena_row_types:row_var_id(Var),
    case maps:find(VarId, Subst) of
        {ok, Replacement} ->
            Replacement;
        error ->
            catena_row_types:effect_row(Es, Var)
    end.

%% @doc Compose two substitutions.
%% Result of compose(S1, S2) applies S1 then S2.
-spec compose_row_subst(row_subst(), row_subst()) -> row_subst().
compose_row_subst(S1, S2) ->
    maps:map(fun(_Key, Row) ->
        apply_row_subst(Row, S2)
    end, S1).

%%%---------------------------------------------------------------------
%%% Row Constraint Operations
%%%---------------------------------------------------------------------

%% @doc Generate unification constraints for two effect rows.
-spec generate_row_constraints(catena_row_types:effect_row(), catena_row_types:effect_row()) -> [row_constraint()].
generate_row_constraints(Row1, Row2) ->
    [{unify, Row1, Row2}].

%% @doc Solve row constraints to produce a substitution.
-spec solve_row_constraints([row_constraint()]) -> {ok, row_subst()} | {error, term()}.
solve_row_constraints(Constraints) ->
    solve_constraints(Constraints, #{}, []).

%% @doc Internal constraint solver.
-spec solve_constraints([row_constraint()], row_subst(), [row_constraint()]) -> {ok, row_subst()} | {error, term()}.
solve_constraints([], Subst, []) ->
    {ok, Subst};
solve_constraints([], Subst, Pending) ->
    %% Try to solve pending constraints
    solve_constraints(lists:reverse(Pending), Subst, []);
solve_constraints([{unify, Row1, Row2} | Rest], Subst, Pending) ->
    case unify_rows(Row1, Row2, Subst) of
        {ok, NewSubst} ->
            solve_constraints(Rest, NewSubst, Pending);
        {error, _Reason} ->
            %% Defer to pending
            solve_constraints(Rest, Subst, [{unify, Row1, Row2} | Pending])
    end.

%%%---------------------------------------------------------------------
%%% Utilities
%%%---------------------------------------------------------------------

%% @doc Convert substitution to a list for debugging.
-spec row_subst_to_list(row_subst()) -> [{catena_row_types:row_var_id(), catena_row_types:effect_row()}].
row_subst_to_list(Subst) ->
    maps:to_list(Subst).

%% @doc Create a substitution from a list.
-spec list_to_row_subst([{catena_row_types:row_var_id(), catena_row_types:effect_row()}]) -> row_subst().
list_to_row_subst(List) ->
    maps:from_list(List).

%% @doc Create an empty substitution.
-spec empty_row_subst() -> row_subst().
empty_row_subst() ->
    #{}.

%%%---------------------------------------------------------------------
%%% Internal Helpers
%%%---------------------------------------------------------------------

%% @doc Helper to create effect row with optional row variable.
-spec effect_row_with_var([atom()], catena_row_types:row_var() | undefined) -> catena_row_types:effect_row().
effect_row_with_var(Elements, undefined) ->
    catena_row_types:effect_row(Elements);
effect_row_with_var(Elements, Var) ->
    ElementsRow = catena_row_types:effect_row(Elements),
    ElementsRow#{row_var => Var}.
