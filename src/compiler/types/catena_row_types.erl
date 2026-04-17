-module(catena_row_types).

%% Row types for extensible effect sets.
%% Row polymorphism allows effect sets to be extended with additional effects,
%% similar to row polymorphism for records. Effect rows can contain concrete
%% effects and row variables representing unknown effect sets.

-export([
    %% Row type constructors
    empty_row/0,
    effect_row/1,
    row_var/1,
    row_var/0,
    %% Row type predicates
    is_row_var/1,
    is_effect_row/1,
    is_empty_row/1,
    %% Row type operations
    row_union/2,
    row_intersection/2,
    row_difference/2,
    row_contains/2,
    row_normalize/1,
    row_to_list/1,
    list_to_row/1,
    %% Row variable management
    fresh_row_var/1,
    fresh_row_var/0,
    row_var_id/1,
    %% Row type validation
    is_valid_row/1,
    is_valid_effect/1
]).

-export_type([
    effect_row/0,
    row_var/0,
    row_element/0,
    row_var_id/0
]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

%% @doc Row variable identifier.
-type row_var_id() :: {row_var, non_neg_integer()}.

%% @doc Row variable representing an unknown set of effects.
-type row_var() :: #{
    kind => row_var,
    id => row_var_id()
}.

%% @doc Single element in an effect row (either concrete effect or row variable).
-type row_element() :: atom() | row_var().

%% @doc Effect row type - extensible set of effects.
%% Represented as a sorted list of unique atoms and optional row variable.
-type effect_row() :: #{
    kind => effect_row,
    elements => [atom()],
    row_var => row_var() | undefined
}.

%%%---------------------------------------------------------------------
%%% Row Type Constructors
%%%---------------------------------------------------------------------

%% @doc Create an empty effect row.
-spec empty_row() -> effect_row().
empty_row() ->
    #{
        kind => effect_row,
        elements => [],
        row_var => undefined
    }.

%% @doc Create an effect row from a list of effects.
-spec effect_row([atom()]) -> effect_row().
effect_row(Effects) when is_list(Effects) ->
    Unique = lists:usort(Effects),
    #{
        kind => effect_row,
        elements => Unique,
        row_var => undefined
    }.

%% @doc Create a row variable with a specific ID.
-spec row_var(row_var_id()) -> row_var().
row_var(Id) ->
    #{
        kind => row_var,
        id => Id
    }.

%% @doc Create a fresh row variable.
-spec row_var() -> row_var().
row_var() ->
    row_var(fresh_row_var_id()).

%%%---------------------------------------------------------------------
%%% Row Type Predicates
%%%---------------------------------------------------------------------

%% @doc Check if a term is a row variable.
-spec is_row_var(term()) -> boolean().
is_row_var(#{kind := row_var}) -> true;
is_row_var(_) -> false.

%% @doc Check if a term is an effect row.
-spec is_effect_row(term()) -> boolean().
is_effect_row(#{kind := effect_row}) -> true;
is_effect_row(_) -> false.

%% @doc Check if an effect row is empty.
-spec is_empty_row(effect_row()) -> boolean().
is_empty_row(#{elements := []}) -> true;
is_empty_row(_) -> false.

%%%---------------------------------------------------------------------
%%% Row Type Operations
%%%---------------------------------------------------------------------

%% @doc Compute the union of two effect rows.
-spec row_union(effect_row(), effect_row()) -> effect_row().
row_union(#{elements := Es1, row_var := Rv1}, #{elements := Es2, row_var := Rv2}) ->
    UnionElements = lists:usort(Es1 ++ Es2),
    UnionRowVar = case {Rv1, Rv2} of
        {undefined, undefined} -> undefined;
        {undefined, _} -> Rv2;
        {_, undefined} -> Rv1;
        _ -> Rv1  %% Keep first row var if both present
    end,
    #{
        kind => effect_row,
        elements => UnionElements,
        row_var => UnionRowVar
    }.

%% @doc Compute the intersection of two effect rows.
-spec row_intersection(effect_row(), effect_row()) -> effect_row().
row_intersection(#{elements := Es1}, #{elements := Es2}) ->
    IntersectElements = lists:filter(fun(E) -> lists:member(E, Es2) end, Es1),
    #{
        kind => effect_row,
        elements => IntersectElements,
        row_var => undefined
    }.

%% @doc Compute the difference of two effect rows (Es1 - Es2).
-spec row_difference(effect_row(), effect_row()) -> effect_row().
row_difference(#{elements := Es1, row_var := Rv}, #{elements := Es2}) ->
    DiffElements = lists:filter(fun(E) -> not lists:member(E, Es2) end, Es1),
    #{
        kind => effect_row,
        elements => DiffElements,
        row_var => Rv
    }.

%% @doc Check if an effect row contains a specific effect.
-spec row_contains(effect_row(), atom()) -> boolean().
row_contains(#{elements := Es}, Effect) ->
    lists:member(Effect, Es).

%% @doc Check if an effect row contains all effects from another row.
-spec row_contains_all(effect_row(), effect_row()) -> boolean().
row_contains_all(#{elements := Es1}, #{elements := Es2}) ->
    lists:all(fun(E) -> lists:member(E, Es1) end, Es2).

%% @doc Normalize an effect row (sort and deduplicate elements).
-spec row_normalize(effect_row()) -> effect_row().
row_normalize(#{elements := Es, row_var := Rv}) ->
    #{
        kind => effect_row,
        elements => lists:usort(Es),
        row_var => Rv
    }.

%% @doc Convert an effect row to a list of elements.
-spec row_to_list(effect_row()) -> [row_element()].
row_to_list(#{elements := Es, row_var := undefined}) ->
    Es;
row_to_list(#{elements := Es, row_var := Rv}) ->
    Es ++ [Rv].

%% @doc Convert a list of elements to an effect row.
-spec list_to_row([row_element()]) -> effect_row().
list_to_row(Elements) when is_list(Elements) ->
    {Effects, Vars} = lists:partition(fun(E) -> is_atom(E) end, Elements),
    RowVar = case Vars of
        [] -> undefined;
        [Var] -> Var;
        _ -> hd(Vars)  %% Take first row var if multiple
    end,
    #{
        kind => effect_row,
        elements => lists:usort(Effects),
        row_var => RowVar
    }.

%%%---------------------------------------------------------------------
%%% Row Variable Management
%%%---------------------------------------------------------------------

%% @doc Create a fresh row variable with state counter.
-spec fresh_row_var(map()) -> {row_var(), map()}.
fresh_row_var(#{row_var_counter := Counter} = State) ->
    Id = {row_var, Counter},
    Var = row_var(Id),
    NewState = State#{row_var_counter => Counter + 1},
    {Var, NewState}.

%% @doc Create a fresh row variable without state (uses process dictionary).
-spec fresh_row_var() -> row_var().
fresh_row_var() ->
    Counter = case get(row_var_counter) of
        undefined -> 0;
        N -> N
    end,
    put(row_var_counter, Counter + 1),
    row_var({row_var, Counter}).

%% @doc Get the ID of a row variable.
-spec row_var_id(row_var()) -> row_var_id().
row_var_id(#{id := Id}) ->
    Id.

%% @doc Internal: Generate a fresh row variable ID.
-spec fresh_row_var_id() -> row_var_id().
fresh_row_var_id() ->
    {row_var, erlang:unique_integer([positive, monotonic])}.

%%%---------------------------------------------------------------------
%%% Row Type Validation
%%%---------------------------------------------------------------------

%% @doc Validate a term as a proper effect row.
-spec is_valid_row(term()) -> boolean().
is_valid_row(#{kind := effect_row, elements := Es, row_var := Rv}) when is_list(Es) ->
    lists:all(fun is_atom/1, Es) andalso (Rv =:= undefined orelse is_row_var(Rv));
is_valid_row(_) ->
    false.

%% @doc Validate a term as a valid effect name.
-spec is_valid_effect(term()) -> boolean().
is_valid_effect(Effect) when is_atom(Effect) ->
    case atom_to_list(Effect) of
        [] -> false;
        [C | _] when C >= $A, C =< $Z -> true;  %% Uppercase start
        _ -> false
    end;
is_valid_effect(_) ->
    false.
