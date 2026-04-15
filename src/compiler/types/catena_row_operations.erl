-module(catena_row_operations).

%% Effect set operations using row polymorphism.
%% Extends basic row operations with effect-specific semantics
%% including subsumption and normalization.

-export([
    %% Effect union with rows
    effect_union_rows/2,
    effect_union_rows/3,
    %% Effect difference with rows
    effect_difference/2,
    effect_difference/3,
    %% Effect subsumption
    effect_subsumes/2,
    effect_subsumes/3,
    %% Effect normalization
    effect_normalize/1,
    effect_normalize/2,
    %% Effect validation
    is_valid_effect_set/1,
    %% Row-aware effect operations
    extend_effect_row/2,
    restrict_effect_row/2,
    %% Effect set comparison
    effect_eq/2,
    effect_subset/2
]).

-export_type([
    effect_set/0,
    effect_op_options/0
]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

%% @doc Effect set with row polymorphism.
-type effect_set() :: catena_row_types:effect_row().

%% @doc Options for effect operations.
-type effect_op_options() :: #{
    normalize => boolean(),
    allow_row_vars => boolean(),
    strict => boolean()
}.

%%%---------------------------------------------------------------------
%%% Effect Union with Rows
%%%---------------------------------------------------------------------

%% @doc Compute the union of two effect sets preserving row variables.
-spec effect_union_rows(effect_set(), effect_set()) -> effect_set().
effect_union_rows(Row1, Row2) ->
    effect_union_rows(Row1, Row2, #{}).

%% @doc Compute union with options.
-spec effect_union_rows(effect_set(), effect_set(), effect_op_options()) -> effect_set().
effect_union_rows(#{elements := Es1, row_var := Rv1}, #{elements := Es2, row_var := Rv2}, Options) ->
    Union = catena_row_types:row_union(
        #{elements => Es1, row_var => Rv1},
        #{elements => Es2, row_var => Rv2}
    ),
    case maps:get(normalize, Options, true) of
        true -> catena_row_types:row_normalize(Union);
        false -> Union
    end.

%%%---------------------------------------------------------------------
%%% Effect Difference with Rows
%%%---------------------------------------------------------------------

%% @doc Compute the difference of two effect sets (Es1 - Es2).
-spec effect_difference(effect_set(), effect_set()) -> effect_set().
effect_difference(Row1, Row2) ->
    effect_difference(Row1, Row2, #{}).

%% @doc Compute difference with options.
-spec effect_difference(effect_set(), effect_set(), effect_op_options()) -> effect_set().
effect_difference(#{elements := Es1, row_var := Rv1}, #{elements := Es2}, Options) ->
    Diff = catena_row_types:row_difference(
        #{elements => Es1, row_var => Rv1},
        #{elements => Es2, row_var => undefined}
    ),
    case maps:get(normalize, Options, true) of
        true -> catena_row_types:row_normalize(Diff);
        false -> Diff
    end.

%%%---------------------------------------------------------------------
%%% Effect Subsumption
%%%---------------------------------------------------------------------

%% @doc Check if one effect set subsumes another.
%% Effect set A subsumes B if B's effects are all in A.
-spec effect_subsumes(effect_set(), effect_set()) -> boolean().
effect_subsumes(Row1, Row2) ->
    effect_subsumes(Row1, Row2, #{}).

%% @doc Check subsumption with options.
-spec effect_subsumes(effect_set(), effect_set(), effect_op_options()) -> boolean().
effect_subsumes(#{elements := Es1, row_var := Rv1}, #{elements := Es2, row_var := Rv2}, Options) ->
    AllowRowVars = maps:get(allow_row_vars, Options, true),

    %% Check if all elements of Es2 are in Es1
    ElementsSubsume = lists:all(fun(E) ->
        lists:member(E, Es1)
    end, Es2),

    %% Check row variable subsumption
    RowVarsSubsume = case AllowRowVars of
        true ->
            %% Row variables can represent any effects
            case {Rv1, Rv2} of
                {_, undefined} -> true;
                {undefined, _} -> true;
                _ -> true  %% Both have row vars, they subsume each other
            end;
        false ->
            %% Strict mode: both must have same row var status
            Rv1 =:= Rv2
    end,

    ElementsSubsume andalso RowVarsSubsume.

%%%---------------------------------------------------------------------
%%% Effect Normalization
%%%---------------------------------------------------------------------

%% @doc Normalize an effect set (sort, deduplicate, canonicalize).
-spec effect_normalize(effect_set()) -> effect_set().
effect_normalize(Row) ->
    effect_normalize(Row, #{}).

%% @doc Normalize with options.
-spec effect_normalize(effect_set(), effect_op_options()) -> effect_set().
effect_normalize(#{elements := Es, row_var := Rv} = Row, Options) ->
    NormalizedEs = lists:usort(Es),
    case maps:get(strict, Options, false) of
        true ->
            %% Strict mode: remove row var if elements are empty
            case NormalizedEs of
                [] -> catena_row_types:empty_row();
                _ -> Row#{elements => NormalizedEs}
            end;
        false ->
            Row#{elements => NormalizedEs}
    end.

%%%---------------------------------------------------------------------
%%% Effect Validation
%%%---------------------------------------------------------------------

%% @doc Validate an effect set.
-spec is_valid_effect_set(effect_set()) -> boolean().
is_valid_effect_set(Row) ->
    catena_row_types:is_valid_row(Row) andalso
    lists:all(fun catena_row_types:is_valid_effect/1,
        catena_row_types:row_to_list(Row)).

%%%---------------------------------------------------------------------
%%% Row-Aware Effect Operations
%%%---------------------------------------------------------------------

%% @doc Extend an effect row with additional effects.
-spec extend_effect_row(effect_set(), [atom()]) -> effect_set().
extend_effect_row(Row, Effects) ->
    NewRow = catena_row_types:effect_row(Effects),
    effect_union_rows(Row, NewRow).

%% @doc Restrict an effect row to only specified effects.
-spec restrict_effect_row(effect_set(), [atom()]) -> effect_set().
restrict_effect_row(#{elements := Es, row_var := Rv}, Allowed) ->
    Filtered = lists:filter(fun(E) -> lists:member(E, Allowed) end, Es),
    #{
        kind => effect_row,
        elements => Filtered,
        row_var => Rv
    }.

%%%---------------------------------------------------------------------
%%% Effect Set Comparison
%%%---------------------------------------------------------------------

%% @doc Check if two effect sets are equal (modulo ordering).
-spec effect_eq(effect_set(), effect_set()) -> boolean().
effect_eq(#{elements := Es1, row_var := Rv1}, #{elements := Es2, row_var := Rv2}) ->
    lists:usort(Es1) =:= lists:usort(Es2) andalso Rv1 =:= Rv2.

%% @doc Check if one effect set is a subset of another.
-spec effect_subset(effect_set(), effect_set()) -> boolean().
effect_subset(#{elements := Es1}, #{elements := Es2}) ->
    lists:all(fun(E) -> lists:member(E, Es2) end, Es1).

%%%---------------------------------------------------------------------
%%% Internal Helpers
%%%---------------------------------------------------------------------

%% @doc Internal helper to get row elements safely.
-spec get_elements(effect_set()) -> [atom()].
get_elements(#{elements := Es}) ->
    Es.

%% @doc Internal helper to get row variable safely.
-spec get_row_var(effect_set()) -> catena_row_types:row_var() | undefined.
get_row_var(#{row_var := Rv}) ->
    Rv.
