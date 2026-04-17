%%%-------------------------------------------------------------------
%%% @doc Catena Row Polymorphism Integration (Phase 14.2.2)
%%%
%%% This module integrates row polymorphism throughout the type system,
%%% enabling polymorphic effect types that can be extended with additional
%%% effects.
%%%
%%% == Row Polymorphism for Effects ==
%%%
%%% Row polymorphism allows effect sets to be extended, similar to how
%%% record rows can be extended. A function type like:
%%   forall e. a -> b / e
%%% can be instantiated with any effect set that includes the required effects.
%%%
%%% == Integration Points ==
%%%
%%% 1. Type Schemes: Extend type schemes to include row variables
%%% 2. Generalization: Generalize free row variables in let-bound expressions
%%% 3. Instantiation: Instantiate row variables with fresh row variables
%%% 4. Unification: Unify effect rows with row variable substitution
%%% 5. Constraints: Row polymorphic effect constraints
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_row_poly_integration).

%% Row variable management in type schemes
-export([
    add_row_vars_to_scheme/2,
    get_row_vars_from_scheme/1,
    scheme_has_row_vars/1,
    generalize_row_vars/2
]).

%% Row polymorphism in type schemes
-export([
    row_poly_scheme/3,
    instantiate_row_poly_scheme/2,
    extend_with_row_var/2
]).

%% Integration with type inference
-export([
    infer_with_row_poly/3,
    generalize_with_row_poly/2,
    instantiate_with_row_poly/2
]).

%% Row constraint management
-export([
    row_constraint/2,
    row_constraints_to_list/1,
    merge_row_constraints/2,
    satisfy_row_constraints/2
]).

%%====================================================================
%% Types
%%====================================================================

-type type_var_id() :: catena_types:type_var_id().
-type row_var_id() :: catena_row_types:row_var_id().
-type type_scheme() :: catena_type_scheme:scheme().
-type ty() :: catena_types:ty().
-type effect_row() :: catena_row_types:effect_row().
-type row_var() :: catena_row_types:row_var().

-type row_poly_scheme() :: {
    poly,
    [type_var_id()],  % Type variables
    [row_var_id()],   % Row variables
    ty()
}.

-type row_constraint() :: {
    row_constraint,
    row_var_id(),
    effect_row()
}.

%%====================================================================
%% Row Variable Management in Type Schemes
%%====================================================================

%% @doc Add row variables to an existing type scheme.
%%
%% Extends a type scheme to include row polymorphism for effects.
-spec add_row_vars_to_scheme(type_scheme(), [row_var_id()]) -> row_poly_scheme().
add_row_vars_to_scheme({mono, Type}, RowVars) ->
    {poly, [], RowVars, Type};
add_row_vars_to_scheme({mono, Type, Constraints}, RowVars) ->
    {poly, [], RowVars, Type, Constraints};
add_row_vars_to_scheme({poly, TyVars, Type}, RowVars) ->
    {poly, TyVars, RowVars, Type};
add_row_vars_to_scheme({poly, TyVars, Constraints, Type}, RowVars) ->
    {poly, TyVars, RowVars, Type, Constraints}.

%% @doc Extract row variables from a type scheme.
-spec get_row_vars_from_scheme(type_scheme()) -> [row_var_id()].
get_row_vars_from_scheme({poly, _TyVars, RowVars, _Type}) when is_list(RowVars) ->
    RowVars;
get_row_vars_from_scheme({poly, _TyVars, _Constraints, _Type}) ->
    [];  % Old format without row vars
get_row_vars_from_scheme(_Other) ->
    [].

%% @doc Check if a type scheme has row variables.
-spec scheme_has_row_vars(type_scheme()) -> boolean().
scheme_has_row_vars(Scheme) ->
    case get_row_vars_from_scheme(Scheme) of
        [] -> false;
        _ -> true
    end.

%% @doc Generalize free row variables in a type.
%%
%% Finds free row variables in a type and creates a polymorphic scheme.
-spec generalize_row_vars(ty(), [row_var_id()]) -> row_poly_scheme().
generalize_row_vars(Type, RowVars) when is_list(RowVars) ->
    {poly, [], RowVars, Type}.

%%====================================================================
%% Row Polymorphism in Type Schemes
%%====================================================================

%% @doc Create a row-polymorphic type scheme.
%%
%% @param TyVars Type variables to quantify
%% @param RowVars Row variables to quantify
%% @param Type The type to quantify
-spec row_poly_scheme([type_var_id()], [row_var_id()], ty()) -> row_poly_scheme().
row_poly_scheme(TyVars, RowVars, Type) ->
    {poly, TyVars, RowVars, Type}.

%% @doc Instantiate a row-polymorphic scheme with fresh variables.
%%
%% @param Scheme The row-polymorphic scheme
%% @param State The inference state (for fresh variable generation)
-spec instantiate_row_poly_scheme(row_poly_scheme(), catena_infer_state:infer_state()) ->
    {ty(), catena_infer_state:infer_state()}.
instantiate_row_poly_scheme({poly, TyVars, RowVars, Type}, State) ->
    %% Instantiate type variables
    {TypeInst1, State1} = instantiate_type_vars(TyVars, Type, State),
    %% Instantiate row variables
    {TypeInst2, State2} = instantiate_row_vars(RowVars, TypeInst1, State1),
    {TypeInst2, State2}.

%% @doc Extend a type scheme with a fresh row variable.
%%
%% Adds a new row variable to the scheme, allowing more effects to be added.
-spec extend_with_row_var(type_scheme(), catena_infer_state:infer_state()) ->
    {row_poly_scheme(), catena_infer_state:infer_state()}.
extend_with_row_var(Scheme, State) ->
    {RowVar, NewState} = catena_row_types:fresh_row_var(State),
    RowVarId = catena_row_types:row_var_id(RowVar),
    NewScheme = add_row_vars_to_scheme(Scheme, [RowVarId]),
    {NewScheme, NewState}.

%%====================================================================
%% Integration with Type Inference
%%====================================================================

%% @doc Infer types with row polymorphism enabled.
%%
%% Performs type inference while tracking and generalizing row variables.
-spec infer_with_row_poly(expr(), env(), catena_infer_state:infer_state()) ->
    {ty(), catena_infer_state:infer_state()}.
infer_with_row_poly(Expr, Env, State) ->
    %% Perform standard type inference
    {Type, State1} = catena_infer:infer_expr(Expr, Env),
    %% Extract and generalize row variables from the type
    RowVars = extract_row_vars(Type),
    case RowVars of
        [] -> {Type, State1};
        _ -> {generalize_row_vars(Type, RowVars), State1}
    end.

%% @doc Generalize a type with its free row variables.
%%
%% Creates a polymorphic scheme quantifying all free row variables.
-spec generalize_with_row_poly(ty(), catena_infer_state:infer_state()) ->
    {type_scheme(), catena_infer_state:infer_state()}.
generalize_with_row_poly(Type, State) ->
    %% Find free row variables in the type
    RowVars = extract_row_vars(Type),
    case RowVars of
        [] -> {catena_type_scheme:mono(Type), State};
        _ -> {{poly, [], RowVars, Type}, State}
    end.

%% @doc Instantiate a scheme, handling row variables specially.
%%
%% Row variables are instantiated with fresh row variables that can
%% represent any extension of the required effect set.
-spec instantiate_with_row_poly(type_scheme(), catena_infer_state:infer_state()) ->
    {ty(), catena_infer_state:infer_state()}.
instantiate_with_row_poly({mono, Type}, State) ->
    {Type, State};
instantiate_with_row_poly({mono, Type, _Constraints}, State) ->
    {Type, State};
instantiate_with_row_poly({poly, _TyVars, [], Type}, State) ->
    %% No row variables - standard instantiation
    catena_type_scheme:instantiate({poly, _TyVars, Type}, State);
instantiate_with_row_poly({poly, TyVars, RowVars, Type}, State) ->
    %% Instantiate both type and row variables
    instantiate_row_poly_scheme({poly, TyVars, RowVars, Type}, State).

%%====================================================================
%% Row Constraint Management
%%====================================================================

%% @doc Create a row constraint.
%%
%% Row constraints specify that a row variable must contain certain effects.
-spec row_constraint(row_var_id(), [atom()]) -> row_constraint().
row_constraint(RowVarId, RequiredEffects) ->
    EffectRow = catena_row_types:effect_row(RequiredEffects),
    {row_constraint, RowVarId, EffectRow}.

%% @doc Convert row constraints to a list.
-spec row_constraints_to_list([row_constraint()]) -> [{row_var_id(), [atom()]}].
row_constraints_to_list(Constraints) ->
    lists:map(fun({row_constraint, RowVarId, EffectRow}) ->
        {RowVarId, catena_row_types:row_to_list(EffectRow)}
    end, Constraints).

%% @doc Merge row constraints.
%%
%% Combines constraints for the same row variable.
-spec merge_row_constraints([row_constraint()], [row_constraint()]) -> [row_constraint()].
merge_row_constraints(Constraints1, Constraints2) ->
    All = lists:append(Constraints1, Constraints2),
    %% Group by row variable and merge
    lists:foldl(fun({row_constraint, RowVarId, EffectRow} = C, Acc) ->
        case lists:keyfind(RowVarId, 2, Acc) of
            false -> [C | Acc];
            {row_constraint, RowVarId, ExistingRow} ->
                MergedRow = catena_row_types:row_union(ExistingRow, EffectRow),
                [{row_constraint, RowVarId, MergedRow} | lists:keydelete(RowVarId, 2, Acc)]
        end
    end, [], All).

%% @doc Check if row constraints are satisfied.
%%
%% Verifies that all required effects are present in the effect row.
-spec satisfy_row_constraints([row_constraint()], effect_row()) -> boolean().
satisfy_row_constraints(Constraints, EffectRow) ->
    lists:all(fun({row_constraint, _RowVarId, RequiredRow}) ->
        catena_row_types:row_contains_all(EffectRow, RequiredRow)
    end, Constraints).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Extract row variables from a type.
-spec extract_row_vars(ty()) -> [row_var_id()].
extract_row_vars({tfun, _From, To, {effect_row, _Elements, RowVar}}) when RowVar =/= undefined ->
    [catena_row_types:row_var_id(RowVar)];
extract_row_vars({tfun, _From, To, Effects}) ->
    extract_row_vars(Effects);
extract_row_vars({effect_set, Effects}) ->
    lists:filtermap(fun(E) ->
        case catena_row_types:is_row_var(E) of
            true -> {true, catena_row_types:row_var_id(E)};
            false -> false
        end
    end, Effects);
extract_row_vars(_Type) ->
    [].

%% @doc Instantiate type variables in a type.
-spec instantiate_type_vars([type_var_id()], ty(), catena_infer_state:infer_state()) ->
    {ty(), catena_infer_state:infer_state()}.
instantiate_type_vars([], Type, State) ->
    {Type, State};
instantiate_type_vars(TyVars, Type, State) ->
    %% Create substitution mapping each type var to a fresh var
    Subst = maps:from_list(lists:map(fun(TyVar) ->
        {FreshVar, NewState} = catena_types:fresh_var(State),
        {TyVar, FreshVar}
    end, TyVars)),
    %% Apply substitution
    NewType = catena_type_subst:apply(Subst, Type),
    {NewType, State}.

%% @doc Instantiate row variables in a type.
-spec instantiate_row_vars([row_var_id()], ty(), catena_infer_state:infer_state()) ->
    {ty(), catena_infer_state:infer_state()}.
instantiate_row_vars([], Type, State) ->
    {Type, State};
instantiate_row_vars(RowVars, Type, State) ->
    %% Create fresh row variables
    {FreshRowVars, NewState} = lists:mapfoldl(fun(_RowVar, S) ->
        catena_row_types:fresh_row_var(S)
    end, State, RowVars),

    %% Build substitution
    SubstList = lists:zip(RowVars, FreshRowVars),
    Subst = maps:from_list(SubstList),

    %% Apply row substitution
    NewType = apply_row_subst(Subst, Type),
    {NewType, NewState}.

%% @doc Apply row variable substitution to a type.
-spec apply_row_subst(#{row_var_id() => row_var()}, ty()) -> ty().
apply_row_subst(_Subst, {tcon, _} = T) -> T;
apply_row_subst(_Subst, {tvar, _} = T) -> T;
apply_row_subst(Subst, {tfun, From, To, Effects}) ->
    {tfun,
        apply_row_subst(Subst, From),
        apply_row_subst(Subst, To),
        apply_row_subst_to_effects(Subst, Effects)};
apply_row_subst(Subst, Type) ->
    Type.

%% @doc Apply row substitution to effect sets.
-spec apply_row_subst_to_effects(#{row_var_id() => row_var()}, term()) -> term().
apply_row_subst_to_effects(Subst, {effect_set, Effects}) ->
    NewEffects = lists:map(fun(E) ->
        case catena_row_types:is_row_var(E) of
            true ->
                RowVarId = catena_row_types:row_var_id(E),
                case maps:find(RowVarId, Subst) of
                    {ok, NewRowVar} -> NewRowVar;
                    error -> E
                end;
            false -> E
        end
    end, Effects),
    {effect_set, NewEffects};
apply_row_subst_to_effects(_Subst, Effects) ->
    Effects.
