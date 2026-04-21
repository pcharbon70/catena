%%%-------------------------------------------------------------------
%%% @doc Row-polymorphism helpers for effect rows.
%%%
%%% This module bridges the existing type-scheme machinery with the newer
%%% effect-row representation used in the algebraic-effects track.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_row_poly_integration).

-export([
    add_row_vars_to_scheme/2,
    get_row_vars_from_scheme/1,
    scheme_has_row_vars/1,
    generalize_row_vars/2
]).

-export([
    row_poly_scheme/3,
    instantiate_row_poly_scheme/2,
    extend_with_row_var/2
]).

-export([
    infer_with_row_poly/3,
    generalize_with_row_poly/2,
    instantiate_with_row_poly/2
]).

-export([
    row_constraint/2,
    row_constraints_to_list/1,
    merge_row_constraints/2,
    satisfy_row_constraints/2
]).

-type expr() :: term().
-type env() :: catena_type_env:env().
-type type_var_id() :: catena_types:type_var_id().
-type row_var_id() :: catena_row_types:row_var_id().
-type type_scheme() :: catena_type_scheme:scheme().
-type ty() :: catena_types:ty().
-type effect_row() :: catena_row_types:effect_row().
-type row_var() :: catena_row_types:row_var().
-type constraint_set() :: catena_constraint:constraint_set().

-type row_poly_scheme() ::
    {poly, [type_var_id()], [row_var_id()], ty()} |
    {poly, [type_var_id()], [row_var_id()], ty(), constraint_set()}.

-type row_constraint() ::
    {row_constraint, row_var_id(), effect_row()}.

%%====================================================================
%% Scheme Helpers
%%====================================================================

-spec add_row_vars_to_scheme(type_scheme() | row_poly_scheme(), [row_var_id()]) -> row_poly_scheme().
add_row_vars_to_scheme({mono, Type}, RowVars) ->
    {poly, [], lists:usort(RowVars), Type};
add_row_vars_to_scheme({mono, Type, Constraints}, RowVars) ->
    {poly, [], lists:usort(RowVars), Type, Constraints};
add_row_vars_to_scheme({poly, TyVars, Type}, RowVars) ->
    {poly, TyVars, lists:usort(RowVars), Type};
add_row_vars_to_scheme({poly, TyVars, Constraints, Type}, RowVars) when is_list(Constraints) ->
    {poly, TyVars, lists:usort(RowVars), Type, Constraints};
add_row_vars_to_scheme({poly, TyVars, ExistingRows, Type}, RowVars) ->
    {poly, TyVars, lists:usort(ExistingRows ++ RowVars), Type};
add_row_vars_to_scheme({poly, TyVars, ExistingRows, Type, Constraints}, RowVars) ->
    {poly, TyVars, lists:usort(ExistingRows ++ RowVars), Type, Constraints}.

-spec get_row_vars_from_scheme(type_scheme() | row_poly_scheme()) -> [row_var_id()].
get_row_vars_from_scheme({poly, _TyVars, MaybeRows, _Type}) ->
    case is_row_var_ids(MaybeRows) of
        true -> MaybeRows;
        false -> []
    end;
get_row_vars_from_scheme({poly, _TyVars, MaybeRows, _Type, _Constraints}) ->
    case is_row_var_ids(MaybeRows) of
        true -> MaybeRows;
        false -> []
    end;
get_row_vars_from_scheme(_Other) ->
    [].

-spec scheme_has_row_vars(type_scheme() | row_poly_scheme()) -> boolean().
scheme_has_row_vars(Scheme) ->
    get_row_vars_from_scheme(Scheme) =/= [].

-spec generalize_row_vars(ty(), [row_var_id()]) -> row_poly_scheme().
generalize_row_vars(Type, RowVars) ->
    {poly, [], lists:usort(RowVars), Type}.

-spec row_poly_scheme([type_var_id()], [row_var_id()], ty()) -> row_poly_scheme().
row_poly_scheme(TyVars, RowVars, Type) ->
    {poly, TyVars, lists:usort(RowVars), Type}.

-spec instantiate_row_poly_scheme(row_poly_scheme(), catena_infer_state:infer_state()) ->
    {ty(), catena_infer_state:infer_state()}.
instantiate_row_poly_scheme({poly, TyVars, RowVars, Type}, State) ->
    {TypeInst1, State1} = instantiate_type_vars(TyVars, Type, State),
    {TypeInst2, State2} = instantiate_row_vars(RowVars, TypeInst1, State1),
    {TypeInst2, State2};
instantiate_row_poly_scheme({poly, TyVars, RowVars, Type, _Constraints}, State) ->
    instantiate_row_poly_scheme({poly, TyVars, RowVars, Type}, State).

-spec extend_with_row_var(type_scheme() | row_poly_scheme(), catena_infer_state:infer_state()) ->
    {row_poly_scheme(), catena_infer_state:infer_state()}.
extend_with_row_var(Scheme, State) ->
    {FreshId, NewState} = catena_infer_state:fresh_var_id(State),
    RowVarId = {row_var, FreshId},
    {add_row_vars_to_scheme(Scheme, [RowVarId]), NewState}.

%%====================================================================
%% Inference Integration
%%====================================================================

-spec infer_with_row_poly(expr(), env(), catena_infer_state:infer_state()) ->
    {ty(), catena_infer_state:infer_state()}.
infer_with_row_poly(Expr, Env, State) ->
    case catena_infer:infer_expr(Expr, Env) of
        {ok, Type} ->
            {Type, State};
        {error, _Errors} ->
            {catena_types:tcon(error), State}
    end.

-spec generalize_with_row_poly(ty(), catena_infer_state:infer_state()) ->
    {type_scheme() | row_poly_scheme(), catena_infer_state:infer_state()}.
generalize_with_row_poly(Type, State) ->
    RowVars = extract_row_vars(Type),
    case RowVars of
        [] ->
            {catena_type_scheme:mono(Type), State};
        _ ->
            {{poly, [], RowVars, Type}, State}
    end.

-spec instantiate_with_row_poly(type_scheme() | row_poly_scheme(), catena_infer_state:infer_state()) ->
    {ty(), catena_infer_state:infer_state()}.
instantiate_with_row_poly({mono, Type}, State) ->
    {Type, State};
instantiate_with_row_poly({mono, Type, _Constraints}, State) ->
    {Type, State};
instantiate_with_row_poly({poly, TyVars, Type}, State) ->
    {TypeInst, _Constraints, State1} = catena_type_scheme:instantiate({poly, TyVars, Type}, State),
    {TypeInst, State1};
instantiate_with_row_poly({poly, TyVars, MaybeRows, Type}, State) ->
    case is_row_var_ids(MaybeRows) of
        true ->
            instantiate_row_poly_scheme({poly, TyVars, MaybeRows, Type}, State);
        false ->
            {TypeInst, _Constraints, State1} =
                catena_type_scheme:instantiate({poly, TyVars, MaybeRows, Type}, State),
            {TypeInst, State1}
    end;
instantiate_with_row_poly({poly, TyVars, RowVars, Type, _Constraints}, State) ->
    instantiate_row_poly_scheme({poly, TyVars, RowVars, Type}, State).

%%====================================================================
%% Row Constraint Helpers
%%====================================================================

-spec row_constraint(row_var_id(), [atom()]) -> row_constraint().
row_constraint(RowVarId, RequiredEffects) ->
    {row_constraint, RowVarId, catena_row_types:effect_row(RequiredEffects)}.

-spec row_constraints_to_list([row_constraint()]) -> [{row_var_id(), [atom()]}].
row_constraints_to_list(Constraints) ->
    lists:map(
        fun({row_constraint, RowVarId, EffectRow}) ->
            {RowVarId, catena_row_types:row_to_list(EffectRow)}
        end,
        Constraints
    ).

-spec merge_row_constraints([row_constraint()], [row_constraint()]) -> [row_constraint()].
merge_row_constraints(Constraints1, Constraints2) ->
    lists:foldl(
        fun({row_constraint, RowVarId, EffectRow} = Constraint, Acc) ->
            case lists:keyfind(RowVarId, 2, Acc) of
                false ->
                    [Constraint | Acc];
                {row_constraint, RowVarId, ExistingRow} ->
                    MergedRow = catena_row_types:row_union(ExistingRow, EffectRow),
                    [{row_constraint, RowVarId, MergedRow} |
                        lists:keydelete(RowVarId, 2, Acc)]
            end
        end,
        [],
        Constraints1 ++ Constraints2
    ).

-spec satisfy_row_constraints([row_constraint()], effect_row()) -> boolean().
satisfy_row_constraints(Constraints, EffectRow) ->
    lists:all(
        fun({row_constraint, _RowVarId, RequiredRow}) ->
            catena_row_types:row_contains_all(EffectRow, RequiredRow)
        end,
        Constraints
    ).

%%====================================================================
%% Internal Helpers
%%====================================================================

-spec extract_row_vars(ty()) -> [row_var_id()].
extract_row_vars({tvar, _}) ->
    [];
extract_row_vars({tcon, _}) ->
    [];
extract_row_vars({tapp, Constructor, Args}) ->
    lists:usort(extract_row_vars(Constructor) ++ lists:append([extract_row_vars(Arg) || Arg <- Args]));
extract_row_vars({tfun, From, To, Effects}) ->
    lists:usort(
        extract_row_vars(From) ++
        extract_row_vars(To) ++
        extract_row_vars_from_effects(Effects)
    );
extract_row_vars({trecord, Fields, _RowVar}) ->
    lists:usort(lists:append([extract_row_vars(FieldType) || {_Field, FieldType} <- Fields]));
extract_row_vars({ttuple, Elements}) ->
    lists:usort(lists:append([extract_row_vars(Element) || Element <- Elements]));
extract_row_vars({tvariant, Constructors}) ->
    lists:usort(
        lists:append(
            [lists:append([extract_row_vars(ArgType) || ArgType <- ArgTypes]) ||
                {_Name, ArgTypes} <- Constructors]
        )
    );
extract_row_vars(_) ->
    [].

-spec instantiate_type_vars([type_var_id()], ty(), catena_infer_state:infer_state()) ->
    {ty(), catena_infer_state:infer_state()}.
instantiate_type_vars([], Type, State) ->
    {Type, State};
instantiate_type_vars(TyVars, Type, State) ->
    {Subst, NewState} = lists:foldl(
        fun(TyVar, {AccSubst, AccState}) ->
            {FreshVar, NextState} = catena_types:fresh_var(AccState),
            {catena_type_subst:extend(AccSubst, TyVar, FreshVar), NextState}
        end,
        {catena_type_subst:empty(), State},
        TyVars
    ),
    {catena_type_subst:apply(Subst, Type), NewState}.

-spec instantiate_row_vars([row_var_id()], ty(), catena_infer_state:infer_state()) ->
    {ty(), catena_infer_state:infer_state()}.
instantiate_row_vars([], Type, State) ->
    {Type, State};
instantiate_row_vars(RowVars, Type, State) ->
    {Subst, NewState} = lists:foldl(
        fun(RowVarId, {AccSubst, AccState}) ->
            {FreshId, NextState} = catena_infer_state:fresh_var_id(AccState),
            FreshRowVar = catena_row_types:row_var({row_var, FreshId}),
            {maps:put(RowVarId, FreshRowVar, AccSubst), NextState}
        end,
        {#{}, State},
        RowVars
    ),
    {apply_row_subst(Subst, Type), NewState}.

-spec apply_row_subst(#{row_var_id() => row_var()}, ty()) -> ty().
apply_row_subst(_Subst, {tcon, _} = Type) ->
    Type;
apply_row_subst(_Subst, {tvar, _} = Type) ->
    Type;
apply_row_subst(Subst, {tapp, Constructor, Args}) ->
    {tapp, apply_row_subst(Subst, Constructor), [apply_row_subst(Subst, Arg) || Arg <- Args]};
apply_row_subst(Subst, {tfun, From, To, Effects}) ->
    {tfun,
        apply_row_subst(Subst, From),
        apply_row_subst(Subst, To),
        apply_row_subst_to_effects(Subst, Effects)};
apply_row_subst(Subst, {trecord, Fields, RowVar}) ->
    {trecord,
        [{Name, apply_row_subst(Subst, FieldType)} || {Name, FieldType} <- Fields],
        RowVar};
apply_row_subst(Subst, {ttuple, Elements}) ->
    {ttuple, [apply_row_subst(Subst, Element) || Element <- Elements]};
apply_row_subst(Subst, {tvariant, Constructors}) ->
    {tvariant,
        [{Name, [apply_row_subst(Subst, ArgType) || ArgType <- ArgTypes]} ||
            {Name, ArgTypes} <- Constructors]};
apply_row_subst(_Subst, Type) ->
    Type.

-spec apply_row_subst_to_effects(#{row_var_id() => row_var()}, term()) -> term().
apply_row_subst_to_effects(Subst, {effect_set, Effects}) ->
    {effect_set,
        [
            case catena_row_types:is_row_var(Effect) of
                true ->
                    RowVarId = catena_row_types:row_var_id(Effect),
                    maps:get(RowVarId, Subst, Effect);
                false ->
                    Effect
            end
         || Effect <- Effects
        ]};
apply_row_subst_to_effects(_Subst, Effects) ->
    Effects.

-spec extract_row_vars_from_effects(term()) -> [row_var_id()].
extract_row_vars_from_effects({effect_set, Effects}) ->
    lists:usort(
        lists:filtermap(
            fun(Effect) ->
                case catena_row_types:is_row_var(Effect) of
                    true -> {true, catena_row_types:row_var_id(Effect)};
                    false -> false
                end
            end,
            Effects
        )
    );
extract_row_vars_from_effects(_) ->
    [].

-spec is_row_var_ids(term()) -> boolean().
is_row_var_ids([]) ->
    false;
is_row_var_ids(RowVars) when is_list(RowVars) ->
    lists:all(fun({row_var, _}) -> true; (_) -> false end, RowVars);
is_row_var_ids(_) ->
    false.
