-module(catena_row_inference).

%% Row polymorphic type inference for effects.
%% Extends the type inference system to handle row polymorphic effects,
%% including row variable generalization and instantiation.

-export([
    %% Row variable generalization
    generalize_row_vars/2,
    generalize_row_vars/3,
    %% Row variable instantiation
    instantiate_row_vars/2,
    instantiate_row_vars/3,
    %% Row polymorphic function types
    infer_row_poly_function/2,
    infer_row_poly_operation/2,
    infer_row_poly_handler/2,
    %% Row constraint propagation
    propagate_row_constraints/2,
    %% Row polymorphic schemes
    row_poly_scheme/2,
    row_poly_scheme_instantiate/2
]).

-export_type([
    row_poly_type/0,
    row_poly_scheme/0,
    inference_state/0
]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

%% @doc Row polymorphic type.
-type row_poly_type() :: #{
    kind => row_poly,
    effects => catena_row_types:effect_row(),
    row_vars => [catena_row_types:row_var_id()]
}.

%% @doc Row polymorphic type scheme (like type schemes for HM).
-type row_poly_scheme() :: #{
    kind => row_scheme,
    type => row_poly_type(),
    row_vars => [catena_row_types:row_var_id()]
}.

%% @doc Inference state for row polymorphism.
-type inference_state() :: #{
    row_var_counter => non_neg_integer(),
    constraints => [catena_row_unify:row_constraint()],
    substitutions => catena_row_unify:row_subst()
}.

%%%---------------------------------------------------------------------
%%% Row Variable Generalization
%%%---------------------------------------------------------------------

%% @doc Generalize free row variables in a type.
-spec generalize_row_vars(row_poly_type(), inference_state()) -> {row_poly_scheme(), inference_state()}.
generalize_row_vars(#{effects := Effects, row_vars := RowVars}, State) ->
    FreeRowVars = lists:usort(RowVars ++ row_var_ids_from_effect_row(Effects)),
    GeneralizedType = #{
        kind => row_poly,
        effects => Effects,
        row_vars => FreeRowVars
    },
    Scheme = #{
        kind => row_scheme,
        type => GeneralizedType,
        row_vars => FreeRowVars
    },
    {Scheme, State}.

%% @doc Generalize with a specific row variable set.
-spec generalize_row_vars(row_poly_type(), [catena_row_types:row_var_id()], inference_state()) -> {row_poly_scheme(), inference_state()}.
generalize_row_vars(Type, ExplicitVars, State) ->
    ExplicitType = Type#{
        row_vars => lists:usort(ExplicitVars ++ row_var_ids_from_effect_row(maps:get(effects, Type)))
    },
    Scheme = #{
        kind => row_scheme,
        type => ExplicitType,
        row_vars => ExplicitVars
    },
    {Scheme, State}.

%%%---------------------------------------------------------------------
%%% Row Variable Instantiation
%%%---------------------------------------------------------------------

%% @doc Instantiate row variables in a scheme with fresh variables.
-spec instantiate_row_vars(row_poly_scheme(), inference_state()) -> {row_poly_type(), inference_state()}.
instantiate_row_vars(#{type := Type, row_vars := RowVarIds}, State) ->
    {Subst, NewState} = lists:foldl(
        fun(RowVarId, {AccSubst, AccState}) ->
            {FreshVar, NextState} = catena_row_types:fresh_row_var(AccState),
            FreshRow = catena_row_types:effect_row([], FreshVar),
            {maps:put(RowVarId, FreshRow, AccSubst), NextState}
        end,
        {#{}, State},
        RowVarIds
    ),
    InstanceType = apply_row_subst_to_type(Type, Subst),
    {InstanceType, NewState}.

%% @doc Instantiate with specific row variables.
-spec instantiate_row_vars(row_poly_scheme(), [catena_row_types:row_var()], inference_state()) -> {row_poly_type(), inference_state()}.
instantiate_row_vars(#{type := Type, row_vars := RowVarIds}, Vars, State) ->
    Subst = maps:from_list(
        lists:zip(
            RowVarIds,
            [catena_row_types:effect_row([], Var) || Var <- Vars]
        )
    ),
    InstanceType = apply_row_subst_to_type(Type, Subst),
    {InstanceType, State}.

%%%---------------------------------------------------------------------
%%% Row Polymorphic Function Types
%%%---------------------------------------------------------------------

%% @doc Infer row polymorphic type for a function.
-spec infer_row_poly_function(map(), inference_state()) -> {row_poly_type(), inference_state()}.
infer_row_poly_function(#{effects := EffectList}, State) when is_list(EffectList) ->
    Effects = catena_row_types:effect_row(EffectList),
    Type = #{
        kind => row_poly,
        effects => Effects,
        row_vars => row_var_ids_from_effect_row(Effects)
    },
    {Type, State};
infer_row_poly_function(#{effects := Effects}, State) when is_map(Effects) ->
    EffectRow = effect_row_from_term(Effects),
    Type = #{
        kind => row_poly,
        effects => EffectRow,
        row_vars => row_var_ids_from_effect_row(EffectRow)
    },
    {Type, State}.

%% @doc Infer row polymorphic type for an effect operation.
-spec infer_row_poly_operation(atom(), inference_state()) -> {row_poly_type(), inference_state()}.
infer_row_poly_operation(Operation, State) ->
    {Var, NextState} = catena_row_types:fresh_row_var(State),
    Effects = catena_row_types:effect_row([Operation]),
    EffectsWithVar = Effects#{row_var => Var},
    Type = #{
        kind => row_poly,
        effects => EffectsWithVar,
        row_vars => [catena_row_types:row_var_id(Var)]
    },
    {Type, NextState}.

%% @doc Infer row polymorphic type for a handler.
-spec infer_row_poly_handler(map(), inference_state()) -> {row_poly_type(), inference_state()}.
infer_row_poly_handler(HandlerInfo, State) ->
    case HandlerInfo of
        #{handled := Handled, remaining := Remaining, body_effects := BodyEffects} ->
            HandledEffects = effect_row_from_term(Handled),
            RemainingEffects = effect_row_from_term(Remaining),
            BodyRow = effect_row_from_term(BodyEffects),
            RemainingBodyEffects = catena_row_operations:effect_difference(BodyRow, HandledEffects),
            Combined = catena_row_operations:effect_union_rows(RemainingBodyEffects, RemainingEffects),
            Type = #{
                kind => row_poly,
                effects => Combined,
                row_vars => row_var_ids_from_effect_row(Combined)
            },
            {Type, State};
        #{handled := _Handled, remaining := Remaining} ->
            RemainingEffects = effect_row_from_term(Remaining),
            Type = #{
                kind => row_poly,
                effects => RemainingEffects,
                row_vars => row_var_ids_from_effect_row(RemainingEffects)
            },
            {Type, State};
        #{handled := _Handled} ->
            Effects = catena_row_types:empty_row(),
            Type = #{
                kind => row_poly,
                effects => Effects,
                row_vars => row_var_ids_from_effect_row(Effects)
            },
            {Type, State}
    end.

%%%---------------------------------------------------------------------
%%% Row Constraint Propagation
%%%---------------------------------------------------------------------

%% @doc Propagate row constraints through the type.
-spec propagate_row_constraints(row_poly_type(), inference_state()) -> {ok, inference_state()} | {error, term()}.
propagate_row_constraints(#{effects := Effects}, State) ->
    Constraints = catena_row_constraints:merge_constraints(
        maps:get(constraints, State, []),
        catena_row_constraints:generate_constraints(Effects, Effects)
    ),
    case catena_row_constraints:solve_constraints(Constraints) of
        {ok, Subst} ->
            ExistingSubst = maps:get(substitutions, State, catena_row_unify:empty_row_subst()),
            ComposedSubst = catena_row_unify:compose_row_subst(ExistingSubst, Subst),
            {ok, State#{constraints => Constraints, substitutions => ComposedSubst}};
        {error, Reason} ->
            {error, Reason}
    end.

%%%---------------------------------------------------------------------
%%% Row Polymorphic Schemes
%%%---------------------------------------------------------------------

%% @doc Create a row polymorphic scheme from a type and state.
-spec row_poly_scheme(row_poly_type(), inference_state()) -> row_poly_scheme().
row_poly_scheme(Type, _State) ->
    RowVars = lists:usort(
        maps:get(row_vars, Type, []) ++ row_var_ids_from_effect_row(maps:get(effects, Type))
    ),
    Scheme = #{
        kind => row_scheme,
        type => Type#{row_vars => RowVars},
        row_vars => RowVars
    },
    Scheme.

%% @doc Instantiate a row polymorphic scheme.
-spec row_poly_scheme_instantiate(row_poly_scheme(), inference_state()) -> {row_poly_type(), inference_state()}.
row_poly_scheme_instantiate(Scheme, State) ->
    instantiate_row_vars(Scheme, State).

%%%---------------------------------------------------------------------
%%% Internal Helpers
%%%---------------------------------------------------------------------

%% @doc Apply row substitution to a type.
-spec apply_row_subst_to_type(row_poly_type(), catena_row_unify:row_subst()) -> row_poly_type().
apply_row_subst_to_type(#{effects := Effects}, Subst) ->
    NewEffects = catena_row_unify:apply_row_subst(Effects, Subst),
    #{
        kind => row_poly,
        effects => NewEffects,
        row_vars => row_var_ids_from_effect_row(NewEffects)
    }.

-spec row_var_ids_from_effect_row(catena_row_types:effect_row()) -> [catena_row_types:row_var_id()].
row_var_ids_from_effect_row(#{row_var := undefined}) ->
    [];
row_var_ids_from_effect_row(#{row_var := RowVar}) ->
    [catena_row_types:row_var_id(RowVar)].

-spec effect_row_from_term(term()) -> catena_row_types:effect_row().
effect_row_from_term(Effects) when is_list(Effects) ->
    catena_row_types:effect_row(Effects);
effect_row_from_term(#{kind := effect_row} = Effects) ->
    catena_row_types:row_normalize(Effects).
