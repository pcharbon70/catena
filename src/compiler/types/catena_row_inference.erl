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
    %% Find free row variables (those not in the environment)
    FreeRowVars = RowVars,
    Scheme = #{
        kind => row_scheme,
        type => #{effects => Effects, row_vars => RowVars},
        row_vars => FreeRowVars
    },
    {Scheme, State}.

%% @doc Generalize with a specific row variable set.
-spec generalize_row_vars(row_poly_type(), [catena_row_types:row_var_id()], inference_state()) -> {row_poly_scheme(), inference_state()}.
generalize_row_vars(Type, ExplicitVars, State) ->
    Scheme = #{
        kind => row_scheme,
        type => Type,
        row_vars => ExplicitVars
    },
    {Scheme, State}.

%%%---------------------------------------------------------------------
%%% Row Variable Instantiation
%%%---------------------------------------------------------------------

%% @doc Instantiate row variables in a scheme with fresh variables.
-spec instantiate_row_vars(row_poly_scheme(), inference_state()) -> {row_poly_type(), inference_state()}.
instantiate_row_vars(#{type := Type, row_vars := RowVarIds}, State) ->
    %% Create fresh row variables for each quantified variable
    {FreshVars, NewState} = lists:mapfoldl(
        fun(_Id, S) ->
            Var = catena_row_types:fresh_row_var(),
            {catena_row_types:row_var_id(Var), S}
        end,
        State,
        RowVarIds
    ),

    %% Build substitution
    Subst = maps:from_list(lists:zip(RowVarIds, FreshVars)),

    %% Apply substitution to get instance type
    InstanceType = apply_row_subst_to_type(Type, Subst),

    {InstanceType, NewState}.

%% @doc Instantiate with specific row variables.
-spec instantiate_row_vars(row_poly_scheme(), [catena_row_types:row_var()], inference_state()) -> {row_poly_type(), inference_state()}.
instantiate_row_vars(#{type := Type}, Vars, State) ->
    %% Use provided variables
    InstanceType = Type#{row_vars => [catena_row_types:row_var_id(V) || V <- Vars]},
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
        row_vars => []
    },
    {Type, State};
infer_row_poly_function(#{effects := Effects}, State) when is_map(Effects) ->
    Type = #{
        kind => row_poly,
        effects => Effects,
        row_vars => []
    },
    {Type, State}.

%% @doc Infer row polymorphic type for an effect operation.
-spec infer_row_poly_operation(atom(), inference_state()) -> {row_poly_type(), inference_state()}.
infer_row_poly_operation(Operation, State) ->
    %% Effect operations introduce fresh row variables
    Var = catena_row_types:fresh_row_var(),
    Effects = catena_row_types:effect_row([Operation]),
    EffectsWithVar = Effects#{row_var => Var},

    Type = #{
        kind => row_poly,
        effects => EffectsWithVar,
        row_vars => [catena_row_types:row_var_id(Var)]
    },
    {Type, State}.

%% @doc Infer row polymorphic type for a handler.
-spec infer_row_poly_handler(map(), inference_state()) -> {row_poly_type(), inference_state()}.
infer_row_poly_handler(HandlerInfo, State) ->
    case HandlerInfo of
        #{handled := Handled, remaining := Remaining} ->
            %% Handler removes handled effects and may introduce remaining effects
            HandledEffects = catena_row_types:effect_row(Handled),
            RemainingEffects = catena_row_types:effect_row(Remaining),
            Combined = catena_row_operations:effect_union_rows(HandledEffects, RemainingEffects),
            Type = #{
                kind => row_poly,
                effects => Combined,
                row_vars => []
            },
            {Type, State};
        #{handled := Handled} ->
            Effects = catena_row_types:effect_row(Handled),
            Type = #{
                kind => row_poly,
                effects => Effects,
                row_vars => []
            },
            {Type, State}
    end.

%%%---------------------------------------------------------------------
%%% Row Constraint Propagation
%%%---------------------------------------------------------------------

%% @doc Propagate row constraints through the type.
-spec propagate_row_constraints(row_poly_type(), inference_state()) -> {ok, inference_state()} | {error, term()}.
propagate_row_constraints(#{effects := Effects}, State) ->
    Constraints = catena_row_unify:generate_row_constraints(Effects, Effects),
    case catena_row_unify:solve_row_constraints(Constraints) of
        {ok, Subst} ->
            {ok, State#{substitutions => Subst}};
        {error, Reason} ->
            {error, Reason}
    end.

%%%---------------------------------------------------------------------
%%% Row Polymorphic Schemes
%%%---------------------------------------------------------------------

%% @doc Create a row polymorphic scheme from a type and state.
-spec row_poly_scheme(row_poly_type(), inference_state()) -> row_poly_scheme().
row_poly_scheme(Type, #{row_var_counter := Counter}) ->
    Scheme = #{
        kind => row_scheme,
        type => Type,
        row_vars => []
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
apply_row_subst_to_type(#{effects := Effects, row_vars := RowVars}, Subst) ->
    NewEffects = catena_row_unify:apply_row_subst(Effects, Subst),
    #{
        kind => row_poly,
        effects => NewEffects,
        row_vars => RowVars
    }.

%% @doc Create fresh inference state.
-spec fresh_state() -> inference_state().
fresh_state() ->
    #{
        row_var_counter => 0,
        constraints => [],
        substitutions => #{}
    }.

%% @doc Update state with new constraint.
-spec add_constraint(catena_row_unify:row_constraint(), inference_state()) -> inference_state().
add_constraint(Constraint, #{constraints := Constraints} = State) ->
    State#{constraints => [Constraint | Constraints]}.
