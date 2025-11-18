%%%
%%% @doc Type Schemes for Polymorphism
%%%
%%% Implements type schemes (forall-quantified types) for let-polymorphism
%%% in Algorithm W. Type schemes represent polymorphic types like:
%%%   ∀α. α -> α (the identity function)
%%%   ∀α β. (α -> β) -> List α -> List β (the map function)
%%%
%%% @end
%%%
-module(catena_type_scheme).

-export([
    mono/1,
    mono/2,
    poly/2,
    poly/3,
    generalize/2,
    generalize/3,
    instantiate/2,
    ftv_scheme/1,
    get_constraints/1
]).

%%====================================================================
%% Type Definitions
%%====================================================================

%% Type scheme: monomorphic type or polymorphic type with quantified variables
%% Extended to support constraint contexts for qualified types:
%%   forall a. Eq a => a -> a -> Bool
-type scheme() :: {mono, catena_types:ty()}                          % Monomorphic (no constraints)
                | {mono, catena_types:ty(), catena_constraint:constraint_set()}  % Monomorphic with constraints
                | {poly, [catena_types:type_var_id()], catena_types:ty()}  % Polymorphic (no constraints, backward compat)
                | {poly, [catena_types:type_var_id()], catena_constraint:constraint_set(), catena_types:ty()}.  % Qualified polymorphic

-export_type([scheme/0]).

%%====================================================================
%% Type Scheme Construction
%%====================================================================

%% @doc Create a monomorphic type scheme.
%%
%% Wraps a concrete type in a monomorphic scheme with no quantified variables.
%% Monomorphic types cannot be instantiated with different types - they
%% represent specific, non-polymorphic types.
%%
%% @param Type The concrete type to wrap
%% @returns A monomorphic type scheme `{mono, Type}'
%%
%% @see poly/2
%% @see generalize/2
%%
%% @example
%% ```
%% %% Monomorphic Int type
%% IntScheme = catena_type_scheme:mono(catena_types:tcon(integer)).
%% %% → {mono, {tcon, integer}}
%%
%% %% Monomorphic function: Int -> String
%% FuncScheme = catena_type_scheme:mono(
%%     catena_types:tfun(
%%         catena_types:tcon(integer),
%%         catena_types:tcon(string),
%%         catena_types:empty_effects()
%%     )
%% ).
%% '''
-spec mono(catena_types:ty()) -> scheme().
mono(Type) ->
    {mono, Type}.

%% @doc Create a monomorphic type scheme with constraint context.
%%
%% Wraps a concrete type with its associated constraints in a monomorphic scheme.
%% Constraints represent trait requirements that must be satisfied for the type to be valid.
%%
%% @param Type The concrete type to wrap
%% @param Constraints The trait constraints associated with this type
%% @returns A monomorphic type scheme with constraints `{mono, Type, Constraints}'
%%
%% @example
%% ```
%% %% Eq a => a type (constrained monomorphic type)
%% EqConstraint = catena_constraint:trait_constraint('Eq', [Alpha]),
%% Scheme = catena_type_scheme:mono(Alpha, [EqConstraint]).
%% %% → {mono, {tvar, 1}, [{trait, 'Eq', [{tvar, 1}], unknown}]}
%% '''
-spec mono(catena_types:ty(), catena_constraint:constraint_set()) -> scheme().
mono(Type, []) ->
    {mono, Type};  % Optimize: no constraints = simple mono
mono(Type, Constraints) ->
    {mono, Type, Constraints}.

%% @doc Create a polymorphic type scheme with explicit quantification.
%%
%% Constructs a polymorphic scheme by explicitly listing which type variables
%% are quantified (bound by forall). The quantified variables can be replaced
%% with different types at each instantiation site.
%%
%% @param Vars List of type variable IDs to quantify
%% @param Type The type containing the quantified variables
%% @returns A polymorphic type scheme `{poly, Vars, Type}'
%%
%% @see mono/1
%% @see generalize/2
%% @see instantiate/2
%%
%% @example
%% ```
%% %% Identity function: ∀α. α -> α
%% State0 = catena_type_state:new(),
%% {Alpha, _State1} = catena_types:fresh_var(State0),
%% IdentityScheme = catena_type_scheme:poly(
%%     [1],  %% Quantify α₁
%%     catena_types:tfun(Alpha, Alpha, catena_types:empty_effects())
%% ).
%% %% → {poly, [1], {tfun, {tvar, 1}, {tvar, 1}, {effect_set, []}}}
%%
%% %% Map function: ∀α β. (α -> β) -> List α -> List β
%% {Beta, _State2} = catena_types:fresh_var(State0),
%% MapScheme = catena_type_scheme:poly([1, 2], MapType).
%% '''
-spec poly([catena_types:type_var_id()], catena_types:ty()) -> scheme().
poly(Vars, Type) when is_list(Vars) ->
    {poly, Vars, Type}.

%% @doc Create a polymorphic type scheme with constraints (qualified type).
%%
%% Constructs a qualified polymorphic scheme with both quantified variables and
%% constraint contexts. This represents types like:
%%   forall a. Eq a => a -> a -> Bool
%%
%% @param Vars List of type variable IDs to quantify
%% @param Constraints Trait constraints on the quantified variables
%% @param Type The type containing the quantified variables
%% @returns A qualified polymorphic type scheme `{poly, Vars, Constraints, Type}'
%%
%% @example
%% ```
%% %% forall a. Eq a => a -> a -> Bool
%% EqConstraint = catena_constraint:trait_constraint('Eq', [Alpha]),
%% EqualScheme = catena_type_scheme:poly(
%%     [1],  %% Quantify α
%%     [EqConstraint],
%%     catena_types:tfun(Alpha, catena_types:tfun(Alpha, BoolType, Effects), Effects)
%% ).
%% '''
-spec poly([catena_types:type_var_id()], catena_constraint:constraint_set(), catena_types:ty()) -> scheme().
poly(Vars, [], Type) when is_list(Vars) ->
    {poly, Vars, Type};  % Optimize: no constraints = simple poly
poly(Vars, Constraints, Type) when is_list(Vars) ->
    {poly, Vars, Constraints, Type}.

%%====================================================================
%% Generalization and Instantiation
%%====================================================================

%% @doc Generalize a type into a polymorphic type scheme.
%%
%% This implements the generalization step in Algorithm W (Hindley-Milner
%% type inference). Quantifies over all type variables that appear in the
%% type but NOT in the environment's free variables.
%%
%% Generalization creates polymorphic types at let-bindings, allowing
%% different uses of the same function to have different instantiated types.
%% Variables free in the environment cannot be quantified, as they may already
%% be constrained by outer scopes.
%%
%% @param Type The type to generalize
%% @param EnvFreeVars Set of type variables free in the current environment
%% @returns A type scheme quantifying over local variables
%%
%% @see poly/2
%% @see instantiate/2
%%
%% @example
%% ```
%% %% Generalize with no environment constraints
%% %% Type: α₁ -> α₁
%% State0 = catena_type_state:new(),
%% {Alpha, _State1} = catena_types:fresh_var(State0),
%% IdType = catena_types:tfun(Alpha, Alpha, catena_types:empty_effects()),
%% Scheme = catena_type_scheme:generalize(IdType, sets:new()).
%% %% → {poly, [1], {tfun, {tvar, 1}, {tvar, 1}, ...}}
%%
%% %% Generalize with environment constraints
%% %% Type: α₁ -> α₂
%% %% Environment has free variable α₂
%% {Beta, _State2} = catena_types:fresh_var(State0),
%% FuncType = catena_types:tfun(Alpha, Beta, catena_types:empty_effects()),
%% EnvVars = sets:from_list([2]),  %% β is free in environment
%% Scheme2 = catena_type_scheme:generalize(FuncType, EnvVars).
%% %% → {poly, [1], {tfun, {tvar, 1}, {tvar, 2}, ...}}
%% %% Only α₁ is quantified; α₂ remains free
%% '''
-spec generalize(catena_types:ty(), sets:set(catena_types:type_var_id())) -> scheme().
generalize(Type, EnvFreeVars) ->
    generalize(Type, [], EnvFreeVars).

%% @doc Generalize a type with constraints into a qualified polymorphic type scheme.
%%
%% Extended version of generalize/2 that also handles constraint contexts.
%% Quantifies over all type variables that appear in the type or constraints
%% but NOT in the environment's free variables.
%%
%% @param Type The type to generalize
%% @param Constraints Trait constraints associated with the type
%% @param EnvFreeVars Set of type variables free in the current environment
%% @returns A type scheme quantifying over local variables with constraints
%%
%% @example
%% ```
%% %% Generalize: Eq a => a -> a -> Bool
%% EqConstraint = catena_constraint:trait_constraint('Eq', [Alpha]),
%% Scheme = catena_type_scheme:generalize(FuncType, [EqConstraint], sets:new()).
%% %% → {poly, [1], [{trait, 'Eq', [...]}], {tfun, ...}}
%% '''
-spec generalize(catena_types:ty(), catena_constraint:constraint_set(),
                 sets:set(catena_types:type_var_id())) -> scheme().
generalize(Type, Constraints, EnvFreeVars) ->
    TypeVars = catena_types:type_vars(Type),
    ConstraintVars = sets:from_list(catena_constraint:type_vars(Constraints)),
    AllVars = sets:union(TypeVars, ConstraintVars),

    % Quantify over variables that appear in type/constraints but not in environment
    QuantVars = sets:subtract(AllVars, EnvFreeVars),
    QuantVarsList = lists:sort(sets:to_list(QuantVars)),

    case {QuantVarsList, Constraints} of
        {[], []} -> {mono, Type};           % No quantified variables, no constraints
        {[], _} -> {mono, Type, Constraints};  % No quantified variables, but has constraints
        {_, []} -> {poly, QuantVarsList, Type};  % Polymorphic, no constraints
        {_, _} -> {poly, QuantVarsList, Constraints, Type}  % Qualified polymorphic
    end.

%% @doc Instantiate a type scheme with fresh type variables.
%%
%% This implements the instantiation step in Algorithm W. Replaces all
%% quantified variables with fresh type variables, allowing the same
%% polymorphic function to be used at different types.
%%
%% Uses explicit state threading to ensure all fresh variables have
%% unique IDs. Monomorphic schemes instantiate to themselves unchanged.
%%
%% @param Scheme The type scheme to instantiate
%% @param State The current type state for fresh variable generation
%% @returns Tuple `{InstantiatedType, NewState}'
%%
%% @see generalize/2
%% @see poly/2
%%
%% @example
%% ```
%% %% Instantiate identity function: ∀α. α -> α
%% State0 = catena_type_state:new(),
%% IdScheme = {poly, [1], {tfun, {tvar, 1}, {tvar, 1}, {effect_set, []}}},
%% {InstType1, State1} = catena_type_scheme:instantiate(IdScheme, State0).
%% %% → {{tfun, {tvar, 2}, {tvar, 2}, ...}, State1}
%% %% α₁ replaced with fresh α₂
%%
%% %% Second instantiation gets different fresh variables
%% {InstType2, State2} = catena_type_scheme:instantiate(IdScheme, State1).
%% %% → {{tfun, {tvar, 3}, {tvar, 3}, ...}, State2}
%% %% α₁ replaced with fresh α₃
%%
%% %% Monomorphic schemes don't change
%% MonoScheme = {mono, {tcon, integer}},
%% {IntType, State3} = catena_type_scheme:instantiate(MonoScheme, State2).
%% %% → {{tcon, integer}, State3}
%% '''
%% @doc Instantiate a type scheme with fresh type variables.
%%
%% Extended to handle constraint contexts. Returns both the instantiated type
%% and the instantiated constraints that must be satisfied.
%%
%% @returns Tuple `{InstantiatedType, InstantiatedConstraints, NewState}'
-spec instantiate(scheme(), catena_infer_state:infer_state()) ->
    {catena_types:ty(), catena_constraint:constraint_set(), catena_infer_state:infer_state()}.
instantiate({mono, Type}, State) ->
    % Monomorphic types instantiate to themselves, no constraints
    {Type, [], State};
instantiate({mono, Type, Constraints}, State) ->
    % Monomorphic with constraints - return type and constraints
    {Type, Constraints, State};
instantiate({poly, QuantVars, Type}, State0) ->
    % Polymorphic without constraints (backward compatible)
    {InstType, _InstConstraints, StateFinal} = instantiate_poly(QuantVars, Type, [], State0),
    {InstType, [], StateFinal};
instantiate({poly, QuantVars, Constraints, Type}, State0) ->
    % Qualified polymorphic - instantiate both type and constraints
    {InstType, InstConstraints, StateFinal} = instantiate_poly(QuantVars, Type, Constraints, State0),
    {InstType, InstConstraints, StateFinal}.

%% Internal helper for polymorphic instantiation
-spec instantiate_poly([catena_types:type_var_id()], catena_types:ty(),
                       catena_constraint:constraint_set(), catena_infer_state:infer_state()) ->
    {catena_types:ty(), catena_constraint:constraint_set(), catena_infer_state:infer_state()}.
instantiate_poly(QuantVars, Type, Constraints, State0) ->
    % Create substitution mapping each quantified variable to a fresh one
    % Thread state through the fold
    {Subst, StateFinal} = lists:foldl(
        fun(VarId, {AccSubst, AccState}) ->
            {FreshVar, NewState} = catena_types:fresh_var(AccState),
            NewSubst = catena_type_subst:extend(AccSubst, VarId, FreshVar),
            {NewSubst, NewState}
        end,
        {catena_type_subst:empty(), State0},
        QuantVars
    ),
    % Apply substitution to both type and constraints
    InstType = catena_type_subst:apply(Subst, Type),
    InstConstraints = catena_constraint:substitute(Subst, Constraints),
    {InstType, InstConstraints, StateFinal}.

%%====================================================================
%% Free Type Variables
%%====================================================================

%% @doc Compute the free type variables of a type scheme.
%%
%% Free type variables are those that appear in the type but are NOT
%% quantified by the scheme's forall. For monomorphic schemes, all
%% variables are free. For polymorphic schemes, free variables are
%% those in the type minus the quantified ones.
%%
%% Essential for type environment operations and generalization,
%% as it determines which variables are still "unknowns" that can
%% be constrained by unification.
%%
%% @param Scheme The type scheme to analyze
%% @returns Set of free type variable IDs
%%
%% @see generalize/2
%% @see catena_types:type_vars/1
%%
%% @example
%% ```
%% %% Monomorphic scheme - all variables are free
%% State0 = catena_type_state:new(),
%% {Alpha, _State1} = catena_types:fresh_var(State0),
%% MonoScheme = {mono, Alpha},
%% FreeVars1 = catena_type_scheme:ftv_scheme(MonoScheme).
%% %% → set containing 1
%%
%% %% Polymorphic scheme - quantified variables not free
%% %% ∀α. α -> β  (β is free, α is quantified)
%% {Beta, _State2} = catena_types:fresh_var(State0),
%% PolyScheme = {poly, [1], {tfun, Alpha, Beta, {effect_set, []}}},
%% FreeVars2 = catena_type_scheme:ftv_scheme(PolyScheme).
%% %% → set containing only 2 (β is free, α is bound)
%%
%% %% Fully quantified scheme - no free variables
%% FullyQuantified = {poly, [1, 2], {tfun, Alpha, Beta, {effect_set, []}}},
%% FreeVars3 = catena_type_scheme:ftv_scheme(FullyQuantified).
%% %% → empty set
%% '''
-spec ftv_scheme(scheme()) -> sets:set(catena_types:type_var_id()).
ftv_scheme({mono, Type}) ->
    % All variables in monomorphic type are free
    catena_types:type_vars(Type);
ftv_scheme({mono, Type, Constraints}) ->
    % All variables in type and constraints are free
    TypeVars = catena_types:type_vars(Type),
    ConstraintVars = sets:from_list(catena_constraint:type_vars(Constraints)),
    sets:union(TypeVars, ConstraintVars);
ftv_scheme({poly, QuantVars, Type}) ->
    % Free vars = all vars in type minus quantified vars
    TypeVars = catena_types:type_vars(Type),
    QuantVarsSet = sets:from_list(QuantVars),
    sets:subtract(TypeVars, QuantVarsSet);
ftv_scheme({poly, QuantVars, Constraints, Type}) ->
    % Free vars = all vars in type/constraints minus quantified vars
    TypeVars = catena_types:type_vars(Type),
    ConstraintVars = sets:from_list(catena_constraint:type_vars(Constraints)),
    AllVars = sets:union(TypeVars, ConstraintVars),
    QuantVarsSet = sets:from_list(QuantVars),
    sets:subtract(AllVars, QuantVarsSet).

%%====================================================================
%% Constraint Extraction
%%====================================================================

%% @doc Extract constraints from a type scheme.
%%
%% Returns the constraint set associated with a type scheme.
%% For schemes without constraints, returns an empty list.
%%
%% @param Scheme The type scheme to extract constraints from
%% @returns The constraint set (empty list if no constraints)
%%
%% @example
%% ```
%% %% Monomorphic without constraints
%% Scheme1 = {mono, {tcon, int}},
%% Constraints1 = catena_type_scheme:get_constraints(Scheme1).
%% %% → []
%%
%% %% Monomorphic with constraints
%% EqConstraint = catena_constraint:trait_constraint('Eq', [Alpha]),
%% Scheme2 = {mono, Alpha, [EqConstraint]},
%% Constraints2 = catena_type_scheme:get_constraints(Scheme2).
%% %% → [{trait, 'Eq', [{tvar, 1}], unknown}]
%%
%% %% Qualified polymorphic
%% Scheme3 = {poly, [1], [EqConstraint], FuncType},
%% Constraints3 = catena_type_scheme:get_constraints(Scheme3).
%% %% → [{trait, 'Eq', [{tvar, 1}], unknown}]
%% ```
-spec get_constraints(scheme()) -> catena_constraint:constraint_set().
get_constraints({mono, _Type}) ->
    [];
get_constraints({mono, _Type, Constraints}) ->
    Constraints;
get_constraints({poly, _Vars, _Type}) ->
    [];
get_constraints({poly, _Vars, Constraints, _Type}) ->
    Constraints.
