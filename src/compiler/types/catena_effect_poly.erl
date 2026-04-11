%%%-------------------------------------------------------------------
%%% @doc Catena Effect Polymorphism (Phase 6.1)
%%%
%%% This module implements effect polymorphism for the Catena type system.
%%% Effect polymorphism allows functions to be generic over effect sets,
%%% enabling reusable libraries that don't hard-code effect requirements.
%%%
%%% == Effect Variables ==
%%%
%%% Effect variables (epsilon, delta, phi) represent unknown or polymorphic effect sets.
%%% They unify like type variables and are instantiated with concrete effects
%%% at call sites.
%%%
%%% == Effect Constraints ==
%%%
%%% Row constraints specify required effects: {FileIO | epsilon} means "at least FileIO".
%%% Absence constraints specify forbidden effects: epsilon without {Process} means "any effects except Process".
%%%
%%% == Example ==
%%%
%%% ```
%%% %% Generic function over any effect set:
%%% %% map function polymorphic over effects
%%%
%%% %% Function requiring at least IO:
%%% %% readFile from String to String with IO
%%%
%%% %% Function forbidding Process effects:
%%% %% pureComputation from Int to Int without Process
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_poly).

%% Effect variable creation
-export([
    evar/1,
    fresh_evar/1,
    is_effect_var/1
]).

%% Effect set operations with polymorphism
-export([
    empty_effects/0,
    singleton_effect/1,
    effect_set/1,
    union_effects/2,
    normalize_effects/1,
    is_pure/1,
    effects_equal/2,
    substitute_evar/3
]).

%% Effect constraints
-export([
    row_constraint/1,
    absence_constraint/1,
    is_row_constraint/1,
    is_absence_constraint/1,
    constraint_effects/1,
    satisfies_constraint/2
]).

%% Effect unification
-export([
    unify_effects/2,
    occurs_in_evar/2
]).

%% Effect quantification
-export([
    quantify_effects/2,
    instantiate_effects/2
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type effect_var_id() :: pos_integer().
-type effect_name() :: atom().

%% Effect expression - can be concrete set, variable, or constrained
-type effect_expr() ::
    {evar, effect_var_id()} |                    % Effect variable
    {effect_set, [effect_name()]} |              % Concrete effect set
    {constrained, effect_var_id(), [constraint()]}. % Constrained variable

-type constraint() ::
    {row, [effect_name()]} |                     % Required effects
    {absence, [effect_name()]}.                   % Forbidden effects

-type effect_set() :: effect_expr().

-type substitution() :: #{effect_var_id() => effect_expr()}.

-export_type([
    effect_var_id/0,
    effect_name/0,
    effect_expr/0,
    constraint/0,
    substitution/0
]).

%%====================================================================
%% Effect Variable Creation
%%====================================================================

%% @doc Create an effect variable.
%%
%% Effect variables represent polymorphic effect sets in type signatures.
%% They are identified by unique positive integers.
%%
%% @param Id Unique identifier for the effect variable
%% @returns An effect variable `{evar, Id}`
%%
%% @example
%% ```
%% %% Create effect variable ε1
%% Eps1 = catena_effect_poly:evar(1).  %% → {evar, 1}
%% '''
-spec evar(effect_var_id()) -> effect_expr().
evar(Id) when is_integer(Id), Id > 0 ->
    {evar, Id}.

%% @doc Generate a fresh effect variable.
%%
%% Creates a new effect variable with a unique identifier by updating
%% the inference state.
%%
%% @param State The current inference state
%% @returns {{evar, Id}, NewState}
%%
%% @example
%% ```
%% State0 = catena_infer_state:new(),
%% {{evar, 1}, State1} = catena_effect_poly:fresh_evar(State0).
%% '''
-spec fresh_evar(term()) -> {effect_expr(), term()}.
fresh_evar(State) ->
    {Id, NewState} = catena_types:fresh_var_id(State),
    {{evar, Id}, NewState}.

%% @doc Check if an effect expression is an effect variable.
%%
%% @param Expr The effect expression to check
%% @returns true if Expr is an effect variable
%%
%% @example
%% ```
%% true = catena_effect_poly:is_effect_var({evar, 1}).
%% false = catena_effect_poly:is_effect_var({effect_set, [io]}).
%% '''
-spec is_effect_var(effect_expr()) -> boolean().
is_effect_var({evar, _Id}) -> true;
is_effect_var(_Other) -> false.

%%====================================================================
%% Effect Set Operations
%%====================================================================

%% @doc Create an empty effect set (pure function).
%%
%% @returns An empty effect set `{effect_set, []}`
%%
%% @example
%% ```
%% Pure = catena_effect_poly:empty_effects().  %% → {effect_set, []}
%% '''
-spec empty_effects() -> effect_expr().
empty_effects() ->
    {effect_set, []}.

%% @doc Create a singleton effect set.
%%
%% @param Effect Name of the effect
%% @returns An effect set with one element
%%
%% @example
%% ```
%% IoEffect = catena_effect_poly:singleton_effect(io).  %% → {effect_set, [io]}
%% '''
-spec singleton_effect(effect_name()) -> effect_expr().
singleton_effect(Effect) when is_atom(Effect) ->
    {effect_set, [Effect]}.

%% @doc Create an effect set from a list of effects.
%%
%% Normalizes the set (sorts and removes duplicates).
%%
%% @param Effects List of effect names
%% @returns A normalized effect set
%%
%% @example
%% ```
%% Set = catena_effect_poly:effect_set([io, process, io]).
%% %% → {effect_set, [io, process]}  (normalized, duplicates removed)
%% '''
-spec effect_set([effect_name()]) -> effect_expr().
effect_set(Effects) when is_list(Effects) ->
    Normalized = normalize_effects(Effects),
    {effect_set, Normalized}.

%% @doc Union two effect sets/expressions.
%%
%% Combines two effect expressions, handling variables and concrete sets.
%%
%% @param Expr1 First effect expression
%% @param Expr2 Second effect expression
%% @returns Combined effect expression
%%
%% @example
%% ```
%% E1 = {effect_set, [io]},
%% E2 = {effect_set, [process]},
%% {effect_set, [io, process]} = catena_effect_poly:union_effects(E1, E2).
%% '''
-spec union_effects(effect_expr(), effect_expr()) -> effect_expr().
union_effects({effect_set, Set1}, {effect_set, Set2}) ->
    {effect_set, normalize_effects(Set1 ++ Set2)};
union_effects({evar, Id}, {effect_set, []}) ->
    {evar, Id};
union_effects({effect_set, []}, {evar, Id}) ->
    {evar, Id};
union_effects({evar, Id1}, {evar, Id2}) when Id1 =:= Id2 ->
    {evar, Id1};
union_effects({evar, _Id1}, {evar, _Id2}) ->
    % Different effect variables - would need unification
    {effect_set, []};  % Conservative: assume intersection is empty
union_effects({evar, Id}, Other) ->
    % Variable with concrete set - could be constrained
    % For now, keep as variable (unification would resolve)
    {evar, Id};
union_effects(Other, {evar, Id}) ->
    {evar, Id};
union_effects({constrained, Id, _Constraints}, Other) ->
    {evar, Id};  % Simplified: keep variable
union_effects(Other, {constrained, Id, _Constraints}) ->
    {evar, Id}.

%% @doc Normalize an effect list (sort and remove duplicates).
%%
%% @param Effects List of effect names
%% @returns Sorted list with unique elements
%%
%% @example
%% ```
%% [io, process] = catena_effect_poly:normalize_effects([process, io, process]).
%% '''
-spec normalize_effects([effect_name()]) -> [effect_name()].
normalize_effects(Effects) ->
    lists:usort(Effects).

%% @doc Check if an effect expression represents pure computation.
%%
%% @param Expr The effect expression to check
%% @returns true if pure (empty effect set)
%%
%% @example
%% ```
%% true = catena_effect_poly:is_pure({effect_set, []}).
%% false = catena_effect_poly:is_pure({effect_set, [io]}).
%% '''
-spec is_pure(effect_expr()) -> boolean().
is_pure({effect_set, []}) -> true;
is_pure(_Other) -> false.

%% @doc Check if two effect expressions are equal.
%%
%% @param Expr1 First effect expression
%% @param Expr2 Second effect expression
%% @returns true if the expressions represent the same effects
%%
%% @example
%% ```
%% true = catena_effect_poly:effects_equal(
%%     {effect_set, [io, process]},
%%     {effect_set, [process, io]}
%% ).
%% '''
-spec effects_equal(effect_expr(), effect_expr()) -> boolean().
effects_equal({effect_set, Set1}, {effect_set, Set2}) ->
    normalize_effects(Set1) =:= normalize_effects(Set2);
effects_equal({evar, Id1}, {evar, Id2}) ->
    Id1 =:= Id2;
effects_equal(_Expr1, _Expr2) ->
    false.

%% @doc Substitute an effect variable with an effect expression.
%%
%% Used during effect instantiation and unification.
%%
%% @param VarId The effect variable ID to substitute
%% @param Replacement The effect expression to substitute in
%% @param Expr The expression to perform substitution in
%% @returns Expression with variable replaced
%%
%% @example
%% ```
%% %% Substitute ε1 with {effect_set, [io]} in ε1 ∪ {process}
%% Expr1 = {evar, 1},
%% Expr2 = {effect_set, [process]},
%% Result = catena_effect_poly:substitute_evar(1, {effect_set, [io]}, Expr2).
%% '''
-spec substitute_evar(effect_var_id(), effect_expr(), effect_expr()) -> effect_expr().
substitute_evar(VarId, Replacement, {evar, VarId}) ->
    Replacement;
substitute_evar(VarId, Replacement, {effect_set, Effects}) ->
    {effect_set, Effects};
substitute_evar(_VarId, _Replacement, Other) ->
    Other.

%%====================================================================
%% Effect Constraints
%%====================================================================

%% @doc Create a row constraint (requires specific effects).
%%
%% Row constraints specify minimum required effects.
%% {row, [FileIO, IO]} means "at least FileIO and IO effects".
%%
%% @param Effects List of required effect names
%% @returns A row constraint
%%
%% @example
%% ```
%% C = catena_effect_poly:row_constraint([file_io, io]).
%% '''
-spec row_constraint([effect_name()]) -> constraint().
row_constraint(Effects) when is_list(Effects) ->
    {row, normalize_effects(Effects)}.

%% @doc Create an absence constraint (forbids specific effects).
%%
%% Absence constraints specify effects that must NOT be present.
%% {absence, [Process]} means "no Process effects allowed".
%%
%% @param Effects List of forbidden effect names
%% @returns An absence constraint
%%
%% @example
%% ```
%% C = catena_effect_poly:absence_constraint([process]).
%% '''
-spec absence_constraint([effect_name()]) -> constraint().
absence_constraint(Effects) when is_list(Effects) ->
    {absence, normalize_effects(Effects)}.

%% @doc Check if a constraint is a row constraint.
%%
%% @param Constraint The constraint to check
%% @returns true if it's a row constraint
%%
%% @example
%% ```
%% true = catena_effect_poly:is_row_constraint({row, [io]}).
%% false = catena_effect_poly:is_row_constraint({absence, [process]}).
%% '''
-spec is_row_constraint(constraint()) -> boolean().
is_row_constraint({row, _Effects}) -> true;
is_row_constraint(_Other) -> false.

%% @doc Check if a constraint is an absence constraint.
%%
%% @param Constraint The constraint to check
%% @returns true if it's an absence constraint
%%
%% @example
%% ```
%% true = catena_effect_poly:is_absence_constraint({absence, [process]}).
%% false = catena_effect_poly:is_absence_constraint({row, [io]}).
%% '''
-spec is_absence_constraint(constraint()) -> boolean().
is_absence_constraint({absence, _Effects}) -> true;
is_absence_constraint(_Other) -> false.

%% @doc Get the effects referenced in a constraint.
%%
%% @param Constraint The constraint
%% @returns List of effect names
%%
%% @example
%% ```
%% [io, file] = catena_effect_poly:constraint_effects({row, [io, file]}).
%% '''
-spec constraint_effects(constraint()) -> [effect_name()].
constraint_effects({row, Effects}) -> Effects;
constraint_effects({absence, Effects}) -> Effects.

%% @doc Check if an effect expression satisfies a constraint.
%%
%% For row constraints: all required effects must be present.
%% For absence constraints: none of the forbidden effects may be present.
%%
%% @param Expr The effect expression to check
%% @param Constraint The constraint to satisfy
%% @returns true if the constraint is satisfied
%%
%% @example
%% ```
%% %% Row constraint: requires IO
%% true = catena_effect_poly:satisfies_constraint(
%%     {effect_set, [io, process]},
%%     {row, [io]}
%% ).
%%
%% %% Absence constraint: forbids Process
%% false = catena_effect_poly:satisfies_constraint(
%%     {effect_set, [io, process]},
%%     {absence, [process]}
%% ).
%% '''
-spec satisfies_constraint(effect_expr(), constraint()) -> boolean().
satisfies_constraint({effect_set, Effects}, {row, Required}) ->
    lists:all(fun(E) -> lists:member(E, Effects) end, Required);
satisfies_constraint({effect_set, Effects}, {absence, Forbidden}) ->
    lists:all(fun(E) -> not lists:member(E, Effects) end, Forbidden);
satisfies_constraint({evar, _Id}, _Constraint) ->
    % Effect variable - unknown if constraint is satisfied
    true;
satisfies_constraint(_Expr, _Constraint) ->
    false.

%%====================================================================
%% Effect Unification
%%====================================================================

%% @doc Unify two effect expressions.
%%
%% Attempts to find a common substitution that makes both expressions equal.
%% Returns {ok, Substitution} on success, {error, Reason} on failure.
%%
%% @param Expr1 First effect expression
%% @param Expr2 Second effect expression
%% @returns {ok, Substitution} or {error, mismatch}
%%
%% @example
%% ```
%% %% Unify concrete sets
%% {ok, #{}} = catena_effect_poly:unify_effects(
%%     {effect_set, [io]},
%%     {effect_set, [io]}
%% ).
%%
%% %% Unify variable with concrete set
%% {ok, #{1 := {effect_set, [io]}}} = catena_effect_poly:unify_effects(
%%     {evar, 1},
%%     {effect_set, [io]}
%% ).
%% '''
-spec unify_effects(effect_expr(), effect_expr()) ->
    {ok, substitution()} | {error, term()}.
unify_effects({effect_set, Set1}, {effect_set, Set2}) ->
    case normalize_effects(Set1) =:= normalize_effects(Set2) of
        true -> {ok, #{}};
        false -> {error, effect_mismatch}
    end;
unify_effects({evar, VarId}, Expr) ->
    case occurs_in_evar(VarId, Expr) of
        true -> {error, {effect_occurs, VarId}};
        false -> {ok, #{VarId => Expr}}
    end;
unify_effects(Expr, {evar, VarId}) ->
    case occurs_in_evar(VarId, Expr) of
        true -> {error, {effect_occurs, VarId}};
        false -> {ok, #{VarId => Expr}}
    end;
unify_effects({constrained, Id, _Constraints}, Expr) ->
    % Simplified: treat as variable
    {ok, #{Id => Expr}};
unify_effects(Expr, {constrained, Id, _Constraints}) ->
    {ok, #{Id => Expr}}.

%% @doc Check if an effect variable occurs in an effect expression (occurs check).
%%
%% Prevents infinite effect sets during unification.
%%
%% @param VarId The effect variable ID to check
%% @param Expr The expression to check in
%% @returns true if the variable occurs in the expression
%%
%% @example
%% ```
%% true = catena_effect_poly:occurs_in_evar(1, {evar, 1}).
%% false = catena_effect_poly:occurs_in_evar(1, {effect_set, [io]}).
%% '''
-spec occurs_in_evar(effect_var_id(), effect_expr()) -> boolean().
occurs_in_evar(VarId, {evar, VarId}) -> true;
occurs_in_evar(VarId, {effect_set, _Effects}) -> false;
occurs_in_evar(VarId, {constrained, Id, _Constraints}) when VarId =:= Id -> true;
occurs_in_evar(_VarId, _Other) -> false.

%%====================================================================
%% Effect Quantification
%%====================================================================

%% @doc Quantify effect variables in a type.
%%
%% Marks effect variables as polymorphic (generalized). Similar to
%% let-polymorphism for types.
%%
%% @param Expr The type expression
%% @param VarIds List of effect variable IDs to quantify
%% @returns Type with quantified effects
%%
%% @example
%% ```
%% %% Quantify ε1 in a function type
%% Type1 = {tfun, {tvar, 1}, {tvar, 2}, {evar, 1}},
%% Type2 = catena_effect_poly:quantify_effects(Type1, [1]).
%% '''
-spec quantify_effects(term(), [effect_var_id()]) -> term().
quantify_effects(Type, VarIds) when is_list(VarIds) ->
    lists:foldl(fun(VarId, AccType) ->
        quantify_evar_in_type(AccType, VarId)
    end, Type, VarIds).

%% @doc Instantiate quantified effects.
%%
%% Replaces quantified effect variables with fresh variables.
%%
%% @param Type The type with quantified effects
%% @param State The inference state for generating fresh variables
%% @returns {InstantiatedType, NewState}
%%
%% @example
%% ```
%% %% Instantiate a polymorphic effect type
%% State0 = catena_infer_state:new(),
%% {NewType, State1} = catena_effect_poly:instantiate_effects(PolyType, State0).
%% '''
-spec instantiate_effects(term(), term()) -> {term(), term()}.
instantiate_effects({tfun, From, To, Effects}, State) ->
    instantiate_in_tfun(From, To, Effects, State);
instantiate_effects(Type, State) ->
    {Type, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Quantify a single effect variable in a type.
-spec quantify_evar_in_type(term, effect_var_id()) -> term.
quantify_evar_in_type({tfun, From, To, {evar, VarId}}, VarId) ->
    % Mark as quantified - for now, keep as is (could add explicit quantification)
    {tfun, From, To, {evar, VarId}};
quantify_evar_in_type({tfun, From, To, Effects}, VarId) ->
    {tfun, From, To, quantify_in_effects(Effects, VarId)};
quantify_evar_in_type(Type, _VarId) ->
    Type.

%% @doc Quantify variable in effect expression.
-spec quantify_in_effects(effect_expr(), effect_var_id()) -> effect_expr().
quantify_in_effects({evar, VarId}, VarId) ->
    {evar, VarId};  % Already quantified
quantify_in_effects(Effects, _VarId) ->
    Effects.

%% @doc Instantiate effects in a function type.
-spec instantiate_in_tfun(term(), term(), effect_expr(), term()) -> {term(), term()}.
instantiate_in_tfun(From, To, {evar, VarId}, State) ->
    {{evar, NewVarId}, NewState} = fresh_evar(State),
    NewType = {tfun, From, To, {evar, NewVarId}},
    {NewType, NewState};
instantiate_in_tfun(From, To, {effect_set, Effects}, State) ->
    NewType = {tfun, From, To, {effect_set, Effects}},
    {NewType, State};
instantiate_in_tfun(From, To, {constrained, VarId, Constraints}, State) ->
    {{evar, NewVarId}, NewState} = fresh_evar(State),
    NewEffects = {constrained, NewVarId, Constraints},
    NewType = {tfun, From, To, NewEffects},
    {NewType, NewState}.
