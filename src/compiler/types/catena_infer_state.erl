%%%-------------------------------------------------------------------
%%% @doc Inference State Management for Algorithm W
%%%
%%% This module manages the state used during type inference,
%%% including fresh variable generation, substitution accumulation,
%%% and error collection.
%%%
%%% The inference state is threaded through all inference operations
%%% to maintain functional purity (no process dictionary).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_infer_state).

-export([
    new/0,
    new/1,
    fresh_var/1,
    fresh_vars/2,
    fresh_var_id/1,
    get_subst/1,
    add_subst/2,
    compose_subst/2,
    add_error/2,
    get_errors/1,
    has_errors/1,
    get_next_var/1,
    set_next_var/2,
    get_counter/1,
    % Constraint operations
    add_constraint/2,
    add_constraints/2,
    get_constraints/1,
    clear_constraints/1,
    substitute_constraints/1,
    % Effect tracking (Section 1.5.6)
    get_effects/1,
    set_effects/2,
    add_effect/2,
    % Effect scope tracking (Phase 6 preparation)
    push_effect_scope/1,
    pop_effect_scope/1,
    get_effect_scope_depth/1,
    % Expression depth tracking (security)
    get_expr_depth/1,
    inc_expr_depth/1,
    dec_expr_depth/1,
    check_expr_depth/1
]).

-export_type([infer_state/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(infer_state, {
    next_var :: pos_integer(),                    % Next fresh type variable ID
    subst :: catena_type_subst:subst(),            % Accumulated substitution
    errors :: [catena_type_error:type_error()],    % Collected errors
    constraints :: catena_constraint:constraint_set(), % Trait constraints
    effects :: catena_infer_effect:effect_set(),  % Current effect set (Section 1.5.6)
    effect_scope_stack :: [catena_infer_effect:effect_set()], % Effect scope stack (Phase 6)
    expr_depth :: non_neg_integer()               % Current expression nesting depth
}).

%% Maximum expression depth to prevent stack overflow
-define(MAX_EXPR_DEPTH, 1000).



-opaque infer_state() :: #infer_state{}.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create a new inference state
-spec new() -> infer_state().
new() ->
    #infer_state{
        next_var = 1,
        subst = catena_type_subst:empty(),
        errors = [],
        constraints = catena_constraint:empty_constraint_set(),
        effects = catena_infer_effect:pure(),
        effect_scope_stack = [],
        expr_depth = 0
    }.

%% @doc Create a new state with specified starting counter
%% Useful for resuming inference or testing with specific IDs
-spec new(pos_integer()) -> infer_state().
new(StartCounter) when is_integer(StartCounter), StartCounter > 0 ->
    #infer_state{
        next_var = StartCounter,
        subst = catena_type_subst:empty(),
        errors = [],
        constraints = catena_constraint:empty_constraint_set(),
        effects = catena_infer_effect:pure(),
        effect_scope_stack = [],
        expr_depth = 0
    }.

%% @doc Generate a fresh type variable
%% Returns a new type variable and updated state
%%
%% **Pattern 3 Error:** Uses error/1 for overflow since this represents
%% a programming/DoS error that should never happen in valid code.
-spec fresh_var(infer_state()) -> {{tvar, pos_integer()}, infer_state()}.
fresh_var(#infer_state{next_var = N} = State) ->
    MaxId = catena_config:get_max_type_var_id(),
    case N =< MaxId of
        true ->
            Var = {tvar, N},
            State1 = State#infer_state{next_var = N + 1},
            {Var, State1};
        false ->
            error(catena_type_error:type_var_overflow(N, MaxId))
    end.

%% @doc Generate a fresh type variable ID and return updated state
%% Returns {VarId, NewState}
%%
%% Example:
%%   State0 = catena_infer_state:new(),
%%   {Id1, State1} = catena_infer_state:fresh_var_id(State0),
%%   {Id2, State2} = catena_infer_state:fresh_var_id(State1),
%%   Id1 =:= 1, Id2 =:= 2
-spec fresh_var_id(infer_state()) -> {pos_integer(), infer_state()}.
fresh_var_id(#infer_state{next_var = N} = State) ->
    MaxId = catena_config:get_max_type_var_id(),
    case N =< MaxId of
        true ->
            NewVar = N,
            State1 = State#infer_state{next_var = N + 1},
            {NewVar, State1};
        false ->
            error(catena_type_error:type_var_overflow(N, MaxId))
    end.

%% @doc Generate multiple fresh type variables
%% Returns a list of new type variables and updated state
-spec fresh_vars(non_neg_integer(), infer_state()) ->
    {[{tvar, pos_integer()}], infer_state()}.
fresh_vars(Count, State) when Count >= 0 ->
    fresh_vars_acc(Count, [], State).

fresh_vars_acc(0, Acc, State) ->
    {lists:reverse(Acc), State};
fresh_vars_acc(N, Acc, State) ->
    {Var, State1} = fresh_var(State),
    fresh_vars_acc(N - 1, [Var | Acc], State1).

%% @doc Get the current substitution
-spec get_subst(infer_state()) -> catena_type_subst:subst().
get_subst(#infer_state{subst = Subst}) ->
    Subst.

%% @doc Add a substitution to the state
%% This replaces the current substitution (for single unification steps)
-spec add_subst(catena_type_subst:subst(), infer_state()) -> infer_state().
add_subst(NewSubst, #infer_state{} = State) ->
    State#infer_state{subst = NewSubst}.

%% @doc Compose a substitution with the current state substitution
%% Composes S2 with S1 (current), resulting in S2 ∘ S1
-spec compose_subst(catena_type_subst:subst(), infer_state()) -> infer_state().
compose_subst(S2, #infer_state{subst = S1} = State) ->
    Composed = catena_type_subst:compose(S2, S1),
    State#infer_state{subst = Composed}.

%% @doc Add an error to the state
-spec add_error(catena_type_error:type_error(), infer_state()) -> infer_state().
add_error(Error, #infer_state{errors = Errors} = State) ->
    State#infer_state{errors = [Error | Errors]}.

%% @doc Get all collected errors (in reverse order of occurrence)
-spec get_errors(infer_state()) -> [catena_type_error:type_error()].
get_errors(#infer_state{errors = Errors}) ->
    lists:reverse(Errors).

%% @doc Check if any errors have been collected
-spec has_errors(infer_state()) -> boolean().
has_errors(#infer_state{errors = []}) ->
    false;
has_errors(#infer_state{errors = [_|_]}) ->
    true.

%% @doc Get the next variable ID (for testing/debugging)
-spec get_next_var(infer_state()) -> pos_integer().
get_next_var(#infer_state{next_var = N}) ->
    N.

%% @doc Set the next variable ID (for testing/debugging)
-spec set_next_var(pos_integer(), infer_state()) -> infer_state().
set_next_var(N, #infer_state{} = State) when N > 0 ->
    State#infer_state{next_var = N}.

%% @doc Get the current counter value without modifying state
%% Alias for get_next_var/1 for compatibility with catena_type_state
%% Useful for debugging and testing
-spec get_counter(infer_state()) -> pos_integer().
get_counter(#infer_state{next_var = N}) ->
    N.

%%%===================================================================
%%% Constraint Operations
%%%===================================================================

%% @doc Add a single constraint to the state
-spec add_constraint(catena_constraint:constraint(), infer_state()) -> infer_state().
add_constraint(Constraint, #infer_state{constraints = Cs} = State) ->
    NewCs = catena_constraint:add_constraint(Constraint, Cs),
    State#infer_state{constraints = NewCs}.

%% @doc Add multiple constraints to the state
-spec add_constraints(catena_constraint:constraint_set(), infer_state()) -> infer_state().
add_constraints(NewConstraints, #infer_state{constraints = Cs} = State) ->
    UpdatedCs = catena_constraint:add_constraints(NewConstraints, Cs),
    State#infer_state{constraints = UpdatedCs}.

%% @doc Get all constraints from the state
-spec get_constraints(infer_state()) -> catena_constraint:constraint_set().
get_constraints(#infer_state{constraints = Cs}) ->
    Cs.

%% @doc Clear all constraints from the state
-spec clear_constraints(infer_state()) -> infer_state().
clear_constraints(State) ->
    State#infer_state{constraints = catena_constraint:empty_constraint_set()}.

%% @doc Apply the current substitution to all constraints
%%
%% This should be called after unification to ensure constraints
%% are kept in sync with the current type knowledge.
%%
-spec substitute_constraints(infer_state()) -> infer_state().
substitute_constraints(#infer_state{subst = Subst, constraints = Cs} = State) ->
    NewCs = catena_constraint:substitute(Subst, Cs),
    State#infer_state{constraints = NewCs}.

%%%===================================================================
%%% Effect Tracking (Section 1.5.6)
%%%===================================================================

%% @doc Get the current effect set from the state
-spec get_effects(infer_state()) -> catena_infer_effect:effect_set().
get_effects(#infer_state{effects = Effects}) ->
    Effects.

%% @doc Set the effect set in the state
-spec set_effects(catena_infer_effect:effect_set(), infer_state()) -> infer_state().
set_effects(Effects, State) ->
    State#infer_state{effects = Effects}.

%% @doc Add a single effect to the current effect set
-spec add_effect(atom(), infer_state()) -> infer_state().
add_effect(EffectName, #infer_state{effects = Effects} = State) ->
    NewEffects = catena_infer_effect:union(Effects,
                                           catena_infer_effect:from_list([EffectName])),
    State#infer_state{effects = NewEffects}.

%%%===================================================================
%%% Effect Scope Tracking (Phase 6 Preparation)
%%%===================================================================

%% @doc Push current effects onto scope stack and start fresh
%%
%% Used when entering a scope where effects should be tracked separately
%% (e.g., inside a handle block or let binding with local effects).
%% The current effects are saved and can be restored with pop_effect_scope/1.
-spec push_effect_scope(infer_state()) -> infer_state().
push_effect_scope(#infer_state{effects = Effects, effect_scope_stack = Stack} = State) ->
    State#infer_state{
        effect_scope_stack = [Effects | Stack],
        effects = catena_infer_effect:pure()
    }.

%% @doc Pop effect scope and merge with current effects
%%
%% Restores the saved effects from the stack and unions them with
%% the effects accumulated in the current scope.
-spec pop_effect_scope(infer_state()) -> infer_state().
pop_effect_scope(#infer_state{effects = CurrentEffects, effect_scope_stack = [Saved | Rest]} = State) ->
    MergedEffects = catena_infer_effect:union(Saved, CurrentEffects),
    State#infer_state{
        effect_scope_stack = Rest,
        effects = MergedEffects
    };
pop_effect_scope(#infer_state{effect_scope_stack = []} = State) ->
    %% No scope to pop - return unchanged (shouldn't happen in correct usage)
    State.

%% @doc Get the current effect scope depth
-spec get_effect_scope_depth(infer_state()) -> non_neg_integer().
get_effect_scope_depth(#infer_state{effect_scope_stack = Stack}) ->
    length(Stack).

%%%===================================================================
%%% Expression Depth Tracking (Security)
%%%===================================================================

%% @doc Get current expression nesting depth
-spec get_expr_depth(infer_state()) -> non_neg_integer().
get_expr_depth(#infer_state{expr_depth = Depth}) ->
    Depth.

%% @doc Increment expression depth (call when entering nested expression)
-spec inc_expr_depth(infer_state()) -> infer_state().
inc_expr_depth(#infer_state{expr_depth = Depth} = State) ->
    State#infer_state{expr_depth = Depth + 1}.

%% @doc Decrement expression depth (call when exiting nested expression)
-spec dec_expr_depth(infer_state()) -> infer_state().
dec_expr_depth(#infer_state{expr_depth = Depth} = State) when Depth > 0 ->
    State#infer_state{expr_depth = Depth - 1};
dec_expr_depth(State) ->
    State.

%% @doc Check if expression depth exceeds limit
%%
%% Returns ok if within limit, or throws an error if exceeded.
%% This is a security measure to prevent stack overflow from
%% deeply nested expressions (potential DoS vector).
-spec check_expr_depth(infer_state()) -> ok.
check_expr_depth(#infer_state{expr_depth = Depth}) when Depth > ?MAX_EXPR_DEPTH ->
    error({expr_depth_exceeded, Depth, ?MAX_EXPR_DEPTH});
check_expr_depth(_State) ->
    ok.