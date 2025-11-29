%%%-------------------------------------------------------------------
%%% @doc Effect Inference Module
%%%
%%% This module implements the full effect inference algorithm for Catena.
%%% It is the main entry point for effect inference during type checking,
%%% orchestrating effect tracking, propagation, and resolution.
%%%
%%% == Effect System Overview ==
%%%
%%% Catena uses an algebraic effect system where:
%%% - Effects are declared with `effect EffectName { operations }`
%%% - Effects are introduced with `perform Effect.operation(args)`
%%% - Effects are handled with `handle expr with { Effect { handlers } }`
%%%
%%% == Effect Inference Rules ==
%%%
%%% 1. Pure expressions (literals, variables, lambdas) have no effects
%%% 2. `perform Effect.op(args)` introduces {Effect} into the effect set
%%% 3. Function application propagates the function's effects
%%% 4. `handle body with handlers` removes handled effects from body's set
%%% 5. Effects union across branches (if/match/let)
%%%
%%% == Integration with Type Inference ==
%%%
%%% Effect inference runs alongside type inference. The inference state
%%% tracks the current effect set, and this module provides functions
%%% to update and query effects during the inference process.
%%%
%%% == Phase 6 Preparation ==
%%%
%%% This module is designed to support future effect polymorphism:
%%% - Effect variables (e.g., `f a -> b / e` where `e` is a row variable)
%%% - Effect constraints (e.g., `Throws e => ...`)
%%% - Effect row polymorphism
%%%
%%% For Phase 1, effects are monomorphic (concrete effect sets).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_infer).

-export([
    %% Main inference entry points
    infer_effects/2,
    infer_effects/3,

    %% Expression-specific inference
    infer_perform/4,
    infer_handle/4,
    infer_application/4,

    %% Effect propagation
    propagate_effects/2,
    merge_branch_effects/2,

    %% Effect resolution
    resolve_handler/3,
    remove_effect/2,

    %% Effect extraction from types
    extract_function_effects/1,
    build_function_type/3,

    %% Utility functions
    effects_of/1,
    with_effect_scope/2
]).

%%%===================================================================
%%% Types
%%%===================================================================

-type expr() :: term().  % Catena AST expression
-type env() :: catena_type_env:env().
-type state() :: catena_infer_state:infer_state().
-type effect_set() :: catena_infer_effect:effect_set().

%%%===================================================================
%%% Main Inference Entry Points
%%%===================================================================

%% @doc Infer effects for an expression
%%
%% This is the main entry point for effect inference. It walks the
%% expression AST and accumulates effects into the inference state.
%%
%% Returns the updated state with effects added.
-spec infer_effects(expr(), state()) -> state().
infer_effects(Expr, State) ->
    infer_effects(Expr, catena_type_env:empty(), State).

%% @doc Infer effects for an expression with environment
%%
%% The environment provides type information for variables, which
%% is needed to determine effects of function applications.
-spec infer_effects(expr(), env(), state()) -> state().

%% Literals are pure
infer_effects({literal, _, _, _}, _Env, State) ->
    State;

%% Variables are pure (the lookup itself has no effects)
infer_effects({var, _, _}, _Env, State) ->
    State;

%% Lambda construction is pure (effects come from application)
infer_effects({lambda, _Params, Body, _Loc}, Env, State) ->
    %% Enter a new effect scope for the lambda body
    %% Effects inside the lambda don't escape to the enclosing scope
    State1 = catena_infer_state:push_effect_scope(State),
    State2 = infer_effects(Body, Env, State1),
    %% The lambda's effects are captured in its type, not propagated
    catena_infer_state:pop_effect_scope(State2);

%% Let expressions: effects from both binding and body
infer_effects({let_expr, Bindings, Body, _Loc}, Env, State) ->
    State1 = infer_bindings_effects(Bindings, Env, State),
    infer_effects(Body, Env, State1);

%% Match expressions: union of all branch effects
infer_effects({match_expr, Scrutinee, Clauses, _Loc}, Env, State) ->
    State1 = case Scrutinee of
        undefined -> State;
        _ -> infer_effects(Scrutinee, Env, State)
    end,
    infer_match_effects(Clauses, Env, State1);

%% Binary operations: effects from both operands
infer_effects({binary_op, _Op, Left, Right, _Loc}, Env, State) ->
    State1 = infer_effects(Left, Env, State),
    infer_effects(Right, Env, State1);

%% Function application: propagate function's effects
infer_effects({app, Fun, Args, _Loc}, Env, State) ->
    infer_application(Fun, Args, Env, State);

%% Perform: introduces an effect
infer_effects({perform_expr, EffectName, _OpName, Args, _Loc}, Env, State) ->
    %% Add the effect to state and infer effects from arguments
    State1 = catena_infer_state:add_effect(EffectName, State),
    infer_args_effects(Args, Env, State1);

%% Handle: removes effects
infer_effects({handle_expr, Body, Handlers, _Loc}, Env, State) ->
    infer_handle(Body, Handlers, Env, State);

%% Do expression: effects from all statements
infer_effects({do_expr, Stmts, _Loc}, Env, State) ->
    infer_do_effects(Stmts, Env, State);

%% Tuple: effects from all elements
infer_effects({tuple_expr, Elements, _Loc}, Env, State) ->
    infer_args_effects(Elements, Env, State);

%% List: effects from all elements
infer_effects({list_expr, Elements, _Loc}, Env, State) ->
    infer_args_effects(Elements, Env, State);

%% Record: effects from all field values
infer_effects({record_expr, Fields, _Base, _Loc}, Env, State) ->
    infer_field_effects(Fields, Env, State);

%% Record access: effects from the record expression
infer_effects({record_access, Expr, _Field, _Loc}, Env, State) ->
    infer_effects(Expr, Env, State);

%% Cons: effects from head and tail
infer_effects({cons_expr, Head, Tail, _Loc}, Env, State) ->
    State1 = infer_effects(Head, Env, State),
    infer_effects(Tail, Env, State1);

%% Default: assume pure for unknown expressions
infer_effects(_Other, _Env, State) ->
    State.

%%%===================================================================
%%% Expression-Specific Inference
%%%===================================================================

%% @doc Infer effects from a perform expression
%%
%% `perform Effect.operation(args)` introduces {Effect} into the effect set.
%% The effect is added to the current inference state.
-spec infer_perform(atom(), atom(), [expr()], state()) -> state().
infer_perform(EffectName, _OpName, _Args, State) ->
    catena_infer_state:add_effect(EffectName, State).

%% @doc Infer effects from a handle expression
%%
%% `handle body with { Effect { handlers } }` removes the handled effect
%% from the body's effect set. The result has effects: body_effects - {Effect}
-spec infer_handle(expr(), term(), env(), state()) -> state().
infer_handle(Body, Handlers, Env, State) ->
    %% Enter a new scope to track body effects separately
    State1 = catena_infer_state:push_effect_scope(State),

    %% Infer effects from the body
    State2 = infer_effects(Body, Env, State1),

    %% Get the effects from the body
    BodyEffects = catena_infer_state:get_effects(State2),

    %% Get the list of handled effects from handlers
    HandledEffects = extract_handled_effects(Handlers),

    %% Remove handled effects
    ResolvedEffects = resolve_handler(BodyEffects, HandledEffects, State2),

    %% Pop scope and merge resolved effects with outer scope
    State3 = catena_infer_state:pop_effect_scope(State2),

    %% Add the resolved effects to the outer scope
    lists:foldl(
        fun(Effect, S) -> catena_infer_state:add_effect(Effect, S) end,
        State3,
        effect_list(ResolvedEffects)
    ).

%% @doc Infer effects from function application
%%
%% When applying a function, the function's effects propagate to the
%% application site. This requires looking up the function's type
%% to extract its effect annotation.
-spec infer_application(expr(), [expr()], env(), state()) -> state().
infer_application(Fun, Args, Env, State) ->
    %% First infer effects from the function and argument expressions
    State1 = infer_effects(Fun, Env, State),
    State2 = infer_args_effects(Args, Env, State1),

    %% In full implementation, we'd look up Fun's type and extract effects
    %% For now, we assume the type inference will handle this
    State2.

%%%===================================================================
%%% Effect Propagation
%%%===================================================================

%% @doc Propagate effects from a function type to the current state
%%
%% When a function is called, its declared effects are added to the
%% current effect context.
-spec propagate_effects(catena_types:type(), state()) -> state().
propagate_effects({tfun, _Param, _Result, Effects}, State) ->
    CurrentEffects = catena_infer_state:get_effects(State),
    NewEffects = catena_infer_effect:union(CurrentEffects, Effects),
    catena_infer_state:set_effects(NewEffects, State);
propagate_effects(_NonFunction, State) ->
    State.

%% @doc Merge effects from multiple branches
%%
%% In conditional expressions (if/match), the result has the union
%% of effects from all branches.
-spec merge_branch_effects([effect_set()], state()) -> state().
merge_branch_effects(BranchEffects, State) ->
    MergedEffects = lists:foldl(
        fun(Effects, Acc) -> catena_infer_effect:union(Acc, Effects) end,
        catena_infer_effect:pure(),
        BranchEffects
    ),
    CurrentEffects = catena_infer_state:get_effects(State),
    FinalEffects = catena_infer_effect:union(CurrentEffects, MergedEffects),
    catena_infer_state:set_effects(FinalEffects, State).

%%%===================================================================
%%% Effect Resolution
%%%===================================================================

%% @doc Resolve effects by removing handled effects
%%
%% When a handler handles an effect, that effect is removed from the
%% effect set. Returns the remaining effects.
-spec resolve_handler(effect_set(), [atom()], state()) -> effect_set().
resolve_handler(Effects, HandledEffects, _State) ->
    catena_handler_verify:resolve_handled_effects(Effects, HandledEffects).

%% @doc Remove a single effect from an effect set
-spec remove_effect(atom(), effect_set()) -> effect_set().
remove_effect(EffectName, {effect_set, Effects}) ->
    {effect_set, lists:delete(EffectName, Effects)}.

%%%===================================================================
%%% Effect Extraction from Types
%%%===================================================================

%% @doc Extract the effect set from a function type
%%
%% Function types have the form: tfun(Param, Result, Effects)
%% This extracts the Effects component.
-spec extract_function_effects(catena_types:type()) -> effect_set().
extract_function_effects({tfun, _Param, _Result, Effects}) ->
    Effects;
extract_function_effects(_NonFunction) ->
    catena_infer_effect:pure().

%% @doc Build a function type with the given effect set
-spec build_function_type(catena_types:type(), catena_types:type(), effect_set()) ->
    catena_types:type().
build_function_type(ParamType, ResultType, Effects) ->
    {tfun, ParamType, ResultType, Effects}.

%%%===================================================================
%%% Utility Functions
%%%===================================================================

%% @doc Get effects from inference state
-spec effects_of(state()) -> effect_set().
effects_of(State) ->
    catena_infer_state:get_effects(State).

%% @doc Execute a function in a new effect scope
%%
%% Effects accumulated during the function execution are captured
%% and returned, but don't escape to the outer scope.
-spec with_effect_scope(fun((state()) -> state()), state()) ->
    {effect_set(), state()}.
with_effect_scope(Fun, State) ->
    State1 = catena_infer_state:push_effect_scope(State),
    State2 = Fun(State1),
    CapturedEffects = catena_infer_state:get_effects(State2),
    State3 = catena_infer_state:pop_effect_scope(State2),
    {CapturedEffects, State3}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Infer effects from a list of arguments
-spec infer_args_effects([expr()], env(), state()) -> state().
infer_args_effects(Args, Env, State) ->
    lists:foldl(
        fun(Arg, S) -> infer_effects(Arg, Env, S) end,
        State,
        Args
    ).

%% Infer effects from let bindings
-spec infer_bindings_effects([{term(), expr()}], env(), state()) -> state().
infer_bindings_effects(Bindings, Env, State) ->
    lists:foldl(
        fun({_Pat, Expr}, S) -> infer_effects(Expr, Env, S) end,
        State,
        Bindings
    ).

%% Infer effects from match clauses (union of all branches)
-spec infer_match_effects([term()], env(), state()) -> state().
infer_match_effects(Clauses, Env, State) ->
    %% Collect effects from each branch
    {BranchEffects, FinalState} = lists:foldl(
        fun(Clause, {Effects, S}) ->
            S1 = catena_infer_state:push_effect_scope(S),
            S2 = infer_clause_effects(Clause, Env, S1),
            ClauseEffects = catena_infer_state:get_effects(S2),
            S3 = catena_infer_state:pop_effect_scope(S2),
            {[ClauseEffects | Effects], S3}
        end,
        {[], State},
        Clauses
    ),
    %% Merge all branch effects
    merge_branch_effects(BranchEffects, FinalState).

%% Infer effects from a single match clause
-spec infer_clause_effects(term(), env(), state()) -> state().
infer_clause_effects({match_clause, _Pattern, _Guards, Body, _Loc}, Env, State) ->
    infer_effects(Body, Env, State);
infer_clause_effects({transform_clause, _Patterns, _Guards, Body, _Loc}, Env, State) ->
    infer_effects(Body, Env, State);
infer_clause_effects(_Other, _Env, State) ->
    State.

%% Infer effects from do-notation statements
-spec infer_do_effects([term()], env(), state()) -> state().
infer_do_effects([], _Env, State) ->
    State;
infer_do_effects([{do_bind, _Var, Expr, _Loc} | Rest], Env, State) ->
    State1 = infer_effects(Expr, Env, State),
    infer_do_effects(Rest, Env, State1);
infer_do_effects([{do_action, Expr, _Loc} | Rest], Env, State) ->
    State1 = infer_effects(Expr, Env, State),
    infer_do_effects(Rest, Env, State1);
infer_do_effects([{do_let, _Var, Expr, _Loc} | Rest], Env, State) ->
    State1 = infer_effects(Expr, Env, State),
    infer_do_effects(Rest, Env, State1);
infer_do_effects([{do_return, Expr, _Loc} | Rest], Env, State) ->
    State1 = infer_effects(Expr, Env, State),
    infer_do_effects(Rest, Env, State1);
infer_do_effects([_Other | Rest], Env, State) ->
    infer_do_effects(Rest, Env, State).

%% Infer effects from record fields
-spec infer_field_effects([{atom(), expr()}], env(), state()) -> state().
infer_field_effects(Fields, Env, State) ->
    lists:foldl(
        fun({_Name, Expr}, S) -> infer_effects(Expr, Env, S) end,
        State,
        Fields
    ).

%% Extract list of handled effect names from handler clauses
-spec extract_handled_effects(term()) -> [atom()].
extract_handled_effects(Handlers) when is_list(Handlers) ->
    lists:usort([Effect || {handler_clause, Effect, _, _, _, _} <- Handlers]);
extract_handled_effects({handler_clauses, Clauses}) ->
    extract_handled_effects(Clauses);
extract_handled_effects(_) ->
    [].

%% Convert effect set to list of effect names
-spec effect_list(effect_set()) -> [atom()].
effect_list({effect_set, Effects}) ->
    Effects.
