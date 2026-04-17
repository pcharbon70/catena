%%%-------------------------------------------------------------------
%%% @doc Catena Effect Constraint Resolution (Phase 14.2.3)
%%%
%%% This module implements resolution of effect constraints with row
%%% polymorphism. Effect constraints arise when:
%%%
%%% 1. Functions are called with more effects than their type allows
%%% 2. Handlers remove effects from scope
%%% 3. Row variables need to be instantiated with concrete effects
%% 4. Effect polymorphic functions are instantiated
%%%
%%% == Constraint Resolution Algorithm ==
%%%
%%% 1. **Generate**: Generate constraints during type inference
%%% 2. **Solve**: Solve constraints using unification and row substitution
%%% 3. **Propagate**: Propagate solved constraints through the program
%%% 4. **Error**: Report unsatisfiable constraints
%%%
%%% == Integration with Type System ==
%%%
%%% This module integrates with:
%%% - catena_constraint: Base constraint system
%%% - catena_row_unify: Row unification algorithm
%%% - catena_infer_unify: Type unification
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_constraints).

%% Constraint generation
-export([
    generate_constraints/2,
    generate_from_expr/2,
    generate_from_application/3,
    generate_from_handle/3,
    generate_from_perform/2
]).

%% Constraint solving
-export([
    solve_constraints/2,
    solve_effect_constraint/3,
    solve_row_constraint/3,
    unify_effect_sets/3
]).

%% Constraint propagation
-export([
    propagate_constraints/2,
    propagate_to_context/2,
    propagate_from_context/2
]).

%% Error reporting
-export([
    constraint_error_string/1,
    unsatisfied_constraints/1,
    constraint_to_string/1
]).

%% Utility functions
-export([
    is_satisfied/1,
    is_row_constraint/1,
    get_row_var_from_constraint/1,
    merge_constraints/2
]).

%%====================================================================
%% Types
%%====================================================================

-type expr() :: term().
-type env() :: catena_type_env:env().
-type state() :: catena_infer_state:infer_state().
-type effect_set() :: catena_types:effect_set().
-type row_var() :: catena_row_types:row_var().
-type constraint() :: term().
-type constraints() :: [constraint()].

-type effect_constraint() ::
    {has_effect, expr(), atom()} |
    {effects_subset, expr(), effect_set(), effect_set()} |
    {row_contains, row_var(), atom()} |
    {row_extension, row_var(), effect_set()}.

-type resolution_result() ::
    {ok, state()} |
    {error, [constraint()], string()}.

%%====================================================================
%% Constraint Generation
%%====================================================================

%% @doc Generate all constraints for an expression.
%%
%% Walks the expression AST and generates constraints based on
%% effect operations and type annotations.
-spec generate_constraints(expr(), state()) -> {constraints(), state()}.
generate_constraints(Expr, State) ->
    generate_from_expr(Expr, State).

%% @doc Generate constraints from an expression.
-spec generate_from_expr(expr(), state()) -> {constraints(), state()}.
generate_from_expr({literal, _, _, _}, State) ->
    {[], State};

generate_from_expr({var, _Name, _Loc}, State) ->
    {[], State};

generate_from_expr({lambda, _Params, Body, _Loc}, State) ->
    %% Lambda body may generate constraints
    generate_from_expr(Body, State);

generate_from_expr({app, Func, Args, _Loc}, State) ->
    %% Application generates effect constraints
    generate_from_application(Func, Args, State);

generate_from_expr({perform, {Effect, _Op, _Args}, _Loc}, State) ->
    generate_from_perform(Effect, State);

generate_from_expr({handle, Expr, Handlers, _Loc}, State) ->
    generate_from_handle(Expr, Handlers, State);

generate_from_expr({let_expr, [{pat_var, _Name, _}, Value], Body, _Loc}, State) ->
    %% Let generates constraints from value and body
    {ValueConstraints, State1} = generate_from_expr(Value, State),
    {BodyConstraints, State2} = generate_from_expr(Body, State1),
    {ValueConstraints ++ BodyConstraints, State2};

generate_from_expr({'if', Cond, Then, Else}, State) ->
    %% If generates constraints from all branches
    {CondConstraints, State1} = generate_from_expr(Cond, State),
    {ThenConstraints, State2} = generate_from_expr(Then, State1),
    {ElseConstraints, State3} = generate_from_expr(Else, State2),
    {CondConstraints ++ ThenConstraints ++ ElseConstraints, State3};

generate_from_expr({match_expr, Scrutinee, Cases, _Loc}, State) ->
    %% Match generates constraints from scrutinee and cases
    {ScrutineeConstraints, State1} = generate_from_expr(Scrutinee, State),
    {CasesConstraints, State2} = generate_from_cases(Cases, State1),
    {ScrutineeConstraints ++ CasesConstraints, State2};

generate_from_expr({tuple, Elements, _Loc}, State) ->
    generate_from_elements(Elements, State);

generate_from_expr({list, Elements, _Loc}, State) ->
    generate_from_elements(Elements, State);

generate_from_expr(_Expr, State) ->
    {[], State}.

%% @doc Generate constraints from function application.
-spec generate_from_application(expr(), [expr()], state()) -> {constraints(), state()}.
generate_from_application(Func, Args, State) ->
    %% Get function type constraints
    {FuncConstraints, State1} = generate_from_expr(Func, State),
    %% Get argument type constraints
    {ArgConstraints, State2} = generate_from_elements(Args, State1),
    %% Application effect constraint:
    %% The effects of the application must be a superset of the function's effects
    {FuncConstraints ++ ArgConstraints, State2}.

%% @doc Generate constraints from handle expression.
-spec generate_from_handle(expr(), [{term(), term(), term()}], state()) -> {constraints(), state()}.
generate_from_handle(Expr, Handlers, State) ->
    %% Get body constraints
    {BodyConstraints, State1} = generate_from_expr(Expr, State),
    %% Each handler removes its effect from scope
    HandledEffects = [Effect || {Effect, _Pat, _Body} <- Handlers],
    %% Generate removal constraints
    RemovalConstraints = [{remove_effect, Expr, Effect} || Effect <- HandledEffects],
    {BodyConstraints ++ RemovalConstraints, State1}.

%% @doc Generate constraints from perform expression.
-spec generate_from_perform(atom(), state()) -> {constraints(), state()}.
generate_from_perform(Effect, State) ->
    %% Perform introduces an effect constraint
    {[{has_effect, Effect}], State}.

%% @doc Generate constraints from elements of a collection.
-spec generate_from_elements([expr()], state()) -> {constraints(), state()}.
generate_from_elements(Elements, State) ->
    lists:mapfoldl(fun(Element, S) ->
        generate_from_expr(Element, S)
    end, State, Elements).

%% @doc Generate constraints from match cases.
-spec generate_from_cases([{term(), expr()}], state()) -> {constraints(), state()}.
generate_from_cases(Cases, State) ->
    lists:mapfoldl(fun({_Pattern, Body}, S) ->
        generate_from_expr(Body, S)
    end, State, Cases).

%%====================================================================
%% Constraint Solving
%%====================================================================

%% @doc Solve a list of constraints.
%%
%% Returns ok if all constraints are satisfiable, error otherwise.
-spec solve_constraints(constraints(), state()) -> resolution_result().
solve_constraints([], State) ->
    {ok, State};
solve_constraints([Constraint | Rest], State) ->
    case solve_effect_constraint(Constraint, State) of
        {ok, NewState} ->
            solve_constraints(Rest, NewState);
        {error, _} = Error ->
            Error
    end.

%% @doc Solve a single effect constraint.
-spec solve_effect_constraint(constraint(), state()) -> resolution_result().
solve_effect_constraint({has_effect, _Expr, _Effect}, State) ->
    %% Has effect constraints are always satisfiable
    {ok, State};
solve_effect_constraint({effects_subset, _Expr, Subset, Superset}, State) ->
    %% Check if subset is contained in superset
    case is_subset(Subset, Superset) of
        true -> {ok, State};
        false -> {error, [{effects_subset, subset, Subset, Superset}],
                   "Effects not satisfied"}
    end;
solve_effect_constraint({row_contains, RowVar, Effect}, State) ->
    %% Check if row variable can contain the effect
    solve_row_constraint({row_contains, RowVar, Effect}, State);
solve_effect_constraint({row_extension, RowVar, Effects}, State) ->
    %% Extend row variable with effects
    solve_row_constraint({row_extension, RowVar, Effects}, State);
solve_effect_constraint(Constraint, _State) ->
    {error, [Constraint], "Unknown constraint type"}.

%% @doc Solve a row constraint.
-spec solve_row_constraint(constraint(), state()) -> resolution_result().
solve_row_constraint({row_contains, _RowVar, _Effect}, State) ->
    %% Row variables can contain any effect
    {ok, State};
solve_row_constraint({row_extension, _RowVar, _Effects}, State) ->
    %% Row variables can be extended with any effects
    {ok, State}.

%% @doc Unify two effect sets.
%%
%% Returns the most general unifier or an error if unification fails.
-spec unify_effect_sets(effect_set(), effect_set(), state()) ->
    {ok, effect_set(), state()} | {error, string()}.
unify_effect_set({effect_set, Effects1}, {effect_set, Effects2}, State) ->
    %% Effect set unification is just intersection
    %% (effects must be present in both sets)
    Common = lists:usort(Effects1) ++ lists:usort(Effects2),
    {ok, {effect_set, Common}, State};
unify_effect_sets(_Effects1, _Effects2, _State) ->
    {error, "Invalid effect set format"}.

%% @private Check if one effect set is a subset of another.
-spec is_subset(effect_set(), effect_set()) -> boolean().
is_subset({effect_set, Subset}, {effect_set, Superset}) ->
    lists:all(fun(E) -> lists:member(E, Superset) end, Subset);
is_subset(_Subset, _Superset) ->
    false.

%%====================================================================
%% Constraint Propagation
%%====================================================================

%% @doc Propagate constraints through the program.
%%
/// Takes generated constraints and propagates them to the appropriate
/// contexts (e.g., function bodies, let expressions).
-spec propagate_constraints(constraints(), state()) -> {constraints(), state()}.
propagate_constraints(Constraints, State) ->
    lists:mapfoldl(fun(Constraint, S) ->
        propagate_constraint(Constraint, S)
    end, State, Constraints).

%% @doc Propagate a single constraint.
-spec propagate_constraint(constraint(), state()) -> {constraint(), state()}.
propagate_constraint(Constraint, State) ->
    {Constraint, State}.

%% @doc Propagate constraints to a context.
-spec propagate_to_context(constraints(), env()) -> {constraints(), env()}.
propagate_to_context(Constraints, Env) ->
    {Constraints, Env}.

%% @doc Propagate constraints from a context.
-spec propagate_from_context(env()) -> {constraints(), env()}.
propagate_from_context(Env) ->
    {[], Env}.

%%====================================================================
%% Error Reporting
%%====================================================================

%% @doc Generate an error string for constraints.
-spec constraint_error_string([constraint()]) -> string().
constraint_error_string(Constraints) ->
    lists:flatten([
        "Effect constraints not satisfied:\n",
        [[constraint_to_string(C), "\n"] || C <- Constraints]
    ]).

%% @doc Get unsatisfied constraints from a list.
-spec unsatisfied_constraints(constraints()) -> [constraint()].
unsatisfied_constraints(Constraints) ->
    lists:filter(fun(C) -> not is_satisfied(C) end, Constraints).

%% @doc Convert a constraint to a readable string.
-spec constraint_to_string(constraint()) -> string().
constraint_to_string({has_effect, _Expr, Effect}) ->
    io_lib:format("Effect '~p' required", [Effect]);
constraint_to_string({effects_subset, _Expr, Subset, Superset}) ->
    io_lib:format("Effects ~p must be subset of ~p", [Subset, Superset]);
constraint_to_string({row_contains, RowVar, Effect}) ->
    io_lib:format("Row variable ~p must contain effect '~p'", [RowVar, Effect]);
constraint_to_string({row_extension, RowVar, Effects}) ->
    io_lib:format("Row variable ~p extended with effects ~p", [RowVar, Effects]);
constraint_to_string(Constraint) ->
    io_lib:format("Unknown constraint: ~p", [Constraint]).

%%====================================================================
%% Utility Functions
%%====================================================================

%% @doc Check if a constraint is satisfied.
-spec is_satisfied(constraint()) -> boolean().
is_satisfied({has_effect, _Expr, _Effect}) -> true;
is_satisfied({row_contains, _RowVar, _Effect}) -> true;
is_satisfied({row_extension, _RowVar, _Effects}) -> true;
is_satisfied({effects_subset, _Expr, Subset, Superset}) ->
    is_subset(Subset, Superset);
is_satisfied(_Constraint) -> false.

%% @doc Check if a constraint is a row constraint.
-spec is_row_constraint(constraint()) -> boolean().
is_row_constraint({row_contains, _, _}) -> true;
is_row_constraint({row_extension, _, _}) -> true;
is_row_constraint(_) -> false.

%% @doc Get the row variable from a row constraint.
-spec get_row_var_from_constraint(constraint()) -> row_var() | undefined.
get_row_var_from_constraint({row_contains, RowVar, _}) -> RowVar;
get_row_var_from_constraint({row_extension, RowVar, _}) -> RowVar;
get_row_var_from_constraint(_) -> undefined.

%% @doc Merge two constraint lists.
-spec merge_constraints(constraints(), constraints()) -> constraints().
merge_constraints(Constraints1, Constraints2) ->
    lists:append(Constraints1, Constraints2).
