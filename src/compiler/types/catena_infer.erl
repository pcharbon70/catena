%%%-------------------------------------------------------------------
%%% @doc Algorithm W Type Inference Orchestrator
%%%
%%% This module provides the top-level API for Algorithm W type inference,
%%% orchestrating all the inference components (expression inference,
%%% unification, generalization, instantiation) into a cohesive workflow.
%%%
%%% The main entry points provide:
%%% - Single expression inference
%%% - Program-level type checking
%%% - Error collection and formatting
%%%
%%% This is the public interface for the type inference system.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_infer).

-export([
    infer_expr/1,
    infer_expr/2,
    check_program/1,
    infer_expr_with_env/2,
    infer_lambda/3,
    check_type/2,
    format_errors/1
]).

-export_type([
    result/0,
    program_result/0
]).

%%%===================================================================
%%% Types
%%%===================================================================

%% @doc Result of single expression inference
-type result() ::
    {ok, catena_types:type()} |
    {error, [catena_type_error:type_error()]}.

%% @doc Result of program type checking
-type program_result() ::
    {ok, #{atom() => catena_types:type()}} |
    {error, [catena_type_error:type_error()]}.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Infer the type of an expression with empty environment
%% This is the most convenient entry point for simple cases
-spec infer_expr(catena_ast:expr()) -> result().
infer_expr(Expr) ->
    infer_expr(Expr, catena_type_env:empty()).

%% @doc Infer the type of an expression with provided environment
%% This is the main Algorithm W workflow:
%% 1. Initialize inference state
%% 2. Infer expression type
%% 3. Handle errors or return result
-spec infer_expr(catena_ast:expr(), catena_type_env:env()) -> result().
infer_expr(Expr, Env) ->
    infer_expr_with_env(Expr, Env).

%% @doc Internal implementation that does the actual work
-spec infer_expr_with_env(catena_ast:expr(), catena_type_env:env()) -> result().
infer_expr_with_env(Expr, Env) ->
    % Step 1: Initialize fresh inference state
    State0 = catena_infer_state:new(),

    % Step 2: Perform expression inference
    case catena_infer_expr:infer(Expr, Env, State0) of
        {Type, State1} ->
            % Step 3: Check for accumulated errors
            case catena_infer_state:has_errors(State1) of
                false ->
                    % Step 4: Apply final substitution to type and constraints
                    FinalSubst = catena_infer_state:get_subst(State1),
                    FinalType = catena_type_subst:apply(FinalSubst, Type),

                    % Step 5: Get and simplify accumulated constraints
                    Constraints = catena_infer_state:get_constraints(State1),
                    FinalConstraints = catena_constraint:substitute(FinalSubst, Constraints),
                    SimplifiedConstraints = catena_constraint:simplify(FinalConstraints),

                    % Step 6: Solve constraints
                    case solve_constraints(SimplifiedConstraints) of
                        ok ->
                            % All constraints satisfied
                            {ok, FinalType};
                        {error, ConstraintErrors} ->
                            % Constraint solving failed
                            {error, ConstraintErrors}
                    end;
                true ->
                    % Return all collected errors
                    Errors = catena_infer_state:get_errors(State1),
                    {error, Errors}
            end;
        {error, _Error, State1} ->
            % Error already in state - just return all errors
            Errors = catena_infer_state:get_errors(State1),
            {error, Errors}
    end.

%% @doc Type check a complete program
%%
%% A program is a list of let/rec bindings that should be
%% type checked in order, simulating top-level definitions.
%%
%% Returns either a map of variable names to their inferred types
%% or a list of errors.
%%
%% Example program:
%% ```
%% [
%%   {'letrec', id, {'lam', 'x', {'var', 'x'}}}, % let rec id = λx. x
%%   {'letrec', const, {'lam', 'x', {'lit', {int, 42}}}}  % let rec const = λx. 42
%% ]
%% ```
-spec check_program([{atom(), catena_ast:expr()}]) -> program_result().
check_program(Bindings) ->
    check_program(Bindings, catena_type_env:empty(), #{}).

%% @doc Internal implementation for program checking
-spec check_program([{atom(), catena_ast:expr()}], catena_type_env:env(), #{atom() => catena_types:type()}) -> program_result().
check_program([], _Env, Types) ->
    {ok, Types};
check_program([{Name, Expr} | Rest], Env, Types) ->
    case infer_expr_with_env(Expr, Env) of
        {ok, Type} ->
            % Add binding to environment for remaining expressions
            Scheme = catena_type_scheme:mono(Type),
            Env1 = catena_type_env:extend(Env, Name, Scheme),
            Types1 = Types#{Name => Type},
            check_program(Rest, Env1, Types1);
        {error, Errors} ->
            {error, Errors}
    end.

%%%===================================================================
%%% Convenience Functions
%%%===================================================================

%% @doc Infer a lambda expression with the given parameter type
%% This is a helper for common patterns
-spec infer_lambda(atom(), catena_types:type(), catena_ast:expr()) -> result().
infer_lambda(ParamName, ParamType, Body) ->
    % Create environment with the parameter
    ParamScheme = catena_type_scheme:mono(ParamType),
    Env = catena_type_env:extend(catena_type_env:empty(), ParamName, ParamScheme),
    
    % Infer the body
    case infer_expr_with_env(Body, Env) of
        {ok, BodyType} ->
            % Create function type
            Effects = catena_types:empty_effects(),
            FunType = catena_types:tfun(ParamType, BodyType, Effects),
            {ok, FunType};
        {error, Errors} ->
            {error, Errors}
    end.

%% @doc Check if an expression has a specific type (type annotation check)
%% Returns true if the inferred type matches the expected type
-spec check_type(catena_ast:expr(), catena_types:type()) -> boolean().
check_type(Expr, ExpectedType) ->
    case infer_expr(Expr) of
        {ok, InferredType} ->
            % Try to unify the inferred type with expected type
            State0 = catena_infer_state:new(),
            case catena_infer_unify:unify(InferredType, ExpectedType, State0) of
                {ok, _Subst, _State1} -> true;
                {error, _Error, _State1} -> false
            end;
        {error, _Errors} ->
            false
    end.

%% @doc Format type errors for human consumption
%% Provides a clean, readable format for type error messages
-spec format_errors([catena_type_error:type_error()]) -> string().
format_errors(Errors) ->
    Strings = lists:map(
        fun(Error) ->
            catena_type_error:format_error(Error)
        end,
        Errors
    ),
    lists:flatten(lists:join("\n", Strings)).

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

%% @doc Solve accumulated constraints
%%
%% Uses the instance resolution system to verify that all trait constraints
%% can be satisfied. Returns ok if all constraints are satisfied, or an error
%% list if any constraints cannot be resolved.
%%
%% For now, uses an empty instance database. In the future, this will be
%% extended to include built-in trait instances.
-spec solve_constraints(catena_constraint:constraint_set()) ->
    ok | {error, [catena_type_error:type_error()]}.
solve_constraints([]) ->
    % No constraints to solve
    ok;
solve_constraints(Constraints) ->
    % Create instance database with built-in instances
    DB = catena_builtin_instances:builtin_instance_db(),

    % Attempt to resolve all constraints
    case catena_instance:resolve_constraints(Constraints, DB) of
        {ok, _Solutions} ->
            % All constraints successfully resolved
            ok;
        {error, {Constraint, Reason}} ->
            % Convert constraint resolution failure to type error
            Error = constraint_error(Constraint, Reason),
            {error, [Error]}
    end.

%% @doc Convert a constraint resolution failure to a type error
-spec constraint_error(catena_constraint:constraint(), term()) ->
    catena_type_error:type_error().
constraint_error({trait, TraitName, TypeArgs, _Loc}, Reason) ->
    catena_type_error:unsatisfied_constraint(TraitName, TypeArgs, Reason).

%% @doc Create a fresh type environment with common primitives
%% This is useful for testing and interactive use
-spec fresh_env_with_primitives() -> catena_type_env:env().
fresh_env_with_primitives() ->
    % Add common primitive types and functions
    Primitives = [
        {int, catena_type_scheme:mono(catena_types:tcon(int))},
        {float, catena_type_scheme:mono(catena_types:tcon(float))},
        {bool, catena_type_scheme:mono(catena_types:tcon(bool))},
        {string, catena_type_scheme:mono(catena_types:tcon(string))}
    ],
    lists:foldl(
        fun({Name, Type}, Env) ->
            catena_type_env:extend(Env, Name, Type)
        end,
        catena_type_env:empty(),
        Primitives
    ).