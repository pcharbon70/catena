%%%-------------------------------------------------------------------
%%% @doc Expression Type Inference (Algorithm W)
%%%
%%% This module implements the core of Algorithm W - type inference for
%%% expressions. It generates constraints, performs unification, and
%%% implements let-polymorphism through generalization and instantiation.
%%%
%%% Expression inference returns:
%%% - The inferred type of the expression
%%% - Updated inference state (with substitutions and errors)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_infer_expr).

-export([
    infer/3,
    instantiate/2,
    generalize/3
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Infer the type of an expression
%%
%% **Pattern 1 Error:** Returns {error, Error, State} for inference errors
%% since this function threads inference state through the expression
%% inference process.
-spec infer(catena_ast:expr(), catena_type_env:env(), catena_infer_state:infer_state()) ->
    {catena_types:type(), catena_infer_state:infer_state()} |
    {error, catena_type_error:type_error(), catena_infer_state:infer_state()}.

% Expression: Literal
% 42 : Int
infer({lit, Lit}, _Env, State) ->
    Type = catena_infer_utils:literal_type(Lit),
    {Type, State};

% Expression: Variable
% x : instantiate(Γ(x))
infer({var, Name}, Env, State) ->
    case catena_type_env:lookup(Env, Name) of
        {ok, Scheme} ->
            {Type, Constraints, State1} = instantiate(Scheme, State),
            % Add instantiated constraints to the state
            State2 = catena_infer_state:add_constraints(Constraints, State1),
            {Type, State2};
        none ->
            Error = catena_type_error:unbound_variable(Name),
            State1 = catena_infer_state:add_error(Error, State),
            {error, Error, State1}
    end;

% Expression: Lambda abstraction
% λx.e : α → T  where e : T in Γ[x : α]
infer({lam, Param, Body}, Env, State) ->
    % Generate fresh type variable for parameter
    {ParamType, State1} = catena_infer_state:fresh_var(State),

    % Extend environment with parameter binding
    ParamScheme = catena_type_scheme:mono(ParamType),
    Env1 = catena_type_env:extend(Env, Param, ParamScheme),

    % Infer body type
    case infer(Body, Env1, State1) of
        {BodyType, State2} ->
            % Function type: ParamType -> BodyType (pure for now)
            FunType = {tfun, ParamType, BodyType, {effect_set, []}},
            {FunType, State2};
        {error, _, _} = Error ->
            Error
    end;

% Expression: Function application
% e1 e2 : β  where e1 : T1, e2 : T2, unify(T1, T2 → β)
infer({app, Fun, Arg}, Env, State) ->
    % Infer function type
    case infer(Fun, Env, State) of
        {FunType, State1} ->
            % Infer argument type
            case infer(Arg, Env, State1) of
                {ArgType, State2} ->
                    % Generate fresh type variable for result
                    {ResultType, State3} = catena_infer_state:fresh_var(State2),

                    % Unify function type with ArgType → ResultType
                    ExpectedFunType = {tfun, ArgType, ResultType, {effect_set, []}},
                    case catena_infer_unify:unify(FunType, ExpectedFunType, State3) of
                        {ok, _Subst, State4} ->
                            % Apply current substitution to result type
                            FinalSubst = catena_infer_state:get_subst(State4),
                            FinalResultType = catena_type_subst:apply(FinalSubst, ResultType),
                            {FinalResultType, State4};
                        {error, _, _} = Error ->
                            Error
                    end;
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end;

% Expression: Let binding (non-recursive)
% let x = e1 in e2 : T2  where e1 : T1, e2 : T2 in Γ[x : ∀ᾱ.T1]
infer({'let', Name, Expr, Body}, Env, State) ->
    % Infer type of bound expression
    case infer(Expr, Env, State) of
        {ExprType, State1} ->
            % Apply current substitution to environment and expression type
            Subst = catena_infer_state:get_subst(State1),
            ExprType1 = catena_type_subst:apply(Subst, ExprType),

            % Generalize the type (introduce ∀ quantifiers)
            Scheme = generalize(ExprType1, Env, State1),

            % Extend environment with generalized binding
            Env1 = catena_type_env:extend(Env, Name, Scheme),

            % Infer body type
            infer(Body, Env1, State1);
        {error, _, _} = Error ->
            Error
    end;

% Expression: Let-rec binding (recursive)
% For PoC: simplified - just bind as monomorphic
infer({'letrec', Name, Expr, Body}, Env, State) ->
    % Generate fresh type variable for recursive binding
    {RecType, State1} = catena_infer_state:fresh_var(State),

    % Extend environment with monomorphic binding
    RecScheme = catena_type_scheme:mono(RecType),
    Env1 = catena_type_env:extend(Env, Name, RecScheme),

    % Infer type of expression in extended environment
    case infer(Expr, Env1, State1) of
        {ExprType, State2} ->
            % Unify recursive type with expression type
            case catena_infer_unify:unify(RecType, ExprType, State2) of
                {ok, _Subst, State3} ->
                    % Infer body with recursive binding
                    infer(Body, Env1, State3);
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end;

% Expression: If-then-else
% if c then t else e : T  where c : Bool, t : T, e : T
infer({'if', Cond, Then, Else}, Env, State) ->
    % Infer condition type
    case infer(Cond, Env, State) of
        {CondType, State1} ->
            % Unify condition with Bool
            case catena_infer_unify:unify(CondType, {tcon, bool}, State1) of
                {ok, _Subst1, State2} ->
                    % Infer then branch
                    case infer(Then, Env, State2) of
                        {ThenType, State3} ->
                            % Infer else branch
                            case infer(Else, Env, State3) of
                                {ElseType, State4} ->
                                    % Unify both branches
                                    case catena_infer_unify:unify(ThenType, ElseType, State4) of
                                        {ok, _Subst2, State5} ->
                                            % Apply final substitution to then type
                                            FinalSubst = catena_infer_state:get_subst(State5),
                                            FinalType = catena_type_subst:apply(FinalSubst, ThenType),
                                            {FinalType, State5};
                                        {error, _, _} = Error ->
                                            Error
                                    end;
                                {error, _, _} = Error ->
                                    Error
                            end;
                        {error, _, _} = Error ->
                            Error
                    end;
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end;

% Expression: Tuple construction
% (e1, ..., en) : (T1, ..., Tn)
infer({tuple, Elements}, Env, State) ->
    case infer_exprs(Elements, Env, State) of
        {Types, State1} ->
            {{ttuple, Types}, State1};
        {error, _, _} = Error ->
            Error
    end;

% Expression: Record construction
% {l1: e1, ..., ln: en} : {l1: T1, ..., ln: Tn | closed}
infer({record, Fields}, Env, State) ->
    case infer_record_fields(Fields, Env, State) of
        {FieldTypes, State1} ->
            {{trecord, FieldTypes, closed}, State1};
        {error, _, _} = Error ->
            Error
    end;

% Expression: Record field access
% e.field : T  where e : {..., field: T, ... | ρ}
infer({field, Expr, FieldName}, Env, State) ->
    case infer(Expr, Env, State) of
        {ExprType, State1} ->
            % Generate fresh type variable for field
            {FieldType, State2} = catena_infer_state:fresh_var(State1),

            % Generate fresh row variable
            {RowVar, State3} = catena_infer_state:fresh_var(State2),
            {tvar, RowVarId} = RowVar,

            % Expected record type with this field
            ExpectedType = {trecord, [{FieldName, FieldType}], RowVarId},

            % Unify expression type with expected record type
            case catena_infer_unify:unify(ExprType, ExpectedType, State3) of
                {ok, _Subst, State4} ->
                    % Apply substitution to field type
                    FinalSubst = catena_infer_state:get_subst(State4),
                    FinalFieldType = catena_type_subst:apply(FinalSubst, FieldType),
                    {FinalFieldType, State4};
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end;

% Expression: Variant constructor
% C e1 ... en : [... | C T1 ... Tn | ...]
infer({variant, Constructor, Args}, Env, State) ->
    case infer_exprs(Args, Env, State) of
        {ArgTypes, State1} ->
            Type = {tvariant, [{Constructor, ArgTypes}]},
            {Type, State1};
        {error, _, _} = Error ->
            Error
    end;

% Expression: Type annotation
% (e : T) : T  where e : T' and unify(T, T')
infer({ann, Expr, AnnotType}, Env, State) ->
    case infer(Expr, Env, State) of
        {ExprType, State1} ->
            % Unify inferred type with annotation
            case catena_infer_unify:unify(ExprType, AnnotType, State1) of
                {ok, _Subst, State2} ->
                    % Return annotated type
                    FinalSubst = catena_infer_state:get_subst(State2),
                    FinalType = catena_type_subst:apply(FinalSubst, AnnotType),
                    {FinalType, State2};
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end.

%% @doc Instantiate a type scheme by replacing quantified variables with fresh ones
%%
%% TODO: Consider consolidating with catena_type_scheme:instantiate/2.
%% This version differs by also applying the current state's substitution
%% to the result, which is needed during inference. A careful analysis is
%% needed before consolidation to preserve this behavior.
-spec instantiate(catena_type_scheme:scheme(), catena_infer_state:infer_state()) ->
    {catena_types:type(), catena_constraint:constraint_set(), catena_infer_state:infer_state()}.
instantiate({mono, Type}, State) ->
    % Monomorphic scheme - apply current substitution
    Subst = catena_infer_state:get_subst(State),
    InstType = catena_type_subst:apply(Subst, Type),
    {InstType, [], State};
instantiate({mono, Type, Constraints}, State) ->
    % Monomorphic with constraints - apply current substitution to both
    Subst = catena_infer_state:get_subst(State),
    InstType = catena_type_subst:apply(Subst, Type),
    InstConstraints = catena_constraint:substitute(Subst, Constraints),
    {InstType, InstConstraints, State};
instantiate({poly, Quantified, Type}, State) ->
    % Polymorphic scheme - replace quantified vars with fresh ones
    {FreshVars, State1} = catena_infer_state:fresh_vars(length(Quantified), State),

    % Build substitution from quantified vars to fresh vars
    Pairs = lists:zip(Quantified, FreshVars),
    InstSubst = maps:from_list(Pairs),

    % Apply instantiation substitution to type
    Type1 = catena_type_subst:apply(InstSubst, Type),

    % Apply current substitution from state
    CurrentSubst = catena_infer_state:get_subst(State1),
    InstType = catena_type_subst:apply(CurrentSubst, Type1),

    {InstType, [], State1};
instantiate({poly, Quantified, Constraints, Type}, State) ->
    % Qualified polymorphic - replace quantified vars with fresh ones
    {FreshVars, State1} = catena_infer_state:fresh_vars(length(Quantified), State),

    % Build substitution from quantified vars to fresh vars
    Pairs = lists:zip(Quantified, FreshVars),
    InstSubst = maps:from_list(Pairs),

    % Apply instantiation substitution to type and constraints
    Type1 = catena_type_subst:apply(InstSubst, Type),
    Constraints1 = catena_constraint:substitute(InstSubst, Constraints),

    % Apply current substitution from state
    CurrentSubst = catena_infer_state:get_subst(State1),
    InstType = catena_type_subst:apply(CurrentSubst, Type1),
    InstConstraints = catena_constraint:substitute(CurrentSubst, Constraints1),

    {InstType, InstConstraints, State1}.

%% @doc Generalize a type by quantifying over free variables
%% Variables free in the type but not in the environment are generalized
%% Also generalizes constraints accumulated during inference
-spec generalize(catena_types:type(), catena_type_env:env(), catena_infer_state:infer_state()) ->
    catena_type_scheme:scheme().
generalize(Type, Env, State) ->
    % Apply current substitution to type and constraints
    Subst = catena_infer_state:get_subst(State),
    Type1 = catena_type_subst:apply(Subst, Type),

    % Get accumulated constraints and apply substitution
    Constraints = catena_infer_state:get_constraints(State),
    Constraints1 = catena_constraint:substitute(Subst, Constraints),

    % Get free variables in environment
    EnvVars = catena_type_env:ftv_env(Env),

    % Use catena_type_scheme:generalize/3 which handles both type and constraint vars
    catena_type_scheme:generalize(Type1, Constraints1, EnvVars).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Infer types for a list of expressions
-spec infer_exprs([catena_ast:expr()], catena_type_env:env(), catena_infer_state:infer_state()) ->
    {[catena_types:type()], catena_infer_state:infer_state()} |
    {error, catena_type_error:type_error(), catena_infer_state:infer_state()}.
infer_exprs(Exprs, Env, State) ->
    infer_exprs_acc(Exprs, Env, State, []).

infer_exprs_acc([], _Env, State, TypesAcc) ->
    {lists:reverse(TypesAcc), State};
infer_exprs_acc([E | Rest], Env, State, TypesAcc) ->
    case infer(E, Env, State) of
        {Type, State1} ->
            infer_exprs_acc(Rest, Env, State1, [Type | TypesAcc]);
        {error, _, _} = Error ->
            Error
    end.

%% @doc Infer types for record fields
-spec infer_record_fields([{atom(), catena_ast:expr()}], catena_type_env:env(),
                         catena_infer_state:infer_state()) ->
    {[{atom(), catena_types:type()}], catena_infer_state:infer_state()} |
    {error, catena_type_error:type_error(), catena_infer_state:infer_state()}.
infer_record_fields(Fields, Env, State) ->
    infer_record_fields_acc(Fields, Env, State, []).

infer_record_fields_acc([], _Env, State, FieldsAcc) ->
    {lists:reverse(FieldsAcc), State};
infer_record_fields_acc([{Label, Expr} | Rest], Env, State, FieldsAcc) ->
    case infer(Expr, Env, State) of
        {Type, State1} ->
            infer_record_fields_acc(Rest, Env, State1, [{Label, Type} | FieldsAcc]);
        {error, _, _} = Error ->
            Error
    end.


