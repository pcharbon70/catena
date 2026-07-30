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
-spec infer(catena_infer_ast:expr(), catena_type_env:env(), catena_infer_state:infer_state()) ->
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
infer({var, Name, _Location}, Env, State) ->
    infer({var, Name}, Env, State);

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

                    % Preserve the callable's declared effects while
                    % constraining its argument and result types.
                    FunctionEffects = operation_function_effects(FunType),
                    ExpectedFunType = {
                        tfun,
                        ArgType,
                        ResultType,
                        FunctionEffects
                    },
                    case catena_infer_unify:unify(FunType, ExpectedFunType, State3) of
                        {ok, _Subst, State4} ->
                            % Apply current substitution to result type
                            FinalSubst = catena_infer_state:get_subst(State4),
                            FinalResultType = catena_type_subst:apply(FinalSubst, ResultType),
                            {
                                FinalResultType,
                                add_effect_set(FunctionEffects, State4)
                            };
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
    end;

% Expression: Binary operation
% e1 op e2 : T  (type depends on operator)
infer({binary_op, Op, Left, Right, _Loc}, Env, State) ->
    case infer(Left, Env, State) of
        {LeftType, State1} ->
            case infer(Right, Env, State1) of
                {RightType, State2} ->
                    infer_binary_op(Op, LeftType, RightType, State2);
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end;

% Expression: List literal
% [e1, e2, ..., en] : List T  where all ei : T
infer({list, Elements, _Loc}, Env, State) ->
    case Elements of
        [] ->
            % Empty list - polymorphic [] : List a
            {ElemType, State1} = catena_infer_state:fresh_var(State),
            {{tapp, {tcon, list}, [ElemType]}, State1};
        [First | Rest] ->
            case infer(First, Env, State) of
                {FirstType, State1} ->
                    % Infer rest and unify with first element type
                    case infer_list_elements(Rest, FirstType, Env, State1) of
                        {ElemType, State2} ->
                            {{tapp, {tcon, list}, [ElemType]}, State2};
                        {error, _, _} = Error ->
                            Error
                    end;
                {error, _, _} = Error ->
                    Error
            end
    end;

% Expression: Cons (list construction)
% h :: t : List a  where h : a, t : List a
infer({cons, Head, Tail, _Loc}, Env, State) ->
    case infer(Head, Env, State) of
        {HeadType, State1} ->
            case infer(Tail, Env, State1) of
                {TailType, State2} ->
                    % Expected tail type is List HeadType
                    ExpectedTail = {tapp, {tcon, list}, [HeadType]},
                    case catena_infer_unify:unify(TailType, ExpectedTail, State2) of
                        {ok, _Subst, State3} ->
                            FinalSubst = catena_infer_state:get_subst(State3),
                            FinalElemType = catena_type_subst:apply(FinalSubst, HeadType),
                            {{tapp, {tcon, list}, [FinalElemType]}, State3};
                        {error, _, _} = Error ->
                            Error
                    end;
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end;

% Expression: Match (pattern matching)
% match e of | p1 -> e1 | ... | pn -> en end : T
infer({'match', Scrutinee, Clauses, _Loc}, Env, State) ->
    infer_match(Scrutinee, Clauses, Env, State);

% Expression: Match alternative form (from REPL)
infer({match, Scrutinee, Clauses}, Env, State) ->
    infer_match(Scrutinee, Clauses, Env, State);

% Expression: Constructor application
% C e1 ... en : T  where C is a data constructor
infer({constructor, Name, Args, _Loc}, Env, State) ->
    case infer_exprs(Args, Env, State) of
        {ArgTypes, State1} ->
            Type = {tvariant, [{Name, ArgTypes}]},
            {Type, State1};
        {error, _, _} = Error ->
            Error
    end;

% Expression: Literal (alternative format from parser)
infer({literal, Type, Value, _Loc}, _Env, State) ->
    InferredType = case Type of
        integer -> {tcon, int};
        float -> {tcon, float};
        string -> {tcon, string};
        bool -> {tcon, bool};
        atom -> {tcon, atom};
        _ -> {tcon, Type}
    end,
    _ = Value, % Value is not needed for type inference
    {InferredType, State};

% Expression: Identifier (alternative var format from parser)
infer({identifier, Name, _Loc}, Env, State) ->
    infer({var, Name}, Env, State);

%%%===================================================================
%%% Effect Expressions (Section 1.5.6)
%%%===================================================================

% Expression: Perform (introduces effects)
% perform Effect.operation(args) : ResultType / {Effect}
%
% The perform expression introduces an effect into the function's effect set.
% The result type depends on the operation's declared type.
infer({perform_expr, EffectName, OperationName, Args, Loc}, Env, State) ->
    %% Infer types of arguments
    case infer_exprs(Args, Env, State) of
        {ArgTypes, State1} ->
            Binding = catena_effect_resolution:binding_name(
                EffectName,
                OperationName
            ),
            case catena_type_env:lookup(Env, Binding) of
                {ok, Scheme} ->
                    {OperationType, Constraints, State2} =
                        instantiate(Scheme, State1),
                    State3 = catena_infer_state:add_constraints(
                        Constraints,
                        State2
                    ),
                    case infer_operation_application(
                        OperationType,
                        ArgTypes,
                        State3
                    ) of
                        {ResultType, State4} ->
                            State5 = catena_infer_state:
                                add_performed_operation(
                                    EffectName,
                                    OperationName,
                                    ResultType,
                                    Loc,
                                    State4
                                ),
                            State6 = catena_infer_state:add_effect(
                                EffectName,
                                State5
                            ),
                            {ResultType, State6};
                        {error, _, _} = Error ->
                            Error
                    end;
                none ->
                    %% Direct inference-unit callers do not carry module
                    %% declarations. Source compilation resolves every
                    %% operation before entering inference, while this
                    %% compatibility path retains the historical fresh result.
                    {ResultType, State2} =
                        catena_infer_state:fresh_var(State1),
                    State3 = catena_infer_state:
                        add_performed_operation(
                            EffectName,
                            OperationName,
                            ResultType,
                            Loc,
                            State2
                        ),
                    State4 = catena_infer_state:add_effect(
                        EffectName,
                        State3
                    ),
                    {ResultType, State4}
            end;
        {error, _, _} = Error ->
            Error
    end;

% Expression: Resume a typed first-class capability.
infer({resume_expr, Target, Value, Location}, Env, State) ->
    infer_resume(Target, Value, Location, Env, State);

% Expression: Handle establishes the static delimiter used by every operation
% case. Effects in the handled computation are isolated so the residual row
% can be derived before handler bodies are checked.
infer({handle_expr, Body, Handlers, Location}, Env, State) ->
    ScopedState = catena_infer_state:push_effect_scope(State),
    PerformedBefore = catena_infer_state:get_performed_operations(
        ScopedState
    ),
    case infer(Body, Env, ScopedState) of
        {BodyType, BodyState} ->
            BodyPerformed = newly_performed_operations(
                PerformedBefore,
                catena_infer_state:get_performed_operations(BodyState)
            ),
            HandlerEnv = add_performed_operation_metadata(
                BodyPerformed,
                Env
            ),
            BodyEffects = catena_infer_state:get_effects(BodyState),
            HandledEffects = [
                Effect
                || {handler_clause, Effect, _Operations, _HandlerLocation} <-
                    Handlers
            ],
            ResidualEffects = lists:foldl(
                fun catena_types:remove_effect/2,
                BodyEffects,
                HandledEffects
            ),
            ResidualRow = effect_set_to_row(ResidualEffects),
            HandlerState0 = catena_infer_state:set_effects(
                catena_types:empty_effects(),
                BodyState
            ),
            case infer_handler_clauses(
                Handlers,
                BodyType,
                ResidualRow,
                Location,
                HandlerEnv,
                HandlerState0
            ) of
                {ok, HandlerState1} ->
                    HandlerEffects =
                        catena_infer_state:get_effects(HandlerState1),
                    CombinedEffects = catena_types:union_effects(
                        ResidualEffects,
                        HandlerEffects
                    ),
                    FinalState0 = catena_infer_state:set_effects(
                        CombinedEffects,
                        HandlerState1
                    ),
                    FinalSubstitution =
                        catena_infer_state:get_subst(FinalState0),
                    {
                        catena_type_subst:apply(
                            FinalSubstitution,
                            BodyType
                        ),
                        catena_infer_state:pop_effect_scope(FinalState0)
                    };
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end.

infer_resume(Target, Value, Location, Env, State) ->
    case infer(Target, Env, State) of
        {TargetType, State1} ->
            case infer(Value, Env, State1) of
                {ValueType, State2} ->
                    infer_resume_operands(
                        Target,
                        TargetType,
                        ValueType,
                        Location,
                        Env,
                        State2
                    );
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end.

infer_resume_operands(
    Target,
    TargetType,
    ValueType,
    Location,
    Env,
    State
) ->
    {KindId, State1} = catena_infer_state:fresh_var_id(State),
    {OperationResult, State2} = catena_infer_state:fresh_var(State1),
    {DelimiterResult, State3} = catena_infer_state:fresh_var(State2),
    {RowId, State4} = catena_infer_state:fresh_var_id(State3),
    ExpectedTarget = catena_types:tresumption(
        catena_types:resumption_kind_var(KindId),
        OperationResult,
        DelimiterResult,
        catena_types:teffectrow([], RowId)
    ),
    case valid_resume_target_shape(TargetType) of
        false ->
            resumption_inference_error(
                invalid_resume_target,
                resume_error_context(
                    Target,
                    Location,
                    Env,
                    #{
                        expected_type => ExpectedTarget,
                        actual_type => TargetType
                    }
                ),
                State4
            );
        true ->
            case catena_infer_unify:unify(
                TargetType,
                ExpectedTarget,
                State4
            ) of
                {ok, _TargetSubstitution, State5} ->
                    CurrentSubstitution =
                        catena_infer_state:get_subst(State5),
                    ExpectedValue = catena_type_subst:apply(
                        CurrentSubstitution,
                        OperationResult
                    ),
                    case catena_infer_unify:unify(
                        ValueType,
                        ExpectedValue,
                        State5
                    ) of
                        {ok, _ValueSubstitution, State6} ->
                            FinalSubstitution =
                                catena_infer_state:get_subst(State6),
                            FinalTarget = catena_type_subst:apply(
                                FinalSubstitution,
                                ExpectedTarget
                            ),
                            {
                                tresumption,
                                _Kind,
                                _A,
                                FinalDelimiterResult,
                                FinalEffects
                            } = FinalTarget,
                            Evidence = resume_error_context(
                                Target,
                                Location,
                                Env,
                                #{
                                    kind => resume,
                                    type => FinalTarget,
                                    supplied_type =>
                                        catena_type_subst:apply(
                                            FinalSubstitution,
                                            ValueType
                                        )
                                }
                            ),
                            case catena_resumption_flow:
                                validate_supported_mode(
                                    FinalTarget,
                                    Evidence
                                )
                            of
                                ok ->
                                    State7 = add_residual_effect_row(
                                        FinalEffects,
                                        State6
                                    ),
                                    {
                                        FinalDelimiterResult,
                                        catena_infer_state:
                                            add_resumption_evidence(
                                                Evidence,
                                                State7
                                            )
                                    };
                                {error, {FlowReason, FlowContext}} ->
                                    resumption_inference_error(
                                        FlowReason,
                                        FlowContext,
                                        State6
                                    )
                            end;
                        {error, Reason, ErrorState} ->
                            resumption_inference_error(
                                resume_value_type_mismatch,
                                resume_error_context(
                                    Target,
                                    Location,
                                    Env,
                                    #{
                                        expected_type => ExpectedValue,
                                        actual_type => ValueType,
                                        unification => Reason
                                    }
                                ),
                                ErrorState
                            )
                    end;
                {error, Reason, ErrorState} ->
                    resumption_inference_error(
                        invalid_resume_target,
                        resume_error_context(
                            Target,
                            Location,
                            Env,
                            #{
                                expected_type => ExpectedTarget,
                                actual_type => TargetType,
                                unification => Reason
                            }
                        ),
                        ErrorState
                    )
            end
    end.

valid_resume_target_shape({tresumption, _, _, _, _}) -> true;
valid_resume_target_shape({tvar, _}) -> true;
valid_resume_target_shape(_) -> false.

resume_error_context(Target, Location, Env, Extra) ->
    Authority = case resume_target_name(Target) of
        {ok, Name} ->
            case catena_type_env:lookup_metadata(
                Env,
                {resumption_authority, Name}
            ) of
                {ok, Metadata} -> Metadata;
                none -> #{target => Name}
            end;
        none ->
            #{target => Target}
    end,
    maps:merge(
        Authority,
        Extra#{
            resume_location => Location,
            target_origin => expression_origin(Target)
        }
    ).

resume_target_name({var, Name}) when is_atom(Name) -> {ok, Name};
resume_target_name({var, Name, _Location}) when is_atom(Name) -> {ok, Name};
resume_target_name(_) -> none.

expression_origin({var, _Name, Location}) -> Location;
expression_origin(_) -> undefined.

resumption_inference_error(Reason, Context, State) ->
    Error = {Reason, Context},
    ErrorState = catena_infer_state:add_error(Error, State),
    {error, Error, ErrorState}.

infer_handler_clauses(
    [],
    _DelimiterResult,
    _ResidualRow,
    _DelimiterLocation,
    _Env,
    State
) ->
    {ok, State};
infer_handler_clauses(
    [
        {handler_clause, Effect, Operations, _HandlerLocation}
        | Rest
    ],
    DelimiterResult,
    ResidualRow,
    DelimiterLocation,
    Env,
    State
) ->
    case infer_handler_operations(
        Effect,
        Operations,
        DelimiterResult,
        ResidualRow,
        DelimiterLocation,
        Env,
        State
    ) of
        {ok, State1} ->
            infer_handler_clauses(
                Rest,
                DelimiterResult,
                ResidualRow,
                DelimiterLocation,
                Env,
                State1
            );
        {error, _, _} = Error ->
            Error
    end.

infer_handler_operations(
    _Effect,
    [],
    _DelimiterResult,
    _ResidualRow,
    _DelimiterLocation,
    _Env,
    State
) ->
    {ok, State};
infer_handler_operations(
    Effect,
    [
        {
            operation_case,
            Operation,
            Patterns,
            {resumption_binder, Binder, BinderOrigin},
            CaseBody,
            CaseLocation
        }
        | Rest
    ],
    DelimiterResult,
    ResidualRow,
    DelimiterLocation,
    Env,
    State
) ->
    Binding = catena_effect_resolution:binding_name(Effect, Operation),
    case catena_type_env:lookup(Env, Binding) of
        {ok, OperationScheme} ->
            {
                OperationType,
                Constraints,
                State1
            } = instantiate(OperationScheme, State),
            State2 = catena_infer_state:add_constraints(
                Constraints,
                State1
            ),
            case infer_operation_patterns(
                Patterns,
                OperationType,
                Env,
                State2
            ) of
                {ok, OperationResult, PatternBindings, State3} ->
                    case constrain_operation_result(
                        Effect,
                        Operation,
                        OperationResult,
                        Env,
                        State3
                    ) of
                        {
                            ok,
                            ConstrainedOperationResult,
                            State3a
                        } ->
                            ResumptionType = catena_types:tresumption(
                                catena_types:one_shot(),
                                ConstrainedOperationResult,
                                DelimiterResult,
                                ResidualRow
                            ),
                            DeclarationOrigin =
                                operation_declaration_origin(
                                    Effect,
                                    Operation,
                                    Env
                                ),
                            BinderEvidence = #{
                                kind => resumption_binder,
                                binder => Binder,
                                type => ResumptionType,
                                mode => one_shot,
                                effect => Effect,
                                operation => Operation,
                                binder_origin => BinderOrigin,
                                operation_declaration =>
                                    DeclarationOrigin,
                                delimiter_location =>
                                    DelimiterLocation,
                                case_location => CaseLocation,
                                residual_effects => ResidualRow
                            },
                            CaseEnv0 = catena_type_env:merge(
                                Env,
                                PatternBindings
                            ),
                            CaseEnv1 = catena_type_env:extend(
                                CaseEnv0,
                                Binder,
                                catena_type_scheme:mono(
                                    ResumptionType
                                )
                            ),
                            CaseEnv = catena_type_env:put_metadata(
                                CaseEnv1,
                                {resumption_authority, Binder},
                                BinderEvidence
                            ),
                            State4 = catena_infer_state:
                                add_resumption_evidence(
                                    BinderEvidence,
                                    State3a
                                ),
                            case catena_resumption_flow:
                                validate_one_shot_case(
                                    Binder,
                                    ResumptionType,
                                    CaseBody,
                                    BinderEvidence
                                )
                            of
                                ok ->
                                    infer_handler_case_body(
                                        Effect,
                                        Rest,
                                        DelimiterResult,
                                        ResidualRow,
                                        DelimiterLocation,
                                        Env,
                                        CaseBody,
                                        CaseEnv,
                                        BinderEvidence,
                                        State4
                                    );
                                {
                                    error,
                                    {FlowReason, FlowContext}
                                } ->
                                    resumption_inference_error(
                                        FlowReason,
                                        FlowContext,
                                        State4
                                    )
                            end;
                        {error, _, _} = Error ->
                            Error
                    end;
                {error, _, _} = Error ->
                    Error
            end;
        none ->
            case compatibility_operation_type(
                Effect,
                Operation,
                Patterns,
                Env,
                State
            ) of
                {ok, OperationType, State1} ->
                    CompatibilityEnv = catena_type_env:extend(
                        Env,
                        Binding,
                        catena_type_scheme:mono(OperationType)
                    ),
                    infer_handler_operations(
                        Effect,
                        [
                            {
                                operation_case,
                                Operation,
                                Patterns,
                                {
                                    resumption_binder,
                                    Binder,
                                    BinderOrigin
                                },
                                CaseBody,
                                CaseLocation
                            }
                            | Rest
                        ],
                        DelimiterResult,
                        ResidualRow,
                        DelimiterLocation,
                        CompatibilityEnv,
                        State1
                    );
                none ->
                    resumption_inference_error(
                        unknown_handler_operation_type,
                        #{
                            effect => Effect,
                            operation => Operation,
                            case_location => CaseLocation
                        },
                        State
                    )
            end
    end;
infer_handler_operations(
    Effect,
    [Invalid | _Rest],
    _DelimiterResult,
    _ResidualRow,
    _DelimiterLocation,
    _Env,
    State
) ->
    resumption_inference_error(
        invalid_resumption_binder,
        #{effect => Effect, source_term => Invalid},
        State
    ).

infer_handler_case_body(
    Effect,
    Rest,
    DelimiterResult,
    ResidualRow,
    DelimiterLocation,
    Env,
    CaseBody,
    CaseEnv,
    BinderEvidence,
    State
) ->
    case infer(CaseBody, CaseEnv, State) of
        {CaseType, State1} ->
            case catena_infer_unify:unify(
                CaseType,
                DelimiterResult,
                State1
            ) of
                {ok, _CaseSubstitution, State2} ->
                    infer_handler_operations(
                        Effect,
                        Rest,
                        DelimiterResult,
                        ResidualRow,
                        DelimiterLocation,
                        Env,
                        State2
                    );
                {error, Reason, ErrorState} ->
                    resumption_inference_error(
                        resume_delimiter_type_mismatch,
                        BinderEvidence#{
                            expected_type => DelimiterResult,
                            actual_type => CaseType,
                            unification => Reason
                        },
                        ErrorState
                    )
            end;
        {error, _, _} = Error ->
            Error
    end.

infer_operation_patterns(Patterns, OperationType, Env, State) ->
    infer_operation_patterns(
        Patterns,
        OperationType,
        Env,
        State,
        catena_type_env:empty()
    ).

infer_operation_patterns(
    [],
    OperationResult,
    _Env,
    State,
    Bindings
) ->
    Substitution = catena_infer_state:get_subst(State),
    {ok,
        catena_type_subst:apply(Substitution, OperationResult),
        Bindings,
        State};
infer_operation_patterns(
    [Pattern | Rest],
    {tfun, ExpectedPatternType, RemainingType, _Effects},
    Env,
    State,
    Bindings
) ->
    {
        PatternType,
        PatternBindings,
        State1
    } = catena_infer_pattern:infer(Pattern, Env, State),
    case catena_infer_unify:unify(
        PatternType,
        ExpectedPatternType,
        State1
    ) of
        {ok, _Substitution, State2} ->
            CurrentSubstitution = catena_infer_state:get_subst(State2),
            infer_operation_patterns(
                Rest,
                catena_type_subst:apply(
                    CurrentSubstitution,
                    RemainingType
                ),
                Env,
                State2,
                catena_type_env:merge(Bindings, PatternBindings)
            );
        {error, _, _} = Error ->
            Error
    end;
infer_operation_patterns(
    Patterns,
    OperationType,
    _Env,
    State,
    _Bindings
) ->
    resumption_inference_error(
        handler_operation_arity_mismatch,
        #{
            operation_type => OperationType,
            remaining_patterns => Patterns
        },
        State
    ).

constrain_operation_result(Effect, Operation, ResultType, Env, State) ->
    Key = {performed_operation_results, Effect, Operation},
    Results = case catena_type_env:lookup_metadata(Env, Key) of
        {ok, Operations} -> Operations;
        none -> []
    end,
    constrain_operation_results(
        Results,
        ResultType,
        State
    ).

constrain_operation_results([], ResultType, State) ->
    Substitution = catena_infer_state:get_subst(State),
    {
        ok,
        catena_type_subst:apply(Substitution, ResultType),
        State
    };
constrain_operation_results(
    [Operation | Rest],
    ResultType,
    State
) ->
    PerformedResult = maps:get(result_type, Operation),
    case catena_infer_unify:unify(
        ResultType,
        PerformedResult,
        State
    ) of
        {ok, _Substitution, State1} ->
            CurrentSubstitution =
                catena_infer_state:get_subst(State1),
            constrain_operation_results(
                Rest,
                catena_type_subst:apply(
                    CurrentSubstitution,
                    ResultType
                ),
                State1
            );
        {error, _, _} = Error ->
            Error
    end.

compatibility_operation_type(
    Effect,
    Operation,
    Patterns,
    Env,
    State
) ->
    Key = {performed_operation_results, Effect, Operation},
    case catena_type_env:lookup_metadata(Env, Key) of
        {ok, [Performed | _]} ->
            ResultType = maps:get(result_type, Performed),
            fresh_operation_argument_types(
                Patterns,
                ResultType,
                State
            );
        _ ->
            none
    end.

fresh_operation_argument_types([], ResultType, State) ->
    {ok, ResultType, State};
fresh_operation_argument_types([_Pattern | Rest], ResultType, State) ->
    {ArgumentType, State1} = catena_infer_state:fresh_var(State),
    case fresh_operation_argument_types(Rest, ResultType, State1) of
        {ok, RemainingType, State2} ->
            {
                ok,
                catena_types:tfun(
                    ArgumentType,
                    RemainingType,
                    catena_types:empty_effects()
                ),
                State2
            }
    end.

operation_declaration_origin(Effect, Operation, Env) ->
    case catena_type_env:lookup_metadata(
        Env,
        {effect_operation, Effect, Operation}
    ) of
        {ok, Metadata} -> Metadata;
        none -> undefined
    end.

newly_performed_operations(Before, After) ->
    NewCount = erlang:max(0, length(After) - length(Before)),
    lists:sublist(After, NewCount).

add_performed_operation_metadata(Operations, Env) ->
    lists:foldl(
        fun(Operation, CurrentEnv) ->
            Effect = maps:get(effect, Operation),
            Name = maps:get(operation, Operation),
            Key = {performed_operation_results, Effect, Name},
            Existing = case catena_type_env:lookup_metadata(
                CurrentEnv,
                Key
            ) of
                {ok, Results} -> Results;
                none -> []
            end,
            catena_type_env:put_metadata(
                CurrentEnv,
                Key,
                [Operation | Existing]
            )
        end,
        Env,
        Operations
    ).

effect_set_to_row({effect_set, Effects}) ->
    catena_types:teffectrow(Effects).

add_residual_effect_row({teffectrow, Effects, closed}, State) ->
    lists:foldl(
        fun catena_infer_state:add_effect/2,
        State,
        Effects
    );
add_residual_effect_row({teffectrow, _Effects, _Tail} = Row, State) ->
    %% Open rows are retained in the type/evidence. Known labels still
    %% participate in the monomorphic effect accumulator.
    {teffectrow, Effects, _} = Row,
    lists:foldl(
        fun catena_infer_state:add_effect/2,
        State,
        Effects
    ).

infer_operation_application(OperationType, [], State) ->
    Substitution = catena_infer_state:get_subst(State),
    {catena_type_subst:apply(Substitution, OperationType), State};
infer_operation_application(
    OperationType,
    [ArgumentType | Rest],
    State
) ->
    {ResultType, State1} = catena_infer_state:fresh_var(State),
    Effects = operation_function_effects(OperationType),
    ExpectedType = {tfun, ArgumentType, ResultType, Effects},
    case catena_infer_unify:unify(
        OperationType,
        ExpectedType,
        State1
    ) of
        {ok, _Substitution, State2} ->
            CurrentSubstitution = catena_infer_state:get_subst(State2),
            AppliedResult = catena_type_subst:apply(
                CurrentSubstitution,
                ResultType
            ),
            infer_operation_application(
                AppliedResult,
                Rest,
                State2
            );
        {error, _, _} = Error ->
            Error
    end.

operation_function_effects({tfun, _From, _To, Effects}) ->
    Effects;
operation_function_effects(_Type) ->
    catena_types:empty_effects().

add_effect_set({effect_set, Effects}, State) ->
    lists:foldl(
        fun catena_infer_state:add_effect/2,
        State,
        Effects
    ).

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
-spec infer_exprs([catena_infer_ast:expr()], catena_type_env:env(), catena_infer_state:infer_state()) ->
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
-spec infer_record_fields([{atom(), catena_infer_ast:expr()}], catena_type_env:env(),
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

%% @doc Infer type of binary operation
-spec infer_binary_op(atom(), catena_types:type(), catena_types:type(),
                      catena_infer_state:infer_state()) ->
    {catena_types:type(), catena_infer_state:infer_state()} |
    {error, catena_type_error:type_error(), catena_infer_state:infer_state()}.

% Arithmetic operators: +, -, *, /
infer_binary_op(Op, LeftType, RightType, State)
  when Op =:= plus; Op =:= minus; Op =:= star; Op =:= slash ->
    % Both operands must be numeric (int or float)
    case catena_infer_unify:unify(LeftType, RightType, State) of
        {ok, _Subst, State1} ->
            FinalSubst = catena_infer_state:get_subst(State1),
            ResultType = catena_type_subst:apply(FinalSubst, LeftType),
            % For now, accept any unified numeric type
            {ResultType, State1};
        {error, _, _} = Error ->
            Error
    end;

% Comparison operators: <, >, <=, >=
infer_binary_op(Op, LeftType, RightType, State)
  when Op =:= lt; Op =:= gt; Op =:= lte; Op =:= gte ->
    case catena_infer_unify:unify(LeftType, RightType, State) of
        {ok, _Subst, State1} ->
            % Comparison returns Bool
            {{tcon, bool}, State1};
        {error, _, _} = Error ->
            Error
    end;

% Equality operators: ==, /=
infer_binary_op(Op, LeftType, RightType, State)
  when Op =:= eq; Op =:= neq ->
    case catena_infer_unify:unify(LeftType, RightType, State) of
        {ok, _Subst, State1} ->
            {{tcon, bool}, State1};
        {error, _, _} = Error ->
            Error
    end;

% Setoid equality operators: ===, !==
infer_binary_op(Op, LeftType, RightType, State)
  when Op =:= setoid_eq; Op =:= setoid_neq ->
    case catena_infer_unify:unify(LeftType, RightType, State) of
        {ok, _Subst, State1} ->
            % TODO: Should generate Comparable constraint
            {{tcon, bool}, State1};
        {error, _, _} = Error ->
            Error
    end;

% Boolean operators: and, or
infer_binary_op(Op, LeftType, RightType, State)
  when Op =:= 'and'; Op =:= 'or' ->
    case catena_infer_unify:unify(LeftType, {tcon, bool}, State) of
        {ok, _Subst, State1} ->
            case catena_infer_unify:unify(RightType, {tcon, bool}, State1) of
                {ok, _Subst2, State2} ->
                    {{tcon, bool}, State2};
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end;

% List append: ++
infer_binary_op(plus_plus, LeftType, RightType, State) ->
    case catena_infer_unify:unify(LeftType, RightType, State) of
        {ok, _Subst, State1} ->
            FinalSubst = catena_infer_state:get_subst(State1),
            ResultType = catena_type_subst:apply(FinalSubst, LeftType),
            {ResultType, State1};
        {error, _, _} = Error ->
            Error
    end;

% Pipe operator: |>
% e1 |> e2 === e2(e1)
infer_binary_op(pipe_right, LeftType, RightType, State) ->
    % Generate fresh result type
    {ResultType, State1} = catena_infer_state:fresh_var(State),
    % Right side should be a function from LeftType to ResultType
    ExpectedFunType = {tfun, LeftType, ResultType, {effect_set, []}},
    case catena_infer_unify:unify(RightType, ExpectedFunType, State1) of
        {ok, _Subst, State2} ->
            FinalSubst = catena_infer_state:get_subst(State2),
            FinalResultType = catena_type_subst:apply(FinalSubst, ResultType),
            {FinalResultType, State2};
        {error, _, _} = Error ->
            Error
    end;

% Default: unknown operator
infer_binary_op(Op, _LeftType, _RightType, State) ->
    Error = catena_type_error:unknown_operator(Op),
    State1 = catena_infer_state:add_error(Error, State),
    {error, Error, State1}.

%% @doc Infer types for list elements, unifying with expected type
-spec infer_list_elements([catena_infer_ast:expr()], catena_types:type(),
                          catena_type_env:env(), catena_infer_state:infer_state()) ->
    {catena_types:type(), catena_infer_state:infer_state()} |
    {error, catena_type_error:type_error(), catena_infer_state:infer_state()}.
infer_list_elements([], ElemType, _Env, State) ->
    FinalSubst = catena_infer_state:get_subst(State),
    FinalType = catena_type_subst:apply(FinalSubst, ElemType),
    {FinalType, State};
infer_list_elements([Elem | Rest], ElemType, Env, State) ->
    case infer(Elem, Env, State) of
        {ElemInferredType, State1} ->
            case catena_infer_unify:unify(ElemType, ElemInferredType, State1) of
                {ok, _Subst, State2} ->
                    FinalSubst = catena_infer_state:get_subst(State2),
                    UnifiedType = catena_type_subst:apply(FinalSubst, ElemType),
                    infer_list_elements(Rest, UnifiedType, Env, State2);
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end.

%% @doc Infer type of match expression
-spec infer_match(catena_infer_ast:expr(), [{catena_infer_ast:pattern(), catena_infer_ast:expr()}],
                  catena_type_env:env(), catena_infer_state:infer_state()) ->
    {catena_types:type(), catena_infer_state:infer_state()} |
    {error, catena_type_error:type_error(), catena_infer_state:infer_state()}.
infer_match(Scrutinee, Clauses, Env, State) ->
    % Infer scrutinee type
    case infer(Scrutinee, Env, State) of
        {ScrutineeType, State1} ->
            % Generate fresh type variable for result
            {ResultType, State2} = catena_infer_state:fresh_var(State1),
            % Infer each clause
            infer_match_clauses(Clauses, ScrutineeType, ResultType, Env, State2);
        {error, _, _} = Error ->
            Error
    end.

%% @doc Infer types for match clauses
-spec infer_match_clauses([{catena_infer_ast:pattern(), catena_infer_ast:expr()} |
                           {catena_infer_ast:pattern(), catena_infer_ast:expr(), catena_infer_ast:expr()}],
                          catena_types:type(), catena_types:type(),
                          catena_type_env:env(), catena_infer_state:infer_state()) ->
    {catena_types:type(), catena_infer_state:infer_state()} |
    {error, catena_type_error:type_error(), catena_infer_state:infer_state()}.
infer_match_clauses([], _ScrutineeType, ResultType, _Env, State) ->
    FinalSubst = catena_infer_state:get_subst(State),
    FinalType = catena_type_subst:apply(FinalSubst, ResultType),
    {FinalType, State};
infer_match_clauses([Clause | Rest], ScrutineeType, ResultType, Env, State) ->
    case infer_match_clause(Clause, ScrutineeType, ResultType, Env, State) of
        {UpdatedResultType, State1} ->
            infer_match_clauses(Rest, ScrutineeType, UpdatedResultType, Env, State1);
        {error, _, _} = Error ->
            Error
    end.

%% @doc Infer type for a single match clause
-spec infer_match_clause({catena_infer_ast:pattern(), catena_infer_ast:expr()} |
                          {catena_infer_ast:pattern(), catena_infer_ast:expr(), catena_infer_ast:expr()},
                         catena_types:type(), catena_types:type(),
                         catena_type_env:env(), catena_infer_state:infer_state()) ->
    {catena_types:type(), catena_infer_state:infer_state()} |
    {error, catena_type_error:type_error(), catena_infer_state:infer_state()}.
% Clause without guard
infer_match_clause({Pattern, Body}, ScrutineeType, ResultType, Env, State) ->
    % Infer pattern type and get bindings
    % Note: catena_infer_pattern:infer/3 always succeeds (returns 3-tuple)
    {PatternType, PatternBindings, State1} = catena_infer_pattern:infer(Pattern, Env, State),
    % Unify pattern type with scrutinee type
    case catena_infer_unify:unify(PatternType, ScrutineeType, State1) of
        {ok, _Subst, State2} ->
            % Merge pattern bindings into environment
            Env1 = catena_type_env:merge(Env, PatternBindings),
            % Infer body type
            case infer(Body, Env1, State2) of
                {BodyType, State3} ->
                    % Unify body type with result type
                    case catena_infer_unify:unify(BodyType, ResultType, State3) of
                        {ok, _Subst2, State4} ->
                            FinalSubst = catena_infer_state:get_subst(State4),
                            FinalResult = catena_type_subst:apply(FinalSubst, ResultType),
                            {FinalResult, State4};
                        {error, _, _} = Error ->
                            Error
                    end;
                {error, _, _} = Error ->
                    Error
            end;
        {error, _, _} = Error ->
            Error
    end;
% Clause with guard
infer_match_clause({Pattern, Guard, Body}, ScrutineeType, ResultType, Env, State) ->
    % Note: catena_infer_pattern:infer/3 always succeeds (returns 3-tuple)
    {PatternType, PatternBindings, State1} = catena_infer_pattern:infer(Pattern, Env, State),
    case catena_infer_unify:unify(PatternType, ScrutineeType, State1) of
        {ok, _Subst, State2} ->
            Env1 = catena_type_env:merge(Env, PatternBindings),
            % Infer guard type - must be Bool
            case infer(Guard, Env1, State2) of
                {GuardType, State3} ->
                    case catena_infer_unify:unify(GuardType, {tcon, bool}, State3) of
                        {ok, _Subst2, State4} ->
                            % Infer body type
                            case infer(Body, Env1, State4) of
                                {BodyType, State5} ->
                                    case catena_infer_unify:unify(BodyType, ResultType, State5) of
                                        {ok, _Subst3, State6} ->
                                            FinalSubst = catena_infer_state:get_subst(State6),
                                            FinalResult = catena_type_subst:apply(FinalSubst, ResultType),
                                            {FinalResult, State6};
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
    end.
