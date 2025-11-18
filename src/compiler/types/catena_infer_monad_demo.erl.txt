%%%
%%% @doc Monadic Inference Style Demonstrations
%%%
%%% Shows how monadic combinators eliminate deep case nesting
%%% and make inference code more readable and maintainable.
%%%
%%% @end
%%%
-module(topos_infer_monad_demo).

%%===================================================================
%% Before: Deep Nesting (Traditional Style)
%%===================================================================

%% Example: Record field access with 5 levels of nesting
infer_field_traditional(Expr, FieldName, Env, State) ->
    case infer(Expr, Env, State) of                    % Level 1
        {ExprType, State1} ->
            case topos_infer_state:fresh_var(State1) of  % Level 2
                {FieldType, State2} ->
                    case topos_infer_state:fresh_var(State2) of  % Level 3
                        {RowVar, State3} ->
                            {tvar, RowVarId} = RowVar,
                            ExpectedType = {trecord, [{FieldName, FieldType}], RowVarId},
                            case topos_infer_unify:unify(ExprType, ExpectedType, State3) of  % Level 4
                                {ok, _Subst, State4} ->
                                    FinalSubst = topos_infer_state:get_subst(State4),
                                    FinalFieldType = topos_type_subst:apply(FinalSubst, FieldType),  % Level 5
                                    {FinalFieldType, State4};
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

%%===================================================================
%% After: Monadic Style (Clean and Readable)
%%===================================================================

%% Example: Same logic with monadic combinators
infer_field_monadic(Expr, FieldName, Env) ->
    topos_infer_monad:bind(
        fun(S) -> infer(Expr, Env, S) end,                    % Infer expression
        fun(ExprType) ->                                       % Get result
            topos_infer_monad:with_fresh_var(
                fun(FieldType) ->                               % Fresh field type
                    topos_infer_monad:with_fresh_var(
                        fun(RowVar) ->                           % Fresh row var
                            {tvar, RowVarId} = RowVar,
                            ExpectedType = {trecord, [{FieldName, FieldType}], RowVarId},
                            
                            fun(State) ->                        % Final computation
                                case topos_infer_unify:unify(ExprType, ExpectedType, State) of
                                    {ok, _Subst, FinalState} ->
                                        FinalSubst = topos_infer_state:get_subst(FinalState),
                                        FinalFieldType = topos_type_subst:apply(FinalSubst, FieldType),
                                        {FinalFieldType, FinalState};
                                    {error, _, _} = Error ->
                                        Error
                                end
                            end
                        end
                    )
                end
            )
        end
    ).

%%===================================================================
%% Even Better: Pure Monadic Style
%%===================================================================

%% Example: Completely monadic, no manual case handling
infer_field_pure_monadic(Expr, FieldName, Env) ->
    topos_infer_monad:bind(
        fun(S) -> infer(Expr, Env, S) end,
        fun(ExprType) ->
            topos_infer_monad:with_fresh_vars(2,
                fun([FieldType, RowVar]) ->
                    {tvar, RowVarId} = RowVar,
                    ExpectedType = {trecord, [{FieldName, FieldType}], RowVarId},
                    
                    topos_infer_monad:lift(
                        fun(_, UnifyState) ->
                            case topos_infer_unify:unify(ExprType, ExpectedType, UnifyState) of
                                {ok, _Subst, FinalState} ->
                                    FinalSubst = topos_infer_state:get_subst(FinalState),
                                    FinalFieldType = topos_type_subst:apply(FinalSubst, FieldType),
                                    {FinalFieldType, FinalState};
                                {error, _, _} = Error ->
                                    Error
                            end
                        end,
                        topos_infer_monad:return(unit)
                    )
                end
            )
        end
    ).

%%===================================================================
%% Benefits Summary
%%===================================================================

%% COMPLEXITY REDUCTION:
%% Traditional: 5 nested case statements
%% Monadic:   1 monadic bind chain
%% 
%% READABILITY:
%% Traditional: Error handling repeated at every level
%% Monadic:   Single error handling path, clear flow
%%
%% MAINTAINABILITY:
%% Traditional: Adding steps requires more nesting
%% Monadic:   Adding steps is just another bind
%%
%% REUSABILITY:
%% Traditional: Logic is embedded in case statements
%% Monadic:   With_fresh_var, with_fresh_vars are reusable