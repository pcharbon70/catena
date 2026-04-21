%%%-------------------------------------------------------------------
%%% @doc Effect constraint generation and lightweight solving.
%%%
%%% The compiler does not yet have a fully general row-polymorphic effect
%%% solver, but Phase 14.2 needs a coherent layer that matches the live
%%% AST and can validate declared-vs-inferred effect surfaces.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_constraints).

-export([
    generate_constraints/2,
    generate_from_expr/2,
    generate_from_application/3,
    generate_from_handle/3,
    generate_from_perform/2
]).

-export([
    solve_constraints/2,
    solve_effect_constraint/3,
    solve_row_constraint/3,
    unify_effect_sets/3
]).

-export([
    propagate_constraints/2,
    propagate_to_context/2,
    propagate_from_context/2
]).

-export([
    constraint_error_string/1,
    unsatisfied_constraints/1,
    constraint_to_string/1
]).

-export([
    is_satisfied/1,
    is_row_constraint/1,
    get_row_var_from_constraint/1,
    merge_constraints/2
]).

-type expr() :: term().
-type env() :: catena_type_env:env().
-type state() :: catena_infer_state:infer_state().
-type effect_set() :: catena_types:effect_set().
-type row_var() :: catena_row_types:row_var().
-type constraint() :: term().
-type constraints() :: [constraint()].

-type effect_constraint() ::
    {has_effect, term(), atom()} |
    {remove_effect, term(), atom()} |
    {effects_subset, term(), effect_set(), effect_set()} |
    {row_contains, row_var(), atom()} |
    {row_extension, row_var(), effect_set()}.

-type resolution_result() ::
    {ok, state()} |
    {error, [constraint()], string()}.

%%====================================================================
%% Constraint Generation
%%====================================================================

-spec generate_constraints(expr(), state()) -> {constraints(), state()}.
generate_constraints(Expr, State) ->
    generate_from_expr(Expr, State).

-spec generate_from_expr(expr(), state()) -> {constraints(), state()}.
generate_from_expr({lit, _Literal}, State) ->
    {[], State};
generate_from_expr({literal, _Type, _Value, _Loc}, State) ->
    {[], State};
generate_from_expr({var, _Name}, State) ->
    {[], State};
generate_from_expr({var, _Name, _Loc}, State) ->
    {[], State};
generate_from_expr({identifier, _Name, _Loc}, State) ->
    {[], State};
generate_from_expr({lam, _Param, Body}, State) ->
    generate_from_expr(Body, State);
generate_from_expr({lambda, _Params, Body, _Loc}, State) ->
    generate_from_expr(Body, State);
generate_from_expr({app, Func, Arg}, State) ->
    generate_from_application(Func, [Arg], State);
generate_from_expr({app, Func, Args, _Loc}, State) when is_list(Args) ->
    generate_from_application(Func, Args, State);
generate_from_expr({perform_expr, Effect, _Operation, _Args, _Loc}, State) ->
    generate_from_perform(Effect, State);
generate_from_expr({perform, {Effect, _Operation, _Args}, _Loc}, State) ->
    generate_from_perform(Effect, State);
generate_from_expr({handle_expr, Expr, Handlers, _Loc}, State) ->
    generate_from_handle(Expr, Handlers, State);
generate_from_expr({handle, Expr, Handlers, _Loc}, State) ->
    generate_from_handle(Expr, Handlers, State);
generate_from_expr({'let', _Name, Value, Body}, State) ->
    merge_generated(generate_from_expr(Value, State), fun(State1) ->
        generate_from_expr(Body, State1)
    end);
generate_from_expr({let_expr, [{pat_var, _Name, _}, Value], Body, _Loc}, State) ->
    merge_generated(generate_from_expr(Value, State), fun(State1) ->
        generate_from_expr(Body, State1)
    end);
generate_from_expr({if_expr, Cond, Then, Else, _Loc}, State) ->
    generate_from_expr({'if', Cond, Then, Else}, State);
generate_from_expr({'if', Cond, Then, Else}, State) ->
    merge_generated(generate_from_expr(Cond, State), fun(State1) ->
        merge_generated(generate_from_expr(Then, State1), fun(State2) ->
            generate_from_expr(Else, State2)
        end)
    end);
generate_from_expr({match_expr, Scrutinee, Cases, _Loc}, State) ->
    merge_generated(generate_from_expr(Scrutinee, State), fun(State1) ->
        generate_from_cases(Cases, State1)
    end);
generate_from_expr({'match', Scrutinee, Cases, _Loc}, State) ->
    generate_from_expr({match_expr, Scrutinee, Cases, undefined}, State);
generate_from_expr({match, Scrutinee, Cases}, State) ->
    generate_from_expr({match_expr, Scrutinee, Cases, undefined}, State);
generate_from_expr({tuple_expr, Elements, _Loc}, State) ->
    generate_from_elements(Elements, State);
generate_from_expr({tuple, Elements}, State) ->
    generate_from_elements(Elements, State);
generate_from_expr({tuple, Elements, _Loc}, State) ->
    generate_from_elements(Elements, State);
generate_from_expr({list_expr, Elements, _Loc}, State) ->
    generate_from_elements(Elements, State);
generate_from_expr({list, Elements, _Loc}, State) ->
    generate_from_elements(Elements, State);
generate_from_expr({record_expr, Fields, _Base, _Loc}, State) ->
    generate_from_elements([Expr || {_Name, Expr} <- Fields], State);
generate_from_expr({record, Fields}, State) ->
    generate_from_elements([Expr || {_Name, Expr} <- Fields], State);
generate_from_expr({field_access, Expr, _Field, _Loc}, State) ->
    generate_from_expr(Expr, State);
generate_from_expr({field, Expr, _Field}, State) ->
    generate_from_expr(Expr, State);
generate_from_expr({binary_op, _Op, Left, Right, _Loc}, State) ->
    generate_from_elements([Left, Right], State);
generate_from_expr({unary_op, _Op, Expr, _Loc}, State) ->
    generate_from_expr(Expr, State);
generate_from_expr({constructor, _Name, Args, _Loc}, State) ->
    generate_from_elements(Args, State);
generate_from_expr({variant, _Name, Args}, State) ->
    generate_from_elements(Args, State);
generate_from_expr({ann, Expr, _Type}, State) ->
    generate_from_expr(Expr, State);
generate_from_expr(_Expr, State) ->
    {[], State}.

-spec generate_from_application(expr(), [expr()], state()) -> {constraints(), state()}.
generate_from_application(Func, Args, State) ->
    {FuncConstraints, State1} = generate_from_expr(Func, State),
    {ArgConstraints, State2} = generate_from_elements(Args, State1),
    {FuncConstraints ++ ArgConstraints, State2}.

-spec generate_from_handle(expr(), [term()], state()) -> {constraints(), state()}.
generate_from_handle(Expr, Handlers, State) ->
    {BodyConstraints, State1} = generate_from_expr(Expr, State),
    {HandlerConstraints, State2} = generate_from_handlers(Handlers, State1),
    RemovalConstraints = [
        {remove_effect, handle_scope, Effect} ||
        {handler_clause, Effect, _Operations, _Loc} <- Handlers
    ],
    {BodyConstraints ++ HandlerConstraints ++ RemovalConstraints, State2}.

-spec generate_from_perform(atom(), state()) -> {constraints(), state()}.
generate_from_perform(Effect, State) ->
    {[{has_effect, perform_scope, Effect}], State}.

-spec generate_from_elements([expr()], state()) -> {constraints(), state()}.
generate_from_elements(Elements, State) ->
    lists:foldl(
        fun(Element, {Acc, S}) ->
            {ElementConstraints, NewState} = generate_from_expr(Element, S),
            {Acc ++ ElementConstraints, NewState}
        end,
        {[], State},
        Elements
    ).

-spec generate_from_cases([term()], state()) -> {constraints(), state()}.
generate_from_cases(Cases, State) ->
    lists:foldl(
        fun(Case, {Acc, S}) ->
            {CaseConstraints, NewState} = generate_from_case(Case, S),
            {Acc ++ CaseConstraints, NewState}
        end,
        {[], State},
        Cases
    ).

-spec generate_from_case(term(), state()) -> {constraints(), state()}.
generate_from_case({match_clause, _Pattern, undefined, Body, _Loc}, State) ->
    generate_from_expr(Body, State);
generate_from_case({match_clause, _Pattern, Guard, Body, _Loc}, State) ->
    merge_generated(generate_from_expr(Guard, State), fun(State1) ->
        generate_from_expr(Body, State1)
    end);
generate_from_case({_Pattern, Body}, State) ->
    generate_from_expr(Body, State);
generate_from_case({_Pattern, Guard, Body}, State) ->
    merge_generated(generate_from_expr(Guard, State), fun(State1) ->
        generate_from_expr(Body, State1)
    end);
generate_from_case(_Other, State) ->
    {[], State}.

-spec generate_from_handlers([term()], state()) -> {constraints(), state()}.
generate_from_handlers(Handlers, State) ->
    lists:foldl(
        fun(Handler, {Acc, S}) ->
            {HandlerConstraints, NewState} = generate_from_handler(Handler, S),
            {Acc ++ HandlerConstraints, NewState}
        end,
        {[], State},
        Handlers
    ).

-spec generate_from_handler(term(), state()) -> {constraints(), state()}.
generate_from_handler({handler_clause, _Effect, Operations, _Loc}, State) ->
    lists:foldl(
        fun(Operation, {Acc, S}) ->
            {OperationConstraints, NewState} = generate_from_operation_case(Operation, S),
            {Acc ++ OperationConstraints, NewState}
        end,
        {[], State},
        Operations
    );
generate_from_handler(_Other, State) ->
    {[], State}.

-spec generate_from_operation_case(term(), state()) -> {constraints(), state()}.
generate_from_operation_case({operation_case, _Name, _Params, Body, _Loc}, State) ->
    generate_from_expr(Body, State);
generate_from_operation_case(_Other, State) ->
    {[], State}.

%%====================================================================
%% Constraint Solving
%%====================================================================

-spec solve_constraints(constraints(), state()) -> resolution_result().
solve_constraints([], State) ->
    {ok, State};
solve_constraints([Constraint | Rest], State) ->
    case solve_effect_constraint(Constraint, catena_types:empty_effects(), State) of
        {ok, NewState} ->
            solve_constraints(Rest, NewState);
        {error, _, _} = Error ->
            Error
    end.

-spec solve_effect_constraint(constraint(), effect_set(), state()) -> resolution_result().
solve_effect_constraint({has_effect, _Expr, _Effect}, _ContextEffects, State) ->
    {ok, State};
solve_effect_constraint({remove_effect, _Expr, _Effect}, _ContextEffects, State) ->
    {ok, State};
solve_effect_constraint({effects_subset, _Expr, Subset, Superset}, _ContextEffects, State) ->
    case is_subset(Subset, Superset) of
        true ->
            {ok, State};
        false ->
            {error, [{effects_subset, effect_scope, Subset, Superset}],
             "Effects not satisfied"}
    end;
solve_effect_constraint({row_contains, RowVar, Effect}, _ContextEffects, State) ->
    solve_row_constraint({row_contains, RowVar, Effect}, #{}, State);
solve_effect_constraint({row_extension, RowVar, Effects}, _ContextEffects, State) ->
    solve_row_constraint({row_extension, RowVar, Effects}, #{}, State);
solve_effect_constraint(Constraint, _ContextEffects, _State) ->
    {error, [Constraint], "Unknown constraint type"}.

-spec solve_row_constraint(constraint(), map(), state()) -> resolution_result().
solve_row_constraint({row_contains, _RowVar, _Effect}, _RowContext, State) ->
    {ok, State};
solve_row_constraint({row_extension, _RowVar, _Effects}, _RowContext, State) ->
    {ok, State};
solve_row_constraint(Constraint, _RowContext, _State) ->
    {error, [Constraint], "Unknown row constraint"}.

-spec unify_effect_sets(effect_set(), effect_set(), state()) ->
    {ok, effect_set(), state()} | {error, string()}.
unify_effect_sets({effect_set, _} = Effects1, {effect_set, _} = Effects2, State) ->
    {ok, catena_types:union_effects(Effects1, Effects2), State};
unify_effect_sets(_Effects1, _Effects2, _State) ->
    {error, "Invalid effect set format"}.

-spec is_subset(effect_set(), effect_set()) -> boolean().
is_subset({effect_set, Subset}, {effect_set, Superset}) ->
    lists:all(fun(Effect) -> lists:member(Effect, Superset) end, Subset);
is_subset(_Subset, _Superset) ->
    false.

%%====================================================================
%% Constraint Propagation
%%====================================================================

-spec propagate_constraints(constraints(), state()) -> {constraints(), state()}.
propagate_constraints(Constraints, State) ->
    {Constraints, State}.

-spec propagate_to_context(constraints(), env()) -> {constraints(), env()}.
propagate_to_context(Constraints, Env) ->
    {Constraints, Env}.

-spec propagate_from_context(env(), constraints()) -> {constraints(), env()}.
propagate_from_context(Env, Constraints) ->
    {Constraints, Env}.

%%====================================================================
%% Error Reporting
%%====================================================================

-spec constraint_error_string([constraint()]) -> string().
constraint_error_string(Constraints) ->
    lists:flatten([
        "Effect constraints not satisfied:\n",
        [[constraint_to_string(C), "\n"] || C <- Constraints]
    ]).

-spec unsatisfied_constraints(constraints()) -> [constraint()].
unsatisfied_constraints(Constraints) ->
    lists:filter(fun(Constraint) -> not is_satisfied(Constraint) end, Constraints).

-spec constraint_to_string(constraint()) -> string().
constraint_to_string({has_effect, _Expr, Effect}) ->
    io_lib:format("Effect '~p' required", [Effect]);
constraint_to_string({remove_effect, _Expr, Effect}) ->
    io_lib:format("Effect '~p' handled", [Effect]);
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

-spec is_satisfied(constraint()) -> boolean().
is_satisfied({has_effect, _Expr, _Effect}) ->
    true;
is_satisfied({remove_effect, _Expr, _Effect}) ->
    true;
is_satisfied({row_contains, _RowVar, _Effect}) ->
    true;
is_satisfied({row_extension, _RowVar, _Effects}) ->
    true;
is_satisfied({effects_subset, _Expr, Subset, Superset}) ->
    is_subset(Subset, Superset);
is_satisfied(_Constraint) ->
    false.

-spec is_row_constraint(constraint()) -> boolean().
is_row_constraint({row_contains, _, _}) ->
    true;
is_row_constraint({row_extension, _, _}) ->
    true;
is_row_constraint(_) ->
    false.

-spec get_row_var_from_constraint(constraint()) -> row_var() | undefined.
get_row_var_from_constraint({row_contains, RowVar, _}) ->
    RowVar;
get_row_var_from_constraint({row_extension, RowVar, _}) ->
    RowVar;
get_row_var_from_constraint(_) ->
    undefined.

-spec merge_constraints(constraints(), constraints()) -> constraints().
merge_constraints(Constraints1, Constraints2) ->
    Constraints1 ++ Constraints2.

%%====================================================================
%% Internal Helpers
%%====================================================================

-spec merge_generated({constraints(), state()}, fun((state()) -> {constraints(), state()})) ->
    {constraints(), state()}.
merge_generated({Constraints1, State1}, NextFun) ->
    {Constraints2, State2} = NextFun(State1),
    {Constraints1 ++ Constraints2, State2}.
