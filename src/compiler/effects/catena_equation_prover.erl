%%%-------------------------------------------------------------------
%%% @doc Catena Equation Prover (Phase 8.3)
%%%
%%% Public verification surface for equation proving, handler equation
%%% verification, and property-style equation checking.
%%%-------------------------------------------------------------------
-module(catena_equation_prover).

%% Proving and normalization
-export([
    normalize/2,
    normalize/3,
    prove_equivalence/3,
    prove_equivalence/4,
    prove_equation/2,
    prove_equation/3
]).

%% Handler verification
-export([
    verify_handler/4,
    verify_handler/5
]).

%% Property-style verification
-export([
    equation_property/2,
    equation_property/3,
    run_equation_property/3
]).

-type proof_report() :: #{
    strategy := normalized | proof_search,
    normal_form := catena_equations:pattern() | undefined,
    left_normal := catena_equations:pattern(),
    right_normal := catena_equations:pattern(),
    left_trace := [catena_equation_apply:rewrite_step()],
    right_trace := [catena_equation_apply:rewrite_step()],
    proof := catena_equation_proof:proof_chain() | undefined
}.

-type handler_report() :: #{
    handler := atom(),
    validation := ok | {error, [catena_equation_spec:validation_error()]},
    verified := [atom()],
    failed := [map()]
}.

-export_type([proof_report/0, handler_report/0]).

%%====================================================================
%% Public API
%%====================================================================

-spec normalize(catena_equations:pattern(), catena_equation_spec:equation_set()) ->
    {ok, catena_equations:pattern(), map()}.
normalize(Expr, EqSet) ->
    normalize(Expr, EqSet, #{}).

-spec normalize(catena_equations:pattern(), catena_equation_spec:equation_set(), map()) ->
    {ok, catena_equations:pattern(), map()}.
normalize(Expr, EqSet, Opts) ->
    Limit = maps:get(limit, Opts, 50),
    Context0 = catena_equation_apply:new_context(EqSet, Limit),
    Context = maybe_set_max_depth(Context0, maps:get(max_depth, Opts, undefined)),
    case catena_equation_apply:rewrite_with_strategy(Expr, EqSet, normal, Context) of
        {expr, Normal, RewriteContext} ->
            {ok, Normal, RewriteContext};
        {no_match, RewriteContext} ->
            {ok, Expr, RewriteContext}
    end.

-spec prove_equivalence(
    catena_equations:pattern(),
    catena_equations:pattern(),
    catena_equation_spec:equation_set()
) -> {ok, proof_report()} | {error, map()}.
prove_equivalence(Left, Right, EqSet) ->
    prove_equivalence(Left, Right, EqSet, #{}).

-spec prove_equivalence(
    catena_equations:pattern(),
    catena_equations:pattern(),
    catena_equation_spec:equation_set(),
    map()
) -> {ok, proof_report()} | {error, map()}.
prove_equivalence(Left, Right, EqSet, Opts) ->
    {ok, LeftNormal, LeftContext} = normalize(Left, EqSet, Opts),
    {ok, RightNormal, RightContext} = normalize(Right, EqSet, Opts),
    LeftTrace = catena_equation_apply:get_trace(LeftContext),
    RightTrace = catena_equation_apply:get_trace(RightContext),
    case LeftNormal =:= RightNormal of
        true ->
            {ok, #{
                strategy => normalized,
                normal_form => LeftNormal,
                left_normal => LeftNormal,
                right_normal => RightNormal,
                left_trace => LeftTrace,
                right_trace => RightTrace,
                proof => undefined
            }};
        false ->
            MaxSteps = maps:get(max_steps, Opts, 25),
            case catena_equation_proof:find_proof(Left, Right, EqSet, MaxSteps) of
                {ok, Chain} ->
                    {ok, #{
                        strategy => proof_search,
                        normal_form => undefined,
                        left_normal => LeftNormal,
                        right_normal => RightNormal,
                        left_trace => LeftTrace,
                        right_trace => RightTrace,
                        proof => Chain
                    }};
                {error, no_proof} ->
                    {error, #{
                        reason => no_proof,
                        left_normal => LeftNormal,
                        right_normal => RightNormal,
                        left_trace => LeftTrace,
                        right_trace => RightTrace
                    }}
            end
    end.

-spec prove_equation(catena_equations:equation(), catena_equation_spec:equation_set()) ->
    {ok, proof_report()} | {error, map()}.
prove_equation(Equation, EqSet) ->
    prove_equation(Equation, EqSet, #{}).

-spec prove_equation(catena_equations:equation(), catena_equation_spec:equation_set(), map()) ->
    {ok, proof_report()} | {error, map()}.
prove_equation(Equation, EqSet, Opts) ->
    prove_equivalence(
        catena_equations:lhs(Equation),
        catena_equations:rhs(Equation),
        EqSet,
        Opts
    ).

-spec verify_handler(
    atom(),
    fun((catena_equations:pattern()) -> catena_equations:pattern()),
    catena_equation_spec:equation_set(),
    [catena_equations:operation()]
) -> {ok, handler_report()} | {error, handler_report()}.
verify_handler(Handler, HandlerModel, EqSet, ExpectedOperations) ->
    verify_handler(Handler, HandlerModel, EqSet, ExpectedOperations, #{}).

-spec verify_handler(
    atom(),
    fun((catena_equations:pattern()) -> catena_equations:pattern()),
    catena_equation_spec:equation_set(),
    [catena_equations:operation()],
    map()
) -> {ok, handler_report()} | {error, handler_report()}.
verify_handler(Handler, HandlerModel, EqSet, ExpectedOperations, Opts) ->
    Validation = catena_equation_spec:validate_handler_equations(
        EqSet, Handler, ExpectedOperations
    ),
    HandlerNames = handler_equation_names(EqSet, Handler),
    Results = [verify_handler_equation(Name, HandlerModel, EqSet, Opts) || Name <- HandlerNames],
    Verified = [Name || {ok, Name, _Report} <- Results],
    Failed = [Failure || {error, _Name, Failure} <- Results],
    Report = #{
        handler => Handler,
        validation => Validation,
        verified => Verified,
        failed => Failed
    },
    case {Validation, Failed} of
        {ok, []} -> {ok, Report};
        _ -> {error, Report}
    end.

-spec equation_property(catena_equations:equation(), catena_equation_spec:equation_set()) ->
    catena_property:property().
equation_property(Equation, EqSet) ->
    equation_property(Equation, EqSet, #{}).

-spec equation_property(catena_equations:equation(), catena_equation_spec:equation_set(), map()) ->
    catena_property:property().
equation_property(Equation, EqSet, Opts) ->
    PropertyName = maps:get(name, Opts, <<"equation_property">>),
    Generator = substitution_generator(equation_variables(Equation)),
    Predicate = fun(Subst) ->
        case should_check_equation(Equation, Subst) of
            false ->
                discard;
            true ->
                {Left, Right} = instantiate_equation(Equation, Subst),
                case prove_equivalence(Left, Right, EqSet, Opts) of
                    {ok, _} -> true;
                    {error, _} -> false
                end
        end
    end,
    Prop0 = catena_property:new(PropertyName, Generator, Predicate),
    maybe_with_num_tests(Prop0, maps:get(num_tests, Opts, undefined)).

-spec run_equation_property(
    catena_equations:equation(),
    catena_equation_spec:equation_set(),
    map()
) -> catena_runner:run_result().
run_equation_property(Equation, EqSet, Opts) ->
    catena_runner:run_property(
        equation_property(Equation, EqSet, Opts),
        runner_options(Opts)
    ).

%%====================================================================
%% Internal Helpers
%%====================================================================

maybe_set_max_depth(Context, undefined) ->
    Context;
maybe_set_max_depth(Context, MaxDepth) ->
    Context#{max_depth => MaxDepth}.

handler_equation_names(EqSet, Handler) ->
    HandlerIndex = maps:get(handlers, EqSet, #{}),
    maps:get(Handler, HandlerIndex, []).

verify_handler_equation(Name, HandlerModel, EqSet, Opts) ->
    case catena_equation_spec:get_equation(EqSet, Name) of
        {ok, Equation} ->
            Left = HandlerModel(catena_equations:lhs(Equation)),
            Right = HandlerModel(catena_equations:rhs(Equation)),
            case prove_equivalence(Left, Right, EqSet, Opts) of
                {ok, Report} ->
                    {ok, Name, Report};
                {error, Reason} ->
                    {error, Name, #{
                        equation => Name,
                        lhs => Left,
                        rhs => Right,
                        reason => Reason
                    }}
            end;
        error ->
            {error, Name, #{equation => Name, reason => not_found}}
    end.

equation_variables(Equation) ->
    LHSVars = catena_equations:extract_variables(catena_equations:lhs(Equation)),
    RHSVars = catena_equations:extract_variables(catena_equations:rhs(Equation)),
    GuardVars = case catena_equations:condition(Equation) of
        undefined -> [];
        Guard -> catena_equations:guard_vars(Guard)
    end,
    lists:usort(LHSVars ++ RHSVars ++ GuardVars).

substitution_generator([]) ->
    catena_gen:constant(#{});
substitution_generator([Var | Rest]) ->
    catena_gen:gen_bind(catena_gen:gen_int_range(-5, 5), fun(Value) ->
        catena_gen:gen_map(fun(RestMap) ->
            RestMap#{Var => Value}
        end, substitution_generator(Rest))
    end).

instantiate_equation(Equation, Subst) ->
    {
        catena_equations:substitute_variables(catena_equations:lhs(Equation), Subst),
        catena_equations:substitute_variables(catena_equations:rhs(Equation), Subst)
    }.

should_check_equation(Equation, Subst) ->
    catena_equations:evaluate_guard(catena_equations:condition(Equation), Subst).

maybe_with_num_tests(Property, undefined) ->
    Property;
maybe_with_num_tests(Property, NumTests) ->
    catena_property:with_config([{num_tests, NumTests}], Property).

runner_options(Opts) ->
    Base = #{num_tests => maps:get(num_tests, Opts, 25)},
    case maps:find(seed, Opts) of
        {ok, Seed} -> Base#{seed => Seed};
        error -> Base
    end.
