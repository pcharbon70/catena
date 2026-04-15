%%%-------------------------------------------------------------------
%%% @doc Catena Equation Proofs (Phase 8.4)
%%%
%%% This module implements equation proof checking and derivation.
%%% It provides tools for:
%%%
%%% - Proof rules for equational reasoning
%%% - Proof step validation
%%% - Proof chain construction
%%% - Proof normalization
%%% - Proof verification against equation sets
%%%
%%% == Proof Rules ==
%%%
%%% The system supports several proof rules:
%%% - refl: Reflexivity - any term equals itself
%%% - symm: Symmetry - if a = b then b = a
%%% - trans: Transitivity - if a = b and b = c then a = c
%%% - congr: Congruence - if a = b then f(a) = f(b)
%%% - subst: Substitution - if a = b then C[a/x] = C[b/x]
%%% - eq: Equation application - apply known equations
%%%
%%% == Proof Chains ==
%%%
%%% A proof chain is a sequence of proof steps that derive a conclusion
%%% from premises. Each step must be justified by a proof rule.
%%% Chains can be validated and transformed.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_equation_proof).

%% Proof rule types
-export([
    rule_refl/1,
    rule_symm/3,
    rule_trans/5,
    rule_congr/4,
    rule_subst/5,
    rule_eq/4
]).

%% Proof steps and chains
-export([
    new_step/3,
    validate_step/2,
    step_to_string/1,
    is_valid_step/1
]).

%% Proof chains
-export([
    new_chain/0,
    add_step/2,
    validate_chain/2,
    chain_to_string/1,
    simplify_chain/1
]).

%% Proof derivation
-export([
    derive/3,
    prove/4,
    find_proof/4
]).

%%====================================================================
%% Types
%%====================================================================

-type rule_name() :: refl | symm | trans | congr | subst | eq.
-type expr_term() :: catena_equations:pattern().
-type equation() :: catena_equations:equation().
-type equation_set() :: catena_equation_spec:equation_set().
-type justification() :: {rule_name(), expr_term() | [expr_term()]}.

-type proof_step() :: #{
    from := [expr_term()],
    to := expr_term(),
    rule := justification()
}.

-type proof_chain() :: [proof_step()].

-type proof_result() :: {valid, proof_chain()} | {invalid, [error_reason()]}.
-type error_reason() :: {pos_integer(), term(), string()}.

-export_type([
    rule_name/0,
    proof_step/0,
    proof_chain/0,
    justification/0,
    proof_result/0
]).

%%====================================================================
%% Proof Rules
%%====================================================================

%% @doc Reflexivity: any term equals itself.
-spec rule_refl(term()) -> proof_step().
rule_refl(Term) ->
    #{
        from => [Term],
        to => Term,
        rule => {refl, Term}
    }.

%% @doc Symmetry: if a = b then b = a.
-spec rule_symm(term(), term(), proof_step()) -> proof_step().
rule_symm(A, B, _Step) ->
    #{
        from => [A],
        to => B,
        rule => {symm, [A, B]}
    }.

%% @doc Transitivity: if a = b and b = c then a = c.
-spec rule_trans(term(), term(), term(), proof_step(), proof_step()) -> proof_step().
rule_trans(A, B, C, _AB, _BC) ->
    #{
        from => [A, B],
        to => C,
        rule => {trans, [A, B, C]}
    }.

%% @doc Congruence: if a = b then f(a) = f(b) for operation f.
-spec rule_congr(atom(), term(), term(), proof_step()) -> proof_step().
rule_congr(Op, A, B, _AB) ->
    #{
        from => [A],
        to => B,
        rule => {congr, {Op, [A, B]}}
    }.

%% @doc Substitution: if a = b then C[a/x] = C[b/x].
-spec rule_subst(term(), term(), atom(), term(), term()) -> proof_step().
rule_subst(A, B, Var, Context, _Eq) ->
    #{
        from => [Context],
        to => substitute_in_term(Var, A, B, Context),
        rule => {subst, [{Var, A, B}, Context]}
    }.

%% @doc Equation application: apply a named equation.
-spec rule_eq(term(), term(), atom(), equation()) -> proof_step().
rule_eq(LHS, RHS, EqName, _Equation) ->
    #{
        from => [LHS],
        to => RHS,
        rule => {eq, EqName}
    }.

%%====================================================================
%% Proof Steps
%%====================================================================

%% @doc Create a new proof step.
-spec new_step([term()], term(), justification()) -> proof_step().
new_step(From, To, Rule) ->
    #{
        from => From,
        to => To,
        rule => Rule
    }.

%% @doc Validate a proof step against an equation set.
-spec validate_step(proof_step(), equation_set()) -> {valid, proof_step()} | {invalid, string()}.
validate_step(Step, EqSet) ->
    Rule = maps:get(rule, Step),
    case validate_rule(Step, Rule, EqSet) of
        ok -> {valid, Step};
        {error, Reason} -> {invalid, Reason}
    end.

%% @doc Check if a proof step is valid.
-spec is_valid_step(proof_step()) -> boolean().
is_valid_step(Step) ->
    maps:is_key(from, Step) andalso maps:is_key(to, Step) andalso maps:is_key(rule, Step).

%% @doc Convert a proof step to a string.
-spec step_to_string(proof_step()) -> string().
step_to_string(#{from := From, to := To, rule := Rule}) ->
    FromStr = terms_to_string(From),
    ToStr = term_to_string(To),
    RuleStr = rule_to_string(Rule),
    lists:flatten([FromStr, " ≡ ", ToStr, "  (", RuleStr, ")"]).

%% @private Validate a proof rule.
validate_rule(_Step, {refl, _Term}, _EqSet) -> ok;
validate_rule(#{to := To, from := [A]}, {symm, [A, B]}, _EqSet) when To =:= B -> ok;
validate_rule(#{to := C, from := [A, _]}, {trans, [A, _, C]}, _EqSet) -> ok;
validate_rule(#{from := [A], to := B}, {congr, {Op, [A, B]}}, _EqSet) -> ok;
validate_rule(#{from := [A, B], to := C}, {trans, [A, B, C]}, _EqSet) -> ok;
validate_rule(Step, {eq, EqName}, EqSet) ->
    [LHS] = maps:get(from, Step),
    RHS = maps:get(to, Step),
    case catena_equation_spec:get_equation(EqSet, EqName) of
        {ok, Eq} ->
            EqLHS = catena_equations:lhs(Eq),
            case catena_equations:unify_patterns(LHS, EqLHS) of
                {ok, _} -> ok;
                {error, _} -> {error, "LHS does not match equation"}
            end;
        error -> {error, "Equation not found"}
    end;
validate_rule(_Step, _Rule, _EqSet) -> {error, "Unknown rule"}.

%%====================================================================
%% Proof Chains
%%====================================================================

%% @doc Create a new empty proof chain.
-spec new_chain() -> proof_chain().
new_chain() -> [].

%% @doc Add a step to a proof chain.
-spec add_step(proof_chain(), proof_step()) -> proof_chain().
add_step(Chain, Step) ->
    Chain ++ [Step].

%% @doc Validate a proof chain against an equation set.
-spec validate_chain(proof_chain(), equation_set()) -> proof_result().
validate_chain(Chain, EqSet) ->
    % Enumerate the chain with step numbers
    EnumeratedChain = lists:zip(lists:seq(1, length(Chain)), Chain),
    Errors = lists:foldl(fun({StepNum, Step}, Acc) ->
        case validate_step(Step, EqSet) of
            {invalid, Reason} ->
                [{StepNum, Step, Reason} | Acc];
            {valid, _} ->
                Acc
        end
    end, [], EnumeratedChain),
    case Errors of
        [] -> {valid, Chain};
        _ -> {invalid, lists:reverse(Errors)}
    end.

%% @doc Convert a proof chain to a string.
-spec chain_to_string(proof_chain()) -> string().
chain_to_string(Chain) ->
    Lines = lists:map(fun(Step) ->
        step_to_string(Step)
    end, Chain),
    string:join(Lines, "\n").

%% @doc Simplify a proof chain by removing redundant steps.
-spec simplify_chain(proof_chain()) -> proof_chain().
simplify_chain(Chain) ->
    % Remove refl steps (tautologies)
    lists:filter(fun(#{rule := {refl, _}}) -> false; (_) -> true end, Chain).

%%====================================================================
%% Proof Derivation
%%====================================================================

%% @doc Derive a term from another using given equations.
-spec derive(term(), term(), equation_set()) -> {ok, proof_chain()} | {error, term()}.
derive(From, To, EqSet) ->
    case find_proof(From, To, EqSet, 100) of
        {ok, Chain} -> {ok, lists:reverse(Chain)};
        {error, _} = Err -> Err
    end.

%% @doc Prove an equation using proof rules.
-spec prove([term()], term(), equation_set(), non_neg_integer()) -> proof_result().
prove(Premises, Conclusion, EqSet, MaxSteps) ->
    case find_proof(hd(Premises), Conclusion, EqSet, MaxSteps) of
        {ok, Chain} -> {valid, Chain};
        {error, _} -> {invalid, ["Proof not found"]}
    end.

%% @doc Find a proof chain from Source to Target.
-spec find_proof(term(), term(), equation_set(), non_neg_integer()) ->
    {ok, proof_chain()} | {error, no_proof}.
find_proof(Source, Target, EqSet, MaxSteps) when MaxSteps > 0 ->
    case Source of
        Target -> {ok, [rule_refl(Target)]};
        _ ->
            % Try equation application
            EqNames = catena_equation_spec:list_equations(EqSet),
            case try_equations(Source, Target, EqNames, EqSet, MaxSteps) of
                {ok, Chain} -> {ok, Chain};
                {error, no_proof} ->
                    % Try decomposition strategies
                    try_decompose(Source, Target, EqSet, MaxSteps)
            end
    end;
find_proof(_, _, _, _) ->
    {error, no_proof}.

%% @private Try applying equations to derive Target from Source.
try_equations(Source, Target, EqNames, EqSet, MaxSteps) ->
    try_equations_loop(Source, Target, EqNames, EqSet, MaxSteps, []).

%% @private Loop through equations trying to find a derivation.
try_equations_loop(_Source, _Target, [], _EqSet, _MaxSteps, _Chain) ->
    {error, no_proof};
try_equations_loop(Source, Target, [Name | Rest], EqSet, MaxSteps, Chain) ->
    {ok, Eq} = catena_equation_spec:get_equation(EqSet, Name),
    LHS = catena_equations:lhs(Eq),
    RHS = catena_equations:rhs(Eq),
    case catena_equations:unify_patterns(Source, LHS) of
        {ok, Subst} ->
            MidResult = catena_equations:substitute_variables(RHS, Subst),
            Step = rule_eq(Source, MidResult, Name, Eq),
            NewChain = [Step | Chain],
            case MidResult of
                Target -> {ok, NewChain};
                _ when MaxSteps > 0 ->
                    case find_proof(MidResult, Target, EqSet, MaxSteps - 1) of
                        {ok, RestChain} -> {ok, NewChain ++ RestChain};
                        {error, _} -> try_equations_loop(Source, Target, Rest, EqSet, MaxSteps, Chain)
                    end;
                _ ->
                    try_equations_loop(Source, Target, Rest, EqSet, MaxSteps, Chain)
            end;
        {error, _} ->
            try_equations_loop(Source, Target, Rest, EqSet, MaxSteps, Chain)
    end.

%% @private Try decomposition strategies.
try_decompose(Source, Target, EqSet, MaxSteps) ->
    case decompose_source(Source, Target, EqSet, MaxSteps) of
        {ok, Chain} -> {ok, Chain};
        {error, no_proof} -> try_decompose_target(Source, Target, EqSet, MaxSteps)
    end.

%% @private Try decomposing the source term.
decompose_source({op, Op, Val, Arg}, Target, EqSet, MaxSteps) ->
    case find_proof(Arg, Target, EqSet, MaxSteps div 2) of
        {ok, Chain} ->
            Step = rule_congr(Op, Arg, Target, hd(Chain)),
            {ok, Chain ++ [Step]};
        {error, _} -> {error, no_proof}
    end;
decompose_source(_Source, _Target, _EqSet, _MaxSteps) ->
    {error, no_proof}.

%% @private Try decomposing the target term.
try_decompose_target(Source, Target, EqSet, MaxSteps) ->
    case Target of
        {op, Op, Val, Arg} when Source =/= Arg ->
            case find_proof(Source, Arg, EqSet, MaxSteps div 2) of
                {ok, Chain} ->
                    Step = rule_congr(Op, Source, Arg, hd(Chain)),
                    {ok, [Step | Chain]};
                {error, _} -> {error, no_proof}
            end;
        _ -> {error, no_proof}
    end.

%%====================================================================
%% Private Functions
%%====================================================================

%% @private Substitute a term in another term.
-spec substitute_in_term(atom(), term(), term(), term()) -> term().
substitute_in_term(Var, From, To, Context) ->
    case Context of
        {var, Var} when Context =:= From -> To;
        {var, _} -> Context;
        {lit, _} -> Context;
        {op, Op, Val, Arg} ->
            {op, Op, Val, substitute_in_term(Var, From, To, Arg)};
        {seq, Terms} ->
            {seq, [substitute_in_term(Var, From, To, T) || T <- Terms]};
        {bind, Name, Term} ->
            {bind, Name, substitute_in_term(Var, From, To, Term)};
        _ -> Context
    end.

%% @private Convert terms to string representation.
terms_to_string(Terms) ->
    case Terms of
        [T] -> term_to_string(T);
        _ -> io_lib:format("(~s)", [string:join([term_to_string(T) || T <- Terms], ", ")])
    end.

%% @private Convert a term to string.
-spec term_to_string(term()) -> string().
term_to_string({var, Name}) -> atom_to_list(Name);
term_to_string({lit, Value}) -> io_lib:format("~p", [Value]);
term_to_string({op, Op, Val, Arg}) ->
    io_lib:format("~s(~p, ~s)", [Op, Val, term_to_string(Arg)]);
term_to_string({seq, Terms}) ->
    StrTerms = [term_to_string(T) || T <- Terms],
    io_lib:format("[~s]", [string:join(StrTerms, ", ")]);
term_to_string({bind, Name, Term}) ->
    io_lib:format("~s@~s", [Name, term_to_string(Term)]);
term_to_string(Other) -> io_lib:format("~p", [Other]).

%% @private Convert a rule to string.
rule_to_string({Rule, Info}) ->
    case Rule of
        refl -> "refl";
        symm -> io_lib:format("symm(~s ≡ ~s)", [term_to_string(hd(Info)), term_to_string(hd(tl(Info)))]);
        trans -> io_lib:format("trans(~s, ~s, ~s)", [term_to_string(hd(Info)), term_to_string(hd(tl(Info))), term_to_string(hd(tl(tl(Info))))]);
        congr -> {Op, _} = Info, io_lib:format("congr(~s)", [Op]);
        subst -> io_lib:format("subst(~s)", [format_subst_info(Info)]);
        eq -> io_lib:format("eq(~s)", [Info])
    end.

%% @private Format substitution info.
format_subst_info([{Var, From, To} | _]) ->
    io_lib:format("~s/~s: ~s → ~s", [Var, Var, term_to_string(From), term_to_string(To)]).
