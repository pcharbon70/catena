%%%-------------------------------------------------------------------
%%% @doc Catena Equation Type and Representation (Phase 8.1)
%%%
%%% This module implements the equation type and representation for specifying
%%% operation equations. Equations give algebraic effects their "algebraic"
%%% nature by specifying laws that operations must satisfy.
%%%
%%% == Equation Type ==
%%%
%%% An equation consists of:
%%% - lhs: Left-hand side pattern
%%% - rhs: Right-hand side pattern
%%% - condition: Optional guard condition
%%%
%%% Equations specify when two expressions are equivalent, enabling:
%%% - Equational reasoning about effectful code
%%% - Optimization via rewriting
%%% - Verification of handler correctness
%%%
%%% == Pattern Types ==
%%%
%%% Patterns can be:
%%% - Variables: Capture arbitrary values
%%% - Literals: Match specific values
%%% - Operations: Match effect operations
%%% - Sequences: Match chained operations
%%% - Wildcards: Match anything (ignore)
%%%
%%% == Guards ==
%%%
%%% Guards specify conditions for equation application:
%%% - Type checks (is_integer, is_atom, etc.)
%%% - Value comparisons (==, =<, etc.)
%%% - Logical operations (andalso, orelse)
%%% - Custom predicates
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_equations).

%% Equation type and constructors
-export([
    new/3,
    new/2,
    from_patterns/2,
    from_patterns/3
]).

%% Pattern constructors
-export([
    var/1,
    lit/1,
    op/3,
    seq/1,
    wildcard/0,
    bind/2
]).

%% Equation inspection
-export([
    lhs/1,
    rhs/1,
    condition/1,
    has_condition/1
]).

%% Pattern matching
-export([
    match_pattern/2,
    match_pattern/3,
    unify_patterns/2,
    extract_variables/1,
    substitute_variables/2
]).

%% Guard evaluation
-export([
    evaluate_guard/2,
    guard_vars/1,
    combine_guards/2
]).

%% Equation utilities
-export([
    equation_to_string/1,
    is_equation/1,
    flip/1
]).

%%====================================================================
%% Types
%%====================================================================

-type var_name() :: atom().
-type operation() :: atom().
-type pattern() ::
    {var, var_name()} |
    {lit, term()} |
    {op, operation(), term(), pattern()} |
    {seq, [pattern()]} |
    {wildcard} |
    {bind, var_name(), pattern()}.

-type guard() ::
    {is_type, atom()} |
    {is_type, {var_name(), atom()}} |
    {compare, atom(), term()} |
    {logic_op, atom(), [guard()]} |
    {call, atom(), [term()]}.

-type condition() ::
    guard() |
    undefined |
    {'andalso', [guard()]} |
    {'orelse', [guard()]}.

-record(equation, {
    lhs :: pattern(),
    rhs :: pattern(),
    condition :: condition()
}).

-opaque equation() :: #equation{}.
-type substitution() :: #{var_name() => term()}.

-export_type([
    equation/0,
    pattern/0,
    operation/0,
    guard/0,
    condition/0,
    substitution/0
]).

%%====================================================================
%% Equation Type and Constructors
%%====================================================================

%% @doc Create a new equation with lhs, rhs, and condition.
-spec new(pattern(), pattern(), condition()) -> equation().
new(LHS, RHS, Condition) ->
    #equation{lhs = LHS, rhs = RHS, condition = Condition}.

%% @doc Create a new equation without condition (unconditional).
-spec new(pattern(), pattern()) -> equation().
new(LHS, RHS) ->
    #equation{lhs = LHS, rhs = RHS, condition = undefined}.

%% @doc Create an equation from pattern strings (simplified).
-spec from_patterns(term(), term()) -> equation().
from_patterns(LHS, RHS) ->
    new(parse_pattern(LHS), parse_pattern(RHS)).

%% @doc Create a conditional equation from pattern strings.
-spec from_patterns(term(), term(), condition()) -> equation().
from_patterns(LHS, RHS, Condition) ->
    new(parse_pattern(LHS), parse_pattern(RHS), Condition).

%%====================================================================
%% Pattern Constructors
%%====================================================================

%% @doc Create a variable pattern.
-spec var(var_name()) -> pattern().
var(Name) when is_atom(Name) ->
    {var, Name}.

%% @doc Create a literal pattern.
-spec lit(term()) -> pattern().
lit(Value) ->
    {lit, Value}.

%% @doc Create an operation pattern.
-spec op(atom(), term(), pattern()) -> pattern().
op(Operation, Value, Arg) ->
    {op, Operation, Value, Arg}.

%% @doc Create a sequence pattern (chained operations).
-spec seq([pattern()]) -> pattern().
seq(Patterns) when is_list(Patterns) ->
    {seq, Patterns}.

%% @doc Create a wildcard pattern (matches anything).
-spec wildcard() -> pattern().
wildcard() ->
    {wildcard}.

%% @doc Create a binding pattern (bind var to pattern).
-spec bind(var_name(), pattern()) -> pattern().
bind(Name, Pattern) ->
    {bind, Name, Pattern}.

%%====================================================================
%% Equation Inspection
%%====================================================================

%% @doc Get the left-hand side of an equation.
-spec lhs(equation()) -> pattern().
lhs(#equation{lhs = LHS}) -> LHS.

%% @doc Get the right-hand side of an equation.
-spec rhs(equation()) -> pattern().
rhs(#equation{rhs = RHS}) -> RHS.

%% @doc Get the condition of an equation.
-spec condition(equation()) -> condition().
condition(#equation{condition = Condition}) -> Condition.

%% @doc Check if an equation has a condition.
-spec has_condition(equation()) -> boolean().
has_condition(#equation{condition = undefined}) -> false;
has_condition(#equation{condition = _}) -> true.

%%====================================================================
%% Pattern Matching
%%====================================================================

%% @doc Match a pattern against a value, returning a substitution or mismatch.
-spec match_pattern(pattern(), term()) -> {ok, substitution()} | {error, nomatch}.
match_pattern(Pattern, Value) ->
    match_pattern(Pattern, Value, #{}).

%% @doc Match a pattern against a value with an existing substitution.
-spec match_pattern(pattern(), term(), substitution()) -> {ok, substitution()} | {error, nomatch}.
match_pattern({wildcard}, _Value, Subst) ->
    {ok, Subst};
match_pattern({var, Name}, Value, Subst) ->
    case maps:find(Name, Subst) of
        {ok, Value} -> {ok, Subst}; % Already bound to same value
        {ok, _Other} -> {error, nomatch}; % Bound to different value
        error -> {ok, Subst#{Name => Value}} % Unbound, bind it
    end;
match_pattern({lit, LitValue}, {lit, LitValue}, Subst) ->
    {ok, Subst};
match_pattern({lit, LitValue}, Value, Subst) when LitValue =:= Value ->
    {ok, Subst};
match_pattern({lit, _LitValue}, _Value, _Subst) ->
    {error, nomatch};
match_pattern({op, Op, OpValue, ArgPattern}, {op, Op, OpValue, ArgValue}, Subst) ->
    match_pattern(ArgPattern, ArgValue, Subst);
match_pattern({op, _, _, _}, _Value, _Subst) ->
    {error, nomatch};
match_pattern({seq, Patterns}, {seq, Values}, Subst) when is_list(Values) ->
    match_seq_patterns(Patterns, Values, Subst);
match_pattern({seq, Patterns}, Value, Subst) when is_list(Value) ->
    match_seq_patterns(Patterns, Value, Subst);
match_pattern({seq, _Patterns}, _Value, _Subst) ->
    {error, nomatch};
match_pattern({bind, Name, Pattern}, Value, Subst) ->
    case match_pattern(Pattern, Value, Subst) of
        {ok, NewSubst} -> bind_substitution(Name, Value, NewSubst);
        {error, nomatch} -> {error, nomatch}
    end;
match_pattern(_Other, _Value, _Subst) ->
    {error, nomatch}.

%% @private Match sequence patterns against a list.
match_seq_patterns([], [], Subst) ->
    {ok, Subst};
match_seq_patterns([_|_], [], _Subst) ->
    {error, nomatch};
match_seq_patterns([], [_|_], _Subst) ->
    {error, nomatch};
match_seq_patterns([P | Ps], [V | Vs], Subst) ->
    case match_pattern(P, V, Subst) of
        {ok, NewSubst} -> match_seq_patterns(Ps, Vs, NewSubst);
        {error, nomatch} -> {error, nomatch}
    end.

%% @doc Unify two patterns, returning a substitution if compatible.
-spec unify_patterns(pattern(), pattern()) -> {ok, substitution()} | {error, nomatch}.
unify_patterns(P1, P2) ->
    unify_patterns(P1, P2, #{}).

%% @private Unify two patterns with a substitution.
unify_patterns(Pattern, Pattern, Subst) ->
    {ok, Subst};
unify_patterns({var, N1}, {var, N2}, Subst) when N1 =:= N2 ->
    {ok, Subst};
unify_patterns({var, Name}, Pattern, Subst) ->
    case maps:find(Name, Subst) of
        {ok, Bound} -> unify_patterns(Bound, Pattern, Subst);
        error -> {ok, Subst#{Name => Pattern}}
    end;
unify_patterns(Pattern, {var, Name}, Subst) ->
    unify_patterns({var, Name}, Pattern, Subst);
unify_patterns({lit, V1}, {lit, V2}, Subst) when V1 =:= V2 ->
    {ok, Subst};
unify_patterns({op, O1, V1, A1}, {op, O2, V2, A2}, Subst) when O1 =:= O2, V1 =:= V2 ->
    case unify_patterns(A1, A2, Subst) of
        {ok, NewSubst} -> {ok, NewSubst};
        {error, nomatch} -> {error, nomatch}
    end;
unify_patterns({seq, P1s}, {seq, P2s}, Subst) when length(P1s) =:= length(P2s) ->
    unify_seq_patterns(P1s, P2s, Subst);
unify_patterns(_P1, _P2, _Subst) ->
    {error, nomatch}.

%% @private Unify sequence patterns element-wise.
unify_seq_patterns([], [], Subst) ->
    {ok, Subst};
unify_seq_patterns([P1 | Ps1], [P2 | Ps2], Subst) ->
    case unify_patterns(P1, P2, Subst) of
        {ok, NewSubst} -> unify_seq_patterns(Ps1, Ps2, NewSubst);
        {error, nomatch} -> {error, nomatch}
    end.

%% @doc Extract all variable names from a pattern.
-spec extract_variables(pattern()) -> [var_name()].
extract_variables(Pattern) ->
    extract_variables(Pattern, ordsets:new()).

%% @private Extract variables with an accumulator.
extract_variables({wildcard}, Acc) ->
    ordsets:to_list(Acc);
extract_variables({var, Name}, Acc) ->
    ordsets:to_list(ordsets:add_element(Name, Acc));
extract_variables({lit, _Value}, Acc) ->
    ordsets:to_list(Acc);
extract_variables({op, _Op, _Value, Arg}, Acc) ->
    extract_variables(Arg, Acc);
extract_variables({seq, Patterns}, Acc) ->
    lists:foldl(fun(P, A) ->
        ordsets:union(ordsets:from_list(extract_variables(P)), A)
    end, Acc, Patterns);
extract_variables({bind, Name, Pattern}, Acc) ->
    extract_variables(Pattern, ordsets:add_element(Name, Acc)).

%% @doc Substitute variables in a pattern using a substitution.
-spec substitute_variables(pattern(), substitution()) -> pattern().
substitute_variables({wildcard}, _Subst) ->
    {wildcard};
substitute_variables({var, Name}, Subst) ->
    case maps:find(Name, Subst) of
        {ok, Value} -> pattern_from_substitution(Value);
        error -> {var, Name}
    end;
substitute_variables({lit, Value}, _Subst) ->
    {lit, Value};
substitute_variables({op, Op, Value, Arg}, Subst) ->
    {op, Op, Value, substitute_variables(Arg, Subst)};
substitute_variables({seq, Patterns}, Subst) ->
    {seq, [substitute_variables(P, Subst) || P <- Patterns]};
substitute_variables({bind, Name, Pattern}, Subst) ->
    {bind, Name, substitute_variables(Pattern, Subst)}.

%%====================================================================
%% Guard Evaluation
%%====================================================================

%% @doc Evaluate a guard condition with a substitution.
-spec evaluate_guard(condition(), substitution()) -> boolean().
evaluate_guard(undefined, _Subst) ->
    true;
evaluate_guard({is_type, Type}, Subst) when is_atom(Type) ->
    case maps:values(Subst) of
        [] -> true;
        Values -> lists:all(fun(Value) -> value_has_type(Type, unwrap_value(Value)) end, Values)
    end;
evaluate_guard({is_type, {Name, Type}}, Subst) when is_atom(Name), is_atom(Type) ->
    case maps:find(Name, Subst) of
        {ok, Value} -> value_has_type(Type, unwrap_value(Value));
        error -> true
    end;
evaluate_guard({compare, Op, Values}, Subst) ->
    evaluate_compare_guard(Op, Values, Subst);
evaluate_guard({'andalso', Guards}, Subst) ->
    lists:all(fun(G) -> evaluate_guard(G, Subst) end, Guards);
evaluate_guard({'orelse', Guards}, Subst) ->
    lists:any(fun(G) -> evaluate_guard(G, Subst) end, Guards);
evaluate_guard({logic_op, 'andalso', Guards}, Subst) ->
    lists:all(fun(G) -> evaluate_guard(G, Subst) end, Guards);
evaluate_guard({logic_op, 'orelse', Guards}, Subst) ->
    lists:any(fun(G) -> evaluate_guard(G, Subst) end, Guards);
evaluate_guard({call, Fun, Args}, Subst) ->
    ResolvedArgs = [resolve_guard_value(Arg, Subst) || Arg <- Args],
    case lists:any(fun is_unresolved_guard_value/1, ResolvedArgs) of
        true ->
            true;
        false ->
            apply_guard_call(Fun, ResolvedArgs)
    end.

%% @doc Extract variable names referenced in a guard.
-spec guard_vars(condition()) -> [var_name()].
guard_vars(undefined) ->
    [];
guard_vars({is_type, {Name, _Type}}) ->
    [Name];
guard_vars({is_type, _Type}) ->
    [];
guard_vars({compare, _Op, Values}) when is_list(Values) ->
    [V || V <- Values, is_atom(V) andalso V =/= true andalso V =/= false andalso V =/= undefined];
guard_vars({compare, _Op, Value}) ->
    case is_atom(Value) andalso Value =/= true andalso Value =/= false andalso Value =/= undefined of
        true -> [Value];
        false -> []
    end;
guard_vars({'andalso', Guards}) ->
    lists:usort(lists:flatmap(fun guard_vars/1, Guards));
guard_vars({'orelse', Guards}) ->
    lists:usort(lists:flatmap(fun guard_vars/1, Guards));
guard_vars({logic_op, _Op, Guards}) ->
    lists:usort(lists:flatmap(fun guard_vars/1, Guards));
guard_vars({call, _Fun, Args}) when is_list(Args) ->
    [A || A <- Args, is_atom(A)].

%% @doc Combine guards with logical and.
-spec combine_guards(condition(), condition()) -> condition().
combine_guards(undefined, G) -> G;
combine_guards(G, undefined) -> G;
combine_guards(G1, G2) -> {logic_op, 'andalso', [G1, G2]}.

%%====================================================================
%% Equation Utilities
%%====================================================================

%% @doc Convert an equation to a string representation.
-spec equation_to_string(equation()) -> string().
equation_to_string(#equation{lhs = LHS, rhs = RHS, condition = undefined}) ->
    lists:flatten(io_lib:format("~s ≡ ~s", [pattern_to_string(LHS), pattern_to_string(RHS)]));
equation_to_string(#equation{lhs = LHS, rhs = RHS, condition = Condition}) ->
    lists:flatten(io_lib:format("~s ≡ ~s when ~s", [pattern_to_string(LHS), pattern_to_string(RHS), guard_to_string(Condition)])).

%% @doc Check if a term is an equation.
-spec is_equation(term()) -> boolean().
is_equation(#equation{}) -> true;
is_equation(_) -> false.

%% @doc Flip an equation (swap lhs and rhs).
-spec flip(equation()) -> equation().
flip(#equation{lhs = LHS, rhs = RHS, condition = Condition}) ->
    #equation{lhs = RHS, rhs = LHS, condition = Condition}.

%%====================================================================
%% Private Functions
%%====================================================================

%% @private Parse a simple pattern from a term.
-spec parse_pattern(term()) -> pattern().
parse_pattern({var, Name}) when is_atom(Name) -> {var, Name};
parse_pattern({lit, Value}) -> {lit, Value};
parse_pattern({op, Op, Value, Arg}) -> {op, Op, Value, parse_pattern(Arg)};
parse_pattern({seq, Patterns}) when is_list(Patterns) ->
    {seq, [parse_pattern(P) || P <- Patterns]};
parse_pattern({wildcard}) -> {wildcard};
parse_pattern({bind, Name, Pattern}) when is_atom(Name) ->
    {bind, Name, parse_pattern(Pattern)};
parse_pattern(Other) when is_atom(Other), Other =/= true, Other =/= false, Other =/= undefined ->
    % Atoms are treated as variables
    {var, Other};
parse_pattern(Other) -> {lit, Other}.

%% @private Convert a pattern to a string.
-spec pattern_to_string(pattern()) -> string().
pattern_to_string({wildcard}) -> "_";
pattern_to_string({var, Name}) -> atom_to_list(Name);
pattern_to_string({lit, Value}) ->
    case io_lib:format("~p", [Value]) of
        [$", S, $"] -> S;  % Strip quotes from string
        S -> lists:flatten(S)  % Ensure flat string
    end;
pattern_to_string({op, Op, Value, Arg}) ->
    lists:flatten(io_lib:format("~s(~p, ~s)", [Op, Value, pattern_to_string(Arg)]));
pattern_to_string({seq, Patterns}) ->
    Strings = [pattern_to_string(P) || P <- Patterns],
    string:join(["["] ++ Strings ++ ["]"], "; ");
pattern_to_string({bind, Name, Pattern}) ->
    lists:flatten(io_lib:format("~s@~s", [Name, pattern_to_string(Pattern)])).

%% @private Convert a guard to a string.
-spec guard_to_string(condition()) -> string().
guard_to_string({is_type, Type}) ->
    io_lib:format("is_~s(~s)", [Type, "_"]);
guard_to_string({compare, Op, Values}) when is_list(Values) ->
    StrValues = [value_to_string(V) || V <- Values],
    io_lib:format("~s ~s", [string:join(StrValues, atom_to_list(Op)), "_"]);
guard_to_string({logic_op, Op, Guards}) ->
    StrGuards = [guard_to_string(G) || G <- Guards],
    io_lib:format("(~s)", [string:join(StrGuards, atom_to_list(Op))]);
guard_to_string({call, Fun, Args}) when is_list(Args) ->
    StrArgs = [value_to_string(A) || A <- Args],
    io_lib:format("~s(~s)", [Fun, string:join(StrArgs, ", ")]).

%% @private Convert a value to a string for guards.
-spec value_to_string(term()) -> string().
value_to_string(V) when is_atom(V) -> atom_to_list(V);
value_to_string(V) when is_integer(V) -> integer_to_list(V);
value_to_string(V) when is_float(V) -> float_to_list(V);
value_to_string(V) when is_list(V) -> io_lib:format("~p", [V]);
value_to_string(V) -> io_lib:format("~p", [V]).

bind_substitution(Name, Value, Subst) ->
    case maps:find(Name, Subst) of
        {ok, Existing} when Existing =:= Value ->
            {ok, Subst};
        {ok, _Existing} ->
            {error, nomatch};
        error ->
            {ok, Subst#{Name => Value}}
    end.

pattern_from_substitution(Value = {var, _}) ->
    Value;
pattern_from_substitution(Value = {lit, _}) ->
    Value;
pattern_from_substitution(Value = {op, _, _, _}) ->
    Value;
pattern_from_substitution(Value = {seq, _}) ->
    Value;
pattern_from_substitution(Value = {wildcard}) ->
    Value;
pattern_from_substitution(Value = {bind, _, _}) ->
    Value;
pattern_from_substitution(Value) ->
    {lit, Value}.

unwrap_value({lit, Value}) ->
    Value;
unwrap_value(Value) ->
    Value.

evaluate_compare_guard(Op, Values, Subst) when is_list(Values) ->
    case [resolve_guard_value(Value, Subst) || Value <- Values] of
        [Left, Right] ->
            case is_unresolved_guard_value(Left) orelse is_unresolved_guard_value(Right) of
                true -> true;
                false -> compare_values(Op, Left, Right)
            end;
        _ ->
            true
    end;
evaluate_compare_guard(Op, Value, Subst) ->
    evaluate_compare_guard(Op, [Value], Subst).

resolve_guard_value(Value, Subst) when is_atom(Value), Value =/= true, Value =/= false, Value =/= undefined ->
    maps:get(Value, Subst, Value);
resolve_guard_value(Value, _Subst) ->
    Value.

is_unresolved_guard_value(Value) when is_atom(Value), Value =/= true, Value =/= false, Value =/= undefined ->
    true;
is_unresolved_guard_value(_) ->
    false.

compare_values('>', Left, Right) -> Left > Right;
compare_values('<', Left, Right) -> Left < Right;
compare_values('<<', Left, Right) -> Left < Right;
compare_values('<=', Left, Right) -> Left =< Right;
compare_values('>=', Left, Right) -> Left >= Right;
compare_values('==', Left, Right) -> Left == Right;
compare_values('=:=', Left, Right) -> Left =:= Right;
compare_values('/=', Left, Right) -> Left /= Right;
compare_values('=/=', Left, Right) -> Left =/= Right;
compare_values(_, Left, Right) -> Left =:= Right.

value_has_type(integer, Value) -> is_integer(Value);
value_has_type(float, Value) -> is_float(Value);
value_has_type(number, Value) -> is_number(Value);
value_has_type(atom, Value) -> is_atom(Value);
value_has_type(binary, Value) -> is_binary(Value);
value_has_type(list, Value) -> is_list(Value);
value_has_type(tuple, Value) -> is_tuple(Value);
value_has_type(map, Value) -> is_map(Value);
value_has_type(pid, Value) -> is_pid(Value);
value_has_type(reference, Value) -> is_reference(Value);
value_has_type(boolean, Value) -> is_boolean(Value);
value_has_type(_, _Value) -> true.

apply_guard_call(Fun, Args) ->
    try apply(erlang, Fun, Args) of
        Result when is_boolean(Result) -> Result;
        _ -> true
    catch
        _:_ -> true
    end.
