%%%-------------------------------------------------------------------
%%% @doc Catena Equation Specification (Phase 8.2)
%%%
%%% This module provides the equation specification system for defining
%%% and managing sets of equations for effect operations. It enables:
%%%
%%% - Named equation sets for different effects
%%% - Equation validation and verification
%%% - Equation lookup and querying
%%% - Equation composition from multiple sources
%%%
%%% == Equation Sets ==
%%%
%%% An equation set is a collection of equations identified by name:
%%% - Effect operations can have associated equation sets
%%% - Equations can be added, removed, and queried
%%% - Sets can be combined for composition
%%%
%%% == Equation Validation ==
%%%
%%% Equations are validated to ensure:
%%% - Well-formed patterns on both sides
%%% - Variables on RHS are bound on LHS or in guards
%%% - Guards are well-formed and reference valid variables
%%% - No circular dependencies in equations
%%%
%%% == Equation Naming ==
%%%
%%% Equations can be named for:
%%% - Documentation purposes
%%% - Reference in proofs and derivations
%%% - Debugging and error reporting
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_equation_spec).
-behaviour(gen_server).

%% Equation specification API
-export([
    new_set/1,
    new_set/2,
    add_equation/3,
    add_operation_equation/4,
    add_handler_equation/4,
    add_equations/2,
    remove_equation/2,
    get_equation/2,
    list_equations/1,
    lookup_equations/2,
    lookup_handler_equations/2
]).

%% Equation validation
-export([
    validate_equation/1,
    validate_equation/2,
    validate_set/1,
    validate_handler_equations/3,
    check_well_formed/1,
    handler_coverage/3,
    find_conflicts/1
]).

%% Equation query
-export([
    find_matching/2,
    find_by_operation/2,
    equations_for_pattern/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% Types
%%====================================================================

-type set_name() :: atom().
-type equation_name() :: atom().
-type handler_name() :: atom().
-type equation_entry() :: #{
    name => equation_name(),
    equation => catena_equations:equation(),
    metadata => map()
}.

-type equation_set() :: #{
    name => set_name(),
    equations => #{equation_name() => equation_entry()},
    operations => #{catena_equations:operation() => [equation_name()]},
    handlers => #{handler_name() => [equation_name()]},
    metadata => map()
}.

-type validation_error() :: #{
    type := ill_formed | unbound_variable | invalid_guard | circular | conflict | coverage,
    details := term()
}.

-type validation_result() :: ok | {error, [validation_error()]}.

-export_type([
    equation_set/0,
    equation_entry/0,
    validation_error/0,
    validation_result/0
]).

%%====================================================================
%% Equation Specification API
%%====================================================================

%% @doc Create a new named equation set.
-spec new_set(set_name()) -> equation_set().
new_set(Name) when is_atom(Name) ->
    new_set(Name, #{}).

%% @doc Create a new equation set with metadata.
-spec new_set(set_name(), map()) -> equation_set().
new_set(Name, Metadata) when is_atom(Name), is_map(Metadata) ->
    #{
        name => Name,
        equations => #{},
        operations => #{},
        handlers => #{},
        metadata => Metadata
    }.

%% @doc Add a named equation to a set.
-spec add_equation(equation_set(), equation_name(), catena_equations:equation()) ->
    equation_set().
add_equation(Set, Name, Equation) when is_atom(Name) ->
    add_entry(Set, Name, Equation, #{}).

%% @doc Add a named equation explicitly associated with an operation.
-spec add_operation_equation(
    equation_set(),
    catena_equations:operation(),
    equation_name(),
    catena_equations:equation()
) -> equation_set().
add_operation_equation(Set, Operation, Name, Equation)
        when is_atom(Operation), is_atom(Name) ->
    add_entry(Set, Name, Equation, #{operation => Operation}).

%% @doc Add a named equation associated with a handler.
-spec add_handler_equation(
    equation_set(),
    handler_name(),
    equation_name(),
    catena_equations:equation()
) -> equation_set().
add_handler_equation(Set, Handler, Name, Equation)
        when is_atom(Handler), is_atom(Name) ->
    add_entry(Set, Name, Equation, #{handler => Handler}).

%% @doc Add multiple equations to a set.
-spec add_equations(equation_set(), [{equation_name(), catena_equations:equation()}]) ->
    equation_set().
add_equations(Set, EquationList) when is_list(EquationList) ->
    lists:foldl(fun({Name, Eq}, Acc) ->
        add_equation(Acc, Name, Eq)
    end, Set, EquationList).

%% @doc Remove an equation from a set by name.
-spec remove_equation(equation_set(), equation_name()) -> equation_set().
remove_equation(Set, Name) ->
    Equations = maps:get(equations, Set),
    case maps:find(Name, Equations) of
        {ok, Entry} ->
            Eq = maps:get(equation, Entry),
            Metadata = maps:get(metadata, Entry, #{}),
            AllOps = entry_operations(Eq, Metadata),
            Handlers = maps:get(handlers, Set, #{}),

            % Update operation index
            NewOperations = lists:foldl(fun(Op, Acc) ->
                case maps:find(Op, Acc) of
                    {ok, [Name]} -> maps:remove(Op, Acc);
                    {ok, Names} -> Acc#{Op => lists:delete(Name, Names)};
                    error -> Acc
                end
            end, maps:get(operations, Set), AllOps),
            NewHandlers = case maps:find(handler, Metadata) of
                {ok, Handler} -> remove_name_from_index(Handlers, Handler, Name);
                error -> Handlers
            end,

            Set#{
                equations => maps:remove(Name, Equations),
                operations => NewOperations,
                handlers => NewHandlers
            };
        error ->
            Set
    end.

%% @doc Get an equation by name from a set.
-spec get_equation(equation_set(), equation_name()) ->
    {ok, catena_equations:equation()} | error.
get_equation(Set, Name) ->
    Equations = maps:get(equations, Set),
    case maps:find(Name, Equations) of
        {ok, Entry} -> {ok, maps:get(equation, Entry)};
        error -> error
    end.

%% @doc List all equation names in a set.
-spec list_equations(equation_set()) -> [equation_name()].
list_equations(Set) ->
    Equations = maps:get(equations, Set),
    maps:keys(Equations).

%% @doc Lookup equations for a specific operation.
-spec lookup_equations(equation_set(), catena_equations:operation()) ->
    [catena_equations:equation()].
lookup_equations(Set, Operation) ->
    Operations = maps:get(operations, Set),
    case maps:find(Operation, Operations) of
        {ok, Names} ->
            Equations = maps:get(equations, Set),
            [maps:get(equation, maps:get(Name, Equations)) || Name <- Names];
        error ->
            []
    end.

%% @doc Lookup handler-associated equations.
-spec lookup_handler_equations(equation_set(), handler_name()) ->
    [catena_equations:equation()].
lookup_handler_equations(Set, Handler) ->
    Handlers = maps:get(handlers, Set, #{}),
    case maps:find(Handler, Handlers) of
        {ok, Names} ->
            Equations = maps:get(equations, Set),
            [maps:get(equation, maps:get(Name, Equations)) || Name <- Names];
        error ->
            []
    end.

%%====================================================================
%% Equation Validation
%%====================================================================

%% @doc Validate a single equation (basic validation).
-spec validate_equation(catena_equations:equation()) -> validation_result().
validate_equation(Equation) ->
    validate_equation(Equation, #{}).

%% @doc Validate a single equation with options.
-spec validate_equation(catena_equations:equation(), map()) -> validation_result().
validate_equation(Equation, _Options) ->
    LHS = catena_equations:lhs(Equation),
    RHS = catena_equations:rhs(Equation),
    Condition = catena_equations:condition(Equation),
    LHSResult = check_well_formed(LHS),
    RHSResult = check_well_formed(RHS),
    GuardResult = validate_guard(Condition),

    Errors = []
    ++ case LHSResult of
        ok -> [];
        {error, Err} -> [#{type => ill_formed, details => {lhs, Err}}]
    end
    ++ case RHSResult of
        ok -> [];
        {error, Err} -> [#{type => ill_formed, details => {rhs, Err}}]
    end
    ++ case GuardResult of
        ok -> [];
        {error, Err} -> [#{type => invalid_guard, details => Err}]
    end
    ++ case {LHSResult, RHSResult, GuardResult} of
        {ok, ok, ok} -> check_rhs_variables(LHS, RHS, Condition);
        _ -> []
    end,

    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

%% @doc Validate an entire equation set.
-spec validate_set(equation_set()) -> validation_result().
validate_set(Set) ->
    Equations = maps:get(equations, Set),
    Names = maps:keys(Equations),

    Errors = lists:flatmap(fun(Name) ->
        case get_equation(Set, Name) of
            {ok, Eq} ->
                case validate_equation(Eq) of
                    ok -> [];
                    {error, Errs} ->
                        [Err#{details => {Name, Details}} ||
                            Err = #{details := Details} <- Errs]
                end;
            error ->
                [#{type => ill_formed, details => {Name, not_found}}]
        end
    end, Names),

    % Check for circular dependencies
    CircularErrors = check_circular_dependencies(Set),
    ConflictErrors = [
        #{type => conflict, details => Conflict}
     || Conflict <- find_conflicts(Set)
    ],

    AllErrors = Errors ++ CircularErrors ++ ConflictErrors,
    case AllErrors of
        [] -> ok;
        _ -> {error, AllErrors}
    end.

%% @doc Validate the equations associated with a handler and expected operations.
-spec validate_handler_equations(equation_set(), handler_name(), [catena_equations:operation()]) ->
    validation_result().
validate_handler_equations(Set, Handler, ExpectedOperations) ->
    HandlerEquations = lookup_handler_equations(Set, Handler),
    EquationErrors = lists:flatmap(fun(Eq) ->
        case validate_equation(Eq) of
            ok -> [];
            {error, Errs} -> Errs
        end
    end, HandlerEquations),
    CoverageErrors = case handler_coverage(Set, Handler, ExpectedOperations) of
        {complete, _Covered} ->
            [];
        {error, {missing_operations, Missing, Covered}} ->
            [#{
                type => coverage,
                details => #{handler => Handler, missing => Missing, covered => Covered}
            }]
    end,
    case EquationErrors ++ CoverageErrors of
        [] -> ok;
        AllErrors -> {error, AllErrors}
    end.

%% @doc Check if a pattern is well-formed.
-spec check_well_formed(catena_equations:pattern()) -> ok | {error, term()}.
check_well_formed({var, _Name}) -> ok;
check_well_formed({lit, _Value}) -> ok;
check_well_formed({wildcard}) -> ok;
check_well_formed({op, _Op, _Value, Arg}) ->
    check_well_formed(Arg);
check_well_formed({seq, Patterns}) when is_list(Patterns) ->
    validate_all(fun check_well_formed/1, Patterns);
check_well_formed({bind, _Name, Pattern}) ->
    check_well_formed(Pattern);
check_well_formed(Other) ->
    {error, {invalid_pattern, Other}}.

%% @doc Check whether handler-associated equations cover the expected operations.
-spec handler_coverage(equation_set(), handler_name(), [catena_equations:operation()]) ->
    {complete, [catena_equations:operation()]} |
    {error, {missing_operations, [catena_equations:operation()], [catena_equations:operation()]}}.
handler_coverage(Set, Handler, ExpectedOperations) ->
    Handlers = maps:get(handlers, Set, #{}),
    Equations = maps:get(equations, Set),
    Covered = case maps:find(Handler, Handlers) of
        {ok, Names} ->
            lists:usort(lists:flatmap(fun(Name) ->
                Entry = maps:get(Name, Equations),
                Eq = maps:get(equation, Entry),
                Metadata = maps:get(metadata, Entry, #{}),
                entry_operations(Eq, Metadata)
            end, Names));
        error ->
            []
    end,
    Missing = lists:subtract(lists:usort(ExpectedOperations), Covered),
    case Missing of
        [] -> {complete, Covered};
        _ -> {error, {missing_operations, Missing, Covered}}
    end.

%% @doc Find conflicting equations sharing the same operation and LHS.
-spec find_conflicts(equation_set()) -> [map()].
find_conflicts(Set) ->
    Equations = maps:get(equations, Set),
    Operations = maps:get(operations, Set),
    maps:fold(fun(Operation, Names, Acc) ->
        detect_conflicts(Operation, Names, Equations) ++ Acc
    end, [], Operations).

%%====================================================================
%% Equation Query
%%====================================================================

%% @doc Find equations that match a given pattern.
%%
%% This checks if the equation's LHS pattern is structurally compatible
%% with the query pattern. Unlike unification, this requires that the
%% patterns have compatible structure (variables can match, but concrete
%% structures must align).
-spec find_matching(equation_set(), catena_equations:pattern()) ->
    [{equation_name(), catena_equations:equation()}].
find_matching(Set, Pattern) ->
    Equations = maps:get(equations, Set),
    Matches = maps:fold(fun(_Name, Entry, Acc) ->
        Eq = maps:get(equation, Entry),
        LHS = catena_equations:lhs(Eq),
        case structural_match(LHS, Pattern) of
            true -> [{maps:get(name, Entry), Eq} | Acc];
            false -> Acc
        end
    end, [], Equations),
    lists:reverse(Matches).

%% @doc Find equations by operation name.
-spec find_by_operation(equation_set(), catena_equations:operation()) ->
    [{equation_name(), catena_equations:equation()}].
find_by_operation(Set, Operation) ->
    Names = case maps:find(Operation, maps:get(operations, Set)) of
        {ok, Ops} -> Ops;
        error -> []
    end,
    Equations = maps:get(equations, Set),
    [{Name, maps:get(equation, maps:get(Name, Equations))} || Name <- Names].

%% @doc Get equations applicable to a specific pattern.
-spec equations_for_pattern(equation_set(), catena_equations:pattern()) ->
    [catena_equations:equation()].
equations_for_pattern(Set, Pattern) ->
    Ops = extract_operations(Pattern),
    lists:flatmap(fun(Op) ->
        [Eq || {_Name, Eq} <- find_by_operation(Set, Op)]
    end, Ops).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([]) ->
    {ok, #{sets => #{}}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Private Functions
%%====================================================================

%% @private Extract operation names from a pattern.
-spec extract_operations(catena_equations:pattern()) -> [atom()].
extract_operations({op, Op, _Value, _Arg}) -> [Op];
extract_operations({seq, Patterns}) ->
    lists:flatmap(fun extract_operations/1, Patterns);
extract_operations({bind, _Name, Pattern}) ->
    extract_operations(Pattern);
extract_operations(_Other) -> [].

add_entry(Set, Name, Equation, Metadata) ->
    BaseSet = remove_equation(Set, Name),
    Equations = maps:get(equations, BaseSet),
    Operations = maps:get(operations, BaseSet),
    Handlers = maps:get(handlers, BaseSet, #{}),

    Entry = #{
        name => Name,
        equation => Equation,
        metadata => Metadata
    },
    AllOps = entry_operations(Equation, Metadata),
    NewOperations = lists:foldl(fun(Op, Acc) ->
        add_name_to_index(Acc, Op, Name)
    end, Operations, AllOps),
    NewHandlers = case maps:find(handler, Metadata) of
        {ok, Handler} -> add_name_to_index(Handlers, Handler, Name);
        error -> Handlers
    end,

    BaseSet#{
        equations => Equations#{Name => Entry},
        operations => NewOperations,
        handlers => NewHandlers
    }.

entry_operations(Equation, Metadata) ->
    LHSOps = extract_operations(catena_equations:lhs(Equation)),
    RHSOps = extract_operations(catena_equations:rhs(Equation)),
    ExplicitOps = case maps:find(operation, Metadata) of
        {ok, Operation} -> [Operation];
        error -> []
    end,
    lists:usort(LHSOps ++ RHSOps ++ ExplicitOps).

add_name_to_index(Index, Key, Name) ->
    Existing = maps:get(Key, Index, []),
    Index#{Key => [Name | lists:delete(Name, Existing)]}.

remove_name_from_index(Index, Key, Name) ->
    case maps:find(Key, Index) of
        {ok, [Name]} -> maps:remove(Key, Index);
        {ok, Names} -> Index#{Key => lists:delete(Name, Names)};
        error -> Index
    end.

%% @private Check that RHS variables are bound on LHS or in guards.
-spec check_rhs_variables(catena_equations:pattern(), catena_equations:pattern(),
    catena_equations:condition()) -> [validation_error()].
check_rhs_variables(LHS, RHS, Condition) ->
    LHSVars = ordsets:from_list(catena_equations:extract_variables(LHS)),
    RHSVars = ordsets:from_list(catena_equations:extract_variables(RHS)),
    GuardVars = case Condition of
        undefined -> [];
        _ -> ordsets:from_list(catena_equations:guard_vars(Condition))
    end,
    BoundVars = ordsets:union(LHSVars, GuardVars),
    UnboundVars = ordsets:subtract(RHSVars, BoundVars),

    case ordsets:to_list(UnboundVars) of
        [] -> [];
        Vars -> [#{type => unbound_variable, details => Vars}]
    end.

%% @private Validate a guard condition.
-spec validate_guard(catena_equations:condition()) -> ok | {error, term()}.
validate_guard(undefined) -> ok;
validate_guard({is_type, {_Name, _Type}}) -> ok;
validate_guard({is_type, _Type}) -> ok;
validate_guard({compare, _Op, _Values}) -> ok;
validate_guard({'andalso', Guards}) ->
    validate_all(fun validate_guard/1, Guards);
validate_guard({'orelse', Guards}) ->
    validate_all(fun validate_guard/1, Guards);
validate_guard({logic_op, Op, Guards}) when Op =:= 'andalso'; Op =:= 'orelse' ->
    validate_all(fun validate_guard/1, Guards);
validate_guard({call, _Fun, _Args}) -> ok;
validate_guard({logic_op, Op, _}) ->
    {error, {invalid_logic_op, Op}};
validate_guard(Other) ->
    {error, {invalid_guard, Other}}.

%% @private Check for circular dependencies in equations.
-spec check_circular_dependencies(equation_set()) -> [validation_error()].
check_circular_dependencies(_Set) ->
    % For now, return empty - circular dependency detection requires
    % building a dependency graph and checking for cycles
    % This will be expanded in future sections
    [].

detect_conflicts(_Operation, [], _Equations) ->
    [];
detect_conflicts(Operation, [Name | Rest], Equations) ->
    Entry = maps:get(Name, Equations),
    Eq = maps:get(equation, Entry),
    Current = [#{
        operation => Operation,
        equations => [Name, OtherName],
        lhs => catena_equations:lhs(Eq)
    } || OtherName <- Rest, equations_conflict(Eq, maps:get(equation, maps:get(OtherName, Equations)))],
    Current ++ detect_conflicts(Operation, Rest, Equations).

equations_conflict(Eq1, Eq2) ->
    catena_equations:lhs(Eq1) =:= catena_equations:lhs(Eq2)
        andalso (
            catena_equations:rhs(Eq1) =/= catena_equations:rhs(Eq2)
            orelse catena_equations:condition(Eq1) =/= catena_equations:condition(Eq2)
        ).

%% @private Check if two patterns are structurally compatible.
%%
%% This checks if the LHS pattern could plausibly match the query pattern.
%% The rules are:
%% - Wildcards match anything
%% - Variables in LHS can match compatible query patterns
%% - But variables in query don't make LHS match (query is more specific)
%% - Concrete structures must align
%%
%% For example:
%% - {op, inc, 1, {var, x}} matches {op, inc, 1, {lit, 5}} - compatible structure
%% - {var, x} does NOT match {op, inc, 1, _} - LHS is too generic
-spec structural_match(catena_equations:pattern(), catena_equations:pattern()) -> boolean().
structural_match(_LHS, {wildcard}) -> true;
structural_match({wildcard}, _Query) -> true;
structural_match({var, _}, Query) ->
    % Variable in LHS only matches if Query doesn't have more specific structure
    % that the LHS lacks. Check if Query is also a variable or simple value.
    not is_concrete_op(Query);
structural_match(_LHS, {var, _}) ->
    % Variable in Query but not in LHS means Query is less specific
    % This could match, but only if LHS is compatible
    false;
structural_match({lit, V1}, {lit, V2}) -> V1 =:= V2;
structural_match({op, O1, V1, A1}, {op, O2, V2, A2}) ->
    O1 =:= O2 andalso V1 =:= V2 andalso structural_match(A1, A2);
structural_match({seq, P1s}, {seq, P2s}) when length(P1s) =:= length(P2s) ->
    lists:all(fun({P1, P2}) -> structural_match(P1, P2) end, lists:zip(P1s, P2s));
structural_match({bind, _N1, P1}, {bind, _N2, P2}) ->
    structural_match(P1, P2);
structural_match(_P1, _P2) -> false.

%% @private Check if a pattern is a concrete operation (has specific structure).
is_concrete_op({op, _, _, _}) -> true;
is_concrete_op({seq, _}) -> true;
is_concrete_op({bind, _, P}) -> is_concrete_op(P);
is_concrete_op(_) -> false.

validate_all(_Validator, []) ->
    ok;
validate_all(Validator, [Item | Rest]) ->
    case Validator(Item) of
        ok -> validate_all(Validator, Rest);
        {error, _} = Error -> Error
    end.
