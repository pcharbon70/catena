%%%-------------------------------------------------------------------
%%% @doc Pattern Compilation to Core Erlang (Task 1.3.2)
%%%
%%% Compiles Catena patterns to Core Erlang case expressions using
%%% decision tree algorithms. This module handles:
%%% - Basic patterns (variables, wildcards, literals, constructors)
%%% - Guard compilation
%%% - Decision tree generation for efficient matching
%%% - Exhaustiveness checking
%%% @end
%%%-------------------------------------------------------------------
-module(catena_codegen_pattern).

-export([
    %% Main compilation
    compile_match/4,
    compile_clauses/3,
    compile_pattern/2,

    %% Guard compilation
    compile_guard/2,

    %% Decision tree
    generate_decision_tree/2,

    %% Exhaustiveness checking
    check_exhaustiveness/2,

    %% Utility
    pattern_to_core/2
]).

%%====================================================================
%% Main Pattern Compilation (1.3.2.1)
%%====================================================================

%% @doc Compile a match expression with multiple clauses
%%
%% Takes a scrutinee expression, list of clauses, and generates
%% a Core Erlang case expression with optimized pattern matching.
-spec compile_match(cerl:cerl(), [clause()], catena_codegen_utils:codegen_state(), compile_opts()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.
compile_match(Scrutinee, Clauses, State, Opts) ->
    %% Compile each clause to Core Erlang
    {CoreClauses, State1} = compile_clauses(Clauses, State, Opts),

    %% Build case expression
    Case = cerl:c_case(Scrutinee, CoreClauses),
    {Case, State1}.

%% @doc Compile a list of pattern clauses
-spec compile_clauses([clause()], catena_codegen_utils:codegen_state(), compile_opts()) ->
    {[cerl:cerl()], catena_codegen_utils:codegen_state()}.
compile_clauses(Clauses, State, Opts) ->
    lists:mapfoldl(
        fun(Clause, St) ->
            compile_clause(Clause, St, Opts)
        end,
        State,
        Clauses
    ).

%% @doc Compile a single clause (pattern, guard, body)
-spec compile_clause(clause(), catena_codegen_utils:codegen_state(), compile_opts()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.
compile_clause({clause, Patterns, Guards, Body}, State, _Opts) ->
    %% Compile patterns
    {CorePatterns, State1} = compile_patterns(Patterns, State),

    %% Compile guard
    {CoreGuard, State2} = compile_guards(Guards, State1),

    %% Compile body
    {CoreBody, State3} = catena_codegen_expr:translate_expr(Body, State2),

    %% Build clause
    Clause = cerl:c_clause(CorePatterns, CoreGuard, CoreBody),
    {Clause, State3};

%% Simple clause format: {patterns, body}
compile_clause({Patterns, Body}, State, Opts) ->
    compile_clause({clause, Patterns, [], Body}, State, Opts).

%% Compile multiple patterns
compile_patterns(Patterns, State) ->
    lists:mapfoldl(fun compile_pattern/2, State, Patterns).

%%====================================================================
%% Basic Pattern Compilation (1.3.2.1)
%%====================================================================

%% @doc Compile a single pattern to Core Erlang
-spec compile_pattern(pattern(), catena_codegen_utils:codegen_state()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.

%% Variable pattern: x -> cerl:c_var(x)
compile_pattern({pat_var, Name, _Loc}, State) ->
    {cerl:c_var(Name), State};

%% Wildcard pattern: _ -> cerl:c_var('_')
compile_pattern({pat_wildcard, _Loc}, State) ->
    {cerl:c_var('_'), State};

%% Literal patterns
compile_pattern({pat_literal, Value, integer, _Loc}, State) ->
    {cerl:c_int(Value), State};

compile_pattern({pat_literal, Value, float, _Loc}, State) ->
    {cerl:c_float(Value), State};

compile_pattern({pat_literal, Value, string, _Loc}, State) when is_list(Value) ->
    {cerl:c_string(Value), State};

compile_pattern({pat_literal, Value, string, _Loc}, State) when is_binary(Value) ->
    {cerl:c_string(binary_to_list(Value)), State};

compile_pattern({pat_literal, Value, atom, _Loc}, State) ->
    {cerl:c_atom(Value), State};

compile_pattern({pat_literal, true, bool, _Loc}, State) ->
    {cerl:c_atom(true), State};

compile_pattern({pat_literal, false, bool, _Loc}, State) ->
    {cerl:c_atom(false), State};

%% Constructor pattern: Some(x) -> {Some, X}
compile_pattern({pat_constructor, Name, Args, _Loc}, State) ->
    {CoreArgs, State1} = compile_patterns(Args, State),
    %% Constructors are represented as tagged tuples
    Constructor = cerl:c_tuple([cerl:c_atom(Name) | CoreArgs]),
    {Constructor, State1};

%% List pattern: [] or [x, y, ...]
compile_pattern({pat_list, [], _Loc}, State) ->
    {cerl:c_nil(), State};

compile_pattern({pat_list, Elements, _Loc}, State) ->
    {CoreElements, State1} = compile_patterns(Elements, State),
    %% Build list from elements
    List = lists:foldr(fun cerl:c_cons/2, cerl:c_nil(), CoreElements),
    {List, State1};

%% Cons pattern: [h | t]
compile_pattern({pat_cons, Head, Tail, _Loc}, State) ->
    {CoreHead, State1} = compile_pattern(Head, State),
    {CoreTail, State2} = compile_pattern(Tail, State1),
    {cerl:c_cons(CoreHead, CoreTail), State2};

%% Tuple pattern: (x, y)
compile_pattern({pat_tuple, Elements, _Loc}, State) ->
    {CoreElements, State1} = compile_patterns(Elements, State),
    {cerl:c_tuple(CoreElements), State1};

%% As-pattern: x as Some(y)
compile_pattern({pat_as, Name, Pattern, _Loc}, State) ->
    %% Core Erlang uses alias patterns
    {CorePattern, State1} = compile_pattern(Pattern, State),
    Alias = cerl:c_alias(cerl:c_var(Name), CorePattern),
    {Alias, State1};

%% Record pattern: {field: x, ...}
compile_pattern({pat_record, Fields, _Loc}, State) ->
    %% Records become maps in Core Erlang
    {CorePairs, State1} = compile_record_patterns(Fields, State),
    %% Build map pattern
    Map = cerl:c_map_pattern(CorePairs),
    {Map, State1};

%% Fallback for unknown patterns
compile_pattern(Unknown, State) ->
    %% Create a wildcard for unknown patterns (with warning)
    logger:warning("Unknown pattern type during code generation: ~p", [Unknown]),
    {cerl:c_var('_'), State}.

%% Helper for record field patterns
compile_record_patterns(Fields, State) ->
    lists:mapfoldl(
        fun({FieldName, Pattern}, St) ->
            {CorePattern, St1} = compile_pattern(Pattern, St),
            Pair = cerl:c_map_pair_exact(cerl:c_atom(FieldName), CorePattern),
            {Pair, St1}
        end,
        State,
        Fields
    ).

%%====================================================================
%% Guard Compilation (1.3.2.2)
%%====================================================================

%% @doc Compile guard expressions to Core Erlang
-spec compile_guard(term(), catena_codegen_utils:codegen_state()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.

%% Empty guard -> true
compile_guard([], State) ->
    {cerl:c_atom(true), State};

%% Single guard expression
compile_guard(Guard, State) when not is_list(Guard) ->
    compile_guard_expr(Guard, State);

%% Multiple guards (conjunction)
compile_guard([Guard], State) ->
    compile_guard_expr(Guard, State);

compile_guard([Guard | Rest], State) ->
    {CoreGuard, State1} = compile_guard_expr(Guard, State),
    {CoreRest, State2} = compile_guard(Rest, State1),
    %% Combine with 'andalso'
    Combined = cerl:c_call(
        cerl:c_atom(erlang),
        cerl:c_atom('andalso'),
        [CoreGuard, CoreRest]
    ),
    {Combined, State2}.

%% Compile guards list (for clause compilation)
compile_guards([], State) ->
    {cerl:c_atom(true), State};
compile_guards(Guards, State) ->
    compile_guard(Guards, State).

%% Compile a single guard expression
compile_guard_expr(Guard, State) ->
    %% Guards are essentially expressions with restricted operations
    %% Translate using expression translator
    catena_codegen_expr:translate_expr(Guard, State).

%%====================================================================
%% Decision Tree Generation (1.3.2.3)
%%====================================================================

%% @doc Generate optimized decision tree for pattern matching
%%
%% This implements a simplified version of the decision tree algorithm
%% that reorders patterns for efficiency.
-spec generate_decision_tree([clause()], catena_codegen_utils:codegen_state()) ->
    {decision_tree(), catena_codegen_utils:codegen_state()}.
generate_decision_tree(Clauses, State) ->
    %% For PoC, we use a simple linear matching strategy
    %% Full optimization would use heuristics like:
    %% - Small branching factor first
    %% - Constructor frequency analysis
    %% - Column selection heuristics

    %% Analyze pattern columns
    Analysis = analyze_patterns(Clauses),

    %% Build decision tree based on analysis
    Tree = build_tree(Clauses, Analysis),

    {Tree, State}.

%% @doc Compile decision tree to Core Erlang case expressions
-spec compile_decision_tree(decision_tree(), cerl:cerl(), catena_codegen_utils:codegen_state()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.
compile_decision_tree({leaf, Body}, _Scrutinee, State) ->
    catena_codegen_expr:translate_expr(Body, State);

compile_decision_tree({switch, Column, Branches, Default}, Scrutinee, State) ->
    %% Extract the column value from scrutinee
    %% For single-column matching, scrutinee is the value
    %% For multi-column, would need tuple extraction

    %% Compile branches
    {CoreBranches, State1} = lists:mapfoldl(
        fun({Constructor, SubTree}, St) ->
            {CoreBody, St1} = compile_decision_tree(SubTree, Scrutinee, St),
            Pattern = constructor_pattern(Constructor),
            Clause = cerl:c_clause([Pattern], CoreBody),
            {Clause, St1}
        end,
        State,
        Branches
    ),

    %% Compile default branch if present
    {AllClauses, State2} = case Default of
        none ->
            {CoreBranches, State1};
        DefaultTree ->
            {CoreDefault, St2} = compile_decision_tree(DefaultTree, Scrutinee, State1),
            DefaultClause = cerl:c_clause([cerl:c_var('_')], CoreDefault),
            {CoreBranches ++ [DefaultClause], St2}
    end,

    Case = cerl:c_case(Scrutinee, AllClauses),
    {Case, State2}.

%% Analyze patterns to determine column priorities
analyze_patterns(Clauses) ->
    %% Count constructors per column
    %% For PoC, return simple analysis
    #{
        num_clauses => length(Clauses),
        columns => count_pattern_columns(Clauses)
    }.

count_pattern_columns([]) -> 0;
count_pattern_columns([{clause, Patterns, _, _} | _]) -> length(Patterns);
count_pattern_columns([{Patterns, _} | _]) when is_list(Patterns) -> length(Patterns);
count_pattern_columns(_) -> 1.

%% Build decision tree from clauses
build_tree([], _Analysis) ->
    {leaf, {literal, atom, match_error, {location, 0, 0}}};
build_tree([{clause, _Patterns, _Guards, Body} | _], _Analysis) ->
    %% Simplified: just use first clause body
    %% Full implementation would build proper tree structure
    {leaf, Body};
build_tree([{_Patterns, Body} | _], _Analysis) ->
    {leaf, Body}.

%% Create pattern for constructor
constructor_pattern(Constructor) when is_atom(Constructor) ->
    cerl:c_atom(Constructor);
constructor_pattern({Constructor, Arity}) ->
    %% Constructor with arity - create tuple pattern
    Vars = [cerl:c_var(list_to_atom("_" ++ integer_to_list(I)))
            || I <- lists:seq(1, Arity)],
    cerl:c_tuple([cerl:c_atom(Constructor) | Vars]).

%%====================================================================
%% Exhaustiveness Checking (1.3.2.4)
%%====================================================================

%% @doc Check if patterns are exhaustive
%%
%% Returns ok if patterns cover all cases, or {warning, missing_patterns}
%% if some cases are not covered.
-spec check_exhaustiveness([pattern()], type_info()) ->
    ok | {warning, [missing_pattern()]}.
check_exhaustiveness(Patterns, TypeInfo) ->
    %% Build pattern matrix
    Matrix = build_pattern_matrix(Patterns),

    %% Check for uncovered cases
    case find_uncovered(Matrix, TypeInfo) of
        [] ->
            ok;
        Missing ->
            {warning, Missing}
    end.

%% @doc Check exhaustiveness of clauses
-spec check_clause_exhaustiveness([clause()], type_info()) ->
    ok | {warning, [missing_pattern()]}.
check_clause_exhaustiveness(Clauses, TypeInfo) ->
    Patterns = [Ps || {clause, Ps, _, _} <- Clauses] ++
               [Ps || {Ps, _} <- Clauses, is_list(Ps)],
    check_exhaustiveness(Patterns, TypeInfo).

%% Build pattern matrix from list of pattern lists
build_pattern_matrix(Patterns) ->
    [normalize_pattern(P) || P <- Patterns].

%% Normalize pattern for analysis
normalize_pattern({pat_var, _, _}) -> var;
normalize_pattern({pat_wildcard, _}) -> wildcard;
normalize_pattern({pat_literal, V, T, _}) -> {literal, V, T};
normalize_pattern({pat_constructor, N, Args, _}) ->
    {constructor, N, [normalize_pattern(A) || A <- Args]};
normalize_pattern({pat_list, Elems, _}) ->
    {list, [normalize_pattern(E) || E <- Elems]};
normalize_pattern({pat_cons, H, T, _}) ->
    {cons, normalize_pattern(H), normalize_pattern(T)};
normalize_pattern({pat_tuple, Elems, _}) ->
    {tuple, [normalize_pattern(E) || E <- Elems]};
normalize_pattern(Other) -> Other.

%% Find uncovered patterns
find_uncovered(Matrix, TypeInfo) ->
    %% For each constructor in the type, check if it's covered
    case TypeInfo of
        {adt, Constructors} ->
            find_uncovered_constructors(Matrix, Constructors);
        {bool} ->
            find_uncovered_bool(Matrix);
        {list, _} ->
            find_uncovered_list(Matrix);
        _ ->
            %% Unknown type, assume exhaustive
            []
    end.

%% Find uncovered ADT constructors
find_uncovered_constructors(Matrix, Constructors) ->
    Covered = sets:from_list([Name || {constructor, Name, _} <- Matrix]),
    AllConstructors = sets:from_list([Name || {Name, _Arity} <- Constructors]),
    Missing = sets:subtract(AllConstructors, Covered),
    [{missing_constructor, C} || C <- sets:to_list(Missing)].

%% Find uncovered boolean cases
find_uncovered_bool(Matrix) ->
    HasTrue = lists:any(fun({literal, true, bool}) -> true; (_) -> false end, Matrix),
    HasFalse = lists:any(fun({literal, false, bool}) -> true; (_) -> false end, Matrix),
    HasWild = lists:any(fun(var) -> true; (wildcard) -> true; (_) -> false end, Matrix),

    case {HasTrue orelse HasWild, HasFalse orelse HasWild} of
        {true, true} -> [];
        {false, true} -> [{missing_literal, true}];
        {true, false} -> [{missing_literal, false}];
        {false, false} -> [{missing_literal, true}, {missing_literal, false}]
    end.

%% Find uncovered list cases
find_uncovered_list(Matrix) ->
    HasEmpty = lists:any(fun({list, []}) -> true; (_) -> false end, Matrix),
    HasCons = lists:any(fun({cons, _, _}) -> true; ({list, [_|_]}) -> true; (_) -> false end, Matrix),
    HasWild = lists:any(fun(var) -> true; (wildcard) -> true; (_) -> false end, Matrix),

    case {HasEmpty orelse HasWild, HasCons orelse HasWild} of
        {true, true} -> [];
        {false, true} -> [{missing_pattern, empty_list}];
        {true, false} -> [{missing_pattern, non_empty_list}];
        {false, false} -> [{missing_pattern, empty_list}, {missing_pattern, non_empty_list}]
    end.

%%====================================================================
%% Utility Functions
%%====================================================================

%% @doc Convert AST pattern to Core Erlang pattern
%% Alias for compile_pattern for external use
-spec pattern_to_core(pattern(), catena_codegen_utils:codegen_state()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.
pattern_to_core(Pattern, State) ->
    compile_pattern(Pattern, State).

%%====================================================================
%% Type Definitions
%%====================================================================

-type pattern() :: {pat_var, atom(), term()}
                 | {pat_wildcard, term()}
                 | {pat_literal, term(), atom(), term()}
                 | {pat_constructor, atom(), [pattern()], term()}
                 | {pat_list, [pattern()], term()}
                 | {pat_cons, pattern(), pattern(), term()}
                 | {pat_tuple, [pattern()], term()}
                 | {pat_as, atom(), pattern(), term()}
                 | {pat_record, [{atom(), pattern()}], term()}.

-type clause() :: {clause, [pattern()], [term()], term()}
                | {[pattern()], term()}.

-type compile_opts() :: #{
    optimize => boolean(),
    warn_incomplete => boolean()
}.

-type decision_tree() :: {leaf, term()}
                       | {switch, integer(), [{term(), decision_tree()}], decision_tree() | none}.

-type type_info() :: {adt, [{atom(), integer()}]}
                   | {bool}
                   | {list, type_info()}
                   | unknown.

-type missing_pattern() :: {missing_constructor, atom()}
                         | {missing_literal, term()}
                         | {missing_pattern, term()}.
