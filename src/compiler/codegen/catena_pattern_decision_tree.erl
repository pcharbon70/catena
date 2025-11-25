%%%-------------------------------------------------------------------
%%% @doc Decision Tree Pattern Compilation (Phase 3.2)
%%%
%%% Implements efficient decision tree compilation for pattern matching
%%% using the algorithm described by Maranget. Features include:
%%%
%%% - Decision tree data structure with test nodes, leaves, and failure
%%% - Tree construction with optimal test ordering heuristics
%%% - Occurrence analysis for shared subtree detection
%%% - Code generation optimization for Core Erlang
%%% - Failure compilation with helpful error messages
%%%
%%% The algorithm works by:
%%% 1. Building a pattern matrix from clauses
%%% 2. Selecting the best column to test (using heuristics)
%%% 3. Splitting the matrix by constructor at that column
%%% 4. Recursively building subtrees for each constructor
%%% 5. Compiling the tree to efficient Core Erlang case expressions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_pattern_decision_tree).

-export([
    %% Main API
    compile/3,
    compile_with_opts/4,

    %% Decision tree construction
    build_tree/2,

    %% Code generation
    tree_to_core/3,

    %% Analysis
    analyze_patterns/1,

    %% Heuristics
    select_column/2,
    column_cost/2,
    column_benefit/2
]).

%% For testing
-export([
    pattern_matrix_from_clauses/1,
    specialize_matrix/3,
    default_matrix/2,
    is_complete_signature/2
]).

%%====================================================================
%% Types
%%====================================================================

-record(pattern_row, {
    patterns :: [pattern()],      % Pattern for each column
    guard :: term(),              % Guard expression (or [])
    action :: term(),             % Right-hand side expression
    index :: non_neg_integer()    % Original clause index for error reporting
}).

-record(decision_tree, {
    node :: tree_node()
}).

-type pattern() :: {pat_var, atom(), term()}
                 | {pat_wildcard, term()}
                 | {pat_literal, term(), atom(), term()}
                 | {pat_constructor, atom(), [pattern()], term()}
                 | {pat_list, [pattern()], term()}
                 | {pat_cons, pattern(), pattern(), term()}
                 | {pat_tuple, [pattern()], term()}
                 | {pat_as, atom(), pattern(), term()}
                 | {pat_or, [pattern()], term()}
                 | {pat_record, [{atom(), pattern()}], term()}.

-type tree_node() :: {leaf, non_neg_integer(), term(), term()}  % {leaf, Index, Guard, Action}
                   | {switch, column_info(), [branch()], tree_node() | fail}
                   | fail.

-type column_info() :: #{
    index := non_neg_integer(),
    accessor := accessor()
}.

-type accessor() :: {var, atom()}           % Direct variable
                  | {element, non_neg_integer(), accessor()}  % Tuple element
                  | {hd, accessor()}        % List head
                  | {tl, accessor()}        % List tail
                  | {field, atom(), accessor()}.  % Record field

-type branch() :: {constructor(), tree_node()}.

-type constructor() :: {literal, term()}
                     | {ctor, atom(), non_neg_integer()}  % {ctor, Name, Arity}
                     | nil                % Empty list
                     | cons               % Non-empty list
                     | {tuple, non_neg_integer()}.  % Tuple with arity

-type compile_opts() :: #{
    optimize => boolean(),
    lenient => boolean(),       % Return default on failure instead of error
    default_value => term(),    % Default value for lenient mode
    source_loc => term()        % Source location for error messages
}.

%%====================================================================
%% Main API
%%====================================================================

%% @doc Compile pattern clauses to Core Erlang using decision trees
-spec compile([clause()], [cerl:cerl()], catena_codegen_utils:codegen_state()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.
compile(Clauses, Scrutinees, State) ->
    compile_with_opts(Clauses, Scrutinees, State, #{}).

%% @doc Compile with options
-spec compile_with_opts([clause()], [cerl:cerl()], catena_codegen_utils:codegen_state(), compile_opts()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.
compile_with_opts(Clauses, Scrutinees, State, Opts) ->
    %% Build pattern matrix from clauses
    Matrix = pattern_matrix_from_clauses(Clauses),

    %% Construct decision tree
    Tree = build_tree(Matrix, Opts),

    %% Generate Core Erlang from decision tree
    tree_to_core(Tree, Scrutinees, State).

%%====================================================================
%% Pattern Matrix Construction
%%====================================================================

%% @doc Convert clauses to pattern matrix rows
-spec pattern_matrix_from_clauses([clause()]) -> [#pattern_row{}].
pattern_matrix_from_clauses(Clauses) ->
    {Rows, _} = lists:mapfoldl(
        fun(Clause, Idx) ->
            Row = clause_to_row(Clause, Idx),
            {Row, Idx + 1}
        end,
        0,
        Clauses
    ),
    Rows.

%% Convert a single clause to a pattern row
clause_to_row({clause, Patterns, Guards, Body}, Idx) ->
    #pattern_row{
        patterns = Patterns,
        guard = Guards,
        action = Body,
        index = Idx
    };
clause_to_row({Patterns, Body}, Idx) when is_list(Patterns) ->
    #pattern_row{
        patterns = Patterns,
        guard = [],
        action = Body,
        index = Idx
    };
clause_to_row({Pattern, Body}, Idx) ->
    #pattern_row{
        patterns = [Pattern],
        guard = [],
        action = Body,
        index = Idx
    }.

%%====================================================================
%% Decision Tree Construction (3.2.1)
%%====================================================================

%% @doc Build decision tree from pattern matrix
-spec build_tree([#pattern_row{}], compile_opts()) -> tree_node().
build_tree([], Opts) ->
    %% No rows - match failure
    make_failure_node(Opts);

build_tree([#pattern_row{patterns = Patterns, guard = Guard, action = Action, index = Idx} | _] = Rows, Opts) ->
    case Patterns =:= [] orelse all_wildcards(Patterns) of
        true ->
            %% All patterns are wildcards/variables - this row matches
            %% But we need to check the guard
            case Guard of
                [] ->
                    %% No guard - definite match
                    {leaf, Idx, Guard, Action};
                _ ->
                    %% Has guard - need to check it, may fall through to next rows
                    case tl(Rows) of
                        [] ->
                            %% Last row with guard
                            {leaf, Idx, Guard, Action};
                        RestRows ->
                            %% Build alternative tree for guard failure
                            FallbackTree = build_tree(RestRows, Opts),
                            {guarded_leaf, Idx, Guard, Action, FallbackTree}
                    end
            end;
        false ->
            build_tree_switch(Rows, Opts)
    end.

build_tree_switch(Rows, Opts) ->
    %% Select best column to test
    ColIdx = select_column(Rows, Opts),
    ColInfo = #{index => ColIdx, accessor => {var, scrutinee_var(ColIdx)}},

    %% Get all constructors at this column
    Constructors = column_constructors(Rows, ColIdx),

    %% Build branches for each constructor
    Branches = lists:map(
        fun(Ctor) ->
            %% Specialize matrix for this constructor
            SpecializedRows = specialize_matrix(Rows, ColIdx, Ctor),
            SubTree = build_tree(SpecializedRows, Opts),
            {Ctor, SubTree}
        end,
        Constructors
    ),

    %% Build default branch for unmatched constructors
    Default = case needs_default(Rows, ColIdx, Constructors, Opts) of
        true ->
            DefaultRows = default_matrix(Rows, ColIdx),
            build_tree(DefaultRows, Opts);
        false ->
            fail
    end,

    {switch, ColInfo, Branches, Default}.

%% Generate variable name for scrutinee at column
scrutinee_var(0) -> '_scrutinee';
scrutinee_var(N) -> list_to_atom("_scrutinee_" ++ integer_to_list(N)).

%% Check if all patterns are wildcards or variables
all_wildcards([]) -> true;
all_wildcards([{pat_var, _, _} | Rest]) -> all_wildcards(Rest);
all_wildcards([{pat_wildcard, _} | Rest]) -> all_wildcards(Rest);
all_wildcards(_) -> false.

%% Create failure node based on options
make_failure_node(#{lenient := true, default_value := Default}) ->
    {leaf, -1, [], Default};
make_failure_node(Opts) ->
    {fail, maps:get(source_loc, Opts, undefined)}.

%%====================================================================
%% Column Selection Heuristics (3.2.3)
%%====================================================================

%% @doc Select the best column to test next
%% Uses cost/benefit analysis to choose optimally
-spec select_column([#pattern_row{}], compile_opts()) -> non_neg_integer().
select_column([], _Opts) ->
    0;
select_column([#pattern_row{patterns = Patterns} | _] = Rows, Opts) ->
    NumCols = length(Patterns),

    case NumCols of
        0 -> 0;  % No columns
        1 -> 0;  % Single column, no choice
        _ ->
            %% Score each column - handle empty rows gracefully
            Scores = [{column_score(Rows, Col, Opts), Col} || Col <- lists:seq(0, NumCols - 1)],
            case Scores of
                [] -> 0;
                _ ->
                    %% Pick column with best score (highest)
                    {_, BestCol} = lists:max(Scores),
                    BestCol
            end
    end.

%% @doc Calculate score for a column (higher is better)
column_score(Rows, ColIdx, _Opts) ->
    Benefit = column_benefit(Rows, ColIdx),
    Cost = column_cost(Rows, ColIdx),
    %% Avoid division by zero - Cost is always a float
    if
        Cost < 0.001 -> Benefit * 1000.0;  % Very cheap test
        true -> Benefit / Cost
    end.

%% @doc Calculate the benefit of testing a column
%% Benefit = how many patterns are eliminated by testing this column
-spec column_benefit([#pattern_row{}], non_neg_integer()) -> float().
column_benefit([], _ColIdx) ->
    0.0;
column_benefit(Rows, ColIdx) ->
    %% Count non-wildcard patterns at this column
    NonWildcards = length([1 || R <- Rows, not is_wildcard(pattern_at(R, ColIdx))]),

    %% Benefit is proportion of non-wildcards (more specific patterns = more benefit)
    N = length(Rows),
    float(NonWildcards) / float(N).

%% @doc Calculate the cost of testing a column
%% Cost model:
%% - Constructor tag test: 1.0 (cheap)
%% - Literal comparison: 1.0
%% - Tuple element access: 0.5 (BEAM optimizes this)
%% - List cons check: 1.0
%% - Guard evaluation: 5.0 (expensive)
-spec column_cost([#pattern_row{}], non_neg_integer()) -> float().
column_cost(Rows, ColIdx) ->
    %% Cost based on pattern types at this column
    Patterns = [pattern_at(R, ColIdx) || R <- Rows],

    %% Average cost across patterns
    Costs = [pattern_cost(P) || P <- Patterns],
    case Costs of
        [] -> 1.0;
        _ -> lists:sum(Costs) / length(Costs)
    end.

%% Cost of testing a single pattern
pattern_cost({pat_var, _, _}) -> 0.0;        % Free
pattern_cost({pat_wildcard, _}) -> 0.0;       % Free
pattern_cost({pat_literal, _, _, _}) -> 1.0;  % Comparison
pattern_cost({pat_constructor, _, Args, _}) ->
    1.0 + 0.5 * length(Args);  % Tag check + field access
pattern_cost({pat_tuple, Elems, _}) ->
    0.5 + 0.5 * length(Elems);  % Size check + element access
pattern_cost({pat_list, [], _}) -> 1.0;       % Nil check
pattern_cost({pat_list, _, _}) -> 1.5;        % Cons + elements
pattern_cost({pat_cons, _, _, _}) -> 1.0;     % Cons check
pattern_cost({pat_as, _, P, _}) -> pattern_cost(P);  % Same as inner
pattern_cost({pat_or, Ps, _}) ->
    lists:sum([pattern_cost(P) || P <- Ps]) / length(Ps);
pattern_cost({pat_record, Fields, _}) ->
    1.0 + 0.5 * length(Fields);
pattern_cost(_) -> 1.0.

%% Get pattern at column index
pattern_at(#pattern_row{patterns = Patterns}, ColIdx) when ColIdx < length(Patterns) ->
    lists:nth(ColIdx + 1, Patterns);
pattern_at(_, _) ->
    {pat_wildcard, {location, 0, 0}}.

%% Check if pattern is a wildcard/variable
is_wildcard({pat_var, _, _}) -> true;
is_wildcard({pat_wildcard, _}) -> true;
is_wildcard(_) -> false.

%%====================================================================
%% Matrix Operations
%%====================================================================

%% @doc Get all constructors appearing at a column
-spec column_constructors([#pattern_row{}], non_neg_integer()) -> [constructor()].
column_constructors(Rows, ColIdx) ->
    %% Collect constructors from each row
    Ctors = lists:filtermap(
        fun(Row) ->
            Pat = pattern_at(Row, ColIdx),
            case pattern_to_constructor(Pat) of
                none -> false;
                Ctor -> {true, Ctor}
            end
        end,
        Rows
    ),
    %% Remove duplicates while preserving order
    lists:usort(Ctors).

%% Convert pattern to constructor representation
pattern_to_constructor({pat_var, _, _}) -> none;
pattern_to_constructor({pat_wildcard, _}) -> none;
pattern_to_constructor({pat_literal, Value, Type, _}) -> {literal, {Value, Type}};
pattern_to_constructor({pat_constructor, Name, Args, _}) -> {ctor, Name, length(Args)};
pattern_to_constructor({pat_tuple, Elems, _}) -> {tuple, length(Elems)};
pattern_to_constructor({pat_list, [], _}) -> nil;
pattern_to_constructor({pat_list, _, _}) -> cons;
pattern_to_constructor({pat_cons, _, _, _}) -> cons;
pattern_to_constructor({pat_as, _, P, _}) -> pattern_to_constructor(P);
pattern_to_constructor({pat_or, [P | _], _}) -> pattern_to_constructor(P);  % Use first alternative
pattern_to_constructor({pat_record, _, _}) -> map;  % Records are maps
pattern_to_constructor(_) -> none.

%% @doc Specialize matrix for a constructor
%% Keeps rows that can match the constructor, expanding patterns
-spec specialize_matrix([#pattern_row{}], non_neg_integer(), constructor()) -> [#pattern_row{}].
specialize_matrix(Rows, ColIdx, Ctor) ->
    lists:filtermap(
        fun(Row) ->
            Pat = pattern_at(Row, ColIdx),
            case specialize_pattern(Pat, Ctor) of
                {ok, NewPatterns} ->
                    %% Replace column with expanded patterns
                    OldPatterns = Row#pattern_row.patterns,
                    {Before, [_ | After]} = lists:split(ColIdx, OldPatterns),
                    NewRow = Row#pattern_row{patterns = Before ++ NewPatterns ++ After},
                    {true, NewRow};
                nomatch ->
                    false
            end
        end,
        Rows
    ).

%% Specialize a single pattern for a constructor
specialize_pattern({pat_var, _, _}, Ctor) ->
    %% Variable matches anything - expand to appropriate wildcards
    {ok, wildcard_args(Ctor)};
specialize_pattern({pat_wildcard, _}, Ctor) ->
    {ok, wildcard_args(Ctor)};
specialize_pattern({pat_literal, V, T, _}, {literal, {V, T}}) ->
    {ok, []};  % Matches, no sub-patterns
specialize_pattern({pat_literal, _, _, _}, _) ->
    nomatch;
specialize_pattern({pat_constructor, Name, Args, _}, {ctor, Name, Arity}) when length(Args) =:= Arity ->
    {ok, Args};  % Expose constructor arguments
specialize_pattern({pat_constructor, _, _, _}, _) ->
    nomatch;
specialize_pattern({pat_tuple, Elems, _}, {tuple, Arity}) when length(Elems) =:= Arity ->
    {ok, Elems};
specialize_pattern({pat_tuple, _, _}, _) ->
    nomatch;
specialize_pattern({pat_list, [], _}, nil) ->
    {ok, []};
specialize_pattern({pat_list, [H | T], Loc}, cons) ->
    %% Non-empty list - expose head and tail
    Tail = case T of
        [] -> {pat_list, [], Loc};
        _ -> {pat_list, T, Loc}
    end,
    {ok, [H, Tail]};
specialize_pattern({pat_list, _, _}, _) ->
    nomatch;
specialize_pattern({pat_cons, H, T, _}, cons) ->
    {ok, [H, T]};
specialize_pattern({pat_cons, _, _, _}, nil) ->
    nomatch;
specialize_pattern({pat_as, _, P, _}, Ctor) ->
    specialize_pattern(P, Ctor);
specialize_pattern({pat_or, Patterns, _}, Ctor) ->
    %% Or-pattern: try each alternative
    case lists:filtermap(fun(P) ->
        case specialize_pattern(P, Ctor) of
            {ok, Ps} -> {true, Ps};
            nomatch -> false
        end
    end, Patterns) of
        [] -> nomatch;
        [First | _] -> {ok, First}  % Use first matching alternative
    end;
specialize_pattern(_, _) ->
    nomatch.

%% Generate wildcard patterns for constructor arguments
wildcard_args({ctor, _, Arity}) ->
    [{pat_wildcard, {location, 0, 0}} || _ <- lists:seq(1, Arity)];
wildcard_args({tuple, Arity}) ->
    [{pat_wildcard, {location, 0, 0}} || _ <- lists:seq(1, Arity)];
wildcard_args(cons) ->
    [{pat_wildcard, {location, 0, 0}}, {pat_wildcard, {location, 0, 0}}];  % Head and tail
wildcard_args(_) ->
    [].

%% @doc Build default matrix (rows that don't match any specific constructor)
-spec default_matrix([#pattern_row{}], non_neg_integer()) -> [#pattern_row{}].
default_matrix(Rows, ColIdx) ->
    lists:filtermap(
        fun(Row) ->
            Pat = pattern_at(Row, ColIdx),
            case is_wildcard(Pat) of
                true ->
                    %% Remove the column (it's a wildcard)
                    OldPatterns = Row#pattern_row.patterns,
                    {Before, [_ | After]} = lists:split(ColIdx, OldPatterns),
                    NewRow = Row#pattern_row{patterns = Before ++ After},
                    {true, NewRow};
                false ->
                    false
            end
        end,
        Rows
    ).

%% Check if we need a default branch
needs_default(Rows, ColIdx, Constructors, _Opts) ->
    %% Need default if:
    %% 1. There are wildcard patterns at this column, OR
    %% 2. Constructors don't form a complete signature
    HasWildcards = lists:any(
        fun(Row) -> is_wildcard(pattern_at(Row, ColIdx)) end,
        Rows
    ),
    HasWildcards orelse not is_complete_signature(Constructors, unknown).

%% @doc Check if constructors form a complete signature for a type
-spec is_complete_signature([constructor()], term()) -> boolean().
is_complete_signature(Ctors, {bool}) ->
    HasTrue = lists:member({literal, {true, bool}}, Ctors),
    HasFalse = lists:member({literal, {false, bool}}, Ctors),
    HasTrue andalso HasFalse;
is_complete_signature(Ctors, {list, _}) ->
    HasNil = lists:member(nil, Ctors),
    HasCons = lists:member(cons, Ctors),
    HasNil andalso HasCons;
is_complete_signature(_Ctors, _Type) ->
    %% Without type info, assume incomplete
    false.

%%====================================================================
%% Occurrence Analysis (3.2.1.3)
%%====================================================================

%% @doc Analyze patterns for common occurrences and sharing opportunities
-spec analyze_patterns([#pattern_row{}]) -> map().
analyze_patterns(Rows) ->
    %% Count occurrences of each constructor at each column
    NumCols = case Rows of
        [#pattern_row{patterns = Ps} | _] -> length(Ps);
        [] -> 0
    end,

    ColStats = [analyze_column(Rows, Col) || Col <- lists:seq(0, NumCols - 1)],

    #{
        num_rows => length(Rows),
        num_columns => NumCols,
        columns => ColStats,

        %% Potential for sharing
        sharing_opportunities => find_sharing(Rows)
    }.

%% Analyze a single column
analyze_column(Rows, ColIdx) ->
    Patterns = [pattern_at(R, ColIdx) || R <- Rows],
    Ctors = [pattern_to_constructor(P) || P <- Patterns],

    #{
        index => ColIdx,
        constructors => lists:usort([C || C <- Ctors, C =/= none]),
        wildcards => length([1 || C <- Ctors, C =:= none]),
        total => length(Patterns)
    }.

%% Find opportunities for sharing subtrees
find_sharing(Rows) ->
    %% Find rows with identical actions (potential sharing)
    Actions = [{R#pattern_row.action, R#pattern_row.index} || R <- Rows],
    %% Group by action
    GroupedActions = maps:groups_from_list(fun({A, _}) -> A end, fun({_, I}) -> I end, Actions),
    %% Return groups with more than one member
    [{Action, Indices} || {Action, Indices} <- maps:to_list(GroupedActions), length(Indices) > 1].

%%====================================================================
%% Code Generation (3.2.2)
%%====================================================================

%% @doc Convert decision tree to Core Erlang
-spec tree_to_core(tree_node(), [cerl:cerl()], catena_codegen_utils:codegen_state()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.

tree_to_core({leaf, _Idx, [], Action}, _Scrutinees, State) ->
    %% Simple leaf - just compile the action
    catena_codegen_expr:translate_expr(Action, State);

tree_to_core({leaf, _Idx, Guards, Action}, _Scrutinees, State) ->
    %% Leaf with guard - compile guard and action
    {CoreGuard, State1} = compile_guard_list(Guards, State),
    {CoreAction, State2} = catena_codegen_expr:translate_expr(Action, State1),

    %% If guard could fail, we'd need a fallback, but leaf assumes guard passes
    %% The guarded_leaf node handles guard failures
    case CoreGuard of
        true -> {CoreAction, State2};
        _ ->
            %% Wrap in conditional (guard must pass)
            Cond = cerl:c_call(
                cerl:c_atom(erlang),
                cerl:c_atom('=:='),
                [CoreGuard, cerl:c_atom(true)]
            ),
            %% This shouldn't happen for leaf nodes, but handle defensively
            {cerl:c_case(Cond, [
                cerl:c_clause([cerl:c_atom(true)], CoreAction),
                cerl:c_clause([cerl:c_atom(false)],
                    cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(error),
                        [cerl:c_tuple([cerl:c_atom(match_error), cerl:c_atom(guard_failed)])]))
            ]), State2}
    end;

tree_to_core({guarded_leaf, _Idx, Guards, Action, Fallback}, Scrutinees, State) ->
    %% Leaf with guard that may fail
    {CoreGuard, State1} = compile_guard_list(Guards, State),
    {CoreAction, State2} = catena_codegen_expr:translate_expr(Action, State1),
    {CoreFallback, State3} = tree_to_core(Fallback, Scrutinees, State2),

    %% Generate: case Guard of true -> Action; false -> Fallback end
    GuardCase = cerl:c_case(CoreGuard, [
        cerl:c_clause([cerl:c_atom(true)], CoreAction),
        cerl:c_clause([cerl:c_atom(false)], CoreFallback)
    ]),
    {GuardCase, State3};

tree_to_core({switch, ColInfo, Branches, Default}, Scrutinees, State) ->
    %% Get the scrutinee for this column
    #{index := ColIdx} = ColInfo,
    Scrutinee = case Scrutinees of
        [S] -> S;  % Single scrutinee
        _ when ColIdx < length(Scrutinees) -> lists:nth(ColIdx + 1, Scrutinees);
        _ -> cerl:c_var('_scrutinee')
    end,

    %% Compile branches
    {CoreBranches, State1} = compile_branches(Branches, Scrutinees, State),

    %% Compile default branch
    {AllClauses, State2} = case Default of
        fail ->
            %% Add failure clause
            FailClause = cerl:c_clause(
                [cerl:c_var('_')],
                failure_expr(ColInfo)
            ),
            {CoreBranches ++ [FailClause], State1};
        _ ->
            {CoreDefault, St} = tree_to_core(Default, Scrutinees, State1),
            DefaultClause = cerl:c_clause([cerl:c_var('_')], CoreDefault),
            {CoreBranches ++ [DefaultClause], St}
    end,

    Case = cerl:c_case(Scrutinee, AllClauses),
    {Case, State2};

tree_to_core({fail, SourceLoc}, _Scrutinees, State) ->
    %% Match failure
    {failure_expr_with_loc(SourceLoc), State};

tree_to_core(fail, _Scrutinees, State) ->
    {failure_expr(#{}), State}.

%% Compile branches to Core Erlang clauses
compile_branches(Branches, Scrutinees, State) ->
    lists:mapfoldl(
        fun({Ctor, SubTree}, St) ->
            %% Create pattern for constructor
            {Pattern, BindVars} = constructor_to_pattern(Ctor),

            %% Extend scrutinees with bound variables for subtree
            ExtendedScrutinees = Scrutinees ++ BindVars,

            %% Compile subtree
            {CoreBody, St1} = tree_to_core(SubTree, ExtendedScrutinees, St),

            Clause = cerl:c_clause([Pattern], CoreBody),
            {Clause, St1}
        end,
        State,
        Branches
    ).

%% Convert constructor to Core Erlang pattern
constructor_to_pattern({literal, {Value, integer}}) ->
    {cerl:c_int(Value), []};
constructor_to_pattern({literal, {Value, float}}) ->
    {cerl:c_float(Value), []};
constructor_to_pattern({literal, {Value, atom}}) ->
    {cerl:c_atom(Value), []};
constructor_to_pattern({literal, {Value, bool}}) ->
    {cerl:c_atom(Value), []};
constructor_to_pattern({literal, {Value, string}}) when is_list(Value) ->
    {cerl:c_string(Value), []};
constructor_to_pattern({literal, {Value, string}}) when is_binary(Value) ->
    {cerl:c_string(binary_to_list(Value)), []};

constructor_to_pattern({ctor, Name, Arity}) ->
    %% Constructor with arguments - bind variables
    Vars = [cerl:c_var(list_to_atom("_arg" ++ integer_to_list(I))) || I <- lists:seq(1, Arity)],
    Pattern = cerl:c_tuple([cerl:c_atom(Name) | Vars]),
    {Pattern, Vars};

constructor_to_pattern({tuple, Arity}) ->
    Vars = [cerl:c_var(list_to_atom("_elem" ++ integer_to_list(I))) || I <- lists:seq(1, Arity)],
    {cerl:c_tuple(Vars), Vars};

constructor_to_pattern(nil) ->
    {cerl:c_nil(), []};

constructor_to_pattern(cons) ->
    HeadVar = cerl:c_var('_hd'),
    TailVar = cerl:c_var('_tl'),
    {cerl:c_cons(HeadVar, TailVar), [HeadVar, TailVar]};

constructor_to_pattern(map) ->
    %% Map/record - match any map
    {cerl:c_map_pattern([]), []}.

%% Compile guard list
compile_guard_list([], State) ->
    {cerl:c_atom(true), State};
compile_guard_list([Guard], State) ->
    catena_codegen_expr:translate_expr(Guard, State);
compile_guard_list([Guard | Rest], State) ->
    {CoreGuard, State1} = catena_codegen_expr:translate_expr(Guard, State),
    {CoreRest, State2} = compile_guard_list(Rest, State1),
    Combined = cerl:c_call(
        cerl:c_atom(erlang),
        cerl:c_atom('andalso'),
        [CoreGuard, CoreRest]
    ),
    {Combined, State2}.

%%====================================================================
%% Failure Compilation (3.2.4)
%%====================================================================

%% @doc Generate failure expression with helpful error message
failure_expr(Info) ->
    %% Generate: erlang:error({match_error, Info})
    ErrorTuple = cerl:c_tuple([
        cerl:c_atom(match_error),
        cerl:c_tuple([
            cerl:c_atom(no_match),
            info_to_cerl(Info)
        ])
    ]),
    cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(error), [ErrorTuple]).

%% Generate failure with source location
failure_expr_with_loc(undefined) ->
    failure_expr(#{});
failure_expr_with_loc({location, Line, Col}) ->
    ErrorTuple = cerl:c_tuple([
        cerl:c_atom(match_error),
        cerl:c_tuple([
            cerl:c_atom(no_match),
            cerl:c_tuple([
                cerl:c_atom(location),
                cerl:c_int(Line),
                cerl:c_int(Col)
            ])
        ])
    ]),
    cerl:c_call(cerl:c_atom(erlang), cerl:c_atom(error), [ErrorTuple]);
failure_expr_with_loc(Loc) ->
    %% Unknown location format
    failure_expr(#{source_loc => Loc}).

%% Convert info map to Core Erlang
info_to_cerl(Info) when is_map(Info) ->
    Pairs = maps:fold(
        fun(K, V, Acc) ->
            Pair = cerl:c_tuple([cerl:c_atom(K), value_to_cerl(V)]),
            [Pair | Acc]
        end,
        [],
        Info
    ),
    cerl:c_tuple([cerl:c_atom(info) | Pairs]);
info_to_cerl(_) ->
    cerl:c_atom(no_info).

%% Convert arbitrary value to Core Erlang
value_to_cerl(V) when is_atom(V) -> cerl:c_atom(V);
value_to_cerl(V) when is_integer(V) -> cerl:c_int(V);
value_to_cerl(V) when is_float(V) -> cerl:c_float(V);
value_to_cerl(V) when is_list(V) ->
    case io_lib:printable_list(V) of
        true -> cerl:c_string(V);
        false -> cerl:c_tuple([cerl:c_atom(list), cerl:c_int(length(V))])
    end;
value_to_cerl(_) -> cerl:c_atom(unknown).

%%====================================================================
%% Type Definitions for Documentation
%%====================================================================

-type clause() :: {clause, [pattern()], [term()], term()}
                | {[pattern()], term()}
                | {pattern(), term()}.
