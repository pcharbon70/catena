%%%-------------------------------------------------------------------
%%% @doc Pattern Exhaustiveness and Redundancy Checking (Phase 3.3)
%%%
%%% Implements static analysis for pattern matching to ensure:
%%% - Exhaustiveness: Every possible input matches some pattern
%%% - Redundancy: No unreachable patterns exist
%%%
%%% Uses the algorithm from "Warnings for pattern matching" by
%%% Luc Maranget (2007), adapted for Catena's pattern system.
%%%
%%% The key insight is representing pattern sets as matrices and
%%% computing the "usefulness" of patterns - a pattern is useful
%%% if it matches values not matched by previous patterns.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_pattern_check).

-export([
    %% Main API
    check_match/2,
    check_match/3,

    %% Exhaustiveness
    check_exhaustiveness/2,
    find_missing_patterns/2,

    %% Redundancy
    check_redundancy/2,
    find_redundant_patterns/2,

    %% Usefulness (core algorithm)
    is_useful/2,
    useful_pattern/2,

    %% Pattern analysis
    pattern_subsumes/2,
    patterns_overlap/2,

    %% Warning generation
    format_missing_warning/2,
    format_redundancy_warning/3,

    %% Guard exhaustiveness
    check_guard_exhaustiveness/2,

    %% Testing exports
    specialize/2,
    default_matrix/1,
    split_constructors/2
]).

%%====================================================================
%% Types
%%====================================================================

-type pattern() :: {pat_var, atom(), loc()}
                 | {pat_wildcard, loc()}
                 | {pat_literal, term(), atom(), loc()}
                 | {pat_constructor, atom(), [pattern()], loc()}
                 | {pat_list, [pattern()], loc()}
                 | {pat_cons, pattern(), pattern(), loc()}
                 | {pat_tuple, [pattern()], loc()}
                 | {pat_as, atom(), pattern(), loc()}
                 | {pat_or, [pattern()], loc()}
                 | {pat_record, [{atom(), pattern()}], loc()}.

-type loc() :: {location, non_neg_integer(), non_neg_integer()} | undefined.

-type pattern_row() :: [pattern()].
-type pattern_matrix() :: [pattern_row()].

-type type_info() :: {adt, atom(), [{atom(), non_neg_integer()}]}
                   | {bool}
                   | {list, type_info()}
                   | {tuple, [type_info()]}
                   | {int}
                   | {float}
                   | {string}
                   | {any}
                   | unknown.

-type constructor() :: {ctor, atom(), non_neg_integer()}
                     | {literal, term(), atom()}
                     | nil
                     | cons
                     | {tuple, non_neg_integer()}.

-type check_opts() :: #{
    type_info => type_info(),
    source_loc => loc(),
    warnings => [atom()]  % Which warnings to enable
}.

-type check_result() :: #{
    exhaustive := boolean(),
    missing := [pattern()],
    redundant := [{non_neg_integer(), pattern()}],
    warnings := [warning()]
}.

-type warning() :: {warning, warning_type(), term(), loc()}.
-type warning_type() :: non_exhaustive | redundant_pattern | overlapping_patterns.

%%====================================================================
%% Main API
%%====================================================================

%% @doc Check pattern match for exhaustiveness and redundancy
-spec check_match([pattern()], type_info()) -> check_result().
check_match(Patterns, TypeInfo) ->
    check_match(Patterns, TypeInfo, #{}).

-spec check_match([pattern()], type_info(), check_opts()) -> check_result().
check_match(Patterns, TypeInfo, Opts) ->
    %% Convert patterns to matrix (each pattern is a row with one column)
    Matrix = [[P] || P <- Patterns],

    %% Check exhaustiveness
    Missing = find_missing_patterns(Matrix, TypeInfo),
    Exhaustive = Missing =:= [],

    %% Check redundancy
    Redundant = find_redundant_patterns(Matrix, TypeInfo),

    %% Generate warnings
    Warnings = generate_warnings(Missing, Redundant, Opts),

    #{
        exhaustive => Exhaustive,
        missing => Missing,
        redundant => Redundant,
        warnings => Warnings
    }.

%%====================================================================
%% Exhaustiveness Checking (3.3.1)
%%====================================================================

%% @doc Check if patterns are exhaustive for the given type
-spec check_exhaustiveness(pattern_matrix(), type_info()) -> boolean().
check_exhaustiveness(Matrix, TypeInfo) ->
    find_missing_patterns(Matrix, TypeInfo) =:= [].

%% @doc Find patterns missing from the match
%% Returns example patterns that would not be matched
-spec find_missing_patterns(pattern_matrix(), type_info()) -> [pattern()].
find_missing_patterns(Matrix, TypeInfo) ->
    %% Generate a witness pattern for each uncovered case
    %% A pattern is missing if it's "useful" against the matrix
    WitnessRows = generate_witness_rows(TypeInfo),

    lists:filtermap(
        fun(Witness) ->
            case is_useful(Matrix, Witness) of
                true -> {true, row_to_pattern(Witness)};
                false -> false
            end
        end,
        WitnessRows
    ).

%% Generate witness rows for a type (potential uncovered patterns)
generate_witness_rows({adt, _Name, Constructors}) ->
    %% For ADT, generate a witness for each constructor
    [[{pat_constructor, CtorName,
       [{pat_wildcard, undefined} || _ <- lists:seq(1, Arity)],
       undefined}]
     || {CtorName, Arity} <- Constructors];

generate_witness_rows({bool}) ->
    [[{pat_literal, true, bool, undefined}],
     [{pat_literal, false, bool, undefined}]];

generate_witness_rows({list, _ElemType}) ->
    [[{pat_list, [], undefined}],
     [{pat_cons, {pat_wildcard, undefined}, {pat_wildcard, undefined}, undefined}]];

generate_witness_rows({tuple, ElemTypes}) ->
    N = length(ElemTypes),
    [[{pat_tuple, [{pat_wildcard, undefined} || _ <- lists:seq(1, N)], undefined}]];

generate_witness_rows({int}) ->
    %% Integers are infinite - just check for wildcard coverage
    [[{pat_wildcard, undefined}]];

generate_witness_rows({float}) ->
    [[{pat_wildcard, undefined}]];

generate_witness_rows({string}) ->
    [[{pat_wildcard, undefined}]];

generate_witness_rows({any}) ->
    [[{pat_wildcard, undefined}]];

generate_witness_rows(unknown) ->
    %% Unknown type - assume wildcard is sufficient
    [[{pat_wildcard, undefined}]].

%% Convert a row back to a single pattern
row_to_pattern([P]) -> P;
row_to_pattern(Row) -> {pat_tuple, Row, undefined}.

%%====================================================================
%% Redundancy Detection (3.3.2)
%%====================================================================

%% @doc Check if any patterns are redundant
-spec check_redundancy(pattern_matrix(), type_info()) -> boolean().
check_redundancy(Matrix, TypeInfo) ->
    find_redundant_patterns(Matrix, TypeInfo) =:= [].

%% @doc Find redundant patterns (unreachable code)
%% Returns list of {Index, Pattern} for redundant patterns
-spec find_redundant_patterns(pattern_matrix(), type_info()) -> [{non_neg_integer(), pattern()}].
find_redundant_patterns(Matrix, _TypeInfo) ->
    find_redundant_patterns_impl(Matrix, [], 0).

find_redundant_patterns_impl([], _Preceding, _Idx) ->
    [];
find_redundant_patterns_impl([Row | Rest], Preceding, Idx) ->
    case is_useful(Preceding, Row) of
        true ->
            %% Pattern is useful - add to preceding and continue
            find_redundant_patterns_impl(Rest, Preceding ++ [Row], Idx + 1);
        false ->
            %% Pattern is redundant - report and continue
            [{Idx, row_to_pattern(Row)} |
             find_redundant_patterns_impl(Rest, Preceding ++ [Row], Idx + 1)]
    end.

%%====================================================================
%% Usefulness Algorithm (Core)
%%====================================================================

%% @doc Check if a pattern row is "useful" against a matrix
%% A row is useful if it matches some value not matched by any row in the matrix
-spec is_useful(pattern_matrix(), pattern_row()) -> boolean().
is_useful([], _Row) ->
    %% Empty matrix - any pattern is useful
    true;
is_useful(Matrix, []) ->
    %% Empty row - useful only if matrix has empty rows that match
    %% Check if any row in the matrix is also empty (base case match)
    not lists:any(fun([]) -> true; (_) -> false end, Matrix);
is_useful(Matrix, [Pat | RestPat] = _Row) ->
    case is_wildcard(Pat) of
        true ->
            %% Wildcard pattern - need to check all constructors
            useful_wildcard(Matrix, RestPat);
        false ->
            %% Constructor pattern - specialize and recurse
            Ctor = pattern_to_constructor(Pat),
            SpecMatrix = specialize(Matrix, Ctor),
            SpecRow = specialize_row([Pat | RestPat], Ctor),
            is_useful(SpecMatrix, SpecRow)
    end.

%% Check usefulness when the pattern is a wildcard
useful_wildcard(Matrix, RestPat) ->
    %% Get all constructors appearing in the first column
    Ctors = first_column_constructors(Matrix),

    case Ctors of
        [] ->
            %% No constructors in matrix - only wildcards/vars
            %% Check if any row would match - use default matrix
            DefMatrix = default_matrix(Matrix),
            case DefMatrix of
                [] ->
                    %% No default rows - pattern is useful
                    true;
                _ ->
                    %% Check if remaining patterns are useful against defaults
                    is_useful(DefMatrix, RestPat)
            end;
        _ ->
            %% Matrix has constructors - check if wildcard is useful
            %%
            %% Key insight: wildcard is useful if EITHER:
            %% 1. There's a default row (wildcard/var) in the matrix that doesn't cover everything
            %% 2. The constructors in the matrix don't form a complete signature
            %% 3. For any constructor, the specialized wildcard is useful

            %% First check if there's an uncovered constructor (infinite types like int)
            %% For literals, there are always more values than just those in the matrix
            HasInfiniteType = lists:any(fun is_infinite_constructor/1, Ctors),

            %% Check default matrix
            DefaultUseful = case default_matrix(Matrix) of
                [] ->
                    %% No default rows - if type is infinite, wildcard IS useful
                    HasInfiniteType;
                DefMatrix ->
                    is_useful(DefMatrix, RestPat)
            end,

            %% Check each constructor
            CtorUseful = lists:any(
                fun(Ctor) ->
                    SpecMatrix = specialize(Matrix, Ctor),
                    Args = wildcard_args(Ctor),
                    SpecRow = Args ++ RestPat,
                    is_useful(SpecMatrix, SpecRow)
                end,
                Ctors
            ),

            DefaultUseful orelse CtorUseful
    end.

%% Check if a constructor belongs to an infinite type
is_infinite_constructor({literal, _, integer}) -> true;
is_infinite_constructor({literal, _, float}) -> true;
is_infinite_constructor({literal, _, string}) -> true;
is_infinite_constructor(_) -> false.

%% @doc Generate a useful pattern against a matrix (for witness generation)
-spec useful_pattern(pattern_matrix(), type_info()) -> pattern() | none.
useful_pattern(Matrix, TypeInfo) ->
    Witnesses = generate_witness_rows(TypeInfo),
    case lists:dropwhile(
        fun(W) -> not is_useful(Matrix, W) end,
        Witnesses
    ) of
        [Witness | _] -> row_to_pattern(Witness);
        [] -> none
    end.

%%====================================================================
%% Pattern Subsumption and Overlap (3.3.2)
%%====================================================================

%% @doc Check if pattern P1 subsumes pattern P2
%% P1 subsumes P2 if every value matching P2 also matches P1
-spec pattern_subsumes(pattern(), pattern()) -> boolean().
pattern_subsumes(P1, P2) ->
    %% P1 subsumes P2 if P2 is not useful against [P1]
    not is_useful([[P1]], [P2]).

%% @doc Check if two patterns overlap (match some common values)
-spec patterns_overlap(pattern(), pattern()) -> boolean().
patterns_overlap(P1, P2) ->
    %% Patterns overlap if neither subsumes the other but they share values
    %% This is approximated by checking if their intersection is non-empty
    not (pattern_subsumes(P1, P2) orelse pattern_subsumes(P2, P1)) andalso
    patterns_compatible(P1, P2).

%% Check if patterns could match any common value
patterns_compatible({pat_var, _, _}, _) -> true;
patterns_compatible(_, {pat_var, _, _}) -> true;
patterns_compatible({pat_wildcard, _}, _) -> true;
patterns_compatible(_, {pat_wildcard, _}) -> true;
patterns_compatible({pat_literal, V, T, _}, {pat_literal, V, T, _}) -> true;
patterns_compatible({pat_literal, _, _, _}, {pat_literal, _, _, _}) -> false;
patterns_compatible({pat_constructor, N, Args1, _}, {pat_constructor, N, Args2, _})
  when length(Args1) =:= length(Args2) ->
    lists:all(fun({A1, A2}) -> patterns_compatible(A1, A2) end,
              lists:zip(Args1, Args2));
patterns_compatible({pat_constructor, _, _, _}, {pat_constructor, _, _, _}) -> false;
patterns_compatible({pat_tuple, E1, _}, {pat_tuple, E2, _})
  when length(E1) =:= length(E2) ->
    lists:all(fun({A1, A2}) -> patterns_compatible(A1, A2) end,
              lists:zip(E1, E2));
patterns_compatible({pat_tuple, _, _}, {pat_tuple, _, _}) -> false;
patterns_compatible({pat_list, [], _}, {pat_list, [], _}) -> true;
patterns_compatible({pat_list, [_|_], _}, {pat_list, [_|_], _}) -> true;
patterns_compatible({pat_cons, _, _, _}, {pat_cons, _, _, _}) -> true;
patterns_compatible({pat_cons, _, _, _}, {pat_list, [_|_], _}) -> true;
patterns_compatible({pat_list, [_|_], _}, {pat_cons, _, _, _}) -> true;
patterns_compatible(_, _) -> false.

%%====================================================================
%% Matrix Operations
%%====================================================================

%% @doc Specialize matrix for a constructor
%% Keeps rows that match the constructor, expanding patterns
-spec specialize(pattern_matrix(), constructor()) -> pattern_matrix().
specialize(Matrix, Ctor) ->
    lists:filtermap(
        fun([Pat | Rest]) ->
            case specialize_pattern(Pat, Ctor) of
                {ok, Expanded} -> {true, Expanded ++ Rest};
                nomatch -> false
            end;
           ([]) ->
               false
        end,
        Matrix
    ).

%% Specialize a single pattern for a constructor
specialize_pattern({pat_var, _, _}, Ctor) ->
    {ok, wildcard_args(Ctor)};
specialize_pattern({pat_wildcard, _}, Ctor) ->
    {ok, wildcard_args(Ctor)};
specialize_pattern({pat_literal, V, T, _}, {literal, V, T}) ->
    {ok, []};
specialize_pattern({pat_literal, _, _, _}, _) ->
    nomatch;
specialize_pattern({pat_constructor, Name, Args, _}, {ctor, Name, Arity})
  when length(Args) =:= Arity ->
    {ok, Args};
specialize_pattern({pat_constructor, _, _, _}, _) ->
    nomatch;
specialize_pattern({pat_tuple, Elems, _}, {tuple, Arity})
  when length(Elems) =:= Arity ->
    {ok, Elems};
specialize_pattern({pat_tuple, _, _}, _) ->
    nomatch;
specialize_pattern({pat_list, [], _}, nil) ->
    {ok, []};
specialize_pattern({pat_list, [H | T], Loc}, cons) ->
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
specialize_pattern({pat_as, _, Inner, _}, Ctor) ->
    specialize_pattern(Inner, Ctor);
specialize_pattern({pat_or, Alts, _}, Ctor) ->
    %% Or-pattern: try each alternative
    case lists:filtermap(
        fun(Alt) ->
            case specialize_pattern(Alt, Ctor) of
                {ok, Exp} -> {true, Exp};
                nomatch -> false
            end
        end, Alts
    ) of
        [First | _] -> {ok, First};
        [] -> nomatch
    end;
specialize_pattern(_, _) ->
    nomatch.

%% Specialize a row for a constructor
specialize_row([Pat | Rest], Ctor) ->
    case specialize_pattern(Pat, Ctor) of
        {ok, Expanded} -> Expanded ++ Rest;
        nomatch -> []  % Should not happen in well-typed code
    end.

%% @doc Default matrix - rows with wildcard/var in first column
-spec default_matrix(pattern_matrix()) -> pattern_matrix().
default_matrix(Matrix) ->
    lists:filtermap(
        fun([Pat | Rest]) ->
            case is_wildcard(Pat) of
                true -> {true, Rest};
                false -> false
            end;
           ([]) ->
               false
        end,
        Matrix
    ).

%% @doc Get constructors from a type
-spec split_constructors(type_info(), pattern_matrix()) -> [constructor()].
split_constructors({adt, _Name, Ctors}, _Matrix) ->
    [{ctor, N, A} || {N, A} <- Ctors];
split_constructors({bool}, _Matrix) ->
    [{literal, true, bool}, {literal, false, bool}];
split_constructors({list, _}, _Matrix) ->
    [nil, cons];
split_constructors({tuple, Types}, _Matrix) ->
    [{tuple, length(Types)}];
split_constructors(_, Matrix) ->
    %% For unknown types, get constructors from the matrix
    first_column_constructors(Matrix).

%% Get all constructors from the first column of a matrix
first_column_constructors(Matrix) ->
    Ctors = lists:filtermap(
        fun([Pat | _]) ->
            case pattern_to_constructor(Pat) of
                none -> false;
                Ctor -> {true, Ctor}
            end;
           ([]) ->
               false
        end,
        Matrix
    ),
    lists:usort(Ctors).

%%====================================================================
%% Pattern Utilities
%%====================================================================

%% Check if pattern is a wildcard or variable
is_wildcard({pat_var, _, _}) -> true;
is_wildcard({pat_wildcard, _}) -> true;
is_wildcard({pat_as, _, Inner, _}) -> is_wildcard(Inner);
is_wildcard(_) -> false.

%% Convert pattern to constructor representation
pattern_to_constructor({pat_var, _, _}) -> none;
pattern_to_constructor({pat_wildcard, _}) -> none;
pattern_to_constructor({pat_literal, V, T, _}) -> {literal, V, T};
pattern_to_constructor({pat_constructor, N, Args, _}) -> {ctor, N, length(Args)};
pattern_to_constructor({pat_tuple, Elems, _}) -> {tuple, length(Elems)};
pattern_to_constructor({pat_list, [], _}) -> nil;
pattern_to_constructor({pat_list, [_|_], _}) -> cons;
pattern_to_constructor({pat_cons, _, _, _}) -> cons;
pattern_to_constructor({pat_as, _, Inner, _}) -> pattern_to_constructor(Inner);
pattern_to_constructor({pat_or, [Alt | _], _}) -> pattern_to_constructor(Alt);
pattern_to_constructor(_) -> none.

%% Generate wildcard patterns for constructor arguments
wildcard_args({ctor, _, Arity}) ->
    [{pat_wildcard, undefined} || _ <- lists:seq(1, Arity)];
wildcard_args({tuple, Arity}) ->
    [{pat_wildcard, undefined} || _ <- lists:seq(1, Arity)];
wildcard_args(cons) ->
    [{pat_wildcard, undefined}, {pat_wildcard, undefined}];
wildcard_args(_) ->
    [].

%%====================================================================
%% Warning Generation (3.3.3)
%%====================================================================

%% Generate warnings for missing and redundant patterns
generate_warnings(Missing, Redundant, Opts) ->
    Loc = maps:get(source_loc, Opts, undefined),
    EnabledWarnings = maps:get(warnings, Opts, [non_exhaustive, redundant_pattern]),

    MissingWarnings = case lists:member(non_exhaustive, EnabledWarnings) of
        true ->
            [{warning, non_exhaustive, M, Loc} || M <- Missing];
        false ->
            []
    end,

    RedundantWarnings = case lists:member(redundant_pattern, EnabledWarnings) of
        true ->
            [{warning, redundant_pattern, {Idx, P}, Loc} || {Idx, P} <- Redundant];
        false ->
            []
    end,

    MissingWarnings ++ RedundantWarnings.

%% @doc Format a warning message for missing patterns
-spec format_missing_warning([pattern()], loc()) -> binary().
format_missing_warning(Missing, Loc) ->
    LocStr = format_location(Loc),
    PatStrs = [format_pattern(P) || P <- Missing],
    MissingStr = string:join(PatStrs, ", "),
    iolist_to_binary([
        LocStr, "Warning: Non-exhaustive pattern match\n",
        "  Missing cases: ", MissingStr, "\n",
        "  Consider adding patterns for: ", MissingStr
    ]).

%% @doc Format a warning message for redundant patterns
-spec format_redundancy_warning(non_neg_integer(), pattern(), loc()) -> binary().
format_redundancy_warning(Index, Pattern, Loc) ->
    LocStr = format_location(Loc),
    PatStr = format_pattern(Pattern),
    iolist_to_binary([
        LocStr, "Warning: Redundant pattern at clause ", integer_to_list(Index + 1), "\n",
        "  Pattern '", PatStr, "' is unreachable\n",
        "  This clause will never be executed because previous patterns cover all cases"
    ]).

%% Format a source location
format_location(undefined) -> "";
format_location({location, Line, Col}) ->
    io_lib:format("~p:~p: ", [Line, Col]).

%% Format a pattern for display
format_pattern({pat_var, Name, _}) ->
    atom_to_list(Name);
format_pattern({pat_wildcard, _}) ->
    "_";
format_pattern({pat_literal, Val, integer, _}) ->
    integer_to_list(Val);
format_pattern({pat_literal, Val, float, _}) ->
    float_to_list(Val);
format_pattern({pat_literal, Val, bool, _}) ->
    atom_to_list(Val);
format_pattern({pat_literal, Val, atom, _}) ->
    atom_to_list(Val);
format_pattern({pat_literal, Val, string, _}) when is_binary(Val) ->
    "\"" ++ binary_to_list(Val) ++ "\"";
format_pattern({pat_literal, Val, string, _}) ->
    "\"" ++ Val ++ "\"";
format_pattern({pat_constructor, Name, [], _}) ->
    atom_to_list(Name);
format_pattern({pat_constructor, Name, Args, _}) ->
    ArgStrs = [format_pattern(A) || A <- Args],
    atom_to_list(Name) ++ "(" ++ string:join(ArgStrs, ", ") ++ ")";
format_pattern({pat_tuple, Elems, _}) ->
    ElemStrs = [format_pattern(E) || E <- Elems],
    "(" ++ string:join(ElemStrs, ", ") ++ ")";
format_pattern({pat_list, [], _}) ->
    "[]";
format_pattern({pat_list, Elems, _}) ->
    ElemStrs = [format_pattern(E) || E <- Elems],
    "[" ++ string:join(ElemStrs, ", ") ++ "]";
format_pattern({pat_cons, H, T, _}) ->
    format_pattern(H) ++ " :: " ++ format_pattern(T);
format_pattern({pat_as, Name, Inner, _}) ->
    format_pattern(Inner) ++ " as " ++ atom_to_list(Name);
format_pattern({pat_or, Alts, _}) ->
    AltStrs = [format_pattern(A) || A <- Alts],
    string:join(AltStrs, " | ");
format_pattern(Other) ->
    io_lib:format("~p", [Other]).

%%====================================================================
%% SMT Integration for Guards (3.3.4)
%%====================================================================

%% Note: Full SMT integration would require an external solver like Z3.
%% For the PoC, we provide a conservative fallback that assumes guards
%% may or may not be exhaustive.

%% @doc Check guard exhaustiveness with fallback
%% Returns 'unknown' if SMT solving is not available
-spec check_guard_exhaustiveness([{pattern(), term()}], type_info()) ->
    exhaustive | non_exhaustive | unknown.
check_guard_exhaustiveness(PatternGuards, TypeInfo) ->
    %% First check pattern exhaustiveness without guards
    Patterns = [P || {P, _Guard} <- PatternGuards],
    Matrix = [[P] || P <- Patterns],

    case find_missing_patterns(Matrix, TypeInfo) of
        [] ->
            %% Patterns are exhaustive, guards don't matter
            exhaustive;
        _Missing ->
            %% Patterns aren't exhaustive - would need SMT to check guards
            %% For now, return unknown (conservative)
            unknown
    end.

%% @doc Generate SMT-LIB format for a guard (placeholder for future integration)
%% This would be used with an external Z3 process
-spec guard_to_smtlib(term()) -> iolist().
guard_to_smtlib({binary_op, '>', Left, Right, _}) ->
    ["(> ", expr_to_smtlib(Left), " ", expr_to_smtlib(Right), ")"];
guard_to_smtlib({binary_op, '<', Left, Right, _}) ->
    ["(< ", expr_to_smtlib(Left), " ", expr_to_smtlib(Right), ")"];
guard_to_smtlib({binary_op, '>=', Left, Right, _}) ->
    ["(>= ", expr_to_smtlib(Left), " ", expr_to_smtlib(Right), ")"];
guard_to_smtlib({binary_op, '=<', Left, Right, _}) ->
    ["(<= ", expr_to_smtlib(Left), " ", expr_to_smtlib(Right), ")"];
guard_to_smtlib({binary_op, '==', Left, Right, _}) ->
    ["(= ", expr_to_smtlib(Left), " ", expr_to_smtlib(Right), ")"];
guard_to_smtlib({binary_op, '!=', Left, Right, _}) ->
    ["(not (= ", expr_to_smtlib(Left), " ", expr_to_smtlib(Right), "))"];
guard_to_smtlib({binary_op, 'and', Left, Right, _}) ->
    ["(and ", guard_to_smtlib(Left), " ", guard_to_smtlib(Right), ")"];
guard_to_smtlib({binary_op, 'or', Left, Right, _}) ->
    ["(or ", guard_to_smtlib(Left), " ", guard_to_smtlib(Right), ")"];
guard_to_smtlib({unary_op, 'not', Expr, _}) ->
    ["(not ", guard_to_smtlib(Expr), ")"];
guard_to_smtlib({literal, bool, true, _}) ->
    "true";
guard_to_smtlib({literal, bool, false, _}) ->
    "false";
guard_to_smtlib(Other) ->
    %% Unknown guard expression - can't translate
    io_lib:format("; unknown: ~p", [Other]).

expr_to_smtlib({var, Name, _}) ->
    atom_to_list(Name);
expr_to_smtlib({literal, integer, Val, _}) ->
    integer_to_list(Val);
expr_to_smtlib({literal, float, Val, _}) ->
    float_to_list(Val);
expr_to_smtlib({binary_op, '+', L, R, _}) ->
    ["(+ ", expr_to_smtlib(L), " ", expr_to_smtlib(R), ")"];
expr_to_smtlib({binary_op, '-', L, R, _}) ->
    ["(- ", expr_to_smtlib(L), " ", expr_to_smtlib(R), ")"];
expr_to_smtlib({binary_op, '*', L, R, _}) ->
    ["(* ", expr_to_smtlib(L), " ", expr_to_smtlib(R), ")"];
expr_to_smtlib({binary_op, '/', L, R, _}) ->
    ["(/ ", expr_to_smtlib(L), " ", expr_to_smtlib(R), ")"];
expr_to_smtlib(Other) ->
    io_lib:format("; expr: ~p", [Other]).
