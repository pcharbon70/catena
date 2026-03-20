%% @doc Shrink strategies and shrink-tree traversal for Catena property testing.
%%
%% This module implements Property Testing Phase 1, Section 1.6.
%% It provides default shrink sequences for common values together with a
%% deterministic depth-first search that finds smaller failing values in a rose
%% tree while tracking the path taken.
-module(catena_shrink).

-export_type([shrink_result/1]).

-export([
    shrink_towards/2,
    shrink_binary/2,
    shrink_list/1,
    shrink_halves/1,
    find_minimal/2,
    find_minimal/3
]).

-define(DEFAULT_MAX_ATTEMPTS, 1000).

-type shrink_result(A) :: #{
    value := A,
    path := [pos_integer()],
    trail := [A],
    attempts := non_neg_integer(),
    limit_hit := boolean()
}.

%%====================================================================
%% API Functions - Section 1.6.1
%%====================================================================

%% @doc Produce a linear shrink sequence from `Value` toward `Target`.
-spec shrink_towards(integer(), integer()) -> [integer()].
shrink_towards(Target, Value) when is_integer(Target), is_integer(Value), Value =:= Target ->
    [];
shrink_towards(Target, Value) when is_integer(Target), is_integer(Value) ->
    Step = sign(Value - Target),
    [Target + (Step * Offset) || Offset <- lists:seq(0, abs(Value - Target) - 1)];
shrink_towards(Target, Value) ->
    erlang:error({badarg, {shrink_towards, {Target, Value}}}).

%% @doc Produce an efficient binary-style shrink sequence toward `Target`.
-spec shrink_binary(integer(), integer()) -> [integer()].
shrink_binary(Target, Value) when is_integer(Target), is_integer(Value), Value =:= Target ->
    [];
shrink_binary(Target, Value) when is_integer(Target), is_integer(Value) ->
    shrink_binary(Value, Target, Target, [Target]);
shrink_binary(Target, Value) ->
    erlang:error({badarg, {shrink_binary, {Target, Value}}}).

%% @doc Repeatedly halve a number toward zero.
-spec shrink_halves(integer()) -> [integer()].
shrink_halves(0) ->
    [];
shrink_halves(Value) when is_integer(Value) ->
    shrink_halves(Value, []);
shrink_halves(Value) ->
    erlang:error({badarg, {shrink_halves, Value}}).

%% @doc Shrink a list by removing elements and shrinking shrinkable elements.
-spec shrink_list([term()]) -> [[term()]].
shrink_list(List) when is_list(List) ->
    unique_preserving_order(
        shrink_list_removals(List) ++ shrink_list_elements(List)
    );
shrink_list(Value) ->
    erlang:error({badarg, {shrink_list, Value}}).

%%====================================================================
%% API Functions - Section 1.6.2
%%====================================================================

%% @doc Find a minimal failing value in a shrink tree using depth-first search.
-spec find_minimal(catena_tree:tree(A), fun((A) -> boolean())) ->
    {ok, shrink_result(A)} | {error, root_passed}.
find_minimal(Tree, Predicate) ->
    find_minimal(Tree, Predicate, #{}).

%% @doc Find a minimal failing value with options such as `max_attempts`.
-spec find_minimal(catena_tree:tree(A), fun((A) -> boolean()), map()) ->
    {ok, shrink_result(A)} | {error, root_passed}.
find_minimal(Tree, Predicate, Options)
    when is_function(Predicate, 1), is_map(Options) ->
    MaxAttempts = normalize_max_attempts(maps:get(max_attempts, Options, ?DEFAULT_MAX_ATTEMPTS)),
    Root = catena_tree:root(Tree),
    case Predicate(Root) of
        true ->
            search_minimal(Tree, Predicate, MaxAttempts, 0, [], []);
        false ->
            {error, root_passed}
    end;
find_minimal(_Tree, Predicate, _Options) when not is_function(Predicate, 1) ->
    erlang:error({badarg, {predicate, Predicate}});
find_minimal(_Tree, _Predicate, Options) ->
    erlang:error({badarg, {options, Options}}).

%%====================================================================
%% Internal Helpers - Section 1.6.1
%%====================================================================

-spec shrink_binary(integer(), integer(), integer(), [integer()]) -> [integer()].
shrink_binary(Value, _Target, Previous, Acc) when Previous =:= Value ->
    lists:reverse(Acc);
shrink_binary(Value, Target, Previous, Acc) ->
    Candidate = Value - ((Value - Previous) div 2),
    case Candidate =:= Value orelse Candidate =:= Previous of
        true ->
            lists:reverse(Acc);
        false ->
            shrink_binary(Value, Target, Candidate, [Candidate | Acc])
    end.

-spec shrink_halves(integer(), [integer()]) -> [integer()].
shrink_halves(Value, Acc) ->
    Half = halve_towards_zero(Value),
    case Half of
        0 ->
            lists:reverse([0 | Acc]);
        _ ->
            shrink_halves(Half, [Half | Acc])
    end.

-spec shrink_list_removals([A]) -> [[A]].
shrink_list_removals([]) ->
    [];
shrink_list_removals(List) ->
    [[] | [remove_at(Index, List) || Index <- lists:seq(1, length(List))]].

-spec shrink_list_elements([term()]) -> [[term()]].
shrink_list_elements(List) ->
    lists:flatmap(
        fun({Index, Value}) ->
            [replace_at(Index, ShrinkValue, List)
             || ShrinkValue <- default_shrinks(Value),
                ShrinkValue =/= Value]
        end,
        lists:zip(lists:seq(1, length(List)), List)
    ).

-spec default_shrinks(term()) -> [term()].
default_shrinks(Value) when is_integer(Value) ->
    shrink_binary(0, Value);
default_shrinks(true) ->
    [false];
default_shrinks(false) ->
    [];
default_shrinks(Value) when is_list(Value) ->
    shrink_list(Value);
default_shrinks(_Value) ->
    [].

-spec remove_at(pos_integer(), [A]) -> [A].
remove_at(Index, List) ->
    {Prefix, [_Removed | Suffix]} = lists:split(Index - 1, List),
    Prefix ++ Suffix.

-spec replace_at(pos_integer(), A, [A]) -> [A].
replace_at(Index, Replacement, List) ->
    {Prefix, [_Old | Suffix]} = lists:split(Index - 1, List),
    Prefix ++ [Replacement | Suffix].

-spec halve_towards_zero(integer()) -> integer().
halve_towards_zero(Value) ->
    trunc(Value / 2).

%%====================================================================
%% Internal Helpers - Section 1.6.2
%%====================================================================

-spec search_minimal(
    catena_tree:tree(A),
    fun((A) -> boolean()),
    non_neg_integer(),
    non_neg_integer(),
    [pos_integer()],
    [A]
) -> {ok, shrink_result(A)}.
search_minimal(Tree, _Predicate, MaxAttempts, Attempts, PathAcc, TrailAcc)
    when Attempts >= MaxAttempts ->
    shrink_result(Tree, Attempts, PathAcc, TrailAcc, true);
search_minimal(Tree, Predicate, MaxAttempts, Attempts, PathAcc, TrailAcc) ->
    search_children(
        Tree,
        catena_tree:children(Tree),
        Predicate,
        MaxAttempts,
        Attempts,
        PathAcc,
        TrailAcc,
        1
    ).

-spec search_children(
    catena_tree:tree(A),
    [catena_tree:tree(A)],
    fun((A) -> boolean()),
    non_neg_integer(),
    non_neg_integer(),
    [pos_integer()],
    [A],
    pos_integer()
) -> {ok, shrink_result(A)}.
search_children(Tree, [], _Predicate, _MaxAttempts, Attempts, PathAcc, TrailAcc, _Index) ->
    shrink_result(Tree, Attempts, PathAcc, TrailAcc, false);
search_children(Tree, [Child | Rest], Predicate, MaxAttempts, Attempts0, PathAcc, TrailAcc, Index) ->
    Attempts1 = Attempts0 + 1,
    ChildValue = catena_tree:root(Child),
    case Predicate(ChildValue) of
        true ->
            search_minimal(
                Child,
                Predicate,
                MaxAttempts,
                Attempts1,
                [Index | PathAcc],
                [ChildValue | TrailAcc]
            );
        false when Attempts1 >= MaxAttempts ->
            shrink_result(Tree, Attempts1, PathAcc, TrailAcc, true);
        false ->
            search_children(Tree, Rest, Predicate, MaxAttempts, Attempts1, PathAcc, TrailAcc, Index + 1)
    end.

-spec shrink_result(catena_tree:tree(A), non_neg_integer(), [pos_integer()], [A], boolean()) ->
    {ok, shrink_result(A)}.
shrink_result(Tree, Attempts, PathAcc, TrailAcc, LimitHit) ->
    {ok, #{
        value => catena_tree:root(Tree),
        path => lists:reverse(PathAcc),
        trail => lists:reverse(TrailAcc),
        attempts => Attempts,
        limit_hit => LimitHit
    }}.

-spec normalize_max_attempts(term()) -> non_neg_integer().
normalize_max_attempts(MaxAttempts) when is_integer(MaxAttempts), MaxAttempts >= 0 ->
    MaxAttempts;
normalize_max_attempts(MaxAttempts) ->
    erlang:error({badarg, {max_attempts, MaxAttempts}}).

%%====================================================================
%% Shared Helpers
%%====================================================================

-spec sign(integer()) -> -1 | 0 | 1.
sign(N) when N < 0 ->
    -1;
sign(0) ->
    0;
sign(_N) ->
    1.

-spec unique_preserving_order([A]) -> [A].
unique_preserving_order(Items) ->
    unique_preserving_order(Items, []).

-spec unique_preserving_order([A], [A]) -> [A].
unique_preserving_order([], Acc) ->
    lists:reverse(Acc);
unique_preserving_order([Item | Rest], Acc) ->
    case lists:member(Item, Acc) of
        true ->
            unique_preserving_order(Rest, Acc);
        false ->
            unique_preserving_order(Rest, [Item | Acc])
    end.
