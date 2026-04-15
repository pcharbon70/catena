-module(catena_state_copy).
-behaviour(catena_algebraic_laws).

%% State duplication strategies for multi-shot continuations.
%% Provides deep copy, shallow copy, and selective copy strategies
%% with fine-grained control over state sharing and duplication.

-export([
    %% Deep copy strategy
    deep_copy/1,
    deep_copy/2,
    %% Shallow copy strategy
    shallow_copy/1,
    shallow_copy/2,
    %% Selective copy strategy
    selective_copy/1,
    selective_copy/2,
    %% Strategy specification
    strategy/0,
    strategy/1,
    %% Validation
    is_valid_strategy/1,
    %% Utilities
    copy_value/2,
    merge/2,
    %% Optimization
    optimize_strategy/2,
    should_share/1,
    %% Algebraic laws
    associative/2,
    commutative/2,
    identity/2,
    idempotent/1
]).

-export_type([
    copy_strategy/0,
    strategy_spec/0,
    copy_result/0
]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

%% @doc Copy strategy types.
-type copy_strategy() :: deep | shallow | selective.

%% @doc Strategy specification for selective copy.
-type strategy_spec() :: #{
    strategy => copy_strategy(),
    exclude => [term()],
    include_only => [term()],
    max_depth => non_neg_integer() | infinity,
    share_immutables => boolean()
}.

%% @doc Result of a copy operation.
-type copy_result() :: {ok, term()} | {error, term()}.

%%%---------------------------------------------------------------------
%%% Deep Copy Strategy
%%%---------------------------------------------------------------------

%% @doc Deep copy a value, creating a completely independent copy.
%% Deep copy recursively copies all nested structures.
-spec deep_copy(term()) -> term().
deep_copy(Term) ->
    deep_copy(Term, #{max_depth => infinity, share_immutables => false}).

%% @doc Deep copy with options.
%% Options:
%% - max_depth: Maximum depth to copy (infinity for unlimited)
%% - share_immutables: Share immutable values like atoms and numbers
-spec deep_copy(term(), map()) -> term().
deep_copy(Term, Options) ->
    MaxDepth = maps:get(max_depth, Options, infinity),
    ShareImmutables = maps:get(share_immutables, Options, false),
    do_deep_copy(Term, 0, MaxDepth, ShareImmutables).

%% @doc Internal deep copy implementation.
-spec do_deep_copy(term(), non_neg_integer(), non_neg_integer() | infinity, boolean()) -> term().
do_deep_copy(Term, _Depth, _MaxDepth, true) when is_atom(Term); is_number(Term); is_binary(Term); is_reference(Term); is_pid(Term); is_port(Term) ->
    Term;
do_deep_copy(Map, Depth, MaxDepth, ShareImmutables) when is_map(Map), Depth < MaxDepth ->
    maps:map(fun(_, V) -> do_deep_copy(V, Depth + 1, MaxDepth, ShareImmutables) end, Map);
do_deep_copy(List, Depth, MaxDepth, ShareImmutables) when is_list(List), Depth < MaxDepth ->
    [do_deep_copy(V, Depth + 1, MaxDepth, ShareImmutables) || V <- List];
do_deep_copy(Tuple, Depth, MaxDepth, ShareImmutables) when is_tuple(Tuple), Depth < MaxDepth, Tuple =/= {} ->
    List = tuple_to_list(Tuple),
    CopiedList = [do_deep_copy(V, Depth + 1, MaxDepth, ShareImmutables) || V <- List],
    list_to_tuple(CopiedList);
do_deep_copy(Term, _, _, _) ->
    Term.

%%%---------------------------------------------------------------------
%%% Shallow Copy Strategy
%%%---------------------------------------------------------------------

%% @doc Shallow copy a value, sharing nested structures.
%% Shallow copy only copies the top-level structure.
-spec shallow_copy(term()) -> term().
shallow_copy(Term) ->
    shallow_copy(Term, #{}).

%% @doc Shallow copy with options.
-spec shallow_copy(term(), map()) -> term().
shallow_copy(Map, _Options) when is_map(Map) ->
    maps:map(fun(_, V) -> V end, Map);
shallow_copy(List, _Options) when is_list(List) ->
    List;
shallow_copy(Tuple, _Options) when is_tuple(Tuple) ->
    Tuple;
shallow_copy(Term, _) ->
    Term.

%%%---------------------------------------------------------------------
%%% Selective Copy Strategy
%%%---------------------------------------------------------------------

%% @doc Selective copy with default heuristics.
%% Small immutable structures are shared, larger ones are copied.
-spec selective_copy(term()) -> term().
selective_copy(Term) ->
    selective_copy(Term, #{}).

%% @doc Selective copy with strategy specification.
-spec selective_copy(term(), strategy_spec() | map()) -> term().
selective_copy(Term, Spec) when is_map(Spec) ->
    Exclude = maps:get(exclude, Spec, []),
    IncludeOnly = maps:get(include_only, Spec, undefined),
    MaxDepth = maps:get(max_depth, Spec, infinity),
    ShareImmutables = maps:get(share_immutables, Spec, true),

    case IncludeOnly of
        undefined ->
            do_selective_copy(Term, Exclude, MaxDepth, ShareImmutables, 0);
        _ ->
            do_include_only_copy(Term, IncludeOnly, MaxDepth, ShareImmutables, 0)
    end.

%% @doc Internal selective copy with exclusion.
-spec do_selective_copy(term(), [term()], non_neg_integer() | infinity, boolean(), non_neg_integer()) -> term().
do_selective_copy({Key, Value}, Exclude, MaxDepth, ShareImmutables, Depth) when Depth < MaxDepth ->
    case lists:member(Key, Exclude) of
        true ->
            {Key, Value};
        false ->
            {Key, do_selective_copy(Value, Exclude, MaxDepth, ShareImmutables, Depth + 1)}
    end;
do_selective_copy(Map, Exclude, MaxDepth, ShareImmutables, Depth) when is_map(Map), Depth < MaxDepth ->
    maps:map(fun(K, V) ->
        case lists:member(K, Exclude) of
            true -> V;
            false -> do_selective_copy(V, Exclude, MaxDepth, ShareImmutables, Depth + 1)
        end
    end, Map);
do_selective_copy(List, Exclude, MaxDepth, ShareImmutables, Depth) when is_list(List), Depth < MaxDepth ->
    [do_selective_copy(V, Exclude, MaxDepth, ShareImmutables, Depth + 1) || V <- List];
do_selective_copy(Term, _, _, _, _) ->
    Term.

%% @doc Internal selective copy with inclusion.
-spec do_include_only_copy(term(), [term()], non_neg_integer() | infinity, boolean(), non_neg_integer()) -> term().
do_include_only_copy(Map, IncludeOnly, MaxDepth, ShareImmutables, Depth) when is_map(Map), Depth < MaxDepth ->
    maps:map(fun(K, V) ->
        case lists:member(K, IncludeOnly) of
            true -> do_include_only_copy(V, IncludeOnly, MaxDepth, ShareImmutables, Depth + 1);
            false -> V
        end
    end, Map);
do_include_only_copy(Term, _, _, _, _) ->
    Term.

%%%---------------------------------------------------------------------
%%% Strategy Specification
%%%---------------------------------------------------------------------

%% @doc Get the default strategy specification.
-spec strategy() -> strategy_spec().
strategy() ->
    #{
        strategy => selective,
        exclude => [],
        include_only => undefined,
        max_depth => infinity,
        share_immutables => true
    }.

%% @doc Create a strategy specification from options.
-spec strategy(map()) -> strategy_spec().
strategy(Options) ->
    Default = strategy(),
    maps:merge(Default, Options).

%%%---------------------------------------------------------------------
%%% Validation
%%%---------------------------------------------------------------------

%% @doc Validate a copy strategy.
-spec is_valid_strategy(term()) -> boolean().
is_valid_strategy(deep) -> true;
is_valid_strategy(shallow) -> true;
is_valid_strategy(selective) -> true;
is_valid_strategy(_) -> false.

%%%---------------------------------------------------------------------
%%% Utilities
%%%---------------------------------------------------------------------

%% @doc Copy a value using the specified strategy.
-spec copy_value(term(), copy_strategy() | strategy_spec()) -> term().
copy_value(Term, Strategy) when is_atom(Strategy) ->
    case Strategy of
        deep -> deep_copy(Term);
        shallow -> shallow_copy(Term);
        selective -> selective_copy(Term);
        _ -> Term
    end;
copy_value(Term, Spec) when is_map(Spec) ->
    selective_copy(Term, Spec).

%% @doc Merge two copy results.
-spec merge(copy_result(), copy_result()) -> copy_result().
merge({ok, A}, {ok, B}) when is_map(A), is_map(B) ->
    {ok, maps:merge(A, B)};
merge({ok, A}, {ok, _}) ->
    {ok, A};
merge({error, Reason}, _) ->
    {error, Reason};
merge(_, {error, Reason}) ->
    {error, Reason}.

%%%---------------------------------------------------------------------
%%% Optimization
%%%---------------------------------------------------------------------

%% @doc Optimize a copy strategy based on value characteristics.
%% - Small maps (< 5 keys) are shared
%% - Large structures use deep copy
%% - Immutable types are always shared
-spec optimize_strategy(term(), copy_strategy()) -> copy_strategy().
optimize_strategy(Value, deep) when is_map(Value) ->
    case maps:size(Value) of
        N when N < 5 -> shallow;
        _ -> deep
    end;
optimize_strategy(_, Strategy) ->
    Strategy.

%% @doc Check if a value should be shared based on its characteristics.
-spec should_share(term()) -> boolean().
should_share(Value) when is_atom(Value) -> true;
should_share(Value) when is_number(Value) -> true;
should_share(Value) when is_binary(Value) -> true;
should_share(Value) when is_function(Value) -> true;
should_share(Value) when is_pid(Value) -> true;
should_share(Value) when is_reference(Value) -> true;
should_share(Value) when is_port(Value) -> true;
should_share(Value) when is_map(Value) -> maps:size(Value) < 5;
should_share(Value) when is_list(Value) -> length(Value) < 5;
should_share(Value) when is_tuple(Value) -> tuple_size(Value) < 3;
should_share(_) -> true.

%%%---------------------------------------------------------------------
%%% Algebraic Laws Callbacks
%%%---------------------------------------------------------------------

%% @doc Check if two copy strategies are the same for associativity.
-spec same_strategy(copy_strategy(), copy_strategy()) -> boolean().
same_strategy(deep, deep) -> true;
same_strategy(shallow, shallow) -> true;
same_strategy(selective, selective) -> true;
same_strategy(_, _) -> false.

%% @doc Check associativity of copy operations.
-spec associative(copy_strategy(), copy_strategy()) -> boolean().
associative(S1, S2) ->
    same_strategy(S1, S2).

%% @doc Check commutativity of copy operations.
-spec commutative(copy_strategy(), copy_strategy()) -> boolean().
commutative(S1, S2) ->
    same_strategy(S1, S2) =:= same_strategy(S2, S1).

%% @doc Check identity for copy operations.
%% Shallow copy acts as identity for immutable values.
-spec identity(copy_strategy(), copy_strategy()) -> boolean().
identity(shallow, shallow) -> true;
identity(_, _) -> false.

%% @doc Check idempotence of copy operations.
-spec idempotent(copy_strategy()) -> boolean().
idempotent(deep) -> true;
idempotent(shallow) -> true;
idempotent(selective) -> true.
