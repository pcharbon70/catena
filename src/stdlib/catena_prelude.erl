%%%-------------------------------------------------------------------
%%% @doc Catena Standard Prelude (Phase 2.2)
%%%
%%% This module provides the Erlang-side implementation of Catena's
%%% standard prelude. Functions here are available in the REPL and
%%% serve as the runtime implementation until the Catena compiler
%%% can self-compile the prelude.cat files.
%%%
%%% Categories of functions:
%%% - Core functions: identity, const, compose, flip
%%% - List operations: map, filter, fold, foldRight, append
%%% - List helpers: head, tail, length, reverse, take, drop
%%% - Maybe operations: fromMaybe, maybe, isJust, isNothing
%%% - Result operations: fromResult, isOk, isErr
%%%
%%% Type representations:
%%% - Maybe a: {some, Value} | none
%%% - Result a e: {ok, Value} | {err, Error}
%%% - List a: [] | [H | T]  (native Erlang lists)
%%% - Bool: true | false (native Erlang atoms)
%%% @end
%%%-------------------------------------------------------------------
-module(catena_prelude).

-export([
    %% Core functions (2.2.5)
    identity/1,
    const/2,
    compose/2,
    compose/3,
    flip/1,
    flip/3,

    %% List operations (2.2.6)
    map/2,
    filter/2,
    fold/3,
    foldRight/3,
    append/2,

    %% List helpers (2.2.7)
    head/1,
    tail/1,
    length/1,
    reverse/1,
    take/2,
    drop/2,

    %% Maybe operations
    fromMaybe/2,
    'maybe'/3,
    isJust/1,
    isNothing/1,

    %% Result operations
    fromResult/2,
    isOk/1,
    isErr/1,

    %% Functor operations
    fmap/2,

    %% Applicative operations
    pure/1,
    apply_f/2,

    %% Monad operations
    bind/2,
    chain/2,
    join/1,

    %% Environment bindings for REPL
    prelude_bindings/0,
    prelude_types/0
]).

%%====================================================================
%% Core Functions (2.2.5)
%%
%% These are the fundamental building blocks of functional programming.
%% They may seem simple, but they're incredibly useful when composing
%% more complex operations.
%%====================================================================

%% @doc Identity function - returns its argument unchanged.
%%
%% Why is this useful? It's the "do nothing" function, but that's
%% actually valuable! You can use it as a default transformer when
%% you need to pass a function but don't want to change anything.
%%
%% Example: identity(42) => 42
%%          identity("hello") => "hello"
%%
%% identity : a -> a
-spec identity(A) -> A.
identity(X) -> X.

%% @doc Constant function - always returns the first argument, ignoring the second.
%%
%% Why is this useful? When you need a function that ignores its input
%% and always returns a fixed value. Great for creating "placeholder"
%% functions or ignoring unwanted data.
%%
%% Example: const(5, "ignored") => 5
%%          const("always this", 999) => "always this"
%%
%% const : a -> b -> a
-spec const(A, _B) -> A.
const(X, _) -> X.

%% @doc Function composition - chains two functions together.
%%
%% compose(F, G) creates a new function that first applies G, then F.
%% Think of it as a pipeline: data flows through G, then through F.
%% This is the mathematical "F ∘ G" (F after G).
%%
%% Example: If double(x) = x * 2 and addOne(x) = x + 1
%%          compose(double, addOne)(5) => double(addOne(5)) => double(6) => 12
%%
%% compose : (b -> c) -> (a -> b) -> (a -> c)
-spec compose(fun((B) -> C), fun((A) -> B)) -> fun((A) -> C).
compose(F, G) ->
    fun(X) -> F(G(X)) end.

%% @doc Function composition with immediate application.
%%
%% Same as compose/2, but also applies the result to an argument immediately.
%% compose(F, G, X) is equivalent to F(G(X)).
-spec compose(fun((B) -> C), fun((A) -> B), A) -> C.
compose(F, G, X) ->
    F(G(X)).

%% @doc Flip argument order - swaps the order of a function's arguments.
%%
%% Why is this useful? Sometimes you have a function that takes arguments
%% in the "wrong" order for your use case. flip lets you swap them without
%% writing a new function.
%%
%% Example: If subtract(a, b) = a - b
%%          flip(subtract)(3, 10) => subtract(10, 3) => 7
%%
%% flip : (a -> b -> c) -> (b -> a -> c)
-spec flip(fun((A, B) -> C)) -> fun((B, A) -> C).
flip(F) ->
    fun(Y, X) -> F(X, Y) end.

%% @doc Flip with immediate application.
%%
%% flip(F, Y, X) is equivalent to F(X, Y) - applies F with arguments swapped.
-spec flip(fun((A, B) -> C), B, A) -> C.
flip(F, Y, X) ->
    F(X, Y).

%%====================================================================
%% List Operations (2.2.6)
%%
%% These are the workhorses of functional programming. Almost every
%% program that processes collections will use these operations.
%% Master these and you'll handle most data transformations elegantly.
%%====================================================================

%% @doc Map a function over every element in a list.
%%
%% This is one of the most important functions in FP! It transforms
%% each element while preserving the list structure. Think of it as
%% "do this to everything in the list."
%%
%% Example: map(fun(X) -> X * 2 end, [1, 2, 3]) => [2, 4, 6]
%%          map(fun(S) -> string:uppercase(S) end, ["a", "b"]) => ["A", "B"]
%%
%% map : (a -> b) -> List a -> List b
-spec map(fun((A) -> B), [A]) -> [B].
map(F, List) ->
    [F(X) || X <- List].

%% @doc Keep only elements that satisfy a predicate (condition).
%%
%% Filter examines each element and keeps only those where the
%% predicate returns true. It's like a sieve that lets certain
%% elements through.
%%
%% Example: filter(fun(X) -> X > 2 end, [1, 2, 3, 4]) => [3, 4]
%%          filter(fun(X) -> X rem 2 == 0 end, [1, 2, 3, 4]) => [2, 4] (evens only)
%%
%% filter : (a -> Bool) -> List a -> List a
-spec filter(fun((A) -> boolean()), [A]) -> [A].
filter(Pred, List) ->
    [X || X <- List, Pred(X)].

%% @doc Left fold - reduce a list to a single value, processing left to right.
%%
%% Fold is incredibly powerful! It walks through the list, accumulating
%% a result. The accumulator starts with an initial value and gets
%% updated by combining it with each element.
%%
%% Think of it as: "Start with Acc, then for each element, combine Acc with element"
%%
%% Example: fold(fun(Acc, X) -> Acc + X end, 0, [1, 2, 3]) => 6 (sum)
%%          fold(fun(Acc, X) -> Acc * X end, 1, [1, 2, 3, 4]) => 24 (product)
%%
%% fold : (b -> a -> b) -> b -> List a -> b
-spec fold(fun((B, A) -> B), B, [A]) -> B.
fold(_F, Acc, []) -> Acc;
fold(F, Acc, [H | T]) ->
    fold(F, F(Acc, H), T).

%% @doc Right fold - reduce a list to a single value, processing right to left.
%%
%% Like fold, but processes from the end of the list backward. This matters
%% for non-associative operations and is often more natural for building
%% new lists (since lists are built right-to-left in Erlang/Catena).
%%
%% Example: foldRight(fun(X, Acc) -> [X * 2 | Acc] end, [], [1, 2, 3]) => [2, 4, 6]
%%
%% foldRight : (a -> b -> b) -> b -> List a -> b
-spec foldRight(fun((A, B) -> B), B, [A]) -> B.
foldRight(_F, Acc, []) -> Acc;
foldRight(F, Acc, [H | T]) ->
    F(H, foldRight(F, Acc, T)).

%% @doc Concatenate two lists together.
%%
%% Append joins two lists end-to-end. Simple but essential!
%%
%% Example: append([1, 2], [3, 4]) => [1, 2, 3, 4]
%%          append("Hello, ", "World!") => "Hello, World!"
%%
%% append : List a -> List a -> List a
-spec append([A], [A]) -> [A].
append(Xs, Ys) ->
    Xs ++ Ys.

%%====================================================================
%% List Helpers (2.2.7)
%%
%% Convenient utilities for common list operations. These make working
%% with lists easier and more expressive.
%%====================================================================

%% @doc Get the first element of a list safely.
%%
%% Returns Some(element) if the list is non-empty, or None if empty.
%% This is safer than crashing on an empty list! Always use this
%% instead of pattern matching when you're not sure the list has elements.
%%
%% Example: head([1, 2, 3]) => {some, 1}
%%          head([]) => none
%%
%% head : List a -> Maybe a
-spec head([A]) -> {some, A} | none.
head([]) -> none;
head([H | _]) -> {some, H}.

%% @doc Get everything except the first element, safely.
%%
%% Returns Some(rest) if the list has at least one element, or None if empty.
%% Useful for recursive processing where you need the remaining elements.
%%
%% Example: tail([1, 2, 3]) => {some, [2, 3]}
%%          tail([1]) => {some, []}
%%          tail([]) => none
%%
%% tail : List a -> Maybe (List a)
-spec tail([A]) -> {some, [A]} | none.
tail([]) -> none;
tail([_ | T]) -> {some, T}.

%% @doc Count the number of elements in a list.
%%
%% Example: length([1, 2, 3]) => 3
%%          length([]) => 0
%%
%% length : List a -> Natural
-spec length([_]) -> non_neg_integer().
length(List) ->
    erlang:length(List).

%% @doc Reverse the order of elements in a list.
%%
%% Example: reverse([1, 2, 3]) => [3, 2, 1]
%%          reverse("hello") => "olleh"
%%
%% reverse : List a -> List a
-spec reverse([A]) -> [A].
reverse(List) ->
    lists:reverse(List).

%% @doc Take the first n elements from a list.
%%
%% Returns at most n elements. If the list is shorter than n,
%% returns the whole list (no error).
%%
%% Example: take(2, [1, 2, 3, 4]) => [1, 2]
%%          take(10, [1, 2]) => [1, 2]
%%          take(0, [1, 2, 3]) => []
%%
%% take : Natural -> List a -> List a
-spec take(non_neg_integer(), [A]) -> [A].
take(0, _) -> [];
take(_, []) -> [];
take(N, [H | T]) when N > 0 ->
    [H | take(N - 1, T)].

%% @doc Remove the first n elements from a list.
%%
%% The opposite of take. Returns what remains after skipping n elements.
%%
%% Example: drop(2, [1, 2, 3, 4]) => [3, 4]
%%          drop(10, [1, 2]) => []
%%          drop(0, [1, 2, 3]) => [1, 2, 3]
%%
%% drop : Natural -> List a -> List a
-spec drop(non_neg_integer(), [A]) -> [A].
drop(0, List) -> List;
drop(_, []) -> [];
drop(N, [_ | T]) when N > 0 ->
    drop(N - 1, T).

%%====================================================================
%% Maybe Operations
%%
%% Maybe represents optional values - something that might or might not
%% exist. It's a safe alternative to null/nil that forces you to handle
%% the "nothing" case explicitly.
%%
%% Maybe a = None (nothing there) | Some a (contains a value)
%%====================================================================

%% @doc Extract a value from Maybe, with a fallback default.
%%
%% If the Maybe contains a value, return it. Otherwise, return the default.
%% This is the safe way to "unwrap" a Maybe when you have a sensible default.
%%
%% Example: fromMaybe(0, {some, 42}) => 42
%%          fromMaybe(0, none) => 0
%%          fromMaybe("unknown", {some, "Alice"}) => "Alice"
%%
%% fromMaybe : a -> Maybe a -> a
-spec fromMaybe(A, {some, A} | none) -> A.
fromMaybe(Default, none) -> Default;
fromMaybe(_, {some, X}) -> X.

%% @doc Pattern match on Maybe with functions for each case.
%%
%% Like a switch/case but functional style. Provide what to return for
%% None, and a function to apply to the value for Some.
%%
%% Example: maybe(0, fun(X) -> X * 2 end, {some, 21}) => 42
%%          maybe(0, fun(X) -> X * 2 end, none) => 0
%%
%% maybe : b -> (a -> b) -> Maybe a -> b
-spec 'maybe'(B, fun((A) -> B), {some, A} | none) -> B.
'maybe'(Default, _, none) -> Default;
'maybe'(_, F, {some, X}) -> F(X).

%% @doc Check if a Maybe contains a value.
%%
%% Example: isJust({some, 42}) => true
%%          isJust(none) => false
%%
%% isJust : Maybe a -> Bool
-spec isJust({some, _} | none) -> boolean().
isJust({some, _}) -> true;
isJust(none) -> false.

%% @doc Check if a Maybe is empty.
%%
%% Example: isNothing(none) => true
%%          isNothing({some, 42}) => false
%%
%% isNothing : Maybe a -> Bool
-spec isNothing({some, _} | none) -> boolean().
isNothing(none) -> true;
isNothing({some, _}) -> false.

%%====================================================================
%% Result Operations
%%
%% Result represents computations that might fail with an error.
%% Unlike Maybe, Result carries information about WHY something failed.
%%
%% Result a e = Ok a (success with value) | Err e (failure with error)
%%====================================================================

%% @doc Extract a value from Result, with a fallback default on error.
%%
%% If the Result is Ok, return the value. If it's Err, return the default.
%%
%% Example: fromResult(0, {ok, 42}) => 42
%%          fromResult(0, {err, "oops"}) => 0
%%
%% fromResult : a -> Result a e -> a
-spec fromResult(A, {ok, A} | {err, _}) -> A.
fromResult(Default, {err, _}) -> Default;
fromResult(_, {ok, X}) -> X.

%% @doc Check if a Result is successful.
%%
%% Example: isOk({ok, 42}) => true
%%          isOk({err, "failed"}) => false
%%
%% isOk : Result a e -> Bool
-spec isOk({ok, _} | {err, _}) -> boolean().
isOk({ok, _}) -> true;
isOk({err, _}) -> false.

%% @doc Check if a Result is an error.
%%
%% Example: isErr({err, "failed"}) => true
%%          isErr({ok, 42}) => false
%%
%% isErr : Result a e -> Bool
-spec isErr({ok, _} | {err, _}) -> boolean().
isErr({err, _}) -> true;
isErr({ok, _}) -> false.

%%====================================================================
%% Functor Operations
%%
%% A Functor is anything you can "map over" - a container or context
%% that holds values. The fmap function lets you transform the values
%% inside without changing the container structure.
%%
%% Think of it as: "Apply this function to whatever is inside, but
%% keep the wrapper the same."
%%====================================================================

%% @doc Apply a function to values inside a Functor (mappable container).
%%
%% Works on Lists, Maybe, and Result. The container structure is preserved,
%% only the values inside are transformed.
%%
%% Example: fmap(fun(X) -> X * 2 end, [1, 2, 3]) => [2, 4, 6]
%%          fmap(fun(X) -> X * 2 end, {some, 21}) => {some, 42}
%%          fmap(fun(X) -> X * 2 end, none) => none
%%          fmap(fun(X) -> X * 2 end, {ok, 21}) => {ok, 42}
%%          fmap(fun(X) -> X * 2 end, {err, "oops"}) => {err, "oops"}
%%
%% fmap : (a -> b) -> f a -> f b
-spec fmap(fun((term()) -> term()), term()) -> term().
fmap(F, List) when is_list(List) ->
    map(F, List);
fmap(F, {some, X}) ->
    {some, F(X)};
fmap(_, none) ->
    none;
fmap(F, {ok, X}) ->
    {ok, F(X)};
fmap(_, {err, E}) ->
    {err, E}.

%%====================================================================
%% Applicative Operations
%%
%% Applicative extends Functor with two capabilities:
%% 1. pure: Put any value into the container (lift a value)
%% 2. apply: Apply a function INSIDE a container to a value INSIDE a container
%%
%% This lets you work with multiple wrapped values together.
%%====================================================================

%% @doc Lift a plain value into a container context.
%%
%% This wraps a value in the simplest possible container. For Maybe,
%% that's Some. It's like saying "here's a value, and it definitely exists."
%%
%% Example: pure(42) => {some, 42}
%%
%% pure : a -> f a
-spec pure(A) -> {some, A}.
pure(X) -> {some, X}.

%% @doc Apply a wrapped function to a wrapped value.
%%
%% When you have a function inside a container and a value inside a container,
%% apply lets you combine them. If either is "empty" (None/Err), the result
%% is empty too.
%%
%% Example: apply_f({some, fun(X) -> X * 2 end}, {some, 21}) => {some, 42}
%%          apply_f({some, fun(X) -> X * 2 end}, none) => none
%%          apply_f(none, {some, 21}) => none
%%
%% apply : f (a -> b) -> f a -> f b
-spec apply_f(term(), term()) -> term().
apply_f({some, F}, {some, X}) ->
    {some, F(X)};
apply_f(_, none) ->
    none;
apply_f(none, _) ->
    none;
apply_f({ok, F}, {ok, X}) ->
    {ok, F(X)};
apply_f({err, E}, _) ->
    {err, E};
apply_f(_, {err, E}) ->
    {err, E};
apply_f(Fs, Xs) when is_list(Fs), is_list(Xs) ->
    [F(X) || F <- Fs, X <- Xs].

%%====================================================================
%% Monad Operations
%%
%% Monad is the famous "M-word" of functional programming! It extends
%% Applicative with the ability to chain operations that each produce
%% wrapped values.
%%
%% The key insight: bind (>>=) lets you sequence operations where each
%% step depends on the previous result, and any step can "fail" (return
%% None/Err) which short-circuits the whole chain.
%%====================================================================

%% @doc Monadic bind - chain computations that produce wrapped values.
%%
%% This is the heart of monadic programming! Given a wrapped value and
%% a function that takes an unwrapped value and returns a wrapped value,
%% bind "reaches into" the first wrapper, applies the function, and
%% returns the result (which is already wrapped).
%%
%% If the first value is "empty" (None/Err), the function is never called.
%%
%% Example: bind({some, 21}, fun(X) -> {some, X * 2} end) => {some, 42}
%%          bind(none, fun(X) -> {some, X * 2} end) => none
%%          bind([1, 2], fun(X) -> [X, X * 10] end) => [1, 10, 2, 20]
%%
%% bind : m a -> (a -> m b) -> m b
-spec bind(term(), fun((term()) -> term())) -> term().
bind({some, X}, F) ->
    F(X);
bind(none, _) ->
    none;
bind({ok, X}, F) ->
    F(X);
bind({err, E}, _) ->
    {err, E};
bind(List, F) when is_list(List) ->
    lists:flatmap(F, List).

%% @doc Chain - same as bind but with arguments flipped.
%%
%% This ordering (function first, then value) is often more convenient
%% for pipelines and is what do-notation desugars to.
%%
%% Example: chain(fun(X) -> {some, X * 2} end, {some, 21}) => {some, 42}
%%
%% chain : (a -> m b) -> m a -> m b
-spec chain(fun((term()) -> term()), term()) -> term().
chain(F, M) ->
    bind(M, F).

%% @doc Join - flatten a doubly-nested container into a single layer.
%%
%% When you have a container inside a container (like Some(Some(42))),
%% join removes one layer of wrapping.
%%
%% Example: join({some, {some, 42}}) => {some, 42}
%%          join({some, none}) => none
%%          join([[1, 2], [3, 4]]) => [1, 2, 3, 4]
%%
%% join : m (m a) -> m a
-spec join(term()) -> term().
join({some, {some, X}}) -> {some, X};
join({some, none}) -> none;
join(none) -> none;
join({ok, {ok, X}}) -> {ok, X};
join({ok, {err, E}}) -> {err, E};
join({err, E}) -> {err, E};
join(List) when is_list(List) ->
    lists:flatten(List).

%%====================================================================
%% Environment Bindings for REPL
%%
%% These functions provide the prelude to the REPL, making all the
%% above functions available when you start an interactive session.
%%====================================================================

%% @doc Get all prelude bindings for REPL environment.
%%
%% Returns a map of {Name, {Fun, Arity, Type}} for each prelude function.
%% The REPL uses this to populate its initial environment.
-spec prelude_bindings() -> #{atom() => {function(), arity(), term()}}.
prelude_bindings() ->
    #{
        %% Core functions
        identity => {fun identity/1, 1, forall_a_to_a()},
        const => {fun const/2, 2, forall_a_b_to_a()},
        compose => {fun compose/2, 2, compose_type()},
        flip => {fun flip/1, 1, flip_type()},

        %% List operations
        map => {fun map/2, 2, map_type()},
        filter => {fun filter/2, 2, filter_type()},
        fold => {fun fold/3, 3, fold_type()},
        foldRight => {fun foldRight/3, 3, foldRight_type()},
        append => {fun append/2, 2, append_type()},

        %% List helpers
        head => {fun head/1, 1, head_type()},
        tail => {fun tail/1, 1, tail_type()},
        length => {fun ?MODULE:length/1, 1, length_type()},
        reverse => {fun reverse/1, 1, reverse_type()},
        take => {fun take/2, 2, take_type()},
        drop => {fun drop/2, 2, drop_type()},

        %% Maybe operations
        fromMaybe => {fun fromMaybe/2, 2, fromMaybe_type()},
        'maybe' => {fun 'maybe'/3, 3, maybe_type()},
        isJust => {fun isJust/1, 1, isJust_type()},
        isNothing => {fun isNothing/1, 1, isNothing_type()},

        %% Result operations
        fromResult => {fun fromResult/2, 2, fromResult_type()},
        isOk => {fun isOk/1, 1, isOk_type()},
        isErr => {fun isErr/1, 1, isErr_type()},

        %% Functor/Monad operations
        fmap => {fun fmap/2, 2, fmap_type()},
        pure => {fun pure/1, 1, pure_type()},
        bind => {fun bind/2, 2, bind_type()},
        chain => {fun chain/2, 2, chain_type()},
        join => {fun join/1, 1, join_type()}
    }.

%% @doc Get prelude type definitions.
%%
%% Returns AST for type definitions (Maybe, Result, etc.) to be added
%% to the type environment. These define the algebraic data types that
%% the prelude functions work with.
-spec prelude_types() -> [term()].
prelude_types() ->
    [
        %% type Maybe a = None | Some a
        %% Maybe represents optional values - use instead of null!
        {type_decl, 'Maybe', [a],
            [{variant, 'None', []},
             {variant, 'Some', [{tvar, a}]}]},

        %% type Result a e = Ok a | Err e
        %% Result represents operations that can fail with an error message
        {type_decl, 'Result', [a, e],
            [{variant, 'Ok', [{tvar, a}]},
             {variant, 'Err', [{tvar, e}]}]},

        %% type Ordering = LT | EQ | GT
        %% Ordering is returned by comparison functions
        {type_decl, 'Ordering', [],
            [{variant, 'LT', []},
             {variant, 'EQ', []},
             {variant, 'GT', []}]},

        %% type Either a b = Left a | Right b
        %% Either holds one of two possible types - useful for branching
        {type_decl, 'Either', [a, b],
            [{variant, 'Left', [{tvar, a}]},
             {variant, 'Right', [{tvar, b}]}]}
    ].

%%====================================================================
%% Type Signatures (simplified AST representations)
%%
%% These define the types of prelude functions for the type checker.
%% They use a simplified internal representation.
%%====================================================================

%% forall a. a -> a
forall_a_to_a() ->
    {forall, [a], {tfun, {tvar, a}, {tvar, a}}}.

%% forall a b. a -> b -> a
forall_a_b_to_a() ->
    {forall, [a, b], {tfun, {tvar, a}, {tfun, {tvar, b}, {tvar, a}}}}.

%% forall a b c. (b -> c) -> (a -> b) -> (a -> c)
compose_type() ->
    {forall, [a, b, c],
        {tfun,
            {tfun, {tvar, b}, {tvar, c}},
            {tfun,
                {tfun, {tvar, a}, {tvar, b}},
                {tfun, {tvar, a}, {tvar, c}}}}}.

%% forall a b c. (a -> b -> c) -> (b -> a -> c)
flip_type() ->
    {forall, [a, b, c],
        {tfun,
            {tfun, {tvar, a}, {tfun, {tvar, b}, {tvar, c}}},
            {tfun, {tvar, b}, {tfun, {tvar, a}, {tvar, c}}}}}.

%% forall a b. (a -> b) -> List a -> List b
map_type() ->
    {forall, [a, b],
        {tfun,
            {tfun, {tvar, a}, {tvar, b}},
            {tfun, {tapp, list, {tvar, a}}, {tapp, list, {tvar, b}}}}}.

%% forall a. (a -> Bool) -> List a -> List a
filter_type() ->
    {forall, [a],
        {tfun,
            {tfun, {tvar, a}, {tcon, bool}},
            {tfun, {tapp, list, {tvar, a}}, {tapp, list, {tvar, a}}}}}.

%% forall a b. (b -> a -> b) -> b -> List a -> b
fold_type() ->
    {forall, [a, b],
        {tfun,
            {tfun, {tvar, b}, {tfun, {tvar, a}, {tvar, b}}},
            {tfun, {tvar, b}, {tfun, {tapp, list, {tvar, a}}, {tvar, b}}}}}.

%% forall a b. (a -> b -> b) -> b -> List a -> b
foldRight_type() ->
    {forall, [a, b],
        {tfun,
            {tfun, {tvar, a}, {tfun, {tvar, b}, {tvar, b}}},
            {tfun, {tvar, b}, {tfun, {tapp, list, {tvar, a}}, {tvar, b}}}}}.

%% forall a. List a -> List a -> List a
append_type() ->
    {forall, [a],
        {tfun, {tapp, list, {tvar, a}}, {tfun, {tapp, list, {tvar, a}}, {tapp, list, {tvar, a}}}}}.

%% forall a. List a -> Maybe a
head_type() ->
    {forall, [a],
        {tfun, {tapp, list, {tvar, a}}, {tapp, 'Maybe', {tvar, a}}}}.

%% forall a. List a -> Maybe (List a)
tail_type() ->
    {forall, [a],
        {tfun, {tapp, list, {tvar, a}}, {tapp, 'Maybe', {tapp, list, {tvar, a}}}}}.

%% forall a. List a -> Natural
length_type() ->
    {forall, [a],
        {tfun, {tapp, list, {tvar, a}}, {tcon, natural}}}.

%% forall a. List a -> List a
reverse_type() ->
    {forall, [a],
        {tfun, {tapp, list, {tvar, a}}, {tapp, list, {tvar, a}}}}.

%% forall a. Natural -> List a -> List a
take_type() ->
    {forall, [a],
        {tfun, {tcon, natural}, {tfun, {tapp, list, {tvar, a}}, {tapp, list, {tvar, a}}}}}.

%% forall a. Natural -> List a -> List a
drop_type() ->
    {forall, [a],
        {tfun, {tcon, natural}, {tfun, {tapp, list, {tvar, a}}, {tapp, list, {tvar, a}}}}}.

%% forall a. a -> Maybe a -> a
fromMaybe_type() ->
    {forall, [a],
        {tfun, {tvar, a}, {tfun, {tapp, 'Maybe', {tvar, a}}, {tvar, a}}}}.

%% forall a b. b -> (a -> b) -> Maybe a -> b
maybe_type() ->
    {forall, [a, b],
        {tfun, {tvar, b}, {tfun, {tfun, {tvar, a}, {tvar, b}}, {tfun, {tapp, 'Maybe', {tvar, a}}, {tvar, b}}}}}.

%% forall a. Maybe a -> Bool
isJust_type() ->
    {forall, [a],
        {tfun, {tapp, 'Maybe', {tvar, a}}, {tcon, bool}}}.

%% forall a. Maybe a -> Bool
isNothing_type() ->
    {forall, [a],
        {tfun, {tapp, 'Maybe', {tvar, a}}, {tcon, bool}}}.

%% forall a e. a -> Result a e -> a
fromResult_type() ->
    {forall, [a, e],
        {tfun, {tvar, a}, {tfun, {tapp2, result, {tvar, a}, {tvar, e}}, {tvar, a}}}}.

%% forall a e. Result a e -> Bool
isOk_type() ->
    {forall, [a, e],
        {tfun, {tapp2, result, {tvar, a}, {tvar, e}}, {tcon, bool}}}.

%% forall a e. Result a e -> Bool
isErr_type() ->
    {forall, [a, e],
        {tfun, {tapp2, result, {tvar, a}, {tvar, e}}, {tcon, bool}}}.

%% forall f a b. Functor f => (a -> b) -> f a -> f b
fmap_type() ->
    {forall, [a, b],
        {tfun, {tfun, {tvar, a}, {tvar, b}}, {tfun, {tvar, fa}, {tvar, fb}}}}.

%% forall a. a -> Maybe a (simplified - should be forall f. Applicative f => a -> f a)
pure_type() ->
    {forall, [a],
        {tfun, {tvar, a}, {tapp, 'Maybe', {tvar, a}}}}.

%% forall m a b. Monad m => m a -> (a -> m b) -> m b
bind_type() ->
    {forall, [a, b],
        {tfun, {tvar, ma}, {tfun, {tfun, {tvar, a}, {tvar, mb}}, {tvar, mb}}}}.

%% forall m a b. Monad m => (a -> m b) -> m a -> m b
chain_type() ->
    {forall, [a, b],
        {tfun, {tfun, {tvar, a}, {tvar, mb}}, {tfun, {tvar, ma}, {tvar, mb}}}}.

%% forall m a. Monad m => m (m a) -> m a
join_type() ->
    {forall, [a],
        {tfun, {tvar, mma}, {tvar, ma}}}.
