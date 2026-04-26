-module(catena_continuation_kind).
-behaviour(catena_algebraic_laws).

%% Continuation kind types define whether continuations can be resumed
%% multiple times (multi-shot) or only once (one-shot). This distinction
%% is crucial for resource management and enables patterns like backtracking
%% and nondeterminism.

-export([
    %% Type constructors
    one_shot/0,
    multi_shot/0,
    default_kind/0,
    current_kind/0,
    with_kind/2,
    %% Type predicates
    is_one_shot/0,
    is_one_shot/1,
    is_multi_shot/0,
    is_multi_shot/1,
    %% Type validation
    is_valid_kind/1,
    %% Conversion
    to_atom/1,
    from_atom/1,
    compose/2,
    %% Semantics documentation
    semantics/1,
    use_cases/1,
    %% Algebraic laws callbacks
    associative/2,
    commutative/2,
    identity/2,
    idempotent/1
]).

-define(CURRENT_KIND_KEY, catena_current_continuation_kind).

-export_type([
    continuation_kind/0
]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

%% @doc Continuation kind type.
%% - `one_shot`: Continuation can only be resumed once. After resumption,
%%   the continuation is consumed and cannot be used again. This enables
%%   precise resource management and cleanup guarantees.
%% - `multi_shot`: Continuation can be resumed multiple times. Each resume
%%   operates on a copy of the captured state (either deep or shallow copy).
%%   This enables backtracking, nondeterminism, and solution enumeration.
-type continuation_kind() :: one_shot | multi_shot.

%%%---------------------------------------------------------------------
%%% Type Constructors
%%%---------------------------------------------------------------------

%% @doc Create a one-shot continuation kind.
%% One-shot continuations can only be resumed once and provide:
%% - Precise resource cleanup guarantees
%% - Linear consumption semantics
%% - No accidental duplication
-spec one_shot() -> continuation_kind().
one_shot() -> one_shot.

%% @doc Create a multi-shot continuation kind.
%% Multi-shot continuations can be resumed multiple times and provide:
%% - Backtracking capabilities
%% - Nondeterministic search
%% - Solution enumeration
-spec multi_shot() -> continuation_kind().
multi_shot() -> multi_shot.

%% @doc Get the default continuation kind for effect execution.
%%
%% Catena defaults to multi-shot continuations unless a handler
%% explicitly narrows execution to one-shot semantics.
-spec default_kind() -> continuation_kind().
default_kind() ->
    multi_shot.

%% @doc Get the continuation kind currently active in this process.
-spec current_kind() -> continuation_kind().
current_kind() ->
    case get(?CURRENT_KIND_KEY) of
        undefined -> default_kind();
        Kind -> Kind
    end.

%% @doc Execute a function under a specific continuation kind.
%%
%% The previous process-local kind is restored even if the function exits.
-spec with_kind(continuation_kind(), fun(() -> Result)) -> Result when
    Result :: term().
with_kind(Kind, Fun) when is_function(Fun, 0) ->
    Previous = get(?CURRENT_KIND_KEY),
    put(?CURRENT_KIND_KEY, Kind),
    try
        Fun()
    after
        restore_previous_kind(Previous)
    end.

%%%---------------------------------------------------------------------
%%% Type Predicates
%%%---------------------------------------------------------------------

%% @doc Check if the current continuation kind is one-shot.
-spec is_one_shot() -> boolean().
is_one_shot() ->
    is_one_shot(current_kind()).

%% @doc Check if a continuation kind is one-shot.
-spec is_one_shot(continuation_kind()) -> boolean().
is_one_shot(one_shot) -> true;
is_one_shot(_) -> false.

%% @doc Check if the current continuation kind is multi-shot.
-spec is_multi_shot() -> boolean().
is_multi_shot() ->
    is_multi_shot(current_kind()).

%% @doc Check if a continuation kind is multi-shot.
-spec is_multi_shot(continuation_kind()) -> boolean().
is_multi_shot(multi_shot) -> true;
is_multi_shot(_) -> false.

%%%---------------------------------------------------------------------
%%% Type Validation
%%%---------------------------------------------------------------------

%% @doc Validate a term as a continuation kind.
-spec is_valid_kind(term()) -> boolean().
is_valid_kind(one_shot) -> true;
is_valid_kind(multi_shot) -> true;
is_valid_kind(_) -> false.

%%%---------------------------------------------------------------------
%%% Conversion
%%%---------------------------------------------------------------------

%% @doc Convert continuation kind to an atom.
-spec to_atom(continuation_kind()) -> atom().
to_atom(one_shot) -> one_shot;
to_atom(multi_shot) -> multi_shot.

%% @doc Create continuation kind from an atom.
-spec from_atom(atom()) -> {ok, continuation_kind()} | {error, term()}.
from_atom(one_shot) -> {ok, one_shot};
from_atom(multi_shot) -> {ok, multi_shot};
from_atom(Other) -> {error, {invalid_kind, Other}}.

%%%---------------------------------------------------------------------
%%% Semantics Documentation
%%%---------------------------------------------------------------------

%% @doc Get operational semantics for a continuation kind.
-spec semantics(continuation_kind()) -> map().
semantics(one_shot) ->
    #{
        resumption => linear,
        consumption => single,
        state => original,
        cleanup => immediate,
        guarantee => exactly_once
    };
semantics(multi_shot) ->
    #{
        resumption => branching,
        consumption => none,
        state => copied,
        cleanup => reference_counted,
        guarantee => zero_or_more
    }.

%% @doc Get use cases for a continuation kind.
-spec use_cases(continuation_kind()) -> [binary()].
use_cases(one_shot) ->
    [
        <<"Resource management (files, sockets)">>,
        <<"Exception handling and early exit">>,
        <<"Generator consumption">>,
        <<"Stream processing">>,
        <<"Linear state transitions">>
    ];
use_cases(multi_shot) ->
    [
        <<"Backtracking search">>,
        <<"Nondeterministic choice">>,
        <<"Solution enumeration">>,
        <<"Ambivalent operators">>,
        <<"Constraint solving">>,
        <<"Coroutines and iterators">>
    ].

%%%---------------------------------------------------------------------
%%% Algebraic Laws Callbacks
%%%---------------------------------------------------------------------

%% @doc Check associativity of continuation kind composition.
%% For continuation kinds:
%% - one_shot ⊕ one_shot = one_shot (sequential one-shot operations)
%% - multi_shot ⊕ anything = multi_shot (multi-shot dominates)
%% - one_shot ⊕ multi_shot = multi_shot
-spec associative(continuation_kind(), continuation_kind()) -> boolean().
associative(one_shot, one_shot) -> true;
associative(multi_shot, _) -> true;
associative(_, multi_shot) -> true;
associative(_, _) -> false.

%% @doc Check commutativity of continuation kind composition.
%% Composition is not commutative: one_shot ⊕ multi_shot ≠ multi_shot ⊕ one_shot
-spec commutative(continuation_kind(), continuation_kind()) -> boolean().
commutative(one_shot, one_shot) -> true;
commutative(multi_shot, multi_shot) -> true;
commutative(_, _) -> false.

%% @doc Check if a value is the identity element.
%% one_shot acts as an identity for continuation kind composition.
-spec identity(continuation_kind(), continuation_kind()) -> boolean().
identity(one_shot, one_shot) -> true;
identity(_, _) -> false.

%% @doc Check idempotence of continuation kind operations.
%% multi_shot is idempotent: multi_shot ⊕ multi_shot = multi_shot
-spec idempotent(continuation_kind()) -> boolean().
idempotent(multi_shot) -> true;
idempotent(_) -> false.

%%%---------------------------------------------------------------------
%%% Internal helpers
%%%---------------------------------------------------------------------

%% @doc Compose two continuation kinds.
%% Multi-shot dominates any composition, and one_shot acts as identity.
-spec compose(continuation_kind(), continuation_kind()) -> continuation_kind().
compose(one_shot, one_shot) -> one_shot;
compose(_, multi_shot) -> multi_shot;
compose(multi_shot, _) -> multi_shot.

%% @private Restore the prior continuation kind after a scoped override.
-spec restore_previous_kind(continuation_kind() | undefined) -> ok.
restore_previous_kind(undefined) ->
    erase(?CURRENT_KIND_KEY),
    ok;
restore_previous_kind(Kind) ->
    put(?CURRENT_KIND_KEY, Kind),
    ok.
