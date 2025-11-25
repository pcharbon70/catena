%% @doc Catena Property Test Generators (Phase 2.3)
%%
%% This module provides generators for property-based testing. Generators
%% create random test data for verifying properties hold across many inputs.
%%
%% Supported generators:
%%   - Bool: random true/false
%%   - Natural: random integers 0-100
%%   - Int: random integers -100 to 100
%%   - Text: random strings 0-20 characters
%%   - List: random lists with generated elements
%%
%% Example usage in Catena:
%%   property "addition commutes" = forall x : Natural, y : Natural. x + y == y + x
%%
%% The generator names match Catena type names for intuitive syntax.
%%
-module(catena_generators).

-export([
    generate/1,
    generate/2,
    gen_bool/0,
    gen_natural/0,
    gen_natural/1,
    gen_int/0,
    gen_int/1,
    gen_text/0,
    gen_text/1,
    gen_list/1,
    gen_list/2,
    gen_maybe/1,
    gen_result/2,
    shrink/2
]).

%% Default size parameters
-define(DEFAULT_MAX_NATURAL, 100).
-define(DEFAULT_MAX_INT, 100).
-define(DEFAULT_MAX_TEXT_LEN, 20).
-define(DEFAULT_MAX_LIST_LEN, 20).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Generate a random value for a given generator name
%% Generator names correspond to Catena type names for natural syntax.
-spec generate(atom()) -> term().
generate('Bool') ->
    gen_bool();
generate('Natural') ->
    gen_natural();
generate('Int') ->
    gen_int();
generate('Text') ->
    gen_text();
generate('String') ->
    gen_text();  % Alias for Text
generate('List') ->
    gen_list(fun gen_int/0);  % Default to list of integers
generate('Maybe') ->
    gen_maybe(fun gen_int/0);  % Default to Maybe Int
generate('Result') ->
    gen_result(fun gen_int/0, fun gen_text/0);  % Default to Result Int Text
generate(Unknown) ->
    throw({unknown_generator, Unknown}).

%% @doc Generate a value with custom size parameter
-spec generate(atom(), pos_integer()) -> term().
generate('Bool', _Size) ->
    gen_bool();
generate('Natural', Size) ->
    gen_natural(Size);
generate('Int', Size) ->
    gen_int(Size);
generate('Text', Size) ->
    gen_text(Size);
generate('String', Size) ->
    gen_text(Size);
generate('List', Size) ->
    gen_list(fun gen_int/0, Size);
generate(Unknown, _Size) ->
    throw({unknown_generator, Unknown}).

%%====================================================================
%% Individual Generators
%%====================================================================

%% @doc Generate a random boolean
%% Returns true or false with equal probability.
-spec gen_bool() -> boolean().
gen_bool() ->
    rand:uniform() < 0.5.

%% @doc Generate a random natural number (0 to 100)
%% Natural numbers in Catena are non-negative integers.
-spec gen_natural() -> non_neg_integer().
gen_natural() ->
    gen_natural(?DEFAULT_MAX_NATURAL).

%% @doc Generate a random natural number up to Max
-spec gen_natural(non_neg_integer()) -> non_neg_integer().
gen_natural(Max) when Max >= 0 ->
    rand:uniform(Max + 1) - 1.

%% @doc Generate a random integer (-100 to 100)
-spec gen_int() -> integer().
gen_int() ->
    gen_int(?DEFAULT_MAX_INT).

%% @doc Generate a random integer in range -Max to Max
-spec gen_int(non_neg_integer()) -> integer().
gen_int(Max) when Max >= 0 ->
    rand:uniform(2 * Max + 1) - Max - 1.

%% @doc Generate a random text string (0 to 20 chars)
%% Generates lowercase ASCII letters for simplicity.
-spec gen_text() -> string().
gen_text() ->
    gen_text(?DEFAULT_MAX_TEXT_LEN).

%% @doc Generate a random text string up to MaxLen characters
-spec gen_text(non_neg_integer()) -> string().
gen_text(MaxLen) when MaxLen >= 0 ->
    Len = rand:uniform(MaxLen + 1) - 1,
    [rand:uniform(26) + 96 || _ <- lists:seq(1, Len)].

%% @doc Generate a random list using element generator
%% Default max length is 20.
-spec gen_list(fun(() -> term())) -> list().
gen_list(ElemGen) ->
    gen_list(ElemGen, ?DEFAULT_MAX_LIST_LEN).

%% @doc Generate a random list with specified max length
-spec gen_list(fun(() -> term()), non_neg_integer()) -> list().
gen_list(ElemGen, MaxLen) when MaxLen >= 0 ->
    Len = rand:uniform(MaxLen + 1) - 1,
    [ElemGen() || _ <- lists:seq(1, Len)].

%% @doc Generate a random Maybe value
%% Returns either {some, Value} or none with roughly equal probability.
-spec gen_maybe(fun(() -> term())) -> {some, term()} | none.
gen_maybe(ValueGen) ->
    case gen_bool() of
        true -> {some, ValueGen()};
        false -> none
    end.

%% @doc Generate a random Result value
%% Returns either {ok, Value} or {err, Error} with roughly equal probability.
-spec gen_result(fun(() -> term()), fun(() -> term())) -> {ok, term()} | {err, term()}.
gen_result(OkGen, ErrGen) ->
    case gen_bool() of
        true -> {ok, OkGen()};
        false -> {err, ErrGen()}
    end.

%%====================================================================
%% Shrinking (Basic Implementation)
%%====================================================================

%% @doc Shrink a value toward simpler cases
%% Shrinking helps find minimal counterexamples when a property fails.
%% This is a basic implementation; full shrinking is deferred to Phase 6.
-spec shrink(atom(), term()) -> [term()].
shrink('Bool', true) ->
    [false];
shrink('Bool', false) ->
    [];
shrink('Natural', N) when N > 0 ->
    [0, N div 2, N - 1];
shrink('Natural', 0) ->
    [];
shrink('Int', N) when N > 0 ->
    [0, N div 2, N - 1, -N];
shrink('Int', N) when N < 0 ->
    [0, N div 2, N + 1, -N];
shrink('Int', 0) ->
    [];
shrink('Text', S) when length(S) > 0 ->
    [[], tl(S), lists:droplast(S)];
shrink('Text', []) ->
    [];
shrink('List', L) when length(L) > 0 ->
    [[], tl(L), lists:droplast(L)];
shrink('List', []) ->
    [];
shrink('Maybe', {some, _}) ->
    [none];
shrink('Maybe', none) ->
    [];
shrink('Result', {ok, _}) ->
    [];
shrink('Result', {err, _}) ->
    [];
shrink(_, _) ->
    [].
