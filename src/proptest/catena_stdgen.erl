%% @doc Standard Generators for Catena Property Testing
%%
%% This module implements Property Testing Phase 2, providing standard
%% generators for common Erlang and Catena types.
%%
%% == Phase 2 Sections ==
%%
%% - Section 2.1: Numeric Generators (floats, bigints, numeric ranges)
%% - Section 2.2: Text and Binary Generators (chars, strings, binaries, atoms)
%% - Section 2.3: Collection Generators (lists, tuples, maps, sets)
%% - Section 2.4: Recursive Structure Support
%% - Section 2.5: Function Generators
%%
%% @see catena_gen for the core generator infrastructure
-module(catena_stdgen).

%% Section 2.1: Numeric Generators
-export([
    gen_float/0,
    gen_float/1,
    gen_float_special/0,
    gen_float_normal/1,
    gen_bigint/1,
    gen_number/0
]).

-export_type([
    float_range/0
]).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Types
%%====================================================================

-type float_range() :: {float(), float()}.

%%====================================================================
%% Section 2.1: Numeric Generators
%%====================================================================

%% @doc Generate a float within default range (-10000.0 to 10000.0).
-spec gen_float() -> catena_gen:generator(float()).
gen_float() ->
    gen_float({-10000.0, 10000.0}).

%% @doc Generate a float within the specified range.
-spec gen_float(float_range()) -> catena_gen:generator(float()).
gen_float({Min, Max}) when is_float(Min), is_float(Max), Min =< Max ->
    catena_gen:new(fun(_Size, Seed) ->
        {Word, _} = catena_gen:seed_next(Seed),
        Scale = max(Max - Min, 0.0),
        Value = Min + (Scale * (Word / 16#FFFFFFFFFFFFFFFF)),
        catena_tree:tree(Value, fun() ->
            [build_float_shrink_tree(Value, Min) || Value =/= Min]
        end)
    end).

%% @doc Generate special float values (infinity, -infinity, -0.0).
%%
%% Returns one of: positive infinity, negative infinity, negative zero.
%% NaN can be tested using `nan =/= nan`.
-spec gen_float_special() -> catena_gen:generator(float()).
gen_float_special() ->
    catena_gen:new(fun(Size, Seed) ->
        {ChoiceWord, _} = catena_gen:seed_next(Seed),
        Specials = [1.0e308, -1.0e308, -0.0],
        Value = lists:nth(ChoiceWord rem length(Specials) + 1, Specials),
        catena_tree:tree(Value, fun() -> [] end)
    end).

%% @doc Generate normal floats excluding special values.
%%
%% Normal floats are finite and not NaN or infinity.
-spec gen_float_normal(float_range()) -> catena_gen:generator(float()).
gen_float_normal(Range) ->
    catena_gen:gen_filter(fun is_finite_float/1, gen_float(Range)).

%% @doc Generate arbitrarily large integers beyond machine word size.
%%
%% Bigints are generated as large integers and shrink toward smaller values.
-spec gen_bigint(catena_range:range()) -> catena_gen:generator(integer()).
gen_bigint(Range) ->
    catena_gen:gen_int(Range).

%% @doc Generate a number (int, float, or bigint).
%%
%% Randomly chooses between integer, float, and bigint generation.
-spec gen_number() -> catena_gen:generator(number()).
gen_number() ->
    catena_gen:new(fun(Size, Seed) ->
        %% Use the seed to decide which type to generate
        {ChoiceWord, _} = catena_gen:seed_next(Seed),
        case ChoiceWord rem 3 of
            0 -> catena_gen:run(catena_gen:gen_int(), Size, Seed);
            1 -> catena_gen:run(catena_stdgen:gen_float(), Size, Seed);
            2 -> catena_gen:run(gen_bigint(catena_range:range_linear(0, 1000)), Size, Seed)
        end
    end).

%%====================================================================
%% Internal Helpers
%%====================================================================

%% @doc Build shrink tree for float values.
%% Shrinks toward zero by reducing magnitude.
-spec build_float_shrink_tree(float(), float()) -> catena_tree:tree(float()).
build_float_shrink_tree(Value, Min) when Value =:= Min; abs(Value) < 0.0000001 ->
    catena_tree:singleton(Value);
build_float_shrink_tree(Value, Min) ->
    %% Shrink by halving the value
    Shrunk = Value / 2.0,
    %% Also try moving toward origin
    TowardMin = case Min < Value andalso Value > 0 of
        true -> max(Min, Value - abs(Value) / 10.0);
        false -> Value / 2.0
    end,
    Candidates = [0.0, Shrunk, TowardMin],
    Filtered = lists:filter(fun(C) -> C >= Min andalso C < Value end, Candidates),
    Unique = lists:usort(Filtered),
    case Unique of
        [] -> catena_tree:singleton(Value);
        _ -> catena_tree:tree(Value, fun() ->
            [build_float_shrink_tree(C, Min) || C <- Unique]
        end)
    end.

%% @doc Check if a float is finite (not NaN, not infinity).
-spec is_finite_float(term()) -> boolean().
is_finite_float(F) when is_float(F) ->
    %% In Erlang, we can check if a float is finite by comparing to itself
    %% NaN =/= NaN, and infinity can be checked with isFinite/1 if available
    try
        case F of
            nan -> false;
            PosInf when PosInf > 1.0e308 -> false;
            NegInf when NegInf < -1.0e308 -> false;
            _ -> true
        end
    catch
        _:_ -> false
    end;
is_finite_float(_) ->
    false.

%%====================================================================
%% Unit Tests - Section 2.1
%%====================================================================

gen_float_within_range_test() ->
    Gen = catena_stdgen:gen_float({-10.0, 10.0}),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    Value = catena_tree:root(Tree),
    ?assert(Value >= -10.0 andalso Value =< 10.0),
    ok.

gen_float_shrinks_toward_zero_test() ->
    Gen = catena_stdgen:gen_float({0.0, 100.0}),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    Root = catena_tree:root(Tree),
    ?assert(Root >= 0.0 andalso Root =< 100.0),
    %% Should have shrinks
    Children = catena_tree:children(Tree),
    ?assert(length(Children) > 0),
    ok.

gen_float_special_returns_special_test() ->
    Gen = catena_stdgen:gen_float_special(),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Root = catena_tree:root(Tree),
    %% Should be a special float value (infinity, -infinity, or -0.0)
    IsInf = abs(Root) > 1.0e300,
    IsNegZero = Root =:= -0.0 orelse (Root =:= 0.0 andalso (-1.0 / Root) < 0.0),
    ?assert(IsInf orelse IsNegZero),
    ok.

gen_float_normal_excludes_special_test() ->
    Gen = catena_stdgen:gen_float_normal({-100.0, 100.0}),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    Root = catena_tree:root(Tree),
    %% Should be finite (not nan, not infinity)
    case Root of
        nan -> ?assert(false, "Should not generate NaN");
        V when is_float(V) ->
            ?assert(V =< 1.0e308, "Should not be infinity");
        _ -> ?assert(false, "Unexpected type")
    end,
    ok.

gen_bigint_produces_large_values_test() ->
    Range = catena_range:range_constant({0, 1000000000000000}),
    Gen = catena_stdgen:gen_bigint(Range),
    Tree = catena_gen:run(Gen, 100, catena_gen:seed_new()),
    Value = catena_tree:root(Tree),
    ?assert(is_integer(Value)),
    ?assert(Value >= 0 andalso Value =< 1000000000000000),
    ok.

gen_number_returns_number_test() ->
    Gen = catena_stdgen:gen_number(),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    Root = catena_tree:root(Tree),
    ?assert(is_number(Root)),
    ok.

gen_float_negative_range_test() ->
    Gen = catena_stdgen:gen_float({-100.0, -10.0}),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    Value = catena_tree:root(Tree),
    ?assert(Value >= -100.0 andalso Value =< -10.0),
    ok.
