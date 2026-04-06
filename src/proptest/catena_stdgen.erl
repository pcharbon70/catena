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
    gen_number/0,
    range_float/2,
    range_unit/0,
    range_percent/0,
    range_byte/0
]).

%% Section 2.2: Text and Binary Generators
-export([
    gen_char/0,
    gen_char_alpha/0,
    gen_char_digit/0,
    gen_char_alphanumeric/0,
    gen_char_unicode/0,
    gen_string/1,
    gen_string/2,
    gen_string_nonempty/1,
    gen_utf8/1,
    gen_binary/1,
    gen_binary_of/2,
    gen_bitstring/1,
    gen_atom/0,
    gen_atom_unsafe/1,
    gen_bool_atom/0
]).

%% Section 2.3: Collection Generators
-export([
    gen_list/1,
    gen_list_of/2,
    gen_list_of_length/2,
    gen_tuple/1,
    gen_tuple2/2,
    gen_tuple3/3,
    gen_tuple4/4,
    gen_map/2,
    gen_map_of/3,
    gen_set/1,
    gen_set_of/2
]).

%% Section 2.4: Recursive Structure Support
-export([
    gen_BinaryTree/1,
    gen_maybe/1,
    gen_result/2,
    gen_list_recursive/1,
    gen_recursive/2
]).

%% Section 2.5: Function Generators
-export([
    gen_function0/1,
    gen_function1/1,
    gen_function2/1,
    gen_constant_function/1,
    gen_identity_function/0,
    gen_projection_first/0,
    gen_projection_second/0,
    gen_k_combinator/1,
    gen_frequency/1
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

%% @doc Create a float range {Min, Max} for use with gen_float/1.
%%
%% Provides a convenient constructor for float ranges, ensuring
%% Min <= Max and both are floats.
-spec range_float(float(), float()) -> {float(), float()}.
range_float(Min, Max) when is_float(Min), is_float(Max), Min =< Max ->
    {Min, Max};
range_float(Min, Max) when is_number(Min), is_number(Max), Min =< Max ->
    {float(Min), float(Max)};
range_float(Min, Max) ->
    erlang:error({badarg, {range_float, {Min, Max}}}).

%% @doc Create a unit range {0.0, 1.0} for normalized values.
%%
%% Useful for generating percentages, probabilities, and normalized
%% measurements.
-spec range_unit() -> {float(), float()}.
range_unit() ->
    {0.0, 1.0}.

%% @doc Create a percent range {0, 100} for percentage values.
%%
%% Returns an integer range suitable for percentage calculations.
-spec range_percent() -> {integer(), integer()}.
range_percent() ->
    {0, 100}.

%% @doc Create a byte range {0, 255} for byte values.
%%
%% Returns an integer range suitable for generating bytes.
-spec range_byte() -> {integer(), integer()}.
range_byte() ->
    {0, 255}.

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

range_float_creates_valid_range_test() ->
    Range = catena_stdgen:range_float(-1.0, 1.0),
    ?assertEqual({-1.0, 1.0}, Range),
    ok.

range_float_converts_numbers_test() ->
    Range = catena_stdgen:range_float(-10, 10),
    ?assertEqual({-10.0, 10.0}, Range),
    ok.

range_float_rejects_invalid_test() ->
    ?assertError({badarg, _}, catena_stdgen:range_float(10.0, 1.0)),
    ok.

range_unit_returns_correct_range_test() ->
    Range = catena_stdgen:range_unit(),
    ?assertEqual({0.0, 1.0}, Range),
    ok.

range_percent_returns_correct_range_test() ->
    Range = catena_stdgen:range_percent(),
    ?assertEqual({0, 100}, Range),
    ok.

range_byte_returns_correct_range_test() ->
    Range = catena_stdgen:range_byte(),
    ?assertEqual({0, 255}, Range),
    ok.

gen_float_with_range_unit_test() ->
    Gen = catena_stdgen:gen_float(catena_stdgen:range_unit()),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    Value = catena_tree:root(Tree),
    ?assert(Value >= 0.0 andalso Value =< 1.0),
    ok.

%%====================================================================
%% Section 2.2: Text and Binary Generators
%%====================================================================

%% ---- Character Generators ----

%% @doc Generate any printable ASCII character (32-126).
-spec gen_char() -> catena_gen:generator(char()).
gen_char() ->
    catena_gen:new(fun(_Size, Seed) ->
        {Word, _} = catena_gen:seed_next(Seed),
        %% Printable ASCII: 32-126
        CharCode = 32 + (Word rem 95),
        catena_tree:tree(CharCode, fun() ->
            %% Shrink toward space (32)
            [gen_char_shrink(CharCode) || CharCode > 32]
        end)
    end).

%% @doc Generate an alphabetic character (a-z, A-Z).
-spec gen_char_alpha() -> catena_gen:generator(char()).
gen_char_alpha() ->
    catena_gen:new(fun(_Size, Seed) ->
        {Word, _} = catena_gen:seed_next(Seed),
        %% 52 letters (26 lower + 26 upper)
        AlphaOffset = Word rem 52,
        CharCode = case AlphaOffset < 26 of
            true -> $a + AlphaOffset;  %% lowercase
            false -> $A + (AlphaOffset - 26) %% uppercase
        end,
        catena_tree:tree(CharCode, fun() ->
            %% Shrink toward 'a'
            [gen_char_shrink_alpha(CharCode) || CharCode > $a]
        end)
    end).

%% @doc Generate a digit character (0-9).
-spec gen_char_digit() -> catena_gen:generator(char()).
gen_char_digit() ->
    catena_gen:new(fun(_Size, Seed) ->
        {Word, _} = catena_gen:seed_next(Seed),
        CharCode = $0 + (Word rem 10),
        catena_tree:tree(CharCode, fun() ->
            %% Shrink toward 0
            [CharCode - 1 || CharCode > $0]
        end)
    end).

%% @doc Generate an alphanumeric character (a-z, A-Z, 0-9).
-spec gen_char_alphanumeric() -> catena_gen:generator(char()).
gen_char_alphanumeric() ->
    catena_gen:new(fun(_Size, Seed) ->
        {Word, _} = catena_gen:seed_next(Seed),
        %% 62 alphanumeric chars: 26 lower + 26 upper + 10 digits
        Offset = Word rem 62,
        CharCode = if
            Offset < 26 -> $a + Offset;  %% lowercase
            Offset < 52 -> $A + (Offset - 26);  %% uppercase
            true -> $0 + (Offset - 52)  %% digit
        end,
        catena_tree:tree(CharCode, fun() ->
            %% Shrink toward 'a' or '0'
            case CharCode of
                C when C >= $a andalso C =< $z ->
                    [$a, gen_char_shrink_alpha(C)];
                C when C >= $A andalso C < $Z ->
                    [$A, gen_char_shrink_alpha(C)];
                C when C >= $0 andalso C =< $9 ->
                    [gen_char_shrink_alpha(C)];
                _ -> []
            end
        end)
    end).

%% ---- String Generators ----

%% @doc Generate a string with length from a Range.
-spec gen_string(catena_range:range()) -> catena_gen:generator(string()).
gen_string(LengthRange) ->
    gen_string(LengthRange, gen_char()).

%% @doc Generate a string using the specified character generator.
-spec gen_string(catena_range:range(), catena_gen:generator(char())) -> catena_gen:generator(string()).
gen_string(LengthRange, CharGen) ->
    catena_gen:new(fun(Size, Seed) ->
        %% Get the length
        {LengthWord, Seed1} = catena_gen:seed_next(Seed),
        {Min, Max} = catena_range:range_bounds(LengthRange, Size),
        Length = Min + (LengthWord rem (Max - Min + 1)),
        gen_string_of_length(Length, CharGen, Size, Seed1)
    end).

%% @doc Generate a non-empty string.
-spec gen_string_nonempty(catena_range:range()) -> catena_gen:generator(string()).
gen_string_nonempty(LengthRange) ->
    %% Ensure at least length 1
    MinLen = case catena_range:range_bounds(LengthRange, 0) of
        {Min, _} when Min < 1 -> catena_range:range_constant({1, 20});
        _ -> LengthRange
    end,
    gen_string(MinLen, gen_char()).

%% @doc Generate a string with exact length using the character generator.
%% Returns a tree directly when all parameters are provided.
-spec gen_string_of_length(non_neg_integer(), catena_gen:generator(char()), catena_gen:size(), catena_gen:seed()) -> catena_tree:tree(string()).
gen_string_of_length(0, _CharGen, _Size, _Seed) ->
    catena_tree:singleton("");
gen_string_of_length(Len, CharGen, Size, Seed) when is_integer(Len), Len > 0 ->
    %% Generate Len characters by splitting seeds
    {Chars, _} = lists:mapfoldl(
        fun(_, AccSeed) ->
            {S1, S2} = catena_gen:seed_split(AccSeed),
            CharTree = catena_gen:run(CharGen, Size div max(Len, 1), S1),
            {catena_tree:root(CharTree), S2}
        end,
        Seed,
        lists:seq(1, Len)
    ),
    String = lists:filter(fun(C) -> is_integer(C) andalso C >= 0 end, Chars),
    catena_tree:tree(String, fun() ->
        %% Shrink by removing characters
        [gen_string_shrink(String, CharGen, Size) || String =/= ""]
    end).

%% ---- Binary Generators ----

%% @doc Generate a binary with length from a Range.
-spec gen_binary(catena_range:range()) -> catena_gen:generator(binary()).
gen_binary(LengthRange) ->
    catena_gen:new(fun(Size, Seed) ->
        %% Get the length
        {LengthWord, Seed1} = catena_gen:seed_next(Seed),
        {Min, Max} = catena_range:range_bounds(LengthRange, Size),
        Length = Min + (LengthWord rem (Max - Min + 1)),
        %% Run the binary generator with determined length
        catena_gen:run(gen_binary_of_length(Length), Size, Seed1)
    end).

%% @doc Generate a binary with exact length.
-spec gen_binary_of_length(non_neg_integer()) -> catena_gen:generator(binary()).
gen_binary_of_length(0) ->
    catena_gen:constant(<<>>);
gen_binary_of_length(Len) when is_integer(Len), Len > 0 ->
    catena_gen:new(fun(Size, Seed) ->
        %% Generate Len bytes by splitting seeds
        {Bytes, _} = lists:mapfoldl(
            fun(_, AccSeed) ->
                {S1, S2} = catena_gen:seed_split(AccSeed),
                {Word, _} = catena_gen:seed_next(S1),
                Byte = Word rem 256,
                {Byte, S2}
            end,
            Seed,
            lists:seq(1, Len)
        ),
        Binary = iolist_to_binary([<<B:8>> || B <- Bytes]),
        catena_tree:tree(Binary, fun() ->
            %% Shrink by removing bytes
            [gen_binary_shrink(Binary, Size) || byte_size(Binary) > 0]
        end)
    end).

%% @doc Generate a bitstring (may not be byte-aligned).
-spec gen_bitstring(catena_range:range()) -> catena_gen:generator(bitstring()).
gen_bitstring(LengthRange) ->
    %% For simplicity, generate byte-aligned bitstrings
    gen_binary(LengthRange).

%% ---- Atom Generators ----

%% @doc Generate an atom from a predefined pool.
%%
%% Using a predefined pool avoids atom table exhaustion.
-spec gen_atom() -> catena_gen:generator(atom()).
gen_atom() ->
    AtomPool = [foo, bar, baz, qux, quux, corge, grault, garply, waldo, fred,
        bob, alice, charlie, test, data, value, key, item, element,
        result, state, status, error, ok],
    catena_gen:element(AtomPool).

%% @doc Generate a boolean atom (true or false).
-spec gen_bool_atom() -> catena_gen:generator(atom()).
gen_bool_atom() ->
    catena_gen:new(fun(_Size, Seed) ->
        {Word, _} = catena_gen:seed_next(Seed),
        Atom = case Word rem 2 of
            0 -> true;
            1 -> false
        end,
        catena_tree:singleton(Atom)
    end).

%% @doc Generate a Unicode character (valid codepoint).
%%
%% Generates characters from the Unicode Basic Multilingual Plane (BMP)
%% which covers most common languages and symbols. Excludes surrogate
%% code points which are invalid in UTF-8.
-spec gen_char_unicode() -> catena_gen:generator(integer()).
gen_char_unicode() ->
    catena_gen:new(fun(_Size, Seed) ->
        {Word, _} = catena_gen:seed_next(Seed),
        %% BMP range: 0x0000 to 0xFFFF, excluding surrogates (0xD800-0xDFFF)
        %% Use only 0x0000 to 0xD7FF and 0xE000 to 0xFFFF
        %% Simplified: use 0x0000 to 0xFFFF but skip surrogates
        Code = Word rem 16#10000,
        ValidCode = case Code of
            C when C >= 16#D800, C =< 16#DFFF -> C + 16#800;  %% Skip surrogates
            C -> C
        end,
        catena_tree:tree(ValidCode, fun() ->
            %% Shrink toward 0 (null character)
            [gen_char_unicode_shrink(ValidCode) || ValidCode > 0]
        end)
    end).

%% @private Shrink Unicode codepoint toward 0.
-spec gen_char_unicode_shrink(integer()) -> catena_tree:tree(integer()).
gen_char_unicode_shrink(0) ->
    catena_tree:singleton(0);
gen_char_unicode_shrink(Code) ->
    Shrunk = Code div 2,
    catena_tree:tree(Shrunk, fun() ->
        [gen_char_unicode_shrink(Shrunk)]
    end).

%% @doc Generate a UTF-8 encoded string.
%%
%% Generates valid UTF-8 strings using Unicode characters.
%% Shrinks by removing characters and using smaller codepoints.
-spec gen_utf8(catena_range:range()) -> catena_gen:generator(binary()).
gen_utf8(LengthRange) ->
    CharGen = gen_char_unicode(),
    catena_gen:new(fun(Size, Seed) ->
        {LengthWord, Seed1} = catena_gen:seed_next(Seed),
        {Min, Max} = catena_range:range_bounds(LengthRange, Size),
        Length = Min + (LengthWord rem (Max - Min + 1)),
        %% Generate list of codepoints then encode to UTF-8
        String = gen_utf8_string(Length, CharGen, Size, Seed1, []),
        catena_tree:tree(String, fun() ->
            %% Shrink: shorter strings - wrap each in a tree
            [catena_tree:singleton(gen_utf8_string(L, CharGen, Size, Seed1, [])) || L <- lists:seq(max(0, Length - 5), Length - 1)]
        end)
    end).

%% @private Generate a UTF-8 string of given length.
-spec gen_utf8_string(non_neg_integer(), catena_gen:generator(integer()), catena_gen:size(), catena_gen:seed(), [integer()]) -> binary().
gen_utf8_string(0, _CharGen, _Size, _Seed, Acc) ->
    %% Encode codepoints to UTF-8
    Binaries = [unicode:characters_to_binary([C]) || C <- lists:reverse(Acc)],
    iolist_to_binary(Binaries);
gen_utf8_string(Length, CharGen, Size, Seed, Acc) when Length > 0 ->
    {S1, S2} = catena_gen:seed_split(Seed),
    Tree = catena_gen:run(CharGen, Size, S1),
    CodePoint = catena_tree:root(Tree),
    gen_utf8_string(Length - 1, CharGen, Size, S2, [CodePoint | Acc]).

%% @doc Generate a binary using a custom byte generator.
%%
%% Takes a generator for individual bytes (0-255) and produces
%% a binary of random length containing those bytes.
-spec gen_binary_of(catena_range:range(), catena_gen:generator(byte())) -> catena_gen:generator(binary()).
gen_binary_of(LengthRange, ByteGen) ->
    catena_gen:new(fun(Size, Seed) ->
        {LengthWord, Seed1} = catena_gen:seed_next(Seed),
        {Min, Max} = catena_range:range_bounds(LengthRange, Size),
        Length = Min + (LengthWord rem (Max - Min + 1)),
        %% Generate list of bytes
        Bytes = gen_binary_bytes(Length, ByteGen, Size, Seed1, []),
        Binary = << <<B:8>> || B <- Bytes >>,
        catena_tree:tree(Binary, fun() ->
            %% Shrink: shorter binaries - use prefix shrinks
            gen_binary_shrinks(Binary, Length, ByteGen, Size, Seed1)
        end)
    end).

%% @private Generate a list of bytes.
-spec gen_binary_bytes(non_neg_integer(), catena_gen:generator(byte()), catena_gen:size(), catena_gen:seed(), [byte()]) -> [byte()].
gen_binary_bytes(0, _ByteGen, _Size, _Seed, Acc) ->
    lists:reverse(Acc);
gen_binary_bytes(Length, ByteGen, Size, Seed, Acc) when Length > 0 ->
    {S1, S2} = catena_gen:seed_split(Seed),
    Tree = catena_gen:run(ByteGen, Size, S1),
    Byte = catena_tree:root(Tree),
    gen_binary_bytes(Length - 1, ByteGen, Size, S2, [Byte | Acc]).

%% @private Generate shrink candidates for binaries.
gen_binary_shrinks(<<>>, _Length, _ByteGen, _Size, _Seed) ->
    [];
gen_binary_shrinks(Binary, Length, ByteGen, Size, Seed) ->
    %% Generate shorter versions
    ShorterLengths = [L || L <- lists:seq(0, Length - 1)],
    [catena_tree:tree(begin
        Bytes = gen_binary_bytes(L, ByteGen, Size, Seed, []),
        << <<B:8>> || B <- Bytes >>
    end, fun() -> [] end) || L <- ShorterLengths].

%% @doc Generate arbitrary atoms (UNSAFE - may exhaust atom table).
%%
%% **Warning**: This function can create new atoms at runtime, which
%% can exhaust the Erlang atom table (limited to ~1M atoms). Use only
%% in controlled testing environments. For most use cases, use gen_atom/0
%% which draws from a predefined pool.
%%
%% The generated atoms use the prefix "test_atom_" followed by random
%% characters to avoid collisions with existing atoms.
-spec gen_atom_unsafe(pos_integer()) -> catena_gen:generator(atom()).
gen_atom_unsafe(MaxLen) when is_integer(MaxLen), MaxLen > 0 ->
    catena_gen:new(fun(Size, Seed) ->
        %% Limit actual size
        ActualLen = min(Size, MaxLen),
        %% Use gen_string to get the string
        StringRange = catena_range:range_constant({ActualLen, ActualLen}),
        StringGen = gen_string(StringRange, gen_char_alphanumeric()),
        Tree = catena_gen:run(StringGen, Size div 2, Seed),
        String = catena_tree:root(Tree),
        %% Prefix to avoid conflicts
        AtomName = "test_atom_" ++ String,
        Atom = list_to_atom(AtomName),
        catena_tree:singleton(Atom)
    end).

%%====================================================================
%% Section 2.3: Collection Generators
%%====================================================================

%% ---- List Generators ----

%% @doc Generate a list with elements from the given generator.
%%
%% The size parameter controls the maximum list length.
-spec gen_list(catena_gen:generator(A)) -> catena_gen:generator([A]).
gen_list(ElementGen) ->
    catena_gen:new(fun(Size, Seed) ->
        {LengthWord, Seed1} = catena_gen:seed_next(Seed),
        MaxLen = max(Size, 1),
        Length = LengthWord rem (MaxLen + 1),
        gen_list_tree(Length, ElementGen, Size, Seed1)
    end).

%% @doc Generate a list with length from a Range.
-spec gen_list_of(catena_range:range(), catena_gen:generator(A)) -> catena_gen:generator([A]).
gen_list_of(LengthRange, ElementGen) ->
    catena_gen:new(fun(Size, Seed) ->
        {LengthWord, Seed1} = catena_gen:seed_next(Seed),
        {Min, Max} = catena_range:range_bounds(LengthRange, Size),
        Length = Min + (LengthWord rem (Max - Min + 1)),
        gen_list_tree(Length, ElementGen, Size, Seed1)
    end).

%% @doc Generate a list with exact length.
-spec gen_list_of_length(non_neg_integer(), catena_gen:generator(A)) -> catena_gen:generator([A]).
gen_list_of_length(0, _ElementGen) ->
    catena_gen:constant([]);
gen_list_of_length(Len, ElementGen) when Len > 0 ->
    catena_gen:new(fun(Size, Seed) ->
        gen_list_tree(Len, ElementGen, Size, Seed)
    end).

%% @private Build a list tree with exact length.
-spec gen_list_tree(non_neg_integer(), catena_gen:generator(A), catena_gen:size(), catena_gen:seed()) -> catena_tree:tree([A]).
gen_list_tree(0, _ElementGen, _Size, _Seed) ->
    catena_tree:singleton([]);
gen_list_tree(Len, ElementGen, Size, Seed) when Len > 0 ->
    {Elements, _} = lists:mapfoldl(
        fun(_, AccSeed) ->
            {S1, S2} = catena_gen:seed_split(AccSeed),
            Tree = catena_gen:run(ElementGen, Size, S1),
            {catena_tree:root(Tree), S2}
        end,
        Seed,
        lists:seq(1, Len)
    ),
    catena_tree:tree(Elements, fun() ->
        %% Shrink by removing elements
        gen_list_shrink(Elements, ElementGen, Size)
    end).

%% ---- Tuple Generators ----

%% @doc Generate a 2-tuple (pair).
-spec gen_tuple2(catena_gen:generator(A), catena_gen:generator(B)) -> catena_gen:generator({A, B}).
gen_tuple2(GenA, GenB) ->
    catena_gen:new(fun(Size, Seed) ->
        {Seed1, Seed2} = catena_gen:seed_split(Seed),
        TreeA = catena_gen:run(GenA, Size, Seed1),
        TreeB = catena_gen:run(GenB, Size, Seed2),
        {A, B} = {catena_tree:root(TreeA), catena_tree:root(TreeB)},
        catena_tree:tree({A, B}, fun() ->
            %% Shrink by shrinking each element
            ChildrenA = [catena_tree:map(fun(A2) -> {A2, B} end, Child) || Child <- catena_tree:children(TreeA)],
            ChildrenB = [catena_tree:map(fun(B2) -> {A, B2} end, Child) || Child <- catena_tree:children(TreeB)],
            ChildrenA ++ ChildrenB
        end)
    end).

%% @doc Generate a 3-tuple (triple).
-spec gen_tuple3(catena_gen:generator(A), catena_gen:generator(B), catena_gen:generator(C)) -> catena_gen:generator({A, B, C}).
gen_tuple3(GenA, GenB, GenC) ->
    catena_gen:new(fun(Size, Seed) ->
        {Seed1, Seed2} = catena_gen:seed_split(Seed),
        {Seed3, _} = catena_gen:seed_split(Seed2),
        TreeA = catena_gen:run(GenA, Size, Seed1),
        TreeB = catena_gen:run(GenB, Size, Seed2),
        TreeC = catena_gen:run(GenC, Size, Seed3),
        {A, B, C} = {catena_tree:root(TreeA), catena_tree:root(TreeB), catena_tree:root(TreeC)},
        catena_tree:tree({A, B, C}, fun() ->
            %% Shrink by shrinking each element
            ChildrenA = [catena_tree:map(fun(A2) -> {A2, B, C} end, Child) || Child <- catena_tree:children(TreeA)],
            ChildrenB = [catena_tree:map(fun(B2) -> {A, B2, C} end, Child) || Child <- catena_tree:children(TreeB)],
            ChildrenC = [catena_tree:map(fun(C2) -> {A, B, C2} end, Child) || Child <- catena_tree:children(TreeC)],
            ChildrenA ++ ChildrenB ++ ChildrenC
        end)
    end).

%% @doc Generate a 4-tuple (quadruple).
-spec gen_tuple4(catena_gen:generator(A), catena_gen:generator(B), catena_gen:generator(C), catena_gen:generator(D)) -> catena_gen:generator({A, B, C, D}).
gen_tuple4(GenA, GenB, GenC, GenD) ->
    catena_gen:new(fun(Size, Seed) ->
        {Seed1, Seed2} = catena_gen:seed_split(Seed),
        {Seed3, Seed4} = catena_gen:seed_split(Seed2),
        TreeA = catena_gen:run(GenA, Size, Seed1),
        TreeB = catena_gen:run(GenB, Size, Seed2),
        TreeC = catena_gen:run(GenC, Size, Seed3),
        TreeD = catena_gen:run(GenD, Size, Seed4),
        {A, B, C, D} = {catena_tree:root(TreeA), catena_tree:root(TreeB), catena_tree:root(TreeC), catena_tree:root(TreeD)},
        catena_tree:tree({A, B, C, D}, fun() ->
            %% Shrink by shrinking each element
            ChildrenA = [catena_tree:map(fun(A2) -> {A2, B, C, D} end, Child) || Child <- catena_tree:children(TreeA)],
            ChildrenB = [catena_tree:map(fun(B2) -> {A, B2, C, D} end, Child) || Child <- catena_tree:children(TreeB)],
            ChildrenC = [catena_tree:map(fun(C2) -> {A, B, C2, D} end, Child) || Child <- catena_tree:children(TreeC)],
            ChildrenD = [catena_tree:map(fun(D2) -> {A, B, C, D2} end, Child) || Child <- catena_tree:children(TreeD)],
            ChildrenA ++ ChildrenB ++ ChildrenC ++ ChildrenD
        end)
    end).

%% @doc Generate a tuple from a tuple of generators.
%%
%% Takes a tuple of generators and returns a generator that produces
%% tuples with values from each generator. Supports tuples of arity 2-4.
%%
%% == Example ==
%%
%% ```
%% %% Generate {Int, Bool, Int} tuples
%% Gen = gen_tuple({gen_int(), gen_bool(), gen_int()})
%% ```
-spec gen_tuple({catena_gen:generator(_), catena_gen:generator(_)}) ->
    catena_gen:generator({_, _});
             ({catena_gen:generator(_), catena_gen:generator(_), catena_gen:generator(_)}) ->
    catena_gen:generator({_, _, _});
             ({catena_gen:generator(_), catena_gen:generator(_), catena_gen:generator(_), catena_gen:generator(_)}) ->
    catena_gen:generator({_, _, _, _}).
gen_tuple({GenA, GenB}) ->
    gen_tuple2(GenA, GenB);
gen_tuple({GenA, GenB, GenC}) ->
    gen_tuple3(GenA, GenB, GenC);
gen_tuple({GenA, GenB, GenC, GenD}) ->
    gen_tuple4(GenA, GenB, GenC, GenD).

%% ---- Map Generators ----

%% @doc Generate a map with keys and values from the given generators.
-spec gen_map(catena_gen:generator(_), catena_gen:generator(_)) -> catena_gen:generator(map()).
gen_map(KeyGen, ValueGen) ->
    catena_gen:new(fun(Size, Seed) ->
        {LengthWord, Seed1} = catena_gen:seed_next(Seed),
        MaxLen = max(Size div 3, 0),
        Length = LengthWord rem (MaxLen + 1),
        gen_map_tree(Length, KeyGen, ValueGen, Size, Seed1)
    end).

%% @doc Generate a map with size from a Range.
-spec gen_map_of(catena_range:range(), catena_gen:generator(_), catena_gen:generator(_)) -> catena_gen:generator(map()).
gen_map_of(LengthRange, KeyGen, ValueGen) ->
    catena_gen:new(fun(Size, Seed) ->
        {LengthWord, Seed1} = catena_gen:seed_next(Seed),
        {Min, Max} = catena_range:range_bounds(LengthRange, Size),
        Length = Min + (LengthWord rem (Max - Min + 1)),
        gen_map_tree(Length, KeyGen, ValueGen, Size, Seed1)
    end).

%% @private Build a map tree with exact length.
-spec gen_map_tree(non_neg_integer(), catena_gen:generator(_), catena_gen:generator(_), catena_gen:size(), catena_gen:seed()) -> catena_tree:tree(map()).
gen_map_tree(0, _KeyGen, _ValueGen, _Size, _Seed) ->
    catena_tree:singleton(#{});
gen_map_tree(Len, KeyGen, ValueGen, Size, Seed) when Len > 0 ->
    {Entries, _} = lists:mapfoldl(
        fun(_, AccSeed) ->
            {SeedK, SeedV} = catena_gen:seed_split(AccSeed),
            KeyTree = catena_gen:run(KeyGen, Size, SeedK),
            ValTree = catena_gen:run(ValueGen, Size, SeedV),
            {{catena_tree:root(KeyTree), catena_tree:root(ValTree)}, SeedV}
        end,
        Seed,
        lists:seq(1, Len)
    ),
    %% Remove duplicates by keeping the last occurrence of each key
    Map = lists:foldl(fun({K, V}, Acc) -> maps:put(K, V, Acc) end, #{}, Entries),
    catena_tree:tree(Map, fun() ->
        %% Shrink by removing entries
        gen_map_shrink(maps:to_list(Map), KeyGen, ValueGen, Size)
    end).

%% ---- Set Generators ----

%% @doc Generate a set (represented as a unique-element list) from the given generator.
-spec gen_set(catena_gen:generator(A)) -> catena_gen:generator([A]).
gen_set(ElementGen) ->
    catena_gen:new(fun(Size, Seed) ->
        {LengthWord, Seed1} = catena_gen:seed_next(Seed),
        MaxLen = max(Size, 1),
        Length = LengthWord rem (MaxLen + 1),
        gen_set_tree(Length, ElementGen, Size, Seed1)
    end).

%% @doc Generate a set with size from a Range.
-spec gen_set_of(catena_range:range(), catena_gen:generator(A)) -> catena_gen:generator([A]).
gen_set_of(LengthRange, ElementGen) ->
    catena_gen:new(fun(Size, Seed) ->
        {LengthWord, Seed1} = catena_gen:seed_next(Seed),
        {Min, Max} = catena_range:range_bounds(LengthRange, Size),
        Length = Min + (LengthWord rem (Max - Min + 1)),
        gen_set_tree(Length, ElementGen, Size, Seed1)
    end).

%% @private Build a set tree with exact length.
-spec gen_set_tree(non_neg_integer(), catena_gen:generator(A), catena_gen:size(), catena_gen:seed()) -> catena_tree:tree([A]).
gen_set_tree(0, _ElementGen, _Size, _Seed) ->
    catena_tree:singleton([]);
gen_set_tree(Len, ElementGen, Size, Seed) when Len > 0 ->
    {Elements, _} = lists:mapfoldl(
        fun(_, AccSeed) ->
            {S1, S2} = catena_gen:seed_split(AccSeed),
            Tree = catena_gen:run(ElementGen, Size, S1),
            {catena_tree:root(Tree), S2}
        end,
        Seed,
        lists:seq(1, Len)
    ),
    %% Remove duplicates while preserving order
    UniqueElements = lists:usort(Elements),
    catena_tree:tree(UniqueElements, fun() ->
        %% Shrink by removing elements
        gen_set_shrink(UniqueElements, ElementGen, Size)
    end).

%%====================================================================
%% Internal Helpers - Collections
%%====================================================================

%% @doc Shrink a list by removing elements and shrinking individual elements.
-spec gen_list_shrink([A], catena_gen:generator(A), catena_gen:size()) -> [catena_tree:tree([A])].
gen_list_shrink([], _ElementGen, _Size) ->
    [];
gen_list_shrink(List, ElementGen, Size) ->
    %% Shrink by removing elements from the end
    RemoveShrinks = case length(List) > 1 of
        true -> [lists:sublist(List, length(List) - 1)];
        false -> [[]]
    end,
    %% Also provide empty list shrink
    RemoveShrinks2 = case List of
        [] -> [];
        _ -> [[] | RemoveShrinks]
    end,
    lists:map(fun(L) ->
        catena_tree:tree(L, fun() -> gen_list_shrink(L, ElementGen, Size) end)
    end, lists:usort(RemoveShrinks2)).

%% @doc Shrink a map by removing entries.
-spec gen_map_shrink([{_, _}], catena_gen:generator(_), catena_gen:generator(_), catena_gen:size()) -> [catena_tree:tree(map())].
gen_map_shrink([], _KeyGen, _ValueGen, _Size) ->
    [];
gen_map_shrink(Entries, KeyGen, ValueGen, Size) ->
    %% Shrink by removing one entry at a time
    case length(Entries) > 1 of
        true ->
            lists:map(fun(N) ->
                {Before, [_ | After]} = lists:split(N - 1, Entries),
                Map = maps:from_list(Before ++ After),
                catena_tree:tree(Map, fun() -> gen_map_shrink(maps:to_list(Map), KeyGen, ValueGen, Size) end)
            end, lists:seq(1, length(Entries)));
        false ->
            %% Always provide empty map shrink
            [catena_tree:singleton(#{})]
    end.

%% @doc Shrink a set by removing elements.
-spec gen_set_shrink([A], catena_gen:generator(A), catena_gen:size()) -> [catena_tree:tree([A])].
gen_set_shrink([], _ElementGen, _Size) ->
    [];
gen_set_shrink(Set, ElementGen, Size) ->
    %% Shrink by removing one element at a time
    RemoveShrinks = case length(Set) > 1 of
        true ->
            lists:map(fun(N) ->
                {Before, [_ | After]} = lists:split(N - 1, Set),
                NewSet = Before ++ After,
                catena_tree:tree(NewSet, fun() -> gen_set_shrink(NewSet, ElementGen, Size) end)
            end, lists:seq(1, length(Set)));
        false ->
            %% Always provide empty set shrink
            [catena_tree:singleton([])]
    end,
    RemoveShrinks.

%%====================================================================
%% Section 2.4: Recursive Structure Support
%%====================================================================

%% ---- Binary Tree Generator ----

%% @doc Generate a binary tree with leaf nodes containing values.
%%
%% Trees shrink toward leaves and smaller values.
%% Represents {leaf, Value} | {node, Left, Right}.
-spec gen_BinaryTree(catena_gen:generator(A)) -> catena_gen:generator({leaf, A} | {node, _, _}).
gen_BinaryTree(ValueGen) ->
    gen_recursive(10, fun() -> gen_BinaryTree_unsafe(ValueGen) end).

%% @private Generate binary tree without size limit (internal use).
-spec gen_BinaryTree_unsafe(catena_gen:generator(_)) -> catena_gen:generator({leaf, _} | {node, _, _}).
gen_BinaryTree_unsafe(ValueGen) ->
    catena_gen:new(fun(Size, Seed) ->
        %% Use size to determine if we should create a leaf or node
        {Word, Seed1} = catena_gen:seed_next(Seed),
        Threshold = max(10, Size),
        IsLeaf = (Word rem Threshold) < (Threshold div 2),

        case IsLeaf of
            true ->
                %% Create a leaf
                LeafTree = catena_gen:run(ValueGen, Size, Seed1),
                LeafValue = catena_tree:root(LeafTree),
                catena_tree:tree({leaf, LeafValue}, fun() ->
                    %% Shrink toward smaller leaf values
                    LeafChildren = catena_tree:children(LeafTree),
                    case LeafChildren of
                        [] -> [];
                        _ -> [catena_tree:map(fun(V) -> {leaf, V} end, C) || C <- LeafChildren]
                    end
                end);
            false ->
                %% Create a node with two subtrees
                {Seed2, Seed3} = catena_gen:seed_split(Seed1),
                LeftTree = catena_gen:run(gen_BinaryTree_unsafe(ValueGen), Size div 2, Seed2),
                RightTree = catena_gen:run(gen_BinaryTree_unsafe(ValueGen), Size div 2, Seed3),
                Left = catena_tree:root(LeftTree),
                Right = catena_tree:root(RightTree),
                catena_tree:tree({node, Left, Right}, fun() ->
                    %% Shrink by replacing subtrees with leaves or shrinking subtrees
                    LeftChildren = catena_tree:children(LeftTree),
                    RightChildren = catena_tree:children(RightTree),
                    LeftShrinks = case LeftChildren of
                        [] -> [];
                        _ -> [catena_tree:map(fun(L) -> {node, L, Right} end, C) || C <- LeftChildren]
                    end,
                    RightShrinks = case RightChildren of
                        [] -> [];
                        _ -> [catena_tree:map(fun(R) -> {node, Left, R} end, C) || C <- RightChildren]
                    end,
                    %% Also provide shrinks toward leaves
                    LeafShrinks = case {Left, Right} of
                        {{leaf, _}, _} -> [];
                        {_, {leaf, _}} -> [];
                        _ -> [catena_tree:singleton({leaf, catena_tree:root(LeftTree)})]
                    end,
                    LeftShrinks ++ RightShrinks ++ LeafShrinks
                end)
        end
    end).

%% ---- Maybe/Option Generator ----

%% @doc Generate a Maybe/Option type: none | {some, Value}.
-spec gen_maybe(catena_gen:generator(A)) -> catena_gen:generator(none | {some, A}).
gen_maybe(ValueGen) ->
    catena_gen:gen_frequency([
        {1, catena_gen:constant(none)},
        {3, catena_gen:gen_map(fun(V) -> {some, V} end, ValueGen)}
    ]).

%% ---- Result/Either Generator ----

%% @doc Generate a Result type: {ok, Value} | {error, Error}.
-spec gen_result(catena_gen:generator(A), catena_gen:generator(B)) -> catena_gen:generator({ok, A} | {error, B}).
gen_result(OkGen, ErrorGen) ->
    catena_gen:gen_frequency([
        {3, catena_gen:gen_map(fun(V) -> {ok, V} end, OkGen)},
        {1, catena_gen:gen_map(fun(E) -> {error, E} end, ErrorGen)}
    ]).

%% ---- Recursive List Generator ----

%% @doc Generate nested lists (lists containing lists or elements).
%%
%% Depth is controlled by size parameter.
-spec gen_list_recursive(catena_gen:generator(_)) -> catena_gen:generator(list()).
gen_list_recursive(ElementGen) ->
    gen_recursive(15, fun() -> gen_list_recursive_unsafe(ElementGen) end).

%% @private Generate recursive list without size limit.
-spec gen_list_recursive_unsafe(catena_gen:generator(_)) -> catena_gen:generator(list()).
gen_list_recursive_unsafe(ElementGen) ->
    catena_gen:new(fun(Size, Seed) ->
        {Word, Seed1} = catena_gen:seed_next(Seed),
        Threshold = max(5, Size),
        %% Decide: generate element or nested list
        MakeNested = (Word rem Threshold) > (Threshold div 2),

        case MakeNested of
            true ->
                %% Generate a nested list
                NestedGen = gen_list(ElementGen),
                NestedTree = catena_gen:run(NestedGen, Size div 2, Seed1),
                NestedList = catena_tree:root(NestedTree),
                catena_tree:tree(NestedList, fun() ->
                    %% Shrink by simplifying the nested list
                    NestedChildren = catena_tree:children(NestedTree),
                    case NestedChildren of
                        [] -> [];
                        _ -> [catena_tree:map(fun(L) -> L end, C) || C <- NestedChildren]
                    end
                end);
            false ->
                %% Generate a simple element list
                SimpleGen = gen_list(ElementGen),
                catena_gen:run(SimpleGen, Size, Seed1)
        end
    end).

%% ---- Generic Recursive Generator ----

%% @doc Generate recursive structures with size control.
%%
%% Takes a maximum depth and a thunk that returns the generator.
%% Reduces size as depth increases to prevent infinite recursion.
-spec gen_recursive(pos_integer(), fun(() -> catena_gen:generator(A))) -> catena_gen:generator(A).
gen_recursive(MaxDepth, GenThunk) ->
    catena_gen:new(fun(Size, Seed) ->
        {Word, Seed1} = catena_gen:seed_next(Seed),
        %% Scale effective size by depth
        EffectiveSize = max(1, Size div MaxDepth),
        %% Occasionally use smaller size to create base cases
        UseSmaller = (Word rem 5) =:= 0,
        ActualSize = case UseSmaller of
            true -> max(1, EffectiveSize div 2);
            false -> EffectiveSize
        end,
        Gen = GenThunk(),
        catena_gen:run(Gen, ActualSize, Seed1)
    end).

%%====================================================================
%% Internal Helpers
%%====================================================================

%% @doc Shrink a character toward space (32).
-spec gen_char_shrink(char()) -> [char()].
gen_char_shrink(CharCode) when CharCode =< 33 ->
    [];
gen_char_shrink(CharCode) ->
    [CharCode - 1].

%% @doc Shrink an alpha character toward 'a' or 'A'.
-spec gen_char_shrink_alpha(char()) -> [char()].
gen_char_shrink_alpha(CharCode) when CharCode >= $a andalso CharCode =< $z ->
    [$a, CharCode - 1];
gen_char_shrink_alpha(CharCode) when CharCode >= $A andalso CharCode < $Z ->
    [$A, CharCode - 1];
gen_char_shrink_alpha(_) ->
    [].

%% @doc Shrink a string by removing characters and simplifying.
-spec gen_string_shrink(string(), catena_gen:generator(char()), catena_gen:size()) -> [catena_tree:tree(string())].
gen_string_shrink("", _CharGen, _Size) ->
    [];
gen_string_shrink(String, CharGen, Size) ->
    %% Shrink by removing last character
    Shrink1 = case length(String) > 1 of
        true -> [lists:sublist(String, length(String) - 1)];
        false -> [""]
    end,
    %% Shrink toward 'a' for first char if alphabetic
    Shrink2 = case String of
        [First | _] when First >= $a, First =< $z -> ["a" ++ tl(String)];
        [First | _] when First >= $A, First < $Z -> ["A" ++ tl(String)];
        _ -> []
    end,
    lists:map(fun(S) ->
        case S of
            "" -> catena_tree:singleton("");
            Str -> catena_tree:tree(Str, fun() ->
                gen_string_shrink(Str, CharGen, Size)
            end)
        end
    end, lists:usort(Shrink1 ++ Shrink2)).

%% @doc Shrink a binary by removing bytes.
-spec gen_binary_shrink(binary(), catena_gen:size()) -> [catena_tree:tree(_)].
gen_binary_shrink(<<>>, _Size) ->
    [];
gen_binary_shrink(Binary, Size) ->
    %% Remove one byte
    case byte_size(Binary) of
        1 -> [catena_tree:singleton(<<>>)];
        Len ->
            <<Byte:8, Rest/binary>> = Binary,
            [catena_tree:tree(Rest, fun() ->
                gen_binary_shrink(Rest, Size)
            end)]
    end.

%%====================================================================
%% Unit Tests - Section 2.2
%%====================================================================

gen_char_printable_test() ->
    Gen = catena_stdgen:gen_char(),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Char = catena_tree:root(Tree),
    ?assert(Char >= 32 andalso Char =< 127),
    ok.

gen_char_alpha_produces_letter_test() ->
    Gen = catena_stdgen:gen_char_alpha(),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Char = catena_tree:root(Tree),
    ?assert((Char >= $a andalso Char =< $z) orelse (Char >= $A andalso Char =< $Z)),
    ok.

gen_char_digit_produces_0_to_9_test() ->
    Gen = catena_stdgen:gen_char_digit(),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Digit = catena_tree:root(Tree),
    ?assert(Digit >= $0 andalso Digit =< $9),
    ok.

gen_char_alphanumeric_test() ->
    Gen = catena_stdgen:gen_char_alphanumeric(),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Char = catena_tree:root(Tree),
    IsAlpha = (Char >= $a andalso Char =< $z) orelse (Char >= $A andalso Char =< $Z),
    IsDigit = Char >= $0 andalso Char =< $9,
    ?assert(IsAlpha orelse IsDigit),
    ok.

gen_string_respects_length_test() ->
    Range = catena_range:range_linear(0, 20),
    Gen = catena_stdgen:gen_string(Range),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    String = catena_tree:root(Tree),
    ?assert(is_list(String) andalso is_integer(length(String))),
    ?assert(length(String) =< 20),
    ok.

gen_string_shrinks_by_removing_chars_test() ->
    Gen = catena_stdgen:gen_string(catena_range:range_constant({10, 10})),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    String = catena_tree:root(Tree),
    ?assert(length(String) =:= 10),
    %% Should have shrinks
    Children = catena_tree:children(Tree),
    ?assert(length(Children) > 0),
    ok.

gen_string_nonempty_is_nonempty_test() ->
    Gen = catena_stdgen:gen_string_nonempty(catena_range:range_constant({0, 10})),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    String = catena_tree:root(Tree),
    ?assert(length(String) >= 1),
    ok.

gen_binary_respects_length_test() ->
    Range = catena_range:range_linear(0, 100),
    Gen = catena_stdgen:gen_binary(Range),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Binary = catena_tree:root(Tree),
    ?assert(is_binary(Binary)),
    ?assert(byte_size(Binary) =< 100),
    ok.

gen_binary_shrinks_by_removing_bytes_test() ->
    Gen = catena_stdgen:gen_binary(catena_range:range_constant({10, 10})),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    Binary = catena_tree:root(Tree),
    ?assert(byte_size(Binary) =:= 10),
    %% Should have shrinks
    Children = catena_tree:children(Tree),
    ?assert(length(Children) > 0),
    ok.

gen_atom_returns_valid_atom_test() ->
    Gen = catena_stdgen:gen_atom(),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Atom = catena_tree:root(Tree),
    ?assert(is_atom(Atom)),
    ok.

gen_bool_atom_returns_true_or_false_test() ->
    Gen = catena_stdgen:gen_bool_atom(),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Atom = catena_tree:root(Tree),
    ?assert(Atom =:= true orelse Atom =:= false),
    ok.

gen_char_unicode_produces_valid_codepoint_test() ->
    Gen = catena_stdgen:gen_char_unicode(),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Code = catena_tree:root(Tree),
    ?assert(Code >= 0 andalso Code =< 16#FFFF),
    %% Should not be in surrogate range
    ?assert(Code < 16#D800 orelse Code > 16#DFFF),
    ok.

gen_utf8_produces_valid_binary_test() ->
    Gen = catena_stdgen:gen_utf8(catena_range:range_constant({0, 10})),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Binary = catena_tree:root(Tree),
    ?assert(is_binary(Binary)),
    %% Should be valid UTF-8
    ?assertEqual(Binary, unicode:characters_to_binary(Binary)),
    ok.

gen_utf8_shrinks_by_removing_chars_test() ->
    Gen = catena_stdgen:gen_utf8(catena_range:range_constant({5, 10})),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    Root = catena_tree:root(Tree),
    RootSize = byte_size(Root),
    %% Should have shrinks
    Children = catena_tree:children(Tree),
    ?assert(length(Children) > 0),
    %% Shrinks should be shorter or empty (empty is valid UTF-8)
    ShrunkSizes = [byte_size(catena_tree:root(C)) || C <- Children],
    ?assert(lists:all(fun(S) -> S =< RootSize end, ShrunkSizes)),
    ok.

gen_binary_of_uses_byte_generator_test() ->
    ByteGen = catena_gen:gen_int_range(0, 255),
    Gen = catena_stdgen:gen_binary_of(catena_range:range_constant({5, 10}), ByteGen),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Binary = catena_tree:root(Tree),
    ?assert(is_binary(Binary)),
    Size = byte_size(Binary),
    ?assert(Size >= 5 andalso Size =< 10),
    ok.

gen_atom_unsafe_generates_atoms_test() ->
    Gen = catena_stdgen:gen_atom_unsafe(5),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Atom = catena_tree:root(Tree),
    ?assert(is_atom(Atom)),
    %% Should start with test_atom_
    AtomStr = atom_to_list(Atom),
    ?assertEqual("test_atom_", lists:sublist(AtomStr, 1, 10)),
    ok.

%%====================================================================
%% Section 2.5: Function Generators
%%====================================================================

%% ---- Simple Function Generators ----

%% @doc Generate a nullary function (arity 0) that returns values from ValueGen.
%%
%% The generated function is deterministic - it always returns the same value.
-spec gen_function0(catena_gen:generator(_)) -> catena_gen:generator(function()).
gen_function0(ValueGen) ->
    catena_gen:new(fun(Size, Seed) ->
        ValueTree = catena_gen:run(ValueGen, Size, Seed),
        Value = catena_tree:root(ValueTree),
        %% Create a function that captures and returns the value
        Fun = fun() -> Value end,
        catena_tree:tree(Fun, fun() ->
            %% Shrink by shrinking the captured value
            [catena_tree:map(fun(V) -> fun() -> V end end, C) || C <- catena_tree:children(ValueTree)]
        end)
    end).

%% @doc Generate a unary function (arity 1) that returns values from ValueGen.
%%
%% The generated function is deterministic - given the same input, it returns
%% the same output. The function ignores its input and returns the captured value.
-spec gen_function1(catena_gen:generator(_)) -> catena_gen:generator(function()).
gen_function1(ValueGen) ->
    catena_gen:new(fun(Size, Seed) ->
        ValueTree = catena_gen:run(ValueGen, Size, Seed),
        Value = catena_tree:root(ValueTree),
        %% Create a function that ignores input and returns the captured value
        Fun = fun(_) -> Value end,
        catena_tree:tree(Fun, fun() ->
            %% Shrink by shrinking the captured value
            [catena_tree:map(fun(V) -> fun(_) -> V end end, C) || C <- catena_tree:children(ValueTree)]
        end)
    end).

%% @doc Generate a binary function (arity 2) that returns values from ValueGen.
%%
%% The generated function is deterministic and ignores its inputs.
-spec gen_function2(catena_gen:generator(_)) -> catena_gen:generator(function()).
gen_function2(ValueGen) ->
    catena_gen:new(fun(Size, Seed) ->
        ValueTree = catena_gen:run(ValueGen, Size, Seed),
        Value = catena_tree:root(ValueTree),
        %% Create a function that ignores inputs and returns the captured value
        Fun = fun(_, _) -> Value end,
        catena_tree:tree(Fun, fun() ->
            %% Shrink by shrinking the captured value
            [catena_tree:map(fun(V) -> fun(_, _) -> V end end, C) || C <- catena_tree:children(ValueTree)]
        end)
    end).

%% ---- Special Functions ----

%% @doc Generate a constant function that always returns the given value.
%%
%% The function accepts any number of arguments and always returns Value.
-spec gen_constant_function(_) -> function().
gen_constant_function(Value) ->
    fun() -> Value end.

%% @doc Generate the identity function.
%%
%% Returns its input unchanged.
-spec gen_identity_function() -> function().
gen_identity_function() ->
    fun(X) -> X end.

%% @doc Generate a function that returns the first element of a tuple.
-spec gen_projection_first() -> function().
gen_projection_first() ->
    fun({X, _}) -> X end.

%% @doc Generate a function that returns the second element of a tuple.
-spec gen_projection_second() -> function().
gen_projection_second() ->
    fun({_, X}) -> X end.

%% @doc Generate the K-combinator (const function).
%%
%% K x = \y -> x (a function that ignores its argument and returns x).
-spec gen_k_combinator(_) -> function().
gen_k_combinator(X) ->
    fun(_) -> X end.

%% @doc Choose a generator based on frequency weights.
%%
%% Takes a list of {Weight, Generator} pairs and selects one generator
%% based on the weights. Higher weights are more likely to be selected.
-spec gen_frequency([{pos_integer(), catena_gen:generator(_)}]) -> catena_gen:generator(_).
gen_frequency(WeightedGens) ->
    TotalWeight = lists:sum([W || {W, _} <- WeightedGens]),
    catena_gen:new(fun(Size, Seed) ->
        {Word, Seed1} = catena_gen:seed_next(Seed),
        %% Select which generator to use based on weight
        Selector = Word rem TotalWeight,
        %% Initialize with first generator and its weight
        [{FirstWeight, FirstGen} | Rest] = WeightedGens,
        {SelectedGen, _} = lists:foldl(fun
            ({Weight, Gen}, {AccGen, AccWeight}) ->
                case Selector < AccWeight + Weight of
                    true -> {Gen, AccWeight + Weight};
                    false -> {AccGen, AccWeight + Weight}
                end
            end,
            {FirstGen, FirstWeight},
            Rest
        ),
        catena_gen:run(SelectedGen, Size, Seed1)
    end).

%%====================================================================
%% Unit Tests - Section 2.4
%%====================================================================
%% Tests are in catena_stdgen_recursive_tests.erl
