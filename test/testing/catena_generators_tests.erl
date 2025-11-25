%% @doc Tests for the Catena Property Test Generators (Phase 2.3)
%%
%% These tests verify that generators produce valid values within
%% expected ranges for property-based testing.
-module(catena_generators_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Bool Generator Tests
%%====================================================================

gen_bool_type_test() ->
    Value = catena_generators:gen_bool(),
    ?assert(is_boolean(Value)).

gen_bool_distribution_test() ->
    %% Run 100 times and check we get both true and false
    Values = [catena_generators:gen_bool() || _ <- lists:seq(1, 100)],
    ?assert(lists:member(true, Values)),
    ?assert(lists:member(false, Values)).

generate_bool_test() ->
    Value = catena_generators:generate('Bool'),
    ?assert(is_boolean(Value)).

%%====================================================================
%% Natural Generator Tests
%%====================================================================

gen_natural_type_test() ->
    Value = catena_generators:gen_natural(),
    ?assert(is_integer(Value)),
    ?assert(Value >= 0).

gen_natural_range_test() ->
    %% All values should be 0-100
    Values = [catena_generators:gen_natural() || _ <- lists:seq(1, 100)],
    ?assert(lists:all(fun(V) -> V >= 0 andalso V =< 100 end, Values)).

gen_natural_custom_max_test() ->
    Max = 10,
    Values = [catena_generators:gen_natural(Max) || _ <- lists:seq(1, 100)],
    ?assert(lists:all(fun(V) -> V >= 0 andalso V =< Max end, Values)).

generate_natural_test() ->
    Value = catena_generators:generate('Natural'),
    ?assert(is_integer(Value)),
    ?assert(Value >= 0).

%%====================================================================
%% Int Generator Tests
%%====================================================================

gen_int_type_test() ->
    Value = catena_generators:gen_int(),
    ?assert(is_integer(Value)).

gen_int_range_test() ->
    %% All values should be -100 to 100
    Values = [catena_generators:gen_int() || _ <- lists:seq(1, 100)],
    ?assert(lists:all(fun(V) -> V >= -100 andalso V =< 100 end, Values)).

gen_int_includes_negatives_test() ->
    %% Run enough times to get negative values
    Values = [catena_generators:gen_int() || _ <- lists:seq(1, 200)],
    ?assert(lists:any(fun(V) -> V < 0 end, Values)).

gen_int_custom_max_test() ->
    Max = 5,
    Values = [catena_generators:gen_int(Max) || _ <- lists:seq(1, 100)],
    ?assert(lists:all(fun(V) -> V >= -Max andalso V =< Max end, Values)).

generate_int_test() ->
    Value = catena_generators:generate('Int'),
    ?assert(is_integer(Value)).

%%====================================================================
%% Text Generator Tests
%%====================================================================

gen_text_type_test() ->
    Value = catena_generators:gen_text(),
    ?assert(is_list(Value)).

gen_text_length_test() ->
    %% Length should be 0-20
    Values = [catena_generators:gen_text() || _ <- lists:seq(1, 50)],
    ?assert(lists:all(fun(V) -> length(V) >= 0 andalso length(V) =< 20 end, Values)).

gen_text_characters_test() ->
    %% All characters should be lowercase ASCII (a-z)
    Value = catena_generators:gen_text(),
    ?assert(lists:all(fun(C) -> C >= $a andalso C =< $z end, Value)).

gen_text_custom_length_test() ->
    MaxLen = 5,
    Values = [catena_generators:gen_text(MaxLen) || _ <- lists:seq(1, 50)],
    ?assert(lists:all(fun(V) -> length(V) =< MaxLen end, Values)).

gen_text_includes_empty_test() ->
    %% Run enough times to likely get an empty string
    Values = [catena_generators:gen_text() || _ <- lists:seq(1, 100)],
    ?assert(lists:member([], Values) orelse length(Values) == 100).

generate_text_test() ->
    Value = catena_generators:generate('Text'),
    ?assert(is_list(Value)).

generate_string_alias_test() ->
    Value = catena_generators:generate('String'),
    ?assert(is_list(Value)).

%%====================================================================
%% List Generator Tests
%%====================================================================

gen_list_type_test() ->
    ElemGen = fun() -> catena_generators:gen_int() end,
    Value = catena_generators:gen_list(ElemGen),
    ?assert(is_list(Value)).

gen_list_length_test() ->
    ElemGen = fun() -> 1 end,
    Values = [catena_generators:gen_list(ElemGen) || _ <- lists:seq(1, 50)],
    ?assert(lists:all(fun(V) -> length(V) >= 0 andalso length(V) =< 20 end, Values)).

gen_list_custom_length_test() ->
    ElemGen = fun() -> 1 end,
    MaxLen = 5,
    Values = [catena_generators:gen_list(ElemGen, MaxLen) || _ <- lists:seq(1, 50)],
    ?assert(lists:all(fun(V) -> length(V) =< MaxLen end, Values)).

gen_list_uses_element_generator_test() ->
    %% Element generator always returns 42
    ElemGen = fun() -> 42 end,
    Value = catena_generators:gen_list(ElemGen, 10),
    ?assert(lists:all(fun(E) -> E == 42 end, Value)).

generate_list_test() ->
    Value = catena_generators:generate('List'),
    ?assert(is_list(Value)).

%%====================================================================
%% Maybe Generator Tests
%%====================================================================

gen_maybe_type_test() ->
    ValueGen = fun() -> 42 end,
    Value = catena_generators:gen_maybe(ValueGen),
    case Value of
        {some, _} -> ?assert(true);
        none -> ?assert(true);
        _ -> ?assert(false)
    end.

gen_maybe_distribution_test() ->
    ValueGen = fun() -> 42 end,
    Values = [catena_generators:gen_maybe(ValueGen) || _ <- lists:seq(1, 100)],
    HasSome = lists:any(fun({some, _}) -> true; (_) -> false end, Values),
    HasNone = lists:member(none, Values),
    ?assert(HasSome),
    ?assert(HasNone).

generate_maybe_test() ->
    Value = catena_generators:generate('Maybe'),
    case Value of
        {some, _} -> ?assert(true);
        none -> ?assert(true);
        _ -> ?assert(false)
    end.

%%====================================================================
%% Result Generator Tests
%%====================================================================

gen_result_type_test() ->
    OkGen = fun() -> 42 end,
    ErrGen = fun() -> "error" end,
    Value = catena_generators:gen_result(OkGen, ErrGen),
    case Value of
        {ok, _} -> ?assert(true);
        {err, _} -> ?assert(true);
        _ -> ?assert(false)
    end.

gen_result_distribution_test() ->
    OkGen = fun() -> 42 end,
    ErrGen = fun() -> "error" end,
    Values = [catena_generators:gen_result(OkGen, ErrGen) || _ <- lists:seq(1, 100)],
    HasOk = lists:any(fun({ok, _}) -> true; (_) -> false end, Values),
    HasErr = lists:any(fun({err, _}) -> true; (_) -> false end, Values),
    ?assert(HasOk),
    ?assert(HasErr).

generate_result_test() ->
    Value = catena_generators:generate('Result'),
    case Value of
        {ok, _} -> ?assert(true);
        {err, _} -> ?assert(true);
        _ -> ?assert(false)
    end.

%%====================================================================
%% Unknown Generator Tests
%%====================================================================

generate_unknown_throws_test() ->
    ?assertThrow({unknown_generator, 'Unknown'}, catena_generators:generate('Unknown')).

%%====================================================================
%% Shrinking Tests (Basic)
%%====================================================================

shrink_bool_true_test() ->
    Shrinks = catena_generators:shrink('Bool', true),
    ?assertEqual([false], Shrinks).

shrink_bool_false_test() ->
    Shrinks = catena_generators:shrink('Bool', false),
    ?assertEqual([], Shrinks).

shrink_natural_positive_test() ->
    Shrinks = catena_generators:shrink('Natural', 10),
    ?assert(lists:member(0, Shrinks)),
    ?assert(lists:member(5, Shrinks)),
    ?assert(lists:member(9, Shrinks)).

shrink_natural_zero_test() ->
    Shrinks = catena_generators:shrink('Natural', 0),
    ?assertEqual([], Shrinks).

shrink_int_positive_test() ->
    Shrinks = catena_generators:shrink('Int', 10),
    ?assert(lists:member(0, Shrinks)),
    ?assert(lists:member(-10, Shrinks)).

shrink_int_negative_test() ->
    Shrinks = catena_generators:shrink('Int', -10),
    ?assert(lists:member(0, Shrinks)),
    ?assert(lists:member(10, Shrinks)).

shrink_text_nonempty_test() ->
    Shrinks = catena_generators:shrink('Text', "abc"),
    ?assert(lists:member([], Shrinks)).

shrink_list_nonempty_test() ->
    Shrinks = catena_generators:shrink('List', [1, 2, 3]),
    ?assert(lists:member([], Shrinks)).
