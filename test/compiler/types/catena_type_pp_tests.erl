%%%-------------------------------------------------------------------
%%% @doc Unit Tests for catena_type_pp module
%%% @end
%%%-------------------------------------------------------------------
-module(catena_type_pp_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%%====================================================================
%% Basic Type Pretty-Printing Tests
%%====================================================================

basic_types_test_() ->
    [
      ?_test(test_pp_var()),
      ?_test(test_pp_con()),
      ?_test(test_pp_tuple()),
      ?_test(test_pp_empty_tuple())
    ].

test_pp_var() ->
    Var = catena_types:tvar(1),
    ?assertEqual("α1", catena_type_pp:pp_type(Var)),

    Var2 = catena_types:tvar(42),
    ?assertEqual("α42", catena_type_pp:pp_type(Var2)).

test_pp_con() ->
    IntType = catena_types:tcon(integer),
    ?assertEqual("integer", catena_type_pp:pp_type(IntType)),

    StringType = catena_types:tcon(string),
    ?assertEqual("string", catena_type_pp:pp_type(StringType)).

test_pp_tuple() ->
    % (Int, String)
    Tuple = catena_types:ttuple([
        catena_types:tcon(integer),
        catena_types:tcon(string)
    ]),
    ?assertEqual("(integer, string)", catena_type_pp:pp_type(Tuple)).

test_pp_empty_tuple() ->
    % ()
    EmptyTuple = catena_types:ttuple([]),
    ?assertEqual("()", catena_type_pp:pp_type(EmptyTuple)).

%%====================================================================
%% Type Application Tests
%%====================================================================

type_app_test_() ->
    [
      ?_test(test_pp_simple_app()),
      ?_test(test_pp_multiple_args())
    ].

test_pp_simple_app() ->
    % List<Int>
    ListInt = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tcon(integer)]
    ),
    ?assertEqual("List<integer>", catena_type_pp:pp_type(ListInt)).

test_pp_multiple_args() ->
    % Map<String, Int>
    MapStringInt = catena_types:tapp(
        catena_types:tcon('Map'),
        [catena_types:tcon(string), catena_types:tcon(integer)]
    ),
    ?assertEqual("Map<string, integer>", catena_type_pp:pp_type(MapStringInt)).

%%====================================================================
%% Function Type Tests
%%====================================================================

function_test_() ->
    [
      ?_test(test_pp_pure_function()),
      ?_test(test_pp_effectful_function()),
      ?_test(test_pp_higher_order_function()),
      ?_test(test_pp_multiple_effects())
    ].

test_pp_pure_function() ->
    % Int -> String
    Fun = catena_types:tfun(
        catena_types:tcon(integer),
        catena_types:tcon(string),
        catena_types:empty_effects()
    ),
    ?assertEqual("integer -> string", catena_type_pp:pp_type(Fun)).

test_pp_effectful_function() ->
    % String -> Unit / {io}
    Fun = catena_types:tfun(
        catena_types:tcon(string),
        catena_types:tcon(unit),
        catena_types:singleton_effect(io)
    ),
    ?assertEqual("string -> unit / {io}", catena_type_pp:pp_type(Fun)).

test_pp_higher_order_function() ->
    % (Int -> String) -> List<Int> -> List<String>
    InnerFun = catena_types:tfun(
        catena_types:tcon(integer),
        catena_types:tcon(string),
        catena_types:empty_effects()
    ),

    ListInt = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tcon(integer)]
    ),

    ListString = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tcon(string)]
    ),

    MapFun = catena_types:tfun(
        InnerFun,
        catena_types:tfun(
            ListInt,
            ListString,
            catena_types:empty_effects()
        ),
        catena_types:empty_effects()
    ),

    Expected = "(integer -> string) -> List<integer> -> List<string>",
    ?assertEqual(Expected, catena_type_pp:pp_type(MapFun)).

test_pp_multiple_effects() ->
    % Int -> String / {file, io}
    Fun = catena_types:tfun(
        catena_types:tcon(integer),
        catena_types:tcon(string),
        catena_types:normalize_effects([io, file])  % Will be sorted
    ),
    ?assertEqual("integer -> string / {file, io}", catena_type_pp:pp_type(Fun)).

%%====================================================================
%% Record Type Tests
%%====================================================================

record_test_() ->
    [
      ?_test(test_pp_closed_record()),
      ?_test(test_pp_open_record()),
      ?_test(test_pp_empty_open_record())
    ].

test_pp_closed_record() ->
    % {x: Int, y: Float}
    Record = catena_types:trecord([
        {x, catena_types:tcon(integer)},
        {y, catena_types:tcon(float)}
    ], closed),
    ?assertEqual("{x: integer, y: float}", catena_type_pp:pp_type(Record)).

test_pp_open_record() ->
    % {name: String | ρ1}
    Record = catena_types:trecord([
        {name, catena_types:tcon(string)}
    ], 1),
    ?assertEqual("{name: string | ρ1}", catena_type_pp:pp_type(Record)).

test_pp_empty_open_record() ->
    % {| ρ1}
    Record = catena_types:trecord([], 1),
    ?assertEqual("{| ρ1}", catena_type_pp:pp_type(Record)).

%%====================================================================
%% Variant Type Tests
%%====================================================================

variant_test_() ->
    [
      ?_test(test_pp_simple_variant()),
      ?_test(test_pp_variant_with_args())
    ].

test_pp_simple_variant() ->
    % None | Some
    Variant = catena_types:tvariant([
        {'None', []},
        {'Some', [catena_types:tvar(1)]}
    ]),
    ?assertEqual("None | Some α1", catena_type_pp:pp_type(Variant)).

test_pp_variant_with_args() ->
    % Red | Green | Blue Int String
    Variant = catena_types:tvariant([
        {'Red', []},
        {'Green', []},
        {'Blue', [catena_types:tcon(integer), catena_types:tcon(string)]}
    ]),
    ?assertEqual("Red | Green | Blue integer string",
                catena_type_pp:pp_type(Variant)).

%%====================================================================
%% Type Scheme Tests
%%====================================================================

scheme_test_() ->
    [
      ?_test(test_pp_mono_scheme()),
      ?_test(test_pp_poly_scheme()),
      ?_test(test_pp_complex_poly_scheme())
    ].

test_pp_mono_scheme() ->
    Scheme = catena_type_scheme:mono(catena_types:tcon(integer)),
    ?assertEqual("integer", catena_type_pp:pp_scheme(Scheme)).

test_pp_poly_scheme() ->
    % ∀α1. α1 -> α1
    Type = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tvar(1),
        catena_types:empty_effects()
    ),
    Scheme = catena_type_scheme:poly([1], Type),
    ?assertEqual("∀α1. α1 -> α1", catena_type_pp:pp_scheme(Scheme)).

test_pp_complex_poly_scheme() ->
    % ∀α1 α2. (α1 -> α2) -> List<α1> -> List<α2>
    InnerFun = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tvar(2),
        catena_types:empty_effects()
    ),

    ListAlpha1 = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(1)]
    ),

    ListAlpha2 = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(2)]
    ),

    MapType = catena_types:tfun(
        InnerFun,
        catena_types:tfun(
            ListAlpha1,
            ListAlpha2,
            catena_types:empty_effects()
        ),
        catena_types:empty_effects()
    ),

    Scheme = catena_type_scheme:poly([1, 2], MapType),
    Expected = "∀α1 α2. (α1 -> α2) -> List<α1> -> List<α2>",
    ?assertEqual(Expected, catena_type_pp:pp_scheme(Scheme)).

%%====================================================================
%% Effect Set Tests
%%====================================================================

effects_test_() ->
    [
      ?_test(test_pp_empty_effects()),
      ?_test(test_pp_single_effect()),
      ?_test(test_pp_multiple_effects_sorted())
    ].

test_pp_empty_effects() ->
    Effects = catena_types:empty_effects(),
    ?assertEqual("", catena_type_pp:pp_effects(Effects)).

test_pp_single_effect() ->
    Effects = catena_types:singleton_effect(io),
    ?assertEqual("{io}", catena_type_pp:pp_effects(Effects)).

test_pp_multiple_effects_sorted() ->
    Effects = catena_types:normalize_effects([process, io, file]),
    % Should be sorted: file, io, process
    ?assertEqual("{file, io, process}", catena_type_pp:pp_effects(Effects)).

%%====================================================================
%% Complex Integration Tests
%%====================================================================

integration_test_() ->
    [
      ?_test(test_complex_nested_type())
    ].

test_complex_nested_type() ->
    % ∀α β. (α -> β / {io}) -> List<α> -> List<β> / {io}
    InnerFun = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tvar(2),
        catena_types:singleton_effect(io)
    ),

    ListAlpha = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(1)]
    ),

    ListBeta = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(2)]
    ),

    MapType = catena_types:tfun(
        InnerFun,
        catena_types:tfun(
            ListAlpha,
            ListBeta,
            catena_types:singleton_effect(io)
        ),
        catena_types:empty_effects()
    ),

    Scheme = catena_type_scheme:poly([1, 2], MapType),
    Expected = "∀α1 α2. (α1 -> α2 / {io}) -> List<α1> -> List<α2> / {io}",
    ?assertEqual(Expected, catena_type_pp:pp_scheme(Scheme)).
