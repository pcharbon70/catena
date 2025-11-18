%%%-------------------------------------------------------------------
%%% @doc Unit Tests for catena_types module
%%% @end
%%%-------------------------------------------------------------------
-module(catena_types_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Type Construction Tests
%%====================================================================

type_construction_test_() ->
    [
     ?_test(test_tvar()),
     ?_test(test_tcon()),
     ?_test(test_tapp()),
     ?_test(test_tfun()),
     ?_test(test_trecord()),
     ?_test(test_ttuple()),
     ?_test(test_tvariant())
    ].

test_tvar() ->
    Var = catena_types:tvar(1),
    ?assertEqual({tvar, 1}, Var),

    Var2 = catena_types:tvar(42),
    ?assertEqual({tvar, 42}, Var2).

test_tcon() ->
    IntType = catena_types:tcon(integer),
    ?assertEqual({tcon, integer}, IntType),

    BoolType = catena_types:tcon(boolean),
    ?assertEqual({tcon, boolean}, BoolType),

    CustomType = catena_types:tcon('MyType'),
    ?assertEqual({tcon, 'MyType'}, CustomType).

test_tapp() ->
    % List Int
    ListInt = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tcon(integer)]
    ),
    ?assertMatch({tapp, {tcon, 'List'}, [{tcon, integer}]}, ListInt),

    % Maybe String
    MaybeString = catena_types:tapp(
        catena_types:tcon('Maybe'),
        [catena_types:tcon(string)]
    ),
    ?assertMatch({tapp, {tcon, 'Maybe'}, [{tcon, string}]}, MaybeString).

test_tfun() ->
    % Int -> Int (pure)
    IntToInt = catena_types:tfun(
        catena_types:tcon(integer),
        catena_types:tcon(integer),
        catena_types:empty_effects()
    ),
    ?assertMatch({tfun, {tcon, integer}, {tcon, integer}, {effect_set, []}}, IntToInt),

    % String -> Unit / {IO}
    EffectfulFun = catena_types:tfun(
        catena_types:tcon(string),
        catena_types:tcon(unit),
        catena_types:singleton_effect(io)
    ),
    ?assertMatch({tfun, {tcon, string}, {tcon, unit}, {effect_set, [io]}}, EffectfulFun).

test_trecord() ->
    % {x: Int, y: Float} (closed)
    ClosedRecord = catena_types:trecord(
        [{x, catena_types:tcon(integer)}, {y, catena_types:tcon(float)}],
        closed
    ),
    ?assertMatch({trecord, [{x, {tcon, integer}}, {y, {tcon, float}}], closed}, ClosedRecord),

    % {name: String | ρ} (open with row variable)
    OpenRecord = catena_types:trecord(
        [{name, catena_types:tcon(string)}],
        1  % Row variable ID
    ),
    ?assertMatch({trecord, [{name, {tcon, string}}], 1}, OpenRecord).

test_ttuple() ->
    % (Int, String)
    Tuple2 = catena_types:ttuple([
        catena_types:tcon(integer),
        catena_types:tcon(string)
    ]),
    ?assertMatch({ttuple, [{tcon, integer}, {tcon, string}]}, Tuple2),

    % ()
    EmptyTuple = catena_types:ttuple([]),
    ?assertMatch({ttuple, []}, EmptyTuple).

test_tvariant() ->
    % Maybe a = Some a | None
    MaybeVariant = catena_types:tvariant([
        {'Some', [catena_types:tvar(1)]},
        {'None', []}
    ]),
    ?assertMatch({tvariant, [{'Some', [{tvar, 1}]}, {'None', []}]}, MaybeVariant).

%%====================================================================
%% Fresh Variable Generation Tests
%%====================================================================

fresh_var_test_() ->
    [
     ?_test(test_fresh_var_unique()),
     ?_test(test_fresh_var_sequential()),
     ?_test(test_fresh_var_state_independence())
    ].

test_fresh_var_unique() ->
    State0 = catena_infer_state:new(),
    {Var1, State1} = catena_types:fresh_var(State0),
    {Var2, State2} = catena_types:fresh_var(State1),
    {Var3, _State3} = catena_types:fresh_var(State2),

    % All should be different
    ?assert(Var1 =/= Var2),
    ?assert(Var2 =/= Var3),
    ?assert(Var1 =/= Var3),

    % Should all be type variables
    ?assertMatch({tvar, _}, Var1),
    ?assertMatch({tvar, _}, Var2),
    ?assertMatch({tvar, _}, Var3).

test_fresh_var_sequential() ->
    State0 = catena_infer_state:new(),

    {Var1, State1} = catena_types:fresh_var(State0),
    {Var2, State2} = catena_types:fresh_var(State1),
    {Var3, _State3} = catena_types:fresh_var(State2),

    % Should be sequential
    ?assertMatch({tvar, 1}, Var1),
    ?assertMatch({tvar, 2}, Var2),
    ?assertMatch({tvar, 3}, Var3).

test_fresh_var_state_independence() ->
    % Create two independent state threads
    StateA0 = catena_infer_state:new(),
    StateB0 = catena_infer_state:new(),

    {VarA1, StateA1} = catena_types:fresh_var(StateA0),
    {VarB1, StateB1} = catena_types:fresh_var(StateB0),
    {VarA2, _StateA2} = catena_types:fresh_var(StateA1),
    {VarB2, _StateB2} = catena_types:fresh_var(StateB1),

    % Both threads should start at 1 (independent)
    ?assertMatch({tvar, 1}, VarA1),
    ?assertMatch({tvar, 1}, VarB1),
    ?assertMatch({tvar, 2}, VarA2),
    ?assertMatch({tvar, 2}, VarB2).

%%====================================================================
%% Effect Set Operations Tests
%%====================================================================

effect_set_test_() ->
    [
     ?_test(test_empty_effects()),
     ?_test(test_singleton_effect()),
     ?_test(test_union_effects()),
     ?_test(test_normalize_effects()),
     ?_test(test_is_pure()),
     ?_test(test_effects_equal())
    ].

test_empty_effects() ->
    Empty = catena_types:empty_effects(),
    ?assertEqual({effect_set, []}, Empty),
    ?assert(catena_types:is_pure(Empty)).

test_singleton_effect() ->
    IO = catena_types:singleton_effect(io),
    ?assertEqual({effect_set, [io]}, IO),
    ?assertNot(catena_types:is_pure(IO)),

    Process = catena_types:singleton_effect(process),
    ?assertEqual({effect_set, [process]}, Process).

test_union_effects() ->
    IO = catena_types:singleton_effect(io),
    Process = catena_types:singleton_effect(process),

    % Union of two effects
    Union = catena_types:union_effects(IO, Process),
    ?assertEqual({effect_set, [io, process]}, Union),  % Sorted

    % Union with empty
    Empty = catena_types:empty_effects(),
    UnionEmpty = catena_types:union_effects(IO, Empty),
    ?assertEqual(IO, UnionEmpty),

    % Union with self (should deduplicate)
    SelfUnion = catena_types:union_effects(IO, IO),
    ?assertEqual(IO, SelfUnion).

test_normalize_effects() ->
    % Unordered list
    Normalized = catena_types:normalize_effects([process, io, file]),
    ?assertEqual({effect_set, [file, io, process]}, Normalized),  % Sorted

    % With duplicates
    NormalizedDups = catena_types:normalize_effects([io, process, io, file, process]),
    ?assertEqual({effect_set, [file, io, process]}, NormalizedDups),

    % Empty list
    NormalizedEmpty = catena_types:normalize_effects([]),
    ?assertEqual({effect_set, []}, NormalizedEmpty).

test_is_pure() ->
    ?assert(catena_types:is_pure(catena_types:empty_effects())),
    ?assertNot(catena_types:is_pure(catena_types:singleton_effect(io))),

    Multiple = catena_types:normalize_effects([io, process]),
    ?assertNot(catena_types:is_pure(Multiple)).

test_effects_equal() ->
    IO1 = catena_types:singleton_effect(io),
    IO2 = catena_types:singleton_effect(io),
    ?assert(catena_types:effects_equal(IO1, IO2)),

    Process = catena_types:singleton_effect(process),
    ?assertNot(catena_types:effects_equal(IO1, Process)),

    Empty1 = catena_types:empty_effects(),
    Empty2 = catena_types:empty_effects(),
    ?assert(catena_types:effects_equal(Empty1, Empty2)).

%%====================================================================
%% Type Operations Tests
%%====================================================================

type_operations_test_() ->
    [
     ?_test(test_is_function_type()),
     ?_test(test_is_type_var()),
     ?_test(test_extract_function_effects()),
     ?_test(test_type_vars()),
     ?_test(test_type_vars_depth_limit())
    ].

test_is_function_type() ->
    Fun = catena_types:tfun(
        catena_types:tcon(integer),
        catena_types:tcon(integer),
        catena_types:empty_effects()
    ),
    ?assert(catena_types:is_function_type(Fun)),

    NotFun = catena_types:tcon(integer),
    ?assertNot(catena_types:is_function_type(NotFun)).

test_is_type_var() ->
    Var = catena_types:tvar(1),
    ?assert(catena_types:is_type_var(Var)),

    NotVar = catena_types:tcon(integer),
    ?assertNot(catena_types:is_type_var(NotVar)).

test_extract_function_effects() ->
    Effects = catena_types:singleton_effect(io),
    Fun = catena_types:tfun(
        catena_types:tcon(string),
        catena_types:tcon(unit),
        Effects
    ),
    ?assertEqual({ok, Effects}, catena_types:extract_function_effects(Fun)),

    NotFun = catena_types:tcon(integer),
    ?assertEqual(error, catena_types:extract_function_effects(NotFun)).

test_type_vars() ->
    % Type variable α
    Var1 = catena_types:tvar(1),
    Vars1 = catena_types:type_vars(Var1),
    ?assertEqual([1], lists:sort(sets:to_list(Vars1))),

    % Function α -> β
    Fun = catena_types:tfun(
        catena_types:tvar(1),
        catena_types:tvar(2),
        catena_types:empty_effects()
    ),
    Vars2 = catena_types:type_vars(Fun),
    ?assertEqual([1, 2], lists:sort(sets:to_list(Vars2))),

    % No variables in concrete type
    Concrete = catena_types:tcon(integer),
    Vars3 = catena_types:type_vars(Concrete),
    ?assertEqual([], lists:sort(sets:to_list(Vars3))),

    % Tuple (α, β, α) - α appears twice
    Tuple = catena_types:ttuple([
        catena_types:tvar(1),
        catena_types:tvar(2),
        catena_types:tvar(1)
    ]),
    Vars4 = catena_types:type_vars(Tuple),
    ?assertEqual([1, 2], lists:sort(sets:to_list(Vars4))),  % De-duplicated

    % Record {x: α | ρ}
    Record = catena_types:trecord(
        [{x, catena_types:tvar(1)}],
        3  % Row variable
    ),
    Vars5 = catena_types:type_vars(Record),
    ?assertEqual([1, 3], lists:sort(sets:to_list(Vars5))).

test_type_vars_depth_limit() ->
    % Build a deeply nested type: List<List<List<...Int...>>>
    % This should trigger the depth limit and raise an error
    DeepType = build_deeply_nested_list_type(150, catena_types:tcon(integer)),

    % Should raise type_depth_exceeded error
    ?assertError(
        {type_depth_exceeded, _, _},
        catena_types:type_vars(DeepType)
    ).

%% Helper to build deeply nested types for testing
build_deeply_nested_list_type(0, BaseType) ->
    BaseType;
build_deeply_nested_list_type(Depth, BaseType) ->
    InnerType = build_deeply_nested_list_type(Depth - 1, BaseType),
    catena_types:tapp(catena_types:tcon('List'), [InnerType]).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
     ?_test(test_complex_type_construction())
    ].

test_complex_type_construction() ->
    % Build: (α -> β / {IO}) -> List α -> List β / {IO}

    % α -> β / {IO}
    InnerFun = catena_types:tfun(
        catena_types:tvar(1),  % α
        catena_types:tvar(2),  % β
        catena_types:singleton_effect(io)
    ),

    % List α
    ListAlpha = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(1)]
    ),

    % List β
    ListBeta = catena_types:tapp(
        catena_types:tcon('List'),
        [catena_types:tvar(2)]
    ),

    % (α -> β / {IO}) -> List α -> List β / {IO}
    MapType = catena_types:tfun(
        InnerFun,
        catena_types:tfun(
            ListAlpha,
            ListBeta,
            catena_types:singleton_effect(io)
        ),
        catena_types:empty_effects()  % Outer function is pure
    ),

    % Verify structure
    ?assertMatch({tfun, _, _, {effect_set, []}}, MapType),

    % Extract type variables (should be {1, 2})
    Vars = catena_types:type_vars(MapType),
    ?assertEqual([1, 2], lists:sort(sets:to_list(Vars))).
