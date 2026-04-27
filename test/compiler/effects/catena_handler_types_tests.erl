-module(catena_handler_types_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_handler_types_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"handler type constructors", fun test_handler_constructors/0},
        {"operation signature constructors", fun test_operation_constructors/0},
        {"handler type builders", fun test_handler_builders/0},
        {"handler type predicates", fun test_predicates/0},
        {"handler type validation", fun test_validation/0},
        {"handler type composition", fun test_composition/0},
        {"format handler type", fun test_format_handler/0},
        {"format operation signature", fun test_format_operation/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Constructor Tests
%%%---------------------------------------------------------------------

test_handler_constructors() ->
    Empty = catena_handler_types:handler_type(),
    ?assert(catena_handler_types:is_handler_type(Empty)),
    ?assertEqual({type_var, input}, maps:get(input, Empty)),
    ?assertEqual({type_var, output}, maps:get(output, Empty)),
    ?assertEqual(#{total => true, deep => false, pure => false}, maps:get(constraints, Empty)),

    Named = catena_handler_types:handler_type(my_handler),
    ?assertEqual(my_handler, maps:get(name, Named)),

    AtomType = atom,
    WithTypes = catena_handler_types:handler_type(test, AtomType, AtomType),
    ?assertEqual(AtomType, maps:get(input, WithTypes)),
    ?assertEqual(AtomType, maps:get(output, WithTypes)).

test_operation_constructors() ->
    Empty = catena_handler_types:operation_sig(),
    ?assert(catena_handler_types:is_operation_sig(Empty)),
    ?assertEqual({type_var, result}, maps:get(result, Empty)),

    IntType = int,
    WithParams = catena_handler_types:operation_sig([atom, IntType], IntType),
    ?assertEqual([atom, IntType], maps:get(params, WithParams)),
    ?assertEqual(IntType, maps:get(result, WithParams)),

    Full = catena_handler_types:operation_sig([atom], atom, catena_row_types:effect_row([a, b])),
    ?assertEqual([atom], maps:get(params, Full)),
    ?assertEqual(atom, maps:get(result, Full)),
    ?assert(catena_row_types:is_effect_row(maps:get(effects, Full))).

test_handler_builders() ->
    Handler = catena_handler_types:handler_type(),
    IntType = int,
    AtomType = atom,

    Ops = #{
        get => catena_handler_types:operation_sig([], IntType),
        put => catena_handler_types:operation_sig([IntType], AtomType)
    },

    WithOps = catena_handler_types:with_operations(Handler, Ops),
    ?assertEqual(Ops, maps:get(operations, WithOps)),

    WithInput = catena_handler_types:with_input(Handler, list),
    ?assertEqual(list, maps:get(input, WithInput)),

    WithOutput = catena_handler_types:with_output(Handler, bool),
    ?assertEqual(bool, maps:get(output, WithOutput)),

    Effects = catena_row_types:effect_row([a, b]),
    WithEffects = catena_handler_types:with_effects(Handler, Effects),
    ?assertEqual(Effects, maps:get(effects, WithEffects)).

%%%---------------------------------------------------------------------
%%% Predicate Tests
%%%---------------------------------------------------------------------

test_predicates() ->
    Handler = catena_handler_types:handler_type(),
    ?assert(catena_handler_types:is_handler_type(Handler)),
    ?assertNot(catena_handler_types:is_handler_type(#{})),

    OpSig = catena_handler_types:operation_sig(),
    ?assert(catena_handler_types:is_operation_sig(OpSig)),
    ?assertNot(catena_handler_types:is_operation_sig(#{})).

%%%---------------------------------------------------------------------
%%% Validation Tests
%%%---------------------------------------------------------------------

test_validation() ->
    %% Valid handler with operations
    Ops = #{
        op1 => catena_handler_types:operation_sig([atom], atom)
    },
    ValidHandler = catena_handler_types:with_operations(
        catena_handler_types:handler_type(), Ops),
    ?assert(catena_handler_types:is_valid_handler_type(ValidHandler)),

    %% Invalid handler without operations
    EmptyHandler = catena_handler_types:handler_type(),
    ?assertNot(catena_handler_types:is_valid_handler_type(EmptyHandler)),

    %% Valid operation signature
    ValidOp = catena_handler_types:operation_sig([atom], atom),
    ?assert(catena_handler_types:is_valid_operation_sig(ValidOp)),

    %% Invalid handler constraints
    InvalidConstraints = ValidHandler#{constraints => #{total => 'maybe', deep => false, pure => false}},
    ?assertNot(catena_handler_types:is_valid_handler_type(InvalidConstraints)).

%%%---------------------------------------------------------------------
%%% Composition Tests
%%%---------------------------------------------------------------------

test_composition() ->
    IntType = int,
    AtomType = atom,

    Ops1 = #{get => catena_handler_types:operation_sig([], IntType)},
    Ops2 = #{put => catena_handler_types:operation_sig([IntType], AtomType)},

    H1 = catena_handler_types:with_output(
        catena_handler_types:with_operations(
            catena_handler_types:handler_type(h1), Ops1), IntType),
    H2 = catena_handler_types:with_input(
        catena_handler_types:with_operations(
            catena_handler_types:handler_type(h2), Ops2), IntType),

    {ok, Composed} = catena_handler_types:compose_handler_types(H1, H2),
    ComposedOps = maps:get(operations, Composed),
    ?assertEqual(2, maps:size(ComposedOps)),
    ?assert(maps:is_key(get, ComposedOps)),
    ?assert(maps:is_key(put, ComposedOps)),
    ?assertEqual(#{total => true, deep => false, pure => false}, maps:get(constraints, Composed)),

    %% Type mismatch case
    H3 = catena_handler_types:with_output(
        catena_handler_types:handler_type(h3), AtomType),
    H4 = catena_handler_types:with_input(
        catena_handler_types:handler_type(h4), IntType),

    ?assertMatch({error, _}, catena_handler_types:compose_handler_types(H3, H4)).

%%%---------------------------------------------------------------------
%%% Format Tests
%%%---------------------------------------------------------------------

test_format_handler() ->
    IntType = int,
    Ops = #{get => catena_handler_types:operation_sig([], IntType)},
    Handler = catena_handler_types:with_effects(
        catena_handler_types:with_output(
            catena_handler_types:with_input(
                catena_handler_types:with_operations(
                    catena_handler_types:handler_type(state), Ops),
                IntType),
            IntType),
        catena_row_types:effect_row([])),

    Formatted = catena_handler_types:format_handler_type(Handler),
    ?assert(<<>> /= Formatted),
    ?assert(is_binary(Formatted)).

test_format_operation() ->
    IntType = int,
    AtomType = atom,
    OpSig = catena_handler_types:operation_sig([IntType], AtomType),
    Formatted = catena_handler_types:format_operation_sig(OpSig),
    ?assert(<<>> /= Formatted),
    ?assert(is_binary(Formatted)),
    % Check that key patterns exist in the formatted output
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"->">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"/">>)).
