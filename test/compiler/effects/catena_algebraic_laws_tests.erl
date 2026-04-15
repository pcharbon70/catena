%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_algebraic_laws (Phase 8.5)
%%%
%%% Tests for the algebraic laws library:
%%% - State effect laws
%%% - Reader effect laws
%%% - Writer effect laws
%%% - Error effect laws
%%% - Choice effect laws
%%% - Async effect laws
%%% - Monadic laws
%%% - Law set management
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_algebraic_laws_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Law Set Management Tests
%%%=============================================================================

new_law_set_test() ->
    Set = catena_algebraic_laws:new_law_set(test),
    ?assertEqual(test, maps:get(name, Set)),
    ?assertEqual(#{}, maps:get(equations, Set)).

get_law_set_state_test() ->
    Set = catena_algebraic_laws:get_law_set(state),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(Set, state_get_put)).

get_law_set_reader_test() ->
    Set = catena_algebraic_laws:get_law_set(reader),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(Set, reader_ask_local)).

get_law_set_writer_test() ->
    Set = catena_algebraic_laws:get_law_set(writer),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(Set, writer_tell_tell)).

get_law_set_error_test() ->
    Set = catena_algebraic_laws:get_law_set(error),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(Set, error_throw_catch)).

get_law_set_choice_test() ->
    Set = catena_algebraic_laws:get_law_set(choice),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(Set, choice_assoc)).

get_law_set_async_test() ->
    Set = catena_algebraic_laws:get_law_set(async),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(Set, async_spawn_await)).

get_law_set_monadic_test() ->
    Set = catena_algebraic_laws:get_law_set(monadic),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(Set, monad_left_id)).

get_law_set_all_test() ->
    Set = catena_algebraic_laws:get_law_set(all),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(Set, state_get_put)),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(Set, reader_ask_local)),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(Set, writer_tell_tell)).

get_law_set_empty_test() ->
    Set = catena_algebraic_laws:get_law_set(unknown),
    ?assertEqual([], catena_equation_spec:list_equations(Set)).

list_law_sets_test() ->
    Sets = catena_algebraic_laws:list_law_sets(),
    ?assert(lists:member(state, Sets)),
    ?assert(lists:member(reader, Sets)),
    ?assert(lists:member(writer, Sets)),
    ?assert(lists:member(error, Sets)),
    ?assert(lists:member(choice, Sets)),
    ?assert(lists:member(async, Sets)),
    ?assert(lists:member(monadic, Sets)),
    ?assert(lists:member(all, Sets)).

combine_law_sets_test() ->
    Set1 = catena_algebraic_laws:get_law_set(state),
    Set2 = catena_algebraic_laws:get_law_set(reader),
    Combined = catena_algebraic_laws:combine_law_sets(Set1, Set2),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(Combined, state_get_put)),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(Combined, reader_ask_local)).

%%%=============================================================================
%%% State Effect Laws Tests
%%%=============================================================================

state_laws_count_test() ->
    Set = catena_algebraic_laws:state_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assertEqual(4, length(Laws)).

state_laws_names_test() ->
    Set = catena_algebraic_laws:state_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assert(lists:member(state_get_put, Laws)),
    ?assert(lists:member(state_put_get, Laws)),
    ?assert(lists:member(state_put_put, Laws)),
    ?assert(lists:member(state_get_id, Laws)).

state_get_put_structure_test() ->
    Eq = catena_algebraic_laws:state_get_put(),
    LHS = catena_equations:lhs(Eq),
    RHS = catena_equations:rhs(Eq),
    % LHS should be a sequence containing get and bind with put
    ?assertMatch({seq, _}, LHS),
    % RHS should be a put operation
    ?assertMatch({op, put, _, _}, RHS).

state_put_get_structure_test() ->
    Eq = catena_algebraic_laws:state_put_get(),
    LHS = catena_equations:lhs(Eq),
    % LHS should be a sequence with put then get
    ?assertMatch({seq, _}, LHS).

state_put_put_structure_test() ->
    Eq = catena_algebraic_laws:state_put_put(),
    LHS = catena_equations:lhs(Eq),
    % LHS should have two puts
    ?assertMatch({seq, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({op, put, _, _}, RHS).

state_get_id_structure_test() ->
    Eq = catena_algebraic_laws:state_get_id(),
    LHS = catena_equations:lhs(Eq),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({seq, _}, LHS),
    ?assertMatch({op, get, _, _}, RHS).

%%%=============================================================================
%%% Reader Effect Laws Tests
%%%=============================================================================

reader_laws_count_test() ->
    Set = catena_algebraic_laws:reader_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assertEqual(3, length(Laws)).

reader_laws_names_test() ->
    Set = catena_algebraic_laws:reader_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assert(lists:member(reader_ask_local, Laws)),
    ?assert(lists:member(reader_local_local, Laws)),
    ?assert(lists:member(reader_local_id, Laws)).

reader_ask_local_structure_test() ->
    Eq = catena_algebraic_laws:reader_ask_local(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({seq, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({op, local, _, _}, RHS).

reader_local_local_structure_test() ->
    Eq = catena_algebraic_laws:reader_local_local(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({seq, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({op, local, _, _}, RHS).

reader_local_id_structure_test() ->
    Eq = catena_algebraic_laws:reader_local_id(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({seq, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({var, _}, RHS).

%%%=============================================================================
%%% Writer Effect Laws Tests
%%%=============================================================================

writer_laws_count_test() ->
    Set = catena_algebraic_laws:writer_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assertEqual(3, length(Laws)).

writer_laws_names_test() ->
    Set = catena_algebraic_laws:writer_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assert(lists:member(writer_tell_tell, Laws)),
    ?assert(lists:member(writer_tell_unit, Laws)),
    ?assert(lists:member(writer_censor, Laws)).

writer_tell_tell_structure_test() ->
    Eq = catena_algebraic_laws:writer_tell_tell(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({seq, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({op, tell, _, _}, RHS).

writer_tell_unit_structure_test() ->
    Eq = catena_algebraic_laws:writer_tell_unit(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({op, tell, _, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({seq, _}, RHS).

writer_censor_structure_test() ->
    Eq = catena_algebraic_laws:writer_censor(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({seq, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({var, _}, RHS).

%%%=============================================================================
%%% Error Effect Laws Tests
%%%=============================================================================

error_laws_count_test() ->
    Set = catena_algebraic_laws:error_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assertEqual(3, length(Laws)).

error_laws_names_test() ->
    Set = catena_algebraic_laws:error_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assert(lists:member(error_throw_catch, Laws)),
    ?assert(lists:member(error_catch_identity, Laws)),
    ?assert(lists:member(error_throw_map, Laws)).

error_throw_catch_structure_test() ->
    Eq = catena_algebraic_laws:error_throw_catch(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({seq, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({op, throw, _, _}, RHS).

error_catch_identity_structure_test() ->
    Eq = catena_algebraic_laws:error_catch_identity(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({op, 'catch', _, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({var, _}, RHS).

error_throw_map_structure_test() ->
    Eq = catena_algebraic_laws:error_throw_map(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({op, map, _, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({op, throw, _, _}, RHS).

%%%=============================================================================
%%% Choice Effect Laws Tests
%%%=============================================================================

choice_laws_count_test() ->
    Set = catena_algebraic_laws:choice_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assertEqual(4, length(Laws)).

choice_laws_names_test() ->
    Set = catena_algebraic_laws:choice_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assert(lists:member(choice_assoc, Laws)),
    ?assert(lists:member(choice_left_id, Laws)),
    ?assert(lists:member(choice_right_id, Laws)),
    ?assert(lists:member(choice_comm, Laws)).

choice_assoc_structure_test() ->
    Eq = catena_algebraic_laws:choice_assoc(),
    LHS = catena_equations:lhs(Eq),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({op, alt, _, _}, LHS),
    ?assertMatch({op, alt, _, _}, RHS).

choice_left_id_structure_test() ->
    Eq = catena_algebraic_laws:choice_left_id(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({op, alt, _, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({var, _}, RHS).

choice_right_id_structure_test() ->
    Eq = catena_algebraic_laws:choice_right_id(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({op, alt, _, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({var, _}, RHS).

choice_comm_structure_test() ->
    Eq = catena_algebraic_laws:choice_comm(),
    LHS = catena_equations:lhs(Eq),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({op, alt, _, _}, LHS),
    ?assertMatch({op, alt, _, _}, RHS).

%%%=============================================================================
%%% Async Effect Laws Tests
%%%=============================================================================

async_laws_count_test() ->
    Set = catena_algebraic_laws:async_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assertEqual(3, length(Laws)).

async_laws_names_test() ->
    Set = catena_algebraic_laws:async_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assert(lists:member(async_spawn_await, Laws)),
    ?assert(lists:member(async_map, Laws)),
    ?assert(lists:member(async_assoc, Laws)).

async_spawn_await_structure_test() ->
    Eq = catena_algebraic_laws:async_spawn_await(),
    LHS = catena_equations:lhs(Eq),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({seq, _}, LHS),
    ?assertMatch({seq, _}, RHS).

async_map_structure_test() ->
    Eq = catena_algebraic_laws:async_map(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({op, map, _, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({op, spawn, _, _}, RHS).

async_assoc_structure_test() ->
    Eq = catena_algebraic_laws:async_assoc(),
    LHS = catena_equations:lhs(Eq),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({op, spawn, _, _}, LHS),
    ?assertMatch({op, spawn, _, _}, RHS).

%%%=============================================================================
%%% Monadic Laws Tests
%%%=============================================================================

monadic_laws_count_test() ->
    Set = catena_algebraic_laws:monadic_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assertEqual(3, length(Laws)).

monadic_laws_names_test() ->
    Set = catena_algebraic_laws:monadic_laws(),
    Laws = catena_equation_spec:list_equations(Set),
    ?assert(lists:member(monad_left_id, Laws)),
    ?assert(lists:member(monad_right_id, Laws)),
    ?assert(lists:member(monad_assoc, Laws)).

monad_left_id_structure_test() ->
    Eq = catena_algebraic_laws:monad_left_id(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({seq, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({op, apply, _, _}, RHS).

monad_right_id_structure_test() ->
    Eq = catena_algebraic_laws:monad_right_id(),
    LHS = catena_equations:lhs(Eq),
    ?assertMatch({seq, _}, LHS),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({var, _}, RHS).

monad_assoc_structure_test() ->
    Eq = catena_algebraic_laws:monad_assoc(),
    LHS = catena_equations:lhs(Eq),
    RHS = catena_equations:rhs(Eq),
    ?assertMatch({seq, _}, LHS),
    ?assertMatch({seq, _}, RHS).

%%%=============================================================================
%%% Law Validation Tests
%%%=============================================================================

validate_state_laws_test() ->
    Set = catena_algebraic_laws:state_laws(),
    ?assertEqual(ok, catena_equation_spec:validate_set(Set)).

validate_reader_laws_test() ->
    Set = catena_algebraic_laws:reader_laws(),
    ?assertEqual(ok, catena_equation_spec:validate_set(Set)).

validate_writer_laws_test() ->
    Set = catena_algebraic_laws:writer_laws(),
    ?assertEqual(ok, catena_equation_spec:validate_set(Set)).

validate_error_laws_test() ->
    Set = catena_algebraic_laws:error_laws(),
    ?assertEqual(ok, catena_equation_spec:validate_set(Set)).

validate_choice_laws_test() ->
    Set = catena_algebraic_laws:choice_laws(),
    ?assertEqual(ok, catena_equation_spec:validate_set(Set)).

validate_async_laws_test() ->
    Set = catena_algebraic_laws:async_laws(),
    ?assertEqual(ok, catena_equation_spec:validate_set(Set)).

validate_monadic_laws_test() ->
    Set = catena_algebraic_laws:monadic_laws(),
    ?assertEqual(ok, catena_equation_spec:validate_set(Set)).

validate_all_laws_test() ->
    Set = catena_algebraic_laws:get_law_set(all),
    ?assertEqual(ok, catena_equation_spec:validate_set(Set)).

%%%=============================================================================
%%% Law Application Tests
%%%=============================================================================

% NOTE: Full equation application is tested in integration tests.
% These tests verify that laws are well-formed and can be used.

apply_state_put_put_test() ->
    % Test that state_put_put law is well-formed
    Eq = catena_algebraic_laws:state_put_put(),
    Set = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(Set, test_law, Eq),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(SetWithEq, test_law)).

apply_monad_left_id_test() ->
    % Test that monad_left_id law is well-formed
    Eq = catena_algebraic_laws:monad_left_id(),
    Set = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(Set, test_law, Eq),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(SetWithEq, test_law)).

apply_choice_left_id_test() ->
    % Test that choice_left_id law is well-formed
    Eq = catena_algebraic_laws:choice_left_id(),
    Set = catena_equation_spec:new_set(test),
    SetWithEq = catena_equation_spec:add_equation(Set, test_law, Eq),
    ?assertMatch({ok, _}, catena_equation_spec:get_equation(SetWithEq, test_law)).
