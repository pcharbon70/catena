%% @doc Stateful Property Testing - State Machine DSL
%%
%% This module provides the core types and behavior for stateful property testing.
%% Stateful testing generates sequences of commands that modify state and verifies
%% invariants hold throughout. The two-phase approach (symbolic then concrete)
%% enables shrinking of command sequences.
%%
%% @end
-module(catena_statem).

-include("catena_statem.hrl").

%%====================================================================
%% Type Exports
%%====================================================================

-export_type([
    state_machine/0,
    command/0,
    command_result/0,
    symbolic_var/0,
    var_binding/0
]).

%%====================================================================
%% Behavior Callbacks
%%====================================================================

%% State machine implementations must provide these callbacks:
%%
%% - initial_state() -> state()
%%   Returns the starting state for the state machine.
%%
%% - command(state()) -> [command_gen()]
%%   Returns list of possible command generators for the given state.
%%
%% - precondition(state(), symbolic_command()) -> boolean()
%%   Returns true if the command is valid in the current state.
%%
%% - next_state(state(), var_bindings(), symbolic_command()) -> state()
%%   Returns the updated state after executing the command.
%%
%% - postcondition(state(), symbolic_command(), command_result()) -> boolean()
%%   Returns true if the postcondition holds after command execution.
%%
%% Optional callbacks:
%%
%% - setup() -> {ok, system()} | {error, term()}
%%   Initializes the system under test.
%%
%% - cleanup(system()) -> ok
%%   Tears down the system after testing.
%%
%% - invariants() -> [invariant_spec()]
%%   Returns list of invariants that must always hold.

%%====================================================================
%% API Exports
%%====================================================================

-export([
    state_machine/3,
    validate/1,
    extract_commands/2,
    extract_invariants/1
]).

%%====================================================================
%% Section 5.1.3: State Model
%%====================================================================

-export([
    get_initial_state/1,
    state_get/2,
    state_put/3,
    state_update/3
]).

%%====================================================================
%% Section 5.1.4: Invariants
%%====================================================================

-export([
    check_invariants/2,
    format_invariant_failure/1
]).

%%====================================================================
%% Default Options
%%====================================================================

-export([
    default_options/0
]).

%%====================================================================
%% Type Exports
%%====================================================================

%% @doc A state machine specification.
-type state_machine() :: #state_machine{}.

%% @doc A command specification.
-type command() :: #command{}.

%% @doc Command generator specification.
-type command_gen() :: #command_gen{}.
-type command_spec() :: #command_gen{}.

%% @doc Symbolic command with variable bindings.
%%
%% Represents a command with symbolic (not yet executed) arguments.
-type symbolic_command() :: {call, pid() | atom(), atom(), [symbolic_arg()]} | {call, atom(), [symbolic_arg()]}.

%% @doc Symbolic argument - can be a literal or a variable reference.
-type symbolic_arg() :: term() | {var, symbolic_var()}.

%% @doc Symbolic variable representing a command result.
-type symbolic_var() :: {var, pos_integer()}.

%% @doc Variable binding from symbolic to concrete value.
-type var_binding() :: {symbolic_var(), command_result()}.
-type var_bindings() :: [var_binding()].

%% @doc Result of executing a command.
-type command_result() :: term().

%% @doc Abstract state representation.
%% State can be any Erlang term - maps, records, or primitives.
-type state() :: term().

%% @doc System under test - typically a pid or reference.
-type system() :: pid() | term().

%% @doc Invariant specification.
-type invariant_spec() :: #invariant{}.

%% @doc Options for state machine execution.
-type options() :: #options{}.

%% @doc Argument generator - produces command arguments.
-type args_generator() :: catena_gen:generator([symbolic_arg()]).

%% @doc Precondition function - checks if command is valid in current state.
-type precondition() :: fun((state()) -> boolean()).

%%====================================================================
%% Section 5.1.1: State Machine Type Definition
%%====================================================================

%% @doc Create a new state machine specification.
%%
%% @param Name Human-readable name for this state machine
%% @param Module Callback module implementing the statem behavior
%% @param Options Configuration options
-spec state_machine(atom() | binary(), module(), options()) -> state_machine().
state_machine(Name, Module, Options) ->
    InitialState = Module:initial_state(),
    Commands = extract_commands(Module, InitialState),
    Invariants = extract_invariants(Module),
    #state_machine{
        name = Name,
        module = Module,
        initial_state = InitialState,
        commands = Commands,
        invariants = Invariants,
        options = Options
    }.

%% @doc Extract command specifications from the callback module.
-spec extract_commands(module(), state()) -> [command_spec()].
extract_commands(Module, State) ->
    Module:command(State).

%% @doc Extract invariant specifications from the callback module.
-spec extract_invariants(module()) -> [invariant_spec()].
extract_invariants(Module) ->
    case erlang:function_exported(Module, invariants, 0) of
        true -> Module:invariants();
        false -> []
    end.

%% @doc Validate a state machine specification.
%%
%% Checks that all required callbacks are exported and valid.
-spec validate(state_machine()) -> ok | {error, [term()]}.
validate(#state_machine{module = Module}) ->
    CheckCallback = fun({Func, Arity}) ->
        case erlang:function_exported(Module, Func, Arity) of
            true -> false;
            false -> {true, {missing_callback, Func, Arity}}
        end
    end,
    Errors = lists:filtermap(CheckCallback, [
        {initial_state, 0},
        {command, 1},
        {precondition, 2},
        {next_state, 3},
        {postcondition, 3}
    ]),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

%%====================================================================
%% Section 5.1.3: State Model
%%====================================================================

%% @doc Get the initial state from a state machine.
-spec get_initial_state(state_machine()) -> state().
get_initial_state(#state_machine{initial_state = State}) ->
    State.

%% @doc Access state using a key (for map-based states).
-spec state_get(term(), state()) -> term().
state_get(Key, State) when is_map(State) ->
    maps:get(Key, State);
state_get(Key, State) when is_tuple(State) ->
    element(Key, State);
state_get(Key, State) when is_list(State) ->
    proplists:get_value(Key, State).

%% @doc Update state using a key (for map-based states).
-spec state_put(term(), term(), state()) -> state().
state_put(Key, Value, State) when is_map(State) ->
    maps:put(Key, Value, State);
state_put(Key, Value, State) when is_list(State) ->
    lists:keystore(Key, 1, State, {Key, Value});
state_put(_Key, _Value, State) ->
    State.

%% @doc Update state using a function (for map-based states).
-spec state_update(term(), fun((term()) -> term()), state()) -> state().
state_update(Key, Fun, State) when is_map(State) ->
    maps:update_with(Key, Fun, State);
state_update(Key, Fun, State) when is_list(State) ->
    {value, {Key, Value}} = lists:keysearch(Key, 1, State),
    lists:keyreplace(Key, 1, State, {Key, Fun(Value)});
state_update(_Key, _Fun, State) ->
    State.

%%====================================================================
%% Section 5.1.4: Invariants
%%====================================================================

%% @doc Check all invariants for a given state.
%%
%% Returns ok if all invariants pass, otherwise {error, {invariant_failed, Name}}.
-spec check_invariants([invariant_spec()], state()) -> ok | {error, {invariant_failed, term()}}.
check_invariants(Invariants, State) ->
    check_invariants(Invariants, State, []).

check_invariants([], _State, []) ->
    ok;
check_invariants([], _State, Errors) ->
    %% Return first error for simplicity
    [FirstError | _] = lists:reverse(Errors),
    {error, {invariant_failed, FirstError}};
check_invariants([#invariant{name = Name, check = Check, when_fun = WhenFun} | Rest], State, Errors) ->
    ShouldCheck = case WhenFun of
        undefined -> true;
        Fun -> Fun(State)
    end,
    case ShouldCheck of
        false ->
            %% Don't check this invariant, continue
            check_invariants(Rest, State, Errors);
        true ->
            case Check(State) of
                true -> check_invariants(Rest, State, Errors);
                false -> check_invariants(Rest, State, [{Name, State} | Errors])
            end
    end;
check_invariants([_ | Rest], State, Errors) ->
    check_invariants(Rest, State, Errors).

%% @doc Format invariant failure for error reporting.
-spec format_invariant_failure({term(), state()}) -> iolist().
format_invariant_failure({Name, State}) ->
    io_lib:format("Invariant ~p failed with state: ~p~n", [Name, State]).

%%====================================================================
%% Default Options
%%====================================================================

%% @doc Default options for state machine testing.
-spec default_options() -> options().
default_options() ->
    #options{
        num_commands = 100,
        max_shrinks = 1000,
        timeout = 5000,
        parallel_tracks = 2
    }.
