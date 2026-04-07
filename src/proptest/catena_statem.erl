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
%% Section 5.2: Command Generation
%%====================================================================

-export([
    generate_command/2,
    generate_commands/3,
    weight_select/1,
    filter_valid_commands/2,
    shrink_command_seq/2,
    simulate_state/2
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

%% @doc Command sequence for testing.
-type command_seq() :: [symbolic_command()].

%% @doc Command generator result.
-type gen_result(T) :: {T, catena_gen:seed()} | {error, term()}.

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

%%====================================================================
%% Section 5.2: Command Generation
%%====================================================================

%% @doc Generate a single command from the state machine specification.
%%
%% Selects a command based on weights and generates its arguments.
-spec generate_command(state_machine(), state()) -> symbolic_command().
generate_command(#state_machine{module = Module}, State) ->
    CommandSpecs = Module:command(State),
    ValidSpecs = filter_valid_commands(CommandSpecs, State),
    Selected = weight_select(ValidSpecs),
    #command_gen{name = Name, args_gen = ArgsGen} = Selected,
    %% Run the generator with default size and random seed
    Tree = catena_gen:run(ArgsGen, 10, catena_gen:seed_new()),
    Args = catena_tree:root(Tree),
    {call, Module, Name, Args}.

%% @doc Generate a sequence of commands for testing.
%%
%% @param StateMachine The state machine specification
%% @param NumCommands Number of commands to generate
%% @param InitialSeed Random seed for generation (integer or seed record)
-spec generate_commands(state_machine(), pos_integer(), catena_gen:seed() | integer()) -> {command_seq(), catena_gen:seed()}.
generate_commands(StateMachine, NumCommands, Seed) when is_integer(Seed) ->
    SeedRec = catena_gen:seed_from_int(Seed),
    generate_commands(StateMachine, NumCommands, SeedRec, []);
generate_commands(StateMachine, NumCommands, Seed) ->
    generate_commands(StateMachine, NumCommands, Seed, []).

generate_commands(_StateMachine, 0, Seed, Acc) ->
    {lists:reverse(Acc), Seed};
generate_commands(#state_machine{module = Module} = SM, NumCommands, Seed, Acc) ->
    %% Get current state by simulating through accumulated commands
    State = simulate_state(SM, Acc),
    CommandSpecs = Module:command(State),
    ValidSpecs = filter_valid_commands(CommandSpecs, State),
    case ValidSpecs of
        [] ->
            %% No valid commands available, stop generation
            {lists:reverse(Acc), Seed};
        _ ->
            Selected = weight_select(ValidSpecs),
            #command_gen{name = Name, args_gen = ArgsGen} = Selected,
            {_Word, NewSeed} = catena_gen:seed_next(Seed),
            Tree = catena_gen:run(ArgsGen, 10, Seed),
            Args = catena_tree:root(Tree),
            Command = {call, Module, Name, Args},
            generate_commands(SM, NumCommands - 1, NewSeed, [Command | Acc])
    end.

%% @doc Select a command specification based on weights.
%%
%% Commands with higher weights are more likely to be selected.
-spec weight_select([command_spec()]) -> command_spec().
weight_select([]) ->
    error(no_commands_available);
weight_select(CommandSpecs) ->
    TotalWeight = lists:foldl(
        fun(#command_gen{weight = W}, Acc) when is_integer(W), W > 0 -> Acc + W;
           (#command_gen{}, Acc) -> Acc + 1  %% Default weight
        end, 0, CommandSpecs),
    Random = rand:uniform(TotalWeight),
    select_by_weight(CommandSpecs, Random, 0).

select_by_weight([#command_gen{weight = W} = Spec | Rest], Target, Acc) when is_integer(W) ->
    NewAcc = Acc + W,
    if NewAcc >= Target -> Spec;
       true -> select_by_weight(Rest, Target, NewAcc)
    end;
select_by_weight([#command_gen{} = Spec | _Rest], _Target, _Acc) ->
    %% Default weight of 1
    Spec.

%% @doc Filter command specifications to those with valid preconditions.
-spec filter_valid_commands([command_spec()], state()) -> [command_spec()].
filter_valid_commands(CommandSpecs, State) ->
    lists:filter(fun(#command_gen{precondition = undefined}) -> true;
                    (#command_gen{precondition = Precond}) -> Precond(State)
                 end, CommandSpecs).

%% @doc Simulate state by walking through command sequence.
%%
%% Used to determine the abstract state at any point in a command sequence.
-spec simulate_state(state_machine(), command_seq()) -> state().
simulate_state(#state_machine{module = Module, initial_state = InitialState}, Commands) ->
    Module:initial_state(),
    simulate_state_loop(Module, InitialState, Commands, []).

simulate_state_loop(_Module, State, [], _Vars) ->
    State;
simulate_state_loop(Module, State, [Cmd | Rest], Vars) ->
    NewState = Module:next_state(State, Vars, Cmd),
    %% Update Vars - in full implementation we'd extract variable bindings
    simulate_state_loop(Module, NewState, Rest, Vars).

%% @doc Shrink a command sequence while preserving failure.
%%
%% Returns a list of shrunk sequences to try.
-spec shrink_command_seq(command_seq(), state_machine()) -> [command_seq()].
shrink_command_seq(Commands, _StateMachine) ->
    %% Basic shrinking strategies:
    %% 1. Remove commands from the end
    %% 2. Remove individual commands
    %% 3. Shrink command arguments (future enhancement)
    Shrunk1 = shrink_from_end(Commands),
    Shrunk2 = shrink_one_at_a_time(Commands),
    lists:usort(Shrunk1 ++ Shrunk2).

%% @private Shrink by removing commands from the end.
shrink_from_end([]) ->
    [];
shrink_from_end([_Single]) ->
    [[]];  %% Only empty list for single element
shrink_from_end(Commands) ->
    %% Generate all prefixes shorter than the original
    shrink_from_end(Commands, length(Commands) - 1, []).

shrink_from_end(_Commands, 0, Acc) ->
    [[] | Acc];  %% Include empty list as a shrink option
shrink_from_end(Commands, N, Acc) when N > 0 ->
    Prefix = lists:sublist(Commands, N),
    shrink_from_end(Commands, N - 1, [Prefix | Acc]).

%% @private Shrink by removing one command at a time.
shrink_one_at_a_time([]) ->
    [];
shrink_one_at_a_time([_Single]) ->
    [];  %% Removing one from single element list leaves empty list
shrink_one_at_a_time(Commands) ->
    Len = length(Commands),
    [lists:sublist(Commands, I - 1) ++ lists:nthtail(I, Commands) || I <- lists:seq(1, Len)].
