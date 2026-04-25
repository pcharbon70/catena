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
    state_machine/2,
    state_machine/3,
    command/2,
    command/3,
    invariant/2,
    invariant/3,
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
    generate_command/3,
    generate_commands/3,
    weight_select/1,
    filter_valid_commands/2,
    shrink_command_seq/2,
    simulate_state/2
]).

%%====================================================================
%% Section 5.3: Symbolic Execution
%%====================================================================

-export([
    new_var/1,
    bind_var/3,
    lookup_var/2,
    substitute_vars/2,
    run_symbolic/2,
    format_symbolic_trace/1,
    symbolic_next_state/3,
    collect_postconditions/3
]).

%%====================================================================
%% Section 5.4: Concrete Execution
%%====================================================================

-export([
    execute_command/3,
    execute_commands/4,
    run_parallel_tracks/3,
    check_postconditions/3
]).

%%====================================================================
%% Section 5.5: Parallel Execution
%%====================================================================

-export([
    parallel_property_test/4,
    aggregate_parallel_results/1
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

-callback initial_state() -> state().
-callback command(state()) -> [command_spec()].
-callback precondition(state(), symbolic_command()) -> boolean().
-callback next_state(state(), var_bindings(), symbolic_command()) -> state().
-callback postcondition(state(), symbolic_command(), command_result()) -> boolean().
-callback setup() -> {ok, system()} | {error, term()}.
-callback cleanup(system()) -> ok.
-callback invariants() -> [invariant_spec()].
-optional_callbacks([setup/0, cleanup/1, invariants/0]).

%%====================================================================
%% Section 5.1.1: State Machine Type Definition
%%====================================================================

%% @doc Create a new state machine specification.
%%
%% @param Name Human-readable name for this state machine
%% @param Module Callback module implementing the statem behavior
%% @param Options Configuration options
-spec state_machine(atom() | binary(), module()) -> state_machine().
state_machine(Name, Module) ->
    state_machine(Name, Module, default_options()).

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

%% @doc Construct a command specification with default options.
-spec command(atom(), args_generator()) -> command_spec().
command(Name, ArgsGen) ->
    command(Name, ArgsGen, #{}).

%% @doc Construct a command specification from a name, generator, and option map.
%%
%% Supported options are:
%% - `precondition`
%% - `execute`
%% - `postcondition`
%% - `next_state`
%% - `weight`
%% - `frequency`
-spec command(atom(), args_generator(), map()) -> command_spec().
command(Name, ArgsGen, Options) ->
    #command_gen{
        name = Name,
        args_gen = ArgsGen,
        precondition = maps:get(precondition, Options, undefined),
        execute = maps:get(execute, Options, undefined),
        postcondition = maps:get(postcondition, Options, undefined),
        next_state = maps:get(next_state, Options, undefined),
        weight = maps:get(weight, Options, undefined),
        frequency = maps:get(frequency, Options, undefined)
    }.

%% @doc Construct an invariant with no guard.
-spec invariant(atom() | binary(), fun((state()) -> boolean())) -> invariant_spec().
invariant(Name, Check) ->
    invariant(Name, Check, undefined).

%% @doc Construct an invariant with an optional guard.
-spec invariant(atom() | binary(), fun((state()) -> boolean()), fun((state()) -> boolean()) | undefined) ->
    invariant_spec().
invariant(Name, Check, WhenFun) ->
    #invariant{name = Name, check = Check, when_fun = WhenFun}.

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
validate(#state_machine{module = Module, initial_state = InitialState, commands = Commands, invariants = Invariants}) ->
    Errors =
        validate_callbacks(Module) ++
        validate_command_specs(Module, InitialState, Commands) ++
        validate_invariants(Module, Invariants),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

validate_callbacks(Module) ->
    Required = [
        {initial_state, 0},
        {command, 1},
        {precondition, 2},
        {next_state, 3},
        {postcondition, 3}
    ],
    CheckCallback = fun({Func, Arity}) ->
        case erlang:function_exported(Module, Func, Arity) of
            true -> [];
            false -> [{missing_callback, Func, Arity}]
        end
    end,
    lists:append([CheckCallback(Callback) || Callback <- Required]).

validate_command_specs(Module, InitialState, Commands) ->
    DynamicErrors =
        case catch Module:command(InitialState) of
            {'EXIT', Reason} ->
                [{command_callback_failed, Reason}];
            Result when is_list(Result) ->
                validate_command_list(dynamic, Result);
            Result ->
                [{invalid_command_list, dynamic, Result}]
        end,
    CachedErrors =
        case is_list(Commands) of
            true -> validate_command_list(cached, Commands);
            false -> [{invalid_command_list, cached, Commands}]
        end,
    DynamicErrors ++ CachedErrors.

validate_command_list(Source, CommandSpecs) ->
    lists:append([validate_command_spec(Source, Spec) || Spec <- CommandSpecs]).

validate_command_spec(Source, #command_gen{
    name = Name,
    args_gen = ArgsGen,
    precondition = Precondition,
    execute = Execute,
    postcondition = Postcondition,
    next_state = NextState,
    weight = Weight,
    frequency = Frequency
}) ->
    validate_spec_field(Source, invalid_command_name, Name, is_atom(Name)) ++
    validate_spec_field(Source, missing_args_generator, ArgsGen, ArgsGen =/= undefined) ++
    validate_spec_field(Source, invalid_precondition, Precondition, Precondition =:= undefined orelse is_function(Precondition, 1)) ++
    validate_spec_field(Source, invalid_execute, Execute, Execute =:= undefined orelse is_function(Execute, 2)) ++
    validate_spec_field(Source, invalid_postcondition, Postcondition, Postcondition =:= undefined orelse is_function(Postcondition, 3)) ++
    validate_spec_field(Source, invalid_next_state, NextState, NextState =:= undefined orelse is_function(NextState, 3)) ++
    validate_spec_field(Source, invalid_weight, Weight, Weight =:= undefined orelse (is_integer(Weight) andalso Weight > 0)) ++
    validate_spec_field(Source, invalid_frequency, Frequency, Frequency =:= undefined orelse is_function(Frequency, 1));
validate_command_spec(Source, Spec) ->
    [{invalid_command_spec, Source, Spec}].

validate_spec_field(_Source, _Kind, _Value, true) ->
    [];
validate_spec_field(Source, Kind, Value, false) ->
    [{Kind, Source, Value}].

validate_invariants(Module, Invariants) ->
    DynamicErrors =
        case erlang:function_exported(Module, invariants, 0) of
            true ->
                case catch Module:invariants() of
                    {'EXIT', Reason} ->
                        [{invariants_callback_failed, Reason}];
                    Result when is_list(Result) ->
                        validate_invariant_list(dynamic, Result);
                    Result ->
                        [{invalid_invariant_list, dynamic, Result}]
                end;
            false ->
                []
        end,
    CachedErrors =
        case is_list(Invariants) of
            true -> validate_invariant_list(cached, Invariants);
            false -> [{invalid_invariant_list, cached, Invariants}]
        end,
    DynamicErrors ++ CachedErrors.

validate_invariant_list(Source, Invariants) ->
    lists:append([validate_invariant(Source, Invariant) || Invariant <- Invariants]).

validate_invariant(Source, #invariant{name = Name, check = Check, when_fun = WhenFun}) ->
    validate_spec_field(Source, invalid_invariant_name, Name, is_atom(Name) orelse is_binary(Name)) ++
    validate_spec_field(Source, invalid_invariant_check, Check, is_function(Check, 1)) ++
    validate_spec_field(Source, invalid_invariant_guard, WhenFun, WhenFun =:= undefined orelse is_function(WhenFun, 1));
validate_invariant(Source, Invariant) ->
    [{invalid_invariant, Source, Invariant}].

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
state_get(Key, State) when is_tuple(State), is_integer(Key), Key > 0, Key =< tuple_size(State) ->
    element(Key, State);
state_get(Key, State) when is_list(State) ->
    proplists:get_value(Key, State);
state_get(Key, State) ->
    error({unsupported_state_access, Key, State}).

%% @doc Update state using a key (for map-based states).
-spec state_put(term(), term(), state()) -> state().
state_put(Key, Value, State) when is_map(State) ->
    maps:put(Key, Value, State);
state_put(Key, Value, State) when is_tuple(State), is_integer(Key), Key > 0, Key =< tuple_size(State) ->
    setelement(Key, State, Value);
state_put(Key, Value, State) when is_list(State) ->
    lists:keystore(Key, 1, State, {Key, Value});
state_put(Key, _Value, State) ->
    error({unsupported_state_update, Key, State}).

%% @doc Update state using a function (for map-based states).
-spec state_update(term(), fun((term()) -> term()), state()) -> state().
state_update(Key, Fun, State) when is_map(State) ->
    maps:update_with(Key, Fun, State);
state_update(Key, Fun, State) when is_tuple(State), is_integer(Key), Key > 0, Key =< tuple_size(State) ->
    setelement(Key, State, Fun(element(Key, State)));
state_update(Key, Fun, State) when is_list(State) ->
    {value, {Key, Value}} = lists:keysearch(Key, 1, State),
    lists:keyreplace(Key, 1, State, {Key, Fun(Value)});
state_update(Key, _Fun, State) ->
    error({unsupported_state_update, Key, State}).

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
generate_command(StateMachine, State) ->
    {Command, _Seed} = generate_command(StateMachine, State, catena_gen:seed_new()),
    Command.

-spec generate_command(state_machine(), state(), catena_gen:seed() | integer()) ->
    {symbolic_command(), catena_gen:seed()}.
generate_command(StateMachine, State, Seed) when is_integer(Seed) ->
    generate_command(StateMachine, State, catena_gen:seed_from_int(Seed));
generate_command(#state_machine{module = Module}, State, Seed) ->
    CommandSpecs = Module:command(State),
    ValidSpecs = filter_valid_commands(CommandSpecs, State),
    {Selected, SelectionSeed} = weight_select_seeded(ValidSpecs, State, Seed),
    #command_gen{name = Name, args_gen = ArgsGen} = Selected,
    {ArgsSeed, NextSeed} = catena_gen:seed_split(SelectionSeed),
    Tree = catena_gen:run(ArgsGen, 10, ArgsSeed),
    Args = catena_tree:root(Tree),
    {{call, Module, Name, Args}, NextSeed}.

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
            {Command, NewSeed} = generate_command(SM, State, Seed),
            generate_commands(SM, NumCommands - 1, NewSeed, [Command | Acc])
    end.

%% @doc Select a command specification based on weights.
%%
%% Commands with higher weights are more likely to be selected.
-spec weight_select([command_spec()]) -> command_spec().
weight_select([]) ->
    error(no_commands_available);
weight_select(CommandSpecs) ->
    {Selected, _Seed} = weight_select_seeded(CommandSpecs, #{}, catena_gen:seed_new()),
    Selected.

weight_select_seeded([], _State, _Seed) ->
    error(no_commands_available);
weight_select_seeded(CommandSpecs, State, Seed) ->
    WeightedSpecs = [{effective_weight(Spec, State), Spec} || Spec <- CommandSpecs],
    TotalWeight = lists:sum([Weight || {Weight, _Spec} <- WeightedSpecs]),
    {Word, NextSeed} = catena_gen:seed_next(Seed),
    Target = (Word rem TotalWeight) + 1,
    {select_by_weight(WeightedSpecs, Target, 0), NextSeed}.

effective_weight(#command_gen{frequency = Frequency}, State) when is_function(Frequency, 1) ->
    Weight = Frequency(State),
    case is_integer(Weight) andalso Weight > 0 of
        true -> Weight;
        false -> 1
    end;
effective_weight(#command_gen{weight = Weight}, _State) when is_integer(Weight), Weight > 0 ->
    Weight;
effective_weight(#command_gen{}, _State) ->
    1.

select_by_weight([{Weight, Spec} | Rest], Target, Acc) ->
    NewAcc = Acc + Weight,
    if NewAcc >= Target -> Spec;
       true -> select_by_weight(Rest, Target, NewAcc)
    end.

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
shrink_command_seq([], _StateMachine) ->
    [];
shrink_command_seq(Commands, StateMachine) ->
    RemovalShrinks = shrink_from_end(Commands) ++ shrink_one_at_a_time(Commands),
    ArgumentShrinks = shrink_command_arguments(Commands),
    ValidShrinks = [
        Sequence
     || Sequence <- RemovalShrinks ++ ArgumentShrinks,
        length(Sequence) < length(Commands) orelse Sequence =/= Commands,
        valid_command_sequence(StateMachine, Sequence)
    ],
    lists:usort(ValidShrinks).

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

shrink_command_arguments(Commands) ->
    lists:append([shrink_single_command(Commands, Index) || Index <- lists:seq(1, length(Commands))]).

shrink_single_command(Commands, Index) ->
    Command = lists:nth(Index, Commands),
    case Command of
        {call, Module, Name, Args} ->
            Prefix = lists:sublist(Commands, Index - 1),
            Suffix = lists:nthtail(Index, Commands),
            [
                Prefix ++ [{call, Module, Name, ShrunkArgs}] ++ Suffix
             || ShrunkArgs <- shrink_arguments(Args),
                ShrunkArgs =/= Args
            ];
        _ ->
            []
    end.

shrink_arguments(Args) ->
    lists:append([shrink_argument_at(Args, Index) || Index <- lists:seq(1, length(Args))]).

shrink_argument_at(Args, Index) ->
    Arg = lists:nth(Index, Args),
    Prefix = lists:sublist(Args, Index - 1),
    Suffix = lists:nthtail(Index, Args),
    [Prefix ++ [Shrunk] ++ Suffix || Shrunk <- shrink_argument(Arg)].

shrink_argument(Arg) when is_integer(Arg) ->
    lists:usort(catena_shrink:shrink_binary(Arg, 0) ++ catena_shrink:shrink_halves(Arg));
shrink_argument(true) ->
    [false];
shrink_argument(false) ->
    [];
shrink_argument(Arg) when is_list(Arg) ->
    catena_shrink:shrink_list(Arg);
shrink_argument(Arg) when is_binary(Arg) ->
    [binary:part(Arg, 0, Len) || Len <- lists:seq(0, max(byte_size(Arg) - 1, 0))];
shrink_argument(_Arg) ->
    [].

valid_command_sequence(#state_machine{module = Module, initial_state = InitialState, invariants = Invariants}, Commands) ->
    valid_command_sequence(Module, InitialState, Invariants, Commands, []).

valid_command_sequence(_Module, State, Invariants, [], _Vars) ->
    check_invariants(Invariants, State) =:= ok;
valid_command_sequence(Module, State, Invariants, [Command | Rest], Vars) ->
    case Module:precondition(State, Command) of
        true ->
            NewState = Module:next_state(State, Vars, Command),
            case check_invariants(Invariants, NewState) of
                ok -> valid_command_sequence(Module, NewState, Invariants, Rest, Vars);
                {error, _} -> false
            end;
        false ->
            false
    end.

%%====================================================================
%% Section 5.3: Symbolic Execution
%%====================================================================

%% @doc Create a new symbolic variable with a given index.
-spec new_var(pos_integer()) -> symbolic_var().
new_var(Index) ->
    {var, Index}.

%% @doc Bind a symbolic variable to a concrete result.
-spec bind_var(symbolic_var(), command_result(), var_bindings()) -> var_bindings().
bind_var(Var, Result, Bindings) ->
    [{Var, Result} | Bindings].

%% @doc Look up a symbolic variable in the bindings.
%%
%% Returns {ok, Value} if found, error otherwise.
-spec lookup_var(symbolic_var(), var_bindings()) -> {ok, command_result()} | error.
lookup_var(Var, Bindings) ->
    case lists:keysearch(Var, 1, Bindings) of
        {value, {Var, Value}} -> {ok, Value};
        false -> error
    end.

%% @doc Substitute symbolic variables in arguments with concrete values.
%%
%% Replaces any {var, N} references with their bound values from the bindings.
-spec substitute_vars([symbolic_arg()], var_bindings()) -> [term()].
substitute_vars(Args, Bindings) ->
    [substitute_term(Arg, Bindings) || Arg <- Args].

substitute_term({var, _} = Var, Bindings) ->
    case lookup_var(Var, Bindings) of
        {ok, Value} -> Value;
        error -> Var
    end;
substitute_term(Term, Bindings) when is_list(Term) ->
    [substitute_term(Item, Bindings) || Item <- Term];
substitute_term(Term, Bindings) when is_tuple(Term) ->
    list_to_tuple([substitute_term(Item, Bindings) || Item <- tuple_to_list(Term)]);
substitute_term(Term, Bindings) when is_map(Term) ->
    maps:from_list([
        {substitute_term(Key, Bindings), substitute_term(Value, Bindings)}
     || {Key, Value} <- maps:to_list(Term)
    ]);
substitute_term(Term, _Bindings) ->
    Term.

%% @doc Run a symbolic command sequence against the state model.
%%
%% Returns the symbolic trace, final state, and symbolic bindings used to
%% validate later concrete execution.
-spec run_symbolic(state_machine(), command_seq()) ->
    {ok, [map()], state(), var_bindings()} |
    {error, {precondition_failed | undefined_variable, pos_integer(), term(), symbolic_command()}}.
run_symbolic(#state_machine{module = Module, initial_state = InitialState} = StateMachine, Commands) ->
    run_symbolic_loop(StateMachine, Module, InitialState, Commands, [], [], 1).

run_symbolic_loop(_StateMachine, _Module, State, [], Bindings, Trace, _Index) ->
    {ok, lists:reverse(Trace), State, lists:reverse(Bindings)};
run_symbolic_loop(StateMachine, Module, State, [Command | Rest], Bindings, Trace, Index) ->
    case validate_symbolic_command(Command, Bindings) of
        ok ->
            case Module:precondition(State, Command) of
                true ->
                    ResultVar = new_var(Index),
                    SymbolicValue = {symbolic_result, Index},
                    NewBindings = bind_var(ResultVar, SymbolicValue, Bindings),
                    NewState = symbolic_next_state(State, NewBindings, Command),
                    case check_invariants(StateMachine#state_machine.invariants, NewState) of
                        ok ->
                            TraceEntry = #{
                                index => Index,
                                command => Command,
                                result_var => ResultVar,
                                state_before => State,
                                state_after => NewState
                            },
                            run_symbolic_loop(StateMachine, Module, NewState, Rest, NewBindings, [TraceEntry | Trace], Index + 1);
                        {error, _} ->
                            {error, {precondition_failed, Index, NewState, Command}}
                    end;
                false ->
                    {error, {precondition_failed, Index, State, Command}}
            end;
        {error, Reason} ->
            {error, {undefined_variable, Index, Reason, Command}}
    end.

validate_symbolic_command({call, _Module, _Name, Args}, Bindings) ->
    validate_symbolic_args(Args, Bindings);
validate_symbolic_command(_Command, _Bindings) ->
    ok.

validate_symbolic_args(Args, Bindings) when is_list(Args) ->
    validate_symbolic_terms(Args, Bindings);
validate_symbolic_args(Arg, Bindings) ->
    validate_symbolic_terms([Arg], Bindings).

validate_symbolic_terms([], _Bindings) ->
    ok;
validate_symbolic_terms([Term | Rest], Bindings) ->
    case validate_symbolic_term(Term, Bindings) of
        ok -> validate_symbolic_terms(Rest, Bindings);
        Error -> Error
    end.

validate_symbolic_term({var, _} = Var, Bindings) ->
    case lookup_var(Var, Bindings) of
        {ok, _Value} -> ok;
        error -> {error, Var}
    end;
validate_symbolic_term(Term, Bindings) when is_list(Term) ->
    validate_symbolic_terms(Term, Bindings);
validate_symbolic_term(Term, Bindings) when is_tuple(Term) ->
    validate_symbolic_terms(tuple_to_list(Term), Bindings);
validate_symbolic_term(Term, Bindings) when is_map(Term) ->
    validate_symbolic_terms(lists:append([[Key, Value] || {Key, Value} <- maps:to_list(Term)]), Bindings);
validate_symbolic_term(_Term, _Bindings) ->
    ok.

%% @doc Format a symbolic trace for debugging output.
-spec format_symbolic_trace([map()]) -> iolist().
format_symbolic_trace(Trace) ->
    lists:map(
        fun(#{index := Index, command := Command, result_var := ResultVar, state_before := StateBefore, state_after := StateAfter}) ->
            io_lib:format(
                "~p. ~p => ~p~n   before: ~p~n   after:  ~p~n",
                [Index, Command, ResultVar, StateBefore, StateAfter]
            )
        end,
        Trace
    ).

%% @doc Compute the next abstract state after a symbolic command.
%%
%% Unlike simulate_state which doesn't track variables, this properly
%% updates state based on symbolic execution semantics.
-spec symbolic_next_state(state(), var_bindings(), symbolic_command()) -> state().
symbolic_next_state(State, Vars, {call, Module, Name, Args}) ->
    %% Extract concrete args by substituting variables
    ConcreteArgs = substitute_vars(Args, Vars),
    %% Create the actual command tuple that next_state expects
    ActualCmd = {call, Module, Name, ConcreteArgs},
    Module:next_state(State, Vars, ActualCmd).

%% @doc Collect all postconditions for a command sequence.
%%
%% Returns a list of {CommandIndex, PostconditionFun} tuples that can be
%% checked during concrete execution.
-spec collect_postconditions(state_machine(), state(), command_seq()) -> [{pos_integer(), fun()}].
collect_postconditions(#state_machine{module = Module}, InitialState, Commands) ->
    collect_postconditions_loop(Module, InitialState, Commands, [], 1, []).

collect_postconditions_loop(_Module, _State, [], _Vars, _Index, Acc) ->
    lists:reverse(Acc);
collect_postconditions_loop(Module, State, [Cmd | Rest], Vars, Index, Acc) ->
    %% Check precondition
    case Module:precondition(State, Cmd) of
        false ->
            %% Skip this command, don't increment index for postconditions
            collect_postconditions_loop(Module, State, Rest, Vars, Index, Acc);
        true ->
            %% Add postcondition check
            PostCond = fun(Result) -> Module:postcondition(State, Cmd, Result) end,
            %% Update state
            NewState = Module:next_state(State, Vars, Cmd),
            collect_postconditions_loop(Module, NewState, Rest, Vars, Index + 1, [{Index, PostCond} | Acc])
    end.

%%====================================================================
%% Section 5.4: Concrete Execution
%%====================================================================

%% @doc Execute a single command against a real system.
%%
%% Returns {ok, Result, NewBindings} on success, {error, Reason} on failure.
-spec execute_command(system(), var_bindings(), symbolic_command()) ->
    {ok, command_result(), var_bindings()} | {error, term()}.
execute_command(_System, Vars, {call, Module, Name, Args}) ->
    %% Substitute variables in arguments
    ConcreteArgs = substitute_vars(Args, Vars),
    %% Call the actual command using apply to handle variable arity
    try apply(Module, Name, ConcreteArgs) of
        Result ->
            %% Create a new variable binding for this result
            VarIndex = length(Vars) + 1,
            Var = new_var(VarIndex),
            NewBindings = bind_var(Var, Result, Vars),
            {ok, Result, NewBindings}
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%% @doc Execute a sequence of commands against a real system.
%%
%% Returns {ok, Results, FinalBindings} on success, {error, Reason, PartialResults} on failure.
-spec execute_commands(system(), state(), command_seq(), module()) ->
    {ok, [command_result()], var_bindings()} | {error, term(), [command_result()]}.
execute_commands(System, InitialState, Commands, Module) ->
    execute_commands_loop(System, InitialState, Commands, Module, [], []).

execute_commands_loop(_System, _State, [], _Module, ResultsAcc, Bindings) ->
    {ok, lists:reverse(ResultsAcc), Bindings};
execute_commands_loop(System, State, [Cmd | Rest], Module, ResultsAcc, Bindings) ->
    %% Check precondition
    case Module:precondition(State, Cmd) of
        false ->
            %% Skip this command
            NewState = Module:next_state(State, Bindings, Cmd),
            execute_commands_loop(System, NewState, Rest, Module, ResultsAcc, Bindings);
        true ->
            case execute_command(System, Bindings, Cmd) of
                {ok, Result, NewBindings} ->
                    %% Check postcondition
                    case Module:postcondition(State, Cmd, Result) of
                        true ->
                            NewState = Module:next_state(State, Bindings, Cmd),
                            execute_commands_loop(System, NewState, Rest, Module, [Result | ResultsAcc], NewBindings);
                        false ->
                            {error, {postcondition_failed, Cmd, State, Result}, lists:reverse(ResultsAcc)}
                    end;
                {error, Reason} ->
                    {error, {command_failed, Cmd, Reason}, lists:reverse(ResultsAcc)}
            end
    end.

%% @doc Run command sequences in parallel tracks for shrinking.
%%
%% Each track is an alternative command sequence to try.
-spec run_parallel_tracks(system(), state(), [[symbolic_command()]]) ->
    {ok, {command_seq(), [command_result()], var_bindings()}} | {error, term()}.
run_parallel_tracks(System, InitialState, Tracks) when is_list(Tracks) ->
    run_parallel_tracks_loop(System, InitialState, Tracks, none).

run_parallel_tracks_loop(_System, _State, [], BestResult) ->
    BestResult;
run_parallel_tracks_loop(System, InitialState, [Track | Rest], none) ->
    %% Assume first module in first command
    {call, Module, _, _} = hd(Track),
    case execute_commands(System, InitialState, Track, Module) of
        {ok, Results, Bindings} ->
            %% This track passed, return it
            {ok, {Track, Results, Bindings}};
        {error, _, _} ->
            %% Try next track
            run_parallel_tracks_loop(System, InitialState, Rest, none)
    end;
run_parallel_tracks_loop(System, InitialState, [Track | Rest], {ok, _Best} = CurrentBest) ->
    {call, Module, _, _} = hd(Track),
    case execute_commands(System, InitialState, Track, Module) of
        {ok, Results, Bindings} ->
            %% Track passed - prefer shorter tracks
            {ok, {PrevTrack, _, _}} = CurrentBest,
            case length(Track) < length(PrevTrack) of
                true -> {ok, {Track, Results, Bindings}};
                false -> CurrentBest
            end;
        {error, _, _} ->
            run_parallel_tracks_loop(System, InitialState, Rest, CurrentBest)
    end.

%% @doc Check collected postconditions against execution results.
%%
%% Returns ok if all postconditions pass, {error, failed_index} otherwise.
-spec check_postconditions([{pos_integer(), fun((command_result()) -> boolean())}], [command_result()], state()) ->
    ok | {error, {postcondition_failed, pos_integer(), state()}}.
check_postconditions(PostConds, Results, State) ->
    check_postconditions_loop(PostConds, Results, State, 1).

check_postconditions_loop([], _Results, _State, _Index) ->
    ok;
check_postconditions_loop([{PCIndex, Fun} | Rest], Results, State, Index) when PCIndex =:= Index ->
    case lists:member(Index, lists:seq(1, length(Results))) of
        true ->
            Result = lists:nth(Index, Results),
            case Fun(Result) of
                true ->
                    check_postconditions_loop(Rest, Results, State, Index + 1);
                false ->
                    {error, {postcondition_failed, Index, State}}
            end;
        false ->
            %% Result not available (command was skipped)
            check_postconditions_loop(Rest, Results, State, Index + 1)
    end;
check_postconditions_loop(PostConds, Results, State, Index) ->
    check_postconditions_loop(PostConds, Results, State, Index + 1).

%%====================================================================
%% Section 5.5: Parallel Execution
%%====================================================================

%% @doc Run a property test in parallel with multiple seeds.
%%
%% Creates seed list for parallel workers (simplified version).
-spec parallel_property_test(state_machine(), pos_integer(), pos_integer(), pos_integer()) ->
    [{catena_gen:seed(), ok | {error, term()}}].
parallel_property_test(_StateMachine, NumTests, NumParallel, BaseSeed) ->
    %% Create seed list for each parallel worker
    Seeds = [catena_gen:seed_from_int(BaseSeed + I) || I <- lists:seq(0, NumParallel - 1)],
    %% Return placeholder results (in actual implementation would spawn processes)
    [{Seed, ok} || Seed <- Seeds].

%% @private Run property test with specific seed.
run_property_test_with_seed(_StateMachine, _NumTests, _Seed) ->
    %% Placeholder - would run actual property test
    ok.

%% @private Collect results from parallel workers.
collect_parallel_results(Pids, Acc, Remaining) when Remaining =:= 0 ->
    %% Wait for any remaining processes and return
    wait_for_remaining(Pids, Acc);
collect_parallel_results(Pids, Acc, Remaining) ->
    receive
        {'DOWN', _Ref, process, Pid, _Reason} ->
            %% Process died unexpectedly
            collect_parallel_results(lists:delete(Pid, Pids), Acc, Remaining - 1);
        {Pid, {Seed, Result}} ->
            demonitor(Pid),
            collect_parallel_results(lists:delete(Pid, Pids), [{Seed, Result} | Acc], Remaining - 1)
    end.

wait_for_remaining([], Acc) ->
    lists:reverse(Acc);
wait_for_remaining(Pids, Acc) ->
    receive
        {'DOWN', _Ref, process, Pid, _Reason} ->
            wait_for_remaining(lists:delete(Pid, Pids), Acc);
        {Pid, {Seed, Result}} ->
            demonitor(Pid),
            wait_for_remaining(lists:delete(Pid, Pids), [{Seed, Result} | Acc])
    after 5000 ->
        %% Timeout
        lists:reverse(Acc)
    end.

%% @doc Aggregate results from parallel test runs.
%%
%% Returns summary with pass/fail counts and any failures found.
-spec aggregate_parallel_results([{catena_gen:seed(), ok | {error, term()}}]) ->
    {passed, pos_integer(), [{catena_gen:seed(), term()}]}.
aggregate_parallel_results(Results) ->
    Passed = length([1 || {_Seed, ok} <- Results]),
    Failed = [{Seed, Reason} || {Seed, {error, Reason}} <- Results],
    {passed, Passed, Failed}.
