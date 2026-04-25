%% @doc Records for Stateful Property Testing
%%
%% This header file contains record definitions for stateful testing.
%% Include this in test files and statem implementations.

-record(state_machine, {
    name :: atom() | binary(),
    module :: module(),
    initial_state :: term(),
    commands :: [term()],
    invariants :: [term()],
    options :: term()
}).

-record(command, {
    name :: atom(),
    args :: [term()],
    call :: term()
}).

-record(command_gen, {
    name :: atom(),
    args_gen :: term(),
    precondition :: term() | undefined,
    execute :: term() | undefined,
    postcondition :: term() | undefined,
    next_state :: term() | undefined,
    weight :: pos_integer() | undefined,
    frequency :: term() | undefined
}).

-record(invariant, {
    name :: atom() | binary(),
    check :: fun((term()) -> boolean()),
    when_fun :: fun((term()) -> boolean()) | undefined
}).

-record(options, {
    num_commands :: pos_integer(),
    max_shrinks :: pos_integer(),
    timeout :: pos_integer() | infinity,
    parallel_tracks :: pos_integer()
}).
