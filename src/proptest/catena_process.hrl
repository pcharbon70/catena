%% @doc Records for Process Testing
%%
%% This header file contains record definitions for process testing.
%% Include this in test files that use process testing features.

-record(test_process, {
    pid :: pid(),
    name :: atom() | undefined,
    monitor :: reference() | undefined,
    cleanup :: fun(() -> ok) | undefined
}).

-record(process_state, {
    pid :: pid(),
    state :: term(),
    memory :: integer(),
    message_queue_len :: integer(),
    heap_size :: integer(),
    reductions :: non_neg_integer()
}).

-record(protocol, {
    name :: atom() | binary(),
    messages :: [term()],
    ordered :: boolean(),
    allow_extra :: boolean()
}).

-record(schedule, {
    id :: reference(),
    steps :: [term()],
    current :: non_neg_integer()
}).
