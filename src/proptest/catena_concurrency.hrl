%% @doc Records for Concurrency Testing
%%
%% This header file contains record definitions for concurrency testing.

-record(step, {
    process :: pid(),
    action :: atom(),
    args :: [term()]
}).

-record(race_info, {
    operations :: [term()],
    shared_state :: [term()]
}).

-record(operation, {
    id :: reference(),
    process :: pid(),
    type :: atom(),
    target :: term(),
    timestamp :: integer()
}).

-record(lock_info, {
    locks :: #{term() => pid()},
    wait_graph :: #{pid() => [term()]}
}).
