%% @doc Records for Distribution Testing
%%
%% This header file contains record definitions for distributed testing.

-record(test_node, {
    name :: atom() | binary(),
    node :: node(),
    pid :: pid() | undefined,
    cleanup :: fun(() -> ok) | undefined
}).

-record(network_partition, {
    nodes :: [node()],
    healed :: boolean()
}).

-record(crdt_state, {
    value :: term(),
    version :: integer()
}).
