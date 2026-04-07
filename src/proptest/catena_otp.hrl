%% @doc Records for OTP Behavior Testing
%%
%% This header file contains record definitions for OTP testing.

-record(genserver_test, {
    pid :: pid() | undefined,
    module :: module(),
    init_args :: term(),
    state :: term() | undefined
}).

-record(supervisor_test, {
    pid :: pid() | undefined,
    module :: module(),
    init_args :: term(),
    children :: [map()]
}).

-record(statem_test, {
    pid :: pid() | undefined,
    module :: module(),
    init_args :: term(),
    current_state :: term() | undefined
}).

-record(behavior_test, {
    pid :: pid() | undefined,
    module :: module(),
    callbacks :: [atom()]
}).

-record(child_spec, {
    id :: term(),
    start :: {module(), atom(), [term()]},
    restart :: permanent | temporary | transient,
    shutdown :: pos_integer() | brutal_kill | infinity,
    type :: worker | supervisor,
    modules :: [module()] | dynamic
}).
