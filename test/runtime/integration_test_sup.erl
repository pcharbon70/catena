%%%-------------------------------------------------------------------
%%% @doc Test Supervisor for integration tests
%%% @end
%%%-------------------------------------------------------------------
-module(integration_test_sup).
-behaviour(catena_supervisor).

%% Callbacks
-export([init/1]).

init([]) ->
    Child = #{
        id => test_worker,
        start => {test_worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [test_worker]
    },
    {ok, #{strategy => one_for_one, intensity => 10, period => 60}, [Child]};

init([integration_test_sup2]) ->
    Child = #{
        id => test_worker2,
        start => {test_worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [test_worker]
    },
    {ok, #{strategy => one_for_one, intensity => 10, period => 60}, [Child]};

init(_Args) ->
    {ok, #{strategy => one_for_one}, []}.
