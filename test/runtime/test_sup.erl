%%%-------------------------------------------------------------------
%%% @doc Test Supervisor for catena_supervisor_tests
%%% @end
%%%-------------------------------------------------------------------
-module(test_sup).
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
    {ok, #{strategy => one_for_one, intensity => 10, period => 60}, [Child]}.
