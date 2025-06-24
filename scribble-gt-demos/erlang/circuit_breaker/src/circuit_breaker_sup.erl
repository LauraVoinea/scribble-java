%%%-------------------------------------------------------------------
%% @doc circuit_breaker top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(circuit_breaker_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    ChildSpecs = [
        #{id => controller,
            start => {controller, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker},
        #{id => api,
            start => {api, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker},
        #{id => storage,
            start => {storage, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker},
        #{id => usr,
            start => {usr, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker}
    ],
    {ok, {SupFlags, ChildSpecs}}.