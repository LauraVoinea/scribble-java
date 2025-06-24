%%%-------------------------------------------------------------------
%% @doc distributed_logging top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(distributed_logging_sup).

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
        #{id => logs,
            start => {logs, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker}
    ],
    {ok, {SupFlags, ChildSpecs}}.