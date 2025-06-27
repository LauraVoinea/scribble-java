%%%-------------------------------------------------------------------
%% @doc timeout top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(timeout_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
%%    SupFlags = #{strategy => one_for_all, intensity => 5, period => 10},
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    ChildSpecs = [
        #{id => b,
            start => {b, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker},
        #{id => a,
            start => {a, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker},
        #{id => client,
            start => {client, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker}
    ],
    {ok, {SupFlags, ChildSpecs}}.