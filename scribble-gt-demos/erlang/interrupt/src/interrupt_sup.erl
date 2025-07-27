%%%-------------------------------------------------------------------
%% @doc calculator top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(interrupt_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    ChildSpecs = [
        #{id => p,
            start => {p, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker},
        #{id => q,
            start => {q, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker}
    ],
    {ok, {SupFlags, ChildSpecs}}.