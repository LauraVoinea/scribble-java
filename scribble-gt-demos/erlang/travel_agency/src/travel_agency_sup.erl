%%%-------------------------------------------------------------------
%% @doc travel_agency top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(travel_agency_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    ChildSpecs = [
        #{id => client,
            start => {client, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker},
        #{id => supplier,
            start => {supplier, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker},
        #{id => agency,
            start => {agency, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker}
    ],
    {ok, {SupFlags, ChildSpecs}}.