%%%-------------------------------------------------------------------
%% @doc travel_agency public API
%% @end
%%%-------------------------------------------------------------------

-module(travel_agency_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    travel_agency_sup:start_link().

stop(_State) ->
    ok.

