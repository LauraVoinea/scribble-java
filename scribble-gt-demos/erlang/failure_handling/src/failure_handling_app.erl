%%%-------------------------------------------------------------------
%% @doc failure_handling public API
%% @end
%%%-------------------------------------------------------------------

-module(failure_handling_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    failure_handling_sup:start_link().

stop(_State) ->
    ok.

