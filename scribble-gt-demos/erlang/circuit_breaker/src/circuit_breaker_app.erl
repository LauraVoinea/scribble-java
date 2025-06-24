%%%-------------------------------------------------------------------
%% @doc calculator public API
%% @end
%%%-------------------------------------------------------------------

-module(circuit_breaker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    circuit_breaker_sup:start_link().

stop(_State) ->
    ok.

