%%%-------------------------------------------------------------------
%% @doc calculator public API
%% @end
%%%-------------------------------------------------------------------

-module(distributed_logging_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    distributed_logging_sup:start_link().

stop(_State) ->
    ok.

