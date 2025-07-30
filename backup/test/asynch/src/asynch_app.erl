%%%-------------------------------------------------------------------
%% @doc asynch public API
%% @end
%%%-------------------------------------------------------------------

-module(asynch_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    asynch_sup:start_link().

stop(_State) ->
    ok.

