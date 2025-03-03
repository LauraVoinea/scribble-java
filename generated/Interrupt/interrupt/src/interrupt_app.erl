%%%-------------------------------------------------------------------
%% @doc interrupt public API
%% @end
%%%-------------------------------------------------------------------

-module(interrupt_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    interrupt_sup:start_link().


stop(_State) ->
    ok.
