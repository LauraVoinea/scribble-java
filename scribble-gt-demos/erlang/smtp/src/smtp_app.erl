%%%-------------------------------------------------------------------
%% @doc smtp public API
%% @end
%%%-------------------------------------------------------------------

-module(smtp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    smtp_sup:start_link().

stop(_State) ->
    ok.

