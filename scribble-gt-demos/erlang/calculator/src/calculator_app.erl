%%%-------------------------------------------------------------------
%% @doc calculator public API
%% @end
%%%-------------------------------------------------------------------

-module(calculator_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    calculator_sup:start_link().

stop(_State) ->
    ok.

