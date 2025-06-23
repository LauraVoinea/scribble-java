%%%-------------------------------------------------------------------
%% @doc fibonacci public API
%% @end
%%%-------------------------------------------------------------------

-module(fibonacci_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    fibonacci_sup:start_link().

stop(_State) ->
    ok.

