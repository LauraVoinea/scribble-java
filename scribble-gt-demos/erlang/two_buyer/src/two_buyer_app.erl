%%%-------------------------------------------------------------------
%% @doc two_buyer public API
%% @end
%%%-------------------------------------------------------------------

-module(two_buyer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    two_buyer_sup:start_link().

stop(_State) ->
    ok.

