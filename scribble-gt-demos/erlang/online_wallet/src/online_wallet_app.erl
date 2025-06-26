%%%-------------------------------------------------------------------
%% @doc online_wallet public API
%% @end
%%%-------------------------------------------------------------------

-module(online_wallet_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    online_wallet_sup:start_link().

stop(_State) ->
    ok.

