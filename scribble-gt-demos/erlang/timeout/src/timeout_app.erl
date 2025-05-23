%%%-------------------------------------------------------------------
%% @doc other_pingpong public API
%% @end
%%%-------------------------------------------------------------------

-module(timeout_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    timeout_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
