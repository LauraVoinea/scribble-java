%%%-------------------------------------------------------------------
%% @doc interrupt top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(interrupt_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  Children = [
    {a, {a, start_link, []}, temporary, 5000, worker, [a]},
    {b, {b, start_link, []}, temporary, 5000, worker, [b]},
    {c, {c, start_link, []}, temporary, 5000, worker, [c]}
  ],
  {ok, {{one_for_one, 5, 10}, Children}}.

