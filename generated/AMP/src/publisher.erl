-module(publisher).
-define(SERVER, ?MODULE).
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

-compile(export_all).
-compile(nowarn_export_all).

-record(state, {connection, channel}).

% API
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

publish(Message) -> gen_server:call(?MODULE, {publish, Message}).

% gen_server callbacks
init([]) ->
  {ok, Connection} = amqp_connection:start(#amqp_params_network{
    host = "localhost",
    port = 5672,
    virtual_host = <<"/">>,
    username = <<"guest">>,
    password = <<"guest">>
  }),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  {ok, #state{connection = Connection, channel = Channel}}.

handle_call({publish, Message}, _From, #state{channel = Channel} = State) ->
  Exchange = <<>>,
  RoutingKey = <<"my_queue">>,
  Payload = list_to_binary(Message),
  amqp_channel:cast(Channel, #'basic.publish'{exchange = Exchange, routing_key = RoutingKey}, #amqp_msg{payload = Payload}),
  {reply, ok, State}.

terminate(_Reason, #state{connection = Connection}) ->
  amqp_connection:close(Connection),
  ok.
