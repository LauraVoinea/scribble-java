-module(consumer).
-define(SERVER, ?MODULE).
-behaviour(gen_statem).

-include_lib("amqp_client/include/amqp_client.hrl").

-compile(export_all).
-compile(nowarn_export_all).

-record(data, {connection, channel, consumer_tag = undefined}).

% Function to start the state machine
start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions].

send_ack(Channel, DeliveryTag) ->
  amqp_channel:call(Channel, #'basic.ack'{delivery_tag = DeliveryTag}).

send_reject(Channel, DeliveryTag, Requeue) ->
  amqp_channel:call(Channel, #'basic.reject'{delivery_tag = DeliveryTag, requeue = Requeue}).

send_start_channel(Connection) ->
  amqp_connection:open_channel(Connection).

send_declare_queue(Channel) ->
  amqp_channel:call(Channel, #'queue.declare'{queue = <<"my_queue">>}).

stop() ->
  gen_statem:stop(?MODULE).

send_start_connection() ->
  amqp_connection:start(#amqp_params_network{
    host = "localhost",
    port = 5672,
    virtual_host = <<"/">>,
    username = <<"guest">>,
    password = <<"guest">>
  }).

init([]) ->
  Data = #data{connection = undefined, channel = undefined},
  {ok, state1, Data, [{next_event, internal, start_connection}]}.

state1(internal, start_connection, Data) ->
  case send_start_connection() of
    {ok, Connection} ->
      io:format("Connection success: ~p~n", [Connection]),
      {next_state, state2, Data#data{connection = Connection}, [{next_event, internal, start_channel}]};
    {error, Reason} ->
      io:format("Connection error: ~p~n", [Reason]),
      {next_state, state_error, Data}
  end.

state2(internal, start_channel, Data) ->
  case send_start_channel(Data#data.connection) of
    {ok, Channel} ->
      io:format("Channel success: ~p~n", [Channel]),
      {next_state, state3, Data#data{channel = Channel}, [{next_event, internal, declare_queue}]};
    {error, Reason} ->
      io:format("Channel error: ~p~n", [Reason]),
      {next_state, state_error, Data}
  end.

state3(internal, declare_queue, Data) ->
  case send_declare_queue(Data#data.channel) of
    {'queue.declare_ok', QueueName, _MessageCount, _ConsumerCount} ->
      io:format("Queue declaration success: ~p~n", [QueueName]),
      {next_state, state4, Data, [{next_event, internal, consume}]};
    {error, Reason} ->
      io:format("Queue declaration error: ~p~n", [Reason]),
      {next_state, state_error, Data}
  end.

state4(internal, consume, Data) ->
  Queue = <<"my_queue">>,
  ConsumerTag = <<"">>, % Empty consumer tag lets the server generate one
  NoAck = false, % Acknowledgement required
  Args = [],
  io:format("State4 ~n"),
  case amqp_channel:subscribe(Data#data.channel, #'basic.consume'{queue = Queue, no_ack = NoAck}, self()) of
    {'basic.consume_ok', Tag} ->
      io:format("Subscribed to queue ~p with consumer tag ~p~n", [Queue, Tag]),
      {next_state, state5, Data#data{consumer_tag = Tag}};
    {error, Reason} ->
      io:format("Failed to subscribe to queue ~p: ~p~n", [Queue, Reason]),
      {next_state, state_error, Data}
  end.

state5(info, {Event, Tag}, Data) ->
  io:format("Consumer Event: ~p ~p ~n", [Event, Tag]),
  handle_event(Event, Tag, Data).

handle_event('basic.consume_ok', Tag, Data) ->
  io:format("Consume OK: ~p~n", [Tag]),
  {keep_state, Data};

handle_event(#'basic.cancel_ok'{}, _Tag, Data) ->
  io:format("Subscription cancelled.~n"),
  {stop, normal, Data};
%%publisher:publish("Hello, RabbitMQ!").
handle_event({'basic.deliver', Tag,_, _, _, _}, Content, Data) ->
  io:format("Received message: ~p~n", [Content]),
%%  send_ack(Data#data.channel, Tag),
  {keep_state, Data};

handle_event(Unexpected, _Tag, Data) ->
  io:format("Received unexpected event: ~p~n", [Unexpected]),
  {keep_state, Data}.

state_error(_, Data) ->
  io:format("Error state reached.~n"),
  {stop, normal, Data}.
