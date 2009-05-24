-module(btcp2).
-compile(export_all).

-define(LISTEN_PORT, 11300).
-define(TCP_OPTS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).

start() ->
  start(?LISTEN_PORT).

start(Port) ->
  io:format("Starting~n"),
  case gen_tcp:listen(Port, [list, {packet, 0}, {active, false}, {reuseaddr, true}]) of
    {ok, ListeningSocket} ->
      io:format("~p Server started ~p~n", [?MODULE, erlang:localtime()]),
      accept_connection(ListeningSocket);
    Error -> 
      io:format("start_server error: ~p~n", [Error])
  end.

accept_connection(ListeningSocket) ->
  case gen_tcp:accept(ListeningSocket) of
    {ok, Socket} ->
      spawn_link(?MODULE, responder_loop, [Socket]),
      accept_connection(ListeningSocket);
    {error, Reason} ->
      io:format("acception_connection error: ~p~n", [Reason])
  end.

responder_loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, "kill\r\n"} ->
      gen_tcp:send(Socket, "Exiting\r\n"),
      gen_tcp:close(Socket),
      init:stop();
    {ok, Other} ->
      gen_tcp:send(Socket, Other),
      responder_loop(Socket);
    {error, closed} ->
      ok
  end.

