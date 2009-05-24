-module(btcp).
-compile(export_all).

-define(LISTEN_PORT, 11300).
-define(TCP_OPTS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).

start() ->
  start(?LISTEN_PORT).

start(Port) ->
  bqueue:start(bqueue_server),
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
    {ok, "ADD " ++ Data} ->
      Data1 = lists:flatten(string:tokens(Data, "\r\n")),
      io:format("Adding ~p~n", [Data1]),
      bqueue_server ! {self(), {add, Data1}},
      receive
        {ok, NewQueue} ->
          gen_tcp:send(Socket, "OK\r\n");
        Other ->
          gen_tcp:send(Socket, "Error\r\n")
      end,
      responder_loop(Socket);
    {ok, "RESERVE\r\n"} ->
      bqueue_server ! {self(), reserve},
      receive
        {ok, Job} ->
          gen_tcp:send(Socket, Job),
          gen_tcp:send(Socket, "\r\n");
        {empty} ->
          gen_tcp:send(Socket, "EMPTY\r\n");
        Other ->
          gen_tcp:send(Socket, "Error\r\n")
      end,
      responder_loop(Socket);
    {ok, Other} ->
      gen_tcp:send(Socket, Other),
      responder_loop(Socket);
    {error, closed} ->
      ok
  end.

