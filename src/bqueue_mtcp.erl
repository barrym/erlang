-module(bqueue_mtcp).
-compile(export_all).

-include_lib("src/bqueue_m.hrl").

-define(LISTEN_PORT, 11300).
-define(TCP_OPTS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).

start() ->
  start(?LISTEN_PORT).

start(Port) ->
  bqueue_m:start_link(),
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
    {ok, "KILL\r\n"} ->
      gen_tcp:send(Socket, "Exiting\r\n"),
      gen_tcp:close(Socket),
      io:format("Bye~n"),
      init:stop();
    {ok, "ADD " ++ Data} ->
      Data1 = lists:flatten(string:tokens(Data, "\r\n")),
      case bqueue_m:put(Data1, 0, 0, "default") of
        ok ->
          gen_tcp:send(Socket, "OK\r\n");
        _Other ->
          gen_tcp:send(Socket, "Error\r\n")
      end,
      responder_loop(Socket);
    {ok, "RESERVE\r\n"} ->
      case bqueue_m:get("default") of
        {ok, empty} ->
          gen_tcp:send(Socket, "EMPTY\r\n");
        {ok, [Job|_]} ->
          gen_tcp:send(Socket, Job#job.body),
          gen_tcp:send(Socket, "\r\n");
        _Other ->
          gen_tcp:send(Socket, "Error\r\n")
      end,
      responder_loop(Socket);
    {ok, "STATS\r\n"} ->
      case bqueue_m:stats() of
        {ok, Stats} ->
          io:format("stats : ~p~n",[Stats]),
          gen_tcp:send(Socket, "Job goes here"),
          gen_tcp:send(Socket, "\r\n");
        _Other -> 
          gen_tcp:send(Socket, "Error\r\n")
      end,
      responder_loop(Socket);
    {ok, Other} ->
      gen_tcp:send(Socket, Other),
      responder_loop(Socket);
    {error, closed} ->
      ok
  end.
