-module(btcp).
-compile(export_all).

-define(LISTEN_PORT, 11300).
-define(TCP_OPTS, [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, once}]).

start_server() -> 
  % bqueue:start(bqueue_server),
  case gen_tcp:listen(?LISTEN_PORT, ?TCP_OPTS) of
    {ok, Listen} -> 
      Pid = spawn(?MODULE, connect, [Listen]),
      io:format("~p Server Started. ~p~n", [erlang:localtime(), Pid]);
    Error -> 
      io:format("start_server Error: ~p~n", [Error])
  end.

connect(Listen) -> 
  io:format("connect started ~p~n", [self()]),
  io:format("Listen : ~p~n", [Listen]),
  case gen_tcp:accept(Listen) of
    {ok, Socket} ->
      io:format("ok~n"),
      inet:setopts(Socket, ?TCP_OPTS),
      spawn(fun() -> connect(Listen) end),
      recv_loop(Socket),
      gen_tcp:close(Socket),
      io:format("Closed~n");
    {error, Reason} -> 
      io:format("connect Error: ~p~n", [Reason])
  end.

recv_loop(Socket) -> 
  io:format("recv_loop started ~p~n", [self()]),
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"kill\r\n">>} ->
      bqueue_server ! {self(), die};
    {tcp, Socket, Data} -> 
      io:format("~p ~p ~p~n", [inet:peername(Socket), erlang:localtime(), Data]),
      Response = "Received: " ++ binary_to_list(Data) ++ "\r\n",
      gen_tcp:send(Socket, Response),
      recv_loop(Socket);
    {tcp_closed, Socket} ->
      io:format("~p Client Disconnected.~n", [erlang:localtime()])
  end.
