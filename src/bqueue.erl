-module(bqueue).
-compile(export_all).

start(ServerName) ->
  register(ServerName, spawn(fun() -> loop([]) end)),
  % global:register_name(ServerName, spawn(fun() -> loop([]) end)),
  io:format("bqueue server started, registered name :~p~n", [ServerName]).

loop(Queue) ->
  receive
    {From, {add, Data}} -> 
      NewQueue = [Data|Queue],
      io:format("Queue: ~p -> ~p~n",[Queue,NewQueue]),
      From ! {ok, NewQueue},
      loop(NewQueue);
    {From, reserve} when Queue =:= [] ->
      From ! {empty},
      io:format("queue empty~n"),
      loop([]);
    {From, reserve} ->
      [H|T] = lists:reverse(Queue),
      From ! {ok, H},
      io:format("Queue: ~p -> ~p (Returned ~p)~n",[Queue,T,H]),
      loop(lists:reverse(T));
    {From, die} ->
      From ! {bye},
      io:format("Exiting~n");
    {From, stats} ->
      Response = {ok, {jobs, length(Queue)}},
      io:format("Stats: ~p~n", [Response]),
      From ! Response,
      loop(Queue);
    Other ->
      io:format("Huh? Don't understand ~p~n", [Other]),
      loop(Queue)
  end.


