-module(blib).
-compile(export_all).

start() ->
  case whereis(list_server) of
    undefined -> 
      io:format("Starting server\n"),
      register(list_server, spawn(fun list_loop/0));
    Pid -> io:format("list_server has been started at ~p\n", [Pid])
  end.

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} ->
      Response
  after 20 ->
      io:format("Timeout")
  end.



make_list(N) -> make_list(N, []).
make_list(0, L) -> L;
make_list(N, L) -> make_list(N - 1, [N|L]).

sum_list([H|T]) -> H + sum_list(T);
sum_list([]) -> 0.

map(_,[]) -> [];
map(F, [H|T]) -> [F(H) | map(F,T)].


list_loop() -> 
  receive
    {From, {make_list, N}} -> 
      L = make_list(N),
      io:format("Made list of ~p elements : ~p\n",[N,L]), 
      From ! {self(), {list, L}},
      list_loop();
    die ->
      io:format("Goodbye\n");
    {From, Other} -> 
      io:format("\"~p\" from ~p - What?\n", [Other, From]),
      From ! {self(), ok},
      list_loop();
    Other -> 
      io:format("Send messages in the format {self(), Request}"),
      list_loop()
  end.
