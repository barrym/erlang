-module(blib).
-compile(export_all).

start() ->
  spawn(fun list_loop/0).

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} ->
      Response
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
    Other -> 
      io:format("What?\n"),
      list_loop()
  end.
