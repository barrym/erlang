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

make_list(N) -> make_list(N,1,[]).
make_list(N,Step) -> make_list(trunc(N/Step) * Step, Step,[]).

make_list(Current, _Step, L) when Current =< 0 -> L;
make_list(Current, Step, L) -> make_list(Current - Step, Step, [Current|L]).


%make_list(N) -> make_list(N, []).
%make_list(0, L) -> L;
%make_list(N, L) -> make_list(N - 1, [N|L]).

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

ruby() ->
  Cmd = "ruby foo.rb",
  Port = open_port({spawn, Cmd}, [{packet, 4}, use_stdio, exit_status, binary]),
  Payload = term_to_binary({echo, <<"hello world!">>}),
  port_command(Port, Payload),
  receive
    {Port, {data, Data}} ->
      {result, Text} = binary_to_term(Data),
      io:format("~p~n", [Text])
  end.

