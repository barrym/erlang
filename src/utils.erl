-module(utils).
-compile(export_all).

mnesia_do(QH) ->
  F = fun() -> qlc:e(QH) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

auto_increment() ->
  case whereis(incrementer) of
    undefined ->
      register(incrementer, spawn(fun() -> auto_increment(1) end));
    _ -> 
      true
  end,
  incrementer ! {self(), get},
  receive
    {ok, Num} ->
      Num;
    _ ->
      io:format("error~n")
  end.


auto_increment(Next) ->
  receive
    {From, get} ->
      From ! {ok, Next},
      auto_increment(Next+1);
    die ->
      io:format("Dying~n")
  end.
