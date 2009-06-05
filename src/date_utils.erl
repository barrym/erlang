-module(date_utils).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

timestamp() ->
  timestamp(now()).

timestamp({Mega, Seconds, _}) -> 
  Mega * 1000000 + Seconds.

