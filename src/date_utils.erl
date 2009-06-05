-module(date_utils).
-compile(export_all).
% -export([timestamp/0, timestamp/1, timestamp_to_datetime/1]).
-include_lib("eunit/include/eunit.hrl").

% These all use localtime

timestamp() ->
  timestamp(now()).

timestamp({Mega, Seconds, _}) -> 
  Mega * 1000000 + Seconds.

timestamp_to_datetime(Timestamp) ->
  calendar:now_to_local_time(timestamp_to_now(Timestamp)).

timestamp_to_now(Timestamp) ->
  Mega = Timestamp div 1000000,
  Seconds = Timestamp - Mega * 1000000,
  {Mega, Seconds, 0}.

