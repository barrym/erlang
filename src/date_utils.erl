-module(date_utils).
-compile(export_all).
% -export([timestamp/0, timestamp/1, timestamp_to_datetime/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
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

-ifdef(EUNIT).

timestamp_test_() -> 
  {inparallel,
    [
      ?_assertEqual( 1244230930, date_utils:timestamp({1244, 230930, 362862})),
      ?_assertEqual( 1235396117, date_utils:timestamp({1235, 396117, 362862}))
    ]
  }.

timestamp_to_now_test_() -> 
  {inparallel,
    [
      ?_assertEqual( {1244, 230930, 0}, date_utils:timestamp_to_now(1244230930)),
      ?_assertEqual( {1235, 396117, 0}, date_utils:timestamp_to_now(1235396117))
    ]
  }.

timestamp_to_datetime_test_() ->
  {inparallel,
    [
      ?_assertEqual( {{2009, 6, 5}, {20, 42, 10}}, date_utils:timestamp_to_datetime(1244230930)),
      ?_assertEqual( {{2009, 2, 23}, {13, 35, 17}}, date_utils:timestamp_to_datetime(1235396117))
    ]
  }.

-endif.
