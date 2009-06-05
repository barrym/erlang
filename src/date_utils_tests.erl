-module(date_utils_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

% >> Time.local(2009,6,5,20,42,10).to_i
% => 1244230930
% 
% >> Time.local(2009,2,23,13,35,17).to_i
% => 1235396117


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
