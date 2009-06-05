-module(date_utils_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

% >> Time.local(2009,6,5,20,42,10).to_i
% => 1244230930
% 
% >> Time.local(2009,2,23,13,35,17).to_i
% => 1235396117


timestamp_test_() -> 
  [?_assert(date_utils:timestamp({1244,230930,362862}) =:= 1244230930),
   ?_assert(date_utils:timestamp({1235,396117,362862}) =:= 1235396117)
  ].
