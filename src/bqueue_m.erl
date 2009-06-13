-module(bqueue_m).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").

-record(job, { id, body, queue }).

start() ->
  ok = mnesia:start(),
  ok = mnesia:wait_for_tables([job], 5000),
  io:format("Started~n").

add_job(Body, Priority, Delay, QueueName) ->
  Id = {Priority, date_utils:timestamp() + Delay, {now(), node()}},
  Job = #job{id = Id, body = Body, queue = QueueName},
  F = fun() ->
      mnesia:write(Job)
  end,
  Result = mnesia:transaction(F),
  Result.

all_jobs() ->
  utils:mnesia_do(qlc:q([X || X <- mnesia:table(job)])).

get_job(QueueName) ->
  F = fun() ->
      MatchHead = #job{id='$1', body='_', queue='$2'},
      Guard = {'=:=','$2',QueueName},
      Result = '$1',
      case mnesia:select(job, [{MatchHead, [Guard], [Result]}],1,read) of
        {[Key], _} -> 
          Job = mnesia:read(job, Key),
          mnesia:delete({job, Key}),
          Job;
        '$end_of_table' ->
          empty;
        Other ->
          io:format("Error: ~p~n", [Other])
      end
  end,
  {atomic, Response} = mnesia:transaction(F),
  Response.


bootstrap() ->
  mnesia:start(),
  mnesia:delete_table(job),
  mnesia:create_table(job, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, job)}]).

fill(Amount) ->
  fill(Amount, "default").

fill(Amount, QueueName) ->
  [add_job(X, X, 100, QueueName) || X <- lists:seq(1, Amount)].

grab_all(QueueName) ->
  grab(mnesia:table_info(job, size), QueueName).

grab(Amount, QueueName) ->
  case get_job(QueueName) of
    empty ->
      io:format("Finished~n");
    Job ->
      io:format("Got ~p : ~p~n", [Amount, Job#job.body]),
      case Amount of
        1 -> 
          io:format("Done~n");
        _ ->
        grab(Amount - 1, QueueName)
    end
  end.

