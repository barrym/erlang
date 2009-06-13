-module(bqueue_m).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").

-record(job, { id, body }).

start() ->
  ok = mnesia:start(),
  ok = mnesia:wait_for_tables([job], 5000),
  io:format("Started~n").

add_job(Body, Priority, Delay) ->
  Id = {Priority, date_utils:timestamp() + Delay, {now(), node()}},
  Job = #job{id = Id, body = Body},
  F = fun() ->
      mnesia:write(Job)
  end,
  Result = mnesia:transaction(F),
  Result.

all_jobs() ->
  utils:mnesia_do(qlc:q([X || X <- mnesia:table(job)])).

get_job() ->
  F = fun() -> 
      case mnesia:read(job, mnesia:first(job)) of
        [Job] ->
          mnesia:delete({job, Job#job.id}),
          Job;
        [] ->
          empty;
        Other ->
          io:format("Error : ~p~n", [Other])
      end
  end,
  {atomic, Response} = mnesia:transaction(F),
  Response.


bootstrap() ->
  mnesia:start(),
  mnesia:delete_table(job),
  mnesia:create_table(job, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, job)}]).

fill(Amount) ->
  [add_job(X, X, 100) || X <- lists:seq(1, Amount)].

grab_all() ->
  grab(mnesia:table_info(job, size)).

grab(Amount) ->
  case get_job() of
    empty ->
      io:format("Finished~n");
    Job ->
      io:format("Got ~p : ~p~n", [Amount, Job#job.body]),
      case Amount of
        1 -> 
          io:format("Done~n");
        _ ->
        grab(Amount - 1)
    end
  end.

