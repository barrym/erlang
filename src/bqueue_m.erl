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
      [Job] = mnesia:read(job, mnesia:first(job)),
      mnesia:delete({job, Job#job.id}),
      Job
  end,
  mnesia:transaction(F).


bootstrap() ->
  mnesia:start(),
  mnesia:delete_table(job),
  mnesia:create_table(job, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, job)}]).
