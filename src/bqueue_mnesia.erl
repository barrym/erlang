-module(bqueue_mnesia).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(job, { id, queue = default, body, due = 0, priority = 3000 }).

% Eshell V5.7.1  (abort with ^G)
% 1> rr('src/bqueue_mnesia').
% [job]
% 2> X = #job{}.
% #job{queue = default,body = undefined}
% 3> X#job.queue.
% default

start() ->
  io:format("Starting~n"),
  mnesia:start(),
  mnesia:wait_for_tables([job], 5000),
  io:format("Ready~n").

add_job(Body, Priority) ->
  Job = #job{ id = {now(), node()}, body = Body, priority = Priority},
  F = fun() -> mnesia:write(Job) end,
  Result = mnesia:transaction(F),
  Result.

get_job() ->
  get_job("default").

get_job(QueueName) ->
  Q1 = qlc:q([ X || X <- mnesia:table(job) ]),
  Q2 = qlc:sort(Q1, {order, fun(Job1, Job2) -> Job1#job.priority < Job2#job.priority end}),
  H = do(Q2),
  C = qlc:cursor(H),
  R = qlc:next_answers(C,1),
  qlc:delete_cursor(C),
  R.


all_jobs() ->
  Q = qlc:q([X || X <- mnesia:table(job)]),
  do(Q).




do(Query) ->
  F = fun() -> qlc:e(Query) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

clear_table() ->
  mnesia:clear_table(job).

delete_table() ->
  mnesia:delete_table(job).

create_table() ->
  mnesia:create_table(job, [{type, ordered_set},{attributes, record_info(fields, job)}]).

reset() ->
  delete_table(),
  create_table().

dummy() ->
  add_job("one", 5),
  add_job("two", 2),
  add_job("three", 1),
  add_job("four", 7),
  add_job("five", 100),
  add_job("six", 2).

dummy(Amount) ->
  [add_job("foo", X) || X <- lists:seq(1,Amount,1)].
