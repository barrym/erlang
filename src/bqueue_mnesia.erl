-module(bqueue_mnesia).
-compile(export_all).

-record(job, {id, queue = default, body}).

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

add_job(Body) ->
  Job = #job{ id = {now(), node()}, body = Body},
  F = fun() -> mnesia:write(Job) end,
  Result = mnesia:transaction(F),
  Result.

% get_job() ->
%   get_job(default).
% 
% get_job(Queue) ->
%   do




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
