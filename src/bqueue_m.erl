-module(bqueue_m).
-compile(export_all).
-behaviour(gen_server).
-export([start_link/0]).
-export([put/4, get/1, all/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("src/bqueue_m.hrl").

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Client
put(Body, Priority, Delay, QueueName) -> gen_server:call(?MODULE, {add, {Body, Priority, Delay, QueueName}}).
get(QueueName) -> gen_server:call(?MODULE, {get, QueueName}).
all() -> gen_server:call(?MODULE, all).
stats() -> gen_server:call(?MODULE, stats).
kill() -> gen_server:call(?MODULE, kill).

% Server
init([]) -> 
    ok = mnesia:start(),
    ok = mnesia:wait_for_tables([job,queue], 5000),
    io:format("Started~n"),
    {ok, []}.

handle_call({add, {Body, Priority, Delay, QueueName}}, _From, State) -> 
  add_job(Body, Priority, Delay, QueueName),
  Reply = ok,
  {reply, Reply, State};
handle_call({get, QueueName}, _From, State) -> 
  Job = get_job(QueueName),
  Reply = {ok, Job},
  {reply, Reply, State};
handle_call(all, _From, State) ->
  Reply = {ok, all_jobs()},
  {reply, Reply, State};
handle_call(stats, _From, State) ->
  TotalJobs = mnesia:table_info(job, size),
  QueueStats = utils:mnesia_do(qlc:q([{X#queue.name, {current_jobs, X#queue.current_jobs}} || X <- mnesia:table(queue)])),
  Reply = {ok, [{total_jobs, TotalJobs, QueueStats}]},
  {reply, Reply, State};
handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, Extra) -> {ok, State}.

% Internal
add_job(Body, Priority, Delay, QueueName) ->
  Id = {Priority, date_utils:timestamp() + Delay, {now(), node()}},
  Job = #job{id = Id, body = Body, queue = QueueName},
  F = fun() ->
      increment_queue(QueueName),
      mnesia:write(Job)
  end,
  Result = mnesia:transaction(F),
  Result.

all_jobs() ->
  utils:mnesia_do(qlc:q([X || X <- mnesia:table(job)])).

get_job(QueueName) ->
  F = fun() ->
      MatchHead = #job{id={'$1','$2','$3'}, body='_', queue='$5'},
      Guards = [{'=:=','$5',QueueName}, {'<', '$2', date_utils:timestamp()}],
      Result = {{'$1','$2','$3'}},
      case mnesia:select(job, [{MatchHead, Guards, [Result]}],1,read) of
        {[Key], _} -> 
          Job = mnesia:read(job, Key),
          mnesia:delete({job, Key}),
          decrement_queue(QueueName),
          Job;
        '$end_of_table' ->
          empty;
        Other ->
          io:format("Error: ~p~n", [Other])
      end
  end,
  {atomic, Response} = mnesia:transaction(F),
  Response.

increment_queue(QueueName) ->
  F = fun() ->
      case mnesia:read({queue, QueueName}) of
        [{queue, QueueName, Count}] ->
          mnesia:write(#queue{name = QueueName, current_jobs = Count + 1});
        [] ->
          mnesia:write(#queue{name = QueueName, current_jobs = 1})
      end,
      queues()
  end,
  {atomic, Result} = mnesia:transaction(F),
  Result.

decrement_queue(QueueName) ->
  F = fun() ->
      case mnesia:read({queue, QueueName}) of
        [{queue, QueueName, Count}] when Count > 1 ->
          mnesia:write(#queue{name = QueueName, current_jobs = Count - 1});
        [{queue, QueueName, Count}] when Count =:= 1 ->
          unregister_queue(QueueName);
        [] ->
          io:format("~p does not exist~n", [QueueName])
      end,
      queues()
  end,
  {atomic, Result} = mnesia:transaction(F),
  Result.

unregister_queue(QueueName) ->
  F = fun() ->
      mnesia:delete({queue, QueueName}),
      queues()
  end,
  {atomic, Result} = mnesia:transaction(F),
  Result.

queues() ->
  utils:mnesia_do(qlc:q([X#queue.name || X <- mnesia:table(queue)])).



bootstrap() ->
  mnesia:start(),
  mnesia:delete_table(job),
  mnesia:delete_table(queue),
  mnesia:create_table(job, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, job)}]),
  mnesia:create_table(queue, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, queue)}]).

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
