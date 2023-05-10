-module(mnesia_bench_recorder).

-export([mktabs/0]).
-export([record/2, latency/1]).


mktabs() ->
    ets:new(timings_read, [named_table, {write_concurrency, true}, public]),
    ets:new(timings_write, [named_table, {write_concurrency, true}, public]).

record(read, Time) ->
    ets:insert(timings_read, {mono(), Time});

record(write, Time) ->
    ets:insert(timings_write, {mono(), Time}).

latency(read) ->
    average_and_flush(timings_read);

latency(write) ->
    average_and_flush(timings_write).

% private

average_and_flush(Tab) ->
    ets:safe_fixtable(Tab, true),
    Size = ets:info(Tab, size),
    Total = ets:foldl(fun ({_, Timing}, Total) -> Total + Timing end, 0, Tab),
    ets:safe_fixtable(Tab, false),
    ets:delete_all_objects(Tab),
    case Size of
        0 -> -1;
        _ -> Total / Size
    end.

mono() ->
    erlang:monotonic_time().
