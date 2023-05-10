-module(mnesia_bench_stresser).

-behaviour(gen_server).
-export([start_link/1, init/1, handle_cast/2, handle_call/3, handle_continue/2]).
-export([run_queries/2]).


start_link(Mode) ->
    gen_server:start_link(?MODULE, Mode, []).


init(Mode) ->
    {ok, Mode, {continue, undefined}}.

handle_continue(_, Mode) ->
    run_queries(Mode),
    {noreply, undefined, {continue, undefined}}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

run_queries(Mode) ->
    Kind = random_kind(),
    Item = rand:uniform(500000),
    MnesiaF = fun() -> run_query(Kind, Item) end,
    TimingF = fun() -> mnesia:activity(Mode, MnesiaF) end,
    {TimingMicros, _} = timer:tc(TimingF),
    mnesia_bench_recorder:record(Kind, TimingMicros / 1000),
    run_queries(Mode).

run_queries(_Mode, 0) ->
    ok;

run_queries(Mode, Countdown) when Countdown >= 0 ->
    Kind = random_kind(),
    Item = rand:uniform(500000),
    MnesiaF = fun() -> run_query(Kind, Item) end,
    TimingF = fun() -> mnesia:activity(Mode, MnesiaF) end,
    {TimingMicros, _} = timer:tc(TimingF),
    mnesia_bench_recorder:record(Kind, TimingMicros / 1000),
    run_queries(Mode, Countdown - 1).


run_query(read, Item) ->
    mnesia:read(bench, Item);

run_query(write, Item) ->
    mnesia_bench_esi:write_one_record(Item * 2, Item).


random_kind() ->
    case rand:uniform(2) of
        1 -> read;
        2 -> write
    end.
