-module(mnesia_bench_esi).

-define(TABNAME, bench).

-export([start/3, stop/3, reset/3, create_schema/3, create_table/3, mod_table_copies/3, backup/3, dump_tables/3]).
-export([write_table/3, start_workers/3, stop_workers/3]).
-export([transactions/3, latency/3, erlang_cluster_nodes/3, tabsize/3]).
-export([write_one_record/2]).
-export([format_tabsizes/1]).

-record(entry, {key, value}).


start(Session, _Env, "global") ->
    erpc:multicall(nodes(), mnesia, start, []),
    start(Session, _Env, "local");

start(Session, _Env, _Input) ->
    ok = mnesia:start(),
    mod_esi:deliver(Session, "ok").

create_table(Session, _Env, "global," ++ Type) ->
    TabType = erlang:list_to_existing_atom(Type),
    {atomic, ok} = mnesia:create_table(?TABNAME, [{TabType, [node() | nodes()]}, {record_name, entry}]),
    mod_esi:deliver(Session, "ok");

create_table(Session, _Env, "local," ++ Type) ->
    TabType = erlang:list_to_existing_atom(Type),
    {atomic, ok} = mnesia:create_table(?TABNAME, [{TabType, [node()]}, {record_name, entry}]),
    mod_esi:deliver(Session, "ok").

mod_table_copies(Session, _Env, Input) ->
    [RawKind, Rest] = string:split(Input, ","),
    [RawTab, MoreRest] = string:split(Rest, ","),
    [RawType, RawNode] = string:split(MoreRest, ","),
    Tab = list_to_existing_atom(RawTab),
    Type = list_to_existing_atom(RawType),
    Node = list_to_existing_atom(RawNode),
    case RawKind of
        "add" ->
            {atomic, ok} = mnesia:add_table_copy(Tab, Node, Type);
        "remove" ->
            {atomic, ok} = mnesia:del_table_copy(Tab, Node)
    end,
    mod_esi:deliver(Session, "ok").

backup(Session, _Env, _Input) ->
    ok = mnesia:backup("/tmp/mnesia.bak"),
    mod_esi:deliver(Session, "ok").

dump_tables(Session, _Env, _Input) ->
    {atomic, ok} = mnesia:dump_tables([?TABNAME]),
    mod_esi:deliver(Session, "ok").

write_table(Session, _Env, RawAmount) ->
    Amount = erlang:list_to_integer(RawAmount),
    Tab = mnesia:table(?TABNAME),
    {atomic, Top} = mnesia:transaction(fun() ->
        Sorted = qlc:sort(Tab, [{order, descending}]),
        Cursor = qlc:cursor(Sorted),
        Result = case qlc:next_answers(Cursor) of
            [{entry, Top, _} | _] -> Top;
            _ -> 0
        end,
        ok = qlc:delete_cursor(Cursor),
        Result
    end),
    {atomic, _} = mnesia:transaction(fun() -> write_record(Top, Amount) end),
    mod_esi:deliver(Session, "ok").

stop(Session, _Env, "global") ->
    erpc:multicall(nodes(), mnesia, stop, []),
    stop(Session, _Env, "local");

stop(Session, _Env, _Input) ->
    stopped = mnesia:stop(),
    mod_esi:deliver(Session, "ok").

reset(Session, _Env, "global") ->
    ok = mnesia:delete_schema([node() | nodes()]),
    mod_esi:deliver(Session, "ok");

reset(Session, _Env, _Input) ->
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
    mod_esi:deliver(Session, "ok").

create_schema(Session, _Env, "global") ->
    ok = mnesia:create_schema([node() | nodes()]),
    mod_esi:deliver(Session, "ok");

create_schema(Session, _Env, _Input) ->
    ok = mnesia:create_schema([node()]),
    mod_esi:deliver(Session, "ok").

transactions(Session, _Env, _Input) ->
    Commits = mnesia:system_info(transaction_commits),
    Failures = mnesia:system_info(transaction_failures),
    Restarts = mnesia:system_info(transaction_restarts),
    LogWrites = mnesia:system_info(transaction_log_writes),
    mod_esi:deliver(
      Session,
      to_list(Commits) ++ "," ++ to_list(Failures)
      ++ "," ++ to_list(Restarts) ++ "," ++ to_list(LogWrites)).

latency(Session, _Env, _Input) ->
    ReadLatency = mnesia_bench_recorder:latency(read) * 1000,
    WriteLatency = mnesia_bench_recorder:latency(write) * 1000,
    mod_esi:deliver(Session, to_list(ReadLatency) ++ "," ++ to_list(WriteLatency)).

erlang_cluster_nodes(Session, _Env, _Input) ->
    mod_esi:deliver(Session, io_lib:format("~w", [[node() | nodes()]])).

start_workers(Session, _Env, Input) ->
    [RawCount, RawMode] = string:split(Input, ","),
    Count = erlang:list_to_integer(RawCount),
    Mode = erlang:list_to_existing_atom(RawMode),
    do_start_workers(Count, Mode),
    mod_esi:deliver(Session, "ok").

stop_workers(Session, _Env, _Input) ->
    Children = supervisor:which_children(mnesia_bench_sup),
    kill_children(Children),
    mod_esi:deliver(Session, "ok").

tabsize(Session, _Env, _Input) ->
    Nodes = [node() | nodes()],
    Sizes = erpc:multicall(Nodes, mnesia, table_info, [?TABNAME, size]),
    AnnoSizes = lists:zip(Nodes, Sizes),
    Result = format_tabsizes(AnnoSizes),
    mod_esi:deliver(Session, Result).


format_tabsizes([]) ->
    "";

format_tabsizes([{Node, {ok, Size}} | Sizes]) ->
    erlang:atom_to_list(Node) ++ "," ++ erlang:integer_to_list(Size) ++ "\n" ++ format_tabsizes(Sizes).


% private
to_list(Term) when is_integer(Term) ->
    erlang:integer_to_list(Term);

to_list(Term) when is_float(Term) ->
    erlang:float_to_list(Term).

write_record(_Offset, 0) ->
    ok;

write_record(Offset, N) when N > 0 ->
    write_one_record(Offset, N),
    write_record(Offset, N - 1).

write_one_record(Offset, N) ->
    Key = Offset + N,
    Bin = erlang:integer_to_binary(Key),
    mnesia:write(?TABNAME, #entry{key=Key, value=binary:copy(Bin, 3)}, write).


kill_children([]) -> ok;
kill_children([{{bench_monkey, _} = Id, _, _, _} | Rest]) ->
    supervisor:terminate_child(mnesia_bench_sup, Id),
    kill_children(Rest);

kill_children([_ | Rest]) -> kill_children(Rest).

do_start_workers(0, _Mode) ->
    ok;

do_start_workers(N, Mode) when N > 0 ->
    {ok, _Pid} = supervisor:start_child(
      mnesia_bench_sup,
      #{
        id => {bench_monkey, N},
        start => {mnesia_bench_stresser, start_link, [Mode]},
        restart => temporary
       }
    ),
    do_start_workers(N - 1, Mode).
