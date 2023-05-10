%%%-------------------------------------------------------------------
%% @doc mnesia_bench public API
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_bench_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mnesia_bench_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
