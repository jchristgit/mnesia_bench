%%%-------------------------------------------------------------------
%% @doc mnesia_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mnesia_bench_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, Here} = file:get_cwd(),
    ServiceConfig = [
        {port, 4000},
        {bind_address, {127, 0, 0, 1}},
        {server_name, "mnesia_bench"},
        {document_root, filename:join(Here, "public")},
        {directory_index, ["index.html"]},
        {mime_types, [{"html", "text/html"}, 
                      {"css", "text/css"},
                      {"js", "application/javascript"}]},
        {server_root, "/tmp"},
        {error_log, "error.log"},
        {erl_script_alias, {"/esi", [mnesia_bench_esi]}}
    ],
    mnesia_bench_recorder:mktabs(),
    {ok, _Pid} = inets:start(httpd, ServiceConfig),
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
