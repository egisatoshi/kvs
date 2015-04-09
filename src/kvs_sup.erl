
-module(kvs_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Listen) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Listen]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Listen]) ->
    {ok, {{one_for_one, 5, 10}, [{kvs_acceptor_1, {kvs_acceptor, start_link, [Listen]} ,permanent, 5000, worker, [kvs_acceptor]},
                                 {kvs_acceptor_2, {kvs_acceptor, start_link, [Listen]} ,permanent, 5000, worker, [kvs_acceptor]}
                                ]}}.
