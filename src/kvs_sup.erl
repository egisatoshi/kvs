-module(kvs_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Listen) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Listen]).

init([Listen]) ->
    {ok, {{one_for_one, 5, 10},
          [{kvs_acceptor_1, {kvs_acceptor, start_link, [Listen]} ,permanent, 5000, worker, [kvs_acceptor]},
           {kvs_acceptor_2, {kvs_acceptor, start_link, [Listen]} ,permanent, 5000, worker, [kvs_acceptor]}
          ]}}.
