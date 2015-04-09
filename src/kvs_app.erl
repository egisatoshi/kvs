-module(kvs_app).

-behaviour(application).

-include("kvs.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_, _) ->
    Options = [binary, {packet, raw}, {active, true}, {reuseaddr, true}],
    case gen_tcp:listen(?PORT, Options) of
        {ok, Listen} -> kvs_sup:start_link(Listen);
        {error, _} -> stop
    end.
    

stop(_) ->
    ok.
