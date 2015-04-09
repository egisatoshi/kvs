-module(kvs_acceptor).

-export([start_link/1, loop/1]).

-include("kvs.hrl").


start_link(Listen) ->
    Pid = spawn_link(fun() -> loop(Listen) end),
    {ok, Pid}.

loop(Listen) ->
    case gen_tcp:accept(Listen, ?LISTEN_TIMEOUT) of
        {ok, Client} ->
            {ok, Pid} = kvs_server:start_link(Client),
            gen_tcp:controlling_process(Client, Pid),
            loop(Listen);
        {error, closed} ->
            stop;
        {error, timeout} ->
            io:format("accept failed: timeout~n"),
            stop;
        {error, Reason} ->
            io:format("accept failed: ~p~n", [Reason]),
            stop
    end.
