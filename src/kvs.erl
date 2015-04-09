-module(kvs).

-export([start/0]).


start() ->
    application:start(kvs).
