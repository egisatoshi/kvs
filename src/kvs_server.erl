-module(kvs_server).

-behaviour(gen_server).

-include("kvs.hrl").


-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket}).

start_link(Client) ->
    gen_server:start_link(?MODULE, [Client], []).

init([Client]) ->
    {ok, #state{socket=Client}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State=#state{socket=Socket}) ->
	gen_tcp:send(Socket, kvs_binary(Data)),
	{noreply, State, ?CLIENT_TIMEOUT};
handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{stop, normal, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.


% Exercise. Complete implementation of the handlers for set, get and delete requests
kvs_binary(<< ?Magic_Request:8, ?OP_Version:8, KeySize:16, ExtrasSize:8, _:8, _:16, _:32, _:32, _:64,
              _:ExtrasSize/binary, _:KeySize/binary, _/binary >>) ->
    BodySize = byte_size(?Memcache_Protocol_Version),
    << ?Magic_Response:8, ?OP_Version:8, 0:16, 0:8, 0:8, 0:16, BodySize:32, 0:32, 0:64,
       ?Memcache_Protocol_Version/binary >>;
kvs_binary(_) ->
     error_message(<<"Not implemented">>).

error_message(Cause) ->
    BodySize = byte_size(Cause),
    << ?Magic_Response:8, 0:8, 0:16, 0:8, 0:8, 1:16, BodySize:32, 0:32, 0:64,
       Cause/binary >>.
