# Memcache Server in Erlang

We can compile this program and launch it as follow.

```
% ./rebar compile
% erl -pa ebin/
Eshell V6.4  (abort with ^G)
1> kvs:start().
ok
2>
```

We can connect to this server using [`erlmc`](https://github.com/Spawnerl/erlmc) in another terminal.

```
% git clone https://github.com/Spawnerl/erlmc
% cd erlmc
% make
% erl -pa ebin/
Erlang/OTP 17 [erts-6.1] [source] [64-bit halfword] [smp:3:3] [async-threads:10] [kernel-poll:false]

Eshell V6.1  (abort with ^G)
1> erlmc:start().
ok
2> erlmc:version().
[{{"localhost",11211},<<"1.3.1">>}]
3> 
```

We can launch an observer by the following command on the interpreter.

```
2> observer:start().
ok
```


## See Also

Please refer the following documents to understand [this server program](https://github.com/Spawnerl/kvs/blob/master/src/kvs_server.erl).

* [application](http://erlang.org/doc/man/application.html)
* [supervisor](http://erlang.org/doc/man/supervisor.html)
* [gen_server](http://erlang.org/doc/man/gen_server.html)
