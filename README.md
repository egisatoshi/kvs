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

We can connect to this server using `erlmc` in another terminal.

```
```

We can launch an observer by the following command on the interpreter.

```
2> observer:start().
ok
```


## See Also

Please refer the following tutorial or documents to understand [this server program](https://github.com/Spawnerl/kvs/blob/master/kvs_server.erl).

* [Processes](http://www.erlang.org/doc/reference_manual/processes.html)
* [gen_tcp](http://erlang.org/doc/man/gen_tcp.html)