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

## Memcache Binary Protocol

```
   Set request:


     Byte/     0       |       1       |       2       |       3       |
        /              |               |               |               |
       |0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|
       +---------------+---------------+---------------+---------------+
      0| 0x80          | 0x01          | 0x00          | 0x05          |
       +---------------+---------------+---------------+---------------+
      4| 0x08          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
      8| 0x00          | 0x00          | 0x00          | 0x12          |
       +---------------+---------------+---------------+---------------+
     12| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
     16| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
     20| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
     24| 0xde          | 0xad          | 0xbe          | 0xef          |
       +---------------+---------------+---------------+---------------+
     28| 0x00          | 0x00          | 0x0e          | 0x10          |
       +---------------+---------------+---------------+---------------+
     32| 0x48 ('H')    | 0x65 ('e')    | 0x6c ('l')    | 0x6c ('l')    |
       +---------------+---------------+---------------+---------------+
     36| 0x6f ('o')    | 0x57 ('W')    | 0x6f ('o')    | 0x72 ('r')    |
       +---------------+---------------+---------------+---------------+
     40| 0x6c ('l')    | 0x64 ('d')    |
       +---------------+---------------+

       Total 42 bytes (24 byte header, 8 byte extras, 5 byte key and
                       5 byte value)

   Field        (offset) (value)
   Magic        (0)    : 0x80
   Opcode       (1)    : 0x01
   Key length   (2,3)  : 0x0005
   Extra length (4)    : 0x08
   Data type    (5)    : 0x00
   Reserved     (6,7)  : 0x0000
   Total body   (8-11) : 0x00000012 (5 + 5 + 8 = 18)
   Opaque       (12-15): 0x00000000
   CAS          (16-23): 0x0000000000000000
   Extras              :
     Flags      (24-27): 0x00000000
     Expiry     (28-31): 0x00000000
   Key          (32-36): The textual string "Hello"
   Value        (37-41): The textual string "World"

   Successful set response:


     Byte/     0       |       1       |       2       |       3       |
        /              |               |               |               |
       |0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|
       +---------------+---------------+---------------+---------------+
      0| 0x81          | 0x01          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
      4| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
      8| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
     12| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
     16| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
     20| 0x00          | 0x00          | 0x00          | 0x01          |
       +---------------+---------------+---------------+---------------+

       Total 24 bytes

   Field        (offset) (value)
   Magic        (0)    : 0x81
   Opcode       (1)    : 0x01
   Key length   (2,3)  : 0x0000
   Extra length (4)    : 0x00
   Data type    (5)    : 0x00
   Status       (6,7)  : 0x0000
   Total body   (8-11) : 0x00000000
   Opaque       (12-15): 0x00000000
   CAS          (16-23): 0x0000000000000001
   Extras              : None
   Key                 : None
   Value               : None
```

```
   GetK request:


     Byte/     0       |       1       |       2       |       3       |
        /              |               |               |               |
       |0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|
       +---------------+---------------+---------------+---------------+
      0| 0x80          | 0x01          | 0x00          | 0x05          |
       +---------------+---------------+---------------+---------------+
      4| 0x08          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
      8| 0x00          | 0x00          | 0x00          | 0x12          |
       +---------------+---------------+---------------+---------------+
     12| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
     16| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
     20| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
     24| 0x48 ('H')    | 0x65 ('e')    | 0x6c ('l')    | 0x6c ('l')    |
       +---------------+---------------+---------------+---------------+
     28| 0x6f ('o')    | 
       +---------------+

       Total 29 bytes (24 byte header, 0 byte extras, 5 byte key and
                       0 byte value)

   Field        (offset) (value)
   Magic        (0)    : 0x80
   Opcode       (1)    : 0x0c
   Key length   (2,3)  : 0x0005
   Extra length (4)    : 0x00
   Data type    (5)    : 0x00
   Reserved     (6,7)  : 0x0000
   Total body   (8-11) : 0x00000005
   Opaque       (12-15): 0x00000000
   CAS          (16-23): 0x0000000000000000
   Extras              : None
   Key            (-28): The textual string "Hello"
   Value               : None

   Successful get response:


     Byte/     0       |       1       |       2       |       3       |
        /              |               |               |               |
       |0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|0 1 2 3 4 5 6 7|
       +---------------+---------------+---------------+---------------+
      0| 0x81          | 0x01          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
      4| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
      8| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
     12| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
     16| 0x00          | 0x00          | 0x00          | 0x00          |
       +---------------+---------------+---------------+---------------+
     20| 0x00          | 0x00          | 0x00          | 0x01          |
       +---------------+---------------+---------------+---------------+
     24| 0x48 ('H')    | 0x65 ('e')    | 0x6c ('l')    | 0x6c ('l')    |
       +---------------+---------------+---------------+---------------+
     28| 0x6f ('o')    | 0x57 ('W')    | 0x6f ('o')    | 0x72 ('r')    |
       +---------------+---------------+---------------+---------------+
     32| 0x6c ('l')    | 0x64 ('d')    |
       +---------------+---------------+

       Total 34 bytes

   Field        (offset) (value)
   Magic        (0)    : 0x81
   Opcode       (1)    : 0x00
   Key length   (2,3)  : 0x0005
   Extra length (4)    : 0x00
   Data type    (5)    : 0x00
   Status       (6,7)  : 0x0000
   Total body   (8-11) : 0x0000000a (5 + 5 = 10)
   Opaque       (12-15): 0x00000000
   CAS          (16-23): 0x0000000000000001
   Extras              : None
   Key          (24-28): The textual string "Hello"
   Value        (29-33): The textual string "World"
```

## See Also

Please refer the following documents to understand [this server program](https://github.com/Spawnerl/kvs/blob/master/src/kvs_server.erl).

* [application](http://erlang.org/doc/man/application.html)
* [supervisor](http://erlang.org/doc/man/supervisor.html)
* [gen_server](http://erlang.org/doc/man/gen_server.html)
