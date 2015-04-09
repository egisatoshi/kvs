-define(LISTEN_TIMEOUT, 100000).
-define(CLIENT_TIMEOUT, 10000).

-define(PORT, 11211).
 
-define(Memcache_Protocol_Version, <<"1.3.1">>).
 
-define(Magic_Request,  16#80).
-define(Magic_Response, 16#81).
 
%% Op codes
-define(OP_Get,       16#00).
-define(OP_Set,       16#01).
-define(OP_Add,       16#02).
-define(OP_Replace,   16#03).
-define(OP_Delete,    16#04).
-define(OP_Increment, 16#05).
-define(OP_Decrement, 16#06).
-define(OP_Quit,      16#07).
-define(OP_Flush,     16#08).
-define(OP_GetQ,      16#09).
-define(OP_Noop,      16#0A).
-define(OP_Version,   16#0B).
-define(OP_GetK,      16#0C).
-define(OP_GetKQ,     16#0D).
-define(OP_Append,    16#0E).
-define(OP_Prepend,   16#0F).
-define(OP_Stat,      16#10).