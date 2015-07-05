-module(hello).

-export([hello/0]).

hello() ->
    gen_server:call(hello_worker, hello).
