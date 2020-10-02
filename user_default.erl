-module(user_default).
-export([hello/0, clear/0]).

hello() ->
    "Hello Tim, how are you?".

clear() ->
    io:format("\ec").
