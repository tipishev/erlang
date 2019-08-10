-module(user_default).
-compile(export_all).

hello() ->
    "Hello Tim, how are you?".

clear() ->
    io:format("\ec").
