-module(kvs).
-export([start/0, store/2, lookup/1]).

% an exercise in type-checking
-spec start() -> true.
-spec store(string(), string()) -> true.

% exported
start() -> register(kvs, spawn(fun() -> loop() end)).
store(Key, Value) -> rpc({store, Key, Value}).
lookup(Key) -> rpc({lookup, Key}).

% helper
rpc(Query) ->
    kvs ! {self(), Query},
    receive
        {kvs, Reply} ->
            Reply
    end.

% main loop
loop() ->
    receive
        {From, {store, Key, Value}} ->
            put(Key, {ok, Value}),
            From ! {kvs, true},
            loop();
        {From, {lookup, Key}} ->
            From ! {kvs, get(Key)},
            loop()
    end.
