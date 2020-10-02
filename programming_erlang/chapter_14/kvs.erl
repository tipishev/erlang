-module(kvs).
-export([start/0, store/2, lookup/1]).

% an exercise in typing
-spec start() -> true.
-spec store(key(), value()) -> true.
-spec lookup(key()) -> value().

-spec rpc(query()) -> true | value().

-type key() :: string().
-type value() :: any().
-type query() :: {store, key(), value()} | {lookup, key()}.

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

% main loop, acts as kvs server when spawned
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
