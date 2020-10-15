-module(twit_store).

% -export([init/1, store/2, fetch/1]).
-compile(export_all).

-define(TWIT_SIZE, 140).


init(NumTwits) ->
    {ok, F} = file:open("twit_store", [raw, write, binary]),
    file:pwrite(F, NumTwits * ?TWIT_SIZE, <<0>>),
    file:close(F).

store(TwitNum, Twit) ->
    {ok, F} = file:open("twit_store", [raw, write, binary]),


