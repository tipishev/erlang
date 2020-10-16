-module(twit_store).

-export([init/1, store/2, fetch/1]).

-define(TWIT_SIZE, 140).

%%% Public Functions

init(NumTwits) ->
    {ok, S} = file:open("twit_store", write),
    write_empty_lines(S, NumTwits),
    file:close(S).

store(TwitNum, Twit) ->
    AsIs = read_lines("twit_store"),
    ToBe = replace(AsIs, TwitNum, Twit),
    write_lines("twit_store", ToBe),
    ok.

fetch(TwitNum) ->
    lists:nth(TwitNum + 1, read_lines("twit_store")).


%%% Underhood

write_empty_lines(_, 0) -> ok;
write_empty_lines(S, Num) ->
    io:format(S, "~n", []),
    write_empty_lines(S, Num - 1).

read_lines(Filename) ->
    {ok, S} = file:open(Filename, read),
    Lines = read_lines(S, []),
    file:close(S),
    Lines.

read_lines(S, Acc) ->
    case io:get_line(S, '') of
        eof -> lists:reverse(Acc); 
        Line -> read_lines(S, [string:chomp(Line)|Acc])
    end.

write_lines(Filename, Lines) ->
    {ok, S} = file:open(Filename, write),
    writer_lines_to_device(S, Lines),
    file:close(S),
    ok.

writer_lines_to_device(_IoDevice, []) -> ok;
writer_lines_to_device(IoDevice, [H|T]) ->
    io:format(IoDevice, "~s~n", [H]),
    writer_lines_to_device(IoDevice, T).

replace(List, Index, Element) -> 
    replace(List, _Current=0, Index, Element, []).

replace([], _, _, _, Acc) -> lists:reverse(Acc);
replace([_H|T], Current, Index=Current, Element, Acc) ->
    replace(T, Current + 1, Index, Element, [Element|Acc]);
replace([H|T], Current, Index, Element, Acc) ->
    replace(T, Current + 1, Index, Element, [H|Acc]).
