-module(twit_store).

% -export([init/1, store/2, fetch/1]).
-compile(export_all).

-define(TWIT_SIZE, 140).

init(NumTwits) ->
    {ok, S} = file:open("twit_store", write),
    write_empty_lines(S, NumTwits),
    file:close(S).

store(TwitNum, Twit) ->
    {ok, S} = file:open("twit_store", write).

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
