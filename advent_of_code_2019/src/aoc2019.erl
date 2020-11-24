-module(aoc2019).
-export([solve/1]).

-record(solution, {part1, part2}).

solve(day1) ->
    #solution{part1=day1:solve_part1(read_integers(day1)),
              part2=undefined}.


%% reads a list of integers from "inputs/Filename"
read_integers(Filename) ->
    Fullpath = filename:join(["inputs", Filename]),
    {ok, Data} = file:read_file(Fullpath),
    Lines = string:tokens(binary_to_list(Data), "\n"),
    lists:map(fun erlang:list_to_integer/1, Lines).
