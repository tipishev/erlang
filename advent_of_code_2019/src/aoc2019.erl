-module(aoc2019).
-export([solve/1, test/1]).

-record(solution, {part1, part2}).
-define(INPUTS_DIR, "inputs").

%%% solve

solve(day1) ->
    Input = read_newline_separated_integers(day1),
    #solution{part1=day1:solve_part1(Input),
              part2=day1:solve_part2(Input)};

solve(day2) ->
    Input = read_comma_separated_integers(day2),
    #solution{part1=day2:solve_part1(Input),
              part2=day2:solve_part2(Input)}.

%%% test

test(Day) when is_atom(Day) ->
    TestModuleName =unicode:characters_to_list([atom_to_list(Day),
                                               "_tests"]),
    TestModule = list_to_atom(TestModuleName),
    eunit:test(TestModule, [verbose]).

%% reads newline-separated integers from "inputs/Filename"
read_newline_separated_integers(Filename) ->
    Fullpath = filename:join([?INPUTS_DIR, Filename]),
    {ok, Data} = file:read_file(Fullpath),
    Lines = string:tokens(binary_to_list(Data), "\n"),
    lists:map(fun erlang:list_to_integer/1, Lines).

%% reads comma-separated integers from "inputs/Filename"
read_comma_separated_integers(Filename) ->
    Fullpath = filename:join([?INPUTS_DIR, Filename]),
    {ok, Data} = file:read_file(Fullpath),
    StringNumbers = string:tokens(binary_to_list(Data), ",\n"),
    lists:map(fun erlang:list_to_integer/1, StringNumbers).