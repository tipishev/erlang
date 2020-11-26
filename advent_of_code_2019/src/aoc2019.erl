-module(aoc2019).
-export([solve/1, test/1, test/2]).

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
              part2=day2:solve_part2(Input)};

solve(day3) ->
    Input = read_newline_comma_separated_strings(day3),
    #solution{part1=day3:solve_part1(Input),
              part2=day3:solve_part2(Input)};

solve(day4) ->
    Input = {367479, 893698},
    #solution{part1=day4:solve_part1(Input),
              part2=day4:solve_part2(Input)}.

%%% test

%% test the whole day
test(Day) when is_atom(Day) ->
    TestModuleName =unicode:characters_to_list([atom_to_list(Day),
                                               "_tests"]),
    TestModule = list_to_atom(TestModuleName),
    eunit:test(TestModule, [verbose]).
 
%% test a single test case within a day
test(Day, TestCase) when is_atom(Day), is_atom(TestCase) ->
    TestModuleName =unicode:characters_to_list([atom_to_list(Day),
                                               "_tests"]),
    TestModule = list_to_atom(TestModuleName),
    eunit:test({generator, fun TestModule:TestCase/0}).



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

%% reads comma and newline separated strings from "inputs/Filename"
read_newline_comma_separated_strings(Filename) ->
    Fullpath = filename:join([?INPUTS_DIR, Filename]),
    {ok, Data} = file:read_file(Fullpath),
    Lines = string:tokens(binary_to_list(Data), "\n"),
    lists:map(fun(Line) -> string:tokens(Line, ",") end, Lines).
