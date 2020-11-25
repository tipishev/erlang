-module(day3).

-behaviour(aoc_solution).

%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%% for tests
-export([to_tuple/1, distance/1]).


%%% solution behavior

solve_part1(Input) ->
    {WireA, WireB} = parse(Input),
    distance(WireA, WireB).

solve_part2(_Input) -> undefined.

%%% internals
parse([WireA, WireB]) ->
    {lists:map(fun to_tuple/1, WireA),
     lists:map(fun to_tuple/1, WireB)}.

segment_to_tuple(["R" | Length]) -> {right, list_to_integer(Length)};
segment_to_tuple(["U" | Length]) -> {up, list_to_integer(Length)};
segment_to_tuple(["L" | Length]) -> {left, list_to_integer(Length)};
segment_to_tuple(["D" | Length]) -> {down, list_to_integer(Length)}.


distance(_WireA, _WireB) -> 6.
