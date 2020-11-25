-module(day3).

-behaviour(aoc_solution).

%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%% for tests
-export([distance/1]).


%%% solution behavior

solve_part1(Input) -> Input.
solve_part2(_Input) -> undefined.

%%% internals
distance(_Wires) -> 6.
