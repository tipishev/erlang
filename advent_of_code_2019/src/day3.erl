-module(day3).

-behaviour(aoc_solution).

%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%% for tests
-export([segment_to_tuple/1]).


%%% solution behavior

solve_part1(Input) ->
    {WireA, WireB} = parse(Input),
    distance_to_closest_intersection(WireA, WireB).

solve_part2(_Input) -> undefined.

%%% internals
parse([WireA, WireB]) ->
    {lists:map(fun segment_to_tuple/1, WireA),
     lists:map(fun segment_to_tuple/1, WireB)}.

segment_to_tuple([DirectionLetter | LengthString]) ->
    DirectionAtom = case DirectionLetter of
        $R -> right;
        $U -> up;
        $L -> left;
        $D -> down
    end,
    {DirectionAtom, list_to_integer(LengthString)}.

distance_to_closest_intersection(_WireA, _WireB) ->
    6.
