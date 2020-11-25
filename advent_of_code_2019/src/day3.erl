-module(day3).

-behaviour(aoc_solution).

%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%% for tests
-export([parse_segment/1]).


%%% solution behavior

solve_part1(Input) ->
    {WireA, WireB} = parse(Input),
    distance_to_closest_intersection(WireA, WireB).

solve_part2(_Input) -> undefined.

%%% internals
parse([WireA, WireB]) ->
    {lists:map(fun parse_segment/1, WireA),
     lists:map(fun parse_segment/1, WireB)}.

parse_segment([DirectionLetter | LengthString]) ->
    DirectionAtom = case DirectionLetter of
        $R -> right;
        $U -> up;
        $L -> left;
        $D -> down
    end,
    {DirectionAtom, list_to_integer(LengthString)}.

distance_to_closest_intersection(WireA, WireB) ->  % FIXME intersect them
    {affected_spots(WireA), affected_spots(WireB)}.

affected_spots(Wire) ->
    lists:foldl(fun add_spots/2,
                _Acc0={_LastSpot={0,0}, _AllSpots=sets:new()},
                Wire).

add_spots(Segment, _AccIn={LastSpot, AllSpots}) ->
    pass.

