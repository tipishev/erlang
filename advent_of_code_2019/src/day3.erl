-module(day3).

-behaviour(aoc_solution).

%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%% TODO macro to exclude this export line
%% for tests
-export([parse_segment/1, affected_spots/1, delta_spots/2]).


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
    undefined.

delta_spots(_LastSpot={X, Y}, _Segment={Direction, Length}) ->
    case Direction of
        right -> [{X + Delta, Y} || Delta <- lists:seq(1, Length)];
        up -> [{X, Y + Delta} || Delta <- lists:seq(1, Length)];
        left -> [{X - Delta, Y} || Delta <- lists:seq(1, Length)];
        down -> [{X, Y - Delta} || Delta <- lists:seq(1, Length)]
    end.
