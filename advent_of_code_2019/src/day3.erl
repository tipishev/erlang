-module(day3).

-behaviour(aoc_solution).

%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%% TODO macro to exclude this export line
%% for tests
-export([parse_segment/1, covered_spots/1, delta_spots/2, index_of/2]).


%%% solution behavior

solve_part1(Input) ->
    {WireA, WireB} = parse(Input),
    cab_distance_to_closest_intersection(WireA, WireB).

solve_part2(Input) ->
    {WireA, WireB} = parse(Input),
    path_distance_to_closest_intersection(WireA, WireB).

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

path_distance_to_closest_intersection(WireA, WireB) ->
    CommonSpots = sets:to_list(sets:intersection(covered_spots(WireA),
                                                 covered_spots(WireB))),
    WireAPathDistances = path_distances(CommonSpots, WireA),
    WireBPathDistances = path_distances(CommonSpots, WireB),
    TotalPathDistances = [DistA + DistB || DistA <- WireAPathDistances,
                                           DistB <- WireBPathDistances],
    lists:min(TotalPathDistances).

path_distances(CommonSpots, Wire) ->
    CoveredSpots = covered_spots(Wire),
    lists:map(fun(Spot) -> index_of(Spot, CoveredSpots) end, CommonSpots).

%% find the index at which the element is first met in list
index_of(Element, List) ->
    index_of(Element, List, 1).

index_of(_, [], _) -> not_found;
index_of(Element, [Element|_], Index) -> Index;
index_of(Element, [_|Tail], Index) -> index_of(Element, Tail, Index+1).

cab_distance_to_closest_intersection(WireA, WireB) ->
    [SetA, SetB] = [sets:from_list(covered_spots(Wire)) || Wire <- [WireA, WireB]],
    CommonSpots = sets:to_list(sets:intersection(SetA, SetB)),
    lists:min([abs(X) + abs(Y) || {X, Y} <- CommonSpots]).

covered_spots(Wire) ->
    {_FinalSpot, CoveredSpots} = lists:foldl(
                                  fun add_spots/2,
                                  _Acc0={_LastSpot={0,0}, _AllSpots=[]},
                                         Wire),
    CoveredSpots.

add_spots(Segment, _AccIn={LastSpot, AllSpots}) ->
    DeltaSpots = delta_spots(LastSpot, Segment),
    {lists:last(DeltaSpots), AllSpots ++ DeltaSpots}.

delta_spots(_LastSpot={X, Y}, _Segment={Direction, Length}) ->
    Deltas = lists:seq(1, Length),
    case Direction of
        right -> [{X + Delta, Y} || Delta <- Deltas];
        up -> [{X, Y + Delta} || Delta <- Deltas];
        left -> [{X - Delta, Y} || Delta <- Deltas];
        down -> [{X, Y - Delta} || Delta <- Deltas]
    end.
