-module(day3_tests).
-include_lib("eunit/include/eunit.hrl").

-define(assertMinCabDistance(ExpectedDistance, Input),
        ?_assertEqual(ExpectedDistance, day3:solve_part1(Input))).

-define(assertMinPathDistance(ExpectedDistance, Input),
        ?_assertEqual(ExpectedDistance, day3:solve_part2(Input))).

%%% Helpers

segment_to_tuple_test_() ->
    [
     {"Segment to tuple",
     ?_assertEqual({right, 123}, day3:parse_segment("R123"))}
    ].

delta_spots_test_() ->
    [

     {"Going right",
      ?_assertEqual([{1, 0}, {2, 0}, {3,0}],
                    day3:delta_spots({0, 0}, {right, 3}))},

     {"Going up",
      ?_assertEqual([{0, 1}, {0, 2}],
                    day3:delta_spots({0, 0}, {up, 2}))}

    ].

covered_spots_test_() ->
    [

     {"Single segment check",
      ?_assertEqual(sets:from_list([{1, 0}, {2, 0}, {3,0}]),
                    day3:covered_spots([{right, 3}]))},

     {"Double segment check",
      ?_assertEqual(sets:from_list([{1, 0}, {2, 0}, {3,0}, {3, 1}, {3, 2}]),
                    day3:covered_spots([{right, 3}, {up, 2}]))}

    ].

index_of_test_() ->
    [
     {"Simple index_of",
     ?_assertEqual(3, day3:index_of(15, [3, 14, 15, 92]))}
    ].


%%% Tasks

part1_test_() ->
    [

     {"Explained example",
      ?assertMinCabDistance(
         6,
         [ ["R8","U5","L5","D3"],
           ["U7","R6","D4","L4"] ]
         )},

     {"Example 2",
      ?assertMinCabDistance(
         159,
         [["R75","D30","R83","U83","L12","D49","R71","U7","L72"],
          ["U62","R66","U55","R34","D71","R55","D58","R83"]]
         )},

     {"Example 3",
      ?assertMinCabDistance(
         135,
         [["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"],
          ["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]]
         )
     }

    ].

part2_test_() ->
    [

     {"Explained example",
      ?assertMinPathDistance(
         30,
         [ ["R8","U5","L5","D3"],
           ["U7","R6","D4","L4"] ]
         )},

     {"Example 2",
      ?assertMinPathDistance(
         610,
         [["R75","D30","R83","U83","L12","D49","R71","U7","L72"],
          ["U62","R66","U55","R34","D71","R55","D58","R83"]]
         )},

     {"Example 3",
      ?assertMinPathDistance(
         410,
         [["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"],
          ["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]]
         )
     }

    ].
