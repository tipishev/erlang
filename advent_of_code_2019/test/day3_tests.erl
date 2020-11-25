-module(day3_tests).
-include_lib("eunit/include/eunit.hrl").

-define(assertMinDistance(Input, ExpectedDistance),
        ?_assertEqual(day3:solve_part1(Input), ExpectedDistance)).

helpers_test_() ->
    [
     {"Segment to tuple",
     ?_assertEqual(day3:segment_to_tuple("R123"), {right, 123})}
    ].


part1_test_() ->
    [

     {"Explained example",
      ?assertMinDistance(
         [ ["R8","U5","L5","D3"],
           ["U7","R6","D4","L4"] ],
         6)},

     {"Example 2",
      ?assertMinDistance(
         [["R75","D30","R83","U83","L12","D49","R71","U7","L72"],
          ["U62","R66","U55","R34","D71","R55","D58","R83"]],
         159)},

     {"Example 3",
      ?assertMinDistance(
         [["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"],
          ["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]],
         135)}

    ].
