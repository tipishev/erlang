-module(day3_tests).
-include_lib("eunit/include/eunit.hrl").

-define(assertMinDistance(Input, ExpectedDistance),
        ?_assertEqual(day3:distance(Input), ExpectedDistance)).

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
         6)}
    ].
