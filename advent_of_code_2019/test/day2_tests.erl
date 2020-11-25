-module(day2_tests).
-include_lib("eunit/include/eunit.hrl").

-define(assertComputesTo(Input, ExpectedOutput),
        ?_assertEqual(day2:compute(Input), ExpectedOutput)).

part1_test_() ->
    [

     {"Explained example",
      ?assertComputesTo([1,9,10,3, 2,3,11,0, 99,30,40,50],
                        [3500,9,10,70, 2,3,11,0, 99, 30,40,50])},

     {"1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2).",
      ?assertComputesTo([1,0,0,0,99],
                        [2,0,0,0,99])},

     {"2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).",
      ?assertComputesTo([2,3,0,3,99],
                        [2,3,0,6,99])},


     {"2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801).",
      ?assertComputesTo([2,4,4,5,99,0],
                        [2,4,4,5,99,9801])},

     {"2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).",
      ?assertComputesTo([2,3,0,3,99],
                        [2,3,0,6,99])}
    ].
