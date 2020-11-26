-module(day4_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Tasks

part1_test_() ->
    [

     {"111111 meets these criteria (double 11, never decreases).",
      ?_assert(day4:check(111111))},

     {"223450 does not meet these criteria (decreasing pair of digits 50).",
      ?_assertNot(day4:check(223450))},

     {"123789 does not meet these criteria (no double).",
      ?_assertNot(day4:check(123789))}
    ].
