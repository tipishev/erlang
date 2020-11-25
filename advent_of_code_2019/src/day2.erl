-module(day2).

-behaviour(aoc_solution).

%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%% for tests
-export([compute/1]).

%%% solution behavior

solve_part1([Zeroth, _, _ | Tail]) ->
    calculate(Zeroth, Tail, _Verb=12, _Noun=2).

solve_part2([Zeroth, _, _ | Tail]) ->
    SearchSpace = [{Verb, Noun} || Verb <- lists:seq(0, 99),
                                   Noun <- lists:seq(0, 99)],
    {Verb, Noun} = hd(lists:dropwhile(
                        fun({Verb, Noun}) ->
                                calculate(Zeroth, Tail,
                                          Verb, Noun) =/= 19690720 end,
                        SearchSpace)),
    Verb * 100 + Noun.


calculate(Zeroth, Tail, Verb, Noun) ->
    hd(compute([Zeroth, Verb, Noun | Tail])).


%%% internals

compute(Input) ->
    Operations = array:fix(array:from_list(Input)),
    compute(Operations, _Current=0).

compute(Operations, Current) ->
    OpCode = array:get(Current, Operations),
    case OpCode of
        1 ->  % add
            LeftIndex = array:get(Current + 1, Operations),
            RightIndex = array:get(Current + 2, Operations),
            Out_pos = array:get(Current + 3, Operations),
            Left = array:get(LeftIndex, Operations),
            Right = array:get(RightIndex, Operations),
            Result = Left + Right,
            NewOperations = array:set(Out_pos, Result, Operations),
            compute(NewOperations, Current + 4);
        2 ->  % multiply
            LeftIndex = array:get(Current + 1, Operations),
            RightIndex = array:get(Current + 2, Operations),
            Out_pos = array:get(Current + 3, Operations),
            Left = array:get(LeftIndex, Operations),
            Right = array:get(RightIndex, Operations),
            Result = Left * Right,
            NewOperations = array:set(Out_pos, Result, Operations),
            compute(NewOperations, Current + 4);
        99 -> array:to_list(Operations);
        _ -> throw({bad_opcode, OpCode})
    end.
