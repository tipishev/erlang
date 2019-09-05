-module(lib_misc).
-export([permutations/1, my_tuple_to_list/1, my_time_func/1]).

permutations([]) -> [[]];
permutations(L) -> [[H|T] || H <- L, T <- permutations(L--[H])].

% my_tuple_to_list({A, B, C}) -> [A, B, C].
my_tuple_to_list(T) -> my_tuple_to_list(T, 1, tuple_size(T)).
my_tuple_to_list(T, Pos, Size) when Pos =< Size -> [element(Pos, T) | my_tuple_to_list(T, Pos+1, Size)];
my_tuple_to_list(_T, _Pos, _Size) -> [].

my_time_func(F) -> my_time_func(F, erlang:system_time(microsecond)).
my_time_func(F, StartTime) -> {F(), erlang:system_time(microsecond) - StartTime}.
