-module(math_functions).
-export([odd/1, even/1, my_map/2, my_filter/2, split/1]).

odd(Number) -> Number rem 2 =:= 1.  
even(Number) -> Number rem 2 =:= 0.  

% from StackOverflow
% even(X) when X >= 0 -> (X band 1) == 0.
% odd(X) when X > 0 -> not even(X).

my_map(F, [H|T]) -> [F(H) | my_map(F, T)];
my_map(_F, []) -> [].

my_filter(F, [H|T]) ->
    case (F(H)) of
        true -> [H|my_filter(F, T)];
	false -> my_filter(F, T)
    end;
my_filter(_F, []) -> [].

split(L) -> split(L, {odd, []}, {even, []}).
split([H|T], {odd, Odd}, {even, Even}) ->
    case (H rem 2) of
        1 -> split(T, {odd, [H|Odd]}, {even, Even});
        0 -> split(T, {odd, Odd}, {even, [H|Even]})
    end;
split([], {odd, Odd}, {even, Even}) -> {Odd, Even}.

