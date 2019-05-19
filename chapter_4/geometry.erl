-module(geometry).
-export([test/0, area/1, perimeter/1]).
-import(math, [pi/0, sqrt/1]).

test() ->
	12 = area({rectangle, 3, 4}),
	144 = area({square, 12}),
	12.566370614359172 = area({circle, 2}),
	6.0 = area({triangle, 3, 4}),

	10 = perimeter({rectangle, 2, 3}),
	18.84955592153876= perimeter({circle, 3}),
	12.0 = perimeter({triangle, 3, 4}),
	tests_worked.

area({rectangle, Width, Height}) -> Width * Height;
area({square, Side}) -> Side * Side;
area({circle, Radius}) -> pi() * Radius * Radius;
area({triangle, Height, Base}) -> (Height * Base) / 2.

perimeter({rectangle, Width, Height}) -> 2 * (Width + Height);
perimeter({square, Side}) -> 4 * Side;
perimeter({circle, Radius}) -> 2 * pi() * Radius;

%% right-angled, let's say
perimeter({triangle, Height, Base}) ->
    Height + Base + sqrt(Height * Height + Base * Base).




