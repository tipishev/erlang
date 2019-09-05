-module(world).
% -export([hello/0]).
-export([
         hello/0,
         add/2
         % macrofoo/0
        ]).

% -define(p, o, o]).

% -include("world.hrl").

% hello() -> ?GREETING.
hello() -> "wazzup".
add(A, B) -> A + B.

% macrofoo() -> [f, ?p.
