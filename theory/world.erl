-module(world).
% -export([hello/0]).
-export([add/2]).

% -include("world.hrl").

% hello() -> ?GREETING.
add(A, B) -> A + B.
