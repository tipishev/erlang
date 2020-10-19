-module(broadcast).
-export([send/1, listen/0]).

send(IoList) ->
    case inet:ifget("wlp2s0", [broaddr
