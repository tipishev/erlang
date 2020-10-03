-module(lib_chan).
-export([cast/2, start_server/0, start_server/1,
         connect/5, disconnect/1, rpc/2]).
-import(list, [map/2, member/2, foreach/2]).

%%---------------------------------------------
%% Server code

start_server() ->
    case os:getenv("HOME") of
        false ->
            exit({ebadEnv, "HOME"});
        Home ->
            start_server(Home ++ "/.erlang_config/lib_chan.conf")
    end.

start_server(ConfigFile) ->
    io:format("lib_chan starting:~p~n", [ConfigFile]),
    case file:consult(ConfigFile) of
        {ok, ConfigData} ->
            io:format("ConfigData=~p~n", [ConfigData]),
            case check_terms(ConfigData) of
                [] ->
                    start_server1(ConfigData);
                Errors ->
                    exit({eDaemonConfig, Errors})
            end;
        {error, Why} ->
            exit({eDaemonConfig, Why})
    end.

%% check_terms() -> [Error]
check_terms(ConfigData) ->
    L = map(fun check_term/1, ConfigData),
    [X || {error, X} <- L].

%% check a single term
check_term({port, P}) when is_integer(P) -> ok;
check_term({service, _, password, _, mfa, _, _, _}) -> ok;
check_term(X) -> {error, {badTerm, X}}.

start_server1(ConfigData) ->
    register(lib_chan,
             spawn(fun() -> start_server2(ConfigData) end)).

start_server2(ConfigData) ->
    % implicitly checks that there is exactly one in config
    [Port] = [P || {port, P} <- ConfigData],
    start_port_server(Port, ConfigData).


start_port_server(Port, ConfigData) ->
    lib_chan_cs:start_raw_server(
      Port,
      fun(Socket) -> start_port_instance(Socket,
                                         ConfigData) end,
      100,
      4).  % what are 100 and 4?

start_port_instance(Socket, ConfigData) ->


