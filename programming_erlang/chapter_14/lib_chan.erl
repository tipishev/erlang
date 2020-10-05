-module(lib_chan).
-export([cast/2, start_server/0, start_server/1,
         connect/5, disconnect/1, rpc/2]).
-import(lists, [map/2, member/2, foreach/2]).
-import(lib_chan_mm, [send/2, close/1]).

%%---------------------------------------------
%% Server code

% checks that default config exists
start_server() ->
    case os:getenv("HOME") of
        false ->
            exit({ebadEnv, "HOME"});
        Home ->
            start_server(Home ++ "/.erlang_config/lib_chan.conf")
    end.

%  checks that config is correct
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
    % hm.. why not `L = map(check_term, ConfigData),` ?
    L = map(fun check_term/1, ConfigData),
    [X || {error, X} <- L].

%% check a single term
% nice use of pattern matching (PM)
check_term({port, P}) when is_integer(P) -> ok;
check_term({service, _, password, _, mfa, _, _, _}) -> ok;
check_term(X) -> {error, {badTerm, X}}.

% spawns and registers `lib_chan`
start_server1(ConfigData) ->
    register(lib_chan,
             spawn(fun() -> start_server2(ConfigData) end)).

% extracts exactly one port setting with PM and List Comprehension (LC)
start_server2(ConfigData) ->
    [Port] = [P || {port, P} <- ConfigData],
    start_port_server(Port, ConfigData).


start_port_server(Port, ConfigData) ->
    %% This is where the low-level connections is handled
    %% We must become a middle man
    %% But first we spawn a connection handler
    

    % NB, touching raw ports and sockets here
    lib_chan_cs:start_raw_server(
      Port,
      % interesting, passes in a partial function (PF)
      fun(Socket) -> start_port_instance(Socket, ConfigData) end,
      100,
      4).  % what are 100 and 4?

% it only needs a Socket to work
start_port_instance(Socket, ConfigData) ->
    MM = self(),
    Controller = spawn_link(fun() -> start_erl_port_server(MM, ConfigData) end),

    % TODO better understand `lib_chan_mm:loop`
    lib_chan_mm:loop(Socket, Controller).

% tries to get a service def from the config
start_erl_port_server(MM, ConfigData) ->
    receive
        {chan, MM, {startService, Mod, ArgC}} ->
            case get_service_definition(Mod, ConfigData) of
                % MFA is a tuple
                {yes, Pwd, MFA} ->
                    case Pwd of
                        none ->  % `none` is a signal of successful auth
                                send(MM, ack),
                                really_start(MM, ArgC, MFA);
                        _ ->
                                do_authentication(Pwd, MM, ArgC, MFA)
                    end;
                no ->
                    io:format("sending bad service~n"),
                    send(MM, badService),
                    close(MM)
            end;
        Any ->
            io:format("*** ErL port Server got: ~p ~p~n", [MM, Any]),
            exit({protocolViolation, Any})
    end.

do_authentication(Pwd, MM, ArgC, MFA) ->
    Challenge = lib_chan_auth:make_challenge(),
    send(MM, {challenge, Challenge}),
    receive
        {chan, MM, {response, Response}} ->
            case lib_chan_auth:is_response_correct(Challenge, Response, Pwd) of
                true ->
                    send(MM, ack),
                    really_start(MM, ArgC, MFA);
                false ->
                    send(MM, authFail),
                    close(MM)
            end
    end.

%% `ArgC` come from client, `ArgS` from server.
%% auth worked, so now we start
really_start(MM, ArgC, {Mod, Func, ArgS}) ->
    % error handling with case(catch)
    case (catch apply(Mod, Func, [MM, ArgC, ArgS])) of
        {'EXIT', normal} -> true;
        {'EXIT', Why} ->
            io:format("server error:~p~n", [Why]);
        Why ->
            io:format(
              "server error should die with exit(normal) "
              "was:~p~n", [Why]
             )
    end.

%% get_service_definition(Name, ConfigData)

get_service_definition(
  Mod, [{service, Mod, password, Pwd, mfa, M, F, A}|_]) ->
    {yes, Pwd, {M, F, A}};  % `yes`: service is found, tuple-pack MFA
get_service_definition(Name, [_|T]) -> % otherwise
    get_service_definition(Name, T);  % check the tail
get_service_definition(_, []) -> no.  % whelp, `no` service.

%% Client connection code
%% connect(...) -> {ok , MM} | Error

% checks the secret with authenticate
connect(Host, Port, Service, Secret, ArgC) ->
    Self = self(),  % act as a parent
    MM = spawn(fun() -> connect(Self, Host, Port) end),
    receive
        {MM, ok} ->
            case authenticate(MM, Service, Secret, ArgC) of
                ok -> {ok, MM};
                Error -> Error
             end;
        {MM, Error} ->
            Error
    end.

% assumes the secret has worked
connect(Parent, Host, Port) ->
    case lib_chan_cs:start_raw_client(Host, Port, 4) of
        {ok, Socket} ->
            Parent ! {self(), ok},
            lib_chan_mm:loop(Socket, Parent);
        Error ->
            Parent ! {self(), Error}
    end.

authenticate(MM, Service, Secret, ArgC) ->
    send(MM, {startService, Service, ArgC}),
    %% we should get back a challenge or an ack or closed socket
    receive
        {chan, MM, ack} ->
            ok;
        {chan, MM, {challenge, C}} ->  % challenge-response dialogue
            R = lib_chan_autn:make_response(C, Secret),
            send(MM, {response, R}),
            receive
                {chan, MM, ack} ->
                    ok;
                {chan, MM, authFail} ->
                    wait_close(MM),
                    {error, authFail};
                Other ->
                    {error, Other}
            end;
        {chan, MM, badService} ->
            wait_close(MM),
            {error, badService};
        Other ->
            {error, Other}
    end.

% wait 5 seconds before closing the connection
wait_close(MM) ->
    receive
        {chan_closed, MM} -> true  % good
    after 5000 ->
              io:format("**error lib_chan~n"),
              true  % ok
    end.

disconnect(MM) -> close(MM).

rpc(MM, Query) ->
    send(MM, Query),
    receive
        {chan, MM, Reply} -> Reply  % unpack reply for consumption
    end.

% just an alias
cast(MM, Query) -> send(MM, Query).
