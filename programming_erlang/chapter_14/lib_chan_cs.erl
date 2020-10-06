-module(lib_chan_cs).
%% cs = client/server
-export([start_raw_server/4, start_raw_client/3]).
-export([stop/1]).  % hm, exports on different lines
-export([children/1]).

start_raw_client(Host, Port, PacketLength) ->
    get_tcp:connect(Host, Port, [binary, {active, true}, {packet, PacketLength}]).

start_raw_server(Port, Fun, Max, PacketLength) ->
    Name = port_name(Port),
    case whereis(Name) of
        undefined ->  % yay, it's available!
            Self = self(),
            Pid = spawn_link(fun() ->
                                     cold_start(Self, Port, Fun, Max, PacketLength)
                             end),
            receive  % Pid -> Sender?
                {Pid, ok} ->
                    % XXX possible race condition
                    register(Name, Pid),
                    {ok, self()};
                {Pid, Error} ->
                    Error
            end;
        _Pid ->
            {error, already_started}
    end.

stop(Port) when is_integer(Port) ->
    Name = port_name(Port),
    case whereis(Name) of  % interesting pattern
        undefined ->
            not_started;  % TODO should be an eror
        Pid ->
            exit(Pid, kill),
            % FIXME possible race condition
            (catch unregister(Name)),  % interesting pattern, suppress errors?
            stoped
    end.

children(Port) when is_integer(Port) ->
    port_name(Port) ! {children, self()},
    receive
        {session_server, Reply} -> Reply
    end.

port_name(Port) when is_integer(Port) ->
    list_to_atom("portServer" ++ integer_to_list(Port)).

cold_start(Master, Port, Fun, Max, PacketLength) ->
    process_flag(trap_exit, true),  % become a system process, catch signals
    io:format("Starting a port server on ~p...~n", [Port]),
    case get_tcp:listen(Port, [binary,
                               %% {dontroute, true},
                               {nodelay, true},  % what's that?
                               {packet, PacketLength},
                               {reuseaddr, true},
                               {active, true}]) of
        {ok, Listen} ->
            io:format("Listening to:~p~n", [Listen]),
            Master ! {self(), ok},
            New = start_accept(Listen, Fun),
            %% ready to run now
            socket_loop(Listen, New, [], Fun, Max);
        Error ->
            Master ! {self(), Error}
    end.

socket_loop(Listen, New, Active, Fun, Max) ->
    receive
        {istarted, New} ->
            Active1  = [New|Active], % Active1 is a bad name
            possibly_start_another(false, Listen, Active1, Fun, Max);
        {'EXIT', New, Why} ->
            io:format("Child exit=~p~n", [Why]),
            possibly_start_another(false, Listen, Active, Fun, Max);
        {'EXIT', Pid, Why} ->
            io:format("Child exit=~p~n", [Why]),
            Active1 = lists:delete(Pid, Active),
            possibly_start_another(New, Listen, Active1, Fun, Max);
        {children, From} ->
            From ! {session_server, Active},
            socket_loop(Listen, New, Active, Fun, Max);
        _Other ->
            socket_loop(Listen, New, Active, Fun, Max)
    end.

% aka the is_pid(New) clause
possibly_start_another(New, Listen, Active, Fun, Max)
  when is_pid(New) ->
    socket_loop(Listen, New, Active, Fun, Max);

% hm `false` atom
possibly_start_another(false, Listen, Active, Fun, Max) ->
    case length(Active) of
        N when N < Max ->
            New = start_accept(Listen, Fun),
            socket_loop(Listen, New, Active, Fun, Max);
        _ ->
            socket_loop(Listen, false, Active, Fun, Max)
    end.

start_accept(Listen, Fun) ->
    spawn_link(fun() -> start_child(self(), Listen, Fun) end).

start_child(Parent, Listen, Fun) ->
    case get_tcp:accept(Listen) of
        {ok, Socket} ->
            Parent ! {istarted, self()},
            % TODO handle posix() errors: http://erlang.org/doc/man/inet.html#type-posix
            inet:setopts(Socket, [{packet, 4},
                                  binary,
                                  {nodelay, true},
                                  {active, true}]),
            io:format("running the child: ~p Fun=~p~n",
                      [Socket, Fun]),

            % a repeating pattern: sys-process and catch-case
            process_flag(trap_exit, true),
            case (catch Fun(Socket)) of
                {'EXIT', normal} ->
                    true;
                {'EXIT', Why} ->
                    io:format("Port process dies with exit:~p~n", [Why]),
                    true;
                _ ->  %% not an exit, so everything's ok
                    true
            end
    end.

