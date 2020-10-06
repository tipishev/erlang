%% Protocol
%% To the controlling process
%%      {chan, MM, Term}
%%      {chan_closed, MM}
%% From any process
%%      {send, Term}
%%      close

-module(lib_chan_mm).

%% TCP Middle Man
%%      Models the interface to gen_tcp

-export([loop/2, send/2, close/1,
         controller/2, set_trace/2, trace_with_tag/2]).

% not to be confused with erlang:send aka !-alias
send(Pid, Term) -> Pid ! {send, Term}.
close(Pid) -> Pid ! close.
controller(Pid, Pid1) -> Pid ! {setController, Pid1}.
set_trace(Pid, X) -> Pid ! {trace, X}.

% looks like a debugging function
trace_with_tag(Pid, Tag) ->
    set_trace(Pid, {true, fun(Msg) ->
                                io:format("MM: ~p ~p~n", [Tag, Msg])
                          end}).

loop(Socket, Pid) ->
    %% trace_with_tag(self(), trace),
    process_flag(trap_exit, true),
    loop1(Socket, Pid, _Trace=false).

% bad name
loop1(Socket, Pid, Trace) ->
    receive
        {tcp, Socket, Bin} ->
            Term = binary_to_term(Bin),
            trace_it(Trace, {socketReceived, Term}),
            Pid ! {chan, self(), Term},
            loop1(Socket, Pid, Trace);
        {tcp_closed, Socket} ->
            trace_it(Trace, socketClosed),
            Pid ! {chan_closed, self()};

        {'EXIT', Pid, Why} ->
            trace_it(Trace, {controllingProcessExit, Why}),
            gen_tcp:close(Socket);
        {setController, Pid1} ->
            trace_it(Trace, {changedController, Pid}),
            loop1(Socket, Pid1, Trace);
        {trace, Trace1} ->
            trace_it(Trace, {setTrace, Trace1}),
            gen_tcp:close(Socket);
        close ->
            trace_it(Trace, closedByClient),
            gen_tcp:close(Socket);
        {send, Term} ->
            trace_it(Trace, {sendingMessage, Term}),
            gen_tcp:send(Socket, term_to_binary(Term)),
            loop1(Socket, Pid, Trace);
        UUg ->
            io:format("lib_chan_mm: protocol error: ~p~n", [UUg]),
            loop1(Socket, Pid, Trace)
    end.

% debugging 
trace_it(false, _) -> void;
trace_it({true, F}, M) -> F(M).

