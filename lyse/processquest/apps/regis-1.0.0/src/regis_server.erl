-module(regis_server).
-behaviour(gen_server).

-export([start_link/0, stop/0, register/2, unregister/1, whereis/1,
         get_names/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {pid, name}).

%%% Interface

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

register(Name, Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {register, Name, Pid}).

unregister(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

whereis(Name) ->
    gen_server:call(?MODULE, {whereis, Name}).

get_names() ->
    gen_server:call(?MODULE, get_names).


% gen_server callbacks

init([]) ->
    {ok, #state{pid = gb_trees:empty(),
                name = gb_trees:empty()}}.

handle_call({register, Name, Pid}, _From, S = #state{pid=P, name=N}) ->
    case {gb_trees:is_defined(Pid, P), gb_trees:is_defined(Name, N)} of
        {true, _} ->
            {reply, {error, already_named}, S};
        {_, true} ->
            {reply, {error, name_taken}, S};
        {false, false} ->
            Ref = erlang:monitor(process, Pid),
            {reply, ok, S#state{pid=gb_trees:insert(Pid, {Name, Ref}, P),
                                name=gb_trees:insert(Name, {Pid, Ref}, N)}}
    end;
handle_call({unregister, Name}, _From, S = #state{pid=P, name=N}) ->
    case gb_trees:lookup(Name, N) of
        {value, {Pid, Ref}} ->
            erlang:demonitor(Ref, [flush]),
            {reply, ok, S#state{pid=gb_trees:delete(Pid, P),
                                name=gb_trees:delete(Name, N)}};
        none ->
            {reply, ok, S}
    end;
