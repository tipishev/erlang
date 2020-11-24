-module(ppool_supersup).
-behaviour(supervisor).

% user interface
-export([start_link/0, stop/0,
         start_pool/3, stop_pool/1]).

% behaviour callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ppool}, ?MODULE, []).

%% there is no nice way to kill a sup (yet), so brutal kill.
stop() ->
    case whereis(ppool) of
        Pid when is_pid(Pid) ->
            exit(Pid, kill);
        _ -> ok
    end.


init([]) ->
    % allowed to fail 6 times/hour
    {ok, {{one_for_one, _MaxRestart=6, _MaxTime=3600}, []}}.

start_pool(Name, Limit, MFA) ->
    ChildSpec = {Name,
                 {ppool_sup, start_link, [Name, Limit, MFA]}, 
                 permanent,
                 10500,  % shutdown timer, can also be 'infinity'
                 supervisor, [ppool_sup]},
    supervisor:start_child(ppool, ChildSpec).

stop_pool(Name) ->
    supervisor:terminate_child(ppool, Name),
    supervisor:delete_child(ppool, Name).

