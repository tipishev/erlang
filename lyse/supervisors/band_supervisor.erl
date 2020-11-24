-module(band_supervisor).
-behaviour(supervisor).
 
-export([start_link/1]).
-export([init/1]).
 
start_link(Type) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Type).
 
init(softy) ->
    init({one_for_one, 60, 60});
init(lenient) ->
    init({one_for_one, 3, 60});
init(angry) ->
    init({rest_for_one, 2, 60});
init(jerk) ->
    init({one_for_all, 1, 60});

init({RestartStrategy, MaxRestart, MaxTime}) ->
    % NOTE maps can be used instead of tuples
    {ok, {{RestartStrategy, MaxRestart, MaxTime},
         [

          {
           singer,  % ChildId
           {musicians, start_link, [singer, good]},  % StartFunc
           permanent,  % can also be 'transient' or 'temporary'
           1000,
           worker,  % can also be 'supervisor'
           [musicians]  % Modules (child callback) can also be 'dynamic'
          },

          {
           bass,
           {musicians, start_link, [bass, good]},
           temporary,
           1000,
           worker,
           [musicians]
          },

          {
           drum,
           {musicians, start_link, [drum, bad]},
           transient,
           1000,
           worker,
           [musicians]
          },

          {
           keytar,
           {musicians, start_link, [keytar, good]},
           transient,
           1000,
           worker,
           [musicians]
          }
         ]}}.
