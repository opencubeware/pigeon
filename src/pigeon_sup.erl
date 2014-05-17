
-module(pigeon_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
                {pigeon_rfid,
                 {pigeon_rfid, start_link, []}, permanent, 5000, worker,
                 [pigeon_rfid]},
                {pigeon_rfm70,
                 {pigeon_rfm70, start_link, []}, permanent, 5000, worker,
                 [pigeon_rfm70]},
                {pigeon_tdma,
                 {pigeon_tdma, start_link, []}, permanent, 5000, worker,
                 [pigeon_tdma]} 
                ]} }.

