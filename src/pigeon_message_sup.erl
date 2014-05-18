-module(pigeon_message_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    {ok, {{one_for_one, 5, 10},
          [
                ?CHILD(pigeon_control, pigeon_control, worker, []),
                ?CHILD(pigeon_rfid, pigeon_rfid, worker, [])
          ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
