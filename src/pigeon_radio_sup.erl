-module(pigeon_radio_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    {ok, { {one_for_all, 5, 10}, [
                ?CHILD(pigeon_rfm70, worker),
                ?CHILD(pigeon_metrics, worker),
                ?CHILD(pigeon_tdma, worker)
                ]} }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
