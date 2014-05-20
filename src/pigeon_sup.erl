
-module(pigeon_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args, Type),
        {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

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
                ?CHILD(ocw_dnssd,          [pigeon], worker),
                ?CHILD(pigeon_metrics,     [],       worker),
                ?CHILD(pigeon_message_sup, [],       supervisor),
                ?CHILD(pigeon_radio_sup,   [],       supervisor)
                ]} }.

