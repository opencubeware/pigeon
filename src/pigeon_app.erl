-module(pigeon_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    folsom_metrics:new_history(pigeon_traffic),
    folsom_metrics:new_histogram(pigeon_retry),
    folsom_metrics:new_histogram(pigeon_devices, slide),
    folsom_metrics:new_spiral(pigeon_tx),
    folsom_metrics:new_spiral(pigeon_rx),
    pigeon_sup:start_link().

stop(_State) ->
    ok.
