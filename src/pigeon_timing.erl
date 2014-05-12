-module(pigeon_timing).

%% Module for exact time sleeps
%% Use these functions with an extreme care since they freeze the BEAM!

%% API
-export([msleep/1,
         usleep/1]).

-on_load(on_load/0).

%% ===================================================================
%% Module on-load callback
%% ===================================================================
on_load() ->
    LibFile = filename:join(["priv", "lib", ?MODULE]),
    erlang:load_nif(LibFile, 0).

%% ===================================================================
%% NIFs
%% ===================================================================
msleep(_Miliseconds) ->
    error(nif_not_loaded).

usleep(_Microseconds) ->
    error(nif_not_loaded).
