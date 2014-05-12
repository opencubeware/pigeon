-module(pigeon_gpio).

%% API
-export([mode/2,
         write/2,
         interrupt/2]).

-on_load(on_load/0).

%% ===================================================================
%% Module on-load callback
%% ===================================================================
on_load() ->
    LibFile = filename:join(["priv", "lib", ?MODULE]),
    erlang:load_nif(LibFile, 0).

%% ===================================================================
%% API
%% ===================================================================
write(Pin, high) ->
    write_nif(Pin, 1);
write(Pin, low) ->
    write_nif(Pin, 0);
write(Pin, Value) ->
    write_nif(Pin, Value).

%% ===================================================================
%% NIFs
%% ===================================================================
mode(_Pin, _Mode) ->
    error(nif_not_loaded).

write_nif(_Pin, _Value) ->
    error(nif_not_loaded).

interrupt(_Pin, _Type) ->
    error(nif_not_loaded).
