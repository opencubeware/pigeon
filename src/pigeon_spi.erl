-module(pigeon_spi).

%% API
-export([init/2,
         rw/2,
         rw/4]).

-on_load(on_load/0).

%% ===================================================================
%% Module on-load callback
%% ===================================================================
on_load() ->
    LibFile = filename:join(["priv", "lib", ?MODULE]),
    erlang:load_nif(LibFile, 0).

rw(Channel, Data, TPadding, RPadding) ->
    case rw(Channel, <<Data/bitstring, 0:TPadding>>) of
        {ok, <<_:RPadding, Rest/bitstring>>} ->
            {ok, Rest};
        Other ->
            Other
    end.

%% ===================================================================
%% NIFs
%% ===================================================================
init(_Channel, _Speed) ->
    error(nif_not_loaded).

rw(_Channel, _Data) ->
    error(nif_not_loaded).
