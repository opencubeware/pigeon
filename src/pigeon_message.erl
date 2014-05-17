-module(pigeon_message).

%% API
-export([handle/1]).

-include("pigeon_message.hrl").

%%%===================================================================
%%% API
%%%===================================================================
handle(<<From:8, _:1, Retry:7, _Len:8, _:8, Data/binary>>) ->
    handle(Data, From).

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle(<<?MSG_NEW_RFID, Rfid/binary>>, From) ->
    pigeon_rfid:new_rfid(From, Rfid);
handle(Other, From) ->
    error_logger:error_msg("Unable to handle message ~p from ~p", [Other,From]).
