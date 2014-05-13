-module(pigeon_tdma).

-behaviour(gen_server).

%% API
-export([start_link/0,
         send/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {buffer}).

%% length of a tdma window in miliseconds
-define(TDMA_WINDOW, 50).
-define(MAX_RETRIES, 100).
-define(SYNC, <<250, "syn">>).
-define(RECEIVE_MESSAGE, fun(Msg) -> ?MODULE ! {message, Msg} end).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send(Id, Data) when byte_size(Data) =< 28 ->
    gen_server:call(?MODULE, {send, Id, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    pigeon_rfm70:add_callback(?RECEIVE_MESSAGE),
    schedule_window(send_sync),
    {ok, #state{buffer = []}}.

handle_call({send, Id, Data}, _From, #state{buffer=Buffer}=State) ->
    NewBuffer = add_packet_to_ack(Buffer, Id, Data, []),
    {reply, ok, State#state{buffer=NewBuffer}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(send_sync, State) ->
    pigeon_rfm70:send(?SYNC),
    schedule_window(flush_buffer),
    {noreply, State};
handle_info(flush_buffer, #state{buffer=Buf}=State) ->
    NewBuf = send_buffer(Buf, []),
    schedule_window(send_sync),
    {noreply, State#state{buffer=NewBuf}};
handle_info({message, Message}, #state{buffer=Buf}=State) ->
    error_logger:info_msg("Received message: ~p", [Message]),
    NewBuf = handle_message(Message, Buf),
    {noreply, State#state{buffer=NewBuf}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    pigeon_rfm70:delete_callback(?RECEIVE_MESSAGE), 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
schedule_window(Window) ->
    erlang:send_after(?TDMA_WINDOW, self(), Window).

send_buffer([], Acc) ->
    lists:reverse(Acc);
send_buffer([<<_:8,1:1,0:23>>=Ack|Rest], Acc) ->
    error_logger:info_msg("Sending message: ~p", [Ack]),
    pigeon_rfm70:send(Ack),
    send_buffer(Rest, Acc);
send_buffer([<<Head:9,Retry:7,Tail/bitstring>>=Packet|Rest], Acc) ->
    error_logger:info_msg("Sending message: ~p", [Packet]),
    case Retry of
        Greater when Greater > ?MAX_RETRIES ->
            error_logger:error_msg("Maximum retries reached for packet ~p",
                                   [Packet]),
            send_buffer(Rest, Acc);
        _ ->
            pigeon_rfm70:send(Packet),
            NextRetry = <<Head:9,(Retry+1):7,Tail/bitstring>>,
            send_buffer(Rest, [NextRetry|Acc])
    end.

handle_message(<<Id:8,0:1,Retry:7,Len:8,Pos:8,Data/binary>>=NoAck, Buf) ->
    %% @todo handle message actually
    add_ack_to_packet(Buf, Id, []);
handle_message(<<Id:8,1:1,Rest/bitstring>>=Ack, Buf) ->
    %% @todo handle message actually
    remove_retry(Buf, Id, []);
handle_message(_Other, Buf) ->
    Buf.

add_packet_to_ack([], Id, Data, Acc) ->
    lists:reverse([packet(Id, Data, noack)|Acc]);
add_packet_to_ack([<<Id:8,1:1,0:23>>|Rest], Id, Data, Acc) ->
    lists:reverse(Acc) ++ [packet(Id, Data, ack)|Rest];
add_packet_to_ack([Other|Rest], Id, Data, Acc) ->
    add_packet_to_ack(Rest, Id, Data, [Other|Acc]).

add_ack_to_packet([], Id, Acc) ->
    lists:reverse([ack(Id)|Acc]);
add_ack_to_packet([<<Id:8,0:1,Tail/bitstring>>|Rest], Id, Acc) ->
    lists:reverse(Acc) ++ [<<Id:8,1:1,Tail/bitstring>>|Rest];
add_ack_to_packet([Other|Rest], Id, Acc) ->
    add_ack_to_packet(Rest, Id, [Other|Acc]).

remove_retry([], _Id, Acc) ->
    lists:reverse(Acc);
remove_retry([<<Id:8,_/bitstring>>|Rest], Id, Acc) ->
    lists:reverse(Acc) ++ Rest;
remove_retry([Packet|Rest], Id, Acc) ->
    remove_retry(Rest, Id, [Packet|Acc]).

ack(Id) ->
    <<Id:8,1:1,0:7,0:8,0:8>>.

packet(Id, Data, Ack) ->
    Length = byte_size(Data),
    AckBit = case Ack of
        ack -> 1;
        _   -> 0
    end,
    <<Id:8,AckBit:1,1:7,Length:8,0:8,Data/binary>>.
