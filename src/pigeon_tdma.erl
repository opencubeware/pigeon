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
    process_flag(priority, high),
    pigeon_rfm70:add_callback(?RECEIVE_MESSAGE),
    schedule_window(send_sync),
    {ok, #state{buffer = []}}.

handle_call({send, Id, Data}, _From, #state{buffer=Buffer}=State) ->
    NewBuffer = add_frame_to_ack(Buffer, Id, Data, []),
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
    NewBuf = handle_message(Message, Buf),
    pigeon_metrics:rx(Message),
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
send_buffer([<<_To:8,1:1,0:23>>=Ack|Rest], Acc) ->
    pigeon_rfm70:send(Ack),
    pigeon_metrics:tx(Ack),
    send_buffer(Rest, Acc);
send_buffer([<<To:8,_Ack:1,Retry:7,Tail/bitstring>>=Frame|Rest], Acc) ->
    case Retry of
        Greater when Greater > ?MAX_RETRIES ->
            send_buffer(Rest, Acc);
        _ ->
            pigeon_rfm70:send(Frame),
            pigeon_metrics:tx(Frame),
            NextRetry = <<To:8,0:1,(Retry+1):7,Tail/bitstring>>,
            send_buffer(Rest, [NextRetry|Acc])
    end.

handle_message(<<Id:8,0:1,_Retry:7,_Len:8,_Pos:8,_Data/binary>>=NoAck, Buf) ->
    pigeon_message:handle(NoAck),
    add_ack_to_frame(Buf, Id, []);
handle_message(<<Id:8,1:1,0:23>>, Buf) ->
    remove_retry(Buf, Id, []);
handle_message(<<Id:8,1:1,_Retry:7,_Rest/bitstring>>=Ack, Buf) ->
    pigeon_message:handle(Ack),
    Buf1 = remove_retry(Buf, Id, []),
    add_ack_to_frame(Buf1, Id, []);
handle_message(_Other, Buf) ->
    Buf.

%% add frame to currently existing ack frame in the buffer
%% if doesn't exist - append one at the end
add_frame_to_ack([], Id, Data, Acc) ->
    lists:reverse([frame(Id, Data, noack)|Acc]);
add_frame_to_ack([<<Id:8,1:1,0:23>>|Rest], Id, Data, Acc) ->
    lists:reverse(Acc) ++ [frame(Id, Data, ack)|Rest];
add_frame_to_ack([Other|Rest], Id, Data, Acc) ->
    add_frame_to_ack(Rest, Id, Data, [Other|Acc]).

%% add ack to currently existing frame in the buffer
%% if doesn't exist - append one at the end
add_ack_to_frame([], Id, Acc) ->
    lists:reverse([ack(Id)|Acc]);
add_ack_to_frame([<<Id:8,0:1,Tail/bitstring>>|Rest], Id, Acc) ->
    lists:reverse(Acc) ++ [<<Id:8,1:1,Tail/bitstring>>|Rest];
add_ack_to_frame([Other|Rest], Id, Acc) ->
    add_ack_to_frame(Rest, Id, [Other|Acc]).

%% ack arrived - remove retry
remove_retry([], _Id, Acc) ->
    lists:reverse(Acc);
remove_retry([<<Id:8,_/bitstring>>|Rest], Id, Acc) ->
    lists:reverse(Acc) ++ Rest;
remove_retry([Frame|Rest], Id, Acc) ->
    remove_retry(Rest, Id, [Frame|Acc]).

ack(Id) ->
    <<Id:8,1:1,0:7,0:8,0:8>>.

frame(Id, Data, Ack) ->
    Length = byte_size(Data),
    AckBit = case Ack of
        ack -> 1;
        _   -> 0
    end,
    <<Id:8,AckBit:1,1:7,Length:8,0:8,Data/binary>>.
