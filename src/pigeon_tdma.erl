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

-record(state, {next, buffer, pending}).

%% length of a tdma window in miliseconds
-define(TDMA_WINDOW, 66).
-define(MAX_RETRIES, 100).
-define(SYNC, <<250, "syn">>).
-define(RECEIVE_MESSAGE, fun(Msg) -> self() ! {message, Msg} end).

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
    schedule_window(),
    {ok, #state{next = send_sync, buffer = []}}.

handle_call({send, Id, Data}, _From, #state{buffer=Buffer}=State) ->
    Length = byte_size(Data),
    Packet = <<Id:8,0:1,1:7,Length:8,0:8,Data/binary>>,
    NewBuffer = lists:append(Buffer, [Packet]),
    {reply, ok, State#state{buffer=NewBuffer}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(window, #state{next=send_sync}=State) ->
    pigeon_rfm70:send(?SYNC),
    schedule_window(),
    {noreply, State#state{next=flush_buffer}};
handle_info(window, #state{next=flush_buffer, buffer=Buf}=State) ->
    NewBuf = send_buffer(Buf, []),
    schedule_window(),
    {noreply, State#state{buffer=NewBuf, next=send_sync}};
handle_info({message, Message}, #state{buffer=Buf}=State) ->
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
schedule_window() ->
    erlang:send_after(?TDMA_WINDOW, self(), window).

send_buffer([], Acc) ->
    lists:reverse(Acc);
send_buffer([<<_:8,1:1,_/bitstring>>=Ack|Rest], Acc) ->
    pigeon_rfm70:send(Ack),
    send_buffer(Rest, Acc);
send_buffer([<<Head:9,Retry:7,Tail/bitstring>>=NoAck|Rest], Acc) ->
    pigeon_rfm70:send(NoAck),
    case Retry+1 of
        Greater when Greater > ?MAX_RETRIES ->
            error_logger:error_msg("Maximum retries reached for packet ~p",
                                   [NoAck]),
            send_buffer(Rest, Acc);
        Lower ->
            NextRetry = <<Head:9,Lower:7,Tail/bitstring>>,
            send_buffer(Rest, [NextRetry|Acc])
    end.

handle_message(<<Id:8,0:1,Retry:7,Len:8,Pos:8,Data/binary>>=NoAck, Buf) ->
    %%
    Buf;
handle_message(<<Id:8,1:1,Rest/bitstring>>=Ack, Buf) ->
    remove_ack(Buf, Id, []).

remove_ack([], _Id, Acc) ->
    lists:reverse(Acc);
remove_ack([<<Id:8,1:1,_/bitstring>>|Rest], Id, Acc) ->
    lists:reverse(Acc) ++ Rest;
remove_ack([Packet|Rest], Id, Acc) ->
    remove_ack(Rest, Id, [Packet|Acc]).
