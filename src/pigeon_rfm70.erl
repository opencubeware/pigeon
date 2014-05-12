-module(pigeon_rfm70).

-behaviour(gen_server).

%% API
-export([start_link/0,
         send/1,
         add_callback/1,
         delete_callback/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {callbacks = []}).

-define(SPI_CH, 0).
-define(SPI_FREQ, 8000000).

-include("pigeon_rfm70.hrl").

-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send(Data) ->
    gen_server:call(?MODULE, {send, Data}).

add_callback(Callback) when is_function(Callback, 1) ->
    gen_server:call(?MODULE, {add_callback, Callback}).

delete_callback(Callback) when is_function(Callback, 1) ->
    gen_server:call(?MODULE, {delete_callback, Callback}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    init_pins(),
    init_bank0(),
    init_bank1(),
    timer:sleep(100),
    bank(0),
    switch_rx(),
    {ok, #state{}}.

handle_call({add_callback, Callback}, _From,
            #state{callbacks=Callbacks}=State) ->
    NewCallbacks = case lists:member(Callback, Callbacks) of
        true  -> Callbacks;
        false -> [Callback|Callbacks]
    end,
    {reply, ok, State#state{callbacks=NewCallbacks}};
handle_call({delete_callback, Callback}, _From,
            #state{callbacks=Callbacks}=State) ->
    NewCallbacks = lists:delete(Callback, Callbacks),
    {reply, ok, State#state{callbacks=NewCallbacks}};
handle_call({send, Data}, _From, State) ->
    Reply = send_packet(Data),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(interrupt, State) ->
    maybe_receive_packet(State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
maybe_receive_packet(#state{callbacks=Callbacks}) ->
    %% receive packet iff it's a rx interrupt
    case register_read(?RFM70_REG_STATUS) of
        <<_:1,1:1,_/bitstring>>=Status ->
            <<StatusByte>> = Status,
            receive_packet(Callbacks, StatusByte);
        _ ->
            ok
    end.

receive_packet(Callbacks, Status) ->
    <<Length>> = register_read(?RFM70_CMD_R_RX_PL_WID),
    Payload = rw(<<?RFM70_CMD_R_RX_PAYLOAD>>, 8*Length, 8),
    rw(<<?RFM70_CMD_FLUSH_RX, 0>>),
    register_write(?RFM70_REG_STATUS, <<(Status bor 64)>>),
    [Callback(Payload) || Callback <- Callbacks],
    Payload.

send_packet(Data) ->
    switch_tx(),
    Cmd = <<?RFM70_CMD_W_TX_PAYLOAD_NOACK, Data/binary>>,
    rw(Cmd),
    Reply = receive
        interrupt -> ok
    after 100 ->
        {error, timeout}
    end,
    %% clear tx interrupt
    <<Status>> = register_read(?RFM70_REG_STATUS),
    register_write(?RFM70_REG_STATUS, <<(Status bor 32)>>),
    switch_rx(),
    Reply.

switch_rx() ->
    rw(<<?RFM70_CMD_FLUSH_RX, 0>>),
    pigeon_gpio:write(?PIN_CE, low),
    <<Config>> = register_read(?RFM70_REG_CONFIG),
    register_write(?RFM70_REG_CONFIG, <<(Config bor 16#01)>>),
    pigeon_gpio:write(?PIN_CE, high).

switch_tx() ->
    rw(<<?RFM70_CMD_FLUSH_TX, 0>>),
    pigeon_gpio:write(?PIN_CE, low),
    <<Config>> = register_read(?RFM70_REG_CONFIG),
    register_write(?RFM70_REG_CONFIG, <<(Config band 16#FE)>>),
    pigeon_gpio:write(?PIN_CE, high).

init_pins() ->
    pigeon_gpio:mode(?PIN_CE, output),
    pigeon_gpio:mode(?PIN_CSN, output),
    pigeon_gpio:interrupt(?PIN_IRQ, falling),
    {ok, _} = pigeon_spi:init(?SPI_CH, ?SPI_FREQ).

init_bank0() ->
    bank(0),
    %% initialize bank 0 registers
    [register_write(Register, Value) || {Register,Value} <- bank0()],
    %% initialize addresses for pipes
    [register_write(Register, Address) || {Register,Address} <- addresses()],
    %% enable extra features
    case register_read(?RFM70_REG_FEATURE) of
        <<0>> ->
            rw(<<?RFM70_CMD_ACTIVATE:8, 16#73:8>>);
        _ ->
            ok
    end,
    %% enable dynamic payload
    Dynamic = dynamic_payload(),
    [register_write(Register, Value) || {Register,Value} <- Dynamic].

init_bank1() ->
    bank(1),
    [register_write(Register, Value) || {Register,Value} <- bank1()].

bank(Bank) ->
    case register_read(?RFM70_REG_STATUS) of
        <<Bank:1,_/bitstring>> ->
            ok;
        _    ->
            rw(<<?RFM70_CMD_ACTIVATE, 16#53>>),
            ok
    end.

register_write(Register, Value) ->
    Cmd = ?RFM70_CMD_WRITE_REG bor Register,
    rw(<<Cmd, Value/binary>>).

register_read(Register) ->
    Cmd = ?RFM70_CMD_READ_REG bor Register,
    rw(<<Cmd>>, 8, 8).

rw(Data) ->
    pigeon_gpio:write(?PIN_CSN, low),
    {ok, Value} = pigeon_spi:rw(?SPI_CH, Data),
    pigeon_gpio:write(?PIN_CSN, high),
    Value.

rw(Data, TPadding, RPadding) ->
    pigeon_gpio:write(?PIN_CSN, low),
    {ok, Value} = pigeon_spi:rw(?SPI_CH, Data, TPadding, RPadding),
    pigeon_gpio:write(?PIN_CSN, high),
    Value.

%%%===================================================================
%%% Initialization data
%%%===================================================================
%% {RegNum, Value}
bank0() ->
    [{?RFM70_REG_CONFIG,     ?B0_CONFIG},
     {?RFM70_REG_EN_AA,      ?B0_EN_AA},
     {?RFM70_REG_EN_RXADDR,  ?B0_EN_RXADDR},
     {?RFM70_REG_SETUP_AW,   ?B0_SETUP_AW},
     {?RFM70_REG_SETUP_RETR, ?B0_SETUP_RETR},
     {?RFM70_REG_RF_CH,      ?B0_RF_CH},
     {?RFM70_REG_RF_SETUP,   ?B0_RF_SETUP},
     {?RFM70_REG_STATUS,     ?B0_STATUS},
     {?RFM70_REG_OBSERVE_TX, ?B0_OBSERVE_TX},
     {?RFM70_REG_RX_ADDR_P2, ?B0_RX_ADDR_P2},
     {?RFM70_REG_RX_ADDR_P3, ?B0_RX_ADDR_P3},
     {?RFM70_REG_RX_ADDR_P4, ?B0_RX_ADDR_P4},
     {?RFM70_REG_RX_ADDR_P5, ?B0_RX_ADDR_P5},
     {?RFM70_REG_RX_PW_P0,   ?B0_RX_PW_P0},
     {?RFM70_REG_RX_PW_P1,   ?B0_RX_PW_P1},
     {?RFM70_REG_RX_PW_P2,   ?B0_RX_PW_P2},
     {?RFM70_REG_RX_PW_P3,   ?B0_RX_PW_P3},
     {?RFM70_REG_RX_PW_P4,   ?B0_RX_PW_P4},
     {?RFM70_REG_RX_PW_P5,   ?B0_RX_PW_P5},
     {?RFM70_REG_FIFO_STATUS,?B0_FIFO_STATUS}].

dynamic_payload() ->
    [{?RFM70_REG_DYNPD,   ?B0_DYNPD},
     {?RFM70_REG_FEATURE, ?B0_FEATURE}].

addresses() ->
    [{?RFM70_REG_RX_ADDR_P0, ?B0_RX_ADDR_P0},
     {?RFM70_REG_RX_ADDR_P1, ?B0_RX_ADDR_P1},
     {?RFM70_REG_TX_ADDR,    ?B0_TX_ADDR}].

%% "it's a kind of magic"
bank1() ->
    [{16#00, <<16#40,16#4B,16#01,16#E2>>},
     {16#01, <<16#C0,16#4B,16#00,16#00>>},
     {16#02, <<16#D0,16#FC,16#8C,16#02>>},
     {16#03, <<16#99,16#00,16#39,16#41>>},
     {16#04, <<16#D9,16#9E,16#86,16#0B>>},
     {16#05, <<16#24,16#06,16#7F,16#A6>>},
     {16#0C, <<16#00,16#12,16#73,16#00>>},
     {16#0D, <<16#36,16#B4,16#80,16#00>>},
     {16#0E, <<16#41,16#20,16#08,16#04,16#81,16#20,16#CF,16#F7,16#FE,16#FF,16#FF>>}].
