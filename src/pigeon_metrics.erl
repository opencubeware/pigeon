-module(pigeon_metrics).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_metrics/0,
         reset/0,
         tx/1,
         rx/1,
         sync/0,
         retries/0,
         devices/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

-define(WIDTH, 128).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

reset() ->
    gen_server:call(?MODULE, reset).

sync() ->
    folsom_metrics:notify({pigeon_traffic, {sync, out}}).

rx(<<Id:8,_/bitstring>>=Frame) when Id =< ?WIDTH ->
    folsom_metrics:notify({pigeon_rx, 1}),
    folsom_metrics:notify({pigeon_rx_bytes, byte_size(Frame)}),
    handle_rx(Frame);
rx(_Other) ->
    ok.

tx(<<Id:8,_/bitstring>>=Frame) when Id =< ?WIDTH ->
    folsom_metrics:notify({pigeon_tx, 1}),
    folsom_metrics:notify({pigeon_tx_bytes, byte_size(Frame)}),
    handle_tx(Frame);
tx(_Other) ->
    ok.

retries() ->
    trim_zeros_sort(ets:tab2list(pigeon_retry)).

devices() ->
    trim_zeros_sort(ets:tab2list(pigeon_device)).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    folsom_metrics:new_history(pigeon_traffic, 10*1024),
    folsom_metrics:new_spiral(pigeon_tx),
    folsom_metrics:new_spiral(pigeon_rx),
    folsom_metrics:new_spiral(pigeon_tx_bytes),
    folsom_metrics:new_spiral(pigeon_rx_bytes),
    folsom_metrics:new_histogram(pigeon_ifg),
    ets:new(pigeon_retry, [public, named_table, {read_concurrency, true}]),
    ets:new(pigeon_device, [public, named_table, {read_concurrency, true}]),
    lists:foreach(fun(N) ->
                ets:insert(pigeon_retry, {N, 0}),
                ets:insert(pigeon_device, {N, 0})
        end, lists:seq(1, ?WIDTH)),
    ets:new(pigeon_last, [public, named_table, {read_concurrency, true}]),
    {ok, #state{}}.

handle_call(get_metrics, _From, State) ->
    Reply = handle_get_metrics(),
    {reply, Reply, State};
handle_call(reset, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    Metrics = [pigeon_traffic, pigeon_tx, pigeon_rx,
               pigeon_tx_bytes, pigeon_rx_bytes, pigeon_ifg],
    [folsom_metrics:delete_metric(Metric) || Metric <- Metrics],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_get_metrics() ->
    Histogram = folsom_metrics:get_histogram_statistics(pigeon_ifg),
    {histogram, Ifg} = lists:keyfind(histogram, 1, Histogram),
    [{tx,       folsom_metrics:get_metric_value(pigeon_tx)},
     {rx,       folsom_metrics:get_metric_value(pigeon_rx)},
     {tx_bytes, folsom_metrics:get_metric_value(pigeon_tx_bytes)},
     {rx_bytes, folsom_metrics:get_metric_value(pigeon_rx_bytes)},
     {ifg,      Ifg},
     {retries,  retries()},
     {devices,  devices()}].

handle_rx(<<Id:8,1:1,0:23>>) ->
    folsom_metrics:notify({pigeon_traffic, {ack, in, Id}});
handle_rx(<<Id:8,1:1,Retry:7,_Rest/bitstring>>=Ack) ->
    folsom_metrics:notify({pigeon_traffic, {ack_frame, in, Id, Ack}}),
    ets:update_counter(pigeon_retry, Retry, 1),
    ets:update_counter(pigeon_device, Id, 1);
handle_rx(<<Id:8,0:1,Retry:7,_Rest/bitstring>>=NoAck) ->
    folsom_metrics:notify({pigeon_traffic, {frame, in, Id, NoAck}}),
    ets:update_counter(pigeon_retry, Retry, 1),
    ets:update_counter(pigeon_device, Id, 1).

handle_tx(<<Id:8,1:1,0:23>>) ->
    folsom_metrics:notify({pigeon_traffic, {ack, out, Id}});
handle_tx(<<Id:8,1:1,_Rest/bitstring>>=Ack) ->
    folsom_metrics:notify({pigeon_traffic, {ack_frame, out, Id, Ack}}),
    calculate_ifg(Id);
handle_tx(<<Id:8,0:1,_Rest/bitstring>>=NoAck) ->
    folsom_metrics:notify({pigeon_traffic, {frame, out, Id, NoAck}}),
    calculate_ifg(Id).

trim_zeros_sort(List) ->
    Filtered = lists:filter(fun
            ({_, 0}) -> false;
            (_)      -> true
        end, List),
    lists:keysort(1, Filtered).

calculate_ifg(From) ->
    Epoch = folsom_utils:now_epoch_micro(),
    case ets:lookup(pigeon_last, From) of
        [] -> 
            ok;
        [{From, Last}] ->
            folsom_metrics:notify({pigeon_ifg, (Epoch-Last) div 1000})
    end,
    ets:insert(pigeon_last, {From, Epoch}).
