-module(pigeon_rfid).

%% API
-export([verify/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%% Check whether an Unique RFID card number is correct.
%%% The memory map is as follows:
%%% | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1 |
%%%                 |D00|D01|D02|D03|PR0|
%%%                 |D10|D11|D12|D13|PR1|
%%%                 |D20|D21|D22|D23|PR2|
%%%                 |D30|D31|D32|D33|PR3|
%%%                 |D40|D41|D42|D43|PR4|
%%%                 |D50|D51|D52|D53|PR5|
%%%                 |D60|D61|D62|D63|PR6|
%%%                 |D70|D71|D72|D73|PR7|
%%%                 |D80|D81|D82|D83|PR8|
%%%                 |D90|D91|D92|D93|PR9|
%%%                 |PC0|PC1|PC2|PC3| 0 |
%%% Dij stands for an actual data bit
%%% PRi stands for a parity bit for the row i
%%% PCj stands for a parity bit for the column j
verify(<<511:9, Data/bitstring>>) ->
    {Rows, Columns} = data_bits(Data),
    PRows = [<<Row/bitstring,(parity(Row)):1>> || Row <- Rows],
    PCols = [<<(parity(Column)):1>> || Column <- Columns],
    Compare = PRows ++ PCols ++ [<<0:1>>],
    list_to_bitstring(Compare) =:= Data;
verify(_NoHeader) ->
    false.

%%%===================================================================
%%% Internal functions
%%%===================================================================
data_bits(Binary) ->
    data_bits(Binary, [], [<<>>,<<>>,<<>>,<<>>]).

data_bits(<<_:5>>, Rows, Columns) ->
    {lists:reverse(Rows), Columns};
data_bits(<<D1:1,D2:1,D3:1,D4:1,_PR:1,Rest/bitstring>>,Rows,[C1,C2,C3,C4]) ->
    NewColumns = [<<C1/bitstring,D1:1>>, <<C2/bitstring,D2:1>>,
                  <<C3/bitstring,D3:1>>, <<C4/bitstring,D4:1>>],
    data_bits(Rest, [<<D1:1,D2:1,D3:1,D4:1>>|Rows], NewColumns).

parity(Binary) ->
    parity(Binary, 0) rem 2.

parity(<<>>, Current) ->
    Current;
parity(<<1:1,Rest/bitstring>>, Current) ->
    parity(Rest, Current+1);
parity(<<_:1,Rest/bitstring>>, Current) ->
    parity(Rest, Current).
