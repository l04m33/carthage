-module(demo_client).

-export([init_client_id/1]).
-export([init/2]).
-export([network_message/2]).

init_client_id(Opts) ->
    PeerName = proplists:get_value(client_input, Opts),
    PeerName.

init(InitReq, Opts) ->
    ClientInput = proplists:get_value(client_input, Opts),
    io:format("ClientInput = ~p~n", [ClientInput]),
    carthage_req:send(InitReq, <<"Hello Client! ">>),
    {ok, []}.

network_message(Req, State) ->
    Data = carthage_req:get_data(Req),
    io:format("Data = ~p~n", [Data]),
    carthage_req:send(Req, <<Data:32>>),
    {ok, State}.

