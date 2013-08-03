-module(demo_login).

-export([init/2]).
-export([network_message/2]).

init(InitReq, _Opts) ->
    carthage_req:send(InitReq, <<"Hello! ">>),
    {ok, []}.

network_message(Req, _State) ->
    Data = carthage_req:get_data(Req),
    carthage_req:send(Req, Data),
    io:format("~p~n", [Data]),
    {done, Data}.

