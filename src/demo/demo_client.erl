-module(demo_client).

-export([init_client_id/1]).
-export([init/2]).
-export([network_message/2]).

init_client_id(Opts) ->
    PeerName = proplists:get_value(client_input, Opts),
    PeerName.

init(_InitReq, Opts) ->
    ClientInput = proplists:get_value(client_input, Opts),
    io:format("#### ClientInput = ~p~n", [ClientInput]),
    {ok, []}.

network_message(Req, State) ->
    TimeZone = carthage_req:get_data(Req),
    io:format("#### TimeZone = ~p~n", [TimeZone]),
    GMT = calendar:universal_time(),
    GMTSecs = calendar:datetime_to_gregorian_seconds(GMT),
    RemoteSecs = GMTSecs + TimeZone * 60 * 60,
    RemoteDateTime = calendar:gregorian_seconds_to_datetime(RemoteSecs),
    carthage_req:send(Req, RemoteDateTime),
    {ok, State}.

