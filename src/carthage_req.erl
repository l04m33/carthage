-module(carthage_req).

-include("carthage.hrl").

-export([new/3]).
-export([get_peername/1]).
-export([get_data/1]).
-export([set_data/2]).
-export([send/2]).

new({_Socket, _Transport} = SockInfo, Data, OnSend) ->
    #nwk_req{
        sock = SockInfo,
        data = Data,

        on_send = OnSend
    }.

get_peername(#nwk_req{sock = {Socket, Transport}}) ->
    Transport:peername(Socket);
get_peername(#nwk_req{sock = undefined}) ->
    undefined.

get_data(Req) ->
    Req#nwk_req.data.

set_data(Data, Req) ->
    Req#nwk_req{data = Data}.

send(#nwk_req{sock = undefined}, _Data) ->
    ok;
send(#nwk_req{sock = {Socket, Transport}, on_send = undefined}, Data) ->
    Transport:send(Socket, Data);
send(#nwk_req{sock = {Socket, Transport}, on_send = already_sent}, Data) ->
    Transport:send(Socket, Data);
send(Req = #nwk_req{sock = {Socket, Transport}, on_send = OnSend}, Data) ->
    case OnSend(Data, Req#nwk_req{on_send = already_sent}) of
        {stop, _Reason} = Stop ->
            Stop;
        {ok, NData} ->
            Transport:send(Socket, NData)
    end.

