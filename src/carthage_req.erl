-module(carthage_req).

-include("carthage.hrl").

-export([get_data/1]).
-export([set_data/2]).
-export([send/2]).

get_data(Req) ->
    Req#nwk_req.data.

set_data(Data, Req) ->
    Req#nwk_req{data = Data}.

send(#nwk_req{sock = undefined}, _Data) ->
    ok;
send(#nwk_req{sock = {Socket, Transport}}, Data) ->
    Transport:send(Socket, Data).

