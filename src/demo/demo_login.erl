-module(demo_login).

-export([init/2]).
-export([network_message/2]).

init(_InitReq, _Opts) ->
    {ok, []}.

network_message(Req, _State) ->
    _Data = carthage_req:get_data(Req),
    {done, carthage_req:get_peername(Req)}.

