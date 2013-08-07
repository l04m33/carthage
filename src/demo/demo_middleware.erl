-module(demo_middleware).

-export([execute/2]).

execute(Req, Env) ->
    case carthage_middleware:get_context(Env) of
        client ->
            <<Number:32, _/binary>> = carthage_req:get_data(Req),
            NReq = carthage_req:set_data(Number, Req),
            {ok, NReq, Env};
        _ ->
            {ok, Req, Env}
    end.

