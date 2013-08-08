-module(demo_middleware).

-export([on_request/2]).
-export([on_send/3]).

on_request(Req, Env) ->
    io:format("========= on_request =========~n"),
    io:format("Req = ~p~n", [Req]),
    io:format("Env = ~p~n", [Env]),
    case carthage_middleware:get_context(Env) of
        client ->
            <<Number:32, _/binary>> = carthage_req:get_data(Req),
            NReq = carthage_req:set_data(Number, Req),
            {ok, NReq, Env};
        _ ->
            {ok, Req, Env}
    end.

on_send(Data, Req, Env) ->
    io:format("=========  on_send   =========~n"),
    io:format("Req = ~p~n", [Req]),
    io:format("Env = ~p~n", [Env]),
    io:format("Data = ~p~n", [Data]),
    {ok, Data}.

