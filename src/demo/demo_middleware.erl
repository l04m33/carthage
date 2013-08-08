-module(demo_middleware).

-export([on_request/2]).
-export([on_send/3]).

on_request(Req, Env) ->
    io:format("========= on_request =========~n"),
    io:format("Req = ~p~n", [Req]),
    io:format("Env = ~p~n", [Env]),

    case carthage_middleware:get_context(Env) of
        client ->
            case carthage_req:get_data(Req) of
                <<TimeZone:8/signed>> when TimeZone >= -12 andalso TimeZone =< 12 ->
                    NReq = carthage_req:set_data(TimeZone, Req),
                    {ok, NReq, Env};
                _ ->
                    {stop, illegal_request, Env}
            end;
        _ ->
            {ok, Req, Env}
    end.

on_send(Data, Req, Env) ->
    io:format("=========  on_send   =========~n"),
    io:format("Req = ~p~n", [Req]),
    io:format("Env = ~p~n", [Env]),
    io:format("Data = ~p~n", [Data]),

    case carthage_middleware:get_context(Env) of
        client ->
            NData = io_lib:format(<<"~w">>, [Data]),
            {ok, NData};
        _ ->
            {ok, Data}
    end.

