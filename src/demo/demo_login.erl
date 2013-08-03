-module(demo_login).

-export([init/2]).
-export([network_message/2]).
-export([socket_closed/1]).
-export([socket_error/2]).
-export([terminate/2]).

init(InitReq, _Opts) ->
    carthage_req:send(InitReq, <<"Hello! ">>),
    {ok, []}.

network_message(Req, _State) ->
    Data = carthage_req:get_data(Req),
    carthage_req:send(Req, Data),
    io:format("~p~n", [Data]),
    {done, Data}.

socket_closed(State) ->
    io:format("socket_closed~n"),
    {ok, State}.

socket_error(Error, State) ->
    io:format("socket_error: ~p~n", [Error]),
    {ok, State}.

terminate(Reason, _State) ->
    io:format("terminate: ~p~n", [Reason]),
    ok.

