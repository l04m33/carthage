-module(carthage_middleware).

-export([on_request/4]).
-export([on_send/5]).
-export([get_context/1]).

on_request(Middlewares, Req, Env, Context) ->
    on_request(Middlewares, Req, [{context, Context} | Env]).

on_request([Middleware | RestMW], Req0, Env0) ->
    case Middleware:on_request(Req0, Env0) of
        {ok, Req, Env} ->
            on_request(RestMW, Req, Env);
        {stop, Reason, Env} ->
            {stop, Reason, proplists:delete(context, Env)}
    end;
on_request([], Req, Env) ->
    {ok, Req, proplists:delete(context, Env)}.

on_send(Middlewares, Data, Req, Env, Context) ->
    on_send(Middlewares, Data, Req, [{context, Context} | Env]).

on_send([Middleware | RestMW], Data0, Req0, Env0) ->
    case Middleware:on_send(Data0, Req0, Env0) of
        {ok, Data} ->
            on_send(RestMW, Data, Req0, Env0);
        {stop, _Reason} = Ret ->
            Ret
    end;
on_send([], Data, _Req, _Env0) ->
    {ok, Data}.

get_context(Env) ->
    proplists:get_value(context, Env).

