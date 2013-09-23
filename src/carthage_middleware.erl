-module(carthage_middleware).

-export([get_context/1]).
-export([on_request/4]).
-export([on_send/5]).
-export([on_call/4]).
-export([on_reply/5]).
-export([on_cast/4]).

-spec get_context(Env) -> term() when
        Env :: carthage:env().
get_context(Env) ->
    proplists:get_value(context, Env).

on_request(Middlewares, Req, Env, Context) ->
    on_request(Middlewares, Req, [{context, Context} | Env]).

on_request([Middleware | RestMW], Req0, Env0) ->
    case erlang:function_exported(Middleware, on_request, 2) of
        true ->
            case Middleware:on_request(Req0, Env0) of
                {ok, Req, Env} ->
                    on_request(RestMW, Req, Env);
                {stop, Reason, Env} ->
                    {stop, Reason, proplists:delete(context, Env)}
            end;
        false ->
            on_request(RestMW, Req0, Env0)
    end;
on_request([], Req, Env) ->
    {ok, Req, proplists:delete(context, Env)}.

on_send(Middlewares, Data, Req, Env, Context) ->
    on_send(Middlewares, Data, Req, [{context, Context} | Env]).

on_send([Middleware | RestMW], Data0, Req0, Env0) ->
    case erlang:function_exported(Middleware, on_send, 3) of
        true ->
            case Middleware:on_send(Data0, Req0, Env0) of
                {ok, Data} ->
                    on_send(RestMW, Data, Req0, Env0);
                {stop, _Reason} = Ret ->
                    Ret
            end;
        false ->
            on_send(RestMW, Data0, Req0, Env0)
    end;
on_send([], Data, _Req, _Env0) ->
    {ok, Data}.

on_call(Middlewares, Req, Env, Context) ->
    on_call(Middlewares, Req, [{context, Context} | Env]).

on_call([Middleware | RestMW], Req0, Env0) ->
    case erlang:function_exported(Middleware, on_call, 2) of
        true ->
            case Middleware:on_call(Req0, Env0) of
                {ok, Req, Env} ->
                    on_call(RestMW, Req, Env);
                {stop, Reason, Env} ->
                    {stop, Reason, proplists:delete(context, Env)}
            end;
        false ->
            on_call(RestMW, Req0, Env0)
    end;
on_call([], Req, Env) ->
    {ok, Req, proplists:delete(context, Env)}.

on_reply(Middlewares, Reply, Req, Env, Context) ->
    on_reply(Middlewares, Reply, Req, [{context, Context} | Env]).

on_reply([Middleware | RestMW], Reply0, Req0, Env0) ->
    case erlang:function_exported(Middleware, on_reply, 3) of
        true ->
            case Middleware:on_reply(Reply0, Req0, Env0) of
                {ok, Reply} ->
                    on_reply(RestMW, Reply, Req0, Env0);
                {stop, _Reason} = Ret ->
                    Ret
            end;
        false ->
            on_reply(RestMW, Reply0, Req0, Env0)
    end;
on_reply([], Reply, _Req, _Env0) ->
    {ok, Reply}.

on_cast(Middlewares, Req, Env, Context) ->
    on_cast(Middlewares, Req, [{context, Context} | Env]).
on_cast([Middleware | RestMW], Req0, Env0) ->
    case erlang:function_exported(Middleware, on_cast, 2) of
        true ->
            case Middleware:on_cast(Req0, Env0) of
                {ok, Req, Env} ->
                    on_cast(RestMW, Req, Env);
                {stop, Reason, Env} ->
                    {stop, Reason, proplists:delete(context, Env)}
            end;
        false ->
            on_cast(RestMW, Req0, Env0)
    end;
on_cast([], Req, Env) ->
    {ok, Req, proplists:delete(context, Env)}.

