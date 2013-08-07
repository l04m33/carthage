-module(carthage_middleware).

-export([execute/3]).

execute([Middleware | RestMW], Req0, Env0) ->
    case Middleware:execute(Req0, Env0) of
        {ok, Req, Env} ->
            execute(RestMW, Req, Env);
        {stop, _Reason, _Env} = Ret ->
            Ret
    end;
execute([], Req, Env) ->
    {ok, Req, Env}.

