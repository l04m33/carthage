-module(carthage).

-export([start/0]).

start() ->
    ok = ensure_started(ranch),
    ok = ensure_started(carthage).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        Other ->
            Other
    end.

