-module(carthage).

-export([start/4]).

start(LoginHandler, ClientHandler, ListenPort, NumAcceptors) ->
    ok = ensure_started(ranch),
    ok = ensure_started(carthage),

    ServerID = {LoginHandler, ClientHandler},
    %% TODO: Start a new client supervisor
    {ok, _} = ranch:start_listener(ServerID, NumAcceptors,
            ranch_tcp, [{port, ListenPort}], carthage_login,
            [{login_handler, LoginHandler}, {client_handler, ClientHandler}]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        Other ->
            Other
    end.

