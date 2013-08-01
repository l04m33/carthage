-module(carthage).

-export([start/4]).

start(LoginHandler, ClientHandler, ListenPort, NumAcceptors) ->
    ok = ensure_started(ranch),
    ok = ensure_started(carthage),

    %% Use the name of the client handler to identify the supervisor
    %% i.e. one supervisor can only supervise one kind of clients
    ServerID = ClientHandler,

    {ok, _} = supervisor:start_child(carthage_sup,
            {ServerID, {carthage_client_sup, start_link, [ServerID]},
             permanent, 5000, supervisor, [carthage_client_sup]}),

    RanchRet = ranch:start_listener(ServerID, NumAcceptors,
            ranch_tcp, [{port, ListenPort}], carthage_login,
            [{login_handler, LoginHandler}, {client_handler, ClientHandler}]),
    case RanchRet of
        {ok, _} ->
            ok;
        Error ->
            supervisor:terminate_child(carthage_sup, ServerID),
            supervisor:delete_child(carthage_sup, ServerID),
            exit(Error)
    end.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        Other ->
            Other
    end.

