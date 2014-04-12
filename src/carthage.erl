-module(carthage).

-export([start/5]).

-type handler_opts() :: [{atom(), term()}].
-type handler_spec() :: {module(), handler_opts()}.
-type env() :: [{atom(), term()}].
-type middleware_spec() :: {[module()], env()}.

-export_type([env/0, handler_opts/0]).

-spec start(LoginHandlerSpec, ClientHandlerSpec,
            MiddlewareSpec, ListenPort, NumAcceptors) -> ok when
        LoginHandlerSpec :: handler_spec(),
        ClientHandlerSpec :: handler_spec(),
        MiddlewareSpec :: middleware_spec(),
        ListenPort :: non_neg_integer(),
        NumAcceptors :: non_neg_integer().
start({LoginHandler, LoginOpts},
      {ClientHandler, ClientOpts},
      {Middlewares, Env},
      ListenPort, NumAcceptors) ->
    {module, LoginHandler} = code:load_file(LoginHandler),
    {module, ClientHandler} = code:load_file(ClientHandler),
    _ = [{module, MW} = code:load_file(MW) || MW <- Middlewares],

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
            [{login_handler, LoginHandler}, {client_handler, ClientHandler},
             {login_opts, LoginOpts}, {client_opts, ClientOpts},
             {middlewares, Middlewares}, {env, Env}]),
    case RanchRet of
        {ok, _} ->
            case carthage_client_registry:new_registry(ServerID) of
                ok ->
                    ok;
                Error ->
                    remove_server_instance(ServerID),
                    exit(Error)
            end;
        Error ->
            remove_server_instance(ServerID),
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

remove_server_instance(ServerID) ->
    ok = supervisor:terminate_child(carthage_sup, ServerID),
    ok = supervisor:delete_child(carthage_sup, ServerID).

