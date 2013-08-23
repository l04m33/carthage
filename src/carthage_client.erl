-module(carthage_client).

-include("carthage.hrl").

-export([start_link/6]).
-export([start_by_sup/6]).
-export([mutex_start/5]).

-export([init/6]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(client_state, {
        client_id,
        client_handler,
        handler_state,
        middlewares,
        env,

        socket,
        transport,
        tags
       }).

start_link(Ref, ClientID, Socket, Transport, ClientHandler, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, ClientID, Socket, Transport, ClientHandler, Opts]).

start_by_sup(ID, Ref, Socket, Transport, ClientHandler, Opts) ->
    ServerID = ClientHandler,
    supervisor:start_child(ServerID,
        {ID, {?MODULE, start_link, [Ref, ID, Socket, Transport, ClientHandler, Opts]},
         temporary, 5000, worker, [?MODULE]}).

mutex_start(Ref, Socket, Transport, ClientHandler, Opts) ->
    ID = ClientHandler:init_client_id(Opts),
    ServerID = ClientHandler,
    case start_by_sup(ID, Ref, Socket, Transport, ClientHandler, Opts) of
        {ok, _} = Ret ->
            Ret;
        {error, {already_started, _OtherPID}} ->
            case supervisor:terminate_child(ServerID, ID) of
                ok ->
                    case start_by_sup(ID, Ref, Socket, Transport, ClientHandler, Opts) of
                        {ok, _} = Ret ->
                            Ret;
                        Other ->
                            Other
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        Other ->
            Other
    end.

init(StartRef, ClientID, Socket, Transport, ClientHandler, Opts0) ->
    erlang:process_flag(trap_exit, true),
    ok = proc_lib:init_ack({ok, self()}),
    ok = carthage_login:client_start_ack(StartRef),

    try
        ok = Transport:setopts(Socket, [{active, once}])
    catch ErrType : ErrCode ->
        eject(ErrType, ErrCode, erlang:get_stacktrace())
    end,

    Middlewares = proplists:get_value(middlewares, Opts0, []),
    Env = proplists:get_value(env, Opts0, []),
    Opts = proplists:delete(env, proplists:delete(middlewares, Opts0)),

    InitReq = carthage_req:new({Socket, Transport}, undefined,
            fun(DataToSend, Req) ->
                carthage_middleware:on_send(
                        Middlewares, DataToSend, Req, Env, client)
            end),
    case (catch ClientHandler:init(InitReq, Opts)) of
        {ok, HandlerState} ->
            State = #client_state{
                client_id = ClientID,
                client_handler = ClientHandler,
                handler_state = HandlerState,
                middlewares = Middlewares,
                env = Env,

                socket = Socket,
                transport = Transport,
                tags = Transport:messages()
            },

            try
                carthage_client_registry:login(ClientHandler, ClientID, self())
            catch ErrType3 : ErrCode3 ->
                %% ClientHandler:init(...) already succeeded, need some clean-up here
                terminate(login_error, State),
                eject(ErrType3, ErrCode3, erlang:get_stacktrace())
            end,

            gen_server:enter_loop(?MODULE, [], State);

        {stop, Reason} ->
            {stop, Reason};

        {'EXIT', {ErrCode2, Stacktrace}} ->
            eject(error, ErrCode2, Stacktrace);

        Other ->
            Other
    end.

handle_info({Ok, Socket, Data}, State = #client_state{socket = Socket, tags = {Ok, _, _}}) ->
    #client_state{
        client_handler = ClientHandler,
        handler_state = HandlerState,
        transport = Transport,
        middlewares = Middlewares,
        env = Env0
    } = State,

    Req0 = carthage_req:new({Socket, Transport}, Data,
            fun(DataToSend, Req) ->
                carthage_middleware:on_send(
                        Middlewares, DataToSend, Req, Env0, client)
            end),

    case carthage_middleware:on_request(Middlewares, Req0, Env0, client) of
        {stop, Reason, Env} ->
            Transport:close(Socket),
            {stop, Reason, State#client_state{env = Env}};
        {ok, Req, Env} ->
            case ClientHandler:network_message(Req, HandlerState) of
                {ok, NHandlerState} ->
                    ok = Transport:setopts(Socket, [{active, once}]),
                    {noreply, State#client_state{handler_state = NHandlerState, env = Env}};
                {stop, Reason, NHandlerState} ->
                    Transport:close(Socket),
                    {stop, Reason, State#client_state{handler_state = NHandlerState, env = Env}}
            end
    end;

handle_info({Closed, Socket}, State = #client_state{socket = Socket, tags = {_, Closed, _}}) ->
    #client_state{
        client_handler = ClientHandler,
        handler_state = HandlerState
    } = State,
    case erlang:function_exported(ClientHandler, socket_closed, 1) of
        true ->
            {ok, NHandlerState} = ClientHandler:socket_closed(HandlerState),
            {stop, normal, State#client_state{handler_state = NHandlerState}};
        false ->
            {stop, normal, State}
    end;

handle_info({Error, Socket, Reason}, State = #client_state{socket = Socket, tags = {_, _, Error}}) ->
    #client_state{
        client_handler = ClientHandler,
        handler_state = HandlerState,
        transport = Transport
    } = State,
    case erlang:function_exported(ClientHandler, socket_error, 2) of
        true ->
            {ok, NHandlerState} = ClientHandler:socket_error(Reason, HandlerState),
            Transport:close(Socket),
            {stop, normal, State#client_state{handler_state = NHandlerState}};
        false ->
            Transport:close(Socket),
            {stop, normal, State}
    end.

terminate(Reason, State) ->
    #client_state{
        client_id = ClientID,
        client_handler = ClientHandler,
        handler_state = HandlerState
    } = State,
    catch carthage_client_registry:logout(ClientHandler, ClientID, self()),
    case erlang:function_exported(ClientHandler, terminate, 2) of
        true ->
            ok = ClientHandler:terminate(Reason, HandlerState);
        false ->
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


eject(ErrType, ErrCode, Stacktrace) ->
    error_logger:error_report([{ErrType, ErrCode}, {stacktrace, Stacktrace}]),
    exit({ErrType, ErrCode}).

