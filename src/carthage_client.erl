-module(carthage_client).

-include("carthage.hrl").

-export([start_link/5]).
-export([start_by_sup/6]).
-export([mutex_start/5]).

-export([init/5]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(client_state, {
        client_handler,
        handler_state,

        socket,
        transport,
        tags
       }).

start_link(Ref, Socket, Transport, ClientHandler, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, ClientHandler, Opts]).

start_by_sup(ID, Ref, Socket, Transport, ClientHandler, Opts) ->
    supervisor:start_child(carthage_client_sup,
        {ID, {?MODULE, start_link, [Ref, Socket, Transport, ClientHandler, Opts]},
         temporary, 5000, worker, [?MODULE]}).

mutex_start(Ref, Socket, Transport, ClientHandler, Opts) ->
    ID = ClientHandler:init_client_id(Opts),
    case start_by_sup(ID, Ref, Socket, Transport, ClientHandler, Opts) of
        {ok, _} = Ret ->
            Ret;
        {error, {already_started, _OtherPID}} ->
            case supervisor:terminate_child(carthage_client_sup, ID) of
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

init(StartRef, Socket, Transport, ClientHandler, Opts) ->
    erlang:process_flag(trap_exit, true),
    ok = proc_lib:init_ack({ok, self()}),
    ok = carthage_login:client_start_ack(StartRef),

    ok = Transport:setopts(Socket, [{active, once}]),

    InitReq = #nwk_req{
        sock = {Socket, Transport}
    },
    case ClientHandler:init(InitReq, Opts) of
        {ok, HandlerState} ->
            State = #client_state{
                client_handler = ClientHandler,
                handler_state = HandlerState,

                socket = Socket,
                transport = Transport,
                tags = Transport:messages()
            },
            gen_server:enter_loop(?MODULE, [], State);
        {stop, Reason} ->
            {stop, Reason};
        Other ->
            Other
    end.

handle_info({Ok, Socket, Data}, State = #client_state{socket = Socket, tags = {Ok, _, _}}) ->
    #client_state{
        client_handler = ClientHandler,
        handler_state = HandlerState,
        transport = Transport
    } = State,

    Req = #nwk_req{
        sock = {Socket, Transport},
        data = Data
    },
    case ClientHandler:network_message(Req, HandlerState) of
        {ok, NHandlerState} ->
            ok = Transport:setopts(Socket, [{active, once}]),
            {noreply, State#client_state{handler_state = NHandlerState}};
        {stop, Reason, NHandlerState} ->
            Transport:close(Socket),
            {stop, Reason, State#client_state{handler_state = NHandlerState}}
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
        client_handler = ClientHandler,
        handler_state = HandlerState
    } = State,
    case erlang:function_exported(ClientHandler, terminate, 2) of
        true ->
            ok = ClientHandler:terminate(Reason, HandlerState);
        false ->
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

