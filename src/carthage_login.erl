-module(carthage_login).

-include("carthage.hrl").

-export([start_link/4]).
-export([client_start_ack/1]).
-export([
    init/4,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(login_state, {
        login_handler,
        handler_state,
        client_handler,
        client_opts,
        middlewares,
        env,

        socket,
        transport,
        tags
       }).


start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

client_start_ack(Ref) ->
    receive
        {go, Ref} -> ok
    end.


init(Ref, Socket, Transport, Opts) ->
    erlang:process_flag(trap_exit, true),
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),

    LoginHandler    = proplists:get_value(login_handler, Opts),
    LoginOpts       = proplists:get_value(login_opts, Opts, []),
    ClientHandler   = proplists:get_value(client_handler, Opts),
    ClientOpts      = proplists:get_value(client_opts, Opts, []),
    Middlewares     = proplists:get_value(middlewares, Opts, []),
    Env             = proplists:get_value(env, Opts, []),

    PacketHeaderLen = carthage_config:get(packet_header_length),
    %% TODO: should explicitly handle errors here
    ok = Transport:setopts(Socket, [{packet, PacketHeaderLen}, {active, once}]),

    InitReq = #nwk_req{
        sock = {Socket, Transport}
    },
    case LoginHandler:init(InitReq, LoginOpts) of
        {ok, HandlerState} ->
            State = #login_state{
                login_handler = LoginHandler,
                handler_state = HandlerState,
                client_handler = ClientHandler,
                client_opts = ClientOpts,
                middlewares = Middlewares,
                env = Env,

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

handle_info({Ok, Socket, Data}, State = #login_state{socket = Socket, tags = {Ok, _, _}}) ->
    #login_state{
        login_handler = LoginHandler,
        handler_state = HandlerState,
        client_handler = ClientHandler,
        client_opts = ClientOpts,
        middlewares = Middlewares,
        env = Env0,
        transport = Transport
    } = State,

    Req0 = #nwk_req{
        sock = {Socket, Transport},
        data = Data
    },

    case carthage_middleware:execute(Middlewares, Req0, [{context, login} | Env0]) of
        {stop, Reason, Env} ->
            Transport:close(Socket),
            {stop, Reason, State#login_state{env = Env}};
        {ok, Req, Env} ->
            case LoginHandler:network_message(Req, HandlerState) of
                {ok, NHandlerState} ->
                    ok = Transport:setopts(Socket, [{active, once}]),
                    {noreply, State#login_state{handler_state = NHandlerState, env = Env}};
                {stop, Reason, NHandlerState} ->
                    Transport:close(Socket),
                    {stop, Reason, State#login_state{handler_state = NHandlerState, env = Env}};
                {done, ClientInput} ->
                    StartRef = make_ref(),
                    FullOpts = [{client_input, ClientInput},
                                {middlewares, Middlewares},
                                {env, Env} | ClientOpts],
                    case carthage_client:mutex_start(
                            StartRef, Socket, Transport, ClientHandler, FullOpts) of
                        {ok, ClientPID} ->
                            release_client_process(ClientPID, StartRef, Socket, Transport);
                        {error, not_found} ->
                            %% Race condition with other login processes, no big deal
                            void;
                        Other ->
                            error_logger:error_report([{"Failed to start client process", Other}])
                    end,
                    {stop, normal, State#login_state{env = Env}}
            end
    end;

handle_info({Closed, Socket}, State = #login_state{socket = Socket, tags = {_, Closed, _}}) ->
    #login_state{
        login_handler = LoginHandler,
        handler_state = HandlerState
    } = State,
    case erlang:function_exported(LoginHandler, socket_closed, 1) of
        true ->
            {ok, NHandlerState} = LoginHandler:socket_closed(HandlerState),
            {stop, normal, State#login_state{handler_state = NHandlerState}};
        false ->
            {stop, normal, State}
    end;

handle_info({Error, Socket, Reason}, State = #login_state{socket = Socket, tags = {_, _, Error}}) ->
    #login_state{
        login_handler = LoginHandler,
        handler_state = HandlerState,
        transport = Transport
    } = State,
    case erlang:function_exported(LoginHandler, socket_error, 2) of
        true ->
            {ok, NHandlerState} = LoginHandler:socket_error(Reason, HandlerState),
            Transport:close(Socket),
            {stop, normal, State#login_state{handler_state = NHandlerState}};
        false ->
            Transport:close(Socket),
            {stop, normal, State}
    end.

terminate(Reason, State) ->
    #login_state{
        login_handler = LoginHandler,
        handler_state = HandlerState
    } = State,
    case erlang:function_exported(LoginHandler, terminate, 2) of
        true ->
            ok = LoginHandler:terminate(Reason, HandlerState);
        false ->
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


release_client_process(ClientPID, StartRef, Socket, Transport) ->
    Transport:controlling_process(Socket, ClientPID),
    ClientPID ! {go, StartRef}.

