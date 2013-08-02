-module(carthage_login).

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

    HandlerState = LoginHandler:init({Socket, Transport}, LoginOpts),
    PacketHeaderLen = carthage_config:get(packet_header_length),
    ok = Transport:setopts(Socket, [{packet, PacketHeaderLen}, {active, once}]),

    State = #login_state{
        login_handler = LoginHandler,
        handler_state = HandlerState,
        client_handler = ClientHandler,
        client_opts = ClientOpts,

        socket = Socket,
        transport = Transport,
        tags = Transport:messages()
    },
    gen_server:enter_loop(?MODULE, [], State).

handle_info({Ok, Socket, Data}, State = #login_state{socket = Socket, tags = {Ok, _, _}}) ->
    #login_state{
        login_handler = LoginHandler,
        handler_state = HandlerState
    } = State,
    NHandlerState = LoginHandler:network_message(Data, HandlerState),

    Transport = State#login_state.transport,
    ok = Transport:setopts(Socket, [{active, once}]),
    {noreply, State#login_state{handler_state = NHandlerState}};

handle_info({Closed, Socket}, State = #login_state{socket = Socket, tags = {_, Closed, _}}) ->
    #login_state{
        login_handler = LoginHandler,
        handler_state = HandlerState
    } = State,
    NHandlerState = LoginHandler:socket_closed(HandlerState),
    {stop, normal, State#login_state{handler_state = NHandlerState}};

handle_info({Error, Socket, Reason}, State = #login_state{socket = Socket, tags = {_, _, Error}}) ->
    #login_state{
        login_handler = LoginHandler,
        handler_state = HandlerState
    } = State,
    NHandlerState = LoginHandler:socket_error(Reason, HandlerState),

    Transport = State#login_state.transport,
    Transport:close(Socket),
    {stop, normal, State#login_state{handler_state = NHandlerState}}.

terminate(_Reason, _State) ->
    todo.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

