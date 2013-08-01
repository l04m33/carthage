-module(carthage_login).

-export([start_link/4]).
-export([client_start_ack/1]).
-export([
    init/4,
    handle_info/2,
    terminate/2,
    code_change/3]).


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

    State = {LoginHandler, HandlerState, ClientHandler, ClientOpts},
    gen_server:enter_loop(?MODULE, [], State).

handle_info(todo, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    todo.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

