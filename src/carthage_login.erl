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


init(Ref, Socket, Transport, _Opts) ->
    erlang:process_flag(trap_exit, true),
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),

    PacketHeaderLen = carthage_config:get(packet_header_length),
    ok = Transport:setopts(Socket, [{packet, PacketHeaderLen}, {active, once}]),

    NewState = todo,
    gen_server:enter_loop(?MODULE, [], NewState).

handle_info(todo, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    todo.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

