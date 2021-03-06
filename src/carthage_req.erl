-module(carthage_req).

-include("carthage.hrl").

-export([new/3]).
-export([new/4]).
-export([new/6]).
-export([get_peername/1]).
-export([get_data/1]).
-export([set_data/2]).
-export([send/2]).
-export([reply/2]).

-type carthage_request() :: #cthg_req{}.

-export_type([carthage_request/0]).

new({_Socket, _Transport} = SockInfo, Data, OnSend) ->
    #cthg_req{
        sock = SockInfo,
        data = Data,
        on_send = OnSend
    }.

new(SrcProc, {_Socket, _Transport} = SockInfo, Data, OnSend) ->
    #cthg_req{
        sock = SockInfo,
        src_proc = SrcProc,
        data = Data,
        on_send = OnSend
    }.

new(SrcProc, ReplyTo, {_Socket, _Transport} = SockInfo, Data, OnSend, OnReply) ->
    #cthg_req{
        sock = SockInfo,
        src_proc = SrcProc,
        reply_to = ReplyTo,
        data = Data,
        on_send = OnSend,
        on_reply = OnReply
    }.

-spec get_peername(Request) ->  {ok, {Address, Port}} | {error, Reason} when
        Request :: carthage_request(),
        Address :: inet:ip_address(),
        Port :: non_neg_integer(),
        Reason :: inet:posix().
get_peername(#cthg_req{sock = {Socket, Transport}}) ->
    Transport:peername(Socket);
get_peername(#cthg_req{sock = undefined}) ->
    undefined.

-spec get_data(Request) -> term() when
        Request :: carthage_request().
get_data(Req) ->
    Req#cthg_req.data.

-spec set_data(Data, Request) -> carthage_request() when
        Data :: term(),
        Request :: carthage_request().
set_data(Data, Req) ->
    Req#cthg_req{data = Data}.

-spec send(Request, Data) -> ok | {stop, Reason} when
        Request :: carthage_request(),
        Data :: binary() | iolist(),
        Reason :: term().
send(#cthg_req{sock = undefined}, _Data) ->
    ok;
send(#cthg_req{sock = {Socket, Transport}, on_send = undefined}, Data) ->
    do_send(Transport, Socket, Data);
send(#cthg_req{sock = {Socket, Transport}, on_send = already_sent}, Data) ->
    do_send(Transport, Socket, Data);
send(Req = #cthg_req{sock = {Socket, Transport}, on_send = OnSend}, Data) ->
    case OnSend(Data, Req#cthg_req{on_send = already_sent}) of
        {stop, _Reason} = Stop ->
            Stop;
        {ok, NData} ->
            do_send(Transport, Socket, NData)
    end.

-spec reply(Request, Reply) -> ok when
        Request :: carthage_request(),
        Reply :: term().
reply(#cthg_req{reply_to = undefined}, _Reply) ->
    ok;
reply(#cthg_req{reply_to = ReplyTo, on_reply = undefined}, Reply) ->
    gen_server:reply(ReplyTo, Reply),
    ok;
reply(#cthg_req{on_reply = already_replied}, _Reply) ->
    error(already_replied);
reply(Req = #cthg_req{reply_to = ReplyTo, on_reply = OnReply}, Reply) ->
    case OnReply(Reply, Req#cthg_req{on_reply = already_replied}) of
        {stop, _Reason} = Stop ->
            Stop;
        {ok, NReply} ->
            gen_server:reply(ReplyTo, NReply)
    end.


do_send(Transport, Socket, Data) ->
    case Transport:send(Socket, Data) of
        ok ->
            ok;
        {error, Reason} ->
            error({socket_error, Reason})
    end.
