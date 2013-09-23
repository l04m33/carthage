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

-type network_request() :: #nwk_req{}.

-export_type([network_request/0]).

new({_Socket, _Transport} = SockInfo, Data, OnSend) ->
    #nwk_req{
        sock = SockInfo,
        data = Data,
        on_send = OnSend
    }.

new(SrcProc, {_Socket, _Transport} = SockInfo, Data, OnSend) ->
    #nwk_req{
        sock = SockInfo,
        src_proc = SrcProc,
        data = Data,
        on_send = OnSend
    }.

new(SrcProc, ReplyTo, {_Socket, _Transport} = SockInfo, Data, OnSend, OnReply) ->
    #nwk_req{
        sock = SockInfo,
        src_proc = SrcProc,
        reply_to = ReplyTo,
        data = Data,
        on_send = OnSend,
        on_reply = OnReply
    }.

-spec get_peername(Request) ->  {ok, {Address, Port}} | {error, Reason} when
        Request :: network_request(),
        Address :: inet:ip_address(),
        Port :: non_neg_integer(),
        Reason :: inet:posix().
get_peername(#nwk_req{sock = {Socket, Transport}}) ->
    Transport:peername(Socket);
get_peername(#nwk_req{sock = undefined}) ->
    undefined.

-spec get_data(Request) -> term() when
        Request :: network_request().
get_data(Req) ->
    Req#nwk_req.data.

-spec set_data(Data, Request) -> network_request() when
        Data :: term(),
        Request :: network_request().
set_data(Data, Req) ->
    Req#nwk_req{data = Data}.

-spec send(Request, Data) -> ok | {error, Reason} when
        Request :: network_request(),
        Data :: binary() | iolist(),
        Reason :: closed | inet:posix().
send(#nwk_req{sock = undefined}, _Data) ->
    ok;
send(#nwk_req{sock = {Socket, Transport}, on_send = undefined}, Data) ->
    Transport:send(Socket, Data);
send(#nwk_req{sock = {Socket, Transport}, on_send = already_sent}, Data) ->
    Transport:send(Socket, Data);
send(Req = #nwk_req{sock = {Socket, Transport}, on_send = OnSend}, Data) ->
    case OnSend(Data, Req#nwk_req{on_send = already_sent}) of
        {stop, _Reason} = Stop ->
            Stop;
        {ok, NData} ->
            Transport:send(Socket, NData)
    end.

-spec reply(Request, Reply) -> ok when
        Request :: network_request(),
        Reply :: term().
reply(#nwk_req{reply_to = undefined}, _Reply) ->
    ok;
reply(#nwk_req{reply_to = ReplyTo, on_reply = undefined}, Reply) ->
    gen_server:reply(ReplyTo, Reply),
    ok;
reply(#nwk_req{on_reply = already_replied}, _Reply) ->
    exit(already_replied);
reply(Req = #nwk_req{reply_to = ReplyTo, on_reply = OnReply}, Reply) ->
    case OnReply(Reply, Req#nwk_req{on_reply = already_replied}) of
        {stop, _Reason} = Stop ->
            Stop;
        {ok, NReply} ->
            gen_server:reply(ReplyTo, NReply)
    end.

