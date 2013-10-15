-module(carthage_client_registry).

-behaviour(gen_server).

-export([where_is/2]).
-export([who_is/2]).
-export([start_link/0]).
-export([login/3]).
-export([logout/3]).
-export([new_registry/1]).
-export([remove_registry/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).


-spec where_is(ClientName, ClientID) -> undefined | pid() when
        ClientName :: atom(),
        ClientID :: term().
where_is(ClientName, ClientID) when is_atom(ClientName) ->
    TableName = ClientName,
    case ets:lookup(TableName, ClientID) of
        [] ->
            undefined;
        [{_, ClientPID}] ->
            ClientPID
    end.

-spec who_is(ClientName, ClientPID) -> undefined | term() when
        ClientName :: atom(),
        ClientPID :: pid().
who_is(ClientName, ClientPID) when is_atom(ClientName) andalso is_pid(ClientPID) ->
    TableName = ClientName,
    MirrorName = get_mirror_name(TableName),
    case ets:lookup(MirrorName, ClientPID) of
        [] ->
            undefined;
        [{_, ClientID}] ->
            ClientID
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(ClientName, ClientID, ClientPID) when is_pid(ClientPID) ->
    TableName = ClientName,
    MirrorName = get_mirror_name(TableName),
    case ets:lookup(TableName, ClientID) of
        [] ->
            void;
        [{_, OldPID} | _] ->
            case erlang:is_process_alive(OldPID) of
                true ->
                    error(inconsistent_state);
                false ->
                    %% May be killed by the supervisor, clean it up
                    ets:delete(TableName, ClientID),
                    ets:delete(MirrorName, OldPID)
            end
    end,
    ets:insert(MirrorName, {ClientPID, ClientID}),
    ets:insert(TableName, {ClientID, ClientPID}),
    ok.

logout(ClientName, ClientID, ClientPID) when is_pid(ClientPID) ->
    TableName = ClientName,
    MirrorName = get_mirror_name(TableName),
    ets:delete(TableName, ClientID),
    ets:delete(MirrorName, ClientPID),
    ok.

new_registry(ClientName) when is_atom(ClientName) ->
    gen_server:call(?MODULE, {new_registry, ClientName}).

remove_registry(ClientName) when is_atom(ClientName) ->
    gen_server:call(?MODULE, {remove_registry, ClientName}).


init(_) ->
    {ok, []}.

handle_call({new_registry, ClientName}, _From, DBList) ->
    TableName = ClientName,
    case lists:keymember(TableName, 1, DBList) of
        true ->
            {reply, registry_exists, DBList};
        false ->
            MirrorName = get_mirror_name(TableName),
            TableSpec = [named_table, {keypos, 1}, set, public, {read_concurrency, true}],

            TableCreated = try
                ets:new(TableName, TableSpec),
                true
            catch error : badarg ->
                false
            end,

            case TableCreated of
                true ->
                    try
                        ets:new(MirrorName, TableSpec),
                        NewDBList = [{TableName, MirrorName} | DBList],
                        {reply, ok, NewDBList}
                    catch error : badarg ->
                        ets:delete(TableName),
                        {reply, table_exists, DBList}
                    end;
                false ->
                    {reply, table_exists, DBList}
            end
    end;

handle_call({remove_registry, ClientName}, _From, DBList) ->
    TableName = ClientName,
    case lists:keyfind(TableName, 1, DBList) of
        {_, MirrorName} ->
            ets:delete(TableName),
            ets:delete(MirrorName),
            NewDBList = lists:keydelete(TableName, 1, DBList),
            {reply, ok, NewDBList};
        false ->
            {reply, registry_doesnt_exist, DBList}
    end.

handle_cast(_Msg, DBList) ->
    {noreply, DBList}.

handle_info(_Msg, DBList) ->
    {noreply, DBList}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_mirror_name(TableName) ->
    list_to_atom(atom_to_list(TableName) ++ "_mirror").

