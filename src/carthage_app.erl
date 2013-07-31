-module(carthage_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case carthage_sup:start_link() of
        {ok, _} = Ret ->
            ListenPort = carthage_config:get(listening_port),
            NumAcceptors = carthage_config:get(number_of_acceptors),
            {ok, _} = ranch:start_listener(carthage_login, NumAcceptors,
                    ranch_tcp, [{port, ListenPort}], carthage_login, []),
            Ret;
        Other ->
            Other
    end.


stop(_State) ->
    ok.

