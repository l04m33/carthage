-module(carthage_config).

-export([get/1]).
-export([default/1]).

get(Key) ->
    case application:get_env(carthage, Key) of
        {ok, Value} ->
                Value;
        undefined ->
                default(Key)
    end.

default(packet_header_length) ->
    2;
default(_) ->
    undefined.

