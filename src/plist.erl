

-module(plist).
-author("vejmelkam@gmail.com").
-include("include/mcfg.hrl").

-export([set_value/3, get_keys/1, get_value/2, has_key/2]).


get_keys(P) ->
    lists:foldl(fun ({Key, _S}, A) -> [Key|A] end, [], P).
    

set_value(K, V, []) ->
    [{K,V}];
set_value(K, V, [{K,_OldV}|P]) ->
    [{K,V}|P];
set_value(K, V, [{K1,V1}|P]) ->
    [{K1,V1}|set_value(K, V, P)].


get_value(K, []) ->
    not_found;
get_value(K, [{K,V}|P]) ->
    V;
get_value(K, [E|P]) ->
    get_value(K, P).


has_key(K, []) ->
    false;
has_key(K, [{K,_V}|P]) ->
    true;
has_key(K, [E|P]) ->
    has_key(K, P).

