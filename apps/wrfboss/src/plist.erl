
%
%  A list of pairs (tuples) that stores a key-value relationship.
%  Preferred in some cases over dictionaries to preserve order
%  of keys (esp. in namelists).
%

-module(plist).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include("include/wrf_cfg.hrl").

-export([new/0, set_value/3, get_keys/1, get_value/2, has_key/2]).


new() ->
    [].

get_keys(P) ->
    lists:map(fun ({Key, _S}) -> Key end, P).
    

set_value(K, V, []) ->
    [{K,V}];
set_value(K, V, [{K,_OldV}|P]) ->
    [{K,V}|P];
set_value(K, V, [{K1,V1}|P]) ->
    [{K1,V1}|set_value(K, V, P)].


get_value(_K, []) ->
    not_found;
get_value(K, [{K,V}|_P]) ->
    V;
get_value(K, [_E|P]) ->
    get_value(K, P).


has_key(_K, []) ->
    false;
has_key(K, [{K,_V}|_P]) ->
    true;
has_key(K, [_E|P]) ->
    has_key(K, P).
